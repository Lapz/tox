#[macro_use]
mod parsers;
mod symbols;
mod token;

pub const PIE_HEADER_PREFIX: [u8; 4] = [45, 50, 49, 45];
pub const PIE_HEADER_LENGTH: usize = 64;

pub use self::parsers::file;
use self::symbols::{SymbolTable, SymbolType};
use self::token::Token;
use nom::types::CompleteStr;
use std::fs::File;
use std::io::Read;
use std::mem::transmute;

#[derive(Debug)]
/// Responsible for parsing a raw string into bytecode for the VM.
/// Constructing the symbol table
/// Works in two phases:
///     * Phase 1 - Label Extraction and generation of the EPIE header
///     * Phase 2 - Generate bytecode for the body
pub struct Assembler {
    phase: AssemblerPhase,
    symbols: SymbolTable,
    /// The read-only data section constants are put in
    ro: Vec<u8>,
    ro_offset: usize,
    sections: Vec<AssemblerSection>,
    current_section: Option<AssemblerSection>,
    current_instruction: u32,
    errors: Option<Vec<AssemblerError>>,
}

#[derive(Debug, Clone)]
pub enum AssemblerError {
    NoSegmentDeclarationFound { instruction: u32 },
    StringConstantDeclaredWithoutLabel { instruction: u32 },
    SymbolAlreadyDeclared,
    UnknownDirectiveFound { directive: String },
    NonOpcodeInOpcodeField,
    InsufficientSections,
    ParseError { error: String },
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssemblerSection {
    Data { starting_instruction: Option<u32> },
    Code { starting_instruction: Option<u32> },
    Unknown,
}
#[derive(Debug, PartialEq)]
pub enum AssemblerPhase {
    First,
    Second,
}

#[derive(Debug, PartialEq)]
pub struct AssemblerInstruction {
    opcode: Option<Token>,
    label: Option<Token>,
    directive: Option<Token>,
    operand1: Option<Token>,
    operand2: Option<Token>,
    operand3: Option<Token>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub instructions: Vec<AssemblerInstruction>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            phase: AssemblerPhase::First,
            symbols: SymbolTable::new(),
            ro: Vec::new(),
            ro_offset: 4,
            sections: Vec::new(),
            current_section: None,
            current_instruction: 0,
            errors: Some(Vec::new()),
        }
    }

    pub fn assemble_file(&mut self, path: &str) -> Result<Vec<u8>, Vec<AssemblerError>> {
        let mut contents = String::new();

        File::open(path)
            .expect("Couldn't open the file")
            .read_to_string(&mut contents)
            .expect("Coudln't read to file");

        self.assemble(&contents)
    }

    pub fn assemble(&mut self, raw: &str) -> Result<Vec<u8>, Vec<AssemblerError>> {
        match file(CompleteStr(raw)) {
            Ok((_, program)) => {
                // FIRST PHASE



                // Extract labels
                self.process_first_phase(&program);

                if !self.errors.as_ref().unwrap().is_empty() {
                    return Err(self.errors.take().unwrap());
                }

                // Make sure that we have at least one data section and one code section
                if self.sections.len() != 2 {
                    eprintln!("Did not find at least two sections.");
                    self.errors
                        .as_mut()
                        .unwrap()
                        .push(AssemblerError::InsufficientSections);
                    return Err(self.errors.take().unwrap());
                }

                // SECOND PHASE
                let mut body = self.process_second_phase(&program);

                // Get the header
                let mut assembled_program = self.write_pie_header();

                // Merge the header with body
                assembled_program.append(&mut body);

                Ok(assembled_program)
            }

            Err(e) => {
                eprintln!("There was an error assembling the code: {:?}", e);
                Err(vec![AssemblerError::ParseError {
                    error: e.to_string(),
                }])
            }
        }
    }

    pub fn process_first_phase(&mut self, program: &Program) {
        self.extract_labels(program);
        self.phase = AssemblerPhase::Second;
    }

    pub fn process_second_phase(&mut self, program: &Program) -> Vec<u8> {
        program.to_bytes(&self.symbols)
    }

    /// Go through every instruction and look for label declarations.
    /// When label found add it to symbol table, along with the byte we found the label at.
    fn extract_labels(&mut self, p: &Program) {
        for (i, instruction) in p.instructions.iter().enumerate() {
            if instruction.is_label() {
                if self.current_section.is_some() {
                    if let Some(name) = instruction.label_name() {
                        self.symbols.add(name, (i * 4) + 64, SymbolType::Label);
                    }
                } else {
                    self.errors
                        .as_mut()
                        .unwrap()
                        .push(AssemblerError::NoSegmentDeclarationFound {
                            instruction: self.current_instruction,
                        });
                }
            } else if instruction.is_directive() {
                self.process_directive(instruction);
            }

            self.current_instruction += 1;
        }
    }

    fn process_directive(&mut self, i: &AssemblerInstruction) {
        let directive_name = match i.get_directive_name() {
            Some(name) => name,
            None => {
                eprintln!("Directive has an invalid name: {:?}", i);
                return;
            }
        };

        if i.has_operands() {
            if &directive_name == "asciiz" {
                self.handle_asciiz(i);
            } else {
                self.errors
                    .as_mut()
                    .unwrap()
                    .push(AssemblerError::UnknownDirectiveFound {
                        directive: directive_name.clone(),
                    });
                return;
            }
        } else {
            self.process_section_header(&directive_name)
        }
    }

    /// Handles a declaration of a null-terminated string:
    /// hello: .asciiz 'Hello!'
    fn handle_asciiz(&mut self, i: &AssemblerInstruction) {
        // Being a constant declaration, this is only meaningful in the first pass
        if self.phase != AssemblerPhase::First {
            return;
        }

        // In this case, operand1 will have the entire string we need to read in to RO memory
        match i.get_string_constant() {
            Some(s) => {
                match i.get_label_name() {
                    Some(name) => {
                        self.symbols.set_symbol_offset(&name, self.ro_offset);
                    }
                    None => {
                        // This would be someone typing:
                        // .asciiz 'Hello'
                        println!("Found a string constant with no associated label!");
                        return;
                    }
                };
                // We'll read the string into the read-only section byte-by-byte
                for byte in s.as_bytes() {
                    self.ro.push(*byte);
                    self.ro_offset += 1;
                }
                // This is the null termination bit we are using to indicate a string has ended
                self.ro.push(0);
                self.ro_offset += 1;
            }
            None => {
                // This just means someone typed `.asciiz` for some reason
                println!("String constant following an .asciiz was empty");
            }
        }
    }

    /// Handles a declaration of a section header, such as:
    /// .code
    fn process_section_header(&mut self, header_name: &str) {
        let new_section: AssemblerSection = header_name.into();
        // Only specific section names are allowed
        if new_section == AssemblerSection::Unknown {
            println!(
                "Found an section header that is unknown: {:#?}",
                header_name
            );
            return;
        }
        // TODO: Check if we really need to keep a list of all sections seen
        self.sections.push(new_section.clone());
        self.current_section = Some(new_section);
    }

    fn write_pie_header(&self) -> Vec<u8> {
        let mut header = Vec::with_capacity(PIE_HEADER_LENGTH);

        for byte in &PIE_HEADER_PREFIX[0..] {
            header.push(*byte);
        }

        header.extend(unsafe {
            transmute::<u32, [u8; 4]>(self.ro.len() as u32).iter() // writes the length of the header section
        });

        while header.len() < PIE_HEADER_LENGTH {
            header.push(0);
        }

        header
    }
}

impl AssemblerInstruction {
    pub fn to_bytes(&self, symbols: &SymbolTable) -> Vec<u8> {
        let mut results = Vec::with_capacity(4);

        match self.opcode {
            Some(Token::Op(ref code)) => {
                results.push(*code)
            },

            _ => {

                if self.directive.is_some()  || self.label.is_some() {
                    return vec![];
                }else{
                    eprintln!("Non-opcode found in opcode field{:?}",self);
                }

            }
        }

        for operand in &[&self.operand1, &self.operand2, &self.operand3] {
            match operand {
                Some(ref op) => AssemblerInstruction::extract_operand(op, &mut results, symbols),
                None => (),
            }
        }

        while results.len() < 4 {
            results.push(0);
        }

        results
    }

    fn extract_operand(t: &Token, results: &mut Vec<u8>, symbols: &SymbolTable) {
        match t {
            Token::Register(ref reg) => results.push(*reg),
            Token::Number(ref num) => {
                let byte1 = *num as u16;
                let byte2 = byte1 >> 8;

                results.push(byte2 as u8);
                results.push(byte1 as u8);
            }

            Token::LabelUsage(ref label) => {
                if let Some(offset) = symbols.offset(label) {
                    let byte1 = offset;
                    let byte2 = offset >> 8;
                    results.push(byte2 as u8);
                    results.push(byte1 as u8);
                }
            }
            _ => panic!("opcode found in operand field"),
        }
    }

    fn is_label(&self) -> bool {
        self.label.is_some()
    }

    fn is_directive(&self) -> bool {
        self.directive.is_some()
    }

    fn has_operands(&self) -> bool {
        self.operand1.is_some() || self.operand2.is_some() || self.operand3.is_some()
    }

    fn get_directive_name(&self) -> Option<String> {
        match self.directive {
            Some(ref d) => match *d {
                Token::Directive(ref name) => Some(name.to_string()),
                _ => None,
            },
            None => None,
        }
    }
    fn get_string_constant(&self) -> Option<String> {
        match &self.operand1 {
            Some(d) => match d {
                Token::String(name) => Some(name.to_string()),
                _ => None,
            },
            None => None,
        }
    }

    fn get_label_name(&self) -> Option<String> {
        match &self.label {
            Some(l) => match l {
                Token::LabelDeclaration(name) => Some(name.clone()),
                _ => None,
            },
            None => None,
        }
    }

    fn label_name(&self) -> Option<String> {
        match self.label.as_ref() {
            Some(&Token::LabelDeclaration(ref string)) => Some(string.clone()),
            _ => None,
        }
    }
}

impl Program {
    pub fn to_bytes(&self, symbols: &SymbolTable) -> Vec<u8> {
        let mut program = Vec::with_capacity(self.instructions.len() * 4);

        for inst in self.instructions.iter() {
            program.append(&mut inst.to_bytes(symbols));
        }

        program
    }
}

impl Default for AssemblerSection {
    fn default() -> Self {
        AssemblerSection::Unknown
    }
}

impl<'a> From<&'a str> for AssemblerSection {
    fn from(name: &str) -> AssemblerSection {
        match name {
            "data" => AssemblerSection::Data {
                starting_instruction: None,
            },
            "code" => AssemblerSection::Code {
                starting_instruction: None,
            },
            _ => AssemblerSection::Unknown,
        }
    }
}
