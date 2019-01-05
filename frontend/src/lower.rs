use ast as t;
use ir::instructions::*;
use util::symbol::Symbol;
use util::symbol::Symbols;
use std::collections::HashMap;
use ir::types::Type;

#[derive(Debug)]
struct Builder<'a> {
    symbols: &'a Symbols<Register>,
    registers:Vec<Register>,
    parameters:HashMap<Symbol,Register>,
    instructions: Option<Vec<Instruction>>
}




impl <'a> Builder<'a> {
    pub fn new(symbols:&'a Symbols<Register>) -> Self {
        Builder {
            instructions:Some(vec![]),
            symbols,
            registers:Vec::new(),
            parameters:HashMap::new()
        }
    }

    pub fn instructions(&mut self) -> Vec<Instruction> {
        self.instructions.take().unwrap()
    }

    pub fn parameters(&mut self) -> Vec<Register> {
        self.parameters.iter().map(|(_,register)| *register).collect()
    }

    pub fn emit_instruction(&mut self,inst:Inst,ty:Type) {
        self.instructions.as_mut().expect("Basic Block should be started").push(Instruction {
            instruction:inst,
            ty
        });
    }


    pub fn emit_store(&mut self,dest:Value,source:Value,ty:Type) {
        self.instructions.as_mut().expect("Basic block should be started").push(Instruction {
            instruction:Inst::Store(dest,source),
            ty
        })
    }



    pub fn add_param(&mut self,symbol:Symbol) {
        self.parameters.insert(symbol,Register::new());
    }



}

