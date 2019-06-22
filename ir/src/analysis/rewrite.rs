use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, Function, Instruction, Register, STACK_POINTER};
use crate::instructions::{Instruction::*, Value, POINTER_WIDTH};
use cfg_if::cfg_if;
use indexmap::{indexmap, indexset, IndexMap, IndexSet};
use std::cmp::Ordering;
use util::symbol::Symbols;

impl<'a> crate::analysis::color::Allocator<'a> {
    pub(crate) fn rewrite_instruction(
        &mut self,
        instruction: Instruction,
        instructions: &mut Vec<Instruction>,
        new_temps: &mut IndexSet<Register>,
    ) {
        let mut spilling_dest = false;
        let mut spill_dest = None;
        let orig_dest = *instruction.def().get_index(0).unwrap();

        match instruction {
            Instruction::Binary(dest, lhs, op, rhs) => {
                let new_dest = if self.spilled_nodes.contains(&dest) {
                    spilling_dest = true;
                    let reg = Register::new();
                    spill_dest = Some(reg);
                    reg
                } else {
                    dest
                };

                match (
                    self.spilled_nodes.contains(&lhs),
                    self.spilled_nodes.contains(&rhs),
                ) {
                    (true, true) => {
                        let new_lhs = self.new_name(lhs);
                        let new_rhs = self.new_name(rhs);

                        new_temps.insert(new_lhs);
                        new_temps.insert(new_rhs);

                        instructions
                            .push(Instruction::Store(new_lhs, self.spill_mem_location[&lhs])); //re
                        instructions
                            .push(Instruction::Store(new_rhs, self.spill_mem_location[&rhs]));
                        instructions.push(Instruction::Binary(new_dest, new_lhs, op, new_rhs));
                    }
                    (true, false) => {
                        let new_lhs = self.new_name(lhs);

                        new_temps.insert(new_lhs);

                        instructions
                            .push(Instruction::Store(new_lhs, self.spill_mem_location[&lhs])); //re
                        instructions.push(Instruction::Binary(new_dest, new_lhs, op, rhs));
                    }

                    (false, true) => {
                        let new_rhs = self.new_name(rhs);

                        new_temps.insert(new_rhs);

                        instructions
                            .push(Instruction::Store(new_rhs, self.spill_mem_location[&rhs]));
                        instructions.push(Instruction::Binary(new_dest, lhs, op, new_rhs));
                    }

                    _ => {}
                }
            }

            Instruction::Call(dest, name, args) => {
                let new_dest = if self.spilled_nodes.contains(&dest) {
                    spilling_dest = true;
                    let reg = Register::new();
                    spill_dest = Some(reg);
                    reg
                } else {
                    dest
                };

                let mut new_args = Vec::new();

                for reg in args {
                    if self.spilled_nodes.contains(&reg) {
                        let new_reg = self.new_name(reg);

                        new_args.push(new_reg);

                        new_temps.insert(new_reg);

                        instructions
                            .push(Instruction::Store(new_reg, self.spill_mem_location[&reg]));
                    } else {
                        new_args.push(reg);
                    }
                }

                instructions.push(Instruction::Call(dest, name, new_args));
            }

            Instruction::Cast(dest, src, size) => {
                let new_dest = if self.spilled_nodes.contains(&dest) {
                    spilling_dest = true;
                    let reg = Register::new();
                    spill_dest = Some(reg);
                    reg
                } else {
                    dest
                };

                if self.spilled_nodes.contains(&src) {
                    let new_reg = self.new_name(src);
                    instructions.push(Instruction::Store(new_reg, self.spill_mem_location[&src]));
                    instructions.push(Instruction::Cast(new_dest, new_reg, size));

                    new_temps.insert(new_reg);
                } else {
                    instructions.push(Instruction::Cast(new_dest, src, size));
                };
            }

            Instruction::Phi(_, _, _) | Instruction::StatementStart => {}

            Instruction::Store(dest, src) => {
                let new_dest = if self.spilled_nodes.contains(&dest) {
                    spilling_dest = true;
                    let reg = Register::new();
                    spill_dest = Some(reg);
                    reg
                } else {
                    dest
                };
                if self.spilled_nodes.contains(&src) {
                    let new_reg = self.new_name(src);
                    instructions.push(Instruction::Store(new_reg, self.spill_mem_location[&src]));
                    instructions.push(Instruction::Store(new_dest, new_reg));

                    new_temps.insert(new_reg);
                } else {
                    instructions.push(Instruction::Store(new_dest, src));
                };
            }

            Instruction::StoreI(dest, value) => {
                let new_dest = if self.spilled_nodes.contains(&dest) {
                    spilling_dest = true;
                    let reg = Register::new();
                    spill_dest = Some(reg);
                    reg
                } else {
                    dest
                };

                instructions.push(Instruction::StoreI(new_dest, value));
            }

            Instruction::Unary(dest, src, op) => {
                let new_dest = if self.spilled_nodes.contains(&dest) {
                    spilling_dest = true;
                    let reg = Register::new();
                    spill_dest = Some(reg);
                    reg
                } else {
                    dest
                };

                if self.spilled_nodes.contains(&src) {
                    let new_reg = self.new_name(src);
                    new_temps.insert(new_reg);
                    instructions.push(Instruction::Store(new_reg, self.spill_mem_location[&src]));
                    instructions.push(Instruction::Unary(new_dest, new_reg, op));
                } else {
                    instructions.push(Instruction::Unary(new_dest, src, op));
                };
            }
        }

        if spilling_dest {
            instructions.push(Instruction::Store(spill_dest.unwrap(), orig_dest))
        }
    }
}
