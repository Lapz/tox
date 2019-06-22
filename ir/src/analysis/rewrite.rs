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
        match instruction {
            Instruction::Binary(dest, lhs, op, rhs) => {
                match (
                    self.spilled_nodes.contains(&lhs),
                    self.spilled_nodes.contains(&rhs),
                ) {
                    (true, true) => {
                        let new_lhs = *self.old_to_new.entry(lhs).or_insert(Register::new());
                        let new_rhs = *self.old_to_new.entry(rhs).or_insert(Register::new());

                        new_temps.insert(new_lhs);
                        new_temps.insert(new_rhs);

                        instructions.push(Instruction::Store(new_lhs, lhs)); //re
                        instructions.push(Instruction::Store(new_rhs, rhs));
                        instructions.push(Instruction::Binary(dest, new_lhs, op, new_rhs));
                        instructions.push(Instruction::Store(lhs, new_lhs));
                        instructions.push(Instruction::Store(rhs, new_lhs));
                    }
                    (true, false) => {
                        let new_lhs = *self.old_to_new.entry(lhs).or_insert(Register::new());
                        new_temps.insert(new_lhs);

                        instructions.push(Instruction::Store(new_lhs, lhs)); //re
                        instructions.push(Instruction::Binary(dest, new_lhs, op, rhs));
                        instructions.push(Instruction::Store(lhs, new_lhs));
                    }

                    (false, true) => {
                        let new_rhs = *self.old_to_new.entry(rhs).or_insert(Register::new());

                        new_temps.insert(new_rhs);

                        instructions.push(Instruction::Store(new_rhs, rhs));
                        instructions.push(Instruction::Binary(dest, lhs, op, new_rhs));
                        instructions.push(Instruction::Store(rhs, new_rhs));
                    }

                    _ => {}
                }
            }

            Instruction::Call(dest, name, args) => {
                let mut new_args = Vec::new();

                for reg in args {
                    if self.spilled_nodes.contains(&reg) {
                        let new_reg = *self.old_to_new.entry(reg).or_insert(Register::new());
                        new_args.push(new_reg);

                        new_temps.insert(new_reg);

                        instructions.push(Instruction::Store(new_reg, reg));
                    } else {
                        new_args.push(reg);
                    }
                }

                instructions.push(Instruction::Call(dest, name, new_args));
            }

            Instruction::Cast(dest, src, size) => {
                if self.spilled_nodes.contains(&src) {
                    let new_reg = *self.old_to_new.entry(src).or_insert(Register::new());
                    instructions.push(Instruction::Store(new_reg, src));
                    instructions.push(Instruction::Cast(dest, new_reg, size));
                    instructions.push(Instruction::Store(src, new_reg));

                    new_temps.insert(new_reg);
                } else {
                    instructions.push(Instruction::Cast(dest, src, size));
                };
            }

            Instruction::Phi(_, _, _) | Instruction::StatementStart => {}

            Instruction::Store(dest, src) => {
                if self.spilled_nodes.contains(&src) {
                    let new_reg = *self.old_to_new.entry(src).or_insert(Register::new());
                    instructions.push(Instruction::Store(new_reg, src));
                    instructions.push(Instruction::Store(dest, new_reg));
                    instructions.push(Instruction::Store(src, new_reg));
                    new_temps.insert(new_reg);
                } else {
                    instructions.push(Instruction::Store(dest, src));
                };
            }

            Instruction::StoreI(..) => {
                instructions.push(instruction);
            }

            Instruction::Unary(dest, src, op) => {
                if self.spilled_nodes.contains(&src) {
                    let new_reg = *self.old_to_new.entry(src).or_insert(Register::new());
                    new_temps.insert(new_reg);
                    instructions.push(Instruction::Store(new_reg, src));
                    instructions.push(Instruction::Unary(dest, new_reg, op));
                    instructions.push(Instruction::Store(src, new_reg));
                } else {
                    instructions.push(Instruction::Unary(dest, src, op));
                };
            }
        }
    }
}
