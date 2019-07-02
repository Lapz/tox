use crate::analysis::{AnalysisState, Interval};
use crate::instructions::{
    BlockID, Function, Register, POINTER_WIDTH, RAX, RBP, RES, STACK_POINTER,
};
use indexmap::{indexmap, indexset, IndexMap, IndexSet};
use util::symbol::Symbols;
#[derive(Debug)]
pub struct Allocator {
    active: IndexMap<Register, Interval>,
    state: AnalysisState,
    pub(crate) allocated: IndexMap<BlockID, IndexMap<Register, Register>>,
    free: IndexMap<BlockID, IndexMap<Register, Interval>>,
    intervals: IndexMap<BlockID, IndexMap<Register, Interval>>,
    pub(crate) location: IndexMap<BlockID, IndexMap<Register, usize>>,
    offset: usize,
    pool: IndexSet<Register>,
}

// Max number of registers;
const R: usize = 2;

impl Allocator {
    pub fn new(function: &mut Function, symbols: &mut Symbols<()>) -> Self {
        let state = AnalysisState::new(function, symbols);

        let mut free = indexmap!();
        let mut allocated = indexmap!();
        let mut location = indexmap!();

        for (id, _) in &function.blocks {
            free.insert(*id, indexmap!());
            allocated.insert(*id, indexmap!());
            location.insert(*id, indexmap!());
        }

        Self {
            active: indexmap!(),
            intervals: state.intervals.clone(),
            state,
            allocated,
            free,

            location,
            offset: 0,
            pool: indexset!(RAX, RBP),
        }
    }

    pub fn allocate(&mut self, function: &mut Function) {
        for (id, block) in &function.blocks {
            self.state.intervals[id].sort_by(|_, a, _, b| a.start.cmp(&b.start));

            let mut intervals = IndexMap::new();

            std::mem::swap(&mut intervals, &mut self.state.intervals[id]);

            for (reg, interval) in &intervals {
                self.expire_old_intervals(id, *reg, interval);

                if self.active.len() == R {
                    self.spill_at_interval(id, *reg, interval);
                } else {
                    let free = self.pool.pop().unwrap();
                    self.allocated[id].insert(*reg, free);
                    self.active.insert(free, *interval);
                    self.active.sort_by(|_, a, _, b| a.end.cmp(&b.end));
                }
            }

            std::mem::swap(&mut intervals, &mut self.state.intervals[id]);
        }
    }

    fn expire_old_intervals(&mut self, id: &BlockID, reg: Register, interval: &Interval) {
        self.active.sort_by(|_, a, _, b| a.end.cmp(&b.end));

        while !self.active.is_empty() {
            let (r_j, j) = self.active.pop().unwrap();
            if j.end >= interval.start {
                self.active.insert(r_j, j);
                return;
            }

            self.pool.insert(r_j);
        }
    }

    fn spill_at_interval(&mut self, id: &BlockID, reg: Register, interval: &Interval) {
        let (spill_reg, spill) = self.active.pop().unwrap();

        if spill.end > interval.end {
            self.intervals[id].insert(reg, spill);

            self.location[id].insert(reg, self.offset);

            self.offset += POINTER_WIDTH;

            self.active.insert(spill_reg, *interval);

            self.active.sort_by(|_, a, _, b| a.end.cmp(&b.end));
        } else {
            self.location[id].insert(reg, self.offset);
            self.offset += POINTER_WIDTH;
            self.active.insert(spill_reg, spill);
        }
    }
}
