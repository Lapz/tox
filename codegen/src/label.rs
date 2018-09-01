use std::fmt::{self, Display};
#[derive(Debug, Clone, PartialEq, PartialOrd, Copy, Eq, Hash)]
pub struct Label(pub u32);

static mut LABEL_COUNT: u32 = 1;

impl Label {
    pub fn new() -> Self {
        unsafe {
            let label = Label(LABEL_COUNT);

            LABEL_COUNT += 1;
            label
        }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "l{}:", self.0)
    }
}
