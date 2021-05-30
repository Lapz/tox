#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Restrictions {
    pub forbid_record: bool,
}

impl Restrictions {
    pub fn no_records() -> Self {
        Self {
            forbid_record: true,
        }
    }
}
