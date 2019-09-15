pub struct Id(usize);

impl Id {
    pub fn new(id: usize) {
        Id(id)
    }
}

pub struct FunctionId(Id);

pub struct Function {
    id: FunctionId,
}

impl Function {
    fn name() {}

    fn body() {}

    fn ty() {}

    fn infer() {}
}
