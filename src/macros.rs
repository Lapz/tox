macro_rules! nil_type {
    () => {
        InferedType {
            ty:Type::Simple(BaseType::Nil)
        }
    };
}

macro_rules! bool_type {
    () => {
        InferedType {
            ty:Type::Simple(BaseType::Bool)
        }
    };
}

macro_rules! str_type {
    () => {
        InferedType {
            ty:Type::Simple(BaseType::Str)
        }
    };
}

macro_rules! float_type {
    () => {
        InferedType {
            ty:Type::Simple(BaseType::Float)
        }
    };
}

macro_rules! int_type {
    () => {
        InferedType {
            ty:Type::Simple(BaseType::Int)
        }
    };
}
