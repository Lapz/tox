#[macro_export]
macro_rules! nil_type {
    () => {
        InferedType {
            ty:Type::Nil
        }
    };
}

#[macro_export]
macro_rules! bool_type {
    () => {
        InferedType {
            ty:Type::Bool
        }
    };
}

#[macro_export]
macro_rules! str_type {
    () => {
        InferedType {
            ty:Type::Str
        }
    };
}

#[macro_export]
macro_rules! float_type {
    () => {
        InferedType {
            ty:Type::Float
        }
    };
}

#[macro_export]
macro_rules! int_type {
    () => {
        InferedType {
            ty:Type::Int
        }
    };
}
