macro_rules! binary_op {
    ($op:tt,$into:ident,$return_ty:ident,$_self:ident) => {{


            let b = $_self.pop().$into();
          
            let a = $_self.pop().$into();
            
            $_self.push(Value::$return_ty(a $op b))

    }};
}
