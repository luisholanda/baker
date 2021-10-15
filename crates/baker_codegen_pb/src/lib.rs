mod baker {
    pub mod codegen {
        pub mod v1 {
            include!(concat!(env!("OUT_DIR"), "/baker.codegen.v1.rs"));
        }
    }

    pub(self) mod ir {
        pub use baker_ir_pb as v1;
    }

    pub(self) mod pkg {
        pub use baker_pkg_pb as v1;
    }
}

pub use self::baker::codegen::v1::*;
