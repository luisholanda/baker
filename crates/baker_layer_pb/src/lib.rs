mod baker {
    pub(self) mod ir {
        pub use baker_ir_pb as v1;
    }

    pub mod layer {
        pub mod v1 {
            include!(concat!(env!("OUT_DIR"), "/baker.layer.v1.rs"));
        }
    }

    pub(self) mod pkg {
        pub use baker_pkg_pb as v1;
    }
}

pub use self::baker::layer::v1::*;
