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

/// Executes the entire flow of a layer.
pub fn execute_flow<F>(layer_fn: F) -> std::io::Result<()>
where
    F: FnOnce(LayerRequest) -> std::io::Result<LayerResponse>,
{
    use std::io::{Read, Write};

    use prost::Message;

    let mut buf = Vec::with_capacity(4096);

    let n = std::io::stdin().read_to_end(&mut buf)?;

    let req = LayerRequest::decode(&buf[..n])
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))?;

    let resp = layer_fn(req)?;

    buf.clear();
    resp.encode(&mut buf).unwrap();

    std::io::stdout().write_all(&buf[..resp.encoded_len()])
}
