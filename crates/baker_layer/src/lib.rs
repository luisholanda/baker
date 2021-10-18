use std::{
    io::{self, Read, Write},
    path::PathBuf,
    process::{Child, Command, Stdio},
};

use baker_layer_pb::{LayerRequest, LayerResponse};
use prost::Message;

/// A code generation layer.
///
/// Responsible for generating the IR that will be passed to the final code
/// generator.
pub struct Layer {
    exec: PathBuf,
}

impl Layer {
    /// Create a new layer from the executable name.
    pub fn new(exec: &str) -> Option<Self> {
        let full_exec = which::which(exec).ok()?;

        Some(Self { exec: full_exec })
    }

    /// Execute the layer with the given request.
    pub fn execute(&self, req: &LayerRequest) -> io::Result<LayerResponse> {
        let mut layer_proc = self.spawn()?;

        let res = do_layer_flow(&mut layer_proc, req);

        layer_proc.kill()?;

        res
    }

    fn cmd(&self) -> Command {
        let mut cmd = Command::new(&self.exec);
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        cmd
    }

    fn spawn(&self) -> io::Result<Child> {
        self.cmd().spawn()
    }
}

fn do_layer_flow(proc: &mut Child, req: &LayerRequest) -> io::Result<LayerResponse> {
    let mut buf = req.encode_to_vec();

    proc.stdin.take().unwrap().write_all(&buf)?;

    buf.clear();

    let n = proc.stdout.take().unwrap().read_to_end(&mut buf)?;

    LayerResponse::decode(&mut &buf[..n])
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
}
