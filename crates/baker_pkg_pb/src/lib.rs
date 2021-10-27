include!(concat!(env!("OUT_DIR"), "/baker.pkg.v1.rs"));

impl Message {
    pub fn basename(&self) -> &str {
        self.name.rsplit_once('.').map_or(&self.name, |(_, n)| n)
    }

    pub fn scope(&self) -> &str {
        self.name.rsplit_once('.').map_or("", |(s, _)| s)
    }
}
