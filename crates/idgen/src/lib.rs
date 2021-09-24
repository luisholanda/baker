pub type Id = u32;

/// Type that can generate ids.
pub trait IdGenerator {
    // Generate a new id.
    fn generate(&mut self) -> Id;
}

/// An [`IdGenerator`] that generate Ids sequentially.
#[derive(Default, Clone, Copy)]
pub struct SequentialGenerator {
    next_value: Id
}

impl IdGenerator for SequentialGenerator {
    fn generate(&mut self) -> Id {
        let next_value = self.next_value;
        self.next_value += 1;

        next_value
    }
}
