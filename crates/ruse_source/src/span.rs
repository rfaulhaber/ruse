pub struct Span<'s> {
    fragment: &'s str,
    position: u32,
    offset: u32,
}

impl<'s> Span<'s> {
    pub(crate) fn new(fragment: &'s str, position: u32, offset: u32) -> Self {
        Self {
            fragment,
            position,
            offset,
        }
    }

    pub fn fragment(&self) -> &'s str {
        self.fragment
    }

    pub fn position(&self) -> u32 {
        self.position
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }
}
