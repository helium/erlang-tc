use rustler::{Env, Error, Encoder, Decoder, Term, OwnedBinary, Binary};
use std::io::Write as _;

pub struct Bin(pub Vec<u8>);

impl<'a> Encoder for Bin {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut bin = OwnedBinary::new(self.0.len()).expect("Failed to alloc");
        bin.as_mut_slice().write_all(&self.0).unwrap();
        bin.release(env).encode(env)
    }
}

impl<'a> Decoder<'a> for Bin {
    fn decode(term: Term<'a>) -> Result<Bin, Error> {
        let binary: Binary = term.decode()?;
        let bytes: Vec<u8> = binary.as_slice().to_owned();
        Ok(Bin(bytes))
    }
}
