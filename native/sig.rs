use rustler::{Binary, Env, OwnedBinary, ResourceArc};
use std::io::Write as _;
use threshold_crypto::Signature;

/// Struct to hold PublicKey
pub struct SigRes(pub(crate) Signature);

pub type SigArc = ResourceArc<SigRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SigRes, env);
    true
}

#[rustler::nif(name = "sig_to_bytes")]
fn sig_to_bytes<'a>(env: Env<'a>, sig_arc: SigArc) -> Binary<'a> {
    let bin_vec = sig_arc.0.to_bytes();
    let mut binary = OwnedBinary::new(bin_vec.len()).unwrap();
    binary.as_mut_slice().write_all(&bin_vec).unwrap();
    Binary::from_owned(binary, env)
}

#[rustler::nif(name = "sig_parity")]
fn sig_parity(sig_arc: SigArc) -> bool {
    sig_arc.0.parity()
}
