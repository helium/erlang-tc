use crate::bin::Bin;
use rustler::{Env, ResourceArc};
use threshold_crypto::Signature;

/// Struct to hold PublicKey
pub struct SigRes(pub(crate) Signature);

pub type SigArc = ResourceArc<SigRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SigRes, env);
    true
}

#[rustler::nif(name = "sig_parity")]
fn sig_parity(sig_arc: SigArc) -> bool {
    sig_arc.0.parity()
}

#[rustler::nif(name = "sig_serialize")]
fn sig_serialize(sa: SigArc) -> Bin {
    let bytes = bincode::serialize(&sa.0).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "sig_deserialize")]
fn sig_deserialize(bin: rustler::Binary) -> SigArc {
    let sig: Signature = bincode::deserialize(&bin).unwrap();
    ResourceArc::new(SigRes(sig))
}

#[rustler::nif(name = "sig_cmp")]
pub fn sig_cmp(s1: SigArc, s2: SigArc) -> bool {
    s1.0 == s2.0
}
