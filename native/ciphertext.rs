use crate::bin::Bin;
use rustler::{Env, ResourceArc};
use serde::{Deserialize, Serialize};
use threshold_crypto::Ciphertext;

/// Struct to hold PublicKeyShare
#[derive(Serialize, Deserialize)]
pub struct CiphertextRes(pub(crate) Ciphertext);

pub type CiphertextArc = ResourceArc<CiphertextRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(CiphertextRes, env);
    true
}

#[rustler::nif(name = "ciphertext_verify")]
fn ciphertext_verify(cipher: CiphertextArc) -> bool {
    cipher.0.verify()
}

#[rustler::nif(name = "ciphertext_cmp")]
fn ciphertext_cmp(c1a: CiphertextArc, c2a: CiphertextArc) -> bool {
    c1a.0 == c2a.0
}

#[rustler::nif(name = "ciphertext_serialize")]
pub fn ciphertext_serialize(ca: CiphertextArc) -> Bin {
    // TODO: Investigate allowing specifying encoding type using an erlang atom
    let bytes = bincode::serialize(&ca.0).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "ciphertext_deserialize")]
pub fn ciphertext_deserialize(bin: rustler::Binary) -> CiphertextArc {
    CiphertextArc::new(bincode::deserialize(&bin).unwrap())
}
