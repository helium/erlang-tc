use rustler::{Env, ResourceArc};
use threshold_crypto::Ciphertext;

/// Struct to hold PublicKeyShare
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
