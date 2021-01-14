use rustler::{Env, ResourceArc};
use threshold_crypto::Ciphertext;

/// Struct to hold PublicKeyShare
pub struct CiphertextRes {
    pub cipher: Ciphertext,
}

pub type CiphertextArc = ResourceArc<CiphertextRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(CiphertextRes, env);
    true
}

#[rustler::nif(name = "ciphertext_verify")]
fn ciphertext_verify(cipher_arc: CiphertextArc) -> bool {
    cipher_arc.cipher.verify()
}
