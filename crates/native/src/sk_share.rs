use rustler::{Env, ResourceArc};
use threshold_crypto::SecretKeyShare;
use crate::ciphertext::CiphertextArc;
use crate::dec_share::{DecShareRes, DecShareArc};

/// Struct to hold PublicKeyShare
pub struct SKShareRes {
    pub share: SecretKeyShare,
}

pub type SKShareArc = ResourceArc<SKShareRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SKShareRes, env);
    true
}

#[rustler::nif(name = "sk_share_decryption_share")]
fn sk_share_decryption_share(sk_share_arc: SKShareArc, cipher_arc: CiphertextArc) -> DecShareArc {
    ResourceArc::new(DecShareRes {
        dec_share: sk_share_arc.share.decrypt_share(&cipher_arc.cipher).unwrap()
    })
}
