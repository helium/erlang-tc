use rustler::{Env, ResourceArc};
use threshold_crypto::PublicKeyShare;
use crate::ciphertext::CiphertextArc;
use crate::dec_share::DecShareArc;

/// Struct to hold PublicKeyShare
pub struct PKShareRes {
    pub share: PublicKeyShare,
}

pub type PKShareArc = ResourceArc<PKShareRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PKShareRes, env);
    true
}

#[rustler::nif(name = "pk_share_verify_decryption_share")]
fn pk_share_verify_decryption_share(
    pk_share_arc: PKShareArc,
    dec_share_arc: DecShareArc,
    cipher_arc: CiphertextArc,
) -> bool {
    let dec_share = dec_share_arc.dec_share.clone();
    pk_share_arc
        .share
        .verify_decryption_share(&dec_share, &cipher_arc.cipher)
}
