use crate::ciphertext::CiphertextArc;
use crate::dec_share::{DecShareArc, DecShareRes};
use crate::fr::FrArc;
use crate::lazy_binary::LazyBinary;
use crate::pk_share::{PKShareArc, PKShareRes};
use crate::sig_share::{SigShareArc, SigShareRes};
use rustler::{Env, ResourceArc};
use threshold_crypto::SecretKeyShare;

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
        dec_share: sk_share_arc.share.decrypt_share(&cipher_arc.0).unwrap(),
    })
}

#[rustler::nif(name = "sk_share_sign")]
fn sk_share_sign<'a>(sk_share_arc: SKShareArc, msg: LazyBinary<'a>) -> SigShareArc {
    ResourceArc::new(SigShareRes {
        sig_share: sk_share_arc.share.sign(msg),
    })
}

#[rustler::nif(name = "sk_share_from_fr")]
fn sk_share_from_fr<'a>(fr_arc: FrArc) -> SKShareArc {
    let mut fr = fr_arc.fr.clone();
    ResourceArc::new(SKShareRes {
        share: SecretKeyShare::from_mut(&mut fr),
    })
}

#[rustler::nif(name = "sk_share_public_key_share")]
fn sk_share_public_key_share(sk_share_arc: SKShareArc) -> PKShareArc {
    ResourceArc::new(PKShareRes {
        share: sk_share_arc.share.public_key_share(),
    })
}

#[rustler::nif(name = "sk_share_reveal")]
fn sk_share_reveal(sk_share_arc: SKShareArc) -> String {
    sk_share_arc.share.reveal()
}
