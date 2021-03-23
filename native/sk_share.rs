use rustler::{Env, ResourceArc};
use threshold_crypto::SecretKeyShare;
use crate::fr::FrArc;
use crate::ciphertext::CiphertextArc;
use crate::lazy_binary::LazyBinary;
use crate::dec_share::{DecShareRes, DecShareArc};
use crate::sig_share::{SigShareArc, SigShareRes};

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
        dec_share: sk_share_arc.share.decrypt_share(&cipher_arc.0).unwrap()
    })
}

#[rustler::nif(name = "sk_share_sign")]
fn sk_share_sign<'a>(sk_share_arc: SKShareArc, msg: LazyBinary<'a>) -> SigShareArc {
    ResourceArc::new(SigShareRes {
        sig_share: sk_share_arc.share.sign(msg)
    })
}

#[rustler::nif(name = "sk_share_from_fr")]
fn sk_share_from_fr<'a>(fr_arc: FrArc) -> SKShareArc {
    let mut fr = fr_arc.fr.clone();
    ResourceArc::new(SKShareRes {
        share: SecretKeyShare::from_mut(&mut fr)
    })
}
