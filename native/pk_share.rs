use crate::ciphertext::CiphertextArc;
use crate::dec_share::DecShareArc;
use crate::lazy_binary::LazyBinary;
use crate::sig_share::SigShareArc;
use rustler::{Binary, Env, OwnedBinary, ResourceArc};
use std::io::Write as _;
use threshold_crypto::PublicKeyShare;

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
        .verify_decryption_share(&dec_share, &cipher_arc.0)
}

#[rustler::nif(name = "pk_share_verify_signature_share")]
fn pk_share_verify_signature_share<'a>(
    pk_share_arc: PKShareArc,
    sig_share_arc: SigShareArc,
    msg: LazyBinary<'a>,
) -> bool {
    pk_share_arc.share.verify(&sig_share_arc.sig_share, &msg)
}

#[rustler::nif(name = "pk_share_reveal")]
fn pk_share_reveal(pka: PKShareArc) -> String {
    format!("{:#?}", pka.share)
}

#[rustler::nif(name = "pk_share_to_bytes")]
fn pk_share_to_bytes<'a>(env: Env<'a>, pka: PKShareArc) -> Binary<'a> {
    let bin_vec = pka.share.to_bytes();
    let mut binary = OwnedBinary::new(bin_vec.len()).unwrap();
    binary.as_mut_slice().write_all(&bin_vec).unwrap();
    Binary::from_owned(binary, env)
}

#[rustler::nif(name = "pk_share_combine")]
fn pk_share_combine(pka1: PKShareArc, pka2: PKShareArc) -> PKShareArc {
    ResourceArc::new(PKShareRes {
        share: pka1.share.combine(pka2.share)
    })
}
