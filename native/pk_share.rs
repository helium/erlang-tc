use crate::bin::Bin;
use crate::ciphertext::CiphertextArc;
use crate::dec_share::DecShareArc;
use crate::lazy_binary::LazyBinary;
use crate::sig_share::SigShareArc;
use rustler::{Env, ResourceArc};
use serde::{Deserialize, Serialize};
use threshold_crypto::PublicKeyShare;

/// Struct to hold PublicKeyShare
#[derive(Serialize, Deserialize)]
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
    pk_share_arc
        .share
        .verify_decryption_share(&dec_share_arc.dec_share, &cipher_arc.0)
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

#[rustler::nif(name = "pk_share_combine")]
fn pk_share_combine(pka1: PKShareArc, pka2: PKShareArc) -> PKShareArc {
    ResourceArc::new(PKShareRes {
        share: pka1.share.combine(pka2.share),
    })
}

#[rustler::nif(name = "pk_share_serialize")]
pub fn pk_share_serialize(pka: PKShareArc) -> Bin {
    // TODO: Investigate allowing specifying encoding type using an erlang atom
    let bytes = bincode::serialize(&pka.share).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "pk_share_deserialize")]
pub fn pk_share_deserialize(bin: rustler::Binary) -> PKShareArc {
    let pk_share_res = bincode::deserialize(&bin).unwrap();
    PKShareArc::new(pk_share_res)
}

#[rustler::nif(name = "pk_share_cmp")]
pub fn pk_share_cmp(pka1: PKShareArc, pka2: PKShareArc) -> bool {
    pka1.share == pka2.share
}
