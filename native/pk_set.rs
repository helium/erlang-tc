use crate::atom::{cannot_combine, cannot_decrypt, error, ok};
use crate::bin::Bin;
use crate::ciphertext::CiphertextArc;
use crate::commitment::CommitmentArc;
use crate::dec_share::DecShareArc;
use crate::pk::{PkArc, PkRes};
use crate::pk_share::{PKShareArc, PKShareRes};
use crate::sig::SigRes;
use crate::sig_share::SigShareArc;
use rustler::{Encoder, Env, NifResult, ResourceArc, Term};
use serde::{Deserialize, Serialize};
use threshold_crypto::PublicKeySet;

/// Struct to hold PublicKey
#[derive(Serialize, Deserialize)]
pub struct PKSetRes {
    pub pk_set: PublicKeySet,
}

pub type PKSetArc = ResourceArc<PKSetRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PKSetRes, env);
    true
}

#[rustler::nif(name = "pk_set_from_commitment")]
fn pk_set_from_commitment(c_arc: CommitmentArc) -> PKSetArc {
    ResourceArc::new(PKSetRes {
        pk_set: PublicKeySet::from(c_arc.0.clone()),
    })
}

#[rustler::nif(name = "pk_set_public_key")]
fn pk_set_public_key(pk_set_arc: PKSetArc) -> PkArc {
    ResourceArc::new(PkRes {
        pk: pk_set_arc.pk_set.public_key(),
    })
}

#[rustler::nif(name = "pk_set_threshold")]
fn pk_set_threshold(pk_set_arc: PKSetArc) -> usize {
    pk_set_arc.pk_set.threshold()
}

#[rustler::nif(name = "pk_set_public_key_share")]
fn pk_set_public_key_share(pk_set_arc: PKSetArc, i: i64) -> PKShareArc {
    ResourceArc::new(PKShareRes {
        share: pk_set_arc.pk_set.public_key_share(i),
    })
}

#[rustler::nif(name = "pk_set_decrypt", schedule = "DirtyCpu")]
fn pk_set_decrypt<'a>(
    env: Env<'a>,
    pk_set_arc: PKSetArc,
    dec_shares: Vec<(i64, DecShareArc)>,
    cipher_arc: CiphertextArc,
) -> NifResult<Term<'a>> {
    let decrypted: Result<Vec<u8>, threshold_crypto::error::Error> = pk_set_arc.pk_set.decrypt(
        dec_shares.iter().map(|(i, dsa)| (*i, &dsa.dec_share)),
        &cipher_arc.0,
    );

    match decrypted {
        Ok(d) => Ok((ok(), Bin(d)).encode(env)),
        _ => Ok((error(), cannot_decrypt()).encode(env)),
    }
}

#[rustler::nif(name = "pk_set_combine_signatures", schedule = "DirtyCpu")]
fn pk_set_combine_signatures<'a>(
    env: Env<'a>,
    pk_set_arc: PKSetArc,
    sig_shares: Vec<(i64, SigShareArc)>,
) -> NifResult<Term<'a>> {
    let res = pk_set_arc
        .pk_set
        .combine_signatures(sig_shares.iter().map(|(i, sa)| (*i, &sa.sig_share)));

    match res {
        Ok(r) => Ok((ok(), ResourceArc::new(SigRes(r))).encode(env)),
        _ => Ok((error(), cannot_combine()).encode(env)),
    }
}

#[rustler::nif(name = "pk_set_combine")]
pub fn pk_set_combine(pka1: PKSetArc, pka2: PKSetArc) -> PKSetArc {
    ResourceArc::new(PKSetRes {
        pk_set: pka1.pk_set.combine(pka2.pk_set.clone()),
    })
}

#[rustler::nif(name = "pk_set_serialize")]
pub fn pk_set_serialize(pka: PKSetArc) -> Bin {
    // TODO: Investigate allowing specifying encoding type using an erlang atom
    let bytes = bincode::serialize(&pka.pk_set).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "pk_set_deserialize", schedule = "DirtyCpu")]
pub fn pk_set_deserialize(bin: rustler::Binary) -> PKSetArc {
    let pk_set_res = bincode::deserialize(&bin).unwrap();
    PKSetArc::new(pk_set_res)
}

#[rustler::nif(name = "pk_set_cmp")]
pub fn pk_set_cmp(pka1: PKSetArc, pka2: PKSetArc) -> bool {
    pka1.pk_set == pka2.pk_set
}
