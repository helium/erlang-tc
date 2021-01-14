use crate::ciphertext::{CiphertextArc, CiphertextRes};
use crate::commitment::CommitmentArc;
use crate::lazy_binary::LazyBinary;
use crate::pk::{PkArc, PkRes};
use crate::pk_share::{PKShareArc, PKShareRes};
use rustler::{Env, ResourceArc};
use threshold_crypto::PublicKeySet;

/// Struct to hold PublicKey
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
        pk_set: PublicKeySet::from(c_arc.commitment.clone()),
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

#[rustler::nif(name = "pk_set_encrypt")]
fn pk_set_encrypt<'a>(pk_arc: PkArc, msg: LazyBinary<'a>) -> CiphertextArc {
    let pk = pk_arc.pk;
    ResourceArc::new(CiphertextRes {
        cipher: pk.encrypt(&msg),
    })
}
