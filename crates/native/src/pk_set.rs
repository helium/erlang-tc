use rustler::{Env, ResourceArc};
use crate::pk::{PkRes, PkArc};
use crate::commitment::CommitmentArc;
use threshold_crypto::PublicKeySet;

/// Struct to hold PublicKey
pub struct PKSetRes {
    pub pk_set: PublicKeySet
}

pub type PKSetArc = ResourceArc<PKSetRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PKSetRes, env);
    true
}

#[rustler::nif(name = "pk_set_from_commitment")]
fn pk_set_from_commitment(c_arc: CommitmentArc) -> PKSetArc {
    ResourceArc::new(PKSetRes {
        pk_set: PublicKeySet::from(c_arc.commitment.clone())
    })
}

#[rustler::nif(name = "pk_set_public_key")]
fn pk_set_public_key(pk_set_arc: PKSetArc) -> PkArc {
    ResourceArc::new(PkRes {
        pk: pk_set_arc.pk_set.public_key()
    })
}

#[rustler::nif(name = "pk_set_threshold")]
fn pk_set_threshold(pk_set_arc: PKSetArc) -> usize {
    pk_set_arc.pk_set.threshold()
}
