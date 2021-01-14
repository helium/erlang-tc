use crate::ciphertext::CiphertextArc;
use crate::commitment::CommitmentArc;
use crate::pk::{PkArc, PkRes};
use crate::pk_share::{PKShareArc, PKShareRes};
use crate::dec_share::DecShareArc;
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

#[rustler::nif(name = "pk_set_decrypt")]
fn pk_set_decrypt(pk_set_arc: PKSetArc, dec_shares: Vec<(i64, DecShareArc)>, cipher_arc: CiphertextArc) -> Vec<u8> {
    let mut shares: Vec<(i64, threshold_crypto::DecryptionShare)> = vec![];

    for (index, dec_share_arc) in dec_shares {
        let dec_share = dec_share_arc.dec_share.clone();
        shares.push((index, dec_share))
    }

    pk_set_arc.pk_set.decrypt(shares, &cipher_arc.cipher).unwrap()
}
