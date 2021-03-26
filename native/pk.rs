use crate::bin::Bin;
use crate::ciphertext::{CiphertextArc, CiphertextRes};
use crate::lazy_binary::LazyBinary;
use crate::sig::SigArc;
use rustler::{Env, ResourceArc};
use threshold_crypto::PublicKey;

/// Struct to hold PublicKey
pub struct PkRes {
    pub pk: PublicKey,
}

pub type PkArc = ResourceArc<PkRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PkRes, env);
    true
}

#[rustler::nif(name = "pk_reveal")]
fn pk_reveal(pk_arc: PkArc) -> String {
    pk_arc.pk.reveal()
}

#[rustler::nif(name = "pk_verify", schedule = "DirtyCpu")]
fn pk_verify<'a>(pk_arc: PkArc, sig_arc: SigArc, msg: LazyBinary<'a>) -> bool {
    pk_arc.pk.verify(&sig_arc.0, msg)
}

#[rustler::nif(name = "pk_encrypt", schedule = "DirtyCpu")]
fn pk_encrypt<'a>(pk_arc: PkArc, msg: LazyBinary<'a>) -> CiphertextArc {
    let pk = pk_arc.pk;
    ResourceArc::new(CiphertextRes(pk.encrypt(&msg)))
}

#[rustler::nif(name = "pk_serialize")]
fn pk_serialize(pk_arc: PkArc) -> Bin {
    let bytes = bincode::serialize(&pk_arc.pk).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "pk_deserialize")]
fn pk_deserialize(bin: rustler::Binary) -> PkArc {
    let pk: PublicKey = bincode::deserialize(&bin).unwrap();
    ResourceArc::new(PkRes { pk: pk })
}

#[rustler::nif(name = "pk_cmp")]
fn pk_cmp(pka1: PkArc, pka2: PkArc) -> bool {
    pka1.pk == pka2.pk
}
