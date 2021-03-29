use crate::ciphertext::{CiphertextArc, CiphertextRes};
use crate::lazy_binary::LazyBinary;
use crate::sig::SigArc;
use rustler::{Binary, Env, OwnedBinary, ResourceArc};
use std::io::Write as _;
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

#[rustler::nif(name = "pk_to_bytes")]
fn pk_to_bytes<'a>(env: Env<'a>, pk_arc: PkArc) -> Binary<'a> {
    let bin_vec = pk_arc.pk.to_bytes();
    let mut binary = OwnedBinary::new(bin_vec.len()).unwrap();
    binary.as_mut_slice().write_all(&bin_vec).unwrap();
    Binary::from_owned(binary, env)
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
