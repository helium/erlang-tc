use crate::{bin::Bin, lazy_binary::LazyBinary, pk::PkArc};
use rustler::{Env, ResourceArc};
use threshold_crypto::{PublicKey, Signature};

/// Struct to hold PublicKey
pub struct SigRes(pub(crate) Signature);

pub type SigArc = ResourceArc<SigRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SigRes, env);
    true
}

#[rustler::nif(name = "sig_parity")]
fn sig_parity(sig_arc: SigArc) -> bool {
    sig_arc.0.parity()
}

#[rustler::nif(name = "sig_serialize")]
fn sig_serialize(sa: SigArc) -> Bin {
    let bytes = bincode::serialize(&sa.0).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "sig_deserialize")]
fn sig_deserialize(bin: rustler::Binary) -> SigArc {
    let sig: Signature = bincode::deserialize(&bin).unwrap();
    ResourceArc::new(SigRes(sig))
}

#[rustler::nif(name = "sig_cmp")]
pub fn sig_cmp(s1: SigArc, s2: SigArc) -> bool {
    s1.0 == s2.0
}

#[rustler::nif(name = "sig_aggregate_from_sigs")]
pub fn sig_aggregate_from_sigs(sigs: Vec<SigArc>) -> SigArc {
    let sigs: Vec<Signature> = sigs.iter().map(|i| i.0.clone()).collect();
    let sig: Signature = Signature::aggregate_from_sigs(&sigs[..]);
    ResourceArc::new(SigRes(sig))
}

#[rustler::nif(name = "sig_core_aggregate_verify")]
pub fn sig_core_aggregate_verify(sig: SigArc, pubkeys_and_msgs: Vec<(PkArc, LazyBinary)>) -> bool {
    let s: Signature = sig.0.clone();
    let pubkeys: Vec<PublicKey> = pubkeys_and_msgs.iter().map(|(i, _)| i.pk).collect();
    let msgs: Vec<&LazyBinary> = pubkeys_and_msgs.iter().map(|(_, m)| m).collect();
    s.core_aggregate_verify(&pubkeys[..], &msgs)
}
