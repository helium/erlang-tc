use crate::bin::Bin;
use crate::g1::{G1Arc, G1Res};
use crate::pk::{PkArc, PkRes};
use rustler::{Env, ResourceArc};
use serde::{Deserialize, Serialize};
use threshold_crypto::poly::Commitment;
use threshold_crypto::IntoFr;

/// Struct to hold Commitment
#[derive(Serialize, Deserialize)]
pub struct CommitmentRes(pub(crate) Commitment);

pub type CommitmentArc = ResourceArc<CommitmentRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(CommitmentRes, env);
    true
}

#[rustler::nif(name = "degree_commitment")]
fn degree_commitment(c_arc: CommitmentArc) -> usize {
    c_arc.0.degree()
}

#[rustler::nif(name = "eval_commitment")]
fn eval_commitment(c_arc: CommitmentArc, point: i64) -> G1Arc {
    ResourceArc::new(G1Res {
        g1: c_arc.0.evaluate(point.into_fr()),
    })
}

#[rustler::nif(name = "cmp_commitment")]
fn cmp_commitment(c1_arc: CommitmentArc, c2_arc: CommitmentArc) -> bool {
    c1_arc.0 == c2_arc.0
}

#[rustler::nif(name = "add_commitment")]
fn add_commitment(c1_arc: CommitmentArc, c2_arc: CommitmentArc) -> CommitmentArc {
    let c1 = c1_arc.0.clone();
    let c2 = c2_arc.0.clone();
    ResourceArc::new(CommitmentRes(c1 + c2))
}

#[rustler::nif(name = "reveal_commitment")]
fn reveal_commitment(c_arc: CommitmentArc) -> String {
    c_arc.0.reveal()
}

#[rustler::nif(name = "serialize_commitment")]
pub fn serialize_commitment(c_arc: CommitmentArc) -> Bin {
    // TODO: Investigate allowing specifying encoding type using an erlang atom
    let bytes = bincode::serialize(&c_arc.0).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "deserialize_commitment")]
pub fn deserialize_commitment(bin: rustler::Binary) -> CommitmentArc {
    let c_res = bincode::deserialize(&bin).unwrap();
    CommitmentArc::new(c_res)
}

#[rustler::nif(name = "commitment_public_key")]
fn commitment_public_key(c_arc: CommitmentArc) -> PkArc {
    ResourceArc::new(PkRes {
        pk: c_arc.0.public_key(),
    })
}
