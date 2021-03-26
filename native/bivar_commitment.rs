use crate::bin::Bin;
use crate::commitment::{CommitmentArc, CommitmentRes};
use crate::g1::{G1Arc, G1Res};
use rustler::{Env, ResourceArc};
use serde::{Deserialize, Serialize};
use threshold_crypto::poly::BivarCommitment;
use threshold_crypto::IntoFr;

/// Struct to hold BivariateCommitment (a commitment over a bivar_poly)
#[derive(Serialize, Deserialize)]
pub struct BivarCommitmentRes(pub(crate) BivarCommitment);

pub type BivarCommitmentArc = ResourceArc<BivarCommitmentRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(BivarCommitmentRes, env);
    true
}

#[rustler::nif(name = "degree_bivar_commitment")]
fn degree_bivar_commitment(bvc: BivarCommitmentArc) -> usize {
    bvc.0.degree()
}

#[rustler::nif(name = "eval_bivar_commitment", schedule = "DirtyCpu")]
fn eval_bivar_commitment(bvc: BivarCommitmentArc, x: i64, y: i64) -> G1Arc {
    ResourceArc::new(G1Res {
        g1: bvc.0.evaluate(x.into_fr(), y.into_fr()),
    })
}

#[rustler::nif(name = "row_bivar_commitment", schedule = "DirtyCpu")]
fn row_bivar_commitment(bvc: BivarCommitmentArc, x: i64) -> CommitmentArc {
    ResourceArc::new(CommitmentRes(bvc.0.row(x.into_fr())))
}

#[rustler::nif(name = "cmp_bivar_commitment")]
fn cmp_bivar_commitment(bvc1: BivarCommitmentArc, bvc2: BivarCommitmentArc) -> bool {
    bvc1.0 == bvc2.0
}

#[rustler::nif(name = "reveal_bivar_commitment")]
fn reveal_bivar_commitment(bvc: BivarCommitmentArc) -> String {
    bvc.0.reveal()
}

#[rustler::nif(name = "serialize_bivar_commitment", schedule = "DirtyCpu")]
pub fn serialize_bivar_commitment(p: BivarCommitmentArc) -> Bin {
    // TODO: Investigate allowing specifying encoding type using an erlang atom
    let bytes = bincode::serialize(&p.0).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "deserialize_bivar_commitment", schedule = "DirtyCpu")]
pub fn deserialize_bivar_commitment(bin: rustler::Binary) -> BivarCommitmentArc {
    let bvc_res = bincode::deserialize(&bin).unwrap();
    BivarCommitmentArc::new(bvc_res)
}
