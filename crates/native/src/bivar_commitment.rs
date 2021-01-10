use crate::commitment::{CommitmentArc, CommitmentRes};
use crate::g1::{G1Arc, G1Res};
use rustler::{Env, ResourceArc};
use threshold_crypto::poly::BivarCommitment;
use threshold_crypto::IntoFr;

/// Struct to hold BivariateCommitment (a commitment over a bivar_poly)
pub struct BivarCommitmentRes {
    pub bicommitment: BivarCommitment,
}

pub type BivarCommitmentArc = ResourceArc<BivarCommitmentRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(BivarCommitmentRes, env);
    true
}

#[rustler::nif(name = "degree_bivar_commitment")]
fn degree_bivar_commitment(c_arc: BivarCommitmentArc) -> usize {
    let bicommitment = c_arc.bicommitment.clone();
    bicommitment.degree()
}

#[rustler::nif(name = "eval_bivar_commitment")]
fn eval_bivar_commitment(c_arc: BivarCommitmentArc, x: i64, y: i64) -> G1Arc {
    let bicommitment = c_arc.bicommitment.clone();
    ResourceArc::new(G1Res {
        g1: bicommitment.evaluate(x.into_fr(), y.into_fr()),
    })
}

#[rustler::nif(name = "row_bivar_commitment")]
fn row_bivar_commitment(c_arc: BivarCommitmentArc, x: i64) -> CommitmentArc {
    let bicommitment = c_arc.bicommitment.clone();
    ResourceArc::new(CommitmentRes {
        commitment: bicommitment.row(x.into_fr()),
    })
}

#[rustler::nif(name = "cmp_bivar_commitment")]
fn cmp_bivar_commitment(c1_arc: BivarCommitmentArc, c2_arc: BivarCommitmentArc) -> bool {
    let c1 = c1_arc.bicommitment.clone();
    let c2 = c2_arc.bicommitment.clone();
    c1 == c2
}

#[rustler::nif(name = "reveal_bivar_commitment")]
fn reveal_bivar_commitment(c_arc: BivarCommitmentArc) -> String {
    let bicommitment = c_arc.bicommitment.clone();
    bicommitment.reveal()
}
