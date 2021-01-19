use crate::g1::{G1Arc, G1Res};
use rustler::{Env, ResourceArc};
use threshold_crypto::poly::Commitment;
use threshold_crypto::IntoFr;

/// Struct to hold Commitment
pub struct CommitmentRes {
    pub commitment: Commitment,
}

pub type CommitmentArc = ResourceArc<CommitmentRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(CommitmentRes, env);
    true
}

#[rustler::nif(name = "degree_commitment")]
fn degree_commitment(c_arc: CommitmentArc) -> usize {
    let commitment = c_arc.commitment.clone();
    commitment.degree()
}

#[rustler::nif(name = "eval_commitment")]
fn eval_commitment(c_arc: CommitmentArc, point: i64) -> G1Arc {
    let commitment = c_arc.commitment.clone();
    ResourceArc::new(G1Res {
        g1: commitment.evaluate(point.into_fr()),
    })
}

#[rustler::nif(name = "cmp_commitment")]
fn cmp_commitment(c1_arc: CommitmentArc, c2_arc: CommitmentArc) -> bool {
    let c1 = c1_arc.commitment.clone();
    let c2 = c2_arc.commitment.clone();
    c1 == c2
}

#[rustler::nif(name = "add_commitment")]
fn add_commitment(c1_arc: CommitmentArc, c2_arc: CommitmentArc) -> CommitmentArc {
    let c1 = c1_arc.commitment.clone();
    let c2 = c2_arc.commitment.clone();
    ResourceArc::new(CommitmentRes {
        commitment: c1 + c2
    })
}

#[rustler::nif(name = "reveal_commitment")]
fn reveal_commitment(c_arc: CommitmentArc) -> String {
    let commitment = c_arc.commitment.clone();
    commitment.reveal()
}
