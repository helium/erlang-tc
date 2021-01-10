use rustler::{Env, ResourceArc};
use crate::g1::{G1Res, G1Arc};
use crate::fr::FrArc;
use threshold_crypto::G1Affine;
use threshold_crypto::group::CurveAffine;

/// Struct to hold G1
pub struct G1AffineRes {
    pub g1_affine: G1Affine,
}

pub type G1AffineArc = ResourceArc<G1AffineRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(G1AffineRes, env);
    true
}

#[rustler::nif(name = "g1_affine_one")]
fn g1_affine_one() -> G1AffineArc {
    ResourceArc::new(G1AffineRes { g1_affine: G1Affine::one() })
}

#[rustler::nif(name = "g1_affine_mul")]
fn g1_affine_mul(g1_affine_arc_1: G1AffineArc, fr_arc: FrArc) -> G1Arc {
    let g1_affine_1 = g1_affine_arc_1.g1_affine.clone();
    let fr = fr_arc.fr.clone();

    ResourceArc::new(G1Res { g1: g1_affine_1.mul(fr) })
}
