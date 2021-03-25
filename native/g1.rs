use rustler::{Env, ResourceArc};
use threshold_crypto::G1;
use threshold_crypto::group::CurveProjective;

/// Struct to hold G1
pub struct G1Res {
    pub g1: G1,
}

pub type G1Arc = ResourceArc<G1Res>;

pub fn load(env: Env) -> bool {
    rustler::resource!(G1Res, env);
    true
}

#[rustler::nif(name = "g1_zero")]
fn g1_zero() -> G1Arc {
    ResourceArc::new(G1Res { g1: G1::zero() })
}

#[rustler::nif(name = "cmp_g1")]
fn cmp_g1(g1_arc: G1Arc, g2_arc: G1Arc) -> bool {
    g1_arc.g1 == g2_arc.g1
}

#[rustler::nif(name = "g1_random")]
fn g1_random() -> G1Arc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(G1Res { g1: G1::random(rng) })
}
