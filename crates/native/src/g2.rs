use rustler::{Env, ResourceArc};
use threshold_crypto::G2;
use threshold_crypto::group::CurveProjective;

/// Struct to hold G1
pub struct G2Res {
    pub g2: G2,
}

pub type G2Arc = ResourceArc<G2Res>;

pub fn load(env: Env) -> bool {
    rustler::resource!(G2Res, env);
    true
}

#[rustler::nif(name = "g2_random")]
fn g2_random() -> G2Arc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(G2Res { g2: G2::random(rng) })
}
