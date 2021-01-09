use rustler::{Env, ResourceArc};
use threshold_crypto::{Fr, IntoFr};

/// Struct to hold Fr
pub struct FrRes {
    pub fr: Fr,
}

pub type FrArc = ResourceArc<FrRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(FrRes, env);
    true
}

#[rustler::nif(name = "into_fr")]
fn into_fr(num: i64) -> FrArc {
    ResourceArc::new(FrRes { fr: num.into_fr() })
}

#[rustler::nif(name = "cmp_fr")]
fn cmp_fr(f1_arc: FrArc, f2_arc: FrArc) -> bool {
    let f1 = f1_arc.fr;
    let f2 = f2_arc.fr;
    f1 == f2
}
