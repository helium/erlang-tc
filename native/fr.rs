use crate::bin::Bin;
use rustler::{Env, ResourceArc};
use threshold_crypto::ff::Field;
use threshold_crypto::serde_impl::WireFr;
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

#[rustler::nif(name = "into_fr", schedule = "DirtyCpu")]
fn into_fr(num: i64) -> FrArc {
    ResourceArc::new(FrRes { fr: num.into_fr() })
}

#[rustler::nif(name = "cmp_fr")]
fn cmp_fr(f1_arc: FrArc, f2_arc: FrArc) -> bool {
    let f1 = f1_arc.fr;
    let f2 = f2_arc.fr;
    f1 == f2
}

#[rustler::nif(name = "add_assign_fr")]
fn add_assign_fr(f1_arc: FrArc, f2_arc: FrArc) -> FrArc {
    let mut f1 = f1_arc.fr;
    let f2 = f2_arc.fr;
    f1.add_assign(&f2);
    ResourceArc::new(FrRes { fr: f1 })
}

#[rustler::nif(name = "random_fr")]
fn random_fr() -> FrArc {
    // XXX: Do we need to do this on the erlang side? Not quite sure how to do that...
    let mut rng = rand::thread_rng();
    ResourceArc::new(FrRes {
        fr: Fr::random(&mut rng),
    })
}

#[rustler::nif(name = "zero_fr")]
fn zero_fr() -> FrArc {
    ResourceArc::new(FrRes { fr: Fr::zero() })
}

#[rustler::nif(name = "serialize_fr", schedule = "DirtyCpu")]
pub fn serialize_fr(fra: FrArc) -> Bin {
    // TODO: Investigate allowing specifying encoding type using an erlang atom
    let wfr = WireFr::from_fr(fra.fr);
    let bytes = bincode::serialize(&wfr).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "deserialize_fr", schedule = "DirtyCpu")]
pub fn deserialize_fr(bin: rustler::Binary) -> FrArc {
    let wire_fr: WireFr = bincode::deserialize(&bin).unwrap();
    FrArc::new(FrRes {
        fr: wire_fr.into_fr(),
    })
}
