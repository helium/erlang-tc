use rustler::{Env, ResourceArc};
use crate::poly::PolyArc;
use crate::pk_set::{PKSetRes, PKSetArc};
use crate::sk_share::{SKShareRes, SKShareArc};
use threshold_crypto::SecretKeySet;

/// Struct to hold SecretKeySet
pub struct SKSetRes {
    pub sk_set: SecretKeySet
}

pub type SKSetArc = ResourceArc<SKSetRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SKSetRes, env);
    true
}

#[rustler::nif(name = "sk_set_from_poly")]
fn sk_set_from_poly(p_arc: PolyArc) -> SKSetArc {
    ResourceArc::new(SKSetRes {
        sk_set: SecretKeySet::from(p_arc.poly.clone())
    })
}

#[rustler::nif(name = "sk_set_threshold")]
fn sk_set_threshold(sk_set_arc: SKSetArc) -> usize {
    sk_set_arc.sk_set.threshold()
}

#[rustler::nif(name = "sk_set_public_keys")]
fn sk_set_public_keys(sk_set_arc: SKSetArc) -> PKSetArc {
    ResourceArc::new(PKSetRes {
        pk_set: sk_set_arc.sk_set.public_keys()
    })
}

#[rustler::nif(name = "sk_set_secret_key_share")]
fn sk_set_secret_key_share(sk_set_arc: SKSetArc, i: i64) -> SKShareArc {
    ResourceArc::new(SKShareRes {
        share: sk_set_arc.sk_set.secret_key_share(i)
    })
}

#[rustler::nif(name = "sk_set_random")]
fn sk_set_random(threshold: usize) -> SKSetArc {
    let mut rng = &mut rand::thread_rng();
    ResourceArc::new(SKSetRes {
        sk_set: SecretKeySet::random(threshold, &mut rng)
    })
}
