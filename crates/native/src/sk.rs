use crate::fr::FrArc;
use crate::pk::{PkArc, PkRes};
use rustler::{Env, ResourceArc};
use threshold_crypto::SecretKey;

/// Struct to hold SecretKey
pub struct SkRes {
    pub sk: SecretKey,
}

pub type SkArc = ResourceArc<SkRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SkRes, env);
    true
}

#[rustler::nif(name = "sk_random")]
fn sk_random() -> SkArc {
    ResourceArc::new(SkRes {
        sk: SecretKey::random(),
    })
}

// TODO: Add sk_random(RNGCore)...

#[rustler::nif(name = "sk_from_fr")]
fn sk_from_fr(fr_arc: FrArc) -> SkArc {
    let mut fr = fr_arc.fr.clone();
    ResourceArc::new(SkRes {
        sk: SecretKey::from_mut(&mut fr),
    })
}

#[rustler::nif(name = "sk_public_key")]
fn sk_public_key(sk_arc: SkArc) -> PkArc {
    let sk = sk_arc.sk.clone();
    ResourceArc::new(PkRes {
        pk: sk.public_key(),
    })
}

#[rustler::nif(name = "sk_reveal")]
fn sk_reveal(sk_arc: SkArc) -> String {
    let sk = sk_arc.sk.clone();
    sk.reveal()
}
