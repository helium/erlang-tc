use crate::ciphertext::CiphertextArc;
use crate::fr::FrArc;
use crate::lazy_binary::LazyBinary;
use crate::pk::{PkArc, PkRes};
use crate::sig::{SigArc, SigRes};
use rustler::{Binary, Env, OwnedBinary, ResourceArc};
use std::io::Write as _;
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
    ResourceArc::new(PkRes {
        pk: sk_arc.sk.public_key(),
    })
}

#[rustler::nif(name = "sk_reveal")]
fn sk_reveal(sk_arc: SkArc) -> String {
    sk_arc.sk.reveal()
}

#[rustler::nif(name = "sk_sign", schedule = "DirtyCpu")]
fn sk_sign<'a>(sk_arc: SkArc, msg: LazyBinary<'a>) -> SigArc {
    ResourceArc::new(SigRes(sk_arc.sk.sign(msg)))
}

#[rustler::nif(name = "sk_decrypt", schedule = "DirtyCpu")]
fn sk_decrypt<'a>(env: Env<'a>, sk_arc: SkArc, cipher_arc: CiphertextArc) -> Binary<'a> {
    let decrypted = sk_arc.sk.decrypt(&cipher_arc.0).unwrap();
    let mut binary = OwnedBinary::new(decrypted.len()).unwrap();
    binary.as_mut_slice().write_all(&decrypted).unwrap();
    Binary::from_owned(binary, env)
}
