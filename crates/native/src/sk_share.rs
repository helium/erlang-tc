use rustler::{Env, ResourceArc};
use threshold_crypto::SecretKeyShare;

/// Struct to hold PublicKeyShare
pub struct SKShareRes {
    pub share: SecretKeyShare,
}

pub type SKShareArc = ResourceArc<SKShareRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SKShareRes, env);
    true
}
