// use rustler::{Env, ResourceArc};
use rustler::Env;
use threshold_crypto::PublicKeyShare;

/// Struct to hold PublicKeyShare
pub struct PKShareRes {
    pub share: PublicKeyShare,
}

// pub type PKShareArc = ResourceArc<PKShareRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PKShareRes, env);
    true
}
