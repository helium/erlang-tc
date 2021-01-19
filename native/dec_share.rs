use rustler::{Env, ResourceArc};
use threshold_crypto::DecryptionShare;

/// Struct to hold PublicKeyShare
pub struct DecShareRes {
    pub dec_share: DecryptionShare,
}

pub type DecShareArc = ResourceArc<DecShareRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(DecShareRes, env);
    true
}
