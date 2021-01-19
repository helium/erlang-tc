use rustler::{Env, ResourceArc};
use threshold_crypto::SignatureShare;

/// Struct to hold PublicKeyShare
pub struct SigShareRes {
    pub sig_share: SignatureShare,
}

pub type SigShareArc = ResourceArc<SigShareRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(SigShareRes, env);
    true
}
