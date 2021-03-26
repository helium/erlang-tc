use crate::bin::Bin;
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

#[rustler::nif(name = "sig_share_serialize")]
fn sig_share_serialize(ssa: SigShareArc) -> Bin {
    let bytes = bincode::serialize(&ssa.sig_share).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "sig_share_deserialize")]
fn sig_share_deserialize(bin: rustler::Binary) -> SigShareArc {
    let sig_share: SignatureShare = bincode::deserialize(&bin).unwrap();
    ResourceArc::new(SigShareRes {
        sig_share: sig_share,
    })
}
