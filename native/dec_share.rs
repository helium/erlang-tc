use crate::bin::Bin;
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

#[rustler::nif(name = "dec_share_serialize")]
fn dec_share_serialize(ssa: DecShareArc) -> Bin {
    let bytes = bincode::serialize(&ssa.dec_share).unwrap();
    Bin(bytes)
}

#[rustler::nif(name = "dec_share_deserialize")]
fn dec_share_deserialize(bin: rustler::Binary) -> DecShareArc {
    let dec_share: DecryptionShare = bincode::deserialize(&bin).unwrap();
    ResourceArc::new(DecShareRes {
        dec_share: dec_share,
    })
}
