use crate::bivar_commitment::{BivarCommitmentArc, BivarCommitmentRes};
use crate::fr::{FrArc, FrRes};
use crate::poly::{PolyArc, PolyRes};
use rustler::{Env, ResourceArc};
use threshold_crypto::poly::BivarPoly;
use threshold_crypto::IntoFr;
use zeroize::Zeroize;

/// Struct to hold Bivariate Polynomial
pub struct BivarPolyRes(pub(crate) BivarPoly);

pub type BivarPolyArc = ResourceArc<BivarPolyRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(BivarPolyRes, env);
    true
}

#[rustler::nif(name = "random_bivar_poly")]
fn random_bivar_poly(degree: usize) -> BivarPolyArc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(BivarPolyRes(BivarPoly::random(degree, rng)))
}

#[rustler::nif(name = "degree_bivar_poly")]
fn degree_bivar_poly(bvp: BivarPolyArc) -> usize {
    bvp.0.degree()
}

#[rustler::nif(name = "reveal_bivar_poly")]
fn reveal_bivar_poly(bvp: BivarPolyArc) -> String {
    bvp.0.reveal()
}

#[rustler::nif(name = "eval_bivar_poly")]
fn eval_bivar_poly(bvp: BivarPolyArc, x: i64, y: i64) -> FrArc {
    ResourceArc::new(FrRes {
        fr: bvp.0.evaluate(x.into_fr(), y.into_fr()),
    })
}

#[rustler::nif(name = "row_bivar_poly")]
fn row_bivar_poly(bvp: BivarPolyArc, x: i64) -> PolyArc {
    ResourceArc::new(PolyRes(bvp.0.row(x.into_fr())))
}

#[rustler::nif(name = "commitment_bivar_poly")]
fn commitment_bivar_poly(bvp: BivarPolyArc) -> BivarCommitmentArc {
    ResourceArc::new(BivarCommitmentRes(bvp.0.commitment()))
}

#[rustler::nif(name = "zeroize_bivar_poly")]
fn zeroize_bivar_poly(bvp: BivarPolyArc) -> BivarPolyArc {
    let mut bvp = bvp.0.clone();
    bvp.zeroize();
    ResourceArc::new(BivarPolyRes(bvp))
}

#[rustler::nif(name = "with_secret_bivar_poly")]
fn with_secret_bivar_poly(secret: u64, degree: usize) -> BivarPolyArc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(BivarPolyRes(BivarPoly::with_secret(secret, degree, rng)))
}
