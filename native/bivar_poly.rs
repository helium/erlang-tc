use crate::bivar_commitment::{BivarCommitmentArc, BivarCommitmentRes};
use crate::fr::{FrArc, FrRes};
use crate::poly::{PolyArc, PolyRes};
use rustler::{Env, ResourceArc};
use threshold_crypto::poly::BivarPoly;
use threshold_crypto::IntoFr;
use zeroize::Zeroize;

/// Struct to hold Bivariate Polynomial
pub struct BivarPolyRes {
    pub bipoly: BivarPoly,
}

pub type BivarPolyArc = ResourceArc<BivarPolyRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(BivarPolyRes, env);
    true
}

#[rustler::nif(name = "random_bivar_poly")]
fn random_bivar_poly(degree: usize) -> BivarPolyArc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(BivarPolyRes {
        bipoly: BivarPoly::random(degree, rng),
    })
}

#[rustler::nif(name = "degree_bivar_poly")]
fn degree_bivar_poly(p_arc: BivarPolyArc) -> usize {
    let bipoly = p_arc.bipoly.clone();
    bipoly.degree()
}

#[rustler::nif(name = "reveal_bivar_poly")]
fn reveal_bivar_poly(p_arc: BivarPolyArc) -> String {
    let bipoly = p_arc.bipoly.clone();
    bipoly.reveal()
}

#[rustler::nif(name = "eval_bivar_poly")]
fn eval_bivar_poly(p_arc: BivarPolyArc, x: i64, y: i64) -> FrArc {
    let bipoly = p_arc.bipoly.clone();
    ResourceArc::new(FrRes {
        fr: bipoly.evaluate(x.into_fr(), y.into_fr()),
    })
}

#[rustler::nif(name = "row_bivar_poly")]
fn row_bivar_poly(p_arc: BivarPolyArc, x: i64) -> PolyArc {
    let bipoly = p_arc.bipoly.clone();
    ResourceArc::new(PolyRes (bipoly.row(x.into_fr())))
}

#[rustler::nif(name = "commitment_bivar_poly")]
fn commitment_bivar_poly(p_arc: BivarPolyArc) -> BivarCommitmentArc {
    let bipoly = p_arc.bipoly.clone();
    ResourceArc::new(BivarCommitmentRes {
        bicommitment: bipoly.commitment(),
    })
}

#[rustler::nif(name = "zeroize_bivar_poly")]
fn zeroize_bivar_poly(p_arc: BivarPolyArc) -> BivarPolyArc {
    let mut bipoly = p_arc.bipoly.clone();
    bipoly.zeroize();
    ResourceArc::new(BivarPolyRes { bipoly: bipoly })
}

#[rustler::nif(name = "with_secret_bivar_poly")]
fn with_secret_bivar_poly(secret: u64, degree: usize) -> BivarPolyArc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(BivarPolyRes {
        bipoly: BivarPoly::with_secret(secret, degree, rng),
    })
}
