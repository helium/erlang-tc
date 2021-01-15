use crate::commitment::{CommitmentArc, CommitmentRes};
use crate::fr::{FrArc, FrRes};
use rustler::{Env, ResourceArc};
use threshold_crypto::poly::Poly;
use threshold_crypto::{Fr, IntoFr};
use zeroize::Zeroize;

/// Struct to hold Polynomial
pub struct PolyRes {
    pub poly: Poly,
}

pub type PolyArc = ResourceArc<PolyRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PolyRes, env);
    true
}

#[rustler::nif(name = "poly_from_coeffs")]
fn poly_from_coeffs(vec: Vec<i64>) -> PolyArc {
    let coeffs: Vec<Fr> = vec.iter().map(IntoFr::into_fr).collect();

    ResourceArc::new(PolyRes {
        poly: Poly::from(coeffs),
    })
}

#[rustler::nif(name = "eval_uni_poly")]
fn eval_uni_poly(p_arc: PolyArc, point: i64) -> FrArc {
    let poly = p_arc.poly.clone();
    ResourceArc::new(FrRes {
        fr: poly.evaluate(point.into_fr()),
    })
}

#[rustler::nif(name = "eval_uni_poly_from_fr")]
fn eval_uni_poly_from_fr(p_arc: PolyArc, point_arc: FrArc) -> FrArc {
    let poly = p_arc.poly.clone();
    let point = point_arc.fr.clone();
    ResourceArc::new(FrRes {
        fr: poly.evaluate(point),
    })
}

#[rustler::nif(name = "random_poly")]
fn random_poly(degree: usize) -> PolyArc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(PolyRes {
        poly: Poly::random(degree, rng),
    })
}

#[rustler::nif(name = "interpolate_uni_poly")]
fn interpolate_uni_poly(samples: Vec<(i64, i64)>) -> PolyArc {
    ResourceArc::new(PolyRes {
        poly: Poly::interpolate(samples),
    })
}

#[rustler::nif(name = "interpolate_uni_poly_from_fr")]
fn interpolate_uni_poly_from_fr(samples_repr: Vec<(FrArc, FrArc)>) -> PolyArc {
    ResourceArc::new(PolyRes {
        poly: Poly::interpolate_from_fr(
            samples_repr
                .iter()
                .map(|(f1a, f2a)| (f1a.fr, f2a.fr))
                .collect(),
        ),
    })
}

#[rustler::nif(name = "cmp_poly")]
fn cmp_poly(p1_arc: PolyArc, p2_arc: PolyArc) -> bool {
    let p1 = p1_arc.poly.clone();
    let p2 = p2_arc.poly.clone();
    p1 == p2
}

#[rustler::nif(name = "zero_poly")]
fn zero_poly() -> PolyArc {
    ResourceArc::new(PolyRes { poly: Poly::zero() })
}

#[rustler::nif(name = "is_zero_poly")]
fn is_zero_poly(p_arc: PolyArc) -> bool {
    let poly = p_arc.poly.clone();
    poly.is_zero()
}

#[rustler::nif(name = "zeroize_poly")]
fn zeroize_poly(p_arc: PolyArc) -> PolyArc {
    let mut poly = p_arc.poly.clone();
    poly.zeroize();
    ResourceArc::new(PolyRes { poly: poly })
}

#[rustler::nif(name = "constant_poly")]
fn constant_poly(c: i64) -> PolyArc {
    ResourceArc::new(PolyRes {
        poly: Poly::constant(c.into_fr()),
    })
}

#[rustler::nif(name = "gen_monomial")]
fn gen_monomial(degree: usize) -> PolyArc {
    ResourceArc::new(PolyRes {
        poly: Poly::monomial(degree),
    })
}

#[rustler::nif(name = "add_poly")]
fn add_poly(p1_arc: PolyArc, p2_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.poly.clone();
    let p2 = p2_arc.poly.clone();
    ResourceArc::new(PolyRes { poly: p1 + p2 })
}

#[rustler::nif(name = "sub_poly")]
fn sub_poly(p1_arc: PolyArc, p2_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.poly.clone();
    let p2 = p2_arc.poly.clone();
    ResourceArc::new(PolyRes { poly: p1 - p2 })
}

#[rustler::nif(name = "add_scalar_poly")]
fn add_scalar_poly(scalar: u64, p1_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.poly.clone();
    ResourceArc::new(PolyRes { poly: p1 + scalar })
}

#[rustler::nif(name = "sub_scalar_poly")]
fn sub_scalar_poly(scalar: u64, p1_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.poly.clone();
    ResourceArc::new(PolyRes { poly: p1 - scalar })
}

#[rustler::nif(name = "mul_scalar_poly")]
fn mul_scalar_poly(scalar: u64, p1_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.poly.clone();
    ResourceArc::new(PolyRes { poly: p1 * scalar })
}

#[rustler::nif(name = "mul_poly")]
fn mul_poly(p1_arc: PolyArc, p2_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.poly.clone();
    let p2 = p2_arc.poly.clone();
    ResourceArc::new(PolyRes { poly: p1 * p2 })
}

#[rustler::nif(name = "degree_poly")]
fn degree_poly(p_arc: PolyArc) -> usize {
    let poly = p_arc.poly.clone();
    poly.degree()
}

#[rustler::nif(name = "reveal_poly")]
fn reveal_poly(p_arc: PolyArc) -> String {
    let poly = p_arc.poly.clone();
    poly.reveal()
}

#[rustler::nif(name = "commitment_poly")]
fn commitment_poly(p_arc: PolyArc) -> CommitmentArc {
    let poly = p_arc.poly.clone();
    ResourceArc::new(CommitmentRes {
        commitment: poly.commitment(),
    })
}
