use crate::commitment::{CommitmentArc, CommitmentRes};
use crate::fr::{FrArc, FrRes};
use rustler::{Env, ListIterator, ResourceArc};
use threshold_crypto::poly::Poly;
use threshold_crypto::{Fr, IntoFr};
use zeroize::Zeroize;

/// Struct to hold Polynomial
pub struct PolyRes(pub(crate) Poly);

pub type PolyArc = ResourceArc<PolyRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(PolyRes, env);
    true
}

#[rustler::nif(name = "poly_from_coeffs")]
fn poly_from_coeffs<'a>(coeffs: ListIterator<'a>) -> PolyArc {
    let coeffs = coeffs
        .map(|coeff| coeff.decode::<i64>().map(IntoFr::into_fr))
        .collect::<Result<Vec<Fr>, _>>()
        .expect("expected a list of i64s");
    ResourceArc::new(PolyRes(Poly::from(coeffs)))
}

#[rustler::nif(name = "eval_uni_poly")]
fn eval_uni_poly(p: PolyArc, point: i64) -> FrArc {
    ResourceArc::new(FrRes {
        fr: p.0.evaluate(point.into_fr()),
    })
}

#[rustler::nif(name = "eval_uni_poly_from_fr")]
fn eval_uni_poly_from_fr(p: PolyArc, point: FrArc) -> FrArc {
    let point = point.fr.clone();
    ResourceArc::new(FrRes {
        fr: p.0.evaluate(point),
    })
}

#[rustler::nif(name = "random_poly")]
fn random_poly(degree: usize) -> PolyArc {
    let rng = &mut rand::thread_rng();
    ResourceArc::new(PolyRes(Poly::random(degree, rng)))
}

#[rustler::nif(name = "interpolate_uni_poly")]
fn interpolate_uni_poly(samples: Vec<(i64, i64)>) -> PolyArc {
    ResourceArc::new(PolyRes(Poly::interpolate(samples)))
}

#[rustler::nif(name = "interpolate_uni_poly_from_fr")]
fn interpolate_uni_poly_from_fr(samples_repr: Vec<(FrArc, FrArc)>) -> PolyArc {
    ResourceArc::new(PolyRes(Poly::interpolate_from_fr(
        samples_repr
            .iter()
            .map(|(f1a, f2a)| (f1a.fr, f2a.fr))
            .collect(),
    )))
}

#[rustler::nif(name = "cmp_poly")]
fn cmp_poly(p1: PolyArc, p2: PolyArc) -> bool {
    p1.0 == p2.0
}

#[rustler::nif(name = "zero_poly")]
fn zero_poly() -> PolyArc {
    ResourceArc::new(PolyRes(Poly::zero()))
}

#[rustler::nif(name = "is_zero_poly")]
fn is_zero_poly(poly: PolyArc) -> bool {
    poly.0.is_zero()
}

#[rustler::nif(name = "zeroize_poly")]
fn zeroize_poly(p: PolyArc) -> PolyArc {
    let mut p = p.0.clone();
    p.zeroize();
    ResourceArc::new(PolyRes(p))
}

#[rustler::nif(name = "constant_poly")]
fn constant_poly(c: i64) -> PolyArc {
    ResourceArc::new(PolyRes(Poly::constant(c.into_fr())))
}

#[rustler::nif(name = "gen_monomial")]
fn gen_monomial(degree: usize) -> PolyArc {
    ResourceArc::new(PolyRes(Poly::monomial(degree)))
}

#[rustler::nif(name = "add_poly")]
fn add_poly(p1: PolyArc, p2: PolyArc) -> PolyArc {
    ResourceArc::new(PolyRes(&p1.0 + &p2.0))
}

#[rustler::nif(name = "sub_poly")]
fn sub_poly(p1: PolyArc, p2: PolyArc) -> PolyArc {
    ResourceArc::new(PolyRes(&p1.0 - &p2.0))
}

#[rustler::nif(name = "add_scalar_poly")]
fn add_scalar_poly(scalar: u64, p1: PolyArc) -> PolyArc {
    ResourceArc::new(PolyRes(p1.0.clone() + scalar))
}

#[rustler::nif(name = "sub_scalar_poly")]
fn sub_scalar_poly(scalar: u64, p1: PolyArc) -> PolyArc {
    let p1 = p1.0.clone();
    ResourceArc::new(PolyRes(p1 - scalar))
}

#[rustler::nif(name = "mul_scalar_poly")]
fn mul_scalar_poly(scalar: u64, p1_arc: PolyArc) -> PolyArc {
    let p1 = p1_arc.0.clone();
    ResourceArc::new(PolyRes(p1 * scalar))
}

#[rustler::nif(name = "mul_poly")]
fn mul_poly(p1: PolyArc, p2: PolyArc) -> PolyArc {
    ResourceArc::new(PolyRes(&p1.0 * &p2.0))
}

#[rustler::nif(name = "degree_poly")]
fn degree_poly(p: PolyArc) -> usize {
    p.0.degree()
}

#[rustler::nif(name = "reveal_poly")]
fn reveal_poly(p: PolyArc) -> String {
    p.0.reveal()
}

#[rustler::nif(name = "commitment_poly")]
fn commitment_poly(p: PolyArc) -> CommitmentArc {
    ResourceArc::new(CommitmentRes(p.0.commitment()))
}
