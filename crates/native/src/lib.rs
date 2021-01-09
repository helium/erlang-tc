extern crate rustler;
extern crate threshold_crypto;

use rustler::{Env, Term};

mod atom;
mod fr;
mod poly;

fn load(env: Env, _: Term) -> bool {
    poly::load(env);
    fr::load(env);

    true
}

rustler::init!(
    "erlang_tc",
    [
        // Polynomial API
        poly::poly_from_coeffs,
        poly::gen_monomial,
        poly::random_poly,
        poly::zero_poly,
        poly::add_poly,
        poly::sub_poly,
        poly::mul_poly,
        poly::constant_poly,
        poly::add_scalar_poly,
        poly::sub_scalar_poly,
        poly::mul_scalar_poly,
        poly::eval_uni_poly,
        poly::cmp_poly,
        poly::interpolate_uni_poly,
        // Fr API
        fr::into_fr,
        fr::cmp_fr,
    ],
    load = load
);
