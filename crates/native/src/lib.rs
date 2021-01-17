extern crate rustler;
extern crate threshold_crypto;

use rustler::{Env, Term};

mod atom;
mod bivar_commitment;
mod bivar_poly;
mod commitment;
mod fr;
mod g1;
mod g2;
mod g1_affine;
mod lazy_binary;
mod pk;
mod pk_set;
mod pk_share;
mod poly;
mod sk;
mod sk_set;
mod sk_share;
mod sig;
mod sig_share;
mod ciphertext;
mod dec_share;
mod bin;

fn load(env: Env, _: Term) -> bool {
    poly::load(env);
    commitment::load(env);
    bivar_poly::load(env);
    bivar_commitment::load(env);
    fr::load(env);
    g1::load(env);
    g2::load(env);
    g1_affine::load(env);
    pk::load(env);
    pk_set::load(env);
    pk_share::load(env);
    sk::load(env);
    sk_set::load(env);
    sk_share::load(env);
    sig::load(env);
    sig_share::load(env);
    ciphertext::load(env);
    dec_share::load(env);

    true
}

rustler::init!(
    "erlang_tc",
    [
        // Ciphertext API
        ciphertext::ciphertext_verify,
        ciphertext::ciphertext_cmp,
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
        poly::eval_uni_poly_from_fr,
        poly::cmp_poly,
        poly::interpolate_uni_poly,
        poly::interpolate_uni_poly_from_fr,
        poly::zeroize_poly,
        poly::is_zero_poly,
        poly::degree_poly,
        poly::reveal_poly,
        poly::commitment_poly,
        // PublicKey API
        pk::pk_reveal,
        pk::pk_to_bytes,
        pk::pk_verify,
        pk::pk_encrypt,
        // PublicKeyShare API
        pk_share::pk_share_verify_decryption_share,
        pk_share::pk_share_verify,
        // PublicKeySet API
        pk_set::pk_set_from_commitment,
        pk_set::pk_set_public_key,
        pk_set::pk_set_threshold,
        pk_set::pk_set_public_key_share,
        pk_set::pk_set_decrypt,
        pk_set::pk_set_combine_signatures,
        // SecretKey API
        sk::sk_random,
        sk::sk_from_fr,
        sk::sk_public_key,
        sk::sk_reveal,
        sk::sk_sign,
        sk::sk_decrypt,
        // SecretKeyShare API
        sk_share::sk_share_decryption_share,
        sk_share::sk_share_sign,
        // SecretKeySet API
        sk_set::sk_set_from_poly,
        sk_set::sk_set_threshold,
        sk_set::sk_set_public_keys,
        sk_set::sk_set_secret_key_share,
        sk_set::sk_set_random,
        // Signature API
        sig::sig_to_bytes,
        sig::sig_parity,
        // Field Representation API
        fr::into_fr,
        fr::cmp_fr,
        fr::zero_fr,
        fr::add_assign_fr,
        // G1 API
        g1::g1_zero,
        g1::cmp_g1,
        // G2 API
        g2::g2_random,
        // G1Affine API
        g1_affine::g1_affine_one,
        g1_affine::g1_affine_mul,
        // BivarPoly API
        bivar_poly::random_bivar_poly,
        bivar_poly::degree_bivar_poly,
        bivar_poly::reveal_bivar_poly,
        bivar_poly::eval_bivar_poly,
        bivar_poly::row_bivar_poly,
        bivar_poly::commitment_bivar_poly,
        bivar_poly::zeroize_bivar_poly,
        // Commitment API
        commitment::degree_commitment,
        commitment::eval_commitment,
        commitment::cmp_commitment,
        commitment::reveal_commitment,
        commitment::add_commitment,
        // BivarCommitment API
        bivar_commitment::degree_bivar_commitment,
        bivar_commitment::eval_bivar_commitment,
        bivar_commitment::row_bivar_commitment,
        bivar_commitment::cmp_bivar_commitment,
        bivar_commitment::reveal_bivar_commitment,
    ],
    load = load
);
