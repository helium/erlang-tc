extern crate rustler;
extern crate threshold_crypto;

use rustler::{Env, Term};

mod atom;
mod bin;
mod bivar_commitment;
mod bivar_poly;
mod ciphertext;
mod commitment;
mod dec_share;
mod fr;
mod g1;
mod g1_affine;
mod g2;
mod lazy_binary;
mod pk;
mod pk_set;
mod pk_share;
mod poly;
mod sig;
mod sig_share;
mod sk;
mod sk_set;
mod sk_share;

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
        ciphertext::ciphertext_serialize,
        ciphertext::ciphertext_deserialize,
        // Polynomial API
        poly::poly_from_coeffs,
        poly::poly_from_frs,
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
        poly::serialize_poly,
        poly::deserialize_poly,
        // PublicKey API
        pk::pk_reveal,
        pk::pk_verify,
        pk::pk_encrypt,
        pk::pk_serialize,
        pk::pk_deserialize,
        pk::pk_cmp,
        // PublicKeyShare API
        pk_share::pk_share_verify_decryption_share,
        pk_share::pk_share_verify_signature_share,
        pk_share::pk_share_reveal,
        pk_share::pk_share_combine,
        pk_share::pk_share_serialize,
        pk_share::pk_share_deserialize,
        pk_share::pk_share_cmp,
        // PublicKeySet API
        pk_set::pk_set_from_commitment,
        pk_set::pk_set_public_key,
        pk_set::pk_set_threshold,
        pk_set::pk_set_public_key_share,
        pk_set::pk_set_decrypt,
        pk_set::pk_set_combine_signatures,
        pk_set::pk_set_combine,
        pk_set::pk_set_serialize,
        pk_set::pk_set_deserialize,
        pk_set::pk_set_cmp,
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
        sk_share::sk_share_from_fr,
        sk_share::sk_share_public_key_share,
        sk_share::sk_share_reveal,
        sk_share::sk_share_combine,
        sk_share::sk_share_serialize,
        sk_share::sk_share_deserialize,
        sk_share::sk_share_cmp,
        // SecretKeySet API
        sk_set::sk_set_from_poly,
        sk_set::sk_set_threshold,
        sk_set::sk_set_public_keys,
        sk_set::sk_set_secret_key_share,
        sk_set::sk_set_random,
        // Signature API
        sig::sig_parity,
        sig::sig_cmp,
        sig::sig_serialize,
        sig::sig_deserialize,
        sig::sig_core_aggregate_verify,
        sig::sig_aggregate_from_sigs,
        // Field Representation API
        fr::into_fr,
        fr::cmp_fr,
        fr::zero_fr,
        fr::add_assign_fr,
        fr::serialize_fr,
        fr::deserialize_fr,
        // G1 API
        g1::g1_zero,
        g1::cmp_g1,
        g1::g1_random,
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
        bivar_poly::with_secret_bivar_poly,
        bivar_poly::serialize_bivar_poly,
        bivar_poly::deserialize_bivar_poly,
        // Commitment API
        commitment::degree_commitment,
        commitment::eval_commitment,
        commitment::cmp_commitment,
        commitment::reveal_commitment,
        commitment::add_commitment,
        commitment::serialize_commitment,
        commitment::deserialize_commitment,
        commitment::commitment_public_key,
        // BivarCommitment API
        bivar_commitment::degree_bivar_commitment,
        bivar_commitment::eval_bivar_commitment,
        bivar_commitment::row_bivar_commitment,
        bivar_commitment::cmp_bivar_commitment,
        bivar_commitment::reveal_bivar_commitment,
        bivar_commitment::serialize_bivar_commitment,
        bivar_commitment::deserialize_bivar_commitment,
        // Sig Share API
        sig_share::sig_share_serialize,
        sig_share::sig_share_deserialize,
        // Dec Share API
        dec_share::dec_share_serialize,
        dec_share::dec_share_deserialize,
    ],
    load = load
);
