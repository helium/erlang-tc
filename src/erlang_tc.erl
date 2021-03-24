%% NOTE: This module is NOT to be used directly!

-module(erlang_tc).

-export([load/0]).

-on_load(load/0).

-export([
    %% Ciphertext API
    ciphertext_verify/1,
    ciphertext_cmp/2,

    %% PublicKey API
    pk_reveal/1,
    pk_to_bytes/1,
    pk_verify/3,
    pk_encrypt/2,

    %% SecretKey API
    sk_random/0,
    sk_from_fr/1,
    sk_public_key/1,
    sk_reveal/1,
    sk_sign/2,
    sk_decrypt/2,

    %% Signature API
    sig_to_bytes/1,
    sig_parity/1,

    %% PublicKeySet API
    pk_set_from_commitment/1,
    pk_set_public_key/1,
    pk_set_threshold/1,
    pk_set_public_key_share/2,
    pk_set_decrypt/3,
    pk_set_combine_signatures/2,
    pk_set_serialize/1,
    pk_set_deserialize/1,

    %% SecretKeySet API
    sk_set_from_poly/1,
    sk_set_threshold/1,
    sk_set_public_keys/1,
    sk_set_secret_key_share/2,
    sk_set_random/1,

    %% SecretKeyShare API
    sk_share_decryption_share/2,
    sk_share_sign/2,
    sk_share_from_fr/1,
    sk_share_public_key_share/1,
    sk_share_reveal/1,

    %% PublicKeyShare API
    pk_share_verify_decryption_share/3,
    pk_share_verify_signature_share/3,
    pk_share_reveal/1,
    pk_share_to_bytes/1,

    %% Field Representation API
    into_fr/1,
    cmp_fr/2,
    zero_fr/0,
    add_assign_fr/2,
    serialize_fr/1,
    deserialize_fr/1,

    %% G1 API
    g1_zero/0,
    cmp_g1/2,
    g1_random/0,

    %% G2 API
    g2_random/0,

    %% G1Affine API
    g1_affine_one/0,
    g1_affine_mul/2,

    %% Polynomial API
    poly_from_coeffs/1,
    poly_from_frs/1,
    eval_uni_poly/2,
    eval_uni_poly_from_fr/2,
    cmp_poly/2,
    interpolate_uni_poly/1,
    interpolate_uni_poly_from_fr/1,
    gen_monomial/1,
    degree_poly/1,
    random_poly/1,
    zero_poly/0,
    zeroize_poly/1,
    is_zero_poly/1,
    constant_poly/1,
    mul_poly/2,
    add_poly/2,
    sub_poly/2,
    mul_scalar_poly/2,
    add_scalar_poly/2,
    sub_scalar_poly/2,
    reveal_poly/1,
    commitment_poly/1,
    serialize_poly/1,
    deserialize_poly/1,

    %% Commitment API
    degree_commitment/1,
    eval_commitment/2,
    cmp_commitment/2,
    reveal_commitment/1,
    add_commitment/2,
    serialize_commitment/1,
    deserialize_commitment/1,
    commitment_public_key/1,

    %% Bivariate Polynomial API
    random_bivar_poly/1,
    degree_bivar_poly/1,
    reveal_bivar_poly/1,
    eval_bivar_poly/3,
    row_bivar_poly/2,
    commitment_bivar_poly/1,
    zeroize_bivar_poly/1,
    with_secret_bivar_poly/2,
    serialize_bivar_poly/1,
    deserialize_bivar_poly/1,

    %% Bivariate Commitment API
    degree_bivar_commitment/1,
    eval_bivar_commitment/3,
    row_bivar_commitment/2,
    cmp_bivar_commitment/2,
    reveal_bivar_commitment/1,
    serialize_bivar_commitment/1,
    deserialize_bivar_commitment/1
]).

-type coeffs() :: [integer()].
-type uni_samples() :: [{integer(), integer()}, ...].
-type uni_fr_samples() :: [{reference(), reference()}, ...].

%% ==================================================================
%% Ciphertext
%% ==================================================================
-spec ciphertext_verify(Ciphertext :: reference()) -> boolean().
ciphertext_verify(_Ciphertext) ->
    not_loaded(?LINE).

-spec ciphertext_cmp(C1 :: reference(), C2 :: reference()) -> boolean().
ciphertext_cmp(_C1, _C2) ->
    not_loaded(?LINE).

%% ==================================================================
%% PK
%% ==================================================================
-spec pk_reveal(PK :: reference()) -> string().
pk_reveal(_PK) ->
    not_loaded(?LINE).

-spec pk_to_bytes(PK :: reference()) -> binary().
pk_to_bytes(_PK) ->
    not_loaded(?LINE).

-spec pk_verify(PK :: reference(), Sig :: reference(), Msg :: binary()) -> binary().
pk_verify(_PK, _Sig, _Msg) ->
    not_loaded(?LINE).

-spec pk_encrypt(PK :: reference(), Msg :: binary()) -> reference().
pk_encrypt(_PK, _Msg) ->
    not_loaded(?LINE).

%% ==================================================================
%% SK
%% ==================================================================

-spec sk_random() -> reference().
sk_random() ->
    not_loaded(?LINE).

-spec sk_from_fr(Fr :: reference()) -> reference().
sk_from_fr(_Fr) ->
    not_loaded(?LINE).

-spec sk_public_key(SK :: reference()) -> reference().
sk_public_key(_SK) ->
    not_loaded(?LINE).

-spec sk_reveal(SK :: reference()) -> string().
sk_reveal(_SK) ->
    not_loaded(?LINE).

-spec sk_sign(SK :: reference(), Msg :: binary()) -> reference().
sk_sign(_SK, _Msg) ->
    not_loaded(?LINE).

-spec sk_decrypt(SK :: reference(), Cipher :: reference()) -> binary().
sk_decrypt(_SK, _Cipher) ->
    not_loaded(?LINE).

%% ==================================================================
%% SecretKeySet
%% ==================================================================

-spec sk_set_from_poly(Poly :: reference()) -> reference().
sk_set_from_poly(_Poly) ->
    not_loaded(?LINE).

-spec sk_set_threshold(SKSet :: reference()) -> non_neg_integer().
sk_set_threshold(_SKSet) ->
    not_loaded(?LINE).

-spec sk_set_public_keys(SKSet :: reference()) -> reference().
sk_set_public_keys(_SKSet) ->
    not_loaded(?LINE).

-spec sk_set_secret_key_share(SKSet :: reference(), I :: non_neg_integer()) -> reference().
sk_set_secret_key_share(_SKSet, _I) ->
    not_loaded(?LINE).

-spec sk_set_random(Threshold :: non_neg_integer()) -> reference().
sk_set_random(_Threshold) ->
    not_loaded(?LINE).

%% ==================================================================
%% SecretKeyShare
%% ==================================================================

-spec sk_share_decryption_share(SKShare :: reference(), Ciphertext :: reference()) -> reference().
sk_share_decryption_share(_SKShare, _Ciphertext) ->
    not_loaded(?LINE).

-spec sk_share_sign(SKShare :: reference(), Msg :: binary()) -> reference().
sk_share_sign(_SKShare, _Msg) ->
    not_loaded(?LINE).

-spec sk_share_from_fr(Fr :: reference()) -> reference().
sk_share_from_fr(_Fr) ->
    not_loaded(?LINE).

-spec sk_share_public_key_share(SKShare :: reference()) -> reference().
sk_share_public_key_share(_SKShare) ->
    not_loaded(?LINE).

-spec sk_share_reveal(SKShare :: reference()) -> string().
sk_share_reveal(_SKShare) ->
    not_loaded(?LINE).

%% ==================================================================
%% Signature
%% ==================================================================

-spec sig_to_bytes(Sig :: reference()) -> binary().
sig_to_bytes(_Sig) ->
    not_loaded(?LINE).

-spec sig_parity(Sig :: reference()) -> boolean().
sig_parity(_Sig) ->
    not_loaded(?LINE).

%% ==================================================================
%% PublicKeyShare
%% ==================================================================

-spec pk_set_from_commitment(Commitment :: reference()) -> reference().
pk_set_from_commitment(_Commitment) ->
    not_loaded(?LINE).

-spec pk_set_public_key(PKSet :: reference()) -> reference().
pk_set_public_key(_PKSet) ->
    not_loaded(?LINE).

-spec pk_set_threshold(PKSet :: reference()) -> non_neg_integer().
pk_set_threshold(_PKSet) ->
    not_loaded(?LINE).

-spec pk_set_public_key_share(PKSet :: reference(), I :: non_neg_integer()) -> reference().
pk_set_public_key_share(_PKSet, _I) ->
    not_loaded(?LINE).

-spec pk_set_decrypt(PKSet :: reference(),
                     DecShares :: [{non_neg_integer(), reference()}],
                     Cipher :: reference()) -> {ok, binary()} | {error, cannot_decrypt}.
pk_set_decrypt(_PKSet, _DecShares, _Cipher) ->
    not_loaded(?LINE).

-spec pk_set_combine_signatures(PKSet :: reference(),
                     SigShare :: [{non_neg_integer(), reference()}]) -> {ok, reference()} | {error, cannot_combine}.
pk_set_combine_signatures(_PKSet, _SigShares) ->
    not_loaded(?LINE).

-spec pk_set_serialize(PKSet :: reference()) -> binary().
pk_set_serialize(_PKSet) ->
    not_loaded(?LINE).

-spec pk_set_deserialize(Bin :: binary()) -> reference().
pk_set_deserialize(_Bin) ->
    not_loaded(?LINE).

%% ==================================================================
%% PublicKeyShare
%% ==================================================================

-spec pk_share_verify_decryption_share(PKShare :: reference(), DecShare :: reference(), Cipher :: reference()) -> boolean().
pk_share_verify_decryption_share(_PKShare, _DecShare, _Cipher) ->
    not_loaded(?LINE).

-spec pk_share_verify_signature_share(PKShare :: reference(), Sig :: reference(), Msg :: binary()) -> boolean().
pk_share_verify_signature_share(_PKShare, _Sig, _Msg) ->
    not_loaded(?LINE).

-spec pk_share_reveal(PKShare :: reference()) -> string().
pk_share_reveal(_PKShare) ->
    not_loaded(?LINE).

-spec pk_share_to_bytes(PKShare :: reference()) -> binary().
pk_share_to_bytes(_PKShare) ->
    not_loaded(?LINE).

%% ==================================================================
%% Field
%% ==================================================================

-spec into_fr(Num :: integer()) -> reference().
into_fr(_Num) ->
    not_loaded(?LINE).

-spec cmp_fr(FR1 :: reference(), FR2 :: reference()) -> boolean().
cmp_fr(_FR1, _FR2) ->
    not_loaded(?LINE).

-spec zero_fr() -> reference().
zero_fr() ->
    not_loaded(?LINE).

-spec add_assign_fr(FR1 :: reference(), FR2 :: reference()) -> reference().
add_assign_fr(_FR1, _FR2) ->
    not_loaded(?LINE).

-spec serialize_fr(Fr :: reference()) -> binary().
serialize_fr(_Fr) ->
    not_loaded(?LINE).

-spec deserialize_fr(BinFr :: binary()) -> reference().
deserialize_fr(_BinFr) ->
    not_loaded(?LINE).

%% ==================================================================
%% G1
%% ==================================================================

-spec g1_zero() -> reference().
g1_zero() ->
    not_loaded(?LINE).

-spec cmp_g1(G1_1 :: reference(), G1_2 :: reference()) -> boolean().
cmp_g1(_G1_1, _G1_2) ->
    not_loaded(?LINE).

-spec g1_random() -> reference().
g1_random() ->
    not_loaded(?LINE).

%% ==================================================================
%% G2
%% ==================================================================

-spec g2_random() -> reference().
g2_random() ->
    not_loaded(?LINE).

%% ==================================================================
%% G1Affine
%% ==================================================================

-spec g1_affine_one() -> reference().
g1_affine_one() ->
    not_loaded(?LINE).

-spec g1_affine_mul(G1Affine :: reference(), Fr :: reference()) -> reference().
g1_affine_mul(_G1Affine, _Fr) ->
    not_loaded(?LINE).

%% ==================================================================
%% UniVariate Polynomial
%% ==================================================================

-spec poly_from_coeffs(CoeffVec :: coeffs()) -> reference().
poly_from_coeffs(_CoeffVec) ->
    not_loaded(?LINE).

-spec poly_from_frs(FRVec :: [fr:fr()]) -> reference().
poly_from_frs(_FRVec) ->
    not_loaded(?LINE).

-spec eval_uni_poly(Poly :: reference(), Point :: integer()) -> reference().
eval_uni_poly(_Poly, _Point) ->
    not_loaded(?LINE).

-spec eval_uni_poly_from_fr(Poly :: reference(), Point :: reference()) -> reference().
eval_uni_poly_from_fr(_Poly, _Point) ->
    not_loaded(?LINE).

-spec cmp_poly(P1 :: reference(), P2 :: reference()) -> boolean().
cmp_poly(_P1, _P2) ->
    not_loaded(?LINE).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate_uni_poly(Samples :: uni_samples()) -> reference().
interpolate_uni_poly(_Samples) ->
    not_loaded(?LINE).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate_uni_poly_from_fr(Samples :: uni_fr_samples()) -> reference().
interpolate_uni_poly_from_fr(_Samples) ->
    not_loaded(?LINE).

-spec degree_poly(Poly :: reference()) -> non_neg_integer().
degree_poly(_Poly) ->
    not_loaded(?LINE).

-spec gen_monomial(Degree :: non_neg_integer()) -> reference().
gen_monomial(_Degree) ->
    not_loaded(?LINE).

-spec random_poly(Degree :: non_neg_integer()) -> reference().
random_poly(_Degree) ->
    not_loaded(?LINE).

-spec constant_poly(C :: number()) -> reference().
constant_poly(_C) ->
    not_loaded(?LINE).

-spec zero_poly() -> reference().
zero_poly() ->
    not_loaded(?LINE).

-spec zeroize_poly(P :: reference()) -> reference().
zeroize_poly(_P) ->
    not_loaded(?LINE).

-spec is_zero_poly(P :: reference()) -> boolean().
is_zero_poly(_P) ->
    not_loaded(?LINE).

-spec add_poly(P1 :: reference(), P2 :: reference()) -> reference().
add_poly(_P1, _P2) ->
    not_loaded(?LINE).

-spec sub_poly(P1 :: reference(), P2 :: reference()) -> reference().
sub_poly(_P1, _P2) ->
    not_loaded(?LINE).

-spec mul_poly(P1 :: reference(), P2 :: reference()) -> reference().
mul_poly(_P1, _P2) ->
    not_loaded(?LINE).

-spec add_scalar_poly(Scalar :: number(), P :: reference()) -> reference().
add_scalar_poly(Scalar, P) ->
    case Scalar < 0 of
        false ->
            not_loaded(?LINE);
        true ->
            sub_scalar_poly(Scalar, P)
    end.

-spec sub_scalar_poly(Scalar :: number(), P :: reference()) -> reference().
sub_scalar_poly(_Scalar, _P) ->
    not_loaded(?LINE).

-spec reveal_poly(P :: reference()) -> string().
reveal_poly(_P) ->
    not_loaded(?LINE).

-spec commitment_poly(P :: reference()) -> reference().
commitment_poly(_P) ->
    not_loaded(?LINE).

-spec serialize_poly(P :: reference()) -> binary().
serialize_poly(_P) ->
    not_loaded(?LINE).

-spec deserialize_poly(B :: binary()) -> reference().
deserialize_poly(_B) ->
    not_loaded(?LINE).

-spec mul_scalar_poly(Scalar :: number(), P :: reference()) -> reference().
mul_scalar_poly(_Scalar, _P) ->
    not_loaded(?LINE).

%% ==================================================================
%% Commitment
%% ==================================================================

-spec degree_commitment(C :: reference()) -> non_neg_integer().
degree_commitment(_P) ->
    not_loaded(?LINE).

-spec eval_commitment(C :: reference(), Point :: integer()) -> reference().
eval_commitment(_C, _Point) ->
    not_loaded(?LINE).

-spec cmp_commitment(C1 :: reference(), C2 :: reference()) -> boolean().
cmp_commitment(_C1, _C2) ->
    not_loaded(?LINE).

-spec reveal_commitment(C :: reference()) -> string().
reveal_commitment(_C) ->
    not_loaded(?LINE).

-spec add_commitment(C1 :: reference(), C2 :: reference()) -> reference().
add_commitment(_C1, _C2) ->
    not_loaded(?LINE).

-spec serialize_commitment(C :: reference()) -> binary().
serialize_commitment(_C) ->
    not_loaded(?LINE).

-spec deserialize_commitment(B :: binary()) -> reference().
deserialize_commitment(_B) ->
    not_loaded(?LINE).

-spec commitment_public_key(C :: reference()) -> reference().
commitment_public_key(_C) ->
    not_loaded(?LINE).

%% ==================================================================
%% BiVariate Polynomial
%% ==================================================================

-spec random_bivar_poly(Degree :: non_neg_integer()) -> reference().
random_bivar_poly(_Degree) ->
    not_loaded(?LINE).

-spec degree_bivar_poly(BiPoly :: reference()) -> non_neg_integer().
degree_bivar_poly(_BiPoly) ->
    not_loaded(?LINE).

-spec reveal_bivar_poly(BiPoly :: reference()) -> string().
reveal_bivar_poly(_BiPoly) ->
    not_loaded(?LINE).

-spec eval_bivar_poly(BiPoly :: reference(), X :: integer(), Y :: integer()) -> reference().
eval_bivar_poly(_BiPoly, _X, _Y) ->
    not_loaded(?LINE).

-spec row_bivar_poly(BiPoly :: reference(), X :: integer()) -> reference().
row_bivar_poly(_BiPoly, _X) ->
    not_loaded(?LINE).

-spec commitment_bivar_poly(BiPoly :: reference()) -> reference().
commitment_bivar_poly(_BiPoly) ->
    not_loaded(?LINE).

-spec zeroize_bivar_poly(BiPoly :: reference()) -> reference().
zeroize_bivar_poly(_BiPoly) ->
    not_loaded(?LINE).

-spec with_secret_bivar_poly(Secret :: non_neg_integer(), Degree :: non_neg_integer()) -> reference().
with_secret_bivar_poly(_Secret, _Degree) ->
    not_loaded(?LINE).

-spec serialize_bivar_poly(P :: reference()) -> binary().
serialize_bivar_poly(_P) ->
    not_loaded(?LINE).

-spec deserialize_bivar_poly(B :: binary()) -> reference().
deserialize_bivar_poly(_B) ->
    not_loaded(?LINE).

%% ==================================================================
%% BiVariate Commitment
%% ==================================================================

-spec degree_bivar_commitment(C :: reference()) -> non_neg_integer().
degree_bivar_commitment(_C) ->
    not_loaded(?LINE).

-spec eval_bivar_commitment(C :: reference(), X :: integer(), Y :: integer()) -> reference().
eval_bivar_commitment(_C, _X, _Y) ->
    not_loaded(?LINE).

-spec row_bivar_commitment(C :: reference(), X :: integer()) -> reference().
row_bivar_commitment(_C, _X) ->
    not_loaded(?LINE).

-spec cmp_bivar_commitment(C1 :: reference(), C2 :: reference()) -> boolean().
cmp_bivar_commitment(_C1, _C2) ->
    not_loaded(?LINE).

-spec reveal_bivar_commitment(C :: reference()) -> string().
reveal_bivar_commitment(_C) ->
    not_loaded(?LINE).

-spec serialize_bivar_commitment(P :: reference()) -> binary().
serialize_bivar_commitment(_P) ->
    not_loaded(?LINE).

-spec deserialize_bivar_commitment(B :: binary()) -> reference().
deserialize_bivar_commitment(_B) ->
    not_loaded(?LINE).

%% ==================================================================
%% NIF
%% ==================================================================

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
