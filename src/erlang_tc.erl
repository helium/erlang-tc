-module(erlang_tc).

-export([load/0]).

-on_load(load/0).

-export([
    %% field API
    into_fr/1,
    cmp_fr/2,
    zero_fr/0,
    add_assign_fr/2,

    %% G1 API
    g1_zero/0,
    cmp_g1/2,

    %% G1Affine API
    g1_affine_one/0,
    g1_affine_mul/2,

    %% polynomial API
    poly_from_coeffs/1,
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

    %% commitment API
    degree_commitment/1,
    eval_commitment/2,
    cmp_commitment/2,
    reveal_commitment/1,
    add_commitment/2,

    %% bivariate polynomial API
    random_bivar_poly/1,
    degree_bivar_poly/1,
    reveal_bivar_poly/1,
    eval_bivar_poly/3,
    row_bivar_poly/2,
    commitment_bivar_poly/1,
    zeroize_bivar_poly/1,

    %% bivariate commitment API
    degree_bivar_commitment/1,
    eval_bivar_commitment/3,
    row_bivar_commitment/2,
    cmp_bivar_commitment/2,
    reveal_bivar_commitment/1
]).

-type coeff_vec() :: [integer()].
-type uni_samples() :: [{integer(), integer()}, ...].
-type uni_fr_samples() :: [{reference(), reference()}, ...].

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

%% ==================================================================
%% G1
%% ==================================================================

-spec g1_zero() -> reference().
g1_zero() ->
    not_loaded(?LINE).

-spec cmp_g1(G1_1 :: reference(), G1_2 :: reference()) -> boolean().
cmp_g1(_G1_1, _G1_2) ->
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

-spec poly_from_coeffs(CoeffVec :: coeff_vec()) -> reference().
poly_from_coeffs(_CoeffVec) ->
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
