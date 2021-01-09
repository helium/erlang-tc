-module(erlang_tc).

-export([load/0]).

-on_load(load/0).

-export([
    %% field API
    into_fr/1,
    cmp_fr/2,

    %% polynomial API
    poly_from_coeffs/1,
    eval_uni_poly/2,
    cmp_poly/2,
    interpolate_uni_poly/1,
    gen_monomial/1,
    degree/1,
    random_poly/1,
    zero_poly/0,
    constant_poly/1,
    mul_poly/2,
    add_poly/2,
    sub_poly/2,
    mul_scalar_poly/2,
    add_scalar_poly/2,
    sub_scalar_poly/2
]).

-type coeff_vec() :: [integer()].
-type uni_samples() :: [{integer(), integer()}, ...].

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

-spec into_fr(Num :: integer()) -> reference().
into_fr(_Num) ->
    not_loaded(?LINE).

-spec cmp_fr(FR1 :: reference(), FR2 :: reference()) -> boolean().
cmp_fr(_FR1, _FR2) ->
    not_loaded(?LINE).

-spec poly_from_coeffs(CoeffVec :: coeff_vec()) -> reference().
poly_from_coeffs(_CoeffVec) ->
    not_loaded(?LINE).

-spec eval_uni_poly(Poly :: reference(), Point :: integer()) -> reference().
eval_uni_poly(_Poly, _Point) ->
    not_loaded(?LINE).

-spec cmp_poly(P1 :: reference(), P2 :: reference()) -> boolean().
cmp_poly(_P1, _P2) ->
    not_loaded(?LINE).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate_uni_poly(Samples :: uni_samples()) -> reference().
interpolate_uni_poly(_Samples) ->
    not_loaded(?LINE).

-spec degree(PolyVec :: coeff_vec()) -> non_neg_integer().
degree(PolyVec) ->
    length(PolyVec) - 1.

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
    io:format("Foo"),
    not_loaded(?LINE).

-spec mul_scalar_poly(Scalar :: number(), P :: reference()) -> reference().
mul_scalar_poly(_Scalar, _P) ->
    not_loaded(?LINE).

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
