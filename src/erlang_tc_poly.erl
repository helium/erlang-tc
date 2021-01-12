-module(erlang_tc_poly).

-export([
    %% polynomial API
    from_coeffs/1,
    eval/2,
    eval_from_fr/2,
    cmp/2,
    interpolate/1,
    interpolate_from_fr/1,
    gen_monomial/1,
    degree/1,
    random/1,
    zero/0,
    zeroize/1,
    is_zero/1,
    constant/1,
    mul/2,
    add/2,
    sub/2,
    mul_scalar/2,
    add_scalar/2,
    sub_scalar/2,
    reveal/1,
    commitment/1
]).

-type coeffs() :: [integer()].
-type samples() :: [{integer(), integer()}, ...].
-type fr_samples() :: [{reference(), reference()}, ...].

-spec from_coeffs(Coeffs :: coeffs()) -> reference().
from_coeffs(Coeffs) ->
    erlang_tc:poly_from_coeffs(Coeffs).

-spec eval(Poly :: reference(), Point :: integer()) -> reference().
eval(Poly, Point) ->
    erlang_tc:eval_uni_poly(Poly, Point).

-spec eval_from_fr(Poly :: reference(), Point :: reference()) -> reference().
eval_from_fr(Poly, Point) ->
    erlang_tc:eval_uni_poly_from_fr(Poly, Point).

-spec cmp(P1 :: reference(), P2 :: reference()) -> boolean().
cmp(P1, P2) ->
    erlang_tc:cmp_poly(P1, P2).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate(Samples :: samples()) -> reference().
interpolate(Samples) ->
    erlang_tc:interpolate_uni_poly(Samples).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate_from_fr(Samples :: fr_samples()) -> reference().
interpolate_from_fr(Samples) ->
    erlang_tc:interpolate_uni_poly_from_fr(Samples).

-spec degree(Poly :: reference()) -> non_neg_integer().
degree(Poly) ->
    erlang_tc:degree_poly(Poly).

-spec gen_monomial(Degree :: non_neg_integer()) -> reference().
gen_monomial(Degree) ->
    erlang_tc:gen_monomial(Degree).

-spec random(Degree :: non_neg_integer()) -> reference().
random(Degree) ->
    erlang_tc:random_poly(Degree).

-spec constant(C :: number()) -> reference().
constant(C) ->
    erlang_tc:constant_poly(C).

-spec zero() -> reference().
zero() ->
    erlang_tc:zero_poly().

-spec zeroize(P :: reference()) -> reference().
zeroize(P) ->
    erlang_tc:zeroize_poly(P).

-spec is_zero(P :: reference()) -> boolean().
is_zero(P) ->
    erlang_tc:is_zero_poly(P).

-spec add(P1 :: reference(), P2 :: reference()) -> reference().
add(P1, P2) ->
    erlang_tc:add_poly(P1, P2).

-spec sub(P1 :: reference(), P2 :: reference()) -> reference().
sub(P1, P2) ->
    erlang_tc:sub_poly(P1, P2).

-spec mul(P1 :: reference(), P2 :: reference()) -> reference().
mul(P1, P2) ->
    erlang_tc:mul_poly(P1, P2).

-spec add_scalar(Scalar :: number(), P :: reference()) -> reference().
add_scalar(Scalar, P) ->
    case Scalar < 0 of
        false ->
            erlang_tc:add_scalar_poly(Scalar, P);
        true ->
            sub_scalar(Scalar, P)
    end.

-spec sub_scalar(Scalar :: number(), P :: reference()) -> reference().
sub_scalar(Scalar, P) ->
    erlang_tc:sub_scalar_poly(Scalar, P).

-spec mul_scalar(Scalar :: number(), P :: reference()) -> reference().
mul_scalar(Scalar, P) ->
    erlang_tc:mul_scalar_poly(Scalar, P).

-spec reveal(P :: reference()) -> string().
reveal(P) ->
    erlang_tc:reveal_poly(P).

-spec commitment(P :: reference()) -> reference().
commitment(P) ->
    erlang_tc:commitment_poly(P).
