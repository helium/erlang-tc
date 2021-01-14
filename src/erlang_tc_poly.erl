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
-type poly() :: reference().

-export_type([poly/0]).

-spec from_coeffs(Coeffs :: coeffs()) -> poly().
from_coeffs(Coeffs) ->
    erlang_tc:poly_from_coeffs(Coeffs).

-spec eval(Poly :: poly(), Point :: integer()) -> erlang_tc_fr:fr().
eval(Poly, Point) ->
    erlang_tc:eval_uni_poly(Poly, Point).

-spec eval_from_fr(Poly :: poly(), Point :: erlang_tc_fr:fr()) -> erlang_tc_fr:fr().
eval_from_fr(Poly, Point) ->
    erlang_tc:eval_uni_poly_from_fr(Poly, Point).

-spec cmp(P1 :: poly(), P2 :: poly()) -> boolean().
cmp(P1, P2) ->
    erlang_tc:cmp_poly(P1, P2).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate(Samples :: samples()) -> poly().
interpolate(Samples) ->
    erlang_tc:interpolate_uni_poly(Samples).

%% NOTE: only works if the number of samples is `degree + 1` minimum
-spec interpolate_from_fr(Samples :: fr_samples()) -> poly().
interpolate_from_fr(Samples) ->
    erlang_tc:interpolate_uni_poly_from_fr(Samples).

-spec degree(Poly :: poly()) -> non_neg_integer().
degree(Poly) ->
    erlang_tc:degree_poly(Poly).

-spec gen_monomial(Degree :: non_neg_integer()) -> poly().
gen_monomial(Degree) ->
    erlang_tc:gen_monomial(Degree).

-spec random(Degree :: non_neg_integer()) -> poly().
random(Degree) ->
    erlang_tc:random_poly(Degree).

-spec constant(C :: number()) -> poly().
constant(C) ->
    erlang_tc:constant_poly(C).

-spec zero() -> poly().
zero() ->
    erlang_tc:zero_poly().

-spec zeroize(P :: poly()) -> poly().
zeroize(P) ->
    erlang_tc:zeroize_poly(P).

-spec is_zero(P :: poly()) -> boolean().
is_zero(P) ->
    erlang_tc:is_zero_poly(P).

-spec add(P1 :: poly(), P2 :: poly()) -> poly().
add(P1, P2) ->
    erlang_tc:add_poly(P1, P2).

-spec sub(P1 :: poly(), P2 :: poly()) -> poly().
sub(P1, P2) ->
    erlang_tc:sub_poly(P1, P2).

-spec mul(P1 :: poly(), P2 :: poly()) -> poly().
mul(P1, P2) ->
    erlang_tc:mul_poly(P1, P2).

-spec add_scalar(Scalar :: number(), P :: poly()) -> poly().
add_scalar(Scalar, P) ->
    case Scalar < 0 of
        false ->
            erlang_tc:add_scalar_poly(Scalar, P);
        true ->
            sub_scalar(Scalar, P)
    end.

-spec sub_scalar(Scalar :: number(), P :: poly()) -> poly().
sub_scalar(Scalar, P) ->
    erlang_tc:sub_scalar_poly(Scalar, P).

-spec mul_scalar(Scalar :: number(), P :: poly()) -> poly().
mul_scalar(Scalar, P) ->
    erlang_tc:mul_scalar_poly(Scalar, P).

-spec reveal(P :: poly()) -> string().
reveal(P) ->
    erlang_tc:reveal_poly(P).

-spec commitment(P :: poly()) -> erlang_tc_commitment:commitment().
commitment(P) ->
    erlang_tc:commitment_poly(P).
