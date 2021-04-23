-module(poly_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        eval_test/1,
        from_fr_test/1,
        interpolate_with_fr_test/1,
        zeroize_test/1,
        self_subtract_test/1,
        add_zero_test/1,
        sub_zero_test/1,
        mul_poly_test/1,
        add_different_sizes_poly_test/1,
        negative_cmp_test/1,
        f_of_x_test/1,
        serde_test/1
    ]
).

all() ->
    [
        eval_test,
        from_fr_test,
        interpolate_with_fr_test,
        zeroize_test,
        self_subtract_test,
        add_zero_test,
        sub_zero_test,
        mul_poly_test,
        add_different_sizes_poly_test,
        negative_cmp_test,
        f_of_x_test,
        serde_test
    ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

eval_test(_Config) ->
    %% poly = 5x³ + x - 2.
    Poly = tc_poly:from_coeffs([-2, 1, 0, 5]),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],

    %% check f(a) = b
    ?assert(
        lists:all(
            fun({Point, Answer}) ->
                AnswerFr = tc_fr:into(Answer),
                EvalFr = tc_poly:eval(Poly, Point),
                tc_fr:cmp(AnswerFr, EvalFr)
            end,
            Samples
        )
    ),

    %% poly can be interpolated because num_sample >= degree + 1
    ?assert(tc_poly:cmp(Poly, tc_poly:interpolate(Samples))),

    ?assertEqual(3, tc_poly:degree(Poly)),

    ok.

from_fr_test(_Config) ->
    %% poly = 5x³ + x - 2.
    Coeffs = [-2, 1, 0, 5],
    Frs = [tc_fr:into(I) || I <- Coeffs],

    Poly = tc_poly:from_frs(Frs),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],

    %% check f(a) = b
    ?assert(
        lists:all(
            fun({Point, Answer}) ->
                AnswerFr = tc_fr:into(Answer),
                EvalFr = tc_poly:eval(Poly, Point),
                tc_fr:cmp(AnswerFr, EvalFr)
            end,
            Samples
        )
    ),

    %% poly can be interpolated because num_sample >= degree + 1
    ?assert(tc_poly:cmp(Poly, tc_poly:interpolate(Samples))),

    ?assertEqual(3, tc_poly:degree(Poly)),

    ok.

interpolate_with_fr_test(_Config) ->
    %% poly = 5x³ + x - 2.
    Poly = tc_poly:from_coeffs([-2, 1, 0, 5]),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],
    FrSamples = [{tc_fr:into(A), tc_fr:into(B)} || {A, B} <- Samples],

    %% check f(a) = b
    ?assert(
        lists:all(
            fun({Point, Answer}) ->
                AnswerFr = tc_fr:into(Answer),
                EvalFr = tc_poly:eval(Poly, Point),
                tc_fr:cmp(AnswerFr, EvalFr)
            end,
            Samples
        )
    ),

    %% poly can be interpolated because num_sample >= degree + 1
    ?assert(tc_poly:cmp(Poly, tc_poly:interpolate(Samples))),

    %% we should also be able to interpolate poly using fr values of samples
    ?assert(tc_poly:cmp(Poly, tc_poly:interpolate_from_fr(FrSamples))),

    ?assertEqual(3, tc_poly:degree(Poly)),

    ok.

zeroize_test(_Config) ->
    %% random_poly -> zeroize -> is_zero
    ?assert(tc_poly:is_zero(tc_poly:zeroize(tc_poly:random(4)))),

    BiPoly = tc_bipoly:random(3),
    BiCommitment = tc_bipoly:commitment(BiPoly),

    ZeroBiPoly = tc_bipoly:zeroize(BiPoly),
    ZeroBiCommitment = tc_bipoly:commitment(ZeroBiPoly),

    ?assertEqual(false, tc_bicommitment:cmp(ZeroBiCommitment, BiCommitment)),

    ?assert(
        tc_g1:cmp(
            tc_g1:zero(),
            tc_bicommitment:eval(
                ZeroBiCommitment,
                rand:uniform(100),
                rand:uniform(100)
            )
        )
    ),

    ok.

self_subtract_test(_Config) ->
    %% f(x) - f(x) = 0
    P = tc_poly:random(2),
    ?assert(tc_poly:cmp(tc_poly:zero(), tc_poly:sub(P, P))).

add_zero_test(_Config) ->
    %% f(x) + 0 = f(x)
    P = tc_poly:random(2),
    ?assert(tc_poly:cmp(P, tc_poly:add_scalar(0, P))).

sub_zero_test(_Config) ->
    %% f(x) - 0 = f(x)
    P = tc_poly:random(2),
    ?assert(tc_poly:cmp(P, tc_poly:sub_scalar(0, P))).

mul_poly_test(_Config) ->
    %% p1 = (x² + 1)
    %% p2 = (x - 1)
    %% p1 * p2 = p3 =  x³ - x² + x - 1
    %% p1(p) * p2(p) = p3(p)

    P1 = tc_poly:from_coeffs([1, 0, 1]),
    P2 = tc_poly:from_coeffs([-1, 1]),
    P3 = tc_poly:from_coeffs([-1, 1, -1, 1]),

    ?assert(tc_poly:cmp(P3, tc_poly:mul(P1, P2))),

    P1Eval = tc_poly:eval(P1, 5),
    P2Eval = tc_poly:eval(P2, 5),
    P3Eval = tc_poly:eval(P3, 5),

    ?assert(tc_fr:cmp(tc_fr:into(26), P1Eval)),
    ?assert(tc_fr:cmp(tc_fr:into(4), P2Eval)),
    ?assert(tc_fr:cmp(tc_fr:into(104), P3Eval)),

    ok.

add_different_sizes_poly_test(_Config) ->
    P1 = tc_poly:random(5),
    P2 = tc_poly:random(8),

    AddedPoly = tc_poly:add(P1, P2),

    %% result should be of degree 8
    ?assertEqual(8, tc_poly:degree(AddedPoly)),

    %% if we subtract B from the result, we should get back A with degree 5
    SubPoly = tc_poly:sub(AddedPoly, P2),

    ?assertEqual(5, tc_poly:degree(SubPoly)),

    ?assert(tc_poly:cmp(P1, SubPoly)),
    ok.

negative_cmp_test(_Config) ->
    P1 = tc_poly:random(5),
    P2 = tc_poly:add(P1, P1),
    %% since P1 /= 2*P1
    ?assertEqual(false, tc_poly:cmp(P1, P2)),
    ok.

f_of_x_test(_Config) ->
    %% f(x) = 5x², f(2) = 5 * 2 * 2
    P = tc_poly:from_coeffs([0, 0, 5]),
    Eval = tc_poly:eval(P, 2),
    ?assert(tc_fr:cmp(tc_fr:into(5 * 2 * 2), Eval)),
    ok.

serde_test(_Config) ->
    %% f(x) = 5x², f(2) = 5 * 2 * 2
    P = tc_poly:from_coeffs([0, 0, 5]),
    SerializedPoly = tc_poly:serialize(P),
    DeserializePoly = tc_poly:deserialize(SerializedPoly),
    ?assert(tc_fr:cmp(tc_fr:into(5 * 2 * 2), tc_poly:eval(DeserializePoly, 2))),
    ok.
