-module(poly_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        eval_test/1,
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
    %% poly = x³ + x - 2.
    Poly = poly:from_coeffs([-2, 1, 0, 5]),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],

    %% check f(a) = b
    ?assert(
        lists:all(
            fun({Point, Answer}) ->
                AnswerFr = fr:into(Answer),
                EvalFr = poly:eval(Poly, Point),
                fr:cmp(AnswerFr, EvalFr)
            end,
            Samples
        )
    ),

    %% poly can be interpolated because num_sample >= degree + 1
    ?assert(poly:cmp(Poly, poly:interpolate(Samples))),

    ?assertEqual(3, poly:degree(Poly)),

    ok.

interpolate_with_fr_test(_Config) ->
    %% poly = x³ + x - 2.
    Poly = poly:from_coeffs([-2, 1, 0, 5]),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],
    FrSamples = [{fr:into(A), fr:into(B)} || {A, B} <- Samples],

    %% check f(a) = b
    ?assert(
        lists:all(
            fun({Point, Answer}) ->
                AnswerFr = fr:into(Answer),
                EvalFr = poly:eval(Poly, Point),
                fr:cmp(AnswerFr, EvalFr)
            end,
            Samples
        )
    ),

    %% poly can be interpolated because num_sample >= degree + 1
    ?assert(poly:cmp(Poly, poly:interpolate(Samples))),

    %% we should also be able to interpolate poly using fr values of samples
    ?assert(poly:cmp(Poly, poly:interpolate_from_fr(FrSamples))),

    ?assertEqual(3, poly:degree(Poly)),

    ok.

zeroize_test(_Config) ->
    %% random_poly -> zeroize -> is_zero
    ?assert(poly:is_zero(poly:zeroize(poly:random(4)))),

    BiPoly = bipoly:random(3),
    BiCommitment = bipoly:commitment(BiPoly),

    ZeroBiPoly = bipoly:zeroize(BiPoly),
    ZeroBiCommitment = bipoly:commitment(ZeroBiPoly),

    ?assertEqual(false, bicommitment:cmp(ZeroBiCommitment, BiCommitment)),

    ?assert(
        g1:cmp(
            g1:zero(),
            bicommitment:eval(
                ZeroBiCommitment,
                rand:uniform(100),
                rand:uniform(100)
            )
        )
    ),

    ok.

self_subtract_test(_Config) ->
    %% f(x) - f(x) = 0
    P = poly:random(2),
    ?assert(poly:cmp(poly:zero(), poly:sub(P, P))).

add_zero_test(_Config) ->
    %% f(x) + 0 = f(x)
    P = poly:random(2),
    ?assert(poly:cmp(P, poly:add_scalar(0, P))).

sub_zero_test(_Config) ->
    %% f(x) - 0 = f(x)
    P = poly:random(2),
    ?assert(poly:cmp(P, poly:sub_scalar(0, P))).

mul_poly_test(_Config) ->
    %% p1 = (x² + 1)
    %% p2 = (x - 1)
    %% p1 * p2 = p3 =  x³ - x² + x - 1
    %% p1(p) * p2(p) = p3(p)

    P1 = poly:from_coeffs([1, 0, 1]),
    P2 = poly:from_coeffs([-1, 1]),
    P3 = poly:from_coeffs([-1, 1, -1, 1]),

    ?assert(poly:cmp(P3, poly:mul(P1, P2))),

    P1Eval = poly:eval(P1, 5),
    P2Eval = poly:eval(P2, 5),
    P3Eval = poly:eval(P3, 5),

    ?assert(fr:cmp(fr:into(26), P1Eval)),
    ?assert(fr:cmp(fr:into(4), P2Eval)),
    ?assert(fr:cmp(fr:into(104), P3Eval)),

    ok.

add_different_sizes_poly_test(_Config) ->
    P1 = poly:random(5),
    P2 = poly:random(8),

    AddedPoly = poly:add(P1, P2),

    %% result should be of degree 8
    ?assertEqual(8, poly:degree(AddedPoly)),

    %% if we subtract B from the result, we should get back A with degree 5
    SubPoly = poly:sub(AddedPoly, P2),

    ?assertEqual(5, poly:degree(SubPoly)),

    ?assert(poly:cmp(P1, SubPoly)),
    ok.

negative_cmp_test(_Config) ->
    P1 = poly:random(5),
    P2 = poly:add(P1, P1),
    %% since P1 /= 2*P1
    ?assertEqual(false, poly:cmp(P1, P2)),
    ok.

f_of_x_test(_Config) ->
    %% f(x) = 5x², f(2) = 5 * 2 * 2
    P = poly:from_coeffs([0, 0, 5]),
    Eval = poly:eval(P, 2),
    ?assert(fr:cmp(fr:into(5 * 2 * 2), Eval)),
    ok.

serde_test(_Config) ->
    %% f(x) = 5x², f(2) = 5 * 2 * 2
    P = poly:from_coeffs([0, 0, 5]),
    SerializedPoly = poly:serialize(P),
    DeserializePoly = poly:deserialize(SerializedPoly),
    ?assert(fr:cmp(fr:into(5 * 2 * 2), poly:eval(DeserializePoly, 2))),
    ok.
