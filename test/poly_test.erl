-module(poly_test).

-include_lib("eunit/include/eunit.hrl").

eval_test() ->
    %% poly = x³ + x - 2.
    Poly = erlang_tc:poly_from_coeffs([-2, 1, 0, 5]),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],

    %% check f(a) = b
    ?assert(
        lists:all(
            fun({Point, Answer}) ->
                AnswerFr = erlang_tc:into_fr(Answer),
                EvalFr = erlang_tc:eval_uni_poly(Poly, Point),
                erlang_tc:cmp_fr(AnswerFr, EvalFr)
            end,
            Samples
        )
    ),

    %% poly can be interpolated because num_sample >= degree + 1
    ?assert(erlang_tc:cmp_poly(Poly, erlang_tc:interpolate_uni_poly(Samples))),

    ?assertEqual(3, erlang_tc:degree_poly(Poly)),

    ok.

zeorize_test() ->
    %% random_poly -> zeroize -> is_zero
    ?assert(erlang_tc:is_zero_poly(erlang_tc:zeroize_poly(erlang_tc:random_poly(4)))),

    BiPoly = erlang_tc:random_bivar_poly(3),
    BiCommitment = erlang_tc:commitment_bivar_poly(BiPoly),

    ZeroBiPoly = erlang_tc:zeroize_bivar_poly(BiPoly),
    ZeroBiCommitment = erlang_tc:commitment_bivar_poly(ZeroBiPoly),

    ?assertEqual(false, erlang_tc:cmp_bivar_commitment(ZeroBiCommitment, BiCommitment)),

    ?assert(
        erlang_tc:cmp_g1(
            erlang_tc:g1_zero(),
            erlang_tc:eval_bivar_commitment(
                ZeroBiCommitment,
                rand:uniform(100),
                rand:uniform(100)
            )
        )
    ),

    ok.

self_subtract_test() ->
    %% f(x) - f(x) = 0
    P = erlang_tc:random_poly(2),
    ?assert(erlang_tc:cmp_poly(erlang_tc:zero_poly(), erlang_tc:sub_poly(P, P))).

add_zero_test() ->
    %% f(x) + 0 = f(x)
    P = erlang_tc:random_poly(2),
    ?assert(erlang_tc:cmp_poly(P, erlang_tc:add_scalar_poly(0, P))).

sub_zero_test() ->
    %% f(x) - 0 = f(x)
    P = erlang_tc:random_poly(2),
    ?assert(erlang_tc:cmp_poly(P, erlang_tc:sub_scalar_poly(0, P))).

mul_poly_test() ->
    %% p1 = (x² + 1)
    %% p2 = (x - 1)
    %% p1 * p2 = p3 =  x³ - x² + x - 1
    %% p1(p) * p2(p) = p3(p)

    P1 = erlang_tc:poly_from_coeffs([1, 0, 1]),
    P2 = erlang_tc:poly_from_coeffs([-1, 1]),
    P3 = erlang_tc:poly_from_coeffs([-1, 1, -1, 1]),

    ?assert(erlang_tc:cmp_poly(P3, erlang_tc:mul_poly(P1, P2))),

    P1Eval = erlang_tc:eval_uni_poly(P1, 5),
    P2Eval = erlang_tc:eval_uni_poly(P2, 5),
    P3Eval = erlang_tc:eval_uni_poly(P3, 5),

    ?assert(erlang_tc:cmp_fr(erlang_tc:into_fr(26), P1Eval)),
    ?assert(erlang_tc:cmp_fr(erlang_tc:into_fr(4), P2Eval)),
    ?assert(erlang_tc:cmp_fr(erlang_tc:into_fr(104), P3Eval)),

    ok.

add_different_sizes_poly_test() ->
    P1 = erlang_tc:random_poly(5),
    P2 = erlang_tc:random_poly(8),

    AddedPoly = erlang_tc:add_poly(P1, P2),

    %% result should be of degree 8
    ?assertEqual(8, erlang_tc:degree_poly(AddedPoly)),

    %% if we subtract B from the result, we should get back A with degree 5
    SubPoly = erlang_tc:sub_poly(AddedPoly, P2),

    ?assertEqual(5, erlang_tc:degree_poly(SubPoly)),

    ?assert(erlang_tc:cmp_poly(P1, SubPoly)),
    ok.

negative_cmp_test() ->
    P1 = erlang_tc:random_poly(5),
    P2 = erlang_tc:add_poly(P1, P1),
    %% since P1 /= 2*P1
    ?assertEqual(false, erlang_tc:cmp_poly(P1, P2)),
    ok.

f_of_x_test() ->
    %% f(x) = 5x², f(2) = 5 * 2 * 2
    P = erlang_tc:poly_from_coeffs([0, 0, 5]),
    Eval = erlang_tc:eval_uni_poly(P, 2),
    ?assert(erlang_tc:cmp_fr(erlang_tc:into_fr(5 * 2 * 2), Eval)),
    ok.
