-module(poly_test).

-include_lib("eunit/include/eunit.hrl").

eval_test() ->
    %% #[test]
    %% fn poly() {
    %%     // The polynomial 5 X³ + X - 2.
    %%     let x_pow_3 = Poly::monomial(3);
    %%     let x_pow_1 = Poly::monomial(1);
    %%     let poly = x_pow_3 * 5 + x_pow_1 - 2;

    %%     let coeff: Vec<_> = [-2, 1, 0, 5].iter().map(IntoFr::into_fr).collect();
    %%     assert_eq!(Poly { coeff }, poly);
    %%     let samples = vec![(-1, -8), (2, 40), (3, 136), (5, 628)];
    %%     for &(x, y) in &samples {
    %%         assert_eq!(y.into_fr(), poly.evaluate(x));
    %%     }
    %%     let interp = Poly::interpolate(samples);
    %%     assert_eq!(interp, poly);
    %% }

    Poly = erlang_tc:poly_from_coeffs([-2, 1, 0, 5]),

    Samples = [{-1, -8}, {2, 40}, {3, 136}, {5, 628}],

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

    ?assert(erlang_tc:cmp_poly(Poly, erlang_tc:interpolate_uni_poly(Samples))),

    ok.
