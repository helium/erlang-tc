-module(bipoly_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         generate_with_constant_term_test/1,
         serialize_deserialize_test/1
        ]).

all() -> [
          generate_with_constant_term_test,
          serialize_deserialize_test
         ].

init_per_testcase(_, Config) ->
    [{secret, 42}, {degree, 4} | Config].

end_per_testcase(_, Config) ->
    Config.

generate_with_constant_term_test(Config) ->
    Secret = ?config(secret, Config),
    Degree = ?config(degree, Config),
    BiPoly = tc_bipoly:with_secret(Secret, Degree),
    Eval = tc_bipoly:eval(BiPoly, 0, 0),

    %% Evaluating the bipoly at (0,0) implies we only get the constant term,
    %% which is the secret value we constructed the bipoly to begin with
    SecretFr = tc_fr:into(Secret),
    ?assert(tc_fr:cmp(Eval, SecretFr)),

    ok.

serialize_deserialize_test(Config) ->
    Secret = ?config(secret, Config),
    Degree = ?config(degree, Config),
    %% construct some bipoly
    Bipoly = tc_bipoly:with_secret(Secret, Degree),
    %% serialize
    SBipoly = tc_bipoly:serialize(Bipoly),
    %% deserialize
    DBipoly = tc_bipoly:deserialize(SBipoly),

    %% evaluate the polynomials for each row in the original bipoly
    RowPolys = [tc_bipoly:row(Bipoly, I) || I <- lists:seq(1, Degree)],
    Evals = [tc_poly:eval(R, I) || {R, I} <- lists:zip(RowPolys, lists:seq(1, Degree))],


    %% evaluate the polynomials for each row in the deserialized bipoly
    DRowPolys = [tc_bipoly:row(DBipoly, I) || I <- lists:seq(1, Degree)],
    DEvals = [tc_poly:eval(R, I) || {R, I} <- lists:zip(DRowPolys, lists:seq(1, Degree))],

    %% check that they evaluate to the same value
    Results = [tc_fr:cmp(Original, Deserialized) || {Original, Deserialized} <- lists:zip(Evals, DEvals)],
    true = lists:all(fun(R) -> R == true end, Results),

    ok.
