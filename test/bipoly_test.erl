-module(bipoly_test).

-include_lib("eunit/include/eunit.hrl").

with_secret_test() ->
    Secret = 42,
    Degree = 3,
    BiPoly = erlang_tc_bipoly:with_secret(Secret, Degree),
    Eval = erlang_tc_bipoly:eval(BiPoly, 0, 0),

    %% Evaluating the bipoly at (0,0) implies we only get the constant term,
    %% which is the secret value we constructed the bipoly to begin with
    SecretFr = erlang_tc_fr:into(Secret),
    ?assert(erlang_tc_fr:cmp(Eval, SecretFr)),

    ok.
