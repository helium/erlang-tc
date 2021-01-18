-module(bipoly_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([generate_with_constant_term_test/1]).

all() -> [generate_with_constant_term_test].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

generate_with_constant_term_test(_Config) ->
    Secret = 42,
    Degree = 3,
    BiPoly = erlang_tc_bipoly:with_secret(Secret, Degree),
    Eval = erlang_tc_bipoly:eval(BiPoly, 0, 0),

    %% Evaluating the bipoly at (0,0) implies we only get the constant term,
    %% which is the secret value we constructed the bipoly to begin with
    SecretFr = erlang_tc_fr:into(Secret),
    ?assert(erlang_tc_fr:cmp(Eval, SecretFr)),

    ok.
