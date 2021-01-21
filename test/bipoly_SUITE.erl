-module(bipoly_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

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
    BiPoly = bipoly:with_secret(Secret, Degree),
    Eval = bipoly:eval(BiPoly, 0, 0),

    %% Evaluating the bipoly at (0,0) implies we only get the constant term,
    %% which is the secret value we constructed the bipoly to begin with
    SecretFr = fr:into(Secret),
    ?assert(fr:cmp(Eval, SecretFr)),

    ok.
