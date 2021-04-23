-module(secret_key_match_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_secret_key_match/0]).

prop_secret_key_match() ->
    ?FORALL(
        {Secret, Degree},
        {gen_secret(), gen_degree()},
        begin
            BiPoly = tc_bipoly:with_secret(Secret, Degree),
            EvaluatedSecret = tc_bipoly:eval(BiPoly, 0, 0),
            ?WHENFAIL(
                begin
                    io:format("Secret ~p~n", [Secret]),
                    io:format("EvaluatedSecret ~p~n", [EvaluatedSecret])
                end,
                conjunction([
                    {secret_equality, tc_fr:cmp(tc_fr:into(Secret), EvaluatedSecret)}
                ])
            )
        end
    ).

gen_degree() ->
    ?SUCHTHAT(L, int(), L > 0).

gen_secret() ->
    ?SUCHTHAT(L, int(), L > 0).
