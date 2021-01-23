-module(bicommitment_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    verify_poly_test/1,
    verify_point_test/1,
    serde_verify_poly_test/1,
    serde_verify_point_test/1
]).

all() ->
    [
        verify_poly_test,
        verify_point_test,
        serde_verify_poly_test,
        serde_verify_point_test
    ].

init_per_testcase(_, Config) ->
    [{secret, 42}, {degree, 4} | Config].

end_per_testcase(_, Config) ->
    Config.

verify_poly_test(Config) ->
    Secret = ?config(secret, Config),
    Degree = ?config(degree, Config),
    BiPoly = bipoly:with_secret(Secret, Degree),

    BiCommitment = bipoly:commitment(BiPoly),

    RowPolys = [{ID, bipoly:row(BiPoly, ID)} || ID <- lists:seq(1, Degree)],

    ?assert(
        lists:all(
            fun({ID, Poly}) ->
                bicommitment:verify_poly(BiCommitment, Poly, ID)
            end,
            RowPolys
        )
    ).

verify_point_test(Config) ->
    Secret = ?config(secret, Config),
    Degree = ?config(degree, Config),
    BiPoly = bipoly:with_secret(Secret, Degree),

    BiCommitment = bipoly:commitment(BiPoly),
    RowPolys = [{ID, bipoly:row(BiPoly, ID)} || ID <- lists:seq(1, Degree)],
    Res = lists:map(
        fun({SenderID, Poly}) ->
            case bicommitment:verify_poly(BiCommitment, Poly, SenderID) of
                true ->
                    %% verify_poly succeeded, check verify_point for verifiers
                    lists:map(
                        fun({VerifierID, Poly2}) ->
                            bicommitment:verify_point(BiCommitment, Poly2, SenderID, VerifierID)
                        end,
                        RowPolys
                    );
                false ->
                    false
            end
        end,
        RowPolys
    ),
    ct:pal("Res: ~p~n", [Res]),
    ?assert(lists:all(fun(X) -> X end, lists:flatten(Res))),
    ok.

serde_verify_poly_test(Config) ->
    Secret = ?config(secret, Config),
    Degree = ?config(degree, Config),
    BiPoly = bipoly:with_secret(Secret, Degree),

    BiCommitment0 = bipoly:commitment(BiPoly),

    SerializedBiCommitment = bicommitment:serialize(BiCommitment0),
    BiCommitment = bicommitment:deserialize(SerializedBiCommitment),

    RowPolys = [{ID, bipoly:row(BiPoly, ID)} || ID <- lists:seq(1, Degree)],

    ?assert(
        lists:all(
            fun({ID, Poly}) ->
                bicommitment:verify_poly(BiCommitment, Poly, ID)
            end,
            RowPolys
        )
    ).

serde_verify_point_test(Config) ->
    Secret = ?config(secret, Config),
    Degree = ?config(degree, Config),
    BiPoly = bipoly:with_secret(Secret, Degree),

    BiCommitment0 = bipoly:commitment(BiPoly),

    SerializedBiCommitment = bicommitment:serialize(BiCommitment0),
    BiCommitment = bicommitment:deserialize(SerializedBiCommitment),

    RowPolys = [{ID, bipoly:row(BiPoly, ID)} || ID <- lists:seq(1, Degree)],
    Res = lists:map(
        fun({SenderID, Poly}) ->
            case bicommitment:verify_poly(BiCommitment, Poly, SenderID) of
                true ->
                    %% verify_poly succeeded, check verify_point for verifiers
                    lists:map(
                        fun({VerifierID, Poly2}) ->
                            bicommitment:verify_point(BiCommitment, Poly2, SenderID, VerifierID)
                        end,
                        RowPolys
                    );
                false ->
                    false
            end
        end,
        RowPolys
    ),
    ct:pal("Res: ~p~n", [Res]),
    ?assert(lists:all(fun(X) -> X end, lists:flatten(Res))),
    ok.
