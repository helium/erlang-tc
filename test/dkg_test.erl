-module(dkg_test).

-include_lib("eunit/include/eunit.hrl").

rust_example_test() ->
    DealerNum = 3,
    NodeNum = 5,
    FaultyNum = 2,

    %% For distributed key generation, a number of dealers, only one of who needs to be honest,
    %% generates random bivariate polynomials and publicly commits to them. In practice, the
    %% dealers can e.g. be any `faulty_num + 1` nodes.
    BiPolys = [erlang_tc_bipoly:random(FaultyNum) || _ <- lists:seq(0, DealerNum)],
    BiCommitments = [erlang_tc_bipoly:commitment(BiPoly) || BiPoly <- BiPolys],
    SecretKeys0 = [erlang_tc_fr:zero() || _ <- lists:seq(1, NodeNum)],

    %% Each dealer sends row `m` to node `m`, where the index starts at `1`. Don't send row `0`
    %% to anyone! The nodes verify their rows, and send _value_ `s` on to node `s`. They again
    %% verify the values they received, and collect them.
    SecretKeys = lists:foldl(
        fun({BiPoly, BiCommitment}, Acc0) ->
            lists:foldl(
                fun(M, Acc1) ->
                    %% Node `m` receives its row and verifies it.
                    RowPoly = erlang_tc_bipoly:row(BiPoly, M),
                    RowCommit = erlang_tc_bicommitment:row(BiCommitment, M),
                    ?assert(erlang_tc_bicommitment:verify_poly(BiCommitment, RowPoly, M)),

                    %% Node `s` receives the `s`-th value and verifies it.
                    lists:foreach(
                        fun(S) ->
                            Val = erlang_tc_poly:eval(RowPoly, S),
                            ?assert(erlang_tc_bicommitment:verify_point(BiCommitment, RowPoly, S, M)),
                            %% The node can't verify this directly, but it should have the correct value:
                            ?assert(erlang_tc_fr:cmp(erlang_tc_bipoly:eval(BiPoly, M, S), Val))
                        end,
                        lists:seq(1, NodeNum)
                    ),

                    %% A cheating dealer who modified the polynomial would be detected.
                    WrongPoly = erlang_tc_poly:add(RowPoly, erlang_tc_poly:from_coeffs([0, 0, 5])),
                    ?assertEqual(
                        false,
                        erlang_tc_commitment:cmp(erlang_tc_poly:commitment(WrongPoly), RowCommit)
                    ),

                    %% If `2 * faulty_num + 1` nodes confirm that they received a valid row, then at
                    %% least `faulty_num + 1` honest ones did, and sent the correct values on to node
                    %% `s`. So every node received at least `faulty_num + 1` correct entries of their
                    %% column/row (remember that the bivariate polynomial is symmetric). They can
                    %% reconstruct the full row and in particular value `0` (which no other node knows,
                    %% only the dealer). E.g. let's say nodes `1`, `2` and `4` are honest. Then node
                    %% `m` received three correct entries from that row:
                    Received = [
                        {erlang_tc_fr:into(I), erlang_tc_bipoly:eval(BiPoly, M, I)}
                        || I <- [1, 2, 4]
                    ],
                    MyRow = erlang_tc_poly:interpolate_from_fr(Received),
                    ?assert(
                        erlang_tc_fr:cmp(
                            erlang_tc_bipoly:eval(BiPoly, M, 0),
                            erlang_tc_poly:eval(RowPoly, 0)
                        )
                    ),
                    ?assert(erlang_tc_poly:cmp(RowPoly, MyRow)),

                    %% The node sums up all values number `0` it received from the different dealer. No
                    %% dealer and no other node knows the sum in the end.
                    Secret_M_minus_One = lists:nth(M, Acc1),
                    ToSet = erlang_tc_fr:add_assign(
                        Secret_M_minus_One,
                        erlang_tc_poly:eval_from_fr(MyRow, erlang_tc_fr:zero())
                    ),
                    setnth(M, Acc1, ToSet)
                end,
                Acc0,
                lists:seq(1, NodeNum)
            )
        end,
        SecretKeys0,
        lists:zip(BiPolys, BiCommitments)
    ),

    %% Each node now adds up all the first values of the rows it received from the different
    %% dealers (excluding the dealers where fewer than `2 * faulty_num + 1` nodes confirmed).
    %% The whole first column never gets added up in practice, because nobody has all the
    %% information. We do it anyway here; entry `0` is the secret key that is not known to
    %% anyone, neither a dealer, nor a node:
    SecretKeySet0 = erlang_tc_poly:zero(),
    SecretKeySet = lists:foldl(fun(BiPoly, Acc) ->
                        erlang_tc_poly:add(Acc, erlang_tc_bipoly:row(BiPoly, 0))
                end, SecretKeySet0, BiPolys),

    lists:foreach(fun(M) ->
                          ?assert(erlang_tc_fr:cmp(erlang_tc_poly:eval(SecretKeySet, M), lists:nth(M, SecretKeys)))
                  end, lists:seq(1, NodeNum)),

    %% The sum of the first rows of the public commitments is the commitment to the secret key
    %% set.
    SumCommit0 = erlang_tc_poly:commitment(erlang_tc_poly:zero()),
    SumCommit = lists:foldl(fun(BiCommitment, Acc) ->
                                    erlang_tc_commitment:add(Acc, erlang_tc_bicommitment:row(BiCommitment, 0))
                            end, SumCommit0, BiCommitments),

    ?assert(erlang_tc_commitment:cmp(SumCommit, erlang_tc_poly:commitment(SecretKeySet))),

    ok.

setnth(1, [_ | Rest], New) -> [New | Rest];
setnth(I, [E | Rest], New) -> [E | setnth(I - 1, Rest, New)].
