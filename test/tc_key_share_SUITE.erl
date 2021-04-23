-module(tc_key_share_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        n_three_thresold_one/1,
        n_six_thresold_two/1
    ]
).

all() ->
    [
        n_three_thresold_one,
        n_six_thresold_two
    ].

init_per_testcase(TestCase, Config) ->
    {N, T} =
        case TestCase of
            n_three_thresold_one -> {3, 1};
            n_six_thresold_two -> {6, 2}
        end,
    [{n, N}, {t, T} | Config].

end_per_testcase(_, Config) ->
    Config.

n_three_thresold_one(Config) ->
    run(Config).

n_six_thresold_two(Config) ->
    run(Config).

%% Helpers
run(Config) ->
    N = ?config(n, Config),
    T = ?config(t, Config),
    SecretKeyShares = tc_key_share:deal(N, T),

    AKey = hd(SecretKeyShares),
    MessageToSign = crypto:hash(sha256, crypto:strong_rand_bytes(12)),
    Signatures = [tc_key_share:sign_share(KeyShare, MessageToSign) || KeyShare <- SecretKeyShares],
    ct:pal("~p", [
        [tc_key_share:verify_signature_share(AKey, Share, MessageToSign) || Share <- Signatures]
    ]),
    ?assert(
        lists:all(fun(X) -> X end, [
            tc_key_share:verify_signature_share(AKey, Share, MessageToSign)
         || Share <- Signatures
        ])
    ),
    {ok, CombinedSignature} = tc_key_share:combine_signature_shares(AKey, Signatures),
    ?assert(tc_key_share:verify(AKey, CombinedSignature, MessageToSign)),

    Message = crypto:hash(sha256, <<"my hovercraft is full of eels">>),
    CipherText = tc_key_share:encrypt(AKey, Message),
    true = tc_ciphertext:verify(CipherText),
    DecShares = [tc_key_share:decrypt_share(KeyShare, CipherText) || KeyShare <- SecretKeyShares],
    ?assert(
        lists:all(fun(E) -> E end, [
            tc_key_share:verify_decryption_share(AKey, DecShare, CipherText)
         || DecShare <- DecShares
        ])
    ),
    ?assertEqual(
        {ok, Message},
        tc_key_share:combine_decryption_shares(
            AKey,
            [S || S <- random_n(T + 1, DecShares)],
            CipherText
        )
    ),
    ok.

random_n(N, List) ->
    lists:sublist(shuffle(List), N).

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].
