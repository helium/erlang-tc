%% Threshold Encryption - Demonstrates how to encrypt a
%% message to a group of actors with a master public-key, where the number of
%% actors collaborating in the decryption process must exceed a given threshold
%% number before the ciphertext can be successfully decrypted. This example also
%% demonstrates the idea of a "trusted dealer", i.e. some trusted entity that is
%% responsible for generating the keys.

-module(threshold_enc_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    SecretSociety = secret_society:new_secret_society(3, 1),
    ?debugFmt("SecretSociety: ~p", [SecretSociety]),

    A1 = secret_society:get_actor(SecretSociety, 1),
    ?debugFmt("A1: ~p", [A1]),

    ok.
