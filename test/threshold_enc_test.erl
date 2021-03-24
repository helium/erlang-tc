%% Threshold Encryption - Demonstrates how to encrypt a
%% message to a group of actors with a master public-key, where the number of
%% actors collaborating in the decryption process must exceed a given threshold
%% number before the ciphertext can be successfully decrypted. This example also
%% demonstrates the idea of a "trusted dealer", i.e. some trusted entity that is
%% responsible for generating the keys.

-module(threshold_enc_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Create a SecretSociety with 3 actors. Any message encrypted with the society's public-key
    %% will require 2 or more actors working together to decrypt (i.e. the decryption threshold is
    %% 1). Once the secret society has created its master keys, it "deals" a secret-key share and
    %% public-key share to each of its actors. The secret society then publishes its public key
    %% to a publicly accessible key-server.

    SecretSociety = secret_society:new_secret_society(3, 1),
    %% ?debugFmt("SecretSociety: ~p", [SecretSociety]),

    PK = secret_society:publish_public_key(SecretSociety),
    %% ?debugFmt("PublicKey: ~p", [PK]),

    %% Create a named alias for each actor in the secret society.
    Alice = secret_society:get_actor(SecretSociety, 1),
    Bob = secret_society:get_actor(SecretSociety, 2),
    Clara = secret_society:get_actor(SecretSociety, 3),

    %% I, the society's benevolent hacker, want to send an important message to each of my
    %% comrades. I encrypt my message with the society's public-key. I then send the ciphertext to
    %% each of the society's actors.
    Msg = <<"May the force be with you">>,
    Ciphertext = pubkey:encrypt(PK, Msg),
    NewAlice = secret_society:send_msg(Alice, Ciphertext),
    NewBob = secret_society:send_msg(Bob, Ciphertext),
    NewClara = secret_society:send_msg(Clara, Ciphertext),

    %% We start a meeting of the secret society. At the meeting, each actor contributes their
    %% share of the decryption process to decrypt the ciphertext that they each received.
    DecryptionMeeting = secret_society:start_decryption_meeting(SecretSociety),

    %% Alice is the first actor to arrive at the meeting, she provides her decryption share. One
    %% actor alone cannot decrypt the ciphertext, decryption fails.
    DecryptionMeeting1 = secret_society:accept_decryption_share(DecryptionMeeting, NewAlice),
    ?assertEqual({error, cannot_decrypt}, secret_society:decrypt_msg(DecryptionMeeting1)),

    %% Bob joins the meeting and provides his decryption share. Alice and Bob are now collaborating
    %% to decrypt the ciphertext, they succeed because the society requires two or more actors for
    %% decryption.
    DecryptionMeeting2 = secret_society:accept_decryption_share(DecryptionMeeting1, NewBob),
    {ok, DecryptedMsg1} = secret_society:decrypt_msg(DecryptionMeeting2),
    ?assertEqual(DecryptedMsg1, Msg),

    DecryptionMeeting3 = secret_society:accept_decryption_share(DecryptionMeeting1, NewClara),
    {ok, DecryptedMsg2} = secret_society:decrypt_msg(DecryptionMeeting3),
    ?assertEqual(DecryptedMsg2, Msg),

    ok.
