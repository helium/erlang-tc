-module(pkc_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Alice and Bob each generate a public/private key-pair.
    %% NOTE: it is against best practices to use the same key-pair for both encryption/decryption
    %% and signing. The following example could be interpreted as advocating this, which it is not
    %% meant to. This is just a basic example. In this example, Bob's key-pair is used for signing
    %% where as Alice's is used for encryption/decryption.
    AliceSK = erlang_tc_sk:random(),
    AlicePK = erlang_tc_sk:public_key(AliceSK),
    BobSK = erlang_tc_sk:random(),
    BobPK = erlang_tc_sk:public_key(BobSK),

    %% Bob wants to send Alice a message. He signs the plaintext message with his secret key. He
    %% then encrypts the signed message with Alice's public key.
    Msg = <<"Frankly, my dear, I don't give a damn">>,
    Sig = erlang_tc_sk:sign(BobSK, Msg),
    SignedMsg = #{msg => Msg, signature => Sig},
    %% TODO: Test with actual serialization
    Cipher = erlang_tc_pk:encrypt(AlicePK, term_to_binary(SignedMsg)),
    %% Check that this is a valid cipher
    ?assert(erlang_tc_ciphertext:verify(Cipher)),

    %% Alice receives Bob's encrypted message. She decrypts the message using her secret key. She
    %% then verifies that the signature of the plaintext is valid using Bob's public key.
    Decrypted = erlang_tc_sk:decrypt(AliceSK, Cipher),
    DecryptedSignedMsg = binary_to_term(Decrypted),
    ReceivedSignature = maps:get(signature, DecryptedSignedMsg),
    ReceivedMsg = maps:get(msg, DecryptedSignedMsg),
    ?assertEqual(SignedMsg, binary_to_term(Decrypted)),
    ?assert(erlang_tc_pk:verify(BobPK, ReceivedSignature, ReceivedMsg)),

    %% We assert that the message that Alice received is the same message that Bob sent.
    ?assertEqual(ReceivedMsg, Msg),

    ok.
