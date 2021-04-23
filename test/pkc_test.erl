%% Public-Key Cryptography - Demonstrates how to generate a
%% random secret-key and corresponding public-key, sign some bytes using a
%% secret-key, validate the signature for some bytes using a public-key, encrypt
%% some bytes using a public-key, and how to decrypt a ciphertext using a
%% secret-key.

-module(pkc_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Alice and Bob each generate a public/private key-pair.
    %% NOTE: it is against best practices to use the same key-pair for both encryption/decryption
    %% and signing. The following example could be interpreted as advocating this, which it is not
    %% meant to. This is just a basic example. In this example, Bob's key-pair is used for signing
    %% where as Alice's is used for encryption/decryption.
    AliceSK = tc_secret_key:random(),
    AlicePK = tc_secret_key:public_key(AliceSK),
    BobSK = tc_secret_key:random(),
    BobPK = tc_secret_key:public_key(BobSK),

    %% Bob wants to send Alice a message. He signs the plaintext message with his secret key. He
    %% then encrypts the signed message with Alice's public key.
    Msg = <<"Frankly, my dear, I don't give a damn">>,
    Sig = tc_secret_key:sign(BobSK, Msg),
    SignedMsg = {Msg, Sig},
    %% TODO: Test with actual serialization
    Cipher = tc_pubkey:encrypt(AlicePK, term_to_binary(SignedMsg)),
    %% Check that this is a valid cipher
    ?assert(tc_ciphertext:verify(Cipher)),

    %% Alice receives Bob's encrypted message. She decrypts the message using her secret key. She
    %% then verifies that the signature of the plaintext is valid using Bob's public key.
    Decrypted = tc_secret_key:decrypt(AliceSK, Cipher),
    DecryptedSignedMsg = binary_to_term(Decrypted),
    {ReceivedMsg, ReceivedSignature} = DecryptedSignedMsg,
    ?assertEqual(SignedMsg, binary_to_term(Decrypted)),
    ?assert(tc_pubkey:verify(BobPK, ReceivedSignature, ReceivedMsg)),

    %% We assert that the message that Alice received is the same message that Bob sent.
    ?assertEqual(ReceivedMsg, Msg),

    ok.
