-module(public_key_set).

-export([
    %% PublicKeySet API
    from_commitment/1,
    public_key/1,
    threshold/1,
    public_key_share/2,
    decrypt/3,
    combine_signatures/2
]).

-type pk_set() :: reference().

-export_type([pk_set/0]).

-spec from_commitment(Commitment :: commitment:commitment()) -> pk_set().
from_commitment(Commitment) ->
    erlang_tc:pk_set_from_commitment(Commitment).

-spec public_key(PKSet :: pk_set()) -> public_key:pk().
public_key(PKSet) ->
    erlang_tc:pk_set_public_key(PKSet).

-spec threshold(PKSet :: pk_set()) -> non_neg_integer().
threshold(PKSet) ->
    erlang_tc:pk_set_threshold(PKSet).

-spec public_key_share(PKSet :: pk_set(), I :: non_neg_integer()) -> public_key_share:pk_share().
public_key_share(PKSet, I) ->
    erlang_tc:pk_set_public_key_share(PKSet, I).

-spec decrypt(
    PKSet :: pk_set(),
    DecShares :: [{non_neg_integer(), decryption_share:dec_share()}],
    Cipher :: ciphertext:ciphertext()
) -> {ok, binary()} | {error, cannot_decrypt}.
decrypt(PKSet, DecShares, Cipher) ->
    erlang_tc:pk_set_decrypt(PKSet, DecShares, Cipher).

-spec combine_signatures(
    PKSet :: pk_set(),
    SigShares :: [{non_neg_integer(), signature_share:sig_share()}]
) -> {ok, signature:sig()} | {error, cannot_combine}.
combine_signatures(PKSet, SigShares) ->
    erlang_tc:pk_set_combine_signatures(PKSet, SigShares).
