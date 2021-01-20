-module(ciphertext).

-export([
    %% Ciphertext API
    verify/1,
    cmp/2
]).

-type ciphertext() :: reference().
-export_type([ciphertext/0]).

-spec verify(Ciphertext :: ciphertext()) -> boolean().
verify(Ciphertext) ->
    erlang_tc:ciphertext_verify(Ciphertext).

-spec cmp(C1 :: ciphertext(), C2 :: ciphertext()) -> boolean().
cmp(C1, C2) ->
    erlang_tc:ciphertext_cmp(C1, C2).
