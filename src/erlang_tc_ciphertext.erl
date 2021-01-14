-module(erlang_tc_ciphertext).

-export([
    %% Ciphertext API
    verify/1
]).

-type ciphertext() :: reference().
-export_type([ciphertext/0]).

-spec verify(Ciphertext :: ciphertext()) -> boolean().
verify(Ciphertext) ->
    erlang_tc:ciphertext_verify(Ciphertext).
