-module(erlang_tc_ciphertext).

-export([
    %% Ciphertext API
    verify/1
]).

-spec verify(Ciphertext :: reference()) -> boolean().
verify(Ciphertext) ->
    erlang_tc:ciphertext_verify(Ciphertext).
