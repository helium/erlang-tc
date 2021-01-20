-module(g2).

-export([
    %% G2 API
    random/0
]).

-type g2() :: reference().
-export_type([g2/0]).

-spec random() -> g2().
random() ->
    erlang_tc:g2_random().
