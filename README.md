erlang_tc
=====

Erlang bindings for [threshold_crypto](https://github.com/poanetwork/threshold_crypto).

**NOTE**: We advise _not_ to use the `erlang_tc` module directly, preferably use the submodules for performing various threshold cryptography functions.

Usage
-----

Verfy basic secret key usage.

```erlang
1> SK0 = tc_secret_key:random().
#Ref<0.989752667.1337851906.91190>
2> SK1 = tc_secret_key:random().
#Ref<0.989752667.1337851906.91197>
3> PK0 = tc_secret_key:public_key(SK0).
#Ref<0.989752667.1337851906.91209>
4> MSG0 = <<"real news">>.
<<"real news">>
5> MSG1 = <<"fake news">>.
<<"fake news">>
6> %% correct
6> tc_pubkey:verify(PK0, tc_secret_key:sign(SK0, MSG0), MSG0).
true
7> %% wrong key
7> tc_pubkey:verify(PK0, tc_secret_key:sign(SK1, MSG0), MSG0).
false
8> %% wrong msg
8> tc_pubkey:verify(PK0, tc_secret_key:sign(SK0, MSG1), MSG0).
false
```

Build
-----

    $ make


Test
-----

    $ make test
