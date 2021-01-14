-module(secret_society).

-include_lib("eunit/include/eunit.hrl").

-export([
    new_secret_society/2,
    new_actor/3,
    actors/1,
    pk_set/1,
    publish_public_key/1,
    get_actor/2,

    %% Actor API
    id/1,
    sk_share/1,
    pk_share/1,
    msg_inbox/1,
    send_msg/2,

    %% DecryptionMeeting API
    start_decryption_meeting/1,
    accept_decryption_share/2,
    decrypt_msg/1
]).

-record(secret_society, {
    actors :: [actor()],
    pk_set :: erlang_tc_pk_set:pk_set()
}).

-record(actor, {
    id :: non_neg_integer(),
    sk_share :: erlang_tc_sk_share:sk_share(),
    pk_share :: erlang_tc_pk_share:pk_share(),
    msg_inbox = undefined :: undefined | erlang_tc_ciphertext:ciphertext()
}).

-record(decryption_meeting, {
    pk_set :: erlang_tc_pk_set:pk_set(),
    ciphertext = undefined :: undefined | erlang_tc_ciphertext:ciphertext(),
    dec_shares = #{} :: #{non_neg_integer() => erlang_tc_dec_share:dec_share()}
}).

-type actor() :: #actor{}.

new_secret_society(NumActors, Threshold) ->
    SKSet = erlang_tc_sk_set:random(Threshold),
    PKSet = erlang_tc_sk_set:public_keys(SKSet),

    Actors = lists:map(
        fun(ID) ->
            SKShare = erlang_tc_sk_set:secret_key_share(SKSet, ID),
            PKShare = erlang_tc_pk_set:public_key_share(PKSet, ID),
            new_actor(ID, SKShare, PKShare)
        end,
        lists:seq(1, NumActors)
    ),
    #secret_society{actors = Actors, pk_set = PKSet}.

get_actor(SecretSociety, ID) ->
    Actors = actors(SecretSociety),
    case lists:filter(fun(Actor) -> id(Actor) == ID end, Actors) of
        [] -> undefined;
        L -> hd(L)
    end.

publish_public_key(SecretSociety) ->
    erlang_tc_pk_set:public_key(pk_set(SecretSociety)).

new_actor(ID, SecretKeyShare, PublicKeyShare) ->
    #actor{id = ID, sk_share = SecretKeyShare, pk_share = PublicKeyShare}.

actors(SecretSociety) ->
    SecretSociety#secret_society.actors.

pk_set(SecretSociety) ->
    SecretSociety#secret_society.pk_set.

id(Actor) ->
    Actor#actor.id.

sk_share(Actor) ->
    Actor#actor.sk_share.

pk_share(Actor) ->
    Actor#actor.pk_share.

msg_inbox(Actor) ->
    Actor#actor.msg_inbox.

send_msg(Actor, Cipher) ->
    Actor#actor{msg_inbox=Cipher}.

start_decryption_meeting(PKSet) ->
    #decryption_meeting{pk_set = PKSet}.

meeting_cipher(DecryptionMeeting) ->
    DecryptionMeeting#decryption_meeting.ciphertext.

meeting_cipher(DecryptionMeeting, Cipher) ->
    DecryptionMeeting#decryption_meeting{ciphertext=Cipher}.

meeting_pk_set(DecryptionMeeting) ->
    DecryptionMeeting#decryption_meeting.pk_set.

dec_shares(DecryptionMeeting) ->
    DecryptionMeeting#decryption_meeting.dec_shares.

add_share(DecryptionMeeting, ActorID, DecShare) ->
    DecShares = dec_shares(DecryptionMeeting),
    DecryptionMeeting#decryption_meeting{dec_shares=maps:put(ActorID, DecShare, DecShares)}.

accept_decryption_share(DecryptionMeeting, Actor) ->
    case msg_inbox(Actor) of
        undefined ->
            DecryptionMeeting;
        Cipher ->
            case Cipher == meeting_cipher(DecryptionMeeting) of
                false ->
                    DecryptionMeeting;
                true ->
                    DecryptionMeeting1 = meeting_cipher(DecryptionMeeting, Cipher),
                    DecShare = erlang_tc_sk_share:decrypt_share(sk_share(Actor), Cipher),
                    ?assert(erlang_tc_pk_share:verify_decryption_share(pk_share(Actor), DecShare, Cipher)),
                    add_share(DecryptionMeeting1, id(Actor), DecShare)
            end
    end.

decrypt_msg(DecryptionMeeting) ->
    case meeting_cipher(DecryptionMeeting) of
        undefined -> [];
        Cipher ->
            PKSet = meeting_pk_set(DecryptionMeeting),
            erlang_tc_pk_set:decrypt(PKSet, maps:to_list(dec_shares(DecryptionMeeting)), Cipher)
    end.
