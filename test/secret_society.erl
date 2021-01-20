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
    pk_set :: public_key_set:pk_set()
}).

-record(actor, {
    id :: non_neg_integer(),
    sk_share :: secret_key_share:sk_share(),
    pk_share :: public_key_share:pk_share(),
    msg_inbox = undefined :: undefined | ciphertext:ciphertext()
}).

-record(decryption_meeting, {
    pk_set :: public_key_set:pk_set(),
    ciphertext = undefined :: undefined | ciphertext:ciphertext(),
    dec_shares = #{} :: #{non_neg_integer() => decryption_share:dec_share()}
}).

-type actor() :: #actor{}.

new_secret_society(NumActors, Threshold) ->
    SKSet = secret_key_set:random(Threshold),
    PKSet = secret_key_set:public_keys(SKSet),

    Actors = lists:map(
        fun(ID) ->
            SKShare = secret_key_set:secret_key_share(SKSet, ID),
            PKShare = public_key_set:public_key_share(PKSet, ID),
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
    public_key_set:public_key(pk_set(SecretSociety)).

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

start_decryption_meeting(SecretSociety) ->
    #decryption_meeting{pk_set = pk_set(SecretSociety)}.

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
            case meeting_cipher(DecryptionMeeting) of
                undefined ->
                    %% no one in the meeting, accept
                    DecryptionMeeting1 = meeting_cipher(DecryptionMeeting, Cipher),
                    DecShare = secret_key_share:decrypt_share(sk_share(Actor), Cipher),
                    ?assert(public_key_share:verify_decryption_share(pk_share(Actor), DecShare, Cipher)),
                    add_share(DecryptionMeeting1, id(Actor), DecShare);
                MeetingCipher ->
                    case ciphertext:cmp(Cipher, MeetingCipher) of
                        false ->
                            DecryptionMeeting;
                        true ->
                            DecryptionMeeting1 = meeting_cipher(DecryptionMeeting, Cipher),
                            DecShare = secret_key_share:decrypt_share(sk_share(Actor), Cipher),
                            ?assert(public_key_share:verify_decryption_share(pk_share(Actor), DecShare, Cipher)),
                            add_share(DecryptionMeeting1, id(Actor), DecShare)
                    end
            end
    end.

decrypt_msg(DecryptionMeeting) ->
    case meeting_cipher(DecryptionMeeting) of
        undefined ->
            {error, cannot_decrypt};
        Cipher ->
            PKSet = meeting_pk_set(DecryptionMeeting),
            public_key_set:decrypt(PKSet, maps:to_list(dec_shares(DecryptionMeeting)), Cipher)
    end.
