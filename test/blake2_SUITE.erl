-module(blake2_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([blake2_hash_test/1, blake2_hash_personal_test/1]).

%%%===================================================================
%%% api
%%%===================================================================

all() ->
  [blake2_hash_test, blake2_hash_personal_test].

init_per_suite(_Config) ->
  _Config.
end_per_suite(_Config) ->
  ok.

%%%===================================================================
%%% tests
%%%===================================================================

blake2_hash_test(_Config) ->
  ?assertEqual(
    <<"865AE2DA811193E695D41DAFEFF41E5B00912185E909D0CAA2BE949CC9D6145F244EA2D27960380833F1CB761F5E21CD15CD">>,
    blake2:hexhash(400, <<"TEST">>)),
  {ok, State1} = blake2:init(400),
  ok = blake2:update(State1, <<"TEST">>),
  {ok, Result} = blake2:final(State1),
  ?assertEqual(
    <<"865AE2DA811193E695D41DAFEFF41E5B00912185E909D0CAA2BE949CC9D6145F244EA2D27960380833F1CB761F5E21CD15CD">>,
    list_to_binary(hex:bin_to_hexstr(Result))).

blake2_hash_personal_test(_Config) ->
  {ok, State1} = blake2:init_personal(400, <<"PERSONAL01230123">>),
  ok = blake2:update(State1, <<"TEST">>),
  {ok, Result} = blake2:final(State1),
  ?assertEqual(
    <<"BE5DED2E16AE129692D0525388030BAA3F944A8C1B54065B1DEFBA4A21D495E076A345B67EFE38AFF1BBF28C3CE5E2A2500B">>,
    list_to_binary(hex:bin_to_hexstr(Result))),

  ?assertEqual(
    <<"BE5DED2E16AE129692D0525388030BAA3F944A8C1B54065B1DEFBA4A21D495E076A345B67EFE38AFF1BBF28C3CE5E2A2500B">>,
    blake2:hexhash_personal(400, <<"TEST">>, <<"PERSONAL01230123">>)
  ).