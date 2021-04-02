-module(router_discovery_SUITE).

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([disovery_test/1]).

-include_lib("helium_proto/include/blockchain_state_channel_v1_pb.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("router_device_worker.hrl").
-include("lorawan_vars.hrl").
-include("utils/console_test.hrl").

-define(DECODE(A), jsx:decode(A, [return_maps])).
-define(APPEUI, <<0, 0, 0, 2, 0, 0, 0, 1>>).
-define(DEVEUI, <<0, 0, 0, 0, 0, 0, 0, 1>>).
-define(ETS, ?MODULE).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [disovery_test].

%%--------------------------------------------------------------------
%% TEST CASE SETUP
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
    test_utils:init_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------
%% TEST CASE TEARDOWN
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
    test_utils:end_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

disovery_test(_Config) ->
    WSPid =
        receive
            {websocket_init, P} -> P
        after 2500 -> ct:fail(websocket_init_timeout)
        end,

    {ok, PubKey, SigFun, _} = blockchain_swarm:keys(),
    PubKeyBin = libp2p_crypto:pubkey_to_bin(PubKey),
    Hostpost = erlang:list_to_binary(libp2p_crypto:bin_to_b58(PubKeyBin)),
    TxnID = <<"txn_id_1">>,
    Sig = SigFun(<<Hostpost/binary, TxnID/binary>>),
    Map = #{
        <<"hotspot">> => Hostpost,
        <<"transaction_id">> => TxnID,
        <<"device_id">> => <<"device_id_1">>,
        <<"signature">> => base64:encode(Sig)
    },
    WSPid !
        {discovery, Map},
    ?assert(
        libp2p_crypto:verify(
            <<Hostpost/binary, TxnID/binary>>,
            Sig,
            libp2p_crypto:bin_to_pubkey(PubKeyBin)
        )
    ),

    timer:sleep(250),
    ?assert(false),
    ok.

%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------
