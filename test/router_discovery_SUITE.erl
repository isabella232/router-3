-module(router_discovery_SUITE).

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([disovery_test/1]).

-include_lib("helium_proto/include/discovery_pb.hrl").
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

disovery_test(Config) ->
    %% Setup stream stuff
    Swarm = proplists:get_value(swarm, Config),
    libp2p_swarm:add_stream_handler(
        Swarm,
        router_discovery_handler:version(),
        {router_discovery_handler_test, server, [self()]}
    ),
    [Address | _] = libp2p_swarm:listen_addrs(Swarm),
    {ok, _} = libp2p_swarm:connect(blockchain_swarm:swarm(), Address),
    test_utils:wait_until(fun() ->
        case libp2p_swarm:connect(blockchain_swarm:swarm(), libp2p_swarm:p2p_address(Swarm)) of
            {ok, _} -> true;
            _ -> false
        end
    end),

    #{secret := PrivKey, public := PubKey} = proplists:get_value(keys, Config),
    SigFun = libp2p_crypto:mk_sig_fun(PrivKey),
    PubKeyBin = libp2p_crypto:pubkey_to_bin(PubKey),
    Hostpost = erlang:list_to_binary(libp2p_crypto:bin_to_b58(PubKeyBin)),
    TxnID = <<"txn_id_1">>,
    Sig = SigFun(<<Hostpost/binary, ",", TxnID/binary>>),
    EncodedSig = base64:encode(Sig),
    Map = #{
        <<"hotspot">> => Hostpost,
        <<"transaction_id">> => TxnID,
        <<"device_id">> => <<"device_id_1">>,
        <<"signature">> => EncodedSig
    },

    WSPid =
        receive
            {websocket_init, P} -> P
        after 2500 -> ct:fail(websocket_init_timeout)
        end,

    WSPid ! {discovery, Map},

    receive
        {router_discovery_handler_test, Bin} ->
            Data = discovery_pb:decode_msg(Bin, discovery_start_pb),
            #discovery_start_pb{
                hotspot = PubKeyBin,
                transaction_id = TxnID,
                signature = EncodedSig
            } = Data
    after 500 -> ct:fail(router_discovery_handler_test_timeout)
    end,
    ok.

%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------
