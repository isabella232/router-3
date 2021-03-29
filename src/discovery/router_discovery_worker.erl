%%%-------------------------------------------------------------------
%% @doc
%% == Router Discovery Worker ==
%% @end
%%%-------------------------------------------------------------------
-module(router_discovery_worker).

-behavior(gen_server).

-include_lib("helium_proto/include/discovery_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1,
    activate/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, Args, []).

-spec activate(map()) -> ok.
activate(Map) ->
    gen_server:cast(?SERVER, {activate, Map}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    lager:info("~p init with ~p", [?SERVER, _Args]),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    lager:warning("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {reply, ok, State}.

handle_cast({activate, Map}, State) ->
    Hostpost = maps:get(hotspot, Map),
    PubKeyBin = libp2p_crypto:b58_to_bin(erlang:binary_to_list(Hostpost)),
    TxnID = maps:get(transaction_id, Map),
    Sig = maps:get(signature, Map),
    DeviceID = maps:get(device_id, Map),
    case
        libp2p_crypto:verify(
            <<Hostpost, TxnID>>,
            Sig,
            libp2p_crypto:bin_to_pubkey(PubKeyBin)
        )
    of
        false ->
            lager:info("failed to verify signature for ~p (device_id=~p txn_id=~p sig=~p)", [
                blockchain_utils:addr2name(PubKeyBin),
                DeviceID,
                TxnID,
                Sig
            ]);
        true ->
            case router_discovery_handler:dial(PubKeyBin) of
                {error, _Reason} ->
                    lager:info("failed to dial hotpost ~p: )", [
                        blockchain_utils:addr2name(PubKeyBin),
                        _Reason
                    ]);
                {ok, Stream} ->
                    BinMsg = discovery_pb:encode_msg(#discovery_start_pb{
                        hotspot = PubKeyBin,
                        transaction_id = TxnID,
                        signature = Sig
                    }),
                    ok = router_discovery_handler:send(Stream, BinMsg)
            end
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("rcvd unknown cast msg: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Msg, State) ->
    lager:warning("rcvd unknown info msg: ~p", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
