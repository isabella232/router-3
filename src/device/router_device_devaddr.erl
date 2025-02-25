%%%-------------------------------------------------------------------
%% @doc
%% == Router Device Devaddr ==
%%
%% - Process registers itself to the blockchain as an event handler.
%% - Allocates DevAddrs
%% - Helpers for router_devices
%%
%% @end
%%%-------------------------------------------------------------------
-module(router_device_devaddr).

-behavior(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1,
    default_devaddr/0,
    allocate/2,
    sort_devices/2,
    pubkeybin_to_loc/2
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
-define(ETS, router_device_devaddr_ets).
%%
-define(BITS_23, 8388607).

-record(state, {
    chain = undefined :: blockchain:blockchain() | undefined,
    oui :: non_neg_integer(),
    subnets = [] :: [binary()],
    devaddr_used = #{} :: map()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, Args, []).

-spec default_devaddr() -> binary().
default_devaddr() ->
    DevAddrPrefix = application:get_env(blockchain, devaddr_prefix, $H),
    DefaultDevaddr = <<33554431:25/integer-unsigned-little, DevAddrPrefix:7/integer>>,
    case application:get_env(router, default_devaddr) of
        undefined ->
            DefaultDevaddr;
        {ok, Base64Str} ->
            try base64:decode(Base64Str) of
                Decoded -> Decoded
            catch
                _:_ -> DefaultDevaddr
            end
    end.

-spec allocate(router_device:device(), libp2p_crypto:pubkey_bin()) ->
    {ok, binary()} | {error, any()}.
allocate(Device, PubKeyBin) ->
    gen_server:call(?SERVER, {allocate, Device, PubKeyBin}).

-spec sort_devices([router_device:device()], libp2p_crypto:pubkey_bin()) ->
    [router_device:device()].
sort_devices(Devices, PubKeyBin) ->
    Chain = blockchain_worker:blockchain(),
    case ?MODULE:pubkeybin_to_loc(PubKeyBin, Chain) of
        {error, _Reason} ->
            Devices;
        {ok, Index} ->
            lists:sort(fun(A, B) -> sort_devices_fun(A, B, Index, Chain) end, Devices)
    end.

%% TODO: Maybe make this a ets table to avoid lookups all the time
-spec pubkeybin_to_loc(
    undefined | libp2p_crypto:pubkey_bin(),
    undefined | blockchain:blockchain()
) -> {ok, non_neg_integer()} | {error, any()}.
pubkeybin_to_loc(undefined, _Chain) ->
    {error, undef_pubkeybin};
pubkeybin_to_loc(_PubKeyBin, undefined) ->
    {error, no_chain};
pubkeybin_to_loc(PubKeyBin, Chain) ->
    Ledger = blockchain:ledger(Chain),
    case blockchain_ledger_v1:find_gateway_info(PubKeyBin, Ledger) of
        {error, _} = Error ->
            Error;
        {ok, Hotspot} ->
            case blockchain_ledger_gateway_v2:location(Hotspot) of
                undefined -> {error, undef_index};
                Index -> {ok, Index}
            end
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    lager:info("~p init with ~p", [?SERVER, Args]),
    ok = blockchain_event:add_handler(self()),
    OUI =
        case router_utils:get_oui() of
            undefined -> error(no_oui_configured);
            OUI0 -> OUI0
        end,
    self() ! post_init,
    {ok, #state{oui = OUI}}.

handle_call({allocate, _Device, _PubKeyBin}, _From, #state{subnets = []} = State) ->
    {reply, {error, no_subnet}, State};
handle_call(
    {allocate, _Device, PubKeyBin},
    _From,
    #state{chain = Chain, subnets = Subnets, devaddr_used = Used} = State
) ->
    Index =
        case ?MODULE:pubkeybin_to_loc(PubKeyBin, Chain) of
            {error, _} -> h3:from_geo({0.0, 0.0}, 12);
            {ok, IA} -> IA
        end,
    Parent = h3:to_geo(h3:parent(Index, 1)),
    {NthSubnet, DevaddrBase} =
        case maps:get(Parent, Used, undefined) of
            undefined ->
                <<Base:25/integer-unsigned-big, _Mask:23/integer-unsigned-big>> = hd(Subnets),
                {1, Base};
            {Nth, LastBase} ->
                Subnet = lists:nth(Nth, Subnets),
                <<Base:25/integer-unsigned-big, Mask:23/integer-unsigned-big>> = Subnet,
                Max = blockchain_ledger_routing_v1:subnet_mask_to_size(Mask),
                case LastBase + 1 >= Base + Max of
                    true ->
                        {NextNth, NextSubnet} = next_subnet(Subnets, Nth),
                        <<NextBase:25/integer-unsigned-big, _:23/integer-unsigned-big>> =
                            NextSubnet,
                        {NextNth, NextBase};
                    false ->
                        {Nth, LastBase + 1}
                end
        end,
    DevAddrPrefix = application:get_env(blockchain, devaddr_prefix, $H),
    Reply = {ok, <<DevaddrBase:25/integer-unsigned-little, DevAddrPrefix:7/integer>>},
    {reply, Reply, State#state{devaddr_used = maps:put(Parent, {NthSubnet, DevaddrBase}, Used)}};
handle_call(_Msg, _From, State) ->
    lager:warning("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:warning("rcvd unknown cast msg: ~p", [_Msg]),
    {noreply, State}.

handle_info(
    {blockchain_event, {add_block, BlockHash, _Syncing, _Ledger}},
    #state{chain = Chain, oui = OUI} = State
) ->
    {ok, Block} = blockchain:get_block(BlockHash, Chain),
    FilterFun = fun(T) ->
        case blockchain_txn:type(T) of
            blockchain_txn_oui_v1 ->
                blockchain_txn_oui_v1:oui(T) == OUI;
            blockchain_txn_routing_v1 ->
                blockchain_txn_routing_v1:oui(T) == OUI;
            _ ->
                false
        end
    end,
    %% check if there's any txns that affect our OUI
    case blockchain_utils:find_txn(Block, FilterFun) of
        [] ->
            {noreply, State};
        _ ->
            Subnets = subnets(OUI, Chain),
            {noreply, State#state{subnets = Subnets}}
    end;
handle_info({blockchain_event, {integrate_genesis_block, _BlockHash}}, #state{oui = OUI} = State) ->
    case blockchain_worker:blockchain() of
        undefined ->
            erlang:send_after(500, self(), post_init),
            {noreply, State};
        Chain ->
            Subnets = subnets(OUI, Chain),
            {noreply, State#state{chain = Chain, subnets = Subnets}}
    end;
handle_info(post_init, #state{chain = undefined, oui = OUI} = State) ->
    case blockchain_worker:blockchain() of
        undefined ->
            erlang:send_after(500, self(), post_init),
            {noreply, State};
        Chain ->
            Subnets = subnets(OUI, Chain),
            {noreply, State#state{chain = Chain, subnets = Subnets}}
    end;
handle_info(post_init, State) ->
    {noreply, State};
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

-spec subnets(non_neg_integer(), blockchain:blockchain()) -> [binary()].
subnets(OUI, Chain) ->
    case blockchain_ledger_v1:find_routing(OUI, blockchain:ledger(Chain)) of
        {ok, RoutingEntry} ->
            blockchain_ledger_routing_v1:subnets(RoutingEntry);
        _ ->
            []
    end.

-spec next_subnet([binary()], non_neg_integer()) -> {non_neg_integer(), binary()}.
next_subnet(Subnets, Nth) ->
    case Nth + 1 > erlang:length(Subnets) of
        true -> {1, lists:nth(1, Subnets)};
        false -> {Nth + 1, lists:nth(Nth + 1, Subnets)}
    end.

-spec sort_devices_fun(
    router_device:device(),
    router_device:device(),
    h3:index(),
    blockchain:blockchain()
) -> boolean().
sort_devices_fun(DeviceA, DeviceB, Index, Chain) ->
    IndexA =
        case ?MODULE:pubkeybin_to_loc(router_device:location(DeviceA), Chain) of
            {error, _} -> undefined;
            {ok, IA} -> IA
        end,
    IndexB =
        case ?MODULE:pubkeybin_to_loc(router_device:location(DeviceB), Chain) of
            {error, _} -> undefined;
            {ok, IB} -> IB
        end,
    case IndexA == undefined orelse IndexB == undefined of
        true ->
            false;
        false ->
            case
                h3:get_resolution(IndexA) == h3:get_resolution(IndexB) andalso
                    h3:get_resolution(IndexA) == h3:get_resolution(Index)
            of
                false ->
                    {IndexA1, Indexb1, Index1} = indexes_to_lowest_res(IndexA, IndexB, Index),
                    compare_distance(Index1, IndexA1, Indexb1);
                true ->
                    compare_distance(Index, IndexA, IndexB)
            end
    end.

-spec indexes_to_lowest_res(h3:index(), h3:index(), h3:index()) ->
    {h3:index(), h3:index(), h3:index()}.
indexes_to_lowest_res(IndexA, IndexB, IndexC) ->
    Resolutions = [h3:get_resolution(IndexA), h3:get_resolution(IndexB), h3:get_resolution(IndexC)],
    LowestRes = lists:min(Resolutions),
    {to_res(IndexA, LowestRes), to_res(IndexB, LowestRes), to_res(IndexC, LowestRes)}.

-spec to_res(h3:index(), non_neg_integer()) -> h3:index().
to_res(Index, Res) ->
    h3:from_geo(h3:to_geo(Index), Res).

-spec compare_distance(h3:index(), h3:index(), h3:index()) -> boolean().
compare_distance(Index1, Index2, Index3) ->
    try h3:grid_distance(Index2, Index1) < h3:grid_distance(Index3, Index1) of
        Bool -> Bool
    catch
        _:_ -> false
    end.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

%% There H3 indexes (resolution 12) are aligned from farther left to right
-define(INDEX_A, 631210969893275647).
-define(INDEX_B, 631210973995593215).
-define(INDEX_C, 631210968861644799).
-define(INDEX_D, 631210968873637887).

sort_devices_test() ->
    Hotspots = #{
        <<"A">> => ?INDEX_A,
        <<"B">> => ?INDEX_B,
        <<"C">> => ?INDEX_C,
        <<"D">> => ?INDEX_D
    },
    meck:new(blockchain_worker, [passthrough]),
    meck:expect(blockchain_worker, blockchain, fun() -> chain end),
    meck:new(blockchain, [passthrough]),
    meck:expect(blockchain, ledger, fun(chain) -> ledger end),
    meck:new(blockchain_ledger_v1, [passthrough]),
    meck:expect(blockchain_ledger_v1, find_gateway_info, fun(PubKeyBin, ledger) ->
        {ok, blockchain_ledger_gateway_v2:new(PubKeyBin, maps:get(PubKeyBin, Hotspots))}
    end),

    Randomized = lists:sort([{rand:uniform(), N} || N <- maps:keys(Hotspots)]),
    Devices = [router_device:location(ID, router_device:new(ID)) || {_, ID} <- Randomized],

    ?assertEqual([<<"A">>, <<"B">>, <<"C">>, <<"D">>], [
        router_device:id(D)
        || D <- sort_devices(Devices, <<"A">>)
    ]),

    ?assertEqual([<<"B">>, <<"A">>, <<"C">>, <<"D">>], [
        router_device:id(D)
        || D <- sort_devices(Devices, <<"B">>)
    ]),

    ?assertEqual([<<"C">>, <<"D">>, <<"B">>, <<"A">>], [
        router_device:id(D)
        || D <- sort_devices(Devices, <<"C">>)
    ]),

    ?assertEqual([<<"D">>, <<"C">>, <<"B">>, <<"A">>], [
        router_device:id(D)
        || D <- sort_devices(Devices, <<"D">>)
    ]),

    ?assert(meck:validate(blockchain_worker)),
    meck:unload(blockchain_worker),
    ?assert(meck:validate(blockchain)),
    meck:unload(blockchain),
    ?assert(meck:validate(blockchain_ledger_v1)),
    meck:unload(blockchain_ledger_v1),
    ok.

indexes_to_lowest_res_test() ->
    ?assertEqual(
        {?INDEX_A, ?INDEX_B, ?INDEX_C},
        indexes_to_lowest_res(?INDEX_A, ?INDEX_B, ?INDEX_C)
    ).

to_res_test() ->
    ?assertEqual(?INDEX_A, to_res(?INDEX_A, 12)),
    ?assertEqual(?INDEX_B, to_res(?INDEX_B, 12)),
    ?assertEqual(?INDEX_C, to_res(?INDEX_C, 12)),
    ?assertEqual(?INDEX_D, to_res(?INDEX_D, 12)),
    ok.

compare_distance_test() ->
    ?assert(compare_distance(?INDEX_A, ?INDEX_B, ?INDEX_C)),
    ?assert(compare_distance(?INDEX_A, ?INDEX_C, ?INDEX_D)),
    ?assert(compare_distance(?INDEX_A, ?INDEX_B, ?INDEX_D)),
    ok.

-endif.
