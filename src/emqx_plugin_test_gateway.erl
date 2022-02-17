%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_plugin_test_gateway).

-behaviour(gen_statem).

-include("mqtt_sn.hrl").
-include("emqx.hrl").
-include("emqx_mqtt.hrl").

%% API.
-export([start_link/3]).

%% state functions
-export([ idle/3
        ]).

%% gen_statem callbacks
-export([ init/1
        , callback_mode/0
        , handle_event/4
        , terminate/3
        , code_change/4
        ]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-type(maybe(T) :: T | undefined).

-record(state, {gwid                 :: integer(),
                socket               :: port(),
                sockpid              :: pid(),
                sockstate            :: any(),
                sockname             :: {inet:ip_address(), inet:port()},
                peername             :: {inet:ip_address(), inet:port()},
                registry             :: emqx_plugin_test_registry:registry(),
                clientid             :: maybe(binary()),
                username             :: maybe(binary()),
                password             :: maybe(binary()),
                asleep_timer         :: tuple()
               }).


-define(STAT_TIMEOUT, 10000).
-define(IDLE_TIMEOUT, 30000).
-define(DEFAULT_CHAN_OPTIONS, [{max_packet_size, 256}, {zone, external}]).


-define(NO_PEERCERT, undefined).


%%--------------------------------------------------------------------
%% Exported APIs
%%--------------------------------------------------------------------
% esockd:open_udp回调函数
start_link(Transport, Peername, Options) ->
    gen_statem:start_link(?MODULE, [Transport, Peername, Options], [{hibernate_after, 60000}]).


%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
%% esockd:open_udp的回调函数，PeerName：{IP, InportNo}
init([{_, SockPid, Sock}, Peername, Options]) ->
    GwId = proplists:get_value(gateway_id, Options),
    Registry = proplists:get_value(registry, Options),
    Username = proplists:get_value(username, Options, undefined),
    Password = proplists:get_value(password, Options, undefined),
    IdleTimeout = proplists:get_value(idle_timeout, Options, 30000),
    case inet:sockname(Sock) of  % {ok,{ip_address(), port_number()}
        {ok, Sockname} ->
            State = #state{gwid             = GwId,
                           username         = Username,
                           password         = Password,
                           socket           = Sock,
                           sockstate        = running,
                           sockpid          = SockPid,
                           sockname         = Sockname,
                           peername         = Peername,
                           registry         = Registry,
                           asleep_timer     = emqx_plugin_test_asleep_timer:init()
                          },
            {ok, idle, State, [IdleTimeout]};
        {error, Reason} when Reason =:= closed ->
            {stop, normal};
        {error, Reason} -> {stop, Reason}
    end.

callback_mode() -> state_functions.

idle(EventType, EventContent, State = #state{sockpid = SockPid, peername = Peername}) ->
    io:format("idle(EventType, EventContent, State)~n"),
    Data = <<"hello!">>,
    SockPid ! {datagram, Peername, Data},
    {keep_state, State}.

terminate(Reason, _StateName, #state{clientid = ClientId, registry = Registry}) ->
    io:format("terminate~n"),
    ok.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(EventType, EventContent, StateName, State) ->
  io:format("StateName: ~s, EventType: ~p, EventContent:~p, State: ~p ~n",
    [StateName, EventType, EventContent, State]),
  {keep_state, State}.


%%--------------------------------------------------------------------
%% Helper funcs
%%--------------------------------------------------------------------


next_event(Content) ->
    {next_event, cast, Content}.
