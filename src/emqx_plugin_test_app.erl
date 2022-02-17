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

-module(emqx_plugin_test_app).

-behaviour(application).

-emqx_plugin(protocol).

%-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

-define(UDP_SOCKOPTS, []).

-type(listener() :: {esockd:proto(), esockd:listen_on(), [esockd:option()]}).

start(_StartType, _StartArgs) ->
    Addr = {{0,0,0,0}, 1884},
    GwId = 1,
    {ok, Sup} = emqx_plugin_test_sup:start_link(Addr, GwId),
    start_listeners(),
    % emqx_plugin_test:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    stop_listeners(),
    ok.
    %emqx_plugin_test:unload().

%%--------------------------------------------------------------------
%% Listners
%%--------------------------------------------------------------------

-spec start_listeners() -> ok.
start_listeners() ->
  PredefTopics = [{0, "reserved"}, {1, "/predefined/topic/name/hello"}],
  ListenCfs = [begin
                 TabName = tabname(udp, 4000), % "emqx_sn_registry__udp_0.0.0.0:1884"
                 %% 需要被监控的进程，通过supervisor启动
                 {ok, RegistryPid} = emqx_plugin_test_sup:start_registry_proc(emqx_plugin_test_sup, TabName, PredefTopics),
                 {Proto, ListenOn, [{registry, {TabName, RegistryPid}} | Options]}
               end || {Proto, ListenOn, Options} <- listeners_confs()],
  lists:foreach(fun start_listener/1, ListenCfs).

-spec start_listener(listener()) -> ok.
start_listener({Proto, ListenOn, Options}) ->
  case start_listener(Proto, ListenOn, Options) of
    {ok, _} -> io:format("Start mqttsn:~s listener on ~s successfully.~n",
      [Proto, format(ListenOn)]);
    {error, Reason} ->
      io:format(standard_error, "Failed to start mqttsn:~s listener on ~s - ~0p~n!",
        [Proto, format(ListenOn), Reason]),
      error(Reason)
  end.

%% Start listener
-spec start_listener(esockd:proto(), esockd:listen_on(), [esockd:option()])
      -> {ok, pid()} | {error, term()}.
start_listener(udp, ListenOn, Options) ->
  start_udp_listener('test:udp', ListenOn, Options);
start_listener(dtls, ListenOn, Options) ->
  start_udp_listener('test:dtls', ListenOn, Options).

%% @private
start_udp_listener(Name, ListenOn, Options) ->
  SockOpts = esockd:parse_opt(Options),
  esockd:open_udp(Name, ListenOn, merge_default(SockOpts),
    {emqx_plugin_test_gateway, start_link, [Options -- SockOpts]}).

-spec stop_listeners() -> ok.
stop_listeners() ->
  lists:foreach(fun stop_listener/1, listeners_confs()).

-spec stop_listener(listener()) -> ok | {error, term()}.
stop_listener({Proto, ListenOn, Opts}) ->
  StopRet = stop_listener(Proto, ListenOn, Opts),
  case StopRet of
    ok -> io:format("Stop mqttsn:~s listener on ~s successfully.~n",
      [Proto, format(ListenOn)]);
    {error, Reason} ->
      io:format(standard_error, "Failed to stop mqttsn:~s listener on ~s - ~p~n.",
        [Proto, format(ListenOn), Reason])
  end,
  StopRet.

-spec stop_listener(esockd:proto(), esockd:listen_on(), [esockd:option()])
      -> ok | {error, term()}.
stop_listener(udp, ListenOn, _Opts) ->
  esockd:close('test:udp', ListenOn);
stop_listener(dtls, ListenOn, _Opts) ->
  esockd:close('test:dtls', ListenOn).

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

%% @private
%% In order to compatible with the old version of the configuration format
listeners_confs() ->
  ListenOn = 4000,
  GwId = 1,
  Username = "mqtt_sn_user",
  Password = "abc",
  EnableQos3 = false,
  EnableStats = false,
  IdleTimeout = 30000,
  [{udp, ListenOn, [{gateway_id, GwId},
    {username, Username},
    {password, Password},
    {enable_qos3, EnableQos3},
    {enable_stats, EnableStats},
    {idle_timeout, IdleTimeout},
    {max_connections, 1024000},
    {max_conn_rate, 1000},
    {udp_options, []}]}].

merge_default(Options) ->
  Options.

format(Port) when is_integer(Port) ->
  io_lib:format("0.0.0.0:~w", [Port]);
format({Addr, Port}) when is_list(Addr) ->
  io_lib:format("~s:~w", [Addr, Port]);
format({Addr, Port}) when is_tuple(Addr) ->
  io_lib:format("~s:~w", [inet:ntoa(Addr), Port]).

tabname(Proto, ListenOn) ->
  list_to_atom(lists:flatten(["emqx_plugin_test_registry__", atom_to_list(Proto), "_", format(ListenOn)])).
