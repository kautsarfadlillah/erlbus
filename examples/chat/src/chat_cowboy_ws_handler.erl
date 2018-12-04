-module(chat_cowboy_ws_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(TIMEOUT, 5 * 60 * 1000). % Innactivity Timeout

-record(state, {name, handler}).

%% API

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
  % Create the handler from our custom callback
  Handler = ebus_proc:spawn_handler(fun chat_erlbus_handler:handle_msg/2, [self()]),
  {Roomname, _} = cowboy_req:binding(roomname, Req),
  ebus:sub(Handler, Roomname),
  {ok, Req, #state{name = get_name(Req), handler = Handler}, ?TIMEOUT}.

websocket_handle(Data = {text, Msg}, Req, State) ->
  {Roomname, _} = cowboy_req:binding(roomname, Req),
  ebus:pub(Roomname, {State#state.name, Msg}),
  {ok, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({message_published, {Sender, Msg}}, Req, State) ->
  {Username, _} = cowboy_req:binding(username, Req),
  {reply, {text, jiffy:encode({[{sender, Sender}, {msg, Msg}]})}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, Req, State) ->
  % Unsubscribe the handler
  {Roomname, _} = cowboy_req:binding(roomname, Req),
  ebus:unsub(State#state.handler, Roomname),
  ok.

%% Private methods

get_name(Req) ->
  {Username, _} = cowboy_req:binding(username, Req),
  {{Host, Port}, _} = cowboy_req:peer(Req),
  Name = list_to_binary(string:join([binary_to_list(Username), " (", inet_parse:ntoa(Host), 
    ":", io_lib:format("~p", [Port]), ")"], "")),
  Name.
  