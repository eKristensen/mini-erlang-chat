-module(server).
-behaviour(gen_server_plus).


% Public functions in this module
-export([start/0]).

-export([handle_new_session_call/3, handle_new_session_call/4, handle_new_session_cast/3, handle_plus_call/4, handle_plus_cast/3 ]).

start() ->
  {ok,ServerId} = gen_server_plus:start_link(?MODULE, [], []),
  register(server, ServerId),
  ok.

% New chat room, relying heavily on gen_server_plus for session management
handle_new_session_call(create, _From, GlobalState) ->
    {reply, ok, sets:new([{version, 2}]), GlobalState};

handle_new_session_call(_, _From, GlobalState) ->
    % Put in error as session state.
    {reply, error, error, GlobalState}.

% If session state is error, send back error.
handle_plus_call(_Msg, _From, error, GlobalState) ->
    io:format("DEBUG: Client uses invalid session."),
    {reply, error, error, GlobalState};

handle_plus_call(join, {From, _}, SessionState, GlobalState) ->
    NewSessionState = sets:add_element(From, SessionState),
    {reply, ok, NewSessionState, GlobalState};

handle_plus_call(leave, {From, _}, SessionState, GlobalState) ->
    NewSessionState = sets:del_element(From, SessionState),
    {reply, ok, NewSessionState, GlobalState};

handle_plus_call({say, Text}, _From, SessionState, GlobalState) ->
    send_msg_to_room(Text, sets:to_list(SessionState)),
    {reply, ok, SessionState, GlobalState};

handle_plus_call(_Msg, _From, SessionState, GlobalState) ->
    io:format("DEBUG: Server got unsupported message"),
    {reply, error, SessionState, GlobalState}.

send_msg_to_room(_Msg, []) -> ok;
send_msg_to_room(Msg, [Pid|Tail]) ->
    io:format("Sending Msg~n"),
    Pid ! Msg,
    send_msg_to_room(Msg, Tail).

handle_plus_cast(_, SessionState, GlobalState) ->
    io:format("No cast support~n"),
    {noreply, SessionState, GlobalState}.

handle_new_session_call(_, SessionState, GlobalState, _) ->
    io:format("No support for 4 argument new session~n"),
    {noreply, SessionState, GlobalState}.

handle_new_session_cast(_, SessionState, GlobalState) ->
    io:format("No support for new session via cast ~n"),
    {noreply, SessionState, GlobalState}.

