-module(client).

%% Public API
-export([
    create/0, % new_session() -> {ok, RoomId}
    join/1,   % join(RoomId) -> ok
    say/2,    % say(RoomId, Text) -> ok | {error, Reason}
    leave/1  % leave(RoomId) -> ok | {error, Reason}
%    delete/1  % close(RoomId) -> ok | {error, Reason}
]).

create() ->
  ServerId = whereis(server),
  {RoomId, ok} = gen_server_plus:call(ServerId,new,create),
  {ok, RoomId}.

join(RoomId) ->
  ServerId = whereis(server),
  gen_server_plus:call(ServerId, RoomId, join).

say(RoomId, Msg) ->
  ServerId = whereis(server),
  gen_server_plus:call(ServerId, RoomId, {say, Msg}).

leave(RoomId) ->
  ServerId = whereis(server),
  gen_server_plus:call(ServerId, RoomId, leave).

% TODO: Not implemented in gen_server_plus. A session can never be deleted.
% delete(RoomId) ->
%   ServerId = whereis(server),
%   gen_server_plus:call(ServerId, RoomId, delete).

