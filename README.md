# Mini Erlang Chat

## Note implementation choices

- There is no nickname feature. Sender must add their name manually.
- gen_server_plus does not support delete, so chat-rooms cannot be deleted currently.
- messages are cast to chat-room members with `erlang:send`
- Server does not store any messages.
- Anyone can send messages to any chat-room (no access control).
- Join chat-room = subscribe to any message sent to that chat-room.
- Client is dumb: User must check for messages with e.g. `flush().`

## Possible improvements (asides from the obvious based on implementation choices)

- Server not to crash if another server is started.
- Client as `gen_server` to show messages in console when they arrive.
- Use `gen_server` to send cast messages.
- Use supervisor to recover from crash
- More persistent state.

## Server functions

- create room
- join room - open "socket" and listen
- leave - Leave room
- say - Send message to room
- delete room (not implemented)

When joined: Server sends messages to attached clients

Nice element: A client can join "mid-session" when they "join" a room.
This creates a funny responsibility for the sessions. Yet very simple
to understand.

## Example

```erlang
c(gen_server_plus).
c(server).
c(client).
server:start().
f(RoomId).
{ok, RoomId} = client:create().
client:join(RoomId).
client:say(RoomId, "Hello").
client:leave(RoomId).
flush().
```

## Dual client test

One of the clients must start the server as well.

First client:
`erl -name app@localhost.localdomain -setcookie mycookie`

Second client:
`erl -name extra1@localhost.localdomain -setcookie mycookie -remsh app@localhost.localdomain`
