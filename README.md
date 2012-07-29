
# erlmud

erlmud is an Erlang MUD-like server.

The idea is a game server that multiple clients can play. Intended clients
are:

- a MUD-like telnet client (which will be the initial development focus)
- a nethack-like webpage client
- a Dwarven Fortress GUI-styled graphical client.

This project is an experiment in using Erlang/OTP and other libraries.

# Direction

Initially, scope will be small, focusing on features necessary, before
abstracting.

## Milestones

- A player that can "see" the attributes of rooms and navigate between them
- A serialized form for loading collections of rooms (areas?). Later abstract
  as a general fixture loader
- A client (telnet or web) for interacting as the player
- Multi-player support
- Suggestions?

So far, I have a "room" and a "player", both `gen_server`s for the time being
(eventually, a `gen_event` and `gen_fsm`, respectively).
