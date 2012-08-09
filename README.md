# erlmud

erlmud is an Erlang MUD-like server.

The idea is a game server that multiple clients can play.
Considered clients are:

- Telnet. Like the old MUDs before. Also, a low hanging fruit to pick for
	early development.
- Some sort of web-driven client that modern browsers can access
- A thick client. GUI-style, I'm taking inspiration from Dwarven Fortress.

## Dependencies

This project is an experiment in using Erlang/OTP and other libraries.  I
currently am making use of:

- [sync](https://github.com/rustyio/sync) for developmental reloading
- [esupervisor](https://github.com/spawngrid/esupervisor) for being able to
	manipulate supervisor architecture quickly and easily
- [ranch](https://github.com/extend/ranch) for telnet!


# Direction

Initially, scope will be small, focusing on as few features as necessary to
(hopefully) make a non-trivial implementation and abstract after pain points
are found.

## Initial revival milestones

- A player that can "see" the attributes of rooms and navigate between them
- A serialized form for loading collections of rooms (areas?). Later abstract
  as a general fixture loader
- A client telnet for interacting as the player
- Multi-player support

As the above revial milestones have been met, it's time to merge into master!

## Still on the todo list

- Use [cowboy](https://github.com/extend/cowboy) to create an initial web
	client interface
- Abstract groups of rooms under an "area" collection
- Write a parser for [stock areas](http://download.ansalonmud.net/area/) to
	test "area"s with
