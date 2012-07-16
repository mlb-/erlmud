# Revial

Currently, due to the age and neglect, there is a revival branch `revival`
where current development is taking place.

# ErlMUD

ErlMUD is a MUD-like server written in erlang.

The driving idea is to write a game that can be played from multiple clients.
At the time of writing, this means a MUD-like telnet client (which will be the
initial development focus), a nethack-like webpage client (using something fun,
like websockets/socket.io(-erlang)), and a Dwarven Fortress GUI-styled
graphical client.

This project is an experiment into using Erlang/OTP and other libraries.

# Structure

## World

The base world is a collection of areas. An area is a collection of rooms. This
provides the initial idea of:

world :: supervisor(areas :: supervisor(rooms :: gen\_server))

## Areas

To be implemented similar to rooms, as described below. Change "room" to "area".

## Rooms

Each room will be a process, managed under a supervisor. If a room has no
action for whatever timeout, it will hibernate (trying not to pre-maturely
optimize). (Failing to do so:) Possibly add hibernating room to a ETS table
sorted by time, so to be able to apply yet another timeout, after which,
terminate hibernating room.

On request for room, an ETS table lookup is done for the process (gproc
instead, perhaps?). If the room does not exist, the supervisor starts it,
records the entry, and returns the PID.

