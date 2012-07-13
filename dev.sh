#!/bin/sh
rebar clean && \
rebar get-deps && \
rebar compile && \
erl -pa ebin deps/*/ebin -s sync -s erlmud_server
