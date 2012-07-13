
APP=erlmud_server

.PHONY: clean get-deps compile dev


clean:
	rebar clean

get-deps:
	rebar get-deps

compile:
	rebar compile

dev: clean get-deps compile
	erl -pa ebin deps/*/ebin -s $(APP) -s sync

