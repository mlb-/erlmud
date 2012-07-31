
APP=erlmud

.PHONY: clean deps compile dev

all: compile

clean:
	@./rebar clean

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

dev: compile
	erl -pa ../erlmud/ebin deps/*/ebin -s $(APP)
