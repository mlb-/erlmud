
APP=erlmud

.PHONY: clean deps compile dev

all: compile

clean:
	@./rebar clean

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

dev: clean compile
	erl -pa ebin deps/*/ebin -s sync -s $(APP)
