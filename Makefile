APP = lifeguard_graphite

all: deps compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps

devrel: rel
	rm -rf rel/$(APP)/lib/$(APP)-*/ebin
	ln -sf $(abspath ./apps/$(APP)/ebin) rel/$(APP)/lib/$(APP)-*
	rm -rf rel/$(APP)/lib/$(APP)-*/priv
	ln -sf $(abspath ./apps/$(APP)/priv) rel/$(APP)/lib/$(APP)-*
	echo -s sync | tee -a rel/$(APP)/releases/*/vm.args
	mkdir -p dev
	test -f dev/sys.config || cp rel/$(APP)/releases/*/sys.config dev/
	ln -sf $(abspath ./dev/sys.config) rel/$(APP)/releases/*/sys.config

rel: compile
	./rebar generate -f

test: compile
	./rebar eunit apps=$(APP)

.PHONY: all compile clean deps
