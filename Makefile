BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/tanodb
APPNAME = tanodb
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p _build/default/rel/tanodb_data/
	mkdir -p _build/default/rel/tanodb_config/
	cp _build/default/rel/tanodb/etc/* _build/default/rel/tanodb_config/

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release
	mkdir -p _build/dev1/rel/tanodb_data/
	mkdir -p _build/dev1/rel/tanodb_config/
	cp _build/dev1/rel/tanodb/etc/* _build/dev1/rel/tanodb_config/

devrel2:
	$(REBAR) as dev2 release
	mkdir -p _build/dev2/rel/tanodb_data/
	mkdir -p _build/dev2/rel/tanodb_config/
	cp _build/dev2/rel/tanodb/etc/* _build/dev2/rel/tanodb_config/

devrel3:
	$(REBAR) as dev3 release
	mkdir -p _build/dev3/rel/tanodb_data/
	mkdir -p _build/dev3/rel/tanodb_config/
	cp _build/dev3/rel/tanodb/etc/* _build/dev3/rel/tanodb_config/

devrel: devrel1 devrel2 devrel3

dev-attach1:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME) attach

dev-attach2:
	$(BASEDIR)/_build/dev2/rel/tanodb/bin/$(APPNAME) attach

dev-attach3:
	$(BASEDIR)/_build/dev3/rel/tanodb/bin/$(APPNAME) attach

dev-console1:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME) console

dev-console2:
	$(BASEDIR)/_build/dev2/rel/tanodb/bin/$(APPNAME) console

dev-console3:
	$(BASEDIR)/_build/dev3/rel/tanodb/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/tanodb/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/tanodb/bin/$(APPNAME)-admin cluster join tanodb1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/tanodb/bin/$(APPNAME) ping; true; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/tanodb/bin/$(APPNAME) stop; true; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

console:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) console

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

