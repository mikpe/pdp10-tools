REBAR3=$(shell type -p rebar3 || echo ./rebar3)
REBAR3_GIT=https://github.com/erlang/rebar3.git
REBAR3_VSN=3.7.5

PROGRAMS=8to9 od

default: compile link

compile:	$(REBAR3)
	$(REBAR3) compile

link:	$(REBAR3)
	for PROG in $(PROGRAMS); do \
		rm -f _build/$$PROG; \
		ln -s default _build/$$PROG; \
		$(REBAR3) as $$PROG escriptize; \
	done

distclean realclean:	clean
	rm -f ./rebar3

clean:	$(REBAR3)
	$(REBAR3) clean
	rm -rf _build

./rebar3:
	mkdir -p _build; \
	cd _build; \
	git clone --quiet $(REBAR3_GIT); \
	cd rebar3; \
	git checkout --quiet $(REBAR3_VSN); \
	./bootstrap; \
	mv rebar3 ../../; \
	cd ../..; \
	rm -rf _build/rebar3
