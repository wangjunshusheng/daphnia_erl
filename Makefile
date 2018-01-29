REBAR   = rebar3
PROJECT = daphnia_erl

all: compile

compile:
		@$(REBAR) compile

clean:
		@$(REBAR) clean
			rm -f $(PROJECT).tar.gz

distclean:
		@$(REBAR) clean -a
			rm -f $(PROJECT).tar.gz

dialyzer:
		@$(REBAR) dialyzer

test:
		@$(REBAR) ct -v

travis-test:
		@$(REBAR) ct

update:
		@$(REBAR) update

.PHONY: compile clean dialyzer test update