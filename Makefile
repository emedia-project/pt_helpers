ERL					?= erl
ERLC				= erlc
REL_DIR     = rel
NODE				= {{name}}
REL					= {{name}}
APPS        = apps
SCRIPT_PATH  := $(REL_DIR)/$(NODE)/bin/$(REL)
REBAR       = ./rebar

.PHONY: compile rel get-deps doc test

all: compile

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

test:
	@$(REBAR) skip_deps=true eunit

rel: compile
	@$(REBAR) generate

doc:
	@rm -rf doc
	@mkdir doc
	@cp _doc/* doc
	$(REBAR) skip_deps=true doc
	@cp _doc/stylesheet.css doc

dev:
	@erl -pa ebin 

analyze: checkplt
	@$(REBAR) skip_deps=true dialyze

buildplt:
	@$(REBAR) skip_deps=true build-plt

checkplt: buildplt
	@$(REBAR) skip_deps=true check-plt
