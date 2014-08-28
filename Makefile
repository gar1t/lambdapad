PROJECT = lpad

DEPS = jiffy erlydtl getopt

dep_jiffy = https://github.com/davisp/jiffy.git 0.9.0
dep_erlydtl = https://github.com/erlydtl/erlydtl.git 0.8.0
dep_getopt = https://github.com/jcomellas/getopt.git

ERLC_OPTS = +debug_info
COMPILE_FIRST = lpad_generator lpad_data_loader

include erlang.mk

all:: deps/mmd

clean-all:: clean-mmd

mmd_repo = https://github.com/fletcher/MultiMarkdown-4.git

deps/mmd:
	git clone $(mmd_repo) deps/mmd
	cd deps/mmd && git submodule init && git submodule update && make

clean-mmd:
	rm -rf deps/mmd

shell:
	ERL_LIBS=deps; erl -pa ebin -s lpad_reloader
