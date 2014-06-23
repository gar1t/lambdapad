PROJECT = lpad

DEPS = jiffy erlydtl getopt erlmarkdown

dep_jiffy = https://github.com/davisp/jiffy.git 0.9.0
dep_erlydtl = https://github.com/erlydtl/erlydtl.git 0.8.0
dep_getopt = https://github.com/jcomellas/getopt.git
dep_erlmarkdown = https://github.com/erlware/erlmarkdown.git

ERLC_OPTS = -Ddevmode

include erlang.mk

shell:
	ERL_LIBS=deps; erl -pa ebin -s lpad_reloader
