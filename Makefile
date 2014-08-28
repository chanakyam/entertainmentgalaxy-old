PROJECT = showbiz

DEPS = lager cowboy erlydtl jsx ibrowse

dep_lager = https://github.com/basho/lager.git master
dep_cowboy = https://github.com/extend/cowboy master
dep_erlydtl = https://github.com/evanmiller/erlydtl.git master
dep_jsx = https://github.com/talentdeficit/jsx.git master
dep_ibrowse = https://github.com/cmullaparthi/ibrowse.git master

include erlang.mk

shell:
	erl -pa ebin -pa deps/*/ebin -eval "[application:start(App) || App <- [syntax_tools, compiler, jsx, ibrowse, erlydtl, goldrush,lager, ranch, cowlib, crypto, cowboy, showbiz ] ]."
