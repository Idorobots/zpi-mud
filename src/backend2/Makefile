# Some Rebar related env:
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar

all: build

build:
	@$(REBAR) get-deps compile

clean:
	@$(REBAR) clean
