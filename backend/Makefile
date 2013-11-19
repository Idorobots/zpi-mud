# Some Rebar related env:
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar

RESOURCES=../resources

all: build

build:
	@$(REBAR) get-deps compile

run:
	@sh start.sh $(RESOURCES)

clean:
	@$(REBAR) clean
