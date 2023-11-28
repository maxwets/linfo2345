ERLC=/usr/bin/erlc

ERL=/usr/bin/erl
ERL_ARGS=-pa ebin -noshell

SRC=$(wildcard *.erl)
BEAM=$(SRC:.erl=.beam)

all: run

$(BEAM): $(SRC)
	$(ERLC) $(SRC)

run: $(BEAM)
	$(ERL) $(ERL_ARGS) -eval "network:init(200)"

clean:
	rm -f $(BEAM) *.dump log/*.log
