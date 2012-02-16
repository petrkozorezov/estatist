REBAR=rebar
ERLDOCS=erldocs
DOC=doc

.PHONY: all compile get-deps clean distclean config doc

all: compile

$(REBAR):
	true

$(ERLDOCS):
	true

compile: get-deps $(REBAR)
	./$(REBAR) compile

clean: $(REBAR)
	./$(REBAR) clean

distclean: clean
	rm -rfv ebin deps

release: $(REBAR)
	cd rel && ../$(REBAR) generate

get-deps: $(REBAR)
	./$(REBAR) get-deps

doc: $(ERLDOCS)
	./$(ERLDOCS) $(DOC)

