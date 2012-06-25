REBAR=./rebar
ERLDOCS=./erldocs
DIALYZER=dialyzer
DOC=doc

.PHONY: all compile get-deps clean distclean config doc dialyzer

all: compile

$(REBAR):
	true

$(ERLDOCS):
	true

compile: get-deps $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	rm -rfv ebin deps plts

release: $(REBAR)
	cd rel && ../$(REBAR) generate

get-deps: $(REBAR)
	$(REBAR) get-deps

doc: $(ERLDOCS)
	$(ERLDOCS) $(DOC)

## dialyzer
## you have to compile project first
plts/otp.plt: ~/.dialyzer_plt
	mkdir -p plts && cp ~/.dialyzer_plt plts/otp.plt

plts/deps.plt: plts/otp.plt
	$(DIALYZER) --add_to_plt --plt plts/otp.plt --output_plt plts/deps.plt -r deps

dialyzer: plts/deps.plt
	$(DIALYZER) --plt plts/deps.plt -n ebin 

#\
#	-Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_fun_app \
#	-Wunderspecs -Wno_opaque -Wno_return -Wno_unused -Wno_improper_lists
