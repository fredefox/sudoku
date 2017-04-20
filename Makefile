ERLC=erlc
SRCDIR=src
BEAMDIR=./ebin

build: 
	mkdir -p $(BEAMDIR) ;
	$(ERLC) -o $(BEAMDIR) $(SRCDIR)/*.erl ;

clean: 
	rm -rf $(BEAMDIR) ;
	rm -rf erl_crush.dump

benchmark: build
	cp $(SRCDIR)/problems.txt $(BEAMDIR)
	cd $(BEAMDIR) \
	  && erl -noshell \
	       -s sudoku benchmarks \
	       # -s sudoku pbenchmarks \
	       # -s sudoku poolbenchmarks \
	       -s init stop
