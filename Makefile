HC     = ghc
FLAGS  = -O2 
LIBSRC = KMeans.hs
LIBOBJ = $(patsubst %.hs, %.o, $(LIBSRC))

.PHONY: all
all: $(LIBOBJ) 
	$(HC) Main.hs $(FLAGS)

$(LIBOBJ): $(LIBSRC)
	$(HC) $^ $(FLAGS)

.PHONY: clean
clean:
	rm -rf *.hi *.o Main

