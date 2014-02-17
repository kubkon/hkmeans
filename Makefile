HC = ghc
LIBSRC = KMeans.hs
LIBOBJ = $(patsubst %.hs, %.o, $(LIBSRC))

.PHONY: all
all: $(LIBOBJ) 
	$(HC) Main.hs

$(LIBOBJ): $(LIBSRC)
	$(HC) $^

.PHONY: clean
clean:
	rm -rf *.hi *.o Main

