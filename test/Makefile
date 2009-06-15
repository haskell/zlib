HSC=ghc
CC=gcc # can also use $(HSC)

UNCOMPRESSED=`find .. -type f`
GZIPPED=`find .. -type f -name \*.gz`

ALL:

clean:
	rm -f hszpipe zpipe *.o *.hi

bins: hszpipe zpipe hsgzip hsgunzip

cabal-configure:
	cd .. && cabal configure

# TODO: need to handle the Stream.hsc -> Stream.hs conversion
cabal-build: cabal-configure
	cd .. && cabal build

hs%: ../examples/%.hs cabal-build
	$(HSC) -lz -i../dist/build -i.. -o $@ --make $<

test-hs: Test.hs cabal-build
	$(HSC) -lz -i. -i../dist/build -i.. -o $@ --make -main-is Test.main $<

zpipe: zpipe.c
	$(CC) -lz -o zpipe zpipe.c

test: run-test-hs run-test-sh-zpipe run-test-sh-gzip run-test-sh-gunzip

run-test-hs: test-hs
	./test-hs

run-test-sh-zpipe: zpipe hszpipe
	./zpipe-compress.sh $(UNCOMPRESSED)

run-test-sh-gzip: hsgzip hsgunzip
	./gzip-compress.sh $(UNCOMPRESSED)

run-test-sh-gunzip: hsgunzip
	./gzip-uncompress.sh $(GZIPPED)