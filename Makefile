all: Main doc/index.html

Main: *.hs
	ghc --make Main

doc/index.html: Text/LogFormat.hs
	haddock -h -o doc $<

test:
	runhaskell LogFormatTests.hs

clean:
	-rm -rf doc Main *.hi *.o
