bindir := $(shell pwd)

# installation of rcg binary should be in the current directory
CABAL_CONF_FLAGS = --prefix=$(bindir) --bindir=$(bindir)
ifdef DEBUG
  CABAL_CONF_FLAGS += -fDebug
endif

rcg: *.hs *.y RCGen.cabal
	cabal install --only-dependencies
	cabal configure
	cabal build
	cabal install $(CABAL_CONF_FLAGS)
# first run configure/build to check if dependencies are installed
# otherwise, with `cabal install $(CABAL_CONF_FLAGS)`, packages would be installed automatically in the current directory

clean:
	cabal clean
	rm -f rcg
