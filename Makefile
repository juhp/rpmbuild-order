NAME= rpmbuild-order
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

build:
	stack install --test

copy:
	cp -p .hkgr/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/hackage/$(NAME)/
