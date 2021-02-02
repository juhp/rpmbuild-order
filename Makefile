NAME= rpmbuild-order
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

build:
	stack install --test
	LANG=C help2man -N $(NAME) > $(NAME).1

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/hackage/$(NAME)/
