NAME= rpmbuild-order
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

build:
	cabal install
	cabal test
	LANG=C help2man -N $(NAME) > $(NAME).1

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/hackage/$(NAME)/

stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts-16 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
	@echo
	stack --resolver lts-12 --stack-yaml stack-lts12.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts12.yaml build
	@echo
	stack --resolver lts-10 --stack-yaml stack-lts12.yaml build
