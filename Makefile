NAME= rpmbuild-order
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

$(NAME).1: src/Main.hs rpmbuild-order.cabal
	LANG=C help2man -N ~/github/rpmbuild-order/dist-newstyle/build/x86_64-linux/ghc-$(shell ghc --numeric-version)/rpmbuild-order-$(VERSION)/x/rpmbuild-order/build/$(NAME)/$(NAME) > $@

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/hackage/$(NAME)/

stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts build
	@echo
	stack --resolver lts-15 build
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
