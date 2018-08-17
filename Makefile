NAME= rpmbuild-order
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

help:
	@echo "devel targets: git-tag sdist version git-push upload copy"

sdist: $(NAME).1 RpmbuildOrder.hs rpmbuild-order.cabal
	./make-dist $(VERSION)

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

version:
	@echo $(VERSION)

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/$(NAME)/master

$(NAME).1: RpmbuildOrder.hs rpmbuild-order.cabal
	LANG=C help2man -N dist/build/$(NAME)/$(NAME) > $@
