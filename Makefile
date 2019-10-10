NAME= rpmbuild-order

$(NAME).1: Main.hs rpmbuild-order.cabal
	LANG=C help2man -N dist/build/$(NAME)/$(NAME) > $@
