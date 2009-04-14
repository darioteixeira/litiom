#
# Configuration options
#

PKG_NAME=litiom

SRC_DIR=src

LIB_BUILD_DIR=$(SRC_DIR)/_build
LIB_TARGETS=litiom.cma litiom.cmxa litiom.a litiom_blocks.cmi litiom_wizard.cmi
LIB_FQTARGETS=$(foreach TARGET, $(LIB_TARGETS), $(LIB_BUILD_DIR)/$(TARGET))

OCAMLBUILD_OPTS=

#
# Rules
#

all: lib

lib:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) $(LIB_TARGETS)

apidoc: lib
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambdoc.docdir/index.html

install: lib
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(LIB_FQTARGETS)

uninstall:
	ocamlfind remove $(PKG_NAME)

reinstall: lib
	ocamlfind remove $(PKG_NAME)
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(LIB_FQTARGETS)

clean:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) -clean

