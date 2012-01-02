#-*- mode:makefile-gmake; -*-
.DEFAULT_GOAL=help
VERBOSITY ?= 2 # [0, 3]

# Helper
SHUTUP = > /dev/null 2> /dev/null

.PHONY: init
init: | sanity-check quicklisp submodules externals

.PHONY: | sanity-check
sanity-check: $(ROOT)/$(TARGET).asd $(LISP_BIN) $(QL_SETUP)
	@echo "!> Environment looks sane. I'll allow this."

.PHONY: externals
externals: | submodules
	$(MAKE) -C $(ROOT)/vendor all

.PHONY: externals-clean
externals-clean:
	$(MAKE) -C $(ROOT)/vendor clean

.PHONY: submodules
submodules:
	(cd $(ROOT); git submodule update --init --recursive;)
