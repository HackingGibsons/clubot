# Lithp thtuff
LISP_BIN ?= $(shell which sbcl || echo /sbcl/does/not/exist)
LISP_PREFIX ?= CL_SOURCE_REGISTRY='$(ROOT):$(ROOT)/vendor//'
LISP ?= $(LISP_PREFIX) $(LISP_BIN)

# Asdf configuration
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d

lisp-clean:
	@echo "=> Clearing common-lisp cache"
	rm -rf ~/.cache/common-lisp/

lisp-fasl-clean:
	@echo "=> Clearing fasls from $(ROOT)"
	find $(ROOT) -name '*.fasl' -delete
