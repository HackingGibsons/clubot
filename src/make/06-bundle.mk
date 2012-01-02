#-*- mode:makefile-gmake; -*-
STAGEDIR ?= $(ROOT)/build
BUNDLE ?= $(BINDIR)/clubot.bundle.tgz

.PHONY: bundle
bundle: $(BUNDLE)
$(BUNDLE): $(STAGEDIR)
	@echo "=> Writing bundle to: $(BUNDLE)"
	tar cz -C $(STAGEDIR) -f $(BUNDLE) .

$(STAGEDIR): $(BIN)
	@echo "=> Preparing release in $(STAGEDIR)"
	mkdir -p $(STAGEDIR)/bin
	mkdir -p $(STAGEDIR)/lib
	cp $(BIN) $(STAGEDIR)/bin

	@echo "=> Archiving revision"
	echo `git rev-parse HEAD` > $(STAGEDIR)/REV

	@echo "=> Staging select FFI build libs"
	find $(QL_ROOT_PATH) $(ROOT)/vendor $(HOME)/.cache/common-lisp/ \
	     -name '*.dylib' -or -name '*.so' | xargs -I % cp -v % $(STAGEDIR)/lib

	@echo "=> Building bootstrap script: $(STAGEDIR)/$(TARGET)"
	touch $(STAGEDIR)/$(TARGET)
	@echo '#!/bin/sh' > $(STAGEDIR)/$(TARGET)
	@echo 'BASE=$$(dirname $$(greadlink -f $$0 2> /dev/null || readlink -f $$0 2> /dev/null))' >> $(STAGEDIR)/$(TARGET)
	@echo 'export CLUBOT="$$BASE/$$(basename $$0)"' >> $(STAGEDIR)/$(TARGET)
	@echo 'LD_LIBRARY_PATH="$$BASE/lib":$$LD_LIBRARY_PATH $$BASE/bin/'$(TARGET) '$$@' >> $(STAGEDIR)/$(TARGET)
	chmod +x $(STAGEDIR)/$(TARGET)

.PHONY: clean-all-bundle clean-stagedir clean-bundle
clean-all-bundle: | clean-stagedir clean-bundle
clean-stagedir:
	rm -rf $(STAGEDIR)
clean-bundle:
	rm -rf $(BUNDLE)
