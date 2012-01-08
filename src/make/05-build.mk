#-*- mode:makefile-gmake; -*-
BINDIR ?= $(ROOT)/bin
BIN ?= $(BINDIR)/$(TARGET)
COMPONENTS = $(shell find $(ROOT)/src -type f -name '*.lisp' -print)

## Buildapp ##
BUILDAPP ?= $(BINDIR)/buildapp
.PHONY: buildapp
buildapp: $(BUILDAPP)
$(BUILDAPP): | init
	@echo "=> Building buildapp to $(BUILDAPP)"
	$(LISP) --eval '(sb-ext:disable-debugger)' \
            --load $(QL_SETUP) \
	        --eval '(ql:quickload :buildapp)' \
	        --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	        --eval '(quit)'

## Clean
.PHONY: clean-buildapp
clean-buildapp:
	rm -fr $(BUILDAPP)


## Binary ##
.PHONY: clubot
clubot: $(BIN)
$(BIN): $(BUILDAPP) $(ROOT)/$(TARGET).asd $(COMPONENTS) | init
	@echo "=> Building $(TARGET) => $(BIN)"
	$(LISP_PREFIX) $(BUILDAPP) --output $(BIN) \
				               --asdf-path $(ROOT) \
				               --asdf-tree $(ROOT)/vendor \
                               --load $(QL_SETUP) \
                               --load $(ROOT)/src/patches/build.lisp \
				               --require sb-aclrepl \
				               --eval '(ql:quickload :clubot)' \
                               --eval "(setf clubot:*git-revision* \"$$(git rev-parse HEAD || echo UNKNOWN)\")" \
				               --dispatched-entry '/clubot-cli:main' \
	|| { \
	       echo '[ERROR] Build failed!'; \
           rm -f $(BIN); \
           exit 1; \
    }

## Clean
.PHONY: clean-clubot
clean-bin:
	rm -fr $(BIN)

## Developer setup ##
.PHONY: develop
develop: | init asdf
	@echo "=> You should be good to go."

