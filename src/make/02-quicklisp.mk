#Quicklisp
QL_ROOT_NAME ?= quicklisp
QL_URL ?= "https://github.com/quicklisp/quicklisp-bootstrap/raw/master/quicklisp.lisp"
QL_ROOT_PATH = $(HOME)/$(QL_ROOT_NAME)
QL_SETUP = $(QL_ROOT_PATH)/setup.lisp

quicklisp: $(LISP_BIN) $(QL_SETUP) quicklisp-test

quicklisp-test:
	@echo "=> Verifying Quicklisp load ...\c"
	@$(LISP) --eval '(sb-ext:disable-debugger)' \
			 --load $(QL_SETUP) \
	         --eval '(quit :unix-status (if (find-package :ql) 0 1))' $(SHUTUP) \
	         || { E=$$?; echo " [ERROR]"; exit $$E; }
	@echo " [OK]"

$(QL_SETUP):
	@echo "=> Installing quicklisp"
	@curl   -L $(QL_URL) > /tmp/quicklisp.lisp; \
		$(LISP) --eval '(sb-ext:disable-debugger)' --load /tmp/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_ROOT_PATH)/")' \
		--eval '(quit)';

quicklisp-rc:
	@$(LISP) --eval '(sb-ext:disable-debugger)' \
			 --eval '(quit :unix-status (if (find-package :ql) 0 1))' $(SHUTUP) \
	 && { echo "[error] You already have Quicklisp in your rc"; exit 1; } \
	 || { $(MAKE) quicklisp-rc-install; }

quicklisp-rc-install: $(QL_SETUP)
	@echo "=> Installing quicklisp into your RC file"
	@$(LISP) --eval '(sb-ext:disable-debugger)' \
	  		 --load $(QL_SETUP) \
		     --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
			 --eval '(quit)' $(SHUTUP)
