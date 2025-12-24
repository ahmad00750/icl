SLYNK_DIR := $(wildcard ocicl/sly-*/slynk)

.PHONY: all clean lint test

all: icl

slynk.zip: $(SLYNK_DIR)/*
	@echo "Creating slynk.zip..."
	sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd)))))" \
	     --eval "(asdf:load-system :zip)" \
	     --eval "(zip:zip \"slynk.zip\" \"$(SLYNK_DIR)/\" :if-exists :supersede)" \
	     --quit

icl: slynk.zip src/*.lisp *.asd
	sbcl --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd))) (list :tree (merge-pathnames \"3rd-party/\" (uiop:getcwd)))))" \
	     --eval "(asdf:make :icl)" --quit

clean:
	rm -rf *~ icl slynk.zip

lint:
	ocicl lint icl.asd

test:
	sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd))) (list :tree (merge-pathnames \"3rd-party/\" (uiop:getcwd)))))" \
	     --eval "(asdf:load-system :icl-tests :force t)" \
	     --eval "(let ((results (fiveam:run 'icl-tests:icl-tests))) (format t \"~&~%Test Results:~%\") (fiveam:explain! results) (finish-output) (if (fiveam:results-status results) (sb-ext:exit :code 0) (sb-ext:exit :code 1)))"
