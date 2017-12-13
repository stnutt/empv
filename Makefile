EMACS ?= emacs
EMACSFLAGS = -Q --batch

LOADDEFS = empv-loaddefs.el
SRC = $(filter-out $(LOADDEFS),$(wildcard *.el))
OBJ = $(SRC:.el=.elc)
TEST = $(wildcard test/*.el)

.PHONY: all clean test

all: $(OBJ) $(LOADDEFS)

%.elc: %.el
	@$(EMACS) $(EMACSFLAGS) \
		-L . \
		-f batch-byte-compile $<

$(LOADDEFS): $(SRC)
	@$(EMACS) $(EMACSFLAGS) \
		--eval '(setq generated-autoload-file "$(PWD)/$@")' \
		--eval '(setq backup-inhibited t)' \
		-f batch-update-autoloads $(PWD)

clean:
	@rm -rf $(OBJ) $(LOADDEFS)

test: $(OBJ) $(TEST)
	@$(EMACS) $(EMACSFLAGS) \
		-L . $(addprefix -l ,$(TEST)) \
		-f ert-run-tests-batch-and-exit
