#
# This makefile contains command line tools to maintain this project
# Please consult the documentation in doc/index.html for more user oriented information
# Portability at this level is accidental, only LispWorks on Mac OS X is supported here
# For some operations, edit the necessary variables to suit your environment
# Some operations can obviously only be done by a specific person in a very specific context ;-)
#

default:
	@echo Welcome to S-HTTP-CLIENT, a Basic HTTP Client
	@echo
	@echo Possible makefile targets:
	@echo
	@echo clean-fasl --- remove all known lisp compiled fasl files recursively
	@echo clean-emacs --- remove all '*~' recursively
	@echo clean --- all of the above
	@echo dist-clean --- remove all generated files and archives
	@echo compile --- compile the project through ASDF
	@echo check --- run all unit and functional tests for this project
	@echo test --- run all unit and functional tests for this project
	@echo dist --- create a source tarball for distribution
	@echo release --- make a formal, public release
	@echo sync-darcs --- synchronize local and remote darcs repositories
	@echo metrics --- calculate some loc metrics
	@echo
	@echo Please consult the documentation in doc/index.html for more information

clean-fasl:
	find . -name "*.fas" | xargs rm
	find . -name "*.lib" | xargs rm
	find . -name "*.nfasl" | xargs rm
	find . -name "*.dfsl" | xargs rm
	find . -name "*.fasl" | xargs rm

clean-emacs:
	find . -name "*~" | xargs rm

clean: clean-fasl clean-emacs

dist-clean: clean
	rm -rf *.tar.gz
	rm -rf *.asc

metrics:
	find src -name "*.lisp" | xargs wc -l
	find test -name "*.lisp" | xargs wc -l

LISP=/Applications/LispWorks/lispworks-tty
PRJ=s-http-client

compile:
	echo "(asdf:oos 'asdf:compile-op :$(PRJ)) :ok" | $(LISP) 

DIR=`pwd`/
SRCDIR=$(DIR)src/
TESTDIR=$(DIR)test/

test: check

check: 
	echo "(asdf:oos 'asdf:load-op :$(PRJ)) (load \"$(TESTDIR)all-tests.lisp\") :ok" | $(LISP)

dist:
	darcs dist 

IDISK=/Volumes/svc

release: test dist clean
	gpg -a -b $(PRJ).tar.gz
	mkdir -p $(IDISK)/Sites/$(PRJ)/
	cp $(PRJ).tar.gz $(IDISK)/Sites/$(PRJ)/
	cp $(PRJ).tar.gz.asc $(IDISK)/Sites/$(PRJ)/
	cp doc/* $(IDISK)/Sites/$(PRJ)/

USER=
HOST=
RPATH=~/Sites/website/darcs/

sync-darcs: clean
	cd ..; rsync -va -e ssh $(PRJ) $(USER)@$(HOST):$(RPATH)

# EOF
