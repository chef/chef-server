all: index.html

NPM = $(shell which npm)
RAML2HTML = $(shell which raml2html)

index.html:
ifeq ($(NPM),)
	@echo "You don't have node.js installed."
	@echo "It's up to you to decide the best way of doing that for your system"
else
	@if ! [ -f $(RAML2HTML) ] ; then \
		echo "You don't have raml2html, but we'll assume you want it" ; \
		$(NPM) install -g raml2html; \
    fi;
	$(RAML2HTML) base.yml > index.html
endif

clean:
	@rm *.html
