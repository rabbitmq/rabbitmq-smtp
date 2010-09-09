UPSTREAM_GIT=http://github.com/tonyg/erlang-smtp.git
# You need to update the website when you change the revision.
REVISION=69e2621aa334669d3241

EBIN_DIR=ebin
INCLUDE_DIR=include

LIB_PACKAGE=erlang-smtp-$(REVISION)
LIB_PACKAGE_DIR=$(LIB_PACKAGE)
LIB_PACKAGE_NAME=$(LIB_PACKAGE).ez

CHECKOUT_DIR=$(LIB_PACKAGE_DIR)-git

TARGETS=$(LIB_PACKAGE_NAME)

all: $(TARGETS)

clean:
	rm -rf $(EBIN_DIR)
	rm -rf $(INCLUDE_DIR)
	rm -rf $(LIB_PACKAGE_DIR) $(TARGETS)

distclean: clean
	rm -rf $(CHECKOUT_DIR)

$(LIB_PACKAGE_DIR): $(CHECKOUT_DIR)
	cp -r $< $@

$(CHECKOUT_DIR):
	git clone $(UPSTREAM_GIT) $@
	(cd $@ && git checkout $(REVISION)) || rm -rf $@

%.ez: $(LIB_PACKAGE_DIR)
	$(MAKE) -C $(LIB_PACKAGE_DIR) clean all
	rm -rf ebin include
	cp -r $(LIB_PACKAGE_DIR)/ebin $(EBIN_DIR)
	cp -r $(LIB_PACKAGE_DIR)/include $(INCLUDE_DIR)
	zip $(LIB_PACKAGE_NAME) $(LIB_PACKAGE_DIR)/
	zip -r $(LIB_PACKAGE_NAME) $(LIB_PACKAGE_DIR)/$(EBIN_DIR)/
