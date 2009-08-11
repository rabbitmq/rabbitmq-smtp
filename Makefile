PACKAGE=rabbitmq-smtp
DEPS=rabbitmq-server erlang-smtp

include plugin-include.mk

plugin-include.mk:
	curl http://hg.rabbitmq.com/rabbitmq-public-umbrella/raw-file/default/include.mk > $@

distclean: clean
	rm -rf $(DIST_DIR)
	rm -f plugin-include.mk
