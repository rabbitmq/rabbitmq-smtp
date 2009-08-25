PACKAGE=rabbitmq-smtp
DEPS=rabbitmq-server rabbitmq-erlang-client
INTERNAL_DEPS=erlang-smtp
TEST_APPS=rabbitmq_smtp_server
START_RABBIT_IN_TESTS=true

include ../include.mk

clean::
	rm -rf tmp
