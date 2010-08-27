PACKAGE=rabbitmq-smtp
APPNAME=rabbitmq_smtp_server
DEPS=rabbitmq-server rabbitmq-erlang-client
INTERNAL_DEPS=erlang-smtp
TEST_APPS=rabbitmq_smtp_server
START_RABBIT_IN_TESTS=true
TEST_SCRIPTS=spec/run-specs.sh

include ../include.mk

clean::
	rm -rf tmp
