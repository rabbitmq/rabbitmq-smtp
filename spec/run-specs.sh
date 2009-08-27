#!/bin/sh

which spec >/dev/null
[ "$?" = "1" ] && echo "Test cases require ruby and the rspec gem to be installed" && exit 1

SPEC_DIR=`dirname $0`
spec -c -f s $SPEC_DIR/adapter.rb
