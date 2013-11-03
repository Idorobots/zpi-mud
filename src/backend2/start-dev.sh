#!/bin/sh
exec erl -config mud \
     -pa ebin edit deps/*/ebin \
     -boot start_sasl \
     -sname mud_dev \
     -s mud start $@
