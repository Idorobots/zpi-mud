#!/bin/sh

die () {
    echo >&2 "$@"
    exit 1
}

[ "$#" -eq 1 ] || die "USAGE: ./start.sh 'path/to/game/resources/'"

exec erl -config mud \
    -pa ebin edit deps/*/ebin \
    -boot start_sasl \
    -sname mud_dev \
    -s mud start $@