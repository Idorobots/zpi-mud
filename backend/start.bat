@echo off
set path=%path%;C:\erl5.10.3\bin;C:\erl5.10.3\erts-5.10.3\bin

if "%1" == "" (
    goto fail
)
(
   if not "%2" == "" (
       goto fail
   )
   erl -config mud -pa ebin edit deps\bear\ebin  deps\cowboy\ebin  deps\eredis\ebin  deps\folsom\ebin  deps\goldrush\ebin  deps\hive\ebin  deps\ibrowse\ebin  deps\jesse\ebin  deps\jsonx\ebin  deps\lager\ebin  deps\meck\ebin  deps\poolboy\ebin  deps\ranch\ebin -boot start_sasl -sname mud_dev -s mud start %*
   goto :eof
)

:fail
echo USAGE: start.bat path/to/game/resources/
