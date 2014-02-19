
arygon_nfc - Reader  erlang applicationi
======

Compile the application and build the shared object

    $ make all

Arygon_nfc have a dependence "gnuart", is necessarily start the dependence to use, start applications with the load files
     $ erl -pa ebin/ deps/gnuart/ebin
     
     1> application:start(gnuart).
     ok

Start arygon_nfc
     
    2> application:start(arygon_nfc).
    ok

Open the device 

    3> arygon_nfc:open().
    ok

Subscribe the process to the application

    4> arygon_nfc:subscribe().

Sends a TAMA  command to the reader

    5> arygon_nfc:sends("0av").
    06V0.1
