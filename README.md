arygon_nfc - NFC reader erlang application
======

Compile the application

    $ make all

arygon_nfc have a dependence "gnuart", is necessary start it before. start applications with the correct path
     $ erl -pa ebin/ deps/gnuart/ebin
     
     1> application:start(gnuart).
     ok

Start arygon_nfc
     
    2> application:start(arygon_nfc).
    ok

Open the device 

    3> arygon_nfc:open().
    arygon nfc got: <<"device_open">>
    ok

Subscribe the process to the application

    4> arygon_nfc:subscribe().
    {ok, #Ref<0.0.0.39>}
    
All messages sent to the subscribed process are in the form:

    {nfc, Ref, Response} 

Where Ref is the reference for the subscribed process and Response is the decoded 
packet.
    
    
Send a TAMA  command to the reader

    5> arygon_nfc:send("0av").
    arygon nfc got value: 'V6.6'
    ok

Unsubscribe the process

    6> arygon_nfc:unsusbribe().
    ok

Close the device

    7> arygon_nfc:close().
    arygon nfc got: <<"device_close">>
    ok
