-module(arygon_nfc_cmds).
-export([sendTama/1]).

sendTama(CommandTama) ->
        responseOk(<<"FF01001C4B010100440007048A9A6AD02980\n\n">>).

responseOk(<<H:6/binary,T/binary>>) ->
		passHead(H).
passHead(<<_:2/binary,Error:2/binary,_/binary>>) ->
	checkHead(Error).

checkHead(<<"00">>) ->
	io:format("OK = no Error."),
	ok;
checkHead(<<"01">>) ->
	io:format("ERROR [ingbuffer, interrupt write overflow (T AMA side)]");
checkHead(<<"02">>) ->
	io:format("ERROR UART1 receiver framing or overrun error (TAMA side).");
checkHead(<<"03">>) ->
	io:format("ERROR UART2 receiver framing or overrun error (host side).");
checkHead(<<"04">>) ->
	io:format("ERROR TAMA receive packet checksum wrong (packet length or packet data).");
checkHead(<<"05">>) ->
	io:format("ERROR Host receive packet checksum wrong (packet length or packet data).");
checkHead(<<"06">>) ->
	io:format("ERROR Unknown mode-select command from host.");
checkHead(<<"07">>) ->
	io:format("ERROR Ringbuffer, interrupt write overflow (host side).");
checkHead(<<"08">>) ->
	io:format("ERROR One or more command parameter are out of range.");
checkHead(<<"09">>) ->
	io:format("TAMA has detected an error at application level.");
checkHead(<<"0A">>) ->
	io:format("No or wrong TAMA ack received after sending of TAMA command sequences.");
checkHead(<<"0B">>) ->
        io:format("Wrong TAMA set baud rate response received after TAMA set baud rate command.");
checkHead(<<"0C">>) ->
	io:format("Host or TAMA Command packet supervision Timer expired. Packet not complete within 1s.");
checkHead(<<"0D">>) ->
	io:format("Ringbuffer write overflow (host side).");
checkHead(<<"0E">>) ->
	io:format("No or wrong TAMA ack received after μC sends a command to TAMA.");
checkHead(<<"0F">>) ->
	io:format("Host High level language checksum wrong.");
checkHead(<<"10">>) ->
        io:format("Current block has no value block format (block format is corrupted).");
checkHead(<<"11">>) ->
        io:format("Error during increment / decrement / copy value block.");
checkHead(<<"12">>) ->
          io:format("Baudrate not supported with the current low speed (low power) crystal.");
checkHead(<<"13">>) ->
          io:format("Internal EEPROM read after write failed.");
checkHead(<<"14">>) ->
          io:format("Internal EEPROM checksum failed (warning message).");
checkHead(<<"15">>) ->
          io:format("Internal EEPROM address is out of the allowed range.");
checkHead(<<"16">>) ->
          io:format("Internal EEPROM login missing. Application has no write access authorization.");
checkHead(<<"17">>) ->
          io:format("Internal EEPROM login pincode wrong. Access denied.");
checkHead(<<"18">>) ->
          io:format("Receive partylinebuffer overflow (from TAMA to μC).");
checkHead(<<"19">>) ->
          io:format("Infomessage: No response available (partylinebuffer is empty).");
checkHead(<<"1A">>) ->
          io:format("Optional LCD Busy check supervision Timer expired").

responseReceived(<<Status:6/binary,Length:2/binary, Packet:2/binary, TargetsInit:2/binary,TargetNum:2/binary, Sens:4/binary, Sel:2/binary, LengthID:2/binary, ID/binary>>) ->
	io:format("Status OK ~p~n Length ~p~n Packet ~p~n TargetsInit ~p~n TargetNum ~p~n Sens ~p~n Sel ~p~n Length ID ~p~n ID ~p~n ~n",
[Status,Length,Packet,TargetsInit,TargetNum,Sens,Sel,LengthID,ID]).
