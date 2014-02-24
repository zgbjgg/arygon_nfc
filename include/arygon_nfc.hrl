%%
%% Copyright (C) 2014 
%% Authors: Patricia Nuñez cpattynue@gmail.com<>
%%          Jorge Garrido <zgbjgg@gmail.com>
%% All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%%

%% Common purpose
-define(CARRIER_RETURN, <<"\n">>).
-define(EMPTY_BIN, <<"">>).
-define(RESPONSE_PACKET, <<"FF">>).
-define(DEVICE_OPEN, <<"device_open">>).
-define(DEVICE_CLOSE, <<"device_close">>).

%% Incoming websockets commands
-define(GET_CARD_ID, <<"get_card_id">>).
-define(GET_FIRMWARE_VERSION, <<"get_firmware_version">>).

%% TAMA commands
-define(GET_CARD_ID_TAMA, "0s").
-define(GET_FIRMWARE_VERSION_TAMA, "0av").

%% Code for errors
-define(RINGBUFFER_INTERRUPT, <<"01">>).
-define(OVERRUN_ERROR_TAMMA, <<"02">>).
-define(OVERRUN_ERROR_HOST, <<"03">>).
-define(PACKETCHEKSUMWRONG_TAMA, <<"04">>).
-define(PACKETCHEKSUMWRONG_HOST, <<"05">>).
-define(UNKNOWNCOMMAND, <<"06">>).
-define(INTERRUPTWRITE, <<"07">>).
-define(PARAMETEROUT, <<"08">>).
-define(ERRORAPPLICATION, <<"09">>).
-define(WRONGTAMMAACK, <<"0A">>).
-define(WRONGTAMASET, <<"0B">>).
-define(TIMEEXPIRED, <<"0C">>).
-define(RINGBUFFER_OVERFLOW, <<"0D">>).
-define(WRONG_AFTER, <<"0E">>).
-define(LENGUAGE_WRONG, <<"0F">>).
-define(FORMAT_CORRUPTED, <<"10">>).
-define(ERROR_VALUE_BLOCK, <<"11">>).
-define(NOT_SUPPORTED, <<"12">>).
-define(EEPROM_READ, <<"13">>).
-define(EEPROM_CHECKSUM, <<"14">>).
-define(EEPROM_ADDRESS, <<"15">>).
-define(EEPROM_LOGIN, <<"16">>).
-define(EEPROM_LOGIN_PINCODE, <<"17">>).
-define(PARTYLINEBUFFER, <<"18">>).
-define(INFOMESSAGE, <<"19">>).
-define(LCD_BUSY, <<"1A">>).
<<<<<<< HEAD

%% gpio
-define(BEEP_ON, "echo 1 > /sys/class/leds/beep/brightness").
-define(BEEP_OFF, "echo 0 > /sys/class/leds/beep/brightness").
-define(TURN_ON_NFC_LED, "echo 1 > /sys/class/leds/d9/brightness").
-define(TURN_OFF_NFC_LED, "echo 0 > /sys/class/leds/d9/brightness"). 
=======
>>>>>>> 18f6d9133c2b3dcf9091f4ec7eea49be893d7495
