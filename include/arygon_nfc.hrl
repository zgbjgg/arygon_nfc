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
-define(OVERRUN_ERROR, <<"02">>).
