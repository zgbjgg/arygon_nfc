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
-module(arygon_nfc_cmds).

-export([split/1]).

-include("arygon_nfc.hrl").

%% @doc Splits the response to get the value 
%% @spec split(Got :: binary()) -> atom()
-spec split(Got :: binary()) -> atom().
split(?DEVICE_OPEN)  -> ok;
split(?DEVICE_CLOSE) -> ok;
split(Got) 	     ->
    Binary = re:replace(Got, ?CARRIER_RETURN, ?EMPTY_BIN, [{return, binary}, global]),
    get_value(Binary).


%% @doc Check if command is ok
%% @spec get_value(binary()) -> atom()
-spec get_value(binary()) -> atom().
get_value(<<"FF", Status:2/binary, Status1:2/binary, T/binary>>) ->  
    case {Status, Status1} of
        {<<"00">>, <<"00">>} ->
            get_value(T);
	_		       ->
	    format_error(Status, Status1)
    end;
%% Match means is get card id
get_value(<<_Length:2/binary, _Packet:2/binary, _TargetsInit:2/binary, 
	    _TargetNum:2/binary, _Sens:4/binary, _Sel:2/binary, _LengthId:2/binary, CardId/binary>>) ->
    list_to_atom(binary_to_list(CardId));
%% Match means is get firmware version
get_value(<<_Length:2/binary, _FPV:2/binary, FirmwareVersion/binary>>) ->
    list_to_atom(binary_to_list(FirmwareVersion));
get_value(_)						    ->
    error_unknown.

%% @doc Format error as a single atom
%% @spec format_error(binary, binary) -> atom()
-spec format_error(binary, binary) -> atom().
format_error(?RESPONSE_PACKET, ?RINGBUFFER_INTERRUPT) -> ring_buffer_interrumpt;
format_error(?RESPONSE_PACKET, ?OVERRUN_ERROR)        -> overrun_error. 
