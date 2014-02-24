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
            get_value_packet(T);
	_		       ->
	    {error, format_error(Status)}
    end;
get_value(_) ->
    {error, error_unknown_maybe_buffered_stuck}. 


%% Match means is get card id
get_value_packet(<<_Length:2/binary, _Packet:2/binary, _TargetsInit:2/binary, 
	    _TargetNum:2/binary, _Sens:4/binary, _Sel:2/binary, _LengthId:2/binary, CardId/binary>>) ->
    list_to_atom(binary_to_list(CardId));
%% Match means is get firmware version
get_value_packet(<<_Length:2/binary, _FPV:2/binary, FirmwareVersion/binary>>) ->
    list_to_atom(binary_to_list(FirmwareVersion));
get_value_packet(_)						    ->
    {error, error_unknown_maybe_buffered_stuck}.

%% @doc Format error as a single atom
<<<<<<< HEAD
%% @spec format_error(binary()) -> atom()
-spec format_error(binary()) -> atom().
format_error(?RINGBUFFER_INTERRUPT) -> ring_buffer_interrumpt;
format_error(?OVERRUN_ERROR_TAMMA)  -> overrun_error_tamma_side;
format_error(?OVERRUN_ERROR_HOST)   -> overrun_error_host_side;
format_error(?PACKETCHEKSUMWRONG_TAMA)  -> packet_checksum_wrong_tama;
format_error(?PACKETCHEKSUMWRONG_HOST)  -> packet_checksum_wrong_host;
format_error(?UNKNOWNCOMMAND)       -> unknown_command;
format_error(?INTERRUPTWRITE)       -> interrupt_write_overflow;
format_error(?PARAMETEROUT)         -> parameter_out_range;
format_error(?ERRORAPPLICATION)     -> error_application_level;
format_error(?WRONGTAMMAACK)        -> no_or_wrong_command_received;
format_error(?WRONGTAMASET)         -> wrong_command_set_baud_rate;
format_error(?TIMEEXPIRED)          -> time_expired;
format_error(?RINGBUFFER_OVERFLOW)  -> ringbuffer_write_overflow;
format_error(?WRONG_AFTER)          -> wrong_tamma_received_after_sends_command;
format_error(?LENGUAGE_WRONG)       -> host_high_level_language_checksum_wrong;
format_error(?FORMAT_CORRUPTED)     -> block_format_is_corrupted;
format_error(?ERROR_VALUE_BLOCK)    -> error_during_increment_decrement_copy_value_block;
format_error(?NOT_SUPPORTED)        -> baudrate_not_supported_with_the_current_low_speed;
format_error(?EEPROM_READ)          -> internal_eeprom_read_after_write_failed;
format_error(?EEPROM_CHECKSUM)      -> ckecksum_failed_warning_mesage;
format_error(?EEPROM_ADDRESS)       -> internal_eeprom_address_is_out_of_the_allowed_range;
format_error(?EEPROM_LOGIN)         -> internal_eeprom_login_missing;
format_error(?EEPROM_LOGIN_PINCODE) -> internal_eeprom_login_pincode_wrong;
format_error(?PARTYLINEBUFFER)      -> receive_partylinebuffer_overflow;
format_error(?INFOMESSAGE)          -> no_response_available;
format_error(?LCD_BUSY)             -> lcd_timer_expired;
format_error(_)                     -> unknown_error_maybe_buffered_stack.
=======
%% @spec format_error(binary, binary) -> atom()
-spec format_error(binary, binary) -> atom().
format_error(?RESPONSE_PACKET, ?RINGBUFFER_INTERRUPT) -> ring_buffer_interrumpt;
format_error(?RESPONSE_PACKET, ?OVERRUN_ERROR_TAMMA)  -> overrun_error_tamma_side;
format_error(?RESPONSE_PACKET, ?OVERRUN_ERROR_HOST)   -> overrun_error_host_side;
format_error(?RESPONSE_PACKET, ?PACKETCHEKSUMWRONG_TAMA)  -> packet_checksum_wrong_tama;
format_error(?RESPONSE_PACKET, ?PACKETCHEKSUMWRONG_HOST)  -> packet_checksum_wrong_host;
format_error(?RESPONSE_PACKET, ?UNKNOWNCOMMAND)       -> unknown_command;
format_error(?RESPONSE_PACKET, ?INTERRUPTWRITE)       -> interrupt_write_overflow;
format_error(?RESPONSE_PACKET, ?PARAMETEROUT)         -> parameter_out_range;
format_error(?RESPONSE_PACKET, ?ERRORAPPLICATION)     -> error_application_level;
format_error(?RESPONSE_PACKET, ?WRONGTAMMAACK)        -> no_or_wrong_command_received;
format_error(?RESPONSE_PACKET, ?WRONGTAMASET)         -> wrong_command_set_baud_rate;
format_error(?RESPONSE_PACKET, ?TIMEEXPIRED)          -> time_expired;
format_error(?RESPONSE_PACKET, ?RINGBUFFER_OVERFLOW)  -> ringbuffer_write_overflow;
format_error(?RESPONSE_PACKET, ?WRONG_AFTER)          -> wrong_tamma_received_after_sends_command;
format_error(?RESPONSE_PACKET, ?LENGUAGE_WRONG)       -> host_high_level_language_checksum_wrong;
format_error(?RESPONSE_PACKET, ?FORMAT_CORRUPTED)     -> block_format_is_corrupted;
format_error(?RESPONSE_PACKET, ?ERROR_VALUE_BLOCK)    -> error_during_increment_decrement_copy_value_block;
format_error(?RESPONSE_PACKET, ?NOT_SUPPORTED)        -> baudrate_not_supported_with_the_current_low_speed;
format_error(?RESPONSE_PACKET, ?EEPROM_READ)          -> internal_eeprom_read_after_write_failed;
format_error(?RESPONSE_PACKET, ?EEPROM_CHECKSUM)      -> ckecksum_failed_warning_mesage;
format_error(?RESPONSE_PACKET, ?EEPROM_ADDRESS)       -> internal_eeprom_address_is_out_of_the_allowed_range;
format_error(?RESPONSE_PACKET, ?EEPROM_LOGIN)         -> internal_eeprom_login_missing;
format_error(?RESPONSE_PACKET, ?EEPROM_LOGIN_PINCODE) -> internal_eeprom_login_pincode_wrong;
format_error(?RESPONSE_PACKET, ?PARTYLINEBUFFER)      -> receive_partylinebuffer_overflow;
format_error(?RESPONSE_PACKET, ?INFOMESSAGE)          -> no_response_available;
format_error(?RESPONSE_PACKET, ?LCD_BUSY)             -> lcd_timer_expired;
format_error(_,_)                                     -> unknown_error.
>>>>>>> 18f6d9133c2b3dcf9091f4ec7eea49be893d7495
