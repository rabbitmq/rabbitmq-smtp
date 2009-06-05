%%---------------------------------------------------------------------------
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(rabbit_smtp_server).

-include_lib("rabbitmq_server/include/rabbit.hrl").
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

-export([start/0]).

%% Callbacks.
-export([delivery/3, verify_new_rcpt/2]).

start() ->
    generic_tcp_server:start_link(smtp_server_session, "0.0.0.0", 8025,
				  [list,
				   {active, false},
				   {packet, line},
				   {reuseaddr, true}],
				  [{?MODULE, delivery, []},
				   {?MODULE, verify_new_rcpt, []}]).

vhost_map(<<"localhost">>) -> <<"/">>;
vhost_map(X) -> X.

exchange_name({Mailbox, VHost}) ->
    rabbit_misc:r(vhost_map(list_to_binary(VHost)), exchange, list_to_binary(Mailbox)).

verify_new_rcpt(_ReversePath, [Path | _Rest]) ->
    case rabbit_exchange:lookup(exchange_name(Path)) of
	{ok, _} -> ok;
	{error, not_found} -> not_found
    end.

delivery(_ReversePath, ForwardPaths, DataLines) ->
    {rfc2822, Headers, BodyLines} = rfc2822:parse(DataLines),
    Properties = lists:foldr(fun add_header/2,
			     #'P_basic'{headers = []},
			     Headers),
    Message =
	#basic_message{content =
		       #content{class_id = element(1, rabbit_framing:method_id('basic.publish')),
				properties = Properties,
				properties_bin = none,
				payload_fragments_rev = [list_to_binary(BodyLines)]}},
    deliver(ok, ForwardPaths, Message).

deliver(Status, [], _Message) ->
    Status;
deliver(Status, [Path | Rest], Message) ->
    case rabbit_basic:publish(false, false, none,
                              Message#basic_message{exchange_name = exchange_name(Path),
                                                    routing_key = <<>>}) of
	{ok, _, _} -> deliver(Status, Rest, Message);
	_ -> deliver(one_or_more_deliveries_failed, Rest, Message)
    end.

add_header({Key, Value}, P = #'P_basic'{headers = H}) ->
    UpperKey = string:to_upper(Key),
    ValueBin = list_to_binary(Value),
    case UpperKey of
	"MESSAGE-ID" -> setelement(#'P_basic'.message_id, P, ValueBin);
	"CONTENT-TYPE" -> setelement(#'P_basic'.content_type, P, ValueBin);
	"CONTENT-TRANSFER-ENCODING" -> setelement(#'P_basic'.content_encoding, P, ValueBin);
	_ -> P#'P_basic'{headers = [{list_to_binary(Key), longstr, ValueBin} | H]}
    end.
