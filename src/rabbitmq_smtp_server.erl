%%---------------------------------------------------------------------------
%% Copyright (c) 2007--2010 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007--2010 LShift Ltd. <query@lshift.net>
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

-module(rabbitmq_smtp_server).

-behaviour(application).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-export([start/2, stop/1]).

%% Callbacks.
-export([delivery/3, verify_new_rcpt/2]).

start(normal, []) ->
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    io:format("starting SMTP listener        ..."),
    R = generic_tcp_server:start_link(smtp_server_session, Host, Port,
				  [list,
				   {active, false},
				   {packet, line},
				   {reuseaddr, true}],
				  [{?MODULE, delivery, []},
				   {?MODULE, verify_new_rcpt, []}]),
   io:format("done~n"),
   R.

stop(_State) ->
    ok.

vhost_map(Domain) ->
    {ok, Map} = application:get_env(vhost_map),
    case lists:keysearch(Domain, 1, Map) of
        false ->
            case application:get_env(default_vhosts) of
                true ->
                    list_to_binary(Domain);
                false ->
                    not_found
            end;
        {value, {_, VHostBin}} ->
            {ok, VHostBin}
    end.

split_mailbox(Mailbox) ->
    case string:str(Mailbox, "-") of
        0 ->
            {Mailbox, ""};
        N ->
            {string:substr(Mailbox, 1, N - 1), string:substr(Mailbox, N + 1)}
    end.

map_mailbox({Mailbox, Domain}) ->
    case vhost_map(Domain) of
        {ok, VHost} ->
            {Name, RK} = split_mailbox(Mailbox),
            {ok, rabbit_misc:r(VHost, exchange, list_to_binary(Name)), list_to_binary(RK)};
        not_found ->
            not_found
    end.

verify_new_rcpt(_ReversePath, [Path | _Rest]) ->
    case map_mailbox(Path) of
        {ok, XName, _RK} ->
            case rabbit_exchange:lookup(XName) of
                {ok, _} -> ok;
                {error, not_found} -> not_found
            end;
        not_found ->
            not_found
    end.

delivery(_ReversePath, ForwardPaths, DataLines) ->
    {rfc2822, Headers, BodyLines} = rfc2822:parse(DataLines),
    Properties = lists:foldr(fun add_header/2,
			     #'P_basic'{headers = []},
			     Headers),
    Content = rabbit_basic:build_content(Properties, list_to_binary(BodyLines)),
    deliver(ok, ForwardPaths, #basic_message{content = Content}).

deliver(Status, [], _Message) ->
    Status;
deliver(Status, [Path | Rest], Message) ->
    case map_mailbox(Path) of
        {ok, XName, RK} ->
            case rabbit_basic:publish(
                   rabbit_basic:delivery(false, false, none,
                                         Message#basic_message{exchange_name = XName,
                                                               routing_keys = [RK]},
                                         undefined)) of
                {ok, _, _} -> deliver(Status, Rest, Message);
                _ -> deliver(one_or_more_deliveries_failed, Rest, Message)
            end;
        not_found ->
            deliver(one_or_more_deliveries_failed, Rest, Message)
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
