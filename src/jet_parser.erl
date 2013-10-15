-module(jet_parser).

-include("messages.hrl").
-include("elements.hrl").

-export([init/0]).
-export([handle_info/2]).
-export([element/3]).

init() ->
	{ok, undefined}.

handle_info(_Msg, State) ->
	{ok, State}.

element(#startElement{lname = "iq", attributes = Attributes}, undefined, State) ->
	Attr = [{X, Y} || {_, _, X, Y} <- Attributes],
	From = proplists:get_value("from", Attr),
	To = proplists:get_value("to", Attr),
	Type = proplists:get_value("type", Attr),
	Id = proplists:get_value("id", Attr),
	{#iq{from = From, to = To, type = Type, id = Id}, State};

element(#startElement{lname = "presence", attributes = Attributes}, undefined, State) ->
	Attr = [{X, Y} || {_, _, X, Y} <- Attributes],
	From = proplists:get_value("from", Attr),
	To = proplists:get_value("to", Attr),
	Type = proplists:get_value("type", Attr),
	{#presence{from = From, to = To, type = Type}, State};
	
element(#startElement{
				 uri = Uri,
				 lname = LName},
			#iq{} = Msg, State) ->
	{Msg#iq{q = {LName, Uri}}, State};

element(#startElement{lname = "remove"}, #iq{} = Msg, State) ->
	{Msg#iq{remove = true}, State};

element(#startElement{} = _El, Msg, State) ->
	{Msg, State};

element({characters, Value}, Msg, State) ->
	{set(Msg, Value), State};

element(_, Msg, State) ->
	{Msg, State}.

set(Msg, _Value) -> Msg.
