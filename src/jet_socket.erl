-module(jet_socket).

-export([connect/4]).
-export([connect/5]).
-export([upgrade/3]).
-export([upgrade/4]).
-export([init/2]).

-import(typextfun, [to_hex/1]).

-record(state, {socket,
                owner,
                parser = jet_parser,
                parser_state,
                msg,
                tags = []
               }).

-include("messages.hrl").
-include("elements.hrl").

connect(Host, Port, Name, Passwd) -> connect(Host, Port, Name, Passwd, 5000).
connect(Host, Port, Name, Passwd, Timeout) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {buffer, 65535}], Timeout),
  upgrade(Socket, Name, Passwd, Timeout).

upgrade(Socket, Name, Passwd) -> upgrade(Socket, Name, Passwd, 5000).
upgrade(Socket, Name, Passwd, Timeout) when is_port(Socket) ->
  {ok, Pid} = proc_lib:start_link(?MODULE, init, [Socket, self()]),
  Str = iolist_to_binary(["<?xml version='1.0'?>",
                          "<stream:stream xmlns='jabber:component:accept'	",
                          "xmlns:stream='http://etherx.jabber.org/streams' ",
                          "to='",
                          Name,
                          "'>"]),
  gen_tcp:send(Socket, Str),
  gen_tcp:controlling_process(Socket, Pid),
  receive
    {jet, Pid, {stream_id, Id}} ->
      Challenge = to_hex(crypto:hash(sha, iolist_to_binary([Id, Passwd]))),
      lager:debug("challenge ~p", [Challenge]),
      gen_tcp:send(Socket, <<"<handshake>", Challenge/binary, "</handshake>">>)
  after Timeout ->
          exit({connect, timeout})
  end,
  {ok, Pid}.

init(Socket, Owner) ->
  proc_lib:init_ack({ok, self()}),
  State = #state{socket = Socket, owner = Owner},
  xmerl_sax_parser:stream(<<>>, [
                                 {encoding, utf8},
                                 {event_fun, fun event/3},
                                 {event_state, State},
                                 {continuation_fun, fun loop/1},
                                 {continuation_state, {Socket, Owner}}
                                ]).

loop({Socket, Owner} = State) ->
  lager:debug("receive loop"),
  Result = receive
             {tcp, Socket, Msg} ->
               lager:info("tcp msg ~p", [Msg]),
               {reply, {Msg, State}};
             {tcp_closed, Socket} ->
               throw(connection_closed);
             {send, Owner, Data} when is_binary(Data) ->
               send(Socket, Data);
             Msg ->
               erlang:apply(?MODULE, handle_info, [Msg, State])
           end,
  case Result of
    {reply, Value} -> Value;
    _ -> loop(State)
  end.

send(Socket, Msg) ->
  lager:debug("sending data ~p", [Msg]),
  gen_tcp:send(Socket, Msg).

handle(Msg, State) ->
  lager:info("sending msg ~p", [Msg]),
  State#state.owner ! {jet, self(), Msg},
  State.

event(#startElement{
         uri = "http://etherx.jabber.org/streams",
         lname = "stream",
         qname={"stream", "stream"},
         attributes = Attributes},
      _, #state{parser = Parser} = State) ->
  {ok, ParserState} = Parser:init(),
  [{_Uri, _Prefix, "id", Id}] = [X || X <- Attributes, element(3, X) =:= "id"],
  handle({stream_id, Id}, State),
  State#state{parser_state = ParserState};

event(Element, _Location, #state{msg = Msg, parser = Parser, parser_state = PS} = State)
  when
    element(1, Element) =:= startElement
    ->
  NewState = pre_event(Element, State),
  {NewMsg, NewParserState} = Parser:element(Element, Msg, PS),
  NewState#state{msg = NewMsg, parser_state = NewParserState};

event(Element, _Location, State)
  when
    element(1, Element) =:= endElement
    ->
  post_event(Element, State);

event(El, _, State)
  when
    element(1, El) =:= startPrefixMapping;
    element(1, El) =:= ignorableWhitespace;
    element(1, El) =:= endPrefixMapping;
    El =:= startDocument
    -> State;

event(Elem, _Loc, State) ->
  lager:warning("element ~p", [Elem]),
  lager:warning("state ~p", [State]),
  State.

pre_event(#startElement{qname = QName}, #state{tags = Tags} = State) ->
  State#state{tags = [QName | Tags]}.

post_event(#endElement{qname = QName},
           #state{
              tags = [QName],
              msg = Msg
             } = State)
  when
    Msg =/= undefined
    ->
  handle(Msg, State),
  State#state{msg = undefined, tags = []};

post_event(#endElement{qname = QName}, #state{tags = [QName | Tags]} = State) ->
  State#state{tags = Tags}.
