%%%-------------------------------------------------------------------
%%% @author koctep
%%% @copyright (C) 2013, koctep
%%% @doc
%%%
%%% @end
%%% Created : 2013-10-14 00:58:24.168542
%%%-------------------------------------------------------------------
-module(jet).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
				 handle_call/3,
				 handle_cast/2,
				 handle_info/2,
				 terminate/2,
				 code_change/3]).

-record(state, {socket, name}).

-include("messages.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Host, Port, Passwd) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Name, Host, Port, Passwd], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, Host, Port, Passwd]) ->
	process_flag(trap_exit, true),
	{ok, Socket} = jet_socket:connect(Host, Port, Name, Passwd),
	{ok, #state{socket = Socket, name = Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({jet, Socket, Msg},
						#state{
							 socket = Socket,
							 name = Name} = State)
	when
		element(2, Msg) =:= Name
		->
	NewState1 = case catch handle_xmpp(Msg, State) of
		{reply, Reply, NewState} ->
			send(Socket, iolist_to_binary(Reply)),
			NewState;
		{noreply, NewState} -> NewState
	end,
	{noreply, NewState1};

handle_info({'EXIT', Socket, Reason}, #state{socket = Socket} = State) ->
	{stop, Reason, State};

handle_info(_Info, State) ->
	lager:warning("unhandled info msg ~p", [_Info]),
	{noreply, State}.

handle_xmpp(#iq{
							 q = {"query", "http://jabber.org/protocol/disco#info"},
							 type = "get"
							} = Msg,
						State) ->
	lager:debug("disco#info"),
	Reply = reply(Msg, [ 
											"<identity category='gateway' ",
											"type='skype' ",
											"name='Skype Gateway'/>",
											"<feature var='http://jabber.org/protocol/disco#info'/>",
											"<feature var='jabber:iq:register'/>"
										 ]),
	{reply, Reply, State};

handle_xmpp(#iq{
							 q = {"query", "jabber:iq:register"},
							 type = "get"
							} = Msg,
						State) ->
	lager:debug("get jabber:iq:register"),
	Reply = reply_success(Msg),
	%	{ok, Str} = reply(Msg,
	%										[
	%										 "<instructions>",
	%										 "Press any key."
	%										 "</instructions>"]),
	{reply, Reply, State};

handle_xmpp(#iq{
							 q = {"query", "http://jabber.org/protocol/disco#items"},
							 type = "get"
							} = Msg,
						State) ->
	lager:debug("get jabber:iq:register"),
	Reply = reply_success(Msg),
	%	{ok, Str} = reply(Msg,
	%										[
	%										 "<instructions>",
	%										 "Press any key."
	%										 "</instructions>"]),
	{reply, Reply, State};

handle_xmpp(#iq{
							 from = From,
							 to = Name,
							 q = {"query", "jabber:iq:register"},
							 remove = undefined,
							 type = "set"
							} = Msg,
						State) ->
	lager:debug("set jabber:iq:register"),
	[JID, _] = re:split(From, "/"),
	Msg1 = reply_success(Msg),
	Msg2 = [
					"<presence type='subscribe' "
					"from='", Name, "' ",
					"to='", JID, "'>",
					"<nick>Skype transport</nick></presence>"
				 ],
	{reply, [Msg1, Msg2], State};

handle_xmpp(#iq{
							 q = {"query","jabber:iq:gateway"},
							 type = "get"
							} = Msg,
						State) ->
	Reply = reply(Msg, [
											"<desc>Enter skype login</desc>",
											"<login>Skype login</login>",
											"<email>Email</email>"
										 ]),
	{reply, Reply, State};

%handle_xmpp(#iq{
%							 q = {"prompt","jabber:iq:gateway"},
%							 type = "set"
%							} = Msg,
%						State) ->
%	Reply = reply(Msg, [

handle_xmpp(#presence{
							 type = "subscribe",
							 from = From,
							 to = Name
							},
						State) ->
	lager:debug("presence subscribe"),
	Reply = [
				 "<presence type='subscribed' ",
				 "from='", Name, "' ",
				 "to='", From, "'/>"
				],
	{reply, Reply, State};

handle_xmpp(#iq{q = {"vCard", _}} = Msg, State) ->
	{reply, reply(Msg, ["<NICKNAME>Skype transport</NICKNAME>"]), State};

handle_xmpp(#iq{} = Msg, State) ->
	lager:debug("unhandled iq msg ~p", [Msg]),
	{reply, reply_error(Msg), State};

handle_xmpp(Msg, State) ->
	lager:debug("unhandled xmpp msg ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(Pid, Msg) ->
	Pid ! {send, self(), Msg}.

reply_success(#iq{from = From, to = Name, id = Id, q = {Field, Query}}) ->
	[
	 "<iq type='result' ",
	 "from='", Name, "' ",
	 "to='", From, "' ",
	 "id='", Id, "'>",
	 "<", Field, " xmlns='", Query, "'/></iq>"
	].

reply(#iq{from = From, to = Name, id = Id, q = {Field, Query}}, Reply) ->
	[
	 "<iq type='result' ",
	 "from='", Name, "' ",
	 "to='", From, "' ",
	 "id='", Id, "'>",
	 "<", Field, " xmlns='", Query, "'>",
	 Reply,
	 "</", Field, ">",
	 "</iq>"
	].

reply_error(Msg) ->
	reply(Msg, [
							"<error type='cancel'>",
							"<service-unavailable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>",
							"</error>"
						 ]).
