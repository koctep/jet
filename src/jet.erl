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
-export([start_link/3]).
-export([behaviour_info/1]).
-export([reply/2]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {socket, name, module, mod_state}).

-include("messages.hrl").

%%%===================================================================
%%% API
%%%===================================================================
behaviour_info(callbacks) ->
	[
	 {init, 2}
	].

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module, Args, Opts) ->
	gen_server:start_link(?MODULE, [Module | Args], Opts).
start_link(ServerName, Module, Args, Opts) ->
	gen_server:start_link(ServerName, ?MODULE, [Module | Args], Opts).

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
init([Module, Name, Host, Port, Passwd | Opts]) ->
	process_flag(trap_exit, true),
	{ok, Socket} = jet_socket:connect(Host, Port, Name, Passwd),
	{ok, ModState} = Module:init(Name, Opts),
	{ok, #state{socket = Socket, name = Name, module = Module, mod_state = ModState}}.

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
handle_info(
			{jet, Socket, Msg},
			#state{
				module = Module,
				mod_state = ModState,
				socket = Socket} = State) ->
	lager:debug("handling message ~p", [Msg]),
	{ok, Callback, Args} = case catch get_callback(Msg) of
		{ok, C, A} -> {ok, C, A};
		{ok, C} -> {ok, C, []};
		_				->
			lager:warning("callback not defined for ~p", [Msg]),
			{ok, handle_message, [Msg]}
	end,
	lager:debug("calling ~p:~p(~p)", [Module, Callback, [Msg | Args] ++ [ModState]]),
	Answer = case catch erlang:apply(Module, Callback, [Msg | Args] ++ [ModState]) of
		{reply, Reply, NewState} ->
			{reply, reply(Msg, Reply), NewState};
		{noreply, NewState} ->
			{noreply, NewState};
		{'EXIT', Reason} ->
			lager:warning("exception in ~p:~p(~p) ~p", [Module, Callback, [Msg | Args] ++ [ModState], Reason]),
			Reply = reply(Msg, [
							"<error type='cancel'>",
							"<service-unavailable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>",
							"</error>"
						 ]),
			{reply, Reply, State}
	end,
	NewState1 = case Answer of
		{noreply, NS}	->
			NS;
		{reply, R, NS} ->
			send(Socket, iolist_to_binary([R])),
			NS
	end,
	{noreply, State#state{mod_state = NewState1}};

handle_info({'EXIT', Socket, Reason}, #state{socket = Socket} = State) ->
	{stop, Reason, State};

handle_info(_Info, State) ->
	lager:warning("unhandled info msg ~p when ~p", [_Info, State]),
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


%%%===================================================================
%%% Getting callback function and arguments
%%%===================================================================
get_callback(#iq{
				q = {"query", "http://jabber.org/protocol/disco#info"},
				type = "get"
				}) ->
	{ok, disco_info};

get_callback(#iq{
				q = {"query", "jabber:iq:register"},
				type = "get"
				}) ->
	{ok, register_info};
%	lager:debug("get jabber:iq:register"),
%	Reply = reply_success(Msg),
	%	{ok, Str} = reply(Msg,
	%										[
	%										 "<instructions>",
	%										 "Press any key."
	%										 "</instructions>"]),
%	{reply, Reply, State};

%get_callback(#iq{
%				q = {"query", "http://jabber.org/protocol/disco#items"},
%				type = "get"
%				}) ->
%	{ok, disco_items};

%get_callback(#iq{
%				from = From,
%				to = Name,
%				q = {"query", "jabber:iq:register"},
%				remove = undefined,
%				type = "set"
%				}) ->
%	{ok, register};

%get_callback(#iq{
%				q = {"query","jabber:iq:gateway"},
%				type = "get"
%				} = Msg) ->
%	Reply = reply(Msg, [
%											"<desc>Enter skype login</desc>",
%											"<login>Skype login</login>",
%											"<email>Email</email>"
%										 ]),
%	{ok,  xmpp_address};

%get_callback(#iq{
%							 q = {"prompt","jabber:iq:gateway"},
%							 type = "set"
%							} = Msg,
%						State) ->
%	Reply = reply(Msg, [

%get_callback(#presence{
%				type = "subscribe",
%				from = From,
%				to = Name
%				}) ->
%	lager:debug("presence subscribe"),
%	Reply = [
%			"<presence type='subscribed' ",
%			"from='", Name, "' ",
%			"to='", From, "'/>"
%			],
%	{ok, presence};

get_callback(#iq{q = {"vCard", _}} = Msg) ->
	reply(Msg, ["<NICKNAME>Skype transport</NICKNAME>"]),
	{ok, vcard}.
