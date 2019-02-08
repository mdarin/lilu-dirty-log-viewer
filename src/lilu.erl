%%%%----------------------------------------------
%%% Module: LiLu log viewer worker
%%% Description:
%%% Author:
%%%
%%% wiki page -> [link to wiki or page name]
%%%%----------------------------------------------

%%%
%%% A simple "Hello, world" server in the Erlang.
%%%

%https://stackoverflow.com/questions/2206933/how-to-write-a-simple-webserver-in-erlang

%% documentation
% https://erldoc.com/doc/docs-17.4/inets/httpd.html


%Keep in mind, this exposes your /tmp directory.

%To run, simply:
%$ escript ./hello_erlang.erl
%

%%%-------------------------------------------------------------------
%%% File    : gen_server_template.full
%%% Author  : my name <yourname@localhost.localdomain>
%%% Description : 
%%%
%%% Created :  2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module().

-behaviour(gen_server).

% user API
-export([main/1, run_server/0, stop/0, start/0]).

%% server control API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
% get running viewer workers list
	Reply = ok,
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_viewer,ViewerName}, State) ->
% function to start new viewer worker with name ViewerName
	io:format("~p:start ~p~n",[?MODULE,ViewerName]),
	{noreply, State};

handle_cast({stop_viewer,ViewerName}, State) ->
% function to stop viewer worker with name ViewerName
	io:format("~p:stop ~p~n",[?MODULE,ViewerName]),
	{noreply, State};

handle_cast({restart_viewer,ViewerName}, State) ->
% function to restart viewer worker with name ViewerName
	io:format("~p:restart ~p~n",[?MODULE,ViewerName]),
	{noreply, State};

handle_cast({configure_viewer,ViewerName}, State) ->
% function to configure/reconfigure viewer worker with name ViewerName
	io:format("~p:configure ~p~n",[?MODULE,ViewerName]),
	{noreply, State};

% default handler
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


main(_) ->
	start(),
	receive
		stop -> ok
	end.

run_server() ->

	% get working directory
	{ok,CargoRoot} = file:get_cwd(),
	SrvRoot = lists:concat([CargoRoot,"/test/log"]),
	DocRoot = lists:concat([CargoRoot,"/test/log"]),

	case inets:start() of
		ok -> {ok,inets};
		{error,{already_started,inets}} -> {ok,inets} 
	end,


  case inets:start(httpd, [
		{port, 35000},
		{server_name, "ctlogviewer"},
		%{server_root, "/tmp"},
		%{document_root, "/tmp"},
		{server_root, SrvRoot},
		{document_root, DocRoot},
		{directory_index, ["index.hml", "welcome.html"]},
	{bind_address, "localhost"}
	], stand_alone) of 
		{ok, Inets} -> {ok,Inets};
		{error,{already_started,Inets}} -> {ok,Inets}
	end.

start() -> start_link(). 
%run_server().                                                       

stop() ->
	%inets:stop(),
	%TODO:gen_server:cast(stop)
	io:format("ehttpd worker stopped~n",[]).

