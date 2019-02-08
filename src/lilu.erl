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
-module(lilu).
-behaviour(gen_server).

%DEBUG
-compile([export_all]).


% user API
-export([list/0, start_viewer/2, stop_viewer/1]).

%% server control API
-export([start_link/0, stop/0, start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, lilu_viewer_manager).
-define(LILUPORT_DEFAULT, 45813).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
	start_link(). 

stop() ->
	%exit(whereis(?SERVER), shutdown).
	gen_server:cast(?SERVER, stop).

list() ->
	gen_server:call(?SERVER, viewers_list).
	
start_viewer(ViewerName, ViewerSpec) ->
	gen_server:cast(?SERVER, {start_viewer,ViewerName,ViewerSpec}).

stop_viewer(ViewerName) ->
	gen_server:cast(?SERVER, {stop_viewer,ViewerName}).

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
	% state contains pairs viewer ~ pid(InetPid)
	%{ok, #state{}}.
	%{ok,#{}).
	{ok,[]}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(viewers_list = Request, _From, State) ->
% get running viewer workers list
	Reply = {viewers,State}, %ok,
	%io:format("~p: req -> ~p list -> ~p~n",[?MODULE, Request, Reply]),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	io:format("~p:call default ~p~n",[?MODULE, _Request]),
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_viewer,ViewerName,ViewerSpec}, State) ->
% function to start new viewer worker with name ViewerName
	%io:format("~p:start ~p~n spec -> ~p~n",[?MODULE,ViewerName,ViewerSpec]),
	io:format(" ** LiLu starring viever: ~p **~n", [ViewerName]),
	ViewerPid = run_viewer(ViewerName, ViewerSpec),
	NewState = [{ViewerName,ViewerPid}|State],
	{noreply, NewState};

handle_cast({stop_viewer,ViewerName}, State) ->
% function to stop viewer worker with name ViewerName
	NewState = case proplists:is_defined(ViewerName, State) of
		true ->
			io:format(" ** LiLu stopping viever: ~p **~n", [ViewerName]),
			remove_viewer(ViewerName),
			proplists:delete(ViewerName, State);
		_ -> 
			io:format(" ** LiLu stopping skipped~n",[]),
			io:format(" ** unknown viewer: ~p~n", [ViewerName]),
			State
	end,
	{noreply, NewState};

handle_cast({restart_viewer,ViewerName}, State) ->
% function to restart viewer worker with name ViewerName
	io:format("~p:restart ~p~n",[?MODULE,ViewerName]),
	{noreply, State};

handle_cast({configure_viewer,ViewerName}, State) ->
% function to configure/reconfigure viewer worker with name ViewerName
	io:format("~p:configure ~p~n",[?MODULE,ViewerName]),
	{noreply, State};

handle_cast(stop, State) ->
% stop the server LiLu 
	io:format(" ** now LiLu with name ~p is stopping  **~n", [?SERVER]),
	io:format(" ** active viewers that will terminated:~n~p~n", [State]),
	% in this case terminate do not call!
	% lilu_sup:stop_child(lilu),
	{stop, normal, State};

% default handler
handle_cast(_Msg, State) ->
	io:format("~p:cast default ~p~n",[?MODULE,_Msg]),
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
terminate(Reason, State) ->
	%TODO: terminates all instances 
	io:format(" ** LiLu terminates with reason: ~p **~n", [Reason]),
	io:format(" ** stopping yet active viewers:~n", []),
	lists:foreach(
		fun({ViewerName,ViewerPid}) ->
				io:format(" ~p~n", [ViewerName]),
				remove_viewer(ViewerName),
				proplists:delete(ViewerName, State);
			(_Viewer) -> 
				ok 
		end
	, State), 
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

% prepare and run instance of httpd server
run_viewer(ViewerName, ViewerSpec) ->
	% prepare httpd configuration
	HttpdSpec = case is_map(ViewerSpec) of
		true -> 
			%io:format("~p:start ~p~n spec is map -> ~p~n",[?MODULE,ViewerName,ViewerSpec]),
			#{
				bind_address := BindAddress,
				port := Port,
				root := Root,
				server_name := ViewerName,
				server_root := SrvRoot,
				document_root := DocRoot,
				directory_index := DirIndex
			} = ViewerSpec,
			%create httpd spec from map
			[
				{bind_address, BindAddress},
				{port, Port},
				{server_name, ViewerName},
				{server_root, SrvRoot},
				{document_root, DocRoot},
				{directory_index, DirIndex}
			];
		_ -> 
			%io:format("~p:start ~p~n spec is proplist -> ~p~n", [?MODULE,ViewerName,ViewerSpec]),
			BindAddress = proplists:get_value(bind_address, ViewerSpec),
			Port = proplists:get_value(port, ViewerSpec),
			Root = proplists:get_value(root, ViewerSpec),
			ViewerName = proplists:get_value(server_name, ViewerSpec),
			SrvRoot	= proplists:get_value(server_root, ViewerSpec),
			DocRoot = proplists:get_value(document_root, ViewerSpec),
			DirIndex = proplists:get_value(directory_index, ViewerSpec),
			%create httpd spec from proplist
			[
				{bind_address, BindAddress},
				{port, Port},
				{server_name, ViewerName},
				{server_root, SrvRoot},
				{document_root, DocRoot},
				{directory_index, DirIndex}
			]
	end,

	% start httpd instance 
	ChildSpec = #{
		id => ViewerName, %TODO: <T>_to_atom(ViewerName) ??
		start => {inets, start, [httpd, HttpdSpec, stand_alone]},
		restart => permanent,
		shutdown => 25000,
		type => worker,
		modules => [inets]
	},
	{ok,ViewerPid} = lilu_sup:start_child(ViewerName, ChildSpec),
	io:format("~p:viewer -> ~p~n", [?MODULE, ViewerPid]),
	ViewerPid.


% stop viewer instance of httpd server
remove_viewer(VeiwerName) ->
	lilu_sup:stop_child(VeiwerName).

