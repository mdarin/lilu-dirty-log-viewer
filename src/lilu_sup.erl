%%%%----------------------------------------------
%%% Module: LiLu log viewr app supervisor
%%% Description:
%%% Author:
%%%
%%% wiki page -> [link to wiki or page name]
%%%%----------------------------------------------
-module(lilu_sup).
-behaviour(supervisor).

-define(LILUSUP, ?MODULE).

% API 
-export([start_child/2, stop_child/1, count_children/0, which_children/0]).

% Supervisor control API
-export([init/1, start/0, start/2, stop/1]).

-record(state, {}).

start() -> 
	start(normal,[]).

start(_,_) -> 
	supervisor:start_link({local,?LILUSUP},?LILUSUP,[]).

stop(_)    ->
	lilu:stop(), 
	exit(whereis(?LILUSUP), shutdown).

init(Args) ->
	SupervisorSpecs =  #{strategy => one_for_one,
			intensity => 10,
			period => 10000},
	ChildSpec = [
		#{id => lilu,
			%start => {lilu, start, []},
			start => {lilu, start_link, []},
			restart => permanent,
			shutdown => 2000,
			type => worker,
			modules => [lilu]
		}
	],
	{ok, {SupervisorSpecs, ChildSpec}}.

start_child(ChildId, ChildSpec) -> 
	%io:format("~p ::name -> ~p~n", [?MODULE, ChildId]),
	%io:format("~p ::spec -> ~p~n", [?MODULE, ChildSpec]),

	% start inet service before
	%FIXME: or maybe it's better to call application:ensure_all_started(intes) ???
	%case inets:start() of
%		ok -> {ok,inets};
%		{error,{already_started,inets}} -> {ok,inets} 
%	end,
	
	% start the configured child process 
	Pid = case supervisor:start_child(?LILUSUP, ChildSpec) of
		{ok,P} ->  P;  
		{error,{already_started,_}} -> 
			ok = supervisor:terminate_child(?LILUSUP, ChildId),
			ok = supervisor:delete_child(?LILUSUP, ChildId),
			{ok,P} = supervisor:start_child(?LILUSUP, ChildSpec),
			P;
		{error,already_present} -> 
			ok = supervisor:terminate_child(?LILUSUP, ChildId),
			ok = supervisor:delete_child(?LILUSUP, ChildId),
			{ok,P} = supervisor:start_child(?LILUSUP, ChildSpec),
			P
	end,
	{ok,Pid}.

stop_child(ChildId) -> 
	ok = supervisor:terminate_child(?LILUSUP, ChildId),
	ok = supervisor:delete_child(?LILUSUP, ChildId).

count_children() ->
	supervisor:count_children(?LILUSUP).

which_children() ->
	supervisor:which_children(?LILUSUP).

