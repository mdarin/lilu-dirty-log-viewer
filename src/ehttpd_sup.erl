%%%%----------------------------------------------
%%% Module:
%%% Description:
%%% Author:
%%%
%%% wiki page -> [link to wiki or page name]
%%%%----------------------------------------------
-module(ehttpd_sup).
-behaviour(supervisor).

-export([init/1, start/0, start/2, stop/1]).

-record(state, {}).

start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    ->
	ehttpd:stop(), 
	exit(whereis(?MODULE), shutdown).

init(Args) ->
	SupervisorSpecs =  #{strategy => one_for_one,
			intensity => 10,
			period => 1000},
	ChildSpec = [
		#{id => ehttpd,
			start => {ehttpd, start, []},
			restart => permanent,
			shutdown => 2000,
			type => worker,
			modules => [ehttpd]
		}
	],
	{ok, {SupervisorSpecs, ChildSpec}}.


