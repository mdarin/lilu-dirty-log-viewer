%%%
%%% A simple "Hello, world" server in the Erlang.
%%%

%https://stackoverflow.com/questions/2206933/how-to-write-a-simple-webserver-in-erlang

%% documentation
% https://erldoc.com/doc/docs-17.4/inets/httpd.html


%Keep in mind, this exposes your /tmp directory.

%To run, simply:
%$ escript ./hello_erlang.erl

-module(ehttpd).
-export([
  main/1,
  run_server/0,
	stop/0,
  start/0
]).

main(_) ->
  start(),
  receive
    stop -> ok
  end.

run_server() ->
  case inets:start() of
		ok -> {ok,inets};
		{error,{already_started,inets}} -> {ok,inets} 
	end,
  case inets:start(httpd, [
    {port, 35000},
    {server_name, "ctlogviewer"},
    %{server_root, "/tmp"},
    %{document_root, "/tmp"},
		%
    {server_root, "/home/user/src/ehttpd/root/www"},
    {document_root, "/home/user/src/ehttpd/root/www"},
		{directory_index, ["index.hml", "welcome.html"]},
	
    {bind_address, "localhost"}
  ], stand_alone) of 
		{ok, Inets} -> {ok,Inets};
		{error,{already_started,Inets}} -> {ok,Inets}
	end.

start() -> run_server().                                                       

stop() ->
	%inets:stop(),
	io:format("ehttpd worker stopped~n",[]).
