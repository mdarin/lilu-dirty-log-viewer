%%%
%%% A simple "Hello, world" server in the Erlang.
%%%

%https://stackoverflow.com/questions/2206933/how-to-write-a-simple-webserver-in-erlang

%Keep in mind, this exposes your /tmp directory.

%To run, simply:
%$ escript ./hello_erlang.erl

-module(ehttpd).
-export([
  main/1,
  run_server/0,
	%stop/0,
  start/0
]).

main(_) ->
  start(),
  receive
    stop -> ok
  end.

run_server() ->
  ok = inets:start(),
  {ok, _} = inets:start(httpd, [
    {port, 35890},
    {server_name, "ctlogviewer"},
    %{server_root, "/tmp"},
    %{document_root, "/tmp"},

    {server_root, "/home/user/code/cargo/test/log"},
    {document_root, "/home/user/code/cargo/test/log"},
	
    {bind_address, "localhost"}
  ]).

start() -> run_server().                                                       

