# LiLu 
LiLu is a dirty ehttpd server for remote services logs visualization

OTP Application LiLu creates a gen_server that manage instances of httpd specifies for itsown directory.

```
For example let's see the following supervised tree structure:
LiLu [app supervisor]
  [worker]-lilu_viewer_manager handles viewers list
  [worker]-ct-log-viewer handles <address>/log/ct/index.html
  [worker]-console-viewer handles <address>/log/console/index.html
  [worker]-error-viewer handles <address>/log/error/index.html	
```

```erlang

	

Exaple:
	
BindAddress = "localhost".
Port = 12345.
{ok,Root} = file:get_cwd().
SrvRoot = lists:concat([Root,"/test"]).
DocRoot = lists:concat([Root,"/test"]).
DirIndex = ["index.hml", "welcome.html"].

% As proplist:	
ViewerSpec = [
	{bind_address, BindAddress},
	{port, Port},
	{root, Root},
	{server_name, ViewerName},
	{server_root, SrvRoot},
	{document_root, DocRoot},
	{directory_index, DirIndex}
].

% As a map:
ViewerSpec = #{
	bind_address => BindAddress,
	port => Port,
	root => Root,
	server_name => ViewerName,
	server_root => SrvRoot,
	document_root => DocRoot,
	directory_index => DirIndex
}.

% start LiLu viewer manager
>application:start(lilu).

% and start a viewer
>lilu:start_viewer("ct", ViewerSpec).
```
On success you will see the result in your browser

```
localhost:12345/index.html
```
Or you can see directory items if you ommit index.html just

```
localhost:12345
``` 
You can stop viewer using this fucnction call
```erlang
>lilu:stop_viewer("ct").
```
TODO: documentation...
