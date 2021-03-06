-module(eircd_listener).
-export([listen/1]).

listen(Port) ->
	SocketOptions = [binary, { active, false }, { packet, 0 }],
	case gen_tcp:listen(Port, SocketOptions) of
		{ok, ListeningSocket} ->
			io:format("Listening on port ~p~n", [Port]),
			accept(ListeningSocket);
		Error ->
			io:format("Can't listen to socket: ~p~n", [Error])
	end.

accept(ListeningSocket) ->
	{ok, AcceptedSocket} = gen_tcp:accept(ListeningSocket),
	HandlerPid = spawn(eircd_connector, handle_connection, [AcceptedSocket]),
	ok = gen_tcp:controlling_process(AcceptedSocket, HandlerPid),
	accept(ListeningSocket).