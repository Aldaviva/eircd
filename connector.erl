-module(connector).
-export([handle_connection/1, send_server_message/3, send_user_messag/3]).
-include("constants.hrl").

handle_connection(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			Requests = string:tokens(bitstring_to_list(Data), "\r\n"),
			handle_requests(Requests, Socket),
			handle_connection(Socket);
		{tcp_closed, Socket} ->
			delete_user(Socket),
			io:format("Socket ~p closed~n", [Socket]);
		{tcp_error, Socket, Reason} ->
			io:format("Error on socket ~p caused by ~p~n", [Socket, Reason])
	end.

get_user_from_socket(Socket) ->
	user:find(Socket).

handle_requests(Requests, Socket) ->
	[handle_request(Request, Socket) || Request <- Requests].

handle_request(Request, Socket) ->
	Command = parse_command(Request),
	ArgStr = parse_argstr(Request, Command),
	User = case Command of
		"NICK" ->
			Socket;
		_ ->
			get_user_from_socket(Socket)
	end,
	commands:handle_command(Command, User, ArgStr).

delete_user(Socket) ->
	User = get_user_from_socket(Socket),
	user:delete(User).

parse_command(Request) ->
	string:to_upper(string:sub_word(Request, 1)).

parse_argstr(Request, Command) ->
	case string:words(Request) of
		1 ->
			"";
		N when N>1 ->
			string:substr(Request, string:len(Command) + 2)
	end.

send(Socket, Message) ->
	gen_tcp:send(Socket, Message++?CRLF).

send_server_message(User, Code, Message) ->
	Socket = User#user.socket,
	Nickname = User#user.nickname,
	CodeStr = string:right(integer_to_list(Code), 3, $0)
	Payload = ":"++?SERVER_HOSTNAME++" "++CodeStr++" "++Nickname++" "++Message,
	send(Socket, Payload).

send_user_message(User, Command, Message) ->
	Socket = User#user.socket,
	Nickname = User#user.nickname,
	Username = User#user.username,
	Hostname = User#user.hostname,
	Payload = ":"++Nickname++"!"++Username++"@"++Hostname++" "++string:to_upper(Command)++" "++Message,
	send(Socket, Payload).