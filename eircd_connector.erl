-module(eircd_connector).
-export([handle_connection/1, send_server_message/3, send_user_message/4]).
-include("common.hrl").

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
	eircd_users:find_by_socket(Socket).

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
	eircd_commands:handle_command(Command, User, ArgStr).

delete_user(Socket) ->
	%also remove user from channel (abstract all this away)
	User = get_user_from_socket(Socket),
	eircd_users:delete(User).

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
	CodeStr = string:right(integer_to_list(Code), 3, $0),
	Payload = ":"++?SERVER_HOSTNAME++" "++CodeStr++" "++Nickname++" "++Message,
	send(Socket, Payload).

send_user_message(Sender, Recipient, Command, Message) ->
	Socket = Recipient#user.socket,
	Username = Sender#user.username,
	Nickname = Sender#user.nickname,
	Hostname = Sender#user.hostname,
	Payload = ":"++Nickname++"!"++Username++"@"++Hostname++" "++string:to_upper(Command)++" "++Message,
	send(Socket, Payload).