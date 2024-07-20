-module(chat_client).
-export([start/0, stop/0, connect/1, disconnect/0, send_message/1]).

-define(SERVER, {127, 0, 0, 1}).
-define(PORT, 9000).

start() ->
    register(chat_client, self()).

stop() ->
    chat_server ! stop.

connect(Server) ->
    {ok, Socket} = gen_tcp:connect(Server, ?PORT, [{active, true}]),
    gen_tcp:controlling_process(Socket, self()),
    receive
        {tcp, Socket, Data} ->
            io:format("Connected to chat server~n")
    end,
    spawn(fun() -> receive_messages(Socket) end).

receive_messages(Socket) ->
    receive
        {tcp, Socket, Data} ->
            Message = binary_to_list(Data),
            io:format("~s~n", [Message]),
            receive_messages(Socket);
        {tcp_closed, Socket} ->
            io:format("Connection to server closed~n")
    end.

disconnect() ->
    chat_server ! stop.

send_message(Message) ->
    gen_tcp:send(chat_client, Message).
