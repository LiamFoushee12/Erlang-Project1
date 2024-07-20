-module(chat_server).
-export([start/0, stop/0, init/0, handle_client/1, broadcast/1]).

-define(PORT, 9000).

start() ->
    spawn(fun() -> init() end).

stop() ->
    ?MODULE ! stop.

init() ->
    register(chat_server, self()),
    io:format("Chat server started and listening on port ~w~n", [?PORT]),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [{active, true}, {reuseaddr, true}]),
    accept_clients(ListenSocket).

accept_clients(ListenSocket) ->
    spawn(fun() -> accept_clients_loop(ListenSocket) end).

accept_clients_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_clients_loop(ListenSocket);
        {error, _} ->
            io:format("Error accepting connection~n")
    end.

handle_client(Socket) ->
    gen_tcp:controlling_process(Socket, self()),
    receive
        {tcp, Socket, Data} ->
            Message = binary_to_list(Data),
            io:format("Received: ~s~n", [Message]),
            broadcast(Message),
            handle_client(Socket);
        {tcp_closed, Socket} ->
            io:format("Client disconnected~n")
    end.

broadcast(Message) ->
    lists:foreach(
        fun (Pid) -> Pid ! {broadcast, Message} end,
        [Pid || {Pid, _} <- registered()]
    ).
