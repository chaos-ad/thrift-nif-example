-module(packer_speed_test).
-compile(export_all).

-include_lib("thrift/test_types.hrl").

test() ->
    test(1000, 1).

test(Size, Jobs) ->

    {Module, Type, Obj} = make_test_obj(Size),

    io:format("Warming up chaches (if any)...~n"),
    {ok, Packed1} = thrift_packer:serialize({struct, {Module, Type}}, Obj),
    {ok, Packed2} = packer:pack({struct, {Module, Type}}, Obj),

    {ok, Res1} = packer:unpack({struct, {Module, Type}}, Packed1),
    {ok, Res1} = thrift_packer:deserialize({struct, {Module, Type}}, Packed2),

    io:format("===== native implementation =====~n"),
    TotalTimeNative = measure_jobs(fun() -> pack_native({Module, Type, Obj}) end, Jobs),
    io:format("Total pack time: ~p~n", [TotalTimeNative/1000/1000]),

    io:format("===== erlang implementation =====~n"),
    TotalTimeErlang = measure_jobs(fun() -> pack_erlang({Module, Type, Obj}) end, Jobs),
    io:format("Total pack time: ~p~n", [TotalTimeErlang/1000/1000]),
    io:format("=================================~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack_native({Module, Type, Obj}) ->
    packer:pack({struct, {Module, Type}}, Obj).

pack_erlang({Module, Type, Obj}) ->
    thrift_packer:serialize({struct, {Module, Type}}, Obj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

measure_jobs(Fun, Count) ->
    io:format("Starting ~B jobs...~n", [Count]),
    Jobs = [ spawn( wrap_job(self(), Fun) ) || _ <- lists:seq(1, Count) ],
    {TotalTime, _} = timer:tc( fun() -> wait_jobs(Jobs) end ),
    io:format("~n"),
    TotalTime.

wrap_job(Pid, Fun) ->
    fun() ->
%         io:format("Job ~p started~n", [self()]),
        Pid ! {self(), timer:tc(Fun)}
    end.

wait_jobs(List) when is_list(List) ->
    receive {Pid, {_, _}} ->
        NewList = lists:delete(Pid, List),
        io:format("\r~B left     ", [length(NewList)]),
%         io:format("Job ~p finished in ~p seconds; ~B left~n", [Pid, TC/1000/1000, length(NewList)]),
        case NewList =:= [] of
            true  -> ok;
            false -> wait_jobs(NewList)
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_test_obj(Size) ->
    {test_types, library, #library{books=gen_books(Size)}}.

gen_books(N) ->
    dict:from_list( lists:map(fun gen_book/1, lists:seq(1, N)) ).

gen_book(N) ->
    BookID = #bookID{id=N},
    AuthorID = #personID{id=N},
    BookTitle = "some_book_" ++ integer_to_list(N),
    BookContent = "Some very interesting book text here",
    Book = #book{id=BookID, author=AuthorID, title=BookTitle, content=BookContent},
    {BookID, Book}.
