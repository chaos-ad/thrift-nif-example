-module(packer_speed_test).
-compile(export_all).

-include_lib("thrift/test_types.hrl").

test() ->
    test(1000, 1).

test(Size, Jobs) ->

    {Module, Type, Obj} = make_test_obj(Size),

    io:format("Warming up chaches (if any)...~n"),
    {ok, Packed1} = thrift_packer:pack({struct, {Module, Type}}, Obj),
    {ok, Packed2} = packer:pack({struct, {Module, Type}}, Obj),

    {ok, Res1} = packer:unpack({struct, {Module, Type}}, Packed1),
    {ok, Res1} = thrift_packer:unpack({struct, {Module, Type}}, Packed2),

    io:format("===== native implementation =====~n"),
    PackTime1   = measure_jobs(fun() -> packer:pack({struct, {Module, Type}}, Obj) end, Jobs),
    UnpackTime1 = measure_jobs(fun() -> packer:unpack({struct, {Module, Type}}, Packed1) end, Jobs),
    io:format("Total pack time:   ~p seconds~n", [PackTime1]),
    io:format("Total unpack time: ~p seconds~n", [UnpackTime1]),

    io:format("===== erlang implementation =====~n"),
    PackTime2   = measure_jobs(fun() -> thrift_packer:pack({struct, {Module, Type}}, Obj) end, Jobs),
    UnpackTime2 = measure_jobs(fun() -> thrift_packer:unpack({struct, {Module, Type}}, Packed1) end, Jobs),
    io:format("Total pack time:   ~p seconds~n", [PackTime2]),
    io:format("Total unpack time: ~p seconds~n", [UnpackTime2]),
    io:format("=================================~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

measure_jobs(Fun, Count) ->
    measure_jobs(Fun, Count, verbose).

measure_jobs(Fun, Count, silent) ->
    Jobs = [ spawn( wrap_job(self(), Fun) ) || _ <- lists:seq(1, Count) ],
    {TotalTime, _} = timer:tc( fun() -> wait_jobs(Jobs, silent) end ), TotalTime;

measure_jobs(Fun, Count, verbose) ->
    io:format("Starting ~B jobs...~n", [Count]),
    Jobs = [ spawn( wrap_job(self(), Fun) ) || _ <- lists:seq(1, Count) ],
    {TotalTime, _} = timer:tc( fun() -> wait_jobs(Jobs, verbose) end ),
    io:format("~n"), TotalTime.

wrap_job(Pid, Fun) ->
    fun() -> Pid ! {self(), timer:tc(Fun)} end.

wait_jobs(List, silent) when is_list(List) ->
    receive {Pid, {_, _}} ->
        NewList = lists:delete(Pid, List),
        case NewList =:= [] of
            true  -> ok;
            false -> wait_jobs(NewList, silent)
        end
    end;

wait_jobs(List, verbose) when is_list(List) ->
    receive {Pid, {_, _}} ->
        NewList = lists:delete(Pid, List),
        io:format("\r~B left     ", [length(NewList)]),
        case NewList =:= [] of
            true  -> ok;
            false -> wait_jobs(NewList, verbose)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyze() ->
    print_header(),
    Runs = lists:seq(1,10,1) ++ lists:seq(20,100, 10) ++ lists:seq(200, 1000, 100) ++ lists:seq(2000, 10000, 1000),
    lists:foreach( fun(N) -> print_row(measure(N, 1)) end, Runs ),
    print_footer().

measure(Size, Jobs) ->
    {Module, Type, Obj} = make_test_obj(Size),
    {ok, Packed1} = thrift_packer:pack({struct, {Module, Type}}, Obj),
    {ok, Packed2} = packer:pack({struct, {Module, Type}}, Obj),
    {ok, Res1} = packer:unpack({struct, {Module, Type}}, Packed1),
    {ok, Res1} = thrift_packer:unpack({struct, {Module, Type}}, Packed2),
    PackTime1   = measure_jobs(fun() -> packer:pack({struct, {Module, Type}}, Obj) end, Jobs, silent),
    UnpackTime1 = measure_jobs(fun() -> packer:unpack({struct, {Module, Type}}, Packed1) end, Jobs, silent),
    PackTime2   = measure_jobs(fun() -> thrift_packer:pack({struct, {Module, Type}}, Obj) end, Jobs, silent),
    UnpackTime2 = measure_jobs(fun() -> thrift_packer:unpack({struct, {Module, Type}}, Packed1) end, Jobs, silent),
    {{Size, Jobs}, {PackTime1, UnpackTime1}, {PackTime2, UnpackTime2}}.

print_row({{S, J}, {T1, T2}, {T3, T4}}) ->
    io:format("|~6.w|~6w|~10.w|~10.w|~10.w|~10.w|~n", [S, J, T1, T2, T3, T4]).

print_header() ->
    io:format("+------+------+----------+----------+----------+----------+~n"),
    io:format("| Size | Jobs |  Pack N  | Unpack N |  Pack E  | Unpack E |~n"),
    io:format("+------+------+----------+----------+----------+----------+~n").

print_footer() ->
    io:format("+------+------+----------+----------+----------+----------+~n~n").