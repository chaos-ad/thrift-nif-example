-module(packer_test).
-compile(export_all).

-include_lib("thrift/test_types.hrl").

test() ->
    {Module, Type, Obj} = make_test_obj(3),

    {ok, Packed1} = thrift_packer:serialize({struct, {Module, Type}}, Obj),
    {ok, Packed2} = packer:pack({struct, {Module, Type}}, Obj),

    {ok, Res1} = packer:unpack({struct, {Module, Type}}, Packed1),
    {ok, Res2} = thrift_packer:deserialize({struct, {Module, Type}}, Packed2),
%
%     io:format("Packed by me:~n~p~n~n", [Packed1]),
%     io:format("Packed by original:~n~p~n~n", [Packed2]),
%     io:format("Original data:~n~p~n~n", [Obj]),
%     io:format("Unpacked data 1:~n~p~n~n", [Res1]),
%     io:format("Unpacked data 2:~n~p~n~n", [Res2]),

    case Res1 =:= Res2 of
        true  -> io:format("Everything seemed to be ok~n");
        false -> io:format("Inventories are not match!~n")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_test_obj(N) ->
    {test_types, library, #library{books=gen_books(N)}}.

gen_books(N) ->
    dict:from_list( lists:map(fun gen_book/1, lists:seq(1, N)) ).

gen_book(N) ->
    BookID = #bookID{id=N},
    AuthorID = #personID{id=N},
    BookTitle = "some_book_" ++ integer_to_list(N),
    BookContent = "Some very interesting book text here",
    Book = #book{id=BookID, author=AuthorID, title=BookTitle, content=BookContent},
    {BookID, Book}.
