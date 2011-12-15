-module(packer).
-export([pack/2, unpack/2]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    ok = erlang:load_nif(filename:join(PrivDir, "packer"), 0).

do_pack(_, _, _) ->
    exit(nif_library_not_loaded).

do_unpack(_, _, _) ->
    exit(nif_library_not_loaded).

pack({struct, {Module, Name}}, Data) ->
%     io:format("Packing: ~p~n", [Data]),
    FlattenData = pack_maps({struct, {Module, Name}}, Data),
%     io:format("After map flattening: ~p~n", [FlattenData]),
    PackedData = do_pack(Module, Name, FlattenData),
%     io:format("Packed data: ~p~n", [PackedData]),
    PackedData.

unpack({struct, {Module, Name}}, Data) ->
%     io:format("Unpacking: ~p~n", [Data]),
    {ok, UnpackedData} = do_unpack(Module, Name, Data),
%     io:format("After map restoring: ~p~n", [UnpackedData]),
    ResultData = unpack_maps({struct, {Module, Name}}, UnpackedData),
%     io:format("Unpacked data: ~p~n", [ResultData]),
    {ok, ResultData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack_maps({struct, {Module, Name}}, Data) ->
    {struct, FieldInfo} = Module:struct_info(Name),
    UpdFn =
    fun({_, Type}, {N, D}) ->
        Old = element(N, D),
        case pack_maps(Type, Old) of
            Old -> {N+1, D};
            New -> {N+1, setelement(N, D, New)}
        end
    end,
    element(2, lists:foldl(UpdFn, {2, Data}, FieldInfo));

pack_maps({list, Type}, Data) ->
    lists:map(fun(Elem) -> pack_maps(Type,Elem) end, Data);

pack_maps({set, Type}, Data) ->
    lists:map(fun(Elem) -> pack_maps(Type,Elem) end, sets:to_list(Data));

pack_maps({map, KeyType, ValType}, Data) ->
    lists:map(fun({Key, Val}) -> {pack_maps(KeyType,Key), pack_maps(ValType,Val)} end, dict:to_list(Data));

pack_maps(_, Data) -> Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpack_maps({struct, {Module, Name}}, Data) ->
    {struct, FieldInfo} = Module:struct_info(Name),
    UpdFn =
    fun({_, Type}, {N, D}) ->
        Old = element(N, D),
        case unpack_maps(Type, Old) of
            Old -> {N+1, D};
            New -> {N+1, setelement(N, D, New)}
        end
    end,
    element(2, lists:foldl(UpdFn, {2, Data}, FieldInfo));

unpack_maps({list, Type}, Data) ->
    lists:map(fun(Elem) -> unpack_maps(Type,Elem) end, Data);

unpack_maps({set, Type}, Data) ->
    sets:from_list( lists:map(fun(Elem) -> unpack_maps(Type,Elem) end, Data) );

unpack_maps({map, KeyType, ValType}, Data) ->
    dict:from_list( lists:map(fun({Key, Val}) -> {unpack_maps(KeyType,Key), unpack_maps(ValType,Val)} end, Data) );

unpack_maps(_, Data) -> Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
