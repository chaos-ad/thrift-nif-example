-module(thrift_packer).

-export([
         check_event_permissions/2,
         check_request_permissions/2,
         serialize/2,
         deserialize/2,
         deserialize/3,
         pack/2,
         unpack/2,
         pack_cmd/1,
         unpack_cmd/1
        ]).

-include_lib("thrift/test_types.hrl").
-include_lib("thrift/test_constants.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_acl_map(Mod, Struct) ->
    case erlang:get({thrift_acl_map, Mod, Struct}) of
        undefined ->
            Result = make_acl_map(Mod, Struct),
            erlang:put({thrift_acl_map, Mod, Struct}, Result),
            Result;
        Result ->
            Result
    end.

get_polymorphic_map(Mod, Struct) ->
    case erlang:get({thrift_polymorphic_map, Mod, Struct}) of
        undefined ->
            Result = make_polymorphic_map(Mod, Struct),
            erlang:put({thrift_polymorphic_map, Mod, Struct}, Result),
            Result;
        Result ->
            Result
    end.

get_struct_fields(Struct) ->
    get_struct_fields('game_types', Struct).

get_struct_fields(Mod, Struct) ->
    case erlang:get({thrift_struct_fields, Mod, Struct}) of
        undefined ->
            Result = make_struct_fields(Mod, Struct),
            erlang:put({thrift_struct_fields, Mod, Struct}, Result),
            Result;
        Result ->
            Result
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_struct_fields(Struct) ->
    make_struct_fields('game_types', Struct).

make_struct_fields(Mod, Struct) ->
    lists:map(fun({N, {_, {_, X}}}) -> {N, X} end, element(2, Mod:struct_info(Struct))).

make_acl_map(Mod, Struct) ->
    Fn = fun({_, _, _, R, X}) -> {R, element(2, lists:unzip(make_struct_fields(element(1,X))))} end,
    lists:map(Fn, element(2, Mod:struct_info_ext(Struct))).

make_polymorphic_map(Mod, Struct) ->
    Fn = fun({_, {_, {_, {_, T1}}, {_, {M2, T2}}}}) -> {T1, make_struct_fields(M2, T2)} end,
    lists:map(Fn, element(2, Mod:struct_info(Struct))).

check_request_permissions(Role, Request) ->
    case lists:keyfind(Role, 1, get_acl_map('game_types', 'accessControlList')) of
        false              -> false;
        {Role, RequestList} -> lists:member(Request, RequestList)
    end.

check_event_permissions(Role, Event) ->
    case lists:keyfind(Role, 1, get_acl_map('game_types', 'eventFilter')) of
        false             -> false;
        {Role, EventList} -> lists:member(erlang:element(1, Event), EventList)
    end.

polymorphic_packet_id(BaseType, ConcreteType, Map) ->
    case lists:keyfind(BaseType, 1, Map) of
        false -> {error, base_type_not_found};
        {BaseType, ConcreteTypes} ->
            case lists:keyfind(ConcreteType, 2, ConcreteTypes) of
                false -> {error, concrete_type_not_found};
                {N, ConcreteType} -> N
            end
    end.

% polymorphic_packet_type(BaseType, Key, Map) ->
%     case lists:keyfind(BaseType, 1, Map) of
%         false -> {error, base_type_not_found};
%         {BaseType, ConcreteTypes} ->
%             case lists:keyfind(Key, 1, ConcreteTypes) of
%                 false -> {error, concrete_type_not_found};
%                 {Key, ConcreteType} -> ConcreteType
%             end
%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

serialize(Type, Data) ->
    {ok, Transport} = thrift_memory_buffer:new(), %% returns only ok
    {ok, Protocol} = thrift_binary_protocol:new(Transport), %% returns only ok

    try thrift_protocol:write(Protocol, {Type, Data}) of
        {Protocol1, ok} ->
            {_, Buffer} = thrift_protocol:flush_transport(Protocol1),
            {ok, list_to_binary(Buffer)};
        {error, Reason} ->
            {error, Reason}
    catch ErrorType:Reason ->
            {error, {thift_pack_exception, {ErrorType, Reason}}}
    end.

deserialize(Type, Data) ->
    deserialize(Type, Data, undefined).

deserialize(Type, Data, Tag) ->
    {ok, Transport} = thrift_memory_buffer:new(Data), %% returns only ok
    {ok, Protocol} = thrift_binary_protocol:new(Transport), %% returns only ok
    try read(Protocol, Type, Tag) of
        {_Protocol1, {ok, Result}} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    catch ErrorType:Reason ->
            {error, {thift_unpack_exception, {ErrorType, Reason}}}
    end.

read(Protocol, Type, undefined) ->
    thrift_protocol:read(Protocol, Type);
read(Protocol, Type, Tag) ->
    thrift_protocol:read(Protocol, Type, Tag).

pack(TypeName, Data) ->
    %% it's may be hard to make map every time, one solution is to using process table
    pack_any({struct, {'game_types', TypeName}}, Data, get_polymorphic_map('game_types', 'polymorphicTypes'), 'game_types').
    %% serialize({struct, {'game_types', TypeName}}, Data).

unpack(TypeName, Data) ->
    %% deserialize({struct, {'game_types', TypeName}}, Data).
    unpack_any({struct, {'game_types', TypeName}}, Data, get_polymorphic_map('game_types', 'polymorphicTypes'), 'game_types').


pack_any(TypeName, Data, PackMap, TypesMod) ->
    Data1 = pack_impl(TypeName, Data, PackMap, TypesMod),
    serialize(TypeName, Data1).

pack_impl(_, undefined, _, _) ->
    undefined;
pack_impl({struct, {TypeModule, TypeName}}, TypeData, PackMap, TypesMod) ->
    ActualTypeName = element(1, TypeData),
    case polymorphic_packet_id(TypeName, ActualTypeName, PackMap) of
        {error, _} ->
            {struct, Fields} = TypeModule:struct_info(TypeName),
            Fn = fun({{_, FieldType}, Data}) -> pack_impl(FieldType, Data, PackMap, TypesMod) end,
            list_to_tuple([TypeName | lists:map(Fn, lists:zip(Fields, tl(tuple_to_list(TypeData))))]);
        Key when is_integer(Key) ->
            {ok, PackedData} = pack_any({struct, {TypesMod, ActualTypeName}}, TypeData, PackMap, TypesMod),
            {TypeName, Key, base64:encode(PackedData)}
    end;

pack_impl({map, _KeyType, ValueType}, MapData, PackMap, TypesMod) ->
    dict:map(fun(_Key, Value) ->
                     pack_impl(ValueType, Value, PackMap, TypesMod)
             end, MapData);
pack_impl({list, ValueType}, ListData, PackMap, TypesMod) ->
    lists:map(fun(Value) ->
                      pack_impl(ValueType, Value, PackMap, TypesMod)
              end, ListData);
pack_impl({set, ValueType}, SetData, PackMap, TypesMod) ->
    L = lists:map(fun(Value) ->
                    pack_impl(ValueType, Value, PackMap, TypesMod)
            end, sets:to_list(SetData)),
    sets:from_list(L);

pack_impl(SimpleType, SimpleData, _PackMap, _TypesMod) when is_atom(SimpleType) ->
    SimpleData.


unpack_any(TypeName, Data, UnpackMap, TypesMod) ->
    case deserialize(TypeName, Data) of
        {ok, Data1} -> {ok, unpack_impl(TypeName, Data1, UnpackMap, TypesMod)};
        Error       -> Error
    end.

unpack_impl(_, undefined, _, _) ->
    undefined;
unpack_impl({struct, {TypeModule, TypeName}}, TypeData, UnpackMap, TypesMod) ->
    case lists:keyfind(TypeName, 1, UnpackMap) of
        false ->
            {struct, Fields} = TypeModule:struct_info(TypeName),
            Fn = fun({{_, FieldType}, Data}) -> unpack_impl(FieldType, Data, UnpackMap, TypesMod) end,
            list_to_tuple([TypeName | lists:map(Fn, lists:zip(Fields, tl(tuple_to_list(TypeData))))]);
        {TypeName, ConcreteTypes} ->
            {TypeName, Key, PackedData} = TypeData,
            case lists:keyfind(Key, 1, ConcreteTypes) of
                false ->
                    throw({unpack_any, unknown_type_key});
                {Key, ActualTypeName} ->
                    {ok, UnpackedData} = unpack_any({struct, {TypesMod, ActualTypeName}}, base64:decode(PackedData), UnpackMap, TypesMod),
                    UnpackedData
            end
    end;

unpack_impl({map, _KeyType, ValueType}, MapData, UnpackMap, TypesMod) ->
    dict:map(fun(_Key, Value) ->
                     unpack_impl(ValueType, Value, UnpackMap, TypesMod)
             end, MapData);
unpack_impl({list, ValueType}, ListData, UnpackMap, TypesMod) ->
    lists:map(fun(Value) ->
                      unpack_impl(ValueType, Value, UnpackMap, TypesMod)
              end, ListData);
unpack_impl({set, ValueType}, SetData, UnpackMap, TypesMod) ->
    L = lists:map(fun(Value) ->
                    unpack_impl(ValueType, Value, UnpackMap, TypesMod)
            end, sets:to_list(SetData)),
    sets:from_list(L);
unpack_impl(SimpleType, SimpleData, _UnpackMap, _TypesMod) when is_atom(SimpleType) ->
    SimpleData.

pack_cmd(Cmd) ->
    case lists:keyfind(element(1, Cmd), 2, get_struct_fields('commandTypes')) of
        {Id, CmdTag} -> {ok, Result} = pack(CmdTag, Cmd), <<Id:8,  Result/binary>>;
        _            -> {error, unknown_command}
    end.

unpack_cmd(Cmd) ->
    <<CmdId:8,  PackedCmd/binary>> = Cmd,
    unpack_cmd(CmdId, PackedCmd).

unpack_cmd(Id, Params) ->
    case lists:keyfind(Id, 1, get_struct_fields('commandTypes')) of
        {Id, CmdTag} -> {ok, Cmd} = unpack(CmdTag, Params), Cmd;
        _            -> {error, unknown_command}
    end.
