-module(thrift_packer).

-export([pack/2, unpack/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack(Type, Data) ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    case thrift_protocol:write(Protocol, {Type, Data}) of
        {NewProtocol, ok} ->
            {_, Buffer} = thrift_protocol:flush_transport(NewProtocol),
            {ok, list_to_binary(Buffer)};
        {error, Reason} ->
            {error, Reason}
    end.

unpack(Type, Data) ->
    {ok, Transport} = thrift_memory_buffer:new(Data),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    case thrift_protocol:read(Protocol, Type) of
        {error, Reason} ->
            {error, Reason};
        {_, {ok, Result}} ->
            {ok, Result}
    end.

