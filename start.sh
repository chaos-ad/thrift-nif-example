#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main(Args) ->
    {Size, Jobs} =
    case Args of
        []     -> {1000, 100};
        [S, J] -> {list_to_integer(S), list_to_integer(J)}
    end,
    packer_speed_test:test(Size, Jobs),
    init:stop().


