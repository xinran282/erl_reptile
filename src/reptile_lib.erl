%%%-------------------------------------------------------------------
%%% @author chen
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2015 19:32
%%%-------------------------------------------------------------------
-module(reptile_lib).
-author("chen").

%% API
-export([
    now/0, now_str/0, trim/2
    ,set_timmer/3, cancel_timmer/1
]).

now()->
    {M, S, _} = os:timestamp(),
    M*1000000+S.

now_str()->
    {{Y,M,D},{H,MM,S}}= erlang:localtime(),
    Str = io_lib:format("~ts", [[integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ",
        integer_to_list(H), "-", integer_to_list(MM), "-", integer_to_list(S)]]),
    unicode:characters_to_list(Str).

%% @doc 去掉结尾字符
trim([], _Char)->[];
trim(Str, Char)->
    trim_f1(lists:reverse(Str), lists:reverse(Char)).
trim_f1(Str, [])-> lists:reverse(Str);
trim_f1([Char|Str], [Char|Rest])-> trim_f1(Str, Rest);
trim_f1(Str, _Char)-> lists:reverse(Str).

%% @doc 设置定时器
set_timmer(Name, Sec, Info)->
    case erlang:get('@timmer') of
        undefined->
            Dict = dict:new(),
            Dict2 = dict:store(Name, erlang:send_after(Sec*1000, self(), Info), Dict),
            erlang:put('@timmer', Dict2);
        Dict ->
            case dict:find(Name, Dict) of
                error -> ok;
                {ok, Val}-> erlang:cancel_timer(Val)
            end,
            Dict2 = dict:store(Name, erlang:send_after(Sec*1000, self(), Info), Dict),
            erlang:put('@timmer', Dict2)
    end.

%% @doc 取消定时器
cancel_timmer(Name)->
    case erlang:get('@timmer') of
        undefined -> ok;
        Dict ->
            case dict:find(Name, Dict) of
                error -> ok;
                {ok, Val}-> erlang:cancel_timer(Val)
            end
    end.