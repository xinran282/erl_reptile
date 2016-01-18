%%%-------------------------------------------------------------------
%%% @author chen
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2015 11:08
%%%-------------------------------------------------------------------
-module(test).
-author("chen").

-include("reptile.hrl").
%% API
-export([start/0, stop/0, up/0, to_file/1,reload/0, url/0]).

up()->
    {ok, List} = file:list_dir("."),
    up_f1(List).
up_f1([])-> done;
up_f1([Name|Rest])->
    case string:str(Name, ".beam") of
        0 ->
            up_f1(Rest);
        Pos ->
            Name2 = string:sub_string(Name, 1, Pos - 1),
            c:l(list_to_atom(Name2)),
            io:format("~ts~n", [Name2]),
            up_f1(Rest)
    end.

to_file({EtsPath, Path})->
    {ok, Tab} = ets:file2tab(EtsPath),
    {ok, File}=file:open(Path, write),
    L = ets:tab2list(Tab),
    [file:write(File, unicode:characters_to_binary(io_lib:format("{~ts,~w,~w}\n",[Link, Id, Time])))||{Link, Id, Time}<-L],
    file:close(File).

start()->
    inets:start(),
    reptile_sup:start_link({3000, "http://www.qq.com/", "../log/test.txt"}).
%%     httpc:request("http://www.163.com").
%%     do_f2("http://www.hao123.com", L).

reload()->
    reptile_mgr ! loop.

stop()->
    reptile_mgr ! stop.

url()->
    {ok, {_, _, Body}} = httpc:request("http://www.163.com"),
    {match, UrlL} = re:run(Body, "<a\\s.*?href=\"([^\"]+)\"[^>]*>(.*?)</a>", [global, {capture, [1], list}, unicode]),
    do([["http://www.baidu.com"]|UrlL], []).

do([], Rtn)->Rtn;
do([[Url]|Rest], Rtn)->
    case do_url(lists:reverse(Url)) of
        false -> do(Rest, Rtn);
        ok-> do(Rest, [Url|Rtn])
    end.

do_url([$l,$m,$t,$h,$s,$.|_])->ok;
do_url([$g,$e,$p,$j,$.|_])->ok;
do_url([$g,$p,$j,$.|_])->ok;
do_url([$/|_])->ok;
do_url([$l,$m,$t,$h,$.|_])->ok;
do_url([$g,$n,$p,$.|_])->ok;
do_url([$3,$p,$m,$.|_])->ok;
do_url([$4,$p,$m,$.|_])->ok;
do_url([$v,$m,$w,$.|_])->ok;
do_url(_)->false.