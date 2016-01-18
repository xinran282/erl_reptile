%%%-------------------------------------------------------------------
%%% @author chen
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 七月 2015 19:34
%%%-------------------------------------------------------------------
-module(reptile_work).
-author("chen").

-behaviour(gen_server).

%% API

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("reptile.hrl").

-record(state, {mgr_pid}).

%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([MgrPid]) ->
  {ok, #state{mgr_pid = MgrPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({start, Rid}, State) ->
%%   [{_, Rid}] = ets:lookup(rep_opt, read_id),
%%   ets:insert(rep_opt, {read_id, Rid+1}),
  case ets:lookup(rep_read, Rid) of
    [{_, Url}]->
      ets:delete(rep_read, Rid),
      N1 = reptile_lib:now(),
      do(Url),
      N2 = reptile_lib:now(),
      ets:insert(rep_done, {Url, Rid, N2-N1}),
      ets:delete(rep_process, self()),
      reptile_mgr ! {done, self()},
      erlang:garbage_collect(self());
_-> ?INFO("没找到对应数据：~w", [Rid])
  end,
  {noreply, State};
handle_info(stop, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  ?INFO("收到未知信息：~w", [_Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do(Url)->
  case httpc:request(get, {Url, []}, [{timeout, 5000},{connect_timeout, 50000}], []) of
    {ok, {_, _, Body}} -> do_f1(Url, Body);
    {ok, {_, Body}}-> do_f1(Url, Body);
    _Else->
      ?INFO("获取失败：~w ~ts", [_Else, Url])
  end.

do_f1(Url, Body)->
    case re:run(Body, "<a\\s.*?href=\"([^\"]+)\"[^>]*>(.*?)</a>", [global, {capture, [1], list}, unicode]) of
    {match, UrlL}->
        do_f2(Url, UrlL);
    _Else->
      ?INFO("找不到其他链接:~ts", [Url])
  end.

%% 新连接进库
do_f2(_Link, [])->ok;
do_f2(Link, [["#"]|Rest])-> do_f2(Link, Rest);
do_f2(Link, [[Url]|Rest])->
  case string:str(Url, "mailto:") of
    0->
      case string:str(Url, "javascript") of
        0->
          case string:str(Url, "://") of
            0 ->
              case string:str(Url, "/") of
                1 ->
                  {ok, {_Scheme, _UserInfo, Host, Port, _Path, _Query}} = http_uri:parse(Link),
                  do_f3("http://"++Host++":"++integer_to_list(Port)++Url),
                  do_f2(Link, Rest);
                _ ->
                  do_f2(Link, Rest)
              end;
            _ -> do_f3(Url),
              do_f2(Link, Rest)
          end;
        _ ->
          do_f2(Link, Rest)
      end;
    _ ->
      do_f2(Link, Rest)
  end.

do_f3(Url)->
  Url2 =
  case string:str(Url, "#")  of
    0 -> Url;
    Pos -> string:sub_string(Url, 1, Pos-1)
  end,
  case do_url(lists:reverse(Url2)) of
    false -> ok;
    ok -> do_f4(Url);
    Label ->
      ets:insert(rep_done, {Url, Label, 0})
  end.
  %% TODO 对URL进行过滤

do_f4(Url)->
  case ets:lookup(rep_done, Url) of
    [] ->  %% 新链接
      [{_, ProcNum}] = ets:lookup(rep_opt, proc_num),
      case ets:info(rep_read, size) >= (ProcNum*?rep_read_size) of
        true -> %% 防止内存爆掉 ets数量为进程数3倍限制
          [{_, File}] = ets:lookup(rep_opt, file),
          file:write(File, unicode:characters_to_binary(Url++"\n"));
        false ->
          [{_, AutoId}] = ets:lookup(rep_opt, auto_id),
          ets:insert(rep_opt, {auto_id, AutoId+1}),
          ets:insert(rep_read, {AutoId+1, Url})
      end;
    _ -> %% 已有 跳过
      ok
  end.

do_url([$l,$m,$t,$h,$s,$.|_])->ok;
do_url([$g,$e,$p,$j,$.|_])->pic;
do_url([$g,$p,$j,$.|_])->pic;
do_url([$/|_])->ok;
do_url([$l,$m,$t,$h,$.|_])->ok;
do_url([$m,$t,$h,$.|_])->ok;
do_url([$g,$n,$p,$.|_])->pic;
do_url([$3,$p,$m,$.|_])->video;
do_url([$4,$p,$m,$.|_])->video;
do_url([$v,$m,$w,$.|_])->video;
do_url([$l,$m,$x,$.|_])->xml;
do_url(_)->false.