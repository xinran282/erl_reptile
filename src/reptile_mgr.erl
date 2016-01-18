%%%-------------------------------------------------------------------
%%% @author chen
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 七月 2015 19:11
%%%-------------------------------------------------------------------
-module(reptile_mgr).
-author("chen").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("reptile.hrl").

-record(state, {pids = [], read_id = 1, loop_times = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Arge) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Arge], []).

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
init([{Num, Url, UrlFile}]) ->
  ets:new(rep_read, [public, {keypos, 1}, named_table, set]), %% 待获取的链接
  ets:new(rep_done, [public, {keypos, 1}, named_table, set]), %% 已经获取过的链接
  ets:new(rep_opt, [public, {keypos, 1}, named_table, set]), %% 公告参数
  ets:new(rep_process, [public, {keypos, 1}, named_table, set]), %% 工作进程

  {Id, UrlL} = load_url_file(UrlFile, 1, Url),
  Now = reptile_lib:now_str(),
  {ok, File} = file:open("../log/url"++Now++".txt", write),
  ets:insert(rep_opt, [{read_id, Id}, {auto_id, Id}, {proc_num, Num}, {file, File}]),
  ets:insert(rep_read, UrlL),
  self() ! {start, Num},
  {ok, #state{pids = []}}.

init_f1(Num, Num, Rtn)->Rtn;
init_f1(N, Num, Rtn)->
  case gen_server:start_link(reptile_work, [self()], []) of
    {ok, Pid}->
      %% erlang:monitor(process, Pid),
      init_f1(N+1, Num, [Pid|Rtn]);
    _Err ->
      ?ERR("创建失败：~w,~w", [N, _Err]),
      init_f1(N+1, Num, Rtn)
  end.

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
handle_info({start, Num}, State = #state{read_id = Rid}) ->
  PidL = init_f1(0, Num, []),
  [{_, AutoId}] = ets:lookup(rep_opt, auto_id),
  {PidL2, Rid2} = start_work(PidL, Rid, AutoId),
  ets:insert(rep_opt, {read_id, Rid2}),
  reptile_lib:set_timmer(loop, 1, loop),
  {noreply, State#state{pids = PidL2, read_id = Rid2}};
handle_info(loop, State = #state{pids = PidL, read_id = Rid, loop_times = LoopTimes})->
  %% ?INFO("循环执行：~w", [State]),
  reptile_lib:set_timmer(loop, 3, loop),
  [{_, AutoId}] = ets:lookup(rep_opt, auto_id),
  {PidL2, Rid2} = start_work(PidL, Rid, AutoId),
  ?ERR("执行到：~w  ~w 已处理：~w", [Rid2, LoopTimes+1, Rid2-Rid]),
  ets:insert(rep_opt, {read_id, Rid2}),
  erlang:spawn(fun write_done/0),
  erlang:garbage_collect(self()),
  update_file(LoopTimes+1),
  {noreply, State#state{pids = PidL2, read_id = Rid2, loop_times = LoopTimes+1}};
%% 收到进程退出
handle_info({'DOWN', _MonitorRef, process, Pid, normal}, State = #state{pids = PidL}) ->
  PidL2 = lists:delete(Pid, PidL),
  {noreply, State#state{pids = PidL2}};
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State = #state{pids = PidL}) ->
  ?INFO("进程退出:~w", [_Reason]),
  PidL2 = lists:delete(Pid, PidL)++init_f1(0,1,[]),
  {noreply, State#state{pids = PidL2}};
%% 完成处理
handle_info({done, Pid}, State = #state{pids = PidL}) ->
  {noreply, State#state{pids = [Pid|PidL]}};
%% 停止
handle_info(stop, State) ->
  reptile_lib:cancel_timmer(loop),
  {noreply, State};
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
  ?INFO("异常关闭：~w", [_Reason]),
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
start_work([], Rid, _AutoId)->
  {[], Rid};
start_work(PidL, Rid, Rid)->
  {PidL, Rid};
start_work([Pid|PidL], Rid, AutoId) when Rid =< AutoId->
  case ets:lookup(rep_read, Rid) of
    []->
      start_work([Pid|PidL], Rid+1, AutoId);
    _ ->
      case ets:lookup(rep_process, Pid) of
        [] -> %% 进程不在忙
          Pid ! {start, Rid},
          ets:insert(rep_process, {Pid, 1}),
          start_work(PidL, Rid+1, AutoId);
        _ -> %% 当前进程在忙 下一个进程处理吧
          start_work(PidL, Rid, AutoId)
      end
  end;
start_work(PidL, Rid, _AutoId)->
  {PidL, Rid}.

%% @doc 写入文件
write_done()->
  case ets:info(rep_done, size) of
    Size when Size >= 1000000 ->
      Now = reptile_lib:now_str(), %% integer_to_list(M*1000000+S),
      ets:tab2file(rep_done, "../log/done"++Now),
      ets:delete_all_objects(rep_done);
    _ -> ok
  end.

%% @doc 加载url文件
load_url_file(FilePath, Id, Url)->
  case file:open(FilePath, read) of
    {ok, IO}->
      Rtn = load_url_file_f2(load_url_file_f1(IO, Id, []), Url),
      file:close(IO),
      Rtn;
    _Else ->
      ?ERR("读取文件失败：~w", [_Else]),
      load_url_file_f2({Id, []}, Url)
  end.

load_url_file_f2({Id, UrlL}, [])-> {Id, UrlL};
load_url_file_f2({Id, UrlL}, Url)-> {Id+1, [{Id, Url}|UrlL]}.

load_url_file_f1(IO, Id, Rtn)->
  case file:read_line(IO) of
    {ok, DataList}->
      DataList2 = reptile_lib:trim(DataList, "\n"),
      load_url_file_f1(IO, Id+1, [{Id, DataList2}|Rtn]);
    eof ->
      ?ERR("文件读取结束", []),
      {Id, Rtn};
    _Else->
      ?ERR("读文件错误：`w",[_Else]),
      {Id, Rtn}
  end.

%% @doc 更新记录的文件
update_file(Times)->
  case Times rem 2000 of
    0 ->
      [{_, File}] = ets:lookup(rep_opt, file),
      file:close(File),
      Now = reptile_lib:now_str(),
      {ok, File2} = file:open("../log/url"++Now++".txt", write),
      ets:insert(rep_opt, {file, File2});
    _ ->
      ok
  end.