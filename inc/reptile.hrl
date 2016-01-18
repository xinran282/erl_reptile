
%% -define(INFO(Str, Arge), logger:info(Str, Arge, ?MODULE, ?LINE)).

-define(ERR(Str, Arge), logger:error(Str, Arge, ?MODULE, ?LINE)).

-define(INFO(Str, Arge), ok).

%% 待处理连接上限制 为进程数量的N倍
-define(rep_read_size, 3).

-define(if_true(A, B, C), case A of true-> B; false-> C end).
-define(if_true(A, B), case A of true-> B; false-> ok end).