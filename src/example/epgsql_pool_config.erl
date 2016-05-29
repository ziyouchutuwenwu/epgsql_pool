-module(epgsql_pool_config).

-export([get_pool_name/0, get_pool_args/0]).

get_pool_name() ->
  pgsql_pool.

get_pool_args() ->
  PoolArgs = [
    {overrun_warning,5000},
    {overrun_handler,{?MODULE,my_overrun_handler}},
    {workers, 2},
    {worker, {epgsql_worker, [get_worker_args()]}}
  ],
  PoolArgs.

%% private
get_worker_args() ->
  WorkerArgs = [
    {ip, "127.0.0.1"},
    {username, "mmc"},
    {password, "mmc"},
    {database, "mmc"}
  ],
  WorkerArgs.