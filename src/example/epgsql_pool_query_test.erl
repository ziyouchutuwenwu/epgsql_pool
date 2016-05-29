-module(epgsql_pool_query_test).

-compile(export_all).

start_pool()->
  epgsql_pool:start(epgsql_pool_config).

stop_pool()->
  epgsql_pool:stop(epgsql_pool_config).

create_table() ->
  Cmd = "DROP TABLE IF EXISTS users;
        create table users
        (
        id serial primary key,
        title text,
        content text,
        createdtime time
        );",
  Info = epgsql_pool_query:squery(epgsql_pool_config, Cmd),
  io:format("create_table_test ~p~n", [Info]).

select() ->
  Cmd = "SELECT title,content FROM users;",
  {ok, _, Rows} = epgsql_pool_query:squery(epgsql_pool_config, Cmd),
  lists:foreach(
    fun(Row) ->
      {TitleBin, ContentBin} = Row,
      Title = binary_to_atom(TitleBin, utf8),
      Content = binary_to_atom(ContentBin, utf8),
      io:format("select_test ~p,~p~n", [Title, Content])
    end,
    Rows),
  ok.

update() ->
  Cmd = "update users set title='ppp' where id=3;",
  {ok, Count} = epgsql_pool_query:squery(epgsql_pool_config, Cmd),
  io:format("update_test ~p~n", [Count]).

insert() ->
  Cmd = "INSERT INTO users(title,content) values('aasa','ddd');",
  {ok, Count} = epgsql_pool_query:squery(epgsql_pool_config, Cmd),
  io:format("insert_test ~p~n", [Count]).

delete() ->
  Cmd = "DELETE FROM users WHERE id=3;",
  {ok, Count} = epgsql_pool_query:squery(epgsql_pool_config, Cmd),
  io:format("delete_test ~p~n", [Count]).