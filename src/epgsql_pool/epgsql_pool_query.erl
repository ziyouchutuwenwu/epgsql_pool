-module(epgsql_pool_query).

-export([get_parameter/2, squery/2, equery/2, equery/3, parse/2, parse/3, parse/4]).
-export([bind/3, bind/4, execute/2, execute/3, execute/4, describe/2, describe/3, sync/1, with_transaction/2]).

get_parameter(BehaviorMod, Name) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {get_parameter, Name}, best_worker).

squery(BehaviorMod, Sql) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {squery, Sql}, best_worker).

equery(BehaviorMod, Sql) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {equery, Sql}, best_worker).

equery(BehaviorMod, Sql, Parameters) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {equery, Sql,Parameters}, best_worker).

parse(BehaviorMod, Sql) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {parse, Sql}, best_worker).

parse(BehaviorMod, Sql, Types) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {parse, Sql,Types}, best_worker).

parse(BehaviorMod, Name, Sql, Types) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {parse, Name, Sql, Types}, best_worker).

bind(BehaviorMod, Statement, Parameters) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {bind, Statement, Parameters}, best_worker).

bind(BehaviorMod, Statement, PortalName, Parameters) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {bind, Statement, PortalName, Parameters}, best_worker).

execute(BehaviorMod, S) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {execute, S}, best_worker).

execute(BehaviorMod, S, N) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {execute, S, N}, best_worker).

execute(BehaviorMod, S, PortalName, N) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {execute, S, PortalName, N}, best_worker).

describe(BehaviorMod, X0) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {describe, X0}, best_worker).

describe(BehaviorMod, Type, Name) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {describe, Type, Name}, best_worker).

sync(BehaviorMod) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {sync}, best_worker).

with_transaction(BehaviorMod, F) ->
	PoolName = BehaviorMod:get_pool_name(),
	wpool:call(PoolName, {with_transaction, F}, best_worker).