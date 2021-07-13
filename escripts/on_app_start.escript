-module(on_app_start).

main(_Args) ->
  io:format("~n"),
  interprete_modules().

interprete_modules() ->
  int:ni(wpool),
  int:ni(wpool_shutdown),
  int:ni(wpool_sup),

  io:format("输入 int:interpreted(). 或者 il(). 查看模块列表~n").