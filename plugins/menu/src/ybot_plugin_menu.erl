-module(ybot_plugin_menu).

-export([
   start/0,
   stop/0,
   execute/1
 ]).

start() -> ok.
stop() -> ok.

execute(_) ->
  {ok, File} = file:read_file("/tmp/menu.html"),
  [_ | List] = re:split(File, "HauteurMenu", [{return,list}]),
  Seq = lists:seq(1, erlang:length(List)),
  MenuHtml = lists:filtermap(fun({Num, El}) ->
      case Num rem 2 =:= 0 of
        true -> {true, El};
        false -> false
      end
    end, lists:zip(Seq, List)),

  format(lists:map(fun(El) ->
      [_, NameHtml | _] = re:split(El, "span", [{return,list}]),
      [_, _, Name| _] = re:split(NameHtml, "([<>])", [{return,list}]),
      [_, _, _, PriceHtml | _] = re:split(El, "center", [{return,list}]),
      [_, _, Price | _] = re:split(PriceHtml, "([<>])", [{return,list}]),
      {re:replace(Name, "\\s+", " ", [global]), Price}
    end, MenuHtml)).

format(Menu) ->
  MenuFormatted = lists:foldl(fun({Name, Price}, Acc) ->

      Acc ++ lists:flatten(io_lib:format(" - ~s (~s CHF)\n", [Name, Price]))
    end, "", Menu),
  Header = ":fork_and_knife: Hey Y'@/all, it's lunch time! :clock12:",
  unicode:characters_to_list(
    lists:flatten(io_lib:format("~s~n~s\n", [Header, MenuFormatted]))).
