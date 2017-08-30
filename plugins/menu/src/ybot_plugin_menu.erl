-module(ybot_plugin_menu).

-export([
   start/0,
   stop/0,
   execute/1
 ]).

start() -> ok.
stop() -> ok.

execute(_) ->
  % {ok, File} = file:read_file("/tmp/menu.html"),
  File = get_page(),
  [_ | List] = re:split(File, "HauteurMenu", [{return,list}]),
  Seq = lists:seq(1, erlang:length(List)),
  MenuHtml = lists:filtermap(fun({Num, El}) ->
      case Num rem 2 =:= 0 of
        true -> {true, El};
        false -> false
      end
    end, lists:zip(Seq, List)),

  format(lists:map(fun(El) ->
      Clean = re:replace(El, "<([\/\n\ta-zA-Z0-9\\\\\ =\"\;\%\:\-]+)>", "", [global,{return,list}]),
      io:format("clean: ~p~n", [Clean]),
      [_, _, TextDirty | _] = re:split(Clean, "([><]+)", [{return,list}]),
      io:format("dirty: ~p~n", [TextDirty]),
      [Name, Price] = lists:filter(fun(Value) -> Value =/= [] end, re:replace(TextDirty, "(\\n|\\t)", "", [global])),
      % [Name, Price] = lists:flatten(re:replace(TextDirty, "(\\n|\\t)", "", [global, {return, list}])),
      {Name, Price}
      % [_, NameHtml | _] = re:split(El, "span", [{return,list}]),
      % [_, _, Name| _] = re:split(NameHtml, "([<>])", [{return,list}]),
      % [_, _, _, PriceHtml | _] = re:split(El, "center", [{return,list}]),
      % [_, _, Price | _] = re:split(PriceHtml, "([<>])", [{return,list}]),
      % {re:replace(Name, "\\s+", " ", [global]), Price}
    end, MenuHtml)).

format(Menu) ->
  MenuFormatted = lists:foldl(fun({Name, Price}, Acc) ->
      Acc ++ lists:flatten(io_lib:format(" - ~s (~s CHF)\n", [Name, Price]))
    end, "", Menu),
  Header = ":fork_and_knife: Hey Y'@/all, it's lunch time! :clock12:",
  unicode:characters_to_list(
    lists:flatten(io_lib:format("~s~n~s\n", [Header, MenuFormatted]))).

get_page() ->
  BaseUrl = "http://extranet.novae-restauration.ch",
  Url = BaseUrl ++ "/index.php?frame=1&x=ad3f8f75fe1e353b972afcce8e375d6e&y=81dc9bdb52d04dc20036dbd8313ed055&z=135",
  inets:start(),
  {ok, {{"HTTP/1.1",302,"Found"}, Headers, _}} = httpc:request(
                                get, {Url, []}, [{autoredirect, false}], []),
  Location = proplists:get_value("location", Headers),
  Cookie = proplists:get_value("set-cookie", Headers),
  UrlMenu = BaseUrl ++ Location,
  {ok, {{"HTTP/1.1",200,"OK"}, _, Body1}} = httpc:request(
          get, {UrlMenu, [{"Cookie", Cookie}]}, [{autoredirect, false}], []),
  {ok, {{"HTTP/1.1",200,"OK"}, _, Body2}} = httpc:request(
      post, {UrlMenu, [{"Cookie", Cookie}],
             "application/x-www-form-urlencoded", "fn_limite=2&fn_numpage=1"},
      [{autoredirect, false}], []),
  Body1 ++ Body2.
