%%%
%%% Ybot CERN restaurant menu plugin.
%%%

-module(ybot_plugin_menu_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
start(_StartType, _StartArgs) ->
    ybot_plugin_menu_sup:start_link().

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
