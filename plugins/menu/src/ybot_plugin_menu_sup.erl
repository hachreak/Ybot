-module(ybot_plugin_menu_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Params),
    {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%%=============================================================================
%% API functions
%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%=============================================================================
%% Supervisor callbacks
%%=============================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
