%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot main manager. Run transport, load plugins.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_manager).

-behaviour(gen_server).

-export([start_link/3, load_plugin/1, load_channel/1, run_transport/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% Internal state
-record(state, {
       %% ybot transports list (irc, xmpp and etc..)
       %% Example : [{irc, ClientPid, HandlerPid, Nick, Channel, Host}]
       transports = [],
       %% Ybot active plugins list
       plugins = [] :: [{plugin, Source :: string(), PluginName :: string(), Path :: string()}],
       %% Runned transports pid list
       runned_transports = [] :: [pid()],
       %% plugins paths
       plugins_paths = [],
       %% write only channels
       channels = []
    }).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(PluginsDirectory, Transports, Channels) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginsDirectory, Transports, Channels], []).

%%%=============================================================================
%%% ybot_manager callbacks
%%%=============================================================================

init([PluginsDirectory, Transports, Channels]) ->
    %% init plugins
    ok = gen_server:cast(?MODULE, {init_plugins, PluginsDirectory}),

    %% Start transports
    ok = gen_server:cast(?MODULE, {start_transports, Transports}),

    %% start channels
    ok = gen_server:cast(?MODULE, {start_channels, Channels}),

    %% init command history process
    ok = gen_server:cast(?MODULE, init_history),

    %% Start web interface
    case application:get_env(ybot, web_admin) of
        {ok, WebAdminOptions} ->
            case get_option(use_web_admin, WebAdminOptions) of
                true ->
                    lager:info("Web admin started ...."),
                    ybot_web_admin_sup:start_web_admin();
                false ->
                    ok
            end;
        _ ->
            ok
    end,
    {ok, #state{}}.

%% @doc Get plugin metadata by plugin name
handle_call({get_plugin, PluginName}, _From, State) ->
    case lists:keyfind(PluginName, 3, State#state.plugins) of
        false ->
            %% there is no plugin with `PluginName`
            {reply, wrong_plugin, State};
        Plugin ->
            %% return plugin with metadata
            {reply, Plugin, State}
    end;

%% @doc get transports info
handle_call(get_transports, _From, State) ->
    {reply, State#state.transports, State};

%% @doc get channels info
handle_call(get_channels, _From, State) ->
    {reply, State#state.channels, State};

%% @doc get all runned transports pid
handle_call(get_runnned_transports, _From, State) ->
    %% Return all runned transports
    {reply, State#state.runned_transports, State};

%% @doc Return all plugins
handle_call(get_plugins, _From, State) ->
    %% Return all plugins
    {reply, State#state.plugins, State};

handle_call(get_plugins_paths, _From, State) ->
    {reply, State#state.plugins_paths, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Init command history process
handle_cast(init_history, State) ->
    %% Check need command history or not
    case application:get_env(ybot, commands_history) of
        {ok, true} ->
            {ok, Limit} = application:get_env(ybot, history_command_limit_count),

            %% start history process with limit based on configuration
            ybot_history:start_link(Limit);
        _ ->
            ok
    end,
    {noreply, State};

%% @doc update plugins
handle_cast({update_plugins, NewPlugins}, #state{plugins = Plugins} = State) ->
    %% save new plugins
    {noreply, State#state{plugins = lists:flatten([NewPlugins | Plugins])}};

%% @doc Init active plugins
handle_cast({init_plugins, PluginsDirectory}, State) ->
    case filelib:is_dir(PluginsDirectory) of
        true ->
            %% Get all plugins
            PluginsPaths =
                lists:append(ybot_utils:get_all_files(PluginsDirectory),
                             ybot_utils:get_all_directories(PluginsDirectory)),

            %% Parse plugins and load to state
            Plugins = lists:flatten(lists:map(fun load_plugin/1, PluginsPaths)),

            %% observe new plugins after start
            ok = ybot_plugins_observer:observe_new_plugins(PluginsDirectory,
                                                           PluginsPaths),

            {noreply, State#state{plugins = Plugins,
                                  plugins_paths =  PluginsPaths}};
        false ->
            lager:error("Unable to load plugins. Invalid directory ~s",
                        [PluginsDirectory]),
            {noreply, State#state{plugins = []}}
    end;

%% doc Start write-only channels
handle_cast({start_channels, Channels}, State) ->
    %% update channels
    {noreply, State#state{channels = lists:flatmap(fun load_channel/1, Channels)}};

%% @doc Run transports from `Transports` list
handle_cast({start_transports, Transports}, State) ->
    %% Review supported mode of transportation
    TransportList = lists:zf(fun load_transport/1, Transports),

    %% Get runned transports pid list
    RunnedTransport = lists:zf(fun(Transport) ->
                                       if erlang:element(1, Transport) == http ->
                                               %% we no need in http transport
                                               false;
                                          true ->
                                               {true, erlang:element(2, Transport)}
                                       end
                               end, TransportList),
    {noreply, State#state{transports = TransportList,
                          runned_transports = RunnedTransport}};

%% @doc add new runned transport
handle_cast({update_transport, NewTransport, NewTransportPid}, State) ->
    %% update transports
    Transports = lists:flatten([NewTransport | State#state.transports]),
    TransportsPids = lists:flatten([NewTransportPid | State#state.runned_transports]),
    {noreply, State#state{transports = Transports,
                          runned_transports = TransportsPids}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @doc Start irc client
load_transport({irc, Nick, Channel, Host, Options}) ->
    %% Validate transport options
    case ybot_validators:validate_transport_opts(Options) of
        ok ->
            %% Get irc server port
            Port = get_option(port, Options),

            %% SSL?
            UseSsl = get_option(use_ssl, Options),

            %% Start irc handler
            {ok, HandlerPid} = irc_handler:start_link(),

            %% Run new irc client
            {ok, ClientPid} =
                ensure_started(
                  irc_lib_sup:start_irc_client(HandlerPid, Host, Port, Channel,
                                               Nick, UseSsl,
                                               get_reconnect_timeout(Options))),
            %% Start parser process
            {ok, ParserPid} = ybot_parser:start_link(),

            lager:info("Starting IRC transport: ~p, ~p, with nick: ~s",
                       [Host, Channel, Nick]),

            %% send client pid to handler
            ok = gen_server:cast(HandlerPid, {irc_client, ClientPid,
                                              ParserPid, Nick}),

            {true, {irc, ClientPid, HandlerPid, Nick, Channel, Host, Port}};
        _ ->
            %% wrong transport
            false
    end;

%% @doc start xmpp client
load_transport({xmpp, Login, Password, Room, Nick, Host, Resource, Options}) ->
    %% Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),

    %% Validate transport options
    case ybot_validators:validate_transport_opts(Options) of
        ok ->
            %% Get irc server port
            Port = get_option(port, Options),

            %% SSL?
            UseSsl = get_option(use_ssl, Options),

            %% Start xmpp handler
            {ok, HandlerPid} = xmpp_handler:start_link(),

            % Make room
            XmppRoom = list_to_binary(binary_to_list(Room) ++ "/"
                                      ++ binary_to_list(Nick)),

            lager:info("Starting XMPP transport: ~s, ~s, ~s", [Host, Room, Nick]),

            %% Start new xmpp transport
            {ok, ClientPid} =
                ensure_started(
                  xmpp_sup:start_xmpp_client(HandlerPid, Login, Password, Host,
                                             Port, XmppRoom, Nick, Resource,
                                             UseSsl, get_reconnect_timeout(Options))),
            %% Send client pid to handler
            ok = gen_server:cast(HandlerPid, {xmpp_client, ClientPid, ParserPid,
                                              Nick}),

            {true, {xmpp,
             ClientPid, HandlerPid, Login, Password, Host, Room, Nick, Resource}};
        _ ->
            %% wrong options
            false
    end;

%% @doc start hipchat client
load_transport({hipchat, Login, Password, Room, Host, Resource, HipChatNick,
                Options}) ->
    %% Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),

    %% HipChat configuration defaults
    Port = 5223,
    UseSsl = true,

    %% Start xmpp handler
    {ok, HandlerPid} = xmpp_handler:start_link(),

    %% Make room
    XmppRoom =
        list_to_binary(binary_to_list(Room) ++ "/" ++ binary_to_list(HipChatNick)),

    %% Run new xmpp client
    {ok, ClientPid} =
        ensure_started(
          xmpp_sup:start_xmpp_client(HandlerPid, Login, Password, Host, Port,
                                     XmppRoom, Resource, UseSsl,
                                     get_reconnect_timeout(Options))),

    lager:info("Starting HipChat transport: ~s, ~s, ~s", [Host, Room, Resource]),

    %% Send client pid to handler
    ok = gen_server:cast(HandlerPid,
                         {xmpp_client, ClientPid, ParserPid,
                          list_to_binary("@" ++ lists:concat(string:tokens(binary_to_list(HipChatNick), " ")))}),

    {true,
     {hipchat, ClientPid, HandlerPid, Login, Password, Host, Room, Resource}};

%% @doc start campfire client
load_transport({campfire, Login, Token, RoomId, CampfireSubDomain, Options}) ->
    %% Start campfire handler
    {ok, HandlerPid} = campfire_handler:start_link(),

    %% Run new campfire client
    {ok, ClientPid} =
        ensure_started(
          campfire_sup:start_campfire_client(HandlerPid, RoomId, Token,
                                             CampfireSubDomain,
                                             get_reconnect_timeout(Options))),

    lager:info("Starting Campfire transport: ~p, ~s",
               [RoomId, CampfireSubDomain]),

    %% Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),

    %% Send client pid to handler
    ok = gen_server:cast(HandlerPid, {campfire_client, ClientPid, ParserPid,
                                      Login}),

    {true, {campfire, ClientPid, HandlerPid, Login}};

%% @doc Ybot http interface
load_transport({http, Host, Port, BotNick}) ->
    %% Start http server
    {ok, HttpPid} = ensure_started(http_sup:start_http(Host, Port)),
    lager:info("Starting http transport ~p:~p", [Host, Port]),

    %% Send bot nick to http server
    ok = gen_server:cast(HttpPid, {bot_nick, BotNick}),

    {true, {http, HttpPid}};

%% @doc start flowdock client
load_transport({flowdock, NickInChat, Login, Password, FlowdockOrg, Flow,
                Options}) ->
    %% Start flowdock handler
    {ok, HandlerPid} = flowdock_handler:start_link(),

    %% Start flowdock client
    {ok, ClientPid} =
        ensure_started(
          flowdock_sup:start_flowdock_client(HandlerPid, FlowdockOrg, Flow,
                                             Login, Password,
                                             get_reconnect_timeout(Options))),

    lager:info("Starting flowdock transport ~p:~p", [FlowdockOrg, Flow]),

    %% Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),

    %% Send client pid to handler
    ok = gen_server:cast(HandlerPid, {flowdock_client, ClientPid, ParserPid,
                                      NickInChat}),

    {true, {flowdock, ClientPid, HandlerPid, Login}};

%% @doc Use skype or not
load_transport({skype, UseSkype, Host, Port}) ->
    %% Check use skype or not
    case UseSkype of
        true ->
            %% Get skype script from priv dir
            Skype = ybot_utils:get_priv_dir() ++ "skype.py",

            %% Skype command
            Command = "python " ++ Skype ++ " " ++ binary_to_list(Host) ++ " "
                ++ integer_to_list(Port),

            %% Start skype
            skype:start_link(Command),

            lager:info("Starting skype ..."),
            {true, {skype, UseSkype, Host, Port}};
        _ ->
            %% do nothing
            false
    end;

%% @doc start talkerapp client
load_transport({talkerapp, Nick, Room, Token, Options}) ->
    %% Start handler
    {ok, HandlerPid} = talkerapp_handler:start_link(),

    %% Start talker app client
    {ok, ClientPid} =
        ensure_started(
          talker_app_sup:start_talkerapp_client(HandlerPid, Nick, Room, Token,
                                                get_reconnect_timeout(Options))),
    % Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),

    %% Send client pid to handler
    ok = gen_server:cast(HandlerPid, {talkerapp_client, ClientPid, ParserPid,
                                      Nick}),

    lager:info("Starting talkerapp transport ~p:~p", [Room, Nick]),
    {true, {talkerapp, ClientPid, HandlerPid, Nick}};

load_transport(_) ->
    false.

load_plugin(Plugin) ->
    %% Get plugin extension
    Ext = filename:extension(Plugin),
    Name = filename:basename(Plugin, Ext),

    %% Match extension
    case Ext of
        ".py" ->
            %% python plugin
            lager:info("Loading plugin(Python): ~s", [Name]),
            {plugin, "python", Name, Plugin};
        ".rb" ->
            %% ruby plugin
            lager:info("Loading plugin(Ruby): ~s", [Name]),
            {plugin, "ruby", Name, Plugin};
        ".sh" ->
            %% shell plugin
            lager:info("Loading plugin(Shell): ~s", [Name]),
            {plugin, "sh", Name, Plugin};
        ".pl" ->
            %% perl plugin
            lager:info("Loading plugin(Perl) ~s", [Name]),
            {plugin, "perl", Name, Plugin};
        ".ex" ->
            %% elixir plugin
            lager:info("Loading plugin(Elixir) ~s", [Name]),
            {plugin, "elixir", Name, Plugin};
        ".scala" ->
            %% scala plugin
            lager:info("Loading plugin(Scala) ~s", [Name]),
            {plugin, "scala", Name, Plugin};
        [] ->
            % Check Erlang/OTP plugin
            case filelib:wildcard(Plugin ++ "/ebin/*.app") of
                [AppFile] ->
                    AppName = list_to_atom(filename:basename(AppFile, ".app")),
                    [_, Name] = string:tokens(Plugin, "/"),
                    lager:info("Loading plugin(Erlang) ~s", [Name]),
                    application:start(AppName),
                    {plugin, "erlang", Name, atom_to_list(AppName)};
                _ ->
                    []
            end;
        _ ->
            %% this is wrong plugin
            lager:info("Unsupported plugin type: ~s", [Ext]),
            []
    end.

%% @doc run new transport manualy
run_transport(Transport) ->
    case load_transport(Transport) of
        [] ->
            wrong_transport;
        NewTransport ->
            %% update transport
            gen_server:cast(ybot_manager, {update_transport, NewTransport,
                                           element(2, NewTransport)})
    end.

%% @doc start twitter channel
load_channel({twitter, ConsumerKey, ConsumerSecret, AccessToken,
              AccessTokenSecret}) ->
    {ok, TwitterCLientPid} =
        ybot_twitter_sup:start_twitter_client(ConsumerKey, ConsumerSecret,
                                              AccessToken, AccessTokenSecret),
    lager:info("Twitter channel started ~p ~p", [TwitterCLientPid, ConsumerKey]),
    {twitter, TwitterCLientPid, ConsumerKey, ConsumerSecret, AccessToken,
     AccessTokenSecret};

%% @doc start mail channel
load_channel({smtp, From, FromPassword, To, Options}) ->
    {ok, SmtpClientPid} =
        ybot_mail_client_sup:start_smtp_client(From, FromPassword, To, Options),
    lager:info("SMTP channel started ~p ~p", [SmtpClientPid, From]),
    {smtp};

load_channel(_) ->
    false.

ensure_started({ok, Pid}) ->
    {ok, Pid};
ensure_started({error,{already_started, Pid}}) ->
    {ok, Pid};
ensure_started(Other) ->
    Other.

get_reconnect_timeout(Options) ->
    case get_option(reconnect_timeout, Options) of
        false -> 0;
        Value -> Value
    end.

get_option(Key, Options) ->
    case lists:keyfind(Key, 1, Options) of
        {Key, Value} -> Value;
        false        -> false
    end.
