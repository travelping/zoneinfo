-module(zoneinfo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_all,
		 intensity => 0,
		 period => 1},
    ChildSpec =
	#{id       => zoneinfo,
	  start    => {zoneinfo, start_link, []},
	  restart  => permanent,
	  shutdown => 1000,
	  type     => worker,
	  modules  => [zoneinfo]},
    {ok, {SupFlags, [ChildSpec]}}.
