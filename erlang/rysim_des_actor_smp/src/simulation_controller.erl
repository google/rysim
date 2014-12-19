%%%-------------------------------------------------------------------
%%% Copyright 2014 The RySim Authors. All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------
-module(simulation_controller).

-include("rysim.hrl").

%% API
-export([create/2, run_experiment/0, advanced_state/1]).


%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Factory method that will create the appropriate structures for an
%% simulation controller.
%% @spec create(File :: string(), Seed :: integer()) -> {ok, Pid :: pid()}
%% @end
create(File, Seed) when is_integer(Seed) ->
    gen_server:start_link({local, controller}, simulation_controller_server, [File, Seed], []);
create(File, Seed) ->
    error_logger:error_msg("~p:~p Malformed call to create!, [~p, ~p]",
                           [?MODULE, ?LINE, File, Seed]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Executes the experiment contained within the given simulation controller data
%% record.
%% @spec run_experiment(Pid :: pid()) -> {ok, Results}
%%       Results = record(sim_results)
%% @end
%%--------------------------------------------------------------------
run_experiment() ->
    gen_server:call(controller, run_experiment, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Signals to the simulation controller that a specific agent has advanced its
%% state, so it no longer needs to wait
%% for it.
%% @spec run_experiment(Pid :: pid()) -> {ok, Results}
%%       Results = record(sim_results)
%% @end
%%--------------------------------------------------------------------
advanced_state(Label) when is_list(Label) ->
    gen_server:cast(controller, {advanced_state, Label});
advanced_state(Label) ->
    error_logger:error_msg("~p:~p Malformed call to advanced_state!, [~p]",
        [?MODULE, ?LINE, Label]),
    throw(badarg).
