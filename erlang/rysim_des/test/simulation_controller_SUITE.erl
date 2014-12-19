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
-module(simulation_controller_SUITE).

-include("rysim.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Tests
-export([test_trivial_seir_experiment/1,
         test_simple_seir_experiment/1,
         test_loop_seir_experiment/1]).

all() ->
    [test_trivial_seir_experiment,
     test_simple_seir_experiment,
     test_loop_seir_experiment].

init_per_testcase(_, Config) ->
    NewConfig = [{pwd_dir, file:get_cwd()} | Config],
    Dir = ?config(data_dir, Config),
    file:set_cwd(Dir),
    NewConfig.

end_per_testcase(_, Config) ->
    file:set_cwd(?config(pwd_dir, Config)),
    ok.

%% ===================================================================
%% Tests
%% ===================================================================
test_trivial_seir_experiment(_Config) ->
    File = "trivial_seir_experiment.json",
    Expected = {ok, #sim_results{experiment_name = "TrivialSEIRExperiment",
                                 experiment_type = "trivial",
                                 model_name = "TrivialSEIRModel",
                                 event_count = 0,
                                 final_time = 0,
                                 agent_count = 1,
                                 total_connections = 0}},
    {ok, ControllerData} = simulation_controller:create(File, 1),
    Returned = simulation_controller:run_experiment(ControllerData),
    error_logger:error_msg("Expected = ~p~nReturned = ~p~n",
                           [Expected, Returned]),
    Expected = Returned.

test_simple_seir_experiment(_Config) ->
    File = "simple_seir_experiment.json",
    Expected = {ok, #sim_results{experiment_name = "SimpleSEIRExperiment",
                                 experiment_type = "simple",
                                 model_name = "SimpleSEIRExample",
                                 event_count = 7,
                                 final_time = 25,
                                 agent_count = 3,
                                 total_connections = 2}},
    {ok, ControllerData} = simulation_controller:create(File, 1),
    Returned = simulation_controller:run_experiment(ControllerData),
    error_logger:error_msg("Expected = ~p~nReturned = ~p~n",
                           [Expected, Returned]),
    Expected = Returned.

test_loop_seir_experiment(_Config) ->
    File = "loop_seir_experiment.json",
    Expected = {ok, #sim_results{experiment_name = "LoopSEIRExperiment",
                                 experiment_type = "loop",
                                 model_name = "LoopSEIRModel",
                                 event_count = 1002,
                                 final_time = 835,
                                 agent_count = 4,
                                 total_connections = 12}},
    {ok, ControllerData} = simulation_controller:create(File, 1),
    Returned = simulation_controller:run_experiment(ControllerData),
    error_logger:error_msg("Expected = ~p~nReturned = ~p~n",
                           [Expected, Returned]),
    Expected = Returned.
