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
-module(agent_SUITE).

-include("rysim.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Tests
-export([test_susceptible_recv_susceptible/1, test_susceptible_recv_exposed/1,
         test_susceptible_recv_infectious/1,
         test_susceptible_recv_recovered/1]).
-export([test_exposed_recv_susceptible/1, test_exposed_recv_exposed/1,
         test_exposed_recv_infectious/1, test_exposed_recv_recovered/1]).
-export([test_infectious_recv_susceptible/1, test_infectious_recv_exposed/1,
         test_infectious_recv_infectious/1, test_infectious_recv_recovered/1]).
-export([test_recovered_recv_susceptible/1, test_recovered_recv_exposed/1,
         test_recovered_recv_infectious/1, test_recovered_recv_recovered/1]).
-export([test_recovered_noloop_recv_susceptible/1,
         test_recovered_noloop_recv_exposed/1,
         test_recovered_noloop_recv_infectious/1,
         test_recovered_noloop_recv_recovered/1]).

all() ->
    [test_susceptible_recv_susceptible, test_susceptible_recv_exposed,
     test_susceptible_recv_infectious, test_susceptible_recv_recovered,
     test_exposed_recv_susceptible, test_exposed_recv_exposed,
     test_exposed_recv_infectious, test_exposed_recv_recovered,
     test_infectious_recv_susceptible, test_infectious_recv_exposed,
     test_infectious_recv_infectious, test_infectious_recv_recovered,
     test_recovered_recv_susceptible, test_recovered_recv_exposed,
     test_recovered_recv_infectious, test_recovered_recv_recovered,
     test_recovered_noloop_recv_susceptible, test_recovered_noloop_recv_exposed,
     test_recovered_noloop_recv_infectious,
     test_recovered_noloop_recv_recovered].


init_per_testcase(_, Config) ->
    AgentConfig = #agent_config{label="Alice",
                                     connections=["Bob", "Charlie"],
                                     s2e="TestDist",
                                     e2i="TestDist",
                                     i2r="TestDist"},
    [{agent_config, AgentConfig} | Config].

end_per_testcase(_, _) ->
    ok.


%% ===================================================================
%% Tests
%% ===================================================================
test_susceptible_recv_susceptible(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = susceptible,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, susceptible).
test_susceptible_recv_exposed(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = susceptible,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, exposed).

test_susceptible_recv_infectious(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = susceptible,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, infectious).

test_susceptible_recv_recovered(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = susceptible,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, recovered).

test_exposed_recv_susceptible(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = exposed,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, susceptible).

test_exposed_recv_exposed(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = exposed,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, exposed).

test_exposed_recv_infectious(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = exposed,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, infectious).

test_exposed_recv_recovered(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = exposed,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, recovered).

test_infectious_recv_susceptible(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = infectious,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, susceptible).

test_infectious_recv_exposed(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = infectious,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, exposed).

test_infectious_recv_infectious(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = infectious,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, infectious).

test_infectious_recv_recovered(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = infectious,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, recovered).

test_recovered_recv_susceptible(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, susceptible).

test_recovered_recv_exposed(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, ok, exposed).

test_recovered_recv_infectious(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, infectious).

test_recovered_recv_recovered(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s="TestDist"},
    run_test_sequential(AgentConfig, error, recovered).

test_recovered_noloop_recv_susceptible(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s=undefined},
    run_test_sequential(AgentConfig, error, susceptible).

test_recovered_noloop_recv_exposed(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s=undefined},
    run_test_sequential(AgentConfig, ok, exposed).

test_recovered_noloop_recv_infectious(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s=undefined},
    run_test_sequential(AgentConfig, error, infectious).

test_recovered_noloop_recv_recovered(Config) ->
    ProtoAgentConfig = ?config(agent_config, Config),
    AgentConfig = ProtoAgentConfig#agent_config{initial_state = recovered,
                                  r2s=undefined},
    run_test_sequential(AgentConfig, error, recovered).


%% ===================================================================
%% Private functions
%% ===================================================================
run_test_sequential(AgentConfig, error, State) ->
    {ok, NGState} = num_gen:initialize_generator(1),
    {ok, NewNGState} = num_gen:register_distribution("TestDist", gaussian_tail,
                                                     1.0, [1.0, 2.0], NGState),
    {ok, EQState} = event_queue:create(),
    {ok, ModelState} = agent:create_model([AgentConfig], NewNGState, EQState),
    Event = #sim_event{timestamp=0,
                       body=State},
    try
        {ok, NewModelState} = agent:process_event(AgentConfig#agent_config.label,
                                                  Event,
                                                  ModelState),
        {ok, _} = agent:advance_state(0, NewModelState)
    of
        _ ->
            throw(testfailed)
    catch
        _ ->
            ok
    end;
run_test_sequential(AgentConfig, ok, State) ->
    {ok, NGState} = num_gen:initialize_generator(1),
    {ok, NewNGState} = num_gen:register_distribution("TestDist", gaussian_tail,
                                                     1.0, [1.0, 2.0], NGState),
    {ok, EQState} = event_queue:create(),
    {ok, ModelState} = agent:create_model([AgentConfig], NewNGState, EQState),
    Event = #sim_event{timestamp=0,
                       body=State},
    {ok, NewModelState} = agent:process_event(AgentConfig#agent_config.label,
                                              Event,
                                              ModelState),
    {ok, _} = agent:advance_state(0, NewModelState).

