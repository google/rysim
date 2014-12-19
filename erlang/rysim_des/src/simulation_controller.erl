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
-export([create/2, run_experiment/1]).


%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Factory method that will create the appropriate structures for an
%% simulation controller.
%% @spec create(File :: string(), Seed :: integer()) -> {ok, ControllerData}
%%       ControllerData = record(controller_data)
%% @end
create(File, Seed) when is_integer(Seed) ->
    parsers:init(),
    {ok, ExperimentConfig} = parsers:process_experiment(File),
    create_impl(ExperimentConfig, Seed);
create(_, Seed) ->
    error_logger:error_msg("~p:~p Received bad random seed ~p",
                           [?MODULE, ?LINE, Seed]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Executes the experiment contained within the given simulation controller data
%% record.
%% @spec run_experiment(ControllerData) -> Results
%%       ControllerData = record(controller_data)
%%       Results = record(sim_results)
%% @end
%%--------------------------------------------------------------------
run_experiment(ControllerData = #controller_data{body = ModelData,
                                                 results = Results}) ->
    AgentCount = gb_trees:size(ModelData#model_data.agents),
    NewResults = Results#sim_results{event_count = 0,
                                     final_time = 0,
                                     agent_count = AgentCount},
    NewControllerData = ControllerData#controller_data{results = NewResults},
    {ok, FinalControllerData} = simulation_loop(0, 0, NewControllerData),
    {ok, FinalControllerData#controller_data.results}.


%% ===================================================================
%% Private functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Setup the kernel that is specified by the given config and return an
%% initialzed controller_data record that can be used for further calls.
%% @spec create_impl(ExperimentConfig, Seed :: integer()) -> {ok, ControllerData}
%%       ExperimentConfig = record(experiment_config)
%%       ControllerData = record(controller_data)
%% @end
create_impl(#experiment_config{experiment_name = ExperimentName,
                               experiment_type = ExperimentType,
                               event_limit = Limit,
                               model_filename = ModelFilename},
            Seed) when is_integer(Seed)->
    {ok, ModelConfig} = parsers:process_model(ModelFilename),
    ModelName = ModelConfig#model_config.model_name,
    {ok, ModelData} = create_model_data(ModelConfig, Seed),
    Results = #sim_results{experiment_name = ExperimentName,
                           experiment_type = ExperimentType,
                           model_name = ModelName,
                           total_connections = ModelConfig#model_config.total_connections},
    {ok, #controller_data{event_limit = Limit,
                          body = ModelData,
                          results = Results}}.

%%--------------------------------------------------------------------
%% @doc
%% Internal implementation method for creating the appropriate structures for
%% the model.
%% @spec create_model_data(Config, Seed :: integer()) -> {ok, ModelData}
%%       Config = record(model_config)
%%       ModelData = record(model_data)
%% @end
%%--------------------------------------------------------------------
create_model_data(#model_config{distributions = Distributions,
                  agents = AgentConfigs},
                  Seed) when is_integer(Seed) ->
    {ok, NGState} = setup_num_gen(Seed, Distributions),
    {ok, EventQueueData} = event_queue:create(),
    {ok, ModelData} = agent:create_model(AgentConfigs, NGState, EventQueueData),
    {ok, ModelData}.

%%--------------------------------------------------------------------
%% @doc
%% Initialize the internal number generation state.
%% @spec setup_num_gen(Seed :: integer(), Distributions) -> {ok, State}
%%       Distributions = [record(distributions)]
%%       State = record(ng_data)
%% @end
%%--------------------------------------------------------------------
setup_num_gen(Seed, Distributions) when is_integer(Seed),
                                        is_list(Distributions) ->
    {ok, NGState} = num_gen:initialize_generator(Seed),
	initialize_distributions(Distributions, NGState).

%%--------------------------------------------------------------------
%% @doc
%% Walk through the proved list of distributions and make sure that they
%% are registered with the random number generation state.
%% @spec initialize_distributions(Distributions, State) -> {ok, NewState}
%%       Distributions = [record(distribution)]
%%       State = record(ng_data)
%%       NewState = record(ng_data)
%% @end
%%--------------------------------------------------------------------
initialize_distributions([], NGState) when is_record(NGState, ng_data)->
		{ok, NGState};
initialize_distributions([#distribution{label=Label,
                                        type=Type,
                                        scale=Scale,
                                        params=Params}|Tail],
                         OldNGState) when is_list(Tail),
                                          is_record(OldNGState, ng_data) ->
		{ok, NewNGState} = num_gen:register_distribution(Label,
                                                         Type,
                                                         Scale,
                                                         Params,
                                                         OldNGState),
		initialize_distributions(Tail, NewNGState).

%%--------------------------------------------------------------------
%% @doc
%% Main execution loop for the simulation. Processes events on a
%% timeslice and calls itself until either the queue runs out of events
%% or the event count time limit is reached.
%% @spec simulation_loop(EventCount :: event_count(), Time :: timestamp(), ControllerData) -> {ok, NewControllerData}
%%       Event = record(sim_event)
%%       ControllerData = record(controller_data)
%%       NewControllerData = record(controller_data)
%% @end
%%--------------------------------------------------------------------
simulation_loop(EventCount, Time, ControllerData) when is_integer(EventCount),
    is_integer(Time),
    is_record(ControllerData, controller_data) ->
    Limit = ControllerData#controller_data.event_limit,
    case EventCount >= Limit of
        true ->
            {ok, ControllerData};
        _ ->
            {ok, NextEvent} = next_event(ControllerData),
            case NextEvent == undefined of
                true ->
                    {ok, ControllerData};
                _ ->
                    NewTime = NextEvent#sim_event.timestamp,
                    {ok, NewEventCount, NewControllerData} = run_timestamp(EventCount, NewTime, ControllerData),
                    Results = NewControllerData#controller_data.results,
                    NewResults = Results#sim_results{event_count = NewEventCount,
                                                     final_time = NewTime},
                    simulation_loop(NewEventCount, NewTime, NewControllerData#controller_data{results = NewResults})
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return the value of the next event to be processed by this kernel.
%% @spec next_event(ControllerData) -> {ok, SimEvent}
%%       ControllerData = record(controller_data)
%%       SimEvent = record(sim_event)
%% @end
%%--------------------------------------------------------------------
next_event(ControllerData) when is_record(ControllerData, controller_data) ->
    ModelData = ControllerData#controller_data.body,
    EventQueueData = ModelData#model_data.event_queue_data,
    event_queue:top_event(EventQueueData).

%%--------------------------------------------------------------------
%% @doc
%% Process all of the events for a given timeslice and return the controller
%% data structure with the new event count.
%% @spec run_timeslice(EventCount :: event_count(), Timestamp :: timestamp(), ControllerData) -> {ok, NextEvent, NewControllerData}
%%       ControllerData = record(controller_data)
%%       NextEvent = record(sim_event)
%%       NewControllerData = record(controller_data)
%% @end
%%--------------------------------------------------------------------
run_timestamp(EventCount, Timestamp, ControllerData) when is_integer(EventCount),
                                                          is_integer(Timestamp),
                                                          is_record(ControllerData, controller_data) ->
    {ok, NextEvent} = next_event(ControllerData),
    case NextEvent == undefined of
        true ->
            {ok, NewControllerData} = advance_state(Timestamp, ControllerData),
            {ok, EventCount, NewControllerData};
        _ ->
            case NextEvent#sim_event.timestamp =/= Timestamp of
                true ->
                    {ok, NewControllerData} = advance_state(Timestamp, ControllerData),
                    {ok, EventCount, NewControllerData};
                _ ->
                    {ok, NewControllerData} = process_event(NextEvent, ControllerData),
                    run_timestamp(EventCount + 1, Timestamp, NewControllerData)
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Process the supplied event and return the updated controller data
%% @spec process_event(Event, ControllerData) -> {ok, NewControllerData}
%%       Event = record(sim_event)
%%       KernelData = record(controller_data)
%%       NewKernelData = record(controller_data)
%% @end
%%--------------------------------------------------------------------
process_event(Event, ControllerData) when is_record(Event, sim_event),
                                          is_record(ControllerData, controller_data) ->
    ModelData = ControllerData#controller_data.body,
    EventQueueData = ModelData#model_data.event_queue_data,
    {ok, NewEventQueueData} = event_queue:pop_event(EventQueueData),
    InterimModelData = ModelData#model_data{event_queue_data = NewEventQueueData},
    {ok, NewModelData} = agent:process_event(Event#sim_event.receiver, Event,
        InterimModelData),
    {ok, ControllerData#controller_data{body = NewModelData}}.

%%--------------------------------------------------------------------
%% @doc
%% Finalize the timestamp being processed and send pending events.
%% @spec advance_state(Timestamp :: timestamp(), ControllerData) -> {ok, NewControllerData}
%%       ControllerData = record(controler_data)
%%       NewControllerData = record(controller_data)
%% @end
%%--------------------------------------------------------------------
advance_state(Timestamp, ControllerData) when is_integer(Timestamp),
    is_record(ControllerData, controller_data) ->
    ModelData = ControllerData#controller_data.body,
    {ok, NewModelData} = agent:advance_state(Timestamp, ModelData),
    {ok, ControllerData#controller_data{body = NewModelData}}.
