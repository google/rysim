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
-module(simulation_controller_server).

-include("rysim.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([File, Seed]) when is_list(File),
                        is_integer(Seed) ->
    parsers:init(),
    {ok, ExperimentConfig} = parsers:process_experiment(File),
    ExperimentName = ExperimentConfig#experiment_config.experiment_name,
    ExperimentType = ExperimentConfig#experiment_config.experiment_type,
    Limit = ExperimentConfig#experiment_config.event_limit,
    ModelFilename = ExperimentConfig#experiment_config.model_filename,
    {ok, ModelConfig} = parsers:process_model(ModelFilename),
    {ok, ModelData} = create_model_data(ModelConfig, Seed),
    ModelName = ModelConfig#model_config.model_name,
    Results = #sim_results{experiment_name = ExperimentName,
                           experiment_type = ExperimentType,
                           model_name = ModelName,
                           total_connections = ModelConfig#model_config.total_connections},
    {ok, #controller_data{event_limit = Limit,
                          body = ModelData,
                          results = Results}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(run_experiment, From, State) ->
    NewState = State#controller_data{from = From},
    gen_server:cast(controller, run_experiment_impl),
    {noreply, NewState};
handle_call(Msg, From, State) ->
    error_logger:error_msg("~p:~p Received unexpected call, message = ~p, from = ~p!",
        [?MODULE, ?LINE, Msg, From]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(run_experiment_impl, State) ->
    ModelData = State#controller_data.body,
    Results = State#controller_data.results,
    AgentCount = gb_trees:size(ModelData#model_data.agents),
    NewResults = Results#sim_results{event_count = 0,
                                     final_time = 0,
                                     agent_count = AgentCount},
    {ok, NewState} = simulation_loop(State#controller_data{results = NewResults}),
    {noreply, NewState};
handle_cast(run_experiment_finalize, State) ->
     gen_server:reply(State#controller_data.from,
                      {ok, State#controller_data.results}),
     {noreply, State};
handle_cast(advanced_state, State) ->
    OldDirtyAgentsCount = State#controller_data.dirty_agents_count,
    NewDirtyAgentsCount = OldDirtyAgentsCount - 1,
    MiddleState = State#controller_data{dirty_agents_count = NewDirtyAgentsCount},
    {ok, NewState} = case NewDirtyAgentsCount =:= 0 of
                        true ->
                            simulation_loop(MiddleState);
                        _ ->
                            {ok, MiddleState}
                    end,
    {noreply, NewState};
handle_cast(Msg, State) ->
    error_logger:error_msg("~p:~p Received unexpected cast, message = ~p",
        [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
                  Seed) ->
    {ok, NGState} = setup_num_gen(Seed, Distributions),
    {ok, EventQueue} = event_queue:create(),
    agent:create_model(AgentConfigs, NGState, EventQueue).

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
%% Walk through the provided list of distributions and register with the
%% number generation.
%% @spec initialize_distributions(Distributions, State) -> {ok, NewState}
%%       Distributions = [record(distribution)]
%%       State = record(ng_data)
%%       NewState = record(ng_data)
%% @end
%%--------------------------------------------------------------------
initialize_distributions([], NGState) when is_record(NGState, ng_data)->
    {ok, NGState};
initialize_distributions([#distribution{label = Label,
                                        type = Type,
                                        scale = Scale,
                                        params = Params}|Tail],
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
%% Main execution loop for the simulation. Processes all events on a
%% timeslice then calls itself, until either the queue runs out of events
%% or the event count time limit is reached.
%% @spec simulation_loop(EventCount :: event_count(), Time :: timestamp(), ControllerData) -> {ok, NewControllerData}
%%       Event = record(sim_event)
%%       ControllerData = record(controller_data)
%%       NewControllerData = record(controller_data)
%% @end
%%--------------------------------------------------------------------
simulation_loop(ControllerData) when is_record(ControllerData, controller_data) ->
    EventCount = ControllerData#controller_data.results#sim_results.event_count,
    Limit = ControllerData#controller_data.event_limit,
    case EventCount >= Limit of
        true ->
            gen_server:cast(controller, run_experiment_finalize),
            {ok, ControllerData};
        _ ->
            {ok, NextEvent} = next_event(ControllerData),
            case NextEvent == undefined of
                true ->
                    gen_server:cast(controller, run_experiment_finalize),
                    {ok, ControllerData};
                _ ->
                    Timestamp = NextEvent#sim_event.timestamp,
                    {ok, NewEventCount, NewControllerData} = run_timestamp(EventCount, Timestamp, ControllerData),
                    Results = NewControllerData#controller_data.results,
                    NewResults = Results#sim_results{event_count = NewEventCount,
                                                     final_time = Timestamp},
                    {ok, NewControllerData#controller_data{results = NewResults}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return the value of the next event to be processed by the controller.
%% @spec next_event(ControllerData) -> {ok, SimEvent}
%%       ControllerData = record(controller_data)
%%       SimEvent = record(sim_event)
%% @end
%%--------------------------------------------------------------------
next_event(ControllerData) when is_record(ControllerData, controller_data) ->
    ModelData = ControllerData#controller_data.body,
    EventQueue = ModelData#model_data.event_queue,
    event_queue:top_event(EventQueue).

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
    EventQueue = ModelData#model_data.event_queue,
    event_queue:pop_event(EventQueue),
    {ok, NewModelData} = agent:process_event(Event#sim_event.receiver, Event, ModelData),
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
    DirtyAgentsCount = gb_trees:size(ModelData#model_data.dirty_agents),
    {ok, NewModelData} = agent:advance_state(Timestamp, ModelData),
    {ok, ControllerData#controller_data{body = NewModelData,
                                        dirty_agents_count = DirtyAgentsCount}}.