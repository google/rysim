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
-module(agent).

-include("rysim.hrl").

%% API
-export([create_model/3,  process_event/3, advance_state/2]).


%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Factory function that takes in a set of agent configurations and the
%% state of the simulation, and outputs a model.
%% @spec create_model(Configs :: [agent_config()], NGData, EventQueueData) -> {ok, ModelData}
%%       NGData = record(ng_data)
%%       EventQueueData = record(event_queue_data)
%%       ModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
create_model(Configs, NGData, EventQueueData) when is_list(Configs),
                                                   is_record(NGData, ng_data),
                                                   is_record(EventQueueData, event_queue_data) ->
    ModelData = #model_data{agents=gb_trees:empty(),
                            dirty_agents=gb_trees:empty(),
                            ng_data=NGData,
                            event_queue_data=EventQueueData},
    create_agents_acc(Configs, ModelData).

%%--------------------------------------------------------------------
%% @doc
%% Process the given event for agent with given label. This will cause
%% an new updated model_data to be returned.
%% @spec process_event(Label :: string(), Event, ModelData) -> {ok, NewModelData}
%%       Event = record(sim_event)
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
process_event(Label, Event, ModelData)  when is_list(Label),
                                             is_record(Event, sim_event),
                                             is_record(ModelData, model_data) ->
   Agents = ModelData#model_data.agents,
    case gb_trees:lookup(Label, Agents) of
        none ->
            error_logger:error_msg("~p:~p process_event failed, unknown agent ~p!",
                                   [?MODULE, ?LINE, Label]),
            throw(badarg);
        {value, Agent} ->
            Body = Event#sim_event.body,
            {ok, NewAgent} = process_event_impl(Agent, Body),
            NewAgents = gb_trees:update(Label, NewAgent, Agents),
            {ok, NewDirtyAgents} = update_dirty_agents(Label,
                                                       NewAgent,
                                                       ModelData#model_data.dirty_agents),
            NewModelData = ModelData#model_data{agents=NewAgents,
                                                dirty_agents=NewDirtyAgents},
            {ok, NewModelData}
    end;
process_event(Label, Event, ModelData) ->
    error_logger:error_msg("~p:~p Unable to process_event, due to bad arg, ~p!",
                           [?MODULE, ?LINE, [Label, Event, ModelData]]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Advance the state of all the agents to the given timestamp. This
%% should cause pending messages to be sent. This should not cause a
%% jump of more then one interval, e.g. if current at time 0 and there
%% are events to processed at time 1, this should not advance to time 2.
%% @spec advance_state(Timestamp :: timestamp(), ModelData) -> {ok, NewModelData}
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
advance_state(Timestamp, ModelData) when is_integer(Timestamp),
                                         is_record(ModelData, model_data)->
    Agents = gb_trees:to_list(ModelData#model_data.dirty_agents),
    advance_state_acc(Timestamp, Agents, ModelData);
advance_state(Timestamp, ModelData) ->
    error_logger:error_msg("~p:~p Unable to process_event, due to bad arg, ~p!",
                           [?MODULE, ?LINE, [Timestamp, ModelData]]),
    throw(badarg).


%% ===================================================================
%% Private functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% For a given list of agent configurations create the agent aappend them to a
%% given model_data.
%% @spec create_agents_acc(Configs, ModelData) -> {ok, NewModelData}
%%       Configs = [record(agent_config)]
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
create_agents_acc([], ModelData) when is_record(ModelData, model_data)->
    {ok, ModelData};
create_agents_acc([Config|Tail], OldModelData) when is_record(Config, agent_config),
                                                    is_list(Tail),
                                                    is_record(OldModelData, model_data) ->
    case create_agent(Config, OldModelData) of
        {ok, NewModelData} ->
            create_agents_acc(Tail, NewModelData);
        _ ->
            throw(createagentfailed)
    end.

%%--------------------------------------------------------------------
%% @doc
%% For a specific config create a new agent and update the given model_data.
%% @spec create_agent(Config, ModelData) -> {ok, NewModelData}
%%       Config = record(agent_config)
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
create_agent(Config, OldModelData) when is_record(Config, agent_config),
                                        is_record(OldModelData, model_data)->
    Label = Config#agent_config.label,
    OldAgents = OldModelData#model_data.agents,
    case gb_trees:lookup(Label, OldAgents) of
        none ->
            Agent = agent_config_to_data(Config),
            {_, MiddleModelData} = transition_state(
                                     Label,
                                     Agent#agent_data{next_state = Agent#agent_data.current_state},
                                     0,
                                     OldModelData),
            NewAgents = gb_trees:insert(Label, Agent, OldAgents),
            NewModelData = MiddleModelData#model_data{agents=NewAgents},
            {ok, NewModelData};
        _ ->
            error_logger:error_msg("~p:~p Unable to create_agent, ~p, due to duplicate label type!",
                                   [?MODULE, ?LINE, Config]),
            throw(duplabel)
    end;
create_agent(Config, _) ->
    error_logger:error_msg("~p:~p Unable to create_agent, ~p, due to bad type!",
                           [?MODULE, ?LINE, Config]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Converts a agent config to an non-started agent and returns
%% the appropriate data record.
%% @spec agent_config_to_data(Config) -> Agent
%%       Config = record(agent_config)
%%       Agent = record(agent_data)
%% @end
%% --------------------------------------------------------------------
agent_config_to_data(Config) when is_record(Config, agent_config) ->
    #agent_data{current_state = Config#agent_config.initial_state,
                next_state = undefined,
                connections = Config#agent_config.connections,
                s2e = Config#agent_config.s2e,
                e2i = Config#agent_config.e2i,
                i2r = Config#agent_config.i2r,
                r2s = Config#agent_config.r2s}.

%%--------------------------------------------------------------------
%% @doc
%% Do side effects, generating events for the next transition and
%% possibly infection neighbours, associated with state transition. This
%% function does not verify that this is a valid transition, it only
%% looks at the end state.
%% @spec transition_state(Label :: string(), Agent, Timestamp :: timestamp(), ModelData) -> {agent_state(), NewModelData}
%%       Agent = record(agent_data)
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
transition_state(Label, Agent, Timestamp, OldModelData) when is_record(Agent, agent_data),
                                                             is_integer(Timestamp),
                                                             is_record(OldModelData, model_data) ->
    NextState = Agent#agent_data.next_state,
    NGData = OldModelData#model_data.ng_data,
    OldEventQueueData = OldModelData#model_data.event_queue_data,
    case NextState of
        undefined ->
            {Agent#agent_data.current_state, OldModelData};
        susceptible ->
            {susceptible, OldModelData};
        exposed ->
            {ok, Delta} = call_distribution(Agent#agent_data.e2i, NGData),
            {ok, NewEventQueueData} = event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                                                sender=Label,
                                                                receiver=Label,
                                                                body=infectious},
                                                      OldEventQueueData),
            NewModelData = OldModelData#model_data{event_queue_data=NewEventQueueData},
            {exposed, NewModelData};
        infectious ->
            {ok, Delta} = call_distribution(Agent#agent_data.i2r, NGData),
            {ok, MiddleEventQueueData} = event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                                                   sender=Label,
                                                                   receiver=Label,
                                                                   body=recovered},
                                                         OldEventQueueData),
            {ok, NewEventQueueData} = infect_neighbours(Label,
                                                Agent#agent_data.connections,
                                                Agent#agent_data.s2e,
                                                Timestamp,
                                                Timestamp + Delta,
                                                NGData,
                                                MiddleEventQueueData),
            NewModelData = OldModelData#model_data{event_queue_data=NewEventQueueData},
            {infectious, NewModelData};
        recovered ->
            NewModelData = case Agent#agent_data.r2s of
                             undefined ->
                                 OldModelData;
                             Dist ->
                                 {ok, Delta} = call_distribution(Dist, NGData),
                                 {ok, NewEventQueueData} = event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                                                                     sender=Label,
                                                                                     receiver=Label,
                                                                                     body=susceptible},
                                                                           OldEventQueueData),
                                 OldModelData#model_data{event_queue_data=NewEventQueueData}
                               end,
            {recovered, NewModelData};
        State ->
            error_logger:error_msg("Unexpected next state ~p~n",
                                   [State]),
            throw(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc
%% For an agent that have entered the infectious state go through the
%% list of connections and determine if it will infect that connection
%% before it leaves being infectious. If it will then create the
%% appropriate events.
%% @spec infect_neighbours(Sender :: string(), Connections :: [string()], Dist :: string(), Timestamp :: timestamp(), Limit :: timestamp(), NGData, EventQueueData) -> {ok, NewEventQueueData}
%%       NGData = record(ng_data)
%%       EventQueueData = record(event_queue_data)
%%       NewEventQueueData = record(event_queue_data)
%% @end
%% --------------------------------------------------------------------
infect_neighbours(_Sender, [], _Dist, _Timestamp, _Limit, _NGData, EventQueueData) when is_record(EventQueueData, event_queue_data)->
    {ok, EventQueueData};
infect_neighbours(Sender, [Connection|Tail], Dist, Timestamp, Limit, NGData,
                  EventQueueData) when is_list(Tail),
                                       is_integer(Timestamp),
                                       is_integer(Limit),
                                       is_record(NGData, ng_data),
                                       is_record(EventQueueData, event_queue_data) ->
    {ok, Delta} = call_distribution(Dist, NGData),
    {ok, NewEventQueueData} = case Timestamp + Delta =< Limit of
                           true ->
                               event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                                                 sender=Sender,
                                                                 receiver=Connection,
                                                                 body=exposed},
                                                      EventQueueData);
                           _ ->
                               {ok, EventQueueData}
    end,
    infect_neighbours(Sender, Tail, Dist, Timestamp, Limit, NGData,
                      NewEventQueueData).

%%--------------------------------------------------------------------
%% @doc
%% Given an agent, an event determine what the correct next state for the agent
%% is. This function is the one that filters the incoming messages to guarantee
%% that the state transition is valid.
%% @spec process_event_impl(Agent, Body :: agent_state()) -> {ok, NewAgent}
%%       Agent = record(agent_data)
%%       NewAgent = record(agent)
%% @end
%% --------------------------------------------------------------------
process_event_impl(Agent, Body) when is_record(Agent, agent_data),
                                     is_atom(Body) ->
    case Agent#agent_data.current_state of
        susceptible ->
            case Body of
                exposed ->
                    {ok, Agent#agent_data{next_state = exposed}};
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in susceptible~n",
                                           [State]),
                    throw(badstate)
            end;
        exposed ->
            case Body of
                exposed ->
                    {ok, Agent};
                infectious ->
                    {ok, Agent#agent_data{next_state = infectious}};
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in exposed~n",
                                           [State]),
                    throw(badstate)
            end;
        infectious ->
            case Body of
                exposed ->
                    {ok, Agent};
                recovered ->
                    {ok, Agent#agent_data{next_state = recovered}};
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in infectious~n",
                                           [State]),
                    throw(badstate)
            end;
        recovered ->
            case Body of
                exposed ->
                    {ok, Agent};
                susceptible ->
                    case Agent#agent_data.r2s of
                        undefined ->
                            error_logger:error_msg("Unexpected new state susceptible while in susceptible~n",
                                                   []),
                            throw(badstate);
                        _ ->
                            {ok, Agent#agent_data{next_state = susceptible}}
                    end;
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in infectious~n",
                                           [State]),
                    throw(badstate)
            end;
        State ->
            error_logger:error_msg("Unexpected agent state ~p~n",
                                   [State]),
            throw(badstate)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Either add or update the dirty agents with the agent that just processed an
%% event.
%% @spec update_dirty_agents(Label :: string(), Agent , DirtyAgents) -> {ok, NewDirtyAgents)}
%%       Agent = record(agent_data)
%%       DirtyAgents = gb_trees:tree(string(), record(agent_data))
%%       NewDirtyAgents = gb_trees:tree(string(), record(agent_data))
%% @end
%% --------------------------------------------------------------------
update_dirty_agents(Label, Agent, DirtyAgents) when is_record(Agent, agent_data) ->
    case gb_trees:lookup(Label, DirtyAgents) of
        none ->
            {ok, gb_trees:insert(Label, Agent, DirtyAgents)};
        _ ->
            {ok, gb_trees:update(Label, Agent, DirtyAgents)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Go through a list of agents with their labels and next states,
%% advancing them to their next state.
%% @spec advance_state_acc(Timestamp :: timestamp(), Agents, ModelData) -> {ok, NewModelData}
%%       Agents = [{string(), {record(agent_data), agent_state()}}]
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
advance_state_acc(_Timestamp, [], ModelData) when is_record(ModelData, model_data) ->
    {ok, ModelData#model_data{dirty_agents=gb_trees:empty()}};
advance_state_acc(Timestamp, [{Label, Agent}|Tail], OldModelData) when is_integer(Timestamp),
                                                                       is_record(Agent, agent_data),
                                                                       is_list(Tail),
                                                                       is_record(OldModelData, model_data) ->
    {NewState, NewModelData} = transition_state(Label,
                                                Agent,
                                                Timestamp,
                                                OldModelData),
    FinalModelData = case Agent#agent_data.current_state == NewState of
                         true ->
                             NewModelData;
                         _ ->
                             NewAgent = Agent#agent_data{current_state = NewState,
                                                              next_state = undefined},
                             OldAgents = NewModelData#model_data.agents,
                             NewAgents = gb_trees:update(Label,
                                                         NewAgent,
                                                         OldAgents),
                             NewModelData#model_data{agents=NewAgents}
                     end,
    advance_state_acc(Timestamp, Tail, FinalModelData).

%%--------------------------------------------------------------------
%% @doc
%% Wrapper around num_gen:call_distribution that add 1 to the return value
%% to make sure that we generate numbers on the positive interval - 0.
%% @spec call_distribution(Dist :: string(), NGData :: ng_data()) -> {ok, Delta :: integer()}
%% @end
%% --------------------------------------------------------------------
call_distribution(Dist, NGData) ->
    {ok, Delta} = num_gen:call_distribution(Dist, NGData),
    {ok, Delta + 1}.