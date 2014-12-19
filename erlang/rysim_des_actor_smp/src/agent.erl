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
%% @spec create_model(Configs :: [agent_config()], NGData, EventQueue :: pid()) -> {ok, ModelData}
%%       NGData = record(ng_data)
%%       ModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
create_model(Configs, NGData, EventQueue) when is_list(Configs),
                                               is_record(NGData, ng_data),
                                               is_pid(EventQueue) ->
    ModelData = #model_data{agents=gb_trees:empty(),
                            dirty_agents=gb_trees:empty(),
                            ng_data=NGData,
                            event_queue=EventQueue},
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
        {value, Pid} ->
            gen_server:cast(Pid, {process_event, Event}),
            {ok, NewDirtyAgents} = update_dirty_agents(Label,
                                                       Pid,
                                                       ModelData#model_data.dirty_agents),
            NewModelData = ModelData#model_data{dirty_agents=NewDirtyAgents},
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
    advance_state_acc(Timestamp, Agents),
    {ok, ModelData#model_data{dirty_agents=gb_trees:empty()}};
advance_state(Timestamp, ModelData) ->
    error_logger:error_msg("~p:~p Unable to advance_state, due to bad arg, ~p!",
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
            NGData = OldModelData#model_data.ng_data,
            EventQueue = OldModelData#model_data.event_queue,
            {ok, Pid} = gen_server:start_link(agent_server, [Agent, NGData, EventQueue], []),
            NewAgents = gb_trees:insert(Label, Pid, OldAgents),
            {ok, OldModelData#model_data{agents=NewAgents}};
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
    #agent_data{label = Config#agent_config.label,
                current_state = Config#agent_config.initial_state,
                next_state = undefined,
                connections = Config#agent_config.connections,
                s2e = Config#agent_config.s2e,
                e2i = Config#agent_config.e2i,
                i2r = Config#agent_config.i2r,
                r2s = Config#agent_config.r2s}.

%%--------------------------------------------------------------------
%% @doc
%% Add the agent to dirty agents if not already present.
%% @spec update_dirty_agents(Label :: string(), Agent :: pid() , DirtyAgents) -> {ok, NewDirtyAgents)}
%%       DirtyAgents = gb_trees:tree(string(), pid())
%%       NewDirtyAgents = gb_trees:tree(string(), pid())
%% @end
%% --------------------------------------------------------------------
update_dirty_agents(Label, Agent, DirtyAgents) when is_list(Label),
                                                    is_pid(Agent) ->
    case gb_trees:lookup(Label, DirtyAgents) of
        none ->
            {ok, gb_trees:insert(Label, Agent, DirtyAgents)};
        _ ->
            {ok, DirtyAgents}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Go through a list of agents with their labels and next states,
%% advancing them to their next state.
%% @spec advance_state_acc(Timestamp :: timestamp(), Agents) -> {ok, NewModelData}
%%       Agents = [{string(), pid()}]
%%       ModelData = record(model_data)
%%       NewModelData = record(model_data)
%% @end
%% --------------------------------------------------------------------
advance_state_acc(_Timestamp, []) ->
    ok;
advance_state_acc(Timestamp, [{_Label, Pid}|Tail]) when is_integer(Timestamp),
                                                       is_pid(Pid),
                                                       is_list(Tail) ->
    gen_server:cast(Pid, {advance_state, Timestamp}),
    advance_state_acc(Timestamp, Tail).
