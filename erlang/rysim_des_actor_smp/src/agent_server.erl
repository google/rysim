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
-module(agent_server).

-include("rysim.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {agent_data :: agent_data(),
                ng_data :: ng_data(),
                event_queue :: pid()}).

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
init([Agent, NGData, EventQueue]) when is_record(Agent, agent_data),
                                       is_record(NGData, ng_data),
                                       is_pid(EventQueue) ->
    transition_state(Agent#agent_data{next_state = Agent#agent_data.current_state},
                     0,
                     NGData,
                     EventQueue),
    {ok, #state{agent_data = Agent,
                ng_data = NGData,
                event_queue = EventQueue}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({process_event, Event}, State) ->
    Agent = State#state.agent_data,
    Body = Event#sim_event.body,
    NewState = case process_event_impl(Agent, Body) of
                   {ok, NewAgent} ->
                       State#state{agent_data = NewAgent};
                   _ ->
                       State
               end,
    {noreply, NewState};
handle_cast({advance_state, Timestamp}, State) ->
    Agent = State#state.agent_data,
    AgentState = Agent#agent_data.current_state,
    NGData = State#state.ng_data,
    EventQueue = State#state.event_queue,
    NewState = case transition_state(Agent, Timestamp, NGData, EventQueue) of
                    {ok, NewAgentState} ->
                        case AgentState =:= NewAgentState of
                            true ->
                                State;
                            _ ->
                                NewAgent = Agent#agent_data{current_state = NewAgentState,
                                                            next_state = undefined},
                                State#state{agent_data = NewAgent}
                        end;
                    _ ->
                        State
               end,
    gen_server:cast(controller, advanced_state),
    {noreply, NewState};
handle_cast(_Msg, State) ->
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
%% Given an agent, an event determine what the correct next state for the agent
%% is. This function is the one that filters the incoming messages to guarantee
%% that the state transition is valid.
%% @spec process_event_impl(Agent, Event :: agent_state()) -> {ok, NewAgent} |{{error, badstate}, Agent}}
%%       Agent = record(agent_data)
%%       NewAgent = record(agent_data)
%% @end
%% --------------------------------------------------------------------
process_event_impl(Agent, Event) ->
    case Agent#agent_data.current_state of
        susceptible ->
            case Event of
                exposed ->
                    {ok, Agent#agent_data{next_state = exposed}};
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in susceptible in agent, ~p",
                                           [State, Agent]),
                    {{error, badstate}, Agent}
            end;
        exposed ->
            case Event of
                exposed ->
                    {ok, Agent};
                infectious ->
                    {ok, Agent#agent_data{next_state = infectious}};
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in exposed in agent, ~p",
                        [State, Agent]),
                    {{error, badstate}, Agent}
            end;
        infectious ->
            case Event of
                exposed ->
                    {ok, Agent};
                recovered ->
                    {ok, Agent#agent_data{next_state = recovered}};
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in infectious in agent, ~p",
                        [State, Agent]),
                    {{error, badstate}, Agent}
            end;
        recovered ->
            case Event of
                exposed ->
                    {ok, Agent};
                susceptible ->
                    case Agent#agent_data.r2s of
                        undefined ->
                            error_logger:error_msg("Unexpected new state susceptible while in recovered in agent, ~p",
                                [Agent]),
                            {{error, badstate}, Agent};
                        _ ->
                            {ok, Agent#agent_data{next_state = susceptible}}
                    end;
                State ->
                    error_logger:error_msg("Unexpected new state ~p while in recovered in agent, ~p",
                        [State, Agent]),
                    {{error, badstate}, Agent}
            end;
        State ->
            error_logger:error_msg("Unexpected agent state ~p in agent, ~p",
                                   [State, Agent]),
            {{error, badstate}, Agent}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Do side effects, generating events for the next transition and
%% possibly infection neighbours, associated with state transition. This
%% function does not verify that this is a valid transition, it only
%% looks at the end state.
%% @spec transition_state(Agent, Timestamp :: timestamp(), NGData, EventQueue :: pid()) -> {ok, agent_state()}|{error, badarg}
%%       Agent = record(agent_data)
%%       NGData = record(ng_data)
%% @end
%% --------------------------------------------------------------------
transition_state(Agent, Timestamp, NGData, EventQueue) when is_record(Agent, agent_data),
                                                            is_integer(Timestamp),
                                                            is_record(NGData, ng_data),
                                                            is_pid(EventQueue) ->
    Label = Agent#agent_data.label,
    NextState = Agent#agent_data.next_state,
    case NextState of
        undefined ->
            {ok, Agent#agent_data.current_state};
        susceptible ->
            {ok, susceptible};
        exposed ->
            {ok, Delta} = call_distribution(Agent#agent_data.e2i, NGData),
            event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                              sender=Label,
                                              receiver=Label,
                                              body=infectious},
                                   EventQueue),
            {ok, exposed};
        infectious ->
            {ok, Delta} = call_distribution(Agent#agent_data.i2r, NGData),
            event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                              sender=Label,
                                              receiver=Label,
                                              body=recovered},
                                   EventQueue),
            infect_neighbours(Label,
                              Agent#agent_data.connections,
                              Agent#agent_data.s2e,
                              Timestamp,
                              Timestamp + Delta,
                              NGData,
                              EventQueue),
            {ok, infectious};
        recovered ->
            case Agent#agent_data.r2s of
                undefined ->
                    ok;
                Dist ->
                    {ok, Delta} = call_distribution(Dist, NGData),
                    event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                                      sender=Label,
                                                      receiver=Label,
                                                      body=susceptible},
                                           EventQueue)
            end,
            {ok, recovered};
        State ->
            error_logger:error_msg("Unexpected next state ~p~n",
                                   [State]),
            {error, badarg}
    end.

%%--------------------------------------------------------------------
%% @doc
%% For an agent that has entered the infectious state, walk the
%% list of connections and determine if it will infect that connection
%% before it leaves being infectious. If it will then create the
%% appropriate events.
%% @spec infect_neighbours(Sender :: string(), Connections :: [string()], Dist :: string(), Timestamp :: timestamp(), Limit :: timestamp(), NGData, EventQueue :: pid()) -> ok
%%       NGData = record(ng_data)
%% @end
%% --------------------------------------------------------------------
infect_neighbours(_Sender, [], _Dist, _Timestamp, _Limit, _NGData, _EventQueue)->
    ok;
infect_neighbours(Sender, [Connection|Tail], Dist, Timestamp, Limit, NGData, EventQueue) when is_list(Tail),
                                                                                              is_integer(Timestamp),
                                                                                              is_integer(Limit),
                                                                                              is_record(NGData, ng_data),
                                                                                              is_pid(EventQueue) ->
    {ok, Delta} = call_distribution(Dist, NGData),
    case Timestamp + Delta =< Limit of
        true ->
            event_queue:push_event(#sim_event{timestamp=Timestamp + Delta,
                                              sender=Sender,
                                              receiver=Connection,
                                              body=exposed},
                                   EventQueue);
        _ ->
            ok
    end,
    infect_neighbours(Sender, Tail, Dist, Timestamp, Limit, NGData, EventQueue).

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
