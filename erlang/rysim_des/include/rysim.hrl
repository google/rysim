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
%%%
%%% @doc
%%% Header that specifies all of the shared/public record formats used
%%% by different modules of RySim. Records that are private to a module
%%% are specified in the modules .erl file.
%%%
%%% == Records ==
%%% === record(sim_event) ===
%%% <em>timestamp</em> is when this event occurs.
%%%
%%% <em>sender</em> is the agent that generated this event.
%%%
%%% <em>receiver</em> is the agent that is to process this event.
%%%
%%% <em>body</em> is the internal message data to be processed by the agent.
%%%
%%% === record(distribution) ===
%%% <em>label</em> is a user provided string that is associated with a
%%% distribution. This string must be unique amongst other labels.
%%%
%%% <em>type</em> specifies the underlying mathematical distribution to
%%% be used for generating numbers with this distribution.
%%%
%%% <em>scale</em> is a scaling factor to be applied to the results
%%% generated from the distribution. This value should not be negative,
%%% though the system does not enforce this.
%%%
%%% <em>params</em> is a list of floats provided as the control
%%% parameters for the distribution. The number of entries is dependent
%%% on the specific value of type. The expected number entries for each
%%% value of type are as follows:
%%% <ul>
%%% <li>gaussian_tail = 2</li>
%%% <li>exponential = 1</li>
%%% <li>flat = 2</li>
%%% <li>lognormal = 2</li>
%%% <li>poisson = 1</li>
%%% <li>bernoulli = 1</li>
%%% <li>binomial = 2</li>
%%% <li>negative_binomial = 2</li>
%%% <li>geometric = 1</li>
%%% </ul>
%%%
%%% === record(experiment_config) ===
%%% <em>experiment_name</em> is a user defined label/name for the
%%% experiment being run. This value is to be used in debugging/logging
%%% messages.
%%%
%%% <em>event_limit</em> how many events the simulation will process before
%%% beginning it termination sequence if it doesn't exhaust the queue first.
%%%
%%% <em>model_filename</em> is a  string that provide the file
%%% name for the model file to be supplied to the controller.
%%%
%%% === record(agent_config) ===
%%% <em>label</em> is a user supplied string that is unique to this
%%% specific agent. This value is used for indexing the agent and for
%%% logging/debugging messages.
%%%
%%% <em>initial_state</em> is the state that the agent should start in.
%%%
%%% <em>connections</em> is the list of other agents that this agent is
%%% connected to. Specifically when this agent becomes infectious this
%%% is the list of agents that might be infected by this agent.
%%%
%%% <em>s2e', 'e2i', 'i2r', 'r2s</em> are labels of the distributions
%%% used to determine when an agent under goes state tranisition. The
%%% transitions are susceptible -> exposed -> infectious -> recovered
%%% (-> susceptible). If the value of <em>r2s</em> is undefined then the
%%% final transition, in brackets, does not occur.
%%%
%%% === record(agent_data) ===
%%% <em>current_state</em> is the state that the agent is currently
%%% in.
%%%
%%% <em>next_state</em> is the state that the agent should be
%%% transitioning to at the end of the current state. A value of
%%% undefined indicates that no transition should occur and is the
%%% default state. An event needs to be processed by the agent to cause
%%% a different value to be present.
%%%
%%% <em>connections</em> is the list of other agents that this agent is
%%% connected to. Specifically when this agent becomes infectious this
%%% is the list of agents that might be infected by this agent.
%%%
%%% <em>s2e', 'e2i', 'i2r', 'r2s</em> are labels of the distributions
%%% used to determine when an agent under goes state tranisition. The
%%% transitions are susceptible -> exposed -> infectious -> recovered
%%% (-> susceptible). If the value of <em>r2s</em> is undefined then the
%%% final transition, in brackets, does not occur.
%%%
%%% === record(ng_data) ===
%%% <em>dists</em> is the set of registered distibutions. They are indexed
%%% in the tree by a string() that is the distribution label.
%%%
%%% === record(event_queue_data) ===
%%% <em>queue</em> is a reference to the set of currently enqueued
%%% sim_events that is passed into the internal implementation module
%%% being used as determined by <em>type</em>. For 'sequential' this is
%%% actually a gb_tree of sim_events indexed by the absolute value of
%%% their timestamps.
%%%
%%% === record(model_data) ===
%%% <em>agents</em> is a gb_tree of agent_data values indexed by their
%%% associated label.
%%%
%%% <em>dirty_agents</em> is non-strict sub set of <em>agents</em> that
%%% have processed an event this timestamp and thus need to advance
%%% their state.
%%%
%%% <em>ng_data</em> is an opaque blob that is passed into num_gen
%%% calls.
%%%
%%% <em>event_queue_data</em> is an opaque blob that is passed into event_queue
%%% calls.
%%%
%%% === record(model_config) ===
%%% <em>model_name</em> is a user supplied label for this portion of the
%%% system being modelled.
%%%
%%% <em>distributions</em> is a list of the distributions that are
%%% defined for this model. There is probably some way to get ride of
%%% this.
%%%
%%% <em>agents</em> is the set of agents associated with this
%%% model. This needs to be clarified.
%%%
%%% === record(sim_results) ===
%%% <em>experiment_name</em> is the user defined label associated with
%%% the experiment being run. This is parsed from the config file.
%%%
%%% <em>event_count</em> is the number events that were processed during
%%% the simulation.
%%%
%%% <em>final_time</em> is the highest timestamp of the processed
%%% events.
%%%
%%% <em>agent_count</em> is the number of agents in the model being run
%%% by the simulation controller. This is derived from the input files.
%%%
%%% === record(controller_data) ===
%%% <em>event_limit</em> is number of events that should be run. If execution
%%% reaches this event then the controller is expected to stop
%%% themselve. Execution may terminate earlier if the queues are exhausted.
%%%
%%% <em>body</em> is an instance of model_data that is used by the controller
%%  and contains the parsed model.
%%%
%%% <em>results</em> is current state of the results. This gets updated
%%% with each processed event.
%%%
%%% @end
%%%-------------------------------------------------------------------


%% ===================================================================
%% Records & Types
%% ===================================================================
-type(timestamp() :: integer()).
-type(event_count() :: non_neg_integer()).
-type(agent_state() :: susceptible | exposed | infectious | recovered).
-type(sim_event_body() :: agent_state()).

-record(sim_event, {timestamp :: timestamp(),
                    sender :: string(),
                    receiver :: string(),
                    body :: sim_event_body()}).
-type(sim_event() :: #sim_event{}).

-type(distribution_type() :: gaussian_tail | exponential | flat | lognormal
                           | poisson | bernoulli | binomial | negative_binomial
                           | geometric).

-record(distribution, {label :: string(),
                       type :: distribution_type(),
                       scale :: float(),
                       params :: [float()]}).

-record(experiment_config, {experiment_name :: string(),
                            event_limit :: event_count(),
                            model_filename :: string(),
                            experiment_type :: string()}).

-record(agent_config, {label :: string(),
                       initial_state :: agent_state(),
                       connections :: [string()],
                       s2e :: string(),
                       e2i :: string(),
                       i2r :: string(),
                       r2s :: string()}).
-type(agent_config() :: #agent_config{}).

-record(agent_data, {current_state :: agent_state(),
                     next_state :: agent_state(),
                     connections :: [string()],
                     s2e :: string(),
                     e2i :: string(),
                     i2r :: string(),
                     r2s :: string()}).
-type(agent_data() :: #agent_data{}).

-record(ng_data, {dists :: gb_trees:tree(string(), term())}).

-record(event_queue_data, {body :: gb_trees:tree(timestamp(), term())}).

-record(model_data, {agents :: gb_trees:tree(string(), agent_data()),
                     dirty_agents :: gb_trees:tree(string(), agent_data()),
                     ng_data :: #ng_data{},
                     event_queue_data :: #event_queue_data{}}).

-record(model_config, {model_name :: string(),
                       total_connections :: non_neg_integer(),
                       distributions :: list(),
                       agents :: [agent_config()]}).

-record(sim_results, {experiment_name :: string(),
                      experiment_type :: string(),
                      model_name :: string(),
                      event_count :: event_count(),
                      final_time :: timestamp(),
                      agent_count :: non_neg_integer(),
                      total_connections :: non_neg_integer()}).

-record(controller_data, {event_limit :: event_count(),
                          body :: #model_data{},
                          results :: #sim_results{}}).
