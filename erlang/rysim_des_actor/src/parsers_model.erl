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
%%% Low level details of parsing a seir model input file
%%% @end
%%%-------------------------------------------------------------------
-module(parsers_model).

-include("rysim.hrl").

%% API
-export([process/1]).


%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Top level function that takes in the given file name and passes data
%% along the calls that need to be made.
%% @spec process(File :: string()) -> ModelConfig
%%       ModelConfig = record(model_config)
%% @end
%%--------------------------------------------------------------------
process(File) ->
    {ok, Binary} = file:read_file(File),
    Blob = jsx:decode(Binary),
    ModelConfig = parse_model(Blob),
    Result = validate_model(ModelConfig),
    {ok, Result}.


%% ===================================================================
%% Private functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Takes the outputted blob from jiffy and returns a properly formated
%% model record.
%% @spec parse_model(Blob :: EJSON) -> ModelConfig
%%       ModelConfig = record(model_config)
%% @end
%%--------------------------------------------------------------------
parse_model(Blob) when is_list(Blob)->
    parse_model_acc(Blob, #model_config{});
parse_model(Blob) ->
    error_logger:error_msg("~p:~p Blob passed into parse_model is misformated!~n~p~n",
                           [?MODULE, ?LINE, Blob]),
    throw(malformedblob).


%%--------------------------------------------------------------------
%% @doc
%% Walks through the blob provided by yamerl and fills in the details of
%% a model record based on it.
%% @spec parse_model_acc(Blob :: EJSON, Result) -> ModelConfig
%%       Result = record(model_config)
%%       ModelConfig = record(model_config)
%% @end
%%--------------------------------------------------------------------
parse_model_acc([], Result) when is_record(Result, model_config),
                                 Result#model_config.model_name /= undefined,
                                 Result#model_config.total_connections /= undefined,
                                 Result#model_config.distributions /= undefined,
                                 Result#model_config.agents /= undefined ->
    Result;
parse_model_acc([{<<"model_name">>, Name}|Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, model_config),
       OldResult#model_config.model_name == undefined ->
    NewResult = OldResult#model_config{model_name = erlang:binary_to_list(Name)},
    parse_model_acc(Tail, NewResult);
parse_model_acc([{<<"total_connections">>, TotalConnections}|Tail], OldResult)
    when is_integer(TotalConnections),
         TotalConnections >= 0,
         is_list(Tail),
         is_record(OldResult, model_config),
         OldResult#model_config.total_connections == undefined ->
    NewResult = OldResult#model_config{total_connections = TotalConnections},
    parse_model_acc(Tail, NewResult);
parse_model_acc([{<<"distributions">>, Distributions}|Tail], OldResult)
  when is_list(Distributions),
       is_list(Tail),
       is_record(OldResult, model_config),
       OldResult#model_config.distributions == undefined ->
    NewResult = OldResult#model_config{distributions = parse_distributions(Distributions)},
    parse_model_acc(Tail, NewResult);
parse_model_acc([{<<"agents">>, Agents}|Tail], OldResult)
  when is_list(Agents),
       is_list(Tail),
       is_record(OldResult, model_config),
       OldResult#model_config.agents == undefined ->
    NewResult = OldResult#model_config{agents = parse_agents(Agents)},
    parse_model_acc(Tail, NewResult);
parse_model_acc(ModelConfig, _) ->
    error_logger:error_msg("~p:~p Unexpected entry in model!~n~p~n",
                                   [?MODULE, ?LINE, ModelConfig]),
    throw(malformedmodel).

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of distributions from the yamerl blob and returns a list
%% of distribution records.
%% @spec parse_distributions(Blob :: [[{string(), any()}]]) -> Distributions
%%       Distributions = [record(distribution)]
%% @end
%%--------------------------------------------------------------------
parse_distributions(List) when is_list(List), length(List) > 0 ->
    parse_distributions_acc(List, []);
parse_distributions(List) ->
    error_logger:error_msg("~p:~p Malformed distributions list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformeddistributions).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of yamerl blob distributions and converts them
%% to distribution records
%% @spec parse_distributions_acc(Blob :: [[{string(), any()}]], Results) -> Distributions
%%       Results = [record(distribution)]
%%       Distributions = [record(distribution)]
%% @end
%%--------------------------------------------------------------------
parse_distributions_acc([], Results) when is_list(Results)->
    lists:reverse(Results);
parse_distributions_acc([Head|Tail], Results) when is_list(Tail),
                                                   is_list(Results) ->
    Distribution = parse_distribution(Head),
    parse_distributions_acc(Tail, [Distribution|Results]);
parse_distributions_acc(List, _) ->
    error_logger:error_msg("~p:~p Malformed distributions list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformeddistributions).

%%--------------------------------------------------------------------
%% @doc
%% Takes a distribution from the yamerl blob and returns a distribution
%% record.
%% @spec parse_distribution(Blob :: [{string(), any()}]) -> Distribution
%%       Distribution = record(distribution)
%% @end
%%--------------------------------------------------------------------
parse_distribution(Distribution) ->
    parse_distribution_acc(Distribution, #distribution{}).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a yamerl blob distribution and converts it to a
%% distribution record
%% @spec parse_distribution_acc(Blob :: [{string(), any()}], Result) -> Distribution
%%       Result = record(distribution)
%%       Distribution = record(distribution)
%% @end
%%--------------------------------------------------------------------
parse_distribution_acc([], Result) when is_record(Result, distribution),
                                        Result#distribution.label /= undefined,
                                        Result#distribution.type /= undefined,
                                        Result#distribution.params /= undefined ->
    Result;
parse_distribution_acc([{<<"label">>, Label} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, distribution),
       OldResult#distribution.label == undefined ->
    NewResult = OldResult#distribution{label = erlang:binary_to_list(Label)},
    parse_distribution_acc(Tail, NewResult);
parse_distribution_acc([{<<"type">>, Type} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, distribution),
       OldResult#distribution.type == undefined ->
    NewResult = OldResult#distribution{type = distribution_type_string_to_atom(erlang:binary_to_list(Type))},
    parse_distribution_acc(Tail, NewResult);
parse_distribution_acc([{<<"scale">>, Scale} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, distribution),
       OldResult#distribution.scale == undefined ->
    NewResult = OldResult#distribution{scale = Scale},
    parse_distribution_acc(Tail, NewResult);
parse_distribution_acc([{<<"params">>, Params} | Tail], OldResult)
  when is_list(Params),
       is_list(Tail),
       is_record(OldResult, distribution),
       OldResult#distribution.params == undefined ->
    NewResult = OldResult#distribution{params = parse_params(Params)},
    parse_distribution_acc(Tail, NewResult);
parse_distribution_acc(Dist, Result) ->
    error_logger:error_msg("~p:~p Unexpected entry in distribution!~n~p~n~p~n",
                                   [?MODULE, ?LINE, Dist, Result]),
    throw(malformeddistribution).

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of distribution parameters from the yamerl blob and
%% returns a list of floats.
%% @spec parse_params(Blob :: [any()]) -> Params :: [float()]
%% @end
%%--------------------------------------------------------------------
parse_params(List) when is_list(List) ->
    parse_params_acc(List, []);
parse_params(List) ->
    error_logger:error_msg("~p:~p Malformed params list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformedparams).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of distribution parameters from the yamerl blob and
%% converts it to a list of floats.
%% @spec parse_params_acc(Blob :: [any()], Results :: [float()]) -> Params :: [float()]
%% @end
%%--------------------------------------------------------------------
parse_params_acc([], Results) when is_list(Results) ->
    lists:reverse(Results);
parse_params_acc([Param|Tail], List) when is_number(Param),
                                          is_list(Tail),
                                          is_list(List) ->
    parse_params_acc(Tail, [erlang:float(Param)|List]);
parse_params_acc(List, _) ->
    error_logger:error_msg("~p:~p Unexpected entry in params list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformedparams).

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of agent specifications from the yamerl blob and
%% returns a list of agent_config records.
%% @spec parse_agents(Blob :: [[{string(), any()}]]) -> Agents
%%       Agents = [record(agent_config)]
%% @end
%%--------------------------------------------------------------------
parse_agents(List) when is_list(List), length(List) > 0 ->
    parse_agents_acc(List, []);
parse_agents(List) ->
    error_logger:error_msg("~p:~p Malformed agents list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformedagents).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of agent specifications from the yamerl blob and
%% converts them to a list of agent_config records.
%% @spec parse_agents_acc(Blob :: [[{string(), any()}]], Results) -> Agents
%%       Results = [record(agent_config)]
%%       Agents = [record(agent_config)]
%% @end
%%--------------------------------------------------------------------
parse_agents_acc([], Results) when is_list(Results) ->
    lists:reverse(Results);
parse_agents_acc([Head|Tail], List) when is_list(Tail),
                                         is_list(List)->
    Agent = parse_agent_acc(Head, #agent_config{}),
    parse_agents_acc(Tail, [Agent|List]);
parse_agents_acc(List, _) ->
    error_logger:error_msg("~p:~p Malformed agents list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformedagents).

%%--------------------------------------------------------------------
%% @doc
%% Walks through an agent specifications from the yamerl blob and
%% converts it to a agent_config record.
%% @spec parse_agent_acc(Blob :: [[{string(), any()}]], Result) -> Agent
%%       Result = record(agent_config)
%%       Agent = record(agent_config)
%% @end
%%--------------------------------------------------------------------
parse_agent_acc([], Result) when is_record(Result, agent_config),
                                 Result#agent_config.label /= undefined,
                                 Result#agent_config.initial_state /= undefined,
                                 Result#agent_config.connections /= undefined,
                                 Result#agent_config.s2e /= undefined,
                                 Result#agent_config.e2i /= undefined,
                                 Result#agent_config.i2r /= undefined ->
    Result;
parse_agent_acc([{<<"label">>, Label} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.label == undefined ->
    NewResult = OldResult#agent_config{label = erlang:binary_to_list(Label)},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc([{<<"state">>, State} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.initial_state == undefined ->
    NewResult = OldResult#agent_config{initial_state = state_string_to_atom(erlang:binary_to_list(State))},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc([{<<"connections">>, Connections} | Tail], OldResult)
  when is_list(Connections),
       is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.connections == undefined ->
    NewResult = OldResult#agent_config{connections = parse_connections(Connections)},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc([{<<"s2e">>, S2E} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.s2e == undefined ->
    NewResult = OldResult#agent_config{s2e = erlang:binary_to_list(S2E)},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc([{<<"e2i">>, E2I} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.e2i == undefined ->
    NewResult = OldResult#agent_config{e2i = erlang:binary_to_list(E2I)},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc([{<<"i2r">>, I2R} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.i2r == undefined ->
    NewResult = OldResult#agent_config{i2r = erlang:binary_to_list(I2R)},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc([{<<"r2s">>, R2S} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, agent_config),
       OldResult#agent_config.r2s == undefined ->
    NewResult = OldResult#agent_config{r2s = erlang:binary_to_list(R2S)},
    parse_agent_acc(Tail, NewResult);
parse_agent_acc(Agent, _) ->
    error_logger:error_msg("~p:~p Unexpected entry in agents list!~n~p~n",
                                   [?MODULE, ?LINE, Agent]),
    throw(malformedagent).

%%--------------------------------------------------------------------
%% @doc
%% Takes in a list of connections from the yamerl blob and returns a
%% list of strings.
%% @spec parse_connections(Blob :: [any()]) -> Connections :: [string()]
%% @end
%%--------------------------------------------------------------------
parse_connections(List) when is_list(List) ->
    parse_connections_acc(List, []);
parse_connections(List) ->
    error_logger:error_msg("~p:~p Malformed connections list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformedconnections).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of connections from the yamerl blob and
%% converts them to a list of strings.
%% @spec parse_connections_acc(Blob :: [any()], Results :: [string()]) -> Connections :: [string()]
%% @end
%%--------------------------------------------------------------------
parse_connections_acc([], Result) when is_list(Result) ->
    lists:reverse(Result);
parse_connections_acc([Connection|Tail], List) when is_binary(Connection),
                                                    is_list(Tail),
                                                    is_list(List)->
    parse_connections_acc(Tail, [erlang:binary_to_list(Connection)|List]);
parse_connections_acc(List, _) ->
    error_logger:error_msg("~p:~p Unexpected entry in connections list!~n~p~n",
                                   [?MODULE, ?LINE, List]),
    throw(malformedconnections).


%%--------------------------------------------------------------------
%% @doc
%% Takes in a model record and returns it if it is valid.
%% @spec validate_model(ModelConfig) -> Result
%%       ModelConfig = record(model_config)
%%       Result = record(model_config)
%% @end
%%--------------------------------------------------------------------
validate_model(ModelConfig) when is_record(ModelConfig, model_config)->
    DistributionLabels = [ X#distribution.label || X <- ModelConfig#model_config.distributions],
    #model_config{model_name = ModelConfig#model_config.model_name,
                  total_connections = ModelConfig#model_config.total_connections,
                  distributions = validate_distributions(ModelConfig#model_config.distributions, DistributionLabels),
                  agents = validate_agents(ModelConfig#model_config.agents, DistributionLabels)}.


%%--------------------------------------------------------------------
%% @doc
%% Takes in an entry for a model's distributions and returns it, if it is
%% valid.
%% @spec validate_distributions(Distributions, Labels :: [string()]) -> Results
%%       Distributions = [record(distribution)]
%%       Results = [record(distribution)]
%% @end
%%--------------------------------------------------------------------
validate_distributions(List, Labels) when is_list(List),
                                          is_list(Labels) ->
    validate_distributions_acc(List, [], Labels);
validate_distributions(List, Labels) ->
    error_logger:error_msg("~p:~p Failed to validate distributions!~n~p~n~p~n",
                                   [?MODULE, ?LINE, List, Labels]),
    throw(baddistributions).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of distributions from a model, checks they are
%% valid and returns it if they are.
%% @spec validate_distributions_acc(Distributions, Results, Labels :: [string()]) -> Verified
%%       Distributions = [record(distribution)]
%%       Results = [record(distribution)]
%%       Verified = [record(distribution)]
%% @end
%%--------------------------------------------------------------------
validate_distributions_acc([], Results, _Labels) when is_list(Results)->
    lists:reverse(Results);
validate_distributions_acc([Head|Tail], Results, Labels) when is_record(Head, distribution),
                                                                      is_list(Tail),
                                                                      is_list(Results),
                                                                      is_list(Labels)->
    Distribution=validate_distribution(Head, Labels),
    validate_distributions_acc(Tail, [Distribution|Results], Labels);
validate_distributions_acc(Distributions, Results, Labels) ->
		error_logger:error_msg("~p:~p Failed to validate distributions!~n~p~n~p~n~p~n",
		[?MODULE, ?LINE, Distributions, Results, Labels]),
    throw(baddistribions).

%%--------------------------------------------------------------------
%% @doc
%% Takes in distribution record and returns it if it is
%% valid.
%% @spec validate_distribution(Distribution, Labels :: [string()]) -> Result
%%       Distribution = record(distribution)
%%       Result = record(distribution)
%% @end
%%--------------------------------------------------------------------
validate_distribution(Distribution, Labels) when is_record(Distribution, distribution),
                                                 is_list(Labels)->
    #distribution{label=validate_label_unique(Distribution#distribution.label, Labels),
                  type=Distribution#distribution.type,
                  scale=validate_scale_factor(Distribution#distribution.scale),
                  params=validators:validate_params(Distribution#distribution.type,
                                                    Distribution#distribution.params)}.

%%--------------------------------------------------------------------
%% @doc
%% Takes in distribution label and confirms that it is unique by
%% verifying it isn't amongst the remaining labels. The label is
%% returned if it is unique.
%% @spec validate_label_unique(Label :: string(), Labels :: [string()]) -> Result :: string()
%% @end
%%--------------------------------------------------------------------
validate_label_unique(Label, Labels) when is_list(Label),
                                          is_list(Labels) ->

		Count = list_count(Label, Labels),
    case Count =:= 1 of
        true ->
            Label;
        false ->
            error_logger:error_msg("~p:~p Label is not unique or present, ~p!~n~p~n~p~n",
                                   [?MODULE, ?LINE, Count, Label, Labels]),
            throw(badlabel)
    end;
validate_label_unique(_, _) ->
    throw(badlabel).

list_count(Elem, List) when is_list(List) ->
		list_count_impl(Elem, List, 0).

list_count_impl(_Elem, [], Count) when is_integer(Count) ->
		Count;
list_count_impl(Elem, [Head|Tail], Count) when is_list(Tail),
                                               is_integer(Count) ->
		case Elem =:= Head of
				true ->
						list_count_impl(Elem, Tail, Count + 1);
				false ->
						list_count_impl(Elem, Tail, Count)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Takes in scale factor from a distribution and confirms that it is a
%% float by returning it. If it is not a float it attempts to convert it
%% to one safely. If it can it returns the value as a float.
%% @spec validate_scale_factor(Scale :: undefined | integer() | float() | string()) -> float()
%% @end
%%--------------------------------------------------------------------
validate_scale_factor(undefined) ->
    1.0;
validate_scale_factor(Scale) when is_float(Scale),
                                  Scale > 0.0 ->
    Scale;
validate_scale_factor(Scale) when is_integer(Scale),
                                  Scale > 0 ->
    Scale + 0.0;
validate_scale_factor(Scale) when is_binary(Scale) ->
    String = erlang:binary_to_list(Scale),
    NewScale = erlang:list_to_float(String),
    case NewScale > 0.0 of
        true ->
            NewScale;
        _ ->
            error_logger:error_msg("~p:~p Cannot convert string ~p to a valid scale!~n",
                                   [?MODULE, ?LINE, Scale]),
            throw(badscale)
    end;
validate_scale_factor(Scale) ->
    error_logger:error_msg("~p:~p Cannot convert term ~p to a valid scale!~n",
                           [?MODULE, ?LINE, Scale]),
    throw(badscale).

%%--------------------------------------------------------------------
%% @doc
%% Takes in a list of agent_config records from a model and returns it if
%% they are valid.
%% @spec validate_agents(Agents, DistributionLabels :: [string()]) -> Results
%%       Agents = [record(agent_config)]
%%       Results = [record(agent_config)]
%% @end
%%--------------------------------------------------------------------
validate_agents(Agents, DistributionLabels) when is_list(Agents),
                                                 is_list(DistributionLabels) ->
    AgentLabels = [ X#agent_config.label || X <- Agents],
    validate_agents_acc(Agents, [], DistributionLabels, AgentLabels);
validate_agents(_, _) ->
    throw(badlabel).

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of agent_config record and verifies that they are
%% valid. If they are then the list is returned.
%% @spec validate_agents_acc(Agents, Results, DistributionLabels :: [string()], AgentLabels :: [string()]) -> Results
%%       Agents = [record(agent_config)]
%%       Results = [record(agent_config)]
%%       Verified = [record(agent_config)]
%% @end
%%--------------------------------------------------------------------
validate_agents_acc([], Results, _DistributionLabels, _AgentLabels) when is_list(Results)->
    lists:reverse(Results);
validate_agents_acc([Head|Tail], Results, DistributionLabels, AgentLabels) when is_record(Head, agent_config),
                                                                                is_list(Tail),
                                                                                is_list(Results),
                                                                                is_list(DistributionLabels),
																																								is_list(AgentLabels) ->
    Agent=validate_agent(Head, DistributionLabels, AgentLabels),
    validate_agents_acc(Tail, [Agent|Results], DistributionLabels, AgentLabels);
validate_agents_acc(_, _, _, _) ->
    throw(badagents).

%%--------------------------------------------------------------------
%% @doc
%% Takes in a agent record  and returns it if it is valid.
%% @spec validate_agent(Agent, DistributionLabels :: [string()], RemainingAgentLabels :: [string()]) -> Result
%%       Agents = record(agent_config)
%%       Results = record(agent_config)
%% @end
%%--------------------------------------------------------------------
validate_agent(Agent, DistributionLabels, AgentLabels) when is_record(Agent, agent_config),
                                                            is_list(DistributionLabels),
                                                            is_list(AgentLabels) ->
    #agent_config{label=validate_label_unique(Agent#agent_config.label, AgentLabels),
                  initial_state=Agent#agent_config.initial_state,
                  connections=Agent#agent_config.connections,
                  s2e=validate_label_present(Agent#agent_config.s2e, DistributionLabels),
                  e2i=validate_label_present(Agent#agent_config.e2i, DistributionLabels),
                  i2r=validate_label_present(Agent#agent_config.i2r, DistributionLabels),
                  r2s=validate_label_present(Agent#agent_config.r2s, [undefined | DistributionLabels])}.

%%--------------------------------------------------------------------
%% @doc
%% Walks through a list of connections and verifies that the given label
%% is present. If it is then the label is returned.
%% @spec validate_label_present(Label :: string(), Labels :: [string()]) -> Result :: string()
%% @end
%%--------------------------------------------------------------------
validate_label_present(Label, Labels) when is_list(Labels)->
    case lists:member(Label, Labels) of
        true ->
            Label;
        false ->
            error_logger:error_msg("~p:~p Label is not present!~n~p~n~p~n",
                                   [?MODULE, ?LINE, Label, Labels]),
            throw(badlabel)
    end;
validate_label_present(Label, Labels) ->
		error_logger:error_msg("~p:~p Malformed call to validate_label_present!~n~p~n~p~n",
		[?MODULE, ?LINE, Label, Labels]).

%%--------------------------------------------------------------------
%% @doc
%% Converts a string for a state to the appropriate atom
%% @spec state_string_to_atom(String :: string()) -> Atom :: agent_state()
%% @end
%%--------------------------------------------------------------------
state_string_to_atom(String) when is_list(String) ->
    state_string_to_atom_impl(string:to_lower(String)).

state_string_to_atom_impl("susceptible") ->
    susceptible;
state_string_to_atom_impl("exposed") ->
    exposed;
state_string_to_atom_impl("infectious") ->
    infectious;
state_string_to_atom_impl("recovered") ->
    recovered;
state_string_to_atom_impl(String) ->
    error_logger:error_msg("~p:~p Invalid state string ~p!~n",
                           [?MODULE, ?LINE, String]),
    throw(badstatestring).

%%--------------------------------------------------------------------
%% @doc
%% Converts a string for a distribution type to the appropriate atom.
%% @spec distribution_type_string_to_atom(String :: string()) -> Atom :: distribution_type()
%% @end
%%--------------------------------------------------------------------
distribution_type_string_to_atom(String) when is_list(String) ->
    distribution_type_string_to_atom_impl(string:to_lower(String)).

distribution_type_string_to_atom_impl("gaussiantail") ->
    gaussian_tail;
distribution_type_string_to_atom_impl("exponential") ->
    exponential;
distribution_type_string_to_atom_impl("flat") ->
    flat;
distribution_type_string_to_atom_impl("lognormal") ->
    lognormal;
distribution_type_string_to_atom_impl("poisson") ->
    poisson;
distribution_type_string_to_atom_impl("bernoulli") ->
    bernoulli;
distribution_type_string_to_atom_impl("binomial") ->
    binomial;
distribution_type_string_to_atom_impl("negativebinomial") ->
    negative_binomial;
distribution_type_string_to_atom_impl("geometric") ->
    geometric;
distribution_type_string_to_atom_impl(String)->
    error_logger:error_msg("~p:~p Invalid distribution type string ~p!~n",
                           [?MODULE, ?LINE, String]),
    throw(baddistributiontypestring).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_model_test_() ->
    [?_assertEqual(parse_model([{<<"model_name">>, <<"SimpleSEIRExample">>},
                                {<<"total_connections">>, 2},
                                {<<"distributions">>,
                                    [[{<<"label">>, <<"DistributionGaussianTail">>},
                                      {<<"type">>, <<"GaussianTail">>},
                                      {<<"params">>, [0.0, 1.0, 10.0]}]]},
                                {<<"agents">>,
                                    [[{<<"label">>, <<"Patient0">>},
                                      {<<"state">>, <<"Infectious">>},
                                      {<<"connections">>, [<<"Friend0">>, <<"Friend1">>]},
                                      {<<"s2e">>, <<"DistributionGaussianTail">>},
                                      {<<"e2i">>, <<"DistributionGaussianTail">>},
                                      {<<"i2r">>, <<"DistributionGaussianTail">>}]]}]),
                   #model_config{model_name = "SimpleSEIRExample",
                                 total_connections = 2,
                                 distributions = [#distribution{label = "DistributionGaussianTail",
                                                                type = gaussian_tail,
                                                                scale = undefined,
                                                                params = [0.0, 1.0, 10.0]}],
                                 agents = [#agent_config{label = "Patient0",
                                                         initial_state = infectious,
                                                         connections = ["Friend0", "Friend1"],
                                                         s2e = "DistributionGaussianTail",
                                                         e2i = "DistributionGaussianTail",
                                                         i2r = "DistributionGaussianTail"}]})
    ].

parse_distributions_test_() ->
    [?_assertEqual(parse_distributions([[{<<"label">>, <<"DistributionBinomial">>},
                                         {<<"type">>, <<"Binomial">>},
                                         {<<"scale">>, <<"10.0">>},
                                         {<<"params">>, [0.7, 20.0]}],
                                        [{<<"label">>, <<"DistributionGaussianTail">>},
                                         {<<"type">>, <<"GaussianTail">>},
                                         {<<"params">>, [0.0, 1.0, 10.0]}]]),
                   [#distribution{label = "DistributionBinomial",
                                  type = binomial,
                                  scale = <<"10.0">>,
                                  params = [0.7, 20.0]},
                    #distribution{label = "DistributionGaussianTail",
                                  type = gaussian_tail,
                                  scale = undefined,
                                 params = [0.0, 1.0, 10.0]}]),
     ?_assertException(throw, _, parse_distributions([[{<<"label">>, <<"DistributionBinomial">>},
                                                       {<<"type">>, <<"Binomial">>},
                                                       {<<"params">>, [0.7, 20.0]}],
                                                      [{<<"label">>, <<"DistributionBinomial">>},
                                                       {<<"type">>, <<"Binomial">>}]])),
     ?_assertException(throw, _, parse_distributions([[{<<"label">>, <<"DistributionBinomial">>},
                                                       {<<"type">>, <<"Binomial">>}],
                                                      [{<<"label">>, <<"DistributionBinomial">>},
                                                       {<<"type">>, <<"Binomial">>},
                                                       {<<"params">>, [0.7, 20.0]}]]))
    ].

parse_distribution_test_() ->
    [?_assertEqual(parse_distribution([{<<"label">>, <<"DistributionBinomial">>},
                                       {<<"type">>, <<"Binomial">>},
                                       {<<"params">>, [0.7, 20.0]}]),
                   #distribution{label = "DistributionBinomial",
                                 type = binomial,
                                 scale = undefined,
                                 params = [0.7, 20.0]}),
     ?_assertEqual(parse_distribution([{<<"label">>, <<"DistributionBinomial">>},
                                       {<<"type">>, <<"Binomial">>},
                                       {<<"scale">>, 10.0},
                                       {<<"params">>, []}]),
                   #distribution{label = "DistributionBinomial",
                                 type = binomial,
                                 scale = 10.0,
                                 params = []}),
     ?_assertException(throw, _, parse_distribution([{<<"type">>, <<"Binomial">>},
                                                     {<<"params">>, [0.7, 20.0]}])),
     ?_assertException(throw, _, parse_distribution([{<<"label">>, <<"DistributionBinomial">>},
                                                     {<<"params">>, [0.7, 20.0]}])),
     ?_assertException(throw, _, parse_distribution([{<<"label">>, <<"DistributionBinomial">>},
                                                     {<<"type">>, <<"Binomial">>}]))
    ].

parse_params_test_() ->
    [?_assertEqual(parse_params([4.0, 10.0]),
                   [4.0, 10.0]),
     ?_assertEqual(parse_params([4, 10]),
                   [4.0, 10.0]),
     ?_assertException(throw, _, parse_params([four, ten])),
     ?_assertException(throw, _, parse_params(["four", "ten"]))
    ].

parse_agents_test_() ->
    [?_assertEqual(parse_agents([[{<<"label">>, <<"Friend1">>},
                                  {<<"state">>, <<"Susceptible">>},
                                  {<<"connections">>, [<<"Friend0">>, <<"Stranger0">>]},
                                  {<<"s2e">>, <<"DistributionPoisson">>},
                                  {<<"e2i">>, <<"DistributionPoisson">>},
                                  {<<"i2r">>, <<"DistributionPoisson">>}]]),
                   [#agent_config{label = "Friend1",
                                  initial_state = susceptible,
                                  connections = ["Friend0", "Stranger0"],
                                  s2e = "DistributionPoisson",
                                  e2i = "DistributionPoisson",
                                  i2r = "DistributionPoisson"}]),
     ?_assertEqual(parse_agents([[{<<"label">>, <<"Patient0">>},
                                  {<<"state">>, <<"Infectious">>},
                                  {<<"connections">>, [<<"Friend0">>, <<"Friend1">>]},
                                  {<<"s2e">>, <<"DistributionGaussianTail">>},
                                  {<<"e2i">>, <<"DistributionGaussianTail">>},
                                  {<<"i2r">>, <<"DistributionGaussianTail">>}],
                                 [{<<"label">>, <<"Friend0">>},
                                  {<<"state">>, <<"Exposed">>},
                                  {<<"connections">>, [<<"Friend1">>, <<"Stranger0">>]},
                                  {<<"s2e">>, <<"DistributionExponential">>},
                                  {<<"e2i">>, <<"DistributionExponential">>},
                                  {<<"i2r">>, <<"DistributionExponential">>}]]),
                   [#agent_config{label = "Patient0",
                                  initial_state = infectious,
                                  connections = ["Friend0", "Friend1"],
                                  s2e = "DistributionGaussianTail",
                                  e2i = "DistributionGaussianTail",
                                  i2r = "DistributionGaussianTail"},
                    #agent_config{label = "Friend0",
                                  initial_state=exposed,
                                  connections = ["Friend1", "Stranger0"],
                                  s2e = "DistributionExponential",
                                  e2i = "DistributionExponential",
                                  i2r = "DistributionExponential"}]),
     ?_assertException(throw, _, parse_agents([[{<<"label">>, <<"Patient0">>},
                                                {<<"state">>, <<"Infectious">>},
                                                {<<"connections">>, [<<"Friend0">>, <<"Friend1">>]},
                                                {<<"s2e">>, <<"DistributionGaussianTail">>},
                                                {<<"e2i">>, <<"DistributionGaussianTail">>},
                                                {<<"i2r">>, <<"DistributionGaussianTail">>}],
                                               [{<<"label">>, <<"Friend1">>},
                                                {<<"state">>, <<"Susceptible">>},
                                                {<<"connections">>, [friend0, stranger0]},
                                                {<<"s2e">>, <<"DistributionPoisson">>},
                                                {<<"e2i">>, <<"DistributionPoisson">>},
                                                {<<"i2r">>, <<"DistributionPoisson">>}]])),
     ?_assertException(throw, _, parse_agents([[{<<"label">>, <<"Friend1">>},
                                                {<<"state">>, <<"Susceptible">>},
                                                {<<"connections">>, [friend0, stranger0]},
                                                {<<"s2e">>, <<"DistributionPoisson">>},
                                                {<<"e2i">>, <<"DistributionPoisson">>},
                                                {<<"i2r">>, <<"DistributionPoisson">>}],
                                               [{<<"label">>, <<"Patient0">>},
                                                {<<"state">>, <<"Infectious">>},
                                                {<<"connections">>, [<<"Friend0">>, <<"Friend1">>]},
                                                {<<"s2e">>, <<"DistributionGaussianTail">>},
                                                {<<"e2i">>, <<"DistributionGaussianTail">>},
                                                {<<"i2r">>, <<"DistributionGaussia`nTail">>}]]))
    ].

parse_connections_test_() ->
    [?_assertEqual(parse_connections([]),
                   []),
     ?_assertEqual(parse_connections([<<"Friend0">>]),
                   ["Friend0"]),
     ?_assertEqual(parse_connections([<<"Friend0">>, <<"Stranger0">>]),
                   ["Friend0", "Stranger0"]),
     ?_assertException(throw, _, parse_connections(friend0)),
     ?_assertException(throw, _, parse_connections(<<"Friend0">>)),
     ?_assertException(throw, _, parse_connections(4)),
     ?_assertException(throw, _, parse_connections([friend0, stranger0]))
    ].

validate_distributions_test_() ->
    [?_assertEqual(validate_distributions([#distribution{label = "DistributionPoisson",
                                                         type = poisson,
                                                         scale = undefined,
                                                         params = [4.0]},
                                           #distribution{label = "DistributionBinomial",
                                                         type = binomial,
                                                         scale = <<"10.0">>,
                                                         params=[0.7, 20.0]}],
                                          ["DistributionPoisson", "DistributionBinomial"]),
                   [#distribution{label = "DistributionPoisson",
                                  type = poisson,
                                  scale = 1.0,
                                  params = [4.0]},
                    #distribution{label = "DistributionBinomial",
                                  type = binomial,
                                  scale = 10.0,
                                  params = [0.7,20.0]}]),
     ?_assertException(throw, _, validate_distributions([#distribution{label = "DistributionPoisson",
                                                                       type = poisson,
                                                                       params = [4.0]},
                                                         #distribution{label = "DistributionBinomial",
                                                                       type = binomial,
                                                                       params = [0.7, 20.0]}],
                                                        ["Bob", "DistributionBinomial"])),
     ?_assertException(throw, _, validate_distributions([#distribution{label = "DistributionBinomial",
                                                                       type = poisson,
                                                                       params = [4.0]},
                                                         #distribution{label = "DistributionBinomial",
                                                                       type = binomial,
                                                                       params = [0.7, 20.0]}],
                                                        ["DistributionBinomial", "DistributionBinomial"]))
    ].

validate_agents_test() ->
    Expected = [#agent_config{label = "Patient0",
                              initial_state = infectious,
                              connections = ["Friend0"],
                              s2e = "DistributionGaussianTail",
                              e2i = "DistributionGaussianTail",
                              i2r = "DistributionGaussianTail",
                              r2s = "DistributionGaussianTail"},
                #agent_config{label = "Friend0",
                              initial_state = exposed,
                              connections = ["Patient0"],
                              s2e = "DistributionExponential",
                              e2i = "DistributionExponential",
                              i2r = "DistributionExponential"}],
    Returned = validate_agents([#agent_config{label = "Patient0",
                                              initial_state = infectious,
                                              connections = ["Friend0"],
                                              s2e = "DistributionGaussianTail",
                                              e2i = "DistributionGaussianTail",
                                              i2r = "DistributionGaussianTail",
                                              r2s = "DistributionGaussianTail"},
                                #agent_config{label = "Friend0",
                                              initial_state=exposed,
                                              connections = ["Patient0"],
                                              s2e = "DistributionExponential",
                                              e2i = "DistributionExponential",
                                              i2r = "DistributionExponential"}],
                               ["DistributionExponential", "DistributionGaussianTail"]),
    ?assertEqual(Expected, Returned).

validate_model_test() ->
    Expected = #model_config{model_name = "SimpleSEIRExample",
                             distributions = [#distribution{label = "DistributionPoisson",
                                                            type = poisson,
                                                            scale = 1.0,
                                                            params = [4.0]},
                                              #distribution{label = "DistributionBinomial",
                                                            type = binomial,
                                                            scale = 10.0,
                                                            params = [0.7, 20.0]}],
                             agents = [#agent_config{label = "Patient0",
                                                     initial_state = infectious,
                                                     connections = ["Friend0"],
                                                     s2e = "DistributionPoisson",
                                                     e2i = "DistributionPoisson",
                                                     i2r = "DistributionPoisson",
                                                     r2s = "DistributionPoisson"},
                                     #agent_config{label = "Friend0",
                                                   initial_state = exposed,
                                                   connections = ["Patient0"],
                                                   s2e = "DistributionBinomial",
                                                   e2i = "DistributionBinomial",
                                                   i2r = "DistributionBinomial",
                                                   r2s = undefined}]},
    Returned = validate_model(#model_config{model_name = "SimpleSEIRExample",
                                            distributions = [#distribution{label = "DistributionPoisson",
                                                                           type = poisson,
                                                                           params = [4.0]},
                                                             #distribution{label = "DistributionBinomial",
                                                                           type = binomial,
                                                                           scale = <<"10.0">>,
                                                                           params = [0.7, 20.0]}],
                                            agents=[#agent_config{label = "Patient0",
                                                                  initial_state = infectious,
                                                                  connections = ["Friend0"],
                                                                  s2e = "DistributionPoisson",
                                                                  e2i = "DistributionPoisson",
                                                                  i2r = "DistributionPoisson",
                                                                  r2s = "DistributionPoisson"},
                                                    #agent_config{label = "Friend0",
                                                                  initial_state = exposed,
                                                                  connections = ["Patient0"],
                                                                  s2e = "DistributionBinomial",
                                                                  e2i = "DistributionBinomial",
                                                                  i2r = "DistributionBinomial"}]}),
    ?assertEqual(Expected, Returned).
-endif.
