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
%%% Low level details of parsing an experiment input file
%%% @end
%%%-------------------------------------------------------------------
-module(parsers_experiment).

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
%% @spec process(File :: string()) -> ExperimentConfig
%%       ExperimentConfig = record(experiment_config)
%% @end
%%--------------------------------------------------------------------
process(File) ->
    {ok, Binary} = file:read_file(File),
    Blob = jsx:decode(Binary),
    ExperimentConfig = parse_experiment(Blob),
    Result = validate_experiment(ExperimentConfig),
    Dir = filename:dirname(File),
    ModelFilename = Result#experiment_config.model_filename,
    {ok, Result#experiment_config{model_filename = filename:join(Dir, ModelFilename)}}.


%% ===================================================================
%% Private functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Takes the outputted blob from jiffy and returns a properly formated
%% experiment record.
%% @spec parse_experiment(Blob :: EJSON) -> ExperimentConfig
%%       ExperimentConfig = record(experiment_config)
%% @end
%%--------------------------------------------------------------------
parse_experiment(Blob) when is_list(Blob) ->
    parse_experiment_acc(Blob, #experiment_config{});
parse_experiment(_) ->
    throw(malformedblob).

%%--------------------------------------------------------------------
%% @doc
%% Walks through the blob provided by yamerl and fills in the details of
%% an experiment record based on it.
%% @spec parse_experiment_acc(Blob :: EJSON, Result) -> ExperimentConfig
%%       Result = record(experiment_config)
%%       ExperimentConfig = record(experiment_config)
%% @end
%%--------------------------------------------------------------------
parse_experiment_acc([], Result) when is_record(Result, experiment_config),
                                      Result#experiment_config.experiment_name /= undefined,
                                      Result#experiment_config.experiment_type /= undefined,
                                      Result#experiment_config.event_limit /= undefined,
                                      Result#experiment_config.model_filename /= undefined ->
    Result;
parse_experiment_acc([{<<"experiment_name">>, Name} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, experiment_config),
       OldResult#experiment_config.experiment_name == undefined ->
    NewResult = OldResult#experiment_config{experiment_name = erlang:binary_to_list(Name)},
    parse_experiment_acc(Tail, NewResult);
parse_experiment_acc([{<<"type">>, Type} | Tail], OldResult)
    when is_list(Tail),
    is_record(OldResult, experiment_config),
    OldResult#experiment_config.experiment_type == undefined ->
    NewResult = OldResult#experiment_config{experiment_type = erlang:binary_to_list(Type)},
    parse_experiment_acc(Tail, NewResult);
parse_experiment_acc([{<<"event_limit">>, Limit} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, experiment_config),
       OldResult#experiment_config.event_limit == undefined ->
    NewResult = OldResult#experiment_config{event_limit = Limit},
    parse_experiment_acc(Tail, NewResult);
parse_experiment_acc([{<<"model_filename">>, Filename} | Tail], OldResult)
  when is_list(Tail),
       is_record(OldResult, experiment_config),
       OldResult#experiment_config.model_filename == undefined ->
    NewResult = OldResult#experiment_config{model_filename = erlang:binary_to_list(Filename)},
    parse_experiment_acc(Tail, NewResult);
parse_experiment_acc(ExperimentConfig, Result) ->
    error_logger:error_msg("~p:~p Malformed experiment ~p~n~p~n",
                           [?MODULE, ?LINE, ExperimentConfig, Result]),
    throw(malformedexperiment).

%%--------------------------------------------------------------------
%% @doc
%% Takes in a experiment record and returns it if it is valid.
%% @spec validate_experiment(ExperimentConfig) -> Result
%%       ExperimentConfig = record(experiment_config)
%%       Result = record(experimentConfig)
%% @end
%%--------------------------------------------------------------------
validate_experiment(ExperimentConfig) when is_record(ExperimentConfig, experiment_config) ->
    #experiment_config{experiment_name = ExperimentConfig#experiment_config.experiment_name,
                       experiment_type = ExperimentConfig#experiment_config.experiment_type,
                       event_limit = validate_event_limit(ExperimentConfig#experiment_config.event_limit),
                       model_filename = ExperimentConfig#experiment_config.model_filename}.

%%--------------------------------------------------------------------
%% @doc
%% Takes in an entry for the event limit and attempts to convert it to an
%% integer and return it. The value needs to be non-negative.
%% @spec validate_event_limit(Count :: integer() | float() | string()) -> Result :: integer()
%% @end
%%--------------------------------------------------------------------
validate_event_limit(Count) when is_integer(Count),
                                 Count >= 0 ->
    Count;
validate_event_limit(Count) when is_float(Count) ->
    Round = erlang:round(Count),
    case Round >= 0 of
        true ->
            Round;
        _ ->
            throw(badtimelimit)
    end;
validate_event_limit(Count) when is_list(Count) ->
    try erlang:list_to_integer(Count) of
        Result ->
            case Result >= 0 of
                true ->
                    Result;
                _ ->
                    throw(badtimelimit)
            end
    catch
        _ -> throw(badtimelimit)
    end;
validate_event_limit(_) ->
    throw(badeventlimit).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_experiment_test_() ->
    [?_assertEqual(parse_experiment([{<<"experiment_name">>, <<"SimpleSequentialExperiment">>},
                                     {<<"type">>, <<"simple">>},
                                     {<<"event_limit">>, 0},
                                     {<<"model_filename">>, <<"1_test.json">>}]),
                   #experiment_config{experiment_name = "SimpleSequentialExperiment",
                                      experiment_type = "simple",
                                      event_limit = 0,
                                      model_filename = "1_test.json"}),
     ?_assertException(throw, _, parse_experiment([{<<"event_limit">>, 0},
                                                     {<<"model_filename">>, <<"1_test.json">>}])),
     ?_assertException(throw, _, parse_experiment([{<<"experiment_name">>, <<"SimpleSequentialExperiment">>},
                                                     {<<"model_filename">>, <<"1_test.yaml">>}])),
     ?_assertException(throw, _, parse_experiment([{<<"experiment_name">>, <<"SimpleSequentialExperiment">>},
                                                     {"event_limit", 0}]))
    ].

-endif.
