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
%%% Wrapper that allows other modules to call into the parsing code without
%%% having to know the horrible internals of the parsing functions.
%%% @end
%%%-------------------------------------------------------------------
-module(parsers).

%% API
-export([init/0, process_experiment/1, process_model/1]).


%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Make the calls that are needed to get the application that the
%% parsing code depends initialized.
%% @spec init() -> ok
%% @end
%% --------------------------------------------------------------------
init() ->
    application:load(jsx),
    application:start(jsx),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Call into the appropriate module to parse the given experiment file.
%% @spec process_experiment(File :: string()) -> ExperimentConfig
%%       Experiment = record(experiment_config)
%% @end
%% --------------------------------------------------------------------
process_experiment(File) ->
    parsers_experiment:process(File).

%%--------------------------------------------------------------------
%% @doc
%% Call into the appropriate module to parse the given model
%% file. Currently only SEIR is implemented.
%% @spec process_model(File :: string()) -> Model
%%       Model = record(model_config)
%% @end
%% --------------------------------------------------------------------
process_model(File) ->
    parsers_model:process(File).