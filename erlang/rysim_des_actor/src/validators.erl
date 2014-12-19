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
%%% Implementation of various validators used during the parsing of
%%% input files.
%%% @end
%%%-------------------------------------------------------------------
-module(validators).

-include("rysim.hrl").

-export([validate_params/2]).


%% ===================================================================
%% API
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Validates the list of provided parameters is of the correct length
%% and type for the given distribution type.
%% @spec validate_params(Type :: distribution_type(), Params :: [float()]) -> Params :: [float()]
%% @end
%% --------------------------------------------------------------------
validate_params(gaussian_tail, Params) when is_list(Params),
                                            length(Params) == 2 ->
    validate_params_acc(Params, []);
validate_params(exponential, Params) when is_list(Params),
                                          length(Params) == 1 ->
    validate_params_acc(Params, []);
validate_params(flat, Params) when is_list(Params),
                                   length(Params) == 2 ->
    validate_params_acc(Params, []);
validate_params(lognormal, Params) when is_list(Params),
                                        length(Params) == 2 ->
    validate_params_acc(Params, []);
validate_params(poisson, Params) when is_list(Params),
                                      length(Params) == 1 ->
    validate_params_acc(Params, []);
validate_params(bernoulli, Params) when is_list(Params),
                                        length(Params) == 1 ->
    validate_params_acc(Params, []);
validate_params(binomial, Params) when is_list(Params),
                                       length(Params) == 2 ->
    validate_params_acc(Params, []);
validate_params(negative_binomial, Params) when is_list(Params),
                                                length(Params) == 2 ->
    validate_params_acc(Params, []);
validate_params(geometric, Params) when is_list(Params),
                                        length(Params) == 1 ->
    validate_params_acc(Params, []);
validate_params(Type, Params) ->
    error_logger:error_msg("~p:~p: Unable to validate_params with Type = ~p and Params = ~p!",
                           [?MODULE, ?LINE, Type, Params]),
    throw(badparams).

%% ===================================================================
%% Private Functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Validates the list of provided parameters is of the correct type.
%% @spec validate_params_acc(Type :: string, Params :: [float()]) -> Params :: [float()]
%% @end
%% --------------------------------------------------------------------
validate_params_acc([], Result) when is_list(Result) ->
    lists:reverse(Result);
validate_params_acc([Head|Tail], List) when is_number(Head),
		                                        is_list(Tail),
		                                        is_list(List) ->
    validate_params_acc(Tail, [Head|List]);
validate_params_acc(_, _) ->
    throw(badparam).
