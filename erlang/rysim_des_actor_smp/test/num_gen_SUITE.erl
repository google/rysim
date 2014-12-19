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
-module(num_gen_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([test_simple_generation/1, test_multiple_dists/1]).

all() ->
     [test_simple_generation,
      test_multiple_dists].

init_per_testcase(_, Config) ->
    {ok, State} = num_gen:initialize_generator(1),
    [{mod_state, State}|Config].

end_per_testcase(_, _) ->
    ok.


%% ===================================================================
%% Tests
%% ===================================================================
test_simple_generation(Config) ->
    State = ?config(mod_state, Config),
    Label = "Bob",
    Scale = 1.0,
    DistType = gaussian_tail,
    Params = [1.0, 2.0],
    {ok, NewState} = num_gen:register_distribution(Label, DistType, Scale, Params, State),
    {ok, _} = num_gen:call_distribution(Label, NewState),
    ok = unique([State, NewState]).

test_multiple_dists(Config) ->
    State = ?config(mod_state, Config),
    Scale = 1.0,
    DistType = gaussian_tail,
    Params = [1.0, 2.0],
    {ok, NewState0} = num_gen:register_distribution("Bob", DistType, Scale, Params, State),
    {ok, _} = num_gen:call_distribution("Bob", NewState0),
    {ok, NewState1} = num_gen:register_distribution("Bill", DistType, Scale, Params, NewState0),
    {ok, _} = num_gen:call_distribution("Bill", NewState1),
    {ok, _} = num_gen:call_distribution("Bob", NewState1),
    {ok, NewState2} = num_gen:register_distribution("Ben", DistType, Scale, Params, NewState1),
    {ok, _} = num_gen:call_distribution("Ben", NewState2),
    {ok, _} = num_gen:call_distribution("Bill", NewState2),
    {ok, _} = num_gen:call_distribution("Bob", NewState2),
    ok = unique([State, NewState0, NewState1, NewState2]).


%% ===================================================================
%% Utilities
%% ===================================================================
unique([]) ->
    ok;
unique([Head|Tail]) ->
    case lists:any(fun(Item) ->
                          (Item == Head)
                  end,
                  Tail) of
        true ->
            error;
        _ ->
            unique(Tail)
    end.
