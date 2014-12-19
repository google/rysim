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
-module(event_queue_SUITE).

-include("rysim.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([test_push/1, test_push_multiple/1, test_push_pop/1,
         test_push_push_pop_pop/1, test_pop_empty/1]).

all() ->
     [test_push,
     test_push_multiple,
     test_push_pop,
     test_push_push_pop_pop,
     test_pop_empty].

init_per_testcase(_, Config) ->
    {ok, State} = event_queue:create(),
    [{mod_state, State} |Config].

end_per_testcase(_, _) ->
    ok.


%% ===================================================================
%% Tests
%% ===================================================================
test_push(Config) ->
    State = ?config(mod_state, Config),
    Event = #sim_event{timestamp=1,
                       sender="Fred",
                       receiver="Wilma",
                       body=undefined},
    {ok, NewState} = event_queue:push_event(Event, State),
    ok = unique([State, NewState]).

test_push_multiple(Config) ->
    State = ?config(mod_state, Config),
    FirstEvent = #sim_event{timestamp=1,
                            sender="Fred",
                            receiver="Wilma",
                            body=undefined},
    SecondEvent = #sim_event{timestamp=2,
                            sender="Barney",
                            receiver="Betty",
                            body=undefined},
    {ok, NewState0} = event_queue:push_event(FirstEvent, State),
    {ok, NewState1} = event_queue:push_event(SecondEvent, NewState0),
    ok = unique([State, NewState0, NewState1]).

test_push_pop(Config) ->
    State = ?config(mod_state, Config),
    Event = #sim_event{timestamp=1,
                       sender="Fred",
                       receiver="Wilma",
                       body=undefined},
    {ok, NewState0} = event_queue:push_event(Event, State),
    {ok, Event} = event_queue:top_event(NewState0),
    {ok, NewState1} = event_queue:pop_event(NewState0),
    ok = unique([State, NewState0]),
    ok = unique([NewState0, NewState1]),
    State = NewState1.

test_push_push_pop_pop(Config) ->
    State = ?config(mod_state, Config),
    FirstEvent = #sim_event{timestamp=1,
                            sender="Fred",
                            receiver="Wilma",
                            body=undefined},
    SecondEvent = #sim_event{timestamp=2,
                            sender="Barney",
                            receiver="Betty",
                            body=undefined},
    {ok, NewState0} = event_queue:push_event(SecondEvent, State),
    {ok, NewState1} = event_queue:push_event(FirstEvent, NewState0),
    ok = unique([State, NewState0, NewState1]),
    {ok, FirstEvent} = event_queue:top_event(NewState1),
    {ok, NewState2} = event_queue:pop_event(NewState1),
    {ok, SecondEvent} = event_queue:top_event(NewState2),
    {ok, NewState3} = event_queue:pop_event(NewState2),
    ok = unique([NewState1, NewState2, NewState3]),
    State = NewState3.

test_pop_empty(Config) ->
    State = ?config(mod_state, Config),
    {ok, undefined} = event_queue:top_event(State),
    {ok, State} = event_queue:pop_event(State).


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
