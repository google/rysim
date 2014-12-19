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
    {ok, Pid} = event_queue:create(),
    [{eq_pid, Pid} |Config].

end_per_testcase(_, _) ->
    ok.


%% ===================================================================
%% Tests
%% ===================================================================
test_push(Config) ->
    Pid = ?config(eq_pid, Config),
    Event = #sim_event{timestamp=1,
                       sender="Fred",
                       receiver="Wilma",
                       body=undefined},
    ok = event_queue:push_event(Event, Pid).

test_push_multiple(Config) ->
    Pid = ?config(eq_pid, Config),
    FirstEvent = #sim_event{timestamp=1,
                            sender="Fred",
                            receiver="Wilma",
                            body=undefined},
    SecondEvent = #sim_event{timestamp=2,
                            sender="Barney",
                            receiver="Betty",
                            body=undefined},
    ok = event_queue:push_event(FirstEvent, Pid),
    ok = event_queue:push_event(SecondEvent, Pid).

test_push_pop(Config) ->
    Pid = ?config(eq_pid, Config),
    Event = #sim_event{timestamp=1,
                       sender="Fred",
                       receiver="Wilma",
                       body=undefined},
    ok = event_queue:push_event(Event, Pid),
    {ok, Event} = event_queue:top_event(Pid),
    ok = event_queue:pop_event(Pid).

test_push_push_pop_pop(Config) ->
    Pid = ?config(eq_pid, Config),
    FirstEvent = #sim_event{timestamp=1,
                            sender="Fred",
                            receiver="Wilma",
                            body=undefined},
    SecondEvent = #sim_event{timestamp=2,
                            sender="Barney",
                            receiver="Betty",
                            body=undefined},
    ok = event_queue:push_event(SecondEvent, Pid),
    ok = event_queue:push_event(FirstEvent, Pid),
    {ok, FirstEvent} = event_queue:top_event(Pid),
    ok = event_queue:pop_event(Pid),
    {ok, SecondEvent} = event_queue:top_event(Pid),
    ok = event_queue:pop_event(Pid).

test_pop_empty(Config) ->
    Pid = ?config(eq_pid, Config),
    {ok, undefined} = event_queue:top_event(Pid),
    ok = event_queue:pop_event(Pid).
