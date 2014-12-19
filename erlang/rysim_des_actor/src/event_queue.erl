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
-module(event_queue).

-include("rysim.hrl").

%% API
-export([create/0, top_event/1, pop_event/1, push_event/2]).


%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Factory function that creates an instance of event queue. The
%% returned value needs to be passed into other API functions, but should be
%% treated as opaque outside this module.
%% @spec create() -> {ok, pid()}
%% @end
%% --------------------------------------------------------------------
create() ->
    gen_server:start_link(event_queue_server, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Return first entry of the queue. In the event the queue is empty,
%% undefined is returned.
%% @spec top_event(Pid :: pid()) -> {ok, undefined} | {ok, Event}
%%       Event = record(sim_event)
%% @end
%% --------------------------------------------------------------------
top_event(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, top_event, infinity);
top_event(Pid) ->
    error_logger:error_msg("~p:~p Malformed call to top_event, [~p]!",
                           [?MODULE, ?LINE, Pid]).

%%--------------------------------------------------------------------
%% @doc
%% Remove the first entry from the queue.
%% @spec pop_event(Pid :: pid()) -> ok
%% @end
%% --------------------------------------------------------------------
pop_event(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, pop_event);
pop_event(Pid) ->
    error_logger:error_msg("~p:~p Malformed call to pop_event, [~p]!",
                           [?MODULE, ?LINE, Pid]).

%%--------------------------------------------------------------------
%% @doc
%% Adds a new sim_event to the queue using its timestamp's absolute
%% value as the key.
%% @spec push_event(Event, Pid) -> ok
%%       Event = record(sim_event)
%% @end
%% --------------------------------------------------------------------
push_event(Event, Pid) when is_record(Event, sim_event),
                            is_pid(Pid) ->
    gen_server:cast(Pid, {push_event, Event});
push_event(Event, Pid) ->
    error_logger:error_msg("~p:~p Malformed call to push_event, [~p, ~p]!",
                           [?MODULE, ?LINE, Event, Pid]).
