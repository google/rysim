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
%% Factory function that creates an empty queue and returns it the caller.  The
%% returned value needs to be passed into other API functions, but should be
%% treated as opaque outside this module.
%% @spec create() -> {ok, Data}
%%       Data = record(event_queue_data)
%% @end
%% --------------------------------------------------------------------
create() ->
    {ok, #event_queue_data{body = gb_trees:empty()}}.

%%--------------------------------------------------------------------
%% @doc
%% Return first entry of the queue. In the event the queue is empty,
%% undefined is returned.
%% @spec top_event(Data) -> {ok, undefined} | {ok, Event}
%%       Data = record(event_queue_data)
%%       Event = record(sim_event)
%% @end
%% --------------------------------------------------------------------
top_event(Data) when is_record(Data, event_queue_data) ->
    Queue = Data#event_queue_data.body,
    case gb_trees:is_empty(Queue) of
        true ->
            {ok, undefined};
        _ ->
            {_, [Event|_]} = gb_trees:smallest(Queue),
            {ok, Event}
    end;
top_event(Data) ->
    error_logger:error_msg("~p:~p Unable to top_event, from ~p, due to bad type!",
                           [?MODULE, ?LINE, Data]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Remove the first entry from the queue and returns the new queue.
%% Pass through to the underlying behaviour implementation.
%% @spec pop_event(Data) -> {ok, NewData}
%%       Data = record(event_queue_data)
%%       NewData = record(event_queue_data)
%% @end
%% --------------------------------------------------------------------
pop_event(Data) when is_record(Data, event_queue_data) ->
    Queue = Data#event_queue_data.body,
    case gb_trees:is_empty(Queue) of
        true ->
            {ok, Data};
        _ ->
            remove_event(Data)
    end;
pop_event(Data) ->
    error_logger:error_msg("~p:~p Unable to pop_event, from ~p, due to bad type!",
                           [?MODULE, ?LINE, Data]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Adds a new sim_event to the queue using its timestamp's absolute
%% value as the key.
%% @spec push_event(Event, Data) -> {ok, NewData}
%%       Event = record(sim_event)
%%       Data = record(event_queue_data)
%%       NewData = record(event_queue_data)
%% @end
%% --------------------------------------------------------------------
push_event(Event, Data) when is_record(Event, sim_event),
                             is_record(Data, event_queue_data) ->
    Key = erlang:abs(Event#sim_event.timestamp),
    OldQueue = Data#event_queue_data.body,
    NewQueue = case gb_trees:lookup(Key, OldQueue) of
                   none ->
                       gb_trees:insert(Key, [Event], OldQueue);
                   {value, Events} ->
                       gb_trees:update(Key, [Event|Events], OldQueue)
               end,
    NewData = Data#event_queue_data{body = NewQueue},
    {ok, NewData};
push_event(Event, Data) ->
    error_logger:error_msg("~p:~p Unable to push_event, ~p to ~p, due to bad type!",
                           [?MODULE, ?LINE, Event, Data]),
    throw(badarg).


%% ===================================================================
%% Private
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Removes the top event from the queue. This call will fail if the tree
%% is empty.
%% @spec remove_event(Data) -> {ok, NewData}
%%       Data = record(event_queue_data)
%%       NewData = record(event_queue_data)
%% @end
%% --------------------------------------------------------------------
remove_event(Data) when is_record(Data, event_queue_data)->
    OldQueue = Data#event_queue_data.body,
    {Key, [_|Events]} = gb_trees:smallest(OldQueue),
    NewQueue = case Events == [] of
                   true ->
                       gb_trees:delete(Key, OldQueue);
                   _ ->
                       gb_trees:update(Key, Events, OldQueue)
               end,
    NewData = Data#event_queue_data{body = NewQueue},
    {ok, NewData}.
