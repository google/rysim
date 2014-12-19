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
-module(event_queue_server).

-include("rysim.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, gb_trees:empty()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(top_event, _From, State) ->
    Reply = case gb_trees:is_empty(State) of
        true ->
            {ok, undefined};
        _ ->
            {_, [Event|_]} = gb_trees:smallest(State),
            {ok, Event}
    end,
    {reply, Reply, State};
handle_call(Msg, From, State) ->
    error_logger:error_msg("~p:~p Received unexpected call, message = ~p, from = ~p!",
                           [?MODULE, ?LINE, Msg, From]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(pop_event, State) ->
    NewState = case gb_trees:is_empty(State) of
                   true ->
                       State;
                   _ ->
                       remove_event(State)
               end,
    {noreply, NewState};
handle_cast({push_event, Event}, State) when is_record(Event, sim_event) ->
    Key = erlang:abs(Event#sim_event.timestamp),
    NewState = case gb_trees:lookup(Key, State) of
                   {value, Events} ->
                       gb_trees:update(Key, [Event|Events], State);
                   _ ->
                       gb_trees:insert(Key, [Event], State)
               end,
    {noreply, NewState};
handle_cast(Msg, State) ->
    error_logger:error_msg("~p:~p Received unexpected cast, message = ~p!",
                           [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Removes the top event from the queue and returns the new state. This call
%% will fail if the tree is empty.
%% @spec remove_event(State) -> NewState
%%       State = record(gb_trees:tree())
%%       NewState = record(gb_trees:tree())
%% @end
%% --------------------------------------------------------------------
remove_event(State) ->
    {Key, [_|Events]} = gb_trees:smallest(State),
    case Events == [] of
        true ->
            gb_trees:delete(Key, State);
        _ ->
            gb_trees:update(Key, Events, State)
    end.
