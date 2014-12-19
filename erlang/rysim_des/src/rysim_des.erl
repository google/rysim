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
%%% Top level implementation of RySim binary
%%% @end
%%%-------------------------------------------------------------------
-module(rysim_des).

-include("rysim.hrl").

%% Main
-export([main/1]).


%% ===================================================================
%% Main
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Top level function that takes in the command line arguements, parses
%% them, pases them to the simulation system, runs the simulation, and
%% then prints results.
%% along the calls that need to be made.
%% @spec main(Args :: [string()]) -> ok
%% @end
%%--------------------------------------------------------------------
main(Args) when is_list(Args)->
    io:format("Running rysim ~p~n", [Args]),
    {ok, CommandLine} = parse_command_line(Args),
    io:format("Command Line ~p~n", [CommandLine]),
    ValidatedCommandLine = validate_command_line(CommandLine),
    Seed = orddict:fetch(random_seed, ValidatedCommandLine),
    io:format("Using random seed ~p~n", [Seed]),
    File = orddict:fetch(experiment_file, CommandLine),
    io:format("Using experiment file ~p~n", [File]),
    case os:getenv("PROFILING") of
        false ->
            ok;
        _ ->
            eprof:start(),
            eprof:profile([self()])
     end,
    {ok, ControllerData} = simulation_controller:create(File, Seed),
    {ok, Results} = simulation_controller:run_experiment(ControllerData),
    case os:getenv("PROFILING") of
        false ->
            ok;
        _ ->
            eprof:stop_profiling(),
            eprof:analyze(total)
    end,
    print_results(Results).


%% ===================================================================
%% Private Functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Prints the usage instructions for RySim.
%% @spec print_usage() -> ok
%% @end
%%--------------------------------------------------------------------
print_usage() ->
    io:format("rysim: Run simulation experiment using sequential kernel~n"),
    io:format("  --experiment_file <file>~n"),
    io:format("    JSON file that contains experiment specification.~n"),
    io:format("    This must be specified and the file must be readable.~n"),
    io:format("  --random_seed <seed>~n"),
    io:format("    Integer to seed random number generation with, defaults to 1.~n").

%%--------------------------------------------------------------------
%% @doc
%% Takes in the provided command line arguments and retuns an orddict
%% with the params in it.
%% @spec parse_command_line(Args :: [string()]) -> Dict :: orddict()
%% @end
%%--------------------------------------------------------------------
parse_command_line(Args) when is_list(Args) ->
    Dict = orddict:store(params, [], orddict:new()),
    parse_command_line_acc(Args, Dict).

%%--------------------------------------------------------------------
%% @doc
%% Walks through the provided command line arguments and parses them
%% into the parameter orddict.
%% @spec parse_command_line_acc(Args :: [string()], Results :: orddict()) -> Dict :: orddict()
%% @end
%%--------------------------------------------------------------------
parse_command_line_acc([], Dict) ->
    {ok, Dict};
parse_command_line_acc([Head|Tail], OldDict) when is_list(Head),
		                                              is_list(Tail) ->
    {NewDict, NewTail} = case re:run(Head, "^--.*") of
                             {match, _} ->
                                 parse_command_flag(Head, Tail, OldDict);
                             _ ->
                                 parse_command_param(Head,Tail, OldDict)
                         end,
    parse_command_line_acc(NewTail, NewDict).

%%--------------------------------------------------------------------
%% @doc
%% Takes in a single element from the command line argument and adds it the
%% orddict, with any values specified for it.
%% @spec parse_command_flag(RawFlag :: string(), [string()], Dict :: orddict()) -> NewDict :: orddict()
%% @end
%%--------------------------------------------------------------------
parse_command_flag(RawFlag, [Value|Tail], OldDict) when is_list(Tail) ->
    TrimmedFlag = string:substr(RawFlag, 3),
    Flag = list_to_atom(TrimmedFlag),
    NewDict = case orddict:find(Flag, OldDict) of
                  {ok, _} ->
                      orddict:append(Flag, Value, OldDict);
                  _ ->
                      orddict:store(Flag, Value, OldDict)
              end,
    {NewDict, Tail}.

%%--------------------------------------------------------------------
%% @doc
%% Takes in a single element from the command line arguments and parses
%% out a provided value.
%% @spec parse_command_param(Param :: string(), Tail :: [string()], Dict :: orddict()) -> NewDict :: orddict()
%% @end
%%--------------------------------------------------------------------
parse_command_param(Param, Tail, OldDict) when is_list(Param),
		                                           is_list(Tail) ->
    NewDict = orddict:append(params, Param, OldDict),
    {NewDict, Tail}.


%%--------------------------------------------------------------------
%% @doc
%% Takes in a dict of command arguments as an orddict and returns a verified
%% orddict that is guarantee to contain all the needed elements. This may
%% involve inserting defaults.
%% @spec validate_command_line(Dict :: orddict()) -> NewDict :: orddict()
%% @end
%%--------------------------------------------------------------------
validate_command_line(Dict) ->
    validate_random_seed(validate_experiment_file(Dict)).

%%--------------------------------------------------------------------
%% @doc
%% Makes sure that the random_seed value is an integer, inserting 1 if need be
%% for a default.
%% @spec validate_random_seed(Dict :: orddict()) -> NewDict :: orddict()
%% @end
%%--------------------------------------------------------------------
validate_random_seed(Dict) ->
    case orddict:is_key(random_seed, Dict) of
        true ->
            SeedVal = orddict:fetch(random_seed, Dict),
            case is_integer(SeedVal) of
                true ->
                    Dict;
                _ ->
                    {SeedInt, _} = string:to_integer(SeedVal),
                    orddict:store(random_seed, SeedInt, Dict)
            end;
        _ ->
            io:format("Using default random seed of 1~n", []),
            orddict:store(random_seed, 1, Dict)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Makes sure that there is an experiment_file value set, throwing an error if
%% it isn't.
%% @spec validate_experiment_file(Dict :: orddict()) -> NewDict :: orddict()
%% @end
%%--------------------------------------------------------------------
validate_experiment_file(Dict) ->
    case orddict:is_key(experiment_file, Dict) of
               true ->
            Dict;
        _ ->
            print_usage(),
            throw(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Print results from the simulation in a manner that easy to parse for
%% the experiment runner script.
%% @spec print_results(Results) -> ok
%%       Results = record(sim_results)
%% @end
%%--------------------------------------------------------------------
print_results(Results) when is_record(Results, sim_results) ->

    io:format("JSONBEGIN~n", []),
    io:format("~p~n", [erlang:binary_to_list(
                        jsx:encode([{<<"experiment">>, erlang:list_to_binary(Results#sim_results.experiment_name)},
                                    {<<"kernel">>, <<"ErlangDES">>},
                                    {<<"type">>, erlang:list_to_binary(Results#sim_results.experiment_type)},
                                    {<<"model">>, erlang:list_to_binary(Results#sim_results.model_name)},
                                    {<<"event_count">>, Results#sim_results.event_count},
                                    {<<"final_time">>, Results#sim_results.final_time},
                                    {<<"agents">>, Results#sim_results.agent_count},
                                    {<<"connections">>, Results#sim_results.total_connections}]))]),
    io:format("JSONEND~n", []),
    ok.
