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
%%% Allows for registration of distribution specifications and generation of
%%% random numbers based on those specifications.
%%%
%%% == Records ==
%%% === record(dist_call) ===
%%% <em>label</em> is a user provided string that is associated with a
%%% distribution. This string must be unique amongst other labels.
%%%
%%% <em>scale</em> is a scaling factor to be applied to the results
%%% generated from the distribution. This value should not be negative,
%%% though the system does not enforce this.
%%%
%%% <em>params</em> is a list of floats provided as the control
%%% parameters for the distribution. The number of entries is dependent
%%% on the specific value of type. The expected number entries for each
%%% value of type are as follows:
%%% <ul>
%%% <li>gaussian_tail = 2</li>
%%% <li>exponential = 1</li>
%%% <li>flat = 2</li>
%%% <li>lognormal = 2</li>
%%% <li>poisson = 1</li>
%%% <li>bernoulli = 1</li>
%%% <li>binomial = 2</li>
%%% <li>negative_binomial = 2</li>
%%% <li>geometric = 1</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(num_gen).

-include("rysim.hrl").

-export([initialize_generator/1, register_distribution/5,
         call_distribution/2]).


%% ===================================================================
%% Records
%% ===================================================================
-record(dist_call, {type = "" :: string(),
                    scale = 1.0 :: float(),
                    params = [] :: [float()]}).

%% ===================================================================
%% API
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initialize the state of the random number generator to be used by the
%% probability distributions. Calling this affects the global random
%% seed, so should probably only ever be called once for consistency.
%% @spec initialize_generator(Seed :: integer()) -> {ok, NewData}
%%       NewData = record(ng_data)
%% @end
%% --------------------------------------------------------------------
initialize_generator(Seed) when is_integer(Seed) ->
    random:seed(Seed, Seed, Seed),
    {ok, #ng_data{dists=gb_trees:empty()}};
initialize_generator(Seed) ->
    error_logger:error_msg("~p:~p: Invalid call to initialize_generator, ~p!",
                          [?MODULE, ?LINE, Seed]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Registers a distribution call for future usage.
%% @spec register_distribution(Label :: string(), Type :: distribution_type(), Scale :: float(), Params :: [float()],
%%           OldData) -> {ok, NewData}
%%       OldData = record(ng_data)
%%       NewData = record(ng_data)
%% @end
%% --------------------------------------------------------------------
register_distribution(Label, Type, Scale, Params, OldData) when is_list(Label),
                                                                is_float(Scale),
                                                                is_list(Params),
                                                                is_record(OldData, ng_data) ->
    Dists = OldData#ng_data.dists,
    Type = Type,
    Params = validators:validate_params(Type, Params),
    case gb_trees:lookup(Label, Dists) of
        none ->
            {ok, OldData#ng_data{dists=gb_trees:insert(Label,
                                                         #dist_call{type=Type,
                                                                    scale=Scale,
                                                                    params=Params},
                                                         Dists)}};
        _ ->
            throw(duplabel)
    end;
register_distribution(Label, Type, Scale, Params, OldData) ->
    error_logger:error_msg("~p:~p: Invalid call to register_distribution, ~p!",
                          [?MODULE, ?LINE, [Label, Type, Scale, Params, OldData]]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Generates a random number based on the provided registered
%% distribution label. Calls into private implementations of the
%% probability distributions to achieve this.
%% @spec call_distribution(Label :: string(), Data) -> {ok, integer()}
%%       Data = record(ng_data)
%% @end
%% --------------------------------------------------------------------
call_distribution(Label, Data) when is_list(Label),
                                     is_record(Data, ng_data)->
    Dists = Data#ng_data.dists,
    case gb_trees:lookup(Label, Dists) of
        {value, #dist_call{type=Type,
                           scale=Scale,
                           params=Params}} ->
            {ok, Result} = generate_number(Type, Scale, Params),
            {ok, erlang:round(Result)};
        none ->
            error_logger:error_msg("~p:~p: ~p not registered!",
                                   [?MODULE, ?LINE, Label]),
            throw(badarg)
    end;
call_distribution(Label, Data) ->
    error_logger:error_msg("~p:~p: Invalid call to call_distribution, ~p!",
                          [?MODULE, ?LINE, [Label, Data]]),
    throw(badarg).


%% ===================================================================
%% Private functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Generates a random number from the given distribution with the given
%% parameters. The results is scaled also, but not rounded to an
%% integer. Implementations are based off of descriptions from
%% http://ftp.arl.mil/random/random.pdf.
%% @spec generate_number(Type :: string(), Scale :: float(), Params :: [float()]) -> {ok, float()}
%% @end
%% --------------------------------------------------------------------
generate_number(Type = gaussian_tail, Scale, Params = [A,Sigma|_]) when is_float(Scale),
                                                                        is_float(A),
                                                                        is_float(Sigma) ->
    U1 = random:uniform(),
    U2 = random:uniform(),
    X1 = abs(Sigma * math:cos(2 * math:pi() * U1) * math:sqrt(-2 * math:log(U2))),
    case X1 > A of
        true ->
            {ok, Scale * X1};
        _ ->
            X2 = Sigma * math:sin(2 * math:pi() * U1) * math:sqrt(-2 * math:log(U2)),
            case X2 > A of
                true ->
                    {ok, Scale * X2};
                _ ->
                    generate_number(Type, Scale, Params)
            end
    end;
generate_number(exponential, Scale, [Lambda|_]) when is_float(Scale),
                                                     is_float(Lambda) ->
    U = random:uniform(),
    {ok, Scale * -Lambda * math:log(U)};
generate_number(flat, Scale, [A,B|_]) when is_float(Scale),
                                           is_float(A),
                                           is_float(B) ->
    U = random:uniform(),
    {ok, Scale * (A + (B - A) * U)};
generate_number(lognormal, Scale, [Mu,Sigma|_]) when is_float(Scale),
                                                     is_float(Mu),
                                                     is_float(Sigma) ->
    U = random:uniform(),
    {ok, Scale * math:exp(Mu + Sigma * U)};
generate_number(poisson, Scale, [Lambda|_]) when is_float(Scale),
                                                 is_float(Lambda) ->
    U = random:uniform(),
    {ok, Scale * poisson_acc(math:exp(-Lambda), U, 1)};
generate_number(bernoulli, Scale, [P|_]) when is_float(Scale),
                                              is_float(P) ->
    U = random:uniform(),
    case P > U of
        true ->
            {ok, Scale};
        _ ->
            {ok, 0}
    end;
generate_number(binomial, Scale, [P,N|_]) when is_float(Scale),
                                               is_float(P),
                                               is_float(N) ->
    {ok, Scale * binomial_acc(P, 0, erlang:round(N))};
generate_number(negative_binomial, Scale, [P,N|_]) when is_float(Scale),
                                                        is_float(P),
                                                        is_float(N) ->
    {ok, Scale * negative_binomial_acc(P, 0, erlang:round(N))};
generate_number(geometric, Scale, [P|_]) when is_float(Scale),
                                              is_float(P) ->
    U = random:uniform(),
    {ok, Scale * ceiling(math:log(U)/math:log(1-P))};
generate_number(Label, Scale, Params) ->
    error_logger:error_msg("~p:~p: Invalid call to generate_number, ~p!",
                          [?MODULE, ?LINE, [Label, Scale, Params]]),
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Implementation of the looping needed to generate a random number from
%% a poisson distribution.
%% @spec poisson_acc(Target :: float(), Products :: float(), N :: positive_integer()) -> positive_integer()
%% @end
%% --------------------------------------------------------------------
poisson_acc(Target, Products, N) when is_float(Target),
                                      is_float(Products),
                                      is_integer(N),
                                      N > 0,
                                      Target > Products ->
    N;
poisson_acc(Target, Products, N) when is_float(Target),
                                      is_float(Products),
                                      is_integer(N),
                                      N > 0->
    U = random:uniform(),
    poisson_acc(Target, U * Products, N + 1).

%%--------------------------------------------------------------------
%% @doc
%% Implementation of the looping needed to generate a random number from
%% a binomial distribution.
%% @spec binomial_acc(P :: float(), Count :: non_neg_integer(), N :: non_neg_pos_integer()) -> non_neg_pos_integer()
%% @end
%% --------------------------------------------------------------------
binomial_acc(_P, Count, 0) when is_integer(Count),
                                Count >= 0 ->
    Count;
binomial_acc(P, Count, N) when is_float(P),
                               is_integer(Count),
                               is_integer(N),
                               Count >= 0,
                               N >= 0 ->
    U = random:uniform(),
    case U < P of
        true ->
            binomial_acc(P, Count + 1, N - 1);
        _ ->
            binomial_acc(P, Count, N - 1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Implementation of the looping needed to generate a random number from
%% a negative binomial distribution.
%% @spec negative_binomial_acc(P :: float(), Count :: non_neg_integer(), N :: non_neg_pos_integer()) -> non_neg_pos_integer()
%% @end
%% --------------------------------------------------------------------
negative_binomial_acc(_P, Count, 0) when is_integer(Count),
                                         Count >= 0 ->
    Count;
negative_binomial_acc(P, Count, N) when is_float(P),
                                        is_integer(Count),
                                        is_integer(N),
                                        Count >= 0,
                                        N >= 0 ->
    U = random:uniform(),
    case U > P of
        true ->
            negative_binomial_acc(P, Count, N - 1);
        _ ->
            negative_binomial_acc(P, Count + 1, N)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Implemenation of ceiling function, because Erlang doesn't provide
%% one.
%% @spec ceiling(X :: float()) -> integer()
%% @end
%% --------------------------------------------------------------------
ceiling(X) when is_float(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 ->
            T;
        Pos when Pos > 0 ->
            T + 1;
        _ ->
            T
    end.
