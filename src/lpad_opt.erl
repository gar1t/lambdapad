%%% Copyright 2014 Garrett Smith <g@rre.tt>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(lpad_opt).

-export([validate/2, validate/3, value/2, value/3]).

-define(NO_DEFAULT, '$lpad_opt_nodefault').

-record(schema, {implicit, constraints}).
-record(constraint,
        {values,
         type,
         min,
         max,
         pattern,
         validate,
         implicit=false,
         optional=false,
         default=?NO_DEFAULT}).

-define(is_type(T), (T == int orelse
                     T == float orelse
                     T == string orelse
                     T == number orelse
                     T == atom orelse
                     T == list orelse
                     T == boolean orelse
                     T == binary orelse
                     T == iolist orelse
                     T == function)).

%%%===================================================================
%%% API
%%%===================================================================

validate(Options, Schema) ->
    validate(Options, compile_schema(Schema), dict:new()).

validate([], #schema{}=Schema, Opts0) ->
    apply_missing(Schema, Opts0);
validate([Opt|Rest], #schema{}=Schema, Opts0) ->
    validate(Rest, Schema, apply_opt(Opt, Schema, Opts0));
validate(MoreOptions, Schema, Opts0) ->
    validate(MoreOptions, compile_schema(Schema), Opts0).

value(Name, Opts) ->
    dict:fetch(Name, Opts).

value(Name, Opts, Default) ->
    case dict:find(Name, Opts) of
        {ok, Value} -> Value;
        error -> Default
    end.

compile_schema(Schema) ->
    Constraints = [compile_constraint(C) || C <- Schema],
    #schema{implicit=index_implicit(Constraints), constraints=Constraints}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_constraint(Name) when is_atom(Name) ->
    {Name, #constraint{}};
compile_constraint({Name, Opts}) ->
    {Name, apply_constraint_options(Opts, #constraint{})}.

index_implicit(Constraints) ->
    index_implicit(Constraints, dict:new()).

index_implicit([], Imp) -> Imp;
index_implicit([{_, #constraint{implicit=false}}|Rest], Imp) ->
    index_implicit(Rest, Imp);
index_implicit([{Name, #constraint{implicit=true, values=undefined}}|_], _) ->
    error({values_required, Name});
index_implicit([{Name, #constraint{implicit=true, values=Vals}}|Rest], Imp) ->
    index_implicit(Rest, index_implicit_vals(Name, Vals, Imp)).

index_implicit_vals(_, [], Imp) -> Imp;
index_implicit_vals(Name, [Val|Rest], Imp) ->
    case dict:find(Val, Imp) of
        {ok, _} -> error({duplicate_implicit_value, Val});
        error -> index_implicit_vals(Name, Rest, dict:store(Val, Name, Imp))
    end.

-define(constraint_val(Field, Val, C), C#constraint{Field=Val}).

apply_constraint_options([], C) -> C;
apply_constraint_options([{values, Values}|Rest], C) when is_list(Values) ->
    apply_constraint_options(Rest, ?constraint_val(values, Values, C));
apply_constraint_options([{type, Type}|Rest], C) when ?is_type(Type) ->
    apply_constraint_options(Rest, ?constraint_val(type, Type, C));
apply_constraint_options([Type|Rest], C) when ?is_type(Type) ->
    apply_constraint_options(Rest, ?constraint_val(type, Type, C));
apply_constraint_options([{min, Min}|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(min, Min, C));
apply_constraint_options([{max, Max}|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(max, Max, C));
apply_constraint_options([{pattern, Pattern}|Rest], C) ->
    apply_constraint_options(
      Rest, ?constraint_val(pattern, compile_pattern(Pattern), C));
apply_constraint_options([{validate, F}|Rest], C) when is_function(F) ->
    apply_constraint_options(
      Rest, ?constraint_val(validate, check_validate(F), C));
apply_constraint_options([optional|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(optional, true, C));
apply_constraint_options([{optional, B}|Rest], C) when is_boolean(B) ->
    apply_constraint_options(Rest, ?constraint_val(optional, B, C));
apply_constraint_options([{default, Default}|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(default, Default, C));
apply_constraint_options([implicit|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(implicit, true, C));
apply_constraint_options([{Name, _}|_], _) ->
    error({badarg, Name});
apply_constraint_options([Other|_], _) ->
    error({badarg, Other}).

compile_pattern(Pattern) ->
    case re:compile(Pattern) of
        {ok, Re} -> Re;
        {error, _} -> error({badarg, pattern})
    end.

check_validate(F) ->
    case erlang:fun_info(F, arity) of
        {arity, 1} -> F;
        {arity, _} -> error({badarg, validate})
    end.

apply_opt(Opt, Schema, Opts) ->
    {Name, Value} = validate_opt(Opt, Schema),
    case dict:find(Name, Opts) of
        {ok, _} -> error({duplicate, Name});
        error -> dict:store(Name, Value, Opts)
    end.

validate_opt({Name, Value}, Schema) ->
    case find_constraint(Name, Schema) of
        {ok, Constraint} ->
            case check_value(Value, Constraint) of
                ok -> {Name, Value};
                error -> error({badarg, Name})
            end;
        error -> error({badarg, Name})
    end;
validate_opt(Option, Schema) ->
    case implicit_option(Option, Schema) of
        {ok, Name} -> {Name, Option};
        error ->
            validate_opt({Option, true}, Schema)
    end.

find_constraint(Name, #schema{constraints=Constraints}) ->
    case lists:keyfind(Name, 1, Constraints) of
        {Name, Constraint} -> {ok, Constraint};
        false -> error
    end.

implicit_option(Value, #schema{implicit=Implicit}) ->
    case dict:find(Value, Implicit) of
        {ok, Name} -> {ok, Name};
        error -> error
    end.

check_value(Val, Constraint) ->
    apply_checks(Val, Constraint,
                 [fun check_enum/2,
                  fun check_type/2,
                  fun check_range/2,
                  fun check_pattern/2,
                  fun apply_validate/2]).

apply_checks(_Val, _Constraint, []) -> ok;
apply_checks(Val, Constraint, [Check|Rest]) ->
    case Check(Val, Constraint) of
        ok -> apply_checks(Val, Constraint, Rest);
        error -> error
    end.

check_enum(_Val, #constraint{values=undefined}) -> ok;
check_enum(Val, #constraint{values=Values}) ->
    case lists:member(Val, Values) of
        true -> ok;
        false -> error
    end.

-define(is_iolist(T),
        try erlang:iolist_size(Val) of
            _ -> true
        catch
            error:badarg -> false
        end).

check_type(_Val, #constraint{type=undefined}) -> ok;
check_type(Val, #constraint{type=int}) when is_integer(Val) -> ok;
check_type(Val, #constraint{type=float}) when is_float(Val) -> ok;
check_type(Val, #constraint{type=number}) when is_number(Val) -> ok;
check_type(Val, #constraint{type=string}) ->
    case ?is_iolist(Val) of
        true -> ok;
        false -> error
    end;
check_type(Val, #constraint{type=boolean}) when is_boolean(Val) -> ok;
check_type(Val, #constraint{type=list}) when is_list(Val) -> ok;
check_type(Val, #constraint{type=atom}) when is_atom(Val) -> ok;
check_type(Val, #constraint{type=binary}) when is_binary(Val) -> ok;
check_type(Val, #constraint{type=function}) when is_function(Val) -> ok;
check_type(Val, #constraint{type=iolist}) ->
    try iolist_size(Val) of
        _ -> ok
    catch
        error:badarg -> error
    end;
check_type(_, _) -> error.

check_range(_Val, #constraint{min=undefined, max=undefined}) -> ok;
check_range(Val, #constraint{min=undefined, max=Max}) when Val =< Max -> ok;
check_range(Val, #constraint{min=Min, max=undefined}) when Val >= Min -> ok;
check_range(Val, #constraint{min=Min, max=Max}) when Val =< Max,
                                                     Val >= Min-> ok;
check_range(_, _) -> error.

check_pattern(_Val, #constraint{pattern=undefined}) -> ok;
check_pattern(Val, #constraint{pattern=Regex}) ->
    case re:run(Val, Regex, [{capture, none}]) of
        match -> ok;
        nomatch -> error
    end.

apply_validate(_Val, #constraint{validate=undefined}) -> ok;
apply_validate(Val, #constraint{validate=Validate}) ->
    case Validate(Val) of
        ok -> ok;
        error -> error;
        Other -> error({validate_result, Other})
    end.

apply_missing(#schema{constraints=Constraints}, Opts0) ->
    lists:foldl(fun apply_default/2, Opts0, Constraints).

apply_default({Name, #constraint{default=Default, optional=Optional}}, Opts) ->
    case dict:find(Name, Opts) of
        {ok, _} -> Opts;
        error ->
            case Default of
                ?NO_DEFAULT ->
                    case Optional of
                        true -> Opts;
                        false -> error({required, Name})
                    end;
                _ -> dict:store(Name, Default, Opts)
            end
    end.
