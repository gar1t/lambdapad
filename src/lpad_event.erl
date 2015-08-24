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

-module(lpad_event).

-export([notify/1]).

notify({file_create, File}) ->
    lpad_log:log_info("Creating ~s~n", [File]);
notify({file_copy, _Src, Dest}) ->
    lpad_log:log_info("Creating ~s~n", [Dest]);
notify({data_loaded, Data}) ->
    maybe_log_info(
      env_defined(["LPAD_LOG_ALL", "LPAD_LOG_DATA"]),
      lpad_log:format_banner("Data", "~p"), [Data]);
notify({generators_loaded, Generators}) ->
    maybe_log_info(
      env_defined(["LPAD_LOG_ALL", "LPAD_LOG_GENERATORS"]),
      lpad_log:format_banner("Generators", "~p"), [Generators]);
notify({exit, Err}) ->
    lpad_errors:handle_error(Err);
notify(Other) ->
    lpad_log:log_info(lpad_log:format_banner("UNKNOWN", "~p"), [Other]).

env_defined(Names) ->
    lists:any(fun(Name) -> os:getenv(Name) /= false end, Names).

maybe_log_info(true, Msg, Data) ->
    lpad_log:log_info(Msg, Data);
maybe_log_info(false, _Msg, _Data) ->
    ok.
