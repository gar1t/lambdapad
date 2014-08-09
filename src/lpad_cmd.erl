-module(lpad_cmd).

-export([run/2, run/3, run/4]).

run(Exe, Args) ->
    run(Exe, Args, [], infinity).

run(Exe, Args, Options) ->
    run(Exe, Args, clean_options(Options), infinity).

-define(valid_timeout(T), T == infinity; is_integer(T)).

run(Exe, Args, Options, Timeout) when ?valid_timeout(Timeout) ->
    Self = self(),
    FullExe = find_exe(Exe),
    CleanOptions = clean_options(Options),
    spawn(fun() -> start_port(FullExe, Args, CleanOptions, Timeout, Self) end),
    receive
        {N, Out} when is_integer(N) ->
            {N, Out};
        {error, Err} ->
            error({start_port, {Exe, Args, Options, Timeout}, Err})
    end.

clean_options(Options) ->
    clean_env_option(Options).

clean_env_option(Options) ->
    handle_env_option(proplists:get_value(env, Options), Options).

handle_env_option(undefined, Options) -> Options;
handle_env_option(Env, Options) ->
    [{env, env_to_strings(Env)}|proplists:delete(env, Options)].

env_to_strings(Env) ->
    [{to_string(Name), to_string(Val)} || {Name, Val} <- Env].

to_string(L) when is_list(L) -> L;
to_string(B) when is_binary(B) -> binary_to_list(B).

find_exe(Exe) ->
    handle_exe_is_file(filelib:is_file(Exe), Exe).

handle_exe_is_file(true, Exe) -> Exe;
handle_exe_is_file(false, Exe) ->
    handle_os_find_exe(os:find_executable(Exe), Exe).

handle_os_find_exe(false, Exe) -> error({bad_exe, Exe});
handle_os_find_exe(FullExe, _Exe) -> FullExe.

start_port(Exe, Args, Options, Timeout, From) ->
    try open_port({spawn_executable, Exe}, spawn_opts(Args, Options)) of
        Port ->
            port_loop(Port, From, Timeout, [])
    catch
        error:Err ->
            send_error(From, Err)
    end.

spawn_opts(Args, Options) ->
    [{args, Args}, stderr_to_stdout, exit_status | Options].

port_loop(Port, From, Timeout, AccOut) ->
    TimeoutRef = schedule_next_timeout(Timeout),
    receive
	{Port, {data, Data}} ->
            cancel_timeout(TimeoutRef),
	    port_loop(Port, From, Timeout, [Data|AccOut]);
	{Port, {exit_status, Exit}} ->
            cancel_timeout(TimeoutRef),
            send_result(From, Exit, lists:reverse(AccOut));
        timeout ->
            send_error(From, {timeout, lists:reverse(AccOut)})
    end.

schedule_next_timeout(infinity) -> undefined;
schedule_next_timeout(Timeout) ->
    erlang:send_after(Timeout, self(), timeout).

cancel_timeout(undefined) -> ok;
cancel_timeout(TimeoutRef) ->
    erlang:cancel_timer(TimeoutRef).

send_result(Dest, Exit, Out) ->
    erlang:send(Dest, {Exit, Out}).

send_error(Dest, Err) ->
    erlang:send(Dest, {error, Err}).
