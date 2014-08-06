-module(lpad_eterm).

-export([load/1]).

load(File) ->
    handle_consult_eterm(file:consult(File), File).

handle_consult_eterm({ok, [Term]}, _Source) ->
    Term;
handle_consult_eterm({ok, _}, Source) ->
    error({eterm_source, Source, invalid_term});
handle_consult_eterm({error, Err}, Source) ->
    error({eterm_source, Source, Err}).
