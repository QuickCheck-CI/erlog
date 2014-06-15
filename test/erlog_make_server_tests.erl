-module(erlog_make_server_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

pl_arity() ->
    choose(2,10).

clause_name() ->
    elements(['edge','connected', ancestor, descendent,path, travel]).

erl_export() ->
    {erl_export, {'/', clause_name(), pl_arity()}}.

prop_find_exported_clauses() ->
    ?FORALL(Clauses, non_empty(list(erl_export())),
	    begin
		PL0 = erlog:new(),
                PL2 = add_export_clauses(Clauses, PL0),
		{Exports,_PL3} = erlog_make_server:find_exports(PL2),
		length(Exports) =:= length(Clauses) andalso
		    lists:all(fun(_Export = {Fun, Arity}) ->
				      lists:member({erl_export,{'/',Fun, Arity}}, Clauses)
			      end, Exports)
	    end).

add_export_clauses(Clauses, PL0) ->
    lists:foldl(fun(CL,PL) ->
			{{succeed, _},PL1} = PL({prove,{asserta, CL}}),
			PL1
                end, PL0, Clauses).

clauses() ->
    ok.

prop_compile_buffer() ->
    ?FORALL({ModName,Clauses},
	    {'edges',non_empty(list(erl_export()))},
	    begin
		PL		= add_export_clauses(Clauses, erlog:new()),
		{ok,Core,PL}	= erlog_make_server:create_core_erlang(ModName,PL),
		Exports		= ModName:module_info(exports),
		length(Exports) =:= length(Clauses) + 2 andalso
		    lists:all(fun(_Export = {erl_export, {'/',Fun, Arity}}) ->
				      lists:member({Fun, Arity}, Exports)
			      end, Clauses)
	    end).

out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
	     fun prop_compile_buffer/0,
	     fun prop_find_exported_clauses/0

             ],
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(50,P)))
     end
     || Prop <- Props].

