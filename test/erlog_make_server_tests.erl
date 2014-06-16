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


prop_compile_buffer() ->
    ?FORALL({ModName, Clauses},
	    {'edges_pl',
	     non_empty(list(erl_export()))},
	    ?IMPLIES(length(Clauses) =:= length(lists:usort(Clauses)),
	       begin
		   %% Purge any results of old runs
		   code:purge(ModName),
		   code:delete(ModName),
		   
		   PL		= add_export_clauses(Clauses, erlog:new()),
		   {ok,ModName}	= erlog_make_server:create_core_erlang(ModName,PL),
		   Exports		= ModName:module_info(exports),
		   length(Exports) =:= length(Clauses) + 3 andalso
		       lists:all(fun(_Export = {erl_export, {'/',Fun, Arity}}) ->
					 lists:member({Fun, Arity}, Exports)
				 end, Clauses)
	       end)).


prop_supervisor_spec() ->
    ?FORALL({ModName},
	    {'edges_pl'},
	    begin
		%% Purge any results of old runs
		code:purge(ModName),
		code:delete(ModName),
		PL              = erlog:new(),	
		{ok,ModName}	= erlog_make_server:create_core_erlang(ModName,PL),
		Exports		= ModName:module_info(exports),
		lists:member({make_child_spec, 1}, Exports),
		R = ModName:make_child_spec(make_ref()),
		ok =:= supervisor:check_childspecs([R])
	    end).
