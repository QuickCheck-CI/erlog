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
		   
		   PL		   = add_export_clauses(Clauses, erlog:new()),
		   {ok,ModName}    = erlog_make_server:create_core_erlang(ModName,PL),
		   Exports	   = ModName:module_info(exports),
		   length(Exports) =:= length(Clauses) + 3 andalso
		       lists:all(fun(_Export = {erl_export, {'/',Fun, Arity}}) ->
					 lists:member({Fun, Arity}, Exports)
				 end, Clauses)
	       end)).

set_node() ->
    elements([a,b,c,d,e,f]).
edge() ->
    {edge, set_node(), set_node()}.
edges() ->
    non_empty(list(edge())).

prop_supervisor_spec() ->
    erlog_make_server_tests_sup:start_link(),
    ModName		= edges_pl,
    {ok,ModName}	= erlog_make_server:compile_file("priv/edges_pl.pl",ModName),
    R			= ModName:make_child_spec(make_ref()),
    ok			= supervisor:check_childspecs([R]),
    {ok,Pid}		= supervisor:start_child(erlog_make_server_tests_sup, R),
    is_process_alive(Pid).
    
    


prop_run_pl() ->
   
    ?FORALL({ModName, Edges},
	    {'edges_pl',edges() },
	    begin
		code:purge(ModName),
		code:delete(ModName),
		{ok,ModName}	= erlog_make_server:compile_file("priv/edges_pl.pl",ModName),
		Exports		= ModName:module_info(exports),

		?assert(lists:member({add_edge, 3}, Exports)),

		true            = lists:member({make_child_spec, 1}, Exports),
		{ok,Pid}        = erlog_custom_server:start_link("priv/edges_pl.pl"),
		true            = is_process_alive(Pid),
		lists:foreach(fun({edge,S,F}) ->
				      ok = ModName:add_edge(Pid,S,F),
				      true
			  end, Edges),
		unlink(Pid),
		is_process_alive(Pid)
	    end).
