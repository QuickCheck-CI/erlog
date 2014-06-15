-module(erlog_make_server).
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : erlog_make_server.erl
%% Author  : Zachary Kessin
%% Purpose : Convert an erlog .pl file to an erlang gen_server

%% To export clauses use erl_export(clause/N) 
%% The prolog clause <<X>>/N will become the erlang function <<X>>/N, with the first 
%% Element of the erlang function being the pid of the gen server, and the last element 
%% of the prolog clause being the return value
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-ifdef(TEST).
-compile(export_all).
-endif.


-spec(compile_buffer(atom(), iolist()) ->
	     {ok, atom()}).

join([],_) ->
    [];
join([Last],_) ->
    [Last];
join([First|Rest],JoinWith) ->
    [First, JoinWith, join(Rest, JoinWith)].

compile_buffer(Module, _Buffer) ->
    
    {ok, Module}.

core_module_info() ->
    ["'module_info'/0 =\n",
     "    fun () ->\n",
     "	call 'erlang':'get_module_info'\n",
     "	    ('custom_server')\n",
     "'module_info'/1 =\n",
     "    fun (_cor0) ->\n",
     "	call 'erlang':'get_module_info'\n",
     "	    ('custom_server', _cor0)\n",
     "end\n"].

make_prolog_fun({FunName, Arity}) when is_atom(FunName) andalso is_integer(Arity) ->
    CVars     = [io_lib:format("_cor~p",[N]) || N <- lists:seq(0, Arity-1)],
    CVarsList = join(CVars, ","),
    PLLists   = join([io_lib:format("_cor~p",[N]) || N <- lists:seq(1, Arity-1)],","),
    [["'",atom_to_list(FunName), "'/",io_lib:format("~p",[Arity]), " =\n"],
     ["    fun (",CVarsList,") ->\n",
      "       case call 'gen_server':'call'\n",
      "                (_cor1, {'prove',{'",atom_to_list(FunName),"',",PLLists,",{'Y'}}}) of\n",

      "          <{'succeed',[{'Y',Y}|[]]}> when 'true' ->\n",
      
      "              {'ok',Y}\n",
      "          <'fail'> when 'true' ->\n",
      "              'fail'\n",
      "          ( ",io_lib:format("<_cor~p>",[Arity]), " when 'true' ->\n",
      "                primop 'match_fail'\n",
      "                    ({'case_clause',_cor2})\n",
      "            -| ['compiler_generated'] )\n",
      "        end\n"]].

create_core_erlang(Module, PL) when is_atom(Module) ->
    {PLExports,_}	= find_exports(PL),
    Exports		= [{'module_info',0}, {'module_info',1}|PLExports],
    ExportComp		= [io_lib:format("'~p'/~p ~n",[  FunName, Arity]) ||{FunName, Arity} <- Exports],
    Core		= ["module '", atom_to_list(Module), "' [", join(ExportComp,"                    ,"),
			   "]\n","     attributes []\n"],
    PLFuns		= [make_prolog_fun({FunName,Arity}) ||{FunName, Arity} <-PLExports],
    CoreDoc		= iolist_to_binary([Core, PLFuns, core_module_info()]),
    ?debugFmt("~n~s~n", [CoreDoc]),
    {ok, Module, Code}  = compile:forms(CoreDoc, [from_core,debug_info,return_errors]), 
    {ok, Code, CoreDoc}.
    
    
		  
		     
	
     

find_exports(PL) ->
    case PL({prove, {findall, {'X'}, {'erl_export',{'X'} }, {'Xs'}}}) of
	{{succeed, Res},PL1} ->
	    Exports = [{Fun, Arity} || {'/', Fun,Arity} <-proplists:get_value('Xs', Res)],
	    {Exports,PL1};
	fail ->
	   []
    end.