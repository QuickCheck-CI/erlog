-module(erlog_int_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
fail_test() ->
    {ok, PID}   = erlog:start_link(),
    ?assertEqual(fail,erlog:prove(PID, fail)),
    ok.

not_equal_test() ->
    {ok, PID}   = erlog:start_link(),
    ?assertEqual(fail,erlog:prove(PID, {'\=', 1,2})),
    ok.


keys() ->
    [
     "AAAAAAAA",
     "8BFE5E9B",
     "59665E9E",
     "D54BA0D0",
     "3A1D3C2A",
     "DB203B97",
     "EB77972F",
     "7445F8E0",
     "73547A12",
     "3820D3E8",
     "6EABF346",
     "EB75CC5E",
     "BA7F285E",
     "9882CB8F",
     "EA05A25E",
     "C125074F",
     "EC10B758",
     "54BB4C80",
     "537E16D9"].

bool_test() ->
    {ok,E} = erlog:start_link(),
    ?assertEqual({succeed, []}, erlog:prove(E, true)),
    ?assertEqual(fail, erlog:prove(E, false)),
    ok.

option() ->
    oneof([assert, asserta, assertz]).
value() ->
    {model, oneof(keys()), int()}.

prop_assert() ->
    ?FORALL({Op, Value},
            {option(), value()},
            begin
                {ok, PID}   = erlog:start_link(),
                {succeed,_} = erlog:prove(PID, {Op, Value}),
                case  erlog:prove(PID, Value) of
                    {succeed,_} -> true;
                    _           -> false
                end
            end).

prop_retract() ->
    ?FORALL({Op, Value},
            {oneof([retract]), value()},
            begin
                {ok, PID}   = erlog:start_link(),
                {succeed,_} = erlog:prove(PID, {asserta, Value}),
                {succeed,_} = erlog:prove(PID, Value),
                {succeed,_} = erlog:prove(PID, {Op, Value}),
                case  erlog:prove(PID, Value) of
                    {succeed,_} -> false;
                    _           -> true
                end
            end).
              

out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
             fun prop_assert/0,
             fun prop_retract/0
             ],    
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(500,P)))
     end
     || Prop <- Props].



