%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(sext_eqc).

-ifdef(EQC).

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").


sext_test_() ->
    N = 1000,
    {timeout, 60,
     [
      fun()   -> run(N, prop_encode,      fun prop_encode/0) end
      , fun() -> run(N, prop_sort,        fun prop_sort/0) end
      , fun() -> run(N, prop_encode_sb32, fun prop_encode_sb32/0) end
      , fun() -> run(N, prop_sort_sb32,   fun prop_sort_sb32/0) end
     ]}.

run() ->
    run(good_number_of_tests()).

good_number_of_tests() ->
    1000.

run(Num) ->
    [
     run  (Num, prop_encode     , fun prop_encode/0)
     , run(Num, prop_sort       , fun prop_encode/0)
     , run(Num, prop_encode_sb32, fun prop_encode_sb32/0)
     , run(Num, prop_sort_sb32  , fun prop_sort_sb32/0)
    ].

run(Num, Lbl, F) ->
    io:fwrite(user, "EQC test: ~p (~p)... ", [Lbl, Num]),
    Res = eqc:quickcheck(eqc:numtests(Num, F())),
    io:fwrite(user, "-> ~p~n", [Res]).



prop_negbits() ->
    ?FORALL(B, abin(),
	    begin
		{B1,<<>>} = sext:decode_neg_binary(sext:encode_neg_bits(B)),
                B == B1
            end).
	    
%% In this property, the ?IMPLIES condition guards us against the 
%% unfortunate case where {1, 1.0} will have a strict ordering when 
%% encoded (in order to satisfy the encode property), but not in Erlang
%% since they compare as equal. It seems a reasonable limitation, that 
%% we limit ourselves to testing the sort order of term pairs where the
%% values actually differ.
prop_sort() ->
    ?FORALL({T1,T2}, {term(), term()},
	       begin
		   {X1,X2} = {sext:encode(T1), sext:encode(T2)},
		   collect(size(term_to_binary({T1,T2})),
			   comp(X1,X2) == comp_i(T1,T2))
	       end).

prop_sort_sb32() ->
    ?FORALL({T1,T2}, {term(), term()},
	       begin
		   {X1,X2} = {sext:encode_sb32(T1), sext:encode_sb32(T2)},
		   collect(size(term_to_binary({T1,T2})),
			   comp(X1,X2) == comp_i(T1,T2))
	       end).


prop_sort_fs() ->
    ?FORALL({R1,R2}, {pos_float(),pos_float()},
	    begin
		{B1,B2} = {sext:encode(R1), sext:encode(R2)},
		comp(R1,R2) == comp(B1,B2)
	    end).

prop_sort_neg_fs() ->
    ?FORALL({R1,R2}, {neg_float(), neg_float()},
	    begin
		{B1,B2} = {sext:encode(R1), sext:encode(R2)},
		comp(R1,R2) == comp(B1,B2)
	    end).



prop_encode() ->
    ?FORALL(T, term(),
	    sext:decode(sext:encode(T)) == T).

prop_encode_sb32() ->
    ?FORALL(T, term(),
	    sext:decode_sb32(sext:encode_sb32(T)) == T).

prop_encode_neg_fs() ->
    ?FORALL(T, neg_float(),
	    sext:decode(sext:encode(T)) == T).

prop_encode_big() ->
    ?FORALL(T, big(),
	    sext:decode(sext:encode(T)) == T).

prop_encode_neg_big() ->
    ?FORALL(T, neg_big(),
	    sext:decode(sext:encode(T)) == T).


comp(A,B) when A == B, A =/= B ->
    %% can only happen when either is a float and the other an int
    IsMore = if A < 0 ->
		     is_float(B);
		true ->
		     is_float(A)
	     end,
    case IsMore of
	true  -> more;
	false -> less
    end;
comp(A,B) when A < B -> less;
comp(A,A) -> equal;
comp(_,_) -> more.

comp_i(Ta, Tb) when is_tuple(Ta), is_tuple(Tb), tuple_size(Ta) == tuple_size(Tb) ->
    comp_l(tuple_to_list(Ta), tuple_to_list(Tb));
comp_i(La, Lb) when is_list(La), is_list(Lb) ->
    comp_l(La, Lb);
comp_i(A, B) ->
    comp(A, B).

comp_l([]     , []    ) -> equal;
comp_l([]     , [_|_] ) -> less;
comp_l([_|_]  , []    ) -> more;
comp_l([Ha|Ta],[Hb|Tb]) -> 
    case comp(Ha, Hb) of
	equal ->
	    comp_l(Ta, Tb);
	Other ->
	    Other
    end.

    



term() ->
    ?SIZED(Size,term(Size)).

term(Size) ->
    % You need ?LAZY for recursive generators!
    ?LAZY(oneof(
	    [int(),
	     pos_float(),
	     neg_float(),
	     anatom(),
	     % Don't make lists and tuples EXACTLY Size long
	     list(Size,term(Size div 3)),
	     ?LET(L,list(Size,term(Size div 3)),list_to_tuple(L)),
	     astring(Size),
	     abin(),
	     abitstr()])).

big() ->
    ?LET({X,M}, {nat(), pos()},
         (16#ffffFFFF + X) * M).

neg_big() ->
    ?LET(B, big(), -B).

pos() ->
    ?SUCHTHAT(N,nat(),N>0).

% Set the Size just for list generation.
list(Size,G) ->
    ?SIZED(S,resize(Size,list(resize(S,G)))).

anatom() ->
    oneof([a,b,c,aa,bb,cc]).

astring(0) -> "";
astring(Size) ->
    list(choose($A,$z)).

abin() ->
    ?LET(L, list(choose(0,255)), list_to_binary(L)).

abitstr() ->
    ?LET({Bin, Sz}, {abin(), choose(0, 7)},
	 ?LET(N, choose(0, 16#ff bsr (8-Sz)),
	      <<Bin/binary, N:Sz>>)).

pos_float() ->
    ?LET(F, ?SUCHTHAT(R, real(), R > 0 andalso is_float(R)),
	 norm(F)).

neg_float() ->
    ?LET(F, ?SUCHTHAT(R, real(), R < 0 andalso is_float(R)),
	 norm(F)).

norm(F) when is_float(F) ->
    <<G/float>> = <<F/float>>,
    G.

-endif.
