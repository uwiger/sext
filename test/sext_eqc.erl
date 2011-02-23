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
    X = 100,
    {timeout, 60,
     [
      fun()   -> run(N, prop_encode,      fun prop_encode/0) end
      , fun() -> run(N, prop_prefix_equiv,fun prop_prefix_equiv/0) end
      , fun() -> run(N, prop_sort,        fun prop_sort/0) end
      , fun() -> run(N, prop_encode_sb32, fun prop_encode_sb32/0) end
      , fun() -> run(N, prop_sort_sb32,   fun prop_sort_sb32/0) end
      , fun() -> run(X, prop_is_prefix1,  fun prop_is_prefix1/0) end
      , fun() -> run(X, prop_is_prefix2,  fun prop_is_prefix2/0) end
     ]}.

run() ->
    run(good_number_of_tests()).

good_number_of_tests() ->
    1000.

run(Num) ->
    X = Num div 2,
    [
     run  (Num, prop_encode     , fun prop_encode/0)
     , run(Num, prop_prefix_equiv,fun prop_prefix_equiv/0)
     , run(Num, prop_sort       , fun prop_encode/0)
     , run(Num, prop_encode_sb32, fun prop_encode_sb32/0)
     , run(Num, prop_sort_sb32  , fun prop_sort_sb32/0)
     , run(X, prop_is_prefix1,  fun prop_is_prefix1/0)
     , run(X, prop_is_prefix2,  fun prop_is_prefix2/0)
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

prop_prefix_equiv() ->
    ?FORALL(T, term(),
	    sext:encode(T) == sext:prefix(T)).

prop_is_prefix1() ->
    ?FORALL({T,W}, {?SUCHTHAT(Tp, prefixable_term(),
			      positions(Tp) > 0),wild()},
	    ?LET(P, choose(1, positions(T)),
		 begin
		     Pfx = sext:prefix(make_wild(T,P,W)),
		     true = is_prefix(Pfx, sext:encode(T))
		 end)).

prop_is_prefix2() ->
    ?FORALL({T,W}, {?SUCHTHAT(Tp, prefixable_term(),
			      positions(Tp) > 2), wild()},
	    ?LET(P, choose(2, positions(T)),
		 begin
		     {Pfx1,Pfx2} = {sext:prefix(make_wild(T,P,W)),
				    sext:prefix(make_wild(T,P-1,W))},
		     true = is_prefix(Pfx2, Pfx1)
		 end)).

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

is_prefix(A, B) ->
    Sz = byte_size(A),
    binary:longest_common_prefix([A,B]) == Sz.
	


term() ->
    ?SIZED(Size,term(Size)).

term(0) ->
    oneof(simple_types());
term(Size) ->
    % You need ?LAZY for recursive generators!
    ?LAZY(oneof(
	    simple_types() ++
		[
		 %% Don't make lists and tuples EXACTLY Size long
		 alist(Size),
		 non_proper_list(Size),
		 atuple(Size),
		 astring(Size)])).

simple_types() ->
    [int(),
     pos_float(),
     neg_float(),
     anatom(),
     abin(),
     abitstr()].


big() ->
    ?LET({X,M}, {nat(), pos()},
         (16#ffffFFFF + X) * M).

neg_big() ->
    ?LET(B, big(), -B).

pos() ->
    ?SUCHTHAT(N,nat(),N>0).

% Set the Size just for list generation.

list_n(Length, G) ->
    [G || _ <- lists:seq(1,Length)].

alist() ->
    ?SIZED(Size, alist(Size)).

alist(Size) ->
    list(Size,term(Size div 3)).

list(Size,G) ->
    ?SIZED(S,resize(Size,list(resize(S,G)))).

atuple(Size) ->
    ?LET(L, alist(Size), list_to_tuple(L)).

anatom() ->
    oneof([a,b,c,aa,bb,cc]).

astring(0) -> "";
astring(Size) ->
    list(Size, choose($A,$z)).

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

non_proper_list() ->
    ?SIZED(Size, non_proper_list(Size)).

non_proper_list(Size) ->
    ?LET(Len, ?SUCHTHAT(Len1, nat(), Len1 > 1),
	 non_proper_n(Len, term(Size div 3))).

non_proper_n(N, G) ->
    make_non_proper([G || _ <- lists:seq(1,N)]).

make_non_proper([A,B]) -> [A|B];
make_non_proper([A])   -> [A];
make_non_proper([A|B]) -> [A|make_non_proper(B)];
make_non_proper([])    -> [].


prefixable_term() ->
    ?SIZED(Size, prefixable_term(Size)).

prefixable_term(Size) ->
    oneof([non_empty_tuple(Size),
	   non_empty_list(Size)]).

non_empty_tuple(Size) ->
    ?LET(L, non_empty_list(Size),
	 list_to_tuple(L)).

non_empty_list() ->
    ?SIZED(Size, non_empty_list(Size)).

non_empty_list(Size) ->
    ?LET(Len, ?SUCHTHAT(I, nat(), I > 0),
	 list_n(Len, term(Size div 3))).

positions(T) ->
    positions(T, 0).

positions(T, Acc) when is_tuple(T) ->
    positions(tuple_to_list(T), Acc);
positions([H|T], Acc) ->
    positions(T, positions(H) + Acc);
positions([], Acc) ->
    Acc;
positions(_, Acc) ->
    Acc+1.

make_wild(T, P, W) when P > 0 ->
    if is_tuple(T) ->
	    {Res,_} = make_wild1(tuple_to_list(T), P, W, []),
	    list_to_tuple(Res);
       is_list(T) ->
	    {Res,_} = make_wild1(T, P, W, []),
	    Res
    end.

make_wild1(L, 0, _, Acc) ->
    {lists:reverse(Acc) ++ L, 0};
make_wild1(T, P, W, Acc) when not(is_list(T)) ->
    if P == 1 ->
	    {lists:reverse(Acc) ++ W, 0};
       true ->
	    {lists:reverse(Acc) ++ T, P-1}
    end;
make_wild1([_|T], 1, W, Acc) ->
    {lists:reverse(Acc) ++ [W|T], 0};
make_wild1([H|T], P, W, Acc) ->
    if is_tuple(H) ->
	    {H1,P1} = make_wild1(tuple_to_list(H), P, W, []),
	    make_wild1(T, P1, W, [list_to_tuple(H1)|Acc]);
       is_list(H) ->
	    {H1,P1} = make_wild1(H, P, W, []),
	    make_wild1(T, P1, W, [H1|Acc]);
       true ->
	    make_wild1(T, P-1, W, [H|Acc])
    end;
make_wild1([], P, _W, Acc) ->
    {lists:reverse(Acc), P}.
	    
	    
    


wild() ->
    oneof(['_','$1','$9999']).

lists_replace(L, P, V) when P > 0, P =< length(L) ->
    {L1, [_|L2]} = lists:split(P-1, L),
    L1 ++ [V] ++ L2.

-endif.
