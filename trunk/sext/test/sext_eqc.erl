-module(sext_eqc).

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

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
	    ?IMPLIES(
	       T1 /= T2,
	       begin
		   {X1,X2} = {sext:encode(T1), sext:encode(T2)},
		   collect(size(term_to_binary({T1,T2})),
			   comp(X1,X2) == comp(T1,T2))
	       end)).

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

prop_encode_neg_fs() ->
    ?FORALL(T, neg_float(),
	    sext:decode(sext:encode(T)) == T).

prop_encode_big() ->
    ?FORALL(T, big(),
	    sext:decode(sext:encode(T)) == T).

prop_encode_neg_big() ->
    ?FORALL(T, neg_big(),
	    sext:decode(sext:encode(T)) == T).



comp(A,B) when A < B -> less;
comp(A,A) -> equal;
comp(_,_) -> more.


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
