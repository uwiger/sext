%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%% @author Ulf Wiger <ulf.wiger@erlang-consulting.com>
%% @doc Sortable serialization library
%% @end
-module(sext).

-export([encode/1, decode/1]).

-define(negbig   , 8).
-define(neg4     , 9).
-define(pos4     , 10).
-define(posbig   , 11).
-define(atom     , 12).
-define(reference, 13).
-define(port     , 14).
-define(pid      , 15).
-define(tuple    , 16).
-define(list     , 17).
-define(binary   , 18).

-define(IMAX1, 16#ffffFFFFffffFFFF).

-define(dbg(Fmt,Args),
        case get(dbg) of
            true -> io:fwrite("~p: " ++ Fmt, [?LINE|Args]);
            _ -> no_dbg
        end).
%%-define(dbg(F,A),no_debug).

-include_lib("eunit/include/eunit.hrl").


%% @spec encode(T::term()) -> binary()
%% @doc Encodes any Erlang term into a binary.
%% The lexical sorting properties of the encoded binary match those of the
%% original Erlang term. That is, encoded terms sort the same way as the 
%% original terms would.
%% @end
%%
encode(X) when is_tuple(X)     -> encode_tuple(X);
encode(X) when is_list(X)      -> encode_list(X);
encode(X) when is_pid(X)       -> encode_pid(X);
encode(X) when is_port(X)      -> encode_port(X);
encode(X) when is_reference(X) -> encode_ref(X);
encode(X) when is_number(X)    -> encode_number(X);
encode(X) when is_binary(X)    -> encode_binary(X);
encode(X) when is_bitstring(X) -> encode_bitstring(X);
encode(X) when is_atom(X)      -> encode_atom(X).


%% @spec decode(B::binary()) -> term()
%% @doc Decodes a binary generated using the function sext:encode/1.
%% @end
%%
decode(Elems) ->
    case decode_next(Elems) of
	{Term, <<>>} -> Term;
	{Term, []} -> Term;
	Other -> erlang:error(badarg, Other)
    end.




pp(none) -> "<none>";
pp(B) when is_bitstring(B) ->
    [ $0 + I || <<I:1>> <= B ].



encode_tuple(T) ->
    Sz = size(T),
    encode_tuple_elems(1, Sz, T, <<?tuple, Sz:32>>).

%% It's easier to iterate over a tuple by converting it to a list, but
%% since the tuple /can/ be huge, let's do it this way.
encode_tuple_elems(P, Sz, T, Acc) when P =< Sz ->
    E = encode(element(P,T)),
    encode_tuple_elems(P+1, Sz, T, <<Acc/binary, E/binary>>);
encode_tuple_elems(_, _, _, Acc) ->
    Acc.

encode_list(L) ->
    encode_list_elems(L, <<?list>>).


encode_binary(B)    ->
    Enc = encode_bin_elems(B),
    <<?binary:8, Enc/binary>>.

encode_bitstring(B) ->
    Enc = encode_bits_elems(B),
    <<?binary:8, Enc/binary>>.

encode_pid(P) ->
    PBin = term_to_binary(P),
    <<131,103,100,ALen:16,Name:ALen/binary,Rest:9/binary>> = PBin,
    NameEnc = encode_bin_elems(Name),
    <<?pid, NameEnc/binary, Rest/binary>>.
      
encode_port(P) ->
    PBin = term_to_binary(P),
    <<131,102,100,ALen:16,Name:ALen/binary,Rest:5/binary>> = PBin,
    NameEnc = encode_bin_elems(Name),
    <<?port, NameEnc/binary, Rest/binary>>.

encode_ref(R) ->
    RBin = term_to_binary(R),
    <<131,114,_Len:16,100,NLen:16,Name:NLen/binary,Rest/binary>> = RBin,
    NameEnc = encode_bin_elems(Name),
    RestEnc = encode_bin_elems(Rest),
    <<?reference, NameEnc/binary, RestEnc/binary>>.
      

encode_atom(A) ->
    Bin = list_to_binary(atom_to_list(A)),
    Enc = encode_bin_elems(Bin),
    <<?atom, Enc/binary>>.


encode_number(N) when is_integer(N) ->
    encode_int(N, none);
encode_number(F) when is_float(F) ->
    encode_float(F).

%% 
%% IEEE 764 Binary 64 standard representation
%% http://en.wikipedia.org/wiki/Double_precision_floating-point_format
%%
%% |12345678 12345678 12345678 12345678 12345678 12345678 12345678 12345678
%% |iEEEEEEE EEEEffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff|
%%
%% i: sign bit
%% E: Exponent, 11 bits
%% f: fraction, 52 bits
%%
%% We perform the following operations:
%% - if E < 1023 (see Exponent bias), the integer part is 0
%% 
encode_float(F) ->
    <<Sign:1, Exp0:11, Frac:52>> = <<F/float>>,
    ?dbg("F = ~p | Exp0 = ~p | Frac = ~p~n", [cF, Exp0, Frac]),
    {Int0, Fraction} =
	case Exp0 - 1023 of
	    NegExp when NegExp < 0 ->
		Offs = -NegExp,
		?dbg("NegExp = ~p, Offs = ~p~n"
		     "Frac = ~p~n", [NegExp, Offs, Frac]),
		{0, << 0:Offs, 1:1,Frac:52 >>};
	    Exp1 ->
		?dbg("Exp1 = ~p~n", [Exp1]),
		if Exp1 >= 52 ->
			%% Decimal part will be zero
			{trunc(F), <<0:52>>};
		   true ->
			R = 52-Exp1,
			?dbg("R = ~p~n", [R]),
			Exp2 = Exp1 + 1,	% add the leading 1-bit
			?dbg("Exp2 = ~p~n", [Exp2]),
			<<I:Exp2, Frac1:R>> = <<1:1, Frac:52>>,
			?dbg("I = ~p, Frac1 = ~p~n", [I,Frac1]),
			{I, <<Frac1:R>>}
		end
	end,
    if Sign == 1 ->
	    %% explicitly encode a negative int, since Int0 can be zero.
	    Int = if Int0 >= 0 -> -Int0;
		     true -> Int0
		  end,
	    encode_neg_int(Int, Fraction);
       Sign == 0 ->
	    encode_int(Int0, Fraction)
    end.


encode_int(I,R) when I >= 0, I =< 16#7fffffff ->
    ?dbg("encode_int(~p, ~p)~n", [I,R]),
    if R == none ->
	    << ?pos4, I:31, 0:1 >>;
       true -> 
	    RSz = bit_size(R),
	    <<Fraction:RSz>> = R,
	    ?dbg("Fraction = ~p~n", [Fraction]),
	    if Fraction == 0 ->
		    << ?pos4, I:31, 1:1, 8:8 >>;
	       true ->
		    Rbits = encode_bits_elems(R),
		    << ?pos4, I:31, 1:1, Rbits/binary >>
	       end
    end;
encode_int(I,R) when I > 16#7fffffff ->
    ?dbg("encode_int(~p, ~p)~n", [I,R]),
    Bytes = encode_big(I),
    if R == none ->
	    <<?posbig, Bytes/binary, 0:8>>;
       true ->
	    RSz = bit_size(R),
	    <<Fraction:RSz>> = R,
	    ?dbg("Fraction = ~p~n", [Fraction]),
	    if Fraction == 0 ->
		    << ?posbig, Bytes/binary, 1:8, 8:8 >>;
	       true ->
		    Rbits = encode_bits_elems(R),
		    <<?posbig, Bytes/binary, 1:8, Rbits/binary>>
	    end
    end;
encode_int(I, R) when I < 0 ->
    encode_neg_int(I, R).

encode_neg_int(I,R) when I =< 0, I >= -16#7fffffff ->
    ?dbg("encode_neg_int(~p, ~p [sz: ~p])~n", [I,pp(R), try bit_size(R) catch error:_ -> "***" end]),
    Adj = max_value(31) + I,	% keep in mind that I < 0
    ?dbg("Adj = ~p~n", [erlang:integer_to_list(Adj,2)]),
    if R == none ->
	    << ?neg4, Adj:31, 1:1 >>;
       true ->
	    Rbits = encode_neg_bits(R),
	    ?dbg("R = ~p -> RBits = ~p~n", [pp(R), pp(Rbits)]),
	    << ?neg4, Adj:31, 0:1, Rbits/binary >>
    end;
encode_neg_int(I,R) when I < -16#7fFFffFF ->
    ?dbg("encode_neg_int(BIG ~p)~n", [I]),
    Bytes = encode_big_neg(I),
    ?dbg("Bytes = ~p~n", [Bytes]),
    if R == none ->
	    <<?negbig, Bytes/binary, 16#ff:8>>;
       true ->
	    Rbits = encode_neg_bits(R),
	    ?dbg("R = ~p -> RBits = ~p~n", [pp(R), pp(Rbits)]),
	    <<?negbig, Bytes/binary, 0, Rbits/binary>>
    end.

encode_neg_real(R) ->
    ?dbg("encode_neg_real(~p)~n", [R]),
     Sz = bit_size(R),
     MaxR = (1 bsl Sz) - 1,
     <<Ri:Sz>> = R,
     RAdj = MaxR - Ri,
    ?dbg("RAdj = ~p~n", [<<RAdj:Sz>>]),
    encode_bits_elems(<<RAdj:Sz>>).


encode_big(I) ->
    Bl = encode_big1(I),
    ?dbg("Bl = ~p~n", [Bl]),
    Bb = list_to_binary(Bl),
    ?dbg("Bb = ~p~n", [Bb]),
    encode_bin_elems(Bb).

encode_big_neg(I) ->
    {Words, Max} = get_max(-I),
    ?dbg("Words = ~p | Max = ~p~n", [Words,Max]),
    Iadj = Max + I, 		% keep in mind that I < 0
    ?dbg("IAdj = ~p~n", [Iadj]),
    Bin = encode_bin_elems(list_to_binary(encode_big1(Iadj))),
    ?dbg("Bin = ~p~n", [Bin]),
    WordsAdj = 16#ffffFFFF - Words,
    ?dbg("WordsAdj = ~p~n", [WordsAdj]),
    <<WordsAdj:32, Bin/binary>>.
    
encode_big1(I) ->
    encode_big1(I, []).

encode_big1(I, Acc) when I < 16#ff ->
    [I|Acc];
encode_big1(I, Acc) ->
    encode_big1(I bsr 8, [I band 16#ff | Acc]).



encode_list_elems([], Acc) -> <<Acc/binary, 0>>;
encode_list_elems([H|T], Acc) ->
    Enc = encode(H),
    encode_list_elems(T, <<Acc/binary, Enc/binary>>).

encode_bin_elems(<<>>) ->
    <<8>>;
encode_bin_elems(B) ->
    Pad = 8 - (size(B) rem 8),
    << (<< <<1:1, B1:8>> || <<B1>> <= B >>)/bitstring, 0:Pad, 8 >>.

%% encode_neg_bin_elems(<<>>) ->
%%     <<247>>;   % 16#ff - 8
%% encode_neg_bin_elems(B) ->
%%     Pad = 8 - (size(B) rem 8),
%%     << (<< <<0:1, (16#ff-B1):8>> || <<B1>> <= B >>)/bitstring, 1:Pad, 247 >>.

encode_neg_bits(<<>>) ->
    <<247>>;
encode_neg_bits(B) ->
    {Padded, TailBits} = pad_neg_bytes(B),
    ?dbg("TailBits = ~p~n", [TailBits]),
    TailSz0 = bit_size(TailBits),
    TailSz = 16#ff - TailSz0,
    if TailSz0 == 0 ->
	    Pad = 8 - (bit_size(Padded) rem 8),
	    Ip = max_value(Pad), % e.g. max_value(3) -> 2#111
	    <<Padded/bitstring, Ip:Pad, TailSz:8>>;
       true ->
            ?dbg("TailSz0 = ~p~n", [TailSz0]),
	    TailPad = 8 - TailSz0,
            ?dbg("TailPad = ~p~n", [TailPad]),
	    Itp = (1 bsl TailPad)-1,
            ?dbg("Itp = ~p~n", [Itp]),
%% 	    Pad = 8 - ((TailSz0 + TailPad + bit_size(Padded) + 1) rem 8),
            Pad = 8 - ((bit_size(Padded) + 1) rem 8),
            ?dbg("Pad = ~p~n", [Pad]),
	    Ip = max_value(Pad),
            ?dbg("Ip = ~p~n", [Ip]),
	    ?dbg("Pad = ~p~n", [Pad]),
	    ?dbg("TailSz = ~p~n", [TailSz]),
	    <<Padded/bitstring, 0:1, TailBits/bitstring,
	     Itp:TailPad, Ip:Pad, TailSz:8>>
    end.

pad_neg_bytes(Bin) ->
    pad_neg_bytes(Bin, <<>>).

pad_neg_bytes(<<H:8, T/bitstring>>, Acc) ->
    H1 = 16#ff - H,
    pad_neg_bytes(T, <<Acc/bitstring, 0:1, H1>>);
pad_neg_bytes(Bits, Acc) when is_bitstring(Bits) ->
    Sz = bit_size(Bits),
    Max = (1 bsl Sz) - 1,
    <<I0:Sz>> = Bits,
    I1 = Max - I0,
    {Acc, <<I1:Sz>>}.


encode_bits_elems(B) ->
    {Padded, TailBits} = pad_bytes(B),
    TailSz = bit_size(TailBits),
    TailPad = 8-TailSz,
    Pad = 8 - ((TailSz + TailPad + bit_size(Padded) + 1) rem 8),
    <<Padded/bitstring, 1:1, TailBits/bitstring, 0:TailPad, 0:Pad, TailSz:8>>.
	

pad_bytes(Bin) ->
    pad_bytes(Bin, <<>>).

pad_bytes(<<H:8, T/bitstring>>, Acc) ->
    pad_bytes(T, <<Acc/bitstring, 1:1, H>>);
pad_bytes(Bits, Acc) when is_bitstring(Bits) ->
    {Acc, Bits}.


%% ------------------------------------------------------
%% Decoding routines

%% decode_next([?number,N|Rest]) -> {N, Rest};
decode_next(<<?atom,Rest/binary>>) -> decode_atom(Rest);
decode_next(<<?pid, Rest/binary>>) -> decode_pid(Rest);
decode_next(<<?port, Rest/binary>>) -> decode_port(Rest);
decode_next(<<?reference,Rest/binary>>) -> decode_ref(Rest);
decode_next(<<?tuple,Sz:32, Rest/binary>>) -> decode_tuple(Sz,Rest);
decode_next(<<?list, Rest/binary>>) -> decode_list(Rest);
decode_next(<<?negbig, Rest/binary>>) -> decode_neg_big(Rest);
decode_next(<<?posbig, Rest/binary>>) -> decode_pos_big(Rest);
decode_next(<<?neg4, I:31, F:1, Rest/binary>>) -> decode_neg(I,F,Rest);
decode_next(<<?pos4, I:31, F:1, Rest/binary>>) -> decode_pos(I,F,Rest);
decode_next(<<?binary, Rest/binary>>) -> decode_binary(Rest).

decode_atom(B) ->
    {Bin, Rest} = decode_binary(B),
    {list_to_atom(binary_to_list(Bin)), Rest}.

decode_tuple(Sz, Elems) ->
    decode_tuple(Sz,Elems,[]).

decode_tuple(0, Rest, Acc) ->
    {list_to_tuple(lists:reverse(Acc)), Rest};
decode_tuple(N, Elems, Acc) ->
    {Term, Rest} = decode_next(Elems),
    decode_tuple(N-1, Rest, [Term|Acc]).

decode_list(Elems) ->
    decode_list(Elems, []).

decode_list(<<0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_list(Elems, Acc) ->
    {Term, Rest} = decode_next(Elems),
    decode_list(Rest, [Term|Acc]).

decode_pid(Bin) ->
    {Name, Rest} = decode_binary(Bin),
    <<Tail:9/binary, Rest1/binary>> = Rest,
    NameSz = size(Name),
    {binary_to_term(<<131,103,100,NameSz:16,Name/binary,Tail/binary>>), Rest1}.

decode_port(Bin) ->
    {Name, Rest} = decode_binary(Bin),
    <<Tail:5/binary, Rest1/binary>> = Rest,
    NameSz = size(Name),
    {binary_to_term(<<131,102,100,NameSz:16,Name/binary,Tail/binary>>), Rest1}.

decode_ref(Bin) ->
    {Name, Rest} = decode_binary(Bin),
    {Tail, Rest1} = decode_binary(Rest),
    NLen = size(Name),
    Len = (size(Tail)-1) div 4,
    RefBin = <<131,114,Len:16,100,NLen:16,Name/binary,Tail/binary>>,
    {binary_to_term(RefBin), Rest1}.

decode_neg(I, 1, Rest) ->
    {(I - 16#7fffFFFF), Rest};
decode_neg(I0, 0, Bin) ->  % for negative numbers, 0 means that it's a float
%%     {RealBits, Rest} = decode_binary(Bin),
    I = 16#7fffFFFF - I0,
    ?dbg("decode_neg()... I = ~p | Bin = ~p~n", [I, Bin]),
    decode_neg_float(I, Bin).

decode_neg_float(0, Bin) ->
%%     {RAdj, Rest} = decode_neg_binary(Bin),
%%     Sz = bit_size(RAdj),
%%     MaxR = (1 bsl Sz) - 1,
%%     <<IAdj:Sz>> = RAdj,
%%     Ri = MaxR - IAdj,
%%     R = <<Ri:Sz>>,
    {R, Rest} = decode_neg_binary(Bin),
    ?dbg("Bin = ~p~n", [pp(Bin)]),
    ?dbg("R = ~p | Rest = ~p~n", [pp(R), Rest]),
    Sz = bit_size(R),
    Offs = Sz - 53,
    ?dbg("Offs = ~p | Sz - ~p~n", [Offs, Sz]),
    <<_:Offs, 1:1, I:52>> = R,
    Exp = 1023 - Offs,
    <<F/float>> = <<1:1, Exp:11, I:52>>,
    {F, Rest};
decode_neg_float(I, Bin) ->
    {R, Rest} = decode_neg_binary(Bin),
    ?dbg("decode_neg_float: I = ~p | R = ~p~n", [I, R]),
    Sz = bit_size(R),
    ?dbg("Sz = ~p~n", [Sz]),
    <<Ri:Sz>> = R,
    ?dbg("Ri = ~p~n", [Ri]),
    if Ri == 0 ->
	    %% special case
	    {0.0-I, Rest};
       true ->
	    IBits = strip_first_one(I),
	    ?dbg("IBits = ~p~n", [pp(IBits)]),
	    Bits = <<IBits/bitstring, Ri:Sz>>,
	    ?dbg("Bits = ~p (Sz: ~p)~n", [pp(Bits), bit_size(Bits)]),
	    Exp = bit_size(IBits) + 1023,
	    ?dbg("Exp = ~p~n", [Exp]),
	    <<Frac:52, _/bitstring>> = <<Bits/bitstring, 0:52>>,
	    ?dbg("Frac = ~p~n", [Frac]),
	    <<F/float>> = <<1:1, Exp:11, Frac:52>>,
	    {F, Rest}
    end.


decode_pos(I, 0, Rest) ->
    {I, Rest};
decode_pos(0, 1, Bin) ->
    {Real, Rest} = decode_binary(Bin),
    Offs = bit_size(Real) - 53,
    <<0:Offs, 1:1, Frac:52>> = Real,
    Exp = 1023 - Offs,
    <<F/float>> = <<0:1, Exp:11, Frac:52>>,
    {F, Rest};
decode_pos(I, 1, Bin) ->	% float > 1
    ?dbg("decode_pos(~p, 1, ~p)~n", [I, Bin]),
    {Real, Rest} = decode_binary(Bin),
    case decode_binary(Bin) of
	{<<>>, Rest} ->
	    <<F/float>> = <<I/float>>,
	    {F, Rest};
	{Real, Rest} ->
	    ?dbg("Real = ~p~n", [Real]),
	    Exp = 52 - bit_size(Real) + 1023,
	    ?dbg("Exp = ~p~n", [Exp]),
	    Bits0 = <<I:31, Real/bitstring>>,
	    ?dbg("Bits0 = ~p~n", [Bits0]),
	    Bits = strip_one(Bits0),
	    <<Frac:52>> = Bits,
	    <<F/float>> = <<0:1, Exp:11, Frac:52>>,
	    {F, Rest}
    end.



decode_pos_big(Bin) ->
    ?dbg("decode_pos_big(~p)~n", [Bin]),
    {Ib, Rest} = decode_binary(Bin),
    ?dbg("Ib = ~p~n", [Ib]),
    ISz = size(Ib) * 8,
    ?dbg("ISz = ~p~n", [ISz]),
    <<I:ISz>> = Ib,
    ?dbg("I = ~p~n", [I]),
    <<F:8, Rest1/binary>> = Rest,
    ?dbg("Rest1 = ~p~n", [Rest1]),
    decode_pos(I, F, Rest1).

decode_neg_big(Bin) ->
    ?dbg("decode_neg_big(~p)~n", [Bin]),
    <<WordsAdj:32, Rest/binary>> = Bin,
    Words = 16#ffffFFFF - WordsAdj,
%%    {Ib, Rest1} = decode_neg_binary(Rest),
    ?dbg("Words = ~p~n", [Words]),
    {Ib, Rest1} = decode_binary(Rest),
    ?dbg("Ib = ~p | Rest1 = ~p~n", [Ib, Rest1]),
    ISz = size(Ib) * 8,
    <<I0:ISz>> = Ib,
    ?dbg("I0 = ~p~n", [I0]),
    Max = imax(Words),
    ?dbg("Max = ~p~n", [Max]),
    I = Max - I0,
    ?dbg("I = ~p~n", [I]),
    <<F:8, Rest2/binary>> = Rest1,
    ?dbg("F = ~p | Rest2 = ~p~n", [F, Rest2]),
    if F == 0 ->
	    decode_neg_float(I, Rest2);
       F == 16#ff ->
	    {-I, Rest2}
    end.


%% optimization - no need to loop through a very large number of zeros.    
strip_first_one(I) ->
    Sz = if I < 16#ff -> 8;
	    I < 16#ffff -> 16;
	    I < 16#ffffff -> 24;
	    I < 16#ffffffff -> 32;
	    true -> 52
	 end,
    strip_one(<<I:Sz>>).

strip_one(<<0:1, Rest/bitstring>>) -> strip_one(Rest);
strip_one(<<1:1, Rest/bitstring>>) -> Rest.


decode_binary(<<8, Rest/binary>>) ->  {<<>>, Rest};
decode_binary(B)     ->  decode_binary(B, 0, <<>>).

decode_binary(<<1:1,H:8,Rest/bitstring>>, N, Acc) ->
    case Rest of 
	<<1:1,_/bitstring>> ->
	    decode_binary(Rest, N+9, << Acc/binary, H >>);
	_ ->
	    Pad = 8 - ((N+9) rem 8),
	    <<0:Pad,EndBits,Rest1/binary>> = Rest,
	    TailPad = 8-EndBits,
	    <<Tail:EndBits,0:TailPad>> = <<H>>,
	    {<< Acc/binary, Tail:EndBits >>, Rest1}
    end.

decode_neg_binary(<<247, Rest/binary>>) ->  {<<>>, Rest};  % 16#ff - 8
decode_neg_binary(B)     ->  decode_neg_binary(B, 0, <<>>).

decode_neg_binary(<<0:1,H:8,Rest/bitstring>>, N, Acc) ->
    case Rest of 
	<<0:1,_/bitstring>> ->
	    decode_neg_binary(Rest, N+9, << Acc/binary, (16#ff - H) >>);
	_ ->
	    Pad = 8 - ((N+9) rem 8),
	    ?dbg("Pad = ~p~n", [Pad]),
	    IPad = (1 bsl Pad) - 1,
	    <<IPad:Pad,EndBits0,Rest1/binary>> = Rest,
	    ?dbg("EndBits0 = ~p~n", [EndBits0]),
	    EndBits = 16#ff - EndBits0,
	    ?dbg("EndBits = ~p~n", [EndBits]),
	    if EndBits == 0 ->
		    {<< Acc/binary, (16#ff - H)>>, Rest1};
	       true ->
		    <<Tail:EndBits,_/bitstring>> = <<(16#ff - H)>>,
		    ?dbg("Tail = ~p~n", [Tail]),
		    {<< Acc/binary, Tail:EndBits >>, Rest1}
	    end
    end.



max_fraction() ->
    %% 52-bit array of ones
    max_value(52).


%% The largest value that fits in Sz bits
max_value(Sz) ->
    (1 bsl Sz) - 1.

%% The largest value that fits in Words*64 bits.
imax(1) -> max_value(64);
imax(2) -> max_value(128);
imax(Words) -> max_value(Words*64).

imax1(0, Acc) ->
    Acc;
imax1(N, Acc) when N > 0 ->
    imax1(N-1, (Acc bsl 64) bor ?IMAX1).

%% Get the smallest imax/1 value that's larger than I.
get_max(I) -> get_max(I, 1, imax(1)).
get_max(I, W, Max) when I > Max ->
    get_max(I, W+1, (Max bsl 64) bor ?IMAX1);
get_max(_, W, Max) ->
    {W, Max}.
    
    

encode_test() ->
    L = test_list(),
    [{I,I} = {I,catch decode(encode(I))} || I <- L].


test_list() ->
    [-456453453477456464.45456,
     -5.23423564,
     -1.234234,
     -1.23423,
     -0.345,
     -0.34567,
     -0.0034567,
     0,
     0.00012345,
     0.12345,
     1.2345,
     123.45,
     456453453477456464.45456,
     a,
     aaa,
     {},
     {1},
     {1,2},
     {"","123"},
     {"1","234"},
     <<>>,
     <<1>>,
     <<1,5:3>>,
     <<1,5:4>>,
     [1,2,3],
     [],
     self(),
     spawn(fun() -> ok end),
     make_ref(),
     make_ref()|
     lists:sublist(erlang:ports(),1,2)].
