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
%%
%% @author Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% @doc Bare-bones Tokyo Tyrant interface library.
%% This is an example to illustrate the use of Sortable EXernal Term (sext)
%% encoding.
%%
%% <a href="http://1978th.net/tokyotyrant/">Tokyo Tyrant</a> (TT) is an add-on 
%% to <a href="http://1978th.net/tokyocabinet/">Tokyo Cabinet</a>, adding
%% support for concurrent and remote access to Tokyo Cabinet (TC) through a 
%% TCP socket interface. TC supports storage of variable-length byte strings
%% as key-value pairs. The storage type can either be RAM-only or disk, and
%% either hash table or B-tree.
%%
%% Using sext-encoded terms in combination with TT's B-tree storage, it is
%% possible to store very large amounts of data on disk while honoring the 
%% Erlang Term ordering semantics. Using the `sext:prefix/1' function, it is
%% also possible to perform efficient range queries.
%%
%% Tokyo Tyrant is easy to install and get running. This module does not show
%% how that is done, nor does it automate the task of starting a TT server.
%% 
%% @end
-module(tt_proto).

-behaviour(gen_server).

-export([open/2,
	 put/3,
	 get/2,
	 mget/2,
	 keys/2]).

%% internal exports
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-compile(export_all).

-define(DEFAULT_PORT, 1978).

-record(st, {socket}).

%% @spec open(Name, Opts) -> {ok, pid()}
%% Opts = [Opt]
%% Opt  = {regname,atom()} | {port, integer()}
%%
%% @doc Connects to a running Tokyo Tyrant database server.
%% The default port, 1978, will be used unless another port is specified.
%% If the `regname' option is present, the Tokyo Tyrant proxy process will
%% register itself under that name, and the registered name can be used as 
%% an alias when accessing the database.
%% @end
%%
open(Name, Opts) ->
    case lists:keyfind(regname, 1, Opts) of
	false ->
	    gen_server:start_link(?MODULE, {Name, Opts}, []);
	{_,RegName} ->
	    gen_server:start_link({local,RegName}, ?MODULE,
				  {Name, Opts}, [])
    end.

%% @spec put(TT, Key::term(), Value::term()) -> ok | {error, Reason}
%% @doc Inserts a `{Key,Value}' tuple in the database TT.
%% @end
%%
put(TT, Key, Value) ->
    cmd(TT, {put, encode(Key), encode(Value)}).

%% @spec get(TT, Key::term()) -> {ok, Value} | {error, Reason}
%% @doc Looks up Key in the database TT.
%% Returns `{ok,Value}' if found, otherwise `{error,Reason}'.
%% @end
%%
get(TT, Key) ->
    case ask(TT, {get, encode(Key)}) of
	{ok, Vb} ->
	    {ok, decode(Vb)};
	Err ->
	    Err
    end.

%% @spec mget(TT, Keys::[term()]) -> {ok, [{K,V}]} | {error,Reason}
%% @doc Fetches multiple objects from the database TT.
%% All objects matching the list of keys will be returned. If no objects match,
%% the return value will be `{ok, []}'.
%% @end
%%
mget(TT, Keys) when is_list(Keys) ->
    Enc = [encode(K) || K <- Keys],
    case ask(TT, {mget, Enc}) of
	{ok, KVs} ->
	    {ok, [{decode(K),decode(V)} || {K,V} <- KVs]};
	Err ->
	    Err
    end.

%% @spec keys(TT, Prefix) -> {ok, Keys} | {error, Reason}
%% @doc Performs a prefix search in database TT based on Prefix.
%% For details on Prefix, @see sext:prefix/1.
%% @end
%%
keys(TT, Prefix) ->
    case ask(TT, {keys, encode_prefix(Prefix), 100}) of
	{ok, Keys} ->
	    {ok, [decode(K) || K <- Keys]};
	Err ->
	    Err
    end.


%% Tell TokyoTyrant to perform an operation. No reply other than
%% 0 (success), or non-zero (failure).
%%
cmd(TT, Req) ->
    gen_server:call(TT, {cmd, Req}).

ask(TT, Req) ->
    gen_server:call(TT, {ask, Req}).

encode(Term) ->
    sext:encode(Term).


decode(Bin) ->
    sext:decode(Bin).

encode_prefix(Term) ->
    sext:prefix(Term).


%% @hidden
init({_Name, Opts}) ->
%%    TTName = tt_name(Name, Opts),
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    case gen_tcp:connect({127,0,0,1}, Port, [binary,{active,false},
					     {nodelay,true}]) of
	{ok, Socket} ->
	    {ok, #st{socket = Socket}};
	Error ->
	    Error
    end.

%% @hidden
handle_call({cmd, Req}, _From, #st{socket = Sock} = S) ->
    Msg = mk_req(Req),
    gen_tcp:send(Sock, Msg),
    Reply = cmd_reply(Sock),
    {reply, Reply, S};
handle_call({ask, Req}, _From, #st{socket = Sock} = S) ->
    Msg = mk_req(Req),
    gen_tcp:send(Sock, Msg),
    Reply = ask_reply(Req, Sock),
    {reply, Reply, S}.


%% @hidden
handle_info(Msg, S) ->
    io:fwrite("handle_info(~p, ~p)~n", [Msg, S]),
    {noreply, S}.

%% @hidden
handle_cast(_, S) ->
    {stop, unknown_cast, S}.

%% @hidden
terminate(Reason, S) ->
    io:fwrite("terminate(~p, ~p)~n", [Reason, S]).

%% @hidden
code_change(_FromVsn, S, _Extra) ->
    {ok, S}.


mk_req({put, K, V}) ->
    KSz = byte_size(K),
    VSz = byte_size(V),
    << 16#c8, 16#10, KSz:32, VSz:32, K/binary, V/binary >>;
mk_req({get, K}) ->
    KSz = byte_size(K),
    << 16#c8, 16#30, KSz:32, K/binary >>;
mk_req({mget, Ks}) ->
    N = length(Ks),
    Packed = pack_values(Ks),
    << 16#c8, 16#31,
     N:32, Packed/binary >>;
mk_req({keys, Prefix, Limit}) ->
    PSz = byte_size(Prefix),
    << 16#c8, 16#58, PSz:32, Limit:32, Prefix/binary >>.

pack_values(Values) ->
    pack_values(Values, <<>>).

pack_values([H|T], Acc) ->
    Sz = byte_size(H),
    Bin = << Sz:32, H/binary >>,
    pack_values(T, << Acc/binary, Bin/binary >>);
pack_values([], Acc) ->
    Acc.


cmd_reply(Sock) ->
    case gen_tcp:recv(Sock, 1) of
	{ok, <<0>>} ->
	    ok;
	{ok, <<E>>} ->
	    {error, E};
	{error,_} = Err ->
	    Err
    end.

ask_reply(Req, Sock) ->
    Method = element(1, Req),
    case gen_tcp:recv(Sock, 0) of
	{ok, <<0, Rest/binary>>} ->
	    try get_reply(Method, Rest, Sock)
	    catch
		throw:{error,Reason} ->
		    {error, Reason}
	    end;
	{ok, <<E>>} ->
	    {error, E};
	{error,_} = Err ->
	    Err
    end.

get_reply(get, Data, Sock) ->
    {Val, _} = get_value(Data, Sock),
    {ok, Val};
get_reply(mget, Data, Sock) ->
    {N, D1} = get_word(Data, Sock),
    Result = get_N(N, D1, fun get_k_v/2, Sock),
    {ok, Result};
get_reply(keys, Data, Sock) ->
    {N, D1} = get_word(Data, Sock),
    Result = get_N(N, D1, fun get_value/2, Sock),
    {ok, Result}.

get_word(<<W:32, Rest/binary>>, _Sock) ->
    {W, Rest};
get_word(Sofar, Sock) ->
    Bin = get_data(Sock),
    get_word(<<Sofar/binary, Bin/binary>>, Sock).

get_value(<<Sz:32, V:Sz/binary, Rest/binary>>, _Sock) ->
    {V, Rest};
get_value(Sofar, Sock) ->
    Bin = get_data(Sock),
    get_value(<<Sofar/binary, Bin/binary>>, Sock).

get_k_v(<<KSz:32, VSz:32, K:KSz/binary, V:VSz/binary, Rest/binary>>, _Sock) ->
    {{K,V}, Rest};
get_k_v(Sofar, Sock) ->
    Bin = get_data(Sock),
    get_k_v(<<Sofar/binary, Bin/binary>>, Sock).

get_N(0, _, _, _) ->
    [];
get_N(N, Data, F, Sock) when N > 0 ->
    {Item, Rest} = F(Data, Sock),
    [Item | get_N(N-1, Rest, F, Sock)].

get_data(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Bin} ->
	    Bin;
	{error,_} = Err ->
	    throw(Err)
    end.
    
