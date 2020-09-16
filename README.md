# The sext application #

[<img src="http://quickcheck-ci.com/p/uwiger/sext.png" alt="Build Status" width="160px">](http://quickcheck-ci.com/p/uwiger/sext)

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

A sortable serialization library
This library offers a serialization format (a la term_to_binary()) that
preserves the Erlang term order.

```

Copyright 2014-2020 Ulf Wiger

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

```


# 1. Introduction #

The idea to this library came out of the need for disk-based storage
with ordered_set semantics in Erlang. One previous solution used Tokyo Cabinet,
in which a C routine is used to hook into the sorting logic of TC.

I thought a more generic solution would be to be able to have a version
of term_to_binary() that respected the ordering semantics of Erlang terms.

A new addition is support for 'sb32' encoding. This is my own version of
Base32 encoding, with a slightly different alphabet, in order to preserve
sorting properties while generating octet strings that are perfectly safe
to use in file names.

Another feature is "prefix encoding", which encodes a term and truncates
the result if it encounters a "wildcard" (e.g. `'$1'`
or `'_'`). This is to enable a convenient and efficient mapping
of Erlang match specifications to e.g. prefix matching on the external storage
and subsequent match_spec matching on the found erlang terms.

The serialization format supports all Erlang types, and preserves the
internal Erlang term order, with a few exceptions:

* Floats are represented based on the IEEE 764 Binary 64 standard
representation. This is the representation used by Erlang, specifically
the representation used when encoding floats in binaries. To be exact,
`sext` first normalizes the float by encoding it as an Erlang binary, then
serializes it.

* In Erlang, integers are cast to floats before comparing them to a float.
This means e.g. that the relative sort order of `1` and `1.0` is undefined.
It is not possible for `sext` to preserve this ambiguity after serialization,
since it could only be done by producing identical encodings for the two
terms, thereby sacrificing the property that encoding a value and then
decoding it again, should produce the initial value.


# 2. Specification #


## 2.1 Type tags ##

Each data type is encoded using a type tag (1 byte) that represents its order
in the global Erlang term ordering. The number type is divided into several
subtypes, to facilitate a reasonably efficient representation:


<table border="1"><tr align="left"><th>Type</th><th>Description</th><th>Tag</th></tr><tr><td>negbig</td><td>Negative bignum</td><td>8</td></tr><tr><td>neg4</td><td>Negative 31-bit integer</td><td>9</td></tr><tr><td>pos4</td><td>Positive 31-bit integer</td><td>10</td></tr><tr><td>posbig</td><td>Positive bignum</td><td>11</td></tr><tr><td>atom</td><td>Obj of type atom()</td><td>12</td></tr><tr><td>reference</td><td>Obj of type reference()</td><td>13</td></tr><tr><td>port</td><td>Obj of type port()</td><td>14</td></tr><tr><td>pid</td><td>Obj of type pid()</td><td>15</td></tr><tr><td>tuple</td><td>Obj of type tuple()</td><td>16</td></tr><tr><td>list</td><td>Obj of type map()</td><td>17, 1</td></tr><tr><td>list</td><td>Obj of type list()</td><td>17</td></tr><tr><td>binary</td><td>Obj of type binary()</td><td>18</td></tr><tr><td>bin_tail</td><td>Improper-tail marker followed by binary or bitstring</td><td>19</td></tr>
</table>



## 2.2 Tuples ##

Tuples are encoded as the tuple tag, followed by a 32-bit size element,
denoting the number of elements in the tuple, followed by each element
in the tuple individually encoded.


## 2.3 Lists ##

Lists are encoded as the list tag, followed by each element in the list
individually encoded, followed by the number 2 (1 byte).

Improper lists, e.g. `[1,2|3]`, have the number 1 inserted before the improper
tail. Since this also indicates the last element in the list, no end byte
is needed. This ensures that it sorts *before* any corresponding proper list,
as long as the improper tail is not a binary (binaries are greater than the
missing 'cons', or list, cell).

Improper lists that have a binary or bitstring as 'tail', e.g. `[1,2|<<1>>]`,
have a ?bin_tail (code 19) inserted before the tail. This ensures that it
sorts after a corresponding proper list.


## 2.4 Binaries and bitstrings ##

A binary is basically a bitstring whose size is a multiple of 8. From a sorting
perspective, binaries and bitstrings are both sorted as left-aligned bit
arrays.

```erlang
1> bitstring_to_list(<<11111111111:11>>).
[56,<<7:3>>]
```

Binaries and bitstrings are encoded as the binary tag, followed by each whole
byte, each padded with a leading 1 (one bit), followed by a number of 0-bits
to pad again make the size a multiple of 8 bits, followed by a byte whose
value is Bits, where Bits is the number of "remainder bits"; 8 if the original
binary is 8-bit aligned.

Example:

```erlang
2> sext:encode(<<1,2,3>>).
<<18,128,192,160,96,8>>
3> <<18, 1:1,1, 1:1,2, 1:1,3, 0:5, 8>>.
<<18,128,192,160,96,8>>
```

In the example above, we inserted 3 1-bits, and therefore had to insert 5 more
pad bits (zeroes) at the end. The last byte is 8, signifying that the original
binary was 8-bit aligned.

If the remainder is not an even 8 bits, the remainder bits are padded with
a 1-bit, just like the others, then left-aligned and padded up to a whole
byte (excluding the 1-bit added in front).
The value of the last byte is the bit size of the remainder.

Example:

```erlang
2> sext:encode(<<1,2,3>>).
<<18,128,192,160,96,8>>
3> sext:encode(<<18, 1:1,1, 1:1,2, 1:1,3, 0:5, 8>>).
<<18,128,192,160,96,8>>
```

The first part of the bitstring is encoded exactly like above. The number 4:3
is first padded with 1 then padded at the end to become a whole byte. Then
an additional pad, 0:4, is inserted to compensate for the fact that we have
inserted 4 1-bits. Finally, the last byte is 3, to signify the size of the
remainder.


## 2.5 Positive Numbers ##

Numbers are encoded as the corresponding type tag, followed by the integer
part, a marker indicating the presence of a fraction part, and the fraction
part, if any. The integer part is encoded differently depending on the size
of the value. The fraction part is encoded as a binary (without the 'binary'
type tag).


### 2.5.1 Positive small integers, pos4 ###

Integers up to 31 bits are encoded as << ?pos4, I:31, F:1 >>
where I is the integer value, and F is 1 if a fraction part follows;
0 otherwise.


### 2.5.2 Positive large integers ###

Larger integers are converted to a byte string and then encoded like
binaries (without the 'binary' type tag), followed by a byte signifying
whether a fraction part follows (1 if yes; 0 otherwise).

```erlang
Bytes = encode_big(I),
<< ?pos_big, Bytes/binary, F:8 >>
```


### 2.5.3 Fraction part of positive numbers ###

The representation of floating point numbers is based on the [IEEE 764 Binary 64 standard representation](http://en.wikipedia.org/wiki/Double_precision_floating-point_format). This is also the representation used by Erlang:

```erlang
<<Sign:1, Exp:11, Frac:52>> = <<F/float>>
```

The encoding extracts the integer part and encodes it as a positive integer
(either pos4 or pos_big), flags the presence of a fraction part, and encodes
the fraction part as a binary (without the binary tag).


## 2.6 Negative Numbers ##


### 2.6.1 Small negative numbers ###

```erlang
<< ?neg4:8, IRep:31, F:1 >>
```

A negative number I is encoded as IRep = Max + I, where Max is the largest
possible number that can be represented with the number of bits present for
the given subtype. For example, Max for neg4 is 0x7FFF FFFF (31 bits).
Keep in mind that I < 0.

The fraction flag is inverted, compared to the pos4 representation, so it will
be 1 if there is no fraction part; 0 otherwise.


### 2.6.2 Large negative numbers ###

Larger negative numbers are encoded as:

```erlang
encode_negbig(I) ->
    {Words, Max} = get_max(-I),
    Bin = encode_bin_elems(list_to_binary(encode_big(Max + I)),
    WordsRep = 16#FFFFffff - Words,
    << ?neg_big:8, WordsRep:32, Bin/binary, F:8 >>.
```

That is, get_max() figures out how many 64-bit words are needed to represent
-I (the positive number), and also gives the maximum value that can be
represented in so many words. WordsRep in essence becomes a sub-subtag of
the negative bignum.


### 2.6.3 Fraction of negative numbers ###

The fraction is encoded almost like the inverse of the positive fraction
(as a "negative binary", if such a thing existed). Each byte is padded with
a 0-bit rather than a 1-bit, and the byte itself is replaced by 16#ff - Byte.
The sequence is then padded with 1s to become a multiple of 8 bits.

The last byte, denoting the number of significant bits in the last byte,
is similarly inverted.


## 2.7 Atoms ##

Atoms are encoded as the atom tag, followed by the string representation of
the atom using the binary encoding described above (but without the binary
tag).


## 2.8 References ##

The encoding of references is perhaps best described by the code:

```erlang
encode_ref(R) ->
    RBin = term_to_binary(R),
    <<131,114,_Len:16,100,NLen:16,Name:NLen/binary,Rest/binary>> = RBin,
    NameEnc = encode_bin_elems(Name),
    RestEnc = encode_bin_elems(Rest),
    <<?reference, NameEnc/binary, RestEnc/binary>>.
```

where encode_bin_elems(B) encodes the argument B the same way as a binary
(excluding the 'binary' type tag).


## 2.9 Ports ##

The encoding of ports is perhaps best described by the code:

```erlang
encode_port(P) ->
    PBin = term_to_binary(P),
    <<131,102,100,ALen:16,Name:ALen/binary,Rest:5/binary>> = PBin,
    NameEnc = encode_bin_elems(Name),
    <<?port, NameEnc/binary, Rest/binary>>.
```


## 2.10 Pids ##

The encoding of ports is perhaps best described by the code:

```erlang
encode_pid(P) ->
    PBin = term_to_binary(P),
    <<131,103,100,ALen:16,Name:ALen/binary,Rest:9/binary>> = PBin,
    NameEnc = encode_bin_elems(Name),
    <<?pid, NameEnc/binary, Rest/binary>>.
```


## 2.11 Maps ##

The encoding of maps is currently experimental.
Maps sort between tuples and lists. Since the smallest list is represented
by `<<17, 2>>`, maps encoding starts with `<<17, 1>>` (introducing a new tag
would break backwards compatibility), followed by the size of the map (4 bytes),
and each Key-Value pair in the map.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/uwiger/sext/blob/uw-maps/doc/sext.md" class="module">sext</a></td></tr></table>

