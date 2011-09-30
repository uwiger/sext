Module sext
===========


<h1>Module sext</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Sortable serialization library.



__Authors:__ Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decodes a binary generated using the function <a href="sext.md#encode-1"><code>sext:encode/1</code></a>.</td></tr><tr><td valign="top"><a href="#decode_hex-1">decode_hex/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_sb32-1">decode_sb32/1</a></td><td>Decodes a binary generated using the function <a href="#encode_sb32-1"><code>encode_sb32/1</code></a>.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encodes any Erlang term into a binary.</td></tr><tr><td valign="top"><a href="#encode_hex-1">encode_hex/1</a></td><td>Encodes any Erlang term into a hex-encoded binary.</td></tr><tr><td valign="top"><a href="#encode_sb32-1">encode_sb32/1</a></td><td>Encodes any Erlang term into an sb32-encoded binary.</td></tr><tr><td valign="top"><a href="#from_hex-1">from_hex/1</a></td><td>Converts from a hex-encoded binary into a 'normal' binary.</td></tr><tr><td valign="top"><a href="#from_sb32-1">from_sb32/1</a></td><td>Converts from an sb32-encoded bitstring into a 'normal' bitstring.</td></tr><tr><td valign="top"><a href="#prefix-1">prefix/1</a></td><td>Encodes a binary for prefix matching of similar encoded terms.</td></tr><tr><td valign="top"><a href="#prefix_hex-1">prefix_hex/1</a></td><td>Generates a hex-encoded binary for prefix matching.</td></tr><tr><td valign="top"><a href="#prefix_sb32-1">prefix_sb32/1</a></td><td>Generates an sb32-encoded binary for prefix matching.</td></tr><tr><td valign="top"><a href="#to_hex-1">to_hex/1</a></td><td>Converts a binary into a hex-encoded binary
This is conventional hex encoding, with the proviso that
only capital letters are used, e.g.</td></tr><tr><td valign="top"><a href="#to_sb32-1">to_sb32/1</a></td><td>Converts a bitstring into an sb-encoded bitstring.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="decode-1"></a>

<h3>decode/1</h3>





`decode(B::binary()) -> term()`
<br></br>




Decodes a binary generated using the function [`sext:encode/1`](sext.md#encode-1).<a name="decode_hex-1"></a>

<h3>decode_hex/1</h3>





`decode_hex(Data) -> any()`

<a name="decode_sb32-1"></a>

<h3>decode_sb32/1</h3>





`decode_sb32(Data) -> any()`



Decodes a binary generated using the function [`encode_sb32/1`](#encode_sb32-1).<a name="encode-1"></a>

<h3>encode/1</h3>





`encode(T::term()) -> binary()`
<br></br>




Encodes any Erlang term into a binary.
The lexical sorting properties of the encoded binary match those of the
original Erlang term. That is, encoded terms sort the same way as the
original terms would.<a name="encode_hex-1"></a>

<h3>encode_hex/1</h3>





`encode_hex(Term::any()) -> binary()`
<br></br>






Encodes any Erlang term into a hex-encoded binary.
This is similar to [`encode/1`](#encode-1), but produces an octet string that  
can be used without escaping in file names (containing only the characters  
0..9 and A..F). The sorting properties are preserved.

Note: The encoding used is regular hex-encoding, with the proviso that only
capital letters are used (mixing upper- and lowercase characters would break
the sorting property).<a name="encode_sb32-1"></a>

<h3>encode_sb32/1</h3>





`encode_sb32(Term::any()) -> binary()`
<br></br>






Encodes any Erlang term into an sb32-encoded binary.
This is similar to [`encode/1`](#encode-1), but produces an octet string that  
can be used without escaping in file names (containing only the characters  
0..9, A..V and '-'). The sorting properties are preserved.

Note: The encoding used is inspired by the base32 encoding described in
RFC3548, but uses a different alphabet in order to preserve the sort order.<a name="from_hex-1"></a>

<h3>from_hex/1</h3>





`from_hex(Bin::binary()) -> binary()`
<br></br>






Converts from a hex-encoded binary into a 'normal' binary

This function is the reverse of [`to_hex/1`](#to_hex-1).
<a name="from_sb32-1"></a>

<h3>from_sb32/1</h3>





`from_sb32(Bits::bitstring()) -> bitstring()`
<br></br>






Converts from an sb32-encoded bitstring into a 'normal' bitstring

This function is the reverse of [`to_sb32/1`](#to_sb32-1).<a name="prefix-1"></a>

<h3>prefix/1</h3>





`prefix(X::term()) -> binary()`
<br></br>




Encodes a binary for prefix matching of similar encoded terms.
Lists and tuples can be prefixed by using the '_' marker, similarly
to Erlang match specifications. For example:

* `prefix({1,2,`_','_'})' will result in a binary that is the same as
the first part of any encoded 4-tuple with the first two elements being
1 and 2. The prefix algorithm will search for the first '_', and treat
all following elements as if they were '_'.

* `prefix([1,2|`_'])' will result in a binary that is the same as the
first part of any encoded list where the first two elements are 1 and 2.
`prefix([1,2,`_'])' will give the same result, as the prefix pattern
is the same for all lists starting with [1,2|...].

* `prefix(Binary)` will result in a binary that is the same as the encoded
version of Binary, except that, instead of padding and terminating, the
encoded binary is truncated to the longest byte-aligned binary. The same
is done for bitstrings.

* `prefix({1,[1,2|`_'],'_'})' will prefix-encode the second element, and
let it end the resulting binary. This prefix will match any 3-tuple where
the first element is 1 and the second element is a list where the first
two elements are 1 and 2.

* `prefix([1,[1|`_']|'_'])' will result in a prefix that matches all lists
where the first element is 1 and the second element is a list where the
first element is 1.

* For all other data types, the prefix is the same as the encoded term.

<a name="prefix_hex-1"></a>

<h3>prefix_hex/1</h3>





`prefix_hex(X::term()) -> binary()`
<br></br>




Generates a hex-encoded binary for prefix matching.
This is similar to [`prefix/1`](#prefix-1), but generates a prefix for binaries
encoded with [`encode_hex/1`](#encode_hex-1), rather than [`encode/1`](#encode-1).<a name="prefix_sb32-1"></a>

<h3>prefix_sb32/1</h3>





`prefix_sb32(X::term()) -> binary()`
<br></br>




Generates an sb32-encoded binary for prefix matching.
This is similar to [`prefix/1`](#prefix-1), but generates a prefix for binaries
encoded with [`encode_sb32/1`](#encode_sb32-1), rather than [`encode/1`](#encode-1).<a name="to_hex-1"></a>

<h3>to_hex/1</h3>





`to_hex(Bin::binary()) -> binary()`
<br></br>




Converts a binary into a hex-encoded binary
This is conventional hex encoding, with the proviso that
only capital letters are used, e.g. `0..9A..F`.<a name="to_sb32-1"></a>

<h3>to_sb32/1</h3>





`to_sb32(Bits::bitstring()) -> binary()`
<br></br>






Converts a bitstring into an sb-encoded bitstring



sb32 (Sortable base32) is a variant of RFC3548, slightly rearranged to  
preserve the lexical sorting properties. Base32 was chosen to avoid  
filename-unfriendly characters. Also important is that the padding  
character be less than any character in the alphabet

sb32 alphabet:

<pre>
0 0     6 6     12 C     18 I     24 O     30 U
1 1     7 7     13 D     19 J     25 P     31 V
2 2     8 8     14 E     20 K     26 Q  (pad) -
3 3     9 9     15 F     21 L     27 R
4 4    10 A     16 G     22 M     28 S
5 5    11 B     17 H     23 N     29 T
</pre>


_Generated by EDoc, Sep 30 2011, 10:38:30._