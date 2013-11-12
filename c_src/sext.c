#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#define SEXT_MARKER(n) ((unsigned char)n)

#define SEXT_NEGBIG   SEXT_MARKER(8)
#define SEXT_NEG4     SEXT_MARKER(9)
#define SEXT_POS4     SEXT_MARKER(10)
#define SEXT_POSBIG   SEXT_MARKER(11)
#define SEXT_ATOM     SEXT_MARKER(12)
#define SEXT_REF      SEXT_MARKER(13)
#define SEXT_PORT     SEXT_MARKER(14)
#define SEXT_PID      SEXT_MARKER(15)
#define SEXT_TUPLE    SEXT_MARKER(16)
#define SEXT_LIST     SEXT_MARKER(17)
#define SEXT_BINARY   SEXT_MARKER(18)
#define SEXT_BIN_TAIL SEXT_MARKER(19)

#define SIZE_ERROR ((size_t)-1)
#define SIZE_UNSUPPORTED (SIZE_ERROR-1)
#define SEXT_MAX_INT_31 ((uint32_t)0x7fffFFFF)
#define BYTE_HIGH_BIT ((unsigned char)0x80)

// If defined, tries some fancy optimizations. So far I have not seen
// huge improvements with -O3, but about 5-10% ain't bad. Compilers 1, Engel 0
#define HAND_ROLLED

enum DecodeResult {DECODE_OK, DECODE_INVALID, DECODE_UNSUPPORTED, DECODE_ERROR};

static enum DecodeResult
decode_one(ErlNifEnv* env, ErlNifBinary *bin, size_t * ofs,
        ERL_NIF_TERM * result);

static inline uint32_t to_uint_32(unsigned char *d)
{
    // Interpret as unsigned first to avoid sign bit/shift side effects,
    // cast as signed at the end.
    uint32_t b1 = d[0], b2 = d[1], b3 = d[2], b4 = d[3];
    // Sext encodes in big endian (most significant byte first)
    return ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
}

/*
   A sext encoded binary consists of series of 1 bits markers + 8 bits of data.
   It should look like this, where _ denotes a data bit:

   0         1         2         3         4
   1___;____|_1__;____|__1_;____|___1;____|____;1___|
   5         6         7         8         9
   ____;_1__|____;__1_|____;___1|____;____|1... starts all over again

   The sequence ends when the next marker bit is zero. At that point there
   will be some zero padding until the next byte boundary and a end marker
   byte = 8. Bitstrings would have something < 8, but we don't support those.
   */

// Returns largest size_t if failed to decode the binary.
static size_t count_binary_bytes(ErlNifBinary *bin, size_t ofs)
{
    // As a special case, and encoded empty binary is a single 8 byte
    if (bin->data[ofs] == 8) {
        return 0;
    }

    size_t n = 0;
#ifdef HAND_ROLLED
    // This tries to count the marker bits in groups of eight,
    // trading more CPU arithmetic for less branching
    size_t available = bin->size - ofs;
    while (available > 8) {
        const unsigned char * b = bin->data + ofs;
        const unsigned char mask =
            (b[0] >> 7) | (b[1] >> 5 & 2) | (b[2] >> 3 & 4) |
            (b[3] >> 1 & 8) | (b[4] << 1 & 16) | (b[5] << 3 & 32) |
            (b[6] << 5 & 64) | (b[7] << 7 & 128);

        // Not all 8 bits set, so end is here. Skip to normal loop.
        if (mask != (unsigned char)0xff) {
            break;
        }
        available -= 8;
        n += 8;
        ofs += 9;
    }
#endif

    int bit_ofs = 0; // bit within byte offset (0 high bit, 7 low bit)
    // While space for byte + end marker and marker is a 1
    while ((ofs+1) < bin->size) {
        // If next bit is a 1
        if (bin->data[ofs] & (BYTE_HIGH_BIT >> bit_ofs)) {
            ++n;
            ++bit_ofs;
            ofs += 1 + (bit_ofs >> 3);
            bit_ofs &= 7;
        } else { // zero bit marks end sequence
            // Fail if not padded with zeros, followed by an 8 byte.
            unsigned char tailMask = 0xffu >> bit_ofs;
            if ((tailMask & bin->data[ofs])) {
                return SIZE_ERROR;
            }
            if (bin->data[ofs+1] != 8) {
                return SIZE_UNSUPPORTED;
            }
            return n;
        }
    }

    return SIZE_ERROR;
}

// Reads a number of encoded bytes without validation, as that should have
// happened in count_binary_bytes.
static void decode_binary_bytes(ErlNifBinary *bin, size_t * ofs_ptr,
        unsigned char * data, size_t size)
{
    size_t ofs = *ofs_ptr; // byte offset
    size_t i = 0;
#ifdef HAND_ROLLED
    size_t rem = size;

    // Old school unrolling optimization.
    // The theory is to avoid expensive branch prediction and allow the
    // arithmetic pipeline to do its thing crunching integers in parallel.
    for (; rem >= 8; rem -= 8, i += 8, ofs += 9) {
        const unsigned char * in = bin->data + ofs;
        unsigned char * out = data + i;
        out[0] = (in[0] << 1) | (in[1] >> 7);
        out[1] = (in[1] << 2) | (in[2] >> 6);
        out[2] = (in[2] << 3) | (in[3] >> 5);
        out[3] = (in[3] << 4) | (in[4] >> 4);
        out[4] = (in[4] << 5) | (in[5] >> 3);
        out[5] = (in[5] << 6) | (in[6] >> 2);
        out[6] = (in[6] << 7) | (in[7] >> 1);
        out[7] = in[8];
    }

    const unsigned char * in = bin->data + ofs;
    unsigned char * out = data + i;
    switch((int)rem) {
        case 7: out[6] = (in[6] << 7) | (in[7] >> 1);
        case 6: out[5] = (in[5] << 6) | (in[6] >> 2);
        case 5: out[4] = (in[4] << 5) | (in[5] >> 3);
        case 4: out[3] = (in[3] << 4) | (in[4] >> 4);
        case 3: out[2] = (in[2] << 3) | (in[3] >> 5);
        case 2: out[1] = (in[1] << 2) | (in[2] >> 6);
        case 1: out[0] = (in[0] << 1) | (in[1] >> 7);
    }

    ofs += rem;
#else
    int bit_ofs = 0; // bit within byte offset (0-7)
    while (i < size) {
        unsigned char b1 = bin->data[ofs];
        unsigned char b2 = bin->data[ofs+1];
        data[i++] = (b1 << bit_ofs+1) | (b2 >> 7-bit_ofs);
        ++bit_ofs;
        ofs += 1 + (bit_ofs >> 3);
        bit_ofs &= 7; // bitf_ofs % 8
    }
#endif
    // Move offset after final '8' byte marker.
    *ofs_ptr = ofs + 2;
}

static enum DecodeResult decode_pos4(ErlNifEnv* env, ErlNifBinary *bin,
        size_t *ofs, ERL_NIF_TERM * term_out)
{
    if (bin->size - *ofs < 4) {
        return DECODE_INVALID;
    }
    // Bit 32 set to 1 indicates a float, not supported
    unsigned char * d = bin->data + *ofs;
    if (d[3] & 1) {
        return DECODE_UNSUPPORTED;
    }
    uint32_t i1 = d[0], i2 = d[1], i3 = d[2], i4 = d[3];
    int32_t out = (int32_t)((i1 << 23) | (i2 << 15) | (i3 << 7) | (i4 >> 1));
    *ofs += 4;

    *term_out = enif_make_int(env, out);
    return DECODE_OK;
}

static enum DecodeResult decode_neg4(ErlNifEnv* env, ErlNifBinary *bin,
        size_t *ofs, ERL_NIF_TERM * term_out)
{
    if (bin->size - *ofs < 4) {
        return DECODE_INVALID;
    }
    unsigned char * d = bin->data + *ofs;
    // Bit 32 set to 0 means float
    if ((d[3] & 1) == 0) {
        return DECODE_UNSUPPORTED;
    }
    uint32_t i1 = d[0], i2 = d[1], i3 = d[2], i4 = d[3];
    int32_t out = (int32_t)((i1 << 23) | (i2 << 15) | (i3 << 7) | (i4 >> 1))
        - SEXT_MAX_INT_31;
    *ofs += 4;
    *term_out = enif_make_int(env, out);
    return DECODE_OK;
}

static enum DecodeResult decode_binary(ErlNifEnv* env, ErlNifBinary * bin,
        size_t *ofs, ERL_NIF_TERM * term_out)
{
    // Doing this in two passes: count bytes, allocate a binary buffer once
    // and copy once into the allocated buffer.
    // The theory is that the objects are normally small and a second
    // scan would be fast, in cache. That should be lighter than an extra
    // memory allocation and copy
    size_t num_bytes = count_binary_bytes(bin, *ofs);
    if (num_bytes == SIZE_ERROR) {
        return DECODE_INVALID;
    }
    if (num_bytes == SIZE_UNSUPPORTED) {
        return DECODE_UNSUPPORTED;
    }

    ErlNifBinary bout;
    if (!enif_alloc_binary(num_bytes, &bout)) {
        *term_out = enif_make_atom(env, "memory_allocation");
        return DECODE_ERROR;
    }

    decode_binary_bytes(bin, ofs, bout.data, num_bytes);
    *term_out = enif_make_binary(env, &bout);
    return DECODE_OK;
}

#define MAX_BIN_INT_LEN (10)

// Sets the decode_in_erlang flag if the int is too large to handle in a NIF.
// There are no functions to handle Erlang really big ints in C land.
static enum DecodeResult decode_posbig(ErlNifEnv * env, ErlNifBinary * bin,
        size_t * ofs_ptr, ERL_NIF_TERM * term_out)
{
    size_t ofs = *ofs_ptr;
    /*
     * Encoded as a binary, then a zero byte
     * First decode the following bytes as a binary.
     * Then look at the first byte of that binary: 255 means a "new" big int. anything else
     * means a legacy big int.
     * Then the 7 bit tagged size, a single byte if # of int bytes < 128
     */

    size_t bin_size = count_binary_bytes(bin, ofs);
    if (bin_size == SIZE_ERROR || bin_size == 0) {
        return DECODE_INVALID;
    }

    unsigned char buf[MAX_BIN_INT_LEN];
    unsigned char * end = buf + bin_size;
    unsigned char * int_bytes;

    decode_binary_bytes(bin, ofs_ptr, buf, bin_size);

    if (buf[0] == 255) {
        // Skip big int size encoded as a series of 7 bit tagged data.
        // Only useful for correct lexicographical sort of big ints.
        // NOTE: This means an incorrect size won't be detected.
        // Erlang sext ignores this too.
        int_bytes = buf + 1;
        do {
            if (int_bytes == end) {
                return DECODE_INVALID;
            }
        } while (*int_bytes++ & BYTE_HIGH_BIT);
        if (int_bytes == end) {
            return DECODE_INVALID;
        }
    } else { // A legacy, broken big int that does not sort correctly.
        int_bytes = buf;
    }

    // We can't handle ints > 64 bits (8 bytes)
    if (end - int_bytes > 8) {
        return DECODE_UNSUPPORTED;
    }

    uint64_t val = *int_bytes++;
    while (int_bytes < end) {
        val = (val << 8) | *int_bytes++;
    }

    if (bin->data[*ofs_ptr] != 0) {
        return DECODE_UNSUPPORTED;
    }

    ++(*ofs_ptr); // skip float marker
    *term_out = enif_make_uint64(env, val);
    return DECODE_OK;
}
// Sets the decode_in_erlang flag if the int is too large to handle in a NIF.
// There are no functions to handle Erlang really big ints in C land.
static enum DecodeResult decode_negbig(ErlNifEnv * env, ErlNifBinary * bin,
        size_t * ofs_ptr, ERL_NIF_TERM * term_out)
{
    // Should have at least 4 bytes int and one byte for binary.
    if ((bin->size - *ofs_ptr) < 5) {
        return DECODE_INVALID;
    }

    uint32_t words = ((uint32_t)-1) - to_uint_32(bin->data+*ofs_ptr);
    *ofs_ptr += 4;
    uint64_t ceil;

    switch(words) {
        case 0:
            return DECODE_INVALID;
        case 1:
            ceil = ((uint64_t)-1);
            break;
        default:
            return DECODE_UNSUPPORTED;
    }

    size_t bin_size = count_binary_bytes(bin, *ofs_ptr);
    if ((bin_size == SIZE_ERROR) || (bin_size > words * 8)) {
        return DECODE_INVALID;
    }

    unsigned char buf[MAX_BIN_INT_LEN];
    unsigned char * end = buf + bin_size;

    decode_binary_bytes(bin, ofs_ptr, buf, bin_size);

    unsigned char * int_bytes = buf;
    uint64_t val = 0;

    while (int_bytes < end) {
        val = (val << 8) | *int_bytes++;
    }

    unsigned char fbyte = bin->data[*ofs_ptr];

    if (fbyte == 0) { // float
        return DECODE_UNSUPPORTED;
    }

    if (fbyte != 0xff) {
        return DECODE_INVALID;
    }

    ++(*ofs_ptr);
    *term_out = enif_make_int64(env, -((int64_t)ceil - val));
    return DECODE_OK;
}

#define MAX_SMALL_ATOM_LENGTH 128

static enum DecodeResult decode_atom(ErlNifEnv * env, ErlNifBinary * bin,
        size_t *ofs, ERL_NIF_TERM * term_out )
{
    char small_atom_buf[MAX_SMALL_ATOM_LENGTH];
    size_t len = count_binary_bytes(bin, *ofs);
    if (len == (size_t)-1) {
        return DECODE_INVALID;
    }
    char *atom_buf;
    if (len <= MAX_SMALL_ATOM_LENGTH) {
        atom_buf = small_atom_buf;
    } else {
        atom_buf = malloc(len);
    }

    decode_binary_bytes(bin, ofs, (unsigned char*)atom_buf, len);

    *term_out = enif_make_atom_len(env, atom_buf, len);

    if (atom_buf != small_atom_buf) {
        free(atom_buf);
    }

    return DECODE_OK;
}

#define MAX_SMALL_ELEMS (8)

static enum DecodeResult decode_tuple(ErlNifEnv* env, ErlNifBinary *bin,
        size_t *ofs, ERL_NIF_TERM * term_out)
{
    unsigned char * d = bin->data + *ofs;
    unsigned char * end = bin->data + bin->size;
    // Need 4 bytes containing tuple size
    if (end - d < 4) {
        return DECODE_INVALID;
    }
    size_t size = to_uint_32(d);
    ERL_NIF_TERM elems_small[MAX_SMALL_ELEMS];
    ERL_NIF_TERM *elems;

    if (size <= MAX_SMALL_ELEMS) {
        elems = elems_small;
    } else {
        elems = malloc(sizeof(ERL_NIF_TERM)*size);
    }

    (*ofs) += 4;

    enum DecodeResult decode_result = DECODE_OK;
    size_t n;
    for (n = 0; n < size; ++n) {
        ERL_NIF_TERM term;
        decode_result = decode_one(env, bin, ofs, &term);
        if (decode_result != DECODE_OK) {
            break;
        }
        elems[n] = term;
    }

    if (decode_result == DECODE_OK) {
        *term_out = enif_make_tuple_from_array(env, elems, size);
    }

    if (elems != elems_small) {
        free(elems);
    }

    return decode_result;
}

static enum DecodeResult
decode_one(ErlNifEnv* env, ErlNifBinary *bin, size_t * ofs,
        ERL_NIF_TERM * term_out)
{
    unsigned char code = bin->data[(*ofs)++];
    if (*ofs >= bin->size) {
        return DECODE_INVALID;
    }
    switch(code) {
        case SEXT_NEGBIG:
            return decode_negbig(env, bin, ofs, term_out);
        case SEXT_NEG4:
            return decode_neg4(env, bin, ofs, term_out);
        case SEXT_POS4:
            return decode_pos4(env, bin, ofs, term_out);
        case SEXT_POSBIG:
            return decode_posbig(env, bin, ofs, term_out);
        case SEXT_ATOM:
            return decode_atom(env, bin, ofs, term_out);
        case SEXT_TUPLE:
            return decode_tuple(env, bin, ofs, term_out);
        case SEXT_BINARY:
            return decode_binary(env, bin, ofs, term_out);
        case SEXT_REF: case SEXT_PORT: case SEXT_PID:
        case SEXT_LIST: case SEXT_BIN_TAIL:
            return DECODE_UNSUPPORTED;
        default:
            return DECODE_INVALID;
    }
}

static ERL_NIF_TERM
sext_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (enif_inspect_binary(env, argv[0], &bin) && bin.size > 0) {
        size_t ofs = 0;
        ERL_NIF_TERM term;
        switch(decode_one(env, &bin, &ofs, &term)) {
            case DECODE_INVALID:
                return enif_make_tuple2(env,
                        enif_make_atom(env, "error"),
                        enif_make_atom(env, "invalid"));
            case DECODE_UNSUPPORTED:
                return enif_make_tuple2(env,
                        enif_make_atom(env, "error"),
                        enif_make_atom(env, "unsupported"));
            default:
                return enif_make_tuple2(env,
                        enif_make_atom(env, "ok"),
                        term);
        }
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM
sext_decode_binary(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (enif_inspect_binary(env, argv[0], &bin) && bin.size > 0) {
        ERL_NIF_TERM result;
        ERL_NIF_TERM rest;
        size_t ofs = 0;
        enum DecodeResult decode_result =
            decode_binary(env, &bin, &ofs, &result);
        switch(decode_result) {
            case DECODE_OK:
                rest = enif_make_sub_binary(env, argv[0], ofs, bin.size - ofs);
                return enif_make_tuple2(env, result, rest);
            case DECODE_UNSUPPORTED:
                return enif_make_atom(env, "fallback");
            default:
                break;
        }
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM encode_bin_elems_int(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]) {

    ErlNifBinary bin, out;
    uint32_t end;

    if( enif_inspect_binary(env, argv[0], &bin) &&
        enif_get_uint(env, argv[1], &end) ) {

        // make the new size.  This pads out <<>> to 1 byte, which is inadvertently
        // the right thing to do, since we need to return <<"8">>
        int new_bit_size = bin.size * 9;
        int pad_size = 0;
        if( end == 1 ) {
            pad_size = 8 - (new_bit_size % 8);
            if( pad_size == 8 && bin.size != 0) {
                pad_size = 16;
            }
        }
        //add one here for additional padding since we always end on a byte boundary
        int new_byte_size = ((new_bit_size+pad_size)/8);
        if( (new_bit_size % 8) != 0 ) {
            new_byte_size += 1;
        }

        if (!enif_alloc_binary(new_byte_size, &out)) {
            return enif_make_tuple2(env,
                                    enif_make_atom(env, "error"),
                                    enif_make_atom(env, "allocation_error"));
        }

        if (bin.size == 0) {
            if( end == 1 ) {
                out.data[0] = 8;
            }
        } else {
            int i,
                // out_pos is in *bits*, not bytes
                out_pos = 0;
            uint8_t next = 0;

            memset(out.data, 0, new_byte_size);
            if( end == 1 ) {
                out.data[new_byte_size-1] = 8;
            }

            for(i = 0; i <= bin.size; i++) {
                if( i != bin.size) {
                    uint8_t payload = bin.data[i],
                                 wr = 0;
                    // the byte that we're operating on.
                    int cursor = out_pos / 8,
                                 //the offset in the byte
                         shift = (out_pos % 8) + 1;
                    //or in our leftovers from the last byte
                    out.data[cursor] |= next;
                    //1:1
                    out.data[cursor] |= 1 << (8 - shift);
                    if( out_pos != 0 && shift == 8 ) {
                        out.data[cursor+1] |= payload;
                        next = 0;
                    } else {
                        wr = payload >> shift;
                        out.data[cursor] |= wr;
                        next = payload << (8 - shift);
                    }
                    out_pos += 9;
                } else {
                    out.data[(out_pos/8)] |= next;
                }
            }
        }
        return enif_make_binary(env, &out);
    } else {
        // return an error here
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] =
{
    {"encode_bin_elems_int", 2, encode_bin_elems_int},
    {"decode_binary_nif", 1, sext_decode_binary},
    {"decode_nif", 1, sext_decode}
};

ERL_NIF_INIT(sext,nif_funcs,NULL,NULL,NULL,NULL)
