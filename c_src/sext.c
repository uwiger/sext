#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

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

static ERL_NIF_TERM decode_binary_int(ErlNifEnv* env, int argc, 
                                      const ERL_NIF_TERM argv[]) {
    
    ErlNifBinary bin, out, rest;
    uint32_t end; 
    if( enif_inspect_binary(env, argv[0], &bin) &&
        enif_get_uint(env, argv[1], &end) ) {
        // 1280 = (1440 * 8) / 9, the longest size we can get.
        uint8_t scan_buffer[1280], last = 0;
        int scan_point = 0, 
               out_pos = 0;
        memset(scan_buffer, 0, 1280);
        while( scan_point < bin.size ) {
            uint8_t in_byte = bin.data[scan_point],
                      shift = (scan_point % 9) + 1;

            if( scan_point == 0 && bin.data[0] == 8 ) {
                scan_buffer[0] = 8;
                scan_point += 1;
                break;
            }
            if( shift == 9 ) {
                //easy case, just write the unshifted byte
                scan_buffer[out_pos/8] |= in_byte;
                out_pos += 8;
                last = 0;
            } else {
                if( !(in_byte & (1 << (8 - shift))) ) {
                    if( bin.data[scan_point+1] == 8 ) {
                        out_pos += shift;
                        scan_buffer[(out_pos/8)-1] |= last|(in_byte >> (9 - shift));
                        scan_point += 2;
                        break;
                    } else if( bin.data[scan_point+1] < 8 ) {
                        //this requires subbinary support, fall back to slow path
                        return enif_make_atom(env, "fallback");
                    } else {
                        return enif_make_tuple2(env, 
                                                enif_make_atom(env, "error"), 
                                                enif_make_atom(env, "badmatch"));
                    }
                } 
                in_byte ^= (1 << (8 - shift));
                out_pos += (shift-1);
                if( scan_point != 0) {
                    scan_buffer[(out_pos/8)-1] |= last|(in_byte >> (9 - shift));
                }
                last = in_byte << shift;
                out_pos += (8 - shift);
            }
            
            scan_point += 1;
        }
        
        if( !enif_alloc_binary((out_pos/8), &out) ) { 
            return enif_make_tuple2(env, 
                                    enif_make_atom(env, "error"), 
                                    enif_make_atom(env, "allocation_error"));
        }
        memcpy(out.data, scan_buffer, out_pos/8);
        if( end == 1 ) {
            if( !enif_alloc_binary((bin.size-scan_point), &rest) ) { 
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "allocation_error"));
            } else {
                memcpy(rest.data, (uint8_t *)bin.data+scan_point, (bin.size-scan_point));
                
                return enif_make_tuple2(env, 
                                        enif_make_binary(env, &out),
                                        enif_make_binary(env, &rest));
            }
        } else { 
            return enif_make_binary(env, &out);
        }
    } else {
        return enif_make_badarg(env);
    }

} 

static ErlNifFunc nif_funcs[] =
{
    {"encode_bin_elems_int", 2, encode_bin_elems_int},
    {"decode_binary_int", 2, decode_binary_int}
};

ERL_NIF_INIT(sext,nif_funcs,NULL,NULL,NULL,NULL)
