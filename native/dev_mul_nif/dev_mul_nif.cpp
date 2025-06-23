#include <erl_nif.h>
#include "include/dev_mul.h"

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {}

static ERL_NIF_TERM mul_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int a, b;
    if (!enif_get_int(env, argv[0], &a) || !enif_get_int(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }

    int result = multiply(a, b);
    return enif_make_int(env, result);
}

static ErlNifFunc nif_funcs[] = {
    {"multiply", 2, mul_nif}
};

ERL_NIF_INIT(dev_mul_nif, nif_funcs, load, NULL, NULL, unload)
