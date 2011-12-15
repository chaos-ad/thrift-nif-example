#include <boost/unordered_map.hpp>

#include "thrift/test_erl.h"

using namespace apache::thrift::erl_helpers;

/////////////////////////////////////////////////////////////////////////////

typedef boost::unordered_map<std::string, api_fn_t> function_map_t;

function_map_t const& get_pack_functions()
{
    static bool initialied = false;
    static function_map_t instance;
    if(!initialied) {
        initialied = true;
        std::map<std::string, api_fn_t> map = test_nif::get_pack_functions();
        std::map<std::string, api_fn_t>::const_iterator i, end = map.end();
        for( i = map.begin(); i != end; ++i ) {
            instance["test_types" + i->first] = i->second;
        }
    }
    return instance;
}

function_map_t const& get_unpack_functions()
{
    static bool initialied = false;
    static function_map_t instance;
    if(!initialied) {
        initialied = true;
        std::map<std::string, api_fn_t> map = test_nif::get_unpack_functions();
        std::map<std::string, api_fn_t>::const_iterator i, end = map.end();
        for( i = map.begin(); i != end; ++i ) {
            instance["test_types" + i->first] = i->second;
        }
    }
    return instance;
}

api_fn_t get_pack_function(std::string const& module, std::string const& type)
{
    function_map_t::const_iterator i = get_pack_functions().find(module + type);
    if ( i != get_pack_functions().end() ) {
        return i->second;
    }
    return 0;
}

api_fn_t get_unpack_function(std::string const& module, std::string const& type)
{
    function_map_t::const_iterator i = get_unpack_functions().find(module + type);
    if ( i != get_pack_functions().end() ) {
        return i->second;
    }
    return 0;
}

/////////////////////////////////////////////////////////////////////////////

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM enomem;
    ERL_NIF_TERM invalid_args;
    ERL_NIF_TERM invalid_type;
    ERL_NIF_TERM not_implemented;
} atoms;

/////////////////////////////////////////////////////////////////////////////

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    atoms.ok                = enif_make_atom(env, "ok");
    atoms.error             = enif_make_atom(env, "error");
    atoms.enomem            = enif_make_atom(env, "enomem");
    atoms.invalid_args      = enif_make_atom(env, "invalid_args");
    atoms.invalid_type      = enif_make_atom(env, "invalid_type");
    atoms.not_implemented   = enif_make_atom(env, "not_implemented");
    return 0;
}

/////////////////////////////////////////////////////////////////////////////

static ERL_NIF_TERM pack(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        if (argc != 3)
        {
            return enif_make_tuple2(env, atoms.error, atoms.invalid_args);
        }

        atom_t type = read_value<atom_t>(env, argv[1]);
        atom_t module = read_value<atom_t>(env, argv[0]);
        if (api_fn_t pack_fn = get_pack_function(module, type))
        {
            return enif_make_tuple2(env, atoms.ok, pack_fn(env, argv[2]));
        }
        else
        {
            return enif_make_tuple2(env, atoms.error, atoms.invalid_type);
        }
    }
    catch( std::exception & ex )
    {
        return enif_make_tuple2(env, atoms.error, enif_make_atom(env, ex.what()));
    }
}

static ERL_NIF_TERM unpack(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        if (argc != 3)
        {
            return enif_make_tuple2(env, atoms.error, atoms.invalid_args);
        }

        atom_t type = read_value<atom_t>(env, argv[1]);
        atom_t module = read_value<atom_t>(env, argv[0]);
        if (api_fn_t pack_fn = get_unpack_function(module, type))
        {
            return enif_make_tuple2(env, atoms.ok, pack_fn(env, argv[2]));
        }
        else
        {
            return enif_make_tuple2(env, atoms.error, atoms.invalid_type);
        }
    }
    catch( std::exception & ex )
    {
        return enif_make_tuple2(env, atoms.error, enif_make_atom(env, ex.what()));
    }
}

/////////////////////////////////////////////////////////////////////////////

static ErlNifFunc nif_funcs[] = {
    {"do_pack",   3, pack},
    {"do_unpack", 3, unpack}
};

ERL_NIF_INIT(packer, nif_funcs, &load, NULL, NULL, NULL)

/////////////////////////////////////////////////////////////////////////////

