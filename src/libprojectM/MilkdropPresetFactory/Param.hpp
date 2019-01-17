/**
 * projectM -- Milkdrop-esque visualisation SDK
 * Copyright (C)2003-2007 projectM Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * See 'LICENSE.txt' included within this release
 *
 */
/**
 * $Id$
 *
 * Parameter used within a preset
 *
 * $Log$
 */

#ifndef _PARAM_H
#define _PARAM_H

#include "ParamDef.hpp"
#include "Expr.hpp"
#include "Common.hpp"
#include <cmath>
#include <string>
class InitCond;
class Param;
class Preset;
class Test;


#ifdef __SSE2__
#include <immintrin.h>
#endif

/* Parameter Type */
class Param : public LValue
{
protected:
    Param(const std::string &name, short int type, short int flags,
          void * eqn_val, void *matrix,
          CValue default_init_val, CValue upper_bound,
          CValue lower_bound);

    /// Create a user defined floating point parameter
    explicit Param( const std::string &name );

public:
    std::string name; /* name of the parameter, not necessary but useful neverthless */
    short int type; /* parameter number type (int, bool, or float) */
    short int flags; /* read, write, user defined, etc */
protected:
    short int matrix_flag; /* for optimization purposes */
    void * engine_val; /* pointer to the engine variable */
    void * matrix; /* per pixel / per point matrix for this variable */
public:
    CValue default_init_val; /* a default initial condition value */
protected:
    CValue upper_bound; /* this parameter's upper bound */
    CValue lower_bound; /* this parameter's lower bound */

    // for a local variable, engine_val can point here
    float local_value;

public:
    /// Create a new parameter
    static Param * create(const std::string &name, short int type, short int flags,
           void * eqn_val, void *matrix,
           CValue default_init_val, CValue upper_bound,
           CValue lower_bound);

    static Param * createUser(const std::string &name);

    static Test *test();

    virtual ~Param();

    static bool is_valid_param_string( const char *string );
    virtual void set_param( expr_t val );        // same as LValue::set()
    virtual void set_param( CValue val );
    void set_param( std::string &text) { *((std::string*)engine_val) = text; }

    static Param *new_param_float( const char *name, short int flags, void *engine_val,
                             void *matrix, float upper_bound,
                             float lower_bound,
                             float init_val );

    static Param * new_param_int(const char * name, short int flags, void * engine_val,
                           int upper_bound, int lower_bound, int init_val );
    static Param * new_param_bool(const char * name, short int flags, void * engine_val,
                            bool upper_bound, bool lower_bound, bool init_val );
    static Param * new_param_string(const char * name, short int flags, void * engine_val);

    // return an Expr to inject directly into an Eqn
    // this allows the parameter to stay encapsulated, but not add extra levels of virtual functions
    // into the evaluation process

    virtual LValue *getExpr() = 0;
};


/* Sets the parameter engine value to value val.
	clipping occurs if necessary */
inline void Param::set_param( expr_t val )
{
    matrix_flag = false;
    switch (type)
    {
    case P_TYPE_BOOL:
            *((bool*)engine_val) = (val > 0);
        break;
    case P_TYPE_INT:
        /* Make sure value is an integer */
        val = floor(val);
        if (val < lower_bound.int_val())
            *((int*)engine_val) = lower_bound.int_val();
        else if (val > upper_bound.int_val())
            *((int*)engine_val) = upper_bound.int_val();
        else
            *((int*)engine_val) = (int)val;
        break;
    case P_TYPE_DOUBLE:
        /* Make sure value is an integer */
        if (val < lower_bound.float_val())
            *((float*)engine_val) = lower_bound.float_val();
        else if (val > upper_bound.float_val())
            *((float*)engine_val) = upper_bound.float_val();
        else
            *((float*)engine_val) = val;
        break;
    default:
	//abort();
        break;
    }
}

inline void Param::set_param(CValue val)
{
    matrix_flag = false;
    switch (type)
    {
    case P_TYPE_BOOL:
        assert(val.type == P_TYPE_BOOL || val.type == P_TYPE_INT);
        *(bool *)engine_val = val.bool_val();
        break;
    case P_TYPE_INT:
        assert(val.type == P_TYPE_BOOL || val.type == P_TYPE_INT);
        if (val.int_val() < lower_bound.int_val())
            *((int *) engine_val) = lower_bound.int_val();
        else if (val.int_val() > upper_bound.int_val())
            *((int *) engine_val) = upper_bound.int_val();
        else
            *((int *) engine_val) = val.int_val();
        break;
    case P_TYPE_DOUBLE:
        assert(val.type == P_TYPE_DOUBLE);
        if (val.float_val() < lower_bound.float_val())
            *((float*)engine_val) = lower_bound.float_val();
        else if (val.float_val() > upper_bound.float_val())
            *((float*)engine_val) = upper_bound.float_val();
        else
            *((float*)engine_val) = to_float(val.float_val());
        break;
    default:
        //abort();
        break;
    }
}


class ParamRGBA : public Param
{
public:
    ParamRGBA(std::string name_, Param *r, Param *g, Param *b, Param *a):
            Param(name_, P_TYPE_DOUBLE, P_FLAG_RGB, nullptr, nullptr, CValue(0), CValue(0x00ffffff), CValue(0)),
            param_r(r), param_g(g), param_b(b), param_a(a)
    {
    }

    void set_param( expr_t val ) override;
    void set_param( CValue val ) override;
    void set(expr_t value) override;
    void set_matrix(int mesh_i, int mesh_j, expr_t value) override;
    expr_t eval(int mesh_i, int mesh_j) override;
    LValue *getExpr() override;

private:
    Param *param_r, *param_g, *param_b, *param_a;
};

#endif /** !_PARAM_TYPES_H */
