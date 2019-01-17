/**
 * projectM -- Milkdrop-esque visualisation SDK
 * Copyright (C)2003-2004 projectM Team
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

#include "wipemalloc.h"

#include "Expr.hpp"
#include <cassert>

#include <iostream>
#include "Eval.hpp"
#include "BuiltinFuncs.hpp"



/**
 * NOTE in order to allow color functions/values we could either
 *   a) make eval return a union type
 *   b) special case evaluation of rgb/rgba parameters
 *   c) figure out how to return an RGBA value in a double.
 *
 * I went with c. The idea is to 12 bits for each of R, G, B, A to create a 48bit integer,
 * and then return the computed value integer value as as a double.
 *
 * The scheme can be changed as long as packRGB() and unpackRGB() match.
 */

const int RGB_BITS = 12;
const unsigned RGB_MASK = ((1<<RGB_BITS)-1);
const float RGB_SCALE= ((float)RGB_MASK);

expr_t packRGBA(expr_t r, expr_t g, expr_t b, expr_t a)
{
	unsigned long rgba = 0;
	rgba += (unsigned)(RGB_SCALE * fmax(0, fmin(1.0, r)));
	rgba <<= RGB_BITS;
	rgba += (unsigned)(RGB_SCALE * fmax(0, fmin(1.0, g)));
	rgba <<= RGB_BITS;
	rgba += (unsigned)(RGB_SCALE * fmax(0, fmin(1.0, b)));
	rgba <<= RGB_BITS;
	rgba += (unsigned)(RGB_SCALE * fmax(0, fmin(1.0, a)));
	return (double)rgba;
}

void unpackRGBA(double RGBA, float &r, float &g, float &b, float &a)
{
	// extract 48 bit unsigned int
	auto rgba = (uint64_t)fmax(0, fmin((double)0xfffffffffffful, RGBA));
	// extract 4 12-bit integers and scale 0.0-1.0
	a = (rgba & RGB_MASK) / RGB_SCALE;
	rgba >>= RGB_BITS;
	b = (rgba & RGB_MASK) / RGB_SCALE;
	rgba >>= RGB_BITS;
	g = (rgba & RGB_MASK) / RGB_SCALE;
	rgba >>= RGB_BITS;
	r = (rgba & RGB_MASK) / RGB_SCALE;
}


/* Evaluates functions in prefix form */
expr_t PrefunExpr::eval ( int mesh_i, int mesh_j )
{
	assert ( func_ptr );
    expr_t arg_list_stk[10];

    expr_t * arg_list;
    expr_t * argp;
	Expr **expr_listp = expr_list;


	if (this->num_args > 10) {
		arg_list = new expr_t[this->num_args];
	} else {
		arg_list = arg_list_stk;
	}
	argp = arg_list;

	assert(arg_list);

	//printf("numargs %d", num_args);

	/* Evaluate each argument before calling the function itself */
	for ( int i = 0; i < num_args; i++ )
	{
		*(argp++) = (*(expr_listp++))->eval ( mesh_i, mesh_j );
		//printf("numargs %x", arg_list[i]);
	}
	/* Now we call the function, passing a list of
	   floats as its argument */

	const expr_t value = ( func_ptr ) ( arg_list );

	if (arg_list != arg_list_stk) {
		delete[](arg_list);
	}
	return value;
}


class PrefunExprOne : public PrefunExpr
{
    expr_t eval ( int mesh_i, int mesh_j ) override
	{
        expr_t val = expr_list[0]->eval ( mesh_i, mesh_j );
		expr_t ret = (func_ptr)(&val);
		if (ret > 100)
		    ret = (func_ptr)(&val);
		return ret;
	}
};

class SinExpr : public PrefunExpr
{
	expr_t eval ( int mesh_i, int mesh_j ) override
	{
		expr_t val = expr_list[0]->eval ( mesh_i, mesh_j );
		return sinf(val);
	}
};

class CosExpr : public PrefunExpr
{
	expr_t eval ( int mesh_i, int mesh_j ) override
	{
		expr_t val = expr_list[0]->eval ( mesh_i, mesh_j );
		return cosf(val);
	}
};

class LogExpr : public PrefunExpr
{
	expr_t eval ( int mesh_i, int mesh_j ) override
    {
		expr_t val = expr_list[0]->eval ( mesh_i, mesh_j );
        return logf(val);
    }
};

class PowExpr : public PrefunExpr
{
	expr_t eval ( int mesh_i, int mesh_j ) override
    {
		expr_t x = expr_list[0]->eval ( mesh_i, mesh_j );
		expr_t y = expr_list[1]->eval ( mesh_i, mesh_j );
        return powf(x, y);
    }
};

class ConstantExpr : public Expr
{
	expr_t constant;
public:
	ConstantExpr( double value ) : Expr(CONSTANT), constant(value) {}
	ConstantExpr( int type, Term *term ) : Expr(CONSTANT), constant(term->constant) {}
	bool isConstant() override
	{
		return true; 
	}
    expr_t eval(int mesh_i, int mesh_j ) override
	{ 
		return constant; 
	}
	std::ostream &to_string(std::ostream &out) override
	{
		out << constant; return out;
	}
};


class MultAndAddExpr : public Expr
{
	Expr *a, *b, *c;
public:
	MultAndAddExpr(Expr *_a, Expr *_b, Expr *_c) : Expr(OTHER),
		a(_a), b(_b), c(_c)
	{
	}
    ~MultAndAddExpr() override {
        Expr::delete_expr(a);
        Expr::delete_expr(b);
        Expr::delete_expr(c);
    }
    expr_t eval(int mesh_i, int mesh_j) override
	{
        expr_t a_value = a->eval(mesh_i,mesh_j);
        expr_t b_value = b->eval(mesh_i,mesh_j);
        expr_t c_value = c->eval(mesh_i,mesh_j);
		return a_value * b_value + c_value;
	}
	std::ostream &to_string(std::ostream &out) override
	{
		out << "(" << a << " * " << b << ") + " << c;
		return out;
	}
};

class MultConstExpr : public Expr
{
    Expr *expr;
    expr_t c;
public:
    MultConstExpr(Expr *_expr, expr_t _c) : Expr(OTHER),
        expr(_expr), c(_c)
    {
    }
    ~MultConstExpr() override
    {
        Expr::delete_expr(expr);
    }
	expr_t eval(int mesh_i, int mesh_j) override
    {
        expr_t value = expr->eval(mesh_i,mesh_j);
        return value * c;
    }
    std::ostream &to_string(std::ostream &out) override
    {
        out << "(" << expr << " * " << c << ") + " << c;
        return out;
    }
};

std::ostream &TreeExpr::to_string(std::ostream &out)
{
	if (NULL == infix_op)
	{
		out << gen_expr;
	}
	else
	{
		out << "(" << left << " ";
		switch ( infix_op->type )
		{
		case INFIX_ADD:
			out << "+"; break;
		case INFIX_MINUS:
			out << "-"; break;
		case INFIX_MULT:
			out << "*"; break;
		case INFIX_MOD:
			out << "%"; break;
		case INFIX_OR:
			out << "|"; break;
		case INFIX_AND:
			out << "&"; break;
		case INFIX_DIV:
			out << "/"; break;
		default:
			out << "infix_op_ERROR"; break;
		}
		out << " " << right << ")";
	}
	return out;
}

/* NOTE: Parser.cpp directly manipulates TreeExpr, so it is easier to optimize AFTER parsing
 * than while building up the tree initially 
 */
Expr *TreeExpr::_optimize()
{
	if (infix_op == NULL)
	{
		Expr *opt = gen_expr->_optimize();
		if (opt != gen_expr)
			Expr::delete_expr(gen_expr);
		gen_expr = NULL;
		return opt;
	}
	if (left != NULL)
	{
		Expr *l = left->_optimize();
		if (l != left)
		    Expr::delete_expr(left);
		left = l;
	}
	if (right != NULL)
	{
		Expr *r = right->_optimize();
		if (r != right)
			Expr::delete_expr(right);
		right = r;
	}
	if (left == NULL)
	{
		Expr *opt = right;
		right = NULL;
		return opt;
	}
	if (right == NULL)
	{
		Expr *opt = left;
		left = NULL;
		return opt;
	}
	if (left->isConstant() && right->isConstant())
		return Expr::const_to_expr(eval(-1, -1));

	// this is gratuitious, but a*b+c is super common, so as proof-of-concept, let's make a special Expr
	if (infix_op->type == INFIX_ADD && 
		((left->clazz == TREE && ((TreeExpr *)left)->infix_op->type == INFIX_MULT) ||
		(right->clazz == TREE && ((TreeExpr *)right)->infix_op->type == INFIX_MULT)))
	{
		Expr *a, *b, *c;
		if (left->clazz == TREE && ((TreeExpr *)left)->infix_op->type == INFIX_MULT)
		    std::swap(left,right);
        a = ((TreeExpr *)right)->left;
        b = ((TreeExpr *)right)->right;
        c = left;
        ((TreeExpr *)right)->left = NULL;
        ((TreeExpr *)right)->right = NULL;
        left = NULL;
		return new MultAndAddExpr(a,b,c);
	}

	if (infix_op->type == INFIX_MULT && (left->isConstant() || right->isConstant()))
    {
	    if (right->isConstant())
	        std::swap(left, right);
        float c = left->eval(-1,-1);
        Expr *expr = right;
        right = left = nullptr;
        return new MultConstExpr(expr, c);
    }

	return this;
}

/* Evaluates an expression tree */
expr_t TreeExpr::eval ( int mesh_i, int mesh_j )
{
    expr_t left_arg, right_arg;

	/* shouldn't be null if we've called _optimize() */
	assert(NULL != infix_op);

	left_arg = left->eval ( mesh_i, mesh_j );
	right_arg = right->eval ( mesh_i, mesh_j );

	switch ( infix_op->type )
	{
		case INFIX_ADD:
			return ( left_arg + right_arg );
		case INFIX_MINUS:
			return ( left_arg - right_arg );
		case INFIX_MULT:
			return ( left_arg * right_arg );
		case INFIX_MOD:
			if ( ( int ) right_arg == 0 )
			{
				return PROJECTM_DIV_BY_ZERO;
			}
			return ( ( int ) left_arg % ( int ) right_arg );
		case INFIX_OR:
			return ( ( int ) left_arg | ( int ) right_arg );
		case INFIX_AND:
			return ( ( int ) left_arg & ( int ) right_arg );
		case INFIX_DIV:
			if ( right_arg == 0 )
			{
				return MAX_DOUBLE_SIZE;
			}
			return ( left_arg / right_arg );
		default:
			return EVAL_ERROR;
	}

	return EVAL_ERROR;
}

/* Converts a float value to a general expression */
Expr * Expr::const_to_expr ( double val )
{
	Term term;
	term.constant = val;
	return new ConstantExpr( CONSTANT_TERM_T, &term );
}

/* Converts a regular parameter to an expression */
Expr * Expr::param_to_expr ( Param * param )
{
	if ( param == NULL )
		return NULL;

	switch ( param->type )
	{
		case P_TYPE_BOOL:
		    return param->getExpr();
			//return new BoolParameterExpr( PARAM_TERM_T, &term );
		case P_TYPE_INT:
            return param->getExpr();
			//return new IntParameterExpr( PARAM_TERM_T, &term );
		case P_TYPE_DOUBLE:
            return param->getExpr();
            //return new FloatParameterExpr( PARAM_TERM_T, &term );
		default:
			return NULL;
	}
}

/* Converts a prefix function to an expression */
Expr * Expr::prefun_to_expr ( expr_t (*func_ptr)(void *), Expr ** expr_list, int num_args )
{
    PrefunExpr *prefun_expr;
    if (num_args == 1)
    {
        if (func_ptr == (expr_t (*)(void *)) FuncWrappers::sin_wrapper)
            prefun_expr = new SinExpr();
        else if (func_ptr == (expr_t (*)(void *)) FuncWrappers::cos_wrapper)
            prefun_expr = new CosExpr();
        else if (func_ptr == (expr_t (*)(void *)) FuncWrappers::log_wrapper)
            prefun_expr = new LogExpr();
        else
            prefun_expr = new PrefunExprOne();
    }
    else if (num_args == 2)
    {
        if (func_ptr == (expr_t (*)(void *)) FuncWrappers::pow_wrapper)
            prefun_expr = new PowExpr();
        else
            prefun_expr = new PrefunExpr();
    }
    else
    {
        prefun_expr = new PrefunExpr();
    }

	prefun_expr->num_args = num_args;
	prefun_expr->func_ptr = ( expr_t ( * ) ( void* ) ) func_ptr;
	prefun_expr->expr_list = expr_list;
	return prefun_expr;
}

/* Creates a new tree expression */
TreeExpr::TreeExpr ( InfixOp * _infix_op, Expr * _gen_expr, TreeExpr * _left, TreeExpr * _right ) :
		Expr( TREE ),
		infix_op ( _infix_op ), gen_expr ( _gen_expr ),
	left ( _left ), right ( _right ) {}

class TreeExprAdd : public TreeExpr
{
public:
	TreeExprAdd( InfixOp * _infix_op, Expr * _gen_expr, TreeExpr * _left, TreeExpr * _right ) :
	 	TreeExpr( _infix_op, _gen_expr, _left, _right) {}
    expr_t eval( int mesh_i, int mesh_j)
	{
		return left->eval(mesh_i, mesh_j) + right->eval(mesh_i, mesh_j);
	}
};

class TreeExprMinus : public TreeExpr
{
public:
	TreeExprMinus( InfixOp * _infix_op, Expr * _gen_expr, TreeExpr * _left, TreeExpr * _right ) :
	 	TreeExpr( _infix_op, _gen_expr, _left, _right) {}
    expr_t eval( int mesh_i, int mesh_j)
	{
		return left->eval(mesh_i, mesh_j) - right->eval(mesh_i, mesh_j);
	}
};

class TreeExprMult : public TreeExpr
{
public:
	TreeExprMult( InfixOp * _infix_op, Expr * _gen_expr, TreeExpr * _left, TreeExpr * _right ) :
	 	TreeExpr( _infix_op, _gen_expr, _left, _right) {}
    expr_t eval( int mesh_i, int mesh_j)
	{
	    expr_t l=left->eval(mesh_i, mesh_j);
	    expr_t r=right->eval(mesh_i, mesh_j);
	    expr_t ret = l*r;
	    return ret;
	}
};

TreeExpr * TreeExpr::create( InfixOp * _infix_op, Expr * _gen_expr, TreeExpr * _left, TreeExpr * _right )
{
	if ( NULL != _infix_op )
	{
		if ( _infix_op->type == INFIX_ADD )
			return new TreeExprAdd( _infix_op, _gen_expr, _left, _right);
		if ( _infix_op->type == INFIX_MINUS )
			return new TreeExprMinus( _infix_op, _gen_expr, _left, _right);
		if ( _infix_op->type == INFIX_MULT )
			return new TreeExprMult( _infix_op, _gen_expr, _left, _right);
	}
	return new TreeExpr( _infix_op, _gen_expr, _left, _right );
}

/* Frees a function in prefix notation */
PrefunExpr::~PrefunExpr()
{
	int i;

	/* Free every element in expression list */
	for ( i = 0 ; i < num_args; i++ )
	{
		Expr::delete_expr(expr_list[i]);
	}
	free ( expr_list );
}

/* Frees a tree expression */
TreeExpr::~TreeExpr()
{

	/* free left tree */
	if ( left != NULL )
	{
		Expr::delete_expr(left);
	}

	/* free general expression object */
	if ( gen_expr != NULL )
	{
		Expr::delete_expr(gen_expr);
	}

	/* Note that infix operators are always
	   stored in memory unless the program
	   exits, so we don't remove them here */

	/* free right tree */
	if ( right != NULL )
	{
		Expr::delete_expr(right);
	}
}

/* Initializes an infix operator */
InfixOp::InfixOp ( int _type, int _precedence )
{
	this->type = _type;
	this->precedence = _precedence;
}

PrefunExpr::PrefunExpr() : Expr(FUNCTION)
{
}

bool isConstantFn(expr_t (* fn)(void*))
{
    return (expr_t (*)(expr_t *))fn != FuncWrappers::print_wrapper &&
           (expr_t (*)(expr_t *))fn != FuncWrappers::rand_wrapper;
}

Expr *PrefunExpr::_optimize()
{
	bool constant_args = true;
	for (int i=0 ; i < num_args ; i++)
	{
		Expr *orig = expr_list[i];
		expr_list[i] = orig->_optimize();
		if (orig != expr_list[i])
            Expr::delete_expr(orig);
		constant_args = constant_args && expr_list[i]->isConstant();
	}
	if (constant_args && isConstantFn(func_ptr))
    {
        return Expr::const_to_expr(eval(-1, -1));
    }
	return this;
}

std::ostream& PrefunExpr::to_string(std::ostream& out)
{
	char comma = ' ';
	out << "<function>(";
	for (int i=0 ; i < num_args ; i++)
	{
		out << comma;
		out << expr_list[i];
		comma = ',';
	}
	out << ")";
	return out;
}


AssignExpr::AssignExpr(LValue *lhs_, Expr *rhs_) : Expr(ASSIGN), lhs(lhs_), rhs(rhs_) {}

AssignExpr::~AssignExpr()
{
    Expr::delete_expr(lhs);
    Expr::delete_expr(rhs);
}

Expr * AssignExpr::_optimize()
{
    Expr *t = rhs->_optimize();
    if (t != rhs)
        Expr::delete_expr(rhs);
    rhs = t;
    return this;
}

expr_t AssignExpr::eval(int mesh_i, int mesh_j)
{
	expr_t v = rhs->eval( mesh_i, mesh_j );
    lhs->set( v );
    return v;
}

std::ostream& AssignExpr::to_string(std::ostream &out)
{
	out << lhs << " = " << rhs;
	return out;
}

AssignMatrixExpr::AssignMatrixExpr(LValue *lhs_, Expr *rhs_) : AssignExpr(lhs_, rhs_) {}

expr_t AssignMatrixExpr::eval(int mesh_i, int mesh_j)
{
	expr_t v = rhs->eval( mesh_i, mesh_j );
	lhs->set_matrix( mesh_i, mesh_j, v );
	return v;
}

std::ostream& AssignMatrixExpr::to_string(std::ostream &out)
{
	out << lhs << "[i,j] = " << rhs;
	return out;
}





// TESTS


#include <TestRunner.hpp>

#ifndef NDEBUG

#define TEST(cond) if (!verify(#cond,cond)) return false


struct ExprTest : public Test
{
	ExprTest() : Test("ExprTest")
	{}

public:
    bool optimize_constant_expr()
    {
        TreeExpr *a = TreeExpr::create(nullptr, Expr::const_to_expr( 1.0 ), nullptr, nullptr);
        TreeExpr *b = TreeExpr::create(nullptr, Expr::const_to_expr( 2.0 ), nullptr, nullptr);
	    Expr *c = TreeExpr::create(Eval::infix_add, nullptr, a, b);
        //TEST(3.0f == c->eval(-1,-1));
        Expr *x = Expr::optimize(c);
        TEST(x != c);
		Expr::delete_expr(c);
        TEST(x->clazz == CONSTANT);
        TEST(3.0f == x->eval(-1,-1));
		Expr::delete_expr(x);

        Expr **expr_array = (Expr **)malloc(sizeof(Expr *));
        expr_array[0] = TreeExpr::create(nullptr, Expr::const_to_expr( M_PI ), nullptr, nullptr);
        Expr *sin = Expr::prefun_to_expr((expr_t (*)(void *))FuncWrappers::sin_wrapper, expr_array, 1);
        x = Expr::optimize(sin);
        TEST(x != sin);
        Expr::delete_expr( sin );
        TEST(x->clazz == CONSTANT);
        TEST(sinf( M_PI ) == x->eval(-1,-10));
        Expr::delete_expr(x);

        // make sure rand() is not optimized away
        expr_array = (Expr **)malloc(sizeof(Expr *));
		expr_array[0] = TreeExpr::create(nullptr, Expr::const_to_expr( M_PI ), nullptr, nullptr);
		Expr *rand = Expr::prefun_to_expr((expr_t (*)(void *))FuncWrappers::rand_wrapper, expr_array, 1);
		x = Expr::optimize(rand);
		TEST(x == rand);
		TEST(x->clazz != CONSTANT);
		Expr::delete_expr(x);

        return true;
    }

	bool test() override
	{
        Eval::init_infix_ops();
	    bool result = true;
	    result &= optimize_constant_expr();
		return result;
	}
};

Test* Expr::test()
{
	return new ExprTest();
}

#else

Test* Expr::test()
{
    return null;
}

#endif