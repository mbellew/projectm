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
 * Expression
 *
 * $Log$
 */

#ifndef _EXPR_H
#define _EXPR_H

#include "dlldefs.h"
#include "CValue.hpp"
#include <iostream>
#include <vector>
#include <cfloat>
#include <cmath>

class Test;
class Param;


#define USE_DOUBLE_EVAL 1
inline float to_float(double d)
{
    return (float)fmin(FLT_MAX, fmax(-FLT_MAX, d));
}
inline float to_float(float f)
{
    return f;
}
#if USE_DOUBLE_EVAL
    typedef double expr_t;
    inline expr_t expr_value(double d)
    {
        return d;
    }
    inline expr_t expr_value(float f)
    {
        return (double)f;
    }
    double packRGBA(expr_t r, expr_t g, expr_t b, expr_t a=1.0f);
    void unpackRGBA(double RGBA, float &r, float &g, float &b, float &a);
#else
    typedef float expr_t;
    inline expr_t expr_value(double d)
    {
        return to_float(d);
    }
    inline expr_t expr_value(float f)
    {
        return f;
    }
    inline expr_t packRGB(expr_t r, expr_t g, expr_t b, expr_t a=1.0f);
    {
        assert false;
        return 0;       // NOTE SUPPORTED!
    }
    inline void unpackRGBA(float &r, float &g, float &b, float &a)
    {
        r = g = b = a = 0;
    }
#endif


#define CONST_STACK_ELEMENT 0
#define EXPR_STACK_ELEMENT 1

#define EVAL_ERROR -1

/* Infix Operator Function */
class InfixOp
{
public:
  int type;
  int precedence;

  InfixOp( int type, int precedence );
};

/** Term */
class Term
{
public:
  expr_t constant; /* static variable */
  Param *param; /* pointer to a changing variable */

  Term() { this->constant = 0; this->param = 0; }
};
 

enum ExprClass
{
  TREE, CONSTANT, PARAMETER, FUNCTION, ASSIGN, PROGRAM, OTHER
};

class Expr
{
public:
  ExprClass clazz;

  explicit Expr(ExprClass c) : clazz(c) {};
  virtual ~Expr() {};

  virtual bool isConstant() { return false; };
  virtual expr_t eval(int mesh_i, int mesh_j) = 0;
  virtual std::ostream& to_string(std::ostream &out)
  {
      std::cout << "nyi"; return out;
  }

  static Test *test();
  static Expr *const_to_expr( double val );
  static Expr *param_to_expr( Param *param );
  static Expr *prefun_to_expr( expr_t (*func_ptr)(void *), Expr **expr_list, int num_args );

  static void delete_expr(Expr *expr) { if (nullptr != expr) expr->_delete_from_tree(); }
  static Expr *optimize(Expr *root) { return root->_optimize(); };

public: // but don't call these from outside Expr.cpp

  virtual Expr *_optimize() { return this; };

  // override if this expr is not 'owned' by the containg expression tree
  virtual void _delete_from_tree()
  {
    delete this;
  }
};

inline std::ostream& operator<<(std::ostream& out, Expr *expr)
{
  if (NULL == expr)
      out << "NULL";
  else 
      expr->to_string(out);
  return out;
}

/* A binary expression tree ordered by operator precedence */
class TreeExpr : public Expr
{
protected:
  TreeExpr( InfixOp *infix_op, Expr *gen_expr,
                                  TreeExpr *left, TreeExpr *right );
public:
  static TreeExpr *create( InfixOp *infix_op, Expr *gen_expr,
                                  TreeExpr *left, TreeExpr *right );
  InfixOp * infix_op; /* null if leaf */
  Expr * gen_expr;
  // NOTE: before optimize() left and right will always be TreeExpr
  Expr *left, *right;
  // these are for type-safe access in Parser.cpp
  TreeExpr *leftTree()  { return dynamic_cast<TreeExpr *>(left); }
  TreeExpr *rightTree() { return dynamic_cast<TreeExpr *>(right); }

  ~TreeExpr() override;
  
  Expr *_optimize() override;
  expr_t eval(int mesh_i, int mesh_j) override;
  std::ostream& to_string(std::ostream &out) override;
};

/* A function expression in prefix form */
class PrefunExpr : public Expr
{
public:
  expr_t (*func_ptr)(void*);
  int num_args;
  Expr **expr_list;
  PrefunExpr();
  ~PrefunExpr() override;

  Expr *_optimize() override;
  expr_t eval(int mesh_i, int mesh_j) override;
  std::ostream& to_string(std::ostream &out) override;
};

class LValue : public Expr
{
public:
    explicit LValue(ExprClass c) : Expr(c) {};
    virtual void set(expr_t value) = 0;
    virtual void set_matrix(int mesh_i, int mesh_j, expr_t value) = 0;
};


class AssignExpr : public Expr
{
protected:
    LValue *lhs;
    Expr *rhs;
public:
    AssignExpr(LValue *lhs, Expr *rhs);
    ~AssignExpr() override;
    Expr *_optimize() override;
    expr_t eval(int mesh_i, int mesh_j) override;
    std::ostream& to_string(std::ostream &out) override;
};


class AssignMatrixExpr : public AssignExpr
{
public:
    AssignMatrixExpr(LValue *lhs, Expr *rhs);
    expr_t eval(int mesh_i, int mesh_j) override;
    std::ostream& to_string(std::ostream &out) override;
};


class ProgramExpr : public Expr
{
protected:
    std::vector<Expr *> steps;
    bool own;
public:
    ProgramExpr(std::vector<Expr*> &steps_, bool ownSteps) : Expr(PROGRAM), steps(steps_), own(ownSteps)
    {
    }
    ~ProgramExpr()
    {
        if (!own)
            return;
        for (auto it=steps.begin() ; it<steps.end() ; it++)
            Expr::delete_expr(*it);
    }
    expr_t eval(int mesh_i, int mesh_j) override
    {
        float f=0.0f;
        for (auto it=steps.begin() ; it<steps.end() ; it++)
            f = (*it)->eval(mesh_i,mesh_j);
        return f;
    }
};

#endif /** _EXPR_H */
