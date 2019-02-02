#ifndef _vectorclass
#define _vectorclass

#include <cmath>
#include <iostream>

using namespace  std;

class Point;
class Vector;
class Matrix;

// Point Class

class Point
{
    private:
        int     dimension;
        double  *data;
    public:
        Point(int dim);
        Point(const Point& v);
        ~Point();
    int         Dimension() const;
// User Defined Operators
    int         operator==(const Point& v) const;
    int         operator!=(const Point& v) const;
    Point&      operator=(const Point& v);
    double      operator()(const int i) const;
    double&     operator()(const int i);
    void        Print() const;
};

// Vector Class

class Vector
{
    private:
        int dimension;
        double *data;
  
    public:
        Vector();
        Vector(int dim);
        Vector(const Vector& v);
        Vector(int col, const Matrix &A);
        ~Vector();
  
    void    Initialize(int dim);
    int     Dimension() const;
    double  Length();     /* Euclidean Norm of the Vector */
    void    Normalize();
    double  Norm_l1();
    double  Norm_l2();
    double  Norm_linf();
    double  MaxMod();
    double  ElementofMaxMod();
    int     MaxModindex();
  
  // User Defined Operators

    int     operator==(const Vector& v) const;
    int     operator!=(const Vector& v) const;
    Vector& operator=(const Vector& v);
    double  operator()(const int i) const;
    double& operator()(const int i);
    void    Print() const;
    void    Initialize(double a);
    void    Initialize(double *v);
};