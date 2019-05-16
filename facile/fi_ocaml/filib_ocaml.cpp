#include <iostream>
#include <Interval.h>
#define FILIB_NAMESPACES
#define FILIB_EXTENDED
#define FI filib

extern "C"{
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
}
#define INTV_PTR(x) ((FI::Interval *) &Field(x,0))
#define INTV_VAL(x) (* INTV_PTR(x))
#define ALLOC_FLOAT caml_alloc_small(sizeof(double), Double_tag)
#define ALLOC_INTV caml_alloc(sizeof(FI::Interval),Abstract_tag)
#define TO_FLOATREF(x) ((double*)&Double_field(x, 0))
#define TO_FLOATV(x) (*(TO_FLOATREF(x)))

#define OPIII(nom,op_fi)\
  extern "C"{\
    value nom (value v1,value v2){		\
  CAMLparam2(v1,v2);\
  CAMLlocal1(res);\
  res =ALLOC_INTV;\
  INTV_VAL(res)= INTV_VAL(v1) op_fi INTV_VAL(v2);\
  CAMLreturn(res);}}\

#define FUNIII(nom,fun_fi) \
extern "C"\
value nom (value v1,value v2){\
  CAMLparam2(v1,v2);\
  CAMLlocal1(res);\
  res =ALLOC_INTV;\
  INTV_VAL(res)= FI::fun_fi (INTV_VAL(v1), INTV_VAL(v2));	\
  CAMLreturn(res);\
}

#define FUNIIb(nom,fun_fi)\
extern "C"\
value nom(value v1,value v2){\
  CAMLparam2(v1,v2);\
  CAMLlocal1(res);\
  res = Val_bool(FI::fun_fi(INTV_VAL(v1),INTV_VAL(v2)));\
  CAMLreturn(res);\
}

#define FUNIIf(nom,fun_fi)\
extern "C"\
value nom(value v1,value v2){\
  CAMLparam2(v1,v2);	     \
  CAMLlocal1(res);	     \
  res = ALLOC_FLOAT;\
  Double_val(res)=FI::fun_fi(INTV_VAL(v1),INTV_VAL(v2));\
  CAMLreturn(res);\
}
#define FUNII(nom,fun_fi)\
extern "C"\
value nom (value v){\
  CAMLparam1(v);\
  CAMLlocal1(res);\
  res =ALLOC_INTV;\
  INTV_VAL(res)= FI::fun_fi (INTV_VAL(v));	\
  CAMLreturn(res);\
}

#define FUNIf(nom,fun_fi)\
extern "C"\
value nom (value v){\
  CAMLparam1(v);\
  CAMLlocal1(res);\
  res = ALLOC_FLOAT;\
  Double_val(res)= FI::fun_fi(INTV_VAL(v));	\
   CAMLreturn(res);\
}

#define VALUEI(nom,nomcpp,comcpp)\
static FI::Interval nomcpp = nomcpp.comcpp();	\
extern "C"					\
value nom(){\
  CAMLparam0();\
  CAMLlocal1(res);\
  res = ALLOC_INTV;\
  INTV_VAL(res)=nomcpp;\
  CAMLreturn(res);\
}

#define FUNIb(nom,fun_fi)\
extern "C"					\
value nom (value v){				\
  CAMLparam1(v);				\
  CAMLlocal1(b);				\
  b= Val_bool(INTV_VAL(v).fun_fi());		\
  CAMLreturn(b);				\
}

//---------------------------------------------
//                   VALUES
//---------------------------------------------

VALUEI(entire_,entire,ENTIRE);
VALUEI(empty_,empty,EMPTY);
VALUEI(pi_,pi,PI);
VALUEI(neg_infty_,neg_infty,NEG_INFTY);
VALUEI(pos_infty_,pos_infty,POS_INFTY);
VALUEI(zero_,zero,ZERO);
VALUEI(one_,one,ONE);

//---------------------------------------------
//                  CREATION     
//---------------------------------------------


extern "C" 
value create_ (value x,value y){
  CAMLparam2(x,y);
  CAMLlocal1 (res);
  FI::Interval c(Double_val(x),Double_val(y));
  res = ALLOC_INTV;
  INTV_VAL(res) = c;
  CAMLreturn (res);
}

extern "C" value inf_sup_ (value v){
  CAMLparam1(v);
  CAMLlocal1(res);
  res= caml_alloc_small(2*sizeof(double),Double_array_tag);
  Double_field(res,0) =INTV_VAL(v).inf();
  Double_field(res,1) =INTV_VAL(v).sup();
  CAMLreturn(res);
}

//---------------------------------------------
//          ARITHMETIC OPERATORS
//---------------------------------------------

OPIII(addIII_, + );
OPIII(subIII_, - );
OPIII(mulIII_, * );
OPIII(divIII_, / );

extern "C"
value negII_ (value v){
  CAMLparam1(v);
  CAMLlocal1(res);
  res =ALLOC_INTV;
  INTV_VAL(res)= - INTV_VAL(v);		\
  CAMLreturn(res);\
}

//---------------------------------------------
//          ACCESS AND INFORMATION
//---------------------------------------------

FUNIb(is_empty_,isEmpty);
FUNIb(is_point_,isPoint);
FUNIb(is_infinite_,isInfinite)
FUNIf(inf_,inf);
FUNIf(sup_,sup);
FUNIf(mid_,mid);
FUNIf(diam_,diam);
FUNIf(relDiam_,relDiam);
FUNIf(rad_,rad);
FUNIf(mig_,mig);
FUNIf(mag_,mag);
FUNII(absII_,abs);
FUNIIf(dist_,dist);

extern "C" value in_(value d1,value i1){
  CAMLparam2(d1,i1);
  CAMLlocal1(res);
  res=Val_bool(FI::in(d1,i1));
  CAMLreturn(res);
}

//---------------------------------------------
//          SET THEORETIC FUNCTIONS
//---------------------------------------------
FUNIII(iminIII_,imin);
FUNIII(imaxIII_,imax);

FUNIII(intersectIII_,intersect);
FUNIII(hullIII_,hull);
FUNIIb(disjoint_,disjoint);
FUNIIb(interior_,interior);
FUNIIb(proper_subset_,proper_subset);
FUNIIb(subset_,subset);
FUNIIb(proper_superset_,proper_superset);
FUNIIb(superset_,superset);

//---------------------------------------------
//       INTERVAL RELATIONAL FUNCTIONS
//---------------------------------------------

// Set Relational Functions
FUNIIb(seq_,seq);
FUNIIb(sne_,sne);
FUNIIb(sge_,sge);
FUNIIb(sgt_,sgt);
FUNIIb(sle_,sle);
FUNIIb(slt_,slt);


// Certainly Relational Functions
FUNIIb(ceq_,ceq);
FUNIIb(cne_,cne);
FUNIIb(cge_,cge);
FUNIIb(cgt_,cgt);
FUNIIb(cle_,cle);
FUNIIb(clt_,clt);

//Possibly relational Functions
FUNIIb(peq_,peq);
FUNIIb(pne_,pne);
FUNIIb(pge_,pge);
FUNIIb(pgt_,pgt);
FUNIIb(ple_,ple);
FUNIIb(plt_,plt);
//---------------------------------------------
//       ELEMENTARY FUNCTIONS
//---------------------------------------------

FUNII(acosII_,acos);
FUNII(acoshII_,acosh);
FUNII(acotII_,acot);
FUNII(acothII_,acoth);
FUNII(asinII_,asin);
FUNII(asinhII_,asinh);
FUNII(atanII_,atan);
FUNII(atanhII_,atanh);
FUNII(cosII_,cos);
FUNII(coshII_,cosh);
FUNII(cotII_,cot);
FUNII(cothII_,coth);

FUNII(expII_,exp);
FUNII(exp10II_,exp10);
FUNII(exp2II_,exp2);
FUNII(logII_,log);
FUNII(log10II_,log10);
FUNII(log2II_,log2);
FUNII(log1pII_,log1p);

FUNIII(powIII_,pow);
extern "C" 
value powII_ (value v1,value v2){
  CAMLparam2(v1,v2);
  CAMLlocal1(res);
  res = ALLOC_INTV;
  INTV_VAL(res) = FI::power(INTV_VAL(v1),Int_val(v2));
  CAMLreturn(res);
}

FUNII(sinII_,sin);
FUNII(sinhII_,sinh);

FUNII(sqrII_,sqr);
FUNII(sqrtII_,sqrt);

FUNII(tanII_,tan);
FUNII(tanhII_,tanh);

