#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP cdtnorm(SEXP y, SEXP in_mu, SEXP in_sigma, SEXP in_left, SEXP in_right, SEXP in_give_log)
{
    int i, n = length(y);
  
    // Return vector SEXP
    SEXP rval = PROTECT(allocVector(REALSXP, n));
  
    // Initialize pointers for return value and y
    double *rvalptr = REAL(rval);
    double *yptr    = REAL(y);
  
    // Initialize single-numeric/logical inputs
    int    give_log = asLogical(in_give_log);
    double left     = asReal(in_left);
    double right    = asReal(in_right);
    double mu       = asReal(in_mu);
    double sigma    = asReal(in_sigma);

    // Constant values as we have constant right/left/mu/sigma
    double denom = pnorm5((right - mu) / sigma, 0.0, 1.0, 1, 0) -
                   pnorm5((left - mu) / sigma, 0.0, 1.0, 1,0);
    double logsigmadenom = log(sigma * denom);

    for ( i = 0; i < n; i++ ) {
        // y is truncated 
        if ( (yptr[i] < left) | (yptr[i] > right) ) {
            if ( give_log == 0 ) {
                rvalptr[i] = 0.0;
            } else {
                rvalptr[i] = log(0.0);
            }
        // Non-truncated values of y
        } else {
            if ( give_log == 0 ) {
                rvalptr[i] = dnorm((yptr[i] - mu) / sigma, 0.0, 1.0, 0) / sigma / denom; 
            } else {
                rvalptr[i] = dnorm((yptr[i] - mu) / sigma, 0.0, 1.0, 1) - logsigmadenom;
            }
        }
    }

    UNPROTECT(1);
    return rval;
}


SEXP cptnorm(SEXP q, SEXP in_mu, SEXP in_sigma, SEXP in_left, SEXP in_right,
             SEXP in_lower_tail, SEXP in_log_p)
{
    int i, n = length(q);
  
    // Return vector SEXP
    SEXP rval = PROTECT(allocVector(REALSXP, n));
  
    // Initialize pointers for return value and y
    double *rvalptr   = REAL(rval);
    double *qptr      = REAL(q);
  
    // Initialize single-numeric/logical inputs
    double left       = asReal(in_left);
    double right      = asReal(in_right);
    double mu         = asReal(in_mu);
    double sigma      = asReal(in_sigma);
    int    lower_tail = asLogical(in_lower_tail);
    int    log_p      = asLogical(in_log_p);

    double qtmp, denom;
    
    if(lower_tail == 1) {
        if(log_p == 1) {
            for(i = 0; i < n; i++) {
                if(qptr[i] < left) {
                    rvalptr[i] = log(0);
                } else {
                    if(qptr[i] >= right){
                        rvalptr[i] = 0;
                    } else {
                        qtmp = -(pnorm5((left - mu)/sigma, 0.0, 1.0, 1, 0) - 
                          pnorm5((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                        denom = pnorm5((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                          pnorm5((left - mu)/sigma, 0.0, 1.0, 1,0);
                        rvalptr[i] = log(qtmp) - log(denom); 
                    }
                } 
            }
        } else {      
            for(i = 0; i < n; i++) { 
                if(qptr[i] < left) {
                    rvalptr[i] = 0;
                } else {
                    if(qptr[i] >= right){
                        rvalptr[i] = 1;
                    } else {
                        qtmp = -(pnorm5((left - mu)/sigma, 0.0, 1.0, 1, 0) - 
                          pnorm5((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                        denom = pnorm5((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                          pnorm5((left - mu)/sigma, 0.0, 1.0, 1,0);
                        rvalptr[i] = qtmp/denom;
                    }
                }
            }
        }
    } else {
        if(log_p == 1) {
            for(i = 0; i < n; i++) {
                if(qptr[i] < left) {
                    rvalptr[i] = 0;
                } else {
                    if(qptr[i] >= right){
                      rvalptr[i] = log(0);
                    } else {
                        qtmp  = (pnorm5((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                                pnorm5((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                        denom = pnorm5((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                                pnorm5((left - mu)/sigma, 0.0, 1.0, 1,0);
                        rvalptr[i] = log(qtmp) - log(denom); 
                    }
                } 
            }
        } else {      
            for(i = 0; i < n; i++) { 
                if(qptr[i] < left) {
                    rvalptr[i] = 1;
                } else {
                    if(qptr[i] >= right){
                        rvalptr[i] = 0;
                    } else {
                        qtmp  = (pnorm5((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                                pnorm5((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                        denom = pnorm5((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                                pnorm5((left - mu)/sigma, 0.0, 1.0, 1,0);
                        rvalptr[i] = qtmp/denom;
                    }
                }
            }
        }
    }

    UNPROTECT(1);
    return rval;
}


