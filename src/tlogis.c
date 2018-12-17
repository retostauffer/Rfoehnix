#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP cdtlogis(SEXP y, SEXP in_mu, SEXP in_sigma, SEXP in_left, SEXP in_right, SEXP in_give_log)
{
    int i, n = length(y);
  
    // Return vector SEXP
    SEXP rval = PROTECT(allocVector(REALSXP, n));
  
    // Initialize pointers for return value and y
    double *rvalptr = REAL(rval);
    double *yptr = REAL(y);
  
    // Initialize single-numeric/logical inputs
    int    give_log = asLogical(in_give_log);
    double left     = asReal(in_left);
    double right    = asReal(in_right);
    double mu       = asReal(in_mu);
    double sigma    = asReal(in_sigma);

    // Calculate once, is a constant as we have constant values
    // for left/right/mu/sigma
    double denom = plogis((right - mu) / sigma, 0.0, 1.0, 1, 0) -
                   plogis((left - mu) / sigma, 0.0, 1.0, 1, 0);
    double sigmadenom    = sigma * denom;
    double logsigmadenom = log(sigmadenom);

    for(i = 0; i < n; i++) {
        // If truncated
        if ( (yptr[i] < left) | (yptr[i] > right) ) {
            if ( give_log == 0 ) {
                rvalptr[i] = 0.;
            } else {
                rvalptr[i] = log(0.);
            }
        // No truncation
        } else {
            if ( give_log == 0 ) {
                rvalptr[i] = dlogis((yptr[i] - mu) / sigma, 0.0, 1.0, 0) / sigmadenom; 
            } else {
                rvalptr[i] = dlogis((yptr[i] - mu) / sigma, 0.0, 1.0, 1) - logsigmadenom;
            }
        }
    }


    UNPROTECT(1);
    return rval;
}


SEXP cptlogis(SEXP q, SEXP in_mu, SEXP in_sigma, SEXP in_left, SEXP in_right,
              SEXP in_lower_tail, SEXP in_log_p)
{
    int i, n = length(q);

    SEXP rval = PROTECT(allocVector(REALSXP, n));

    // Return vector SEXP
    double *rvalptr = REAL(rval);
    double *qptr    = REAL(q);

    // Initialize pointers for return value and y
    double mu      = asReal(in_mu);
    double sigma   = asReal(in_sigma);
    double left    = asReal(in_left);
    double right   = asReal(in_right);
    int lower_tail = asLogical(in_lower_tail);
    int log_p      = asLogical(in_log_p);

    // Constant as we have constant left/right/mu/sigma.
    double denom = plogis((right - mu) / sigma, 0.0, 1.0, 1, 0) - 
                   plogis((left - mu) / sigma, 0.0, 1.0, 1,0);
    double logdenom = log(denom);

    double qtmp;
    
    // TODO: check if the qptr < left and qptr >= right 
    // are correct in Jakobs c code! Just a feeling ...
    // Evaluating lower tail
    if ( lower_tail == 1 ) {
        if ( log_p == 1 ) {
            for ( i = 0; i < n; i++ ) {
                // q below left truncation point
                if ( qptr[i] < left ) {
                    rvalptr[i] = log(0);
                // q on or above right truncation point
                } else if ( qptr[i] >= right ) {
                    rvalptr[i] = 0;
                // No truncation
                } else {
                    qtmp  = -(plogis((left - mu)/sigma, 0.0, 1.0, 1, 0) - 
                            plogis((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                    rvalptr[i] = log(qtmp) - logdenom; 
                } 
            }
        } else {      
            for ( i = 0; i < n; i++ ) { 
                // q below left truncation point
                if ( qptr[i] < left ) {
                  rvalptr[i] = 0;
                // q on or above right truncation point
                } else if ( qptr[i] >= right ) {
                    rvalptr[i] = 1;
                // No truncation
                } else {
                    qtmp  = -(plogis((left - mu)/sigma, 0.0, 1.0, 1, 0) - 
                            plogis((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                    rvalptr[i] = qtmp / denom;
                }
            }
        }
    // Evaluating upper tail
    } else {
        if ( log_p == 1 ) {
            for ( i = 0; i < n; i++ ) {
                // q below left truncation point
                if ( qptr[i] < left ) {
                    rvalptr[i] = 0;
                // q on or above right truncation point
                } else if ( qptr[i] >= right ) {
                    rvalptr[i] = log(0);
                // No truncation
                } else {
                    qtmp  = (plogis((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                            plogis((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                    rvalptr[i] = log(qtmp) - logdenom; 
                } 
            }
        } else {      
            for ( i = 0; i < n; i++ ) { 
                // q below left truncation point
                if ( qptr[i] < left ) {
                  rvalptr[i] = 1;
                // q on or above right truncation point
                } else if ( qptr[i] >= right ) {
                    rvalptr[i] = 0;
                // No truncation
                } else {
                    qtmp  = (plogis((right - mu)/sigma, 0.0, 1.0, 1, 0) - 
                            plogis((qptr[i] - mu)/sigma, 0.0, 1.0, 1, 0));
                    rvalptr[i] = qtmp / denom;
                }
            }
        }
    }

    UNPROTECT(1);
    return rval;
}


