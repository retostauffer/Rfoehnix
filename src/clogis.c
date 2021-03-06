#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP cdclogis(SEXP y, SEXP in_mu, SEXP in_sigma, SEXP in_left, SEXP in_right, SEXP in_give_log)
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

    // Some values might be used multiple times: pre-calculate
    double dleft  = plogis((left - mu) / sigma, 0.0, 1.0, 1, give_log);
    double dright = plogis((right - mu) / sigma, 0.0, 1.0, 0, give_log);
    double logsigma = log(sigma);

    for(i = 0; i < n; i++) {
        if ( yptr[i] <= left ) {
            rvalptr[i] = dleft;
        } else if ( yptr[i] >= right ) {
            rvalptr[i] = dright;
        } else {
            if ( give_log == 0 ) {
                rvalptr[i] = dlogis((yptr[i] - mu) / sigma, 0.0, 1.0, 0) / sigma;
            } else {
                rvalptr[i] = dlogis((yptr[i] - mu) / sigma, 0.0, 1.0, 1) - logsigma;
            }
        }
        
    }

    UNPROTECT(1);
    return rval;
}


SEXP cpclogis(SEXP q, SEXP in_mu, SEXP in_sigma, SEXP in_left, SEXP in_right,
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

    // For lower tail
    if ( lower_tail == 1 ) {
        for ( i = 0; i < n; i++ ) {
            // q below left censoring point
            if ( qptr[i] < left ) {
                if ( log_p == 1 ) {
                    rvalptr[i] = log(0);
                } else {
                    rvalptr[i] = 0;
                }
            // q above or on right censoring point
            } else if ( qptr[i] >= right ) {
                    rvalptr[i] = 1 * (1 - log_p);
            // Uncensored
            } else {
                rvalptr[i] = plogis((qptr[i] - mu) / sigma, 0.0, 1.0, 1, log_p); 
            }
        }
    // Upper tail
    } else {
        for(i = 0; i < n; i++) {
            // q below left censoring point
            if ( qptr[i] <= left ) {
                rvalptr[i] = 1. * (1. - log_p);
            // q above or on right censoring point
            } else if ( qptr[i] > right ) {
                if ( log_p == 1 ) { rvalptr[i] = log(0.); }
                else              { rvalptr[i] = 0.;      }
            // Uncensored
            } else {
                rvalptr[i] = plogis((qptr[i] - mu) / sigma, 0.0, 1.0, 0, log_p); 
            }
        }
    }

    UNPROTECT(1);
    return rval;
}


