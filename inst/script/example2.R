library(inline)
library(Rcpp)


A=matrix(runif(n*n),n,n)
B=matrix(runif(n*n),n,n)

cppFunction('
        NumericMatrix mmult(const NumericMatrix& m1, const NumericMatrix& m2){
          NumericMatrix out(m1.nrow(),m2.ncol());
          NumericVector rm1, cm2;
          for (size_t i = 0; i < m1.nrow(); ++i) {
              rm1 = m1(i,_);
              for (size_t j = 0; j < m2.ncol(); ++j) {
                cm2 = m2(_,j);
                out(i,j) = std::inner_product(rm1.begin(), rm1.end(), cm2.begin(), 0.);              
              }
            }
          return out;
        }'
)
tic()
res=mmult(A,B)
toc()


C=A%*%B
max(abs(res-C))
