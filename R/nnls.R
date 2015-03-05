nnls <- function(A, b){
  tol <- 1e-10;
# [m,n] = size(A);
  nrows <- dim(A)[1]
  ncols <- dim(A)[2]
#  P = zeros(1,n);  	
  P <- rep(0, ncols);
# Z = 1:n;
  Z <- 1:ncols;
# x = P';
  x <- P;
  ZZ <- Z;
  w <- b-A*x;

  # set up iteration criterion
  iter = 0;		
  iter_max = 10*nrows;
  
  while (any(Z) & any(w[ZZ] > tol)) {
    # [~,t] = max(w(ZZ));
    t = max(w[ZZ]);
    t = ZZ[t];
    P[t] = t;
    Z[t] = 0;
    # PP = find(P);
    PP = which(P != 0)
    # ZZ = find(Z);
    ZZ = which(Z != 0)
    # nzz = size(ZZ);
    nzzrows <- dim(ZZ)[1]
    nzzcols <- dim(ZZ)[2]
    # z(PP')=(b(PP)'/A(PP,PP)');
    z[PP] = b[PP]/A[PP,PP]
    #   z(ZZ) = zeros(nzz(2),nzz(1))';
    z[ZZ] = matrix(0, nzzcols, nzzrows)
    #   z=z(:);
    # inner loop to remove elements from the positive set which no longer belong
      while (any((z[PP] <= tol)) && iter < iter_max) {
          iter = iter + 1;
          #QQ = find((z <= tol) & P');
          QQ = which( ((z <= tol) & P) != 0); 
    #         alpha = min(x(QQ)./(x(QQ) - z(QQ)));
          alpha = min( x[QQ] / (x[QQ]-z[QQ]) );
          x = x + alpha*(z - x);
    #         ij = find(abs(x) < tol & P' ~= 0);
          ij = which((abs(x) < tol & P != 0) != 0)
          Z[ij]=ij;
    #         P(ij)=zeros(1,length(ij));
          P[ij] = rep(0, length(ij))
    #         PP = find(P);
          PP = which(P != 0)
    #         ZZ = find(Z);
          ZZ = which(Z != 0)
    #         nzz = size(ZZ);
          nzzrows <- dim(ZZ)[1]
          nzzcols <- dim(ZZ)[2]
    #         z(PP)=(b(PP)'/A(PP,PP)');
          z[PP] = b[PP]/A[PP,PP]
    #         z(ZZ) = zeros(nzz(2),nzz(1));
          z[ZZ] = matrix(0, nzzcols, nzzrows)
    #         z=z(:);
      }
    x = z;
    w = b-A*x;
  }
}