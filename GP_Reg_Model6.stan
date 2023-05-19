functions {
  matrix multi_normal_cov(matrix x, vector alpha, vector rho) {
    int N = rows(x);
    matrix[N, N] K;
    row_vector[4] rho_row = to_row_vector(rho);
    for (i in 1:N) {
      for (j in 1:N) {
        row_vector[4] d = x[i] - x[j];
        K[i, j] = alpha[1] * exp(-0.5 * dot_self(d ./ rho_row));
      }
    }
    return K;
  }
}

data {
  int<lower=1> N1;
  matrix[N1, 4] x1;
  vector[N1] y1;
  int<lower=1> N2;
  matrix[N2, 4] x2;
}

transformed data {
  real delta = 1e-9;
  int<lower=1> N = N1 + N2;
  matrix[N, 4] x;
  for (n1 in 1:N1) 
    for (d in 1:4)
      x[n1, d] = x1[n1, d];
  for (n2 in 1:N2)
    for (d in 1:4)
      x[N1 + n2, d] = x2[n2, d];
}

parameters {
  vector<lower=0>[4] rho;
  vector<lower=0>[4] alpha;
  real<lower=0> sigma;
  vector[N] eta;
}

transformed parameters {
  vector[N] f;
  {
    matrix[N, N] L_K;
    matrix[N, N] K = multi_normal_cov(x, alpha, rho);

    // diagonal elements
    for (n in 1:N)
      K[n, n] = K[n, n] + delta;

    L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}

model {
  for (d in 1:4) {
    rho[d] ~ inv_gamma(5, 5);
    alpha[d] ~ std_normal();
  }
  sigma ~ std_normal();
  eta ~ std_normal();

  y1 ~ normal(f[1:N1], sigma);
}

generated quantities {
  vector[N2] y2;
  for (n2 in 1:N2)
    y2[n2] = normal_rng(f[N1 + n2], sigma);
}
