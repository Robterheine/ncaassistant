"""Independent FDA RSABE computation (Appendix G, May 2026) for section RSA.

Written apart from R/be_scaled.R, in numpy/scipy, without lm(): the two
regressions are solved with lstsq on explicit design matrices. Reads
rsabe_datasets.csv (per-administration Cmax of replicateBE reference data
sets) and writes rsabe_reference.csv. Run:
    python3 validation/fixtures/make_rsabe_reference.py
Complete cases only: a subject enters when every period has a value.
"""
import os, sys
import numpy as np, pandas as pd, scipy
from scipy import stats

here = os.path.dirname(os.path.abspath(__file__))
data = pd.read_csv(os.path.join(here, "rsabe_datasets.csv"), dtype={"subject": str})
theta = (np.log(1.25) / 0.25) ** 2


def ols_resid_ss(y, X):
    beta, *_ = np.linalg.lstsq(X, y, rcond=None)
    r = y - X @ beta
    return beta, float(r @ r)


rows = []
for name, d in data.groupby("dataset", sort=False):
    n_per = d.groupby("subject").period.nunique().max()
    subj = []
    for sid, s in d.groupby("subject"):
        s = s.sort_values("period")
        if len(s) != n_per or s.pk.isna().any():
            continue
        r = np.log(s.pk[s.treatment == "R"].to_numpy()); t = np.log(s.pk[s.treatment == "T"].to_numpy())
        subj.append((sid, s.sequence.iloc[0], t.mean() - r.mean(), r[0] - r[1]))
    df = pd.DataFrame(subj, columns=["subject", "seq", "I", "D"])
    n = len(df); seqs = sorted(df.seq.unique()); m = len(seqs)
    X = np.column_stack([(df.seq == s).astype(float) for s in seqs])   # cell-means coding
    beta_i, ss_i = ols_resid_ss(df.I.to_numpy(), X)
    _, ss_d = ols_resid_ss(df.D.to_numpy(), X)
    nk = X.sum(axis=0); w = np.full(m, 1.0 / m)
    est = float(w @ beta_i)
    se = float(np.sqrt(ss_i / (n - m) * np.sum(w ** 2 / nk)))
    q = stats.t.ppf(0.95, n - m); lcl, ucl = est - q * se, est + q * se
    x = est ** 2 - se ** 2; boundx = max(abs(lcl), abs(ucl)) ** 2
    s2wr = ss_d / (n - m) / 2
    y = -theta * s2wr; boundy = y * (n - m) / stats.chi2.ppf(0.95, n - m)
    crit = (x + y) + np.sqrt((boundx - x) ** 2 + (boundy - y) ** 2)
    rows.append(dict(dataset=name, n=n, m=m, pe=100 * np.exp(est), lcl=100 * np.exp(lcl), ucl=100 * np.exp(ucl),
                     swr=np.sqrt(s2wr), dfd=n - m, x=x, boundx=boundx, y=y, boundy=boundy, critbound=crit))
out = pd.DataFrame(rows)
out["tool"] = f"python {sys.version.split()[0]}; numpy {np.__version__}; scipy {scipy.__version__}; pandas {pd.__version__}"
out.to_csv(os.path.join(here, "rsabe_reference.csv"), index=False, float_format="%.12g")
print(out.drop(columns="tool").to_string())
