"""Reference values for the covariate-adjusted parallel-group analysis (section COV).

Generates a seeded dataset (cov_parallel_data.csv) and fits the same models
with statsmodels, a second tool independent of R's lm(). Run:
    python3 validation/fixtures/make_covariate_reference.py
The R validation compares the app's adjusted and unadjusted 90% intervals with
cov_parallel_reference.csv. Tool versions are written into that file's
'tool' column.
"""
import os
import numpy as np, pandas as pd, statsmodels, statsmodels.formula.api as smf
from scipy import stats
import scipy

here = os.path.dirname(os.path.abspath(__file__))
rng = np.random.default_rng(20260930)
n = 60
d = pd.DataFrame({
    "Subject": np.arange(1, n + 1),
    "Treat": np.repeat(["R", "T"], n // 2),
    "age": np.round(rng.normal(42, 11, n)),
    "weight": np.round(rng.normal(72, 12, n), 1),
    "sex": rng.choice(["F", "M"], n),
})
d["Var"] = np.round(np.exp(3.0 + 0.02 * (d.age - 42) + 0.008 * (d.weight - 72)
                           + 0.10 * (d.sex == "M") + np.where(d.Treat == "T", 0.04, 0)
                           + rng.normal(0, 0.28, n)), 4)
d.to_csv(os.path.join(here, "cov_parallel_data.csv"), index=False)

d["y"] = np.log(d.Var)
d["lage"] = np.log(d.age)
tool = f"python {os.sys.version.split()[0]}; statsmodels {statsmodels.__version__}; scipy {scipy.__version__}; numpy {np.__version__}"
models = {
    "age_sex": "y ~ C(Treat, Treatment('R')) + age + C(sex)",
    "age_weight": "y ~ C(Treat, Treatment('R')) + age + weight",
    "logage": "y ~ C(Treat, Treatment('R')) + lage",
    "none": "y ~ C(Treat, Treatment('R'))",
}
rows = []
for k, f in models.items():
    m = smf.ols(f, d).fit()
    term = [t for t in m.params.index if t.startswith("C(Treat")][0]
    ci = m.conf_int(alpha=0.10).loc[term]
    rows.append(dict(model=k, pe=100 * np.exp(m.params[term]), lo=100 * np.exp(ci[0]),
                     hi=100 * np.exp(ci[1]), df=m.df_resid, mse=m.mse_resid, tool=tool))
pd.DataFrame(rows).to_csv(os.path.join(here, "cov_parallel_reference.csv"), index=False,
                          float_format="%.10f")
