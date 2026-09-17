# ============================================================================
# NCA Assistant - regenerate the IQ/OQ/PQ protocol tables
# ============================================================================
# Rebuilds the test tables, the traceability matrix and the summary counts of
# validation/NCA_Assistant_IQOQPQ.docx from validation_results.csv of a passing
# run, so the protocol lists exactly the tests validation.R defines.
#
# Run from the project root, after a green run of validation/validation.R:
#   /usr/bin/python3 validation/make_iqoqpq.py
# Requires python-docx. Section 3 is rebuilt between the "Execute:" paragraph
# and the "4. Manual Tests" heading; everything else in the document is kept.
# ============================================================================
import copy, csv, re, docx
P = "validation/NCA_Assistant_IQOQPQ.docx"
d = docx.Document(P)
rows = list(csv.DictReader(open("validation/validation_results.csv", encoding="utf-8")))
auto = [r for r in rows if r["Result"] != "SKIP"]
man  = [r for r in rows if r["Section"] == "MAN"]
assert all(r["Result"] == "PASS" for r in auto), "reference run must pass"

def set_text(par, text):
    if not par.runs: par.add_run(text); return
    par.runs[0].text = text
    for r in par.runs[1:]: r.text = ""
def set_cell(c, text): set_text(c.paragraphs[0], text)
def para(start):
    h = [p for p in d.paragraphs if p.text.startswith(start)]; assert len(h) == 1, start; return h[0]

def fill(table, data):
    """Keep header + first data row as template; one row per item."""
    tpl = copy.deepcopy(table.rows[1]._tr)
    for r in list(table.rows)[1:]: table._tbl.remove(r._tr)
    for vals in data:
        tr = copy.deepcopy(tpl); table._tbl.append(tr)
        row = table.rows[-1]
        for c, v in zip(row.cells, vals): set_cell(c, v)

cls = {"CRITICAL": "CRIT", "SUPPORTIVE": "SUPP"}
def test_rows(sec): return [[r["ID"], r["Test"], r["Method"], r["Expected"], r["URS_Ref"], cls.get(r["Class"], r["Class"]), ""]
                            for r in auto if r["Section"] == sec]

# --- Front matter ---------------------------------------------------------------
set_text(para("NCA Assistant v"), "NCA Assistant v1.5.0")
set_text(para("This protocol defines"),
  "This protocol defines the IQ, OQ, and PQ procedures for NCA Assistant v1.5.0. Execute: Rscript validation/validation.R "
  "from the project root. The test tables below list every automated test with the method, expected result, URS "
  f"reference and classification exactly as defined in validation.R ({len(auto)} automated, {len(man)} manual). "
  "Record the outcome of your own run in the Result column, or attach validation_results.csv.")
for r in d.tables[0].rows:
    if r.cells[0].text == "Date": set_cell(r.cells[1], "September 2026")

env = d.tables[1]
names = [r.cells[0].text for r in env.rows]
tpl = env.rows[names.index("digest")]._tr
for extra in [e for e in ["replicateBE (validation only)", "shiny", "Validation script SHA-256"] if e not in names]:
    new = copy.deepcopy(tpl); env.rows[names.index("Executed by")]._tr.addprevious(new)
    set_cell([x for x in env.rows if x._tr is new][0].cells[0], extra)

set_text([p for p in d.paragraphs if p.text.endswith("tests.") or " tests: R version" in p.text][0], f"{sum(r['Section']=='IQ' for r in auto)} tests: R version, required packages, and that every source file parses.")
fill(d.tables[2], test_rows("IQ"))

# --- Section 3: rebuild subsections ---------------------------------------------------
sections = [("DAT", "Data Handling (OQ)"), ("NCA", "NCA (OQ/PQ)"), ("BE", "Bioequivalence (OQ/PQ)"),
            ("OQ-NEW", "Half-Life Overrides (OQ)"), ("PWR", "Power and Sample Size (OQ)"),
            ("EXP", "Export and Reproducibility (OQ)"), ("UI", "Usability and Code Quality (OQ)"),
            ("VIZ", "Visualization (OQ)"), ("REG", "Correctness Regressions (OQ/PQ)"),
            ("REP", "Replicate Designs (OQ/PQ)"), ("REC", "Analysis Records (PQ)"),
            ("CONV", "ADNCA Conversion (OQ)"), ("REV", "First Adversarial Review (OQ)"),
            ("REV2", "Second Review, Part 1 (OQ)"), ("REV3", "Second Review, Part 2 (OQ)"), ("REV4", "Statistical Audit (OQ/PQ)"),
            ("PAUC", "Partial AUC (OQ/PQ)")]
known = {s for s, _ in sections} | {"IQ", "MAN"}
assert {r["Section"] for r in rows} <= known, {r["Section"] for r in rows} - known

body = d.element.body
start = para("Execute: Rscript validation/validation.R")._p
end = para("4. Manual Tests")._p
heading_tpl = copy.deepcopy(para("3.1 ")._p)
table_tpl = copy.deepcopy(d.tables[4]._tbl)
blank_tpl = copy.deepcopy(start.getnext()) if start.getnext().tag.endswith("}p") and not "".join(start.getnext().itertext()).strip() else None
el = start.getnext()
while el is not end:
    nxt = el.getnext(); body.remove(el); el = nxt
for k, (sec, title) in enumerate(sections, 1):
    data = test_rows(sec)
    h = copy.deepcopy(heading_tpl); end.addprevious(h)
    hp = [p for p in d.paragraphs if p._p is h][0]
    set_text(hp, f"3.{k} {title} ({len(data)} tests)")
    t = copy.deepcopy(table_tpl); end.addprevious(t)
    fill([x for x in d.tables if x._tbl is t][0], data)
    if blank_tpl is not None: end.addprevious(copy.deepcopy(blank_tpl))

# --- Manual tests, traceability, summary --------------------------------------------
tables = d.tables
man_t = [t for t in tables if len(t.columns) > 2 and t.rows[0].cells[2].text == "Procedure"][0]
fill(man_t, [[r["ID"], r["Test"], r["Method"], r["Expected"], r["URS_Ref"], "", ""] for r in man])

src = open("validation/validation.R", encoding="utf-8").read()
urs_all = (["URS-GEN-01"] + [f"URS-GEN-0{i}" for i in (3,4,5,6)] + [f"URS-DAT-0{i}" for i in range(1,8)] +
           [f"URS-NCA-{i:02d}" for i in range(1,15)] + [f"URS-BE-0{i}" for i in range(1,10)] + ["URS-BE-10"] +
           [f"URS-PWR-0{i}" for i in range(1,7)] + [f"URS-EXP-0{i}" for i in range(1,9)] +
           [f"URS-UI-0{i}" for i in range(1,5)] + [f"URS-VIZ-0{i}" for i in range(1,9)])
def refs(r): return [x.strip() for x in r["URS_Ref"].split(",")]
tr_rows = []
for u in urs_all:
    a = [r["ID"] for r in auto if u in refs(r)]; m = [r["ID"] for r in man if u in refs(r)]
    assert a or m, u
    tr_rows.append([u, ", ".join(a) if a else "—", ", ".join(m) if m else "—", str(len(a) + len(m))])
trace = [t for t in tables if t.rows[0].cells[0].text == "URS ID"][0]
fill(trace, tr_rows)

summ = [t for t in tables if t.rows[0].cells[0].text == "Metric"][0]
fill(summ, [["Total tests", str(len(rows))], ["Automated", str(len(auto))],
            ["  of which CRITICAL", str(sum(r["Class"] == "CRITICAL" for r in auto))],
            ["  of which SUPPORTIVE", str(sum(r["Class"] == "SUPPORTIVE" for r in auto))],
            ["Manual", str(len(man))], ["URS requirements covered", f"{len(urs_all)}/{len(urs_all)}"]])
d.save(P)
print("saved", len(rows), len(auto), len(man))
