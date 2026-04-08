const fs = require('fs');
const { Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
        Header, Footer, AlignmentType, HeadingLevel, BorderStyle, WidthType,
        ShadingType, PageNumber, PageBreak, TableOfContents } = require('docx');

// --- Helpers ---
const border = { style: BorderStyle.SINGLE, size: 1, color: "999999" };
const borders = { top: border, bottom: border, left: border, right: border };
const cellMargins = { top: 60, bottom: 60, left: 100, right: 100 };
const headerShading = { fill: "2C3E50", type: ShadingType.CLEAR };
const altShading = { fill: "F7F9FC", type: ShadingType.CLEAR };

function hdrCell(text, width) {
  return new TableCell({
    borders, width: { size: width, type: WidthType.DXA },
    shading: headerShading, margins: cellMargins,
    children: [new Paragraph({ children: [new TextRun({ text, bold: true, color: "FFFFFF", font: "Arial", size: 20 })] })]
  });
}
function cell(text, width, shading) {
  const opts = { borders, width: { size: width, type: WidthType.DXA }, margins: cellMargins, children: [] };
  if (shading) opts.shading = shading;
  opts.children = [new Paragraph({ children: [new TextRun({ text: String(text), font: "Arial", size: 20 })] })];
  return new TableCell(opts);
}
function boldCell(text, width, shading) {
  const opts = { borders, width: { size: width, type: WidthType.DXA }, margins: cellMargins, children: [] };
  if (shading) opts.shading = shading;
  opts.children = [new Paragraph({ children: [new TextRun({ text: String(text), font: "Arial", size: 20, bold: true })] })];
  return new TableCell(opts);
}

function heading(level, text) {
  return new Paragraph({ heading: level, children: [new TextRun({ text, font: "Arial" })] });
}
function para(text, opts = {}) {
  return new Paragraph({ spacing: { after: 120 }, ...opts, children: [new TextRun({ text, font: "Arial", size: 22, ...opts })] });
}

// --- Requirements data ---
const requirements = [
  // GEN
  { id: "URS-GEN-01", cat: "General", desc: "The system shall be accessible via a standard web browser without local installation beyond R and required packages.", acceptance: "App launches via shiny::runApp() and renders in Chrome/Firefox/Edge.", risk: "Low", fmea: "FM-GEN-01" },
  { id: "URS-GEN-03", cat: "General", desc: "A statistical methods page shall document all formulas, algorithms, and references used.", acceptance: "Methods page accessible from navigation; contains NCA, BE, and power method descriptions.", risk: "Low", fmea: "FM-GEN-02" },
  { id: "URS-GEN-04", cat: "General", desc: "The system shall not persistently store user data between sessions.", acceptance: "No database, file cache, or persistent storage beyond the R session.", risk: "Low", fmea: "FM-GEN-03" },
  { id: "URS-GEN-05", cat: "General", desc: "The system shall display the current application version.", acceptance: "APP_VERSION is displayed in the UI and included in exports.", risk: "Low", fmea: "FM-GEN-04" },
  { id: "URS-GEN-06", cat: "General", desc: "PK parameter names shall follow CDISC SDTM conventions where applicable.", acceptance: "cdisc_pk_names() maps NonCompart abbreviations to CDISC terms.", risk: "Low", fmea: "FM-GEN-05" },
  // DAT
  { id: "URS-DAT-01", cat: "Data", desc: "The system shall accept CSV and Excel (.xlsx/.xls) file uploads.", acceptance: "CSV and Excel files load without error; data preview matches source.", risk: "Medium", fmea: "FM-DAT-01" },
  { id: "URS-DAT-02", cat: "Data", desc: "The system shall auto-detect Subject, Time, and Concentration columns from common naming conventions.", acceptance: "7 naming conventions (standard, CDISC, WinNonlin, Dutch, lowercase, abbreviated, mixed) correctly detected for all 3 required columns.", risk: "High", fmea: "FM-DAT-02" },
  { id: "URS-DAT-03", cat: "Data", desc: "The system shall perform 20+ data quality checks and report findings with severity levels (ERROR/WARNING/INFO/OK).", acceptance: "run_data_quality_check returns structured findings; ERRORs block processing; WARNINGs allow with caution.", risk: "High", fmea: "FM-DAT-03" },
  { id: "URS-DAT-04", cat: "Data", desc: "The system shall implement 6 BLQ handling rules compatible with WinNonlin conventions.", acceptance: "All 6 rules produce correct output for known BLQ profiles; multi-subject processing is independent.", risk: "High", fmea: "FM-DAT-04" },
  { id: "URS-DAT-05", cat: "Data", desc: "The system shall block analysis when ERROR-severity data quality issues are present.", acceptance: "Processing halts with notification when qc$pass == FALSE.", risk: "High", fmea: "FM-DAT-05" },
  { id: "URS-DAT-06", cat: "Data", desc: "The system shall handle file read errors gracefully with user notification.", acceptance: "Malformed files produce error notification, not R crash.", risk: "Medium", fmea: "FM-DAT-06" },
  { id: "URS-DAT-07", cat: "Data", desc: "The system shall auto-detect study design (single-arm, parallel, crossover) from data structure.", acceptance: "detect_study_design correctly identifies single_arm, parallel, and crossover designs.", risk: "Medium", fmea: "FM-DAT-07" },
  // NCA
  { id: "URS-NCA-01", cat: "NCA", desc: "The system shall compute standard NCA parameters (Cmax, Tmax, AUClast, AUCinf, t\u00BD, \u03BBz, CL, Vz) using NonCompart.", acceptance: "IV bolus analytical ground truth: Cmax, \u03BBz, t\u00BD, AUCinf, CL, Vz all within tolerance of analytical values.", risk: "Critical", fmea: "FM-NCA-01" },
  { id: "URS-NCA-02", cat: "NCA", desc: "The system shall support 3 administration routes: extravascular, IV bolus, IV infusion.", acceptance: "Route selection determines parameter names (CL vs CL/F, Vz vs Vz/F).", risk: "High", fmea: "FM-NCA-02" },
  { id: "URS-NCA-03", cat: "NCA", desc: "The system shall support 2 trapezoidal methods: linear and linear-up/log-down.", acceptance: "Both methods produce valid AUC; log-down is more accurate for exponential decay.", risk: "High", fmea: "FM-NCA-03" },
  { id: "URS-NCA-04", cat: "NCA", desc: "The system shall estimate \u03BBz using best-fit adjusted R\u00B2 criterion with configurable threshold.", acceptance: "\u03BBz matches analytical value for mono-exponential data; threshold filters poor fits.", risk: "Critical", fmea: "FM-NCA-04" },
  { id: "URS-NCA-05", cat: "NCA", desc: "The system shall provide single-subject NCA with interactive concentration-time plot.", acceptance: "sNCA produces valid results; plot is interactive (plotly).", risk: "Medium", fmea: "FM-NCA-05" },
  { id: "URS-NCA-06", cat: "NCA", desc: "The system shall perform batch NCA across all subjects with summary statistics.", acceptance: "tblNCA returns one row per subject; summary statistics computed for all parameters.", risk: "High", fmea: "FM-NCA-06" },
  { id: "URS-NCA-07", cat: "NCA", desc: "The system shall support steady-state NCA parameters (AUCtau, Cavg, fluctuation).", acceptance: "SS=TRUE produces AUCTAU, CAVG, FLUCTP columns.", risk: "Medium", fmea: "FM-NCA-07" },
  { id: "URS-NCA-08", cat: "NCA", desc: "The system shall compute dose-normalized parameters.", acceptance: "CMAX_DN = CMAX/dose; all expected DN columns created.", risk: "Medium", fmea: "FM-NCA-08" },
  { id: "URS-NCA-09", cat: "NCA", desc: "The system shall support per-subject dosing from a data column.", acceptance: "Different doses per subject produce different CL values.", risk: "Medium", fmea: "FM-NCA-09" },
  { id: "URS-NCA-10", cat: "NCA", desc: "The system shall handle edge cases without crashing (negative conc, all-zero, sparse, extreme values).", acceptance: "No R crash on any edge case input; results may contain NA but are structurally valid.", risk: "High", fmea: "FM-NCA-10" },
  { id: "URS-NCA-11", cat: "NCA", desc: "The system shall parse manually entered time/concentration data from text input.", acceptance: "parse_manual handles normal input, CRLF, blanks, mismatches, non-numeric values correctly.", risk: "Medium", fmea: "FM-NCA-11" },
  { id: "URS-NCA-12", cat: "NCA", desc: "The system shall allow manual selection of terminal phase data points for \u03BBz estimation in batch NCA and BE, with validation guards (minimum 2 points, positive slope required) and user notification of changes.", acceptance: "Recalculated \u03BBz matches manual regression on selected points. Negative slope rejected. 2-point warning displayed.", risk: "High", fmea: "FM-NCA-12" },
  // BE
  { id: "URS-BE-01", cat: "BE", desc: "The system shall perform ANOVA-based bioequivalence analysis on log-transformed PK parameters.", acceptance: "Correct ANOVA model specification per study design; MSE > 0.", risk: "Critical", fmea: "FM-BE-01" },
  { id: "URS-BE-02", cat: "BE", desc: "The system shall support 5 study designs: 2-period crossover, fixed-order, 3-period, replicate, parallel.", acceptance: "Each design produces valid results; fixed-order df = N-1.", risk: "High", fmea: "FM-BE-02" },
  { id: "URS-BE-03", cat: "BE", desc: "The system shall compute 90%% confidence intervals for the geometric mean ratio.", acceptance: "CI contains true ratio when data is generated with known GMR.", risk: "Critical", fmea: "FM-BE-03" },
  { id: "URS-BE-04", cat: "BE", desc: "The system shall determine BE conclusion (YES/NO) based on CI vs. acceptance limits.", acceptance: "BE=YES when CI within [80,125]; BE=NO when CI outside limits.", risk: "Critical", fmea: "FM-BE-04" },
  { id: "URS-BE-05", cat: "BE", desc: "The system shall support both fixed-effects and mixed-effects (nlme) models.", acceptance: "Mixed model converges; GMR agrees with fixed effects within 5%%.", risk: "High", fmea: "FM-BE-05" },
  { id: "URS-BE-06", cat: "BE", desc: "The system shall generate a forest plot of BE results.", acceptance: "Forest plot displays point estimates and CI bars for each parameter.", risk: "Low", fmea: "FM-BE-06" },
  { id: "URS-BE-07", cat: "BE", desc: "The system shall allow configurable CI level and acceptance limits.", acceptance: "Default 90%% CI, 80-125%% limits; user can modify.", risk: "Low", fmea: "FM-BE-07" },
  { id: "URS-BE-08", cat: "BE", desc: "The system shall provide individual concentration-time profiles per subject in the BE module, with treatment overlay and log-scale option.", acceptance: "Per-subject faceted plot renders for crossover data with treatment colour coding.", risk: "Low", fmea: "FM-BE-08" },
  // PWR
  { id: "URS-PWR-01", cat: "Power", desc: "The system shall compute sample sizes using 6 PowerTOST functions: TOST, scABEL, RSABE, NTIDFDA, noninf, dp.", acceptance: "All 6 functions return valid sample sizes for standard inputs.", risk: "High", fmea: "FM-PWR-01" },
  { id: "URS-PWR-02", cat: "Power", desc: "The system shall compute power at a given sample size.", acceptance: "power.TOST returns probability; power increases with N.", risk: "Medium", fmea: "FM-PWR-02" },
  { id: "URS-PWR-03", cat: "Power", desc: "The system shall support 5 study designs for sample size calculation.", acceptance: "All 5 designs (2x2, 2x2x3, 2x3x3, 2x2x4, parallel) return valid N.", risk: "Medium", fmea: "FM-PWR-03" },
  { id: "URS-PWR-04", cat: "Power", desc: "The system shall display power curves showing power vs. sample size.", acceptance: "Power curve rendered in plotly with target power line.", risk: "Low", fmea: "FM-PWR-04" },
  { id: "URS-PWR-05", cat: "Power", desc: "The system shall bridge NCA-derived CV to power calculations.", acceptance: "Geometric CV from NCA Cmax usable in sampleN.TOST.", risk: "Medium", fmea: "FM-PWR-05" },
  { id: "URS-PWR-06", cat: "Power", desc: "The system shall validate power/sample size inputs (CV > 0, valid alpha, etc.).", acceptance: "CV=0 causes caught error; extreme CV still computes.", risk: "Medium", fmea: "FM-PWR-06" },
  // EXP
  { id: "URS-EXP-01", cat: "Export", desc: "The system shall generate a Complete Analysis Record as a ZIP file containing results, settings, script, hash, and summary.", acceptance: "ZIP contains results.xlsx, analysis_settings.json, reproduce_analysis.R, data_integrity.txt, analysis_summary.html.", risk: "High", fmea: "FM-EXP-01" },
  { id: "URS-EXP-02", cat: "Export", desc: "The system shall generate a standalone reproducibility R script.", acceptance: "Script parses as valid R; contains all key settings and file references.", risk: "High", fmea: "FM-EXP-02" },
  { id: "URS-EXP-03", cat: "Export", desc: "The system shall export analysis settings as JSON.", acceptance: "JSON includes app version, R version, column mapping, route, dose, BLQ rule, etc.", risk: "Medium", fmea: "FM-EXP-03" },
  { id: "URS-EXP-04", cat: "Export", desc: "The system shall compute SHA-256 hashes for data integrity verification.", acceptance: "digest::digest returns 64-character hex string.", risk: "High", fmea: "FM-EXP-04" },
  { id: "URS-EXP-05", cat: "Export", desc: "The system shall include BE results in the export when a BE analysis has been performed.", acceptance: "BE CI table and ANOVA tables included in results.xlsx.", risk: "Medium", fmea: "FM-EXP-05" },
  { id: "URS-EXP-06", cat: "Export", desc: "The system shall include application and package version information in exports.", acceptance: "APP_VERSION and core package versions retrievable and included.", risk: "Medium", fmea: "FM-EXP-06" },
  { id: "URS-EXP-07", cat: "Export", desc: "When manual \u03BBz overrides are applied, the Complete Analysis Record shall log: (a) the profile identifier, (b) original and adjusted \u03BBz, (c) original and adjusted R\u00B2, (d) number of points used. This information shall appear in the settings JSON, the reproducibility script, and the HTML summary.", acceptance: "Settings JSON contains lz_overrides array. Reproducibility script applies overrides. HTML shows override table.", risk: "High", fmea: "FM-EXP-07" },
  // UI
  { id: "URS-UI-01", cat: "Usability", desc: "All PK parameter abbreviations shall have plain-language labels and contextual help.", acceptance: ">80%% of NCA output columns have friendly names; help topics defined for all key concepts.", risk: "Low", fmea: "FM-UI-01" },
  { id: "URS-UI-02", cat: "Usability", desc: "A Data Preparation Guide shall be accessible to help users format their data.", acceptance: "Guide accessible with scenario tabs and example downloads.", risk: "Low", fmea: "FM-UI-02" },
  { id: "URS-UI-03", cat: "Usability", desc: "The application shall provide a central hub (landing page) with clear workflow paths.", acceptance: "Landing page shows 5 workflow paths with descriptions.", risk: "Low", fmea: "FM-UI-03" },
  { id: "URS-UI-04", cat: "Usability", desc: "The system shall provide actionable error messages for invalid inputs.", acceptance: "validate_mapping returns specific missing field names; DQ checks have Action field.", risk: "Medium", fmea: "FM-UI-04" },
];

// --- FMEA data ---
const fmea = requirements.map(r => ({
  id: r.fmea,
  mode: `${r.id} not met: ${r.desc.substring(0, 60)}...`,
  severity: r.risk === "Critical" ? 10 : r.risk === "High" ? 7 : r.risk === "Medium" ? 4 : 2,
  occurrence: 2,
  detectability: 2,
  rpn: 0,
  control: `Validation test(s) for ${r.id}`
}));
fmea.forEach(f => { f.rpn = f.severity * f.occurrence * f.detectability; });

// --- Supplier packages ---
const suppliers = [
  { pkg: "NonCompart", ver: "0.7.0+", purpose: "Core NCA computation (tblNCA, sNCA, AUC)", risk: "High", mitigation: "Analytical ground truth tests against known pharmacokinetic solutions" },
  { pkg: "PowerTOST", ver: "1.5+", purpose: "Sample size and power for BE studies", risk: "Medium", mitigation: "Cross-check with published sample size tables (Hauschke et al.)" },
  { pkg: "nlme", ver: "3.1+", purpose: "Mixed-effects models for BE analysis", risk: "Medium", mitigation: "Compare mixed vs fixed effects; verify convergence" },
  { pkg: "shiny", ver: "1.7+", purpose: "Web application framework", risk: "Low", mitigation: "Mature CRAN package; UI verified by manual testing" },
  { pkg: "digest", ver: "0.6+", purpose: "SHA-256 hashing for data integrity", risk: "Low", mitigation: "Industry-standard hashing; verified against known digests" },
];

// --- Build document ---
const children = [];

// Title
children.push(new Paragraph({ spacing: { after: 0 }, children: [] }));
children.push(new Paragraph({ spacing: { before: 2400, after: 200 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: "NCA Assistant", font: "Arial", size: 48, bold: true, color: "2C3E50" })] }));
children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 100 },
  children: [new TextRun({ text: "User Requirement Specification", font: "Arial", size: 36, color: "2C3E50" })] }));
children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 400 },
  children: [new TextRun({ text: "Version 1.1", font: "Arial", size: 28, color: "7F8C8D" })] }));

// Doc info table
const infoData = [
  ["Document ID", "NCA-URS-001"],
  ["Version", "1.1"],
  ["Status", "Draft"],
  ["Author", "Radboud Applied Pharmacometrics"],
  ["Date", new Date().toISOString().split('T')[0]],
  ["Classification", "GAMP 5 Category 5 — Custom Application"],
];
const infoColWidths = [3000, 6360];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: infoColWidths,
  rows: infoData.map(([k, v]) => new TableRow({
    children: [boldCell(k, infoColWidths[0], altShading), cell(v, infoColWidths[1])]
  }))
}));

children.push(new Paragraph({ children: [new PageBreak()] }));

// TOC
children.push(new TableOfContents("Table of Contents", { hyperlink: true, headingStyleRange: "1-3" }));
children.push(new Paragraph({ children: [new PageBreak()] }));

// 1. Purpose
children.push(heading(HeadingLevel.HEADING_1, "1. Purpose"));
children.push(para("This User Requirement Specification (URS) defines the functional and quality requirements for NCA Assistant v1.1, an open-source R/Shiny application for non-compartmental pharmacokinetic analysis, bioequivalence testing, and power/sample size calculation."));
children.push(para("The document serves as the foundation of the validation lifecycle per GAMP 5 principles and ICH Q9 risk management. Each requirement has a unique identifier, acceptance criterion, risk classification, and cross-reference to the FMEA and validation test suite."));
children.push(para("This URS covers 49 requirements across 7 categories: General (5), Data Handling (7), NCA (12), Bioequivalence (8), Power (6), Export (7), and Usability (4)."));

// 2. System Description
children.push(heading(HeadingLevel.HEADING_1, "2. System Description"));
children.push(heading(HeadingLevel.HEADING_2, "2.1 GAMP 5 Classification"));
children.push(para("NCA Assistant is classified as GAMP 5 Category 5 (Custom Application). It is a bespoke R/Shiny application developed by Radboud Applied Pharmacometrics to support pharmacokinetic analysis in clinical research."));
children.push(heading(HeadingLevel.HEADING_2, "2.2 Intended Use"));
children.push(para("The application is intended for use by pharmacokineticists, clinical pharmacologists, and pharmaceutical scientists to perform NCA, bioequivalence assessment, and study planning. It is not a medical device and does not directly influence patient treatment decisions."));
children.push(heading(HeadingLevel.HEADING_2, "2.3 Operating Environment"));
children.push(para("R >= 4.1.0, standard web browser (Chrome/Firefox/Edge), no internet connection required after package installation. Deployed via shiny::runApp() locally or via shinyapps.io."));
children.push(heading(HeadingLevel.HEADING_2, "2.4 ALCOA+ Data Integrity"));
children.push(para("The system supports ALCOA+ principles through: Attributable (analyst name in exports), Legible (formatted reports), Contemporaneous (timestamps), Original (SHA-256 hash of source data), Accurate (validated calculations). The Complete Analysis Record provides a self-contained audit trail."));
children.push(heading(HeadingLevel.HEADING_2, "2.5 Electronic Records Scope"));
children.push(para("The system generates electronic records (CSV, Excel, JSON, HTML) as part of the Complete Analysis Record. These records are designed for regulatory inspection readiness but the system itself does not implement electronic signatures per 21 CFR Part 11. Organizational SOPs govern signature workflows."));

// 3. Supplier Assessment
children.push(heading(HeadingLevel.HEADING_1, "3. Supplier Assessment"));
children.push(para("The following critical R packages are used. Each has been assessed for suitability:"));
const supColWidths = [1400, 800, 2500, 1000, 3660];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: supColWidths,
  rows: [
    new TableRow({ children: ["Package", "Version", "Purpose", "Risk", "Mitigation"].map((t, i) => hdrCell(t, supColWidths[i])) }),
    ...suppliers.map(s => new TableRow({
      children: [cell(s.pkg, supColWidths[0]), cell(s.ver, supColWidths[1]), cell(s.purpose, supColWidths[2]), cell(s.risk, supColWidths[3]), cell(s.mitigation, supColWidths[4])]
    }))
  ]
}));

// 4. Risk Assessment (FMEA)
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(heading(HeadingLevel.HEADING_1, "4. Risk Assessment (FMEA per ICH Q9)"));
children.push(para("Failure Mode and Effects Analysis with Detectability scoring per ICH Q9. RPN = Severity x Occurrence x Detectability."));
const fmeaColWidths = [1200, 3160, 800, 800, 800, 800, 1800];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: fmeaColWidths,
  rows: [
    new TableRow({ children: ["FM ID", "Failure Mode", "Sev", "Occ", "Det", "RPN", "Control"].map((t, i) => hdrCell(t, fmeaColWidths[i])) }),
    ...fmea.map((f, idx) => new TableRow({
      children: [
        cell(f.id, fmeaColWidths[0], idx % 2 === 1 ? altShading : undefined),
        cell(f.mode.substring(0, 80), fmeaColWidths[1], idx % 2 === 1 ? altShading : undefined),
        cell(String(f.severity), fmeaColWidths[2], idx % 2 === 1 ? altShading : undefined),
        cell(String(f.occurrence), fmeaColWidths[3], idx % 2 === 1 ? altShading : undefined),
        cell(String(f.detectability), fmeaColWidths[4], idx % 2 === 1 ? altShading : undefined),
        cell(String(f.rpn), fmeaColWidths[5], idx % 2 === 1 ? altShading : undefined),
        cell(f.control, fmeaColWidths[6], idx % 2 === 1 ? altShading : undefined),
      ]
    }))
  ]
}));

// 5. Requirements
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(heading(HeadingLevel.HEADING_1, "5. Requirements"));

const categories = ["General", "Data", "NCA", "BE", "Power", "Export", "Usability"];
const reqColWidths = [1200, 3600, 2960, 800, 800];

for (const cat of categories) {
  const catReqs = requirements.filter(r => r.cat === cat);
  children.push(heading(HeadingLevel.HEADING_2, `5.${categories.indexOf(cat) + 1} ${cat} Requirements`));

  children.push(new Table({
    width: { size: 9360, type: WidthType.DXA }, columnWidths: reqColWidths,
    rows: [
      new TableRow({ children: ["ID", "Description", "Acceptance Criterion", "Risk", "FMEA"].map((t, i) => hdrCell(t, reqColWidths[i])) }),
      ...catReqs.map((r, idx) => new TableRow({
        children: [
          boldCell(r.id, reqColWidths[0], idx % 2 === 1 ? altShading : undefined),
          cell(r.desc, reqColWidths[1], idx % 2 === 1 ? altShading : undefined),
          cell(r.acceptance, reqColWidths[2], idx % 2 === 1 ? altShading : undefined),
          cell(r.risk, reqColWidths[3], idx % 2 === 1 ? altShading : undefined),
          cell(r.fmea, reqColWidths[4], idx % 2 === 1 ? altShading : undefined),
        ]
      }))
    ]
  }));
  children.push(para(""));
}

// 6. Traceability to validation.R
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(heading(HeadingLevel.HEADING_1, "6. Traceability to Validation Script"));
children.push(para("Each requirement is tested by one or more automated or manual tests in validation/validation.R. The validation script verifies URS coverage by checking that every URS ID appears in at least one test. The complete traceability matrix is generated in the validation report (Attachment A to the IQ/OQ/PQ Protocol)."));
children.push(para("Run the validation script from the project root: Rscript validation/validation.R"));

// 7. Configuration Management
children.push(heading(HeadingLevel.HEADING_1, "7. Configuration Management"));
children.push(heading(HeadingLevel.HEADING_2, "7.1 Version Control"));
children.push(para("Source code is managed in Git (github.com/robterheine/ncaassistant). The master branch represents the validated release."));
children.push(heading(HeadingLevel.HEADING_2, "7.2 Change Categories"));
children.push(para("Category A: Cosmetic/documentation changes (no revalidation required). Category B: Functional changes within validated scope (targeted revalidation of affected modules). Category C: New functionality or architectural changes (full revalidation required)."));
children.push(heading(HeadingLevel.HEADING_2, "7.3 Revalidation Triggers"));
children.push(para("Revalidation is required when: (1) R major version upgrade, (2) NonCompart, PowerTOST, or nlme package update, (3) any Category B or C change to R/*.R source files, (4) deployment to a new server environment."));

// 8. Regulatory References
children.push(heading(HeadingLevel.HEADING_1, "8. Regulatory References"));
const refs = [
  "ISPE GAMP 5: A Risk-Based Approach to Compliant GxP Computerized Systems, 2nd Edition (2022)",
  "ICH Q9(R1): Quality Risk Management (2023), ICH Harmonised Guideline",
  "FDA Guidance for Industry: Bioavailability and Bioequivalence Studies Submitted in NDAs or IND (2014)",
  "EMA Guideline on the Investigation of Bioequivalence, CPMP/EWP/QWP/1401/98 Rev. 1 (2010)",
  "FDA Guidance: Statistical Approaches to Establishing Bioequivalence (2001)",
  "21 CFR Part 320: Bioavailability and Bioequivalence Requirements",
  "EU Annex 11: Computerised Systems (2011)",
  "PIC/S PI 011-3: Good Practices for Computerised Systems in Regulated GxP Environments (2007)",
];
refs.forEach(r => children.push(para(r, { size: 20 })));

// 9. Glossary
children.push(heading(HeadingLevel.HEADING_1, "9. Glossary"));
const glossary = [
  ["ALCOA+", "Attributable, Legible, Contemporaneous, Original, Accurate (+ Complete, Consistent, Enduring, Available)"],
  ["AUC", "Area Under the Concentration-time Curve"],
  ["BE", "Bioequivalence"],
  ["BLQ", "Below Limit of Quantification"],
  ["CDISC", "Clinical Data Interchange Standards Consortium"],
  ["CL", "Clearance"],
  ["FMEA", "Failure Mode and Effects Analysis"],
  ["GAMP", "Good Automated Manufacturing Practice"],
  ["GMR", "Geometric Mean Ratio"],
  ["IQ/OQ/PQ", "Installation/Operational/Performance Qualification"],
  ["LLOQ", "Lower Limit of Quantification"],
  ["NCA", "Non-Compartmental Analysis"],
  ["RPN", "Risk Priority Number"],
  ["SDTM", "Study Data Tabulation Model"],
  ["TOST", "Two One-Sided Tests"],
  ["URS", "User Requirement Specification"],
  ["\u03BBz", "Terminal elimination rate constant"],
];
const glossColWidths = [2000, 7360];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: glossColWidths,
  rows: [
    new TableRow({ children: ["Term", "Definition"].map((t, i) => hdrCell(t, glossColWidths[i])) }),
    ...glossary.map(([term, def], idx) => new TableRow({
      children: [boldCell(term, glossColWidths[0], idx % 2 === 1 ? altShading : undefined),
                 cell(def, glossColWidths[1], idx % 2 === 1 ? altShading : undefined)]
    }))
  ]
}));

// --- Create document ---
const doc = new Document({
  styles: {
    default: { document: { run: { font: "Arial", size: 22 } } },
    paragraphStyles: [
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 32, bold: true, font: "Arial", color: "2C3E50" },
        paragraph: { spacing: { before: 360, after: 200 }, outlineLevel: 0 } },
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 26, bold: true, font: "Arial", color: "2C3E50" },
        paragraph: { spacing: { before: 240, after: 160 }, outlineLevel: 1 } },
      { id: "Heading3", name: "Heading 3", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 24, bold: true, font: "Arial", color: "34495E" },
        paragraph: { spacing: { before: 200, after: 120 }, outlineLevel: 2 } },
    ]
  },
  sections: [{
    properties: {
      page: {
        size: { width: 12240, height: 15840 },
        margin: { top: 1440, right: 1440, bottom: 1440, left: 1440 }
      }
    },
    headers: {
      default: new Header({ children: [new Paragraph({
        children: [new TextRun({ text: "NCA Assistant \u2014 User Requirement Specification v1.1", font: "Arial", size: 18, color: "999999" })],
        alignment: AlignmentType.RIGHT
      })] })
    },
    footers: {
      default: new Footer({ children: [new Paragraph({
        children: [new TextRun({ text: "Page ", font: "Arial", size: 18 }), new TextRun({ children: [PageNumber.CURRENT], font: "Arial", size: 18 })],
        alignment: AlignmentType.CENTER
      })] })
    },
    children
  }]
});

Packer.toBuffer(doc).then(buffer => {
  fs.writeFileSync("validation/NCA_Assistant_URS_v1.1.docx", buffer);
  console.log("URS document generated: validation/NCA_Assistant_URS_v1.1.docx");
});
