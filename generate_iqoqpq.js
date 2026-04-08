const fs = require('fs');
const { Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
        Header, Footer, AlignmentType, HeadingLevel, BorderStyle, WidthType,
        ShadingType, PageNumber, PageBreak, TableOfContents, PageOrientation } = require('docx');

// --- Helpers ---
const border = { style: BorderStyle.SINGLE, size: 1, color: "999999" };
const borders = { top: border, bottom: border, left: border, right: border };
const cm = { top: 50, bottom: 50, left: 80, right: 80 };
const hdrFill = { fill: "2C3E50", type: ShadingType.CLEAR };
const altFill = { fill: "F7F9FC", type: ShadingType.CLEAR };
const passFill = { fill: "D5F5E3", type: ShadingType.CLEAR };
const failFill = { fill: "FADBD8", type: ShadingType.CLEAR };

function hCell(text, w) {
  return new TableCell({ borders, width: { size: w, type: WidthType.DXA }, shading: hdrFill, margins: cm,
    children: [new Paragraph({ children: [new TextRun({ text, bold: true, color: "FFFFFF", font: "Arial", size: 18 })] })] });
}
function tCell(text, w, sh) {
  const o = { borders, width: { size: w, type: WidthType.DXA }, margins: cm,
    children: [new Paragraph({ children: [new TextRun({ text: String(text || ""), font: "Arial", size: 18 })] })] };
  if (sh) o.shading = sh;
  return new TableCell(o);
}
function bCell(text, w, sh) {
  const o = { borders, width: { size: w, type: WidthType.DXA }, margins: cm,
    children: [new Paragraph({ children: [new TextRun({ text: String(text || ""), font: "Arial", size: 18, bold: true })] })] };
  if (sh) o.shading = sh;
  return new TableCell(o);
}
function h1(text) { return new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun({ text, font: "Arial" })] }); }
function h2(text) { return new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun({ text, font: "Arial" })] }); }
function h3(text) { return new Paragraph({ heading: HeadingLevel.HEADING_3, children: [new TextRun({ text, font: "Arial" })] }); }
function p(text) { return new Paragraph({ spacing: { after: 100 }, children: [new TextRun({ text, font: "Arial", size: 22 })] }); }

// --- Parse validation.R to extract all tests ---
const valScript = fs.readFileSync('validation/validation.R', 'utf8');

function extractTests(script) {
  const tests = [];
  const lines = script.split('\n');
  let inCheck = false;
  let checkBuf = '';

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.match(/^check\(/) || line.match(/^skip_manual\(/)) {
      inCheck = true;
      checkBuf = line;
    } else if (inCheck) {
      checkBuf += '\n' + line;
    }
    if (inCheck) {
      let depth = 0;
      for (const ch of checkBuf) { if (ch === '(') depth++; if (ch === ')') depth--; }
      if (depth <= 0) {
        inCheck = false;
        const idM = checkBuf.match(/"([A-Z]+-[A-Z0-9-]+)"/);
        const testM = checkBuf.match(/,\s*"([^"]{5,})"/);
        const ursM = checkBuf.match(/urs_ref\s*=\s*"([^"]+)"/);
        const methodM = checkBuf.match(/method\s*=\s*"([^"]*)"/);
        const expectedM = checkBuf.match(/expected\s*=\s*"([^"]*)"/);
        const critM = checkBuf.match(/critical\s*=\s*(TRUE|FALSE)/);
        const isManual = checkBuf.startsWith('skip_manual');
        if (idM && testM) {
          tests.push({
            id: idM[1], test: testM[1].substring(0, 80),
            urs: ursM ? ursM[1] : '', method: methodM ? methodM[1].substring(0, 120) : '',
            expected: expectedM ? expectedM[1].substring(0, 120) : '',
            critical: critM ? critM[1] === 'TRUE' : false, type: isManual ? 'manual' : 'auto'
          });
        }
        checkBuf = '';
      }
    }
  }
  return tests;
}

const allTests = extractTests(valScript);

// Add loop-generated tests that use dynamic IDs (paste0 in R)
const conventions = ["STA", "CDI", "WIN", "DUT", "LOW", "ABB", "MIX"];
const convNames = ["standard", "cdisc", "winnonlin", "dutch", "lowercase", "abbrev", "mixed"];
for (let i = 0; i < conventions.length; i++) {
  for (const col of [["S", "subject"], ["T", "time"], ["C", "concentration"]]) {
    allTests.push({ id: `DAT-AD-${conventions[i]}-${col[0]}`, test: `Auto-detect ${col[1]}: ${convNames[i]} convention`,
      urs: "URS-DAT-02", method: `auto_detect_columns with ${convNames[i]} naming`, expected: `Correct ${col[1]} column detected`,
      critical: true, type: "auto" });
  }
}
// Design tests
for (const des of ["2x2", "2x2x3", "2x3x3", "2x2x4", "parallel"]) {
  allTests.push({ id: `PWR-DES-${des.replace(/x/g,'')}`, test: `Design ${des} computes sample size`,
    urs: "URS-PWR-03", method: `sampleN.TOST with design='${des}'`, expected: "Valid sample size > 0",
    critical: true, type: "auto" });
}
// Help variable tests
const helpNames = ["data_format","column_mapping","lloq","blq_rules","what_is_nca","admin_route",
  "trapezoidal","lambda_z","r2adj","steady_state","dose_norm","what_is_be","log_transform","ci_level","be_limits"];
for (let i = 0; i < helpNames.length; i++) {
  allTests.push({ id: `UI-HLP-${i+1}`, test: `Help topic exists: help_${helpNames[i]}`,
    urs: "URS-UI-01", method: `grep for help_${helpNames[i]} in help_system.R`, expected: `help_${helpNames[i]} defined`,
    critical: false, type: "auto" });
}
// Deduplicate by ID
const seenIds = new Set();
const dedupedTests = [];
for (const t of allTests) {
  if (!seenIds.has(t.id)) {
    seenIds.add(t.id);
    dedupedTests.push(t);
  }
}
console.log(`Extracted ${dedupedTests.length} raw, deduplicated to ${dedupedTests.length} tests (${dedupedTests.filter(t=>t.type==='auto').length} auto, ${dedupedTests.filter(t=>t.type==='manual').length} manual)`);

// Group by section
function getSection(id) {
  if (id.startsWith("IQ")) return "IQ";
  if (id.startsWith("DAT")) return "DAT";
  if (id.startsWith("NCA")) return "NCA";
  if (id.startsWith("BE")) return "BE";
  if (id.startsWith("PWR")) return "PWR";
  if (id.startsWith("EXP")) return "EXP";
  if (id.startsWith("UI")) return "UI";
  if (id.startsWith("MAN")) return "MAN";
  return "OTHER";
}

const sections = {
  IQ: { title: "Installation Qualification", tests: [] },
  DAT: { title: "Data Handling (OQ)", tests: [] },
  NCA: { title: "Non-Compartmental Analysis (OQ/PQ)", tests: [] },
  BE: { title: "Bioequivalence (OQ/PQ)", tests: [] },
  PWR: { title: "Power & Sample Size (OQ)", tests: [] },
  EXP: { title: "Export & Reproducibility (OQ)", tests: [] },
  UI: { title: "Usability & Code Quality (OQ)", tests: [] },
  MAN: { title: "Manual Tests (PQ)", tests: [] },
};

dedupedTests.forEach(t => {
  const sec = getSection(t.id);
  if (sections[sec]) sections[sec].tests.push(t);
});

// --- Build document ---
const children = [];

// Title page
children.push(new Paragraph({ spacing: { before: 3000 }, children: [] }));
children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 200 },
  children: [new TextRun({ text: "NCA Assistant v1.0", font: "Arial", size: 48, bold: true, color: "2C3E50" })] }));
children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 100 },
  children: [new TextRun({ text: "IQ/OQ/PQ Validation Protocol", font: "Arial", size: 36, color: "2C3E50" })] }));
children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 400 },
  children: [new TextRun({ text: "Version 1.3", font: "Arial", size: 28, color: "7F8C8D" })] }));

const meta = [
  ["Document ID", "NCA-IQOQPQ-001"],
  ["Version", "1.2"],
  ["Status", "Draft"],
  ["Author", "Radboud Applied Pharmacometrics"],
  ["Date", new Date().toISOString().split('T')[0]],
  ["Repo version", "master (current HEAD at time of execution)"],
];
const metaCw = [3000, 6360];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: metaCw,
  rows: meta.map(([k, v]) => new TableRow({ children: [bCell(k, metaCw[0], altFill), tCell(v, metaCw[1])] }))
}));

children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(new TableOfContents("Table of Contents", { hyperlink: true, headingStyleRange: "1-3" }));
children.push(new Paragraph({ children: [new PageBreak()] }));

// 1. Purpose & Architecture
children.push(h1("1. Purpose"));
children.push(p("This protocol defines the Installation Qualification (IQ), Operational Qualification (OQ), and Performance Qualification (PQ) procedures for NCA Assistant v1.0. It specifies every test to be executed, the expected results, and the acceptance criteria."));
children.push(p("The validation uses a single-script architecture: all automated tests are executed by running Rscript validation/validation.R from the project root. The script generates a PDF validation report (Attachment A) containing full results, environment details, file integrity hashes, and the complete script source code."));

children.push(h2("1.1 Deviation Handling"));
children.push(p("Any deviation from expected results shall be documented in the Deviations table (Section 7). CRITICAL test failures block qualification. SUPPORTIVE test failures require a documented risk assessment but do not block qualification."));

children.push(h2("1.2 Execution Log"));
const logCw = [2340, 2340, 2340, 2340];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: logCw,
  rows: [
    new TableRow({ children: ["Start Date/Time", "End Date/Time", "Interruptions", "Observations"].map((t, i) => hCell(t, logCw[i])) }),
    new TableRow({ children: logCw.map(w => tCell("", w)) }),
  ]
}));

children.push(h2("1.3 Test Environment"));
const envCw = [3000, 6360];
const envRows = [
  ["R version", ""], ["Platform", ""], ["Operating System", ""], ["NonCompart version", ""],
  ["PowerTOST version", ""], ["nlme version", ""], ["digest version", ""],
  ["Executed by", ""], ["Institution", ""],
];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: envCw,
  rows: [
    new TableRow({ children: ["Component", "Value"].map((t, i) => hCell(t, envCw[i])) }),
    ...envRows.map(([k, v]) => new TableRow({ children: [bCell(k, envCw[0], altFill), tCell(v, envCw[1])] }))
  ]
}));

children.push(h2("1.4 Document Control"));
children.push(p("The validated configuration corresponds to the master branch of the Git repository github.com/robterheine/ncaassistant at the time of execution. The exact commit hash should be recorded in the test environment table above."));

// --- Test sections ---
const autoColWidths = [900, 2200, 1800, 2060, 1200, 600, 600];
// ID, Test, Method, Expected, URS, Class, Result

function makeAutoTable(tests) {
  return new Table({
    width: { size: 9360, type: WidthType.DXA }, columnWidths: autoColWidths,
    rows: [
      new TableRow({ children: ["ID", "Test", "Method", "Expected", "URS", "Class", "Result"].map((t, i) => hCell(t, autoColWidths[i])) }),
      ...tests.map((t, idx) => {
        const sh = idx % 2 === 1 ? altFill : undefined;
        return new TableRow({
          children: [
            bCell(t.id, autoColWidths[0], sh),
            tCell(t.test, autoColWidths[1], sh),
            tCell(t.method.substring(0, 120), autoColWidths[2], sh),
            tCell(t.expected.substring(0, 120), autoColWidths[3], sh),
            tCell(t.urs, autoColWidths[4], sh),
            tCell(t.critical ? "CRIT" : "SUPP", autoColWidths[5], sh),
            tCell("", autoColWidths[6], sh), // Result to be filled
          ]
        });
      })
    ]
  });
}

const manualColWidths = [800, 1700, 2200, 2060, 1200, 600, 800];
function makeManualTable(tests) {
  return new Table({
    width: { size: 9360, type: WidthType.DXA }, columnWidths: manualColWidths,
    rows: [
      new TableRow({ children: ["ID", "Test", "Procedure", "Expected", "URS", "Obs", "P/F"].map((t, i) => hCell(t, manualColWidths[i])) }),
      ...tests.map((t, idx) => {
        const sh = idx % 2 === 1 ? altFill : undefined;
        return new TableRow({
          children: [
            bCell(t.id, manualColWidths[0], sh),
            tCell(t.test, manualColWidths[1], sh),
            tCell(t.method.substring(0, 150), manualColWidths[2], sh),
            tCell(t.expected.substring(0, 120), manualColWidths[3], sh),
            tCell(t.urs, manualColWidths[4], sh),
            tCell("", manualColWidths[5], sh), // Observed
            tCell("", manualColWidths[6], sh), // Pass/Fail
          ]
        });
      })
    ]
  });
}

// Section 2: IQ
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(h1("2. Installation Qualification (IQ)"));
children.push(p(`This section verifies that the application and its dependencies are correctly installed. ${sections.IQ.tests.length} tests.`));
children.push(makeAutoTable(sections.IQ.tests));

// Section 3: OQ/PQ — Automated Tests
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(h1("3. Operational/Performance Qualification \u2014 Automated Tests"));
children.push(p("Execute all automated tests by running from the project root: Rscript validation/validation.R"));
children.push(p("Attach the generated validation_report.pdf as Attachment A to this protocol."));

const autoSections = ["DAT", "NCA", "BE", "PWR", "EXP", "UI"];
for (const sec of autoSections) {
  const s = sections[sec];
  if (s.tests.length === 0) continue;
  children.push(h2(`3.${autoSections.indexOf(sec) + 1} ${s.title} (${s.tests.length} tests)`));
  children.push(makeAutoTable(s.tests));
  children.push(p(""));
}

// Section 4: Manual Tests
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(h1("4. Manual Tests (PQ)"));
children.push(p("The following tests require execution via the application UI. For each test, follow the procedure, record observations, and mark PASS or FAIL."));
children.push(makeManualTable(sections.MAN.tests));

// Section 5: URS Traceability Matrix
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(h1("5. URS Traceability Matrix"));
children.push(p("Bidirectional traceability: every URS requirement maps to one or more tests, and every test references a URS requirement."));

const ursIds = [...new Set(dedupedTests.map(t => t.urs))].sort();
const traceCw = [1800, 5560, 1000, 1000];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: traceCw,
  rows: [
    new TableRow({ children: ["URS ID", "Test IDs (Automated)", "Manual IDs", "Count"].map((t, i) => hCell(t, traceCw[i])) }),
    ...ursIds.map((urs, idx) => {
      const autoIds = dedupedTests.filter(t => t.urs === urs && t.type === 'auto').map(t => t.id);
      const manIds = dedupedTests.filter(t => t.urs === urs && t.type === 'manual').map(t => t.id);
      const sh = idx % 2 === 1 ? altFill : undefined;
      return new TableRow({
        children: [
          bCell(urs, traceCw[0], sh),
          tCell(autoIds.join(", ") || "\u2014", traceCw[1], sh),
          tCell(manIds.join(", ") || "\u2014", traceCw[2], sh),
          tCell(String(autoIds.length + manIds.length), traceCw[3], sh),
        ]
      });
    })
  ]
}));

// Section 6: Deviations
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(h1("6. Deviations"));
children.push(p("Record all deviations from expected results. Each deviation requires a risk assessment and resolution."));
const devCw = [600, 1200, 2800, 2400, 1160, 1200];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: devCw,
  rows: [
    new TableRow({ children: ["No.", "Test ID", "Description", "Risk Assessment", "Resolution", "Status"].map((t, i) => hCell(t, devCw[i])) }),
    ...[1, 2, 3, 4, 5].map(n => new TableRow({ children: devCw.map(w => tCell("", w)) }))
  ]
}));

// Section 7: Summary & Approval
children.push(new Paragraph({ children: [new PageBreak()] }));
children.push(h1("7. Summary and Approval"));

// Phase totals
const nAuto = dedupedTests.filter(t => t.type === 'auto').length;
const nManual = dedupedTests.filter(t => t.type === 'manual').length;
const nCrit = dedupedTests.filter(t => t.critical).length;
const nSupp = dedupedTests.filter(t => !t.critical && t.type === 'auto').length;

children.push(h2("7.1 Test Totals"));
const totCw = [4680, 4680];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: totCw,
  rows: [
    new TableRow({ children: ["Metric", "Count"].map((t, i) => hCell(t, totCw[i])) }),
    new TableRow({ children: [tCell("Total tests", totCw[0]), tCell(String(dedupedTests.length), totCw[1])] }),
    new TableRow({ children: [tCell("Automated tests", totCw[0], altFill), tCell(String(nAuto), totCw[1], altFill)] }),
    new TableRow({ children: [tCell("Manual tests", totCw[0]), tCell(String(nManual), totCw[1])] }),
    new TableRow({ children: [tCell("CRITICAL tests", totCw[0], altFill), tCell(String(nCrit), totCw[1], altFill)] }),
    new TableRow({ children: [tCell("SUPPORTIVE tests", totCw[0]), tCell(String(nSupp), totCw[1])] }),
  ]
}));

children.push(h2("7.2 Acceptance Criteria"));
children.push(p("Overall acceptance: ALL CRITICAL tests must pass. SUPPORTIVE test failures require documented risk assessment (Section 6) but do not block qualification."));

children.push(h2("7.3 Revalidation Triggers"));
children.push(p("Revalidation is required upon: R major version upgrade, core package updates (NonCompart, PowerTOST, nlme), functional code changes (Category B/C), or deployment environment changes."));

children.push(h2("7.4 Final Approval"));
const sigCw = [2340, 2340, 2340, 2340];
children.push(new Table({
  width: { size: 9360, type: WidthType.DXA }, columnWidths: sigCw,
  rows: [
    new TableRow({ children: ["Role", "Name", "Signature", "Date"].map((t, i) => hCell(t, sigCw[i])) }),
    new TableRow({ children: [tCell("Executed by", sigCw[0]), ...sigCw.slice(1).map(w => tCell("", w))] }),
    new TableRow({ children: [tCell("Reviewed by", sigCw[0], altFill), ...sigCw.slice(1).map(w => tCell("", w, altFill))] }),
    new TableRow({ children: [tCell("Approved by", sigCw[0]), ...sigCw.slice(1).map(w => tCell("", w))] }),
    new TableRow({ children: [tCell("QA sign-off", sigCw[0], altFill), ...sigCw.slice(1).map(w => tCell("", w, altFill))] }),
  ]
}));

children.push(p(""));
children.push(p("Validation outcome: \u25A1 QUALIFIED   \u25A1 NOT QUALIFIED   \u25A1 QUALIFIED WITH DEVIATIONS"));

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
        margin: { top: 1080, right: 1080, bottom: 1080, left: 1080 }
      }
    },
    headers: {
      default: new Header({ children: [new Paragraph({
        children: [new TextRun({ text: "NCA Assistant v1.0 \u2014 IQ/OQ/PQ Protocol v1.3", font: "Arial", size: 18, color: "999999" })],
        alignment: AlignmentType.RIGHT
      })] })
    },
    footers: {
      default: new Footer({ children: [new Paragraph({
        children: [new TextRun({ text: "Page ", font: "Arial", size: 18 }), new TextRun({ children: [PageNumber.CURRENT], font: "Arial", size: 18 }),
                   new TextRun({ text: "  |  CONFIDENTIAL", font: "Arial", size: 16, color: "999999" })],
        alignment: AlignmentType.CENTER
      })] })
    },
    children
  }]
});

Packer.toBuffer(doc).then(buffer => {
  fs.writeFileSync("validation/NCA_Assistant_IQOQPQ_v1.3.docx", buffer);
  console.log("IQ/OQ/PQ Protocol generated: validation/NCA_Assistant_IQOQPQ_v1.3.docx");
});
