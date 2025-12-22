(* ::Package:: *)

(*
  Package: EliminationMatrixGenerator
  Description: Generates didactic materials for solving linear systems via the elimination method.
  Refactored to minimize code duplication while preserving all pedagogical steps.
  Updated: Dynamic step numbering added.
*)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`", "MojeGeneratory`Common`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

Gen01::usage = "Gen01[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc eliminačnou metódou (sčítaním rovníc).

diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

Gen01::baddiff  = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::fail     = "Nepodarilo sa vygenerovať vhodný príklad.";

Options[Gen01] = {SolutionType -> Automatic, Visualization -> False};

Begin["`Private`"];

(* ~-~-~ DIFFICULTY CONFIGURATION ~-~-~ *)
$diffConfig = <|
  "EASY" -> <|"CoeffRange" -> 5, "Bound" -> 60|>,
  "MEDIUM" -> <|"CoeffRange" -> 5, "Bound" -> 90|>,
  "HARD" -> <|"CoeffRange" -> 5, "Bound" -> 180|>
|>;
diffConfig[diff_] := Lookup[$diffConfig, diff, $diffConfig["MEDIUM"]];

(* ~-~-~ COMMON CELL PRINTING HELPERS ~-~-~ *)
printSectionCell[str_] := CellPrintStyle[str, "Section"];
printSubsectionCell[str_] := CellPrintStyle[str, "Subsection"];
printTextCell[str_] := CellPrint @ Cell[str, "Text", ShowStringCharacters -> False];
printTextExprCell[expr_] := CellPrint @ Cell[BoxData @ ToBoxes[expr, StandardForm], "Text", ShowStringCharacters -> False];
printFormulaCell[expr_] := CellPrint @ Cell[BoxData @ ToBoxes[expr, StandardForm], "DisplayFormula", ShowStringCharacters -> False];

(* ~-~-~ COMMON FORMATTING HELPERS ~-~-~ *)
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];
buildTermsRow[terms_List] := Module[{out = {}, first = True, c, v, t},
  If[terms === {} || AllTrue[terms[[All, 1]], # == 0 &], Return[tf[0]]];

  Do[
    {c, v} = terms[[i]];
    If[c == 0, Continue[]];

    t = If[v === None,
      tf[Abs[c]],
      tf[If[Abs[c] === 1, v, Abs[c] v]]
    ];

    If[first,
      If[c < 0, out = Join[out, {"-", t}], out = Join[out, {t}]];
      first = False;,
      out = Join[out, {If[c < 0, " - ", " + "], t}];
    ];
    , {i, 1, Length[terms]}];

  If[out === {}, tf[0], Row[out]]
];

(* ~-~-~ NEZARADENE ~-~-~ *)
makeStepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);

renderStepItem[item_] := Which[ StringQ[item], printTextCell[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]];

(* zarovnaný grid rovníc *)
alignedEquations[data_, breaks_List : {}, gap_ : 1.25] := Module[
  {eq = Style["=", 16], bar = Style["|", GrayLevel[.25]], base = 0.6, n, rowGaps, stepRow},

  n = Length[data];

  stepRow[{lhs_, rhs_, note_}] := {
    lhs, eq, rhs,
    If[note === "" || note === None, "",
      Style[Row[{bar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]
    ]
  };
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  Grid[
    stepRow /@ data,
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, rowGaps},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* formát ľavej strany rovnice *)
formatEquationLHS[coeffs_List, choice_, vars_List] := Module[{terms = {}, first = True, choiceVar, addTerm},
  choiceVar = Which[
    MemberQ[vars, choice], choice,
    choice === "X" && Length[vars] >= 1, vars[[1]],
    choice === "Y" && Length[vars] >= 2, vars[[2]],
    choice === "Z" && Length[vars] >= 3, vars[[3]],
    True, None
  ];
  addTerm[c_, v_] := If[c =!= 0,
    Module[{sign, t},
      sign = If[first, If[c < 0, "-", ""], If[c < 0, " - ", " + "]];
      t = If[Abs[c] === 1, v, Abs[c] v];
      If[v === choiceVar, t = highlightTerm[t]];
      If[sign =!= "", AppendTo[terms, sign]];
      AppendTo[terms, tf[t]];
      first = False;
    ]
  ];
  MapThread[addTerm, {coeffs, vars}];
  If[terms === {}, tf[0], Row[terms]]
];

(* formátovanie ľavej strany pre substitúciu *)
formatSubstLHS[row_, vars_, solMap_, unknownVar_, evalMode_:False] := Module[{terms = {}, first = True, addTerm},

  addTerm[content_, sign_] := (
    AppendTo[
      terms,
      If[first,
        If[sign === -1, Row[{"-", content}], content],
        Row[{If[sign === -1, " - ", " + "], content}]
      ]
    ];
    first = False;
  );

  Do[
    With[{c = row[[i]], v = vars[[i]]},
      If[c =!= 0,
        If[v === unknownVar,
          addTerm[tf[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]],
          If[evalMode,
            With[{prod = Together[c * solMap[v]]},
              If[!PossibleZeroQ[prod], addTerm[tf[Abs[prod]], Sign[prod]]]
            ],
            addTerm[coeffVal[Abs[c], solMap[v]], Sign[c]]
          ]
        ]
      ]
    ],
    {i, 1, Length[vars]}
  ];

  If[terms === {}, tf[0], Row[terms]]
];

(* text znamienka medzi členmi *)
signBtwTerms[c_] := If[c < 0, " - ", " + "];

(* zátvorky pre zápornú hodnotu *)
wrapNegValue[val_] := If[NumericQ[val] && val < 0,  Row[{"(", tft[val], ")"}],  tft[val]];

(* notes pre [ + k / - k / · k / : k ] *)
addNote[k_] := Which[PossibleZeroQ[k], "", TrueQ[k > 0], Row[{"+ ", tft[k]}], TrueQ[k < 0], Row[{"- ", tft[Abs[k]]}], True, ""];
scalarNote[symbol_String, k_] := Which[PossibleZeroQ[k - 1], "", True, Row[{symbol, " ", wrapNegValue[k]}]];
multNote[m_] := scalarNote["\[CenterDot]", m];
divNote[d_] := scalarNote[":", d];

(* note k substitúcii | x ↦ value, y ↦ value, ... *)
substNote[solMap_, remVars_, row_, vars_] := Module[{usedVars},
  usedVars = Select[remVars, row[[First@First@Position[vars, #]]] =!= 0 &];
  If[usedVars === {}, "", Row[Riffle[(Row[{#, " \[Rule] ", tft[solMap[#]]}] & /@ usedVars), ", "]]]
];

(* formátovanie ľavej strany pre kontrolu správnosti *)
checkRowTerms[row_, sol_] := Module[{n = Length[row], first = True, out = {}},
  Do[
    If[row[[j]] === 0, Continue[]];
    If[first,
      out = Join[out,
        If[row[[j]] < 0,
          {"-", tf[Abs[row[[j]]]], "\[CenterDot]", wrapNegValue[sol[[j]]]},
          {tf[row[[j]]], "\[CenterDot]", wrapNegValue[sol[[j]]]}
        ]
      ];
      first = False;
      , out = Join[out, {signBtwTerms[row[[j]]], tf[Abs[row[[j]]]], "\[CenterDot]", wrapNegValue[sol[[j]]]}];
    ];
    , {j, 1, n}];
  If[out === {}, tf[0], Row[out]]
];

(* pridanie krokov kontroly správnosti *)
addCorrectnessCheck[content_, A_, b_, vars_, sol_] := Module[
  {c = content, n = Length[vars], solN, row, lhs, prodRow},

  solN = Together /@ sol;

  c = Append[c, makeStepHeader["Skúška správnosti"]];
  c = Append[c, "Skúšku správnosti robíme dosadením vypočítaných hodnôt neznámych do všetkých rovníc:"];

  Do[
    row = A[[i]];
    lhs = Together[row.solN];

    prodRow = Module[{out = {}, first = True, p},
      Do[
        If[row[[j]] === 0, Continue[]];
        p = Together[row[[j]]*solN[[j]]];
        If[first,
          out = Join[out, {tf[p]}]; first = False;,
          out = Join[out, {signBtwTerms[row[[j]]], tf[Abs[p]]}]
        ];
        , {j, 1, n}];
      If[out === {}, tf[0], Row[out]]
    ];

    c = Join[c, {
      Row[{"ĽS" <> ToString[i] <> " = ", checkRowTerms[row, solN], " = ", prodRow, " = ", tf[lhs]}],
      Row[{"PS" <> ToString[i] <> " = ", tf[b[[i]]]}],
      "ĽS" <> ToString[i] <> " = PS" <> ToString[i]
    }];
    , {i, 1, n}];

  c
];

(* koeficient · hodnota pre dosadzovanie *)
coeffVal[coeff_, val_] := Which[coeff === 0, 0, coeff === 1, wrapNegValue[val], coeff === -1, Row[{"-", wrapNegValue[val]}], True, Row[{coeff, " \[CenterDot] ", wrapNegValue[val]}]];


(* kontrola, či sú čísla v povolenom rozsahu *)
numbersNiceQ[A_, b_, diff_] := Max[Abs @ Join[Flatten[A], Flatten[b]]] <= Lookup[$diffConfig, diff, $diffConfig["MEDIUM"]]["Bound"];

(* vynásobí koeficienty v termoch *)
scaleTerms[terms_List, k_Integer] := ({k #[[1]], #[[2]]} & /@ terms);

(* pripraví hard display rovnicu ekvivalentnú pôvodnej štandardnej rovnici *)
buildHardEq[row_, rhs_, vars_] := Module[
  {n = Length[vars], idxMove, cLeftPool, cLeft, varTerms, kept, moved, leftBase, rightBase},

  (* presuň aspoň 1 a najviac n-1 premenných na pravú stranu *)
  idxMove = RandomSample[Range[n], RandomChoice[Range[1, n - 1]]];

  (* pridaj nenulovú konštantu na ľavú stranu *)
  cLeftPool = DeleteCases[Range[-7, 7], 0];
  cLeft = RandomChoice[cLeftPool];

  varTerms = MapThread[List, {row, vars}];
  kept = varTerms[[Complement[Range[n], idxMove]]];
  moved = ({-#[[1]], #[[2]]} & /@ varTerms[[idxMove]]);

  (* ekvivalencia: ak pridáme cLeft naľavo, musíme ho pridať aj napravo *)
  leftBase = RandomSample @ Join[kept, {{cLeft, None}}];
  rightBase = RandomSample @ Join[moved, {{rhs + cLeft, None}}];

  <|"MoveIdx" -> idxMove, "CLeft" -> cLeft, "LeftBaseTerms" -> leftBase, "RightBaseTerms" -> rightBase|>
];

(* vytvorí iba HARD display z dát so štandardnými rovnicami *)
buildHardDisplay[data_Association, vars_] := Module[
  {A = data["A"], b = data["b"], n = Length[vars], ks, eqMeta = {}, eqDisp = {}, leftBaseAll = {}, rightBaseAll = {},
    leftMultAll = {}, rightMultAll = {}, m, i, lMult, rMult},

  ks = pickHardMultipliers15[n];

  Do[
    m = buildHardEq[A[[i]], b[[i]], vars];

    lMult = scaleTerms[m["LeftBaseTerms"], ks[[i]]];
    rMult = scaleTerms[m["RightBaseTerms"], ks[[i]]];

    AppendTo[eqMeta, <|"MoveIdx" -> m["MoveIdx"], "CLeft" -> m["CLeft"]|>];
    AppendTo[leftBaseAll, m["LeftBaseTerms"]];
    AppendTo[rightBaseAll, m["RightBaseTerms"]];
    AppendTo[leftMultAll, lMult];
    AppendTo[rightMultAll, rMult];

    AppendTo[eqDisp, {buildTermsRow[lMult], buildTermsRow[rMult], ""}];, {i, 1, n}
  ];

  Join[
    data,
    <|
      "HardQ" -> True,
      "Multipliers" -> ks,
      "EqDisplay" -> eqDisp,
      "HardMeta" -> eqMeta,
      "HardLeftBaseTerms" -> leftBaseAll,
      "HardRightBaseTerms" -> rightBaseAll
    |>
  ]
];

(* výber najvhodnejšej premennej na elimináciu *)
pickElimVar2[A_] := Module[{scores, best},
  scores = Table[
    Module[{c1 = A[[1, i]], c2 = A[[2, i]], lcm},
      If[c1 == 0 || c2 == 0, 9999,
        lcm = LCM[Abs[c1], Abs[c2]];
        lcm + If[(lcm/Abs[c1]) > 1 && (lcm/Abs[c2]) > 1, 1000, 0] (* Penalty if both need mult *)
      ]
    ], {i, 1, 2}];
  If[scores[[2]] < scores[[1]], 2, 1]
];
pickElimVar3[A_] := Module[{zp, scorePair, zeroCols, scoreZeroCol, baseScores},
  zp = zeroCoeff3[A];

  scorePair[c1_, c2_] := If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]];

  (* preferujeme premennú, ktorá už v niektorej rovnici chýba *)
  zeroCols = Select[Range[3], zp["ZeroRowsByCol"][[#]] =!= {} &];

  If[zeroCols =!= {},
    scoreZeroCol[j_] := Module[{zeroRows, nonZeroRows, c1, c2},
      zeroRows = zp["ZeroRowsByCol"][[j]];
      nonZeroRows = Complement[Range[3], zeroRows];

      Which[
        Length[nonZeroRows] >= 2,
        {c1, c2} = A[[nonZeroRows[[1 ;; 2]], j]];
        {0, scorePair[c1, c2]},
        Length[zeroRows] >= 2,
        {1, 0},
        True,
        {2, Infinity}
      ]
    ];

    Return[First @ MinimalBy[zeroCols, scoreZeroCol]];
  ];

  (* fallback: pôvodná logika – minimalizovať násobky pre (1,2) a (1,3) *)
  baseScores = Table[
    scorePair[A[[1, j]], A[[2, j]]] + scorePair[A[[1, j]], A[[3, j]]],
    {j, 1, 3}
  ];
  Ordering[baseScores, 1][[1]]
];
eliminationStart2[A_, b_, vars_] := Module[
  {idx, k1, k2, lcm, m1, m2, choice, targetVar, needsMult, content = {}, rows1, rows2},

  idx = pickElimVar2[A];
  targetVar = vars[[idx]];
  choice = If[idx == 1, "X", "Y"];

  k1 = A[[1, idx]]; k2 = A[[2, idx]];
  lcm = LCM[Abs[k1], Abs[k2]];
  m1 = lcm / Abs[k1];
  m2 = lcm / Abs[k2];
  If[Sign[k1] === Sign[k2], m2 = -m2];

  needsMult = !(Sign[k1] =!= Sign[k2] && m1 === 1 && m2 === 1);

  AppendTo[content, makeStepHeader["Príprava na elimináciu"]];
  If[needsMult,
    AppendTo[content, "Chceme vyrušiť premennú " <> ToString[targetVar] <> ". Rovnice preto prenásobíme tak, aby mali pri nej rovnaký koeficient s opačným znamienkom."];,
    AppendTo[content, "Koeficienty pri premennej " <> ToString[targetVar] <> " sú už opačné, takže môžeme hneď sčítať rovnice a premennú vyrušiť."];
  ];

  rows1 = {
    {formatEquationLHS[A[[1]], choice, vars], b[[1]], multNote[m1]},
    {formatEquationLHS[A[[2]], choice, vars], b[[2]], multNote[m2]}
  };

  rows2 = {
    {formatEquationLHS[m1 A[[1]], "", vars], m1 b[[1]], ""},
    {formatEquationLHS[m2 A[[2]], "", vars], m2 b[[2]], ""}
  };

  

  If[needsMult,
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]],
    AppendTo[content, alignedEquations[{
      {formatEquationLHS[A[[1]], choice, vars], b[[1]], ""},
      {formatEquationLHS[A[[2]], choice, vars], b[[2]], ""}
    }]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "A_mod" -> {m1 A[[1]], m2 A[[2]]}, "b_mod" -> {m1 b[[1]], m2 b[[2]]}|>
];

(* výber najvhodnejšej dvojice na elimináciu v 3x3 *)
pickBestElimPair[rowIdx_List, elimCol_Integer, A_] := Module[{pairs, scorePair},
  pairs = Subsets[rowIdx, {2}];
  scorePair[{i_, j_}] := Module[{c1 = A[[i, elimCol]], c2 = A[[j, elimCol]]},
    If[c1 == 0 || c2 == 0, Infinity, If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]]]
  ];
  First @ MinimalBy[pairs, scorePair]
];

(* výber najvhodnejšej rovnice na dosadenie *)
pickSubstRow3[zp_, elimCol_Integer, A_] := Module[
  {rows = Range[3], allNonZero, elimNonZero, score},

  (* najprv preferuj rovnicu bez núl a s nenulovým koeficientom pri elimVar *)
  allNonZero = Select[rows, zp["ZeroColsByRow"][[#]] === {} && A[[#, elimCol]] =!= 0 &];

  (* fallback: aspoň nenulový koeficient pri elimVar *)
  elimNonZero = Select[rows, A[[#, elimCol]] =!= 0 &];

  score[i_] := Module[{row = A[[i]], c = A[[i, elimCol]]},
    {If[Abs[c] == 1, 0, 1], Abs[c], Total[Abs[row]]}
  ];

  Which[
    allNonZero =!= {}, <|"Index" -> First @ MinimalBy[allNonZero, score], "AllNonZeroQ" -> True|>,
    elimNonZero =!= {}, <|"Index" -> First @ MinimalBy[elimNonZero, score], "AllNonZeroQ" -> False|>,
    True, $Failed
  ]
];

(* vyberie náhodne multiplikátory pre HARD *)
pickHardMultipliers15[n_] := Module[{ks},
  ks = RandomChoice[Range[5], n];
  While[Max[ks] < 4, ks = RandomChoice[Range[5], n]];
  ks
];


(* redukcia sústavy 3x3 na 2x2 *)
reduce3to2[A_, b_, vars_] := Module[
  {content = {}, zp, substPick, elimCol, elimVar, zeroRows, nonZeroRows, iKeep, rowIV, rhsIV, rowV, rhsV, remCols, remVars, A2, b2, twoCombosQ, pair, i1, i2},

  AppendTo[content, makeStepHeader["Redukcia sústavy 3x3 na 2x2"]];
  zp = zeroCoeff3[A];
  elimCol = pickElimVar3[A];
  elimVar = vars[[elimCol]];
  AppendTo[content, "Vyrušíme premennú " <> ToString[elimVar] <> ", aby sme získali sústavu 2×2."];

  zeroRows = zp["ZeroRowsByCol"][[elimCol]];
  nonZeroRows = Complement[Range[3], zeroRows];

  (* Strategy Selection *)
  If[Length[zeroRows] >= 1,
    twoCombosQ = False;
    If[Length[nonZeroRows] >= 2,
      iKeep = First[zeroRows];
      pair = pickBestElimPair[nonZeroRows, elimCol, A];
      {i1, i2} = pair;

      AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];
      With[{res = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars, "", ""]},
        content = Join[content, res["Content"]]; rowIV = res["Row"]; rhsIV = res["RHS"];
      ];

      rowV = A[[iKeep]]; rhsV = b[[iKeep]];
      AppendTo[content, Style["b) Rovnica bez vyrušovanej premennej (použijeme ju priamo):", Italic]];
      AppendTo[content, alignedEquations[{{formatEquationLHS[rowV, "", vars], rhsV, ""}}]];
      , (* premenná je eliminovaná v jednej dvojici *)
      {i1, i2} = zeroRows[[1 ;; 2]];
      rowIV = A[[i1]]; rhsIV = b[[i1]];
      rowV = A[[i2]]; rhsV = b[[i2]];

      AppendTo[content, Style["a) Rovnice bez vyrušovanej premennej (použijeme ich priamo):", Italic]];
      AppendTo[content, alignedEquations[{
        {formatEquationLHS[rowIV, "", vars], rhsIV, ""},
        {formatEquationLHS[rowV, "", vars], rhsV, ""}
      }]];
    ];
    , (* treba eliminovať v dvoch pároch *)
    twoCombosQ = True;
    AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];
    With[{res = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "", ""]},
      content = Join[content, res["Content"]]; rowIV = res["Row"]; rhsIV = res["RHS"];
    ];

    AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];
    With[{res = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "", ""]},
      content = Join[content, res["Content"]]; rowV = res["Row"]; rhsV = res["RHS"];
    ];
  ];

  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];
  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  substPick = pickSubstRow3[zp, elimCol, A];
  If[substPick === $Failed, Return[$Failed]];

  <|"Content" -> content, "A2" -> A2, "b2" -> b2, "remVars" -> remVars,
    "elimCol" -> elimCol, "elimVar" -> elimVar, "twoCombosQ" -> twoCombosQ,
    "SubstRowIndex" -> substPick["Index"], "SubstAllNonZeroQ" -> substPick["AllNonZeroQ"]
  |>
];

(* nulové koeficienty v matici 3x3 *)
zeroCoeff3[A_] := Module[{mask, zeroRowsByCol, zeroColsByRow},
  mask = Map[# == 0 &, A, {2}];
  zeroRowsByCol = Table[Flatten @ Position[mask[[All, j]], True], {j, 1, 3}];
  zeroColsByRow = Table[Flatten @ Position[mask[[i]], True], {i, 1, 3}];
  <|"Mask" -> mask, "ZeroRowsByCol" -> zeroRowsByCol, "ZeroColsByRow" -> zeroColsByRow|>
];

(* vyber parametrizácie minimalizáciou menovateľov: skúša x=t, y=t, z=t *)
chooseParametrization[A_, b_, vars_] := Module[
  {eqs, candidates, try, results},

  eqs = Thread[A.vars == b];
  candidates = List /@ vars; (* {{x},{y}} alebo {{x},{y},{z}} *)

  try[{v_}] := Module[{sol, rules, exprs, dens},
    sol = Quiet @ Solve[eqs /. v -> \[FormalT], Complement[vars, {v}], Reals];
    If[sol === {} || sol === $Failed, Return[Nothing]];

    rules = Join[{v -> \[FormalT]}, sol[[1]]];
    (* elementwise; chceme racionálne tvary *)
    exprs = Together /@ (vars /. rules);

    (* skóre = najväčší menovateľ (optimalizujeme len zlomky) *)
    dens = Denominator /@ Rationalize[exprs, 0];

    <|"Var" -> v, "Exprs" -> exprs, "Score" -> Max[dens]|>
  ];

  results = try /@ candidates;
  If[results === {}, $Failed, First @ MinimalBy[results, #Score &]]
];

(* výpis parametrického tvaru pre INFINITE *)
printInfiniteResult[A_, b_, vars_] := Module[
  {nVars = Length[vars], best, exprs, kBox, vecBox, condBox},

  best = chooseParametrization[A, b, vars];
  If[best === $Failed, Return[$Failed]];

  (* uprav formu: odstráň 1/q * (...) -> (...) / q *)
  exprs = Together /@ best["Exprs"];

  exprs = exprs //. {
    Times[Rational[1, q_Integer], e_] :> e/q,
    Times[Rational[-1, q_Integer], e_] :> -e/q
  };

  exprs = Simplify /@ exprs;

  printTextCell["Sústava má nekonečne veľa riešení. Riešenia zapíšeme parametricky."];
  printTextCell["Zvolíme voľnú premennú a označíme ju parametrom."];

  printTextCell["Parameter:"];
  printFormulaCell @ Grid[{{\[FormalT], "\[Element]", "\[DoubleStruckR]"}}, Alignment -> {{Center, Center, Left}}];

  printTextCell["Potom platí:"];
  printFormulaCell @ Grid[
    Table[{vars[[k]], "=", tf[exprs[[k]]]}, {k, 1, nVars}],
    Alignment -> {{Right, Center, Left}}
  ];

  vecBox = RowBox[{"[", RowBox[Riffle[ToBoxes[#, TraditionalForm] & /@ exprs, "; "]], "]"}];
  condBox = RowBox[{ToBoxes[\[FormalT], TraditionalForm], "\[Element]", "\[DoubleStruckR]"}];

  kBox = RowBox[{StyleBox["K", FontSlant -> "Italic"], "=", RowBox[{"{", RowBox[{vecBox, " ", "\[VerticalSeparator]", " ", condBox}], "}"}]}];
  CellPrint @ Cell[BoxData @ FormBox[kBox, TraditionalForm], "DisplayFormula", BaseStyle -> {FontSize -> 14}];

  <|"Type" -> "INFINITE"|>
];

(* generovanie sústavy *)
generateLinearSystem[dim_, diff_, solType_ : "ONE"] := Module[
  {r, nzPool, pickNZ, makeRow2NoZero, makeRow3NoZero, targetZeroCount, zeroRow, zeroCol, makeRow3PlannedZero, makeRow,
    zerosOkQ, fullRankQ, niceOkQ, attemptLimit, A, b, x0, contradiction, k, k1, k2, c1, c2, c3, vars, data, okQ,
    r1, r2, r3},

  r = Lookup[$diffConfig, diff, $diffConfig["MEDIUM"]]["CoeffRange"];

  (* nenulové koeficienty *)
  nzPool = Join[-Range[r, 1], Range[1, r]];
  pickNZ[] := RandomChoice[nzPool];
  makeRow2NoZero[] := {pickNZ[], pickNZ[]};
  makeRow3NoZero[] := {pickNZ[], pickNZ[], pickNZ[]};

  (* MEDIUM: presne 1 nula v celej matici 3x3, inak 0 núl *)
  targetZeroCount = If[dim == 3 && diff === "MEDIUM", 1, 0];
  {zeroRow, zeroCol} = If[dim == 3 && diff === "MEDIUM", {RandomInteger[{1, 3}], RandomInteger[{1, 3}]}, {None, None}];

  makeRow3PlannedZero[col_] := Module[{v = makeRow3NoZero[]}, v[[col]] = 0; v];

  makeRow[i_] := Which[
    dim == 2, makeRow2NoZero[],
    diff === "MEDIUM" && i === zeroRow, makeRow3PlannedZero[zeroCol],
    True, makeRow3NoZero[]
  ];

  zerosOkQ[m_] := Count[Flatten[m], 0] === targetZeroCount;
  fullRankQ[m_] := If[dim == 2, Det[m] =!= 0, MatrixRank[m] === dim];
  niceOkQ[m_, rhs_] := numbersNiceQ[m, rhs, diff];

  attemptLimit = 5000;

  okQ[] := zerosOkQ[A] && fullRankQ[A] && niceOkQ[A, b];

  If[solType === "ONE",
    Do[
      If[dim == 2,
        A = {makeRow[1], makeRow[2]},
        (r1 = makeRow[1]; r2 = makeRow[2]; r3 = makeRow[3];
        While[MatrixRank[{r1, r2}] < 2, r1 = makeRow[1]; r2 = makeRow[2]];
        While[MatrixRank[{r1, r2, r3}] < 3 || !zerosOkQ[{r1, r2, r3}], r3 = makeRow[3]];
        A = {r1, r2, r3})
      ];

      x0 = RandomInteger[{-5, 5}, dim];
      b = A . x0;

      If[okQ[], Break[]];
      , {attemptLimit}
    ];

    data = <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE", "PlannedZeroRC" -> If[dim == 3 && diff === "MEDIUM", {zeroRow, zeroCol}, None]|>;
    , contradiction = If[solType === "NONE", RandomChoice[{-5, -3, 3, 5}], 0];

  Do[
    If[dim == 2,
      (r1 = makeRow[1]; k = RandomChoice[{-3, -2, 2, 3}]; r2 = k r1;
      c1 = RandomInteger[{-10, 10}]; A = {r1, r2}; b = {c1, k c1 + contradiction}; k1 = k; k2 = 0),
      (r1 = makeRow[1]; r2 = makeRow[2];
      While[MatrixRank[{r1, r2}] < 2 || (diff === "MEDIUM" && Count[Join[r1, r2], 0] > 1), r1 = makeRow[1]; r2 = makeRow[2]];

      If[diff === "MEDIUM" && zeroRow === 3,
        (k1 = r2[[zeroCol]]; k2 = -r1[[zeroCol]]),
        (k1 = RandomChoice[{-2, -1, 1, 2}]; k2 = RandomChoice[{-2, -1, 1, 2}])
      ];

      r3 = k1 r1 + k2 r2;

      While[
        MatrixRank[{r1, r2}] < 2 ||
            (diff === "MEDIUM" && zeroRow === 3 && Count[r3, 0] =!= 1) ||
            (diff === "MEDIUM" && zeroRow =!= 3 && Count[r3, 0] =!= 0) ||
            (diff =!= "MEDIUM" && AnyTrue[r3, # == 0 &]),
        (r1 = makeRow[1]; r2 = makeRow[2];
        While[MatrixRank[{r1, r2}] < 2 || (diff === "MEDIUM" && Count[Join[r1, r2], 0] > 1), r1 = makeRow[1]; r2 = makeRow[2]];
        If[diff === "MEDIUM" && zeroRow === 3, k1 = r2[[zeroCol]]; k2 = -r1[[zeroCol]], k1 = RandomChoice[{-2, -1, 1, 2}]; k2 = RandomChoice[{-2, -1, 1, 2}]];
        r3 = k1 r1 + k2 r2)
      ];

      c1 = RandomInteger[{-5, 5}]; c2 = RandomInteger[{-5, 5}]; c3 = k1 c1 + k2 c2 + contradiction;
      A = {r1, r2, r3}; b = {c1, c2, c3})
    ];

    If[zerosOkQ[A] && niceOkQ[A, b], Break[]];
    , {attemptLimit}
  ];

  data = <|"A" -> A, "b" -> b, "type" -> solType|>
  ];

  If[diff === "HARD",
    vars = {x, y, z};
    data = buildHardDisplay[data, vars];
  ];
  data
];

(* ~-~-~ MAIN STEPS FUNCTIONS ~-~-~ *)

(* kroky pe jedno / žiadne / infinity riešení v 2x2 *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, elimVarStr, x, y, sumRHS, sumRow, keepIdx, elimIdx, keepVar, elimVar,
    stepsY, valKeep, valElim, stepsSub, rowOrig, rhsOrig, valProd, rhsRem},

  {x, y} = vars;
  data = eliminationStart2[A, b, vars];
  content = data["content"];
  elimVarStr = data["EliminatedVariable"];
  {m1, m2} = {data["m1"], data["m2"]};

  sumRow = Total[data["A_mod"]];
  sumRHS = Total[data["b_mod"]];

  elimIdx = If[elimVarStr == "X", 1, 2];
  keepIdx = 3 - elimIdx;
  elimVar = vars[[elimIdx]];
  keepVar = vars[[keepIdx]];

  AppendTo[content, makeStepHeader["Sčítanie rovníc"]];
  AppendTo[content, "Sčítame rovnice. Premenná " <> ToString[elimVar] <> " vypadne, lebo má opačné koeficienty."];

  stepsY = {};

  (* vizualizácia sčítania *)
  AppendTo[content, renderAddition2[data["A_mod"], data["b_mod"], vars]];

  AppendTo[stepsY, {sumRow[[keepIdx]] keepVar, sumRHS, ""}];
  AppendTo[content, alignedEquations[stepsY]];

  If[sumRow[[keepIdx]] == 1,
    AppendTo[content, "Po sčítaní sme dostali jednoduchú rovnicu, z ktorej hneď určíme hodnotu premennej " <> ToString[keepVar] <> "."],
    AppendTo[content, "Zostala nám rovnica s jednou premennou. Rovnicu upravíme tak, aby bola neznáma vyjadrená samostatne."];
    AppendTo[content, alignedEquations[{{sumRow[[keepIdx]] keepVar, sumRHS, divNote[sumRow[[keepIdx]]]}}]];
  ];

  valKeep = sumRHS / sumRow[[keepIdx]];
  AppendTo[content, highlightGrid[alignedEquations[{{keepVar, valKeep, ""}}]]];

  AppendTo[content, makeStepHeader["Dosadenie"]];
  AppendTo[content, "Vypočítanú hodnotu " <> ToString[keepVar] <> " dosadíme do jednej pôvodnej rovnice a dopočítame " <> ToString[elimVar] <> "."];

  rowOrig = A[[1]]; rhsOrig = b[[1]];
  stepsSub = {};
  AppendTo[stepsSub, {rowOrig[[1]] vars[[1]] + rowOrig[[2]] vars[[2]], rhsOrig, Row[{keepVar, " \[Rule] ", tft[valKeep]}]}];
  AppendTo[stepsSub, {formatSubstLHS[rowOrig, vars, <|keepVar->valKeep|>, elimVar, False], rhsOrig, ""}];

  valProd = rowOrig[[keepIdx]] * valKeep;
  Module[{noteShift},
    noteShift = addNote[-valProd];
    AppendTo[stepsSub, {formatSubstLHS[rowOrig, vars, <|keepVar->valKeep|>, elimVar, True], rhsOrig, noteShift}];
  ];

  rhsRem = rhsOrig - valProd;
  If[rowOrig[[elimIdx]] =!= 1,
    AppendTo[stepsSub, {rowOrig[[elimIdx]] elimVar, rhsRem, divNote[rowOrig[[elimIdx]]]}];
  ];

  valElim = rhsRem / rowOrig[[elimIdx]];
  AppendTo[stepsSub, {elimVar, tft[valElim], ""}];
  AppendTo[content, alignedEquations[stepsSub]];
  AppendTo[content, highlightGrid[alignedEquations[{{elimVar, tft[valElim], ""}}]]];

  (* skúška správnosti *)
  content = addCorrectnessCheck[
    content, A, b, vars,
    If[elimIdx == 1, {valElim, valKeep}, {valKeep, valElim}]
  ];

  <|"Content" -> content, "Solution" -> If[elimIdx == 1, {valElim, valKeep}, {valKeep, valElim}]|>
];
stepsNone2[A_, b_, vars_, includeConclusion_: True] := stepsSingular2[A, b, vars, "NONE", includeConclusion];
stepsInfinite2[A_, b_, vars_, includeConclusion_: True] := stepsSingular2[A, b, vars, "INFINITE", includeConclusion];
stepsSingular2[A_, b_, vars_, kind_String, includeConclusion_: True] := Module[
  {data, content, sumRHS, rowMod, rhsMod, rhsLine, introText, conclText},

  data = eliminationStart2[A, b, vars];
  content = data["content"];
  rowMod = data["A_mod"];
  rhsMod = data["b_mod"];
  sumRHS = Total[rhsMod];

  AppendTo[content, makeStepHeader["Sčítanie rovníc"]];

  introText = Switch[kind,
    "NONE",
    "Sčítaním rovníc overíme konzistenciu sústavy. Ak vznikne nepravdivá rovnosť 0 ≠ k, sústava nemá riešenie.",
    "INFINITE",
    "Rovnice sčítame. Ak vyjde pravdivá rovnosť 0 = 0, znamená to, že jedna rovnica je násobkom druhej a sústava má nekonečne veľa riešení.",
    _,
    "Sčítame rovnice."
  ];
  AppendTo[content, introText];
  AppendTo[content, renderAddition2[rowMod, rhsMod, vars]];

  rhsLine = Switch[kind,
    "NONE", sumRHS,
    "INFINITE", 0,
    _, sumRHS
  ];
  AppendTo[content, alignedEquations[{{0, rhsLine, ""}}]];

  If[includeConclusion,
    AppendTo[content, makeStepHeader["Záver"]];
    conclText = Switch[kind,
      "NONE",
      "Sústava nemá riešenie, pretože sme dostali nepravdivú rovnosť 0 ≠ " <> ToString[sumRHS] <> ".",
      "INFINITE",
      "Sústava má nekonečne veľa riešení, pretože sme dostali pravdivú rovnosť 0 = 0.",
      _,
      ""
    ];
    AppendTo[content, conclText];
  ];

  <|"Content" -> content, "Solution" -> kind|>
];

(* kroky pe jedno / žiadne / infinity riešení v 3x3 *)
stepsOne3[A_, b_, vars_, data_:<||>] := Module[
  {red, content = {}, A2, b2, remVars, sol2x2, solMap, finalVar, finalVal, row, rhs, knownSum, rhsShift, noteShift, coeffU, subSteps},

  (* hard normalizácia pred všetkými krokmi *)
  If[TrueQ[data["HardQ"]], content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  red = reduce3to2[A, b, vars];
  content = Join[content, red["Content"]];
  {A2, b2, remVars} = {red["A2"], red["b2"], red["remVars"]};

  AppendTo[content, makeStepHeader["Riešenie odvodenej sústavy 2×2"]];
  If[red["twoCombosQ"],
    AppendTo[content, "Dostali sme dve nové rovnice s dvoma neznámymi."],
    AppendTo[content, "Z jednej dvojice rovnic sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2."]
  ];
  AppendTo[content, alignedEquations[{
    {formatEquationLHS[A2[[1]], "", remVars], b2[[1]], ""},
    {formatEquationLHS[A2[[2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsOne2[A2, b2, remVars];
  If[sol2x2 === $Failed, Return[$Failed]];

  (* skúška pre 2x2 *)
  Module[{c2 = sol2x2["Content"], pos},
    pos = FirstPosition[
      c2,
      Style[s_String, Bold, ___] /; StringContainsQ[s, "Skúška správnosti"],
      Missing["NotFound"]
    ];
    If[pos === Missing["NotFound"],
      content = Join[content, c2],
      content = Join[content, Take[c2, pos[[1]] - 1]]
    ];
  ];
  stepsCounter --;
  solMap = AssociationThread[remVars -> sol2x2["Solution"]];
  finalVar = red["elimVar"];

  AppendTo[content, makeStepHeader["Dosadenie do pôvodnej rovnice"]];
  AppendTo[content, "Dosadíme známe premenné do jednej z pôvodých rovníc a dopočítame poslednú neznámu."];

  AppendTo[content,
    If[TrueQ[red["SubstAllNonZeroQ"]],
      "Na dosadenie zvolíme " <> ToString[red["SubstRowIndex"]] <> ". rovnicu (má nenulové koeficienty pri všetkých neznámych).",
      "Na dosadenie zvolíme " <> ToString[red["SubstRowIndex"]] <> ". rovnicu (má nenulový koeficient pri poslednej neznámej)."
    ]
  ];

  row = A[[red["SubstRowIndex"]]];
  rhs = b[[red["SubstRowIndex"]]];
  coeffU = row[[red["elimCol"]]];

  subSteps = {};
  AppendTo[subSteps, {formatEquationLHS[row, "", vars], rhs, substNote[solMap, remVars, row, vars]}];
  AppendTo[subSteps, {formatSubstLHS[row, vars, solMap, finalVar, False], rhs, ""}];

  knownSum = Together @ Total @ Table[If[i == red["elimCol"], 0, row[[i]] * solMap[vars[[i]]]], {i, 1, 3}];
  rhsShift = Together[rhs - knownSum];
  noteShift = addNote[-knownSum];

  AppendTo[subSteps, {formatSubstLHS[row, vars, solMap, finalVar, True], rhs, noteShift}];

  If[coeffU === 1,
    AppendTo[subSteps, {tf[finalVar], tf[rhsShift], ""}],
    AppendTo[subSteps, {tf[coeffU finalVar], tf[rhsShift], divNote[coeffU]}];
    AppendTo[subSteps, {tf[finalVar], tft[rhsShift/coeffU], ""}]
  ];
  finalVal = Together[rhsShift/coeffU];

  AppendTo[content, alignedEquations[subSteps]];
  AppendTo[content, highlightGrid[alignedEquations[{{finalVar, tf[finalVal], ""}}]]];

  (* skúška správnosti *)
  content = addCorrectnessCheck[
    content, A, b, vars,
    Table[If[i == red["elimCol"], finalVal, solMap[vars[[i]]]], {i, 1, 3}]
  ];


  <|"Content" -> content, "Solution" -> Table[If[i == red["elimCol"], finalVal, solMap[vars[[i]]]], {i, 1, 3}]|>
];
stepsNone3[A_, b_, vars_, data_:<||>] := stepsSingular3[A, b, vars, "NONE", data];
stepsInfinite3[A_, b_, vars_, data_:<||>] := stepsSingular3[A, b, vars, "INFINITE", data];
stepsSingular3[A_, b_, vars_, kind_String, data_:<||>] := Module[
  {red, content = {}, A2, b2, remVars, sol2x2, headerText, midText, conclText},

  (* hard normalizácia pred všetkými krokmi *)
  If[TrueQ[data["HardQ"]], content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  red = reduce3to2[A, b, vars];
  content = Join[content, red["Content"]];
  {A2, b2, remVars} = {red["A2"], red["b2"], red["remVars"]};

  AppendTo[content, makeStepHeader["Riešenie odvodenej sústavy 2×2"]];

  headerText = Switch[kind,
    "NONE",
    If[red["twoCombosQ"],
      "Dostali sme dve nové rovnice s dvoma neznámymi. Ak po úpravách vznikne spor (napr. 0 = nenulové číslo), pôvodná sústava 3×3 nemá riešenie.",
      "Z jednej dvojice rovníc sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2. Ak v nej vznikne spor, pôvodná sústava 3×3 nemá riešenie."
    ],
    "INFINITE",
    If[red["twoCombosQ"],
      "Dostali sme dve nové rovnice s dvoma neznámymi. Ak po úpravách vyjde totožná rovnica (napr. 0 = 0), znamená to nekonečne veľa riešení.",
      "Z jednej dvojice rovníc sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2. Ak v nej vyjde totožná rovnica (0 = 0), sústava má nekonečne veľa riešení."
    ],
    _,
    "Riešime odvodenej sústavu 2×2."
  ];
  AppendTo[content, headerText];

  AppendTo[content, alignedEquations[{
    {formatEquationLHS[A2[[1]], "", remVars], b2[[1]], ""},
    {formatEquationLHS[A2[[2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsSingular2[A2, b2, remVars, kind, False];

  If[sol2x2 === $Failed,
    midText = Switch[kind,
      "NONE", "Po úpravách dostávame spornú rovnicu (napr. 0 = k). Preto sústava nemá riešenie.",
      "INFINITE", "Sústava má nekonečne veľa riešení.",
      _, ""
    ];
    If[midText =!= "", AppendTo[content, midText];],
    content = Join[content, sol2x2["Content"]];
  ];

  AppendTo[content, makeStepHeader["Záver"]];
  conclText = Switch[kind,
    "NONE", "Keďže po eliminácii vznikol spor, pôvodná sústava 3×3 nemá riešenie.",
    "INFINITE", "Keďže po eliminácii vyšla totožná rovnica, pôvodná sústava 3×3 má nekonečne veľa riešení.",
    _, "Záver."
  ];
  AppendTo[content, conclText];

  <|"Content" -> content, "Solution" -> kind|>
];

(* ~-~-~ PART OF STEPS ~-~-~ *)

(* vizualizácia sčítania v 2x2 *)
renderAddition2[rowMod_, rhsMod_, vars_] := Module[{},
  alignedEquations[{{
    Row[{
      tf[rowMod[[1,1]] vars[[1]]],
      signBtwTerms[rowMod[[2,1]]], tf[Abs[rowMod[[2,1]]] vars[[1]]],
      signBtwTerms[rowMod[[1,2]]], tf[Abs[rowMod[[1,2]]] vars[[2]]],
      signBtwTerms[rowMod[[2,2]]], tf[Abs[rowMod[[2,2]]] vars[[2]]]
    }],
    Row[{rhsMod[[1]], signBtwTerms[rhsMod[[2]]], Abs[rhsMod[[2]]]}],
    ""
  }}]
];

(* vizualizácia normalizácie v 3x3 *)
hardNormalizationSteps3[A_, b_, vars_, data_Association] := Module[
  {content = {}, ks, leftBase, rightBase, k, lMult, rMult,
    coeffL, coeffR, constL, constR, addTerms, addNote,
    rowStd, rhsStd, rowsStd, rhsStdAll, gcds, rowDiv, rhsDiv,
    rowsFinal, rhsFinal, anyDivQ},

  (* z termov spraví { {ax,ay,az}, c } pre dané vars *)
  termsToCoeffsConst[terms_List] := Module[{cVar, cConst},
    cVar = Table[Total @ Cases[terms, {c_, vars[[j]]} :> c], {j, 1, Length[vars]}];
    cConst = Total @ Cases[terms, {c_, None} :> c];
    {cVar, cConst}
  ];

  (* poznámka typu +20 +16y + 12z (ako Row) *)
  addNoteFromTerms[terms_List] := Module[{pairs, pieces},
    pairs = Select[terms, MatchQ[#, {_, _}] && #[[1]] =!= 0 &];
    If[pairs === {}, Return[""]];

    pieces = Table[
      With[{c = pairs[[j, 1]], sym = pairs[[j, 2]]},
        Row[{
          If[c >= 0, "+", "-"],
          If[sym === None,
            tf[Abs[c]],
            tf[If[Abs[c] === 1, sym, Abs[c] sym]]
          ]
        }]
      ],
      {j, 1, Length[pairs]}
    ];

    Row @ Riffle[pieces, " "]
  ];

  ks = data["Multipliers"];
  leftBase = data["HardLeftBaseTerms"];
  rightBase = data["HardRightBaseTerms"];

  AppendTo[content, makeStepHeader["Normalizácia"]];
  AppendTo[content,
    "V každej rovnici presunieme všetky členy s neznámymi na ľavú stranu a všetky konštanty na pravú stranu."
  ];

  (* 1) úprava rovníc priamo na prenásobených rovniciach *)
  rowsStd = ConstantArray[0, {3, 3}];
  rhsStdAll = ConstantArray[0, 3];

  AppendTo[content,
    alignedEquations@Table[
      k = ks[[i]];
      lMult = scaleTerms[leftBase[[i]], k];
      rMult = scaleTerms[rightBase[[i]], k];

      {coeffL, constL} = termsToCoeffsConst[lMult];
      {coeffR, constR} = termsToCoeffsConst[rMult];

      (* čo pripočítame na obe strany: -constL a -pravé premenné *)
      addTerms = Join[
        If[constL =!= 0, {{-constL, None}}, {}],
        Join @@ Table[
          If[coeffR[[j]] =!= 0, {{-coeffR[[j]], vars[[j]]}}, {}],
          {j, 1, 3}
        ]
      ];
      addNote = addNoteFromTerms[addTerms];

      (* výsledok po úprave: (Lvars - Rvars) . vars = (Rconst - Lconst) *)
      rowStd = coeffL - coeffR;
      rhsStd = constR - constL;

      rowsStd[[i]] = rowStd;
      rhsStdAll[[i]] = rhsStd;

      {buildTermsRow[lMult], buildTermsRow[rMult], addNote}, {i, 1, 3}
    ]
  ];

  AppendTo[content, "Po úprave dostaneme:"];

  AppendTo[content,
    alignedEquations@Table[
      {formatEquationLHS[rowsStd[[i]], "", vars], rhsStdAll[[i]], ""},
      {i, 1, 3}
    ]
  ];

  (* 2) delenie spoločným deliteľom > 1 *)
  gcds = Table[GCD @@ Abs @ Join[rowsStd[[i]], {rhsStdAll[[i]]}], {i, 1, 3}];
  anyDivQ = AnyTrue[gcds, # > 1 &];

  rowsFinal = rowsStd;
  rhsFinal = rhsStdAll;

  If[anyDivQ,
    AppendTo[content,
      "Ak majú všetky koeficienty v rovnici spoločný deliteľ väčší ako 1, rovnicu vydelíme týmto číslom, aby sme dostali jednoduchší tvar."
    ];

    Do[
      If[gcds[[i]] > 1,
        rowDiv = rowsFinal[[i]]/gcds[[i]];
        rhsDiv = rhsFinal[[i]]/gcds[[i]];

        AppendTo[content, alignedEquations[{  {formatEquationLHS[rowsFinal[[i]], "", vars], rhsFinal[[i]], divNote[gcds[[i]]]}}]];

        rowsFinal[[i]] = rowDiv;
        rhsFinal[[i]] = rhsDiv;
      ],
      {i, 1, 3}
    ];

    AppendTo[content, "Po úpravách dostaneme sústavu rovníc v štandardnom tvare, pripravenú na riešenie eliminačnou metódou:"];

    AppendTo[content,
      alignedEquations@Table[
        {formatEquationLHS[rowsFinal[[i]], "", vars], rhsFinal[[i]], ""},
        {i, 1, 3}
      ]
    ];
  ];
  content
];

(* vizualizácia eliminácie dvoch rovníc v 3x3 *)
reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_, tagA_, tagB_] := Module[
  {content = {}, valA = rowA[[elimCol]], valB = rowB[[elimCol]], choiceStr = {"X", "Y", "Z"}[[elimCol]],
    lcm, m1, m2, rowA2, rhsA2, rowB2, rhsB2, newRow, newRHS, rows1, rows2},

  If[valA == 0 || valB == 0,
    AppendTo[content, alignedEquations[{
      {formatEquationLHS[rowA, choiceStr, vars], rhsA, ""},
      {formatEquationLHS[rowB, choiceStr, vars], rhsB, ""}
    }]];
    If[valB == 0, {newRow, newRHS} = {rowB, rhsB}, {newRow, newRHS} = {rowA, rhsA}];,

    lcm = LCM[Abs[valA], Abs[valB]];
    m1 = lcm/Abs[valA]; m2 = lcm/Abs[valB];
    If[Sign[valA] == Sign[valB], m2 = -m2];

    (* 1) povodne rovnice + poznamky k nasobeniu *)
    rows1 = {
      {formatEquationLHS[rowA, choiceStr, vars], rhsA, multNote[m1]},
      {formatEquationLHS[rowB, choiceStr, vars], rhsB, multNote[m2]}
    };

    (* 2) prenásobené rovnice (medzikrok) *)
    rowA2 = m1 rowA; rhsA2 = m1 rhsA;
    rowB2 = m2 rowB; rhsB2 = m2 rhsB;

    rows2 = {
      {formatEquationLHS[rowA2, "", vars], rhsA2, ""},
      {formatEquationLHS[rowB2, "", vars], rhsB2, ""}
    };

    (* 2 a 2: medzi dvojicami vacsia medzera, vnutri dvojic mensia *)
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]];

    (* 3) výsledok sčítania *)
    newRow = rowA2 + rowB2;
    newRHS = rhsA2 + rhsB2;
  ];

  AppendTo[content,
    alignedEquations[{{Style[formatEquationLHS[newRow, "", vars], Darker[Green, 0.2]], Style[newRHS, Darker[Green, 0.2]], ""}}]
  ];

  <|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>
];

(* ~-~-~ GRAPHICAL VISUALIZATIONS ~-~-~ *)
visualize2[A_, b_, vars_, sol_] := Module[
  {x, y, pt, xrange, yrange, seg, center, subtitle, range = 10,
    lineStyles, lineLabels, extraLegStyles, extraLegLabels, legend},

  printTextCell[" "];

  {x, y} = vars;

  (* určenie bodu prieniku a rozsahu *)
  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    range = 10;
    xrange = center[[1]] + {-range, range};
    yrange = center[[2]] + {-range, range};
    subtitle = "Priamky sa pretínajú v jednom bode (riešenie sústavy).",

    pt = None;
    xrange = {-10, 10};
    yrange = {-10, 10};
    subtitle = If[sol === "NONE",
      "Priamky sú rovnobežné, nepretínajú sa – sústava nemá riešenie.",
      "Priamky sú totožné (prekrývajú sa) – sústava má nekonečne veľa riešení."
    ]
  ];

  seg[row_, rhs_] := With[{a = row[[1]], bb = row[[2]]},
    If[bb != 0,
      Line[{{xrange[[1]], (rhs - a*xrange[[1]])/bb}, {xrange[[2]], (rhs - a*xrange[[2]])/bb}}],
      Line[{{rhs/a, yrange[[1]]}, {rhs/a, yrange[[2]]}}]
    ]
  ];

  printTextExprCell[subtitle];

  lineStyles =
      If[sol === "INFINITE",
        {Directive[Magenta, AbsoluteThickness[2], Opacity[0.9]], Directive[Blue, AbsoluteThickness[2], Opacity[0.9], Dashing[0.05]]},
        {Directive[Magenta, Thick],Directive[Blue, Thick]}
      ];

  lineLabels = {
    tf[A[[1, 1]] x + A[[1, 2]] y == b[[1]]],
    tf[A[[2, 1]] x + A[[2, 2]] y == b[[2]]]
  };

  (* legenda pre priesečník *)
  {extraLegStyles, extraLegLabels} =
      If[pt =!= None, {
        {Directive[Black]},
        {Row[{"prienik: [",
          TraditionalForm @ Together[pt[[1]]], ", ",
          TraditionalForm @ Together[pt[[2]]], "]"}]}
      }, {{}, {}}
      ];

  legend = LineLegend[
    Join[lineStyles, extraLegStyles],
    Join[lineLabels, extraLegLabels],
    LegendMarkerSize -> {50, 20},
    LegendMarkers -> Join[ {None, None},
      If[pt =!= None, {Graphics[{Black, Disk[]}, ImageSize -> 10]}, {}]
    ]
  ];

  printFormulaCell @ Legended[
    Graphics[
      { {lineStyles[[1]], seg[A[[1]], b[[1]]]},
        {lineStyles[[2]], seg[A[[2]], b[[2]]]},
        If[pt =!= None,
          { {Black, Thick, Circle[pt, 0.4]}, {Green, PointSize[0.02], Point[pt]}},
          {}
        ]
      },
      PlotRange -> {xrange, yrange},
      Axes -> True,
      GridLines -> Automatic,
      ImageSize -> Medium
    ],
    legend
  ]
];
visualize3[A_, b_, vars_, sol_] := Module[
  {x, y, z, range = 10, xmin, xmax, ymin, ymax, zmin, zmax,
    n1, n2, n3, d1, d2, d3, inter, best, subtitle,
    planes, mark, plot, eqLbl, planeStyles, planeLabels,
    extraLegStyles, extraLegLabels, legend},

  printTextCell[" "];

  {x, y, z} = vars;
  {xmin, xmax} = {-range, range};
  {ymin, ymax} = {-range, range};
  {zmin, zmax} = {-range, range};

  n1 = N @ A[[1]]; d1 = N @ b[[1]];
  n2 = N @ A[[2]]; d2 = N @ b[[2]];
  n3 = N @ A[[3]]; d3 = N @ b[[3]];

  inter = systemIntersection3[A, b, vars];
  best = If[inter["Type"] === "LINE", chooseParametrization[A, b, vars], $Failed];

  subtitle = Switch[inter["Type"],
    "POINT", "Tri roviny majú spoločný prienik v jednom bode (riešenie sústavy).",
    "LINE",  "Tri roviny majú spoločný prienik – priamku (nekonečne veľa riešení).",
    "PLANE", "Všetky tri rovnice opisujú tú istú rovinu (nekonečne veľa riešení).",
    "NONE",  "Roviny nemajú spoločný prienik všetkých troch naraz (sústava nemá riešenie).",
    _,       "Prienik sa nepodarilo jednoznačne určiť."
  ];

  (* rovnako ako v 2D *)
  printTextExprCell[subtitle];

  (* popisy rovín do legendy *)
  eqLbl[row_, rhs_] := tf[row.{x, y, z} == rhs];
  planeLabels = {eqLbl[A[[1]], b[[1]]], eqLbl[A[[2]], b[[2]]], eqLbl[A[[3]], b[[3]]]};
  planeStyles = {Cyan, Magenta, Yellow};

  planes = ContourPlot3D[
    {n1.{x, y, z} == d1, n2.{x, y, z} == d2, n3.{x, y, z} == d3},
    {x, xmin, xmax}, {y, ymin, ymax}, {z, zmin, zmax},
    Mesh -> None,
    PlotPoints -> 25,
    PerformanceGoal -> "Speed",
    ContourStyle -> {
      Directive[Cyan, Opacity[0.4]],
      Directive[Magenta, Opacity[0.4]],
      Directive[Yellow, Opacity[0.4]]
    },
    BoundaryStyle -> None
  ];

  (* zvýraznenie prieniku *)
  mark = Graphics3D @ Switch[inter["Type"],
    "POINT", {Black, PointSize[0.03], Point[N @ inter["Point"]], Black, Sphere[N @ inter["Point"], 0.35]},
    "LINE", Module[{p0, v},
      p0 = N @ inter["Point"];
      v  = 20 N @ inter["Dir"];
      {Black, Specularity[White, 20], Tube[{p0 - v, p0 + v}, 0.18]}
    ],
    _, {}
  ];

  plot = Show[planes, mark,
    PlotRange -> {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}},
    BoxRatios -> {1, 1, 1}, Axes -> True,
    AxesLabel -> {"x", "y", "z"}, SphericalRegion -> True,
    ImageSize -> 400, Lighting -> "Neutral", ViewAngle -> 35 Degree, ViewPoint -> {2.2, -2.0, 1.4},
    Method -> {"MouseInteraction" -> {"Rotate" -> True, "Pan" -> False, "Zoom" -> False}}
  ];

  (* legenda pre prienik *)
  {extraLegStyles, extraLegLabels} = Switch[inter["Type"],
    "POINT", {{Black}, {Row[{"prienik: [", Sequence @@ Riffle[TraditionalForm /@ inter["Point"], ", "], "]"}]}},
    "LINE",  {{Black}, {Row[{"priesečník: ", TraditionalForm @ best["Exprs"], ", ", tf[\[FormalT]], "\[Element]", "\[DoubleStruckR]"}]}},
    _, {{}, {}}
  ];

  legend = If[extraLegStyles === {},
    SwatchLegend[planeStyles, planeLabels],
    SwatchLegend[Join[planeStyles, extraLegStyles], Join[planeLabels, extraLegLabels]]
  ];

  printFormulaCell @ Legended[plot, legend];
];
systemIntersection3[A_, b_, vars_] := Module[{rA=MatrixRank[A], rAb=MatrixRank[Join[A, Transpose[{b}], 2]], ns},
  If[rAb > rA, <|"Type" -> "NONE"|>,
    If[rA == 3, <|"Type" -> "POINT", "Point" -> LinearSolve[A, b]|>,
      ns = NullSpace[A];
      If[Length[ns] == 1, <|"Type" -> "LINE", "Point" -> (vars /. First@FindInstance[A.vars == b, vars, Reals]), "Dir" -> ns[[1]]|>,
        If[Length[ns] >= 2, <|"Type" -> "PLANE"|>, <|"Type" -> "INFINITE"|>]]]]];

(* ~-~-~ MAIN CONTROLLER ~-~-~ *)
Gen01[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {dim, vars, st, data, A, b, steps, sol, genFunc, stepsFunc},

  If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff], Message[Gen01::baddiff, diff]; Return[$Failed]];
  If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode], Message[Gen01::badmode, mode]; Return[$Failed]];

  st = Replace[OptionValue[SolutionType], Automatic -> RandomChoice[{"ONE", "ONE", "ONE", "NONE", "INFINITE"}]];
  dim = Switch[diff, "EASY", 2, "MEDIUM" | "HARD", 3];
  vars = Take[{x, y, z}, dim];

  (* zjednotené generovanie cez jednu funkciu *)
  genFunc = generateLinearSystem[dim, diff, st];

  data = WithRetries[Function[Null, genFunc], 200];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  A = data["A"]; b = data["b"];

  printSectionCell["Eliminačná metóda"];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc"];

  If[diff === "HARD" && KeyExistsQ[data, "EqDisplay"],
    printFormulaCell @ alignedEquations[data["EqDisplay"]],
    If[dim == 2,
      printFormulaCell @ alignedEquations @ Table[{formatEquationLHS[A[[i]], "", vars], b[[i]], ""}, {i, 2}],
      printFormulaCell @ alignedEquations @ Table[{formatEquationLHS[A[[i]], "", vars], b[[i]], ""}, {i, 3}]
    ]
  ];

  printTextCell["Riešte v množine reálnych čísel eliminačnou metódou (sčítaním rovníc)."];

  stepsCounter = 0;

  stepsFunc = Which[
    dim == 2 && st == "ONE",      stepsOne2,
    dim == 2 && st == "NONE",     stepsNone2,
    dim == 2 && st == "INFINITE", stepsInfinite2,
    dim == 3 && st == "ONE",      stepsOne3,
    dim == 3 && st == "NONE",     stepsNone3,
    dim == 3 && st == "INFINITE", stepsInfinite3
  ];

  steps = If[dim == 3, stepsFunc[A, b, vars, data], stepsFunc[A, b, vars]];
  If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];
  sol = steps["Solution"];

  If[mode === "TASK_STEPS_RESULT", printSubsectionCell["Postup"]; Scan[renderStepItem, steps["Content"]]];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    Switch[sol,
      "NONE", printTextCell["Sústava nemá riešenie (pri sčítaní vznikol spor)."],
      "INFINITE",
      If[dim == 3, printInfiniteResult[A, b, vars],
        printInfiniteResult[A, b, vars]
      ],
      _, (* ONE *)
      CellPrint@Cell[BoxData@ToBoxes[
        If[dim == 2,
          Row[{"Riešením sústavy rovníc je usporiadaná dvojica čísel [x,y] = ", Style[Row[{"[", tft[sol[[1]]], ", ", tft[sol[[2]]], "]"}], Bold]}],
          Row[{"Riešením sústavy rovníc je usporiadaná trojica čísel [x,y,z] = ", Style[Row[{"[", tft[sol[[1]]], ", ", tft[sol[[2]]], ", ", tft[sol[[3]]], "]"}], Bold]}]],
        TraditionalForm], "Text", ShowStringCharacters -> False]
    ];

    If[OptionValue[Visualization], If[dim == 2, visualize2[A, b, vars, sol], visualize3[A, b, vars, sol]]];
  ];
];

End[];
EndPackage[];