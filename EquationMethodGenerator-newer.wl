(* ::Package:: *)

(*
  Package: EquationMethodGenerator
  Description: Generátory príkladov riešenia sústav lineárnych rovníc eliminačnou a dosadzovacou metódou.
*)

BeginPackage["MojeGeneratory`EquationMethodGenerator`"];

$CharacterEncoding = "UTF-8";

GenElimination::usage = "GenElimination[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc eliminačnou metódou (sčítaním rovníc).\n\n" <>
    "diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)\n" <>
    "mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"\n" <>
    "opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

GenSubstitution::usage = "GenSubstitution[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc dosadzovacou (substitučnou) metódou.\n\n" <>
    "diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)\n" <>
    "mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"\n" <>
    "opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

GenElimination::baddiff = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenElimination::badmode = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenElimination::badst   = "Neplatný typ riešenia `1`. Použi Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenElimination::fail    = "Nepodarilo sa vygenerovať vhodný príklad.";

GenSubstitution::baddiff = GenElimination::baddiff;
GenSubstitution::badmode = GenElimination::badmode;
GenSubstitution::badst   = GenElimination::badst;
GenSubstitution::fail    = GenElimination::fail;

Options[GenElimination] = {SolutionType -> Automatic, Visualization -> False};
Options[GenSubstitution] = {SolutionType -> Automatic, Visualization -> False};

Begin["`Private`"];

Internal`$ContextMarks = False;

(* ~-~-~ VALIDATION ~-~-~ *)

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.6, 0.2, 0.2} -> {"ONE", "NONE", "INFINITE"}]];

dimByDifficulty[diff_] := Switch[diff, "EASY", 2, "MEDIUM" | "HARD", 3, _, 3];
varsByDim[dim_] := Take[{x, y, z}, dim];

WithRetries[f_, max_Integer : 200] := Module[{res = $Failed, i = 0},
  While[res === $Failed && i < max, i++; res = f[]];
  res
];

(* ~-~-~ RENDER / FORMATTING ~-~-~ *)

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;

printCellStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False], Print[expr]];
printTextCell[str_String] := printCellStyle[str, "Text"];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printTextExprCell[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "Text"]];
printFormulaCell[expr_] := Module[{boxes},
  boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr];
  printCellStyle[boxes, "DisplayFormula"]
];

renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  MatchQ[item, Style[_, ___]], printTextExprCell[item],
  Head[item] === Graphics || Head[item] === Graphics3D,
  If[inNotebookQ[], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"], Print[item]],
  True, printFormulaCell[item]
];

renderTermsRow[terms_List, mode_ : "Numeric"] := Module[
  {pairs, out = {}, first = True, c, v, t, zeroQ, negQ},
  pairs = Select[terms, MatchQ[#, {_, _}] &];
  zeroQ = If[mode === "Symbolic", PossibleZeroQ, (# == 0 &)];
  negQ = If[mode === "Symbolic", (TrueQ[# < 0] &), (# < 0 &)];
  If[pairs === {} || AllTrue[pairs[[All, 1]], zeroQ], Return[tf[0]]];
  Do[
    {c, v} = pairs[[i]];
    If[zeroQ[c], Continue[]];
    t = If[v === None, tf[Abs[c]], tf[If[Abs[c] === 1, v, Abs[c] v]]];
    If[first,
      If[negQ[c], out = Join[out, {"-", t}], out = Join[out, {t}]];
      first = False;,
      out = Join[out, {If[negQ[c], " - ", " + "], t}]
    ],
    {i, 1, Length[pairs]}
  ];
  If[out === {}, tf[0], Row[out]]
];

$diffConfig = <|
  "EASY" -> <|"CoeffRange" -> 5, "Bound" -> 60|>,
  "MEDIUM" -> <|"CoeffRange" -> 5, "Bound" -> 90|>,
  "HARD" -> <|"CoeffRange" -> 5, "Bound" -> 180|>
|>;

highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

stepsCounter = 0;
makeStepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);

alignedEquations[data_, breaks_List : {}, gap_ : 1.25] := Module[
  {eq = Style["=", 16], bar = Style["|", GrayLevel[.25]], n, rowGaps, stepRow, baseGap = 0.5, bigGap = gap},
  n = Length[data];
  rowGaps = If[n <= 1, {}, ConstantArray[baseGap, n - 1]];
  Do[If[IntegerQ[b] && 1 <= b <= n - 1, rowGaps[[b]] = bigGap], {b, breaks}];
  stepRow[{lhs_, rhs_, note_}] := {lhs, eq, rhs, If[note === "" || note === None, "", Style[Row[{bar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]]};
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];
  Grid[stepRow /@ data, Alignment -> {{Right, Center, Left, Left}}, Spacings -> {0.5, rowGaps}, BaseStyle -> {FontSize -> 14}]
];

formatEquationLHS[coeffs_List, choice_, vars_List] := Module[
  {terms = {}, first = True, c, v, sign, t, choiceVar, i, n},
  choiceVar = Which[
    MemberQ[vars, choice], choice,
    choice === "X" && Length[vars] >= 1, vars[[1]],
    choice === "Y" && Length[vars] >= 2, vars[[2]],
    choice === "Z" && Length[vars] >= 3, vars[[3]],
    True, None
  ];
  n = Min[Length[coeffs], Length[vars]];
  For[i = 1, i <= n, i++,
    c = coeffs[[i]];
    v = vars[[i]];
    If[c === 0, Continue[]];
    sign = If[first, If[c < 0, "-", ""], If[c < 0, " - ", " + "]];
    t = If[Abs[c] === 1, v, Abs[c] v];
    If[v === choiceVar, t = highlightTerm[t]];
    If[sign =!= "", AppendTo[terms, sign]];
    AppendTo[terms, tf[t]];
    first = False;
  ];
  If[terms === {}, tf[0], Row[terms]]
];

wrapNegValue[val_] := Which[
  NumericQ[val] && val < 0, Row[{"(", tft[val], ")"}],
  Head[val] === Plus, Row[{"(", tft[val], ")"}],
  MatchQ[val, Times[c_?NumericQ, __] /; c < 0], Row[{"(", tft[val], ")"}],
  MatchQ[val, Times[-1, __]], Row[{"(", tft[val], ")"}],
  MatchQ[val, _Rational] && val < 0, Row[{"(", tft[val], ")"}],
  True, tft[val]
];

coeffVal[coeff_, val_] := Which[
  coeff === 0, 0,
  coeff === 1, wrapNegValue[val],
  coeff === -1, Row[{"-", wrapNegValue[val]}],
  True, Row[{tft[coeff], " \[CenterDot] ", wrapNegValue[val]}]
];

signBtwTerms[c_] := If[c < 0, " - ", " + "];

addNote[k_] := Which[
  PossibleZeroQ[k], "",
  TrueQ[k > 0], Row[{"+ ", tft[k]}],
  TrueQ[k < 0], Row[{"- ", tft[Abs[k]]}],
  MatchQ[k, Times[c_?NumericQ, __] /; c < 0], Row[{tft[k]}],
  MatchQ[k, Times[-1, __]], Row[{tft[k]}],
  MatchQ[k, Plus[c_, __]] && (TrueQ[c < 0] || (MatchQ[c, Times[n_, __]] && TrueQ[n < 0])), Row[{tft[k]}],
  True, Row[{"+ ", tft[k]}]
];

scalarNote[symbol_String, k_] := Which[PossibleZeroQ[k - 1], "", True, Row[{symbol, " ", wrapNegValue[k]}]];
multNote[m_] := scalarNote["\[CenterDot]", m];
divNote[d_] := scalarNote[":", d];
substNote[solMap_, remVars_, row_, vars_] := Module[{usedVars},
  usedVars = Select[remVars, row[[First @ First @ Position[vars, #]]] =!= 0 &];
  If[usedVars === {}, "", Row[Riffle[(Row[{#, " \[Rule] ", tft[solMap[#]]}] & /@ usedVars), ", "]]]
];

formatSubstLHS[row_, vars_, solMap_, unknownVar_, evalMode_ : False] := Module[
  {terms = {}, first = True, addTerm, emitKnownTerm, emitUnknownTerm},
  addTerm[content_, sign_] := (AppendTo[terms, If[first, If[sign === -1, Row[{"-", content}], content], Row[{If[sign === -1, " - ", " + "], content}]]]; first = False);
  emitKnownTerm[c_, v_] := Module[{prod},
    If[!KeyExistsQ[solMap, v], Return[Null]];
    If[TrueQ[evalMode], prod = Together[c*solMap[v]]; If[!PossibleZeroQ[prod], addTerm[tf[Abs[prod]], Sign[prod]]], addTerm[coeffVal[Abs[c], solMap[v]], Sign[c]]]
  ];
  emitUnknownTerm[c_, v_] := addTerm[tf[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]];
  Do[
    With[{c = row[[i]], v = vars[[i]]},
      If[c =!= 0, If[v === unknownVar, emitUnknownTerm[c, v], emitKnownTerm[c, v]]]
    ],
    {i, 1, Length[vars]}
  ];
  If[terms === {}, tf[0], Row[terms]]
];

checkRowTerms[row_, sol_] := Module[{n = Length[row], first = True, out = {}},
  Do[
    If[row[[j]] === 0, Continue[]];
    If[first,
      out = Join[out, If[row[[j]] < 0, {"-", tf[Abs[row[[j]]]], "\[CenterDot]", wrapNegValue[sol[[j]]]}, {tf[row[[j]]], "\[CenterDot]", wrapNegValue[sol[[j]]]}]];
      first = False;,
      out = Join[out, {signBtwTerms[row[[j]]], tf[Abs[row[[j]]]], "\[CenterDot]", wrapNegValue[sol[[j]]]}]
    ],
    {j, 1, n}
  ];
  If[out === {}, tf[0], Row[out]]
];

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
        If[first, out = Join[out, {tf[p]}]; first = False;, out = Join[out, {signBtwTerms[row[[j]]], tf[Abs[p]]}]],
        {j, 1, n}
      ];
      If[out === {}, tf[0], Row[out]]
    ];
    c = Join[c, {
      Row[{"ĽS" <> ToString[i] <> " = ", checkRowTerms[row, solN], " = ", prodRow, " = ", tf[lhs]}],
      Row[{"PS" <> ToString[i] <> " = ", tf[b[[i]]]}],
      "ĽS" <> ToString[i] <> " = PS" <> ToString[i]
    }],
    {i, 1, n}
  ];
  c
];

numbersNiceQ[A_, b_, diff_] := Max[Abs @ Join[Flatten[A], Flatten[b]]] <= Lookup[$diffConfig, diff, $diffConfig["MEDIUM"]]["Bound"];

(* ~-~-~ HARD DISPLAY HELPERS ~-~-~ *)

scaleTerms[terms_List, k_Integer] := ({k #[[1]], #[[2]]} & /@ terms);

buildHardEq[row_, rhs_, vars_] := Module[
  {n = Length[vars], idxMove, cLeftPool, cLeft, varTerms, kept, moved, leftBase, rightBase},
  idxMove = RandomSample[Range[n], RandomChoice[Range[1, n - 1]]];
  cLeftPool = DeleteCases[Range[-7, 7], 0];
  cLeft = RandomChoice[cLeftPool];
  varTerms = MapThread[List, {row, vars}];
  kept = varTerms[[Complement[Range[n], idxMove]]];
  moved = ({-#[[1]], #[[2]]} & /@ varTerms[[idxMove]]);
  leftBase = RandomSample @ Join[kept, {{cLeft, None}}];
  rightBase = RandomSample @ Join[moved, {{rhs + cLeft, None}}];
  <|"MoveIdx" -> idxMove, "CLeft" -> cLeft, "LeftBaseTerms" -> leftBase, "RightBaseTerms" -> rightBase|>
];

pickHardMultipliers15[n_] := Module[{ks},
  ks = RandomChoice[Range[5], n];
  While[Max[ks] < 4, ks = RandomChoice[Range[5], n]];
  ks
];

buildHardDisplay[data_Association, vars_] := Module[
  {A = data["A"], b = data["b"], n = Length[vars], ks, eqMeta = {}, eqDisp = {}, leftBaseAll = {}, rightBaseAll = {}, leftMultAll = {}, rightMultAll = {}, m, i, lMult, rMult},
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
    AppendTo[eqDisp, {renderTermsRow[lMult], renderTermsRow[rMult], ""}],
    {i, 1, n}
  ];
  Join[data, <|"HardQ" -> True, "Multipliers" -> ks, "EqDisplay" -> eqDisp, "HardMeta" -> eqMeta, "HardLeftBaseTerms" -> leftBaseAll, "HardRightBaseTerms" -> rightBaseAll|>]
];

zeroCoeff3[A_] := Module[{mask, zeroRowsByCol, zeroColsByRow},
  mask = Map[# == 0 &, A, {2}];
  zeroRowsByCol = Table[Flatten @ Position[mask[[All, j]], True], {j, 1, 3}];
  zeroColsByRow = Table[Flatten @ Position[mask[[i]], True], {i, 1, 3}];
  <|"Mask" -> mask, "ZeroRowsByCol" -> zeroRowsByCol, "ZeroColsByRow" -> zeroColsByRow|>
];

hardNormalizationSteps3[A_, b_, vars_, data_Association] := Module[
  {content = {}, ks, leftBase, rightBase, k, lMult, rMult, coeffL, coeffR, constL, constR, addTerms, addNoteFromTerms,
    addNoteLocal, rowStd, rhsStd, rowsStd, rhsStdAll, gcds, rowDiv, rhsDiv, rowsFinal, rhsFinal, anyDivQ},

  termsToCoeffsConst[terms_List] := Module[{cVar, cConst},
    cVar = Table[Total @ Cases[terms, {cc_, vars[[j]]} :> cc], {j, 1, Length[vars]}];
    cConst = Total @ Cases[terms, {cc_, None} :> cc];
    {cVar, cConst}
  ];

  addNoteFromTerms[terms_List] := Module[{pairs, pieces},
    pairs = Select[terms, MatchQ[#, {_, _}] && #[[1]] =!= 0 &];
    If[pairs === {}, Return[""]];
    pieces = Table[
      With[{cc = pairs[[j, 1]], sym = pairs[[j, 2]]},
        Row[{If[cc >= 0, "+", "-"], If[sym === None, tf[Abs[cc]], tf[If[Abs[cc] === 1, sym, Abs[cc] sym]]]}]
      ],
      {j, 1, Length[pairs]}
    ];
    Row @ Riffle[pieces, " "]
  ];

  ks = data["Multipliers"];
  leftBase = data["HardLeftBaseTerms"];
  rightBase = data["HardRightBaseTerms"];

  AppendTo[content, makeStepHeader["Normalizácia"]];
  AppendTo[content, "V každej rovnici presunieme všetky členy s neznámymi na ľavú stranu a všetky konštanty na pravú stranu."];

  rowsStd = ConstantArray[0, {3, 3}];
  rhsStdAll = ConstantArray[0, 3];

  AppendTo[content,
    alignedEquations @ Table[
      k = ks[[i]];
      lMult = scaleTerms[leftBase[[i]], k];
      rMult = scaleTerms[rightBase[[i]], k];

      {coeffL, constL} = termsToCoeffsConst[lMult];
      {coeffR, constR} = termsToCoeffsConst[rMult];

      addTerms = Join[
        If[constL =!= 0, {{-constL, None}}, {}],
        Join @@ Table[If[coeffR[[j]] =!= 0, {{-coeffR[[j]], vars[[j]]}}, {}], {j, 1, 3}]
      ];
      addNoteLocal = addNoteFromTerms[addTerms];

      rowStd = coeffL - coeffR;
      rhsStd = constR - constL;

      rowsStd[[i]] = rowStd;
      rhsStdAll[[i]] = rhsStd;

      {renderTermsRow[lMult], renderTermsRow[rMult], addNoteLocal},
      {i, 1, 3}
    ]
  ];

  AppendTo[content, "Po úprave dostaneme:"];
  AppendTo[content, alignedEquations @ Table[{formatEquationLHS[rowsStd[[i]], "", vars], rhsStdAll[[i]], ""}, {i, 1, 3}]];

  gcds = Table[GCD @@ Abs @ Join[rowsStd[[i]], {rhsStdAll[[i]]}], {i, 1, 3}];
  anyDivQ = AnyTrue[gcds, # > 1 &];

  rowsFinal = rowsStd;
  rhsFinal = rhsStdAll;

  If[anyDivQ,
    AppendTo[content, "Ak majú všetky koeficienty v rovnici spoločný deliteľ väčší ako 1, rovnicu vydelíme týmto číslom, aby sme dostali jednoduchší tvar."];
    Do[
      If[gcds[[i]] > 1,
        rowDiv = rowsFinal[[i]]/gcds[[i]];
        rhsDiv = rhsFinal[[i]]/gcds[[i]];
        AppendTo[content, alignedEquations[{{formatEquationLHS[rowsFinal[[i]], "", vars], rhsFinal[[i]], divNote[gcds[[i]]]}}]];
        rowsFinal[[i]] = rowDiv;
        rhsFinal[[i]] = rhsDiv;
      ],
      {i, 1, 3}
    ];
    AppendTo[content, "Po úpravách dostaneme sústavu rovníc v štandardnom tvare, pripravenú na riešenie:"];
    AppendTo[content, alignedEquations @ Table[{formatEquationLHS[rowsFinal[[i]], "", vars], rhsFinal[[i]], ""}, {i, 1, 3}]];
  ];

  content
];

(* ~-~-~ INFINITE SOLUTIONS & PARAMETRIZATION ~-~-~ *)

chooseParametrization[A_, b_, vars_] := Module[{eqs, candidates, try, results},
  eqs = Thread[A.vars == b];
  candidates = List /@ vars;
  try[{v_}] := Module[{sol, rules, exprs, dens},
    sol = Quiet @ Solve[eqs /. v -> \[FormalT], Complement[vars, {v}], Reals];
    If[sol === {} || sol === $Failed, Return[Nothing]];
    rules = Join[{v -> \[FormalT]}, sol[[1]]];
    exprs = Together /@ (vars /. rules);
    dens = Denominator /@ Rationalize[exprs, 0];
    <|"Var" -> v, "Exprs" -> exprs, "Score" -> Max[dens]|>
  ];
  results = try /@ candidates;
  If[results === {}, $Failed, First @ MinimalBy[results, #Score &]]
];

printInfiniteResult[A_, b_, vars_] := Module[
  {nVars = Length[vars], best, exprs, kBox, vecBox, condBox},
  best = chooseParametrization[A, b, vars];
  If[best === $Failed, Return[$Failed]];
  exprs = Together /@ best["Exprs"];
  exprs = exprs //. {Times[Rational[1, q_Integer], e_] :> e/q, Times[Rational[-1, q_Integer], e_] :> -e/q};
  exprs = Simplify /@ exprs;

  printTextCell["Sústava má nekonečne veľa riešení. Riešenia zapíšeme parametricky."];
  printTextCell["Zvolíme voľnú premennú a označíme ju parametrom."];

  printTextCell["Parameter:"];
  printFormulaCell @ Grid[{{\[FormalT], "\[Element]", "\[DoubleStruckR]"}}, Alignment -> {{Center, Center, Left}}];

  printTextCell["Potom platí:"];
  printFormulaCell @ Grid[Table[{vars[[k]], "=", tf[exprs[[k]]]}, {k, 1, nVars}], Alignment -> {{Right, Center, Left}}];

  vecBox = RowBox[{"[", RowBox[Riffle[ToBoxes[#, TraditionalForm] & /@ exprs, "; "]], "]"}];
  condBox = RowBox[{ToBoxes[\[FormalT], TraditionalForm], "\[Element]", "\[DoubleStruckR]"}];
  kBox = RowBox[{StyleBox["K", FontSlant -> "Italic"], "=", RowBox[{"{", RowBox[{vecBox, " ", "\[VerticalSeparator]", " ", condBox}], "}"}]}];

  CellPrint @ Cell[BoxData @ FormBox[kBox, TraditionalForm], "DisplayFormula", BaseStyle -> {FontSize -> 14}];
  <|"Type" -> "INFINITE"|>
];

(* ~-~-~ SYSTEM GENERATION ~-~-~ *)

Options[generateLinearSystem] = {RequireUnitCoeff -> False};

generateLinearSystem[dim_, diff_, solType_ : "ONE", opts : OptionsPattern[]] := Module[
  {r, nzPool, pickNZ, makeRow2NoZero, makeRow3NoZero, targetZeroCount, zeroRow, zeroCol, makeRow3PlannedZero, makeRow,
    zerosOkQ, fullRankQ, niceOkQ, oneCoeffQ, attemptLimit, A = {}, b = {}, x0, contradiction, k, k1, k2, c1, c2, c3,
    vars, data, okQ, r1, r2, r3, foundQ = False},

  r = Lookup[$diffConfig, diff, $diffConfig["MEDIUM"]]["CoeffRange"];

  nzPool = Join[-Range[r], Range[r]];
  pickNZ[] := RandomChoice[nzPool];
  makeRow2NoZero[] := {pickNZ[], pickNZ[]};
  makeRow3NoZero[] := {pickNZ[], pickNZ[], pickNZ[]};

  targetZeroCount = If[dim == 3 && diff === "MEDIUM", 1, 0];
  {zeroRow, zeroCol} = If[dim == 3 && diff === "MEDIUM", {RandomInteger[{1, 3}], RandomInteger[{1, 3}]}, {None, None}];

  makeRow3PlannedZero[col_] := ReplacePart[makeRow3NoZero[], col -> 0];

  makeRow[i_] := Which[
    dim == 2, makeRow2NoZero[],
    diff === "MEDIUM" && i === zeroRow, makeRow3PlannedZero[zeroCol],
    True, makeRow3NoZero[]
  ];

  zerosOkQ[m_] := Count[Flatten[m], 0] === targetZeroCount;
  fullRankQ[m_] := If[dim == 2, Det[m] =!= 0, MatrixRank[m] === dim];
  niceOkQ[m_, rhs_] := numbersNiceQ[m, rhs, diff];
  oneCoeffQ[m_] := MemberQ[Flatten[m], 1] || MemberQ[Flatten[m], -1];

  attemptLimit = 5000;

  okQ[] := zerosOkQ[A] && fullRankQ[A] && niceOkQ[A, b] && If[TrueQ[OptionValue[RequireUnitCoeff]], oneCoeffQ[A], True];

  If[solType === "ONE",
    Do[
      If[dim == 2,
        A = {makeRow[1], makeRow[2]},
        (
          r1 = makeRow[1]; r2 = makeRow[2]; r3 = makeRow[3];
          While[MatrixRank[{r1, r2}] < 2, r1 = makeRow[1]; r2 = makeRow[2]];
          While[MatrixRank[{r1, r2, r3}] < 3 || !zerosOkQ[{r1, r2, r3}], r3 = makeRow[3]];
          A = {r1, r2, r3}
        )
      ];
      x0 = RandomInteger[{-9, 9}, dim];
      b = A . x0;
      If[okQ[], foundQ = True; Break[]],
      {attemptLimit}
    ];
    If[!TrueQ[foundQ], Return[$Failed]];
    data = <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE", "PlannedZeroRC" -> If[dim == 3 && diff === "MEDIUM", {zeroRow, zeroCol}, None]|>,
    contradiction = If[solType === "NONE", RandomChoice[{-5, -3, 3, 5}], 0];
    foundQ = False;
    Do[
      If[dim == 2,
        (
          r1 = makeRow[1]; k = RandomChoice[{-3, -2, 2, 3}]; r2 = k r1;
          c1 = RandomInteger[{-10, 10}];
          A = {r1, r2};
          b = {c1, k c1 + contradiction};
          k1 = k; k2 = 0
        ),
        (
          r1 = makeRow[1]; r2 = makeRow[2];
          While[MatrixRank[{r1, r2}] < 2 || (diff === "MEDIUM" && Count[Join[r1, r2], 0] > 1), r1 = makeRow[1]; r2 = makeRow[2]];
          If[diff === "MEDIUM" && zeroRow === 3, (k1 = r2[[zeroCol]]; k2 = -r1[[zeroCol]]), (k1 = RandomChoice[{-2, -1, 1, 2}]; k2 = RandomChoice[{-2, -1, 1, 2}])];
          r3 = k1 r1 + k2 r2;
          While[
            MatrixRank[{r1, r2}] < 2 ||
                (diff === "MEDIUM" && zeroRow === 3 && Count[r3, 0] =!= 1) ||
                (diff === "MEDIUM" && zeroRow =!= 3 && Count[r3, 0] =!= 0) ||
                (diff =!= "MEDIUM" && AnyTrue[r3, # == 0 &]),
            (
              r1 = makeRow[1]; r2 = makeRow[2];
              While[MatrixRank[{r1, r2}] < 2 || (diff === "MEDIUM" && Count[Join[r1, r2], 0] > 1), r1 = makeRow[1]; r2 = makeRow[2]];
              If[diff === "MEDIUM" && zeroRow === 3, (k1 = r2[[zeroCol]]; k2 = -r1[[zeroCol]]), (k1 = RandomChoice[{-2, -1, 1, 2}]; k2 = RandomChoice[{-2, -1, 1, 2}])];
              r3 = k1 r1 + k2 r2
            )
          ];
          c1 = RandomInteger[{-5, 5}]; c2 = RandomInteger[{-5, 5}]; c3 = k1 c1 + k2 c2 + contradiction;
          A = {r1, r2, r3};
          b = {c1, c2, c3}
        )
      ];
      If[zerosOkQ[A] && niceOkQ[A, b] && If[TrueQ[OptionValue[RequireUnitCoeff]], oneCoeffQ[A], True], foundQ = True; Break[]],
      {attemptLimit}
    ];
    If[!TrueQ[foundQ], Return[$Failed]];
    data = <|"A" -> A, "b" -> b, "type" -> solType|>
  ];

  If[diff === "HARD", vars = {x, y, z}; data = buildHardDisplay[data, vars]];
  data
];

(* ~-~-~ VISUALIZATION HELPERS ~-~-~ *)

visualize2[A_, b_, vars_, sol_] := Module[
  {x, y, pt, xrange, yrange, seg, center, subtitle, range = 10, lineStyles, lineLabels, extraLegStyles, extraLegLabels, legend},
  printTextCell[" "];
  {x, y} = vars;

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
    subtitle = If[sol === "NONE", "Priamky sú rovnobežné, nepretínajú sa – sústava nemá riešenie.", "Priamky sú totožné (prekrývajú sa) – sústava má nekonečne veľa riešení."]
  ];

  seg[row_, rhs_] := With[{a = row[[1]], bb = row[[2]]},
    If[bb =!= 0, InfiniteLine[{{0, rhs/bb}, {1, (rhs - a)/bb}}], InfiniteLine[{{rhs/a, 0}, {rhs/a, 1}}]]
  ];

  printTextExprCell[subtitle];

  lineStyles = If[sol === "INFINITE",
    {Directive[Magenta, AbsoluteThickness[2], Opacity[0.9]], Directive[Blue, AbsoluteThickness[2], Opacity[0.9], Dashing[0.05]]},
    {Directive[Magenta, Thick], Directive[Blue, Thick]}
  ];

  lineLabels = {tf[A[[1, 1]] x + A[[1, 2]] y == b[[1]]], tf[A[[2, 1]] x + A[[2, 2]] y == b[[2]]]};

  {extraLegStyles, extraLegLabels} =
      If[pt =!= None,
        {{Directive[Black]}, {Row[{"prienik: [", TraditionalForm @ Together[pt[[1]]], ", ", TraditionalForm @ Together[pt[[2]]], "]"}]}},
        {{}, {}}
      ];

  legend = LineLegend[
    Join[lineStyles, extraLegStyles],
    Join[lineLabels, extraLegLabels],
    LegendMarkerSize -> {50, 20},
    LegendMarkers -> Join[{None, None}, If[pt =!= None, {Graphics[{Black, Disk[]}, ImageSize -> 10]}, {}]]
  ];

  printFormulaCell @ Legended[
    Graphics[
      {
        {lineStyles[[1]], seg[A[[1]], b[[1]]]},
        {lineStyles[[2]], seg[A[[2]], b[[2]]]},
        If[pt =!= None, {{Black, Thick, Circle[pt, 0.4]}, {Green, PointSize[0.02], Point[pt]}}, {}]
      },
      PlotRange -> {xrange, yrange}, Axes -> True, GridLines -> Automatic, ImageSize -> Medium, PlotRangeClipping -> True
    ],
    legend
  ]
];

systemIntersection3[A_, b_, vars_] := Module[{rA = MatrixRank[A], rAb = MatrixRank[Join[A, Transpose[{b}], 2]], ns},
  If[rAb > rA, <|"Type" -> "NONE"|>,
    If[rA == 3, <|"Type" -> "POINT", "Point" -> LinearSolve[A, b]|>,
      ns = NullSpace[A];
      If[Length[ns] == 1, <|"Type" -> "LINE", "Point" -> (vars /. First @ FindInstance[A.vars == b, vars, Reals]), "Dir" -> ns[[1]]|>,
        If[Length[ns] >= 2, <|"Type" -> "PLANE"|>, <|"Type" -> "INFINITE"|>]
      ]
    ]
  ]
];

visualize3[A_, b_, vars_, sol_] := Module[
  {x, y, z, range = 15, xmin, xmax, ymin, ymax, zmin, zmax, n1, n2, n3, d1, d2, d3, inter, best, subtitle, planes, mark,
    plot, eqLbl, planeStyles, planeLabels, extraLegStyles, extraLegLabels, legend},

  printTextCell[" "];
  {x, y, z} = vars;
  {xmin, xmax} = {-range, range}; {ymin, ymax} = {-range, range}; {zmin, zmax} = {-range, range};

  n1 = N @ A[[1]]; d1 = N @ b[[1]];
  n2 = N @ A[[2]]; d2 = N @ b[[2]];
  n3 = N @ A[[3]]; d3 = N @ b[[3]];

  inter = systemIntersection3[A, b, vars];
  best = If[inter["Type"] === "LINE", chooseParametrization[A, b, vars], $Failed];

  subtitle = Switch[inter["Type"],
    "POINT", "Tri roviny majú spoločný prienik v jednom bode (riešenie sústavy).",
    "LINE", "Tri roviny majú spoločný prienik – priamku (nekonečne veľa riešení).",
    "PLANE", "Všetky tri rovnice opisujú tú istú rovinu (nekonečne veľa riešení).",
    "NONE", "Roviny nemajú spoločný prienik všetkých troch naraz (sústava nemá riešenie).",
    _, "Prienik sa nepodarilo jednoznačne určiť."
  ];

  printTextExprCell[subtitle];

  eqLbl[row_, rhs_] := tf[row.{x, y, z} == rhs];
  planeLabels = {eqLbl[A[[1]], b[[1]]], eqLbl[A[[2]], b[[2]]], eqLbl[A[[3]], b[[3]]]};
  planeStyles = {Cyan, Magenta, Yellow};

  planes = ContourPlot3D[
    {n1.{x, y, z} == d1, n2.{x, y, z} == d2, n3.{x, y, z} == d3},
    {x, xmin, xmax}, {y, ymin, ymax}, {z, zmin, zmax},
    Mesh -> None, PlotPoints -> 25, PerformanceGoal -> "Speed",
    ContourStyle -> {Directive[Cyan, Opacity[0.4]], Directive[Magenta, Opacity[0.4]], Directive[Yellow, Opacity[0.4]]},
    BoundaryStyle -> None
  ];

  mark = Graphics3D @ Switch[inter["Type"],
    "POINT", {Black, PointSize[0.03], Point[N @ inter["Point"]], Black, Sphere[N @ inter["Point"], 0.35]},
    "LINE", Module[{p0, v}, p0 = N @ inter["Point"]; v = 20 N @ inter["Dir"]; {Black, Specularity[White, 20], Tube[{p0 - v, p0 + v}, 0.18]}],
    _, {}
  ];

  plot = Show[
    planes, mark,
    PlotRange -> {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}},
    BoxRatios -> {1, 1, 1},
    Axes -> True, AxesLabel -> {"x", "y", "z"},
    SphericalRegion -> True, ImageSize -> 400,
    Lighting -> "Neutral",
    ViewAngle -> 35 Degree,
    ViewPoint -> {2.2, -2.0, 1.4},
    Method -> {"MouseInteraction" -> {"Rotate" -> True, "Pan" -> False, "Zoom" -> False}}
  ];

  {extraLegStyles, extraLegLabels} = Switch[inter["Type"],
    "POINT", {{Black}, {Row[{"prienik: [", Sequence @@ Riffle[TraditionalForm /@ inter["Point"], ", "], "]"}]}},
    "LINE", {{Black}, {Row[{"priesečník: ", TraditionalForm @ best["Exprs"], ", ", tf[\[FormalT]], "\[Element]", "\[DoubleStruckR]"}]}},
    _, {{}, {}}
  ];

  legend = If[extraLegStyles === {}, SwatchLegend[planeStyles, planeLabels], SwatchLegend[Join[planeStyles, extraLegStyles], Join[planeLabels, extraLegLabels]]];
  printFormulaCell @ Legended[plot, legend];
];

(* ~-~-~ ELIMINATION ~-~-~ *)

equationClass[row_List, rhs_] := Which[
  AllTrue[row, PossibleZeroQ] && PossibleZeroQ[rhs], "IDENTITY",
  AllTrue[row, PossibleZeroQ] && !PossibleZeroQ[rhs], "CONTRADICTION",
  True, "NORMAL"
];

solveOneVarEquationSteps[{row_List, rhs_}, {var_}] := Module[
  {c = row[[1]], cls, steps = {}, value},
  cls = equationClass[row, rhs];
  If[cls === "CONTRADICTION", Return[<|"Type" -> "NONE", "Content" -> {{tf[0], tf[rhs], ""}}|>]];
  If[cls === "IDENTITY", Return[<|"Type" -> "INFINITE", "Content" -> {{tf[0], tf[0], ""}}|>]];

  AppendTo[steps, {tf[c var], tf[rhs], Which[c === 1, "", c === -1, multNote[-1], True, divNote[c]]}];

  value = Together[rhs/c];
  AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "ONE", "Value" -> value, "Content" -> steps|>
];

backSubstituteElimVarSteps[{row_List, rhs_}, vars_List, solMap_Association, elimVar_] := Module[
  {pos, coeffU, knownSum, rhsShift, noteShift, steps = {}},
  pos = First @ First @ Position[vars, elimVar];
  coeffU = row[[pos]];

  AppendTo[steps, {formatEquationLHS[row, "", vars], rhs, substNote[solMap, Keys[solMap], row, vars]}];
  AppendTo[steps, {formatSubstLHS[row, vars, solMap, elimVar, False], rhs, ""}];

  knownSum = Together @ Total @ Table[
    If[i === pos, 0, If[KeyExistsQ[solMap, vars[[i]]], row[[i]] * solMap[vars[[i]]], 0]],
    {i, 1, Length[vars]}
  ];
  rhsShift = Together[rhs - knownSum];
  noteShift = addNote[-knownSum];

  AppendTo[steps, {formatSubstLHS[row, vars, solMap, elimVar, True], rhs, noteShift}];

  If[PossibleZeroQ[coeffU], Return[<|"Type" -> If[PossibleZeroQ[rhsShift], "INFINITE", "NONE"], "Steps" -> steps|>]];

  If[coeffU === 1,
    AppendTo[steps, {tf[elimVar], tf[rhsShift], ""}],
    (
      AppendTo[steps, {tf[coeffU elimVar], tf[rhsShift], divNote[coeffU]}];
      AppendTo[steps, {tf[elimVar], tf[Together[rhsShift/coeffU]], ""}]
    )
  ];

  <|"Type" -> "ONE", "Value" -> Together[rhsShift/coeffU], "Steps" -> steps|>
];

reduceOnceByElimination[eqs_List, vars_List] := Module[
  {n = Length[vars], A, b, content = {}, data2, sumRow, sumRHS, elimIdx, keepIdx, keepVar, elimVar, pivotEq, newEq, cls,
    red, A2, b2, remVars, idx},

  A = eqs[[All, 1]]; b = eqs[[All, 2]];

  If[n === 2,
    (
      data2 = eliminationStart2[A, b, vars];
      content = Join[content, data2["content"]];

      sumRow = Total[data2["A_mod"]];
      sumRHS = Total[data2["b_mod"]];

      elimIdx = If[data2["EliminatedVariable"] === "X", 1, 2];
      keepIdx = 3 - elimIdx;
      elimVar = vars[[elimIdx]];
      keepVar = vars[[keepIdx]];

      AppendTo[content, makeStepHeader["Sčítanie rovníc"]];
      AppendTo[content, "Sčítame rovnice, aby sme vyrušili premennú " <> ToString[elimVar] <> "."];
      AppendTo[content, renderAddition2[data2["A_mod"], data2["b_mod"], vars]];

      newEq = {{{sumRow[[keepIdx]]}, sumRHS}};
      cls = equationClass[{sumRow[[keepIdx]]}, sumRHS];

      pivotEq = {A[[1]], b[[1]]};

      Return[<|
        "Content" -> content,
        "NewEqs" -> newEq,
        "NewVars" -> {keepVar},
        "ElimVar" -> elimVar,
        "PivotEq" -> pivotEq,
        "Classes" -> {cls}
      |>];
    )
  ];

  (* n === 3 *)
  red = reduce3to2[A, b, vars];
  If[red === $Failed, Return[$Failed]];

  content = Join[content, red["Content"]];
  A2 = red["A2"]; b2 = red["b2"]; remVars = red["remVars"];
  idx = red["SubstRowIndex"];

  pivotEq = {A[[idx]], b[[idx]]};

  <|
    "Content" -> content,
    "NewEqs" -> Table[{A2[[i]], b2[[i]]}, {i, 1, 2}],
    "NewVars" -> remVars,
    "ElimVar" -> red["elimVar"],
    "PivotEq" -> pivotEq,
    "Classes" -> (equationClass @@@ Thread[{A2, b2}])
  |>
];

elimSteps[A_, b_, vars_, data_ : <||>] := Module[
  {content = {}, kind, eqs, varsNow, stack = {}, step, lastSolve, solMap, back, solVec, origVars = vars, k},

  kind = Lookup[data, "type", "ONE"];

  (* hard normalizácia je špeciálny krok *)
  If[TrueQ[data["HardQ"]] && Length[vars] === 3, content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  eqs = Table[{A[[i]], b[[i]]}, {i, 1, Length[vars]}];
  varsNow = vars;

  While[Length[varsNow] > 1,
    step = reduceOnceByElimination[eqs, varsNow];
    If[step === $Failed, Return[$Failed]];
    content = Join[content, step["Content"]];

    If[AnyTrue[step["Classes"], # === "CONTRADICTION" &],
      AppendTo[content, makeStepHeader["Záver"]];
      AppendTo[content, "Pri eliminácii vznikol spor (nepravdivá rovnosť), preto sústava nemá riešenie."];
      Return[<|"Content" -> content, "Solution" -> "NONE"|>]
    ];

    If[AllTrue[step["Classes"], # === "IDENTITY" &],
      AppendTo[content, makeStepHeader["Záver"]];
      AppendTo[content, "Pri eliminácii vyšla identita (pravdivá rovnosť), preto sústava má nekonečne veľa riešení."];
      Return[<|"Content" -> content, "Solution" -> "INFINITE"|>]
    ];

    AppendTo[stack, <|"PivotEq" -> step["PivotEq"], "VarsBefore" -> varsNow, "ElimVar" -> step["ElimVar"]|>];
    eqs = step["NewEqs"];
    varsNow = step["NewVars"];
  ];

  lastSolve = solveOneVarEquationSteps[First[eqs], varsNow];

  If[lastSolve["Type"] =!= "ONE",
    AppendTo[content, makeStepHeader["Záver"]];
    AppendTo[content, If[lastSolve["Type"] === "NONE", "Sústava nemá riešenie.", "Sústava má nekonečne veľa riešení."]];
    Return[<|"Content" -> content, "Solution" -> If[lastSolve["Type"] === "NONE", "NONE", "INFINITE"]|>]
  ];

  AppendTo[content, makeStepHeader["Výpočet poslednej neznámej"]];
  AppendTo[content, alignedEquations[lastSolve["Content"]]];
  AppendTo[content, highlightGrid[alignedEquations[{{varsNow[[1]], tf[lastSolve["Value"]], ""}}]]];

  solMap = <|varsNow[[1]] -> lastSolve["Value"]|>;

  Do[
    AppendTo[content, makeStepHeader["Dosadenie do pôvodnej rovnice"]];
    back = backSubstituteElimVarSteps[stack[[k, "PivotEq"]], stack[[k, "VarsBefore"]], solMap, stack[[k, "ElimVar"]]];
    If[back["Type"] =!= "ONE", Return[<|"Content" -> content, "Solution" -> If[back["Type"] === "NONE", "NONE", "INFINITE"]|>]];
    AppendTo[content, alignedEquations[back["Steps"]]];
    AppendTo[content, highlightGrid[alignedEquations[{{stack[[k, "ElimVar"]], tf[back["Value"]], ""}}]]];
    solMap[stack[[k, "ElimVar"]]] = back["Value"],
    {k, Length[stack], 1, -1}
  ];

  solVec = (solMap /@ origVars);

  If[kind === "ONE",
    content = addCorrectnessCheck[content, A, b, origVars, solVec];
    Return[<|"Content" -> content, "Solution" -> solVec|>]
  ];

  <|"Content" -> content, "Solution" -> If[kind === "NONE", "NONE", "INFINITE"]|>
];

pickElimVar2[A_] := Module[{scores},
  scores = Table[
    Module[{c1 = A[[1, i]], c2 = A[[2, i]], lcm},
      If[c1 == 0 || c2 == 0, 9999,
        lcm = LCM[Abs[c1], Abs[c2]];
        lcm + If[(lcm/Abs[c1]) > 1 && (lcm/Abs[c2]) > 1, 1000, 0]
      ]
    ],
    {i, 1, 2}
  ];
  If[scores[[2]] < scores[[1]], 2, 1]
];

pickElimVar3[A_] := Module[{zp, scorePair, zeroCols, scoreZeroCol, baseScores},
  zp = zeroCoeff3[A];
  scorePair[c1_, c2_] := If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]];
  zeroCols = Select[Range[3], zp["ZeroRowsByCol"][[#]] =!= {} &];
  If[zeroCols =!= {},
    scoreZeroCol[j_] := Module[{zeroRows, nonZeroRows, c1, c2},
      zeroRows = zp["ZeroRowsByCol"][[j]];
      nonZeroRows = Complement[Range[3], zeroRows];
      Which[
        Length[nonZeroRows] >= 2, {c1, c2} = A[[nonZeroRows[[1 ;; 2]], j]]; {0, scorePair[c1, c2]},
        Length[zeroRows] >= 2, {1, 0},
        True, {2, Infinity}
      ]
    ];
    Return[First @ MinimalBy[zeroCols, scoreZeroCol]];
  ];
  baseScores = Table[scorePair[A[[1, j]], A[[2, j]]] + scorePair[A[[1, j]], A[[3, j]]], {j, 1, 3}];
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
    AppendTo[content, "Chceme vyrušiť premennú " <> ToString[targetVar] <> ". Rovnice preto prenásobíme tak, aby mali pri nej rovnaký koeficient s opačným znamienkom."],
    AppendTo[content, "Koeficienty pri premennej " <> ToString[targetVar] <> " sú už opačné, takže môžeme hneď sčítať rovnice a premennú vyrušiť."]
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
    AppendTo[content, alignedEquations[{{formatEquationLHS[A[[1]], choice, vars], b[[1]], ""}, {formatEquationLHS[A[[2]], choice, vars], b[[2]], ""}}]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "A_mod" -> {m1 A[[1]], m2 A[[2]]}, "b_mod" -> {m1 b[[1]], m2 b[[2]]}|>
];

pickBestElimPair[rowIdx_List, elimCol_Integer, A_] := Module[{pairs, scorePair},
  pairs = Subsets[rowIdx, {2}];
  scorePair[{i_, j_}] := Module[{c1 = A[[i, elimCol]], c2 = A[[j, elimCol]]},
    If[c1 == 0 || c2 == 0, Infinity, If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]]]
  ];
  First @ MinimalBy[pairs, scorePair]
];

pickSubstRow3[zp_, elimCol_Integer, A_] := Module[{rows = Range[3], allNonZero, elimNonZero, score},
  allNonZero = Select[rows, zp["ZeroColsByRow"][[#]] === {} && A[[#, elimCol]] =!= 0 &];
  elimNonZero = Select[rows, A[[#, elimCol]] =!= 0 &];
  score[i_] := Module[{row = A[[i]], c = A[[i, elimCol]]}, {If[Abs[c] == 1, 0, 1], Abs[c], Total[Abs[row]]}];
  Which[
    allNonZero =!= {}, <|"Index" -> First @ MinimalBy[allNonZero, score], "AllNonZeroQ" -> True|>,
    elimNonZero =!= {}, <|"Index" -> First @ MinimalBy[elimNonZero, score], "AllNonZeroQ" -> False|>,
    True, $Failed
  ]
];

reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_, tagA_, tagB_] := Module[
  {content = {}, valA = rowA[[elimCol]], valB = rowB[[elimCol]], choiceStr = {"X", "Y", "Z"}[[elimCol]], lcm, m1, m2,
    rowA2, rhsA2, rowB2, rhsB2, newRow, newRHS, rows1, rows2},

  If[valA == 0 || valB == 0,
    AppendTo[content, alignedEquations[{{formatEquationLHS[rowA, choiceStr, vars], rhsA, ""}, {formatEquationLHS[rowB, choiceStr, vars], rhsB, ""}}]];
    If[valB == 0, {newRow, newRHS} = {rowB, rhsB}, {newRow, newRHS} = {rowA, rhsA}],
    lcm = LCM[Abs[valA], Abs[valB]];
    m1 = lcm/Abs[valA];
    m2 = lcm/Abs[valB];
    If[Sign[valA] == Sign[valB], m2 = -m2];

    rows1 = {{formatEquationLHS[rowA, choiceStr, vars], rhsA, multNote[m1]}, {formatEquationLHS[rowB, choiceStr, vars], rhsB, multNote[m2]}};

    rowA2 = m1 rowA; rhsA2 = m1 rhsA;
    rowB2 = m2 rowB; rhsB2 = m2 rhsB;

    rows2 = {{formatEquationLHS[rowA2, "", vars], rhsA2, ""}, {formatEquationLHS[rowB2, "", vars], rhsB2, ""}};
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]];

    newRow = rowA2 + rowB2;
    newRHS = rhsA2 + rhsB2;
  ];

  AppendTo[content, alignedEquations[{{Style[formatEquationLHS[newRow, "", vars], Darker[Green, 0.2]], Style[newRHS, Darker[Green, 0.2]], ""}}]];
  <|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>
];

renderAddition2[rowMod_, rhsMod_, vars_] := alignedEquations[{{
  Row[{tf[rowMod[[1, 1]] vars[[1]]], signBtwTerms[rowMod[[2, 1]]], tf[Abs[rowMod[[2, 1]]] vars[[1]]],
    signBtwTerms[rowMod[[1, 2]]], tf[Abs[rowMod[[1, 2]]] vars[[2]]], signBtwTerms[rowMod[[2, 2]]], tf[Abs[rowMod[[2, 2]]] vars[[2]]]}],
  Row[{rhsMod[[1]], signBtwTerms[rhsMod[[2]]], Abs[rhsMod[[2]]]}],
  ""
}}];

reduce3to2[A_, b_, vars_] := Module[
  {content = {}, zp, substPick, elimCol, elimVar, zeroRows, nonZeroRows, iKeep, rowIV, rhsIV, rowV, rhsV, remCols, remVars,
    A2, b2, twoCombosQ, pair, i1, i2},

  AppendTo[content, makeStepHeader["Redukcia sústavy 3x3 na 2x2"]];
  zp = zeroCoeff3[A];
  elimCol = pickElimVar3[A];
  elimVar = vars[[elimCol]];
  AppendTo[content, "Vyrušíme premennú " <> ToString[elimVar] <> ", aby sme získali sústavu 2×2."];

  zeroRows = zp["ZeroRowsByCol"][[elimCol]];
  nonZeroRows = Complement[Range[3], zeroRows];

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
      AppendTo[content, alignedEquations[{{formatEquationLHS[rowV, "", vars], rhsV, ""}}]],
      {i1, i2} = zeroRows[[1 ;; 2]];
      rowIV = A[[i1]]; rhsIV = b[[i1]];
      rowV = A[[i2]]; rhsV = b[[i2]];

      AppendTo[content, Style["a) Rovnice bez vyrušovanej premennej (použijeme ich priamo):", Italic]];
      AppendTo[content, alignedEquations[{{formatEquationLHS[rowIV, "", vars], rhsIV, ""}, {formatEquationLHS[rowV, "", vars], rhsV, ""}}]];
    ],
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

  <|"Content" -> content, "A2" -> A2, "b2" -> b2, "remVars" -> remVars, "elimCol" -> elimCol, "elimVar" -> elimVar,
    "twoCombosQ" -> twoCombosQ, "SubstRowIndex" -> substPick["Index"], "SubstAllNonZeroQ" -> substPick["AllNonZeroQ"]|>
];

(* ~-~-~ SUBSTITUTION ~-~-~ *)

backSubstituteSubstVarSteps[solvedVar_, expr_, solMap_Association] := Module[
  {filledExpr, steps = {}},
  AppendTo[steps, {tf[solvedVar], tf[expr], substNote[solMap, Keys[solMap], {0, 0, 0}, Keys[solMap]]}];
  filledExpr = expr /. solMap;
  AppendTo[steps, {tf[solvedVar], tf[filledExpr], ""}];
  AppendTo[steps, {tf[solvedVar], tf[Together[filledExpr]], ""}];
  <|"Value" -> Together[filledExpr], "Steps" -> steps|>
];

reduceOnceBySubstitution[eqs_List, vars_List] := Module[
  {n = Length[vars], A, b, content = {}, rI, cI, solveData, substRule, elimVar, remVars, otherRows, res, newEq, cls,
    red, A2, b2},

  A = eqs[[All, 1]]; b = eqs[[All, 2]];

  If[n === 2,
    (
      {rI, cI} = pickSubstSolve2[A, b, vars];
      elimVar = vars[[cI]];
      remVars = Delete[vars, cI];

      AppendTo[content, makeStepHeader["Vyjadrenie neznámej"]];
      AppendTo[content, "Z " <> ToString[rI] <> ". rovnice vyjadríme neznámu " <> ToString[elimVar] <> "."];

      solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
      AppendTo[content, highlightGrid[alignedEquations[solveData["Content"]]]];
      substRule = solveData["Rule"];

      AppendTo[content, makeStepHeader["Dosadenie"]];
      AppendTo[content, "Výraz dosadíme do druhej rovnice a upravíme ju."];

      res = substituteIntoEquationSteps[A[[3 - rI]], b[[3 - rI]], vars, substRule, remVars];
      AppendTo[content, alignedEquations[res["Content"]]];

      newEq = {res["NewEq"][[1]], res["NewEq"][[2]]};
      cls = equationClass[newEq[[1]], newEq[[2]]];

      Return[<|
        "Content" -> content,
        "NewEqs" -> {newEq},
        "NewVars" -> remVars,
        "SolvedVar" -> elimVar,
        "RuleExpr" -> substRule[[2]],
        "Classes" -> {cls}
      |>];
    )
  ];

  (* n === 3 *)
  red = reduce3to2BySubstitution[A, b, vars];
  If[red === $Failed, Return[$Failed]];

  content = Join[content, red["Content"]];
  A2 = red["A2"]; b2 = red["b2"]; remVars = red["remVars"];

  <|
    "Content" -> content,
    "NewEqs" -> Table[{A2[[i]], b2[[i]]}, {i, 1, 2}],
    "NewVars" -> remVars,
    "SolvedVar" -> red["elimVar"],
    "RuleExpr" -> red["substRule"][[2]],
    "Classes" -> (equationClass @@@ Thread[{A2, b2}])
  |>
];

substSteps[A_, b_, vars_, data_ : <||>] := Module[
  {content = {}, kind, eqs, varsNow, stack = {}, step, lastSolve, solMap, back, solVec, origVars = vars, k},

  kind = Lookup[data, "type", "ONE"];

  (* hard normalizácia je špeciálny krok *)
  If[TrueQ[data["HardQ"]] && Length[vars] === 3, content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  eqs = Table[{A[[i]], b[[i]]}, {i, 1, Length[vars]}];
  varsNow = vars;

  While[Length[varsNow] > 1,
    step = reduceOnceBySubstitution[eqs, varsNow];
    If[step === $Failed, Return[$Failed]];
    content = Join[content, step["Content"]];

    If[AnyTrue[step["Classes"], # === "CONTRADICTION" &],
      AppendTo[content, makeStepHeader["Záver"]];
      AppendTo[content, "Pri úpravách vznikol spor (nepravdivá rovnosť), preto sústava nemá riešenie."];
      Return[<|"Content" -> content, "Solution" -> "NONE"|>]
    ];

    If[AllTrue[step["Classes"], # === "IDENTITY" &],
      AppendTo[content, makeStepHeader["Záver"]];
      AppendTo[content, "Pri úpravách vyšla identita (pravdivá rovnosť), preto sústava má nekonečne veľa riešení."];
      Return[<|"Content" -> content, "Solution" -> "INFINITE"|>]
    ];

    AppendTo[stack, <|"SolvedVar" -> step["SolvedVar"], "Expr" -> step["RuleExpr"]|>];
    eqs = step["NewEqs"];
    varsNow = step["NewVars"];
  ];

  lastSolve = solveOneVarEquationSteps[First[eqs], varsNow];

  If[lastSolve["Type"] =!= "ONE",
    AppendTo[content, makeStepHeader["Záver"]];
    AppendTo[content, If[lastSolve["Type"] === "NONE", "Sústava nemá riešenie.", "Sústava má nekonečne veľa riešení."]];
    Return[<|"Content" -> content, "Solution" -> If[lastSolve["Type"] === "NONE", "NONE", "INFINITE"]|>]
  ];

  AppendTo[content, makeStepHeader["Výpočet poslednej neznámej"]];
  AppendTo[content, alignedEquations[lastSolve["Content"]]];
  AppendTo[content, highlightGrid[alignedEquations[{{varsNow[[1]], tf[lastSolve["Value"]], ""}}]]];

  solMap = <|varsNow[[1]] -> lastSolve["Value"]|>;

  Do[
    AppendTo[content, makeStepHeader["Dopočítanie neznámej"]];
    back = backSubstituteSubstVarSteps[stack[[k, "SolvedVar"]], stack[[k, "Expr"]], solMap];
    AppendTo[content, alignedEquations[back["Steps"]]];
    AppendTo[content, highlightGrid[alignedEquations[{{stack[[k, "SolvedVar"]], tf[back["Value"]], ""}}]]];
    solMap[stack[[k, "SolvedVar"]]] = back["Value"],
    {k, Length[stack], 1, -1}
  ];

  solVec = (solMap /@ origVars);

  If[kind === "ONE",
    content = addCorrectnessCheck[content, A, b, origVars, solVec];
    Return[<|"Content" -> content, "Solution" -> solVec|>]
  ];

  <|"Content" -> content, "Solution" -> If[kind === "NONE", "NONE", "INFINITE"]|>
];

orderTermsByVars[terms_List, vars_List] := Module[{pairs, varOrder, key},
  pairs = Select[terms, MatchQ[#, {_, _}] &];
  varOrder = AssociationThread[vars -> Range[Length[vars]]];
  key[t_] := Which[t[[2]] === None, Infinity, KeyExistsQ[varOrder, t[[2]]], varOrder[t[[2]]], True, Infinity];
  SortBy[pairs, key]
];

linearDecompose[expr_, vrs_List] := Module[{ee, coeffs, c0},
  ee = Expand[Together[expr]];
  coeffs = Together /@ (Coefficient[ee, #] & /@ vrs);
  c0 = Together[ee /. (Rule[#, 0] & /@ vrs)];
  {coeffs, c0}
];

formatLinearExpr[expr_, vrs_List] := Module[{coeffs, c0, terms = {}},
  {coeffs, c0} = linearDecompose[expr, vrs];
  Do[If[!PossibleZeroQ[coeffs[[k]]], AppendTo[terms, {coeffs[[k]], vrs[[k]]}]], {k, 1, Length[vrs]}];
  If[!PossibleZeroQ[c0], AppendTo[terms, {c0, None}]];
  renderTermsRow[terms]
];

formatSubstOnceLHS[row_, vars_, targetVar_, substExpr_] := Module[
  {terms = {}, first = True, addTerm},
  addTerm[content_, sign_] := (AppendTo[terms, If[first, If[sign === -1, Row[{"-", content}], content], Row[{If[sign === -1, " - ", " + "], content}]]]; first = False);
  Do[
    With[{c = row[[i]], v = vars[[i]]},
      If[c =!= 0,
        If[v === targetVar, addTerm[coeffVal[Abs[c], substExpr], Sign[c]], addTerm[tf[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]]]
      ]
    ],
    {i, 1, Length[vars]}
  ];
  If[terms === {}, tf[0], Row[terms]]
];

pickSubstSolve2[A_, b_, vars_] := Module[{scores},
  scores = Table[With[{c = A[[i, j]]}, If[c == 0, Infinity, If[Abs[c] == 1, 0, Abs[c] + 10]]], {i, 2}, {j, 2}];
  First @ Position[scores, Min[scores]]
];

pickSubstSolve3[A_, b_, vars_] := Module[{scores},
  scores = Table[With[{c = A[[i, j]]}, If[c == 0, Infinity, If[Abs[c] == 1, 0 + Count[A[[i]], 0]*(-1), Abs[c] + 100]]], {i, 3}, {j, 3}];
  First @ Position[scores, Min[scores]]
];

solveForVarSteps[row_, rhs_, vars_, varIndex_] := Module[
  {targetVar, c, otherTerms, rhsExpr, solExpr, stepsIso, moveNote, divNoteVal, currentLHS, isoLHS},
  targetVar = vars[[varIndex]];
  c = row[[varIndex]];
  otherTerms = Delete[row, varIndex] . Delete[vars, varIndex];

  stepsIso = {};
  currentLHS = formatEquationLHS[row, "", vars];
  moveNote = If[PossibleZeroQ[otherTerms], "", addNote[-otherTerms]];

  If[PossibleZeroQ[otherTerms],
    divNoteVal = Which[c == 1, "", c == -1, multNote[-1], True, divNote[c]];
    AppendTo[stepsIso, {currentLHS, tf[rhs], divNoteVal}],
    AppendTo[stepsIso, {currentLHS, tf[rhs], moveNote}]
  ];

  rhsExpr = rhs - otherTerms;
  isoLHS = tf[If[Abs[c] == 1 && c > 0, targetVar, If[Abs[c] == 1, -targetVar, c targetVar]]];

  If[!PossibleZeroQ[otherTerms],
    divNoteVal = Which[c == 1, "", c == -1, multNote[-1], True, divNote[c]];
    If[c =!= 1, AppendTo[stepsIso, {isoLHS, tf[rhsExpr], divNoteVal}]];
  ];

  solExpr = Expand[Together[rhsExpr / c]];

  If[!(PossibleZeroQ[otherTerms] && c == 1), AppendTo[stepsIso, {tf[targetVar], formatLinearExpr[solExpr, DeleteCases[vars, targetVar]], ""}]];

  <|"Content" -> stepsIso, "Rule" -> (targetVar -> solExpr), "Expr" -> solExpr, "Var" -> targetVar|>
];

substituteIntoEquationSteps[row_, rhs_, vars_, rule_, remainingVars_] := Module[
  {targetVar, substExpr, stepRows, currentLHS, sNote, pos, targetCoeff, baseTerms, subCoeffs, subConst, distTerms, lhsCombined,
    newRow, constLeft, newRHS},

  targetVar = rule[[1]];
  substExpr = rule[[2]];
  stepRows = {};

  currentLHS = formatEquationLHS[row, "", vars];
  sNote = substNote[<|targetVar -> substExpr|>, {targetVar}, row, vars];
  AppendTo[stepRows, {currentLHS, tf[rhs], sNote}];

  pos = First @ First @ Position[vars, targetVar];
  targetCoeff = row[[pos]];

  baseTerms = Select[Delete[MapThread[List, {row, vars}], pos], #[[1]] =!= 0 &];
  {subCoeffs, subConst} = linearDecompose[substExpr, remainingVars];

  distTerms = Join[
    DeleteCases[Table[If[PossibleZeroQ[subCoeffs[[k]]], Nothing, {targetCoeff*subCoeffs[[k]], remainingVars[[k]]}], {k, 1, Length[remainingVars]}], Nothing],
    If[PossibleZeroQ[subConst], {}, {{targetCoeff*subConst, None}}],
    baseTerms
  ];

  If[Abs[targetCoeff] =!= 1, AppendTo[stepRows, {formatSubstOnceLHS[row, vars, targetVar, substExpr], tf[rhs], "roznásobenie"}]];
  AppendTo[stepRows, {renderTermsRow[orderTermsByVars[distTerms, vars], "Symbolic"], tf[rhs], "zlučovanie členov"}];

  lhsCombined = Expand[row.vars /. rule];
  newRow = Coefficient[lhsCombined, #] & /@ remainingVars;
  constLeft = lhsCombined /. (Rule[#, 0] & /@ remainingVars);
  newRHS = rhs - constLeft;

  AppendTo[stepRows, {formatEquationLHS[newRow, "", remainingVars], tf[newRHS], ""}];
  <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>
];

reduce3to2BySubstitution[A_, b_, vars_] := Module[
  {content = {}, rI, cI, solveData, elimVar, substRule, otherRowsIdx, A2, b2, remVars, remCols, idx},

  {rI, cI} = pickSubstSolve3[A, b, vars];
  elimVar = vars[[cI]];

  AppendTo[content, makeStepHeader["Vyjadrenie neznámej z jednej rovnice"]];
  AppendTo[content, "Vyberieme si " <> ToString[rI] <> ". rovnicu a vyjadríme z nej neznámu " <> ToString[elimVar] <> "."];

  solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
  AppendTo[content, highlightGrid[alignedEquations[solveData["Content"]]]];

  substRule = solveData["Rule"];

  AppendTo[content, makeStepHeader["Dosadenie do zvyšných rovníc"]];
  AppendTo[content, "Získaný výraz dosadíme do ostatných dvoch rovníc a upravíme ich."];

  otherRowsIdx = Delete[Range[3], rI];
  remCols = Delete[Range[3], cI];
  remVars = vars[[remCols]];
  A2 = {}; b2 = {};

  Do[
    idx = otherRowsIdx[[k]];
    AppendTo[content, Style["Dosadenie do " <> ToString[idx] <> ". rovnice:", Italic]];
    With[{res = substituteIntoEquationSteps[A[[idx]], b[[idx]], vars, substRule, remVars]},
      AppendTo[content, alignedEquations[res["Content"]]];
      AppendTo[A2, res["NewEq"][[1]]];
      AppendTo[b2, res["NewEq"][[2]]];
    ],
    {k, 1, 2}
  ];

  <|"Content" -> content, "A2" -> A2, "b2" -> b2, "remVars" -> remVars, "elimVar" -> elimVar, "substRule" -> substRule, "elimCol" -> cI, "sourceRow" -> rI|>
];

(* ~-~-~ MAIN ~-~-~ *)

normalizeEquationSpec[spec_Association] := Module[{s = spec},
  s["DimByDiff"] = Lookup[s, "DimByDiff", dimByDifficulty];
  s["VarsByDim"] = Lookup[s, "VarsByDim", varsByDim];
  s["RenderTask"] = Lookup[s, "RenderTask", Missing["NotSet"]];
  s["RenderResult"] = Lookup[s, "RenderResult", Missing["NotSet"]];
  s["VisualizationFn"] = Lookup[s, "VisualizationFn", None];
  s
];

buildEquationRun[spec0_Association, diff_String, opts___?OptionQ] := Module[
  {spec = normalizeEquationSpec[spec0], entryFn, msgPrefix, stRaw, st, dim, vars, data, A, b},

  entryFn = spec["EntryFn"];
  msgPrefix = spec["MsgPrefix"];

  stRaw = OptionValue[entryFn, {opts}, SolutionType];
  If[!TrueQ[ValidateSolutionType[stRaw]], Message[MessageName[msgPrefix, "badst"], stRaw]; Return[$Failed]];
  st = ResolveSolutionType[stRaw];

  dim = spec["DimByDiff"][diff];
  vars = spec["VarsByDim"][dim];

  data = WithRetries @ Function[Null, spec["GenerateData"][dim, diff, st, opts]];
  If[data === $Failed, Message[MessageName[msgPrefix, "fail"]]; Return[$Failed]];

  A = data["A"]; b = data["b"];

  <|
    "Spec" -> spec,
    "Diff" -> diff,
    "Dim" -> dim,
    "Vars" -> vars,
    "SolutionType" -> st,
    "Data" -> data,
    "A" -> A,
    "b" -> b
  |>
];

buildEquationSteps[run_Association] := Module[
  {spec = run["Spec"], steps},
  steps = spec["StepsFn"][run["A"], run["b"], run["Vars"], run["Data"]];
  If[steps === $Failed, Message[MessageName[spec["MsgPrefix"], "fail"]]; Return[$Failed]];
  steps
];

renderTaskDefault[run_Association] := Module[
  {data = run["Data"], A = run["A"], b = run["b"], vars = run["Vars"], dim = run["Dim"]},
  printTextCell["Riešte sústavu rovníc"];
  If[run["Diff"] === "HARD" && KeyExistsQ[data, "EqDisplay"],
    printFormulaCell @ alignedEquations[data["EqDisplay"]],
    printFormulaCell @ alignedEquations @ Table[{formatEquationLHS[A[[i]], "", vars], b[[i]], ""}, {i, 1, dim}]
  ];
];

renderResultDefault[run_Association, steps_Association] := Module[
  {sol = steps["Solution"], dim = run["Dim"], vars = run["Vars"], A = run["A"], b = run["b"]},
  Switch[sol,
    "NONE",
    printTextCell["Sústava nemá riešenie (pri riešení vznikol spor)."],
    "INFINITE",
    printInfiniteResult[A, b, vars],
    _,
    CellPrint @ Cell[
      BoxData @ ToBoxes[
        If[dim == 2,
          Row[{"Riešením sústavy rovníc je usporiadaná dvojica čísel [x,y] = ", Style[Row[{"[", tft[sol[[1]]], ", ", tft[sol[[2]]], "]"}], Bold]}],
          Row[{"Riešením sústavy rovníc je usporiadaná trojica čísel [x,y,z] = ", Style[Row[{"[", tft[sol[[1]]], ", ", tft[sol[[2]]], ", ", tft[sol[[3]]], "]"}], Bold]}]
        ],
        TraditionalForm
      ],
      "Text",
      ShowStringCharacters -> False
    ]
  ];
];

visualizationDefault[run_Association, steps_Association] := Module[
  {dim = run["Dim"], A = run["A"], b = run["b"], vars = run["Vars"], sol = steps["Solution"]},
  If[dim == 2, visualize2[A, b, vars, sol], visualize3[A, b, vars, sol]]
];

renderEquationRun[run_Association, steps_Association, mode_String, opts___?OptionQ] := Module[
  {spec = run["Spec"], entryFn, visQ},

  entryFn = spec["EntryFn"];

  printSectionCell[spec["SectionTitle"]];
  printSubsectionCell["Zadanie"];
  spec["RenderTask"][run];
  printTextCell[spec["TaskInstruction"]];

  If[mode === "TASK_STEPS_RESULT",
    stepsCounter = 0;
    printSubsectionCell["Postup"];
    Scan[renderStepItem, steps["Content"]];
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    spec["RenderResult"][run, steps];

    visQ = TrueQ @ OptionValue[entryFn, {opts}, Visualization];
    If[visQ,
      If[Head[spec["VisualizationFn"]] === Function, spec["VisualizationFn"][run, steps], visualizationDefault[run, steps]]
    ];
  ];
];

runEquationGenerator[spec0_Association, diff_String, mode_String, opts___?OptionQ] := Module[
  {spec = normalizeEquationSpec[spec0], entryFn, msgPrefix, run, steps, sol},

  entryFn = spec["EntryFn"];
  msgPrefix = spec["MsgPrefix"];

  If[!TrueQ[ValidateDifficulty[diff]], Message[MessageName[msgPrefix, "baddiff"], diff]; Return[$Failed]];
  If[!TrueQ[ValidateMode[mode]], Message[MessageName[msgPrefix, "badmode"], mode]; Return[$Failed]];

  (* defaultné renderery, ak nie sú dodané v spec *)
  If[spec["RenderTask"] === Missing["NotSet"], spec["RenderTask"] = renderTaskDefault];
  If[spec["RenderResult"] === Missing["NotSet"], spec["RenderResult"] = renderResultDefault];
  If[spec["VisualizationFn"] === None, spec["VisualizationFn"] = Function[{r, s}, visualizationDefault[r, s]]];

  run = buildEquationRun[spec, diff, opts];
  If[run === $Failed, Return[$Failed]];

  steps = buildEquationSteps[run];
  If[steps === $Failed, Return[$Failed]];

  renderEquationRun[run, steps, mode, opts];

  sol = steps["Solution"];
  sol
];

buildSpecElimination[] := <|
  "EntryFn" -> GenElimination,
  "MsgPrefix" -> GenElimination,
  "SectionTitle" -> "Eliminačná metóda",
  "TaskInstruction" -> "Riešte v množine reálnych čísel eliminačnou metódou (sčítaním rovníc).",
  "GenerateData" -> Function[{dim, diff, st, opts}, generateLinearSystem[dim, diff, st, RequireUnitCoeff -> False]],
  "StepsFn" -> Function[{A, b, vars, data}, elimSteps[A, b, vars, data]]
|>;

buildSpecSubstitution[] := <|
  "EntryFn" -> GenSubstitution,
  "MsgPrefix" -> GenSubstitution,
  "SectionTitle" -> "Dosadzovacia (substitučná) metóda",
  "TaskInstruction" -> "Riešte v množine reálnych čísel dosadzovacou (substitučnou) metódou.",
  "GenerateData" -> Function[{dim, diff, st, opts}, generateLinearSystem[dim, diff, st, RequireUnitCoeff -> True]],
  "StepsFn" -> Function[{A, b, vars, data}, substSteps[A, b, vars, data]]
|>;

GenElimination[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {spec = buildSpecElimination[]},
  runEquationGenerator[spec, diff, mode, opts]
];

GenSubstitution[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {spec = buildSpecSubstitution[]},
  runEquationGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];