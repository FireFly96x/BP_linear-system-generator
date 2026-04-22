(* ::Package:: *)

BeginPackage["`EquationGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

GenElimination::usage = "GenElimination[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc eliminačnou metódou (sčítaním rovníc).
diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

GenSubstitution::usage = "GenSubstitution[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc dosadzovacou (substitučnou) metódou.
diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

GenElimination::baddiff = "Neplatná úroveň obtiažnosti `1`. Použiť \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenElimination::badmode = "Neplatný režim výstupu `1`. Použiť \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenElimination::badst   = "Neplatný typ riešenia `1`. Použiť Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenElimination::fail    = "Nepodarilo sa vygenerovať sústavu s požadovanými parametrami.";

GenSubstitution::baddiff = GenElimination::baddiff;
GenSubstitution::badmode = GenElimination::badmode;
GenSubstitution::badst   = GenElimination::badst;
GenSubstitution::fail    = GenElimination::fail;

$CommonEquationOptions = {SolutionType -> Automatic, Visualization -> False};

Options[GenElimination] = $CommonEquationOptions;
Options[GenSubstitution] = $CommonEquationOptions;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.8, 0.1, 0.1} -> {"ONE", "NONE", "INFINITE"}]];

DimensionByDifficulty[diff_String] := Switch[diff, "EASY", 2, "MEDIUM" | "HARD", 3];
buildVars[n_] := Take[{x, y, z}, n];

(* ~-~-~ RENDER / FORMATTING ~-~-~ *)

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;
printCellStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False], Print[expr]];
printTextCell[str_String] := printCellStyle[str, "Text"];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printTextExprCell[expr_] := printCellStyle[BoxData @ ToBoxes[expr, StandardForm], "Text"];

printFormulaCell[expr_] := Module[{boxes},
  boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr];
  printCellStyle[boxes, "DisplayFormula"]
];

printTextBlock[text_] := Module[{ },
  If[StringQ[text],
    printTextCell[text],
    printCellStyle[BoxData @ ToBoxes[text, StandardForm], "Text"]
  ]
];

SetAttributes[addGap, HoldFirst];
addGap[content_, h_ : 5] := AppendTo[
  content,
  Cell["", "Text", CellMargins -> {{Inherited, Inherited}, {0, 0}}, CellSize -> {Automatic, h}]
];

withStepCounter[renderFn_] := Block[{stepsCounter = 0}, renderFn[]];
makeStepHeader[text_] := (stepsCounter++; Style[Row[{stepsCounter, ". ", text}], Bold]);

SetAttributes[appendStepHeader, HoldFirst];
appendStepHeader[content_, text_, gap_ : 2] := (
  If[Length[content] > 0, addGap[content, gap]];
  AppendTo[content, makeStepHeader[text]]
);

renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  Head[item] === Spacer, printCellStyle["", "Text"],
  MatchQ[item, Style[_, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

renderTermsRow[terms_List, mode_ : "Numeric", highlightVar_ : None] := Module[
  {pairs, out = {}, first = True, c, v, zeroQ, negQ, t},
  pairs = Select[terms, MatchQ[#, {_, _}] &];
  zeroQ = If[mode === "Symbolic", PossibleZeroQ, (# == 0 &)];
  negQ = If[mode === "Symbolic", (TrueQ[# < 0] &), (# < 0 &)];
  If[pairs === {} || AllTrue[pairs[[All, 1]], zeroQ], Return[tf[0]]];
  Do[
    {c, v} = pairs[[i]];
    If[zeroQ[c], Continue[]];
    t = If[v === None, tf[Abs[c]],
      tf @ If[highlightVar =!= None && v === highlightVar,
        highlightTerm[If[Abs[c] === 1, v, Abs[c] v]],
        If[Abs[c] === 1, v, Abs[c] v]
      ]
    ];
    If[first,
      If[negQ[c], out = Join[out, {"-", t}], out = Join[out, {t}]];
      first = False;,
      out = Join[out, {If[negQ[c], " - ", " + "], t}]
    ],
    {i, 1, Length[pairs]}
  ];
  If[out === {}, tf[0], Row[out]]
];


highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];


alignedEquations[data_, breaks_List : {}, gap_ : 1.25] := Module[{eq = Style["=", 16], bar = Style["|", GrayLevel[.25]], n, rowGaps, stepRow, baseGap = 0.5, bigGap = gap},
  n = Length[data];
  rowGaps = ConstantArray[baseGap, Max[n, 1]];
  rowGaps[[1]] = 0;
  Do[If[IntegerQ[b] && 1 <= b <= n - 1, rowGaps[[b + 1]] = bigGap], {b, breaks}];

  stepRow[{lhs_, rhs_, note_}] := {lhs, eq, rhs, If[note === "" || note === None, "", Style[Row[{bar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]]};
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  Grid[stepRow /@ data, Alignment -> {{Right, Center, Left, Left}}, Spacings -> {0.5, rowGaps}, BaseStyle -> {FontSize -> 14}]
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

isolateVarFromCoeffEqSteps[c_, var_, rhs_] := Module[{steps = {}, value},
  Which[
    PossibleZeroQ[c],
    <|"Type" -> "ZERO_COEFF", "Steps" -> steps, "Value" -> $Failed|>,

    c === 1, ( value = Together[rhs];
      AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "GENERAL", "Steps" -> steps, "Value" -> value|>
    ),
    c === -1, ( AppendTo[steps, {tf[-var], tf[Together[rhs]], multNote[-1]}];
      value = Together[-rhs];
      AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "GENERAL", "Steps" -> steps, "Value" -> value|>
    ),

    True, (AppendTo[steps, {tf[c var], tf[Together[rhs]], divNote[c]}];
      value = Together[rhs/c];
      AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "GENERAL", "Steps" -> steps, "Value" -> value|>
    )
  ]
];

formatSubstLHS[row_, vars_, solMap_, unknownVar_, evalMode_ : False] := Module[{terms = {}, first = True, addTerm, emitKnownTerm, emitUnknownTerm},
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

verificationStepsEquation[A_, b_, vars_, sol_] := Module[
  {content = {}, solN = Together /@ sol, lhs, prodRow, sumRow},

  Do[
    prodRow = Row @ Riffle[
      DeleteCases[
        MapThread[
          If[#1 === 0, Nothing,
            Row@{tf[#1], "\[CenterDot]", Style[wrapNegValue[#2], Bold]}
          ] &,
          {A[[i]], solN}
        ],
        Nothing
      ],
      " + "
    ];

    sumRow = Row @ Riffle[
      DeleteCases[
        MapThread[
          If[#1 === 0, Nothing, tf[Together[#1 #2]]] &,
          {A[[i]], solN}
        ],
        Nothing
      ],
      " + "
    ];

    lhs = Together[A[[i]] . solN];

    AppendTo[content,
      Grid[
        {
          {Row[{"LS", i, " = ", prodRow, " = ", sumRow, " = ", Style[tft[lhs], Bold]}]},
          {Row[{"PS", i, " = ", Style[tft[b[[i]]], Bold]}]},
          {If[lhs === b[[i]],
            Style[Row[{"LS", i, " = PS", i, " (OK)"}], Darker[Green]],
            Style[Row[{"LS", i, " \[NotEqual] PS", i, " (CHYBA)"}], Red]
          ]}
        },
        Alignment -> Left,
        Spacings -> {0, 0.4},
        BaseStyle -> {FontSize -> 13}
      ]
    ],
    {i, Length[b]}
  ];

  content
];


(* ~-~-~ HARD DISPLAY HELPERS ~-~-~ *)

scaleTerms[terms_List, k_Integer] := ({k #[[1]], #[[2]]} & /@ terms);

buildHardEq[row_, rhs_, vars_] := Module[{n = Length[vars], idxMove, cLeftPool, cLeft, varTerms, kept, moved, leftBase, rightBase},
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

buildHardDisplay[data_Association, vars_] := Module[{A = data["A"], b = data["b"], n = Length[vars], ks, eqDisp = {}, leftBaseAll = {}, rightBaseAll = {}, m, lMult, rMult},
  ks = pickHardMultipliers15[n];

  Do[
    m = buildHardEq[A[[i]], b[[i]], vars];
    lMult = scaleTerms[m["LeftBaseTerms"], ks[[i]]];
    rMult = scaleTerms[m["RightBaseTerms"], ks[[i]]];

    AppendTo[leftBaseAll, m["LeftBaseTerms"]];
    AppendTo[rightBaseAll, m["RightBaseTerms"]];
    AppendTo[eqDisp, {renderTermsRow[lMult], renderTermsRow[rMult], ""}],
    {i, 1, n}
  ];

  Join[data, <|
    "HardQ" -> True,
    "Multipliers" -> ks,
    "EqDisplay" -> eqDisp,
    "HardLeftBaseTerms" -> leftBaseAll,
    "HardRightBaseTerms" -> rightBaseAll
  |>]
];

zeroCoeff3[A_] := Module[{mask, zeroRowsByCol, zeroColsByRow},
  mask = Map[# == 0 &, A, {2}];
  zeroRowsByCol = Table[Flatten @ Position[mask[[All, j]], True], {j, 1, 3}];
  zeroColsByRow = Table[Flatten @ Position[mask[[i]], True], {i, 1, 3}];
  <|"Mask" -> mask, "ZeroRowsByCol" -> zeroRowsByCol, "ZeroColsByRow" -> zeroColsByRow|>
];

hardNormalizationSteps3[A_, b_, vars_, data_Association] := Module[{content = {}, ks, leftBase, rightBase, k, lMult, rMult, coeffL, coeffR, constL, constR, addTerms, addNoteFromTerms,
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

  appendStepHeader[content, "Normalizácia"];
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
  AppendTo[content, alignedEquations @ Table[{renderTermsRow[Transpose[{rowsStd[[i]], vars}]], rhsStdAll[[i]], ""}, {i, 1, 3}]];

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
        AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{rowsFinal[[i]], vars}]], rhsFinal[[i]], divNote[gcds[[i]]]}}]];
        rowsFinal[[i]] = rowDiv;
        rhsFinal[[i]] = rhsDiv;
      ],
      {i, 1, 3}
    ];
    AppendTo[content, "Po úpravách dostaneme sústavu rovníc v štandardnom tvare, pripravenú na riešenie:"];
    AppendTo[content, alignedEquations @ Table[{renderTermsRow[Transpose[{rowsFinal[[i]], vars}]], rhsFinal[[i]], ""}, {i, 1, 3}]];
  ];

  content
];

(* ~-~-~ INFINITE SOLUTIONS & PARAMETRIZATION ~-~-~ *)

chooseParametrization[A_, b_, vars_] := Module[{eqs, candidates, try, results},
  eqs = Thread[A . vars == b];
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

printInfiniteResult[A_, b_, vars_] := Module[{nVars = Length[vars], best, exprs, kBox, vecBox, condBox},
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

$EquationMaxRetryCount = 300;

augFromAb[A_, b_] := Join[A, List /@ b, 2];

abFromAug[aug_] := Module[{nCols},
  nCols = Length[First[aug]];
  <|"A" -> aug[[All, 1 ;; nCols - 1]], "b" -> aug[[All, nCols]]|>
];

rowAbsGCD[row_List] := Module[{g = Apply[GCD, Abs[row]]},
  If[g === 0, 1, g]
];

normalizeEquationRow[row_List] := Module[{g, first},
  g = rowAbsGCD[row];
  first = FirstCase[Most[row], x_ /; x =!= 0, 1];
  If[g > 1, row/g, If[first === -1, -row, row]]
];

matrixMaxAbs[m_] := Max[Abs[Flatten[m]]];

equationKSet[diff_String] := Switch[
  diff,
  "EASY", {-2, -1, 1, 2},
  "MEDIUM", {-3, -2, -1, 1, 2, 3},
  "HARD", {-4, -3, -2, -1, 1, 2, 3, 4}
];

equationBound[diff_String] := Switch[
  diff,
  "EASY", 40,
  "MEDIUM", 80,
  "HARD", 160
];

equationOperationCount[diff_String, dim_Integer, method_String] := Switch[
  method,
  "Elimination", Switch[diff, "EASY", dim, "MEDIUM", 2 dim, "HARD", 3 dim],
  "Substitution", Switch[diff, "EASY", dim, "MEDIUM", 2 dim, "HARD", 2 dim + 2],
  _, dim
];

makeDiagonalEquationSystem[dim_Integer, solType_String] := Module[{A, b, x0, paramCol, badRhs, pivotCount, coeffPool},
  coeffPool = {-4, -3, -2, -1, 1, 2, 3, 4};

  Switch[
    solType,

    "ONE",
    x0 = RandomInteger[{-8, 8}, dim];
    A = IdentityMatrix[dim];
    b = x0;
    <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>,

    "INFINITE",
    pivotCount = dim - 1;
    paramCol = RandomChoice[coeffPool, pivotCount];

    A = ConstantArray[0, {dim, dim}];
    Do[A[[i, i]] = 1, {i, 1, pivotCount}];
    Do[A[[i, dim]] = paramCol[[i]], {i, 1, pivotCount}];

    b = Join[RandomInteger[{-8, 8}, pivotCount], {0}];

    <|
      "A" -> A,
      "b" -> b,
      "type" -> "INFINITE",
      "ParamIdx" -> dim,
      "ParamSymbol" -> \[FormalT]
    |>,

    "NONE",
    pivotCount = dim - 1;
    paramCol = RandomChoice[coeffPool, pivotCount];
    badRhs = RandomChoice[{-4, -3, -2, -1, 1, 2, 3, 4}];

    A = ConstantArray[0, {dim, dim}];
    Do[A[[i, i]] = 1, {i, 1, pivotCount}];
    Do[A[[i, dim]] = paramCol[[i]], {i, 1, pivotCount}];

    b = Join[RandomInteger[{-8, 8}, pivotCount], {badRhs}];

    <|"A" -> A, "b" -> b, "type" -> "NONE", "BadRow" -> dim|>,

    _,
    $Failed
  ]
];

genScrambleEquationRows[diff_String, baseData_Association, method_String] := Module[
  {aug, n, bnd, kSet, opCount, pairs, targetRow, sourceRow, k, rowNew, unitPositions},

  aug = augFromAb[baseData["A"], baseData["b"]];
  n = Length[aug];

  bnd = equationBound[diff];
  kSet = equationKSet[diff];
  opCount = equationOperationCount[diff, n, method];

  pairs = DeleteCases[Tuples[Range[n], 2], {i_, i_}];

  Do[
    {targetRow, sourceRow} = RandomChoice[pairs];
    k = RandomChoice[kSet];

    rowNew = normalizeEquationRow[aug[[targetRow]] + k aug[[sourceRow]]];

    If[method === "Substitution",
      unitPositions = Position[rowNew[[1 ;; n]], 1 | -1];
      If[unitPositions =!= {} && Max[Abs[rowNew]] <= bnd, aug[[targetRow]] = rowNew],
      If[Max[Abs[rowNew]] <= bnd, aug[[targetRow]] = rowNew]
    ],
    {op, 1, opCount}
  ];

  normalizeEquationRow /@ aug
];

genScrambleElimination[diff_String, baseData_Association] := Module[{aug},
  aug = genScrambleEquationRows[diff, baseData, "Elimination"];
  Join[baseData, abFromAug[aug], <|"Aug" -> aug, "ScrambleType" -> "Elimination"|>]
];

genScrambleSubstitution[diff_String, baseData_Association] := Module[{aug},
  aug = genScrambleEquationRows[diff, baseData, "Substitution"];
  Join[baseData, abFromAug[aug], <|"Aug" -> aug, "ScrambleType" -> "Substitution"|>]
];

validEquationDataQ[data_Association, diff_String] := Module[{aug, bnd},
  If[data === $Failed, Return[False]];

  aug = Lookup[data, "Aug", augFromAb[data["A"], data["b"]]];
  bnd = equationBound[diff];

  matrixMaxAbs[aug] <= bnd
];

addHardDisplayIfNeeded[data_Association, dim_Integer, diff_String, vars_List] := Module[{out = data},
  If[diff === "HARD" && dim === 3,
    out = buildHardDisplay[out, vars]
  ];
  out
];

generateDataWithBounds[dim_Integer, diff_String, solType_String, vars_List, scrambleFn_] := Module[
  {baseData, data, retries = 0},

  While[retries < $EquationMaxRetryCount,
    baseData = makeDiagonalEquationSystem[dim, solType];

    If[baseData === $Failed,
      retries++;
      Continue[]
    ];

    data = scrambleFn[diff, baseData];

    If[TrueQ[validEquationDataQ[data, diff]],
      Return[addHardDisplayIfNeeded[data, dim, diff, vars]]
    ];

    retries++;
  ];

  $Failed
];


(* ~-~-~ VISUALIZATION HELPERS ~-~-~ *)

systemIntersection3[A_, b_, vars_] := Module[{rA = MatrixRank[A], rAb = MatrixRank[Join[A, Transpose[{b}], 2]], ns},
  If[rAb > rA, <|"Type" -> "NONE"|>,
    If[rA == 3, <|"Type" -> "POINT", "Point" -> LinearSolve[A, b]|>,
      ns = NullSpace[A];
      If[Length[ns] == 1, <|"Type" -> "LINE", "Point" -> (vars /. First @ FindInstance[A . vars == b, vars, Reals]), "Dir" -> ns[[1]]|>,
        If[Length[ns] >= 2, <|"Type" -> "PLANE"|>, <|"Type" -> "INFINITE"|>]
      ]
    ]
  ]
];

(* graf pre 2D a 3D *)
visualize2[A_, b_, vars_, sol_] := Module[{x, y, pt, xrange, yrange, seg, center, subtitle, range = 10, lineStyles, lineLabels, extraLegStyles, extraLegLabels, legend},
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
    subtitle = If[sol === "NONE", "Priamky sú rovnobežné, nepretínajú sa - sústava nemá riešenie.", "Priamky sú totožné (prekrývajú sa) - sústava má nekonečne veľa riešení."]
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
      If[pt =!= None, {{Directive[Black]}, {Row[{"prienik: [", TraditionalForm @ Together[pt[[1]]], ", ", TraditionalForm @ Together[pt[[2]]], "]"}]}},
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
      {{lineStyles[[1]], seg[A[[1]], b[[1]]]}, {lineStyles[[2]], seg[A[[2]], b[[2]]]}, If[pt =!= None, {{Black, Thick, Circle[pt, 0.4]}, {Green, PointSize[0.02], Point[pt]}}, {}]},
      PlotRange -> {xrange, yrange}, Axes -> True, GridLines -> Automatic, ImageSize -> Medium, PlotRangeClipping -> True
    ],
    legend
  ]
];
visualize3[A_, b_, vars_, sol_] := Module[{x, y, z, range = 15, xmin, xmax, ymin, ymax, zmin, zmax, n1, n2, n3, d1, d2, d3, inter, best, subtitle, planes, mark, plot, eqLbl, planeStyles, planeLabels, extraLegStyles, extraLegLabels, legend},

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
    "LINE", "Tri roviny majú spoločný prienik - priamku (nekonečne veľa riešení).",
    "PLANE", "Všetky tri rovnice opisujú tú istú rovinu (nekonečne veľa riešení).",
    "NONE", "Roviny nemajú spoločný prienik všetkých troch naraz (sústava nemá riešenie).",
    _, "Prienik sa nepodarilo jednoznačne určiť."
  ];

  printTextExprCell[subtitle];

  eqLbl[row_, rhs_] := tf[row . {x, y, z} == rhs];
  planeLabels = {eqLbl[A[[1]], b[[1]]], eqLbl[A[[2]], b[[2]]], eqLbl[A[[3]], b[[3]]]};
  planeStyles = {Cyan, Magenta, Yellow};

  planes = ContourPlot3D[
    {n1 . {x, y, z} == d1, n2 . {x, y, z} == d2, n3 . {x, y, z} == d3},
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

(* ~-~-~ ELIMINATION HELPERS ~-~-~ *)

equationClass[row_List, rhs_] := Which[
  AllTrue[row, PossibleZeroQ] && PossibleZeroQ[rhs], "IDENTITY",
  AllTrue[row, PossibleZeroQ] && !PossibleZeroQ[rhs], "CONTRADICTION",
  True, "NORMAL"
];

solveOneVarEquationSteps[{row_List, rhs_}, {var_}] := Module[{c = row[[1]], cls, steps = {}, value},
  cls = equationClass[row, rhs];
  If[cls === "CONTRADICTION", Return[<|"Type" -> "NONE", "Content" -> {{tf[0], tf[rhs], ""}}|>]];
  If[cls === "IDENTITY", Return[<|"Type" -> "INFINITE", "Content" -> {{tf[0], tf[0], ""}}|>]];

  Module[{iso},
    iso = isolateVarFromCoeffEqSteps[c, var, rhs];
    steps = Join[steps, iso["Steps"]];
    <|"Type" -> "ONE", "Value" -> iso["Value"], "Content" -> steps|>
  ]
];

backSubstituteElimVarSteps[{row_List, rhs_}, vars_List, solMap_Association, elimVar_] := Module[{pos, coeffU, knownSum, rhsShift, noteShift, steps = {}},
  pos = First @ First @ Position[vars, elimVar];
  coeffU = row[[pos]];

  AppendTo[steps, {renderTermsRow[Transpose[{row, vars}]], rhs, substNote[solMap, Keys[solMap], row, vars]}];
  AppendTo[steps, {formatSubstLHS[row, vars, solMap, elimVar, False], rhs, ""}];

  knownSum = Together @ Total @ Table[
    If[i === pos, 0, If[KeyExistsQ[solMap, vars[[i]]], row[[i]] * solMap[vars[[i]]], 0]],
    {i, 1, Length[vars]}
  ];
  rhsShift = Together[rhs - knownSum];
  noteShift = addNote[-knownSum];

  AppendTo[steps, {formatSubstLHS[row, vars, solMap, elimVar, True], rhs, noteShift}];

  If[PossibleZeroQ[coeffU], Return[<|"Type" -> If[PossibleZeroQ[rhsShift], "INFINITE", "NONE"], "Steps" -> steps|>]];

  Module[{iso},
    iso = isolateVarFromCoeffEqSteps[coeffU, elimVar, rhsShift];
    steps = Join[steps, iso["Steps"]];
    <|"Type" -> "ONE", "Value" -> iso["Value"], "Steps" -> steps|>
  ]

];

reduceOnceByElimination[eqs_List, vars_List] := Module[{n = Length[vars], A, b, content = {}, data2, sumRow, sumRHS, elimIdx, keepIdx, keepVar, elimVar, pivotEq, newEq, cls,
    red, A2, b2, remVars, idx},

  A = eqs[[All, 1]]; b = eqs[[All, 2]];

  If[n === 2,
    Module[{rowsShow, zeroCase, rowKeep, elimIdx0, keepIdx0, elimVar0, keepVar0, pivotEq0, newEq0, cls0},

      (* zobrazenie pôvodných rovníc *)
      rowsShow = {
        {renderTermsRow[Transpose[{A[[1]], vars}]], b[[1]], ""},
        {renderTermsRow[Transpose[{A[[2]], vars}]], b[[2]], ""}
      };

      (* ak už máme nulový koeficient v 2x2, nerobíme LCM (zabránime 1/0) *)
      zeroCase = Which[
        A[[1, 1]] === 0, {1, 1},  (* 1. rovnica nemá x *)
        A[[2, 1]] === 0, {2, 1},  (* 2. rovnica nemá x *)
        A[[1, 2]] === 0, {1, 2},  (* 1. rovnica nemá y *)
        A[[2, 2]] === 0, {2, 2},  (* 2. rovnica nemá y *)
        True, None
      ];

      If[zeroCase =!= None,
        {rowKeep, elimIdx0} = zeroCase;
        keepIdx0 = 3 - elimIdx0;

        elimVar0 = vars[[elimIdx0]];
        keepVar0 = vars[[keepIdx0]];

        (* pivotEq musí byť tá druhá rovnica (aby sme z nej dopočítali eliminovanú premennú) *)
        pivotEq0 = {A[[3 - rowKeep]], b[[3 - rowKeep]]};

        appendStepHeader[content, "Priama redukcia"];
        AppendTo[content,
          "V jednej rovnici je koeficient pri premennej " <> ToString[elimVar0] <>
              " nulový, preto už táto rovnica obsahuje iba " <> ToString[keepVar0] <> "."
        ];
        AppendTo[content, alignedEquations[rowsShow]];

        newEq0 = {{{A[[rowKeep, keepIdx0]]}, b[[rowKeep]]}};
        cls0 = equationClass[{A[[rowKeep, keepIdx0]]}, b[[rowKeep]]];

        Return[<|
          "Content" -> content,
          "NewEqs" -> newEq0,
          "NewVars" -> {keepVar0},
          "ElimVar" -> elimVar0,
          "PivotEq" -> pivotEq0,
          "Classes" -> {cls0}
        |>];
      ];

      (* štandardný prípad - bez núl, môžeme robiť LCM a sčítanie *)
      data2 = eliminationStart2[A, b, vars];
      content = Join[content, data2["content"]];

      sumRow = Total[data2["A_mod"]];
      sumRHS = Total[data2["b_mod"]];

      elimIdx = If[data2["EliminatedVariable"] === "X", 1, 2];
      keepIdx = 3 - elimIdx;
      elimVar = vars[[elimIdx]];
      keepVar = vars[[keepIdx]];

      appendStepHeader[content, "Sčítanie rovníc"];
      AppendTo[content, "Sčítame rovnice, aby sme vyrušili premennú " <> ToString[elimVar] <> "."];
      AppendTo[content, renderAddition2[data2["A_mod"], data2["b_mod"], vars]];

      If[PossibleZeroQ[sumRow[[keepIdx]]],
        AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{sumRow, vars}]], sumRHS, ""}}]];
      ];

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

    ]
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

eliminationStart2[A_, b_, vars_] := Module[{idx, k1, k2, lcm, m1, m2, choice, targetVar, needsMult, content = {}, rows1, rows2},
  idx = pickElimVar2[A];
  targetVar = vars[[idx]];
  choice = If[idx == 1, "X", "Y"];
  k1 = A[[1, idx]]; k2 = A[[2, idx]];
  lcm = LCM[Abs[k1], Abs[k2]];
  m1 = lcm / Abs[k1];
  m2 = lcm / Abs[k2];
  If[Sign[k1] === Sign[k2], m2 = -m2];
  needsMult = !(Sign[k1] =!= Sign[k2] && m1 === 1 && m2 === 1);

  appendStepHeader[content, "Príprava na elimináciu"];
  If[needsMult,
    AppendTo[content, "Chceme vyrušiť premennú " <> ToString[targetVar] <> ". Rovnice preto prenásobíme tak, aby mali pri nej rovnaký koeficient s opačným znamienkom."],
    AppendTo[content, "Koeficienty pri premennej " <> ToString[targetVar] <> " sú už opačné, takže môžeme hneď sčítať rovnice a premennú vyrušiť."]
  ];

  rows1 = {
    {renderTermsRow[Transpose[{A[[1]], vars}], "Numeric", targetVar], b[[1]], multNote[m1]},
    {renderTermsRow[Transpose[{A[[2]], vars}], "Numeric", targetVar], b[[2]], multNote[m2]}
  };

  rows2 = {
    {renderTermsRow[Transpose[{m1 A[[1]], vars}]], m1 b[[1]], ""},
    {renderTermsRow[Transpose[{m2 A[[2]], vars}]], m2 b[[2]], ""}
  };

  If[needsMult,
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]],
    AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{A[[1]], vars}], "Numeric", targetVar], b[[1]], ""}, {renderTermsRow[Transpose[{A[[2]], vars}], "Numeric", targetVar], b[[2]], ""}}]]
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

reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_] := Module[{content = {}, valA = rowA[[elimCol]], valB = rowB[[elimCol]], lcm, m1, m2, rowA2, rhsA2, rowB2, rhsB2, newRow, newRHS, rows1, rows2},
  If[valA == 0 || valB == 0,
    AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{rowA, vars}], "Numeric", vars[[elimCol]]], rhsA, ""}, {renderTermsRow[Transpose[{rowB, vars}], "Numeric", vars[[elimCol]]], rhsB, ""}}]];
    If[valB == 0, {newRow, newRHS} = {rowB, rhsB}, {newRow, newRHS} = {rowA, rhsA}],
    lcm = LCM[Abs[valA], Abs[valB]];
    m1 = lcm/Abs[valA];
    m2 = lcm/Abs[valB];
    If[Sign[valA] == Sign[valB], m2 = -m2];

    rows1 = {{renderTermsRow[Transpose[{rowA, vars}], "Numeric", vars[[elimCol]]], rhsA, multNote[m1]}, {renderTermsRow[Transpose[{rowB, vars}], "Numeric", vars[[elimCol]]], rhsB, multNote[m2]}};

    rowA2 = m1 rowA; rhsA2 = m1 rhsA;
    rowB2 = m2 rowB; rhsB2 = m2 rhsB;

    rows2 = {{renderTermsRow[Transpose[{rowA2, vars}]], rhsA2, ""}, {renderTermsRow[Transpose[{rowB2, vars}]], rhsB2, ""}};
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]];

    newRow = rowA2 + rowB2;
    newRHS = rhsA2 + rhsB2;
  ];

  AppendTo[content, alignedEquations[{{Style[renderTermsRow[Transpose[{newRow, vars}]], Darker[Green, 0.2]], Style[newRHS, Darker[Green, 0.2]], ""}}]];
  <|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>
];

renderAddition2[rowMod_, rhsMod_, vars_] := alignedEquations[{{
  Row[{tf[rowMod[[1, 1]] vars[[1]]], signBtwTerms[rowMod[[2, 1]]], tf[Abs[rowMod[[2, 1]]] vars[[1]]],
    signBtwTerms[rowMod[[1, 2]]], tf[Abs[rowMod[[1, 2]]] vars[[2]]], signBtwTerms[rowMod[[2, 2]]], tf[Abs[rowMod[[2, 2]]] vars[[2]]]}],
  Row[{rhsMod[[1]], signBtwTerms[rhsMod[[2]]], Abs[rhsMod[[2]]]}],
  ""
}}];

reduce3to2[A_, b_, vars_] := Module[{content = {}, zp, substPick, elimCol, elimVar, zeroRows, nonZeroRows, iKeep, rowIV, rhsIV, rowV, rhsV, remCols, remVars, A2, b2, twoCombosQ, pair, i1, i2},
  appendStepHeader[content, "Redukcia sústavy 3x3 na 2x2"];
  zp = zeroCoeff3[A];
  elimCol = pickElimVar3[A];
  elimVar = vars[[elimCol]];
  AppendTo[content, "Vyrušíme premennú " <> ToString[elimVar] <> ", aby sme získali sústavu 2x2."];

  zeroRows = zp["ZeroRowsByCol"][[elimCol]];
  nonZeroRows = Complement[Range[3], zeroRows];

  If[Length[zeroRows] >= 1,
    twoCombosQ = False;
    If[Length[nonZeroRows] >= 2,
      iKeep = First[zeroRows];
      pair = pickBestElimPair[nonZeroRows, elimCol, A];
      {i1, i2} = pair;

      AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];
      With[{res = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars]},
        content = Join[content, res["Content"]]; rowIV = res["Row"]; rhsIV = res["RHS"];
      ];

      rowV = A[[iKeep]]; rhsV = b[[iKeep]];
      AppendTo[content, Style["b) Rovnica bez vyrušovanej premennej (použijeme ju priamo):", Italic]];
      AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{rowV, vars}]], rhsV, ""}}]],
      {i1, i2} = zeroRows[[1 ;; 2]];
      rowIV = A[[i1]]; rhsIV = b[[i1]];
      rowV = A[[i2]]; rhsV = b[[i2]];

      AppendTo[content, Style["a) Rovnice bez vyrušovanej premennej (použijeme ich priamo):", Italic]];
      AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{rowIV, vars}]], rhsIV, ""}, {renderTermsRow[Transpose[{rowV, vars}]], rhsV, ""}}]];
    ],
    twoCombosQ = True;

    AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];
    With[{res = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars]},
      content = Join[content, res["Content"]]; rowIV = res["Row"]; rhsIV = res["RHS"];
    ];

    AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];
    With[{res = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars]},
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

(* ~-~-~ SUBSTITUTION HELPERS ~-~-~ *)

makeLinearSubstitutionTraceSteps[solvedVar_, expr_, solMap_Association, varsPool_List] := Module[
  {steps = {}, usedVars, coeffs, c0, note, rhsPretty, rhsExpanded, rhsValue, exprAfter,
    hasAnySubstQ, needExpandQ, rhsBoxes, dedup},

  usedVars = Select[varsPool, !FreeQ[expr, #] &];
  {coeffs, c0} = If[usedVars === {}, {{}, Together[expr]}, linearDecompose[expr, usedVars]];

  hasAnySubstQ = AnyTrue[usedVars, KeyExistsQ[solMap, #] &];
  note = If[hasAnySubstQ, substNote[solMap, Keys[solMap], coeffs, usedVars], ""];

  (* porovnanie riadkov cez boxy, aby sme odfiltrovali vizuálne duplicity *)
  rhsBoxes[e_] := Quiet @ Check[ToBoxes[e, TraditionalForm], HoldForm[e]];
  dedup[list_] := Module[{out = {}, last = None, cur},
    Do[
      cur = rhsBoxes[list[[i]]];
      If[last =!= None && cur === last, Continue[]];
      AppendTo[out, list[[i]]];
      last = cur,
      {i, 1, Length[list]}
    ];
    out
  ];

  (* po dosadení - koeficient . (hodnota) / koeficient . premenná *)
  rhsPretty = Module[{out = {}, first = True, addTerm, c, v, term, s},
    addTerm[content_, sign_] := (AppendTo[out, If[first, If[sign === -1, Row[{"-", content}], content], Row[{If[sign === -1, " - ", " + "], content}]]]; first = False);

    Do[
      c = coeffs[[i]]; v = usedVars[[i]];
      If[PossibleZeroQ[c], Continue[]];

      If[KeyExistsQ[solMap, v],
        term = coeffVal[Abs[c], solMap[v]]; s = Sign[c],
        term = tf[If[Abs[c] === 1, v, Abs[c] v]]; s = Sign[c]
      ];
      addTerm[term, s],
      {i, 1, Length[usedVars]}
    ];

    If[!PossibleZeroQ[c0], addTerm[tf[Abs[c0]], Sign[c0]]];
    If[out === {}, tf[0], Row[out]]
  ];

  (* roznásobené -  ak  |c| != 1 pri dosádzanej premennej *)
  needExpandQ = AnyTrue[Transpose[{coeffs, usedVars}],
    (#[[1]] =!= 1 && #[[1]] =!= -1 && KeyExistsQ[solMap, #[[2]]]) &
  ];

  rhsExpanded = Module[{out = {}, first = True, addTerm, c, v, p},
    addTerm[val_, sign_] := (AppendTo[out, If[first, If[sign === -1, Row[{"-", tf[Abs[val]]}], tf[val]], Row[{If[sign === -1, " - ", " + "], tf[Abs[val]]}]]]; first = False);

    Do[
      c = coeffs[[i]]; v = usedVars[[i]];
      If[PossibleZeroQ[c], Continue[]];
      If[KeyExistsQ[solMap, v],
        p = Together[c*solMap[v]];
        If[!PossibleZeroQ[p], addTerm[p, Sign[p]]],
        (* ak by zostala neznáma, nemá zmysel tvoriť \[OpenCurlyDoubleQuote]expanded\[CloseCurlyDoubleQuote] riadok *)
        Null
      ],
      {i, 1, Length[usedVars]}
    ];

    If[!PossibleZeroQ[c0], addTerm[c0, Sign[c0]]];
    If[out === {}, tf[0], Row[out]]
  ];

  (* výsledok *)
  exprAfter = expr /. solMap;
  rhsValue = Together[exprAfter];

  (* zostavíme RHS kandidátov a vyhodíme duplicity *)
  With[{rhsList0 = Join[
    {tf[expr]},
    If[hasAnySubstQ, {rhsPretty}, {}],
    If[hasAnySubstQ && needExpandQ, {rhsExpanded}, {}],
    {tf[rhsValue]}
  ]},
    With[{rhsList = dedup[rhsList0]},
      steps = Map[{tf[solvedVar], #, ""} &, rhsList];
      (* prvý riadok má mať note *)
      If[steps =!= {} && note =!= "", steps[[1, 3]] = note];
    ]
  ];

  <|"Steps" -> steps, "Value" -> rhsValue|>
];

backSubstituteSubstVarSteps[solvedVar_, expr_, solMap_Association] := Module[{res},
  res = makeLinearSubstitutionTraceSteps[solvedVar, expr, solMap, {x, y, z}];
  <|"Value" -> res["Value"], "Steps" -> res["Steps"]|>
];

reduceOnceBySubstitution[eqs_List, vars_List] := Module[{n = Length[vars], A, b, content = {}, rI, cI, solveData, substRule, elimVar, remVars, res, newEq, cls, lastSolve, red, A2, b2},
  A = eqs[[All, 1]]; b = eqs[[All, 2]];

  If[n === 2,
    (
      {rI, cI} = pickSubstSolve2[A, b, vars];
      elimVar = vars[[cI]];
      remVars = Delete[vars, cI];

      appendStepHeader[content, "Vyjadrenie neznámej"];
      AppendTo[content, "Z " <> ToString[rI] <> ". rovnice vyjadríme neznámu " <> ToString[elimVar] <> "."];

      solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
      AppendTo[content, alignedEquations[solveData["Content"]]];
      substRule = solveData["Rule"];

      appendStepHeader[content, "Dosadenie"];
      AppendTo[content, "Výraz dosadíme do druhej rovnice a upravíme ju."];

      res = substituteIntoEquationSteps[A[[3 - rI]], b[[3 - rI]], vars, substRule, remVars];
      AppendTo[content, alignedEquations[res["Content"]]];

      newEq = {res["NewEq"][[1]], res["NewEq"][[2]]};
      cls = equationClass[newEq[[1]], newEq[[2]]];

      lastSolve = solveOneVarEquationSteps[newEq, remVars];
      If[lastSolve["Type"] === "ONE", AppendTo[content, highlightGrid[alignedEquations[{{remVars[[1]], tf[lastSolve["Value"]], ""}}]]]];

      Return[<|"Content" -> content, "NewEqs" -> {newEq}, "NewVars" -> remVars, "SolvedVar" -> elimVar, "RuleExpr" -> substRule[[2]], "Classes" -> {cls}|>];
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

formatSubstOnceLHS[row_, vars_, targetVar_, substExpr_] := Module[{terms = {}, first = True, addTerm},
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

solveForVarSteps[row_, rhs_, vars_, varIndex_] := Module[{targetVar, c, otherTerms, rhsExpr, stepsIso = {}, moveNote, currentLHS, iso, solExpr},
  targetVar = vars[[varIndex]];
  c = row[[varIndex]];
  otherTerms = Delete[row, varIndex] . Delete[vars, varIndex];

  currentLHS = renderTermsRow[Transpose[{row, vars}]];
  moveNote = If[PossibleZeroQ[otherTerms], "", addNote[-otherTerms]];

  AppendTo[stepsIso, {currentLHS, tf[rhs], moveNote}];
  rhsExpr = Together[rhs - otherTerms];

  iso = isolateVarFromCoeffEqSteps[c, targetVar, rhsExpr];
  stepsIso = Join[stepsIso, iso["Steps"]];
  solExpr = iso["Value"];

  If[!(PossibleZeroQ[otherTerms] && c === 1), AppendTo[stepsIso, {tf[targetVar], formatLinearExpr[solExpr, DeleteCases[vars, targetVar]], ""}]];

  <|"Content" -> stepsIso, "Rule" -> (targetVar -> solExpr), "Expr" -> solExpr, "Var" -> targetVar|>
];

substituteIntoEquationSteps[row_, rhs_, vars_, rule_, remainingVars_] := Module[{targetVar, substExpr, stepRows, currentLHS, sNote, pos, targetCoeff, baseTerms, subCoeffs, subConst, distTerms, lhsCombined, newRow, constLeft, newRHS, c},
  targetVar = rule[[1]]; substExpr = rule[[2]]; stepRows = {};

  currentLHS = renderTermsRow[Transpose[{row, vars}]];
  sNote = substNote[<|targetVar -> substExpr|>, {targetVar}, row, vars];
  AppendTo[stepRows, {currentLHS, tf[rhs], sNote}];

  pos = First @ First @ Position[vars, targetVar];
  targetCoeff = row[[pos]];

  If[PossibleZeroQ[targetCoeff], (* ak premenná nebola v rovnici, nič nemeníme *)
    newRow = Coefficient[row . vars, #] & /@ remainingVars;
    constLeft = (row . vars) /. (Rule[#, 0] & /@ remainingVars);
    newRHS = rhs - constLeft;

    If[Length[remainingVars] === 1,
      Module[{c = newRow[[1]], v = remainingVars[[1]], iso},
        iso = isolateVarFromCoeffEqSteps[c, v, newRHS];
        Which[
          c === 1, Return[<|"Content" -> {{renderTermsRow[Transpose[{row, vars}]], tf[rhs], ""}}, "NewEq" -> {newRow, newRHS}|>],
          c =!= 0 && iso["Type"] === "GENERAL", Return[<|"Content" -> iso["Steps"], "NewEq" -> {{1}, iso["Value"]}|>],
          True, Return[<|"Content" -> {{renderTermsRow[Transpose[{row, vars}]], tf[rhs], ""}}, "NewEq" -> {newRow, newRHS}|>]
        ]
      ],
      Return[<|"Content" -> {{renderTermsRow[Transpose[{row, vars}]], tf[rhs], ""}}, "NewEq" -> {newRow, newRHS}|>]
    ];
  ];

  baseTerms = Select[Delete[MapThread[List, {row, vars}], pos], #[[1]] =!= 0 &];
  {subCoeffs, subConst} = linearDecompose[substExpr, remainingVars];

  distTerms = Join[
    DeleteCases[Table[If[PossibleZeroQ[subCoeffs[[k]]], Nothing, {targetCoeff*subCoeffs[[k]], remainingVars[[k]]}], {k, 1, Length[remainingVars]}], Nothing],
    If[PossibleZeroQ[subConst], {}, {{targetCoeff*subConst, None}}],
    baseTerms
  ];

  If[Abs[targetCoeff] =!= 1, AppendTo[stepRows, {formatSubstOnceLHS[row, vars, targetVar, substExpr], tf[rhs], ""}]];
  AppendTo[stepRows, {renderTermsRow[orderTermsByVars[distTerms, vars], "Symbolic"], tf[rhs], ""}];

  lhsCombined = Expand[row . vars /. rule];
  newRow = Coefficient[lhsCombined, #] & /@ remainingVars;
  constLeft = lhsCombined /. (Rule[#, 0] & /@ remainingVars);
  newRHS = rhs - constLeft;
  AppendTo[stepRows, {renderTermsRow[Transpose[{newRow, remainingVars}]], tf[newRHS], ""}];

  If[Length[remainingVars] === 1,
    With[{c = newRow[[1]], v = remainingVars[[1]], iso = isolateVarFromCoeffEqSteps[newRow[[1]], remainingVars[[1]], newRHS]},
      Which[
        c === 1,
        <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>,

        c =!= 0 && iso["Type"] === "GENERAL",
        <|
          "Content" -> Join[Most[stepRows], iso["Steps"]],
          "NewEq" -> {{1}, iso["Value"]}
        |>,

        True,
        <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>
      ]
    ],
    <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>
  ]


];

reduce3to2BySubstitution[A_, b_, vars_] := Module[{content = {}, rI, cI, solveData, elimVar, substRule, otherRowsIdx, A2, b2, remVars, remCols, idx},
  {rI, cI} = pickSubstSolve3[A, b, vars];
  elimVar = vars[[cI]];

  appendStepHeader[content, "Vyjadrenie neznámej z jednej rovnice"];
  AppendTo[content, "Vyberieme si " <> ToString[rI] <> ". rovnicu a vyjadríme z nej neznámu " <> ToString[elimVar] <> "."];

  solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
  AppendTo[content, alignedEquations[solveData["Content"]]];

  substRule = solveData["Rule"];

  appendStepHeader[content, "Dosadenie do zvyšných rovníc"];
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

(* ~-~-~ STEP-BY-STEP SOLVERS ~-~-~ *)

stepsElimination[A_, b_, vars_, data_ : <||>] := Module[{content = {}, kind, eqs, varsNow, stack = {}, step, lastSolve, solMap, back, solVec, origVars = vars, k},
  kind = Lookup[data, "type", "ONE"];

  (* hard normalizácia je len pre HARD mód *)
  If[TrueQ[data["HardQ"]] && Length[vars] === 3, content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  eqs = Table[{A[[i]], b[[i]]}, {i, 1, Length[vars]}];
  varsNow = vars;

  (* eliminácia neznámych, kým zostane jedna *)
  While[Length[varsNow] > 1,
    step = reduceOnceByElimination[eqs, varsNow];
    If[step === $Failed, Return[$Failed]];
    content = Join[content, step["Content"]];

    If[AnyTrue[step["Classes"], # === "CONTRADICTION" &],
      appendStepHeader[content, "Záver"];
      AppendTo[content, "Pri eliminácii nám vyšla nepravdivá rovnosť (spor). To znamená, že sústava nemá riešenie."];
      Return[<|"Content" -> content, "Solution" -> "NONE"|>]
    ];

    If[AllTrue[step["Classes"], # === "IDENTITY" &],
      appendStepHeader[content, "Záver"];
      AppendTo[content, "Pri eliminácii nám vyšla identita (pravdivá rovnosť). Jedna neznáma zostáva voľná, preto má sústava nekonečne veľa riešení."];
      Return[<|"Content" -> content, "Solution" -> "INFINITE"|>]
    ];

    AppendTo[stack, <|"PivotEq" -> step["PivotEq"], "VarsBefore" -> varsNow, "ElimVar" -> step["ElimVar"]|>];
    eqs = step["NewEqs"];
    varsNow = step["NewVars"];
  ];

  lastSolve = solveOneVarEquationSteps[First[eqs], varsNow];

  (* riešenie poslednej rovnice *)
  If[lastSolve["Type"] =!= "ONE",
    appendStepHeader[content, "Záver"];
    AppendTo[content, If[lastSolve["Type"] === "NONE",
      "Pri riešení poslednej rovnice dostaneme spor, takže sústava nemá riešenie.",
      "Pri riešení poslednej rovnice dostaneme identitu, takže sústava má nekonečne veľa riešení."
    ]];
    Return[<|"Content" -> content, "Solution" -> If[lastSolve["Type"] === "NONE", "NONE", "INFINITE"]|>]
  ];

  AppendTo[content, alignedEquations[lastSolve["Content"]]];
  AppendTo[content, highlightGrid[alignedEquations[{{varsNow[[1]], tf[lastSolve["Value"]], ""}}]]];

  solMap = <|varsNow[[1]] -> lastSolve["Value"]|>;

  Do[
    appendStepHeader[content, "Spätné dosadenie do pivotnej rovnice (určenie ďalšej neznámej)"];
    back = backSubstituteElimVarSteps[stack[[k, "PivotEq"]], stack[[k, "VarsBefore"]], solMap, stack[[k, "ElimVar"]]];
    If[back["Type"] =!= "ONE", Return[<|"Content" -> content, "Solution" -> If[back["Type"] === "NONE", "NONE", "INFINITE"]|>]];
    AppendTo[content, alignedEquations[back["Steps"]]];
    AppendTo[content, highlightGrid[alignedEquations[{{stack[[k, "ElimVar"]], tf[back["Value"]], ""}}]]];
    solMap[stack[[k, "ElimVar"]]] = back["Value"],
    {k, Length[stack], 1, -1}
  ];

  solVec = (solMap /@ origVars);

  If[kind === "ONE",
    appendStepHeader[content, "Skúška správnosti"];
    AppendTo[content, "Správnosť overíme dosadením nájdeného riešenia do pôvodnej sústavy rovníc a porovnáme ľavú a pravú stranu v každom riadku:"];
    content = Join[content, verificationStepsEquation[A, b, origVars, solVec]];
    Return[<|"Content" -> content, "Solution" -> solVec|>]
  ];


  <|"Content" -> content, "Solution" -> If[kind === "NONE", "NONE", "INFINITE"]|>
];
stepsSubstitution[A_, b_, vars_, data_ : <||>] := Module[{content = {}, kind, eqs, varsNow, stack = {}, step, lastSolve, solMap, back, solVec, origVars = vars, k},
  kind = Lookup[data, "type", "ONE"];

  If[TrueQ[data["HardQ"]] && Length[vars] === 3, content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  eqs = Table[{A[[i]], b[[i]]}, {i, 1, Length[vars]}];
  varsNow = vars;

  While[Length[varsNow] > 1,
    step = reduceOnceBySubstitution[eqs, varsNow];
    If[step === $Failed, Return[$Failed]];
    content = Join[content, step["Content"]];

    If[AnyTrue[step["Classes"], # === "CONTRADICTION" &],
      appendStepHeader[content, "Záver"];
      AppendTo[content, "Pri úpravách nám vyšla nepravdivá rovnosť (spor). To znamená, že sústava nemá riešenie."];
      Return[<|"Content" -> content, "Solution" -> "NONE"|>]
    ];

    If[AllTrue[step["Classes"], # === "IDENTITY" &],
      appendStepHeader[content, "Záver"];
      AppendTo[content, "Pri úpravách nám vyšla identita (pravdivá rovnosť). Jedna neznáma zostáva voľná, preto má sústava nekonečne veľa riešení."];
      Return[<|"Content" -> content, "Solution" -> "INFINITE"|>]
    ];

    AppendTo[stack, <|"SolvedVar" -> step["SolvedVar"], "Expr" -> step["RuleExpr"]|>];
    eqs = step["NewEqs"];
    varsNow = step["NewVars"];
  ];

  lastSolve = solveOneVarEquationSteps[First[eqs], varsNow];

  If[lastSolve["Type"] =!= "ONE",
    appendStepHeader[content, "Záver"];
    AppendTo[content, If[lastSolve["Type"] === "NONE",
      "Pri riešení poslednej rovnice dostaneme spor, takže sústava nemá riešenie.",
      "Pri riešení poslednej rovnice dostaneme identitu, takže sústava má nekonečne veľa riešení."
    ]];
    Return[<|"Content" -> content, "Solution" -> If[lastSolve["Type"] === "NONE", "NONE", "INFINITE"]|>]
  ];

  solMap = <|varsNow[[1]] -> lastSolve["Value"]|>;

  Do[
    appendStepHeader[content, "Spätné dosadenie (dopočítanie neznámej)"];
    back = backSubstituteSubstVarSteps[stack[[k, "SolvedVar"]], stack[[k, "Expr"]], solMap];
    AppendTo[content, alignedEquations[back["Steps"]]];
    AppendTo[content, highlightGrid[alignedEquations[{{stack[[k, "SolvedVar"]], tf[back["Value"]], ""}}]]];
    solMap[stack[[k, "SolvedVar"]]] = back["Value"],
    {k, Length[stack], 1, -1}
  ];

  solVec = (solMap /@ origVars);

  If[kind === "ONE",
    appendStepHeader[content, "Skúška správnosti"];
    AppendTo[content, "Správnosť overíme dosadením nájdeného riešenia do pôvodnej sústavy rovníc a porovnáme ľavú a pravú stranu v každom riadku:"];
    content = Join[content, verificationStepsEquation[A, b, origVars, solVec]];
    Return[<|"Content" -> content, "Solution" -> solVec|>]
  ];

  <|"Content" -> content, "Solution" -> If[kind === "NONE", "NONE", "INFINITE"]|>
];

(* ~-~--~ EQUATION RUN BUILDER ~-~-~ *)

normalizeEquationSpec[spec_Association] := Module[{s = spec},
  s["DimByDiff"] = Lookup[s, "DimByDiff", DimensionByDifficulty];
  s["VarsByDim"] = Lookup[s, "VarsByDim", buildVars];
  s["TaskPrinter"] = Lookup[s, "TaskPrinter", Missing["NotSet"]];
  s["ResultPrinter"] = Lookup[s, "ResultPrinter", Missing["NotSet"]];
  s["VisualizationFn"] = Lookup[s, "VisualizationFn", None];
  s
];

buildEquationRun[spec0_Association, diff_String, opts___?OptionQ] := Module[{spec = normalizeEquationSpec[spec0], entryFn, msgPrefix, stRaw, st, dim, vars, data, A, b},
  entryFn = spec["EntryFn"];
  msgPrefix = spec["MsgPrefix"];

  (* ziskame a overíme typ riešenia *)
  stRaw = OptionValue[entryFn, {opts}, SolutionType];
  If[!TrueQ[ValidateSolutionType[stRaw]], Message[MessageName[msgPrefix, "badst"], stRaw]; Return[$Failed]];
  st = ResolveSolutionType[stRaw];

  dim = spec["DimByDiff"][diff];
  vars = spec["VarsByDim"][dim];

  data = generateDataWithBounds[dim, diff, st, vars, spec["ScrambleFn"]];
  If[data === $Failed,
    Message[MessageName[msgPrefix, "fail"]];
    Return[$Failed]
  ];

  A = data["A"]; b = data["b"];

  <|"Spec" -> spec, "Diff" -> diff, "Dim" -> dim, "Vars" -> vars, "SolutionType" -> st, "Data" -> data, "A" -> A, "b" -> b|>
];

buildEquationSteps[run_Association] := Module[{ spec = run["Spec"], steps},
  steps = spec["StepsFn"][run["A"], run["b"], run["Vars"], run["Data"]];

  If[steps === $Failed,
    Message[MessageName[spec["MsgPrefix"], "fail"]];
    Return[$Failed]
  ];

  steps
];

printTaskBlock[run_Association] := Module[{data = run["Data"], A = run["A"], b = run["b"], vars = run["Vars"], dim = run["Dim"], taskText},
  taskText = Lookup[run["Spec"], "TaskText", "Riešte sústavu rovníc."];

  printTextBlock[taskText];

  If[run["Diff"] === "HARD" && KeyExistsQ[data, "EqDisplay"],
    printFormulaCell @ alignedEquations[data["EqDisplay"]],
    printFormulaCell @ alignedEquations @ Table[
      {renderTermsRow[Transpose[{A[[i]], vars}]], b[[i]], ""},
      {i, 1, dim}
    ]
  ];
];

solutionRow[vars_List, solution_List] := Row[
  Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ solution, ", "], ")"}]
];

printResultBlock[text_, expr_] := Module[{ },
  printTextBlock[text];
  printFormulaCell[expr]
];

printDefaultResult[run_Association, steps_Association] := Module[{sol = steps["Solution"], vars = run["Vars"], A = run["A"], b = run["b"], best, solExprs},
  If[ListQ[sol],
    printResultBlock["Riešenie sústavy:", solutionRow[vars, sol]]
  ];

  If[sol === "NONE",
    printTextCell["Sústava nemá riešenie."]
  ];

  If[sol === "INFINITE",
    printTextCell["Sústava má nekonečne veľa riešení."];

    best = chooseParametrization[A, b, vars];

    If[best === $Failed,
      printTextCell["Parametrický zápis riešenia sa nepodarilo zostaviť."],
      solExprs = best["Exprs"];

      printFormulaCell[
        Row[{
          "K = { [",
          Row @ Riffle[TraditionalForm /@ solExprs, ", "],
          "], ",
          \[FormalT],
          " \[Element] ",
          Reals,
          " }"
        }]
      ]
    ];
  ];
];

visualizationDefault[run_Association, steps_Association] := Module[{dim = run["Dim"], A = run["A"], b = run["b"], vars = run["Vars"], sol = steps["Solution"]}, If[dim == 2, visualize2[A, b, vars, sol], visualize3[A, b, vars, sol]]];

renderEquationRun[run_Association, steps_, mode_String, opts___?OptionQ] := Module[{spec = run["Spec"], entryFn, visQ},
  entryFn = spec["EntryFn"];

  printSectionCell[spec["SectionTitle"]];
  printSubsectionCell["Zadanie"];
  spec["TaskPrinter"][run];

  If[mode === "TASK_STEPS_RESULT",
    printSubsectionCell["Postup"];
    Scan[renderStepItem, steps["Content"]];
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    spec["ResultPrinter"][run, steps];

    visQ = TrueQ @ OptionValue[entryFn, {opts}, Visualization];
    If[visQ,
      If[
        Head[spec["VisualizationFn"]] === Function,
        spec["VisualizationFn"][run, steps],
        visualizationDefault[run, steps]
      ]
    ];
  ];
];

(* ~-~-~ MAIN CONTROLLER ~-~-~ *)

runEquationGenerator[spec0_Association, diff_String, mode_String, opts___?OptionQ] := Module[{spec = normalizeEquationSpec[spec0], entryFn, msgPrefix, run, steps = Missing["NotComputed"], sol},
  entryFn = spec["EntryFn"];
  msgPrefix = spec["MsgPrefix"];

  If[!TrueQ[ValidateDifficulty[diff]],
    Message[MessageName[msgPrefix, "baddiff"], diff];
    Return[$Failed]
  ];

  If[!TrueQ[ValidateMode[mode]],
    Message[MessageName[msgPrefix, "badmode"], mode];
    Return[$Failed]
  ];

  If[spec["TaskPrinter"] === Missing["NotSet"], spec["TaskPrinter"] = printTaskBlock];
  If[spec["ResultPrinter"] === Missing["NotSet"], spec["ResultPrinter"] = printDefaultResult];
  If[spec["VisualizationFn"] === None, spec["VisualizationFn"] = Function[{r, s}, visualizationDefault[r, s]]];

  run = buildEquationRun[spec, diff, opts];
  If[run === $Failed, Return[$Failed]];

  If[mode =!= "TASK",
    steps = withStepCounter @ Function[Null, buildEquationSteps[run]];
    If[steps === $Failed, Return[$Failed]];
  ];

  renderEquationRun[run, steps, mode, opts];

  sol = If[AssociationQ[steps], steps["Solution"], Missing["NotComputed"]];
  sol
];

GenElimination[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenElimination,
    "MsgPrefix" -> GenElimination,
    "SectionTitle" -> "Eliminačná metóda",
    "ScrambleFn" -> genScrambleElimination,
    "StepsFn" -> stepsElimination,
    "TaskText" -> "Riešte sústavu rovníc eliminačnou metódou (sčítaním rovníc)."
  |>;

  runEquationGenerator[spec, diff, mode, opts]
];

GenSubstitution[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenSubstitution,
    "MsgPrefix" -> GenSubstitution,
    "SectionTitle" -> "Dosadzovacia (substitučná) metóda",
    "ScrambleFn" -> genScrambleSubstitution,
    "StepsFn" -> stepsSubstitution,
    "TaskText" -> "Riešte sústavu rovníc dosadzovacou (substitučnou) metódou."
  |>;

  runEquationGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];