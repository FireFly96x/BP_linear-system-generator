(* ::Package:: *)

(*
  Package: EliminationMatrixGenerator
  Description: Generates didactic materials for solving linear systems via the elimination method.
  Includes logic for problem creation, solution steps generation, and geometric visualization.
*)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];
$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

(* Public interface *)

Gen01::usage =
    "Gen01[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc sčítacou (eliminačnou) metódou.

diff:
  \"EASY\"    (2×2)
  \"MEDIUM\" (3×3)
  \"HARD\"    (3×3 - momentálne v kóde ešte nie je implementované)

mode:
  \"TASK\"              - vypíše iba zadanie
  \"TASK_RESULT\"       - zadanie + výsledok
  \"TASK_STEPS_RESULT\" - zadanie + postup + výsledok

opts:
  Visualization -> True|False   (2×2: graf priamok, 3×3: graf rovín)
  SolutionType  -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"
    - ak sa nezadá (Automatic): 80% šanca na práve jedno riešenie
    - \"ONE\"/\"NONE\"/\"INFINITE\" slúži len na riadenie generovania.";

(* Error messages *)
Gen01::baddiff  = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtiažnosť `1` zatiaľ nie je implementovaná v tomto generátore.";
Gen01::fail     = "Nepodarilo sa vygenerovať vhodný príklad.";

Options[Gen01] = {
  SolutionType -> Automatic,
  Visualization -> False
};

Begin["`Private`"];

(* --- Output Formatting Helpers --- *)

CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];

(* Helper for plain Unicode text strings *)
CellTextU[str_String] := CellPrint @ Cell[
  str,
  "Text",
  ShowStringCharacters -> False
];

(* Helper for Expression-based text (e.g. Row[{...}]) *)
CellTextExpr[expr_] := CellPrint @ Cell[
  BoxData @ ToBoxes[expr, StandardForm],
  "Text",
  ShowStringCharacters -> False
];

CellBox[expr_] := CellPrint @ Cell[
  BoxData @ ToBoxes[expr, TraditionalForm],
  "DisplayFormula",
  ShowStringCharacters -> False
];

(* Renders mixed content (Strings, Graphics, Expressions) into Notebook cells *)
renderItem[item_] := Which[
  StringQ[item], CellTextU[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Graphics || Head[item] === Graphics3D,
  CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, CellBox[item]
];

(* --- Mathematical Helpers --- *)

(*
  Selects the best pair of rows [i, j] to eliminate a specific column variable.
  Heuristic: Prefers pairs with coefficients that are equal magnitude (direct cancellation)
  or have a small Least Common Multiple (LCM).
*)
pickBestElimPair[rowIdx_List, elimCol_Integer, A_] := Module[
  {pairs, scorePair},

  pairs = Subsets[rowIdx, {2}];

  scorePair[{i_, j_}] := Module[{c1, c2},
    c1 = A[[i, elimCol]];
    c2 = A[[j, elimCol]];

    If[c1 == 0 || c2 == 0, Return[Infinity]]; (* Cannot eliminate if coeff is zero *)

    (* Priority: Direct elimination (e.g., 4 and -4) *)
    If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], Return[0]];

    (* Fallback: minimize LCM *)
    LCM[Abs[c1], Abs[c2]]
  ];

  First @ MinimalBy[pairs, scorePair]
];

(*
  Classifies the intersection of three planes in 3D space.
  Returns: <|"Type" -> "POINT"|"LINE"|"PLANE"|"NONE"|"INFINITE", ...|>
  Uses Rank(A) vs Rank(Ab) theorem and NullSpace analysis.
*)
systemIntersection3[A_, b_, vars_] := Module[
  {rankA, rankAb, sol, ns, x0, v, v2, x, y, z, inst},

  {x, y, z} = vars;

  rankA  = MatrixRank[A];
  rankAb = MatrixRank[Join[A, Transpose[{b}], 2]];

  If[rankAb > rankA, Return[<|"Type" -> "NONE"|>]];

  If[rankA == 3,
    sol = LinearSolve[A, b];
    Return[<|"Type" -> "POINT", "Point" -> sol|>];
  ];

  ns = NullSpace[A];

  (* Use FindInstance to get a guaranteed numeric point on the solution space *)
  inst = Quiet @ FindInstance[A.vars == b, vars, Reals, 1];

  If[inst === {}, Return[<|"Type" -> "NONE"|>]]; (* Should be covered by rank check, but safe fallback *)

  x0 = {x, y, z} /. First[inst];

  If[Length[ns] == 1,
    v = First[ns];
    Return[<|"Type" -> "LINE", "Point" -> x0, "Dir" -> v|>];
  ];

  If[Length[ns] >= 2,
    {v, v2} = ns[[1 ;; 2]];
    Return[<|"Type" -> "PLANE", "Point" -> x0, "Dir1" -> v, "Dir2" -> v2|>];
  ];

  <|"Type" -> "INFINITE"|>
];

(*
  Handles formatting of infinite solutions for 3x3 systems.
  Attempts to find a "nice" integer parameterization. If that fails, falls back to standard RREF.
*)
printInfiniteResult3[A_, b_, vars_] := Module[
  {
    nVars, aug, rref,
    pivotCols = {}, freeCols, nFree,
    formalParams, exprs,
    i, row, pcol, pcoef, rhs,
    nonzeroQ,
    wordParam, wordChoose,
    parBox, rPowBox, condBox, vecBox, kBox,
    best
  },

  nVars = Length[vars];
  aug  = Normal @ Join[A, Transpose[{b}], 2];
  rref = Normal @ RowReduce[aug];

  nonzeroQ[x_] := If[NumericQ[x], Chop[x] =!= 0, !TrueQ[PossibleZeroQ[x]]];

  (* Check for contradiction in RREF *)
  If[
    AnyTrue[rref, (AllTrue[#[[1 ;; nVars]], !nonzeroQ[#] &] && nonzeroQ[#[[nVars + 1]]]) &],
    CellTextU["Sústava nemá riešenie (v redukovanej sústave vznikol spor)."];
    Return[<|"Type" -> "NONE"|>];
  ];

  (* Attempt nice parametrization *)
  best = chooseNiceParametrization3[A, b, vars];

  If[best =!= $Failed,
    formalParams = best["Params"];
    exprs = best["Exprs"];,

    (* FALLBACK: Standard RREF parameterization *)
    Do[
      row = rref[[i, 1 ;; nVars]];
      pcol = SelectFirst[Range[nVars], nonzeroQ[row[[#]]] &, Missing["NotFound"]];
      If[pcol =!= Missing["NotFound"], AppendTo[pivotCols, pcol]],
      {i, 1, Length[rref]}
    ];

    pivotCols = DeleteDuplicates[pivotCols];
    freeCols  = Complement[Range[nVars], pivotCols];
    nFree     = Length[freeCols];

    formalParams = Take[{\[FormalT], \[FormalS], \[FormalR], \[FormalU], \[FormalV]}, nFree];
    exprs = ConstantArray[0, nVars];

    Do[
      exprs[[freeCols[[k]]]] = formalParams[[k]],
      {k, 1, nFree}
    ];

    Do[
      row = rref[[i, 1 ;; (nVars + 1)]];
      pcol = SelectFirst[Range[nVars], nonzeroQ[row[[#]]] &, Missing["NotFound"]];
      If[pcol === Missing["NotFound"], Continue[]];

      pcoef = row[[pcol]];
      rhs   = row[[nVars + 1]];

      exprs[[pcol]] = Together[
        (rhs - If[nFree > 0, row[[freeCols]].exprs[[freeCols]], 0]) / pcoef
      ],
      {i, 1, Length[rref]}
    ];
  ];

  nFree = Length[formalParams];
  wordParam  = If[nFree == 1, "parametra", "parametrov"];
  wordChoose = If[nFree == 1, "parameter", "parametre"];

  CellTextU["Riešenia zapíšeme pomocou " <> wordParam <> "."];

  If[nFree > 0,
    CellTextU["Zvolíme " <> wordChoose <> " (voľné hodnoty):"];
    CellBox @ Grid[
      Table[{formalParams[[k]], "\[Element]", "\[DoubleStruckR]"}, {k, 1, nFree}],
      Alignment -> {{Center, Center, Left}},
      Spacings -> {0.6, 0.8}
    ];
  ];

  CellTextU["Potom platí:"];
  CellBox @ Grid[
    Table[{vars[[k]], "=", TraditionalForm[exprs[[k]]]}, {k, 1, nVars}],
    Alignment -> {{Right, Center, Left}},
    Spacings -> {0.6, 0.8}
  ];

  (* Format the solution set K *)
  vecBox = RowBox @ Join[
    {"["},
    Riffle[ToBoxes[#, TraditionalForm] & /@ exprs, RowBox[{";", " "}]],
    {"]"}
  ];

  If[nFree > 0,
    parBox = RowBox @ Riffle[ToBoxes[#, TraditionalForm] & /@ formalParams, RowBox[{",", " "}]];
    rPowBox = If[nFree == 1, "\[DoubleStruckR]", SuperscriptBox["\[DoubleStruckR]", ToBoxes[nFree]]];

    condBox = If[
      nFree == 1,
      RowBox[{ToBoxes[formalParams[[1]], TraditionalForm], "\[Element]", rPowBox}],
      RowBox[{RowBox[{"(", parBox, ")"}], "\[Element]", rPowBox}]
    ];

    kBox = RowBox[{
      StyleBox["K", FontSlant -> "Italic"],
      "=",
      RowBox[{"{", RowBox[{vecBox, " ", "\[VerticalSeparator]", " ", condBox}], "}"}]
    }];

    CellPrint @ Cell[
      BoxData @ FormBox[kBox, TraditionalForm],
      "DisplayFormula",
      BaseStyle -> {FontSize -> 14},
      ShowStringCharacters -> False
    ];
  ];

  <|
    "Type" -> "INFINITE",
    "Parameters" -> formalParams,
    "Expressions" -> AssociationThread[vars -> exprs]
  |>
];

(*
  Heuristic search for a "nice" parameterization (avoiding fractions where possible).
  Iterates through possible free variable choices and scores them based on the
  simplicity (denominators) of the resulting expressions.
*)
chooseNiceParametrization3[A_, b_, vars_] := Module[
  {eqs, n, rank, nFree, params0, candidates, try, results, best,
    scoreExpr, scoreAll, scalesFor, applyScales},

  n = Length[vars];
  eqs = Thread[A.vars == b];

  rank = MatrixRank[A];
  nFree = n - rank;
  If[nFree <= 0, Return[$Failed]];

  params0 = Take[{\[FormalT], \[FormalS], \[FormalR], \[FormalU], \[FormalV]}, nFree];
  candidates = Subsets[vars, {nFree}];

  (* Score based on denominators *)
  scoreExpr[ex_, params_] := Module[{c0, coeffs, dens},
    c0 = ex /. Thread[params -> 0];
    coeffs = Coefficient[ex, params];
    dens = Denominator /@ Rationalize[Join[{c0}, coeffs], 0] /. 0 -> 1;
    Total[If[# == 1, 0, #] & /@ dens]
  ];

  scoreAll[exprs_, params_] := Total[scoreExpr[#, params] & /@ exprs];

  (* Calculate scaling factors to eliminate fractions in parameters *)
  scalesFor[exprs_, params_] := Table[
    Module[{coeffs, dens},
      coeffs = Coefficient[exprs, params[[j]]];
      dens = Denominator /@ Rationalize[coeffs, 0] /. 0 -> 1;
      LCM @@ dens
    ],
    {j, Length[params]}
  ];

  applyScales[exprs_, params_, scales_] :=
      Together @ (exprs /. Thread[params -> (scales*params)]);

  try[freeVars_] := Module[
    {params = params0, remVars, sol, rules, exprs, scales, exprsScaled, sc},

    remVars = Complement[vars, freeVars];
    sol = Quiet @ Check[
      Solve[eqs /. Thread[freeVars -> params], remVars, Reals],
      $Failed
    ];
    If[sol === $Failed || sol === {}, Return[Nothing]];

    rules = Join[Thread[freeVars -> params], sol[[1]]];
    exprs = Together[vars /. rules];

    scales = scalesFor[exprs, params];
    exprsScaled = applyScales[exprs, params, scales];

    sc = scoreAll[exprsScaled, params] + Total[If[# == 1, 0, #] & /@ scales];

    <|"FreeVars" -> freeVars, "Params" -> params, "Scales" -> scales, "Exprs" -> exprsScaled, "Score" -> sc|>
  ];

  results = try /@ candidates;
  If[results === {}, Return[$Failed]];

  best = First @ MinimalBy[results, #Score &];
  best
];

(* --- Equation Visualization Helpers --- *)

highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

(* Displays equations in a structured grid with alignment *)
alignedEquations[data_] := Module[
  {
    eqSign = Style["=", 16],
    vbar   = Style["\[VerticalSeparator]", GrayLevel[.25]],
    stepRow
  },

  stepRow[{lhs_, rhs_, note_}] := {
    lhs,
    eqSign,
    rhs,
    If[note === "" || note === None,
      "",
      Style[Row[{vbar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]
    ]
  };
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  Grid[
    stepRow /@ data,
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, 0.6},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* Custom grid formatting to visually separate elimination steps (e.g. before/after multiplication) *)
alignedEquationsGrouped[data_, breaks_List : {2}, gap_ : 1.25] := Module[
  {eqSign = Style["=", 16], vbar = Style["\[VerticalSeparator]", GrayLevel[.25]],
    stepRow, n, baseGap = 0.6, rowGaps},

  stepRow[{lhs_, rhs_, note_}] := {
    lhs,
    eqSign,
    rhs,
    If[note === "" || note === None,
      "",
      Style[Row[{vbar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]
    ]
  };
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  n = Length[data];

  rowGaps = Join[
    {baseGap},
    Table[If[MemberQ[breaks, i], gap, baseGap], {i, 1, Max[0, n - 1]}],
    {baseGap}
  ];

  Grid[
    stepRow /@ data,
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, rowGaps},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* Formats LHS for 2D systems (ax + by) *)
formatLHS[cx_, cy_, choice_, vars2_List : {x, y}] := Module[
  {xv, yv, tX, tY, sign},

  {xv, yv} = vars2;
  tX = If[choice == "X", highlightTerm[cx xv], cx xv];

  If[cy == 0,
    TraditionalForm[tX],
    sign = If[cy < 0, " - ", " + "];
    tY = If[Abs[cy] == 1, yv, Abs[cy] yv];
    tY = If[choice == "Y", highlightTerm[tY], tY];
    Row[{TraditionalForm[tX], sign, TraditionalForm[tY]}]
  ]
];

(* Formats LHS for 3D systems (ax + by + cz) *)
formatLHS3[cx_, cy_, cz_, choice_] := Module[{tX, tY, tZ, sY, sZ, terms = {}},
  If[cx != 0,
    tX = Which[cx === 1, x, cx === -1, -x, True, cx x];
    If[choice == "X", tX = highlightTerm[tX]];
    AppendTo[terms, TraditionalForm[tX]];
  ];

  If[cy != 0,
    sY = If[Length[terms] > 0, If[cy < 0, " - ", " + "], If[cy < 0, "-", ""]];
    tY = If[Abs[cy] == 1, y, Abs[cy] y];
    If[choice == "Y", tY = highlightTerm[tY]];
    AppendTo[terms, sY];
    AppendTo[terms, TraditionalForm[tY]];
  ];

  If[cz != 0,
    sZ = If[Length[terms] > 0, If[cz < 0, " - ", " + "], If[cz < 0, "-", ""]];
    tZ = If[Abs[cz] == 1, z, Abs[cz] z];
    If[choice == "Z", tZ = highlightTerm[tZ]];
    AppendTo[terms, sZ];
    AppendTo[terms, TraditionalForm[tZ]];
  ];

  If[Length[terms] == 0, TraditionalForm[0], Row[terms]]
];

lineLegendText[a_, b_, c_] := Module[{m, q, fmt, mStr},
  fmt[t_] := ToString[TraditionalForm[Together[t]]];

  If[b == 0,
    "x = " <> fmt[c/a],
    m = Together[-a/b];
    q = Together[c/b];
    mStr = Which[m === 1, "", m === -1, "-", True, fmt[m]];
    "y = " <> mStr <> "x" <> Which[q === 0, "", q > 0, " + " <> fmt[q], True, " - " <> fmt[Abs[q]]]
  ]
];

(* Generates legend expression for a plane ax+by+cz=d *)
planeLegendText[row_, rhs_, vars_] := Module[
  {lhsExpr},
  lhsExpr = formatLHS3[row[[1]], row[[2]], row[[3]], ""];
  Row[{lhsExpr, " = ", TraditionalForm[rhs]}]
];

multiplyNoteString[m_] := Which[
  m == 1, "",
  m < 0, "\[CenterDot] (" <> ToString[m] <> ")",
  True, "\[CenterDot] " <> ToString[m]
];

(* Format substitution notes, e.g., "x -> 5" *)
substNote[solMap_, remVars_, row_, vars_] := Module[
  {usedVars},
  usedVars = Select[remVars, row[[First@First@Position[vars, #]]] =!= 0 &];

  If[usedVars === {},
    "",
    Row[
      Riffle[
        (Row[{#, " \[Rule] ", TraditionalForm[Together[solMap[#]]]}] & /@ usedVars),
        ", "
      ]
    ]
  ]
];

numOrParen[val_] := If[
  NumericQ[val] && val < 0,
  Row[{"(", TraditionalForm[Together[val]], ")"}],
  TraditionalForm[Together[val]]
];

coeffTimesValue[coeff_, val_] := Which[
  coeff === 0, 0,
  coeff === 1, numOrParen[val],
  coeff === -1, Row[{"-", numOrParen[val]}],
  True, Row[{coeff, " \[CenterDot] ", numOrParen[val]}]
];

(* Constructs LHS expression substituting known values while keeping the unknown variable symbolic *)
formatSubstLHS3[row_, vars_, solMap_, unknownVar_] := Module[
  {terms = {}, first = True, addTerm},

  addTerm[expr_, sign_] := (
    If[first,
      AppendTo[terms, If[sign === -1, Row[{"-", expr}], expr]];
      first = False;,
      AppendTo[terms, If[sign === -1, " - ", " + "]];
      AppendTo[terms, expr];
    ]
  );

  Do[
    If[row[[i]] =!= 0,
      If[vars[[i]] === unknownVar,
        (* Unknown variable term *)
        With[{c = row[[i]]},
          If[c > 0,
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, c unknownVar]], +1],
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, Abs[c] unknownVar]], -1]
          ]
        ],
        (* Known variable substitution *)
        With[{c = row[[i]], v = solMap[vars[[i]]]},
          If[c > 0,
            addTerm[coeffTimesValue[c, v], +1],
            addTerm[coeffTimesValue[Abs[c], v], -1]
          ]
        ]
      ];
    ],
    {i, 1, Length[vars]}
  ];

  If[Length[terms] == 0, TraditionalForm[0], Row[terms]]
];

(* Evaluates numeric terms in the substitution expression *)
formatSubstLHS3Eval[row_, vars_, solMap_, unknownVar_] := Module[
  {terms = {}, first = True, addTerm},

  addTerm[val_, sign_] := (
    If[first,
      AppendTo[terms, If[sign === -1, Row[{"-", val}], val]];
      first = False;,
      AppendTo[terms, If[sign === -1, " - ", " + "]];
      AppendTo[terms, val];
    ]
  );

  Do[
    If[row[[i]] =!= 0,
      If[vars[[i]] === unknownVar,
        With[{c = row[[i]]},
          If[c > 0,
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, c unknownVar]], +1],
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, Abs[c] unknownVar]], -1]
          ]
        ],
        With[{prod = Together[row[[i]] * solMap[vars[[i]]]]},
          If[PossibleZeroQ[prod],
            Null,
            If[TrueQ[prod > 0] || prod === 0,
              addTerm[TraditionalForm[prod], +1],
              addTerm[TraditionalForm[Abs[prod]], -1]
            ]
          ]
        ]
      ];
    ],
    {i, 1, Length[vars]}
  ];

  If[Length[terms] == 0, TraditionalForm[0], Row[terms]]
];

(* --- Difficulty Settings --- *)

coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff["MEDIUM"] := 5;
coeffRangeByDiff["HARD"] := 5;
coeffRangeByDiff[_] := 5;

boundByDiff["EASY"] := 60;
boundByDiff[_] := 90;

randomRow[n_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, n];
  If[AllTrue[v, # == 0 &], randomRow[n, r], v]
];

(* Optimized 2D row generator for EASY level (ax +/- y) *)
randomRow2NoZeros["EASY", r_] := Module[{a, s},
  a = RandomInteger[{-r, r}];
  If[a == 0, Return[randomRow2NoZeros["EASY", r]]];
  s = RandomChoice[{-1, 1}];
  {a, s}
];

randomRow2NoZeros[_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, 2];
  If[v[[1]] == 0 || v[[2]] == 0, randomRow2NoZeros["OTHER", r], v]
];

(* 3D row generator (MEDIUM): Max 1 zero allowed per row *)
randomRow3["MEDIUM", r_] := Module[{v},
  v = RandomInteger[{-r, r}, 3];
  If[Count[v, 0] > 1 || AllTrue[v, #==0&], randomRow3["MEDIUM", r], v]
];
randomRow3[_, r_] := randomRow3["MEDIUM", r];

numbersNiceQ[A_, b_, diff_] := Module[{bd = boundByDiff[diff]},
  Max[Abs @ Join[Flatten[A], Flatten[b]]] <= bd
];

(* --- System Generators --- *)

generateSystemOne[dim_, diff_] := Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-5, 5}, dim];

  If[dim == 2,
    A = Table[randomRow2NoZeros[diff, r], {2}];,
    (* dim 3 *)
    A = Table[randomRow3[diff, r], {3}];
    If[Count[Flatten[A], 0] > 1, Return[$Failed]];
  ];

  If[Det[A] == 0, Return[$Failed]];
  b = A . x0;
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>
];

generateSystemNone2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow2NoZeros[diff, r];
  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1 + RandomChoice[{-5, -3, 3, 5}]; (* Contradiction in RHS *)
  A = {row1, row2};
  b = {c1, c2};
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

generateSystemNone3[diff_] := Module[{r, row1, row2, row3, k1, k2, c1, c2, c3, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow3[diff, r];
  row2 = randomRow3[diff, r];

  If[LinearDependentQ[{row1, row2}], Return[$Failed]];

  k1 = RandomChoice[{-2, -1, 1, 2}];
  k2 = RandomChoice[{-2, -1, 1, 2}];

  row3 = k1 * row1 + k2 * row2; (* Linear dependence *)

  c1 = RandomInteger[{-5, 5}];
  c2 = RandomInteger[{-5, 5}];
  c3 = k1 * c1 + k2 * c2 + RandomChoice[{-5, -3, 3, 5}]; (* Contradiction *)

  A = {row1, row2, row3};
  If[Count[Flatten[A], 0] > 1, Return[$Failed]];
  b = {c1, c2, c3};

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

generateSystemInfinite2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow2NoZeros[diff, r];
  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1;
  A = {row1, row2};
  b = {c1, c2};
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

generateSystemInfinite3[diff_] := Module[{r, row1, row2, row3, k1, k2, c1, c2, c3, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow3[diff, r];
  row2 = randomRow3[diff, r];

  If[LinearDependentQ[{row1, row2}], Return[$Failed]];

  k1 = RandomChoice[{-2, -1, 1, 2}];
  k2 = RandomChoice[{-2, -1, 1, 2}];

  row3 = k1 * row1 + k2 * row2;
  c1 = RandomInteger[{-5, 5}];
  c2 = RandomInteger[{-5, 5}];
  c3 = k1 * c1 + k2 * c2; (* Consistent *)

  A = {row1, row2, row3};
  If[Count[Flatten[A], 0] > 1, Return[$Failed]];
  b = {c1, c2, c3};

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

LinearDependentQ[vecs_] := MatrixRank[vecs] < Length[vecs];

generateSystemHard3[args___] := (Message[Gen01::notimpl, "HARD"]; $Failed);

(* --- Elimination Analysis & Steps --- *)

(* Analyzes a column to find optimal multipliers based on LCM *)
analyzeVariableElimination[colIndex_, A_] := Module[
  {c1, c2, lcm, mul1, mul2, score},
  c1 = A[[1, colIndex]];
  c2 = A[[2, colIndex]];
  If[c1 == 0 || c2 == 0, Return[<|"Score" -> 9999|>]];
  lcm = LCM[Abs[c1], Abs[c2]];
  mul1 = lcm / Abs[c1];
  mul2 = lcm / Abs[c2];
  score = lcm + If[mul1 > 1 && mul2 > 1, 1000, 0];
  <|"Score" -> score, "LCM" -> lcm, "RawMul1" -> mul1, "RawMul2" -> mul2, "Coeffs" -> {c1, c2}|>
];

(* Finds the best column to eliminate in a 3x3 system by summing pairwise LCM scores *)
analyzeElimination3[A_] := Module[{scores, bestCol},
  scores = Table[
    Module[{col, c1, c2, c3, s12, s13},
      col = A[[All, j]];
      {c1, c2, c3} = col;
      If[c1 == 0, 9999,
        s12 = LCM[Abs[c1], If[c2 == 0, 1, Abs[c2]]];
        s13 = LCM[Abs[c1], If[c3 == 0, 1, Abs[c3]]];
        s12 + s13
      ]
    ],
    {j, 1, 3}
  ];
  Ordering[scores, 1][[1]]
];

(* Prepares 2x2 elimination: selects variable and calculates multipliers *)
eliminationStart[A_, b_, vars_] := Module[
  {content = {}, x, y, a1, b1, c1, a2, b2, c2,
    resX, resY, choice, targetVar,
    rawM1, rawM2, m1, m2, k1, k2,
    c1x, c1y, c1rhs, c2x, c2y, c2rhs,
    rows1, rows2, needsMultiplication, preparedRows},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  (* Heuristic comparison of X vs Y elimination difficulty *)
  resX = analyzeVariableElimination[1, A];
  resY = analyzeVariableElimination[2, A];

  If[resY["Score"] < resX["Score"],
    choice = "Y"; targetVar = y;
    {k1, k2} = resY["Coeffs"];
    {rawM1, rawM2} = {resY["RawMul1"], resY["RawMul2"]};
    ,
    choice = "X"; targetVar = x;
    {k1, k2} = resX["Coeffs"];
    {rawM1, rawM2} = {resX["RawMul1"], resX["RawMul2"]};
  ];

  AppendTo[content, Style["1. Príprava na elimináciu - vyrušenie jednej premennej", Bold]];

  needsMultiplication = !(Sign[k1] =!= Sign[k2] && rawM1 === 1 && rawM2 === 1);

  If[needsMultiplication,
    AppendTo[content, "Aby sme vyrušili premennú " <> ToString[targetVar] <> ", upravíme rovnice násobením tak, aby mali pri tejto premennej rovnaký koeficient s opačným znamienkom."],
    AppendTo[content, "Koeficienty pri premennej " <> ToString[targetVar] <> " sú už opačné, preto nemusíme nič žiadnym číslom prenásobovať. Môžeme hneď prejsť na sčítanie rovnic a označiť si členy, ktoré sa vyrušia."]
  ];

  m1 = rawM1;
  m2 = rawM2;
  If[Sign[k1] === Sign[k2], m2 = -m2];

  rows1 = {
    {formatLHS[a1, b1, choice, vars], c1, multiplyNoteString[m1]},
    {formatLHS[a2, b2, choice, vars], c2, multiplyNoteString[m2]}
  };

  c1x = m1*a1; c1y = m1*b1; c1rhs = m1*c1;
  c2x = m2*a2; c2y = m2*b2; c2rhs = m2*c2;

  rows2 = {
    {formatLHS[c1x, c1y, "", vars], c1rhs, ""},
    {formatLHS[c2x, c2y, "", vars], c2rhs, ""}
  };

  If[needsMultiplication,
    AppendTo[content, alignedEquationsGrouped[Join[rows1, rows2], {2}, 1]],
    (* Skip initial state if no multiplication needed *)
    preparedRows = {
      {formatLHS[a1, b1, choice, vars], c1, ""},
      {formatLHS[a2, b2, choice, vars], c2, ""}
    };
    AppendTo[content, alignedEquations[preparedRows]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "failed" -> False|>
];

(* Steps: 2x2 Unique Solution *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y,
    sumRHS, sumCoeffX, sumCoeffY, calcVar, calcVal, otherVar, otherVal,
    stepsY, stepsSub, valProduct, rhsRem, elimVarStr,
    explicitSubstLHS, calculatedSubstLHS,
    substCoeff, substConst, termResult, termUnknown, solPair},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  elimVarStr = data["EliminatedVariable"];

  sumRHS = m1 c1 + m2 c2;
  sumCoeffX = m1 a1 + m2 a2;
  sumCoeffY = m1 b1 + m2 b2;

  AppendTo[content, Style["2. Sčítanie rovníc – získame jednu premennú", Bold]];
  AppendTo[content, "Teraz rovnice sčítame. Premenná " <> ToString[If[elimVarStr=="X", x, y]] <> " sa vyruší (zmizne), pretože má v oboch rovniciach opačné koeficienty."];

  stepsY = {};

  (* Explicit phase: Visualize addition *)
  Module[{c1x2, c1y2, c1rhs2, c2x2, c2y2, c2rhs2, signSep, explicitLHS, explicitRHS},
    c1x2 = m1*a1;  c1y2 = m1*b1;  c1rhs2 = m1*c1;
    c2x2 = m2*a2;  c2y2 = m2*b2;  c2rhs2 = m2*c2;
    signSep[v_] := If[v < 0, " - ", " + "];
    explicitLHS = Row[{
      TraditionalForm[c1x2*x], signSep[c2x2], TraditionalForm[Abs[c2x2]*x],
      signSep[c1y2], TraditionalForm[Abs[c1y2]*y],
      signSep[c2y2], TraditionalForm[Abs[c2y2]*y]
    }];
    explicitRHS = Row[{c1rhs2, signSep[c2rhs2], Abs[c2rhs2]}];
    AppendTo[stepsY, {explicitLHS, explicitRHS, ""}];
  ];

  If[elimVarStr == "X",
    termResult = sumCoeffY*y;
    AppendTo[stepsY, {termResult, sumRHS, ""}];
    AppendTo[content, alignedEquations[stepsY]];
    stepsY = {};

    If[sumCoeffY == 1,
      AppendTo[content, "Po sčítaní sme dostali jednoduchú rovnicu, z ktorej hneď určíme hodnotu premennej " <> ToString[y] <> "."],
      AppendTo[content, "Zostala nám rovnica s jednou premennou. Upravíme ju (napr. vydelením koeficientom), aby sme dostali samotnú premennú."]
    ];

    If[sumCoeffY == 0, Return[$Failed]];
    calcVar = y; calcVal = sumRHS / sumCoeffY; otherVar = x;
    If[sumCoeffY =!= 1, AppendTo[stepsY, {sumCoeffY y, sumRHS, ": " <> ToString[sumCoeffY]}]];
  ];

  If[elimVarStr == "Y",
    termResult = sumCoeffX*x;
    AppendTo[stepsY, {termResult, sumRHS, ""}];
    AppendTo[content, alignedEquations[stepsY]];
    stepsY = {};

    If[sumCoeffX == 1,
      AppendTo[content, "Po sčítaní sme dostali jednoduchú rovnicu, z ktorej hneď určíme hodnotu premennej " <> ToString[x] <> "."],
      AppendTo[content, "Zostala nám rovnica s jednou premennou. Upravíme ju (napr. vydelením koeficientom), aby sme dostali samotnú premennú."]
    ];

    If[sumCoeffX == 0, Return[$Failed]];
    calcVar = x; calcVal = sumRHS / sumCoeffX; otherVar = y;
    If[sumCoeffX =!= 1, AppendTo[stepsY, {sumCoeffX x, sumRHS, ": " <> ToString[sumCoeffX]}]];
  ];

  If[Length[stepsY] > 0, AppendTo[content, alignedEquations[stepsY]]];
  AppendTo[content, highlightGrid[alignedEquations[{{calcVar, calcVal, ""}}]]];

  AppendTo[content, Style["3. Dosadenie – vypočítame druhú premennú", Bold]];
  AppendTo[content, "Vypočítanú hodnotu " <> ToString[calcVar] <> " dosadíme do jednej z pôvodných rovníc (napr. do prvej). Získame rovnicu len s druhou premennou a vyrátame jej hodnotu."];

  stepsSub = {};
  AppendTo[stepsSub, {a1 x + b1 y, c1, Row[{calcVar, " \[Rule] ", TraditionalForm[Together[calcVal]]}]}];

  If[elimVarStr == "X",
    substCoeff = a1; substConst = b1;
    explicitSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[substConst < 0, " - ", " + "],
      Abs[substConst], " \[CenterDot] ", If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal]
    }];
    valProduct = substConst * calcVal;
    calculatedSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[valProduct < 0, " - ", " + "], Abs[valProduct]
    }];
    ,
    (* Eliminated Y, computing X *)
    substCoeff = b1; substConst = a1;
    explicitSubstLHS = Row[{
      substConst, " \[CenterDot] ", If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal],
      If[substCoeff < 0, " - ", " + "], If[Abs[substCoeff] == 1, y, Row[{Abs[substCoeff], y}]]
    }];
    valProduct = substConst * calcVal;
    calculatedSubstLHS = Row[{
      valProduct,
      If[substCoeff < 0, " - ", " + "], If[Abs[substCoeff] == 1, y, Row[{Abs[substCoeff], y}]]
    }];
  ];

  AppendTo[stepsSub, {explicitSubstLHS, c1, ""}];

  noteShift = Which[
    PossibleZeroQ[valProduct], "",
    TrueQ[valProduct > 0],      Row[{"- ", TraditionalForm[valProduct]}],
    True,                     Row[{"+ ", TraditionalForm[Abs[valProduct]]}]
  ];

  AppendTo[stepsSub, {calculatedSubstLHS, c1, noteShift}];

  rhsRem = c1 - valProduct;

  If[substCoeff =!= 1,
    termUnknown = If[elimVarStr == "X", a1 x, b1 y];
    AppendTo[stepsSub, {termUnknown, rhsRem, ": " <> ToString[substCoeff]}];
  ];

  otherVal = rhsRem / substCoeff;
  AppendTo[stepsSub, {otherVar, TraditionalForm[Together[otherVal]], ""}];
  AppendTo[content, alignedEquations[stepsSub]];
  AppendTo[content, highlightGrid[alignedEquations[{{otherVar, TraditionalForm[Together[otherVal]], ""}}]]];

  solPair = If[elimVarStr == "X", {otherVal, calcVal}, {calcVal, otherVal}];
  <|"Content" -> content, "Solution" -> solPair|>
];

(* Steps: 2x2 No Solution *)
stepsNone2[A_, b_, vars_, includeConclusion_: True] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, sumRHS, stepsY},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  sumRHS = m1 c1 + m2 c2;

  AppendTo[content, Style["2. Sčítanie rovníc – kontrola, či nevznikne spor", Bold]];
  AppendTo[content, "Rovnice sčítame (po úprave z kroku 1). Sledujeme, či nevznikne nemožná rovnosť."];

  stepsY = {};

  Module[{c1x2, c1y2, c1rhs2, c2x2, c2y2, c2rhs2, signSep, explicitLHS, explicitRHS},
    c1x2 = m1*a1;  c1y2 = m1*b1;  c1rhs2 = m1*c1;
    c2x2 = m2*a2;  c2y2 = m2*b2;  c2rhs2 = m2*c2;
    signSep[v_] := If[v < 0, " - ", " + "];
    explicitLHS = Row[{
      TraditionalForm[c1x2*x], signSep[c2x2], TraditionalForm[Abs[c2x2]*x],
      signSep[c1y2], TraditionalForm[Abs[c1y2]*y],
      signSep[c2y2], TraditionalForm[Abs[c2y2]*y]
    }];
    explicitRHS = Row[{c1rhs2, signSep[c2rhs2], Abs[c2rhs2]}];
    AppendTo[stepsY, {explicitLHS, explicitRHS, ""}];
  ];

  AppendTo[stepsY, {0, sumRHS, ""}];
  AppendTo[content, alignedEquations[stepsY]];

  If[includeConclusion,
    AppendTo[content, Style["3. Záver", Bold]];
    AppendTo[content, "Po sčítaní vyšla nepravdivá rovnosť (napr. 0 = nenulové číslo). To je spor, preto sústava nemá riešenie."];
  ];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

(* Steps: 2x2 Infinite Solutions *)
stepsInfinite2[A_, b_, vars_, includeConclusion_: True] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, stepsY},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];

  AppendTo[content, Style["2. Sčítanie rovníc – overíme, či sú rovnice totožné", Bold]];
  AppendTo[content, "Rovnice sčítame (po úprave z kroku 1). Ak vyjde 0 = 0, znamená to, že sme dostali totožnú rovnicu."];

  stepsY = {};

  Module[{c1x2, c1y2, c1rhs2, c2x2, c2y2, c2rhs2, signSep, explicitLHS, explicitRHS},
    c1x2 = m1*a1;  c1y2 = m1*b1;  c1rhs2 = m1*c1;
    c2x2 = m2*a2;  c2y2 = m2*b2;  c2rhs2 = m2*c2;
    signSep[v_] := If[v < 0, " - ", " + "];
    explicitLHS = Row[{
      TraditionalForm[c1x2*x], signSep[c2x2], TraditionalForm[Abs[c2x2]*x],
      signSep[c1y2], TraditionalForm[Abs[c1y2]*y],
      signSep[c2y2], TraditionalForm[Abs[c2y2]*y]
    }];
    explicitRHS = Row[{c1rhs2, signSep[c2rhs2], Abs[c2rhs2]}];
    AppendTo[stepsY, {explicitLHS, explicitRHS, ""}];
  ];

  AppendTo[stepsY, {0, 0, ""}];
  AppendTo[content, alignedEquations[stepsY]];

  If[includeConclusion,
    AppendTo[content, Style["3. Záver", Bold]];
    AppendTo[content, "Po sčítaní vyšla pravdivá rovnosť 0 = 0. To znamená, že druhá rovnica je len násobkom prvej (opisujú tú istú priamku). Sústava má nekonečne veľa riešení."];
  ];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

(* --- 3x3 Steps --- *)

(* Performs elimination of a specific variable between two rows *)
reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_, _, _] := Module[
  {content = {}, valA, valB, lcm, m1, m2, newRow, newRHS, rowsDisp, elimVarName, choiceStr},

  valA = rowA[[elimCol]];
  valB = rowB[[elimCol]];
  elimVarName = vars[[elimCol]];
  choiceStr = {"X", "Y", "Z"}[[elimCol]];

  If[valA == 0 || valB == 0,
    rowsDisp = {
      {formatLHS3[rowA[[1]], rowA[[2]], rowA[[3]], choiceStr], rhsA, ""},
      {formatLHS3[rowB[[1]], rowB[[2]], rowB[[3]], choiceStr], rhsB, ""}
    };
    AppendTo[content, alignedEquations[rowsDisp]];

    If[valB == 0,
      newRow = rowB; newRHS = rhsB;,
      newRow = rowA; newRHS = rhsA;
    ];

    AppendTo[content,
      alignedEquations[{
        {
          Style[formatLHS3[newRow[[1]], newRow[[2]], newRow[[3]], ""], Darker[Green, 0.2]],
          Style[newRHS, Darker[Green, 0.2]],
          ""
        }
      }]
    ];

    Return[<|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>];
  ];

  lcm = LCM[Abs[valA], Abs[valB]];
  m1 = lcm/Abs[valA];
  m2 = lcm/Abs[valB];

  If[Sign[valA] == Sign[valB], m2 = -m2];

  rowsDisp = {
    {formatLHS3[rowA[[1]], rowA[[2]], rowA[[3]], choiceStr], rhsA, multiplyNoteString[m1]},
    {formatLHS3[rowB[[1]], rowB[[2]], rowB[[3]], choiceStr], rhsB, multiplyNoteString[m2]}
  };
  AppendTo[content, alignedEquations[rowsDisp]];

  newRow = m1*rowA + m2*rowB;
  newRHS = m1*rhsA + m2*rhsB;

  AppendTo[content,
    alignedEquations[{
      {
        Style[formatLHS3[newRow[[1]], newRow[[2]], newRow[[3]], ""], Darker[Green, 0.2]],
        Style[newRHS, Darker[Green, 0.2]],
        ""
      }
    }]
  ];

  <|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>
];

(* Steps: 3x3 Unique Solution *)
stepsOne3[A_, b_, vars_] := Module[
  {content = {}, elimVarStr, elimCol, remVars, remCols,
    resPair1, resPair2,
    rowIV, rhsIV, rowV, rhsV,
    subSteps, sol2x2,
    eqSubst, finalVar, finalVal, solMap, A2, b2, solFull},

  AppendTo[content, Style["1. Redukcia sústavy 3×3 na 2×2", Bold]];
  elimCol = analyzeElimination3[A];
  elimVarStr = vars[[elimCol]];

  AppendTo[content, "Vylúčime premennú " <> ToString[elimVarStr] <> ". Použijeme na to dve dvojice rovníc, napríklad prvú s druhou a prvú s treťou."];

  Module[
    {zeroRows, nonZeroRows, twoCombosQ, iKeep, pair, i1, i2},

    zeroRows    = Flatten @ Position[A[[All, elimCol]], 0];
    nonZeroRows = Complement[Range[3], zeroRows];

    If[Length[zeroRows] >= 1 && Length[nonZeroRows] >= 2,
      twoCombosQ = False;
      iKeep = First[zeroRows];
      pair = pickBestElimPair[nonZeroRows, elimCol, A];
      {i1, i2} = pair;

      AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];
      resPair1 = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars, "I", "II"];
      content  = Join[content, resPair1["Content"]];
      rowIV    = resPair1["Row"]; rhsIV = resPair1["RHS"];

      rowV  = A[[iKeep]];
      rhsV  = b[[iKeep]];
      AppendTo[content, Style["b) Rovnica bez vyrušovanej premennej (použijeme ju priamo):", Italic]];
      AppendTo[content, alignedEquations[{{formatLHS3[rowV[[1]], rowV[[2]], rowV[[3]], ""], rhsV, ""}}]];
      ,
      (* Fallback: Standard case - two combinations *)
      twoCombosQ = True;
      AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];
      resPair1 = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "I", "II"];
      content = Join[content, resPair1["Content"]];
      rowIV = resPair1["Row"]; rhsIV = resPair1["RHS"];

      AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];
      resPair2 = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "I", "III"];
      content = Join[content, resPair2["Content"]];
      rowV = resPair2["Row"]; rhsV = resPair2["RHS"];
    ];

    AppendTo[content, Style["2. Riešenie vzniknutej sústavy 2×2", Bold]];
    If[twoCombosQ,
      AppendTo[content, "Dostali sme dve nové rovnice s dvoma neznámymi."],
      AppendTo[content, "Z jednej dvojice rovnic sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2."]
    ];
  ];

  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];

  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  AppendTo[content, alignedEquations[{
    {formatLHS[A2[[1, 1]], A2[[1, 2]], "", remVars], b2[[1]], ""},
    {formatLHS[A2[[2, 1]], A2[[2, 2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsOne2[A2, b2, remVars];
  If[sol2x2 === $Failed, Return[$Failed]];
  content = Join[content, sol2x2["Content"]];
  solMap = AssociationThread[remVars -> sol2x2["Solution"]];

  AppendTo[content, Style["3. Dosadenie do pôvodnej rovnice", Bold]];
  AppendTo[content, "Vypočítané premenné dosadíme napríklad do prvej rovnice a vypočítame poslednú neznámu."];

  finalVar = vars[[elimCol]];
  eqSubst = formatLHS3[A[[1, 1]], A[[1, 2]], A[[1, 3]], ""];
  subSteps = {};

  AppendTo[subSteps, {eqSubst, b[[1]], substNote[solMap, remVars, A[[1]], vars]}];
  AppendTo[subSteps, {formatSubstLHS3[A[[1]], vars, solMap, finalVar], b[[1]], ""}];

  Module[{row, rhs, coeffU, knownSum, rhsShifted, noteShift},
    row = A[[1]];
    rhs = b[[1]];
    coeffU = row[[elimCol]];

    knownSum = Together @ Total @ Table[If[i == elimCol, 0, row[[i]] * solMap[vars[[i]]]], {i, 1, Length[vars]}];
    rhsShifted = Together[rhs - knownSum];

    noteShift = Which[
      PossibleZeroQ[knownSum], "",
      TrueQ[knownSum > 0],      Row[{"- ", TraditionalForm[knownSum]}],
      True,                     Row[{"+ ", TraditionalForm[Abs[knownSum]]}]
    ];

    AppendTo[subSteps, {formatSubstLHS3Eval[A[[1]], vars, solMap, finalVar], b[[1]], noteShift}];

    If[coeffU === 1,
      AppendTo[subSteps, {TraditionalForm[finalVar], TraditionalForm[rhsShifted], ""}],
      AppendTo[subSteps, {TraditionalForm[coeffU finalVar], TraditionalForm[rhsShifted], ": " <> ToString[coeffU]}];
      AppendTo[subSteps, {TraditionalForm[finalVar], TraditionalForm[Together[rhsShifted/coeffU]], ""}];
    ];
    finalVal = Together[rhsShifted/coeffU];
  ];

  AppendTo[content, alignedEquations[subSteps]];
  AppendTo[content, highlightGrid[alignedEquations[{{finalVar, TraditionalForm[finalVal], ""}}]]];

  solFull = Table[If[i == elimCol, finalVal, solMap[vars[[i]]]], {i, 1, 3}];
  <|"Content" -> content, "Solution" -> solFull|>
];

stepsNone3[A_, b_, vars_] := Module[
  {
    content = {}, elimVarStr, elimCol,
    resPair1, resPair2, rowIV, rhsIV, rowV, rhsV,
    remCols, remVars, A2, b2, sol2x2,
    zeroRows, nonZeroRows, twoCombosQ, iKeep, pair, i1, i2
  },

  AppendTo[content, Style["1. Redukcia sústavy 3×3 na 2×2", Bold]];
  elimCol = analyzeElimination3[A];
  elimVarStr = vars[[elimCol]];
  AppendTo[content, "Vylúčime premennú " <> ToString[elimVarStr] <> ". Použijeme na to dve dvojice rovníc (alebo jednu kombináciu a jednu rovnicu, ktorá už túto premennú neobsahuje)."];

  (* Same logic for selecting combinations as in stepsOne3 *)
  zeroRows    = Flatten @ Position[A[[All, elimCol]], 0];
  nonZeroRows = Complement[Range[3], zeroRows];

  If[Length[zeroRows] >= 1 && Length[nonZeroRows] >= 2,
    twoCombosQ = False;
    iKeep = First[zeroRows];
    pair = pickBestElimPair[nonZeroRows, elimCol, A];
    {i1, i2} = pair;

    AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];
    resPair1 = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars, "I", "II"];
    content  = Join[content, resPair1["Content"]];
    rowIV    = resPair1["Row"]; rhsIV = resPair1["RHS"];
    rowV = A[[iKeep]]; rhsV = b[[iKeep]];

    AppendTo[content, Style["b) Rovnica bez vyrušovanej premennej (použijeme ju priamo):", Italic]];
    AppendTo[content, alignedEquations[{{formatLHS3[rowV[[1]], rowV[[2]], rowV[[3]], ""], rhsV, ""}}]];
    ,
    twoCombosQ = True;
    AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];
    resPair1 = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "I", "II"];
    content  = Join[content, resPair1["Content"]];
    rowIV    = resPair1["Row"]; rhsIV = resPair1["RHS"];
    AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];
    resPair2 = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "I", "III"];
    content  = Join[content, resPair2["Content"]];
    rowV     = resPair2["Row"]; rhsV = resPair2["RHS"];
  ];

  AppendTo[content, Style["2. Riešenie vzniknutej sústavy 2×2", Bold]];
  If[twoCombosQ,
    AppendTo[content, "Dostali sme dve nové rovnice s dvoma neznámymi. Ak po úpravách vznikne spor (napr. 0 = nenulové číslo), pôvodná sústava 3×3 nemá riešenie."],
    AppendTo[content, "Z jednej dvojice rovnic sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2. Ak v nej vznikne spor, pôvodná sústava 3×3 nemá riešenie."]
  ];

  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];
  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  AppendTo[content, alignedEquations[{
    {formatLHS[A2[[1, 1]], A2[[1, 2]], "", remVars], b2[[1]], ""},
    {formatLHS[A2[[2, 1]], A2[[2, 2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsNone2[A2, b2, remVars, False];

  If[sol2x2 === $Failed,
    AppendTo[content, "Po úpravách dostávame spornú rovnicu (napr. 0 = k). Preto sústava nemá riešenie."],
    content = Join[content, sol2x2["Content"]];
  ];

  AppendTo[content, Style["3. Záver", Bold]];
  AppendTo[content, "Keďže po eliminácii vznikol spor, pôvodná sústava 3×3 nemá riešenie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

stepsInfinite3[A_, b_, vars_] := Module[
  {
    content = {}, elimVarStr, elimCol,
    resPair1, resPair2, rowIV, rhsIV, rowV, rhsV,
    remCols, remVars, A2, b2, sol2x2,
    zeroRows, nonZeroRows, twoCombosQ, iKeep, pair, i1, i2
  },

  AppendTo[content, Style["1. Redukcia sústavy 3×3 na 2×2", Bold]];
  elimCol = analyzeElimination3[A];
  elimVarStr = vars[[elimCol]];

  AppendTo[content, "Vylúčime premennú " <> ToString[elimVarStr] <> ". Použijeme na to dve dvojice rovníc (alebo jednu kombináciu a jednu rovnicu, ktorá už túto premennú neobsahuje)."];

  (* Same logic for selecting combinations as in stepsOne3 *)
  zeroRows    = Flatten @ Position[A[[All, elimCol]], 0];
  nonZeroRows = Complement[Range[3], zeroRows];

  If[Length[zeroRows] >= 1 && Length[nonZeroRows] >= 2,
    twoCombosQ = False;
    iKeep = First[zeroRows];
    pair = pickBestElimPair[nonZeroRows, elimCol, A];
    {i1, i2} = pair;

    AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];
    resPair1 = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars, "I", "II"];
    content  = Join[content, resPair1["Content"]];
    rowIV    = resPair1["Row"]; rhsIV = resPair1["RHS"];
    rowV = A[[iKeep]]; rhsV = b[[iKeep]];

    AppendTo[content, Style["b) Rovnica bez vyrušovanej premennej (použijeme ju priamo):", Italic]];
    AppendTo[content, alignedEquations[{{formatLHS3[rowV[[1]], rowV[[2]], rowV[[3]], ""], rhsV, ""}}]];
    ,
    twoCombosQ = True;
    AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];
    resPair1 = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "I", "II"];
    content  = Join[content, resPair1["Content"]];
    rowIV    = resPair1["Row"]; rhsIV = resPair1["RHS"];
    AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];
    resPair2 = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "I", "III"];
    content  = Join[content, resPair2["Content"]];
    rowV     = resPair2["Row"]; rhsV = resPair2["RHS"];
  ];

  AppendTo[content, Style["2. Riešenie vzniknutej sústavy 2×2", Bold]];
  If[twoCombosQ,
    AppendTo[content, "Dostali sme dve nové rovnice s dvoma neznámymi. Ak po úpravách vyjde totožná rovnica (napr. 0 = 0), znamená to nekonečne veľa riešení."],
    AppendTo[content, "Z jednej dvojice rovnic sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2. Ak v nej vyjde totožná rovnica (0 = 0), sústava má nekonečne veľa riešení."]
  ];

  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];
  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  AppendTo[content, alignedEquations[{
    {formatLHS[A2[[1, 1]], A2[[1, 2]], "", remVars], b2[[1]], ""},
    {formatLHS[A2[[2, 1]], A2[[2, 2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsInfinite2[A2, b2, remVars, False];

  If[sol2x2 === $Failed,
    AppendTo[content, "Sústava má nekonečne veľa riešení."],
    content = Join[content, sol2x2["Content"]];
  ];

  AppendTo[content, Style["3. Záver", Bold]];
  AppendTo[content, "Keďže po eliminácii vyšla totožná rovnica, pôvodná sústava 3×3 má nekonečne veľa riešení."];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

stepsHard3[args___] := $Failed;


(* 2D Visualization (2x2) *)
visualize2[A_, b_, vars_, sol_] := Module[
  { x, y, pt, subtitle, xrange, yrange, lineSeg, seg1, seg2, g, col1, col2, legend1, legend2, center, half, labelOffset},

  {x, y} = vars;
  half = 10;
  labelOffset = {0, 1.3};

  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    xrange = center[[1]] + {-half, half};
    yrange = center[[2]] + {-half, half};
    xrange = {Min[xrange[[1]], 0], Max[xrange[[2]], 0]};
    yrange = {Min[yrange[[1]], 0], Max[yrange[[2]], 0]};

    subtitle = Row[{
      "V grafe sú zobrazené obe priamky. Ich priesečník je riešením sústavy a je vyznačený v bode ",
      "[", TraditionalForm[Together[pt[[1]]]], ", ",
      TraditionalForm[Together[pt[[2]]]], "]."
    }];,

    pt = None;
    center = {0, 0};
    xrange = {-10, 10};
    yrange = {-10, 10};

    subtitle = If[sol === "NONE",
      "Priamky sú rovnobežné, nepretínajú sa – sústava nemá riešenie.",
      "Priamky sú totožné (prekrývajú sa) – sústava má nekonečne veľa riešení."
    ];
  ];

  If[StringQ[subtitle], CellTextU[subtitle], CellTextExpr[subtitle]];

  lineSeg[{a_, bb_}, c_] := Module[{pA, pB},
    If[bb =!= 0,
      pA = {xrange[[1]], (c - a*xrange[[1]])/bb};
      pB = {xrange[[2]], (c - a*xrange[[2]])/bb};,
      pA = {c/a, yrange[[1]]};
      pB = {c/a, yrange[[2]]};
    ];
    Line[{pA, pB}]
  ];

  seg1 = lineSeg[A[[1]], b[[1]]];
  seg2 = lineSeg[A[[2]], b[[2]]];

  col1 = Magenta; col2 = Blue;
  legend1 = lineLegendText[A[[1, 1]], A[[1, 2]], b[[1]]];
  legend2 = lineLegendText[A[[2, 1]], A[[2, 2]], b[[2]]];

  g = Legended[
    Graphics[
      {
        If[sol === "INFINITE",
          {
            {col1, AbsoluteThickness[2], Opacity[0.95], seg1},
            {col2, AbsoluteThickness[2], Opacity[0.95], Dashing[{0.03, 0.03}], seg2}
          },
          {
            {col1, Thick, seg1},
            {col2, Thick, seg2}
          }
        ],

        If[pt =!= None,
          {
            {Black, Thick, Circle[pt, 0.4]},
            {Green, PointSize[0.02], Point[pt]},
            Inset[
              Style[
                Row[{"[", TraditionalForm[Together[pt[[1]]]], ", ", TraditionalForm[Together[pt[[2]]]], "]"}],
                14
              ],
              pt + labelOffset,
              Background -> Directive[White, Opacity[0.8]],
              FrameMargins -> {{6, 6}, {3, 3}}
            ]
          },
          {}
        ]
      },
      PlotRange -> {xrange, yrange},
      PlotRangeClipping -> True,
      GridLines -> Automatic,
      Axes -> True,
      ImageSize -> Medium,
      Method -> {"MouseInteraction" -> {"Rotate" -> False, "Pan" -> False, "Zoom" -> False}}
    ],
    Placed[LineLegend[{col1, col2}, {legend1, legend2}], After]
  ];

  CellBox @ g
];

(* 3D Visualization (3x3) - Non-interactive, fast rendering using ContourPlot3D for stability *)
visualize3[A_, b_, vars_, sol_] := Module[
  {x, y, z, range = 10, xmin, xmax, ymin, ymax, zmin, zmax,
    n1, n2, n3, d1, d2, d3, inter, subtitle, planes, mark, legendLabels},

  {x, y, z} = vars;
  {xmin, xmax} = {-range, range};
  {ymin, ymax} = {-range, range};
  {zmin, zmax} = {-range, range};

  n1 = N @ A[[1]]; d1 = N @ b[[1]];
  n2 = N @ A[[2]]; d2 = N @ b[[2]];
  n3 = N @ A[[3]]; d3 = N @ b[[3]];

  inter = systemIntersection3[A, b, vars];

  subtitle = Switch[inter["Type"],
    "POINT", "Tri roviny majú spoločný prienik v jednom bode (riešenie sústavy).",
    "LINE",  "Tri roviny majú spoločný prienik – priamku (nekonečne veľa riešení).",
    "PLANE", "Všetky tri rovnice opisujú tú istú rovinu (nekonečne veľa riešení).",
    "NONE",  "Roviny nemajú spoločný prienik všetkých troch naraz (sústava nemá riešenie).",
    _,       "Prienik sa nepodarilo jednoznačne určiť."
  ];

  CellTextU[subtitle];

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

  (* Highlight intersection based on type *)
  mark = Graphics3D @ Switch[inter["Type"],
    "POINT",
    {
      Black, PointSize[0.03], Point[N@inter["Point"]],
      Black, Sphere[N@inter["Point"], 0.15]
    },
    "LINE",
    Module[{p0, v},
      p0 = N@inter["Point"];
      v  = N@inter["Dir"];
      (* Draw a tube along the intersection line *)
      {
        Black, Specularity[White, 20],
        Tube[{p0 - 20 v, p0 + 20 v}, 0.1]
      }
    ],
    (* For NONE and PLANE, no specific point/line highlight *)
    _, {}
  ];

  (* Construct legend labels using 3D equation formatting *)
  legendLabels = Table[planeLegendText[A[[i]], b[[i]], vars], {i, 1, 3}];

  CellBox @ Legended[
    Show[
      planes,
      mark,
      PlotRange -> {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}},
      BoxRatios -> {1, 1, 1},
      Axes -> True,
      AxesLabel -> {"x", "y", "z"},
      SphericalRegion -> True,
      ImageSize -> 400,
      Lighting -> "Neutral",
      ViewAngle -> 35 Degree,
      ViewPoint -> {2.2, -2.0, 1.4},
      Method -> {"MouseInteraction" -> {"Rotate" -> False, "Pan" -> False, "Zoom" -> False}}
    ],
    Placed[
      SwatchLegend[{Cyan, Magenta, Yellow}, legendLabels, LegendFunction -> "Frame"],
      After
    ]
  ];
];

(* --- Main Controller --- *)
(* Orchestrates the generation pipeline: Args -> Dim -> Generator -> Solver -> Output *)
Gen01[diff_String, mode_String, opts : OptionsPattern[]] :=
    Module[{dim, vars, st, gen, data, A, b, steps, sol},

      If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff],
        Message[Gen01::baddiff, diff]; Return[$Failed]
      ];
      If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode],
        Message[Gen01::badmode, mode]; Return[$Failed]
      ];

      If[diff === "HARD", Message[Gen01::notimpl, diff]; Return[$Failed]];

      st = Replace[OptionValue[SolutionType], {
        Automatic -> RandomChoice[{"ONE", "ONE", "ONE", "ONE", "NONE", "INFINITE"}],
        s_ :> s
      }];

      dim = Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3];
      vars = Take[{x, y, z}, dim];

      gen := Which[
        dim == 2 && st == "ONE",      generateSystemOne[2, diff],
        dim == 2 && st == "NONE",     generateSystemNone2[diff],
        dim == 2 && st == "INFINITE", generateSystemInfinite2[diff],

        dim == 3 && st == "ONE",      generateSystemOne[3, diff],
        dim == 3 && st == "NONE",     generateSystemNone3[diff],
        dim == 3 && st == "INFINITE", generateSystemInfinite3[diff],

        True, $Failed
      ];

      data = WithRetries[Function[Null, gen], 200];
      If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

      A = data["A"]; b = data["b"];

      CellSection["Sčítavacia (eliminačná) metóda"];
      CellSubsection["Zadanie"];

      CellTextU["Vyriešte nasledujúcu sústavu lineárnych rovníc sčítacou (eliminačnou) metódou."];

      If[dim == 2,
        CellBox @ alignedEquations[Table[{formatLHS[A[[i, 1]], A[[i, 2]], "", vars], b[[i]], ""}, {i, Length[b]}]],
        CellBox @ alignedEquations[Table[{formatLHS3[A[[i, 1]], A[[i, 2]], A[[i, 3]], ""], b[[i]], ""}, {i, Length[b]}]]
      ];

      If[mode === "TASK", Return[<|"A" -> A, "b" -> b, "vars" -> vars|>]];

      steps = Which[
        dim == 2 && data["type"] == "ONE",      stepsOne2[A, b, vars],
        dim == 2 && data["type"] == "NONE",     stepsNone2[A, b, vars],
        dim == 2 && data["type"] == "INFINITE", stepsInfinite2[A, b, vars],

        dim == 3 && data["type"] == "ONE",      stepsOne3[A, b, vars],
        dim == 3 && data["type"] == "NONE",     stepsNone3[A, b, vars],
        dim == 3 && data["type"] == "INFINITE", stepsInfinite3[A, b, vars],

        True, $Failed
      ];

      If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];

      sol = steps["Solution"];

      If[mode === "TASK_STEPS_RESULT",
        CellSubsection["Postup"];
        Scan[renderItem, steps["Content"]];
      ];

      CellSubsection["Výsledok"];

      Switch[sol,
        "NONE",
        CellTextU["Sústava nemá riešenie (pri sčítaní vznikol spor)."],

        "INFINITE",
        CellTextU["Sústava má nekonečne veľa riešení. Riešenia zapíšeme pomocou parametra."];

        If[dim == 2,
          Module[
            {par, exprX, exprY, a1 = A[[1, 1]], b1 = A[[1, 2]], c1 = b[[1]], baseEq, solvedEq},
            par = \[FormalT];

            CellTextU["Vyjadríme jednu premennú z jednej rovnice (napr. y vyjadríme pomocou x)."];

            If[b1 =!= 0,
              baseEq    = a1 x + b1 y;
              solvedEq = Simplify[(c1 - a1 x)/b1];

              CellBox @ alignedEquations[{{baseEq, c1, ""}}];
              CellBox @ alignedEquations[{{y, solvedEq, ""}}];

              CellTextU["Zvolíme parameter (voľná hodnota):"];
              CellBox @ Grid[{{x, "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}}, Alignment -> {{Right, Center, Left, Center, Left, Left}}, Spacings -> {0.6, 0.8}];

              CellTextU["Dosadíme parameter a dostaneme tvar pre druhú premennú:"];
              CellBox @ alignedEquations[{{y, Simplify[solvedEq /. x -> par], ""}}];
              ,
              baseEq    = a1 x;
              solvedEq = Simplify[c1/a1];

              CellBox @ alignedEquations[{{baseEq, c1, ""}}];
              CellBox @ alignedEquations[{{x, solvedEq, ""}}];

              CellTextU["Zvolíme parameter (voľná hodnota):"];
              CellBox @ Grid[{{y, "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}}, Alignment -> {{Right, Center, Left, Center, Left, Left}}, Spacings -> {0.6, 0.8}];
              CellTextU["V tomto prípade vyšlo x ako konštanta a premenná y môže byť ľubovoľná (parameter)."];
            ];

            If[b1 =!= 0,
              exprX = par;
              exprY = Simplify[(c1 - a1*par)/b1];
              ,
              exprY = par;
              exprX = Simplify[c1/a1];
            ];

            CellBox @ Grid[{{x, "=", TraditionalForm[exprX]}, {y, "=", TraditionalForm[exprY]}}, Alignment -> {{Right, Center, Left}}, Spacings -> {0.6, 0.8}];
            CellPrint @ Cell[
              BoxData @ FormBox[
                RowBox[{
                  StyleBox["K", FontSlant -> "Italic"], "=",
                  RowBox[{"{", RowBox[{RowBox[{"[", RowBox[{ToBoxes[exprX, TraditionalForm], ";", " ", ToBoxes[exprY, TraditionalForm]}], "]"}], " ", "\[VerticalSeparator]", " ", RowBox[{ToBoxes[par, TraditionalForm], "\[Element]", "\[DoubleStruckR]"}]}], "}"}]
                }], TraditionalForm],
              "DisplayFormula", BaseStyle -> {FontSize -> 14}
            ];
          ];
        ];

        If[dim == 3, printInfiniteResult3[A, b, vars]];,

        _,
        CellPrint @ Cell[
          BoxData @ ToBoxes[
            If[dim == 2,
              Row[{"Riešením sústavy rovníc je usporiadaná dvojica čísel ", Style[Row[{"[", TraditionalForm[Together[sol[[1]]]], ", ", TraditionalForm[Together[sol[[2]]]], "]"}], Bold]}],
              Row[{"Riešením sústavy rovníc je usporiadaná trojica čísel ", Style[Row[{"[", TraditionalForm[Together[sol[[1]]]], ", ", TraditionalForm[Together[sol[[2]]]], ", ", TraditionalForm[Together[sol[[3]]]], "]"}], Bold]}]
            ],
            TraditionalForm
          ],
          "Text", ShowStringCharacters -> False
        ];
      ];

      If[OptionValue[Visualization],
        If[dim == 2, visualize2[A, b, vars, sol]];
        If[dim == 3, visualize3[A, b, vars, sol]];
      ];
      Null
    ];
End[];
EndPackage[];