(* ::Package:: *)

(*
  Package: SubstitutionMatrixGenerator
  Description: Generates didactic materials for solving linear systems via the substitution method.
  Derived from EliminationMatrixGenerator to maintain structural and pedagogical consistency.
*)

BeginPackage["MojeGeneratory`SubstitutionMatrixGenerator`", "MojeGeneratory`Common`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

Gen01::usage = "Gen01[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc dosadzovacou (substitučnou) metódou.

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

(* rovnaké ako buildTermsRow, ale NEZLUČUJE rovnaké premenné – umožní 5x - 4x - 12 *)
buildTermsRowNoCombine[terms_List] := Module[{pairs, out = {}, first = True, c, v, t},
  pairs = Select[terms, MatchQ[#, {_, _}] &];

  If[pairs === {} || AllTrue[pairs[[All, 1]], PossibleZeroQ[#] &], Return[tf[0]]];

  Do[
    {c, v} = pairs[[i]];
    If[PossibleZeroQ[c], Continue[]];

    t = If[v === None, tf[Abs[c]], tf[If[Abs[c] === 1, v, Abs[c] v]]];

    If[first,
      If[TrueQ[c < 0], out = Join[out, {"-", t}], out = Join[out, {t}]];
      first = False;,
      out = Join[out, {If[TrueQ[c < 0], " - ", " + "], t}];
    ];
    , {i, 1, Length[pairs]}];

  If[out === {}, tf[0], Row[out]]
];

(* zoradí termy podľa poradia vars; zachová poradie v rámci rovnakej premennej *)
orderTermsByVars[terms_List, vars_List] := Module[{pairs, varOrder, key},
  pairs = Select[terms, MatchQ[#, {_, _}] &];

  varOrder = AssociationThread[vars -> Range[Length[vars]]];

  key[t_] := Which[
    t[[2]] === None, Infinity,
    KeyExistsQ[varOrder, t[[2]]], varOrder[t[[2]]],
    True, Infinity
  ];

  (* stabilné triedenie podľa poradia premenných; v rámci rovnakej premennej zachová vstupné poradie *)
  SortBy[pairs, key]
];


(* rozklad lineárneho výrazu na koeficienty pri premenných + konštantu *)
linearDecompose[expr_, vrs_List] := Module[{ee, coeffs, c0},
  ee = Expand[Together[expr]];
  coeffs = Together /@ (Coefficient[ee, #] & /@ vrs);
  c0 = Together[ee /. (Rule[#, 0] & /@ vrs)];
  {coeffs, c0}
];

(* naformátuje lineárny výraz tak, aby boli premenné v poradí vrs a konštanta na konci *)
formatLinearExpr[expr_, vrs_List] := Module[{coeffs, c0, terms = {}, k},
  {coeffs, c0} = linearDecompose[expr, vrs];

  Do[
    If[!PossibleZeroQ[coeffs[[k]]], AppendTo[terms, {coeffs[[k]], vrs[[k]]}]];
    , {k, 1, Length[vrs]}];

  If[!PossibleZeroQ[c0], AppendTo[terms, {c0, None}]];

  buildTermsRow[terms]
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

(* UPDATED: zátvorky pre zápornú hodnotu A VÝRAZY *)
wrapNegValue[val_] := Which[
  NumericQ[val] && val < 0, Row[{"(", tft[val], ")"}],
  Head[val] === Plus, Row[{"(", tft[val], ")"}], (* Wrap sums e.g. (1 - 2y) *)
  MatchQ[val, Times[c_?NumericQ, __] /; c < 0], Row[{"(", tft[val], ")"}], (* Wrap negative products like -2y *)
  MatchQ[val, Times[-1, __]], Row[{"(", tft[val], ")"}], (* Wrap -x *)
  MatchQ[val, _Rational] && val < 0, Row[{"(", tft[val], ")"}],
  True, tft[val]
];

(* UPDATED: koeficient · hodnota pre dosadzovanie (handles expressions) *)
coeffVal[coeff_, val_] := Which[
  coeff === 0, 0,
  coeff === 1, wrapNegValue[val],
  coeff === -1, Row[{"-", wrapNegValue[val]}],
  True, Row[{tft[coeff], " \[CenterDot] ", wrapNegValue[val]}]
];


(* UPDATED: formátovanie ľavej strany pre substitúciu (handles symbolic solMap) *)
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
            (* Numeric/Evaluation mode *)
            With[{prod = Together[c * solMap[v]]},
              If[!PossibleZeroQ[prod], addTerm[tf[Abs[prod]], Sign[prod]]]
            ],
            (* Symbolic Substitution mode *)
            (* Use coeffVal to format c * (expression) *)
            addTerm[coeffVal[Abs[c], solMap[v]], Sign[c]]
          ]
        ]
      ]
    ],
    {i, 1, Length[vars]}
  ];

  If[terms === {}, tf[0], Row[terms]]
];

(* dosadenie len jednej premennej do rovnice: ostatné premenné nechá symbolicky *)
formatSubstOnceLHS[row_, vars_, targetVar_, substExpr_] := Module[
  {terms = {}, first = True, addTerm},

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
        If[v === targetVar,
          (* tu chceme 2·(expr), -3·(expr), ... *)
          addTerm[coeffVal[Abs[c], substExpr], Sign[c]],
          (* ostatné premenné normálne: 5x, 4y, ... *)
          addTerm[tf[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]]
        ]
      ]
    ],
    {i, 1, Length[vars]}
  ];

  If[terms === {}, tf[0], Row[terms]]
];


(* text znamienka medzi členmi *)
signBtwTerms[c_] := If[c < 0, " - ", " + "];

(* notes pre [ + k / - k / · k / : k ] *)
addNote[k_] := Which[
  PossibleZeroQ[k], "",
  TrueQ[k > 0], Row[{"+ ", tft[k]}],
  TrueQ[k < 0], Row[{"- ", tft[Abs[k]]}],
  (* Heuristic for symbolic negatives like -2y or -x to avoid "+ -2y" *)
  MatchQ[k, Times[c_?NumericQ, __] /; c < 0], Row[{tft[k]}],
  MatchQ[k, Times[-1, __]], Row[{tft[k]}],
  (* Heuristic for sums starting with negative like -2x - 3y *)
  MatchQ[k, Plus[c_, __]] && (TrueQ[c < 0] || (MatchQ[c, Times[n_, __]] && TrueQ[n < 0])), Row[{tft[k]}],
  True, Row[{"+ ", tft[k]}]
];
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

(* vyberie náhodne multiplikátory pre HARD *)
pickHardMultipliers15[n_] := Module[{ks},
  ks = RandomChoice[Range[5], n];
  While[Max[ks] < 4, ks = RandomChoice[Range[5], n]];
  ks
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

  (* 1/q * (...) -> (...) / q *)
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
  nzPool = Join[-Range[r], Range[r]];
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

  (* aspoň jedna jednotka v matici A - pre dosadzovanie bez zlomku *)
  oneCoeffQ[m_] := MemberQ[Flatten[m], 1] || MemberQ[Flatten[m], -1];

  attemptLimit = 5000;

  okQ[] := zerosOkQ[A] && fullRankQ[A] && niceOkQ[A, b] && oneCoeffQ[A];

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

    If[zerosOkQ[A] && niceOkQ[A, b] && oneCoeffQ[A], Break[]];
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

(* ~-~-~ SUBSTITUTION CORE LOGIC ~-~-~ *)

(* 5.1 Choosing what to solve for *)
pickSubstSolve2[A_, b_, vars_] := Module[{scores},
  scores = Table[
    With[{c = A[[i, j]]},
      If[c == 0, Infinity,
        If[Abs[c] == 1, 0, Abs[c] + 10]
      ]
    ], {i, 2}, {j, 2}
  ];
  First @ Position[scores, Min[scores]] (* returns {rowIdx, colIdx} *)
];

pickSubstSolve3[A_, b_, vars_] := Module[{scores},
  scores = Table[
    With[{c = A[[i, j]]},
      If[c == 0, Infinity,
        (* Prefer +/- 1, penalize if row has many terms *)
        If[Abs[c] == 1, 0 + Count[A[[i]], 0]*(-1), Abs[c] + 100]
      ]
    ], {i, 3}, {j, 3}
  ];
  First @ Position[scores, Min[scores]]
];

(* 5.2 Solving one equation for a variable *)
solveForVarSteps[row_, rhs_, vars_, varIndex_] := Module[
  {content = {}, targetVar, c, otherTerms, rhsExpr, solExpr, stepsIso, moveNote, divNoteVal, currentLHS, isoLHS},

  targetVar = vars[[varIndex]];
  c = row[[varIndex]];

  (* Terms to move *)
  otherTerms = Delete[row, varIndex] . Delete[vars, varIndex];

  stepsIso = {};

  (* 1. Pôvodná rovnica + Note o presune *)
  currentLHS = formatEquationLHS[row, "", vars];
  moveNote = If[PossibleZeroQ[otherTerms], "", addNote[-otherTerms]];

  (* Ak netreba nic presuvat (např. 2x = 4), poznámka bude o delení (ak treba deliť) *)
  If[PossibleZeroQ[otherTerms],
    divNoteVal = Which[
      c == 1, "",
      c == -1, multNote[-1],
      True, divNote[c]
    ];
    AppendTo[stepsIso, {currentLHS, tf[rhs], divNoteVal}];
    ,
    (* Ak presúvame členy *)
    AppendTo[stepsIso, {currentLHS, tf[rhs], moveNote}];
  ];

  (* 2. Stav po presune (izolovaný člen) *)
  rhsExpr = rhs - otherTerms;
  isoLHS = tf[If[Abs[c] == 1 && c > 0, targetVar, If[Abs[c] == 1, -targetVar, c targetVar]]];

  (* Ak sme presúvali členy, zobrazíme tento medzikrok *)
  If[!PossibleZeroQ[otherTerms],
    divNoteVal = Which[
      c == 1, "",
      c == -1, multNote[-1],
      True, divNote[c]
    ];
    (* Ak je koeficient 1, medzikrok je zároveň výsledok, takže ho nepridáme sem, ale až v závere *)
    If[c =!= 1,
      AppendTo[stepsIso, {isoLHS, tf[rhsExpr], divNoteVal}]
    ];
  ];

  (* 3. Finálny výsledok (delenie) *)
  solExpr = Expand[Together[rhsExpr / c]];

  (* Pridáme finálny riadok, iba ak sa líši od predchádzajúceho (napr. ak c=1 a už sme vypísali isoLHS) *)
  (* Alebo ak sme mali 2x=4, tak prvý riadok bol 2x=4 |:2 a tento bude x=2. *)
  (* Ak sme mali x+y=5, prvý bol x+y=5 |-y, druhý by bol x=5-y. *)

  If[!(PossibleZeroQ[otherTerms] && c == 1),
    AppendTo[stepsIso, {tf[targetVar], formatLinearExpr[solExpr, DeleteCases[vars, targetVar]], ""}]
  ];


  <|"Content" -> stepsIso, "Rule" -> (targetVar -> solExpr), "Expr" -> solExpr, "Var" -> targetVar|>
];

(* 5.3 Substitution into another equation *)
substituteIntoEquationSteps[row_, rhs_, vars_, rule_, remainingVars_] := Module[
  {targetVar, substExpr, stepRows, currentLHS, sNote,
    pos, targetCoeff, baseTerms, subCoeffs, subConst, distTerms,
    lhsCombined, newRow, constLeft, newRHS},

  targetVar = rule[[1]];
  substExpr = rule[[2]];
  stepRows = {};

  (* 1. pôvodná rovnica + poznámka o substitúcii *)
  currentLHS = formatEquationLHS[row, "", vars];
  sNote = substNote[<|targetVar -> substExpr|>, {targetVar}, row, vars];
  AppendTo[stepRows, {currentLHS, tf[rhs], sNote}];

  (* koeficient dosadzovanej premennej *)
  pos = First@First@Position[vars, targetVar];
  targetCoeff = row[[pos]];

  (* pôvodné členy okrem targetVar *)
  baseTerms = Select[
    Delete[MapThread[List, {row, vars}], pos],
    #[[1]] =!= 0 &
  ];

  (* rozklad dosadzovaného výrazu na koeficienty remainingVars + konštantu *)
  {subCoeffs, subConst} = linearDecompose[substExpr, remainingVars];

  (* roznásobené členy za targetVar (bez zlučovania rovnakých premenných) *)
  distTerms = Join[
    DeleteCases[
      Table[
        If[PossibleZeroQ[subCoeffs[[k]]], Nothing, {targetCoeff*subCoeffs[[k]], remainingVars[[k]]}],
        {k, 1, Length[remainingVars]}
      ],
      Nothing
    ],
    If[PossibleZeroQ[subConst], {}, {{targetCoeff*subConst, None}}],
    baseTerms
  ];

  (* 2. roznásobenie – iba ak |koef| != 1 *)
  If[Abs[targetCoeff] =!= 1,
    AppendTo[stepRows, {
      formatSubstOnceLHS[row, vars, targetVar, substExpr],
      tf[rhs],
      "roznásobenie"
    }]
  ];

  (* 3. zlučovanie členov – vypíšeme roznásobené členy + pôvodné členy v poradí premenných *)
  AppendTo[stepRows, {
    buildTermsRowNoCombine[orderTermsByVars[distTerms, vars]],
    tf[rhs],
    "zlučovanie členov"
  }];

  (* 4. finálny štandardný tvar (jediný výstupný riadok pre novú rovnicu) *)
  lhsCombined = Expand[row.vars /. rule];
  newRow = Coefficient[lhsCombined, #] & /@ remainingVars;
  constLeft = lhsCombined /. (Rule[#, 0] & /@ remainingVars);
  newRHS = rhs - constLeft;

  AppendTo[stepRows, {
    formatEquationLHS[newRow, "", remainingVars],
    tf[newRHS],
    ""
  }];


  <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>
];

(* 5.4 Reducing 3x3 to 2x2 by substitution *)
reduce3to2BySubstitution[A_, b_, vars_] := Module[
  {content = {}, rI, cI, solveData, eqIdx, varIdx, elimVar, expr, substRule,
    otherRowsIdx, subSteps, A2, b2, remVars, remCols},

  {rI, cI} = pickSubstSolve3[A, b, vars];
  elimVar = vars[[cI]];

  AppendTo[content, makeStepHeader["Vyjadrenie neznámej z jednej rovnice"]];
  AppendTo[content, "Vyberieme si " <> ToString[rI] <> ". rovnicu a vyjadríme z nej neznámu " <> ToString[elimVar] <> "."];

  (* UPDATED: Removed explicit equation print here, handled inside solveForVarSteps *)

  (* Perform isolation *)
  solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
  AppendTo[content, highlightGrid[alignedEquations[solveData["Content"]]]];

  substRule = solveData["Rule"];
  expr = solveData["Expr"];

  AppendTo[content, makeStepHeader["Dosadenie do zvyšných rovníc"]];
  AppendTo[content, "Získaný výraz dosadíme do ostatných dvoch rovníc a upravíme ich."];

  otherRowsIdx = Delete[Range[3], rI];
  remCols = Delete[Range[3], cI];
  remVars = vars[[remCols]];
  A2 = {}; b2 = {};

  Do[
    idx = otherRowsIdx[[k]];
    AppendTo[content, Style["Dosadenie do " <> ToString[idx] <> ". rovnice:", Italic]];

    (* UPDATED: substituteIntoEquationSteps now prints the original row first *)
    With[{res = substituteIntoEquationSteps[A[[idx]], b[[idx]], vars, substRule, remVars]},
      AppendTo[content, alignedEquations[res["Content"]]];
      AppendTo[A2, res["NewEq"][[1]]];
      AppendTo[b2, res["NewEq"][[2]]];
    ];
    , {k, 1, 2}];

  <|"Content" -> content, "A2" -> A2, "b2" -> b2, "remVars" -> remVars,
    "elimVar" -> elimVar, "substRule" -> substRule, "elimCol" -> cI, "sourceRow" -> rI|>
];

(* ~-~-~ MAIN STEPS FUNCTIONS ~-~-~ *)

(* 2x2 SUBSTITUTION STEPS *)
stepsOne2[A_, b_, vars_] := Module[
  {content = {}, rI, cI, solveData, substRule, elimVar, remIdx, remVar,
    subSteps, newRow, newRHS, valRem, valElim, finalSol, filledExpr,
    res, resRows, coeff},

  (* 1. Pick and Solve *)
  {rI, cI} = pickSubstSolve2[A, b, vars];
  elimVar = vars[[cI]];
  remIdx = 3 - cI;
  remVar = vars[[remIdx]];

  AppendTo[content, makeStepHeader["Vyjadrenie neznámej"]];
  AppendTo[content, "Z " <> ToString[rI] <> ". rovnice vyjadríme neznámu " <> ToString[elimVar] <> "."];

  (* UPDATED: Removed explicit equation print here, handled inside solveForVarSteps *)

  solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
  AppendTo[content, highlightGrid[alignedEquations[solveData["Content"]]]];
  substRule = solveData["Rule"];

  (* 2. Substitute *)
  AppendTo[content, makeStepHeader["Dosadenie"]];
  AppendTo[content, "Výraz dosadíme do druhej rovnice."];

  res = substituteIntoEquationSteps[A[[3-rI]], b[[3-rI]], vars, substRule, {remVar}];
  resRows = res["Content"];
  {newRow, newRHS} = res["NewEq"];

  (* 3. Solve remaining linear eq *)
  (* newRow is like {coeff} for remVar *)
  If[PossibleZeroQ[newRow[[1]]],
    Return[$Failed]
  ];

  AppendTo[content, "Vypočítame hodnotu " <> ToString[remVar] <> "."];

  coeff = newRow[[1]];
  valRem = Together[newRHS / coeff];

  If[coeff =!= 1,
    (* Ak treba deliť, pridáme note k poslednému riadku substitúcie *)
    resRows[[-1, 3]] = divNote[coeff]; (* Update last row note *)
    AppendTo[content, alignedEquations[resRows]];
    (* A potom vypíšeme už len výsledok *)
    AppendTo[content, highlightGrid[alignedEquations[{{remVar, tf[valRem], ""}}]]];
    ,
    (* Ak je koeficient 1, výsledok je už v substitúcii *)
    AppendTo[content, highlightGrid[alignedEquations[resRows]]];
  ];

  (* 4. Back substitution - UPDATED: with numerical fill-in step *)
  AppendTo[content, makeStepHeader["Dopočítanie druhej neznámej"]];
  AppendTo[content, "Dosadíme vypočítanú hodnotu do výrazu pre prvú neznámu."];

  filledExpr = solveData["Expr"] /. remVar -> valRem; (* Numeric fill *)

  AppendTo[content, alignedEquations[{
    {elimVar, tf[solveData["Expr"]], substNote[<|remVar -> valRem|>, {remVar}, {0,0}, {remVar}]},
    {elimVar, tf[filledExpr], ""}, (* Show e.g. 5 - 2(3) *)
    {elimVar, tf[Together[filledExpr]], ""} (* Show e.g. -1 *)
  }]];
  valElim = Together[filledExpr];
  AppendTo[content, highlightGrid[alignedEquations[{{elimVar, tf[valElim], ""}}]]];

  finalSol = If[cI == 1, {valElim, valRem}, {valRem, valElim}];

  (* Skúška *)
  content = addCorrectnessCheck[content, A, b, vars, finalSol];

  <|"Content" -> content, "Solution" -> finalSol|>
];

(* Singular logic for 2x2 *)
stepsSingular2[A_, b_, vars_, kind_String, includeConclusion_: True] := Module[
  {content = {}, rI, cI, solveData, substRule, elimVar, remIdx, remVar,
    res, newRow, newRHS, introText, conclText},

  {rI, cI} = pickSubstSolve2[A, b, vars];
  elimVar = vars[[cI]];
  remVar = vars[[3-cI]];

  AppendTo[content, makeStepHeader["Vyjadrenie neznámej"]];
  AppendTo[content, "Z " <> ToString[rI] <> ". rovnice vyjadríme neznámu " <> ToString[elimVar] <> "."];

  solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
  AppendTo[content, alignedEquations[solveData["Content"]]];
  substRule = solveData["Rule"];

  AppendTo[content, makeStepHeader["Dosadenie"]];
  introText = Switch[kind,
    "NONE", "Dosadením do druhej rovnice overíme konzistenciu. Ak vznikne nepravdivá rovnosť 0 ≠ k, sústava nemá riešenie.",
    "INFINITE", "Dosadením do druhej rovnice overíme závislosť. Ak vznikne pravdivá rovnosť 0 = 0, sústava má nekonečne veľa riešení.",
    _, "Dosadíme výraz do druhej rovnice."
  ];
  AppendTo[content, introText];

  res = substituteIntoEquationSteps[A[[3-rI]], b[[3-rI]], vars, substRule, {remVar}];
  AppendTo[content, alignedEquations[res["Content"]]];
  {newRow, newRHS} = res["NewEq"];

  (* Result should be 0 = k *)
  (* Ak posledný riadok už je 0=k, nevypisujeme ho znova *)
  If[res["Content"][[-1, 1]] =!= tf[0],
    AppendTo[content, alignedEquations[{{0, tf[newRHS], ""}}]];
  ];

  If[includeConclusion,
    AppendTo[content, makeStepHeader["Záver"]];
    conclText = Switch[kind,
      "NONE",
      "Sústava nemá riešenie, pretože sme dostali nepravdivú rovnosť 0 ≠ " <> ToString[tf[newRHS]] <> ".",
      "INFINITE",
      "Sústava má nekonečne veľa riešení, pretože sme dostali pravdivú rovnosť 0 = 0.",
      _, ""
    ];
    AppendTo[content, conclText];
  ];

  <|"Content" -> content, "Solution" -> kind|>
];

stepsNone2[A_, b_, vars_, includeConclusion_: True] := stepsSingular2[A, b, vars, "NONE", includeConclusion];
stepsInfinite2[A_, b_, vars_, includeConclusion_: True] := stepsSingular2[A, b, vars, "INFINITE", includeConclusion];


(* 3x3 SUBSTITUTION STEPS *)
stepsOne3[A_, b_, vars_, data_:<||>] := Module[
  {red, content = {}, A2, b2, remVars, sol2x2, solMap, finalVar, finalVal,
    substExpr, subSteps, elimVar, valElim, filledExpr},

  (* Hard Normalization *)
  If[TrueQ[data["HardQ"]], content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  (* Reduce 3x3 -> 2x2 *)
  red = reduce3to2BySubstitution[A, b, vars];
  content = Join[content, red["Content"]];
  {A2, b2, remVars} = {red["A2"], red["b2"], red["remVars"]};
  elimVar = red["elimVar"];
  substExpr = red["substRule"][[2]];

  AppendTo[content, makeStepHeader["Riešenie odvodenej sústavy 2×2"]];
  AppendTo[content, "Dostali sme sústavu dvoch rovníc s dvoma neznámymi. Vyriešime ju (opäť môžeme použiť substitúciu alebo sčítaciu metódu - tu pokračujeme substitúciou)."];

  (* Tu len vypíšeme systém, riešenie voláme potom *)
  AppendTo[content, alignedEquations[{
    {formatEquationLHS[A2[[1]], "", remVars], b2[[1]], ""},
    {formatEquationLHS[A2[[2]], "", remVars], b2[[2]], ""}
  }]];

  (* Recursively call stepsOne2 for the reduced system *)
  (* stepsOne2 works on generic A, b, vars *)
  sol2x2 = stepsOne2[A2, b2, remVars];
  If[sol2x2 === $Failed, Return[$Failed]];

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

  solMap = AssociationThread[remVars -> sol2x2["Solution"]];

  AppendTo[content, makeStepHeader["Dopočítanie poslednej neznámej"]];
  AppendTo[content, "Vypočítané hodnoty dosadíme do výrazu pre prvú neznámu (" <> ToString[elimVar] <> "), ktorý sme si vyjadrili na začiatku."];

  (* UPDATED: with numerical fill-in step *)
  filledExpr = substExpr /. solMap;

  AppendTo[content, alignedEquations[{
    {elimVar, tf[substExpr], substNote[solMap, remVars, {0,0,0}, remVars]}, (* Trigger note *)
    {elimVar, tf[filledExpr], ""},
    {elimVar, tf[Together[filledExpr]], ""}
  }]];
  valElim = Together[filledExpr];
  AppendTo[content, highlightGrid[alignedEquations[{{elimVar, tf[valElim], ""}}]]];

  (* Full Check *)
  (* Construct full solution vector *)
  (* We need to map elimVar and remVars to their positions in original vars {x,y,z} *)
  finalVal = Table[
    If[vars[[i]] === elimVar, valElim, solMap[vars[[i]]]],
    {i, 1, 3}
  ];

  content = addCorrectnessCheck[content, A, b, vars, finalVal];

  <|"Content" -> content, "Solution" -> finalVal|>
];

stepsSingular3[A_, b_, vars_, kind_String, data_:<||>] := Module[
  {red, content = {}, A2, b2, remVars, sol2x2, conclText},

  If[TrueQ[data["HardQ"]], content = Join[content, hardNormalizationSteps3[A, b, vars, data]]];

  red = reduce3to2BySubstitution[A, b, vars];
  content = Join[content, red["Content"]];
  {A2, b2, remVars} = {red["A2"], red["b2"], red["remVars"]};

  AppendTo[content, makeStepHeader["Riešenie odvodenej sústavy 2×2"]];
  AppendTo[content, "Riešime odvodenú sústavu rovníc."];
  AppendTo[content, alignedEquations[{
    {formatEquationLHS[A2[[1]], "", remVars], b2[[1]], ""},
    {formatEquationLHS[A2[[2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsSingular2[A2, b2, remVars, kind, False];

  If[sol2x2 === $Failed,
    (* If 2x2 didn't fail in standard way but math failed, add generic msg *)
    AppendTo[content, "Pri riešení sústavy vznikol spor alebo identita."];,
    content = Join[content, sol2x2["Content"]];
  ];

  AppendTo[content, makeStepHeader["Záver"]];
  conclText = Switch[kind,
    "NONE", "Keďže pri riešení vznikol spor (nepravdivá rovnosť), pôvodná sústava 3×3 nemá riešenie.",
    "INFINITE", "Keďže pri riešení vznikla pravdivá rovnosť (identita) a premenné sa vyrušili, pôvodná sústava 3×3 má nekonečne veľa riešení.",
    _, "Záver."
  ];
  AppendTo[content, conclText];

  <|"Content" -> content, "Solution" -> kind|>
];

stepsNone3[A_, b_, vars_, data_:<||>] := stepsSingular3[A, b, vars, "NONE", data];
stepsInfinite3[A_, b_, vars_, data_:<||>] := stepsSingular3[A, b, vars, "INFINITE", data];


(* ~-~-~ GRAPHICAL VISUALIZATIONS ~-~-~ *)
visualize2[A_, b_, vars_, sol_] := Module[
  {x, y, pt, xrange, yrange, seg, center, subtitle, range = 10,
    lineStyles, lineLabels, extraLegStyles, extraLegLabels, legend},

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

  printTextExprCell[subtitle];

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

(* ~-~-~ HARD NORMALIZATION ~-~-~ *)
hardNormalizationSteps3[A_, b_, vars_, data_Association] := Module[
  {content = {}, ks, leftBase, rightBase, k, lMult, rMult,
    coeffL, coeffR, constL, constR, addTerms, addNote,
    rowStd, rhsStd, rowsStd, rhsStdAll, gcds, rowDiv, rhsDiv,
    rowsFinal, rhsFinal, anyDivQ},

  termsToCoeffsConst[terms_List] := Module[{cVar, cConst},
    cVar = Table[Total @ Cases[terms, {c_, vars[[j]]} :> c], {j, 1, Length[vars]}];
    cConst = Total @ Cases[terms, {c_, None} :> c];
    {cVar, cConst}
  ];

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

  rowsStd = ConstantArray[0, {3, 3}];
  rhsStdAll = ConstantArray[0, 3];

  AppendTo[content,
    alignedEquations@Table[
      k = ks[[i]];
      lMult = scaleTerms[leftBase[[i]], k];
      rMult = scaleTerms[rightBase[[i]], k];

      {coeffL, constL} = termsToCoeffsConst[lMult];
      {coeffR, constR} = termsToCoeffsConst[rMult];

      addTerms = Join[
        If[constL =!= 0, {{-constL, None}}, {}],
        Join @@ Table[
          If[coeffR[[j]] =!= 0, {{-coeffR[[j]], vars[[j]]}}, {}],
          {j, 1, 3}
        ]
      ];
      addNote = addNoteFromTerms[addTerms];

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

    AppendTo[content, "Po úpravách dostaneme sústavu rovníc v štandardnom tvare, pripravenú na riešenie dosadzovacou metódou:"];

    AppendTo[content,
      alignedEquations@Table[
        {formatEquationLHS[rowsFinal[[i]], "", vars], rhsFinal[[i]], ""},
        {i, 1, 3}
      ]
    ];
  ];
  content
];

(* ~-~-~ MAIN CONTROLLER ~-~-~ *)
Gen01[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {dim, vars, st, data, A, b, steps, sol, genFunc, stepsFunc},

  If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff], Message[Gen01::baddiff, diff]; Return[$Failed]];
  If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode], Message[Gen01::badmode, mode]; Return[$Failed]];

  st = Replace[OptionValue[SolutionType], Automatic -> RandomChoice[{"ONE", "ONE", "ONE", "NONE", "INFINITE"}]];
  dim = Switch[diff, "EASY", 2, "MEDIUM" | "HARD", 3];
  vars = Take[{x, y, z}, dim];

  genFunc = generateLinearSystem[dim, diff, st];

  data = WithRetries[Function[Null, genFunc], 200];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  A = data["A"]; b = data["b"];

  printSectionCell["Dosadzovacia (substitučná) metóda"];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc"];

  If[diff === "HARD" && KeyExistsQ[data, "EqDisplay"],
    printFormulaCell @ alignedEquations[data["EqDisplay"]],
    If[dim == 2,
      printFormulaCell @ alignedEquations @ Table[{formatEquationLHS[A[[i]], "", vars], b[[i]], ""}, {i, 2}],
      printFormulaCell @ alignedEquations @ Table[{formatEquationLHS[A[[i]], "", vars], b[[i]], ""}, {i, 3}]
    ]
  ];

  printTextCell["Riešte v množine reálnych čísel dosadzovacou (substitučnou) metódou."];

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
      "NONE", printTextCell["Sústava nemá riešenie (pri riešení vznikol spor)."],
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