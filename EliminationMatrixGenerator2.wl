(* ::Package:: *)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];

Internal`$ContextMarks = False;

Gen01::usage =
    "Gen01[diff, mode, opts] vygeneruje pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c s\[CHacek]\[IAcute]tavacou (elimina\[CHacek]nou) met\[OAcute]dou.

diff:
  \"EASY\"   (2\[Times]2)
  \"MEDIUM\" (3\[Times]3) (moment\[AAcute]lne v k\[OAcute]de e\[SHacek]te nie je implementovan\[EAcute])
  \"HARD\"   (3\[Times]3) (moment\[AAcute]lne v k\[OAcute]de e\[SHacek]te nie je implementovan\[EAcute])

mode:
  \"TASK\"              \[Dash] vyp\[IAcute]\[SHacek]e iba zadanie
  \"TASK_RESULT\"       \[Dash] zadanie + v\[YAcute]sledok
  \"TASK_STEPS_RESULT\" \[Dash] zadanie + postup + v\[YAcute]sledok

opts:
  Visualization -> True|False   (jeden graf s oboma priamkami; iba pre 2\[Times]2)
  SolutionType   -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"
    - ak sa nezad\[AAcute] (Automatic): 80% \[SHacek]anca na pr\[AAcute]ve jedno rie\[SHacek]enie
    - \"ONE\"/\"NONE\"/\"INFINITE\" sl\[UAcute]\[ZHacek]i len na riadenie generovania; pou\[ZHacek]\[IAcute]vate\:013eovi sa nevypisuje.";

Gen01::baddiff  = "Neplatn\[AAcute] obtia\[ZHacek]nos\[THacek] `1`. Pou\[ZHacek]i \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatn\[YAcute] re\[ZHacek]im `1`. Pou\[ZHacek]i \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtia\[ZHacek]nos\[THacek] `1` zatia\:013e nie je implementovan\[AAcute] v tomto gener\[AAcute]tore.";
Gen01::fail     = "Nepodarilo sa vygenerova\[THacek] vhodn\[YAcute] pr\[IAcute]klad.";

Options[Gen01] = {
  SolutionType -> Automatic,
  Visualization -> False
};

Begin["`Private`"];

CellBox[expr_] := CellPrint @ Cell[
  BoxData @ ToBoxes[expr, TraditionalForm],
  "DisplayFormula",
  ShowStringCharacters -> False
];

renderItem[item_] := Which[
  StringQ[item], CellText[item],
  Head[item] === Graphics, CellPrint @ Cell[BoxData @ ToBoxes[item], "Graphics"],
  True, CellBox[item]
];

highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

alignedEquations[data_] := Module[
  {
    eqSign = Style["=", 16],
    vbar   = Style["\[VerticalSeparator]", GrayLevel[.25]],
    fmtL, fmtR
  },

  fmtL[expr_] := expr;
  fmtR[expr_] := expr;

  Grid[
    data /. {lhs_, rhs_, note_: ""} :>
        {
          fmtL[lhs],
          eqSign,
          fmtR[rhs],
          If[note === "" || note === None,
            "",
            Style[Row[{vbar, Spacer[4], note}], GrayLevel[.25], FontSize -> 13]
          ]
        },
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, 0.6},
    BaseStyle -> {FontSize -> 14}
  ]
];

formatLHS[cx_, cy_, choice_] := Module[{tX, tY, sign},
  tX = If[choice == "X", highlightTerm[cx x], cx x];

  If[cy == 0,
    TraditionalForm[tX],

    sign = If[cy < 0, " - ", " + "];
    tY = If[choice == "Y", highlightTerm[Abs[cy] y], Abs[cy] y];

    Row[{TraditionalForm[tX], sign, TraditionalForm[tY]}]
  ]
];

lineLegendText[a_, b_, c_] := Module[{m, q, fmt},
  fmt[t_] := ToString[TraditionalForm[Together[t]]];

  If[b == 0,
    "x = " <> fmt[c/a],
    m = Together[-a/b];
    q = Together[c/b];
    "y = " <> fmt[m] <> "x" <> If[q >= 0, " + " <> fmt[q], " - " <> fmt[Abs[q]]]
  ]
];

multiplyNoteString[m_] := Which[
  m == 1, "",
  m < 0, "\[CenterDot] (" <> ToString[m] <> ")",
  True, "\[CenterDot] " <> ToString[m]
];

coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff[_] := 5;

boundByDiff["EASY"] := 60;
boundByDiff[_] := 90;

randomRow[n_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, n];
  If[AllTrue[v, # == 0 &], randomRow[n, r], v]
];

numbersNiceQ[A_, b_, diff_] := Module[{bd = boundByDiff[diff]},
  Max[Abs @ Join[Flatten[A], Flatten[b]]] <= bd
];

generateSystemOne[dim_, diff_] := Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-10, 10}, dim];
  A = Table[randomRow[dim, r], {dim}];

  If[dim == 2 && (A[[1, 1]] == 0 || A[[2, 1]] == 0), Return[$Failed]];
  If[Det[A] == 0, Return[$Failed]];
  b = A . x0;
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>
];

generateSystemNone2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow[2, r];
  If[row1[[1]] == 0, row1[[1]] = RandomChoice[{-r, -1, 1, r}]];
  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1 + RandomChoice[{-5, -3, 3, 5}];
  A = {row1, row2};
  b = {c1, c2};
  If[A[[1, 1]] == 0 || A[[2, 1]] == 0, Return[$Failed]];
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

generateSystemInfinite2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow[2, r];
  If[row1[[1]] == 0, row1[[1]] = RandomChoice[{-r, -1, 1, r}]];
  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1;
  A = {row1, row2};
  b = {c1, c2};
  If[A[[1, 1]] == 0 || A[[2, 1]] == 0, Return[$Failed]];
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

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

eliminationStart[A_, b_, vars_] := Module[
  {content = {}, x, y, a1, b1, c1, a2, b2, c2,
    resX, resY, choice, targetVar, elimReason,
    rawM1, rawM2, m1, m2, k1, k2,
    c1x, c1y, c1rhs, c2x, c2y, c2rhs,
    rows1, rows2},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  resX = analyzeVariableElimination[1, A];
  resY = analyzeVariableElimination[2, A];

  If[resY["Score"] < resX["Score"],
    choice = "Y"; targetVar = y;
    {k1, k2} = resY["Coeffs"];
    {rawM1, rawM2} = {resY["RawMul1"], resY["RawMul2"]};
    elimReason = If[rawM1 == 1 || rawM2 == 1,
      "sta\[CHacek]\[IAcute] vyn\[AAcute]sobi\[THacek] len jednu rovnicu.", "koeficienty maj\[UAcute] men\[SHacek]\[IAcute] spolo\[CHacek]n\[YAcute] n\[AAcute]sobok."];,
    choice = "X"; targetVar = x;
    {k1, k2} = resX["Coeffs"];
    {rawM1, rawM2} = {resX["RawMul1"], resX["RawMul2"]};
    elimReason = If[rawM1 == 1 || rawM2 == 1,
      "sta\[CHacek]\[IAcute] vyn\[AAcute]sobi\[THacek] len jednu rovnicu.", "je to v\[YAcute]hodnej\[SHacek]ie pre v\[YAcute]po\[CHacek]et."];
  ];

  AppendTo[content, "1. \[CapitalUAcute]prava rovn\[IAcute]c"];
  AppendTo[content, "Rozhodli sme sa eliminova\[THacek] premenn\[UAcute] " <> ToString[targetVar] <> ", preto\[ZHacek]e " <> elimReason];

  If[Sign[k1] != Sign[k2],
    m1 = rawM1; m2 = rawM2;,
    If[c1 < 0, m1 = -rawM1; m2 = rawM2,
      If[c2 < 0, m1 = rawM1; m2 = -rawM2,
        m1 = rawM1; m2 = -rawM2]]
  ];

  rows1 = {
    {a1*x + b1*y, c1, multiplyNoteString[m1]},
    {a2*x + b2*y, c2, multiplyNoteString[m2]}
  };

  AppendTo[content, alignedEquations[rows1]];

  c1x = m1*a1; c1y = m1*b1; c1rhs = m1*c1;
  c2x = m2*a2; c2y = m2*b2; c2rhs = m2*c2;

  rows2 = {
    {formatLHS[c1x, c1y, choice], c1rhs, ""},
    {formatLHS[c2x, c2y, choice], c2rhs, ""}
  };
  AppendTo[content, alignedEquations[rows2]];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "failed" -> False|>
];

stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y,
    sumRHS, sumCoeffX, sumCoeffY, calcVar, calcVal, otherVar, otherVal,
    stepsY, stepsSub, valProduct, op, rhsRem, elimVarStr,
    explicitSubstLHS, termRemAbs, substCoeff, substConst, termResult},

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

  AppendTo[content, "2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c a v\[YAcute]po\[CHacek]et prvej premennej"];
  AppendTo[content, "Rovnice s\[CHacek]\[IAcute]tame. \[CapitalCHacek]leny s premennou " <> ToString[If[elimVarStr=="X", x, y]] <> " vypadn\[UAcute]."];

  stepsY = {};

  If[elimVarStr == "X",
    termResult = sumCoeffY*y;
    AppendTo[stepsY, {termResult, sumRHS, ""}];

    If[sumCoeffY == 0, Return[$Failed]];
    calcVar = y; calcVal = sumRHS / sumCoeffY; otherVar = x;

    If[sumCoeffY =!= 1,
      AppendTo[stepsY, {sumCoeffY y, sumRHS, ": " <> ToString[sumCoeffY]}];
    ];
  ];

  If[elimVarStr == "Y",
    termResult = sumCoeffX*x;
    AppendTo[stepsY, {termResult, sumRHS, ""}];

    If[sumCoeffX == 0, Return[$Failed]];
    calcVar = x; calcVal = sumRHS / sumCoeffX; otherVar = y;

    If[sumCoeffX =!= 1,
      AppendTo[stepsY, {sumCoeffX x, sumRHS, ": " <> ToString[sumCoeffX]}];
    ];
  ];

  AppendTo[content, alignedEquations[stepsY]];
  AppendTo[content, highlightGrid[alignedEquations[{{calcVar, calcVal, ""}}]]];


  AppendTo[content, "3. Dosadenie a v\[YAcute]po\[CHacek]et druhej premennej"];
  AppendTo[content, "Vypo\[CHacek]\[IAcute]tan\[UAcute] hodnotu " <> ToString[calcVar] <> " dosad\[IAcute]me do prvej rovnice (alebo ktorejko\:013evek inej)."];

  stepsSub = {};

  AppendTo[stepsSub, {a1 x + b1 y, c1, ""}];

  If[elimVarStr == "X",
    substCoeff = a1; substConst = b1;

    explicitSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[substConst < 0, " - ", " + "],
      Abs[substConst],
      If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal]
    }];

    valProduct = substConst * calcVal;

    calculatedSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[valProduct < 0, " - ", " + "],
      Abs[valProduct]
    }];
    ,
    substCoeff = b1; substConst = a1;

    explicitSubstLHS = Row[{
      substConst,
      If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal],
      If[substCoeff < 0, " - ", " + "],
      If[Abs[substCoeff] == 1, y, Row[{Abs[substCoeff], y}]]
    }];

    valProduct = substConst * calcVal;

    calculatedSubstLHS = Row[{
      valProduct,
      If[substCoeff < 0, " - ", " + "],
      If[Abs[substCoeff] == 1, y, Row[{Abs[substCoeff], y}]]
    }];
  ];

  AppendTo[stepsSub, {explicitSubstLHS, c1, ""}];

  op = If[valProduct > 0, "- " <> ToString[valProduct], "+ " <> ToString[Abs[valProduct]]];
  AppendTo[stepsSub, {calculatedSubstLHS, c1, op}];

  rhsRem = c1 - valProduct;

  If[substCoeff =!= 1,
    termUnknown = If[elimVarStr == "X", a1 x, b1 y];
    AppendTo[stepsSub, {termUnknown, rhsRem, ": " <> ToString[substCoeff]}];
  ];

  AppendTo[content, alignedEquations[stepsSub]];

  otherVal = rhsRem / substCoeff;
  AppendTo[content, highlightGrid[alignedEquations[{{otherVar, otherVal, ""}}]]];

  solPair = If[elimVarStr == "X", {otherVal, calcVal}, {calcVal, otherVal}];

  <|"Content" -> content, "Solution" -> solPair|>
];

stepsNone2[A_, b_, vars_] := Module[
  {data, content, m1, m2, b1, b2, c1, c2, sumRHS, coeffY, x, y},

  x = vars[[1]]; y = vars[[2]];
  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  c1 = b[[1]]; c2 = b[[2]];
  sumRHS = m1 c1 + m2 c2;

  AppendTo[content, "2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c"];
  AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] oboch str\[AAcute]n dostaneme:"];
  AppendTo[content, alignedEquations[{{0, sumRHS, ""}}]];

  AppendTo[content, "3. Z\[AAcute]ver"];
  AppendTo[content, "Ke\[DHacek]\[ZHacek]e sme dostali nepravdiv\[UAcute] rovnos\[THacek] (spor), s\[UAcute]stava nem\[AAcute] \[ZHacek]iadne rie\[SHacek]enie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

stepsInfinite2[A_, b_, vars_] := Module[
  {data, content, m1, m2, b1, b2, c1, c2, sumRHS, coeffY, x, y, a1},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];

  AppendTo[content, "2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c"];
  AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] oboch str\[AAcute]n dostaneme:"];
  AppendTo[content, alignedEquations[{{0, 0, ""}}]];

  AppendTo[content, "3. Z\[AAcute]ver"];
  AppendTo[content, "Dostali sme pravdiv\[UAcute] rovnos\[THacek] pre ak\[EAcute]ko\:013evek x a y. S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
  AppendTo[content, "Vyjadrenie rie\[SHacek]enia pomocou parametra:"];

  AppendTo[content,
    alignedEquations[{
      {(c1 - b1*y)/a1, x, ""},
      {y, y, ""}
    }]
  ];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

visualize2[A_, b_, vars_, sol_] := Module[
  { x, y, pt, subtitle, xrange, yrange, lineSeg, seg1, seg2, g, col1, col2, legend1, legend2, center, half},

  {x, y} = vars;

  half = 10;

  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    xrange = center[[1]] + {-half, half};
    yrange = center[[2]] + {-half, half};
    xrange = {Min[xrange[[1]], 0], Max[xrange[[2]], 0]};
    yrange = {Min[yrange[[1]], 0], Max[yrange[[2]], 0]};

    subtitle = Row[{
      "Na grafe s\[UAcute] zn\[AAcute]zornen\[EAcute] obe priamky. Ich priese\[CHacek]n\[IAcute]k je vyzna\[CHacek]en\[YAcute] kru\[ZHacek]nicou a zodpoved\[AAcute] rie\[SHacek]eniu s\[UAcute]stavy [",
      TraditionalForm[Together[pt[[1]]]], ", ",
      TraditionalForm[Together[pt[[2]]]], "]."
    }];,

    pt = None;
    center = {0, 0};
    xrange = {-10, 10};
    yrange = {-10, 10};

    subtitle = If[sol === "NONE",
      "Priamky s\[UAcute] rovnobe\[ZHacek]n\[EAcute] a nemaj\[UAcute] spolo\[CHacek]n\[YAcute] bod (s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie).",
      "Priamky s\[UAcute] toto\[ZHacek]n\[EAcute] (s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute])."
    ];
  ];

  CellText[subtitle];

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
        {col1, Thick, seg1},
        {col2, Thick, seg2},

        If[pt =!= None,
          {
            {Black, Thick, Circle[pt, 0.4]},
            {Green, PointSize[0.02], Point[pt]},

            Text[
              Style[
                Row[{"[", TraditionalForm[Together[pt[[1]]]], ", ",
                  TraditionalForm[Together[pt[[2]]]], "]"}],
                14
              ],
              pt + labelOffset
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
      Method -> {
        "CoordinatesToolOptions" -> {
          "DisplayFunction" -> (Row[{"x=", NumberForm[#[[1]], {Infinity, 2}], ", y=", NumberForm[#[[2]], {Infinity, 2}]}] &),
          "CopiedValueFunction" -> (#[[1 ;; 2]] &)
        }
      }

    ],
    Placed[
      LineLegend[{col1, col2}, {legend1, legend2}],
      After
    ]
  ];


  CellBox @ g
];

Gen01[diff_String, mode_String, opts : OptionsPattern[]] :=
    Module[{dim, vars, st, gen, data, A, b, steps, sol},

      If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff], Message[Gen01::baddiff, diff]; Return[$Failed]];
      If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode], Message[Gen01::badmode, mode]; Return[$Failed]];
      If[diff =!= "EASY", Message[Gen01::notimpl, diff]; Return[$Failed]];

      st = ResolveSolutionType[OptionValue[SolutionType]];
      dim = DimensionByDifficulty["Elimination", diff];
      vars = Take[{x, y, z}, dim];

      gen := Which[
        dim == 2 && st == "ONE",      generateSystemOne[2, diff],
        dim == 2 && st == "NONE",     generateSystemNone2[diff],
        dim == 2 && st == "INFINITE", generateSystemInfinite2[diff],
        True, $Failed
      ];

      data = WithRetries[Function[Null, gen], 200];
      If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

      A = data["A"]; b = data["b"];

      CellSection["S\[CHacek]\[IAcute]tavacia (elimina\[CHacek]n\[AAcute]) met\[OAcute]da"];
      CellSubsection["Zadanie"];

      CellText["Vyrie\[SHacek]te nasleduj\[UAcute]cu s\[UAcute]stavu line\[AAcute]rnych rovn\[IAcute]c pomocou s\[CHacek]\[IAcute]tavacej (elimina\[CHacek]nej) met\[OAcute]dy."];

      CellBox @ alignedEquations[Table[{A[[i]] . vars, b[[i]], ""}, {i, Length[b]}]];

      If[mode === "TASK",
        Return[<|"A" -> A, "b" -> b, "vars" -> vars|>]
      ];

      steps = Which[
        data["type"] == "ONE",      stepsOne2[A, b, vars],
        data["type"] == "NONE",     stepsNone2[A, b, vars],
        data["type"] == "INFINITE", stepsInfinite2[A, b, vars],
        True, $Failed
      ];

      If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];

      sol = steps["Solution"];

      If[mode === "TASK_STEPS_RESULT",
        CellSubsection["Postup"];
        Scan[renderItem, steps["Content"]];
      ];

      CellSubsection["V\[YAcute]sledok"];

      Switch[sol,
        "NONE",
        CellText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];
        Null,
        "INFINITE",
        CellText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];,
        _,
        CellText["Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy je:"];
        CellBox @ alignedEquations[{{x, sol[[1]], ""}, {y, sol[[2]], ""}}];
      ];

      If[OptionValue[Visualization] && dim == 2,
        visualize2[A, b, vars, sol];
      ];

      Null
    ];

CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];

End[];
EndPackage[];