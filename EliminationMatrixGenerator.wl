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

(* ======================== POMOCN\[CapitalEAcute] FUNKCIE PRE FORM\[CapitalAAcute]TOVANIE ======================== *)

(* Bezpe\[CHacek]n\[EAcute] zobrazenie rovnice *)
holdEq[lhs_, rhs_] := HoldForm[lhs == rhs];
eqTF[expr_] := TraditionalForm[expr];

(* Form\[AAcute]tovanie rovnice s pozn\[AAcute]mkou vpravo (napr. / * 3) *)
(* Style[note, "Text"] zabezpe\[CHacek]\[IAcute], \[ZHacek]e sa nezobrazia \[UAcute]vodzovky *)
eqWithNote[eq_, note_String] := 
  Grid[{{eqTF[eq], Style[note, "Text", GrayLevel[0.25], Italic, FontSize -> 14]}}, 
   Alignment -> {Left, Center}, Spacings -> {1, 0}];

(* Zv\[YAcute]raznenie \[CHacek]lena, ktor\[YAcute] sa ide vyru\[SHacek]i\[THacek] *)
highlightTerm[term_] := Style[term, Bold, Background -> RGBColor[1, 0.9, 0.6]];

(* Form\[AAcute]tovanie syst\[EAcute]mu rovn\[IAcute]c pod sebou *)
systemColumn[A_, b_, vars_] := 
  Column[
    Table[eqTF[holdEq[A[[i]] . vars, b[[i]]]], {i, Length[b]}],
    Spacings -> 0.5, Alignment -> Left
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


(* ======================== GENEROVANIE D\[CapitalAAcute]T ======================== *)

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

(* --- Gener\[AAcute]tory syst\[EAcute]mov --- *)

generateSystemOne[dim_, diff_] := Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-10, 10}, dim]; 
  A = Table[randomRow[dim, r], {dim}];
  
  (* Pre elimin\[AAcute]ciu X nesmieme ma\[THacek] nulu pri X *)
  If[dim == 2 && (A[[1, 1]] == 0 || A[[2, 1]] == 0), Return[$Failed]];

  If[Det[A] == 0, Return[$Failed]];
  b = A . x0;
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>
];

generateSystemNone2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow[2, r];
  
  (* K\:013d\[CapitalUAcute]\[CapitalCHacek]OV\[CapitalAAcute] OPRAVA: Pre elimin\[AAcute]ciu X nesmie by\[THacek] koeficient pri X nulov\[YAcute] *)
  (* Ak vygenerujeme 0, zmen\[IAcute]me ju na n\[AAcute]hodn\[EAcute] nenulov\[EAcute] \[CHacek]\[IAcute]slo *)
  If[row1[[1]] == 0, row1[[1]] = RandomChoice[{-r, -1, 1, r}]];
  
  k = RandomChoice[{-3, -2, 2, 3}]; 
  row2 = k * row1;
  
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1 + RandomChoice[{-5, -3, 3, 5}]; (* Posun pre NONE *)
  
  A = {row1, row2};
  b = {c1, c2};
  
  (* Znova skontrolujeme nuly pre istotu *)
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

(* ======================== KROKY RIE\[CapitalSHacek]ENIA ======================== *)

(* Pomocn\[AAcute] funkcia pre za\[CHacek]iatok elimin\[AAcute]cie *)
eliminationStart[A_, b_, vars_] := Module[
  {content = {}, x, y, a1, b1, c1, a2, b2, c2, lcm, m1, m2, eq1Mod, eq2Mod, separator},
  
  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];
  
  (* Vizu\[AAcute]lny separ\[AAcute]tor - \[CHacek]iara (StringRepeat) *)
  separator = Style[
    Row[{StringRepeat["\[LongDash]", 30]}],
    GrayLevel[0], FontSize -> 14];

  AppendTo[content, 
   "Vyn\[AAcute]sob\[IAcute]me rovnice tak, aby koeficienty pri premennej " <> ToString[x] <> " mali opa\[CHacek]n\[EAcute] znamienka a po s\[CHacek]\[IAcute]tan\[IAcute] sa navz\[AAcute]jom vyru\[SHacek]ili.\n\n" <>
   "Cie\:013eom je n\[AAcute]js\[THacek] najmen\[SHacek]\[IAcute] spolo\[CHacek]n\[YAcute] n\[AAcute]sobok koeficientov a pripravi\[THacek] rovnice na s\[CHacek]\[IAcute]tanie, aby sa jedna premenn\[AAcute] eliminovala."
  ];

  (* Ochrana pred delen\[IAcute]m nulou *)
  If[a1 == 0 || a2 == 0, Return[<|"failed" -> True|>]];

  lcm = LCM[Abs[a1], Abs[a2]];
  
  m1 = lcm / Abs[a1];
  If[a1 < 0, m1 = -m1];
  
  m2 = -lcm / Abs[a2];
  If[a2 < 0, m2 = -m2]; 
  
  (* Zobrazenie n\[AAcute]sobenia - Style[..., "Text"] odstr\[AAcute]ni \[UAcute]vodzovky *)
  AppendTo[content, Grid[{
    {eqTF[holdEq[a1 x + b1 y, c1]], Style["/ \[CenterDot] " <> ToString[m1], "Text", GrayLevel[0.25], Italic, FontSize -> 14]},
    {eqTF[holdEq[a2 x + b2 y, c2]], Style["/ \[CenterDot] " <> ToString[m2], "Text", GrayLevel[0.25], Italic, FontSize -> 14]}
  }, Alignment -> Left, Spacings -> {1, 0.5}]];
  
  AppendTo[content, separator];
  
  eq1Mod = holdEq[highlightTerm[m1*a1*x] + (m1*b1)*y, m1*c1];
  eq2Mod = holdEq[highlightTerm[m2*a2*x] + (m2*b2)*y, m2*c2];
  
  AppendTo[content, Column[{eqTF[eq1Mod], eqTF[eq2Mod]}, Spacings -> 0.5, Alignment -> Left]];
  AppendTo[content, "S\[CHacek]\[IAcute]tame upraven\[EAcute] rovnice. \[CapitalCHacek]leny s premennou " <> ToString[x] <> " sa vyru\[SHacek]ia."];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "failed" -> False|>
];

(* --- Kroky pre ONE --- *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, 
   sumRHS, coeffY, yVal, stepsY, stepsSub, valProduct, lhsSimple, op, rhsX, xVal},
  
  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  
  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];
  
  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  b2 = A[[2, 2]]; c2 = b[[2]];

  sumRHS = m1 c1 + m2 c2;
  coeffY = m1 b1 + m2 b2;
  
  (* S\[UAcute]\[CHacek]et po elimin\[AAcute]cii - pou\[ZHacek]itie \[CHacek]istej rovnice bez Row *)
  AppendTo[content, eqTF[holdEq[0*x + coeffY*y, sumRHS]]];
  
  If[coeffY == 0, Return[$Failed]]; 

  yVal = sumRHS / coeffY;
  stepsY = {};
  
  If[coeffY < 0,
    AppendTo[stepsY, eqWithNote[holdEq[coeffY y, sumRHS], "/ \[CenterDot] (-1)"]];
    AppendTo[stepsY, eqWithNote[holdEq[-coeffY y, -sumRHS], "/ : " <> ToString[-coeffY]]];
    ,
    AppendTo[stepsY, eqWithNote[holdEq[coeffY y, sumRHS], "/ : " <> ToString[coeffY]]];
  ];
  
  AppendTo[stepsY, eqTF[holdEq[y, yVal]]];
  AppendTo[content, Column[stepsY, Alignment -> Left, Spacings -> 0.5]];
  
  AppendTo[content, "Dosad\[IAcute]me vypo\[CHacek]\[IAcute]tan\[UAcute] hodnotu " <> ToString[y] <> " do prvej rovnice."];
  stepsSub = {};
  
  AppendTo[stepsSub, eqWithNote[holdEq[a1 x + b1 y, c1], "/ " <> ToString[y] <> " = " <> ToString[yVal]]];
  
  (* Dosadenie - vizu\[AAcute]lna substit\[UAcute]cia pomocou v\[YAcute]razu *)
  AppendTo[stepsSub, eqTF[holdEq[a1*x + b1*(yVal), c1]]];
  
  valProduct = b1 * yVal;
  lhsSimple = a1 x + valProduct; 
  op = If[valProduct > 0, "/ - " <> ToString[valProduct], "/ + " <> ToString[Abs[valProduct]]];
  AppendTo[stepsSub, eqWithNote[holdEq[lhsSimple, c1], op]];
  
  rhsX = c1 - valProduct;
  If[a1 == 0, Return[$Failed]]; 
  AppendTo[stepsSub, eqWithNote[holdEq[a1 x, rhsX], "/ : " <> ToString[a1]]];
  
  xVal = rhsX / a1;
  AppendTo[stepsSub, eqTF[holdEq[x, xVal]]];
  
  AppendTo[content, Column[stepsSub, Alignment -> Left, Spacings -> 0.5]];
  
  <|"Content" -> content, "Solution" -> {xVal, yVal}|>
];

(* --- Kroky pre NONE --- *)
stepsNone2[A_, b_, vars_] := Module[
  {data, content, m1, m2, b1, b2, c1, c2, sumRHS, coeffY, x, y},

  x = vars[[1]]; y = vars[[2]];
  
  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];
  
  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  b1 = A[[1, 2]]; b2 = A[[2, 2]];
  c1 = b[[1]]; c2 = b[[2]];

  sumRHS = m1 c1 + m2 c2;
  coeffY = m1 b1 + m2 b2; (* Malo by by\[THacek] 0 *)
  
  (* Vizu\[AAcute]lne: 0x + 0y = sumRHS *)
  AppendTo[content, eqTF[holdEq[0*x + 0*y, sumRHS]]];
  
  (* Z\[AAcute]ver *)
  AppendTo[content, eqTF[holdEq[0, sumRHS]]];
  
  (* Lep\[SHacek]\[IAcute] popis sporu *)
  AppendTo[content, "Dostali sme rovnos\[THacek] 0 = " <> ToString[sumRHS] <> ", \[CHacek]o neplat\[IAcute]."];
  AppendTo[content, "Preto\[ZHacek]e sme dostali nepravdiv\[YAcute] v\[YAcute]rok (spor), s\[UAcute]stava nem\[AAcute] \[ZHacek]iadne rie\[SHacek]enie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

(* --- Kroky pre INFINITE --- *)
stepsInfinite2[A_, b_, vars_] := Module[
  {data, content, m1, m2, b1, b2, c1, c2, sumRHS, coeffY, x, y, a1, solSet},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  
  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];
  
  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  b1 = A[[1, 2]]; b2 = A[[2, 2]];
  c1 = b[[1]]; c2 = b[[2]];

  sumRHS = m1 c1 + m2 c2; (* 0 *)
  coeffY = m1 b1 + m2 b2; (* 0 *)
  
  (* Vizu\[AAcute]lne: 0x + 0y = 0 *)
  AppendTo[content, eqTF[holdEq[0*x + 0*y, sumRHS]]];
  
  AppendTo[content, eqTF[holdEq[0, 0]]];
  
  AppendTo[content, "Dostali sme pravdiv\[UAcute] rovnos\[THacek] 0 = 0. S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
  AppendTo[content, "Vyjadrenie rie\[SHacek]enia pomocou parametra p:"];
  
  solSet = Row[{
    "(", 
    If[a1 == 1, 
       Row[{c1 - b1*y}], 
       FractionBox[ToBoxes[HoldForm[c1 - b1*y]], ToBoxes[a1]] // DisplayForm
    ], 
    "; ", y, ")"
  }];
  
  AppendTo[content, solSet];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

(* ======================== VIZUALIZ\[CapitalAAcute]CIA ======================== *)

visualize2[A_, b_, vars_, sol_] := Module[
  { x, y, pt, subtitle, xrange, yrange, lineSeg, seg1, seg2, g, col1, col2, legend1, legend2, center, half},

  {x, y} = vars;

  half = 10;

  (* text + bod *)
  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;

    center = 5 Round[pt/5];                 (* posun po 5 *)
    xrange = center[[1]] + {-half, half};
    yrange = center[[2]] + {-half, half};

    xrange = {Min[xrange[[1]], 0], Max[xrange[[2]], 0]};
    yrange = {Min[yrange[[1]], 0], Max[yrange[[2]], 0]};


    subtitle = Row[{
      "Na grafe s\[UAcute] zn\[AAcute]zornen\[EAcute] obe priamky. Ich priese\[CHacek]n\[IAcute]k je vyzna\[CHacek]en\[YAcute] kru\[ZHacek]nicou a zodpoved\[AAcute] rie\[SHacek]eniu s\[UAcute]stavy [",
      TraditionalForm[Together[pt[[1]]]], ", ",
      TraditionalForm[Together[pt[[2]]]], "]."
    }];
    ,

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

  (* pomocn\[AAcute] funkcia: z (a x + b y = c) sprav\[IAcute] \[UAcute]se\[CHacek]ku v r\[AAcute]mci rozsahu *)
  lineSeg[{a_, bb_}, c_] := Module[{pA, pB},
    If[bb =!= 0,
      (* vypo\[CHacek]\[IAcute]taj y pre \:013eav\[YAcute] a prav\[YAcute] okraj *)
      pA = {xrange[[1]], (c - a*xrange[[1]])/bb};
      pB = {xrange[[2]], (c - a*xrange[[2]])/bb};,
      (* zvisl\[AAcute] priamka: x = c/a *)
      pA = {c/a, yrange[[1]]};
      pB = {c/a, yrange[[2]]};
    ];
    Line[{pA, pB}]
  ];

  seg1 = lineSeg[A[[1]], b[[1]]];
  seg2 = lineSeg[A[[2]], b[[2]]];
  
  (* smerov\[EAcute] vektory priamok *)
  v1 = Normalize[{A[[1, 2]], -A[[1, 1]]}];
  v2 = Normalize[{A[[2, 2]], -A[[2, 1]]}];

  (* bisektor tup\[EAcute]ho uhla - aby bolo pekne viditelne suradnice *) 
  bisector =
    If[v1 . v2 < 0,
      Normalize[v1 + v2],
      Normalize[v1 - v2]
    ];

  labelOffset = 1.2 bisector;


  col1 = Magenta;
  col2 = Blue;

  legend1 = lineLegendText[A[[1, 1]], A[[1, 2]], b[[1]]];
  legend2 = lineLegendText[A[[2, 1]], A[[2, 2]], b[[2]]];

  g =
  Legended[
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


  CellExpr @ g
];


(* ======================== HLAVN\[CapitalAAcute] FUNKCIA ======================== *)

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
  
  (* V\[CapitalZHacek]DY vyp\[IAcute]sa\[THacek] text zadania *)
  CellText["Vyrie\[SHacek]te nasleduj\[UAcute]cu s\[UAcute]stavu line\[AAcute]rnych rovn\[IAcute]c pomocou s\[CHacek]\[IAcute]tavacej (elimina\[CHacek]nej) met\[OAcute]dy."];
  
  CellExpr @ systemColumn[A, b, vars];
  
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
    Scan[
      Which[
        StringQ[#], CellText[#], 
        Head[#] === Graphics, CellPrint[Cell[BoxData[ToBoxes[#]], "Graphics"]],
        True, CellExpr[#]
      ] &,
      steps["Content"]
    ];
  ];

  CellSubsection["V\[YAcute]sledok"];
  
  Switch[sol,
    "NONE", 
      CellText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];
      CellExpr @ eqTF[False], 
    "INFINITE",
      CellText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];,
    _,
      CellText["Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy je:"];
      CellExpr @ Column[{eqTF[x == sol[[1]]], eqTF[y == sol[[2]]]}, Alignment -> Left];
  ];

  If[OptionValue[Visualization] && dim == 2,
    visualize2[A, b, vars, sol];
  ];

  Null
];

CellSection[str_] := CellPrint[Cell[str, "Section"]];
CellSubsection[str_] := CellPrint[Cell[str, "Subsection"]];

End[];
EndPackage[];
