(* ::Package:: *)

(*
  Balíček: EliminationMatrixGenerator
  Popis: Tento balíček slúži na automatické generovanie didaktických materiálov pre riešenie sústav lineárnych rovníc sčítacou (eliminačnou) metódou.
  Obsahuje komplexnú logiku pre tvorbu zadaní, generovanie krokov riešenia a grafickú vizualizáciu geometrickej interpretácie sústavy.
*)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];

Internal`$ContextMarks = False;

(* Definícia verejného rozhrania a dokumentácie pre hlavnú funkciu Gen01 *)
Gen01::usage =
    "Gen01[diff, mode, opts] vygeneruje pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c s\[CHacek]\[IAcute]tavacou (elimina\[CHacek]nou) met\[OAcute]dou.

diff:
  \"EASY\"   (2\[Times]2)
  \"MEDIUM\" (3\[Times]3)
  \"HARD\"   (3\[Times]3) (moment\[AAcute]lne v k\[OAcute]de e\[SHacek]te nie je implementovan\[EAcute])

mode:
  \"TASK\"              \[Dash] vyp\[IAcute]\[SHacek]e iba zadanie
  \"TASK_RESULT\"       \[Dash] zadanie + v\[YAcute]sledok
  \"TASK_STEPS_RESULT\" \[Dash] zadanie + postup + v\[YAcute]sledok

opts:
  Visualization -> True|False   (2\[Times]2: graf priamok, 3\[Times]3: graf rov\[IAcute]n)
  SolutionType   -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"
    - ak sa nezad\[AAcute] (Automatic): 80% \[SHacek]anca na pr\[AAcute]ve jedno rie\[SHacek]enie
    - \"ONE\"/\"NONE\"/\"INFINITE\" sl\[UAcute]\[ZHacek]i len na riadenie generovania; pou\[ZHacek]\[IAcute]vate\:013eovi sa nevypisuje.";

(* Definícia chybových hlásení pre neplatné vstupy alebo stavy generátora *)
Gen01::baddiff  = "Neplatn\[AAcute] obtia\[ZHacek]nos\[THacek] `1`. Pou\[ZHacek]i \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatn\[YAcute] re\[ZHacek]im `1`. Pou\[ZHacek]i \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtia\[ZHacek]nos\[THacek] `1` zatia\:013e nie je implementovan\[AAcute] v tomto gener\[AAcute]tore.";
Gen01::fail     = "Nepodarilo sa vygenerova\[THacek] vhodn\[YAcute] pr\[IAcute]klad.";

Options[Gen01] = {
  SolutionType -> Automatic,
  Visualization -> False
};

Begin["`Private`"];

(* --- Sekcia pomocných funkcií zabezpečujúcich formátovaný výstup do buniek notebooku (Cells) --- *)

CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];

CellBox[expr_] := CellPrint @ Cell[
  BoxData @ ToBoxes[expr, TraditionalForm],
  "DisplayFormula",
  ShowStringCharacters -> False
];

renderItem[item_] := Which[
  StringQ[item], CellText[item],
  (* Podpora pre objekt Style[_String, ...] zabezpečuje výpis vo formáte Text namiesto Formula *)
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Graphics || Head[item] === Graphics3D,
  CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, CellBox[item]
];

pickBestElimPair[rows_List, elimCol_Integer] := Module[
  {pairs, scorePair},

  pairs = Subsets[rows, {2}];

  scorePair[{i_, j_}] := Module[{c1, c2},
    c1 = rows[[i, elimCol]];
    c2 = rows[[j, elimCol]];

    (* ak by niekto mal 0, nech to má najhoršie (toto sa rieši inde) *)
    If[c1 == 0 || c2 == 0, Return[Infinity]];

    (* priorita: priame vyrušenie bez násobenia (napr. 4 a -4) *)
    If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], Return[0]];

    (* inak podľa LCM *)
    LCM[Abs[c1], Abs[c2]]
  ];

  First @ MinimalBy[pairs, scorePair]
];


(* Funkcia pre vizuálne zvýraznenie konkrétneho člena rovnice, napríklad pri procese eliminácie premennej *)
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

(* Funkcia pre vizuálne odlíšenie mriežky/tabuľky s výsledkom *)
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

(* Funkcia zabezpečujúca zarovnanie sústavy rovníc do mriežky (Grid).
   Použitie Map (/@) namiesto ReplaceAll (/.) zvyšuje robustnosť pri spracovaní zoznamov.
*)
alignedEquations[data_] := Module[
  {
    eqSign = Style["=", 16],
    vbar   = Style["\[VerticalSeparator]", GrayLevel[.25]],
    stepRow
  },

  (* Definícia formátovania jedného riadku mriežky *)
  stepRow[{lhs_, rhs_, note_}] := {
    lhs,
    eqSign,
    rhs,
    If[note === "" || note === None,
      "",
      Style[Row[{vbar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]
    ]
  };
  (* Ošetrenie prípadu, kedy nie je zadaná poznámka k úprave rovnice *)
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  Grid[
    stepRow /@ data,
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, 0.6},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* Rozšírená funkcia pre Grid s možnosťou vloženia extra medzery medzi skupinami riadkov (napr. pre oddelenie fáz výpočtu) *)
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

  (* Konfigurácia rozstupov medzi riadkami: {top, medzi1, medzi2, ..., bottom} *)
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

(* Formátovanie ľavej strany rovnice (2D) *)
formatLHS[cx_, cy_, choice_] := Module[{tX, tY, sign},
  tX = If[choice == "X", highlightTerm[cx x], cx x];

  If[cy == 0,
    TraditionalForm[tX],

    sign = If[cy < 0, " - ", " + "];
    tY = If[Abs[cy] == 1, y, Abs[cy] y];
    tY = If[choice == "Y", highlightTerm[tY], tY];

    Row[{TraditionalForm[tX], sign, TraditionalForm[tY]}]
  ]
];

(* Formátovanie ľavej strany rovnice (3D) - ax + by + cz *)
formatLHS3[cx_, cy_, cz_, choice_] := Module[{tX, tY, tZ, sY, sZ, terms = {}},
  (* Spracovanie X *)
  If[cx != 0,
    tX = Which[
      cx === 1,  x,
      cx === -1, -x,
      True,      cx x
    ];
    If[choice == "X", tX = highlightTerm[tX]];
    AppendTo[terms, TraditionalForm[tX]];
  ];


  (* Spracovanie Y *)
  If[cy != 0,
    sY = If[Length[terms] > 0, If[cy < 0, " - ", " + "], If[cy < 0, "-", ""]];
    tY = If[Abs[cy] == 1, y, Abs[cy] y];
    If[choice == "Y", tY = highlightTerm[tY]];
    AppendTo[terms, sY];
    AppendTo[terms, TraditionalForm[tY]];
  ];

  (* Spracovanie Z *)
  If[cz != 0,
    sZ = If[Length[terms] > 0, If[cz < 0, " - ", " + "], If[cz < 0, "-", ""]];
    tZ = If[Abs[cz] == 1, z, Abs[cz] z];
    If[choice == "Z", tZ = highlightTerm[tZ]];
    AppendTo[terms, sZ];
    AppendTo[terms, TraditionalForm[tZ]];
  ];

  If[Length[terms] == 0,
    TraditionalForm[0],
    Row[terms]
  ]
];

(* Generovanie textového popisu legendy pre graf (rovnica priamky v tvare y = kx + q alebo x = c) *)
lineLegendText[a_, b_, c_] := Module[{m, q, fmt, mStr},
  fmt[t_] := ToString[TraditionalForm[Together[t]]];

  If[b == 0,
    "x = " <> fmt[c/a],
    m = Together[-a/b];
    q = Together[c/b];

    mStr = Which[
      m === 1,  "",
      m === -1, "-",
      True,      fmt[m]
    ];

    "y = " <> mStr <> "x" <>
        Which[
          q === 0, "",
          q > 0, " + " <> fmt[q],
          True,  " - " <> fmt[Abs[q]]
        ]
  ]
];


(* Formátovanie poznámky pre násobenie rovnice (symbolická reprezentácia úpravy, napr. "· (-2)") *)
multiplyNoteString[m_] := Which[
  m == 1, "",
  m < 0, "\[CenterDot] (" <> ToString[m] <> ")",
  True, "\[CenterDot] " <> ToString[m]
];

(* Pomocná funkcia: poznámka k dosadeniu (napr. x -> 5, y -> -2) *)
(* vypíše len tie premenné, ktoré nemajú nenulový koeficient v rovnici *)
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


(* Pomocná funkcia: ak je hodnota záporná, dáme ju do zátvoriek *)
numOrParen[val_] := If[
  NumericQ[val] && val < 0,
  Row[{"(", TraditionalForm[Together[val]], ")"}],
  TraditionalForm[Together[val]]
];

(* Pomocná funkcia: vytvorí "a · (hodnota)" s korektným tvarom pri ±1 *)
coeffTimesValue[coeff_, val_] := Which[
  coeff === 0, 0,
  coeff === 1, numOrParen[val],
  coeff === -1, Row[{"-", numOrParen[val]}],
  True, Row[{coeff, " \[CenterDot] ", numOrParen[val]}]
];

(* Pomocná funkcia: zostaví ľavú stranu po dosadení známych premenných (ponechá jednu neznámu) *)
formatSubstLHS3[row_, vars_, solMap_, unknownVar_] := Module[
  {terms = {}, first = True, addTerm},

  (* Pridanie člena do výsledného Row[] so správnym znamienkom *)
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
        (* Neznáma ostáva symbolicky *)
        With[{c = row[[i]]},
          If[c > 0,
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, c unknownVar]], +1],
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, Abs[c] unknownVar]], -1]
          ]
        ],
        (* Známa premenná: dosadíme číslom *)
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

(* Pomocná funkcia: po dosadení vypočíta súčiny (napr. -4·(-3) -> 12), poradie členov ostáva *)
formatSubstLHS3Eval[row_, vars_, solMap_, unknownVar_] := Module[
  {terms = {}, first = True, addTerm},

  (* Pridanie člena so správnym znamienkom (už bez "+ -...") *)
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
        (* Neznáma ostáva symbolicky *)
        With[{c = row[[i]]},
          If[c > 0,
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, c unknownVar]], +1],
            addTerm[TraditionalForm[If[Abs[c] === 1, unknownVar, Abs[c] unknownVar]], -1]
          ]
        ],
        (* Známa: vyhodnotíme súčin a znamienko berieme z výsledku *)
        With[{prod = Together[row[[i]] * solMap[vars[[i]]]]},
           If[PossibleZeroQ[prod],
             Null, (* nechceme pridávať 0 člen *)
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

(* --- Nastavenia parametrov generovania podľa úrovne obťažnosti --- *)

coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff["MEDIUM"] := 5;
coeffRangeByDiff["HARD"] := 5;
coeffRangeByDiff[_] := 5;

boundByDiff["EASY"] := 60;
boundByDiff[_] := 90;

(* Generovanie náhodného vektora koeficientov s kontrolou proti vzniku nulového riadku *)
randomRow[n_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, n];
  If[AllTrue[v, # == 0 &], randomRow[n, r], v]
];

(* Špecializovaný generátor 2D riadku pre úroveň EASY: tvar ax ± y (optimalizované pre jednoduché výpočty) *)
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

(* Generátor pre 3D riadok (MEDIUM): max 1 nula *)
randomRow3["MEDIUM", r_] := Module[{v},
  v = RandomInteger[{-r, r}, 3];
  If[Count[v, 0] > 1 || AllTrue[v, #==0&], randomRow3["MEDIUM", r], v]
];
randomRow3[_, r_] := randomRow3["MEDIUM", r];


(* Validácia vygenerovaných koeficientov - zabezpečuje, aby čísla neprekročili stanovené limity a boli didakticky vhodné *)
numbersNiceQ[A_, b_, diff_] := Module[{bd = boundByDiff[diff]},
  Max[Abs @ Join[Flatten[A], Flatten[b]]] <= bd
];

(* --- Generátory sústav rovníc --- *)

(* Generovanie sústavy s práve jedným riešením (regulárna matica sústavy) *)
generateSystemOne[dim_, diff_] := Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-5, 5}, dim]; (* Pre istotu menší rozsah riešenia *)

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

(* Generovanie sústavy bez riešenia (2x2) *)
generateSystemNone2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow2NoZeros[diff, r];
  k = RandomChoice[{-3, -2, 2, 3}]; (* Koeficient lineárnej závislosti *)
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1 + RandomChoice[{-5, -3, 3, 5}]; (* Pravá strana porušuje lineárnu závislosť -> spor *)
  A = {row1, row2};
  b = {c1, c2};
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

(* Generovanie sústavy bez riešenia (3x3) - MEDIUM *)
generateSystemNone3[diff_] := Module[{r, row1, row2, row3, k1, k2, c1, c2, c3, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow3[diff, r];
  row2 = randomRow3[diff, r];

  (* Zabezpečíme, aby R1 a R2 neboli závislé, nech spor vznikne až pri R3 *)
  If[LinearDependentQ[{row1, row2}], Return[$Failed]];

  k1 = RandomChoice[{-2, -1, 1, 2}];
  k2 = RandomChoice[{-2, -1, 1, 2}];

  row3 = k1 * row1 + k2 * row2; (* R3 je LK R1 a R2 *)

  c1 = RandomInteger[{-5, 5}];
  c2 = RandomInteger[{-5, 5}];
  c3 = k1 * c1 + k2 * c2 + RandomChoice[{-5, -3, 3, 5}]; (* Spor na pravej strane *)

    A = {row1, row2, row3};
    If[Count[Flatten[A], 0] > 1, Return[$Failed]];
    b = {c1, c2, c3};

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

(* Generovanie sústavy s nekonečne veľa riešeniami (2x2) *)
generateSystemInfinite2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow2NoZeros[diff, r];
  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1; (* Pravá strana zachováva lineárnu závislosť -> identita *)
  A = {row1, row2};
  b = {c1, c2};
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

(* Generovanie sústavy s nekonečne veľa riešeniami (3x3) - MEDIUM *)
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
  c3 = k1 * c1 + k2 * c2; (* Bez sporu *)

    A = {row1, row2, row3};
    If[Count[Flatten[A], 0] > 1, Return[$Failed]];
    b = {c1, c2, c3};

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

(* Helper: LinearDependentQ *)
LinearDependentQ[vecs_] := MatrixRank[vecs] < Length[vecs];

(* Placeholder pre HARD generátory *)
generateSystemHard3[args___] := (Message[Gen01::notimpl, "HARD"]; $Failed);


(* --- Analýza sústavy a logika eliminácie --- *)

(* Analýza stĺpca matice sústavy za účelom určenia optimálnych koeficientov pre elimináciu (výpočet LCM) *)
analyzeVariableElimination[colIndex_, A_] := Module[
  {c1, c2, lcm, mul1, mul2, score},
  c1 = A[[1, colIndex]];
  c2 = A[[2, colIndex]];
  If[c1 == 0 || c2 == 0, Return[<|"Score" -> 9999|>]];
  lcm = LCM[Abs[c1], Abs[c2]];
  mul1 = lcm / Abs[c1];
  mul2 = lcm / Abs[c2];
  (* Skórovacia funkcia preferuje menší najmenší spoločný násobok (LCM) a riešenia nevyžadujúce násobenie oboch rovníc *)
  score = lcm + If[mul1 > 1 && mul2 > 1, 1000, 0];
  <|"Score" -> score, "LCM" -> lcm, "RawMul1" -> mul1, "RawMul2" -> mul2, "Coeffs" -> {c1, c2}|>
];

(* Analýza pre 3x3: hľadá najlepší stĺpec na elimináciu *)
analyzeElimination3[A_] := Module[{scores, bestCol},
  scores = Table[
    Module[{col, c1, c2, c3, s12, s13},
      col = A[[All, j]];
      {c1, c2, c3} = col;

      If[c1 == 0,
        9999,
        s12 = LCM[Abs[c1], If[c2 == 0, 1, Abs[c2]]];
        s13 = LCM[Abs[c1], If[c3 == 0, 1, Abs[c3]]];
        s12 + s13
      ]
    ],
    {j, 1, 3}
  ];

  bestCol = Ordering[scores, 1][[1]];
  bestCol
];


(* Inicializácia eliminačného procesu (2x2): výber najvhodnejšej premennej a výpočet multiplikátorov *)
eliminationStart[A_, b_, vars_] := Module[
  {content = {}, x, y, a1, b1, c1, a2, b2, c2,
    resX, resY, choice, targetVar, elimReason,
    rawM1, rawM2, m1, m2, k1, k2,
    c1x, c1y, c1rhs, c2x, c2y, c2rhs,
    rows1, rows2, needsMultiplication, preparedRows},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  (* Heuristické porovnanie náročnosti eliminácie premennej X a Y *)
  resX = analyzeVariableElimination[1, A];
  resY = analyzeVariableElimination[2, A];

  If[resY["Score"] < resX["Score"],
    choice = "Y"; targetVar = y;
    {k1, k2} = resY["Coeffs"];
    {rawM1, rawM2} = {resY["RawMul1"], resY["RawMul2"]};
    ,
    (* inak: vyberáme elimináciu X *)
    choice = "X"; targetVar = x;
    {k1, k2} = resX["Coeffs"];
    {rawM1, rawM2} = {resX["RawMul1"], resX["RawMul2"]};
  ];

  AppendTo[content, Style["1. Pr\[IAcute]prava na elimin\[AAcute]ciu - vyru\[SHacek]enie jednej premennej", Bold]];

  (* Detekcia potreby úpravy rovníc: ak sú koeficienty už opačné, násobenie nie je nutné *)
  needsMultiplication =
      !(Sign[k1] =!= Sign[k2] && rawM1 === 1 && rawM2 === 1);

  If[needsMultiplication,
    AppendTo[content,
      "Aby sme vyru\[SHacek]ili premenn\[UAcute] " <> ToString[targetVar] <>
          ", uprav\[IAcute]me rovnice n\[AAcute]soben\[IAcute]m tak, aby mali pri tejto premennej rovnak\[YAcute] koeficient s opa\[CHacek]n\[YAcute]m znamienkom."
    ],
    AppendTo[content,
      "Koeficienty pri premennej " <> ToString[targetVar] <>
          " s\[UAcute] u\[ZHacek] opa\[CHacek]n\[EAcute], preto nemus\[IAcute]me ni\[CHacek] \[ZHacek]iadnym \[CHacek]\[IAcute]slom pren\[AAcute]sobova\[THacek]. M\[OHat]\[ZHacek]eme hne\[DHacek] prejs\[THacek] na s\[CHacek]\[IAcute]tanie rovnic a ozna\[CHacek]i\[THacek] si \[CHacek]leny, ktor\[EAcute] sa vyru\[SHacek]ia."
    ]
  ];

  (* Determinácia znamienok multiplikátorov pre zabezpečenie opačných koeficientov *)
  m1 = rawM1;
  m2 = rawM2;

  If[Sign[k1] === Sign[k2],
    m2 = -m2;
  ];


  rows1 = {
    {formatLHS[a1, b1, choice], c1, multiplyNoteString[m1]},
    {formatLHS[a2, b2, choice], c2, multiplyNoteString[m2]}
  };

  (* Aplikácia multiplikátorov na rovnice *)
  c1x = m1*a1; c1y = m1*b1; c1rhs = m1*c1;
  c2x = m2*a2; c2y = m2*b2; c2rhs = m2*c2;

  rows2 = {
    {formatLHS[c1x, c1y, ""], c1rhs, ""},
    {formatLHS[c2x, c2y, ""], c2rhs, ""}
  };

  If[needsMultiplication,
    (* Štandardný postup: zobrazenie stavu pred a po úprave *)
    AppendTo[content, alignedEquationsGrouped[Join[rows1, rows2], {2}, 1]],
    (* Optimalizácia: ak netreba násobiť, preskočíme pôvodný stav a ukážeme priamo pripravené rovnice *)
    preparedRows = {
      {formatLHS[a1, b1, choice], c1, ""},
      {formatLHS[a2, b2, choice], c2, ""}
    };
    AppendTo[content, alignedEquations[preparedRows]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "failed" -> False|>
];

(* --- Kroky pre 2x2 --- *)

(* Generovanie krokov riešenia pre sústavu s jedným unikátnym riešením *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y,
    sumRHS, sumCoeffX, sumCoeffY, calcVar, calcVal, otherVar, otherVal,
    stepsY, stepsSub, valProduct, op, rhsRem, elimVarStr,
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

  AppendTo[content, Style["2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c \[Dash] z\[IAcute]skame jednu premenn\[UAcute]", Bold]];
  AppendTo[content,
    "Teraz rovnice s\[CHacek]\[IAcute]tame. Premenn\[AAcute] " <> ToString[If[elimVarStr=="X", x, y]] <>
        " sa vyru\[SHacek]\[IAcute] (zmizne), preto\[ZHacek]e m\[AAcute] v oboch rovniciach opa\[CHacek]n\[EAcute] koeficienty."];

  stepsY = {};

  (* --- Explicitná fáza: vizualizácia sčítania rovníc pred algebraickým zjednodušením --- *)
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

  (* Výpočet hodnoty premennej, ktorá zostala po eliminácii *)
  If[elimVarStr == "X",
    termResult = sumCoeffY*y;
    AppendTo[stepsY, {termResult, sumRHS, ""}];

    (* Výpis doterajších krokov do gridu a vyčistenie buffera *)
    AppendTo[content, alignedEquations[stepsY]];
    stepsY = {};

    If[sumCoeffY == 1,
      AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] sme dostali jednoduch\[UAcute] rovnicu, z ktorej hne\[DHacek] ur\[CHacek]\[IAcute]me hodnotu premennej " <> ToString[y] <> "."];
      ,
      AppendTo[content, "Zostala n\[AAcute]m rovnica s jednou premennou. Uprav\[IAcute]me ju (napr. vydelen\[IAcute]m koeficientom), aby sme dostali samotn\[UAcute] premenn\[UAcute]."];
    ];

    If[sumCoeffY == 0, Return[$Failed]];
    calcVar = y; calcVal = sumRHS / sumCoeffY; otherVar = x;

    If[sumCoeffY =!= 1,
      AppendTo[stepsY, {sumCoeffY y, sumRHS, ": " <> ToString[sumCoeffY]}];
    ];
  ];

  If[elimVarStr == "Y",
    termResult = sumCoeffX*x;
    AppendTo[stepsY, {termResult, sumRHS, ""}];

    (* Výpis doterajších krokov do gridu a vyčistenie buffera *)
    AppendTo[content, alignedEquations[stepsY]];
    stepsY = {};

    If[sumCoeffX == 1,
      AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] sme dostali jednoduch\[UAcute] rovnicu, z ktorej hne\[DHacek] ur\[CHacek]\[IAcute]me hodnotu premennej " <> ToString[x] <> "."];
      ,
      AppendTo[content, "Zostala n\[AAcute]m rovnica s jednou premennou. Uprav\[IAcute]me ju (napr. vydelen\[IAcute]m koeficientom), aby sme dostali samotn\[UAcute] premenn\[UAcute]."];
    ];

    If[sumCoeffX == 0, Return[$Failed]];
    calcVar = x; calcVal = sumRHS / sumCoeffX; otherVar = y;

    If[sumCoeffX =!= 1,
      AppendTo[stepsY, {sumCoeffX x, sumRHS, ": " <> ToString[sumCoeffX]}];
    ];
  ];

  If[Length[stepsY] > 0, AppendTo[content, alignedEquations[stepsY]]];
  AppendTo[content, highlightGrid[alignedEquations[{{calcVar, calcVal, ""}}]]];


  AppendTo[content, Style["3. Dosadenie \[Dash] vypo\[CHacek]\[IAcute]tame druh\[UAcute] premenn\[UAcute]", Bold]];
  AppendTo[content,
    "Vypo\[CHacek]\[IAcute]tan\[UAcute] hodnotu " <> ToString[calcVar] <>
        " dosad\[IAcute]me do jednej z p\[OAcute]vodn\[YAcute]ch rovn\[IAcute]c (napr. do prvej). Z\[IAcute]skame rovnicu len s druhou premennou a vyr\[AAcute]tame jej hodnotu."];

  stepsSub = {};

    AppendTo[
      stepsSub,
      {a1 x + b1 y, c1, Row[{calcVar, " \[Rule] ", TraditionalForm[Together[calcVal]]}]}
    ];

  (* Proces substitúcie: dosadenie vypočítanej hodnoty späť do vybranej rovnice *)
  If[elimVarStr == "X",
    substCoeff = a1; substConst = b1;

    explicitSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[substConst < 0, " - ", " + "],
      Abs[substConst], " \[CenterDot] ",
      If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal]
    }];

    valProduct = substConst * calcVal;

    calculatedSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[valProduct < 0, " - ", " + "],
      Abs[valProduct]
    }];
    ,
    (* inak: eliminovali sme Y, teda počítali X, teraz dosádzame za X *)
    substCoeff = b1; substConst = a1;

    explicitSubstLHS = Row[{
      substConst, " \[CenterDot] ",
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

  otherVal = rhsRem / substCoeff;

  (* normálny výsledok ešte do toho istého bloku stepsSub *)
  AppendTo[stepsSub, {otherVar, TraditionalForm[Together[otherVal]], ""}];

  (* vytlačíme celý blok naraz *)
  AppendTo[content, alignedEquations[stepsSub]];

  (* až potom zvýraznený výsledok *)
  AppendTo[content, highlightGrid[alignedEquations[{{otherVar, TraditionalForm[Together[otherVal]], ""}}]]];

  solPair = If[elimVarStr == "X", {otherVal, calcVal}, {calcVal, otherVal}];

  <|"Content" -> content, "Solution" -> solPair|>
];

(* Generovanie krokov riešenia pre prípad "žiadne riešenie" (spor) *)
stepsNone2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, sumRHS, stepsY},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  sumRHS = m1 c1 + m2 c2;

  AppendTo[content, Style["2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c \[Dash] kontrola, \[CHacek]i nevznikne spor", Bold]];
  AppendTo[content, "Rovnice s\[CHacek]\[IAcute]tame (po \[UAcute]prave z kroku 1). Sledujeme, \[CHacek]i nevznikne nemo\[ZHacek]n\[AAcute] rovnos\[THacek]."];

  stepsY = {};

  (* Blok pre explicitné zobrazenie sčítania *)
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

  (* Blok výsledku operácie *)
  AppendTo[stepsY, {0, sumRHS, ""}];

  AppendTo[content, alignedEquations[stepsY]];

  AppendTo[content, Style["3. Z\[AAcute]ver", Bold]];
  AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] vy\[SHacek]la nepravdiv\[AAcute] rovnos\[THacek] (napr. 0 = nenulov\[EAcute] \[CHacek]\[IAcute]slo). To je spor, preto s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

(* Generovanie krokov riešenia pre prípad "nekonečne veľa riešení" (identita) *)
stepsInfinite2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, stepsY},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];

  AppendTo[content, Style["2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c \[Dash] over\[IAcute]me, \[CHacek]i s\[UAcute] rovnice toto\[ZHacek]n\[EAcute]", Bold]];
  AppendTo[content, "Rovnice s\[CHacek]\[IAcute]tame (po \[UAcute]prave z kroku 1). Ak vyjde 0 = 0, znamen\[AAcute] to, \[ZHacek]e sme dostali toto\[ZHacek]n\[UAcute] rovnicu."];

  stepsY = {};

  (* Blok pre explicitné zobrazenie sčítania *)
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

  (* Blok výsledku operácie *)
  AppendTo[stepsY, {0, 0, ""}];

  AppendTo[content, alignedEquations[stepsY]];

  AppendTo[content, Style["3. Z\[AAcute]ver", Bold]];
  AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] vy\[SHacek]la pravdiv\[AAcute] rovnos\[THacek] 0 = 0. To znamen\[AAcute], \[ZHacek]e druh\[AAcute] rovnica je len n\[AAcute]sobkom prvej (opisuj\[UAcute] t\[UAcute] ist\[UAcute] priamku). S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];

  (* Poznámka: Redundantné parametrické vyjadrenie bolo odstránené, nakoľko je riešené v hlavnej funkcii Gen01 *)

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

(* --- Kroky pre 3x3 (MEDIUM) --- *)

(* Pomocná funkcia na elimináciu jednej premennej z dvojice rovníc 3D *)
(* Vráti {koeficienty novej rovnice, obsah krokov pre túto dvojicu} *)
reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_, _, _] := Module[
  {content = {}, valA, valB, lcm, m1, m2, newRow, newRHS, rowsDisp, elimVarName, choiceStr},

  valA = rowA[[elimCol]];
  valB = rowB[[elimCol]];
  elimVarName = vars[[elimCol]];
  choiceStr = {"X", "Y", "Z"}[[elimCol]];

  (* Ak už jedna z rovníc nemá eliminovanú premennú, netreba nič kombinovať (vyhneš sa 1/0) *)
  If[valA == 0 || valB == 0,
    rowsDisp = {
      {formatLHS3[rowA[[1]], rowA[[2]], rowA[[3]], choiceStr], rhsA, ""},
      {formatLHS3[rowB[[1]], rowB[[2]], rowB[[3]], choiceStr], rhsB, ""}
    };
    AppendTo[content, alignedEquations[rowsDisp]];

    (* vyberieme tú rovnicu, ktorá už eliminovanú premennú nemá *)
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

  (* Znamienka *)
  If[Sign[valA] == Sign[valB],
    m2 = -m2; (* musia mať opačné znamienko *)
  ];

  rowsDisp = {
    {formatLHS3[rowA[[1]], rowA[[2]], rowA[[3]], choiceStr], rhsA, multiplyNoteString[m1]},
    {formatLHS3[rowB[[1]], rowB[[2]], rowB[[3]], choiceStr], rhsB, multiplyNoteString[m2]}
  };
  AppendTo[content, alignedEquations[rowsDisp]];

  (* Výpočet novej rovnice *)
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

(* Kroky riešenia 3x3 - ONE *)
stepsOne3[A_, b_, vars_] := Module[
  {content = {}, elimVarStr, elimCol, remVars, remCols,
    resPair1, resPair2,
    rowIV, rhsIV, rowV, rhsV,
    subSteps, sol2x2, xVal, yVal, zVal,
    eqSubst, finalVar, finalVal, solMap, A2, b2, solFull},

  AppendTo[content, Style["1. Redukcia s\[UAcute]stavy 3\[Times]3 na 2\[Times]2", Bold]];
  elimCol = analyzeElimination3[A];
  elimVarStr = vars[[elimCol]];

  AppendTo[content,
    "Vyl\[UAcute]\[CHacek]ime premenn\[UAcute] " <> ToString[elimVarStr] <>
      ". Pou\[ZHacek]ijeme na to dve dvojice rovn\[IAcute]c, napr\[IAcute]klad prv\[UAcute] s druhou a prv\[UAcute] s tretou."
  ];

    (* --- REDUKCIA 3×3 -> 2×2 s ošetrením rovnice bez vyrušovanej premennej --- *)
    Module[
      {zeroRows, nonZeroRows, twoCombosQ, iKeep, pair, i1, i2},

      (* riadky, kde je vyrušovaná premenná už nulová *)
      zeroRows    = Flatten @ Position[A[[All, elimCol]], 0];
      nonZeroRows = Complement[Range[3], zeroRows];

      (* Ak existuje rovnica bez vyrušovanej premennej, stačí iba 1 kombinácia.
         Inak spravíme 2 kombinácie ako doteraz. *)
      If[Length[zeroRows] >= 1 && Length[nonZeroRows] >= 2,

        twoCombosQ = False;
        iKeep = First[zeroRows];

        (* vyberieme najlepší pár z rovníc, ktoré vyrušovanú premennú majú *)
        pair = pickBestElimPair[nonZeroRows, elimCol];
        {i1, i2} = pair;

        AppendTo[content, Style[
          "a) Kombin\[AAcute]cia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:",
          Italic
        ]];

        resPair1 = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars, "I", "II"];
        content  = Join[content, resPair1["Content"]];
        rowIV    = resPair1["Row"]; rhsIV = resPair1["RHS"];

        (* druhá rovnica do 2×2 je priamo tá, ktorá už neobsahuje vyrušovanú premennú *)
        rowV  = A[[iKeep]];
        rhsV  = b[[iKeep]];

        AppendTo[content, Style[
          "b) Rovn\[IAcute]ca bez vyru\[SHacek]ovanej premennej (pou\[ZHacek]ijeme ju priamo):",
          Italic
        ]];

        AppendTo[content, alignedEquations[{
          {formatLHS3[rowV[[1]], rowV[[2]], rowV[[3]], ""], rhsV, ""}
        }]];

        ,
        (* fallback: klasický prípad – dve kombinácie (ako doteraz) *)
        twoCombosQ = True;

        AppendTo[content, Style["a) Kombin\[AAcute]cia 1. a 2. rovnice:", Italic]];
        resPair1 = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "I", "II"];
        content = Join[content, resPair1["Content"]];
        rowIV = resPair1["Row"]; rhsIV = resPair1["RHS"];

        AppendTo[content, Style["b) Kombin\[AAcute]cia 1. a 3. rovnice:", Italic]];
        resPair2 = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "I", "III"];
        content = Join[content, resPair2["Content"]];
        rowV = resPair2["Row"]; rhsV = resPair2["RHS"];
      ];

      (* Krok 2 – text podľa toho, či sme robili dve kombinácie alebo jednu *)
      AppendTo[content, Style["2. Rie\[SHacek]enie vzniknutej s\[UAcute]stavy 2\[Times]2", Bold]];

      If[twoCombosQ,
        AppendTo[content, "Dostali sme dve nov\[EAcute] rovnice s dvoma nezn\[AAcute]mymi."],
        AppendTo[content,
          "Z jednej dvojice rovnic sme elimin\[AAcute]ciou z\[IAcute]skali jednu nov\[UAcute] rovnicu a druh\[AAcute] rovnica bola u\[ZHacek] v zadan\[IAcute] bez vyru\[SHacek]ovanej premennej. Spolu tvoria s\[UAcute]stavu 2\[Times]2."
        ]
      ];
    ];

  (* Získanie 2x2 systému *)
  AppendTo[content, Style["2. Rie\[SHacek]enie vzniknutej s\[UAcute]stavy 2\[Times]2", Bold]];
  AppendTo[content, "Dostali sme dve nov\[EAcute] rovnice s dvoma nezn\[AAcute]mymi."];

  (* Extrakcia 2x2 matice *)
  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];

  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  AppendTo[content, alignedEquations[{
    {formatLHS[A2[[1, 1]], A2[[1, 2]], ""], b2[[1]], ""},
    {formatLHS[A2[[2, 1]], A2[[2, 2]], ""], b2[[2]], ""}
  }]];

  (* Reuse 2x2 logic *)
  sol2x2 = stepsOne2[A2, b2, remVars];
  If[sol2x2 === $Failed, Return[$Failed]]; (* Nemalo by sa stať pre ONE *)

  (* Pripojíme kroky z 2x2, ale mierne odsadené alebo len vložené *)
  content = Join[content, sol2x2["Content"]];

  (* Mapovanie výsledkov *)
  solMap = AssociationThread[remVars -> sol2x2["Solution"]];

  AppendTo[content, Style["3. Dosadenie do p\[OAcute]vodnej rovnice", Bold]];
  AppendTo[content,
    "Vypo\[CHacek]\[IAcute]tan\[EAcute] premenn\[EAcute] dosad\[IAcute]me napr\[IAcute]klad do prvej rovnice a vypo\[CHacek]\[IAcute]tame posledn\[UAcute] nezn\[AAcute]mu."
  ];

  (* Dosadenie do R1 - rovnaký štýl ako v stepsOne2 (subSteps + alignedEquations na konci) *)
  finalVar = vars[[elimCol]];
  eqSubst = formatLHS3[A[[1, 1]], A[[1, 2]], A[[1, 3]], ""];

  subSteps = {};

  (* (I) pôvodná rovnica + poznámka, čo dosádzame *)
  AppendTo[subSteps, {eqSubst, b[[1]], substNote[solMap, remVars, A[[1]], vars]}];

  (* dosadenie: napr. -4·(-3) + 2·2 + z = 21 (poradie zostáva) *)
  AppendTo[subSteps, {formatSubstLHS3[A[[1]], vars, solMap, finalVar], b[[1]], ""}];

  Module[{row, rhs, coeffU, knownSum, rhsShifted, noteShift, lhsUnknown},
    row = A[[1]];
    rhs = b[[1]];
    coeffU = row[[elimCol]];

    (* súčet známych členov (bez neznámej) *)
    knownSum = Together @ Total @ Table[
      If[i == elimCol, 0, row[[i]] * solMap[vars[[i]]]],
      {i, 1, Length[vars]}
    ];

    (* presun známych členov na pravú stranu *)
    rhsShifted = Together[rhs - knownSum];

    (* poznámka ako v 2×2: - 16 / + 16 (podľa znamienka knownSum) *)
    noteShift = Which[
      PossibleZeroQ[knownSum], "",
      TrueQ[knownSum > 0],     Row[{"- ", TraditionalForm[knownSum]}],
      True,                    Row[{"+ ", TraditionalForm[Abs[knownSum]]}]
    ];

    (* roznásobenie: napr. 12 + 4 + z = 21 (poradie zostáva) *)
    AppendTo[subSteps, {formatSubstLHS3Eval[A[[1]], vars, solMap, finalVar], b[[1]], noteShift}];

    (* ľavá strana len s neznámou: coeffU*finalVar *)
    lhsUnknown = If[coeffU === 1, finalVar, coeffU finalVar];

    If[coeffU === 1,
      (* netreba deliť – rovno normálny výsledok *)
      AppendTo[subSteps,
        {TraditionalForm[finalVar], TraditionalForm[rhsShifted], ""}
      ],
      (* treba deliť – najprv riadok s delením... *)
      AppendTo[subSteps,
        {TraditionalForm[coeffU finalVar], TraditionalForm[rhsShifted], ": " <> ToString[coeffU]}
      ];

      (* ...potom normálny riadok po vydelení *)
      AppendTo[subSteps,
        {TraditionalForm[finalVar], TraditionalForm[Together[rhsShifted/coeffU]], ""}
      ];
    ];

    finalVal = Together[rhsShifted/coeffU];

  ];

  (* vytlačíme všetko naraz *)
  AppendTo[content, alignedEquations[subSteps]];

  (* zvýraznený výsledok ako v 2×2 *)
  AppendTo[content, highlightGrid[alignedEquations[{{finalVar, TraditionalForm[finalVal], ""}}]]];

  (* Celkové riešenie v správnom poradí *)
  solFull = Table[If[i == elimCol, finalVal, solMap[vars[[i]]]], {i, 1, 3}];

  <|"Content" -> content, "Solution" -> solFull|>
];

(* Kroky riešenia 3x3 - NONE *)
stepsNone3[A_, b_, vars_] := Module[
  {content={}, elimVarStr, elimCol, resPair1, resPair2, rowIV, rhsIV, rowV, rhsV, remCols, A2, b2, sol2x2},

  AppendTo[content, Style["1. Redukcia s\[UAcute]stavy 3\[Times]3 na 2\[Times]2", Bold]];
  elimCol = analyzeElimination3[A];
  elimVarStr = vars[[elimCol]];

  resPair1 = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "I", "II"];
  content = Join[content, resPair1["Content"]];
  rowIV = resPair1["Row"]; rhsIV = resPair1["RHS"];

  resPair2 = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "I", "III"];
  content = Join[content, resPair2["Content"]];
  rowV = resPair2["Row"]; rhsV = resPair2["RHS"];

  AppendTo[content, Style["2. Rie\[SHacek]enie vzniknutej s\[UAcute]stavy 2\[Times]2", Bold]];
  remCols = Delete[Range[3], elimCol];
  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  sol2x2 = stepsNone2[A2, b2, vars[[remCols]]];
  If[sol2x2 === $Failed,
    (* Ak náhodou spor vznikol už pri redukcii (napr 0=5), zachytíme to *)
    AppendTo[content, "Po \[UAcute]prav\[AAcute]ch dost\[AAcute]vame sporn\[UAcute] rovnicu (napr. 0 = k)."];
    ,
    content = Join[content, sol2x2["Content"]];
  ];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

(* Kroky riešenia 3x3 - INFINITE *)
stepsInfinite3[A_, b_, vars_] := Module[
  {content={}, elimVarStr, elimCol, resPair1, resPair2, rowIV, rhsIV, rowV, rhsV, remCols, A2, b2, sol2x2},

  AppendTo[content, Style["1. Redukcia s\[UAcute]stavy 3\[Times]3 na 2\[Times]2", Bold]];
  elimCol = analyzeElimination3[A];
  elimVarStr = vars[[elimCol]];


  resPair1 = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "I", "II"];
  content = Join[content, resPair1["Content"]];
  rowIV = resPair1["Row"]; rhsIV = resPair1["RHS"];

  resPair2 = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "I", "III"];
  content = Join[content, resPair2["Content"]];
  rowV = resPair2["Row"]; rhsV = resPair2["RHS"];

  AppendTo[content, Style["2. Rie\[SHacek]enie vzniknutej s\[UAcute]stavy 2\[Times]2", Bold]];
  remCols = Delete[Range[3], elimCol];
  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  sol2x2 = stepsInfinite2[A2, b2, vars[[remCols]]];

  If[sol2x2 === $Failed,
    (* Fallback ak by 2x2 nebola infinite (napr 3x3 bola závislá, ale redukcia niečo pokazila) *)
    AppendTo[content, "S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
    ,
    content = Join[content, sol2x2["Content"]];
  ];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

(* Placeholder pre HARD kroky *)
stepsHard3[args___] := $Failed;


(* Grafická vizualizácia sústavy dvoch lineárnych rovníc s dvoma neznámymi v 2D rovine *)
visualize2[A_, b_, vars_, sol_] := Module[
  { x, y, pt, subtitle, xrange, yrange, lineSeg, seg1, seg2, g, col1, col2, legend1, legend2, center, half, labelOffset},

  {x, y} = vars;

  half = 10;
  labelOffset = {0, 1.3};

  (* Dynamické určenie rozsahu grafu a titulku na základe typu riešenia *)
  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    xrange = center[[1]] + {-half, half};
    yrange = center[[2]] + {-half, half};
    xrange = {Min[xrange[[1]], 0], Max[xrange[[2]], 0]};
    yrange = {Min[yrange[[1]], 0], Max[yrange[[2]], 0]};

    subtitle = Row[{
      "V grafe s\[UAcute] zobrazen\[EAcute] obe priamky. Ich priese\[CHacek]n\[IAcute]k je rie\[SHacek]en\[IAcute]m s\[UAcute]stavy a je vyzna\[CHacek]en\[YAcute] v bode ",
      "[", TraditionalForm[Together[pt[[1]]]], ", ",
      TraditionalForm[Together[pt[[2]]]], "]."
    }];,

    pt = None;
    center = {0, 0};
    xrange = {-10, 10};
    yrange = {-10, 10};

    subtitle = If[sol === "NONE",
      "Priamky s\[UAcute] rovnobe\[ZHacek]n\[EAcute], nepret\[IAcute]naj\[UAcute] sa \[Dash] s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie.",
      "Priamky s\[UAcute] toto\[ZHacek]n\[EAcute] (prekr\[YAcute]vaj\[UAcute] sa) \[Dash] s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."
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
                Row[{"[", TraditionalForm[Together[pt[[1]]]], ", ",
                  TraditionalForm[Together[pt[[2]]]], "]"}],
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

(* Vizualizácia pre 3x3 (3D) *)
visualize3[A_, b_, vars_, sol_] := Module[
  {pt, subtitle, x, y, z, range = {-10, 10}, planes},
  {x, y, z} = vars;

  If[MatchQ[sol, {_?NumericQ, _?NumericQ, _?NumericQ}],
    pt = sol;
    subtitle = Row[{
      "V grafe s\[UAcute] zobrazen\[EAcute] tri roviny. Ich priese\[CHacek]n\[IAcute]k je rie\[SHacek]en\[IAcute]m s\[UAcute]stavy a je vyzna\[CHacek]en\[YAcute] v bode ",
      "[", TraditionalForm[Together[pt[[1]]]], ", ", TraditionalForm[Together[pt[[2]]]], ", ", TraditionalForm[Together[pt[[3]]]], "]."
    }];
    ,
    pt = None;
    subtitle = If[sol === "NONE",
      "Roviny nemaj\[UAcute] spolo\[CHacek]n\[YAcute] prienik v\[SHacek]etk\[YAcute]ch troch naraz \[Dash] s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie.",
      "Roviny maj\[UAcute] spolo\[CHacek]n\[YAcute] prienik (priamku alebo rovinu) \[Dash] s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."
    ];
  ];

  CellText[subtitle];

  (* Definícia rovín pomocou ContourPlot3D *)
  planes = ContourPlot3D[
    {
      A[[1]] . vars == b[[1]],
      A[[2]] . vars == b[[2]],
      A[[3]] . vars == b[[3]]
    },
    {x, range[[1]], range[[2]]},
    {y, range[[1]], range[[2]]},
    {z, range[[1]], range[[2]]},
    Mesh -> None,
    ContourStyle -> {
      Directive[Red, Opacity[0.3]],
      Directive[Green, Opacity[0.3]],
      Directive[Blue, Opacity[0.3]]
    },
    AxesLabel -> {"x", "y", "z"}
  ];

  If[pt =!= None,
    CellBox @ Show[
      planes,
      Graphics3D[{Black, PointSize[0.03], Point[pt]}],
      ImageSize -> Medium
    ],
    CellBox @ Show[planes, ImageSize -> Medium]
  ];
];

(* Implementácia hlavnej riadiacej funkcie generátora, ktorá koordinuje výber typu úlohy, generovanie dát a následný výstup *)
Gen01[diff_String, mode_String, opts : OptionsPattern[]] :=
    Module[{dim, vars, st, gen, data, A, b, steps, sol},

      If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff],
        Message[Gen01::baddiff, diff]; Return[$Failed]
      ];
      If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode],
        Message[Gen01::badmode, mode]; Return[$Failed]
      ];

      (* HARD zatiaľ vracia notimpl *)
      If[diff === "HARD", Message[Gen01::notimpl, diff]; Return[$Failed]];

      st = Replace[OptionValue[SolutionType], {
        Automatic -> RandomChoice[{"ONE", "ONE", "ONE", "ONE", "NONE", "INFINITE"}],
        s_ :> s
      }];

      (* Explicitné nastavenie dimenzie podľa diff *)
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

      (* Pokus o vygenerovanie "pekného" zadania s opakovaním pre zabezpečenie vhodných koeficientov *)
      data = WithRetries[Function[Null, gen], 200];
      If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

      A = data["A"]; b = data["b"];

      CellSection["S\[CHacek]\[IAcute]tavacia (elimina\[CHacek]n\[AAcute]) met\[OAcute]da"];
      CellSubsection["Zadanie"];

      CellText[
        "Vyrie\[SHacek]te nasleduj\[UAcute]cu s\[UAcute]stavu line\[AAcute]rnych rovn\[IAcute]c s\[CHacek]\[IAcute]tacou (elimina\[CHacek]nou) met\[OAcute]dou."
      ];

      (* Výpis zadania: rozlíšime 2D a 3D formatLHS *)
      If[dim == 2,
        CellBox @ alignedEquations[
          Table[{formatLHS[A[[i, 1]], A[[i, 2]], ""], b[[i]], ""}, {i, Length[b]}]
        ],
        CellBox @ alignedEquations[
          Table[{formatLHS3[A[[i, 1]], A[[i, 2]], A[[i, 3]], ""], b[[i]], ""}, {i, Length[b]}]
        ]
      ];

      If[mode === "TASK",
        Return[<|"A" -> A, "b" -> b, "vars" -> vars|>]
      ];

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

      CellSubsection["V\[YAcute]sledok"];

      Switch[sol,
        "NONE",
        CellText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie (pri s\[CHacek]\[IAcute]tan\[IAcute] vznikol spor)."];
        Null,

        "INFINITE",
        CellText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]. Rie\[SHacek]enia zap\[IAcute]\[SHacek]eme pomocou parametra."];

        If[dim == 2,
          Module[
            {par, exprX, exprY, a1 = A[[1, 1]], b1 = A[[1, 2]], c1 = b[[1]], baseEq, solvedEq},
            par = \[FormalT];

            CellText["Vyjadr\[IAcute]me jednu premenn\[UAcute] z jednej rovnice (napr. y vyjadr\[IAcute]me pomocou x)."];

            If[b1 =!= 0,
              (* vyjadríme y z a1 x + b1 y = c1 *)
              baseEq   = a1 x + b1 y;
              solvedEq = Simplify[(c1 - a1 x)/b1];

              CellBox @ alignedEquations[{{baseEq, c1, ""}}];
              CellBox @ alignedEquations[{{y, solvedEq, ""}}];

              CellText["Zvol\[IAcute]me parameter (vo\:013en\[AAcute] hodnota):"];
              CellBox @ Grid[
                {{x, "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}},
                Alignment -> {{Right, Center, Left, Center, Left, Left}},
                Spacings -> {0.6, 0.8}
              ];

              CellText["Dosad\[IAcute]me parameter a dostaneme tvar pre druh\[UAcute] premenn\[UAcute]:"];
              CellBox @ alignedEquations[{{y, Simplify[solvedEq /. x -> par], ""}}];
              ,
              (* b1 == 0 -> vyjadríme x z a1 x = c1 *)
              baseEq   = a1 x;
              solvedEq = Simplify[c1/a1];

              CellBox @ alignedEquations[{{baseEq, c1, ""}}];
              CellBox @ alignedEquations[{{x, solvedEq, ""}}];

              CellText["Zvol\[IAcute]me parameter (vo\:013en\[AAcute] hodnota):"];
              CellBox @ Grid[
                {{y, "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}},
                Alignment -> {{Right, Center, Left, Center, Left, Left}},
                Spacings -> {0.6, 0.8}
              ];
              CellText["V tomto pr\[IAcute]pade vy\[SHacek]lo x ako kon\[SHacek]tanta a premenn\[AAcute] y m\[OAcute]\[ZHacek]e by\[THacek] \:013eubovo\:013en\[AAcute] (parameter)."];
            ];

            If[b1 =!= 0,
              exprX = par;
              exprY = Simplify[(c1 - a1*par)/b1];
              ,
              exprY = par;
              exprX = Simplify[c1/a1];
            ];

            (* Parametrické vyjadrenie *)
            CellBox @ Grid[
              {
                {x, "=", TraditionalForm[exprX]},
                {y, "=", TraditionalForm[exprY]}
              },
              Alignment -> {{Right, Center, Left}},
              Spacings -> {0.6, 0.8}
            ];

            (* Zápis množiny K *)
            CellPrint @ Cell[
              BoxData @ FormBox[
                RowBox[{
                  StyleBox["K", FontSlant -> "Italic"],
                  "=",
                  RowBox[{"{",
                    RowBox[{
                      RowBox[{"[",
                        RowBox[{
                          ToBoxes[exprX, TraditionalForm], ";", " ",
                          ToBoxes[exprY, TraditionalForm]
                        }],
                        "]"}],
                      " ", "\[VerticalSeparator]", " ",
                      RowBox[{
                        ToBoxes[par, TraditionalForm],
                        "\[Element]",
                        "\[DoubleStruckR]"
                      }]
                    }],
                    "}"}]
                }],
                TraditionalForm
              ],
              "DisplayFormula",
              BaseStyle -> {FontSize -> 14}
            ];
          ];
        ];

        If[dim == 3,
          (* Zjednodušený výpis pre 3D Infinite *)
          CellText["(Parametrick\[EAcute] vyjadrenie pre 3\[Times]3 v tomto bal\[IAcute]ku zatia\:013e nie je plne podporovan\[EAcute], ale s\[UAcute]stava m\[AAcute] rie\[SHacek]enie z\[AAcute]visl\[EAcute] na parametroch.)"];
        ];
        ,

        _,
        CellPrint @ Cell[
          BoxData @ ToBoxes[
            If[dim == 2,
              Row[{
                "Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy rovn\[IAcute]c je usporiadan\[AAcute] dvojica \[CHacek]\[IAcute]sel ",
                Style[
                  Row[{"[", TraditionalForm[Together[sol[[1]]]], ", ", TraditionalForm[Together[sol[[2]]]], "]"}],
                  Bold
                ]
              }],
              (* 3D *)
              Row[{
                "Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy rovn\[IAcute]c je usporiadan\[AAcute] trojica \[CHacek]\[IAcute]sel ",
                Style[
                  Row[{"[", TraditionalForm[Together[sol[[1]]]], ", ", TraditionalForm[Together[sol[[2]]]], ", ", TraditionalForm[Together[sol[[3]]]], "]"}],
                  Bold
                ]
              }]
            ],
            TraditionalForm
          ],
          "Text",
          ShowStringCharacters -> False
        ];

        If[OptionValue[Visualization],
          If[dim == 2, visualize2[A, b, vars, sol]];
          If[dim == 3, visualize3[A, b, vars, sol]];
        ];

        Null
      ];
    ];
End[];
EndPackage[];