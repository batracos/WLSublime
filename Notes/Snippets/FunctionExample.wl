(*
*******************************************************************************
					MySharpen
*******************************************************************************
*)

(* Function definition pipeline for the symbol SharpenNew *)

ClearAll[SharpenNew]


(* Define eventual options for the function *)

Options[MySharpen] = {
	ColorSpace -> Automatic,
	Padding -> "Reversed"
} // SortBy[ToString @* First];


(* Define eventual hidden options for the function *)

MySharpenHiddenOptions = {
	Method -> "RGB"
} // SortBy[ToString @* First];


(* Define a basic wrapper that performs early syntax checking and deals with the evaluation semantics *)

MySharpen[s___] :=
Block[
	{
		a, args, opts, result,
		$FunctionName, $AllowFailure,
		GetOption, Fail
	},

	a = System`Private`Arguments[
		MySharpen[s],  (* input pattern *)
		{1, 4}, (* arguments number *)
		List,   (* arguments and options wrapper *)
		MySharpenHiddenOptions
	];

	(
		$FunctionName = MySharpen; (* available in the function's scope *)
		$AllowFailure = False; (* set to True to allow a failure output *)
		{args, opts} = a;

		With[
			{o = opts, f = $FunctionName},
			(* utilitiy to query options *)
			GetOption[name_] := OptionValue[{f, MySharpenHiddenOptions}, o, name];
					(* utilitiy to abort upon error *)
			Fail[name_, values___] := (
				Message[MessageName[f, name], values];
				Throw[$Failed, f]
			);
			Fail[] := (
				Throw[$Failed, f]
			);
		];

		result = Catch[iMySharpen[args, opts], $FunctionName];

		result /; !FailureQ[result] || $AllowFailure
	) /; Length[a] == 2

]


(* Interface *)

(* Define the interface function *)

iMySharpen[args_, opts_] := 
Module[
	{img, strenght, radius, threshold, method, res, L, a, b},

	(* argument parsing *)
	If[
		!Image`Utilities`SetImage2D[img, args[[1]]],
		Fail["imginv", args[[1]]]
	];
	If[Length[args] > 1,
		If[
			Internal`RealValuedNumericQ[args[[2]]] && args[[2]] >= 0,

			strenght = Clip[args[[2]], {0, Infinity}],

			Fail[]
		],

		strenght = 1/3
	];
	If[Length[args] > 2,
		If[
			Internal`RealValuedNumericQ[args[[3]]] && args[[3]] >= 0,

			radius = Clip[Round[args[[3]]], {0, Developer`$MaxMachineInteger}],

			Fail[]
		],
		radius = 1
	];
	If[Length[args] > 3,
		If[
			Internal`RealValuedNumericQ[args[[4]]] && 0 <= args[[4]] <= 1,

			threshold = Clip[args[[4]], {0, 1}],

			Fail[]
		],
		threshold = 0
	];

	Switch[
		method = GetOption[Method],

		"RGB",
		res = oMySharpen[Image[img, "Real32"], strenght, radius, threshold, GetOption[Padding]],

		"Luminance",
		{L, a, b} = ColorSeparate[img, "LAB"];
		L = oMySharpen[L, strenght, radius, threshold, GetOption[Padding]];
		res = ColorCombine[{L, a, b}, "LAB"],

		_,
		Fail["method", method]
	];
	
	Image`CopyOptions[img, res, ColorSpace -> "FromTarget"]

]


(* Main function *)

(* This function performs the actual operation *)

oMySharpen[i_, strenght_, radius_, threshold_, padding_] :=
With[
	{detail = ImageSubtract[i, GaussianFilter[i, radius, Padding -> padding]]},
	ImageAdd[
		i,
		ImageMultiply[
			Threshold[detail, {"SmoothGarrote", Max@Abs[detail]threshold, 5}],
			strenght
		]
	]
]

