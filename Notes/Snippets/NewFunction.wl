(*
*******************************************************************************
					MyFun
*******************************************************************************
*)

Options[MyFun] = {
	
} // SortBy[ToString @* First];

MyFunHiddenOptions = {
	
} // SortBy[ToString @* First];

MyFun[s___] :=
Block[
	{
		a, args, opts, result,
		$FunctionName, $AllowFailure,
		GetOption, Fail
	},

	a = System`Private`Arguments[
		MyFun[s],  (* input pattern *)
		{1, 2}, (* arguments number *)
		List,   (* arguments and options wrapper *)
		MyFunHiddenOptions
	];

	(
		$FunctionName = MyFun; (* available in the function's scope *)
		$AllowFailure = False; (* set to True to allow a failure output *)
		{args, opts} = a;

		With[
			{o = opts, f = $FunctionName},
			(* utilitiy to query options *)
			GetOption[name_] := OptionValue[{f, MyFunHiddenOptions}, o, name];
			
			(* utilitiy to abort upon error *)
			Fail[name_, values___] := (
				Message[MessageName[f, name], values];
				Throw[$Failed, f]
			);
			Fail[] := (
				Throw[$Failed, f]
			);
		];

		result = Catch[iMyFun[args, opts], $FunctionName];

		result /; !FailureQ[result] || $AllowFailure
	) /; Length[a] == 2

]

(* Interface *)

iMyFun[args_, opts_] := 
Module[
	{
		
	},

	(* Arguments parsing *)

	(* Options parsing *)

	(* Result *)
	oMyFun[___]

]

(* Main function *)

oMyFun[___] := Module[
	{},

	Success[]
]

