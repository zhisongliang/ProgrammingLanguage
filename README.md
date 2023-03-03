# ProgrammingLanguage

Current version of IKEU

Definitions:

Syntax of IKEU
The concrete syntax of the IKEU5 language with these additional features can be captured with the following EBNF:

Expr	=       |   Num
              |	 	id
              |	 	String
              |	 	{if Expr then Expr else Expr}
              |	 	{let {[id = Expr] ...} Expr}
              |	 	{{id ...} : Expr}
              |	 	{Expr Expr ...}
... where an id is not let, =, if, then, else, or :.

