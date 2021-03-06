{
  open Parser
}
rule token = parse
  | eof { EOF }
  | [' ' '\t' '\n'] { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "+" { ADD }
  | "-" { SUB }
  | "fun" { FUN }
  | "->" { ARROW }
  | "=" { EQUAL }
  | "let" { LET }
  | "in" { IN }
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']+ { IDENT (Lexing.lexeme lexbuf) }
