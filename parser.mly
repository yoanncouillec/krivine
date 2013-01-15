%token<int> INT
%token<string> IDENT
%token FUN ARROW LET IN ADD SUB LPAREN RPAREN EQUAL EOF
%left ADD SUB
%start start
%type <Machine.expression> start

%%

start: 
| expression EOF { $1 }

expression:
| LPAREN expression RPAREN { $2 }
| INT { Machine.Integer ($1) }
| IDENT { Machine.Var ($1) }
| expression ADD expression { Machine.Add ($1, $3) }
| expression SUB expression { Machine.Sub ($1, $3) }
| FUN IDENT ARROW expression { Machine.Lambda ($2, $4) }
| LET IDENT EQUAL expression IN expression { Machine.Let ($2, $4, $6) }
| expression expression { Machine.App ($1, $2) }

