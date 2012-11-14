%{



%}


%token in
%token let
%token ident
%token rec
%token and
%token ref
%token array
%token bool
%token boolean
%token int
%token integer
%token string
%token Array.make
%token IF
%token THEN
%token ELSE
%token WHILE
%token done
%token DO
%token begin
%token end
%token print_int
%token print_string
%token read_int
%token not

%token EXEC
%token EINS
%token CPAR
%token OPAR
%token UMOIN
%token PLUS
%token MOIN
%token FOIS
%token DIV
%token INF
%token INFE
%token SUP
%token SUPE
%token EGAL
%token DIFF
%token ET
%token OU
%token AFFECT
%token TYPAGE
%token AFFECTTAB
%token POINT
%token MATCH
%token VALVAR 
%left AFFECT
%left INF INFE SUP SUPE EGAL DIFF ET OU
%left MOIN PLUS
%left FOIS DIV
%left UMOIN not








%start program

%%


program : decllist instr EXEC
        ;


decllist :
    | decl in decllist
    ;

vardecllist :
    | vardecl in vardecllist
    ;

decl: vardecl
    | fundecl
    ;

vardecl: let ident TYPAGE type EGAL valcst
        | let ident TYPAGE atomictype ref EGAL ref atomiccst
        ;

fundecl : let fundef
    | let rec fundefs
    ;

fundefs : fundef
    | fundef and fundefs
    ;

fundef :  ident arglist EGAL vardecllist instr
        |  ident arglist TYPAGE atomictype EGAL vardecllist instr EINS expr
        |  ident arglist TYPAGE atomictype EGAL vardecllist expr
        ;

type : atomictype
        | atomictype array
        ;

atomictype : bool
        | int
        ;

valcst : atomiccst
        | Array.make integer atomiccst
        ;

atomiccst : integer
        | OPAR UMOIN integer CPAR
        | boolean
        ;

arglist : OPAR CPAR
        | arglist1
        ;

arglist1 : OPAR arg CPAR
        | OPAR arg CPAR arglist1
        ;

arg : ident TYPAGE type
    |  ident TYPAGE atomictype ref
    ;

other : 
   WHILE expr DO sequence done

     |   begin sequence end    MATCH    begin end

     |   ident AFFECT expr

     |   ident POINT OPAR expr CPAR AFFECTTAB expr

     |   ident exprlist   MATCH    ident OPAR CPAR
			 
     |   print_int expr

     |   print_string string
     ;
instr :
	instr_close
     |	instr_ouverte
;
instr_close :
	IF expr THEN instr_close ELSE instr_close
	| other
;
instr_ouverte :
	IF expr THEN instr
	| IF expr THEN instr_close ELSE instr_ouverte
;


sequence : instr
    | sequence EINS instr
    ;

expr :
        integer
        | boolean
        | OPAR expr CPAR
        | expr opb expr
        | opu expr %prec UMOIN
        | IF expr THEN expr ELSE expr
        | ident exprlist
	| ident OPAR CPAR
	| ident
        | read_int OPAR CPAR
	| VALVAR ident
        ;
exprlist :
        expr
        | exprlist expr
        ;

opb :
    PLUS
    | MOIN
    | FOIS
    | DIV
    | INF
    | INFE
    | SUP
    | SUPE
    | EGAL
    | DIFF
    | ET
    | OU
    ;

opu :
    UMOIN
    | not
    ;
