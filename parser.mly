%{
open Error
open Ktype
%}

/* keywords */
%token <Error.info> TYPE

/* symbolic tokens */

%token <Error.info> COL
%token <Error.info> SCOL
%token <Error.info> LBRA
%token <Error.info> RBRA
%token <Error.info> LPAR
%token <Error.info> RPAR
%token <Error.info> LBR
%token <Error.info> RBR
%token <Error.info> COM 
%token <Error.info> DOT 
%token <Error.info> EQ 
%token <Error.info> ABS
%token <Error.info> PRE
%token <Error.info> PERCENT
%token <Error.info> ARROW
%token <Error.info> BARROW
%token <Error.info> EOF

/* identifiers and constant values */ 
%token <string Error.withinfo> VAR
%token <string Error.withinfo> NAME 
%token <string Error.withinfo> UNAME 

%start main
%type <Ktype.t> ktype
%type <string Error.withinfo> name
%type <Ktype.constraints> main
%% 

main: 
types constraints EOF { { compdecl = $1.v ; consts = $2.v } }
;
  
ktype:
    name                        { TBase $1.v }
|   LBR row BARROW row RBR       { TRecord ($2,$4)  }
|   LBR row RBR                  { TRecord ( TAbsR, $2 )  }
|   VAR                         { TVr $1.v }
;

row:
     ABS                        { TAbsR }
|    VAR                       { TVr $1.v }
|    name COL field SCOL row    { TRow ($1.v, $3, $5) }
;

field:
     ABS                        { TAbsF }
|    PRE LPAR ktype RPAR        { TPres $3 }
|    NAME                      { TBase $1.v }
|    LBR row BARROW row RBR     { TRecord ($2, $4) }
|    VAR                       { TVr $1.v }
;

listassoc:
                                { [] }
|   name COL ktype              { ($1.v, $3) :: [] }
|   name COL ktype SCOL listassoc
                                { ($1.v, $3) :: $5 }
;

comptype:
     LBR listassoc RBR ARROW LBR listassoc RBR 
        { {input = $2 ; output = $6 } }
;

types:
                              { { i = dummyinfo ; v = [] } }
|    name COL comptype types  { { i = $1.i ; v = ($1.v, $3) :: $4.v }  }
;

constraints:
  { { i = dummyinfo ; v = [] }  }
|  PERCENT consts     { $2 }
;
	
consts:
                      { { i = dummyinfo ; v = [] }  }
|    name DOT name EQ name DOT name consts
      {  { i = $1.i ; v = ($1.v, $3.v, $5.v, $7.v) :: $8.v } }


name:
	NAME		  { $1 }
|	UNAME		  { $1 }
;


