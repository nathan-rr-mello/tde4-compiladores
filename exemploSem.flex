%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
    yyline = 1;
  }


  public int getLine() {
      return yyline;
  }

%}

NUM = [0-9]+
NL  = \n|\r|\r\n

%%


"$TRACE_ON"  { yyparser.setDebug(true);  }
"$TRACE_OFF" { yyparser.setDebug(false); }
"$MOSTRA_TS" { yyparser.listarTS(); }

"&&" { return Parser.AND; }
"||" { return Parser.OR; }
"==" { return Parser.EQUAL; }
"!=" { return Parser.NOTEQUAL; }
">=" { return Parser.GTEQUAL; }
"<=" { return Parser.LTEQUAL; }

/* operators */
"+" | 
"=" |
">" |
"<" |
";" |
"(" |
")" |
"," |
"{" |
"}" |
"[" | 
"]"    { return (int) yycharat(0); }

{NUM}  { yyparser.yylval = new ParserVal(Integer.parseInt(yytext())); 
         return Parser.NUM; }

if { return Parser.IF;}
else { return Parser.ELSE;}
while { return Parser.WHILE;}
func { return Parser.FUNC;}
int { return Parser.INT;}
double { return Parser.DOUBLE;}
boolean { return Parser.BOOLEAN;}
void { return Parser.VOID;}
return { return Parser.RETURN;}

[a-zA-Z][a-zA-Z_0-9]* { yyparser.yylval = new ParserVal(yytext());
                     return Parser.IDENT; }

{NL}   {yyline++;}
[ \t]+ { }

.    { System.err.println("Error: unexpected character '"+yytext()+"' na linha "+yyline); return YYEOF; }






