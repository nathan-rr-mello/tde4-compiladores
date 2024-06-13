    
%{
  import java.io.*;
%}


%token IDENT, NUM, FUNC
%token IF, ELSE, WHILE, RETURN
%token INT, DOUBLE, BOOLEAN, VOID
%token EQUALOP, NOTEQUALOP, LTEOP, GTEOP, AND, OR
// EQUALOP ==, NOTEQUALOP !=, LTEOP <=, GTEOP >=

%right '='
%nonassoc '>'
%left '+'
%left AND
%left '['  

%type <sval> IDENT
%type <ival> NUM
%type <obj> Type
%type <obj> TypeOrVoid
%type <obj> Exp

%%

prog : { currentClass = ClasseID.VarGlobal; } DecList /*Main*/ ;

DecList : Decl DecList
        | 
        ;

Decl  : DeclVar
      | DeclFuncProt
     // | DeclProt
     // | DeclFunc
      ;

DeclVar : Type { currentType = (TS_entry) $1; } TArray IdList ';'
        ;

IdList  : Id ',' IdList
        | Id
        ;

Id  : IDENT   { TS_entry nodo = pesquisa($1);
                if (nodo != null) 
                    yyerror("(sem) variavel >" + $1 + "< jah declarada");
                else ts.insert(new TS_entry($1, currentType, currentClass)); 
              }  
    ;

TArray : '[' NUM ']'  TArray { currentType = new TS_entry("?", Tp_ARRAY, 
                                                   currentClass, $2, currentType); }
          
       |
       ;

// colocar o nome da funcao na tabela de simbolos com os parametros e o tipo de retorno
/* DeclProt  : FUNC TypeOrVoid IDENT { openScope(); } '(' FormalPar ')' ';'
              {
                //criar funcao que valida declarao com base no prototipo
                TS_entry nodo = new TS_entry($3, (TS_entry) $2, ClasseID.NomeFuncao);
                nodo.locals = ts;
                closeScope();
                ts.insert(nodo);
              }
          ;

DeclFunc  :  FUNC TypeOrVoid IDENT { openScope(); } '(' FormalPar ')' '{' DeclFuncVar ListaCmd '}' 
              {
                //criar funcao que valida declarao com base no prototipo
                // TS_entry nodo = new TS_entry($3, (TS_entry) $2, ClasseID.NomeFuncao);
                // nodo.locals = ts;
                 closeScope();
                // ts.insert(nodo);
              }
          ; */

DeclFuncProt  : FUNC TypeOrVoid IDENT { openScope(); } '(' FormalPar ')' RestoFuncProt
                {
                  //criar funcao que valida declarao com base no prototipo
                  TS_entry tipoFuncao = new TS_entry("?", Tp_Func, currentClass);
                  tipoFuncao.setTipoBase((TS_entry) $2);
                  TS_entry nodo = new TS_entry($3, tipoFuncao, currentClass);
                  nodo.setLocals(ts);
                  closeScope();
                  ts.insert(nodo);
                }
              ;

RestoFuncProt : '{' DeclFuncVar ListaCmd '}' { currentClass = ClasseID.NomeFuncao;}
              | ';' { currentClass = ClasseID.NomePrototipo; }
              ;

DeclFuncVar : DeclVar
            | 
            ;

FormalPar : ParamList
          | // vazio 
          ;

ParamList : Type IDENT ',' ParamList { ts.insert(new TS_entry($2, (TS_entry) $1, ClasseID.NomeParam)); }
          | Type IDENT { ts.insert(new TS_entry($2, (TS_entry) $1, ClasseID.NomeParam)); }
          ; 

Block : '{' ListaCmd '}'
      ;

ListaCmd  : Cmd ListaCmd
          |  // vazio
          ;

Cmd : Block
    | WHILE '(' Exp ')' Cmd { if ( ((TS_entry)$3) != Tp_BOOL) 
                                     yyerror("(sem) expressão (while) deve ser lógica "+((TS_entry)$3).getTipo());
                            }
    | IF '(' Exp ')' Cmd RestoIf  {  if ( ((TS_entry)$3) != Tp_BOOL) 
                                     yyerror("(sem) expressão (if) deve ser lógica "+((TS_entry)$3).getTipo());
                                  }     
    | IDENT '=' Exp ';' { TS_entry nodo = pesquisa($1);
                          if (nodo != null) validaTipo(ATRIB, nodo.getTipo(), (TS_entry) $3);
                          else yyerror("(sem) var <" + $1 + "> nao declarada");
                        }                              
    | IDENT'[' Exp ']' '=' Exp ';'
    | RETURN Exp ';'
    ;

RestoIf : ELSE Cmd
        |   // vazio
        ;
             //
              // faria mais sentido reconhecer todos os tipos como ident! 
              // 

Type : INT    { $$ = Tp_INT; }
     | DOUBLE  { $$ = Tp_DOUBLE; }
     | BOOLEAN   { $$ = Tp_BOOL; }   
     ;

//criar a acao para retorno do tipo void
TypeOrVoid  : Type  { $$ = $1; }
            | VOID { $$ = Tp_Void; }
            ;

//Main :  VOID MAIN '(' ')' Block ;

Exp : Exp '+' Exp { $$ = validaTipo('+', (TS_entry)$1, (TS_entry)$3); }
    | Exp '>' Exp { $$ = validaTipo('>', (TS_entry)$1, (TS_entry)$3); }
    | Exp AND Exp { $$ = validaTipo(AND, (TS_entry)$1, (TS_entry)$3); } 
    | NUM         { $$ = Tp_INT; }      
    | '(' Exp ')' { $$ = $2; }
    | IDENT       { TS_entry nodo = pesquisa($1);
                    if (nodo == null) {
                       yyerror("(sem) var <" + $1 + "> nao declarada"); 
                       $$ = Tp_ERRO;    
                       }           
                    else
                        $$ = nodo.getTipo();
                  }                   
     | Exp '=' Exp  {  $$ = validaTipo(ATRIB, (TS_entry)$1, (TS_entry)$3);  }
     | Exp '[' Exp ']'  {  if ((TS_entry)$3 != Tp_INT) 
                              yyerror("(sem) indexador não é numérico ");
                           else 
                               if (((TS_entry)$1).getTipo() != Tp_ARRAY)
                                  yyerror("(sem) elemento não indexado ");
                               else 
                                  $$ = ((TS_entry)$1).getTipoBase();
                         } 
    ;

%%
  private Yylex lexer;

  public static TS_entry Tp_INT =  new TS_entry("int", null, ClasseID.TipoBase);
  public static TS_entry Tp_DOUBLE = new TS_entry("double", null,  ClasseID.TipoBase);
  public static TS_entry Tp_BOOL = new TS_entry("boolean", null,  ClasseID.TipoBase);

  public static TS_entry Tp_Void = new TS_entry("void", null,  ClasseID.TipoBase);
  public static TS_entry Tp_Func = new TS_entry("func", null,  ClasseID.TipoBase);
  public static TS_entry Tp_ARRAY = new TS_entry("array", null,  ClasseID.TipoBase);

  public static TS_entry Tp_ERRO = new TS_entry("_erro_", null,  ClasseID.TipoBase);

  public static final int ARRAY = 1500;
  public static final int ATRIB = 1600;

  private String currEscopo;
  private TabSimb globalScope = new TabSimb();
  private TabSimb ts = globalScope;

  private ClasseID currentClass;
  private TS_entry currentType;

  private int yylex () {
    int yyl_return = -1;
    try {
      yylval = new ParserVal(0);
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }


  public void yyerror (String error) {
    //System.err.println("Erro (linha: "+ lexer.getLine() + ")\tMensagem: "+error);
    System.err.printf("Erro (linha: %2d) \tMensagem: %s\n", lexer.getLine(), error);
  }


  public Parser(Reader r) {
    lexer = new Yylex(r, this);

    //ts = new TabSimb();

    //
    // não me parece que necessitem estar na TS
    // já que criei todas como public static...
    //
    ts.insert(Tp_ERRO);
    ts.insert(Tp_INT);
    ts.insert(Tp_DOUBLE);
    ts.insert(Tp_BOOL);
    ts.insert(Tp_ARRAY);
    ts.insert(Tp_Void);
  }  

  public void setDebug(boolean debug) {
    yydebug = debug;
  }

  public void listarTS() { ts.listar();}

  public static void main(String args[]) throws IOException {
    System.out.println("\n\nVerificador semantico simples\n");
    

    Parser yyparser;
    if ( args.length > 0 ) {
      // parse a file
      yyparser = new Parser(new FileReader(args[0]));
    }
    else {
      // interactive mode
      System.out.println("[Quit with CTRL-D]");
      System.out.print("Programa de entrada:\n");
        yyparser = new Parser(new InputStreamReader(System.in));
    }

    yyparser.yyparse();

      yyparser.listarTS();

      System.out.print("\n\nFeito!\n");
    
  }


   TS_entry validaTipo(int operador, TS_entry A, TS_entry B) {
       
         switch ( operador ) {
              case ATRIB:
                    if ( (A == Tp_INT && B == Tp_INT)                        ||
                         ((A == Tp_DOUBLE && (B == Tp_INT || B == Tp_DOUBLE))) ||
                         (A == B) )
                         return A;
                     else
                         yyerror("(sem) tipos incomp. para atribuicao: "+ A.getTipoStr() + " = "+B.getTipoStr());
                    break;

              case '+' :
                    if ( A == Tp_INT && B == Tp_INT)
                          return Tp_INT;
                    else if ( (A == Tp_DOUBLE && (B == Tp_INT || B == Tp_DOUBLE)) ||
                                            (B == Tp_DOUBLE && (A == Tp_INT || A == Tp_DOUBLE)) ) 
                         return Tp_DOUBLE;     
                    else
                        yyerror("(sem) tipos incomp. para soma: "+ A.getTipoStr() + " + "+B.getTipoStr());
                    break;

             case '>' :
                     if ((A == Tp_INT || A == Tp_DOUBLE) && (B == Tp_INT || B == Tp_DOUBLE))
                         return Tp_BOOL;
                      else
                        yyerror("(sem) tipos incomp. para op relacional: "+ A.getTipoStr() + " > "+B.getTipoStr());
                      break;

             case AND:
                     if (A == Tp_BOOL && B == Tp_BOOL)
                         return Tp_BOOL;
                      else
                        yyerror("(sem) tipos incomp. para op lógica: "+ A.getTipoStr() + " && "+B.getTipoStr());
                 break;
            }

            return Tp_ERRO;
           
     }

    void openScope() {
        ts = new TabSimb();
    }

    void closeScope() {
        ts = globalScope;
    }

    TS_entry pesquisa(String nome) {
      TS_entry nodo = ts.pesquisa(nome);
      if (nodo != null) return nodo;

      return globalScope.pesquisa(nome);
    }

