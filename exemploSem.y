    
%{
  import java.io.*;
  import java.util.List;
  import java.util.Stack;
%}


%token IDENT, NUM, FUNC
%token IF, ELSE, WHILE, RETURN
%token INT, DOUBLE, BOOLEAN, VOID
%token EQUAL, NOTEQUAL, LTEQUAL, GTEQUAL, AND, OR
// EQUAL ==, NOTEQUAL !=, LTEQUAL <=, GTEQUAL >=

%right '='
%nonassoc '>' '<' EQUAL GTEQUAL LTEQUAL NOTEQUAL
%left '+'
%left AND OR
%left '['

%type <sval> IDENT
%type <ival> NUM
%type <obj> Type
%type <obj> TypeOrVoid
%type <obj> Exp
%type <obj> ListaCmd
%type <obj> Cmd
%type <obj> RestoFuncProt

%%

prog : { currentClass = ClasseID.VarGlobal; } DecList ;

DecList : Decl DecList
        | 
        ;

Decl  : DeclVar
      | DeclFuncProt
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

DeclFuncProt  : FUNC TypeOrVoid IDENT { openScope(); ts.insert(new TS_entry("return", (TS_entry) $2, ClasseID.VarLocal)); } '(' FormalPar ')' RestoFuncProt
                {
                  TS_entry nodo = pesquisa($3);
                  TS_entry typeOrVoid = (TS_entry) $2; TS_entry retorno = (TS_entry) $8;
                  if (currentClass == ClasseID.NomeFuncao) {
                    if (nodo == null) yyerror("(sem) falta prototipo da funcao: " + $3);
                    else {
                      if (!validaParams(nodo.getLocals(), ts)) yyerror("(sem) prototipo e implementacao de parametros de funcao nao correspondem: " + $3);
                      if (nodo.getTipo().getTipoBase() != typeOrVoid) yyerror("(sem) tipo de retorno incompativel com o prototipo da funcao: " + $3);
                    } 
                    if (retorno == Tp_Void && typeOrVoid != Tp_Void) yyerror("(sem) funcao" + $3 + "retorna void mas declaracao espera " + typeOrVoid.getTipoStr());
                  } else if (nodo != null) yyerror("(sem) identificador " + $3 + " ja declarado");
                  ts.remove("return");

                  TS_entry tipoFuncao = new TS_entry("?", Tp_Func, currentClass);
                  tipoFuncao.setTipoBase((TS_entry) $2);
                  tipoFuncao.setLocals(ts);
                  nodo = new TS_entry($3, tipoFuncao, currentClass);
                  nodo.setLocals(ts);
                  closeScope();
                  ts.insert(nodo);
                }
              ;

RestoFuncProt : '{' { currentClass = ClasseID.VarLocal; } DeclFuncVar ListaCmd '}' 
                { $$ = $4; currentClass = ClasseID.NomeFuncao; }
              | ';' { currentClass = ClasseID.NomePrototipo; }
              ;

DeclFuncVar : DeclVar
            | 
            ;

FormalPar : ParamList
          | // vazio 
          ;

ParamList : Type IDENT { ts.insert(new TS_entry($2, (TS_entry) $1, ClasseID.NomeParam)); } ',' ParamList 
          | Type IDENT { ts.insert(new TS_entry($2, (TS_entry) $1, ClasseID.NomeParam)); }
          ; 

Block : '{' ListaCmd '}'
      ;

ListaCmd  : Cmd ListaCmd {  TS_entry cmd = (TS_entry) $1; TS_entry listaCmd = (TS_entry) $2;
                            if (cmd == Tp_Void) $$ = listaCmd;
                            else if (listaCmd == Tp_Void) $$ = cmd;
                            else $$ = Tp_Void;
                          }
          | { $$ = Tp_Void; } // vazio 
          ;

Cmd : Block { $$ = Tp_Void; } 
    | WHILE '(' Exp ')' Cmd { if (((TS_entry)$3) != Tp_BOOL)
                                     yyerror("(sem) expressão (while) deve ser lógica "+((TS_entry)$3).getTipo());
                              $$ = Tp_Void;
                            }
    | IF '(' Exp ')' Cmd RestoIf  { if (((TS_entry)$3) != Tp_BOOL) yyerror("(sem) expressão (if) deve ser lógica "+((TS_entry)$3).getTipo());
                                    $$ = Tp_Void;
                                  }
    | IDENT '=' Exp ';' { TS_entry nodo = pesquisa($1);
                          if (nodo != null) validaTipo(ATRIB, nodo.getTipo(), (TS_entry) $3);
                          else yyerror("(sem) var <" + $1 + "> nao declarada");
                          $$ = Tp_Void;
                        }                              
    | IDENT'[' Exp ']' '=' Exp ';' { $$ = Tp_Void;}
    | RETURN Exp ';'  { $$ = $2; 
                        TS_entry expected = pesquisa("return").getTipo(); TS_entry actual = (TS_entry) $2;
                        if (expected != actual) yyerror("(sem) tipo de retorno incompativel com a funcao esperava " + expected.getTipoStr() + " era " + actual.getTipoStr());
                      }
    | RETURN ';'  { $$ = Tp_Void; 
                    TS_entry expected = pesquisa("return").getTipo();
                    if (expected != Tp_Void) yyerror("(sem) tipo de retorno incompativel com a funcao esperava " + expected.getTipoStr() + " era void");
                  }
    ;

RestoIf : ELSE Cmd
        |   // vazio
        ;

Type : INT    { $$ = Tp_INT; }
     | DOUBLE  { $$ = Tp_DOUBLE; }
     | BOOLEAN   { $$ = Tp_BOOL; }   
     ;

//criar a acao para retorno do tipo void
TypeOrVoid  : Type  { $$ = $1; }
            | VOID { $$ = Tp_Void; }
            ;

Exp : Exp '+' Exp { $$ = validaTipo('+', (TS_entry)$1, (TS_entry)$3); }
    | Exp '>' Exp { $$ = validaTipo('>', (TS_entry)$1, (TS_entry)$3); }
    | Exp '<' Exp { $$ = validaTipo('<', (TS_entry)$3, (TS_entry)$1); }
    | Exp LTEQUAL Exp { $$ = validaTipo(LTEQUAL, (TS_entry)$1, (TS_entry)$3); }
    | Exp GTEQUAL Exp { $$ = validaTipo(GTEQUAL, (TS_entry)$1, (TS_entry)$3); }
    | Exp EQUAL Exp { $$ = validaTipo(EQUAL, (TS_entry)$1, (TS_entry)$3); }
    | Exp NOTEQUAL Exp { $$ = validaTipo(NOTEQUAL, (TS_entry)$1, (TS_entry)$3); }
    | Exp AND Exp { $$ = validaTipo(AND, (TS_entry)$1, (TS_entry)$3); }
    | Exp OR Exp { $$ = validaTipo(OR, (TS_entry)$1, (TS_entry)$3); }
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
    | IDENT { openScope(); } '(' Args ')' {  TS_entry nodo = pesquisa($1);
                                            if (nodo == null) {
                                              yyerror("(sem) função <" + $1 + "> nao declarada");
                                              $$ = Tp_ERRO;
                                            } else if (nodo.getTipo().getTipo() != Tp_Func) {
                                              yyerror("(sem) <" + $1 + "> não é uma função");
                                              $$ = Tp_ERRO;

                                            } else if (!validaParams(nodo.getLocals(), ts)) {
                                              yyerror("(sem) parametros incompativeis na chamada da funcao " + $1);
                                              $$ = Tp_ERRO;

                                            } else {
                                              $$ = nodo.getTipo().getTipoBase();
                                            }
                                            closeScope();
                                          }
    ;

Args : ArgList
     | //vazio
     ;

ArgList : Exp ',' ArgList { ts.insert(new TS_entry("?", (TS_entry) $1, ClasseID.NomeParam)); }
        | Exp { ts.insert(new TS_entry("?", (TS_entry) $1, ClasseID.NomeParam)); }
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
  private Stack<TabSimb> scopes = new Stack<>();

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
    scopes.push(globalScope);
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
              case '<' :
              case LTEQUAL:
              case GTEQUAL:
                     if ((A == Tp_INT || A == Tp_DOUBLE) && (B == Tp_INT || B == Tp_DOUBLE))
                         return Tp_BOOL;
                      else
                        yyerror("(sem) tipos incomp. para op relacional: "+ A.getTipoStr() + " > "+B.getTipoStr());
                      break;
             case AND:
             case OR:
                     if (A == Tp_BOOL && B == Tp_BOOL)
                         return Tp_BOOL;
                      else
                        yyerror("(sem) tipos incomp. para op lógica: "+ A.getTipoStr() + " && "+B.getTipoStr());
                 break;
            case NOTEQUAL:
            case EQUAL:
                     if (A == B)
                         return Tp_BOOL;
                      else
                        yyerror("(sem) tipos incomp. para op de igualdade: "+ A.getTipoStr() + " == "+B.getTipoStr());
                 break;
            }

            return Tp_ERRO;
           
     }

    void openScope() {
        scopes.push(ts);
        ts = new TabSimb();
    }

    void closeScope() {
        ts = scopes.pop();
    }

    TS_entry pesquisa(String nome) {
      TS_entry nodo = ts.pesquisa(nome);
      if (nodo != null) return nodo;

      return globalScope.pesquisa(nome);
    }

    boolean validaParams(TabSimb definition, TabSimb actual) {
      List<TS_entry> definitionList = definition.getLista().stream().filter(e -> e.getClasse() == ClasseID.NomeParam).toList();
      List<TS_entry> actualList = actual.getLista().stream().filter(e -> e.getClasse() == ClasseID.NomeParam).toList();

      if (definitionList.size() != actualList.size()) {
        return false;
      }

      for (int i = 0; i < definitionList.size(); i++) {
        if (definitionList.get(i).getTipo() != actualList.get(i).getTipo()) {
          return false;
        }
      }

      return true;
    }

