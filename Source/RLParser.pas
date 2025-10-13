{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios são constituídos por bandas que  }
{ têm funções específicas no fluxo de impressão. Você definir agrupamentos     }
{ subníveis e totais simplesmente pela relação hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              Márcio Martins                                  }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informações você pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descrição...
******************************************************************************}

{$I RLReport.inc}

{@unit RLParser - Implementação do componente de avaliação de expressões. }
unit RLParser;

interface

uses
  {$IfDef WINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  Classes, Contnrs, Math, TypInfo,
  {$IfDef SUPPORTS_VARIANT}
   Variants,
  {$EndIf}
  SysUtils, RLUtils, RLTypes;

type
  {@type TRLParserTokenKind - Tipo de token. :}
  TRLParserTokenKind = (tkNull, tkUnknown, 
                      tkIdentifier, tkValue, tkLiteral, tkOperator, 
                      tkEnterBrackets, tkExitBrackets, tkBracketsBreak, 
                      tkCodeEnter, tkCodeBreak, tkCodeExit);
  {/@type}

  TRLParserOperation = (opNull, // none
                      opIdentifier, opValue, opLiteral, // identifier or const
                      opSum, opSubtract, opMultiply, opDivide, opPower, opRoot, // binary math operation
                      opPlus, opMinus, opNot, // unary operation
                      opGreater, opLess, opEqual, opInequal, opNotLess, opNotGreater, // relational operation
                      opOr, opAnd, // logical operation
                      opSet, // attribution
                      opInline); // inline code

  TRLParserNode = class;
  TRLParserFunction = class;
  TRLParserFunctionList = class;
  TRLParserValue = class;
  TRLParserValueList = class;
  TRLParserParamList = class;

  {@type TRLParserOnIdentifierEvent - Identificador encontrado. Este evento é disparado para todo identificador encontrado na expressão.
   Sender é o expressionparser que invocou o evento;
   Identifier é uma sequência alfanumérica que é o nome da variável encontrada;
   Params é um vetor variant que contém a lista de parâmetros que seguem o identificador, caso seja uma chamada a função;
   Result é o resultado da expressão que pode ser informado pelo usuário.
   @links TRLExpressionParser, TRLExpressionParser.OnIdentifier, TRLExpressionParser.OnUnknown. :/}
  TRLParserOnIdentifierEvent = procedure(Sender: TObject; const Identifier: String; Params: variant; var Result: variant) of object;

  {@type TRLParserOnTokenEvent - Evento para sequências de caracteres.
   Sender é o expressionparser que invocou o evento;
   Token é uma sequência qualquer de caracteres que tem a mesma semântica e que pode ser traduzida pelo usuário;
   Kind é o tipo de sequência do modo como foi identificada pelo parser.
   @links TRLExpressionParser, TRLParserTokenKind. :/}
  TRLParserOnTokenEvent = procedure(Sender: TObject; var Token: String; var Kind: TRLParserTokenKind) of object;

  {@type TRLParserOnException - Evento disparado quando ocorre uma exceção durante a tradução da expressão.
   Sender é o expressionparser que invocou o evento;
   E é uma referência para o objeto exceção;
   Result é o valor assumido.
   @links TRLExpressionParser. :/}
  TRLParserOnException = procedure(Sender: TObject; var E: Exception; var Result: variant) of object;

  TRLParserOption = (poScanComponents);
  TRLParserOptions = set of TRLParserOption;

  {@type TRLParserOnFindAgregateEvent - Oportunidade para procura de um elemtno agregado. :/}
  TRLParserOnFindAgregateEvent = procedure(Sender: TObject; Owner: TPersistent; const Name: String; var Agregate: TPersistent) of object;

  {@type TRLParserOnGetAttributeEvent - Oportunidade para obter um atributo de um objeto. :/}
  TRLParserOnGetAttributeEvent = procedure(Sender: TObject; Owner: TPersistent; const Name: String; var Value: variant) of object;

  {@type TRLParserOnSetAttributeEvent - Oportunidade para alterar um atributo de um objeto. :/}
  TRLParserOnSetAttributeEvent = procedure(Sender: TObject; Owner: TPersistent; const Name: String; const Value: variant; var Handled: Boolean) of object;

  { TRLExpressionParser }

  {@class TRLExpressionParser - Avaliador de expressões.
   @pub }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLExpressionParser = class(TComponent)
  private
    { Private declarations }
    FExprBuffer: PChar;
    FExprBuffLen: Integer;
    FExprBuffCh: Integer;
    //
    FNextCh: char;
    FNextToken: String;
    FNextKind: TRLParserTokenKind;
    FLastToken: String;
    FLastKind: TRLParserTokenKind;
    FFunctionList: TRLParserFunctionList;
    FValueList: TRLParserValueList;
    FIdentifierId: Integer;
    FRootNode: TRLParserNode;
    FExpression: String;
    FOptions: TRLParserOptions;
    FParams: TRLParserParamList;
    //
    FResourceProc: TRLParserOnIdentifierEvent;
    FOnUnknown: TRLParserOnIdentifierEvent;
    FOnIdentifier: TRLParserOnIdentifierEvent;
    FTokenProc: TRLParserOnTokenEvent;
    FOnToken: TRLParserOnTokenEvent;
    FOnException: TRLParserOnException;
    //
    FFindAgregateProc: TRLParserOnFindAgregateEvent;
    FSetAttributeProc: TRLParserOnSetAttributeEvent;
    FGetAttributeProc: TRLParserOnGetAttributeEvent;
    //
    function NextCh(var ACh: char): Boolean;
    procedure RefuseCh(var ACh: char);
    function NextValidCh(var ACh: char): Boolean;
    function NextToken(var AToken: String; var AKind: TRLParserTokenKind): Boolean;
    function CompileNode: TRLParserNode;
    procedure ResolveIdentifier(const AIdentifier: String; AId: Integer; AParams: variant; var AResult: variant);
    procedure ResolveToken(var AToken: String; var AKind: TRLParserTokenKind);
    function TryEvalNode(ANode: TRLParserNode): variant;
    function EvalNode(ANode: TRLParserNode): variant;
    procedure Compile(const AExpression: String; var ARoot: TRLParserNode);
    procedure ParseFunction(const ADeclaration: String; var AName: String; AParams: TStrings);
    function InternalEvaluate(const AExpression: String): variant;
    procedure SetExpression(const Value: String);
    function GetGlobalProperty(const APropPath: String): variant;
    function SetGlobalProperty(const APropPath: String; AValue: variant): Boolean;
    function DoResolveIdentifier(const AIdentifier: String; AParams: variant): variant;
    function GetPropValue(APersistent: TPersistent; const APropName: String): variant;
    function SetPropValue(APersistent: TPersistent; const APropName: String; AValue: variant): Boolean;
    //
    function FindDependentPart(AOwner: TPersistent; const AName: String): TPersistent;
    function FindPropertyPair(AOwner: TPersistent; const APropPath: String; var APersistent: TPersistent; var APropName: String): Boolean;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {@method CreateFunction - Adiciona uma função pré-definida.
     Uma função para o expressionparser é uma expressão associada a um símbolo com parâmetros,
     na forma: "NOME(P1,P2,...,Pn)=EXPRESSÃO".
     Ex.: Min(p1,p2)=If(p1<p2,p1,p2) :/}
    function CreateFunction(const ADeclaration: String; const AExpression: String): TRLParserFunction;

    {@method CreateValue - Associa um símbolo a uma constante. Quando o expressionparser tenta resolver uma expressão,
     procura pelos símbolos nas listas de símbolos e funções pré-definidas antes de disparar o evento OnUnknown.
     @links builtin:build-in. :/}
    function CreateValue(const AName: String; const AValue: variant): TRLParserValue;
    
    {@method Evaluate - Avalia uma expressão e retorna o resultado.
     Chamado sem parâmetros indica que o cálculo deve ser executado com a prop Expression.
     O parâmetro aExpression é uma fórmula cujos elementos podem ser de qualquer tipo de dado primário: integer,
     float, string e boolean.
     Podem ser incluídos e serão resolvidos automaticamente: nomes de campos, funções e símbolos pré-definidos, e
     funções built-in.
     @links Expression, BuiltIn:build-in. :}
    function Evaluate: variant; overload;
    function Evaluate(const AExpression: String): variant; overload;
    {/@method}

    {@method Invalidate - Invalida a última compilação feita para a expressão. Força a recompilaçào na próxima avaliação.
     @links Evaluate, Clear. :/}
    procedure Invalidate;

    {@method Clear - Esvazia a lista de valores e funções pré-definidos.
     @links ValueList, FunctionList, CreateFunction, CreateValue. :/}
    procedure Clear;

    {@prop FunctionList - Referência para a lista de funções pré-definidas. :/}
    property FunctionList: TRLParserFunctionList read FFunctionList;

    {@prop ValueList - Referência para a lista de valores pré-definidos. :/}
    property ValueList: TRLParserValueList read FValueList;
    
    property IdentifierId: Integer read FIdentifierId;
    //
    property ResourceProc: TRLParserOnIdentifierEvent read FResourceProc write FResourceProc;
    property TokenProc: TRLParserOnTokenEvent read FTokenProc write FTokenProc;
    property FindAgregateProc: TRLParserOnFindAgregateEvent read FFindAgregateProc write FFindAgregateProc;
    property SetAttributeProc: TRLParserOnSetAttributeEvent read FSetAttributeProc write FSetAttributeProc;
    property GetAttributeProc: TRLParserOnGetAttributeEvent read FGetAttributeProc write FGetAttributeProc;

  published
    { Published declarations }

    {@prop Expression - Expressão padrão a avaliar.
     A expressão pode ser simples como a soma de dois números ou nomes de campos de uma
     tabela. Pode ser também um script complexo em dialeto pascal acessando componentes
     e modificando suas propriedades.
     O resultado será sempre o valor do último número, literal, identificador ou operação
     mencionado na expressão. :/}
    property Expression: String read FExpression write SetExpression;

    {@prop Options - Opções de escopo para resolução de identificadores. :/}
    property Options: TRLParserOptions read FOptions write FOptions default [poScanComponents];
    {@event OnIdentifier - Evento disparado antes da avaliação de um identificador.
     @links TRLParserOnIdentifierEvent. :/}
    property OnIdentifier: TRLParserOnIdentifierEvent read FOnIdentifier write FOnIdentifier;

    {@event OnUnknown - Evento disparado quando um identificador não pode ser resolvido pelo parser.
     @links TRLParserOnIdentifierEvent. :/}
    property OnUnknown: TRLParserOnIdentifierEvent read FOnUnknown write FOnUnknown;

    {@event OnToken - Evento chamado para cada sequência de caracteres encontrada.
     Os operadores e estruturas de controle do expressionparser são simbólicos e escritos em inglês. Utilize este
     evento para customizar o dialéto utilizado.
     @links TRLParserOnTokenEvent. :/}
    property OnToken: TRLParserOnTokenEvent read FOnToken write FOnToken;

    {@event OnException - Evento disparado quando ocorre uma exceção na tradução da expressão.
     @links TRLParserOnException. :/}
    property OnException: TRLParserOnException read FOnException write FOnException;
  end;
  {/@class}

  {@article BuiltIn - Funções e constantes pré-compiladas do componente TRLExpressionParser.
   PI - Retorna o valor de PI = 3.1415926535897932385;
   TRUE - Contante booleana equivalente a verdadeiro;
   FALSE - Contante booleana equivalente a falso;
   MINOR(V1,V2,..,VN) - Função que retorna o menor dos valores passados;
   MAJOR(V1,V2,..,VN) - Função que retorna o maior dos valores passados;
   IF(C,V1,V2) - Função que retorna o valor V1 se a condição C for verdadeira, e V2, caso contrário. /}
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLParserParam = class
  private
    FFunction: TRLParserFunction;
    FName: String;
    FValue: variant;
  public
    constructor Create(AFunction: TRLParserFunction);
    destructor Destroy; override;
    //
    procedure Clear;
    //
    property Name: String read FName write FName;
    property Value: variant read FValue write FValue;
  end;
	
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLParserParamList = class(TObjectList)
  public
    procedure Assign(AParams: variant); overload;
    procedure Assign(AParams: array of variant); overload;
    procedure Reset;
    function ByIndex(AIndex: Integer): TRLParserParam;
    function ByName(const AName: String): TRLParserParam;
    //
    property Params[AIndex: Integer]: TRLParserParam read ByIndex; default;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLParserFunction = class(TPersistent)
  private
    FParser: TRLExpressionParser;
    FName: String;
    FExpression: String;
    FRootNode: TRLParserNode;
    FParams: TRLParserParamList;
    //
    procedure SetExpression(const Value: String);
    //
    function InternalEvaluate: variant;
  public
    constructor Create(AParser: TRLExpressionParser); reintroduce;
    destructor Destroy; override;

    {@method Invalidate - Invalida a última compilação feita para a expressão. Força a recompilaçào na próxima avaliação.
     @links Evaluate. :/}
    procedure Invalidate;
    
    function Evaluate: variant; overload;
    function Evaluate(AParams: variant): variant; overload;
    function Evaluate(AParams: array of variant): variant; overload;
    //
    property Parser: TRLExpressionParser read FParser;
    property Params: TRLParserParamList read FParams;
  published
    property Expression: String read FExpression write SetExpression;
    property Name: String read FName write FName;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLParserFunctionList = class(TObjectList)
  public
    function ByIndex(AIndex: Integer): TRLParserFunction;
    function ByName(const AName: String): TRLParserFunction;
    //
    property Functions[AIndex: Integer]: TRLParserFunction read ByIndex; default;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLParserValue = class(TPersistent)
  private
    FParser: TRLExpressionParser;
    FName: String;
    FValue: variant;
  public
    constructor Create(AParser: TRLExpressionParser); reintroduce;
    destructor Destroy; override;
    //
    property Parser: TRLExpressionParser read FParser;
    //
    property Name: String read FName write FName;
    property Value: variant read FValue write FValue;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLParserValueList = class(TObjectList)
  public
    function ByIndex(AIndex: Integer): TRLParserValue;
    function ByName(const AName: String): TRLParserValue;
    //
    property Values[AIndex: Integer]: TRLParserValue read ByIndex; default;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLParserNode = class
  private
    FParser: TRLExpressionParser;
    FParentNode: TRLParserNode;
    FOperation: TRLParserOperation;
    FChildren: TObjectList;
    FValue: variant;
    FPriority: Byte;
    FIsolated: Boolean;
    //
    procedure SetParentNode(const Value: TRLParserNode);
    function GetChildren(AIndex: Integer): TRLParserNode;
    function GetChildCount: Integer;
  protected
    procedure InsertBefore(ANode: TRLParserNode);
    procedure AddChild(ANode: TRLParserNode);
    function GetRootDistance: Integer;
    //
    property ChildList: TObjectList read FChildren;
  public
    constructor Create(AParser: TRLExpressionParser; const AOperation: TRLParserOperation; const AValue: String = '');
    destructor Destroy; override;
    //
    function Eval: variant;
    //
    property ParentNode: TRLParserNode read FParentNode write SetParentNode;
    property Priority: Byte read FPriority write FPriority;
    property Isolated: Boolean read FIsolated write FIsolated;
    //
    property Parser: TRLExpressionParser read FParser;
    property Operation: TRLParserOperation read FOperation;
    property Value: variant read FValue;
    //
    property Children[AIndex: Integer]: TRLParserNode read GetChildren;
    property ChildCount: Integer read GetChildCount;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLParserInlineNode = class(TRLParserNode)
  end;
  
{/@unit}

implementation

const
  DIGITSET = ['0'..'9'];
  ALPHASET = ['A'..'Z', 'a'..'z', '_'];
  ALPHASETEX = ALPHASET + DIGITSET + ['.'];
  NUMSET = DIGITSET;
  NUMSETEX = NUMSET + ['.', 'E', 'e', '-', '+'];
  QUOTESET = ['''', '"'];
  MATHOPSET = ['+', '-', '*', '/', '^', '\'];
  RELATIONALOPSET = ['>', '<', '=', '#'];
  LOGICALOPSET = ['|', '&'];
  UNARYOPSET = ['+', '-', '!'];
  ALLOPSET = MATHOPSET + RELATIONALOPSET + LOGICALOPSET + UNARYOPSET;
  NULLSET = [#32, #13, #10, #9];

procedure Abort(const AMsg: String);
begin
  raise EAbort.Create(AMsg);
end;

function TokenToOperation(const AToken: String; AUnary: Boolean = False): TRLParserOperation;
begin
  // binary math operation
  if not AUnary and (AToken = '+') then
    Result := opSum
  else if not AUnary and (AToken = '-') then
    Result := opSubtract
  else if AToken = '*' then
    Result := opMultiply
  else if AToken = '/' then
    Result := opDivide
  else if AToken = '^' then
    Result := opPower
  else if AToken = '\' then 
    Result := opRoot
  // unary operation
  else if AUnary and (AToken = '+') then
    Result := opPlus
  else if AUnary and (AToken = '-') then
    Result := opMinus
  else if AUnary and (AToken = '!') then
    Result := opNot
  // relational operation  
  else if AToken = '>' then
    Result := opGreater
  else if AToken = '<' then
    Result := opLess
  else if AToken = '=' then
    Result := opEqual
  else if (AToken = '#') or (AToken = '<>') then
    Result := opInequal
  else if AToken = '>=' then
    Result := opNotLess
  else if AToken = '<=' then
    Result := opNotGreater
  // logical operation  
  else if AToken = '|' then
    Result := opOr
  else if AToken = '&' then
    Result := opAnd
  // attribution  
  else if AToken = ':=' then
    Result := opSet
  //
  else
    Result := opNull;
end;

function OperationToToken(const AOperation: TRLParserOperation): String;
begin
  case AOperation of
    // binary math operation
    opSum: Result := '+';
    opSubtract: Result := '-';
    opMultiply: Result := '*';
    opDivide: Result := '/';
    opPower: Result := '^';
    opRoot: Result := '\';
    // unary operation
    opPlus: Result := '+';
    opMinus: Result := '-';
    opNot: Result := '!';
    // relational operation
    opGreater: Result := '>';
    opLess: Result := '<';
    opEqual: Result := '=';
    opInequal: Result := '#';
    opNotLess: Result := '>=';
    opNotGreater: Result := '<=';
    // logical operation
    opOr: Result := '|';
    opAnd: Result := '&';
    // attribution
    opSet: Result := ':=';
  else
    Result := '';
  end;
end;

function OperationPriority(const AOperation: TRLParserOperation): Byte;
begin
  case AOperation of
    // attribution
    opSet: Result := 0;
    // relational operation
    opEqual: Result := 1;
    opInequal: Result := 2;
    opGreater: Result := 3;
    opLess: Result := 4;
    opNotLess: Result := 5;
    opNotGreater: Result := 6;
    // logical operation
    opOr: Result := 7;
    opAnd: Result := 8;
    // binary math operation
    opSum: Result := 9;
    opSubtract: Result := 10;
    opMultiply: Result := 11;
    opDivide: Result := 12;
    opPower: Result := 13;
    opRoot: Result := 14;
    // unary operation
    opPlus: Result := 15;
    opMinus: Result := 16;
    opNot: Result := 17;
  else
    Result := 255;
  end;
end;

{ TRLExpressionParser }

constructor TRLExpressionParser.Create(AOwner: TComponent);
begin
  FExprBuffer := nil;
  FExprBuffLen := 0;
  FExprBuffCh := 0;
  FNextCh := #0;
  FNextToken := '';
  FNextKind := tkNull;
  FLastToken := '';
  FLastKind := tkNull;
  FFunctionList := nil;
  FValueList := nil;
  FIdentifierId := 0;
  FExpression := '';
  FRootNode := nil;
  FOptions := [poScanComponents];
  FParams := nil;
  //
  FResourceProc := nil;
  FOnUnknown := nil;
  FOnIdentifier := nil;
  FTokenProc := nil;
  FOnToken := nil;
  FOnException := nil;
  FFindAgregateProc := nil;
  FSetAttributeProc := nil;
  FGetAttributeProc := nil;
  //
  FFunctionList := TRLParserFunctionList.Create;
  FValueList := TRLParserValueList.Create;
  //
  inherited Create(AOwner);
end;

destructor TRLExpressionParser.Destroy;
begin
  Clear;
  //
  if Assigned(FRootNode) then
    FRootNode.Free;
  if Assigned(FFunctionList) then
    FFunctionList.Free;
  if Assigned(FValueList) then
    FValueList.Free;
  //
  inherited;
end;

procedure TRLExpressionParser.Clear;
begin
  if Assigned(FFunctionList) then
    FFunctionList.Clear;
  if Assigned(FValueList) then
    FValueList.Clear;
end;

function TRLExpressionParser.NextCh(var ACh: char): Boolean;
begin
  if FNextCh <> #0 then
  begin
    ACh := FNextCh;
    FNextCh := #0;
  end
  else if FExprBuffCh < FExprBuffLen then
  begin
    ACh := FExprBuffer[FExprBuffCh];
    Inc(FExprBuffCh);
  end
  else
    ACh := #0;
  //  
  Result := (ACh <> #0);
end;

procedure TRLExpressionParser.RefuseCh(var ACh: char);
begin
  if ACh <> #0 then
    FNextCh := ACh;
end;

// skip spaces and control chars
function TRLExpressionParser.NextValidCh(var ACh: char): Boolean;
begin
  while NextCh(ACh) and CharInSet(ACh, NULLSET) do;
  Result := (ACh <> #0);
end;

function TRLExpressionParser.NextToken(var AToken: String; var AKind: TRLParserTokenKind): Boolean;
var
  ch, quote: char;
  pointed: Boolean;
  exped: Boolean;
  expsignd: Boolean;
begin
  if FNextKind <> tkNull then
  begin
    AToken := FNextToken;
    AKind := FNextKind;
    FNextToken := '';
    FNextKind := tkNull;
  end
  else
  begin
    AToken := '';
    AKind := tkNull;
    //
    if NextValidCh(ch) then
      if CharInSet(ch, ALPHASET) then
      begin
        AKind := tkIdentifier;
        AToken := ch;
        while NextCh(ch) and CharInSet(ch, ALPHASETEX) do
          AToken := AToken + ch;
        RefuseCh(ch);
      end
      else if CharInSet(ch, NUMSET) then
      begin
        pointed := False;
        exped := False;
        expsignd := False;
        AKind := tkValue;
        AToken := ch;
        while NextCh(ch) and CharInSet(ch, NUMSETEX) do
        begin
          if ch = '.' then
            if exped then
              Abort('Incorrect number format at ' + IntToStr(FExprBuffCh) + '!')
            else if not pointed then
              pointed := True
            else
              Abort('Incorrect number format at ' + IntToStr(FExprBuffCh) + '!')
          else if CharInSet(ch, ['e', 'E']) then
            if not exped then
              exped := True
            else
              Abort('Incorrect number format at ' + IntToStr(FExprBuffCh) + '!')
          else if CharInSet(ch, ['+', '-']) then
            if not exped then
              Break
            else if not expsignd then
              expsignd := True
            else
              Abort('Incorrect number format at ' + IntToStr(FExprBuffCh) + '!');
          AToken := AToken + ch;
        end;
        RefuseCh(ch);
      end
      else if CharInSet(ch, QUOTESET) then
      begin
        quote := ch;
        AKind := tkLiteral;
        AToken := '';
        repeat
          while NextCh(ch) and (ch <> quote) do
            AToken := AToken + ch;
          if ch = quote then
            if NextValidCh(ch) and (ch = quote) then
            begin
              AToken := AToken + ch;
              Continue;
            end; 
          Break; 
        until False;
        RefuseCh(ch);
      end
      else if CharInSet(ch, ALLOPSET) then
      begin
        AKind := tkOperator;
        AToken := ch;
        if CharInSet(ch, ['>', '<']) then
          if NextValidCh(ch) then
            if (ch = '=') or ((ch = '>') and (AToken = '<')) then
              AToken := AToken + ch
            else
              RefuseCh(ch);
      end
      else if ch = ':' then
      begin
        AKind := tkUnknown;
        AToken := ch;
        if NextValidCh(ch) then
          if ch = '=' then
          begin
            AKind := tkOperator;
            AToken := AToken + ch;
          end
          else
            RefuseCh(ch);
      end
      else if ch = '(' then
      begin
        AKind := tkEnterBrackets;
        AToken := ch;
      end
      else if ch = ')' then
      begin
        AKind := tkExitBrackets;
        AToken := ch;
      end
      else if ch = ',' then
      begin
        AKind := tkBracketsBreak;
        AToken := ch;
      end
      else if ch = '{' then
      begin
        AKind := tkCodeEnter;
        AToken := ch;
      end
      else if ch = ';' then
      begin
        AKind := tkCodeBreak;
        AToken := ch;
      end
      else if ch = '}' then
      begin
        AKind := tkCodeExit;
        AToken := ch;
      end
      else
      begin
        AKind := tkUnknown;
        AToken := ch;
      end; 
  end; 
  //
  if AKind in [tkUnknown, tkIdentifier, tkValue, tkLiteral, tkOperator] then
    ResolveToken(AToken, AKind);
  FLastToken := AToken;
  FLastKind := AKind;
  //
  Result := (AKind <> tkNull);
end;

procedure TRLExpressionParser.ResolveToken(var AToken: String; var AKind: TRLParserTokenKind);
begin
  // defaults
  if AKind = tkIdentifier then
    if AnsiSameText(AToken, 'not') then
    begin
      AToken := '!';
      AKind := tkOperator;
    end
    else if AnsiSameText(AToken, 'and') then
    begin
      AToken := '&';
      AKind := tkOperator;
    end
    else if AnsiSameText(AToken, 'or') then
    begin
      AToken := '|';
      AKind := tkOperator;
    end
    else if AnsiSameText(AToken, 'div') then
    begin
      AToken := '/';
      AKind := tkOperator;
    end
    else if AnsiSameText(AToken, 'begin') then
    begin
      AToken := '{';
      AKind := tkCodeEnter;
    end
    else if AnsiSameText(AToken, 'end') then
    begin
      AToken := '}';
      AKind := tkCodeExit;
    end;
  // internal defined use
  if Assigned(FTokenProc) then
    FTokenProc(Self, AToken, AKind);
  // user defined
  if Assigned(FOnToken) then
    FOnToken(Self, AToken, AKind);
end;

function TRLExpressionParser.CompileNode: TRLParserNode;
var
  token: String;
  kind: TRLParserTokenKind;
  node: TRLParserNode;
  unary: Boolean;
  last: TRLParserNode;
  prior: TRLParserNode;
  this: TRLParserNode;
begin
  Result := nil;
  last := nil;
  unary := True;
  //
  while NextToken(token, kind) do
  begin
    case kind of
      tkNull: // impossible
                       ;
      tkUnknown: // error
                       ;
      tkIdentifier: // variable or const name
                       begin
                         node := TRLParserNode.Create(Self, opIdentifier, token);
                         if last <> nil then
                           last.AddChild(node)
                         else
                           Result := node;
                         last := node;
                       end;
      tkValue: // numeric value float or int
                       begin
                         node := TRLParserNode.Create(Self, opValue, token);
                         if last <> nil then
                           last.AddChild(node)
                         else
                           Result := node;
                         last := node;
                       end;
      tkLiteral: // quoted string
                       begin
                         node := TRLParserNode.Create(Self, opLiteral, token);
                         if last <> nil then
                           last.AddChild(node)
                         else
                           Result := node;
                         last := node;
                       end;
      tkOperator: // operators with priority
                       begin
                         if CharInSet(token[1], UNARYOPSET) then
                           if last = nil then
                             unary := True
                           else if last.Operation in [opIdentifier, opValue, opLiteral] then
                           else if last.Isolated then
                           else
                             unary := True;
                         node := TRLParserNode.Create(Self, TokenToOperation(token, unary), token);
                         if last <> nil then
                         begin
                           prior := nil;
                           while (node.Priority <= last.Priority) and Assigned(last.ParentNode) do
                           begin
                             prior := last;
                             last := last.ParentNode;
                           end;
                           if node.Priority <= last.Priority then
                           begin
                             last.InsertBefore(node);
                             if not Assigned(node.ParentNode) then
                               Result := node;
                           end
                           else if Assigned(prior) then
                             prior.InsertBefore(node)
                           else
                             last.AddChild(node);
                         end
                         else
                           Result := node;
                         last := node;
                       end;
      tkEnterBrackets: // brackets
                       begin
                         repeat
                           node := CompileNode;
                           this := node;
                           if node <> nil then
                             if last <> nil then
                               last.AddChild(node)
                             else
                               Result := node;
                           if FLastKind = tkBracketsBreak then
                             if (last = nil) or (last.Operation <> opIdentifier) then
                               Abort('Parameter list not suported at ' + IntToStr(FExprBuffCh) + '!')
                             else
                           else if FLastKind <> tkExitBrackets then
                             Abort('Brackets expected at ' + IntToStr(FExprBuffCh) + '!');
                         until FLastKind = tkExitBrackets;
                         if (last = nil) or (last.Operation <> opIdentifier) then
                           last := this;
                       end; 
      tkBracketsBreak: // ends a parameter
                       Break;
      tkExitBrackets: // ends brackets
                       Break;
      tkCodeEnter: // code piece
                       begin
                         node := TRLParserNode.Create(Self, opInline, token);
                         if last <> nil then
                           last.AddChild(node)
                         else
                           Result := node;
                         if (last <> nil) and (last.Operation = opIdentifier) then
                         else
                           last := node;
                         this := node;
                         //
                         repeat
                           node := CompileNode;
                           if node <> nil then
                             this.AddChild(node);
                           if not (FLastKind in [tkCodeBreak, tkCodeExit]) then
                             Abort('Inline expected at ' + IntToStr(FExprBuffCh) + '!');
                         until FLastKind = tkCodeExit;
                       end;
      tkCodeBreak: // code separator
                       Break;
      tkCodeExit: // ends code piece
                       Break;
    end;
    //
    unary := False;
  end;
  //
  if Assigned(Result) then
    with Result do
    begin
      Priority := 255;
      Isolated := True;
    end;
end;

procedure TRLExpressionParser.Compile(const AExpression: String; var ARoot: TRLParserNode);
begin
  FExprBuffLen := Length(AExpression);
  FExprBuffer := @AExpression[1];
  FExprBuffCh := 0;
  //
  FNextCh := #0;
  FNextToken := '';
  FNextKind := tkNull;
  FLastToken := '';
  FLastKind := tkNull;
  //
  ARoot := CompileNode;
end;

procedure TRLExpressionParser.ParseFunction(const ADeclaration: String; var AName: String; AParams: TStrings);
var
  I, L: Integer;
  N: String;
begin
  L := Length(ADeclaration);
  I := 1;
  // parse name
  while (I <= L) and CharInSet(ADeclaration[I], NULLSET) do
    Inc(I);
  if I > L then
    Abort('Identifier name expected!');
  if not CharInSet(ADeclaration[I], ALPHASET) then
    Abort('Identifier expected!');
  AName := '';
  repeat
    AName := AName + ADeclaration[I];
    Inc(I);
  until (I > L) or not CharInSet(ADeclaration[I], ALPHASETEX);
  // parse params
  AParams.Clear;
  while (I <= L) and CharInSet(ADeclaration[I], NULLSET) do
    Inc(I);
  if I > L then
    Exit;
  if ADeclaration[I] <> '(' then
    Abort('Brackets expected!');
  Inc(I);
  while I <= L do
  begin
    while (I <= L) and CharInSet(ADeclaration[I], NULLSET) do
      Inc(I);
    if (I > L) or (ADeclaration[I] = ')') then
      Break;
    if not CharInSet(ADeclaration[I], ALPHASET) then
      Abort('Identifier expected!');
    N := '';
    repeat
      N := N + ADeclaration[I];
      Inc(I);
    until (I > L) or not CharInSet(ADeclaration[I], ALPHASETEX);
    AParams.Add(N); 
    while (I <= L) and CharInSet(ADeclaration[I], NULLSET) do
      Inc(I);
    if I > L then
      Abort('Unclosed brackets!');
    if ADeclaration[I] = ')' then
      Break
    else if ADeclaration[I] <> ',' then
      Abort('Sintax error!');
    Inc(I);
  end;
end;

function TRLExpressionParser.CreateFunction(const ADeclaration: String; const AExpression: String): TRLParserFunction;
var
  pars: TStringList;
  fnam: String;
  func: TRLParserFunction;
  I: Integer;
begin
  pars := TStringList.Create;
  try
    ParseFunction(ADeclaration, fnam, pars);
    //
    func := FunctionList.ByName(fnam);
    if func <> nil then
      func.Free;
    //
    Result := TRLParserFunction.Create(Self);
    with Result do
    begin
      Name := fnam;
      Expression := AExpression;
      for I := 0 to pars.Count - 1 do
        TRLParserParam.Create(Result).Name := pars[I];
    end;
  finally
    pars.Free;
  end;
end;

function TRLExpressionParser.CreateValue(const AName: String; const AValue: variant): TRLParserValue;
var
  valu: TRLParserValue;
begin
  valu := ValueList.ByName(AName);
  if valu <> nil then
    valu.Free;
  Result := TRLParserValue.Create(Self);
  Result.Name := AName;
  Result.Value := AValue;
end;

function ArrayMinor(AValues: variant): variant;
var
  I: Integer;
begin
  Result := Unassigned;
  for I := VarArrayLowBound(AValues, 1) to VarArrayHighBound(AValues, 1) do
    if VarIsEmpty(Result) or (AValues[I] < Result) then
      Result := AValues[I];
end;

function ArrayMajor(AValues: variant): variant;
var
  I: Integer;
begin
  Result := Unassigned;
  for I := VarArrayLowBound(AValues, 1) to VarArrayHighBound(AValues, 1) do
    if VarIsEmpty(Result) or (AValues[I] > Result) then
      Result := AValues[I];
end;

function TRLExpressionParser.GetPropValue(APersistent: TPersistent; const APropName: String): variant;
var
  info: PPropInfo;
begin
  Result := Unassigned;
  if Assigned(FGetAttributeProc) then
    FGetAttributeProc(Self, APersistent, APropName, Result);
  if VarIsEmpty(Result) then
    if APropName = '' then
      if Assigned(APersistent) then
        Result := '[' + APersistent.ClassName + ']'
      else
        Result := 'nil'
    else
    begin
      info := GetPropInfo(APersistent, APropName);
      if info <> nil then
        case info^.PropType^{$ifndef FPC}^{$endif}.Kind of
          tkInteger, 
          tkChar, 
          tkWChar: Result := GetOrdProp(APersistent, info);
          tkEnumeration: Result := GetEnumProp(APersistent, info);
          tkSet: Result := GetSetProp(APersistent, info, False);
          tkFloat: Result := GetFloatProp(APersistent, info);
          tkMethod: Result := info^.PropType^.Name;
          tkString, 
          tkLString, 
          tkWString: Result := GetStrProp(APersistent, info);
          tkVariant: Result := GetVariantProp(APersistent, info);
          tkInt64: Result := GetInt64Prop(APersistent, info) + 0.0;
        end;
    end;
end;

function TRLExpressionParser.SetPropValue(APersistent: TPersistent; const APropName: String; AValue: variant): Boolean;
var
  info: PPropInfo;
begin
  Result := False;
  if APropName <> '' then
  begin
    info := GetPropInfo(APersistent, APropName);
    if info <> nil then
      case info^.PropType^{$ifndef FPC}^{$endif}.Kind of
        tkInteger, 
        tkChar, 
        tkWChar: begin
                         SetOrdProp(APersistent, info, AValue);
                         Result := True;
                       end;
        tkEnumeration: begin
                         SetEnumProp(APersistent, info, AValue);
                         Result := True;
                       end;
        tkSet: begin
                         SetSetProp(APersistent, info, AValue);
                         Result := True;
                       end;
        tkFloat: begin
                         SetFloatProp(APersistent, info, AValue);
                         Result := True;
                       end;
        tkMethod: ;
        tkString, 
        tkLString, 
        tkWString: begin
                         SetStrProp(APersistent, info, AValue);
                         Result := True;
                       end;
        tkVariant: begin
                         SetVariantProp(APersistent, info, AValue);
                         Result := True;
                       end;
        tkInt64: begin
                         SetInt64Prop(APersistent, info, PtrInt(AValue));
                         Result := True;
                       end;
      end;
  end;
  if not Result and Assigned(FSetAttributeProc) then
    FSetAttributeProc(Self, APersistent, APropName, AValue, Result);
end;

function TRLExpressionParser.FindDependentPart(AOwner: TPersistent; const AName: String): TPersistent;
  function FindComponent(AOwner: TComponent; const AName: String): TComponent;
  var
    I: Integer;
  begin
    Result := nil;
    if AnsiSameText(AOwner.Name, AName) then
      Result := AOwner
    else
    begin
      I := 0;
      while (I < AOwner.ComponentCount) and not AnsiSameText(AOwner.Components[I].Name, AName) do
        Inc(I);
      if I < AOwner.ComponentCount then
        Result := AOwner.Components[I];
    end; 
  end;
  function FindAgregate(AOwner: TPersistent; const AName: String): TPersistent;
  var
    info: PPropInfo;
    obj: TObject;
  begin
    Result := nil;
    info := GetPropInfo(AOwner, AName);
    if Assigned(info) and (info^.PropType^{$ifndef FPC}^{$endif}.Kind = tkClass) then
    begin
      obj := TObject(GetOrdProp(AOwner, info));
      if Assigned(obj) and (obj is TPersistent) then
        Result := TPersistent(obj);
    end;
  end;
begin
  Result := nil;
  if Assigned(FFindAgregateProc) then
  begin
    FFindAgregateProc(Self, AOwner, AName, Result);
    if Result <> nil then
      Exit;
  end;
  if AOwner is TComponent then
  begin
    Result := FindComponent(TComponent(AOwner), AName);
    if Result <> nil then
      Exit;
  end;
  if Result = nil then
  begin
    Result := FindAgregate(AOwner, AName);
    if Result <> nil then
      Exit;
  end;
end;

function TRLExpressionParser.FindPropertyPair(AOwner: TPersistent; const APropPath: String; var APersistent: TPersistent; var APropName: String): Boolean;
var
  parent: TPersistent;
  found: TPersistent;
  path: String;
  part: String;
  I: Integer;
begin
  Result := False;
  parent := AOwner;
  path := APropPath;
  repeat
    I := Pos('.', path);
    if I = 0 then
      I := Length(path) + 1;
    part := Copy(path, 1, I - 1);
    Delete(path, 1, I);
    if part = '' then
      Break;
    // procura parte dependente, podendo apelar para o usuário  
    found := FindDependentPart(parent, part);
    // se não achou a parte dependente, então ela talvez seja uma prop
    if found = nil then
      // mas, se ainda houver mais partes a procurar, então desiste
      if path <> '' then
        Break
      else
      begin
        APersistent := parent;
        APropName := part;
        Result := True;
        Break;
      end
    // se encontrou a parte e não há outras partes a procurar, então retorna a parte sem prop
    else if path = '' then
    begin
      APersistent := found;
      APropName := '';
      Result := True;
      Break;
    end
    // senão, procura dentro da parte encontrada
    else
      parent := found;
  until False; 
end;

function TRLExpressionParser.GetGlobalProperty(const APropPath: String): variant;
var
  parent: TPersistent;
  prop: String;
begin
  if FindPropertyPair(Self.Owner, APropPath, parent, prop) then
    Result := GetPropValue(parent, prop)
  else
    Result := Unassigned;
end;

function TRLExpressionParser.SetGlobalProperty(const APropPath: String; AValue: variant): Boolean;
var
  parent: TPersistent;
  prop: String;
begin
  if FindPropertyPair(Self.Owner, APropPath, parent, prop) then
    Result := SetPropValue(parent, prop, AValue)
  else
    Result := False;
end;

function TRLExpressionParser.DoResolveIdentifier(const AIdentifier: String; AParams: variant): variant;
var
  fncitem: TRLParserFunction;
  valitem: TRLParserValue;
  prmitem: TRLParserParam;
begin
  Result := Unassigned;
  // o usuário resolve previamente
  if Assigned(FOnIdentifier) then
  begin
    // user defined
    FOnIdentifier(Self, AIdentifier, AParams, Result);
    if not VarIsEmpty(Result) then
      Exit;
  end;
  // function params
  if Assigned(FParams) then
  begin
    prmitem := FParams.ByName(AIdentifier);
    if Assigned(prmitem) then
    begin
      Result := prmitem.Value;
      Exit;
    end;
  end;
  // stored functions //
  fncitem := FFunctionList.ByName(AIdentifier);
  if Assigned(fncitem) then
  begin
    Result := fncitem.Evaluate(AParams);
    Exit;
  end;
  // stored values //
  valitem := FValueList.ByName(AIdentifier);
  if Assigned(valitem) then
  begin
    Result := valitem.Value;
    Exit;
  end;
  // predefined constants //
  // pi
  if AnsiSameText(AIdentifier, 'pi') then
    Result := pi
  // boolean
  else if AnsiSameText(AIdentifier, 'true') then
    Result := True
  else if AnsiSameText(AIdentifier, 'false') then
    Result := False
  // predefined functions //
  // min & max
  else if AnsiSameText(AIdentifier, 'minor') then
    Result := ArrayMinor(AParams)
  else if AnsiSameText(AIdentifier, 'major') then
    Result := ArrayMajor(AParams)
  else
  begin
    // internal use only
    if Assigned(FResourceProc) then
      FResourceProc(Self, AIdentifier, AParams, Result);
    if VarIsEmpty(Result) and (poScanComponents in FOptions) then
      Result := GetGlobalProperty(AIdentifier);
    // user defined
    if VarIsEmpty(Result) and Assigned(FOnUnknown) then
      FOnUnknown(Self, AIdentifier, AParams, Result);
  end;
end;

procedure TRLExpressionParser.ResolveIdentifier(const AIdentifier: String; AId: Integer; AParams: variant; var AResult: variant);
var
  savedid: Integer;
begin
  // save id
  savedid := FIdentifierId;
  try
    FIdentifierId := AId;
    AResult := DoResolveIdentifier(AIdentifier, AParams);
    if VarIsEmpty(AResult) then
      Abort('Undefined identifier "' + AIdentifier + '"');
  finally
    // restore id
    FIdentifierId := savedid;
  end;
end;

function TRLExpressionParser.EvalNode(ANode: TRLParserNode): variant;
var
  I: Integer;
  P: variant;
begin
  case ANode.Operation of
    opNull: Result := Unassigned;
    //
    opIdentifier: begin
                    // builtin
                    if AnsiSameText(ANode.Value, 'if') then
                      if ANode.ChildCount <> 3 then
                        Abort('Wrong number of arguments')
                      else if ANode.Children[0].Eval then
                        Result := ANode.Children[1].Eval
                      else
                        Result := ANode.Children[2].Eval
                    else
                    begin
                      if ANode.ChildCount = 0 then
                        P := Unassigned
                      else
                      begin
                        P := VarArrayCreate([0, ANode.ChildCount - 1], varVariant);
                        for I := 0 to ANode.ChildCount - 1 do
                          P[I] := ANode.Children[I].Eval;
                      end;
                      ResolveIdentifier(ANode.Value, ANode.GetRootDistance, P, Result);
                    end;
                  end;
    opValue: Result := ANode.Value;
    opLiteral: Result := ANode.Value;
    //
    opSum: Result := ANode.Children[0].Eval + ANode.Children[1].Eval;
    opSubtract: Result := ANode.Children[0].Eval - ANode.Children[1].Eval;
    opMultiply: Result := ANode.Children[0].Eval * ANode.Children[1].Eval;
    opDivide: Result := ANode.Children[0].Eval / ANode.Children[1].Eval;
    opPower: Result := Power(ANode.Children[0].Eval, ANode.Children[1].Eval);
    opRoot: Result := Power(ANode.Children[0].Eval, 1 / ANode.Children[1].Eval);
    //
    opPlus: Result := ANode.Children[0].Eval;
    opMinus: Result := -ANode.Children[0].Eval;
    opNot: Result := not ANode.Children[0].Eval;
    //
    opGreater: Result := ANode.Children[0].Eval > ANode.Children[1].Eval;
    opLess: Result := ANode.Children[0].Eval < ANode.Children[1].Eval;
    opEqual: Result := ANode.Children[0].Eval = ANode.Children[1].Eval;
    opInequal: Result := ANode.Children[0].Eval <> ANode.Children[1].Eval;
    opNotLess: Result := ANode.Children[0].Eval >= ANode.Children[1].Eval;
    opNotGreater: Result := ANode.Children[0].Eval <= ANode.Children[1].Eval;
    //
    opOr: Result := ANode.Children[0].Eval or ANode.Children[1].Eval;
    opAnd: Result := ANode.Children[0].Eval and ANode.Children[1].Eval;
    //
    opSet: begin
                    if ANode.Children[0].Operation <> opIdentifier then
                      Abort('Left side cannot be Assigned');
                    Result := ANode.Children[1].Eval;
                    if (poScanComponents in FOptions) and SetGlobalProperty(ANode.Children[0].Value, Result) then
                    else
                      CreateValue(ANode.Children[0].Value, Result);
                  end;
    //
    opInline: for I := 0 to ANode.ChildCount - 1 do
                    Result := ANode.Children[I].Eval;
  else
    Result := Unassigned;
  end;
end;

procedure TRLExpressionParser.Invalidate;
begin
  if Assigned(FRootNode) then
    FRootNode.Free;
  FRootNode := nil;
end;

function TRLExpressionParser.TryEvalNode(ANode: TRLParserNode): variant;
begin
  Result := Unassigned;
  if Assigned(ANode) then
    try
      Result := ANode.Eval;
    except
      on E: Exception do
        if Assigned(FOnException) then
          FOnException(Self, E, Result)
        else
          raise;
    end;
end;

function TRLExpressionParser.InternalEvaluate(const AExpression: String): variant;
var
  root: TRLParserNode;
begin
  root := nil;
  try
    Compile(AExpression, root);
    Result := TryEvalNode(root);
  finally
    if Assigned(root) then
      root.Free;
  end;
end;

function TRLExpressionParser.Evaluate: variant;
begin
  if not Assigned(FRootNode) then
    Compile(FExpression, FRootNode);
  Result := TryEvalNode(FRootNode);
end;

function TRLExpressionParser.Evaluate(const AExpression: String): variant;
begin
  Result := InternalEvaluate(AExpression);
end;

procedure TRLExpressionParser.SetExpression(const Value: String);
begin
  if Value = FExpression then
    Exit;
  FExpression := Value;
  Invalidate;
end;

{ TRLParserNode }

constructor TRLParserNode.Create(AParser: TRLExpressionParser; const AOperation: TRLParserOperation; const AValue: String = '');
var
  E: Integer;
  V: Double;
begin
  FParser := AParser;
  FParentNode := nil;
  FChildren := nil;
  FIsolated := False;
  FOperation := AOperation;
  FPriority := OperationPriority(FOperation);
  //
  case FOperation of
    opIdentifier: FValue := AValue;
    opValue: begin
                    val(AValue, V, E);
                    FValue := V;
                  end; 
    opLiteral: FValue := AValue;
  end;
  //
  FChildren := TObjectList.Create;
  //
  inherited Create;
end;

destructor TRLParserNode.Destroy;
begin
  if Assigned(FParentNode) then
    FParentNode.ChildList.Extract(Self);
  if Assigned(FChildren) then
    FChildren.Free;
  //
  inherited;
end;

function TRLParserNode.Eval: variant;
begin
  Result := FParser.EvalNode(Self);
end;

procedure TRLParserNode.InsertBefore(ANode: TRLParserNode);
begin
  ANode.ParentNode := Self.ParentNode;
  Self.ParentNode := ANode;
end;

procedure TRLParserNode.AddChild(ANode: TRLParserNode);
begin
  ANode.ParentNode := Self;
end;

function TRLParserNode.GetRootDistance: Integer;
begin
  if ParentNode <> nil then
    Result := ParentNode.ChildList.IndexOf(Self) + 1 + ParentNode.GetRootDistance
  else
    Result := 0;
end;

procedure TRLParserNode.SetParentNode(const Value: TRLParserNode);
begin
  if Assigned(FParentNode) then
    FParentNode.ChildList.Extract(Self);
  FParentNode := Value;
  if Assigned(FParentNode) then
    FParentNode.ChildList.Add(Self);
end;

function TRLParserNode.GetChildren(AIndex: Integer): TRLParserNode;
begin
  Result := TRLParserNode(FChildren[AIndex]);
end;

function TRLParserNode.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

{ TRLParserFunction }

constructor TRLParserFunction.Create(AParser: TRLExpressionParser);
begin
  FParser := AParser;
  FName := '';
  FExpression := '';
  FParams := nil;
  FRootNode := nil;
  //
  FParams := TRLParserParamList.Create;
  //
  inherited Create;
  //
  if Assigned(Parser) and (Parser.FunctionList.IndexOf(Self) = -1) then
    Parser.FunctionList.Add(Self);
end;

destructor TRLParserFunction.Destroy;
begin
  if Assigned(Parser) then
    Parser.FunctionList.Extract(Self);
  //
  if Assigned(FRootNode) then
    FRootNode.free;
  if Assigned(FParams) then
    FParams.Free;
  //
  inherited;
end;

function TRLParserFunction.InternalEvaluate: variant;
var
  savedparams: TRLParserParamList;
begin
  if not Assigned(FRootNode) then
    Parser.Compile(FExpression, FRootNode);
  savedparams := Parser.FParams;
  try
    Parser.FParams := Self.FParams;
    Result := Parser.TryEvalNode(FRootNode);
  finally
    Parser.FParams := savedparams;
  end;
end;

function TRLParserFunction.Evaluate: variant;
begin
  Params.Reset;
  Result := InternalEvaluate;
end;

function TRLParserFunction.Evaluate(AParams: variant): variant;
begin
  Params.Assign(AParams);
  Result := InternalEvaluate;
end;

function TRLParserFunction.Evaluate(AParams: array of variant): variant;
begin
  Params.Assign(AParams);
  Result := InternalEvaluate;
end;

procedure TRLParserFunction.Invalidate;
begin
  if Assigned(FRootNode) then
    FRootNode.Free;
  FRootNode := nil;
end;

procedure TRLParserFunction.SetExpression(const Value: String);
begin
  if Value = FExpression then
    Exit;
  FExpression := Value;
  Invalidate;
end;

{ TRLParserFunctionList }

function TRLParserFunctionList.ByIndex(AIndex: Integer): TRLParserFunction;
begin
  Result := TRLParserFunction(Items[AIndex]);
end;

function TRLParserFunctionList.ByName(const AName: String): TRLParserFunction;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AnsiSameText(ByIndex(I).Name, AName) then
    begin
      Result := ByIndex(I);
      Break;
    end;
end;

{ TRLParserParam }

constructor TRLParserParam.Create(AFunction: TRLParserFunction);
begin
  FFunction := AFunction;
  FName := '';
  FValue := Unassigned;
  //
  inherited Create;
  //
  if Assigned(FFunction) then
    FFunction.Params.Add(Self);
end;

destructor TRLParserParam.Destroy;
begin
  if Assigned(FFunction) then
    FFunction.Params.Extract(Self);
  //
  inherited;
end;

procedure TRLParserParam.Clear;
begin
  FValue := Unassigned;
end;

{ TRLParserParamList }

function TRLParserParamList.ByIndex(AIndex: Integer): TRLParserParam;
begin
  Result := TRLParserParam(Items[AIndex]);
end;

function TRLParserParamList.ByName(const AName: String): TRLParserParam;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AnsiSameText(ByIndex(I).Name, AName) then
    begin
      Result := ByIndex(I);
      Break;
    end;
end;

procedure TRLParserParamList.Reset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ByIndex(I).Clear;
end;

procedure TRLParserParamList.Assign(AParams: variant);
var
  I, J, i1, i2: Integer;
begin
  Reset;
  if (VarType(AParams) and varArray) = varArray then
  begin
    i1 := VarArrayLowBound(AParams, 1);
    i2 := VarArrayHighBound(AParams, 1);
    for I := i1 to i2 do
    begin
      J := I - i1;
      if J < Count then
        ByIndex(J).Value := AParams[I];
    end;
  end
  else if not VarIsEmpty(AParams) and (Count > 0) then
    ByIndex(0).Value := AParams;
end;

procedure TRLParserParamList.Assign(AParams: array of variant);
var
  I: Integer;
begin
  Reset;
  for I := 0 to High(AParams) do
    if I < Count then
      ByIndex(I).Value := AParams[I];
end;


{ TRLParserValue }

constructor TRLParserValue.Create(AParser: TRLExpressionParser);
begin
  FParser := AParser;
  FName := '';
  FValue := Unassigned;
  //
  inherited Create;
  //
  if Assigned(Parser) and (Parser.ValueList.IndexOf(Self) = -1) then
    Parser.ValueList.Add(Self);
end;

destructor TRLParserValue.Destroy;
begin
  if Assigned(Parser) then
    Parser.ValueList.Extract(Self);
  //
  inherited;
end;

{ TRLParserValueList }

function TRLParserValueList.ByIndex(AIndex: Integer): TRLParserValue;
begin
  Result := TRLParserValue(Items[AIndex]);
end;

function TRLParserValueList.ByName(const AName: String): TRLParserValue;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AnsiSameText(ByIndex(I).Name, AName) then
    begin
      Result := ByIndex(I);
      Break;
    end;
end;

end.

