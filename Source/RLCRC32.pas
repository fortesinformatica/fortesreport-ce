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

unit RLCRC32;

interface

uses
  Classes;

{@proc CRC32Begin - Inicializa uma variável para o cálculo do CRC em blocos. :/}
procedure CRC32Begin(var CRC:Cardinal);
{@proc CRC32Write - Adiciona um bloco de bytes para o cálculo do CRC. :/}
procedure CRC32Write(var CRC:Cardinal; Buffer:PAnsiChar; BufLen:Integer);
{@proc CRC32End - Encerra o cálculo do CRC em blocos. :/}
procedure CRC32End(var CRC:Cardinal);

{@proc CRC32 - Calcula o CRC de uma string. :/}
function CRC32(const Data:AnsiString):Cardinal; overload;

{@proc CRC32 - Calcula o CRC de um buffer. :/}
function CRC32(Buffer:PAnsiChar; BufLen:Integer):Cardinal; overload;

{@proc CRC32 - Calcula o CRC de uma stream a partir da posição atual.
 Opcionalmente copia os dados para uma segunda stream. :/}
function CRC32(Stream:TStream; Count:Int64; DestStream:TStream=nil):Cardinal; overload;

implementation

var
  CRCTable:array[0..255] of Cardinal=(
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9,
    $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A,
    $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950,
    $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5,
    $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6,
    $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C,
    $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31,
    $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7,
    $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8,
    $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

procedure CRC32Begin(var CRC:Cardinal);
begin
  CRC:=$FFFFFFFF;
end;

procedure CRC32Write(var CRC:Cardinal; Buffer:PAnsiChar; BufLen:Integer);
var
  i:Integer;
begin
  for i:=0 to BufLen-1 do
    CRC:=(CRC shr 8) xor (CRCTable[Byte(CRC) xor Ord(Buffer[i])]);
end;

procedure CRC32End(var CRC:Cardinal);
begin
  CRC:=CRC xor $FFFFFFFF;
end;

function CRC32(const Data:AnsiString):Cardinal;
var
  Aux: AnsiString;
begin
  Aux := Data;
  Result:=CRC32(@Aux[1],Length(Aux));
end;

function CRC32(Buffer:PAnsiChar; BufLen:Integer):Cardinal;
begin
  CRC32Begin(Result);
  CRC32Write(Result,Buffer,BufLen);
  CRC32End(Result);
end;

function CRC32(Stream:TStream; Count:Int64; DestStream:TStream=nil):Cardinal;
const
  MegaByte=1*1024*1024; // 1MB 
var
  Buffer    :PAnsiChar;
  BufLen    :Int64;
  TotalRead :Int64;
  OptimalBuf:Int64;
  Aux       :Int64;
  Dif       :Int64;
begin
  Aux:=Count div MegaByte; // quantas unidades de megas tem em
  Dif:=0;
  if Aux>0 then
  begin
    OptimalBuf:=Count div Aux;
    Dif:=Count mod Aux;
  end
  else
    OptimalBuf:=Count;
  GetMem(Buffer,OptimalBuf+1);
  try
    CRC32Begin(Result);
    TotalRead:=0;
    while TotalRead<Count do
    begin
      BufLen:=Count-TotalRead;
      if BufLen>OptimalBuf then
        BufLen:=OptimalBuf;
      if Dif>0 then
      begin
        Inc(BufLen);
        Dec(Dif);
      end;
      BufLen:=Stream.Read(Buffer^,BufLen);
      if BufLen=0 then
        Break;
      CRC32Write(Result,Buffer,BufLen);
      DestStream.Write(Buffer^,BufLen);
      Inc(TotalRead,BufLen);
    end;
    CRC32End(Result);
  finally
    FreeMem(Buffer);
  end;
end;

end.
