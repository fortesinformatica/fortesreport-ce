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

{@unit RLTypes - Definição de tipos comuns e rotinas de manipulação de tipos. }
unit RLTypes;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  Classes, Printers;

type
  TRLPrinterMetrics = record
    PPIX: Integer;
    PPIY: Integer;
    PhysicalWidth: Integer;
    PhysicalHeight: Integer;
    MarginLeft: Integer;
    MarginTop: Integer;
    MarginRight: Integer;
    MarginBottom: Integer;
    ClientWidth: Integer;
    ClientHeight: Integer;
  end;

  {$IfNDef FPC}
   {$IFDEF WIN64}
    PtrInt = NativeInt;
   {$ELSE}
    PtrInt = Integer;
   {$ENDIF}
  {$EndIf}

  TRLSystemPaperType = Integer;
  TRLSystemOrientation = Integer;

  TRLPaperInfo = record
                 Width: Double;
                 Height: Double;
                 Description: String;
                 Emulated: Boolean;
                 SystemCode: TRLSystemPaperType;
               end;

  TRLPageOrientation = (poPortrait, poLandscape);

  TRLPaperSize = (fpA0, fpA1, fpA2, fpA3, fpA4, fpA5, fpA6, fpA7, fpA8, fpA9, fpA10, 
                fpB0, fpB1, fpB2, fpB3, fpB4, fpB5, fpB6, fpB7, fpB8, fpB9, fpB10, 
                fpSRA0, fpSRA1, fpSRA2, fpSRA3, fpSRA4, 
                fpRA0, fpRA1, fpRA2, fpRA3, fpRA4, 
                fp2A, fp2B, fp4A, fp4B, 
                fp9x11, fp10x11, fp10x14, fp11x17, fp12x11, fp15x11, 
                fpA3_Extra, fpA3_Extra_Transverse, fpA3_Transverse, fpA4_Small, fpA4_Extra, fpA4_Plus, fpA4_Transverse, fpA5_Extra, fpA5_Transverse, fpA_Plus, 
                fpB4_JIS, fpB5_Extra, fpB5_Transverse, fpB6_JIS, fpB_Plus, 
                fpEnv_9, fpEnv_10, fpEnv_11, fpEnv_12, fpEnv_14, 
                fpEnv_B4, fpEnv_B5, fpEnv_B6, 
                fpEnv_C0, fpEnv_C1, fpEnv_C2, fpEnv_C3, fpEnv_C4, fpEnv_C5, fpEnv_C5E, fpEnv_C6, fpEnv_C65, fpEnv_C7, fpEnv_C8, fpEnv_C9, fpEnv_C10, 
                fpEnv_DL, fpEnv_Invite, fpEnv_Italy, fpEnv_Monarch, fpEnv_Personal, 
                fpLetter, fpLegal, fpLedger, fpTabloid, fpExecutive, fpFanfold_US, fpComm10E, fpDLE, fpFolio, fpNote, fpPostcard, fpQuarto, fpStatement, 
                fpLegal_Extra, fpLegal3, fpLetter_Small, fpLetter_Extra, fpLetter_Extra_Transverse, fpLetter_Plus, fpLetter_Transverse, 
                fpP16K, fpP32K, fpP32KBig, 
                fpPEnv_1, fpPEnv_2, fpPEnv_3, fpPEnv_4, fpPEnv_5, fpPEnv_6, fpPEnv_7, fpPEnv_8, fpPEnv_9, fpPEnv_10, 
                fpTabloid_Extra, 
                fpGerman_Legal_Fanfold, fpGerman_Std_Fanfold, 
                fpJap_DblPostcard, fpJap_Postcard, 
                fpEng_Foolscap, fpEng_DoubleFoolscap, fpEng_QuadFoolscap, fpEng_Crown, fpEng_DoubleCrown, fpEng_QuadCrown, fpEng_DoubleQuad, fpEng_Post, 
                fpEng_DoublePost, fpEng_DoubleLarge, fpEng_Demy, fpEng_DoubleDemy, fpEng_QuadDemy, fpEng_MusicDemy, fpEng_Medium, fpEng_Royal, fpEng_SuperRoyal, 
                fpEng_Elephant, fpEng_Imperial, 
                fpCustom);

const
{$IfDef FPC}
  DMPAPER_USER = 256;
  DMORIENT_PORTRAIT = 1;
  DMORIENT_LANDSCAPE = 2;
  DMBIN_MANUAL = 4;
{$EndIf}
  UserPaperCode = DMPAPER_USER;

var
  PaperInfo: array[TRLPaperSize] of TRLPaperInfo;

function PaperSizeBySize(APaperWidth, APaperHeight: Double): TRLPaperSize;
                          
procedure DetectPaperSize(APaperWidth, APaperHeight: Double;
                          AOrientationLandscape: Boolean;
                          AForceEmulation: Boolean;
                          var AResultPaperSize: TRLSystemPaperType;
                          var AResultPaperWidth, AResultPaperHeight: Double;
                          var AResultOrientation: TRLSystemOrientation);

{/@unit}

implementation

function PaperSizeBySize(APaperWidth, APaperHeight: Double): TRLPaperSize;
var
  PaperSize: TRLPaperSize;
  Distance: Double;
  NearestPaperSize: TRLPaperSize;
  NearestDistance: Double;
function HowDistant(APaperSize: TRLPaperSize): Double;
begin
  Result := Abs(APaperWidth - PaperInfo[APaperSize].Width) + 
          Abs(APaperHeight - PaperInfo[APaperSize].Height);
end;
begin
  NearestPaperSize := fpCustom;
  NearestDistance := 0;
  // procura o tamanho padrão que mais se aproxima do informado
  for PaperSize := Low(TRLPaperSize) to High(TRLPaperSize) do
    if (PaperSize <> fpCustom) and not PaperInfo[PaperSize].Emulated then
    begin
      Distance := HowDistant(PaperSize);
      if (Distance < 1 / 100) and ((NearestPaperSize = fpCustom) or (Distance < NearestDistance)) then
      begin
        NearestPaperSize := PaperSize;
        NearestDistance := Distance;
      end;
    end;
  //
  Result := NearestPaperSize;
end;

procedure DetectPaperSize(APaperWidth, APaperHeight: Double;
                          AOrientationLandscape: Boolean;
                          AForceEmulation: Boolean;
                          var AResultPaperSize: TRLSystemPaperType;
                          var AResultPaperWidth, AResultPaperHeight: Double;
                          var AResultOrientation: TRLSystemOrientation);
var
  PaperSize: TRLPaperSize;
begin
  PaperSize := PaperSizeBySize(APaperWidth, APaperHeight);
  //
  if PaperSize = fpCustom then
  begin
    AResultPaperSize := UserPaperCode;
    AResultPaperWidth := APaperWidth;
    AResultPaperHeight := APaperHeight;
  end
  else
  begin
    AResultPaperSize := PaperInfo[PaperSize].SystemCode;
    AResultPaperWidth := PaperInfo[PaperSize].Width;
    AResultPaperHeight := PaperInfo[PaperSize].Height;
  end;
{$ifndef CLX}
  if AOrientationLandscape then
    AResultOrientation := DMORIENT_LANDSCAPE
  else
    AResultOrientation := DMORIENT_PORTRAIT;
{$else}
  if AOrientationLandscape then
    AResultOrientation := QPrinters.poLandscape
  else
    AResultOrientation := QPrinters.poPortrait;
{$endif}
end;

procedure SetPaperInfo(APaperSize: TRLPaperSize; const AWidth, AHeight: Double; const ADescription: String);
begin
  with PaperInfo[APaperSize] do
  begin
    Width := AWidth;
    Height := AHeight;
    Description := ADescription;
    Emulated := True;
  end;
end;

procedure SetPaperEqv(APaperSize: TRLPaperSize; ASysCode: TRLSystemPaperType);
begin
  with PaperInfo[APaperSize] do
  begin
    SystemCode := ASysCode;
    Emulated := False;
  end;
end;

initialization
  // paper sizes
  SetPaperInfo(fpA0, 841, 1189, 'A0 841 x 1189 mm');
  SetPaperInfo(fpA1, 594, 841, 'A1 594 x 841 mm');
  SetPaperInfo(fpA2, 420, 594, 'A2 420 x 594 mm');
  SetPaperInfo(fpA3, 297, 420, 'A3 297 x 420 mm');
  SetPaperInfo(fpA4, 210, 297, 'A4 210 x 297 mm');
  SetPaperInfo(fpA5, 148, 210, 'A5 148 x 210 mm');
  SetPaperInfo(fpA6, 105, 148, 'A6 105 x 148 mm');
  SetPaperInfo(fpA7, 74, 105, 'A7 74 x 105 mm');
  SetPaperInfo(fpA8, 52, 74, 'A8 52 x 74 mm');
  SetPaperInfo(fpA9, 37, 52, 'A9 37 x 52 mm');
  SetPaperInfo(fpA10, 26, 37, 'A10 26 x 37 mm');
  //
  SetPaperInfo(fpB0, 1000, 1414, 'B0 1000 x 1414 mm');
  SetPaperInfo(fpB1, 707, 1000, 'B1 707 x 1000 mm');
  SetPaperInfo(fpB2, 500, 707, 'B2 500 x 707 mm');
  SetPaperInfo(fpB3, 353, 500, 'B3 353 x 500 mm');
  SetPaperInfo(fpB4, 250, 353, 'B4 250 x 353 mm');
  SetPaperInfo(fpB5, 176, 250, 'B5 176 x 250 mm');
  SetPaperInfo(fpB6, 125, 176, 'B6 125 x 176 mm');
  SetPaperInfo(fpB7, 88, 125, 'B7 88 x 125 mm');
  SetPaperInfo(fpB8, 62, 88, 'B8 62 x 88 mm');
  SetPaperInfo(fpB9, 44, 62, 'B9 44 x 62 mm');
  SetPaperInfo(fpB10, 31, 44, 'B10 31 x 44 mm');
  //
  SetPaperInfo(fpSRA0, 900, 1280, 'SRA0 900 x 1280 mm');
  SetPaperInfo(fpSRA1, 640, 900, 'SRA1 640 x 900 mm');
  SetPaperInfo(fpSRA2, 450, 640, 'SRA2 450 x 640 mm');
  SetPaperInfo(fpSRA3, 320, 450, 'SRA3 320 x 450 mm');
  SetPaperInfo(fpSRA4, 225, 320, 'SRA4 225 x 320 mm');
  SetPaperInfo(fpRA0, 860, 1220, 'RA0 860 x 1220 mm');
  SetPaperInfo(fpRA1, 610, 860, 'RA1 610 x 860 mm');
  SetPaperInfo(fpRA2, 430, 610, 'RA2 430 x 610 mm');
  SetPaperInfo(fpRA3, 305, 430, 'RA3 305 x 430 mm');
  SetPaperInfo(fpRA4, 215, 305, 'RA3 215 x 305 mm');
  //
  SetPaperInfo(fp2A, 1189, 1682, '2A 1189 x 1682 mm');
  SetPaperInfo(fp2B, 1414, 2000, '2B 1414 x 2000 mm');
  SetPaperInfo(fp4A, 1682, 2378, '4A 1682 x 2378 mm');
  SetPaperInfo(fp4B, 2000, 2828, '4B 2000 x 2828 mm');
  //
  SetPaperInfo(fp9x11, 228.6, 279.4, '9x11 228.6 x 279.4 mm');
  SetPaperInfo(fp10x11, 254, 279.4, '10x11 254 x 279.4 mm');
  SetPaperInfo(fp10x14, 254, 355.6, '10x14 254 x 355.6 mm');
  SetPaperInfo(fp11x17, 279.4, 431.8, '11x17 279.4 x 431.8 mm');
  SetPaperInfo(fp12x11, 304.8, 279.4, '12x11 304.8 x 279.4 mm');
  SetPaperInfo(fp15x11, 381, 279.4, '15x11 381 x 279.4 mm');
  //
  SetPaperInfo(fpA3_Extra, 322.44, 445.2, 'A3 Extra 322.44 x 445.2 mm');
  SetPaperInfo(fpA3_Extra_Transverse, 322, 445, 'A3 Extra Transverse 322 x 445 mm');
  SetPaperInfo(fpA3_Transverse, 297, 420, 'A3 Transverse 297 x 420 mm');
  SetPaperInfo(fpA4_Small, 210, 297, 'A4 Small 210 x 297 mm');
  SetPaperInfo(fpA4_Extra, 235.3, 322.44, 'A4 Extra 235.3 x 322.44 mm');
  SetPaperInfo(fpA4_Plus, 210, 330, 'A4 Plus 210 x 330 mm');
  SetPaperInfo(fpA4_Transverse, 210, 297, 'A4 Transverse 210 x 297 mm');
  SetPaperInfo(fpA5_Extra, 174, 235, 'A5 Extra 174 x 235 mm');
  SetPaperInfo(fpA5_Transverse, 148, 210, 'A5 Transverse 148 x 210 mm');
  SetPaperInfo(fpA_Plus, 227, 356, 'SuperA/SuperA/A4 227 x 356 mm');
  //
  SetPaperInfo(fpB4_JIS, 250, 354, 'B4 (JIS) 250 x 354');
  SetPaperInfo(fpB5_Extra, 201, 276, 'B5 (ISO) Extra 201 x 276 mm');
  SetPaperInfo(fpB5_Transverse, 182, 257, 'B5 (JIS) Transverse 182 x 257 mm');
  SetPaperInfo(fpB6_JIS, 128, 182, 'B6 (JIS) 128 x 182 mm');
  SetPaperInfo(fpB_Plus, 305, 487, 'SuperB/SuperB/A3 305 x 487 mm');
  //
  SetPaperInfo(fpEnv_9, 76.2, 203.2, 'Envelope #9 76.2 x 203.2 mm');
  SetPaperInfo(fpEnv_10, 101.6, 228.6, 'Envelope #10 101.6 x 228.6 mm');
  SetPaperInfo(fpEnv_11, 101.6, 254, 'Envelope #11 101.6 x 254 mm');
  SetPaperInfo(fpEnv_12, 101.6, 279.4, 'Envelope #12 101.6 x 279.4 mm');
  SetPaperInfo(fpEnv_14, 127, 279.4, 'Envelope #14 127 x 279.4 mm');
  SetPaperInfo(fpEnv_B4, 250, 353, 'Envelope B4 250 x 353 mm');
  SetPaperInfo(fpEnv_B5, 176, 250, 'Envelope B5 176 x 250 mm');
  SetPaperInfo(fpEnv_B6, 176, 125, 'Envelope B6 176 x 125 mm');
  SetPaperInfo(fpEnv_C0, 917, 1297, 'Envelope C0 917 x 1297 mm');
  SetPaperInfo(fpEnv_C1, 648, 917, 'Envelope C1 648 x 917 mm');
  SetPaperInfo(fpEnv_C2, 458, 648, 'Envelope C2 458 x 648 mm');
  SetPaperInfo(fpEnv_C3, 324, 458, 'Envelope C3 324 x 458 mm');
  SetPaperInfo(fpEnv_C4, 229, 324, 'Envelope C4 229 x 324 mm');
  SetPaperInfo(fpEnv_C5, 162, 229, 'Envelope C5 162 x 229 mm');
  SetPaperInfo(fpEnv_C5E, 163, 229, 'Envelope C5E 163 x 229 mm');
  SetPaperInfo(fpEnv_C6, 114, 162, 'Envelope C6 114 x 162 mm');
  SetPaperInfo(fpEnv_C65, 114, 229, 'Envelope C65 114 x 229 mm');
  SetPaperInfo(fpEnv_C7, 81, 114, 'Envelope C7 81 x 114 mm');
  SetPaperInfo(fpEnv_C8, 57, 81, 'Envelope C8 57 x 81 mm');
  SetPaperInfo(fpEnv_C9, 40, 57, 'Envelope C9 40 x 57 mm');
  SetPaperInfo(fpEnv_C10, 28, 40, 'Envelope C10 28 x 40 mm');
  SetPaperInfo(fpEnv_DL, 110, 220, 'Envelope DL 110 x 220 mm');
  SetPaperInfo(fpEnv_Invite, 220, 220, 'Envelope Invite 220 x 220 mm');
  SetPaperInfo(fpEnv_Italy, 110, 230, 'Envelope 110 x 230 mm');
  SetPaperInfo(fpEnv_Monarch, 98.43, 190.5, 'Envelope Monarch 98.43 x 190.5 mm');
  SetPaperInfo(fpEnv_Personal, 76.2, 152.4, 'Envelope 76.2 x 152.4 mm');
  //
  SetPaperInfo(fpLetter, 215.9, 279.4, 'Letter 215.9 x 279.4 mm');
  SetPaperInfo(fpLegal, 215.9, 355.6, 'Legal 215.9 x 355.6 mm');
  SetPaperInfo(fpLedger, 431.8, 279, 'Ledger 431.8 x 279 mm');
  SetPaperInfo(fpTabloid, 279, 431.8, 'Tabloid 279 x 431.8 mm');
  SetPaperInfo(fpExecutive, 190.5, 254, 'Executive 190.5 x 254 mm');
  SetPaperInfo(fpFanfold_US, 355.6, 279.4, 'US Std Fanfold 355.6 x 279.4 mm');
  SetPaperInfo(fpComm10E, 105, 241, 'Comm10E 105 x 241 mm');
  SetPaperInfo(fpDLE, 110, 220, 'DLE 110 x 220 mm');
  SetPaperInfo(fpFolio, 215.9, 330.2, 'Folio 215.9 x 330.2 mm');
  SetPaperInfo(fpNote, 304.8, 279.4, 'Note 8 304.8 x 279.4 mm');
  SetPaperInfo(fpPostcard, 100, 148, 'Postcard 100 x 148 mm');
  SetPaperInfo(fpQuarto, 215, 275, 'Quarto 215 x 275 mm');
  SetPaperInfo(fpStatement, 139.7, 215.9, 'Statement 139.7 x 215.9 mm');
  //
  SetPaperInfo(fpLegal_Extra, 241.3, 381, 'Legal Extra 241.3 x 381 mm');
  SetPaperInfo(fpLegal3, 215.9, 330.2, 'Legal 3 215.9 x 330.2 mm');
  SetPaperInfo(fpLetter_Small, 304.8, 279.4, 'Letter Small 8 304.8 x 279.4 mm');
  SetPaperInfo(fpLetter_Extra, 241.3, 304.8, 'Letter Extra 241.3 x 304.8 mm');
  SetPaperInfo(fpLetter_Extra_Transverse, 241.3, 304.8, 'Letter Extra Transverse 241.3 x 304.8 mm');
  SetPaperInfo(fpLetter_Plus, 215.9, 322.33, 'Letter Plus 215.9 x 322.33 mm');
  SetPaperInfo(fpLetter_Transverse, 215.9, 279.4, 'Letter Transverse 215.9 x 279.4 mm');
  SetPaperInfo(fpP16K, 146, 215, 'PRC 16K 146 x 215 mm');
  SetPaperInfo(fpP32K, 97, 151, 'PRC 32K 97 x 151 mm');
  SetPaperInfo(fpP32KBig, 97, 151, 'PRC 32K(Big) 97 x 151 mm');
  SetPaperInfo(fpPEnv_1, 102, 165, 'PRC Envelope #1 102 x 165 mm');
  SetPaperInfo(fpPEnv_2, 102, 176, 'PRC Envelope #2 102 x 176 mm');
  SetPaperInfo(fpPEnv_3, 125, 176, 'PRC Envelope #3 125 x 176 mm');
  SetPaperInfo(fpPEnv_4, 110, 208, 'PRC Envelope #4 110 x 208 mm');
  SetPaperInfo(fpPEnv_5, 110, 220, 'PRC Envelope #5 110 x 220 mm');
  SetPaperInfo(fpPEnv_6, 120, 230, 'PRC Envelope #6 120 x 230 mm');
  SetPaperInfo(fpPEnv_7, 160, 230, 'PRC Envelope #7 160 x 230 mm');
  SetPaperInfo(fpPEnv_8, 120, 309, 'PRC Envelope #8 120 x 309 mm');
  SetPaperInfo(fpPEnv_9, 229, 324, 'PRC Envelope #9 229 x 324 mm');
  SetPaperInfo(fpPEnv_10, 324, 458, 'PRC Envelope #10 324 x 458 mm');
  SetPaperInfo(fpTabloid_Extra, 296.93, 457.2, 'Tabloid Extra 296.93 x 457.2 mm');
  //
  SetPaperInfo(fpGerman_Legal_Fanfold, 203.2, 330.2, 'German Legal Fanfold 203.2 x 330.2 mm');
  SetPaperInfo(fpGerman_Std_Fanfold, 203.2, 304.8, 'German Std Fanfold 203.2 x 304.8 mm');
  //
  SetPaperInfo(fpJap_DblPostcard, 200, 148, 'Japanese Double Postcard 200 x 148 mm');
  SetPaperInfo(fpJap_Postcard, 100, 148, 'Japanese Postcard 100 x 148 mm');
  //
  SetPaperInfo(fpEng_Foolscap, 419, 336, 'Foolscap 419 x 336 mm');
  SetPaperInfo(fpEng_DoubleFoolscap, 685.8, 431.8, 'Double Foolscap 685.8 x 431.8 mm');
  SetPaperInfo(fpEng_QuadFoolscap, 863.6, 685.8, 'Quad Foolscap 863.6 x 685.8 mm');
  SetPaperInfo(fpEng_Crown, 508, 381, 'Crown 508 x 381 mm');
  SetPaperInfo(fpEng_DoubleCrown, 762, 508, 'Double Crown 762 x 508 mm');
  SetPaperInfo(fpEng_QuadCrown, 1016, 762, 'Quad Crown 1016 x 762 mm');
  SetPaperInfo(fpEng_DoubleQuad, 1524, 1016, 'Double Quad Crown 1524 x 1016 mm');
  SetPaperInfo(fpEng_Post, 488.95, 393.7, 'Post 488.95 x 393.7 mm');
  SetPaperInfo(fpEng_DoublePost, 800.1, 495.3, 'Double Post 800.1 x 495.3 mm');
  SetPaperInfo(fpEng_DoubleLarge, 838.2, 533.4, 'Double Large Post 838.2 x 533.4 mm');
  SetPaperInfo(fpEng_Demy, 571.5, 444.5, 'Demy 571.5 x 17.5 mm');
  SetPaperInfo(fpEng_DoubleDemy, 889, 571.5, 'Double Demy 889 x 571.5 mm');
  SetPaperInfo(fpEng_QuadDemy, 1143, 889, 'Quad Demy 1143 x 889 mm');
  SetPaperInfo(fpEng_MusicDemy, 508, 393.7, 'Music Demy 508 x 393.7 mm');
  SetPaperInfo(fpEng_Medium, 584.2, 457.2, 'Medium 584.2 x 457.2 mm');
  SetPaperInfo(fpEng_Royal, 634, 507, 'Royal 634 x 507 mm');
  SetPaperInfo(fpEng_SuperRoyal, 698.5, 520.7, 'Super Royal 698.5 x 520.7 mm');
  SetPaperInfo(fpEng_Elephant, 711.2, 584.2, 'Elephant 711.2 x 584.2 mm');
  SetPaperInfo(fpEng_Imperial, 761, 559, 'Imperial 761 x 559 mm');
  //
  SetPaperInfo(fpCustom, 0, 0, 'User Defined');

  // Equivalências para Windows
{$IfDef MSWINDOWS}
  SetPaperEqv(fpA2, DMPAPER_A2);
  SetPaperEqv(fpA3, DMPAPER_A3);
  SetPaperEqv(fpA4, DMPAPER_A4);
  SetPaperEqv(fpA5, DMPAPER_A5);
  SetPaperEqv(fpA6, DMPAPER_A6);
  SetPaperEqv(fpB4, DMPAPER_B4);
  SetPaperEqv(fpB5, DMPAPER_B5);
  SetPaperEqv(fpB6, DMPAPER_B6_JIS);
  SetPaperEqv(fp9x11, DMPAPER_9X11);
  SetPaperEqv(fp10x11, DMPAPER_10X11);
  SetPaperEqv(fp10x14, DMPAPER_10X14);
  SetPaperEqv(fp11x17, DMPAPER_11X17);
  SetPaperEqv(fp12x11, DMPAPER_12X11);
  SetPaperEqv(fp15x11, DMPAPER_15X11);
  SetPaperEqv(fpA3_Extra, DMPAPER_A3_EXTRA);
  SetPaperEqv(fpA3_Extra_Transverse, DMPAPER_A3_EXTRA_TRANSVERSE);
  SetPaperEqv(fpA3_Transverse, DMPAPER_A3_TRANSVERSE);
  SetPaperEqv(fpA4_Small, DMPAPER_A4SMALL);
  SetPaperEqv(fpA4_Extra, DMPAPER_A4_EXTRA);
  SetPaperEqv(fpA4_Plus, DMPAPER_A4_PLUS);
  SetPaperEqv(fpA4_Transverse, DMPAPER_A4_TRANSVERSE);
  SetPaperEqv(fpA5_Extra, DMPAPER_A5_EXTRA);
  SetPaperEqv(fpA5_Transverse, DMPAPER_A5_TRANSVERSE);
  SetPaperEqv(fpA_Plus, DMPAPER_A_PLUS);
  SetPaperEqv(fpB5_Extra, DMPAPER_B5_EXTRA);
  SetPaperEqv(fpB5_Transverse, DMPAPER_B5_TRANSVERSE);
  SetPaperEqv(fpB6_JIS, DMPAPER_B6_JIS);
  SetPaperEqv(fpB_Plus, DMPAPER_B_PLUS);
  SetPaperEqv(fpEnv_9, DMPAPER_ENV_9);
  SetPaperEqv(fpEnv_10, DMPAPER_ENV_10);
  SetPaperEqv(fpEnv_11, DMPAPER_ENV_11);
  SetPaperEqv(fpEnv_12, DMPAPER_ENV_12);
  SetPaperEqv(fpEnv_14, DMPAPER_ENV_14);
  SetPaperEqv(fpEnv_B4, DMPAPER_ENV_B4);
  SetPaperEqv(fpEnv_B5, DMPAPER_ENV_B5);
  SetPaperEqv(fpEnv_B6, DMPAPER_ENV_B6);
  SetPaperEqv(fpEnv_C3, DMPAPER_ENV_C3);
  SetPaperEqv(fpEnv_C4, DMPAPER_ENV_C4);
  SetPaperEqv(fpEnv_C5, DMPAPER_ENV_C5);
  SetPaperEqv(fpEnv_C6, DMPAPER_ENV_C6);
  SetPaperEqv(fpEnv_C65, DMPAPER_ENV_C65);
  SetPaperEqv(fpEnv_DL, DMPAPER_ENV_DL);
  SetPaperEqv(fpEnv_Invite, DMPAPER_ENV_INVITE);
  SetPaperEqv(fpEnv_Italy, DMPAPER_ENV_ITALY);
  SetPaperEqv(fpEnv_Monarch, DMPAPER_ENV_MONARCH);
  SetPaperEqv(fpEnv_Personal, DMPAPER_ENV_PERSONAL);
  SetPaperEqv(fpLetter, DMPAPER_LETTER);
  SetPaperEqv(fpLegal, DMPAPER_LEGAL);
  SetPaperEqv(fpLedger, DMPAPER_LEDGER);
  SetPaperEqv(fpTabloid, DMPAPER_TABLOID);
  SetPaperEqv(fpExecutive, DMPAPER_EXECUTIVE);
  SetPaperEqv(fpFanfold_US, DMPAPER_FANFOLD_US);
  SetPaperEqv(fpFolio, DMPAPER_FOLIO);
  SetPaperEqv(fpNote, DMPAPER_NOTE);
  SetPaperEqv(fpQuarto, DMPAPER_QUARTO);
  SetPaperEqv(fpStatement, DMPAPER_STATEMENT);
  SetPaperEqv(fpLegal_Extra, DMPAPER_LEGAL_EXTRA);
  SetPaperEqv(fpLetter_Small, DMPAPER_LETTERSMALL);
  SetPaperEqv(fpLetter_Extra, DMPAPER_LETTER_EXTRA);
  SetPaperEqv(fpLetter_Extra_Transverse, DMPAPER_LETTER_EXTRA_TRANSVERSE);
  SetPaperEqv(fpLetter_Plus, DMPAPER_LETTER_PLUS);
  SetPaperEqv(fpLetter_Transverse, DMPAPER_LETTER_TRANSVERSE);
  SetPaperEqv(fpP16K, DMPAPER_P16K);
  SetPaperEqv(fpP32K, DMPAPER_P32K);
  SetPaperEqv(fpP32KBig, DMPAPER_P32KBIG);
  SetPaperEqv(fpPEnv_1, DMPAPER_PENV_1);
  SetPaperEqv(fpPEnv_2, DMPAPER_PENV_2);
  SetPaperEqv(fpPEnv_3, DMPAPER_PENV_3);
  SetPaperEqv(fpPEnv_4, DMPAPER_PENV_4);
  SetPaperEqv(fpPEnv_5, DMPAPER_PENV_5);
  SetPaperEqv(fpPEnv_6, DMPAPER_PENV_6);
  SetPaperEqv(fpPEnv_7, DMPAPER_PENV_7);
  SetPaperEqv(fpPEnv_8, DMPAPER_PENV_8);
  SetPaperEqv(fpPEnv_9, DMPAPER_PENV_9);
  SetPaperEqv(fpPEnv_10, DMPAPER_PENV_10);
  SetPaperEqv(fpTabloid_Extra, DMPAPER_TABLOID_EXTRA);
  SetPaperEqv(fpGerman_Legal_Fanfold, DMPAPER_FANFOLD_LGL_GERMAN);
  SetPaperEqv(fpGerman_Std_Fanfold, DMPAPER_FANFOLD_STD_GERMAN);
  SetPaperEqv(fpJap_DblPostcard, DMPAPER_DBL_JAPANESE_POSTCARD);
  SetPaperEqv(fpJap_Postcard, DMPAPER_JAPANESE_POSTCARD);
  SetPaperEqv(fpCustom, DMPAPER_USER);
{$endif}

  // Equivalências para Linux/CLX
{$IfDef CLX}
  SetPaperEqv(fpA0, psA0);
  SetPaperEqv(fpA1, psA1);
  SetPaperEqv(fpA2, psA2);
  SetPaperEqv(fpA3, psA3);
  SetPaperEqv(fpA4, psA4);
  SetPaperEqv(fpA5, psA5);
  SetPaperEqv(fpA6, psA6);
  SetPaperEqv(fpA7, psA7);
  SetPaperEqv(fpA8, psA8);
  SetPaperEqv(fpA9, psA9);
  SetPaperEqv(fpB0, psB0);
  SetPaperEqv(fpB1, psB1);
  SetPaperEqv(fpB2, psB2);
  SetPaperEqv(fpB3, psB3);
  SetPaperEqv(fpB4, psB4);
  SetPaperEqv(fpB5, psB5);
  SetPaperEqv(fpB6, psB6);
  SetPaperEqv(fpB7, psB7);
  SetPaperEqv(fpB8, psB8);
  SetPaperEqv(fpB9, psB9);
  SetPaperEqv(fpB10, psB10);
  SetPaperEqv(fpLetter, psLetter);
  SetPaperEqv(fpLegal, psLegal);
  SetPaperEqv(fpExecutive, psExecutive);
  SetPaperEqv(fpEnv_C5E, psC5E);
  SetPaperEqv(fpComm10E, psComm10E);
  SetPaperEqv(fpDLE, psDLE);
  SetPaperEqv(fpFolio, psFolio);
  SetPaperEqv(fpLedger, psLedger);
  SetPaperEqv(fpTabloid, psTabloid);
  SetPaperEqv(fpCustom, psNPageSize);
{$endif}

end.
