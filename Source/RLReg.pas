{ Projeto: FortesReport Community Edition                                      }
{ � um poderoso gerador de relat�rios dispon�vel como um pacote de componentes }
{ para Delphi. Em FortesReport, os relat�rios s�o constitu�dos por bandas que  }
{ t�m fun��es espec�ficas no fluxo de impress�o. Voc� definir agrupamentos     }
{ subn�veis e totais simplesmente pela rela��o hier�rquica entre as bandas.    }
{ Al�m disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright � 1999-2015 Fortes Inform�tica     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              M�rcio Martins                                  }
{                              R�gys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informa��es voc� pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descri��o...
******************************************************************************}

{$I RLReport.inc}

unit RLReg;

interface

uses
  Classes, SysUtils, RLDesign,
{$IFDEF DELPHI5}
  DsgnIntF, 
{$ELSE}
  DesignIntF, 
{$ENDIF}
{$IFDEF DELPHI2007_UP}ToolsApi, Windows, Graphics,{$ENDIF}
  RLReport, RLDraftFilter, RLRichFilter, RLHTMLFilter, RLPDFFilter, RLParser,
  RLPreview, RLMetaFile, RLBarcode, RLRichText, RLPreviewForm, RLXLSFilter, RLXLSXFilter,
  RLConsts;

procedure Register;

implementation

{$R 'RLReport.dcr'}

{$IFDEF DELPHI2007_UP}
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  Assert(Assigned(AboutBoxServices), '');
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'FRCE');
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(cRLSobreTitulo , cRLSobreDescricao,
    ProductImage, False, cRLSobreLicencaStatus);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure AddSplash;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromResourceName(HInstance, 'FRCE');
  SplashScreenServices.AddPluginBitmap(cRLSobreDialogoTitulo,bmp.Handle,false,cRLSobreLicencaStatus,'');
  bmp.Free;
end;
{$ENDIF}

procedure Register;
begin
  // componentes
  RegisterComponents('Fortes Report', [TRLReport, 
                                      TRLBand, 
                                      TRLDetailGrid, 
                                      TRLGroup, 
                                      TRLSubDetail, 
                                      TRLLabel, 
                                      TRLAngleLabel, 
                                      TRLDBText, 
                                      TRLMemo, 
                                      TRLDBMemo, 
                                      TRLRichText, 
                                      TRLDBRichText, 
                                      TRLImage, 
                                      TRLDBImage, 
                                      TRLSystemInfo, 
                                      TRLDraw, 
                                      TRLPanel, 
                                      TRLDBResult, 
                                      TRLBarcode, 
                                      TRLDBBarcode, 
                                      TRLPreview, 
                                      TRLExpressionParser, 
                                      TRLDraftFilter, 
                                      TRLRichFilter, 
                                      TRLHTMLFilter, 
                                      TRLPDFFilter, 
                                      TRLXLSFilter, 
                                      TRLXLSXFilter, 
                                      TRLPreviewSetup]);
  // editores de componentes
  RegisterComponentEditor(TRLReport, TRLReportDesigner);
  // editores de propriedades
  RegisterPropertyEditor(TypeInfo(TRLDataFieldProperty), nil, 'DataField', TRLDataFieldEditor);
  RegisterPropertyEditor(TypeInfo(TRLDataFieldsProperty), TRLCustomGroup, 'DataFields', TRLDataFieldsEditor);
end;

{$IFDEF DELPHI2007_UP}
initialization
	AddSplash;
	RegisterAboutBox;

finalization
	UnregisterAboutBox;
{$ENDIF}

end.
