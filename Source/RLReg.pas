unit RLReg;

{$ifdef FPC}
  {$MODE DELPHI}
{$endif}

interface

uses
  Classes,
{$ifdef FPC}
  PropEdits, ComponentEditors, LCLType, LResources,
{$else}
 {$ifdef DELPHI5}
   DsgnIntF,
 {$else}
   DesignIntF,
 {$endif}
{$endif}
  RLDesign, RLReport,
   RLDraftFilter, RLPDFFilter, RLHTMLFilter, RLRichFilter, RLXLSFilter,
  RLParser, RLPreview, RLMetaFile, RLBarcode, RLRichText, RLPreviewForm;

procedure Register;

implementation

{$R 'RLReport.dcr'}

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
                                      TRLHTMLFilter,
                                      TRLRichFilter,
                                      TRLPDFFilter,
                                      TRLXLSFilter,
                                      TRLPreviewSetup]);
  // editores de componentes
  RegisterComponentEditor(TRLReport, TRLReportDesigner);
  // editores de propriedades
  RegisterPropertyEditor(TypeInfo(TRLDataFieldProperty), nil, 'DataField', TRLDataFieldEditor);
  RegisterPropertyEditor(TypeInfo(TRLDataFieldsProperty), TRLCustomGroup, 'DataFields', TRLDataFieldsEditor);
end;

initialization
{$ifdef FPC}
  {$I Fortes4.lrs}
{$endif}

end.

