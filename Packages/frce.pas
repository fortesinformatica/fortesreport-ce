{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit frce;

interface

uses
  RLPrinters, RLReport, RLTypes, RLUtils, RLBarcode, RLComponentFactory, 
  RLConsts, RLFilters, RLMetaFile, RLMetaVCL, RLParser, RLPreview, RLRichText, 
  RLReg, RLSpoolFilter, RLAbout, RLFeedBack, RLFindDialog, RLPreviewForm, 
  RLPrintDialog, RLSaveDialog, RLRichFilter, RLPDFFilter, RLXLSFilter, 
  RLHTMLFilter, RLDraftFilter, RLDesign, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RLReg', @RLReg.Register);
end;

initialization
  RegisterPackage('frce', @Register);
end.
