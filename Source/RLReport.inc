// eh .net
{$ifdef CLR} {$define DOTNET} {$endif}

// eh kylix
{$ifdef KYLIX1} {$define KYLIX} {$define KYLIX1_UP} {$define KYLIX1_DOWN} {$endif}
{$ifdef KYLIX2} {$define KYLIX} {$define KYLIX2_UP} {$define KYLIX2_DOWN} {$endif}
{$ifdef KYLIX3} {$define KYLIX} {$define KYLIX3_UP} {$define KYLIX3_DOWN} {$endif}

// eh cpp builder
{$ifdef CBUILDER1} {$define CBUILDER} {$endif}
{$ifdef CBUILDER3} {$define CBUILDER} {$endif}
{$ifdef CBUILDER4} {$define CBUILDER} {$endif}
{$ifdef CBUILDER5} {$define CBUILDER} {$endif}
{$ifdef CBUILDER} {$define CPP} {$endif}

// se eh Linux eh Kylix
{$ifdef LINUX} {$define KYLIX} {$endif}

// se nao eh Kylix, entao eh Delphi e Windows
{$ifndef KYLIX} {$define DELPHI} {$define WINDOWS} {$endif}

// se eh Kylix eh CLX
{$ifdef KYLIX} {$define CLX} {$endif}

// UPs compreendem versoes superiores (ex.: DELPHI6_UP = a partir do DELPHI6, inclusive)
// DOWNs compreendem versoes inferiores (DELPHI2007_DOWN = ate o DELPHI2007, inclusive)
{$ifdef VER80} {$define DELPHI1} {$endif}
{$ifdef VER90} {$define DELPHI2} {$endif}
{$ifdef VER93} {$define CBUILDER1} {$endif}
{$ifdef VER100} {$define DELPHI3} {$endif}
{$ifdef VER110} {$define CBUILDER3} {$endif}
{$ifdef VER120} {$define DELPHI4} {$endif}
{$ifdef VER125} {$define CBUILDER4} {$endif}
{$ifdef VER130} {$define DELPHI5} {$endif}
{$ifdef VER135} {$define CBUILDER5} {$endif}
{$ifdef VER140} {$define DELPHI6} {$endif}
{$ifdef VER150} {$define DELPHI7} {$endif}
{$ifdef VER160} {$define DELPHI8} {$endif}
{$ifdef VER170} {$define DELPHI2005} {$endif}
// confusao da borland: versoes 180, 185 e 190 representam o Delphi2007
// versoes 180 e 185 tambem sao Delphi6e 190 representam o Delphi2007
{$ifdef VER180} {$define DELPHI2006} {$endif}
{$ifdef VER180} {$define DELPHI2007} {$endif}
{$ifdef VER185} {$undef DELPHI2006} {$define DELPHI2007} {$endif}
{$ifdef VER190} {$define DELPHI2007} {$define DELPHI2007_DOTNET} {$define DOTNET} {$endif}
{$ifdef VER200} {$define DELPHI2009} {$endif}
{$ifdef VER210} {$define DELPHI2010} {$endif}
{$ifdef VER220} {$define DELPHIXE} {$endif}
{$ifdef VER230} {$define DELPHIXE2} {$endif}
{$ifdef VER240} {$define DELPHIXE3} {$endif}
{$ifdef VER250} {$define DELPHIXE4} {$endif}
{$ifdef VER260} {$define DELPHIXE5} {$endif}
{$ifdef VER270} {$define DELPHIXE6} {$endif}
{$ifdef VER280} {$define DELPHIXE7} {$endif}

// UPs e DOWNs

{$ifdef CBUILDER1} {$define CBUILDER} {$define CBUILDER1_UP} {$define CBUILDER1_DOWN} {$define DELPHI2_UP} {$define DELPHI2_DOWN} {$endif}
{$ifdef CBUILDER3} {$define CBUILDER} {$define CBUILDER3_UP} {$define CBUILDER3_DOWN} {$define DELPHI3_UP} {$define DELPHI3_DOWN} {$endif}
{$ifdef CBUILDER4} {$define CBUILDER} {$define DELPHI4_UP} {$define DELPHI4_DOWN} {$endif}
{$ifdef CBUILDER5} {$define CBUILDER} {$define DELPHI5_UP} {$define DELPHI5_DOWN} {$endif}
{$ifdef DELPHI1} {$define DELPHI1_UP} {$define DELPHI1_DOWN} {$endif}
{$ifdef DELPHI2} {$define DELPHI2_UP} {$define DELPHI2_DOWN} {$endif}
{$ifdef DELPHI3} {$define DELPHI3_UP} {$define DELPHI3_DOWN} {$endif}
{$ifdef DELPHI4} {$define DELPHI4_UP} {$define DELPHI4_DOWN} {$endif}
{$ifdef DELPHI5} {$define DELPHI5_UP} {$define DELPHI5_DOWN} {$endif}
{$ifdef DELPHI6} {$define DELPHI6_UP} {$define DELPHI6_DOWN} {$endif}
{$ifdef DELPHI7} {$define DELPHI7_UP} {$define DELPHI7_DOWN} {$endif}
{$ifdef DELPHI8} {$define DELPHI8_UP} {$define DELPHI8_DOWN} {$endif}
{$ifdef DELPHI2005} {$define DELPHI2005_UP} {$define DELPHI2005_DOWN} {$endif}
{$ifdef DELPHI2006} {$define DELPHI2006_UP} {$define DELPHI2006_DOWN} {$endif}
{$ifdef DELPHI2007} {$define DELPHI2007_UP} {$define DELPHI2007_DOWN} {$endif}
{$ifdef DELPHI2009} {$define DELPHI2009_UP} {$define DELPHI2009_DOWN} {$endif}
{$ifdef DELPHI2010} {$define DELPHI2010_UP} {$define DELPHI2010_DOWN} {$endif}
{$ifdef DELPHIXE} {$define DELPHIXE_UP} {$define DELPHIXE_DOWN} {$endif}
{$ifdef DELPHIXE2} {$define DELPHIXE2_UP} {$define DELPHIXE2_DOWN} {$endif}
{$ifdef DELPHIXE3} {$define DELPHIXE3_UP} {$define DELPHIXE3_DOWN} {$endif}
{$ifdef DELPHIXE4} {$define DELPHIXE4_UP} {$define DELPHIXE4_DOWN} {$endif}
{$ifdef DELPHIXE5} {$define DELPHIXE5_UP} {$define DELPHIXE5_DOWN} {$endif}
{$ifdef DELPHIXE6} {$define DELPHIXE6_UP} {$define DELPHIXE6_DOWN} {$endif}
{$ifdef DELPHIXE7} {$define DELPHIXE7_UP} {$define DELPHIXE7_DOWN} {$endif}


// UP

{$ifdef KYLIX3_UP} {$define KYLIX2_UP} {$endif}
{$ifdef KYLIX2_UP} {$define KYLIX1_UP} {$endif}
{$ifdef CBUILDER5_UP} {$define CBUILDER4_UP} {$endif}
{$ifdef CBUILDER4_UP} {$define CBUILDER3_UP} {$endif}
{$ifdef CBUILDER3_UP} {$define CBUILDER1_UP} {$endif}

{$ifdef DELPHIXE7_UP} {$define DELPHIXE6_UP} {$endif}
{$ifdef DELPHIXE6_UP} {$define DELPHIXE5_UP} {$endif}
{$ifdef DELPHIXE5_UP} {$define DELPHIXE4_UP} {$endif}
{$ifdef DELPHIXE4_UP} {$define DELPHIXE3_UP} {$endif}
{$ifdef DELPHIXE3_UP} {$define DELPHIXE2_UP} {$endif}
{$ifdef DELPHIXE2_UP} {$define DELPHIXE_UP} {$endif}
{$ifdef DELPHIXE_UP} {$define DELPHI2010_UP} {$endif}
{$ifdef DELPHI2010_UP} {$define DELPHI2009_UP} {$endif}
{$ifdef DELPHI2009_UP} {$define DELPHI2007_UP} {$endif}
{$ifdef DELPHI2007_UP} {$define DELPHI2006_UP} {$endif}
{$ifdef DELPHI2006_UP} {$define DELPHI2005_UP} {$endif}
{$ifdef DELPHI2005_UP} {$define DELPHI8_UP} {$endif}
{$ifdef DELPHI8_UP} {$define DELPHI7_UP} {$endif}
{$ifdef DELPHI7_UP} {$define DELPHI6_UP} {$endif}
{$ifdef DELPHI6_UP} {$define DELPHI5_UP} {$endif}
{$ifdef DELPHI5_UP} {$define DELPHI4_UP} {$endif}
{$ifdef DELPHI4_UP} {$define DELPHI3_UP} {$endif}
{$ifdef DELPHI3_UP} {$define DELPHI2_UP} {$endif}
{$ifdef DELPHI2_UP} {$define DELPHI1_UP} {$endif}

// DOWN

{$ifdef KYLIX1_DOWN} {$define KYLIX2_DOWN} {$endif}
{$ifdef KYLIX2_DOWN} {$define KYLIX3_DOWN} {$endif}
{$ifdef CBUILDER1_DOWN} {$define CBUILDER3_DOWN} {$endif}
{$ifdef CBUILDER3_DOWN} {$define CBUILDER4_DOWN} {$endif}
{$ifdef CBUILDER4_DOWN} {$define CBUILDER5_DOWN} {$endif}

{$ifdef DELPHI1_DOWN} {$define DELPHI2_DOWN} {$endif}
{$ifdef DELPHI2_DOWN} {$define DELPHI3_DOWN} {$endif}
{$ifdef DELPHI3_DOWN} {$define DELPHI4_DOWN} {$endif}
{$ifdef DELPHI4_DOWN} {$define DELPHI5_DOWN} {$endif}
{$ifdef DELPHI5_DOWN} {$define DELPHI6_DOWN} {$endif}
{$ifdef DELPHI6_DOWN} {$define DELPHI7_DOWN} {$endif}
{$ifdef DELPHI7_DOWN} {$define DELPHI8_DOWN} {$endif}
{$ifdef DELPHI2005_DOWN} {$define DELPHI2006_DOWN} {$endif}
{$ifdef DELPHI2006_DOWN} {$define DELPHI2007_DOWN} {$endif}
{$ifdef DELPHI2007_DOWN} {$define DELPHI2008_DOWN} {$endif}
{$ifdef DELPHI2008_DOWN} {$define DELPHI2009_DOWN} {$endif}
{$ifdef DELPHI2009_DOWN} {$define DELPHI2010_DOWN} {$endif}
{$ifdef DELPHI2010_DOWN} {$define DELPHIXE_DOWN} {$endif}
{$ifdef DELPHIXE_DOWN} {$define DELPHIXE2_DOWN} {$endif}
{$ifdef DELPHIXE2_DOWN} {$define DELPHIXE3_DOWN} {$endif}
{$ifdef DELPHIXE3_DOWN} {$define DELPHIXE4_DOWN} {$endif}
{$ifdef DELPHIXE4_DOWN} {$define DELPHIXE5_DOWN} {$endif}
{$ifdef DELPHIXE5_DOWN} {$define DELPHIXE6_DOWN} {$endif}

// ate o Delphi5 so existia VCL, depois disso o proprio projeto eh q tem q se pronunciar
{$ifdef DELPHI5_DOWN} {$define VCL} {$endif}

// Simbolos especiais
{$ifdef DELPHI5_UP} {$define USE_VARIANTS} {$endif}

