{******************************************************************************}
{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios são constituídos por bandas que  }
{ têm funções específicas no fluxo de impressão. Você definir agrupamentos     }
{ subníveis e totais simplesmente pela relação hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Márcio Martins                                  }
{                              Ronaldo Moreira                                 }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto         }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
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

//Atualmente homologado somente para VCL, porém há uma trabalho sendo feito para
//que o mesmo código seja usado no Delphi ou no Lazarus
{.$DEFINE VCL}
{.$DEFINE FMK}
{.$DEFINE KYLIX}
{.$DEFINE FREEPASCAL}

// Para Remover Warnings de CAST causados por WideString/String/AnsiString, descomente a linha abaixo. (Copiado do Projeto ACBr)
{.$DEFINE REMOVE_CAST_WARN}

{$IFDEF FPC}
  {$DEFINE VCL}
  {$DEFINE UNICODE} // Lazarus 0.9.26 usa UnicodeString como default para String
  {$DEFINE DELPHI7}
  {$DEFINE COMPILER7}
  {$DEFINE HAS_FORMATSETTINGS}
  {$H+}
  {$UNDEF REMOVE_CAST_WARN} //Devido a problemas com a mensagem de erro "Message recordind not yet supported"
{$ELSE}
  {$DEFINE BORLAND}
{$ENDIF}

// biblioteca do Jedi assim temos atualizado sempre
{$I RLReport_jedi.inc}

//Para compilação em IDE C++Builder (sem Delphi personality)
{$IFDEF BCB}
  {$I RLReport_BCB.inc}
{$ENDIF}

{$IFDEF DELPHI15_UP}
  {$DEFINE HAS_FORMATSETTINGS}
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
  {$IFDEF BORLAND}
    {$UNDEF REMOVE_CAST_WARN}
  {$ENDIF}
{$ENDIF}

{$IFDEF REMOVE_CAST_WARN}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {.$WARN SYMBOL_DEPRECATED OFF}
  {$IFDEF BORLAND}
    {$WARN DUPLICATE_CTOR_DTOR OFF}
    {$WARN CVT_ACHAR_TO_WCHAR OFF}
  {$ENDIF}
{$ENDIF}

// Remova o Ponto do DEFINE abaixo, se seu Delphi suporta PNG.
//    - Se não suportar (D7),
//  	- Acesse: https://sourceforge.net/projects/pngdelphi2/files/
//      - Instale o projeto e depois remova o Ponto da Linha abaixo
{.$DEFINE SUPPORT_PNG}

{$IFDEF DELPHI2009_UP}
  {$DEFINE SUPPORT_PNG}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE SUPPORT_PNG}
{$ENDIF}



