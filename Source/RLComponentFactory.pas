unit RLComponentFactory;

interface

uses
  Classes;

type
  TCreateRLComponentProc = procedure(ComponentClass: TComponentClass; AOwner: TComponent; var AComponent);

  TRLComponentFactory = class
  public
    class procedure CreateComponent(ComponentClass: TComponentClass; AOwner: TComponent; var AComponent);
  end;

var
  CreateRLComponentProc: TCreateRLComponentProc = nil;

implementation

uses
  StdCtrls;

class procedure TRLComponentFactory.CreateComponent(ComponentClass: TComponentClass; AOwner: TComponent; var AComponent);
begin
  TComponent(AComponent) := nil;

  if Assigned(CreateRLComponentProc) then
    CreateRLComponentProc(ComponentClass, AOwner, AComponent);

  if not Assigned(TComponent(AComponent)) then
    TComponent(AComponent) := ComponentClass.Create(AOwner);
end;

end.
