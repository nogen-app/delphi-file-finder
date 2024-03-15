unit DLLEntry;

interface

uses
  SysUtils, ToolsAPI, DFFWizard, System.Classes, Winapi.Windows, vcl.Graphics;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;


exports
  InitWizard name WizardEntryPoint;

implementation

const
  InvalidIndex = -1;


var
  FWizardIndex: Integer = InvalidIndex;

procedure AddSplashScreenInfo;
begin
  if Assigned(SplashScreenServices) then
  begin
    var lIcon := LoadBitmap(HInstance, 'nogen_SplashScreen_icon');

    SplashScreenServices.AddProductBitmap('nogen V1.0', lIcon);
  end;
end;

procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if FWizardIndex <> InvalidIndex then
  begin
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    WizardServices.RemoveWizard(FWizardIndex);
    FWizardIndex := InvalidIndex;
  end;
end;

procedure Registerkeybinds(const BorlandIDEServices: IBorlandIDEServices; aDFFWiz: TDFFWizard);
begin
  var keyboardBinding: IOTAKeyboardServices;
  if Supports(BorlandIDEServices, IOTAKeyboardServices, keyboardBinding) then
  begin
    keyboardBinding.AddKeyboardBinding(aDFFWiz);
  end;
end;


function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  Result := BorlandIDEServices <> nil;
  if Result then
  begin
    Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);
    Terminate := FinalizeWizard;
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    DFFWiz := TDFFWizard.Create;

    RegisterKeyBinds(BorlandIDEServices, DFFWiz);

    FWizardIndex := WizardServices.AddWizard(DFFWiz as IOTAWizard);
    Result := (FWizardIndex >= 0);
  end;
end;

initialization
  AddSplashScreenInfo;

end.
