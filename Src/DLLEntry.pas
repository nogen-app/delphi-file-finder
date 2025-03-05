unit DLLEntry;

interface

uses
  SysUtils, ToolsAPI, DFFWizard;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
exports
  InitWizard name WizardEntryPoint;

implementation

const
  INVALID_INDEX = -1;

var
  WizardIndex: Integer = INVALID_INDEX;

procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if WizardIndex <> INVALID_INDEX then
  begin
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    WizardServices.RemoveWizard(WizardIndex);
    WizardIndex := INVALID_INDEX;
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

    WizardIndex := WizardServices.AddWizard(DFFWiz as IOTAWizard);
    Result := (WizardIndex >= 0);
  end;
end;

end.
