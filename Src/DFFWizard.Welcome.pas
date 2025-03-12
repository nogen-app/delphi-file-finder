unit DFFWizard.Welcome;

interface

implementation

uses
  ToolsAPI, Windows, Graphics, SysUtils;

const
  ri_logo = 'NOGEN_LOGO';

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

resourcestring
  resPackageName = 'nogen DFF';
  resLicense = 'MIT License';
  resAboutCopyright = 'nogen I/S';
  resAboutTitle = 'File finder';
  resAboutDescription = 'An IDE plugin for Embarcadero delphi used to fuzzy search files available in the active project';

procedure RegisterSplashScreen;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(HInstance, ri_logo);
    SplashScreenServices.AddPluginBitmap(resPackageName, bmp.Handle, False, resLicense);
  finally
    bmp.Free;
  end;
end;

procedure RegisterAboutBox;
begin
  if Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices) then
  begin
    var lProductImage := LoadBitmap(FindResourceHInstance(HInstance), ri_logo);
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(resPackageName,
                                                resAboutCopyright + sLineBreak +
                                                resAboutDescription, lProductImage, False, resLicense);
  end;
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

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;
end.
