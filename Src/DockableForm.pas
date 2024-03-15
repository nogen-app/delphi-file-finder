unit DockableForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ActnList, vcl.ImgList, vcl.Menus, vcl.ComCtrls, System.IniFiles,
  DesignIntf, ToolsAPI;

type
  TfrmDockableForm = class(TForm, INTACustomDockableForm)
  private
    { Private declarations }
  public //Interface declaration INTACustomDockableForm
    function GetCaption: string; virtual;
    function GetIdentifier: string; virtual;
    function GetFrameClass: TCustomFrameClass; virtual;
    procedure FrameCreated(AFrame: TCustomFrame); virtual;
    function GetMenuActionList: TCustomActionList; virtual;
    function GetMenuImageList: TCustomImageList; virtual;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu); virtual;
    function GetToolBarActionList: TCustomActionList; virtual;
    function GetToolBarImageList: TCustomImageList; virtual;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean); virtual;
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string); virtual;
    function GetEditState: TEditState; virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;

    property Caption: string read GetCaption;
    property Identifier: string read Getidentifier;
    property FrameClass: TCustomFrameClass read GetFrameClass;
    property MenuActionList: TCustomActionList read GetMenuActionList;
    property MenuImageList: TCustomImageList read GetMenuImageList;
    property ToolbarActionList: TCustomActionList read GetToolbarActionList;
    property ToolbarImageList: TCustomImageList read GetToolbarImageList;
  end;

implementation

{$R *.dfm}

{ TfrmDockableForm }

procedure TfrmDockableForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin

end;

procedure TfrmDockableForm.CustomizeToolBar(ToolBar: TToolBar);
begin

end;

function TfrmDockableForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := false;
end;

procedure TfrmDockableForm.FrameCreated(AFrame: TCustomFrame);
begin

end;

function TfrmDockableForm.GetCaption: string;
begin
  Result := 'nogen baseform';
end;

function TfrmDockableForm.GetEditState: TEditState;
begin

end;

function TfrmDockableForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TCustomFrame;
end;

function TfrmDockableForm.GetIdentifier: string;
begin
  Result := 'nogen_baseform';
end;

function TfrmDockableForm.GetMenuActionList: TCustomActionList;
begin
  Result := TCustomActionList.Create(Self);
end;

function TfrmDockableForm.GetMenuImageList: TCustomImageList;
begin
  Result := TCustomImageList.Create(Self);
end;

function TfrmDockableForm.GetToolBarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TfrmDockableForm.GetToolBarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TfrmDockableForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin

end;

procedure TfrmDockableForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin

end;

end.
