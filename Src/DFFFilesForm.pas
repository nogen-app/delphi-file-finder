unit DFFFilesForm;

interface

uses
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  ToolsAPI,
  DockableForm, FilesFrame;

type
  TfrmDFFFiles = class(TfrmDockableForm, INTACustomDockableForm)
  private
    FFrame: TfrmFilesFrame;
  public //INTACustomDockableForm
    function GetCaption: string; override;
    function GetIdentifier: string; override;
    function GetFrameClass: TCustomFrameClass; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;

    property Frame: TfrmFilesFrame read FFrame write FFrame;

  end;

implementation

{$R *.dfm}

{ TfrmDFFFiles }

procedure TfrmDFFFiles.FrameCreated(AFrame: TCustomFrame);
begin
  inherited;
  FFrame := aFrame as TfrmFilesFrame;
  AFrame.Align := alClient;
  InsertControl(AFrame);
end;

function TfrmDFFFiles.GetCaption: string;
begin
  Result := 'Files';
end;

function TfrmDFFFiles.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmFilesFrame;
end;

function TfrmDFFFiles.GetIdentifier: string;
begin
  Result := 'nogen_files_finder';
end;


end.
