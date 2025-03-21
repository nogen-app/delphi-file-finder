library DFF_Athens;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions thatt pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters.

  Important note about VCL usage: when this DLL will be implicitly
  loaded and this DLL uses TWicImage / TImageCollection created in
  any unit initialization section, then Vcl.WicImageInit must be
  included into your library's USES clause. }











{$R *.dres}

uses
  System.SysUtils,
  System.Classes,
  DLLEntry in 'Src\DLLEntry.pas',
  DFFWizard in 'Src\DFFWizard.pas',
  DFFFilesForm in 'Src\DFFFilesForm.pas' {frmDFFFiles},
  DockableForm in 'Src\DockableForm.pas' {frmDockableForm},
  FilesFrame in 'Src\FilesFrame.pas' {frmFilesFrame: TFrame},
  ServiceRegistration in 'Src\ServiceRegistration.pas',
  FileIndexThread in 'Src\FileIndexThread.pas',
  Services.IRepo in 'Src\Services.IRepo.pas',
  Services.Repo in 'Src\Services.Repo.pas',
  DFFWizard.Welcome in 'Src\DFFWizard.Welcome.pas';

{$R *.res}

begin
end.

