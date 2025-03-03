unit ServiceRegistration;

interface

uses
  Spring, Spring.Container,
  FileIndexThread,

  Services.IRepo, Services.Repo,
  FilesFrame;


function RegisterServices: TContainer;

implementation

function RegisterServices: TContainer;
begin
    Result := GlobalContainer;
    Result.RegisterInstance<TContainer>(Result);

    Result.RegisterType<TFileIndexThread>.AsSingleton;
    Result.RegisterType<TfrmFilesFrame>.AsSingleton;

    Result.RegisterType<IRepo, TRepo>.AsSingleton;

    Result.Build;
end;

end.
