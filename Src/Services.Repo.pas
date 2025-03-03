unit Services.Repo;

interface

uses
  Services.IRepo,

  Spring.Collections,

  System.SysUtils,
  System.Generics.Collections,
  System.Variants,
  System.SyncObjs,

  FireDAC.VCLUI.Wait,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  Data.DB, FireDAC.Comp.Client, FireDAC.Phys.SQLite,
  Firedac.DApt, FireDac.Comp.UI;

type

  TRepo = class(TInterfacedObject, IRepo)
    private
      FSQLLite: TFDConnection;
      FManager: TFDCustomManager;
      FCT: TCriticalSection;

    public
      constructor Create;

      function GetFiles(aQuery: string): TList<TSearchResult>;
      procedure AddFile(aFileName: string; aPath: string);
      procedure BatchAddFiles(aFiles: IList<string>);

      destructor Destroy; override;
  end;


implementation

uses
  System.IOUtils;

{ TRepo }

procedure TRepo.AddFile(aFileName, aPath: string);
begin
  FCT.Enter;
  try
    var lQuery := TFDQuery.Create(nil);
    lQuery.Connection := FSQLLite;
    try
      with lQuery do
      begin
        Open('SELECT Path FROM files WHERE Path=?', [aPath]);

        if RecordCount = 0 then
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO files (Path,Name) VALUES (:path,:name)');
          Params.ParamByName('path').AsString := aPath;
          Params.ParamByName('name').AsString := aFileName;

          Execute;
        end;
      end;
    finally
      FreeAndNil(lQuery);
    end;
  finally
    FCT.Leave;
  end;
end;

procedure TRepo.BatchAddFiles(aFiles: IList<string>);
begin
  FCT.Enter;
  try
    var lQuery := TFDQuery.Create(nil);
    lQuery.Connection := FSQLLite;
    try
      with lQuery do
      begin
        SQL.Add('delete from files;');

        for var lFile in aFiles do
        begin
          var lFileName := TPath.GetFileName(lFile);
          SQL.Add('INSERT INTO files (Path,Name) VALUES ('''+lFIle+''','''+lFileName+''');');
        end;

        Execute;
      end;
    finally
      FreeAndNil(lQuery);
    end;
  finally
    FCT.Leave;
  end;
end;

constructor TRepo.Create;
begin
  FCT := TCriticalSection.Create;

  FManager := FDManager;

  FManager.SilentMode := True;

  FSQLLite := TFDConnection.Create(nil);
  FSQLLite.DriverName := 'SQLite';

  FSQLLite.Open;


  var lQuery := TFDQuery.Create(nil);
  lQuery.Connection := FSQLLite;
  try
    with lQuery do
    begin
      SQL.Add('''
                  CREATE TABLE `files` (
                    `Path`	TEXT NOT NULL UNIQUE,
                    `Name`	TEXT NOT NULL,
                    PRIMARY KEY(Path)
                  );
              ''');

      Execute;
    end;
  finally
    FreeAndNil(lQuery);
  end;
end;

destructor TRepo.Destroy;
begin
  FreeAndNil(FSQLLite);
  FreeAndNil(FCT);

  inherited;
end;

function TRepo.GetFiles(aQuery: string): TList<TSearchResult>;
begin
  Result := TList<TSearchResult>.Create;

  FCT.Enter;
  try
    var lQuery := TFDQuery.Create(nil);
    lQuery.Connection := FSQLLite;
    try
      with lQuery do
      begin
        Open('SELECT * FROM files WHERE Name LIKE ?', [('%'+aQuery+'%')]);

        while not EOF do
        begin
          var lResult: TSearchResult;

          lResult.FileName := FieldByName('Name').AsString;
          lResult.Path := FieldByName('Path').AsString;

          Result.Add(lResult);

          Next;
        end;

      end;
    finally
      FreeAndNil(lQuery);
    end;
  finally
    FCT.Leave;
  end;



end;

end.
