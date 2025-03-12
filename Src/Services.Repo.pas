unit Services.Repo;

interface

uses
  Services.IRepo,

  Spring.Collections,

  System.SysUtils,
  System.Variants,
  System.SyncObjs,

  FireDAC.VCLUI.Wait,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  Data.DB, FireDAC.Comp.Client, FireDAC.Phys.SQLite,
  Firedac.DApt;

type

  TRepo = class(TInterfacedObject, IRepo)
    private
      FSQLLite: TFDConnection;
      FManager: TFDCustomManager;
      FCT: TCriticalSection;

    public
      constructor Create;

      function GetFiles(aQuery: string): IList<TSearchResult>;
      procedure AddFile(aFileName: string; aPath: string);
      procedure BatchAddFiles(aFiles: IOrderedSet<string>);

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
        Open('SELECT path FROM files WHERE path=?', [aPath]);

        if RecordCount = 0 then
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO files (path,name) VALUES (:path,:name)');
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

procedure TRepo.BatchAddFiles(aFiles: IOrderedSet<string>);
begin
  FCT.Enter;
  try
    var lQuery := TFDQuery.Create(nil);
    lQuery.Connection := FSQLLite;
    try
      with lQuery do
      begin
        SQL.Add('DELETE FROM files;');

        for var lFile in aFiles do
        begin
          var lFileName := TPath.GetFileName(lFile);
          SQL.Add('INSERT INTO files (path,name) VALUES ('''+lFIle+''','''+lFileName+''');');
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
                    `path`	TEXT NOT NULL UNIQUE,
                    `name`	TEXT NOT NULL,
                    PRIMARY KEY(path)
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

function TRepo.GetFiles(aQuery: string): IList<TSearchResult>;
begin
  Result := TCollections.CreateList<TSearchResult>;

  FCT.Enter;
  try
    var lQuery := TFDQuery.Create(nil);
    lQuery.Connection := FSQLLite;
    try
      with lQuery do
      begin
        Open('SELECT * FROM files WHERE name LIKE ?', [('%'+aQuery+'%')]);

        while not EOF do
        begin
          var lResult: TSearchResult;

          lResult.FileName := FieldByName('name').AsString;
          lResult.Path := FieldByName('path').AsString;

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
