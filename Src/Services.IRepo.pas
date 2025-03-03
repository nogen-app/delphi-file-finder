unit services.IRepo;

interface

uses
  System.Generics.Collections,
  Spring.Collections;

type
  TSearchResult = record
    Path: string;
    FileName: string;
  end;

  IRepo = interface(IInterface)
    ['{95ED51F2-81DE-46A5-8B07-71C406D24FDC}']

    function GetFiles(aQuery: string): TList<TSearchResult>;
    procedure AddFile(aFileName: string; aPath: string);
    procedure BatchAddFiles(aFiles: IList<string>);

  end;

implementation

end.
