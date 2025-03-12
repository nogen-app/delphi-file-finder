unit services.IRepo;

interface

uses
  Spring.Collections;

type
  TSearchResult = record
    Path: string;
    FileName: string;
  end;

  IRepo = interface(IInterface)
    ['{95ED51F2-81DE-46A5-8B07-71C406D24FDC}']

    function GetFiles(aQuery: string): IList<TSearchResult>;
    procedure AddFile(aFileName: string; aPath: string);
    procedure BatchAddFiles(aFiles: IOrderedSet<string>);

  end;

implementation

end.
