unit Helpers;

interface

type

  TFileObj = class(TObject)
    private
      FRelativePath: string;
      FFullPath: string;
    public
      constructor Create(aFullPath, aRelativePath: string);

      property RelativePath: string read FRelativePath write FRelativePath;
      property FullPath: string read FFullPath write FFullPath;
  end;

implementation

{ TFileObj }

constructor TFileObj.Create(aFullPath, aRelativePath: string);
begin
  FFullPath := aFullPath;
  FRelativePath := aRelativePath;
end;

end.
