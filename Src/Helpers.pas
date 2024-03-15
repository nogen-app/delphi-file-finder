unit Helpers;

interface

type

  TStrObj = class(TObject)
    strict private
      FValue: string;
    public
      constructor Create(aValue:string);

      property Value: string read FValue write FValue;
  end;

implementation

{ TStrObj }

constructor TStrObj.Create(aValue: string);
begin
  FValue := aValue;
end;

end.
