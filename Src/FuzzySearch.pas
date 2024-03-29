unit FuzzySearch;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IOUtils
  ,Math, System.types;

type
  TWeightedResult = record
      Weight: Integer;
      Value: string;

      class function New(aValue: string; aWeight: integer): TWeightedResult; static;
  end;

  TFuzzySearch = class
  public
    class function TrigramIndex(const s: string): TList<string>;
    class function LevenshteinDistance(const s1, s2: string): Integer;
    class function FuzzySearch(const pattern: string; const fileList: TList<string>; tolerance: Integer): TList<TWeightedResult>;
  end;

 implementation

 uses
  System.Generics.Defaults;

{ TFileSearch }

class function TFuzzySearch.TrigramIndex(const s: string): TList<string>;
var
  i: Integer;
begin
  Result := TList<string>.Create;
  for i := 1 to Length(s) - 2 do
    Result.Add(Copy(s, i, 3));
end;

class function TFuzzySearch.LevenshteinDistance(const s1, s2: string): Integer;
var
  m, n, i, j: Integer;
  cost: TArray<Integer>;
begin
  m := Length(s1);
  n := Length(s2);

  SetLength(cost, n + 1);

  for i := 0 to n do
    cost[i] := i;

  for i := 1 to m do
  begin
    cost[0] := i;
    var lastdiag := i - 1;
    for j := 1 to n do
    begin
      var olddiag := cost[j];
      if s1[i] = s2[j] then
        cost[j] := lastdiag
      else
        cost[j] := Min(Min(cost[j - 1], cost[j]), lastdiag) + 1;
      lastdiag := olddiag;
    end;
  end;

  Result := cost[n];
end;


class function TFuzzySearch.FuzzySearch(const pattern: string; const fileList: TList<string>; tolerance: Integer): TList<TWeightedResult>;
var
  j: Integer;
  patternIndex: TList<string>;
  fileIndex: TDictionary<string, TList<string>>;
begin
  patternIndex := TrigramIndex(pattern);
  fileIndex := TDictionary<string, TList<string>>.Create;
  Result := TList<TWeightedResult>.Create;

  // Build trigram index for file names
  for var i := 0 to fileList.Count-1 do
  begin
    if not fileIndex.ContainsKey(fileList[i]) then
      fileIndex.Add(fileList[i], TrigramIndex(fileList[i]));
  end;

  // Search through trigram index for matches
  for var i := 0 to fileList.Count-1 do
  begin
    var fileTrigrams := fileIndex[fileList[i]];
    var intersectionCount := 0;
    for j := 0 to patternIndex.Count - 1 do
    begin
      if fileTrigrams.Contains(patternIndex[j]) then
        Inc(intersectionCount);
    end;
    if intersectionCount >= tolerance then
    begin
      Result.Add(TWeightedResult.New(fileList[i], intersectionCount));
    end;
  end;

  fileIndex.Free;
  patternIndex.Free;

  Result.Sort(TComparer<TWeightedResult>.Construct(
    function(const left, right: TWeightedResult): Integer
    begin
      Result := TComparer<Integer>.Default.Compare(left.Weight, right.Weight);
    end));

  Result.Reverse;
end;

{ TWeightedResult }

class function TWeightedResult.New(aValue: string;
  aWeight: integer): TWeightedResult;
begin
  Result.Value := aValue;
  Result.Weight := aWeight;
end;

end.

