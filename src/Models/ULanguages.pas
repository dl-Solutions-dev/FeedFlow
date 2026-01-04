(* C2PP
  ***************************************************************************

  Feed Flow

    Copyright 2026 - Dany Leblanc under AGPL 3.0 license.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
  OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  This program is a demo of the possibilities offered by the new WebStencils
  framework, combined with the HTMX JavaScript library.

  ***************************************************************************

  Author(s) :
  Dany Leblanc

  Project site :
  https://github.com/dl-Solutions-dev/FeedFlow

  ***************************************************************************
  File last update : 2026-01-04T14:37:08.841+01:00
  Signature : 9da83ce1275cd2032577ecccba449c7d892f216e
  ***************************************************************************
*)

/// <summary>
///   Modèle pou rles langues
/// </summary>
unit ULanguages;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  ///   Class langues
  /// </summary>
  TLanguages = class
  strict private
    /// <summary>
    ///   Instance du FDQuery
    /// </summary>
    FQryLanguages: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne un FDQuery avec la liste des langues existantes
    /// </summary>
    function GetListOfLanguages( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TLanguages }

constructor TLanguages.Create;
begin
  FQryLanguages := TFDQuery.Create( nil );

  FQryLanguages.Name := 'QryListeLanguages';
  FQryLanguages.SQL.Clear;
  FQryLanguages.SQL.Add( '''
    select * from LANGUAGE
    order by LANGUAGE_NAME
  ''');
end;

destructor TLanguages.Destroy;
begin
FreeAndNil(FQryLanguages);

  inherited;
end;

function TLanguages.GetListOfLanguages(aConnection: TFDConnection): TFDQuery;
begin
  FQryLanguages.Close;
  FQryLanguages.Connection := aConnection;
  FQryLanguages.Open;

  Result := FQryLanguages;
end;

end.
