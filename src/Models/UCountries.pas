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
  File last update : 2026-01-04T14:37:08.818+01:00
  Signature : 289cb1cd6a530af0b265b280646ddbe923cc24ec
  ***************************************************************************
*)

/// <summary>
///   Modèle pour les pays
/// </summary>
unit UCountries;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  ///   Pays
  /// </summary>
  TCountries = class
  strict private
    /// <summary>
    ///   Query retournant la liste des pays
    /// </summary>
    FQryCountries: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne la liste des pays
    /// </summary>
    function GetListOfCountries( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TCountries }

constructor TCountries.Create;
begin
  FQryCountries := TFDQuery.Create( nil );

  FQryCountries.Name := 'QryListeCountries';
  FQryCountries.SQL.Clear;
  FQryCountries.SQL.Add( '''
    select * from COUNTRY
    order by COUNTRY_NAME
  ''');
end;

destructor TCountries.Destroy;
begin
  FreeAndNil(FQryCountries);

  inherited;
end;

function TCountries.GetListOfCountries(aConnection: TFDConnection): TFDQuery;
begin
  FQryCountries.Close;
  FQryCountries.Connection := aConnection;
  FQryCountries.Open;

  Result := FQryCountries;
end;

end.
