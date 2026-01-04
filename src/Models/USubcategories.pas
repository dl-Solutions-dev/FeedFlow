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
  File last update : 2026-01-04T14:37:08.867+01:00
  Signature : 822c256edc2b5916378547b3c07a27ab32231a11
  ***************************************************************************
*)

/// <summary>
///   Modèle sous-catégories
/// </summary>
unit USubcategories;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  ///   Sous-catégories
  /// </summary>
  TSubcategories = class
  strict private
    /// <summary>
    ///   Intance du FDQuery
    /// </summary>
    FQrySubcategories: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retournel al iste des sous-catégories
    /// </summary>
    function GetListOfSubcategories( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TSubcategories }

constructor TSubcategories.Create;
begin
  FQrySubcategories := TFDQuery.Create( nil );

  FQrySubcategories.Name := 'QryListeSubcategories';
  FQrySubcategories.SQL.Clear;
  FQrySubcategories.SQL.Add( '''
    select * from SUBCATEGORY
    order by SUBCATEGORY_NAME
  ''');
end;

destructor TSubcategories.Destroy;
begin
  FreeAndNil(FQrySubcategories);

  inherited;
end;

function TSubcategories.GetListOfSubcategories(
  aConnection: TFDConnection): TFDQuery;
begin
  FQrySubcategories.Close;
  FQrySubcategories.Connection := aConnection;
  FQrySubcategories.Open;

  Result := FQrySubcategories;
end;

end.
