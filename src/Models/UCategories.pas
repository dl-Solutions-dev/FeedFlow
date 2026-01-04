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
  File last update : 2026-01-04T14:37:08.807+01:00
  Signature : 97beccc2e5d24a819416d4601074b74ecab01e81
  ***************************************************************************
*)

/// <summary>
///   Modèle Catégories
/// </summary>
unit UCategories;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  ///   Catégories
  /// </summary>
  TCategories = class
  strict private
    /// <summary>
    ///   Dataset retournant la liste des catégories
    /// </summary>
    FQryCategories: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Retourne la liste des catégories
    /// </summary>
    function GetListOfCategories( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TCategories }

constructor TCategories.Create;
begin
  FQryCategories := TFDQuery.Create( nil );

  FQryCategories.Name := 'QryListeCategories';
  FQryCategories.SQL.Clear;
  FQryCategories.SQL.Add( '''
    select * from CATEGORY
    order by CATEGORY_NAME
  ''');
end;

destructor TCategories.Destroy;
begin
  FreeAndNil( FQryCategories );

  inherited;
end;

function TCategories.GetListOfCategories( aConnection: TFDConnection ): TFDQuery;
begin
  FQryCategories.Close;
  FQryCategories.Connection := aConnection;
  FQryCategories.Open;

  Result := FQryCategories;
end;

end.

