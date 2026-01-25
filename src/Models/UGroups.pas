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
  File last update : 2026-01-25T10:25:50.000+01:00
  Signature : 614ceafbf511dd0c7a9927bfa0e435be638212f2
  ***************************************************************************
*)

/// <summary>
///   Groupe de feed
/// </summary>
unit UGroups;

interface

uses
  System.Classes,
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB;

type
  /// <summary>
  ///   Groupe
  /// </summary>
  TGroups = class
  strict private
    /// <summary>
    ///   Dataset contenant la liste des groupes existant
    /// </summary>
    FQryGroups: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Retourne un dataset contenant la liste des groupes existant
    /// </summary>
    function GetListOfGroups( aConnection: TFDConnection ): TFDQuery;
  end;

implementation

{ TGroups }

constructor TGroups.Create;
begin
  FQryGroups := TFDQuery.Create( nil );
  FQryGroups.SQL.Text := '''
    select * from GROUPS
    order by GROUP_ID
  ''';
end;

destructor TGroups.Destroy;
begin
  FreeAndNil( FQryGroups );

  inherited;
end;

function TGroups.GetListOfGroups( aConnection: TFDConnection ): TFDQuery;
begin
  FQryGroups.Connection := aConnection;
  FQryGroups.Open;

  Result := FQryGroups;
end;

end.

