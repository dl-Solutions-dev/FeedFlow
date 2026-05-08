unit Utils.Images;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Imaging.JPEG,
  Vcl.Imaging.PNGImage,
  Vcl.Imaging.GIFImg;

type
  TImageType = ( itUnknown, itJPEG, itPNG, itGIF, itWEBP, itBMP );

  TImageValidationResult = record
    IsValid: Boolean;
    ImageType: TImageType;
    Width: Integer;
    Height: Integer;
    ErrorMsg: string;
  end;

function DetectImageType( Stream: TStream ): TImageType;
function ImageTypeToExt( ImageType: TImageType ): string;
function ImageTypeToMime( ImageType: TImageType ): string;
function IsFileSizeOk( Stream: TStream ): Boolean;
function ValidateImage(Stream: TStream): TImageValidationResult;

implementation

const
  MAX_IMAGE_SIZE = 5 * 1024 * 1024; // 5 Mo
  MAX_IMAGE_WIDTH  = 8000; // pixels
  MAX_IMAGE_HEIGHT = 8000;

function DetectImageType( Stream: TStream ): TImageType;
var
  Header: array[ 0..11 ] of Byte;
  BytesRead: Integer;
begin
  Result := itUnknown;
  Stream.Position := 0;
  BytesRead := Stream.Read( Header, SizeOf( Header ) );
  if BytesRead < 4 then
    Exit;

  // JPEG : FF D8 FF
  if ( Header[ 0 ] = $FF ) and ( Header[ 1 ] = $D8 ) and ( Header[ 2 ] = $FF ) then
    Result := itJPEG

    // PNG : 89 50 4E 47 0D 0A 1A 0A
  else if ( Header[ 0 ] = $89 ) and ( Header[ 1 ] = $50 ) and
    ( Header[ 2 ] = $4E ) and ( Header[ 3 ] = $47 ) then
    Result := itPNG

    // GIF : 47 49 46 38 (GIF8)
  else if ( Header[ 0 ] = $47 ) and ( Header[ 1 ] = $49 ) and
    ( Header[ 2 ] = $46 ) and ( Header[ 3 ] = $38 ) then
    Result := itGIF

    // WEBP : 52 49 46 46 ?? ?? ?? ?? 57 45 42 50
  else if ( Header[ 0 ] = $52 ) and ( Header[ 1 ] = $49 ) and
    ( Header[ 2 ] = $46 ) and ( Header[ 3 ] = $46 ) and
    ( BytesRead >= 12 ) and
    ( Header[ 8 ] = $57 ) and ( Header[ 9 ] = $45 ) and
    ( Header[ 10 ] = $42 ) and ( Header[ 11 ] = $50 ) then
    Result := itWEBP

    // BMP : 42 4D
  else if ( Header[ 0 ] = $42 ) and ( Header[ 1 ] = $4D ) then
    Result := itBMP;
end;

function ImageTypeToExt( ImageType: TImageType ): string;
begin
  case ImageType of
    itJPEG: Result := '.jpg';
    itPNG: Result := '.png';
    itGIF: Result := '.gif';
    itWEBP: Result := '.webp';
    itBMP: Result := '.bmp';
  else
    Result := '';
  end;
end;

function ImageTypeToMime( ImageType: TImageType ): string;
begin
  case ImageType of
    itJPEG: Result := 'image/jpeg';
    itPNG: Result := 'image/png';
    itGIF: Result := 'image/gif';
    itWEBP: Result := 'image/webp';
    itBMP: Result := 'image/bmp';
  else
    Result := '';
  end;
end;

function IsFileSizeOk( Stream: TStream ): Boolean;
begin
  Result := Stream.Size <= MAX_IMAGE_SIZE;
end;

function ValidateImage(Stream: TStream): TImageValidationResult;
var
  Bmp  : TBitmap;
  Jpeg : TJPEGImage;
  Png  : TPngImage;
  Gif  : TGIFImage;
begin
  Result.IsValid   := False;
  Result.ImageType := itUnknown;
  Result.Width     := 0;
  Result.Height    := 0;
  Result.ErrorMsg  := '';

  // 1. Taille fichier
  if Stream.Size > MAX_IMAGE_SIZE then
  begin
    Result.ErrorMsg := 'File too large (max 5MB)';
    Exit;
  end;

  // 2. Détection magic bytes
  Result.ImageType := DetectImageType(Stream);
  if Result.ImageType = itUnknown then
  begin
    Result.ErrorMsg := 'Unsupported or invalid image format';
    Exit;
  end;

  // 3. Décodage réel selon le type
  Stream.Position := 0;
  try
    case Result.ImageType of

      itJPEG:
      begin
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.LoadFromStream(Stream);
          // Forcer le décodage complet via un TBitmap
          Bmp := TBitmap.Create;
          try
            Bmp.Assign(Jpeg);
            Result.Width  := Bmp.Width;
            Result.Height := Bmp.Height;
          finally
            Bmp.Free;
          end;
        finally
          Jpeg.Free;
        end;
      end;

      itPNG:
      begin
        Png := TPngImage.Create;
        try
          Png.LoadFromStream(Stream);
          Result.Width  := Png.Width;
          Result.Height := Png.Height;
        finally
          Png.Free;
        end;
      end;

      itGIF:
      begin
        Gif := TGIFImage.Create;
        try
          Gif.LoadFromStream(Stream);
          Result.Width  := Gif.Width;
          Result.Height := Gif.Height;
        finally
          Gif.Free;
        end;
      end;

      itBMP:
      begin
        Bmp := TBitmap.Create;
        try
          Bmp.LoadFromStream(Stream);
          Result.Width  := Bmp.Width;
          Result.Height := Bmp.Height;
        finally
          Bmp.Free;
        end;
      end;

      itWEBP:
      begin
        // Delphi VCL ne décode pas WEBP nativement.
        // Option A : rejeter WEBP (plus simple)
        // Option B : utiliser libwebp.dll (voir note plus bas)
        Result.ErrorMsg := 'WEBP not supported';
        Exit;
      end;

    end; // case

  except
    on E: Exception do
    begin
      Result.ErrorMsg := 'Invalid or corrupted image: ' + E.Message;
      Exit;
    end;
  end;

  // 4. Dimensions raisonnables (protection décompression bomb)
  if (Result.Width > MAX_IMAGE_WIDTH) or (Result.Height > MAX_IMAGE_HEIGHT) then
  begin
    Result.ErrorMsg := Format('Image too large (%dx%d, max %dx%d)',
      [Result.Width, Result.Height, MAX_IMAGE_WIDTH, MAX_IMAGE_HEIGHT]);
    Exit;
  end;

  if (Result.Width = 0) or (Result.Height = 0) then
  begin
    Result.ErrorMsg := 'Image has zero dimensions';
    Exit;
  end;

  Result.IsValid := True;
end;

end.

