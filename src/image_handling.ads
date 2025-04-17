with Ada.Text_IO;	use Ada.Text_IO;

package Image_Handling is 
  -- Typer
  type Pixel_Type is record
    R : Natural := 127;
    G : Natural := 0;
    B : Natural := 255;
    Alpha : Boolean := False;
  end record;
  type Image_Area_Type is Array(Positive range <>, Positive range <>) of Pixel_Type;
  type Area_Type is access all Image_Area_Type;
  type Image_Type is private;
  type Area_Container_Type is record
    Area : Area_Type;
  end record;
  --type Is_Transparent_Type is range 0 .. 1;

  --Underprogram
  procedure Put(Pixel : in Pixel_Type);
  procedure Get(F : in File_Type; Pixel : out Pixel_Type);
  procedure Print_Image_Information(Image : in Image_Type);
  procedure Read(File_Name : in String; Image : out Image_Type);
  procedure Print(Image : in Image_Type; Start_X, Start_Y : in Natural);
  function Image_Exists(Image : in Image_Type) return Boolean;
  procedure Delete(Image : in out Image_Type);


  private

  type Image_Type is record
    X_Dim : Positive;
    Y_Dim : Positive;
    Area_Container : Area_Container_Type;
  end record;

end Image_Handling;
