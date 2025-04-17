with Ada.Text_IO;	use Ada.Text_IO;

package Image_Handling is 
  -- Typer
  type Pixel_Type is record
    R : Natural := 127;
    G : Natural := 0;
    B : Natural := 255;
    Alpha : Boolean := False;
  end record;
  type Image_Area_Type is Array(1 .. 30, 1 .. 30) of Pixel_Type;
  type Image_Type is private;

  --Underprogram
  procedure Put(Pixel : in Pixel_Type);
  procedure Get(F : in File_Type; Pixel : out Pixel_Type);
  procedure Print_Image_Information(Image : in Image_Type);
  procedure Read(File_Name : in String; Image : out Image_Type);
  procedure Print(Image : in Image_Type; Start_X, Start_Y : in Natural);

  private

  type Image_Type is record
    X_Dim : Positive := 30;
    Y_Dim : Positive := 30;
    Image_Area : Image_Area_Type;
  end record;

end Image_Handling;
