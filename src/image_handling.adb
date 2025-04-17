with Ada.Text_IO;	use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

with TJa.Window.Elementary;             use TJa.Window.Elementary;
with TJa.Window.Text_RGB;               use TJa.Window.Text_RGB;

package body Image_Handling is 
	procedure Put(Pixel : in Pixel_Type) is
	begin
		Put(Pixel.R, width => 4);
		Put(Pixel.G, width => 4);
		Put(Pixel.B, width => 4);
		if Pixel.Alpha then
			Put("True ");
		else
			Put("False");
		end if;
	end Put;

	procedure Print_Image_Information(Image : in Image_Type) is
	begin
		for Y in Image.Area'Range(2) loop
			for X in Image.Area'Range(1) loop
				Put(Image.Area(X, Y));
			end loop;
		end loop;
	end Print_Image_Information;

	procedure Get(F : in File_Type; Pixel : out Pixel_Type) is
	begin
		Get(F, Pixel.R);
		Get(F, Pixel.G);
		Get(F, Pixel.B);
	end Get;

-- 	procedure Get(F : in File_Type; Pixel : Image_Format out Image_Format_Type) is
-- 	begin
-- 		return Image_Format_Type'Value()
-- 	end Get;

	procedure Read(File_Name : in String; Image : out Image_Type) is
		F : File_Type;
	begin
		Open(F, In_File, File_Name);

		Skip_Line(F); -- TODO: hämta filtyp

		Skip_Line(F);  -- och kommentar

		-- Hämta dimensioner
		Get(F, Image.X_Dim);
		Get(F, Image.Y_Dim);
		Skip_Line(F);


		Skip_Line(F); -- skippa maxvärde

		Image.Area := new Image_Area_Type(1 .. Image.X_Dim, 1 .. Image.Y_Dim);

		while not End_Of_File(F) loop
			for Y in Image.Area'Range(2) loop
				for X in Image.Area'Range(1) loop
					Get(F, Image.Area(X, Y));
				end loop;
				Skip_Line(F);
			end loop;
		end loop;
		Close(F);
	end Read;

	function Is_New_Colour(Current_Pixel, Prev_Pixel : Pixel_Type) return Boolean is
	begin
		if Current_Pixel.R /= Prev_Pixel.R then
			return True;
		end if;
		if Current_Pixel.G /= Prev_Pixel.G then
			return True;
		end if;
		if Current_Pixel.B /= Prev_Pixel.B then
			return True;
		end if;
		return False;
	end Is_New_Colour;

	procedure Print(Image : in Image_Type; Start_X, Start_Y : Natural) is
		Current_Pixel : Pixel_Type;
		Prev_Pixel :  Pixel_Type;
	begin
		Reset_Colours;
		Clear_Window;

		for Y in Image.Area'Range(2) loop
			for X in Image.Area'Range(1) loop
				Current_Pixel := Image.Area(X,Y);
				if X = Image.Area'First(1) then
					Goto_XY(X*2 + Start_X, Y + Start_Y);
				end if;
				if (X = Image.Area'First(1) and Y = Image.Area'First(2)) or else Is_New_Colour(Current_Pixel, Prev_Pixel) then
					Set_Background_Colour(To_Colour_Type(Current_Pixel.R, Current_Pixel.G, Current_Pixel.B));
				end if;
				Put("  ");
			end loop;

		end loop;
	end Print;
	function Image_Exists(Image : in Image_Type) return Boolean is
	begin
		if Image.Area = null then
			return False;
		else
			return True;
		end if;
	end Image_Exists;

	procedure Free is new Ada.Unchecked_Deallocation(
	Object => Image_Area_Type,
	Name => Area_Type
	);

	procedure Delete(Image : in out Image_Type) is
	begin
		if Image_Exists(Image) then
			Free(Image.Area);
		end if;
	end Delete;
end Image_Handling;
