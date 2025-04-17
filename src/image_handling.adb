with Ada.Text_IO;	use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;

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
		for Y in 1 .. Image.Y_Dim loop
			for X in 1 .. Image.X_Dim loop
				Put(Image.Image_Area(X, Y));
			end loop;
		end loop;
	end Print_Image_Information;

	procedure Get(F : in File_Type; Pixel : out Pixel_Type) is
	begin
		Get(F, Pixel.R);
		Get(F, Pixel.G);
		Get(F, Pixel.B);
	end Get;

	procedure Read(File_Name : in String; Image : out Image_Type) is
		F : File_Type;
	begin
		Open(F, In_File, File_Name);
		
		Skip_Line(F, 2); -- skippa 'P3' och kommentar

		-- Hämta dimensioner
		Get(F, Image.X_Dim);
		Get(F, Image.Y_Dim);
		Skip_Line(F);
		

		Skip_Line(F); -- skippa maxvärde

		while not End_Of_File(F) loop
			for Y in 1 .. Image.Y_Dim loop
				for X in 1 .. Image.X_Dim loop
					Get(F, Image.Image_Area(X, Y));
				end loop;
				Skip_Line(F);
			end loop;
		end loop;
		Close(F);
	end Read;

	procedure Print(Image : in Image_Type; Start_X, Start_Y : Natural) is
		Current_Pixel : Pixel_Type;
	begin
		Reset_Colours;
		Clear_Window;

		for Y in 1 .. Image.Y_Dim loop
			for X in 1 .. Image.X_Dim loop
				Current_Pixel := Image.Image_Area(X,Y);
				Set_Background_Colour(To_Colour_Type(Current_Pixel.R, Current_Pixel.G, Current_Pixel.B));
				Goto_XY(X*2 + Start_X, Y + Start_Y);
				Put("..");
			end loop;

		end loop;
	end Print;
end Image_Handling;
