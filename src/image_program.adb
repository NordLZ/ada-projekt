with Ada.Text_IO;	use Ada.Text_IO;
with Ada.Command_Line;	use Ada.Command_Line;
with Image_Handling;  use Image_Handling;

with TJa.Window.Elementary;             use TJa.Window.Elementary;
with TJa.Window.Text_RGB;               use TJa.Window.Text_RGB;

procedure Image_Program is
	procedure Print_Error(Error : in String) is
	begin
		Put_Line("Error! " & Error);
		Put_Line("Usage: " & Command_Name & " IMAGE_FILENAME N [X Y]");
	end Print_Error;
	Image : Image_Type;
	function Corrent_Argument_Amount return Boolean is
	begin

		if Argument_Count = 2 or Argument_Count = 4 then
			return True;
		end if;
		Print_Error("Incorrect number of arguments!");
		return False;
	end Corrent_Argument_Amount;

begin

	if Corrent_Argument_Amount then
		Read(Argument(1), Image);

		if Integer'Value(Argument(2)) mod 2 = 0 then
			Print(Image, Integer'Value(Argument(3)), Integer'Value(Argument(4)));

		else
			Print_Image_Information(Image);

		end if;
	end if;
end Image_Program;
