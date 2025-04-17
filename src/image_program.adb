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
begin
	Set_Default_Colours(White, Black);
	case Argument_Count is
		when 2 => 
			if Integer'Value(Argument(2)) mod 2 = 0 then
				Read(Argument(1), Image);
				Print(Image, 0, 0);
			else
				Read(Argument(1), Image);
				Print_Image_Information(Image);
			end if;
		when 4 =>
			Read(Argument(1), Image);
			Print(Image, Integer'Value(Argument(3)), Integer'Value(Argument(4)));
		when others =>
			Print_Error("Incorrect number of arguments!");
	end case;
exception
	when Name_Error =>
		Print_Error("File does not exist!");
end Image_Program;
