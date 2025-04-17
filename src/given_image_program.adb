with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Command_Line;			use Ada.Command_Line;

with Image_Handling;			use Image_Handling;

procedure Given_Image_Program is
  
  procedure Error_Message(Message       : in String;
			  Usage_Message : in Boolean := False) is
    
  begin
    Put_Line("Error! " & Message);
    if Usage_Message then
      Put_Line("Usage: " & Command_Name & " IMAGE_FILENAME N [X Y]");
    end if;
  end Error_Message;

  function File_Exists(Filename : in String) return Boolean is
    
    File : File_Type;
    
  begin
    Open(File, In_File, Filename);
    Close(File);
    return True;
    
  exception
     when Name_Error =>
       return False;
  end File_Exists;
  
  function Command_Line_Arguments_Seems_OK return Boolean is
    
  begin
    -- Kontrollera antal argument
    if Argument_Count /= 2 and Argument_Count /= 4 then
      Error_Message("Incorrect number of arguments!", Usage_Message => True);
      return False;
    end if;
    return True;
  end Command_Line_Arguments_Seems_OK;
  
	--   function Begins_With(Text, Beginning : in String) return Boolean is
	--
	--   begin
	--     if Text'Length < Beginning'Length then
	--       return False;
	--     end if;
	--
	--     for I in Beginning'Range loop
	--       if Text(I) /= Beginning(I) then
	--  return False;
	--       end if;
	--     end loop;
	--
	--     return True;
	--   end Begins_With;
  
  Image, Smaller_Image : Image_Type;
  X, Y                 : Integer;
  
begin
  if Command_Line_Arguments_Seems_OK then
    Read(Argument(1), Image);
    
    if not Image_Exists(Image) then
      Error_Message("Image was not read correctly!", Usage_Message => False);
      
    else
      if Integer'Value(Argument(2)) mod 2 = 1 then
	Print_Image_Information(Image);
      
      else
	X := Integer'Value(Argument(3));
	Y := Integer'Value(Argument(4));
	
	Print(Image, X, Y);
	--  if Begins_With(Argument(1), "slice_test_") then
	--    Smaller_Image := Slice(Image, 3, 10, 8, 17);
	--    Print(Image, 1, 1);
	--    Print(Smaller_Image, X, Y);
	--    Delete(Smaller_Image);
	--  else
	--    Print(Image, X, Y);
	--  end if;
      end if;
      
      Delete(Image);
    end if;
  end if;
  
--  exception
--     when Image_Constraint_Error =>
--       Put_Line("Error! Slice outside image.");
--       Delete(Image);
end Given_Image_Program;
