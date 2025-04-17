-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                  T J A . W I N D O W . T E X T _ R G B                  |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  1.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2022                           |--
--|                  Torbjörn Jonsson,  TorJo38@Ida.LiU.se                  |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2022-02-26  Version 1.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
--|   Uses 'ansi codes' for changing modes and colours.                     |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package body TJa.Window.Text_RGB is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| To_Colour_Type
  -----------------------------------------------------------------------------
  
  function To_Colour_Type(R, G, B : in RGB_Value_Type) return Colour_Type is
    
  begin
    return (R => R, G => G, B => B);
  end To_Colour_Type;

  -----------------------------------------------------------------------------
  --| Global "constants" for default and current settings.
  -----------------------------------------------------------------------------

  -- Low contrast colors
  
  function Black return Colour_Type is
  begin
    return To_Colour_Type(  0,   0,   0);
  end Black;
  -----------------------------------------------------------------------------
  function Red return Colour_Type is
  begin
    return To_Colour_Type(127,   0,   0);
  end Red;
  -----------------------------------------------------------------------------
  function Green return Colour_Type is
  begin
    return To_Colour_Type(  0, 127,   0);
  end Green;
  -----------------------------------------------------------------------------
  function Yellow return Colour_Type is
  begin
    return To_Colour_Type(127, 127,   0);
  end Yellow;
  -----------------------------------------------------------------------------
  function Blue return Colour_Type is
  begin
    return To_Colour_Type(  0,   0, 127);
  end Blue;
  -----------------------------------------------------------------------------
  function Magenta return Colour_Type is
  begin
    return To_Colour_Type(127,   0, 127);
  end Magenta;
  -----------------------------------------------------------------------------
  function Cyan return Colour_Type is
  begin
    return To_Colour_Type(  0, 127, 127);
  end Cyan;
  -----------------------------------------------------------------------------
  function Light_Grey return Colour_Type is
  begin
    return To_Colour_Type(211, 211, 211);
  end Light_Grey;
  -----------------------------------------------------------------------------
  
  -- High contrast colors
  
  function Dark_Grey return Colour_Type is
  begin
    return To_Colour_Type(169, 169, 169);
  end Dark_Grey;
  -----------------------------------------------------------------------------
  function Bright_Red return Colour_Type is
  begin
    return To_Colour_Type(255,   0,   0);
  end Bright_Red;
  -----------------------------------------------------------------------------
  function Bright_Green return Colour_Type is
  begin
    return To_Colour_Type(  0, 255,   0);
  end Bright_Green;
  -----------------------------------------------------------------------------
  function Bright_Yellow return Colour_Type is
  begin
    return To_Colour_Type(255, 255,   0);
  end Bright_Yellow;
  -----------------------------------------------------------------------------
  function Bright_Blue return Colour_Type is
  begin
    return To_Colour_Type(  0,   0, 255);
  end Bright_Blue;
  -----------------------------------------------------------------------------
  function Bright_Magenta return Colour_Type is
  begin
    return To_Colour_Type(255,   0, 255);
  end Bright_Magenta;
  -----------------------------------------------------------------------------
  function Bright_Cyan return Colour_Type is
  begin
    return To_Colour_Type(  0, 255, 255);
  end Bright_Cyan;
  -----------------------------------------------------------------------------
  function White return Colour_Type is
  begin
    return To_Colour_Type(255, 255, 255);
  end White;
  
  -----------------------------------------------------------------------------
  
  function Default_Foreground_Colour return Colour_Type is
  begin
    return Black;
  end Default_Foreground_Colour;
  -----------------------------------------------------------------------------
  function Default_Background_Colour return Colour_Type is
  begin
    return White;
  end Default_Background_Colour;

  -----------------------------------------------------------------------------
  --| Global variables for default and current settings.
  -----------------------------------------------------------------------------

  Current_Default_Foreground_Colour : Colour_Type := Default_Foreground_Colour;
  Current_Default_Background_Colour : Colour_Type := Default_Background_Colour;

  Current_Foreground_Colour : Colour_Type := Default_Foreground_Colour;
  Current_Background_Colour : Colour_Type := Default_Background_Colour;

  Current_Default_Underlined_Mode : Underlined_Mode_Type
                                      := Default_Underlined_Mode;
  Current_Default_Inverted_Mode   : Inverted_Mode_Type
                                      := Default_Inverted_Mode;
  Current_Default_Bold_Mode       : Bold_Mode_Type
                                      := Default_Bold_Mode;

  Current_Underlined_Mode   : Underlined_Mode_Type := Default_Underlined_Mode;
  Current_Inverted_Mode     : Inverted_Mode_Type   := Default_Inverted_Mode;
  Current_Bold_Mode         : Bold_Mode_Type       := Default_Bold_Mode;


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Set_Default_Colours, Set_Default_Text_Modes
  -----------------------------------------------------------------------------

  procedure Set_Default_Colours(Foreground, Background : in Colour_Type) is

  begin
    Current_Default_Foreground_Colour := Foreground;
    Current_Default_Background_Colour := Background;
  end Set_Default_Colours;

  --===========================================================================
  procedure Set_Default_Text_Modes(Underlined : in Underlined_Mode_Type;
                                   Inverted   : in Inverted_Mode_Type;
                                   Bold       : in Bold_Mode_Type) is

  begin
    Current_Default_Underlined_Mode := Underlined;
    Current_Default_Inverted_Mode := Inverted;
    Current_Default_Bold_Mode := Bold;
  end Set_Default_Text_Modes;

  -----------------------------------------------------------------------------
  --| Reset_Colours, Reset_Text_Modes, Reset_Colours_And_Text_Modes
  -----------------------------------------------------------------------------

  procedure Reset_Colours is

  begin
    Set_Colours_And_Text_Modes(Foreground => Current_Default_Foreground_Colour,
                               Background => Current_Default_Background_Colour,
                               Underlined => Current_Underlined_Mode,
                               Inverted   => Current_Inverted_Mode,
                               Bold       => Current_Bold_Mode);
  end Reset_Colours;

  --===========================================================================
  procedure Reset_Text_Modes is

  begin
    Set_Colours_And_Text_Modes(Foreground => Current_Foreground_Colour,
                               Background => Current_Background_Colour,
                               Underlined => Current_Default_Underlined_Mode,
                               Inverted   => Current_Default_Inverted_Mode,
                               Bold       => Current_Default_Bold_Mode);
  end Reset_Text_Modes;

  --===========================================================================
  procedure Reset_Colours_And_Text_Modes is

  begin
    Set_Colours_And_Text_Modes(Foreground => Current_Default_Foreground_Colour,
                               Background => Current_Default_Background_Colour,
                               Underlined => Current_Default_Underlined_Mode,
                               Inverted   => Current_Default_Inverted_Mode,
                               Bold       => Current_Default_Bold_Mode);
  end Reset_Colours_And_Text_Modes;

  --===========================================================================
  procedure Reset_To_Original_Window_Settings is

  begin
    Put(Ascii.Esc);
    Put("[0m");
    Flush;

    Current_Default_Foreground_Colour := Default_Foreground_Colour;
    Current_Default_Background_Colour := Default_Background_Colour;
    Current_Default_Underlined_Mode   := Default_Underlined_Mode;
    Current_Default_Inverted_Mode     := Default_Inverted_Mode;
    Current_Default_Bold_Mode         := Default_Bold_Mode;
  end Reset_To_Original_Window_Settings;

  -----------------------------------------------------------------------------
  --| Set_Colours, Set_Text_Modes, Set_Colours_And_Text_Modes
  -----------------------------------------------------------------------------

  procedure Set_Colours(Foreground, Background : in Colour_Type) is

  begin
    Set_Foreground_Colour(Foreground);
    Set_Background_Colour(Background);
  end Set_Colours;

  --===========================================================================
  procedure Set_Text_Modes(Underlined : in Underlined_Mode_Type;
                           Inverted   : in Inverted_Mode_Type;
                           Bold       : in Bold_Mode_Type) is

  begin
    Set_Colours_And_Text_Modes(Foreground => Current_Foreground_Colour,
                               Background => Current_Background_Colour,
                               Underlined => Underlined,
                               Inverted   => Inverted,
                               Bold       => Bold);
  end Set_Text_Modes;

  --===========================================================================
  procedure Set_Colours_And_Text_Modes(Foreground : in Colour_Type;
                                       Background : in Colour_Type;
                                       Underlined : in Underlined_Mode_Type;
                                       Inverted   : in Inverted_Mode_Type;
                                       Bold       : in Bold_Mode_Type) is

  begin
    Reset_To_Original_Window_Settings;

    if Underlined = On then
      Put(Ascii.Esc);
      Put("[4m");
    end if;
    if Underlined /= Current_Underlined_Mode then
      Current_Underlined_Mode := Underlined;
    end if;

    if Inverted = On then
      Put(Ascii.Esc);
      Put("[7m");
    end if;
    if Inverted /= Current_Inverted_Mode then
      Current_Inverted_Mode := Inverted;
    end if;

    if Bold = On then
      Put(Ascii.Esc);
      Put("[1m");
    end if;
    if Bold /= Current_Bold_Mode then
      Current_Bold_Mode := Bold;
    end if;

    Set_Colours(Foreground, Background);
  end Set_Colours_And_Text_Modes;

  -----------------------------------------------------------------------------
  --| Set_Foreground_Colour, Set_Background_Colour
  -----------------------------------------------------------------------------

  procedure Set_Foreground_Colour(Colour : in Colour_Type) is

  begin
    Put(Ascii.Esc);
    Put("[38;2;");
    Put(Colour.R, Width => 0);
    Put(";");
    Put(Colour.G, Width => 0);
    Put(";");
    Put(Colour.B, Width => 0);
    Put('m');
    Flush;

    Current_Foreground_Colour := Colour;
  end Set_Foreground_Colour;
  -----------------------------------------------------------------------------
  procedure Set_Background_Colour(Colour : in Colour_Type) is

  begin
    Put(Ascii.Esc);
    Put("[48;2;");
    Put(Colour.R, Width => 0);
    Put(";");
    Put(Colour.G, Width => 0);
    Put(";");
    Put(Colour.B, Width => 0);
    Put('m');
    Flush;

    Current_Background_Colour := Colour;
  end Set_Background_Colour;

  -----------------------------------------------------------------------------
  --| Get_Foreground_Colour, Get_Background_Colour
  -----------------------------------------------------------------------------

  function Get_Foreground_Colour
           return Colour_Type is

  begin
    return Current_Foreground_Colour;
  end Get_Foreground_Colour;
  -----------------------------------------------------------------------------
  function Get_Background_Colour
           return Colour_Type is

  begin
    return Current_Background_Colour;
  end Get_Background_Colour;

  -----------------------------------------------------------------------------
  --| Set_Underlined_Mode, Set_Inverted_Mode, Set_Bold_Mode
  -----------------------------------------------------------------------------

  procedure Set_Underlined_Mode(Mode : in Underlined_Mode_Type := On) is

  begin
    if Mode /= Current_Underlined_Mode then
      Set_Colours_And_Text_Modes(Foreground => Current_Foreground_Colour,
                                 Background => Current_Background_Colour,
                                 Underlined => Mode,
                                 Inverted   => Current_Inverted_Mode,
                                 Bold       => Current_Bold_Mode);
    end if;
  end Set_Underlined_Mode;
  -----------------------------------------------------------------------------
  procedure Set_Inverted_Mode(Mode : in Inverted_Mode_Type := On) is

  begin
    if Mode /= Current_Inverted_Mode then
      Set_Colours_And_Text_Modes(Foreground => Current_Foreground_Colour,
                                 Background => Current_Background_Colour,
                                 Underlined => Current_Underlined_Mode,
                                 Inverted   => Mode,
                                 Bold       => Current_Bold_Mode);
    end if;
  end Set_Inverted_Mode;
  -----------------------------------------------------------------------------
  procedure Set_Bold_Mode(Mode : in Bold_Mode_Type := On) is

  begin
    if Mode /= Current_Bold_Mode then
      Set_Colours_And_Text_Modes(Foreground => Current_Foreground_Colour,
                                 Background => Current_Background_Colour,
                                 Underlined => Current_Underlined_Mode,
                                 Inverted   => Current_Inverted_Mode,
                                 Bold       => Mode);
    end if;
  end Set_Bold_Mode;

  -----------------------------------------------------------------------------
  --| Get_Underlined_Mode, Get_Inverted_Mode, Get_Bold_Mode
  -----------------------------------------------------------------------------

  function Get_Underlined_Mode
           return Underlined_Mode_Type is

  begin
    return Current_Underlined_Mode;
  end Get_Underlined_Mode;
  -----------------------------------------------------------------------------
  function Get_Inverted_Mode
           return Inverted_Mode_Type is

  begin
    return Current_Inverted_Mode;
  end Get_Inverted_Mode;
  -----------------------------------------------------------------------------
  function Get_Bold_Mode
           return Bold_Mode_Type is

  begin
    return Current_Bold_Mode;
  end Get_Bold_Mode;

  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -- Nothing just now.


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Reset modes so that current mode variables is ok.
  Reset_Colours_And_Text_Modes;
end TJa.Window.Text_RGB;
