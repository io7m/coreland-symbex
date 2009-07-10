with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Wide_Unbounded_IO;
with Symbex.Lex;
with Symbex.Parse;
with Symbex.Walk;

procedure Dump is
  package Exceptions renames Ada.Exceptions;
  package IO         renames Ada.Text_IO;
  package Lex        renames Symbex.Lex;
  package Parse      renames Symbex.Parse;
  package Walk       renames Symbex.Walk;
  package WIO        renames Ada.Wide_Text_IO;
  package WUIO       renames Ada.Wide_Text_IO.Wide_Unbounded_IO;
  package Fixed      renames Ada.Strings.Wide_Fixed;

  use type Lex.Lexer_Status_t;
  use type Lex.Lexer_t;
  use type Lex.Token_Kind_t;
  use type Parse.Tree_Status_t;
  use type Parse.List_Depth_t;
  use type Parse.List_Length_t;
  use type Walk.Walk_Status_t;

  Done         : Boolean;
  Lexer_State  : Lex.Lexer_t;
  Lexer_Status : Lex.Lexer_Status_t;
  Token        : Lex.Token_t;
  Tree         : Parse.Tree_t;
  Tree_Status  : Parse.Tree_Status_t;

  Failure     : exception;

  --
  -- Read one character from standard input.
  --

  procedure Read_Character
    (Item   : out Wide_Character;
     Status : out Lex.Stream_Status_t) is
  begin
    WIO.Get_Immediate
      (File => WIO.Current_Input,
       Item => Item);
    Status := Lex.Stream_OK;
  exception
    when WIO.End_Error =>
      Status := Lex.Stream_EOF;
    when others =>
      Status := Lex.Stream_Error;
  end Read_Character;

  procedure Get_Lexer_Token is new Lex.Get_Token
    (Read_Item => Read_Character);

  --
  -- Walk tree.
  --

  Indent : Natural := 0;

  procedure Handle_List_Open
   (List_ID : in     Parse.List_ID_t;
    Depth   : in     Parse.List_Depth_t;
    Status  :    out Walk.Walk_Status_t)
  is
    pragma Assert (List_ID'Valid);
    pragma Assert (Depth'Valid);
  begin
    if Depth > 1 then
      WIO.New_Line;
      WIO.Put (Fixed."*" (Indent, " ") & "(");
      Indent := Indent + 2;
    end if;
    Status := Walk.Walk_Continue;
  end Handle_List_Open;

  procedure Handle_Symbol
   (Name          : in     Parse.Node_Symbol_Name_t;
    List_ID       : in     Parse.List_ID_t;
    List_Position : in     Parse.List_Position_t;
    List_Length   : in     Parse.List_Length_t;
    Status        :    out Walk.Walk_Status_t)
  is
    pragma Assert (List_ID'Valid);
    pragma Assert (List_Position'Valid);
    pragma Assert (List_Length'Valid);
  begin
    WUIO.Put (Name);
    if Parse.List_Length_t (List_Position) /= List_Length then
      WIO.Put (" ");
    end if;
    Status := Walk.Walk_Continue;
  end Handle_Symbol;

  procedure Handle_String
    (Data          : in     Parse.Node_String_Data_t;
     List_ID       : in     Parse.List_ID_t;
     List_Position : in     Parse.List_Position_t;
     List_Length   : in     Parse.List_Length_t;
     Status        :    out Walk.Walk_Status_t)
  is
    pragma Assert (List_ID'Valid);
    pragma Assert (List_Position'Valid);
    pragma Assert (List_Length'Valid);
  begin
    WUIO.Put (Data);
    if Parse.List_Length_t (List_Position) /= List_Length then
      WIO.Put (" ");
    end if;
    Status := Walk.Walk_Continue;
  end Handle_String;

  procedure Handle_List_Close
   (List_ID : in     Parse.List_ID_t;
    Depth   : in     Parse.List_Depth_t;
    Status  :    out Walk.Walk_Status_t)
  is
    pragma Assert (List_ID'Valid);
    pragma Assert (Depth'Valid);
  begin
    if Depth > 1 then
      WIO.Put (")");
      WIO.New_Line;
      Indent := Indent - 2;
    end if;
    Status := Walk.Walk_Continue;
  end Handle_List_Close;

  procedure Dump_Tree is new Symbex.Walk.Walk_Tree
    (Handle_List_Open  => Handle_List_Open,
     Handle_Symbol     => Handle_Symbol,
     Handle_String     => Handle_String,
     Handle_List_Close => Handle_List_Close);

  Walk_Status : Walk.Walk_Status_t;

begin
  Done         := False;
  Lexer_Status := Lex.Lexer_OK;

  Lex.Initialize_Lexer
    (Lexer  => Lexer_State,
     Status => Lexer_Status);
  pragma Assert (Lexer_Status = Lex.Lexer_OK);

  Parse.Initialize_Tree
    (Tree   => Tree,
     Status => Tree_Status);
  pragma Assert (Tree_Status = Parse.Tree_OK);

  -- Parse loop.
  loop exit when Done;
    Get_Lexer_Token
      (Lexer     => Lexer_State,
       Token     => Token,
       Status    => Lexer_Status);
    if Lexer_Status in Lex.Lexer_Error_Status_t then
      raise Failure with Lex.Lexer_Error_Status_t'Image (Lexer_Status);
    else
      if Lexer_Status = Lex.Lexer_OK then
        if Token.Kind = Lex.Token_EOF then
          Done := True;
        end if;

        -- Consume token.
        Parse.Process_Token
          (Tree   => Tree,
           Token  => Token,
           Status => Tree_Status);
        if Tree_Status in Parse.Tree_Error_Status_t then
          raise Failure with Parse.Tree_Error_Status_t'Image (Tree_Status);
        end if;
      end if;
    end if;
  end loop;

  -- Dump tree.
  Dump_Tree
    (Tree   => Tree,
     Status => Walk_Status);
  pragma Assert (Walk_Status = Walk.Walk_Continue);

exception
  when E : Failure => IO.Put_Line ("error: " & Exceptions.Exception_Message (E));
end Dump;
