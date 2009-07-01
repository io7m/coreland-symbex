with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Wide_Unbounded_IO;
with Symbex.Lex;

procedure Lexer is
  package Exceptions renames Ada.Exceptions;
  package IO         renames Ada.Text_IO;
  package Lex        renames Symbex.Lex;
  package WIO        renames Ada.Wide_Text_IO;
  package WUIO       renames Ada.Wide_Text_IO.Wide_Unbounded_IO;

  use type Lex.Lexer_Status_t;
  use type Lex.Lexer_t;
  use type Lex.Token_Kind_t;

  Done        : Boolean;
  Lexer_State : Lex.Lexer_t;
  Status      : Lex.Lexer_Status_t;
  Token       : Lex.Token_t;

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

  procedure Get_Token is new Lex.Get_Token
    (Read_Item => Read_Character);

begin
  Done   := False;
  Status := Lex.Lexer_OK;

  Lex.Initialize_Lexer
    (Lexer  => Lexer_State,
     Status => Status);
  pragma Assert (Status = Lex.Lexer_OK);

  loop exit when Done;
    Get_Token
      (Lexer     => Lexer_State,
       Token     => Token,
       Status    => Status);
    if Status in Lex.Lexer_Error_Status_t then
      raise Failure with Lex.Lexer_Error_Status_t'Image (Status);
    else
      if Status = Lex.Lexer_OK then
        WIO.Put
          (File => WIO.Current_Output,
           Item => Lex.Token_Kind_t'Wide_Image (Token.Kind));
        WIO.Put
          (File => WIO.Current_Output,
           Item => ":");
        WUIO.Put_Line
          (File => WIO.Current_Output,
           Item => Token.Text);
        if Token.Kind = Lex.Token_EOF then
          Done := True;
        end if;
      end if;
    end if;
  end loop;

exception
  when E : Failure => IO.Put_Line ("error: " & Exceptions.Exception_Message (E));
end Lexer;
