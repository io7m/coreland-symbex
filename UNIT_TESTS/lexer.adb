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

  use type Lex.Lexer_t;
  use type Lex.Lexer_Status_t;

  Done        : Boolean;
  Item_Next   : Wide_Character;
  Item        : Wide_Character;
  Lexer_State : Lex.Lexer_t;
  Status      : Lex.Lexer_Status_t;
  Token       : Lex.Token_t;

  Failure     : exception;
begin
  Done      := False;
  Status    := Lex.Lexer_OK;

  Lex.Initialize_Lexer
    (Lexer  => Lexer_State,
     Status => Status);
  pragma Assert (Status = Lex.Lexer_OK);

  -- Read initial character.
  begin
    WIO.Get_Immediate
      (File => WIO.Current_Input,
       Item => Item_Next);
  exception
    when WIO.End_Error => Done := True;
  end;

  loop exit when Done;
    begin
      Item := Item_Next;

      WIO.Get_Immediate
        (File => WIO.Current_Input,
         Item => Item_Next);

      Lex.Get_Token
        (Lexer     => Lexer_State,
         Item      => Item,
         Item_Next => Item_Next,
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
        end if;
      end if;
    exception
      when WIO.End_Error =>
        if Status = Lex.Lexer_Needs_More_Data then
          raise Failure with Lex.Lexer_Error_Status_t'Image (Status);
        end if;
        Done := True;
    end;
  end loop;

exception
  when E : Failure => IO.Put_Line ("error: " & Exceptions.Exception_Message (E));
end Lexer;
