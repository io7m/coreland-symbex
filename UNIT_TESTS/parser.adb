with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Wide_Unbounded_IO;
with Symbex.Lex;
with Symbex.Parse;

procedure Parser is
  package Exceptions renames Ada.Exceptions;
  package IO         renames Ada.Text_IO;
  package Lex        renames Symbex.Lex;
  package Parse      renames Symbex.Parse;
  package WIO        renames Ada.Wide_Text_IO;
  package WUIO       renames Ada.Wide_Text_IO.Wide_Unbounded_IO;

  use type Lex.Lexer_Status_t;
  use type Lex.Lexer_t;
  use type Lex.Token_Kind_t;
  use type Parse.Tree_Status_t;

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

exception
  when E : Failure => IO.Put_Line ("error: " & Exceptions.Exception_Message (E));
end Parser;
