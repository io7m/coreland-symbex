with Ada.Strings.Wide_Unbounded;

package Symbex.Lex is

  type Lexer_t is private;

  --
  -- Token type.
  --

  type Token_Kind_t is
    (Token_Quoted_String,
     Token_Symbol,
     Token_List_Open,
     Token_List_Close);

  type Line_Number_t is new Positive;

  type Token_t is record
    Valid       : Boolean;
    Line_Number : Line_Number_t;
    Kind        : Token_Kind_t;
    Text        : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
  end record;

  --
  -- Token value for incomplete or invalid tokens.
  --

  Invalid_Token : constant Token_t;

  --
  -- Lexer status value.
  --

  type Lexer_Status_t is
    (Lexer_OK,
     Lexer_Needs_More_Data,
     Lexer_Error_Line_Overflow,
     Lexer_Error_Out_Of_Memory);

  --
  -- Status values corresponding to error conditions.
  --

  subtype Lexer_Error_Status_t is Lexer_Status_t
    range Lexer_Error_Line_Overflow .. Lexer_Error_Out_Of_Memory;

  --
  -- Lexer is initialized?
  --

  function Initialized
    (Lexer  : in     Lexer_t) return Boolean;

  --
  -- Initialize lexer state.
  --

  procedure Initialize_Lexer
    (Lexer  :    out Lexer_t;
     Status :    out Lexer_Status_t);
  pragma Postcondition
    (((Status  = Lexer_OK) and     Initialized (Lexer)) or
     ((Status /= Lexer_OK) and not Initialized (Lexer)));

  --
  -- Return token from 'stream' of characters.
  --

  procedure Get_Token
    (Lexer     : in out Lexer_t;
     Item      : in     Wide_Character;
     Item_Next : in     Wide_Character;
     Token     :    out Token_t;
     Status    :    out Lexer_Status_t);
  pragma Precondition (Initialized (Lexer));
  pragma Postcondition
    (((Status  = Lexer_OK) and (Token /= Invalid_Token)) or
     ((Status /= Lexer_OK) and (Token  = Invalid_Token)));

private
  package UBW_Strings renames Ada.Strings.Wide_Unbounded;

  --
  -- Lexer state machine.
  --

  type State_Stage_t is
    (Inside_String,
     Inside_Escape,
     Inside_Comment);

  type State_t is array (State_Stage_t) of Boolean;

  type Lexer_t is record
    Inited       : Boolean;
    Current_Line : Line_Number_t;
    Token_Buffer : UBW_Strings.Unbounded_Wide_String;
    State        : State_t;
  end record;

  --
  -- Token deferred.
  --

  Invalid_Token : constant Token_t :=
    Token_t'(Valid       => False,
             Line_Number => Line_Number_t'First,
             Text        => UBW_Strings.Null_Unbounded_Wide_String,
             Kind        => Token_Kind_t'First);

  --
  -- Character class.
  --

  type Character_Class_t is
    (Comment_Delimiter,
     Escape_Character,
     Line_Break,
     List_Close_Delimiter,
     List_Open_Delimiter,
     Ordinary_Text,
     String_Delimiter,
     Whitespace);

end Symbex.Lex;
