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
     Token_List_Close,
     Token_EOF);

  type Line_Number_t is new Positive;

  type Token_t is record
    Is_Valid    : Boolean;
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
     Lexer_Error_Out_Of_Memory,
     Lexer_Error_Stream_Error,
     Lexer_Error_Early_EOF);

  --
  -- Status values corresponding to error conditions.
  --

  subtype Lexer_Error_Status_t is Lexer_Status_t
    range Lexer_Error_Line_Overflow .. Lexer_Status_t'Last;

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
--  pragma Postcondition
--    (((Status  = Lexer_OK) and     Initialized (Lexer)) or
--     ((Status /= Lexer_OK) and not Initialized (Lexer)));

  --
  -- Return token from Read_Item 'stream'.
  --

  type Stream_Status_t is
    (Stream_OK,
     Stream_EOF,
     Stream_Error);

  generic
    with procedure Read_Item
      (Item   :     out Wide_Character;
       Status :     out Stream_Status_t);

  procedure Get_Token
    (Lexer     : in out Lexer_t;
     Token     :    out Token_t;
     Status    :    out Lexer_Status_t);
--  pragma Precondition (Initialized (Lexer));
--  pragma Postcondition
--    (((Status  = Lexer_OK) and (Token /= Invalid_Token)) or
--     ((Status /= Lexer_OK) and (Token  = Invalid_Token)));

private
  package UBW_Strings renames Ada.Strings.Wide_Unbounded;

  --
  -- Lexer state machine.
  --

  type State_Stage_t is
    (Inside_String,
     Inside_Escape,
     Inside_Comment);

  type State_t                 is array (State_Stage_t) of Boolean;
  type Input_Buffer_Position_t is (Current, Next);
  type Input_Buffer_t          is array (Input_Buffer_Position_t) of Wide_Character;
  subtype Token_Buffer_t       is UBW_Strings.Unbounded_Wide_String;

  type Lexer_t is record
    Inited       : Boolean;
    Current_Line : Line_Number_t;
    Token_Buffer : Token_Buffer_t;
    Input_Buffer : Input_Buffer_t;
    State        : State_t;
  end record;

  --
  -- Token deferred.
  --

  Invalid_Token : constant Token_t :=
    Token_t'(Is_Valid    => False,
             Line_Number => Line_Number_t'First,
             Text        => <>,
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

  --
  -- Utility subprograms.
  --

  procedure Set_State
    (Lexer : in out Lexer_t;
     State : in     State_Stage_t);
  pragma Precondition (not Lexer.State (State));
  pragma Postcondition (Lexer.State (State));

  procedure Unset_State
    (Lexer : in out Lexer_t;
     State : in     State_Stage_t);
  pragma Precondition (Lexer.State (State));
  pragma Postcondition (not Lexer.State (State));

  function State_Is_Set
    (Lexer : in     Lexer_t;
     State : in     State_Stage_t) return Boolean;

  function Token_Is_Nonzero_Length
    (Lexer : in Lexer_t) return Boolean;

  procedure Complete_Token
    (Lexer : in out Lexer_t;
     Kind  : in     Token_Kind_t;
     Token :    out Token_t);
  pragma Precondition (Token_Is_Nonzero_Length (Lexer));
--  pragma Postcondition
--    ((Token /= Invalid_Token) and (not Token_Is_Nonzero_Length (Lexer)));

end Symbex.Lex;
