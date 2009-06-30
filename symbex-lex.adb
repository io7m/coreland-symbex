with Ada.Wide_Characters.Unicode; -- GNAT Specific.

package body Symbex.Lex is
  package Unicode renames Ada.Wide_Characters.Unicode;

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
    (Lexer : in Lexer_t;
     State : in State_Stage_t) return Boolean;

  function Token_Is_Nonzero_Length
    (Lexer : in Lexer_t) return Boolean;

  procedure Complete_Token
    (Lexer : in out Lexer_t;
     Kind  : in     Token_Kind_t;
     Token :    out Token_t);
  pragma Precondition (Token_Is_Nonzero_Length (Lexer));
  pragma Postcondition
    ((Token /= Invalid_Token) and (not Token_Is_Nonzero_Length (Lexer)));

  --
  -- Implementations.
  --

  function Initialized (Lexer : in Lexer_t) return Boolean is
  begin
    return Lexer.Inited;
  end Initialized;

  procedure Initialize_Lexer
    (Lexer  :    out Lexer_t;
     Status :    out Lexer_Status_t) is
  begin
    Lexer := Lexer_t'
      (Inited       => True,
       Current_Line => Line_Number_t'First,
       Token_Buffer => UBW_Strings.Null_Unbounded_Wide_String,
       State        => State_t'(others => False));
    Status       := Lexer_OK;
  end Initialize_Lexer;

  procedure Append_To_Token
    (Lexer : in out Lexer_t;
     Item  : in     Wide_Character) is
  begin
    UBW_Strings.Append (Lexer.Token_Buffer, Item);
  end Append_To_Token;

  procedure Set_State
    (Lexer : in out Lexer_t;
     State : in     State_Stage_t) is
  begin
    Lexer.State (State) := True;
  end Set_State;

  procedure Unset_State
    (Lexer : in out Lexer_t;
     State : in     State_Stage_t) is
  begin
    Lexer.State (State) := False;
  end Unset_State;

  function State_Is_Set
    (Lexer : in Lexer_t;
     State : in State_Stage_t) return Boolean is
  begin
    return Lexer.State (State);
  end State_Is_Set;

  function Token_Is_Nonzero_Length
    (Lexer : in Lexer_t) return Boolean is
  begin
    return UBW_Strings.Length (Lexer.Token_Buffer) /= 0;
  end Token_Is_Nonzero_Length;

  procedure Complete_Token
    (Lexer : in out Lexer_t;
     Kind  : in     Token_Kind_t;
     Token :    out Token_t) is
  begin
    Token := Token_t'
      (Valid       => True,
       Line_Number => Lexer.Current_Line,
       Text        => Lexer.Token_Buffer,
       Kind        => Kind);
    Lexer.Token_Buffer := UBW_Strings.Null_Unbounded_Wide_String;
  end Complete_Token;

  function Categorize_Character
    (Item : in Wide_Character) return Character_Class_t
  is
    Class : Character_Class_t := Ordinary_Text;
  begin
    case Item is
      when '('    => Class := List_Open_Delimiter;
      when ')'    => Class := List_Close_Delimiter;
      when '"'    => Class := String_Delimiter;
      when ';'    => Class := Comment_Delimiter;
      when '\'    => Class := Escape_Character;
      when others =>
        if Unicode.Is_Line_Terminator (Item) then
          Class := Line_Break;
        else
          if Unicode.Is_Space (Item) then
            Class := Whitespace;
          end if;
        end if;
    end case;
    return Class;
  end Categorize_Character;

  procedure Consume_Characters
    (Lexer     : in out Lexer_t;
     Item      : in     Wide_Character;
     Item_Next : in     Wide_Character;
     Token     :    out Token_t;
     Status    :    out Lexer_Status_t) is
  begin
    -- Default status.
    Status := Lexer_Needs_More_Data;
    Token  := Invalid_Token;

    case Categorize_Character (Item) is
      when Comment_Delimiter =>
        if State_Is_Set (Lexer, Inside_String) or
           State_Is_Set (Lexer, Inside_Escape) then
          Append_To_Token (Lexer, Item);
        else
          if not State_Is_Set (Lexer, Inside_Comment) then
            Set_State (Lexer, Inside_Comment);
          end if;
        end if;

      when Escape_Character =>
        if not State_Is_Set (Lexer, Inside_Comment) then
          if State_Is_Set (Lexer, Inside_String) then
            if State_Is_Set (Lexer, Inside_Escape) then
              Append_To_Token (Lexer, Item);
              Unset_State (Lexer, Inside_Escape);
            else
              Set_State (Lexer, Inside_Escape);
            end if;
          end if;
        end if;

      when Line_Break =>
        if State_Is_Set (Lexer, Inside_Comment) then
          Unset_State (Lexer, Inside_Comment);
        else
          begin
            -- Potential overflow.
            Lexer.Current_Line := Lexer.Current_Line + 1;

            if State_Is_Set (Lexer, Inside_Escape) then
              Unset_State (Lexer, Inside_Escape);
            end if;
            if State_Is_Set (Lexer, Inside_String) then
              Append_To_Token (Lexer, Item);
            else
              if Token_Is_Nonzero_Length (Lexer) then
                Status := Lexer_OK;
                Complete_Token
                  (Lexer => Lexer,
                   Token => Token,
                   Kind  => Token_Symbol);
              end if;
            end if;
          exception
            when Constraint_Error =>
              Status := Lexer_Error_Line_Overflow;
              Token  := Invalid_Token;
          end;
        end if;

      when List_Open_Delimiter =>
        if not State_Is_Set (Lexer, Inside_Comment) then
          Append_To_Token (Lexer, Item);
          if not State_Is_Set (Lexer, Inside_String) then
            Status := Lexer_OK;
            Complete_Token
              (Lexer => Lexer,
               Token => Token,
                Kind => Token_List_Open);
          end if;
        end if;

      when List_Close_Delimiter =>
        if not State_Is_Set (Lexer, Inside_Comment) then
          Append_To_Token (Lexer, Item);
          if not State_Is_Set (Lexer, Inside_String) then
            Status := Lexer_OK;
            Complete_Token
              (Lexer => Lexer,
               Token => Token,
               Kind  => Token_List_Close);
          end if;
        end if;

      when String_Delimiter =>
        if not State_Is_Set (Lexer, Inside_Comment) then
          if State_Is_Set (Lexer, Inside_String) then
            if State_Is_Set (Lexer, Inside_Escape) then
              Append_To_Token (Lexer, Item);
              Unset_State (Lexer, Inside_Escape);
            else
              Status := Lexer_OK;
              Complete_Token
                (Lexer => Lexer,
                 Token => Token,
                 Kind  => Token_Quoted_String);
              Unset_State (Lexer, Inside_String);
            end if;
          else
            Set_State (Lexer, Inside_String);
          end if;
        end if;

      when Whitespace =>
        if not State_Is_Set (Lexer, Inside_Comment) then
          if State_Is_Set (Lexer, Inside_Escape) then
            Unset_State (Lexer, Inside_Escape);
          end if;
          if State_Is_Set (Lexer, Inside_String) then
            Append_To_Token (Lexer, Item);
          else
            if Token_Is_Nonzero_Length (Lexer) then
              Status := Lexer_OK;
              Complete_Token
                (Lexer => Lexer,
                 Token => Token,
                 Kind  => Token_Symbol);
            end if;
          end if;
        end if;

      when Ordinary_Text =>
        if not State_Is_Set (Lexer, Inside_Comment) then
          if State_Is_Set (Lexer, Inside_Escape) then
            Unset_State (Lexer, Inside_Escape);
          end if;
          Append_To_Token (Lexer, Item);

          -- End of token if not inside a string.
          case Categorize_Character (Item_Next) is
            when List_Open_Delimiter  | List_Close_Delimiter | String_Delimiter =>
              if State_Is_Set (Lexer, Inside_String) then
                Status := Lexer_OK;
                Complete_Token
                  (Lexer => Lexer,
                   Token => Token,
                   Kind  => Token_Symbol);
              end if;
            when others => null;
          end case;
        end if;
    end case;

  exception
    when Storage_Error =>
      Status := Lexer_Error_Out_Of_Memory;
      Token  := Invalid_Token;
  end Consume_Characters;

end Symbex.Lex;
