with Ada.Wide_Characters.Unicode; -- GNAT Specific.

-- with Ada.Text_IO;

package body Symbex.Lex is
  package Unicode renames Ada.Wide_Characters.Unicode;

  --
  -- Private subprograms.
  --

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

  --
  -- Return true if internal token buffer contains data.
  --

  function Token_Is_Nonzero_Length
    (Lexer : in Lexer_t) return Boolean is
  begin
--    Ada.Text_IO.Put_Line ("length: " & Natural'Image (UBW_Strings.Length (Lexer.Token_Buffer)));
    return UBW_Strings.Length (Lexer.Token_Buffer) /= 0;
  end Token_Is_Nonzero_Length;

  --
  -- Append item to internal token buffer.
  --

  procedure Append_To_Token
    (Lexer : in out Lexer_t;
     Item  : in     Wide_Character) is
  begin
    UBW_Strings.Append (Lexer.Token_Buffer, Item);
  end Append_To_Token;

  --
  -- Finish token in buffer and assign kind Kind.
  --

  procedure Complete_Token
    (Lexer : in out Lexer_t;
     Kind  : in     Token_Kind_t;
     Token :    out Token_t) is
  begin
    Token := Token_t'
      (Is_Valid    => True,
       Line_Number => Lexer.Current_Line,
       Text        => Lexer.Token_Buffer,
       Kind        => Kind);
    Lexer.Token_Buffer := UBW_Strings.Null_Unbounded_Wide_String;
  end Complete_Token;

  --
  -- Categorize Item into character class.
  --

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

  --
  -- Public API.
  --

  function Initialized (Lexer : in Lexer_t) return Boolean is
  begin
    return Lexer.Inited;
  end Initialized;

  --
  -- Initialize the lexer.
  --

  procedure Initialize_Lexer
    (Lexer  :    out Lexer_t;
     Status :    out Lexer_Status_t) is
  begin
    Lexer := Lexer_t'
      (Inited       => True,
       Current_Line => Line_Number_t'First,
       Token_Buffer => UBW_Strings.Null_Unbounded_Wide_String,
       Input_Buffer => Input_Buffer_t'(others => Wide_Character'Val (0)),
       State        => State_t'(others => False));
    Status       := Lexer_OK;
  end Initialize_Lexer;

  --
  -- Retrieve token from input stream.
  --

  procedure Get_Token
    (Lexer     : in out Lexer_t;
     Token     :    out Token_t;
     Status    :    out Lexer_Status_t)
  is
    --
    -- Stream characters via Read_Item.
    --
    procedure Fetch_Characters
      (Lexer     : in out Lexer_t;
       Item      :    out Wide_Character;
       Item_Next :    out Wide_Character;
       Status    :    out Stream_Status_t)
    is
      Null_Item : constant Wide_Character := Wide_Character'Val (0);
    begin
      if Lexer.Input_Buffer (Next) /= Null_Item then
        Lexer.Input_Buffer (Current) := Lexer.Input_Buffer (Next);
        Read_Item
          (Item   => Lexer.Input_Buffer (Next),
           Status => Status);
        case Status is
          when Stream_EOF   =>
            Lexer.Input_Buffer (Next) := Null_Item;
            Status                    := Stream_OK;
          when Stream_Error => null;
          when Stream_OK    => null;
        end case;
      else
        Read_Item
          (Item   => Lexer.Input_Buffer (Current),
           Status => Status);
        case Status is
          when Stream_EOF   => null;
          when Stream_Error => null;
          when Stream_OK    =>
            Read_Item
              (Item   => Lexer.Input_Buffer (Next),
               Status => Status);
            case Status is
              when Stream_EOF   =>
                Lexer.Input_Buffer (Next) := Null_Item;
                Status                    := Stream_OK;
              when Stream_Error => null;
              when Stream_OK    => null;
            end case;
        end case;
      end if;

      Item      := Lexer.Input_Buffer (Current);
      Item_Next := Lexer.Input_Buffer (Next);
    end Fetch_Characters;

    Item          : Wide_Character;
    Item_Next     : Wide_Character;
    Stream_Status : Stream_Status_t;
  begin
    -- Default status.
    Status := Lexer_Needs_More_Data;
    Token  := Invalid_Token;

    -- Fetch characters from input stream.
    Fetch_Characters
      (Lexer     => Lexer,
       Item      => Item,
       Item_Next => Item_Next,
       Status    => Stream_Status);
    case Stream_Status is
      when Stream_OK    => null;
      when Stream_Error =>
        Status := Lexer_Error_Stream_Error;
        return;
      when Stream_EOF   =>
        if State_Is_Set (Lexer, Inside_String) or
           State_Is_Set (Lexer, Inside_Escape) then
          Status := Lexer_Error_Early_EOF;
        else
          -- Reached upon EOF with no preceding newline/whitespace.
          Status := Lexer_OK;

          -- Have unfinished token?
          if Token_Is_Nonzero_Length (Lexer) then
            Complete_Token
              (Lexer => Lexer,
               Token => Token,
               Kind  => Token_Symbol);
          else
            Append_To_Token (Lexer, 'E');
            Complete_Token
              (Lexer => Lexer,
               Token => Token,
               Kind  => Token_EOF);
          end if;
        end if;
        return;
    end case;

    pragma Assert (Stream_Status = Stream_OK);

    -- Process character.
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
            when List_Open_Delimiter | List_Close_Delimiter | String_Delimiter =>
              if not State_Is_Set (Lexer, Inside_String) then
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
  end Get_Token;

end Symbex.Lex;
