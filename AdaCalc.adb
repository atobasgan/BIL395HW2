with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;

procedure Main is

   -- Parantez dışındaki operatörü bul
   function Find_Operator(Expr : String; Op : Character) return Natural is
      Depth : Integer := 0;
   begin
      for I in reverse Expr'Range loop
         if Expr(I) = ')' then
            Depth := Depth + 1;
         elsif Expr(I) = '(' then
            Depth := Depth - 1;
         elsif Expr(I) = Op and then Depth = 0 then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Operator;

   -- En dıştaki parantezleri temizle
   function Trim_Parens(Expr : String) return String is
      Cleaned : String := Trim(Expr, Both);
   begin
      while Cleaned'Length >= 2
        and then Cleaned(Cleaned'First) = '('
        and then Cleaned(Cleaned'Last)  = ')' loop
         Cleaned := Trim(Cleaned(Cleaned'First + 1 .. Cleaned'Last - 1), Both);
      end loop;
      return Cleaned;
   end Trim_Parens;

   -- Boşlukları kaldır
   function Remove_Spaces(S : String) return String is
      subtype Clean_String is String(1 .. S'Length);
      Temp   : Clean_String;
      Index  : Natural := 0;
   begin
      for I in S'Range loop
         if S(I) /= ' ' then
            Index := Index + 1;
            Temp(Index) := S(I);
         end if;
      end loop;

      if Index = 0 then
         return "";
      else
         return Temp(1 .. Index);
      end if;
   end Remove_Spaces;

   -- İfadeyi hesapla
   function Evaluate(Expr : String) return Integer is
      Cleaned : constant String := Trim_Parens(Trim(Expr, Both));
      Pos     : Natural;
      L, R    : Integer;
   begin
      -- Sayıya direkt çevrilebiliyor mu?
      begin
         return Integer'Value(Cleaned);
      exception
         when others =>
            null;
      end;

      -- Operatör önceliğine göre sırayla çözümle
      Pos := Find_Operator(Cleaned, '+');
      if Pos /= 0 then
         return Evaluate(Cleaned(1 .. Pos - 1))
              + Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));
      end if;

      Pos := Find_Operator(Cleaned, '-');
      if Pos /= 0 then
         return Evaluate(Cleaned(1 .. Pos - 1))
              - Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));
      end if;

      Pos := Find_Operator(Cleaned, '*');
      if Pos /= 0 then
         return Evaluate(Cleaned(1 .. Pos - 1))
              * Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));
      end if;

      Pos := Find_Operator(Cleaned, '/');
      if Pos /= 0 then
         R := Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));
         if R = 0 then
            Put_Line("Hata: Sıfıra bölme!");
            raise Constraint_Error;
         end if;
         return Evaluate(Cleaned(1 .. Pos - 1)) / R;
      end if;

      Pos := Find_Operator(Cleaned, '^');
      if Pos /= 0 then
         L := Evaluate(Cleaned(1 .. Pos - 1));
         R := Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));
         declare
            Res : Integer := 1;
         begin
            for I in 1 .. R loop
               Res := Res * L;
            end loop;
            return Res;
         end;
      end if;

      raise Constraint_Error;
   end Evaluate;

   -- Girdi almak için değişkenler
   Input : String(1 .. 100);
   Len   : Natural;

begin
   Put_Line("Ada Hesap Makinesi");
   Put_Line("Desteklenen işlemler: + - * / ^ (parantez desteklenir)");
   Put_Line("Çıkmak için 'exit' yaz");

   loop
      Put(">> ");
      Get_Line(Input, Len);
      declare
         Raw     : constant String := Input(1 .. Len);
         Cleaned : constant String := Remove_Spaces(Raw);
      begin
         if Cleaned'Length = 0 then
            Put_Line("Boş ifade!");
            next;
         end if;

         exit when Cleaned = "exit";

         declare
            Result : Integer := Evaluate(Cleaned);
         begin
            Put("Sonuç: ");
            Put(Result);
            New_Line;
         exception
            when Constraint_Error =>
               Put_Line("Hatalı ifade (Constraint_Error)!");
            when others =>
               Put_Line("Hatalı ifade!");
         end;
      end;
   end loop;
end Main;
