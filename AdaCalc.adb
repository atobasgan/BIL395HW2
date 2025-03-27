{\rtf1\ansi\ansicpg1254\cocoartf2821
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 with Ada.Text_IO;         use Ada.Text_IO;\
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;\
with Ada.Strings;         use Ada.Strings;\
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;\
\
procedure Main is\
\
   -- Parantez d\uc0\u305 \u351 \u305 ndaki operat\'f6r\'fc bul\
   function Find_Operator(Expr : String; Op : Character) return Natural is\
      Depth : Integer := 0;\
   begin\
      -- Ters s\uc0\u305 rayla tarayarak en d\u305 \u351 taki (derinlik 0) operat\'f6r\'fc bul\
      for I in reverse Expr'Range loop\
         if Expr(I) = ')' then\
            Depth := Depth + 1;\
         elsif Expr(I) = '(' then\
            Depth := Depth - 1;\
         elsif Expr(I) = Op and then Depth = 0 then\
            return I;\
         end if;\
      end loop;\
      return 0;\
   end Find_Operator;\
\
   -- En d\uc0\u305 \u351 taki parantezleri temizle\
   function Trim_Parens(Expr : String) return String is\
      Cleaned : String := Trim(Expr, Both);\
   begin\
      -- Ba\uc0\u351 ta ve sonda parantez varsa k\u305 rp\
      while Cleaned'Length >= 2\
        and then Cleaned(Cleaned'First) = '('\
        and then Cleaned(Cleaned'Last)  = ')' loop\
         Cleaned := Trim(Cleaned(Cleaned'First + 1 .. Cleaned'Last - 1), Both);\
      end loop;\
      return Cleaned;\
   end Trim_Parens;\
\
   -- Bo\uc0\u351 luklar\u305  kald\u305 r\
   function Remove_Spaces(S : String) return String is\
      subtype Clean_String is String(1 .. S'Length);\
      Temp   : Clean_String;\
      Index  : Natural := 0;\
   begin\
      for I in S'Range loop\
         if S(I) /= ' ' then\
            Index := Index + 1;\
            Temp(Index) := S(I);\
         end if;\
      end loop;\
\
      if Index = 0 then\
         return "";\
      else\
         return Temp(1 .. Index);\
      end if;\
   end Remove_Spaces;\
\
   -- \uc0\u304 fadeyi hesapla\
   function Evaluate(Expr : String) return Integer is\
      Cleaned : constant String := Trim_Parens(Trim(Expr, Both));\
      Pos     : Natural;\
      L, R    : Integer;\
   begin\
      -- 1) Do\uc0\u287 rudan say\u305  m\u305 ?\
      begin\
         return Integer'Value(Cleaned);\
      exception\
         when others =>\
            null;  -- De\uc0\u287 ilse, devam et\
      end;\
\
      -- 2) Operat\'f6r \'f6nceli\uc0\u287 i (en d\'fc\u351 \'fckten y\'fckse\u287 e): + - * / ^\
      -- + i\uc0\u351 lemi\
      Pos := Find_Operator(Cleaned, '+');\
      if Pos /= 0 then\
         return Evaluate(Cleaned(1 .. Pos - 1))\
              + Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));\
      end if;\
\
      -- - i\uc0\u351 lemi\
      Pos := Find_Operator(Cleaned, '-');\
      if Pos /= 0 then\
         return Evaluate(Cleaned(1 .. Pos - 1))\
              - Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));\
      end if;\
\
      -- * i\uc0\u351 lemi\
      Pos := Find_Operator(Cleaned, '*');\
      if Pos /= 0 then\
         return Evaluate(Cleaned(1 .. Pos - 1))\
              * Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));\
      end if;\
\
      -- / i\uc0\u351 lemi\
      Pos := Find_Operator(Cleaned, '/');\
      if Pos /= 0 then\
         R := Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));\
         if R = 0 then\
            Put_Line("Hata: S\uc0\u305 f\u305 ra b\'f6lme!");\
            raise Constraint_Error;\
         end if;\
         return Evaluate(Cleaned(1 .. Pos - 1)) / R;\
      end if;\
\
      -- ^ i\uc0\u351 lemi (\'fcs)\
      Pos := Find_Operator(Cleaned, '^');\
      if Pos /= 0 then\
         L := Evaluate(Cleaned(1 .. Pos - 1));\
         R := Evaluate(Cleaned(Pos + 1 .. Cleaned'Last));\
         declare\
            Res : Integer := 1;\
         begin\
            for I in 1 .. R loop\
               Res := Res * L;\
            end loop;\
            return Res;\
         end;\
      end if;\
\
      -- E\uc0\u287 er hi\'e7bir durum uymad\u305 ysa ge\'e7ersiz ifade\
      raise Constraint_Error;\
   end Evaluate;\
\
   -- Kullan\uc0\u305 c\u305 dan giri\u351  almak i\'e7in de\u287 i\u351 kenler\
   Input : String(1 .. 100);\
   Len   : Natural;\
\
begin\
   Put_Line("Ada Hesap Makinesi");\
   Put_Line("Desteklenen i\uc0\u351 lemler: + - * / ^ (parantez desteklenir)");\
   Put_Line("\'c7\uc0\u305 kmak i\'e7in 'exit' yaz");\
\
   loop\
      Put(">> ");\
      Get_Line(Input, Len);\
      declare\
         Raw     : constant String := Input(1 .. Len);\
         Cleaned : constant String := Remove_Spaces(Raw);\
      begin\
         if Cleaned'Length = 0 then\
            Put_Line("Bo\uc0\u351  ifade!");\
            next;\
         end if;\
\
         exit when Cleaned = "exit";\
\
         declare\
            Result : Integer := Evaluate(Cleaned);\
         begin\
            -- Sonucu yazd\uc0\u305 r\
            Put("Sonu\'e7: ");\
            Put(Result);\
            New_Line;\
         exception\
            when Constraint_Error =>\
               Put_Line("Hatal\uc0\u305  ifade (Constraint_Error)!");\
            when others =>\
               Put_Line("Hatal\uc0\u305  ifade!");\
         end;\
      end;\
   end loop;\
end Main;\
}