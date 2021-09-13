package body PID is

   ---------
   -- Max --
   ---------

   function Max (X, Y : in Float) return Float is
   begin
      if X > Y then
         return X;
      end if;

      return Y;
   end Max;

   ---------
   -- Min --
   ---------

   function Min (X, Y : in Float) return Float is
   begin
      if X < Y then
         return X;
      end if;

      return Y;
   end Min;

   --------------
   -- Saturate --
   --------------

   procedure Saturate (X       : in out Float;
                       Maximum : in Float;
                       Is_Sat  : out Boolean)
   is
      Scaling : Float;
   begin
      Is_Sat := X > Maximum;

      if Is_Sat then
         Scaling := Maximum / Max (X, Float'Succ (0.0));
         X := Scaling * X;
      end if;

   end Saturate;

   --------------
   -- Saturate --
   --------------

   function Saturate (X       : in Float;
                      Maximum : in Float;
                      Minimum : in Float)
                      return Float
   is
      (Max (Min (X, Maximum), Minimum));

   -------------
   -- Compose --
   -------------

   function Compose (Kp, Ki, Kd : in Float) return Kpid is
     (Kpid'(Kp       => Kp,
            Ki       => Ki,
            Kd       => Kd,
            Integral => 0.0,
            E_Prev   => 0.0,
            Output   => 0.0));

   ------------
   -- Update --
   ------------

   procedure Update (This     : in out Kpid;
                     Setpoint : in Float;
                     Actual   : in Float;
                     Ts       : in Float;
                     Is_Sat   : in Boolean := False)
   is
      E : Float := Setpoint - Actual;
      D_Term : constant Float := (E - This.E_Prev) / Ts;
   begin

      --  Anti-windup
      if Is_Sat then
         if This.Integral < 0.0 then
            --  Only allow integral to increase
            E := Max (E, 0.0);
         else
            --  Only allow integral to decrease
            E := Min (E, 0.0);
         end if;
      end if;

      This.Integral := This.Integral + E * This.Ki * Ts;

      This.Output := This.Kp * E + This.Integral + This.Kd * D_Term;

   end Update;

   ----------------
   -- Get_Output --
   ----------------

   function Get_Output (This : in Kpid) return Float is
   begin
      return This.Output;
   end Get_Output;

end PID;