with PID;       use PID;

package body Inverter_ADC.Control is

   Is_Saturated : Boolean := False;

   -----------------------
   -- Calculate_Voltage --
   -----------------------

   procedure Calculate_Voltage (V_Setpoint : in Measure_E := Output_V;
                                V_Actual   : in Measure_E := Get_Sample(V_Output);
                                V_Max      : in Measure_E := Output_V_Range'Last;
                                Voltage    : out Measure_E) is

      Kp_Param : constant Float := 0.1;

      PID_V : Kpid := Compose (Kp => Kp_Param,
                               Ki => 0.0,
                               Kd => 0.0);
   begin

      PID_V.Update (Setpoint => V_Setpoint,
                    Actual   => V_Actual / Output_Relation,
                    Ts       => Nominal_Period,
                    Is_Sat   => Is_Saturated);
      
      Voltage := PID_V.Get_Output;

      Saturate (X       => Voltage,
                Maximum => V_Max,
                Is_Sat  => Is_Saturated);

   end Calculate_Voltage;

   ------------------
   -- Battery_Gain --
   ------------------

   --  Battery gain is 1.0 when battery voltage is minimum
   --  and 0.667 when battery voltage is maximum.


   function Battery_Gain
     (V_Setpoint : Battery_V_Range := Battery_V_Range'First;
      V_Actual   : Measure_E := Get_Sample (V_Battery))
   return Gain_Range is

   begin
      if (V_Actual / Battery_Relation < Battery_V_Range'First)
      then
         return 0.0;
      elsif (V_Actual / Battery_Relation > Battery_V_Range'Last)
      then
         return 1.0;
      else
         return V_Setpoint / V_Actual * Battery_Relation;
      end if;
   end Battery_Gain;

   -----------------
   -- Output_Gain --
   -----------------

   --  Output gain is 1.0 when AC output voltage is minimum
   --  and 0.818 when AC output voltage is maximum.

   function Output_Gain
     (V_Setpoint : Output_V_Range := Output_V_Range'First;
      V_Actual   : Measure_E := Get_Sample (V_Output))
   return Gain_Range is

   begin
      if (V_Actual / Output_Relation < Output_V_Range'First)
      then
         return 0.0;
      elsif (V_Actual / Output_Relation > Output_V_Range'Last)
      then
         return 1.0;
      else
         return V_Setpoint / V_Actual * Output_Relation;
      end if;
   end Output_Gain;

   ---------------------
   -- Output_PID_Gain --
   ---------------------

   --  Output gain is 1.0 when AC output voltage is minimum
   --  and 0.818 when AC output voltage is maximum.

   function Output_PID_Gain
     (V_Setpoint  : Output_V_Range := Output_V_Range'First;
      PID_Voltage : Measure_E) return Gain_Range is

   begin
      if (PID_Voltage < Output_V_Range'First)
      then
         return 0.0;
      elsif (PID_Voltage > Output_V_Range'Last)
      then
         return 1.0;
      else
         return V_Setpoint / PID_Voltage;
      end if;
   end Output_PID_Gain;

end Inverter_ADC.Control;
