with Inverter_PWM; use Inverter_PWM;

package Inverter_ADC.Control is

   Output_Loop_Gain : Boolean := False;
   --  Choose whether use or not the PID lopback control

   Nominal_Period : constant Float := 1.0 / PWM_Frequency_Hz;

   procedure Calculate_Voltage
        (V_Setpoint : Measure_E := Output_V; --  Setpoint voltage
         V_Actual   : Measure_E := Get_Sample(V_Output); --  Actual voltage
         V_Max      : in Measure_E := Output_V_Range'Last; --  Maximum voltage
         Voltage    : out Measure_E);
   --  Calculates the new output voltage using a PID.

   function Battery_Gain
     (V_Setpoint : Battery_V_Range := Battery_V_Range'First;
      V_Actual   : Measure_E := Get_Sample (V_Battery)) return Gain_Range;
   --  Calculate the gain of the sinusoid as a function of the
   --  battery voltage.

   function Output_Gain
     (V_Setpoint : Output_V_Range := Output_V_Range'First;
      V_Actual   : Measure_E := Get_Sample (V_Output)) return Gain_Range;
   --  Calculate the gain of the sinusoid as a function of the
   --  output voltage.

   function Output_PID_Gain
     (V_Setpoint  : Output_V_Range := Output_V_Range'First;
      PID_Voltage : Measure_E) return Gain_Range;
   --  Calculate the gain of the sinusoid as a function of the
   --  output voltage using PID.

end Inverter_ADC.Control;
