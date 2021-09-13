with STM32.Device;         use STM32.Device;

with Inverter_ADC.Control; use Inverter_ADC.Control;
with Inverter_PWM;         use Inverter_PWM;

package body Inverter_ADC is

   function To_Voltage (Value : in UInt16) return Measure_E
   with
      Inline;

   ----------------------
   -- Initialize_ADC --
   ----------------------

   procedure Initialize_ADC is

      All_Regular_Conversions : constant Regular_Channel_Conversions :=
        (1 => (Channel     => ADC_Battery_V_Point.Channel,
               Sample_Time => Sample_480_Cycles),
         2 => (Channel     => ADC_Battery_I_Point.Channel,
               Sample_Time => Sample_480_Cycles),
         3 => (Channel     => ADC_Output_V_Point.Channel,
               Sample_Time => Sample_480_Cycles));
      
   begin

      --  Initialize GPIO for analog input
      for Reading of ADC_Reading_Settings loop
         STM32.Device.Enable_Clock (Reading.GPIO_Entry);
         Reading.GPIO_Entry.Configure_IO
           (Config => GPIO_Port_Configuration'(Mode => Mode_Analog, others => <>));
      end loop;

      --  Initialize ADC mode
      Enable_Clock (Sensor_ADC.all);
      Disable (Sensor_ADC.all);

      Configure_Unit
        (Sensor_ADC.all,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      --  Conversions are triggered by Sensor Timer.
      Configure_Regular_Conversions
        (Sensor_ADC.all,
         Continuous  => False,  -- False is ESSENTIAL when externally triggered!
         Trigger     => (Trigger_Rising_Edge, Event => Sensor_Trigger_Event),
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);

      Configure_Common_Properties
        (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);  -- arbitrary

      --  Finally, enable the used ADCs
      Enable_Interrupts (Sensor_ADC.all, Regular_Channel_Conversion_Complete);
      --  Each conversion generates an interrupt signalling conversion complete.
      Enable (Sensor_ADC.all);

      Initialized := True;
   End Initialize_ADC;

   ----------------
   -- To_Voltage --
   ----------------

   function To_Voltage (Value : in UInt16) return Measure_E is
   begin
      return Measure_E (ADC_V_Per_Lsb * Float (Value));
   end To_Voltage;

   ----------------
   -- Get_Sample --
   ----------------

   function Get_Sample (Reading : in ADC_Reading)
      return Measure_E is
   begin
      if Reading'Valid then
         return To_Voltage (Regular_Samples(Reading));
      else
         return 0.0;
      end if;
   end Get_Sample;

   --------------------
   -- Test_V_Battery --
   --------------------

   function Test_V_Battery return Boolean is
      V_Actual : constant Measure_E := Get_Sample (Reading => V_Battery);
   begin
      if (V_Actual / Battery_Relation < Battery_V_Range'First or
          V_Actual / Battery_Relation > Battery_V_Range'Last)
      then
         return False;
      else
         return True;
      end if;
   end Test_V_Battery;

   --------------------
   -- Test_I_Battery --
   --------------------

   function Test_I_Battery return Boolean is
      V_Actual : constant Measure_E := Get_Sample (Reading => I_Battery);
   begin
      if V_Actual > Battery_I_Range'Last then
         return False;
      else
         return True;
      end if;
   end Test_I_Battery;

   -------------------
   -- Test_V_Output --
   -------------------

   function Test_V_Output return Boolean is
      V_Actual : constant Measure_E := Get_Sample (Reading => V_Output);
   begin
      if (V_Actual / Output_Relation < Output_V_Range'First or V_Actual > Output_V_Range'Last) then
         return False;
      else
         return True;
      end if;
   end Test_V_Output;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized
      return Boolean is (Initialized);

   --------------------
   -- Sensor Handler --
   --------------------

   protected body Sensor_Handler is

      -------------------------
      -- Get_Regular_Samples --
      -------------------------

      function Get_Regular_Samples return Regular_Samples_Array is
      begin
         return Regular_Samples;
      end Get_Regular_Samples;

      -----------------------
      -- Await_New_Samples --
      -----------------------

      entry Await_New_Samples (Sensor_Samples : out Regular_Samples_Array)
        when New_Samples is

      begin
         for R in ADC_Reading'Range loop
            Sensor_Samples(R) :=
              Conversion_Value (ADC_Reading_Settings(R).ADC_Entry.ADC.all);
         end loop;

         New_Samples := False;
      end Await_New_Samples;

      ------------------------
      -- Sensor_ISR_Handler --
      ------------------------

      procedure Sensor_ISR_Handler is
         Out_V : Measure_E;
      begin
         if Status (Sensor_ADC.all, Regular_Channel_Conversion_Complete) then
            if Interrupt_Enabled (Sensor_ADC.all, Regular_Channel_Conversion_Complete) then
               Clear_Interrupt_Pending (Sensor_ADC.all, Regular_Channel_Conversion_Complete);
               
               --  Save the ADC values into a buffer
               for R in ADC_Reading'Range loop
                  Regular_Samples(R) := Conversion_Value
                                         (ADC_Reading_Settings(R).ADC_Entry.ADC.all);
               end loop;

               --  Calculate the new Sine_Gain based on battery voltage
               Sine_Gain := Battery_Gain;

               if (Output_Loop_Gain) then
                  --  Calculate the new Sine_Gain based on output voltages
                  Calculate_Voltage (V_Setpoint => Output_V,
                                     V_Actual   => Get_Sample (V_Output),
                                     V_Max      => Output_V_Range'Last,
                                     Voltage    => Out_V);

                  Sine_Gain := Sine_Gain * Output_PID_Gain (PID_Voltage => Out_V);
               end if;

               New_Samples := True;

               --  Testing the 5 kHz output with 1 Hz LED blinking. Because there are three
               --  regular channel conversions, this frequency will be three times greater.
               if Counter = 2_500 then
                  Set_Toggle (Blue_LED);
                  Counter := 0;
               end if;
               Counter := Counter + 1;

            end if;
         end if;
      end Sensor_ISR_Handler;

   end Sensor_Handler;

end Inverter_ADC;
