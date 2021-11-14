
--  This demonstration illustrates the use of PWM to control the brightness of
--  an LED. The effect is to make the LED increase and decrease in brightness,
--  iteratively, for as long as the application runs. In effect the LED light
--  waxes and wanes as function of a PWM sine wave generated with the CORDIC
--  coprocessor.
--
--  The demo uses an abstract data type PWM_Modulator to control the power to
--  the LED via pulse-width-modulation. A timer is still used underneath, but
--  the details are hidden.  For direct use of the timer see the other demo.

with HAL;          use HAL;

with STM32.Device; use STM32.Device;
with STM32.Timers; use STM32.Timers;
with STM32.PWM;    use STM32.PWM;
with STM32.CORDIC; use STM32.CORDIC;

with STM_Board;    use STM_Board;

with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

procedure Test_LED_PWM is

   --  The LD2 green led at GPIO PA5 is the TIM2_CH1 timer output when
   --  programmed as AF1 alternate function.
   Selected_Timer : Timer renames Timer_2;
   Timer_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM2_1;
   Output_Channel : constant Timer_Channel := Channel_1;
   Requested_Frequency : constant Hertz := 25_000;
   Power_Control : PWM_Modulator;

begin

   Configure_PWM_Timer (Selected_Timer'Access, Requested_Frequency);

   Power_Control.Attach_PWM_Channel
     (Selected_Timer'Access,
      Channel => Output_Channel,
      Point   => Green_LED,
      PWM_AF  => Timer_AF);

   Power_Control.Enable_Output;

   --  Configure CORDIC coprocessor with sine function
   Enable_Clock (CORDIC_Unit);
   Configure_CORDIC_Coprocessor
     (CORDIC_Unit,
      Operation => Sine,
      Precision => Iteration_12,
      Data_Size => Data_32_Bit);

   declare
      --  The CORDIC operates in fixed point signed integer format. Input and
      --  output values can be either q1.31 or q1.15.
      --  In q1.31 format, numbers are represented by one sign bit and 31
      --  fractional bits (binary decimal places). The numeric range is
      --  therefore -1 (0x80000000) to 1 - 2**(-31) (0x7FFFFFFF).
      --  In q1.15 format, the numeric range is 1 (0x8000) to 1 - 2**(-15)
      --  (0x7FFF).

      Angle : Q1_31 := -0.5;
      --  The start angle is -0.5, that corresponds to -Pi/2, so the LED start
      --  off.
      Modulus : constant UInt32 := 16#7FFFFFFF#; --  1 - 2**(-31)
      --  For sine function, the first argument is the angle, while the second
      --  argument is the modulus, that in this case doesn't change.
      Increment : Q1_31 := 1.0 / 4_194_304; --  = 1.0 / 2**22
      --  This value must be a multiple of delta (2.0**(-31)).
      --  The Increment value controls the rate at which the brightness
      --  increases and decreases and depends on the CPU clock frequency.
      Data_In : Block_32 := (Q1_31_To_UInt32 (Angle), Modulus);
      --  The sine function goes from -1.0 to 0.0 to 1.0 in a complete sine
      --  period, that corresponds to -Pi to 0 to Pi.
      Data_Out : Block_32 := (0, 0);

      Value : Percentage;
   begin
      loop
         --  Calculate sine function
         Calculate_CORDIC_Function
           (CORDIC_Unit,
            Argument => Data_In,
            Result   => Data_Out);

         --  The input (WDATA) and output (RDATA) data of the CORDIC uses UInt32
         --  to represent the fixed point values. So we need to convert the type
         --  Fraction_32 to UInt32 and vice-versa.
         Value := Percentage (50.0 * (1.0 +
                              Float (UInt32_To_Q1_31 (Data_Out (1)))));
         Power_Control.Set_Duty_Cycle (Value);

         --  Data input to CORDIC must be between -1.0 and 1.0 - 1 / 2**(-31),
         --  and this corresponds to -pi and pi. The value calculation considers
         --  that the LED is OFF when the Angle is -0.5 (-Pi/2), it is half
         --  bright when the Angle is 0 and it is ON when the Angle is 0.5 (Pi/2).
         --  When the angle value reaches any of these maximum, it must be
         --  incremented/decremented.
         if (Angle + Increment > 0.5) or
           (Angle + Increment < -0.5)
         then
            Increment := -Increment;
         end if;

         Angle := Angle + Increment;
         --  Write the new angle value into the first position of the buffer.
         Data_In (1) := Q1_31_To_UInt32 (Angle);
      end loop;
   end;
end Test_LED_PWM;
