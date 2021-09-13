with STM32_SVD; use STM32_SVD;
with System;    use System;

package body Inverter_Timer is

   ----------------------
   -- Initialize_Timer --
   ----------------------

   procedure Initialize_Timer is
   begin   
      --  Initialize the general timer
      Enable_Clock (Sensor_Timer);

      Reset (Sensor_Timer);

      Configure
        (Sensor_Timer,
         Prescaler     => Prescaler,
         Period        => UInt32 (UInt16'Last),  --  all the way up
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Configure_Prescaler
        (Sensor_Timer,
         Prescaler   => Prescaler,
         Reload_Mode => Immediate);

      -- Configure ADC timer
      Configure_Channel_Output
        (Sensor_Timer,
         Channel  => Channel_1,
         Mode     => PWM1,
         State    => Enable,
         Pulse    => Channel_1_Period,
         Polarity => High);
      Set_Output_Preload_Enable (Sensor_Timer, Channel_1, True);

      --  Set 50% duty cycle
      Set_Compare_Value
        (Sensor_Timer,
         Channel => Channel_1,
         Value   => Channel_1_Period / 2);

      --  It is not necessary to enable interrupt for this timer, the ADC
      --  conversions are started by the trigger of Sensor_Timer CC1 event.

      -- Configure low frequency timer
      Configure_Channel_Output
        (Sensor_Timer,
         Channel  => Channel_2,
         Mode     => Frozen,
         State    => Enable,
         Pulse    => Channel_2_Period,
         Polarity => High);
      Set_Output_Preload_Enable (Sensor_Timer, Channel_2, False);

      --  Enable the timer's channel interrupts and go
      Enable_Interrupt (Sensor_Timer, Timer_CC2_Interrupt);

      Enable (Sensor_Timer);

      Initialized := True;
   End Initialize_Timer;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized
      return Boolean is (Initialized);

   ------------------------
   -- Has_APB2_Frequency --
   ------------------------

   function Has_APB2_Frequency (This : Timer) return Boolean is
     (This'Address = STM32_SVD.TIM1_Base or
      This'Address = STM32_SVD.TIM8_Base or
      This'Address = STM32_SVD.TIM9_Base or
      This'Address = STM32_SVD.TIM10_Base or
      This'Address = STM32_SVD.TIM11_Base);

   ------------------------
   -- Has_APB1_Frequency --
   ------------------------

   function Has_APB1_Frequency (This : Timer) return Boolean is
     (This'Address = STM32_SVD.TIM2_Base or
      This'Address = STM32_SVD.TIM3_Base or
      This'Address = STM32_SVD.TIM4_Base or
      This'Address = STM32_SVD.TIM5_Base or
      This'Address = STM32_SVD.TIM6_Base or
      This'Address = STM32_SVD.TIM7_Base or
      This'Address = STM32_SVD.TIM12_Base or
      This'Address = STM32_SVD.TIM13_Base or
                      This'Address = STM32_SVD.TIM14_Base);

   --------------------------
   -- Sensor_Timer_Handler --
   --------------------------

   protected body Sensor_Timer_Handler is

      ------------------------------
      -- Sensor_Timer_ISR_Handler --
      ------------------------------

      procedure Sensor_Timer_ISR_Handler is
         Current : UInt16;
      begin
         if Status (Sensor_Timer, Timer_CC1_Indicated) then
            if Interrupt_Enabled (Sensor_Timer, Timer_CC1_Interrupt) then
               Clear_Pending_Interrupt (Sensor_Timer, Timer_CC1_Interrupt);
               Current := Current_Capture_Value (Sensor_Timer, Channel_1);
               Set_Compare_Value (Sensor_Timer, Channel_1, Current + Channel_1_Period);

               --  No need for routines for General_Timer Channel_1 because this
               --  timer only starts the ADC conversion and they are done inside
               --  Sensor_ISR_Handler.
               
            end if;
         end if;
         if Status (Sensor_Timer, Timer_CC2_Indicated) then
            if Interrupt_Enabled (Sensor_Timer, Timer_CC2_Interrupt) then
               Clear_Pending_Interrupt (Sensor_Timer, Timer_CC2_Interrupt);
               Current := Current_Capture_Value (Sensor_Timer, Channel_2);
               Set_Compare_Value (Sensor_Timer, Channel_2, Current + Channel_2_Period);
               
               --  Routines for General_Timer Channel_2
               --  Put here your low frequency routine
               
            end if;
         end if;
      end Sensor_Timer_ISR_Handler;

   end Sensor_Timer_Handler;

end Inverter_Timer;
