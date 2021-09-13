with HAL;                  use HAL;
with System;               use System;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.Timers;         use STM32.Timers;
with STM32.ADC;            use STM32.ADC;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

package STM_Board is

   ---------------
   -- Constants --
   ---------------

   subtype Frequency_Hz is Float;

   ---------------------
   -- PWM Full-bridge --
   ---------------------

   PWM_Timer     : Timer renames Timer_1;
   --  Timer for reading sine table values
   PWM_Interrupt : Ada.Interrupts.Interrupt_ID renames TIM1_CC_Interrupt;
   PWM_ISR_Priority : constant Interrupt_Priority := Interrupt_Priority'Last - 3;

   PWM_A_Channel : Timer_Channel renames Channel_1;
   PWM_A_H_Pin   : GPIO_Point renames PA8;
   PWM_A_L_Pin   : GPIO_Point renames PA7;
   PWM_A_GPIO_AF : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM1_6;

   PWM_B_Channel : Timer_Channel renames Channel_2;
   PWM_B_H_Pin   : GPIO_Point renames PA9;
   PWM_B_L_Pin   : GPIO_Point renames PB0;
   PWM_B_GPIO_AF : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM1_6;

   PWM_Gate_Power : GPIO_Point renames PA11;
   --  Output for the FET/IGBT gate drivers.


   ------------------------------
   -- Voltage and Current ADCs --
   ------------------------------

   Sensor_ADC           : constant access Analog_To_Digital_Converter := ADC_1'Access;
   Sensor_Trigger_Event : External_Events_Regular_Group :=  Timer3_CC1_Event;
   Sensor_Interrupt     : Ada.Interrupts.Interrupt_ID renames ADC1_2_Interrupt;
   Sensor_ISR_Priority  : constant Interrupt_Priority := Interrupt_Priority'Last - 2;

   --  ADC channels
   ADC_Battery_V_Point : constant ADC_Point := (Sensor_ADC, Channel => 6);
   ADC_Battery_V_Pin   : GPIO_Point renames PC0;

   ADC_Battery_I_Point : constant ADC_Point := (Sensor_ADC, Channel => 7);
   ADC_Battery_I_Pin   : GPIO_Point renames PC1;

   ADC_Output_V_Point : constant ADC_Point := (Sensor_ADC, Channel => 8);
   ADC_Output_V_Pin   : GPIO_Point renames PC2;

   ---------------
   -- ADC Timer --
   ---------------

   --  To syncronize A/D conversion and timers, the ADCs could be triggered
   --  by any of TIM1, TIM2, TIM3, TIM6, TIM7, TIM15, TIM16 or TIM17 timer.
   Sensor_Timer              : Timer renames Timer_3;
   Sensor_Timer_Interrupt    : Ada.Interrupts.Interrupt_ID renames TIM3_Interrupt;
   Sensor_Timer_ISR_Priority : constant Interrupt_Priority := Interrupt_Priority'Last - 2;

   --  Channel for reading analog inputs (5 kHz, 200 us)
   Sensor_Timer_Channel : Timer_Channel renames Channel_1;
   Sensor_Timer_AF      : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM3_2;
   Sensor_Timer_Point   : GPIO_Point renames PA6;
   --  Point not used because this timer only start ADC conversion.
   Sensor_Frequency_Hz  : constant Frequency_Hz := 5_000.0;
   -- Timer PWM frequency that controls start of ADC convertion.

   -------------------
   -- General Timer --
   -------------------

   General_Timer              : Timer renames Timer_4;
   General_Timer_Interrupt    : Ada.Interrupts.Interrupt_ID renames TIM4_Interrupt;
   General_Timer_ISR_Priority : constant Interrupt_Priority := Interrupt_Priority'Last - 1;

   --  Channel for general use (1 kHz, 1 ms)
   General_Timer_Channel_1 : Timer_Channel renames Channel_1;
   General_Timer_AF_1      : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM4_2;
   General_Timer_Point_1   : GPIO_Point renames PB6;
   --  Point not used because this timer only start an interrupt.
   General_Frequency_Hz_1  : constant Frequency_Hz := 1_000.0;

   --  Channel for soft-start and auto restarting (10 Hz, 100 ms)
   General_Timer_Channel_2 : Timer_Channel renames Channel_2;
   General_Timer_AF_2      : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM4_2;
   General_Timer_Point_2   : GPIO_Point renames PB7;
   --  Point not used because this timer only start an interrupt.
   General_Frequency_Hz_2  : constant Frequency_Hz := 10.0;

   -------------------------
   -- Other GPIO Channels --
   -------------------------

   AC_Frequency_Pin : GPIO_Point renames PA0;
   --  Input for AC frequency select jumper.

   Green_LED : GPIO_Point renames PA5;
   --  Output for OK indication in the nucleo board.

   Red_LED   : GPIO_Point renames PB1;
   --  Output for problem indication in the nucleo board.
   LCH_LED   : GPIO_Point renames Red_LED;
   --  Last chance handler led.

   All_LEDs  : GPIO_Points := Green_LED & Red_LED;

   Buzzer : GPIO_Point renames PB2;
   --  Output for buzzer alarm.

   -----------------------------
   -- Procedures and function --
   -----------------------------

   procedure Initialize_GPIO;
   --  Initialize GPIO inputs and outputs.

   function Read_AC_Frequency return Boolean;
   --  Read AC frequency selection into global variable.

   procedure Turn_On (This : in out GPIO_Point)
   with
      Pre => Is_Initialized and (This /= PWM_Gate_Power);
   --  Turns ON the specified output.

   procedure Turn_Off (This : in out GPIO_Point)
   with
      Pre => Is_Initialized and (This /= PWM_Gate_Power);
   --  Turns OFF the specified output.

   procedure Set_Toggle (This : in out GPIO_Point)
   with
      Pre => Is_Initialized and (This /= PWM_Gate_Power);
   --  Toggle the specified output.

   procedure All_LEDs_Off
   with
      Pre => Is_Initialized;
   --  Turns OFF all LEDs.

   procedure All_LEDs_On
   with
      Pre => Is_Initialized;
   --  Turns ON all LEDs.

   procedure Toggle_LEDs (These : in out GPIO_Points)
   with
      Pre => Is_Initialized;
   --  Toggle the specified LEDs.

   function Is_Initialized return Boolean;
   --  Returns True if the board specifics are initialized.

private

   Initialized : Boolean := False;

end STM_Board;
