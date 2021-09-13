with HAL;                  use HAL;
with STM32.Device;         use STM32.Device;
with STM32.Timers;         use STM32.Timers;

with STM_Board;            use STM_Board;

--  StartUp is executed after the Initialization and if the system restarted
--  after a fault condition.
package Inverter_Timer is


   --  STM32F429 operates at 168 MHz with 84 MHz into prescaler.
   --  With prescaler = (84 - 1) we have 1 MHz into Counter, so period has
   --  a 1 us unit time.
   --  With Prescaler => 139, Period => 59 we have 10 KHz into Counter,
   --  so period has a 1 ms unit time.
   --  With prescaler => 139, Period => 599 we have 1 kHz into Counter,
   --  so period has a 1 ms unit time.
   --  With prescaler => 139, Period => 59999 we have 10 Hz into Counter,
   --  so period has a 100 ms unit time.

   SystemCoreClock : constant UInt32 := System_Clock_Frequencies.SYSCLK;

   Prescaler : constant UInt16 := UInt16 (((SystemCoreClock / 2) / 600_000) - 1);
   --  600 kHz into Counter

   Channel_1_Period : constant := 120 - 1; --  5 kHz, 200 us
   --  Used for starting ADC conversions.
   Channel_2_Period : constant := 60_000 - 1; --  10 Hz, 100 ms
   --  Used for general low frequency tasks.
   Channel_3_Period : constant := 1 - 1; --  not used
   Channel_4_Period : constant := 1 - 1; --  not used

   --  A convenience array for the sake of using a loop to configure the timer
   --  channels
   Channel_Periods : constant array (Timer_Channel) of UInt32 :=
     (Channel_1_Period,
      Channel_2_Period,
      Channel_3_Period,
      Channel_4_Period);

   procedure Initialize_Timer;
   
   function Is_Initialized return Boolean;
   --  Returns True if the board specifics are initialized.

   function Has_APB2_Frequency (This : Timer) return Boolean;
   --  Timers 1, 8, 9, 10, 11

   function Has_APB1_Frequency (This : Timer) return Boolean;
   --  Timers 3, 4, 6, 7, 12, 13, 14

private

   Initialized : Boolean := False;
   
   protected Sensor_Timer_Handler is
      pragma Interrupt_Priority (Sensor_Timer_ISR_Priority);
   private
      procedure Sensor_Timer_ISR_Handler with
        Attach_Handler => Sensor_Timer_Interrupt;
   end Sensor_Timer_Handler;

end Inverter_Timer;
