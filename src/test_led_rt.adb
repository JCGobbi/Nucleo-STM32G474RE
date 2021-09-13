with Ada.Real_Time; use Ada.Real_Time;

with STM_Board;     use STM_Board;

procedure Test_LED_RT is
   --  This demonstration program only initializes the GPIOs and flash a LED
   --  with Ada.Real_Time. There is no initialization for PWM, ADC and timer.
begin
   --  Initialize GPIO ports
   Initialize_GPIO;

   --  Enter steady state
   loop
      Set_Toggle (Green_LED);
      delay until Clock + Milliseconds (1000);  -- arbitrary
   end loop;

end Test_LED_RT;
