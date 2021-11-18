package STM32.CORDIC.Polling is

   procedure Initialize_CORDIC (This : in out CORDIC_Coprocessor)
     with Post => not Interrupt_Enabled (This);
   --  Must be called once, prior to any call to get a calculated function
   --  value via polling. Enables the clock.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : Block_32;
      Result   : out Block_32);
   --  Polls the CORDIC directly to get the the calculated funtion result.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : Block_16;
      Result   : out Block_16);
   --  Polls the CORDIC directly to get the the calculated funtion result.

end STM32.CORDIC.Polling;
