package STM32.CORDIC.Polling is

   procedure Initialize_CORDIC (This : in out CORDIC_Coprocessor)
     with Post => not Interrupt_Enabled (This);
   --  Must be called once, prior to any call to get a calculated function
   --  value via polling. Enables the clock.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt32_Array;
      Result   : out UInt32_Array);
   --  Polls the CORDIC directly to get the calculated funtion result.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt16_Array;
      Result   : out UInt16_Array);
   --  Polls the CORDIC directly to get the calculated funtion result.

end STM32.CORDIC.Polling;
