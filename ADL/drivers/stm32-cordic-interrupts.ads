with Ada.Interrupts.Names;

package STM32.CORDIC.Interrupts is

   procedure Initialize_CORDIC (This : in out CORDIC_Coprocessor)
     with Post => not Interrupt_Enabled (This);
   --  Must be called once, prior to any call to get a calculated function
   --  value via polling. Enables the clock.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : Block_32;
      Result   : out Block_32);
   --  Uses the interrupt interface to get the the calculated funtion result.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : Block_16;
      Result   : out Block_16);
   --  Uses the interrupt interface to get the the calculated funtion result.

private
   type Buffer_Content is array (Integer range <>) of UInt32;

   type Ring_Buffer is record
      Content : Buffer_Content (0 .. 9);
      Head    : Integer := 0;
      Tail    : Integer := 0;
   end record;

   Ring_Buffer_Full : exception;
   --  Raised when the Ring Buffer is full (Head and Tail is the same).

   protected Receiver is
      pragma Interrupt_Priority;
      entry Get_Result (Value : out UInt32);

   private
      Buffer         : Ring_Buffer;
      Data_Available : Boolean := False;

      procedure Interrupt_Handler;

      pragma Attach_Handler
        (Interrupt_Handler,
         Ada.Interrupts.Names.Cordic_Interrupt);

   end Receiver;

end STM32.CORDIC.Interrupts;
