with Ada.Unchecked_Conversion;

private with STM32_SVD.FMAC;

package STM32.FMAC is
   pragma Elaborate_Body;

   type FMAC_Accelerator is limited private;

   procedure Reset_FMAC (This : in out FMAC_Accelerator);
   --  Resets the write and read pointers, the internal control logic,
   --  the FMAC_SR register and the FMAC_PARAM register, including the START
   --  bit if active. Other register settings are not affected. This bit is
   --  reset by hardware.

   type FMAC_Buffer is (X1, X2, Y);

   procedure Set_FMAC_Buffer_Address
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8);
   --  Define the base address for the buffers.

   procedure Set_FMAC_Buffer_Size
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Size         : UInt8)
     with Pre => (if Buffer = X2 then not FMAC_Started (This));
   --  Define the size of buffers in 16-bit words.
   --  For X1 buffer the minimum buffer size is the number of feed-forward taps
   --  in the filter (+ the watermark threshold - 1).
   --  For Y buffer and for FIR filters, the minimum buffer size is 1 (+ the
   --  watermark threshold). For IIR filters the minimum buffer size is the
   --  number of feedback taps (+ the watermark threshold).
   --  See buffer configuration at pg 490, chapter 18.3.2 RM0440 rev 6.

   type FMAC_Watermark is
     (Threshold_1,
      Threshold_2,
      Threshold_3,
      Threshold_4);

   procedure Set_FMAC_Buffer_Watermark
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Watermark    : FMAC_Watermark := Threshold_1)
     with Pre => (if DMA_Enabled (This, Write_DMA) or
                     DMA_Enabled (This, Read_DMA)
                  then Watermark = Threshold_1);
   --  For X1 buffer define the threshold for setting the X1 buffer full flag
   --  when operating in circular mode. The flag is set if the number of free
   --  spaces in the buffer is less than 2**FULL_WM.
   --  For Y buffer define the threshold for setting the Y buffer empty flag
   --  when operating in circular mode. The flag is set if the number of unread
   --  values in the buffer is less than 2**EMPTY_WM.

   procedure Configure_FMAC_Buffer
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8;
      Size         : UInt8;
      Watermark    : FMAC_Watermark := Threshold_1);
   --  See buffer configuration at pg 490, chapter 18.3.2 RM0440 rev 6.

   procedure Set_FMAC_Start
     (This  : in out FMAC_Accelerator;
      Start : Boolean)
     with Post => FMAC_Started (This) = Start;
   --  Triggers the execution of the function selected in the FUNC bitfield.
   --  Resetting it by software stops any ongoing function. For initialization
   --  functions (Load X1 buffer, Load X2 buffer, Load Y buffer), this bit is
   --  reset by hardware.

   function FMAC_Started
     (This : FMAC_Accelerator) return Boolean;

   type FMAC_Function is
     (Load_X1_Buffer,
      Load_X2_Buffer,
      Load_Y_Buffer,
      FIR_Filter_Convolution,
      IIR_Filter_Direct_Form_1)
     with Size => 7;

   for FMAC_Function use
     (Load_X1_Buffer           => 16#01#,
      Load_X2_Buffer           => 16#02#,
      Load_Y_Buffer            => 16#03#,
      FIR_Filter_Convolution   => 16#08#,
      IIR_Filter_Direct_Form_1 => 16#09#);

   procedure Configure_FMAC_Parameters
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Function;
      Input_P   : UInt8;
      Input_Q   : UInt8 := 0;
      Input_R   : UInt8 := 0)
     with Pre => FMAC_Started (This) = False;
   --  Trigger by writing the appropriate value in the FUNC bitfield of the
   --  FMAC_PARAM register, with the START bit set. The P, Q and R bitfields
   --  must also contain the appropriate parameter values for each function.
   --  For initialization functions (load X1, X2 or Y buffers), the function
   --  completes when N writes have been performed to the FMAC_WDATA register,
   --  then the START bit is automatically reset by hardware.
   --  For filter functions (FIR or IIR), the filter functions continue to run
   --  until the START bit is reset by software.
   --  See RM0440 rev 6 section 18.3.5 for detailed instructions about each
   --  initialization functions (Load X1, X2 and Y buffers) and section 18.3.6
   --  for filter functions (FIR and IIR).

   procedure Set_FMAC_Start_Function
     (This      : in out FMAC_Accelerator;
      Start     : Boolean;
      Operation : FMAC_Function;
      Input_P   : UInt8;
      Input_Q   : UInt8 := 0;
      Input_R   : UInt8 := 0);
   --  Trigger by writing the appropriate value in the FUNC bitfield of the
   --  FMAC_PARAM register, with the START bit set. The P, Q and R bitfields
   --  must also contain the appropriate parameter values for each function.
   --  For initialization functions (load X1, X2 or Y buffers), the function
   --  completes when N writes have been performed to the FMAC_WDATA register,
   --  then the START bit is automatically reset by hardware.
   --  For filter functions (FIR or IIR), the filter functions continue to run
   --  until the START bit is reset by software.
   --  See RM0440 rev 6 section 18.3.5 for detailed instructions about each
   --  initialization functions (Load X1, X2 and Y buffers) and section 18.3.6
   --  for filter functions (FIR and IIR).

   --  The FMAC operates in fixed point signed integer format. Input and output
   --  values are q1.15.
   --  In q1.15 format, numbers are represented by one sign bit and 15 fractional
   --  bits (binary decimal places). The numeric range is therefore -1 (0x8000)
   --  to 1 - 2**(-15) (0x7FFF).
   type Q1_15 is delta 2.0**(-15) range -1.0 .. 1.0 - 2.0**(-15)
     with Size => 16;

   --  The input (WDATA) and output (RDATA) data of the FMAC uses UInt16
   --  to represent the fixed point values. So we need to convert the type
   --  Fraction_16 to UInt16 and vice-versa.
   function Q1_15_To_UInt16 is new
     Ada.Unchecked_Conversion (Q1_15, UInt16);
   function UInt16_To_Q1_15 is new
     Ada.Unchecked_Conversion (UInt16, Q1_15);

   procedure Write_FMAC_Data
     (This : in out FMAC_Accelerator;
      Data : UInt16);
   --  When a write access to this register occurs, the write data are
   --  transferred to the address offset indicated by the write pointer. The
   --  pointer address is automatically incremented after each write access.

   function Read_FMAC_Data
     (This : FMAC_Accelerator) return UInt16;
   --  When a read access to this register occurs, the read data are the
   --  contents of the Y output buffer at the address offset indicated by the
   --  READ pointer. The pointer address is automatically incremented after
   --  each read access.

   procedure Set_FMAC_Clipping
     (This   : in out FMAC_Accelerator;
      Enable : Boolean);
   --  Values at the output of the accumulator, which exceed the q1.15 range,
   --  wrap (clipping disabled) or are saturated to the maximum positive or
   --  negative value (+1 or -1) according to the sign.

   type FMAC_Status is
     (Y_Buffer_Empty,
      X1_Buffer_Full,
      Overflow_Error,
      Underflow_Error,
      Saturation_Error);

   function Status
     (This : FMAC_Accelerator;
      Flag : FMAC_Status) return Boolean;

   type FMAC_Interrupt is
     (Read_Interrupt,
      Write_Interrupt,
      Overflow_Error,
      Underflow_Error,
      Saturation_Error);

   procedure Set_Interrupt
     (This      : in out FMAC_Accelerator;
      Interrupt : FMAC_Interrupt;
      Enable    : Boolean)
     with Post => Interrupt_Enabled (This, Interrupt) = Enable;

   function Interrupt_Enabled
     (This      : FMAC_Accelerator;
      Interrupt : FMAC_Interrupt) return Boolean;

   type FMAC_DMA is (Read_DMA, Write_DMA);

   procedure Set_DMA
     (This   : in out FMAC_Accelerator;
      DMA    : FMAC_DMA;
      Enable : Boolean)
     with Post => DMA_Enabled (This, DMA) = Enable;

   function DMA_Enabled
     (This : FMAC_Accelerator;
      DMA  : FMAC_DMA) return Boolean;

private

   type FMAC_Accelerator is new STM32_SVD.FMAC.FMAC_Peripheral;

end STM32.FMAC;
