with Ada.Unchecked_Conversion;

private with STM32_SVD.FMAC;

package STM32.FMAC is
   pragma Elaborate_Body;

   type FMAC_Accelerator is limited private;

   procedure Reset_FMAC (This : in out FMAC_Accelerator);

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
   --  Trigger the execution of the function selected in the FUNC bitfield.
   --  Resetting it by software stops any ongoing function.

   function FMAC_Started
     (This : FMAC_Accelerator) return Boolean;

   type FMAC_Function is
     (Load_X1_Buffer,
      Load_X2_Buffer,
      Load_Y_Buffer,
      Convolution_FIR_Filter,
      IIR_Filter)
     with Size => 7;

   for FMAC_Function use
     (Load_X1_Buffer         => 16#01#,
      Load_X2_Buffer         => 16#02#,
      Load_Y_Buffer          => 16#03#,
      Convolution_FIR_Filter => 16#08#,
      IIR_Filter             => 16#09#);

   procedure Configure_FMAC_Parameters
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Function;
      Input_P   : UInt8;
      Input_Q   : UInt8 := 0;
      Input_R   : UInt8 := 0)
     with Pre => FMAC_Started (This) = True;
   --  See RM0440 rev 6 section 18.3.6 for detailed instructions about each
   --  initialization functions (load X1, X2 and Y buffers) and section 18.3.6
   --  for filter functions (FIR and IIR).

   --  The FMAC operates in fixed point signed integer format. Input and output
   --  values are q1.15.
   --  In q1.15 format, numbers are represented by one sign bit and 15 fractional
   --  bits (binary decimal places). The numeric range is therefore -1 (0x8000)
   --  to 1 - 2**(-15) (0x7FFF).
   type Fraction_16 is delta 2.0**(-15) range -1.0 .. 1.0 - 2.0**(-15);

   --  The input (WDATA) and output (RDATA) data of the FMAC uses UInt16
   --  to represent the fixed point values. So we need to convert the type
   --  Fraction_16 to UInt16 and vice-versa.
   function Fraction_16_To_UInt16 is new
     Ada.Unchecked_Conversion (Fraction_16, UInt16);
   function UInt16_To_Fraction_16 is new
     Ada.Unchecked_Conversion (UInt16, Fraction_16);

   procedure Write_FMAC_Data
     (This : in out FMAC_Accelerator;
      Data : UInt16);

   function Read_FMAC_Data
     (This : FMAC_Accelerator) return UInt16;

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

   function Read_FMAC_Status
     (This   : FMAC_Accelerator;
      Status : FMAC_Status) return Boolean;

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
