
package body STM32.CORDIC is

   -------------------------
   -- Set_CORDIC_Function --
   -------------------------

   procedure Set_CORDIC_Function
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Function)
   is
   begin
      This.CSR.FUNC := Value'Enum_Rep;
   end Set_CORDIC_Function;

   -------------------------
   -- Get_CORDIC_Function --
   -------------------------

   function Get_CORDIC_Function (This : CORDIC_Coprocessor)
     return CORDIC_Function
   is
   begin
      return CORDIC_Function'Val (This.CSR.FUNC);
   end Get_CORDIC_Function;

   --------------------------
   -- Set_CORDIC_Precision --
   --------------------------

   procedure Set_CORDIC_Precision
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Iterations)
   is
   begin
      This.CSR.PRECISION := Value'Enum_Rep;
   end Set_CORDIC_Precision;

   -------------------------------
   -- Set_CORDIC_Scaling_Factor --
   -------------------------------

   procedure Set_CORDIC_Scaling_Factor
     (This  : in out CORDIC_Coprocessor;
      Value : UInt3)
   is
   begin
      This.CSR.SCALE := Value;
   end Set_CORDIC_Scaling_Factor;

   ---------------------------
   -- Set_CORDIC_Data_Width --
   ---------------------------

   procedure Set_CORDIC_Data_Width
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Data_Size)
   is
   begin
      This.CSR.ARGSIZE := Value = Data_16_Bit;
      This.CSR.RESSIZE := Value = Data_16_Bit;
   end Set_CORDIC_Data_Width;

   ---------------------------
   -- Get_CORDIC_Data_Width --
   ---------------------------

   function Get_CORDIC_Data_Width
     (This  : CORDIC_Coprocessor)
      return CORDIC_Data_Size
   is
   begin
      return (if This.CSR.ARGSIZE then Data_16_Bit else Data_32_Bit);
   end Get_CORDIC_Data_Width;

   ---------------------------------
   -- Set_CORDIC_Arguments_Number --
   ---------------------------------

   procedure Set_CORDIC_Arguments_Number
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Arguments_Number)
   is
   begin
      This.CSR.NARGS := Value = Two_32_Bit;
   end Set_CORDIC_Arguments_Number;

   ------------------------
   -- Set_CORDIC_Results --
   ------------------------

   procedure Set_CORDIC_Results
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Arguments_Number)
   is
   begin
      This.CSR.NRES := Value = Two_32_Bit;
   end Set_CORDIC_Results;

   -----------------------------------
   -- Configure_CORDIC_Coprocesssor --
   -----------------------------------

   procedure Configure_CORDIC_Coprocessor
     (This      : in out CORDIC_Coprocessor;
      Operation : CORDIC_Function;
      Precision : CORDIC_Iterations := Iteration_20;
      Scaling   : UInt3 := 0;
      Data_Size : CORDIC_Data_Size)
   is
   begin
      This.CSR.FUNC := Operation'Enum_Rep;
      This.CSR.PRECISION := Precision'Enum_Rep;
      This.CSR.SCALE := Scaling;
      This.CSR.ARGSIZE := Data_Size = Data_16_Bit;
      This.CSR.RESSIZE := Data_Size = Data_16_Bit;

      case Operation is
         when Cosine | Sine | Phase | Modulus =>
            case Data_Size is
               when Data_32_Bit =>
                  This.CSR.NARGS := True; --  Two_32_Bit
                  This.CSR.NRES := True; --  Two_32_Bit
               when Data_16_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit
                  This.CSR.NRES := False; --  One_32_Bit
            end case;
         when Hyperbolic_Cosine | Hyperbolic_Sine =>
            case Data_Size is
               when Data_32_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit
                  This.CSR.NRES := True; --  Two_32_Bit
               when Data_16_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit
                  This.CSR.NRES := False; --  One_32_Bit
            end case;
         when Arctangent | Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            case Data_Size is
               when Data_32_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit
                  This.CSR.NRES := False; --  One_32_Bit
               when Data_16_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit
                  This.CSR.NRES := False; --  One_32_Bit
            end case;
      end case;

   end Configure_CORDIC_Coprocessor;

   -------------------
   -- Set_Interrupt --
   -------------------

   procedure Set_Interrupt
     (This   : in out CORDIC_Coprocessor;
      Enable : Boolean)
   is
   begin
      This.CSR.IEN := Enable;
   end Set_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : in out CORDIC_Coprocessor) return Boolean
   is
   begin
      return This.CSR.IEN;
   end Interrupt_Enabled;

   -------------
   -- Set_DMA --
   -------------

   procedure Set_DMA
     (This   : in out CORDIC_Coprocessor;
      DMA    : CORDIC_DMA;
      Enable : Boolean)
   is
   begin
      case DMA is
         when Read_DMA =>
            This.CSR.DMAREN := Enable;
         when Write_DMA =>
            This.CSR.DMAWEN := Enable;
      end case;
   end Set_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This   : CORDIC_Coprocessor;
      DMA    : CORDIC_DMA)
      return Boolean
   is
   begin
      case DMA is
         when Read_DMA =>
            return This.CSR.DMAREN;
         when Write_DMA =>
            return This.CSR.DMAWEN;
      end case;
   end DMA_Enabled;

end STM32.CORDIC;
