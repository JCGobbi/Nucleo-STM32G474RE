with STM32.Device;

package body STM32.FMAC is

   ----------------
   -- Reset_FMAC --
   ----------------

   procedure Reset_FMAC (This : in out FMAC_Accelerator) is
   begin
      This.CR.RESET := True;
   end Reset_FMAC;

   -----------------------------
   -- Set_FMAC_Buffer_Address --
   -----------------------------

   procedure Set_FMAC_Buffer_Address
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.X1_BASE := Base_Address;
         when X2 =>
            This.X2BUFCFG.X2_BASE := Base_Address;
         when Y =>
            This.YBUFCFG.Y_BASE := Base_Address;
      end case;
   end Set_FMAC_Buffer_Address;

   --------------------------
   -- Set_FMAC_Buffer_Size --
   --------------------------

   procedure Set_FMAC_Buffer_Size
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Size         : UInt8)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.X1_BUF_SIZE := Size;
         when X2 =>
            This.X2BUFCFG.X2_BUF_SIZE := Size;
         when Y =>
            This.YBUFCFG.Y_BUF_SIZE := Size;
      end case;
   end Set_FMAC_Buffer_Size;

   -------------------------------
   -- Set_FMAC_Buffer_Watermark --
   -------------------------------

   procedure Set_FMAC_Buffer_Watermark
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Watermark    : FMAC_Watermark := Threshold_1)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.FULL_WM := Watermark'Enum_Rep;
         when X2 =>
            null;
         when Y =>
            This.YBUFCFG.EMPTY_WM := Watermark'Enum_Rep;
      end case;
   end Set_FMAC_Buffer_Watermark;

   ---------------------------
   -- Configure_FMAC_Buffer --
   ---------------------------

   procedure Configure_FMAC_Buffer
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8;
      Size         : UInt8;
      Watermark    : FMAC_Watermark := Threshold_1)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.X1_BASE := Base_Address;
            This.X1BUFCFG.X1_BUF_SIZE := Size;
            This.X1BUFCFG.FULL_WM := Watermark'Enum_Rep;
         when X2 =>
            This.X2BUFCFG.X2_BASE := Base_Address;
            This.X2BUFCFG.X2_BUF_SIZE := Size;
         when Y =>
            This.YBUFCFG.Y_BASE := Base_Address;
            This.YBUFCFG.Y_BUF_SIZE := Size;
            This.YBUFCFG.EMPTY_WM := Watermark'Enum_Rep;
      end case;
   end Configure_FMAC_Buffer;

   ---------------------
   -- Initialize_FMAC --
   ---------------------

   procedure Initialize_FMAC
     (This   : in out FMAC_Accelerator;
      Config : FMAC_Configuration)
   is
   begin
      STM32.Device.Enable_Clock (This);
      --  Configure X1 buffer
      This.X1BUFCFG.X1_BASE := Config.Input_Base_Address;
      This.X1BUFCFG.X1_BUF_SIZE := Config.Input_Buffer_Size;
      --  Configure X2 buffer
      This.X1BUFCFG.FULL_WM := Config.Input_Buffer_Threshold'Enum_Rep;
      This.X2BUFCFG.X2_BASE := Config.Coeff_Base_Address;
      This.X2BUFCFG.X2_BUF_SIZE := Config.Coeff_Base_Size;
      --  Configure Y buffer
      This.YBUFCFG.Y_BASE := Config.Output_Base_Address;
      This.YBUFCFG.Y_BUF_SIZE := Config.Output_Buffer_Size;
      This.YBUFCFG.EMPTY_WM := Config.Output_Buffer_Threshold'Enum_Rep;

      This.CR.CLIPEN := Config.Clipping;
   end Initialize_FMAC;

   --------------------
   -- Set_FMAC_Start --
   --------------------

   procedure Set_FMAC_Start
     (This  : in out FMAC_Accelerator;
      Start : Boolean)
   is
   begin
      This.PARAM.START := Start;
   end Set_FMAC_Start;

   ------------------
   -- FMAC_Started --
   ------------------

   function FMAC_Started (This : FMAC_Accelerator) return Boolean is
   begin
      return This.PARAM.START;
   end FMAC_Started;

   -------------------------------
   -- Configure_FMAC_Parameters --
   -------------------------------

   procedure Configure_FMAC_Parameters
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Function;
      Input_P   : UInt8;
      Input_Q   : UInt8 := 0;
      Input_R   : UInt8 := 0)
   is
   begin
      This.PARAM.P := Input_P;

      case Operation is
         when Load_X2_Buffer =>
            This.PARAM.Q := Input_Q;
         when FIR_Filter_Convolution =>
            This.PARAM.R := Input_R;
         when IIR_Filter_Direct_Form_1 =>
            This.PARAM.Q := Input_Q;
            This.PARAM.R := Input_R;
         when others => null;
      end case;

      This.PARAM.FUNC := Operation'Enum_Rep;
   end Configure_FMAC_Parameters;

   ---------------------
   -- Write_FMAC_Data --
   ---------------------

   procedure Write_FMAC_Data
     (This : in out FMAC_Accelerator;
      Data : UInt16)
   is
   begin
      This.WDATA.WDATA := Data;
   end Write_FMAC_Data;

   --------------------
   -- Read_FMAC_Data --
   --------------------

   function Read_FMAC_Data
     (This : FMAC_Accelerator) return UInt16
   is
   begin
      return This.RDATA.RDATA;
   end Read_FMAC_Data;

   ---------------------
   -- Write_FMAC_Buffer --
   ---------------------

   procedure Write_FMAC_Buffer
     (This   : in out FMAC_Accelerator;
      Vector : Block_Q1_15)
   is
   begin
      for N in Vector'Range loop
         This.WDATA.WDATA := Q1_15_To_UInt16 (Vector (N));
      end loop;
   end Write_FMAC_Buffer;

   -----------------------
   -- Set_FMAC_Clipping --
   -----------------------

   procedure Set_FMAC_Clipping
     (This   : in out FMAC_Accelerator;
      Enable : Boolean)
   is
   begin
      This.CR.CLIPEN := Enable;
   end Set_FMAC_Clipping;

   ------------
   -- Status --
   ------------

   function Status
     (This : FMAC_Accelerator;
      Flag : FMAC_Status) return Boolean
   is
   begin
      case Flag is
         when Y_Buffer_Empty =>
            return This.SR.YEMPTY;
         when X1_Buffer_Full =>
            return This.SR.X1FULL;
         when Overflow_Error =>
            return This.SR.OVFL;
         when Underflow_Error =>
            return This.SR.UNFL;
         when Saturation_Error =>
            return This.SR.SAT;
      end case;
   end Status;

   -------------------
   -- Set_Interrupt --
   -------------------

   procedure Set_Interrupt
     (This      : in out FMAC_Accelerator;
      Interrupt : FMAC_Interrupt;
      Enable    : Boolean)
   is
   begin
      case Interrupt is
         when Read_Interrupt =>
            This.CR.RIEN := Enable;
         when Write_Interrupt =>
            This.CR.WIEN := Enable;
         when Overflow_Error =>
            This.CR.OVFLIEN := Enable;
         when Underflow_Error =>
            This.CR.UNFLIEN := Enable;
         when Saturation_Error =>
            This.CR.SATIEN := Enable;
      end case;
   end Set_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This      : FMAC_Accelerator;
      Interrupt : FMAC_Interrupt) return Boolean
   is
   begin
      case Interrupt is
         when Read_Interrupt =>
            return This.CR.RIEN;
         when Write_Interrupt =>
            return This.CR.WIEN;
         when Overflow_Error =>
            return This.CR.OVFLIEN;
         when Underflow_Error =>
            return This.CR.UNFLIEN;
         when Saturation_Error =>
            return This.CR.SATIEN;
      end case;
   end Interrupt_Enabled;

   -------------
   -- Set_DMA --
   -------------

   procedure Set_DMA
     (This   : in out FMAC_Accelerator;
      DMA    : FMAC_DMA;
      Enable : Boolean)
   is
   begin
      case DMA is
         when Read_DMA =>
            This.CR.DMAREN := Enable;
         when Write_DMA =>
            This.CR.DMAWEN := Enable;
      end case;
   end Set_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This   : FMAC_Accelerator;
      DMA    : FMAC_DMA)
      return Boolean
   is
   begin
      case DMA is
         when Read_DMA =>
            return This.CR.DMAREN;
         when Write_DMA =>
            return This.CR.DMAWEN;
      end case;
   end DMA_Enabled;

end STM32.FMAC;
