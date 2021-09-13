with STM32.SYSCFG;

package body STM32.COMP is

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Comparator) is
      use STM32.SYSCFG;
   begin
      --  Enable clock for the COMP peripheral
      Enable_SYSCFG_Clock;
      --  There is no COMP-dedicated clock enable control bit in the RCC
      --  controller. Reset and clock enable bits are common for COMP and
      --  SYSCFG. See RM0440 pg 781 chapter 24.3.3.

      This.CSR.EN := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Comparator) is
   begin
      This.CSR.EN := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Comparator) return Boolean is
   begin
      return This.CSR.EN;
   end Enabled;

   ------------------------------
   -- Set_Inverting_Input_Port --
   ------------------------------

   procedure Set_Inverting_Input_Port
     (This  : in out Comparator;
      Input : Inverting_Input_Port) is
   begin
      This.CSR.INMSEL := Input'Enum_Rep;
   end Set_Inverting_Input_Port;

   -------------------------------
   -- Read_Inverting_Input_Port --
   -------------------------------

   function Read_Inverting_Input_Port
     (This : Comparator) return Inverting_Input_Port
   is
   begin
      return Inverting_Input_Port'Val (This.CSR.INMSEL);
   end Read_Inverting_Input_Port;

   ---------------------------------
   -- Set_NonInverting_Input_Port --
   ---------------------------------

   procedure Set_NonInverting_Input_Port
     (This  : in out Comparator;
      Input : NonInverting_Input_Port) is
   begin
      This.CSR.INPSEL := Boolean'Val (Input'Enum_Rep);
   end Set_NonInverting_Input_Port;

   ----------------------------------
   -- Read_NonInverting_Input_Port --
   ----------------------------------

   function Read_NonInverting_Input_Port
     (This : Comparator) return NonInverting_Input_Port
   is
   begin
      if This.CSR.INPSEL then
         return Option_1;
      else
         return Option_2;
      end if;
   end Read_NonInverting_Input_Port;

   -------------------------
   -- Set_Output_Polarity --
   -------------------------

   procedure Set_Output_Polarity (This   : in out Comparator;
                                  Output : Output_Polarity) is
   begin
      This.CSR.POL := Output = Inverted;
   end Set_Output_Polarity;

   --------------------------
   -- Read_Output_Polarity --
   --------------------------

   function Read_Output_Polarity (This : Comparator) return Output_Polarity is
   begin
      if This.CSR.POL then
         return Inverted;
      else
         return Not_Inverted;
      end if;
   end Read_Output_Polarity;

   -------------------------------
   -- Set_Comparator_Hysteresis --
   -------------------------------

   procedure Set_Comparator_Hysteresis (This  : in out Comparator;
                                        Value : Comparator_Hysteresis) is
   begin
      This.CSR.HYST := Value'Enum_Rep;
   end Set_Comparator_Hysteresis;

   --------------------------------
   -- Read_Comparator_Hysteresis --
   --------------------------------

   function Read_Comparator_Hysteresis (This : Comparator)
                                        return Comparator_Hysteresis is
   begin
      return Comparator_Hysteresis'Val (This.CSR.HYST);
   end Read_Comparator_Hysteresis;

   -------------------------
   -- Set_Output_Blanking --
   -------------------------

   procedure Set_Output_Blanking (This   : in out Comparator;
                                  Output : Output_Blanking) is
   begin
      This.CSR.BLANKSEL := Output'Enum_Rep;
   end Set_Output_Blanking;

   --------------------------
   -- Read_Output_Blanking --
   --------------------------

   function Read_Output_Blanking (This : Comparator) return Output_Blanking is
   begin
      return Output_Blanking'Val (This.CSR.BLANKSEL);
   end Read_Output_Blanking;

   ---------------------------------
   -- Set_Vrefint_Scaler_Resistor --
   ---------------------------------

   procedure Set_Vrefint_Scaler_Resistor
     (This   : in out Comparator;
      Output : Boolean)
   is
   begin
      This.CSR.BRGEN := Output;
   end Set_Vrefint_Scaler_Resistor;

   ----------------------------------
   -- Read_Vrefint_Scaler_Resistor --
   ----------------------------------

   function Read_Vrefint_Scaler_Resistor (This  : Comparator) return Boolean is
   begin
      return This.CSR.BRGEN;
   end Read_Vrefint_Scaler_Resistor;

   ------------------------
   -- Set_Vrefint_Scaler --
   ------------------------

   procedure Set_Vrefint_Scaler
     (This   : in out Comparator;
      Output : Boolean)
   is
   begin
      This.CSR.SCALEN := Output;
   end Set_Vrefint_Scaler;

   -------------------------
   -- Read_Vrefint_Scaler --
   -------------------------

   function Read_Vrefint_Scaler (This  : Comparator) return Boolean is
   begin
      return This.CSR.SCALEN;
   end Read_Vrefint_Scaler;

   ----------------------------
   -- Read_Comparator_Output --
   ----------------------------

   function Read_Comparator_Output
     (This : Comparator) return Comparator_Output is
   begin
      if This.CSR.VALUE then
         return High;
      else
         return Low;
      end if;
   end Read_Comparator_Output;

   -------------------------
   -- Set_Lock_Comparator --
   -------------------------

   procedure Set_Lock_Comparator (This : in out Comparator) is
   begin
      This.CSR.LOCK := True;
   end Set_Lock_Comparator;

   --------------------------
   -- Read_Lock_Comparator --
   --------------------------

   function Read_Lock_Comparator (This : Comparator) return Boolean is
   begin
      return This.CSR.LOCK;
   end Read_Lock_Comparator;

end STM32.COMP;
