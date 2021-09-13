
--  This file provides interfaces for the comparators on the
--  STM32F3 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

with System; use System;

package STM32.COMP is

   type Comparator is limited private;

   procedure Enable (This : in out Comparator)
     with Post => Enabled (This);

   procedure Disable (This : in out Comparator)
     with Post => not Enabled (This);

   function Enabled (This : Comparator) return Boolean;

   type Inverting_Input_Port is
     (One_Quarter_Vrefint,
      One_Half_Vrefint,
      Three_Quarter_Vrefint,
      Vrefint,
      Option_5,
      Option_6,
      Option_7,
      Option_8);
   --  These bits allows to select the source connected to the inverting input
   --  of the comparator. The first 4 options are common, the last 4 options
   --  change for each comparator:
   --  Option  COMP1     COMP2     COMP3     COMP4     COMP5     COMP6     COMP7
   --  5    DAC3_CH1  DAC3_CH2  DAC3_CH1  DAC3_CH2  DAC4_CH1  DAC4_CH2  DAC4_CH1
   --  6    DAC1_CH1  DAC1_CH2  DAC1_CH1  DAC1_CH1  DAC1_CH2  DAC2_CH1  DAC2_CH1
   --  7         PA4       PA5       PF1       PE8      PB10      PD10      PD15
   --  8         PA0       PA2       PC0       PB2      PD13      PB15      PB12
   --  See Table 196: COMPx inverting input assignment at pg 781 chapter 24.3.2
   --  RM0440 rev 6.

   procedure Set_Inverting_Input_Port
     (This  : in out Comparator;
      Input : Inverting_Input_Port)
     with Post => Read_Inverting_Input_Port (This) = Input;
   --  Select the source connected to the inverting input of the comparator.
   --  See Table 196: COMPx inverting input assignment at pg 781 chapter 24.3.2
   --  RM0440 rev 6.

   function Read_Inverting_Input_Port
     (This : Comparator) return Inverting_Input_Port;
   --  Return the source connected to the inverting input of the comparator.

   type NonInverting_Input_Port is
     (Option_1,
      Option_2);
   --  These bits allows to select the source connected to the non-inverting
   --  input of the comparator:
   --  Option  COMP1     COMP2     COMP3     COMP4     COMP5     COMP6     COMP7
   --  1         PA1       PA7       PA0       PB0      PB13      PB11      PB14
   --  2         PB1       PA3       PC1       PE7      PD12      PD11      PD14
   --  See Table 195: COMPx non-inverting input assignment at pg 781 chapter
   --  24.3.2 RM0440 rev 6.

   procedure Set_NonInverting_Input_Port
     (This  : in out Comparator;
      Input : NonInverting_Input_Port)
     with Post => Read_NonInverting_Input_Port (This) = Input;
   --  Select the source connected to the inverting input of the comparator.
   --  See Table 195: COMPx non-inverting input assignment at pg 781 chapter
   --  24.3.2 RM0440 rev 6.

   function Read_NonInverting_Input_Port
     (This : Comparator) return NonInverting_Input_Port;
   --  Return the source connected to the inverting input of the comparator.

   type Output_Polarity is
     (Not_Inverted,
      Inverted);
   --  This bit is used to invert the comparator output.

   procedure Set_Output_Polarity
     (This  : in out Comparator;
      Output : Output_Polarity)
     with Post => Read_Output_Polarity (This) = Output;
   --  Used to invert the comparator output.

   function Read_Output_Polarity (This : Comparator) return Output_Polarity;
   --  Return the comparator output polarity.

   type Comparator_Hysteresis is
     (No_Hysteresis,
      Ten_mV,
      Twenty_mV,
      Thirty_mV,
      Fourty_mV,
      Fifty_mV,
      Sixty_mV,
      Seventy_mV);
   --  These bits select the hysteresis of the comparator.

   procedure Set_Comparator_Hysteresis
     (This  : in out Comparator;
      Value : Comparator_Hysteresis)
     with Post => Read_Comparator_Hysteresis (This) = Value;
   --  Select the comparator hysteresis value.

   function Read_Comparator_Hysteresis (This : Comparator)
     return Comparator_Hysteresis;
   --  Return the comparator hysteresis value.

   type Output_Blanking is
     (No_Blanking,
      Option_2,
      Option_3,
      Option_4,
      Option_5,
      TIM20_OC5,
      TIM15_OC1,
      TIM4_OC3);
   --  These bits select which Timer output controls the comparator output
   --  blanking:
   --  Option  COMP1     COMP2     COMP3     COMP4     COMP5     COMP6     COMP7
   --  2    TIM1_OC5  TIM1_OC5  TIM1_OC5  TIM3_OC4  TIM2_OC3  TIM8_OC5  TIM1_OC5
   --  3    TIM2_OC5  TIM2_OC3  TIM3_OC3  TIM8_OC5  TIM8_OC5  TIM2_OC4  TIM8_OC5
   --  4    TIM3_OC5  TIM3_OC3  TIM2_OC4 TIM15_OC1  TIM3_OC3 TIM15_OC2  TIM3_OC3
   --  5    TIM8_OC5  TIM8_OC5  TIM8_OC5  TIM1_OC5  TIM1_OC5  TIM1_OC5 TIM15_OC2
   --  See Table 197: COMPx blanking sources assignment at pg 783 chapter
   --  24.3.6 RM0440 rev 6.

   procedure Set_Output_Blanking
     (This  : in out Comparator;
      Output : Output_Blanking)
     with Post => Read_Output_Blanking (This) = Output;
   --  Select which Timer output controls the comparator output blanking.

   function Read_Output_Blanking (This : Comparator) return Output_Blanking;
   --  Return which Timer output controls the comparator output blanking.

   procedure Set_Vrefint_Scaler_Resistor
     (This   : in out Comparator;
      Output : Boolean)
     with Post => Read_Vrefint_Scaler_Resistor (This) = Output;
   --  Enables the operation of resistor bridge in the VREFINT scaler. To
   --  disable the resistor bridge, BRGEN bits of all COMP_CxCSR registers must
   --  be set to Disable state. When the resistor bridge is disabled, the 1/4
   --  VREFINT, 1/2 VREFINT, and 3/4 VREFINT inputs of the input selector
   --  receive VREFINT voltage.

   function Read_Vrefint_Scaler_Resistor (This : Comparator) return Boolean;
   --  Return True if VREFINT resistor bridge is enabled.

   procedure Set_Vrefint_Scaler
     (This  : in out Comparator;
      Output : Boolean)
     with Post => Read_Vrefint_Scaler (This) = Output;
   --  Enables the operation of VREFINT scaler at the inverting input of all
   --  comparator. To disable the VREFINT scaler, SCALEN bits of all COMP_CxCSR
   --  registers must be set to Disable state. When the VREFINT scaler is
   --  disabled, the 1/4 VREFINT, 1/2 VREFINT, 3/4 VREFINT and VREFINT inputs
   --  of the multiplexer should not be selected.

   function Read_Vrefint_Scaler (This : Comparator) return Boolean;
   --  Return True if VREFINT scaler is enabled.

   type Comparator_Output is (Low, High);

   function Read_Comparator_Output (This : Comparator) return Comparator_Output;
   --  Read the comparator output before the polarity selector and blanking:
   --  Low = non-inverting input is below inverting input,
   --  High = (non-inverting input is above inverting input

   procedure Set_Lock_Comparator (This : in out Comparator)
     with Post => Read_Lock_Comparator (This) = True;
   --  Allows to have COMPx_CSR register as read-only. It can only be cleared
   --  by a system reset.

   function Read_Lock_Comparator (This : Comparator) return Boolean;
   --  Return the comparator lock bit state.

private

   --  representation for Comparator 2, 4 and 6 Control and Status Registers  ----------

   subtype CxCSR_INMSEL_Field is HAL.UInt3;
   subtype CxCSR_HYST_Field is HAL.UInt3;
   subtype CxCSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type CxCSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : CxCSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : CxCSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : CxCSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CxCSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  representation for the whole Comparator type  -----------------

   type Comparator is record
      CSR : CxCSR_Register;
   end record with Volatile, Size => 1 * 32;

   for Comparator use record
      CSR at 16#00# range  0 .. 31;
   end record;

end STM32.COMP;
