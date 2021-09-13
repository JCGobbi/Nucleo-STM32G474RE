
--  This file provides interfaces for the operational amplifiers on the
--  STM32G4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

with System; use System;

package STM32.OPAMP is

   type Operational_Amplifier is limited private;

   procedure Enable (This : in out Operational_Amplifier)
     with Post => Enabled (This);

   procedure Disable (This : in out Operational_Amplifier)
     with Post => not Enabled (This);

   function Enabled (This : Operational_Amplifier) return Boolean;

   type NI_Input_Mode is (Normal_Mode, Calibration_Mode);

   procedure Set_NI_Input_Mode
     (This  : in out Operational_Amplifier;
      Input : NI_Input_Mode)
     with Post => Read_NI_Input_Mode (This) = Input;
   --  Select a calibration reference voltage on non-inverting input and
   --  disables external connections.

   function Read_NI_Input_Mode
     (This : Operational_Amplifier) return NI_Input_Mode;
   --  Return the source connected to the non-inverting input of the
   --  operational amplifier.

   type NI_Input_Port is (VINP0, VINP1, VINP2, Option_4);
   --  These bits allows to select the source connected to the non-inverting
   --  input of the operational amplifier. The first 3 options are common, the
   --  last option change for each OPAMP:
   --  Option  OPAMP1    OPAMP2    OPAMP3    OPAMP4    OPAMP5    OPAMP6
   --  4     DAC3_CH1     VIMP3  DAC3_CH2  DAC4_CH1  DAC4_CH2  DAC3_CH1

   procedure Set_NI_Input_Port
     (This  : in out Operational_Amplifier;
      Input : NI_Input_Port)
     with Post => Read_NI_Input_Port (This) = Input;
   --  Select the source connected to the non-inverting input of the
   --  operational amplifier.

   function Read_NI_Input_Port
     (This : Operational_Amplifier) return NI_Input_Port;
   --  Return the source connected to the non-inverting input of the
   --  operational amplifier.

   type I_Input_Port is
     (VINM0,
      VINM1,
      Feedback_Resistor_PGA_Mode,
      Follower_Mode);

   procedure Set_I_Input_Port
     (This  : in out Operational_Amplifier;
      Input : I_Input_Port)
     with Post => Read_I_Input_Port (This) = Input;
   --  Select the source connected to the inverting input of the
   --  operational amplifier.

   function Read_I_Input_Port
     (This : Operational_Amplifier) return I_Input_Port;
   --  Return the source connected to the inverting input of the
   --  operational amplifier.

   type NI_Secondary_Input_Port is (VINP0, VINP1, VINP2, Option_4);
   --  These bits allows to select the source connected to the non-inverting
   --  input of the operational amplifier. The first 3 options are common, the
   --  last option change for each OPAMP:
   --  Option  OPAMP1    OPAMP2    OPAMP3    OPAMP4    OPAMP5    OPAMP6
   --  4     DAC3_CH1     VIMP3  DAC3_CH2  DAC4_CH1  DAC4_CH2  DAC3_CH1

   procedure Set_NI_Secondary_Input_Port
     (This  : in out Operational_Amplifier;
      Input : NI_Secondary_Input_Port)
     with Post => Read_NI_Secondary_Input_Port (This) = Input;
   --  Select the secondary source connected to the non-inverting input
   --  of the operational amplifier when the controlled mux mode is enabled
   --  (T1CM_EN = 1 or T8CM_EN = 1 or T20CM_EN = 1).

   function Read_NI_Secondary_Input_Port
     (This : Operational_Amplifier) return NI_Secondary_Input_Port;
   --  Return the secondary source connected to the non-inverting input
   --  of the operational amplifier.

   type I_Secondary_Input_Port is
     (VINM0_Or_Feedback_Resistor_PGA_Mode,
      VINM1_Or_Follower_Mode);
   --  When standalone mode is used (i.e. VM_SEL = “00” or “01”):
   --  0: Input from VINM0
   --  1: Input from VINM1
   --  When PGA (VM_SEL = “10”) or Follower mode (VM_SEL = “11”) is used:
   --  0: Resistor feedback output selected (PGA mode)
   --  1: VOUT selected as input minus (follower mode)

   procedure Set_I_Secondary_Input_Port
     (This  : in out Operational_Amplifier;
      Input : I_Secondary_Input_Port)
     with Post => Read_I_Secondary_Input_Port (This) = Input;
   --  Select the secondary source connected to the inverting input of the
   --  operational amplifier when the controlled mux mode is enabled
   --  (T1CM_EN = 1 or T8CM_EN = 1 or T20CM_EN = 1).

   function Read_I_Secondary_Input_Port
     (This : Operational_Amplifier) return I_Secondary_Input_Port;
   --  Return the secondary source connected to the inverting input of the
   --  operational amplifier.

   type Input_Mux_Mode is (Manual, Automatic);
   --  Timer controlled mux mode.

   type Input_Mux_Timer is (TIM1, TIM8, TIM20);
   --  Timer that constrols the mux mode.

   procedure Set_Input_Mux_Mode
     (This  : in out Operational_Amplifier;
      Timer : Input_Mux_Timer;
      Mode  : Input_Mux_Mode)
     with Post => Read_Input_Mux_Mode (This, Timer) = Mode;
   --  Select automatically the switch between the default selection
   --  (VP_SEL and VM_SEL) and the secondary selection (VPS_SEL and VMS_SEL)
   --  of the inverting and non inverting inputs of the operational amplifier.
   --  This automatic switch is triggered by the TIMx CC6 output arriving on

   function Read_Input_Mux_Mode
     (This  : Operational_Amplifier;
      Timer : Input_Mux_Timer) return Input_Mux_Mode;
   --  Return the selection of the selection between the default and the
   --  secondary inputs of the inverting and non inverting inputs of the
   --  operational amplifier.

   type Calibration_Mode_On is (Disabled, Enabled);
   --  Enable/disable the calibration mode.

   procedure Set_Calibration_Mode
     (This  : in out Operational_Amplifier;
      Input : Calibration_Mode_On)
     with Post => Read_Calibration_Mode (This) = Input;
   --  Select the calibration mode connecting VM and VP to the OPAMP
   --  internal reference voltage.

   function Read_Calibration_Mode
     (This : Operational_Amplifier) return Calibration_Mode_On;
   --  Return the calibration mode.

   type Calibration_Value is
     (VREFOPAMP_Is_3_3_VDDA, --  3.3%
      VREFOPAMP_Is_10_VDDA, --  10%
      VREFOPAMP_Is_50_VDDA, --  50%
      VREFOPAMP_Is_90_VDDA --  90%
      );
   --  Offset calibration bus to generate the internal reference voltage.

   procedure Set_Calibration_Value
     (This  : in out Operational_Amplifier;
      Input : Calibration_Value)
     with Post => Read_Calibration_Value (This) = Input;
   --  Select the offset calibration bus used to generate the internal
   --  reference voltage when CALON = 1 or FORCE_VP = 1.

   function Read_Calibration_Value
     (This : Operational_Amplifier) return Calibration_Value;
   --  Return the offset calibration bus voltage.

   type PGA_Mode_Gain is
     (NI_Gain_2,
      NI_Gain_4,
      NI_Gain_8,
      NI_Gain_16,
      NI_Gain_32,
      NI_Gain_64,
      I_Gain_1_NI_Gain_2_VINM0,
      I_Gain_3_NI_Gain_4_VINM0,
      I_Gain_7_NI_Gain_8_VINM0,
      I_Gain_15_NI_Gain_16_VINM0,
      I_Gain_31_NI_Gain_32_VINM0,
      I_Gain_63_NI_Gain_64_VINM0,
      NI_Gain_2_Filtering_VINM0,
      NI_Gain_4_Filtering_VINM0,
      NI_Gain_8_Filtering_VINM0,
      NI_Gain_16_Filtering_VINM0,
      NI_Gain_32_Filtering_VINM0,
      NI_Gain_64_Filtering_VINM0,
      I_Gain_1_NI_Gain_2_VINM0_VINM1,
      I_Gain_3_NI_Gain_4_VINM0_VINM1,
      I_Gain_7_NI_Gain_8_VINM0_VINM1,
      I_Gain_15_NI_Gain_16_VINM0_VINM1,
      I_Gain_31_NI_Gain_32_VINM0_VINM1,
      I_Gain_63_NI_Gain_64_VINM0_VINM1)
     with Size => 5;
   --  Gain in PGA mode.

   for PGA_Mode_Gain use
     (NI_Gain_2                        => 2#00000#,
      NI_Gain_4                        => 2#00001#,
      NI_Gain_8                        => 2#00010#,
      NI_Gain_16                       => 2#00011#,
      NI_Gain_32                       => 2#00100#,
      NI_Gain_64                       => 2#00101#,
      I_Gain_1_NI_Gain_2_VINM0         => 2#01000#,
      I_Gain_3_NI_Gain_4_VINM0         => 2#01001#,
      I_Gain_7_NI_Gain_8_VINM0         => 2#01010#,
      I_Gain_15_NI_Gain_16_VINM0       => 2#01011#,
      I_Gain_31_NI_Gain_32_VINM0       => 2#01100#,
      I_Gain_63_NI_Gain_64_VINM0       => 2#01101#,
      NI_Gain_2_Filtering_VINM0        => 2#10000#,
      NI_Gain_4_Filtering_VINM0        => 2#10001#,
      NI_Gain_8_Filtering_VINM0        => 2#10010#,
      NI_Gain_16_Filtering_VINM0       => 2#10011#,
      NI_Gain_32_Filtering_VINM0       => 2#10100#,
      NI_Gain_64_Filtering_VINM0       => 2#10101#,
      I_Gain_1_NI_Gain_2_VINM0_VINM1   => 2#11000#,
      I_Gain_3_NI_Gain_4_VINM0_VINM1   => 2#11001#,
      I_Gain_7_NI_Gain_8_VINM0_VINM1   => 2#11010#,
      I_Gain_15_NI_Gain_16_VINM0_VINM1 => 2#11011#,
      I_Gain_31_NI_Gain_32_VINM0_VINM1 => 2#11100#,
      I_Gain_63_NI_Gain_64_VINM0_VINM1 => 2#11101#);

   procedure Set_PGA_Mode_Gain
     (This  : in out Operational_Amplifier;
      Input : PGA_Mode_Gain)
     with Post => Read_PGA_Mode_Gain (This) = Input;
   --  Select the gain in PGA mode.

   function Read_PGA_Mode_Gain
     (This : Operational_Amplifier) return PGA_Mode_Gain;
   --  Return the gain in PGA mode.

   type User_Trimming is (Disabled, Enabled);
   --  Enable/disable user trimming.

   procedure Set_User_Trimming
     (This  : in out Operational_Amplifier;
      Input : User_Trimming)
     with Post => Read_User_Trimming (This) = Input;
   --  Allows to switch from ‘factory’ AOP offset trimmed values to ‘user’ AOP
   --  offset trimmed values.

   function Read_User_Trimming
     (This : Operational_Amplifier) return User_Trimming;
   --  Return the state of user trimming.

   type Differential_Pair is (NMOS, PMOS);

   procedure Set_Offset_Trimming
     (This  : in out Operational_Amplifier;
      Pair  : Differential_Pair;
      Input : UInt5)
     with Post => Read_Offset_Trimming (This, Pair) = Input;
   --  Select the offset trimming value for NMOS or PMOS.

   function Read_Offset_Trimming
     (This : Operational_Amplifier;
      Pair : Differential_Pair) return UInt5;
   --  Return the offset trimming value for NMOS or PMOS.

   procedure Calibrate (This : in out Operational_Amplifier);
   --  Calibrate the NMOS and PMOS differential pair. This routine
   --  is described in the RM0440 rev 6 pg. 797. The offset trim time,
   --  during calibration, must respect the minimum time needed
   --  between two steps to have 1 mV accuracy.
   --  This routine must be executed first with normal speed mode, then with
   --  high-speed mode, if used.

   type Internal_Output is
     (Is_Output,
      Is_Not_Output);

   procedure Set_Internal_Output
     (This  : in out Operational_Amplifier;
      Input : Internal_Output)
     with Post => Read_Internal_Output (This) = Input;
   --  Connect the internal OPAMP output to output pin or internally to an ADC
   --  channel and disconnected from the output pin.

   function Read_Internal_Output
     (This : Operational_Amplifier) return Internal_Output;
   --  Return the internal output reference voltage state.

   type Speed_Mode is (Normal_Mode, HighSpeed_Mode);

   procedure Set_Speed_Mode
     (This  : in out Operational_Amplifier;
      Input : Speed_Mode)
     with Pre => not Enabled (This),
       Post => Read_Speed_Mode (This) = Input;
   --  OPAMP in normal or high-speed mode.

   function Read_Speed_Mode
     (This : Operational_Amplifier) return Speed_Mode;
   --  Return the OPAMP speed mode.

   type Output_Status_Flag is
     (NI_Lesser_Then_I,
      NI_Greater_Then_I);

   function Read_Output_Status_Flag
     (This : Operational_Amplifier) return Output_Status_Flag;
   --  Return the output status flag when the OPAMP is used as comparator
   --  during calibration.

   procedure Set_Lock_OpAmp (This : in out Operational_Amplifier)
     with Post => Read_Lock_OpAmp (This) = True;
   --  Allows to have OPAMPx_CSR register as read-only. It can only be cleared
   --  by a system reset.

   function Read_Lock_OpAmp (This : Operational_Amplifier) return Boolean;
   --  Return the OPAMP lock bit state.

private

   --  representation for OpAmp x Control and Status Registers  ----------

   subtype OPAMPx_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMPx_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMPx_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMPx_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMPx_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMPx_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMPx control/status register
   type OPAMPx_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMPx_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMPx_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMPx_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMPx_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMPx_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMPx_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMPx_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMPx_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMPx control/status register
   type OPAMPx_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMPx_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMPx_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   --  representation for the whole Operationa Amplifier type  -----------------

   type Operational_Amplifier is limited record
      CSR  : OPAMPx_CSR_Register;
      TCMR : OPAMPx_TCMR_Register;
   end record with Volatile, Size => 7 * 32;

   for Operational_Amplifier use record
      CSR  at 16#00# range  0 .. 31;
      TCMR at 16#18# range  0 .. 31;
   end record;

end STM32.OPAMP;
