with HAL; use HAL;

with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;
with STM32.PWM;    use STM32.PWM;
with STM32.CORDIC; use STM32.CORDIC;

with STM_Board;    use STM_Board;

package Inverter_PWM is

   -----------------
   -- Definitions --
   -----------------

   type PWM_Phase is (A, B);
   --  Each phase of a full bridge circuit.

   type PWM_Alignment is
      (Edge, --  Positive edge
       Center --  Center of positive part
      );
   --  Describes where on the PWM waveform the signals shall be aligned.

   --  The final maximum amplitude for the sine voltage is defined by the
   --  maximum sine value, that is 1.0.
   --  Considering that the battery nominal voltage is 12 Volts, this will
   --  be the peak AC value, which corresponds to a primary AC RMS voltage
   --  of 12 V / sqrt(2) = 8.485 V.
   --  With a minimum battery voltage of 10 V, the minimum AC RMS voltage
   --  will be 10 V / sqrt(2) = 7.07 V.
   --  The transformer voltage ratio between the primary and secondary, for
   --  a maximum output voltage of 230 V RMS, will be 230 V / 7.07 V = 32.5,
   --  so the turns ratio of the transformer will be (Ns / Np) = 33.

   subtype Sine_Range is Float range 0.0 .. 1.0;
   Sine_Amplitude : Sine_Range := 0.0;

   subtype Gain_Range is Float range 0.0 .. 1.0;
   --  For correcting battery voltage and AC output variation.
   Sine_Gain : Gain_Range := 0.0;

   subtype Duty_Cycle is Float range 0.0 .. 100.0;

   --  The upload frequency of the duty cycle is defined by the number of points
   --  for each semi-sinusoid.
   --  For 50 Hz we have 2 half senoids * 50 Hz * 250 points = 25000 Hz.
   --  For 60 Hz we have 2 half senoids * 60 Hz * 250 points = 30000 Hz.
   --  For 400 Hz we have 2 half senoids * 400 Hz * 250 points = 200000 Hz.

   PWM_Frequency_Hz : Frequency_Hz := 30_000.0; -- for 60 Hz

   --  Actually the STM32G474 operates at 150 MHz with 150 MHz into Prescaler.
   --  With (10 - 1) for prescaler we have 15 MHz for counter period, that has
   --  values of 600, 500 and 75 for 25, 30 and 200 KHz.

   subtype Deadtime_Range is Float range 0.0 .. 400.0e-9;
   --  Maximum deadtime permissible is 126 us.
   --  Maximum deadtime chosen is 1% of the PWM_Frequency_Hz = 0.01/25_000.
   PWM_Deadtime : constant Deadtime_Range := 166.7e-9;
   --  The delay exists in the rising edges.
   --  It depends on the electronic circuit rise and fall times.
   --  166.7e-9 * 30 kHz * 100 = 0.5% of the total period.

   subtype Sine_Step_Range is Positive range 1 .. 256;
   --  Number of steps for the half sine table.

   --  A table for sine generation is produced knowing the number of points
   --  to complete 1/2 sine period. The sine function goes from 0 to 1 to 0 in
   --  1/2 sine period, that corresponds to 0 to Pi/2 to Pi.
   --  The equation which defines each point is:
   --
   --  D = A * sin(pi * x/N)
   --  D = Duty cycle at a given discrete point;
   --  A = Signal amplitude of the maximum duty cycle. We adopt 1.
   --  pi = 1/2 of the sine period
   --  x = Step number
   --  N = Number of points = 256

   --  The STM32F474 CPU has hardware acceleration of mathematical functions
   --  (mainly trigonometric ones), so we benefit of it with sine calculations
   --  and, instead of using a sine table, we calculate it directly.
   --  The values introduced into the CORDIC doesn't need the Pi multiplication,
   --  so Pi corresponds to 1.0 and -Pi corresponds to -1.0. See the definition
   --  for Fraction_16 and Fraction_32 in the stm32-cordic.ads file.
   --  The only limitation is that any value introduced into the CORDIC must be
   --  a multiple of 2**(-31) when using Fraction_32 or 2**(-15) when using
   --  Fraction_16.

   Increment : constant Fraction_16 := 1.0 / Sine_Step_Range'Last;
   --  This value must be a multiple of delta (2.0**(-15)).
   --  The Increment value determine the number of points to complete 1/2
   --  sine period. The complete sinusoid or sine period is completed with
   --  these same points but using the second half-bridge, so it will be 512
   --  points.

   --  For sine function, the first argument is the angle, while the second
   --  argument is the modulus, that in this case doesn't change.
   Angle : Fraction_16;
   Modulus : constant UInt16 := 16#7FFF#; --  1 - 2**(-31)

   --  The initial angle must be the first point after 0, and the last point
   --  will be 0.
   Initial_Angle : constant Fraction_16 := Increment;

   Data_In : Block_16 := (Fraction_16_To_UInt16 (Initial_Angle), Modulus);
   Data_Out : Block_16 := (0, 0);

   -----------------------------
   -- Procedures and function --
   -----------------------------

   procedure Initialize_CORDIC;
   --  Enable clock and configure CORDIC coprocessor with sine function.

   procedure Initialize_PWM
      (Frequency : Frequency_Hz;
       Deadtime  : Deadtime_Range;
       Alignment : PWM_Alignment);
   --  Initialize the timer peripheral for PWM.
   --  Each phase needs to be enabled manually after this.

   procedure Enable_Phase (This : PWM_Phase)
   with inline;
   --  Enable PWM generation for the specified phase.

   procedure Disable_Phase (This : PWM_Phase)
   with inline;
   --  Disable PWM generation for the specified phase.

   procedure Start_PWM
   with
      Pre => Is_Initialized;
   --  Start the generation of sinusoid wave by enabling interrupt.

   procedure Stop_PWM
   with
      Pre => Is_Initialized;
   --  Stop the generation of sinusoid wave by disabling interrupt.

   function Get_Duty_Resolution return Duty_Cycle;
   --  Return the minimum step that the duty can be changed, in percent.

   procedure Set_Duty_Cycle
      (This  : PWM_Phase;
       Value : Duty_Cycle);
   --  Sets the duty cycle in percent for the specified phase.

   procedure Set_Duty_Cycle
      (This      : PWM_Phase;
       Amplitude : Sine_Range;
       Gain      : Gain_Range);
   --  Sets the duty cycle for the specified phase.

   procedure Set_PWM_Gate_Power (Enabled : in Boolean)
   with
      Pre => (if Enabled = False then STM_Board.Is_Initialized
              else Is_Initialized and STM_Board.Is_Initialized);
   --  Enable or disable the output of the gate drivers.

   procedure Reset_Sine_Step;
   --  Set the sine table step counter to the last position of the
   --  table, or 250, whose amplitude value is 0.

   procedure Safe_State;
   --  Forces the inverter into a state that is considered safe.
   --  Typically this disables the PWM generation (all switches off), and
   --  turns off the power to the gate drivers.

   function Is_Initialized return Boolean;
   --  Returns True if the board specifics are initialized.

private

   Initialized : Boolean := False;

   Sine_Step : Sine_Step_Range := 250;
   --  The table last step value is 0.

   PWM_Timer_Ref : access Timer := PWM_Timer'Access;

   Modulators : array (PWM_Phase'Range) of PWM_Modulator;

   type Gate_Setting is record
      Channel   : Timer_Channel;
      Pin_H     : GPIO_Point;
      Pin_L     : GPIO_Point;
      Pin_AF    : STM32.GPIO_Alternate_Function;
   end record;

   type Gate_Settings is array (PWM_Phase'Range) of Gate_Setting;

   Gate_Phase_Settings : constant Gate_Settings :=
      ((A) => Gate_Setting'(Channel => PWM_A_Channel,
                            Pin_H   => PWM_A_H_Pin,
                            Pin_L   => PWM_A_L_Pin,
                            Pin_AF  => PWM_A_GPIO_AF),
       (B) => Gate_Setting'(Channel => PWM_B_Channel,
                            Pin_H   => PWM_B_H_Pin,
                            Pin_L   => PWM_B_L_Pin,
                            Pin_AF  => PWM_B_GPIO_AF));

   protected PWM_Handler is
      pragma Interrupt_Priority (PWM_ISR_Priority);

   private

      Counter : Integer := 0;
      --  For testing the output.

      Semi_Senoid : Boolean := False;
      --  Defines False = 1'st half sinusoid, True = 2'nd half sinusoid.

      procedure PWM_ISR_Handler with
        Attach_Handler => PWM_Interrupt;

   end PWM_Handler;

end Inverter_PWM;
