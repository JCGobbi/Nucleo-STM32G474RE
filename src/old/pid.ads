package PID is
   --  This package defines a very simple implementation of a conventional PID
   --  controller.

   type Kpid is tagged record
      Kp       : Float; --  Proportional gain.
      Ki       : Float; --  Integral gain.
      Kd       : Float; --  Derivative gain.
      Integral : Float; --  Holds the error integrated over time.
      E_Prev   : Float; --  Holds the previous error value.
      Output   : Float; --  Holds the latest calculated control output.
   end record;
   --  A PID object where the gain parameters are defined such that
   --  Output = Kp*e + Ki*Integral(e) + Kd*de/dt

   function Max (X, Y : in Float) return Float;
   --  Returns the maximum value.

   function Min (X, Y : in Float) return Float;
   --  Returns the minimum value.

   procedure Saturate
      (X       : in out Float;
       Maximum : in Float;
       Is_Sat  : out Boolean);
   --  Limits the magnitude of X to be <= than Maximum.

   function Saturate
      (X       : in Float;
       Maximum : in Float;
       Minimum : in Float) return Float;
   --  Limits the input X such that Minimum <= X_out <= Maximum.

   function Compose (Kp, Ki, Kd : in Float) return Kpid;
   --  Create and reset a controller to use the specified gain parameters.

   procedure Update (This     : in out Kpid;
                     Setpoint : in Float; --  The current set-point value.
                     Actual   : in Float; --  The current actual value.
                     Ts       : in Float; --  The time since last update.
                     Is_Sat   : in Boolean := False); --  An optional parameter used by the anti-windup. Will stop integration if set True.
   --  Update the controller output.

   function Get_Output (This : in Kpid) return Float;
   --  Get the latest calculated control output.

end PID;
