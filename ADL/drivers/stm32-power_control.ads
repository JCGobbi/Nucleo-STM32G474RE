------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with STM32_SVD.PWR;       use STM32_SVD.PWR;

package STM32.Power_Control is

   procedure Enable;
   --  Enable power control module

   procedure Disable_Backup_Domain_Protection;
   procedure Enable_Backup_Domain_Protection;

   type Wakeup_Pin is (WKUP_1, WKUP_2, WKUP_3, WKUP_4, WKUP_5);
   type Wakeup_Pin_List is array (Wakeup_Pin) of Wakeup_Pin;

   procedure Enable_Wakeup_Pin (Pin : Wakeup_Pin);
   --  When enabled, the wakeup pin (PA0) is used for wakeup from Standby mode
   --  and forced in input pull down configuration (rising edge on WKUP pin
   --  wakes-up the system from Standby mode).
   --  When disabled, the wakeup  pin is used for general purpose I/O. An event
   --  on the wakeup  pin does not wakeup the device from Standby mode.

   function Wakeup_Flag (Pin : Wakeup_Pin) return Boolean;
   --  This flag is set by hardware and cleared either by a system reset or by
   --  calling the Clear_Wakeup_Flag procedure.

   procedure Clear_Wakeup_Flag (Pin : Wakeup_Pin);
   procedure Clear_Wakeup_Flag (Pins : Wakeup_Pin_List);

   function Standby_Flag return Boolean;
   --  This flag is set by hardware and cleared only by a POR/PDR (power-on
   --  reset/power-down reset) or by calling the Clear_Standby_Flag procedure.

   procedure Clear_Standby_Flag;

   type Low_Power_Mode is
     (Stop_0,
      Stop_1,
      Standby,
      Shutdown);

   for Low_Power_Mode use
     (Stop_0   => 2#000#,
      Stop_1   => 2#001#,
      Standby  => 2#011#,
      Shutdown => 2#100#);

   procedure Set_Power_Down_Deepsleep (Enabled : Boolean := True);
   --  When enabled, the MCU enters Standby mode when CPU enters deepsleep.
   --  When disabled, the MCU enters Stop mode when CPU enters deepsleep.

   procedure Set_Low_Power_Deepsleep (Enabled : Boolean := True);
   --  When enabled, the voltage regulator is in low-power during MCU Stop mode.
   --  When disabled, the voltage regulator is on during MCU Stop mode.

   procedure Enter_Standby_Mode
     with No_Return;
   --  Clear the wakeup and standby flag, set the power-down on CPU deep sleep
   --  and trigger MCU deep sleep.
   --  MCU gets out of standby with a reset, so this procedure does not return.

end STM32.Power_Control;
