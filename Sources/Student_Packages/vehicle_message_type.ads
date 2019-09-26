--
-- Danny (u6611178), Australia, September 2019
--

with Ada.Real_Time; use Ada.Real_Time;
with Swarm_Structures_Base; use Swarm_Structures_Base;

package Vehicle_Message_Type is
   -- 6611178 means uninitialised or not specified
   NA : constant Positive := 6611178;

   type Globes_I_Know is record
      Pos : Positions := (0.0, 0.0, 0.0);
      Pos_Time : Time;
   end record;

   type Inter_Vehicle_Messages is record
      Original_Sender : Positive := NA; -- If it's 6611178 means it's not initialised
      -- If Original_Sender is leader or Target_Receiver is the leader, highest prority
      Target_Receiver : Positive := NA;
      -- If it's for me, handle it. If it's for the leader, if newer than local copy, keep passing, otherwise discard.
      Message_Time : Time;
      New_LeaderID : Positive := NA; -- Employ a leader help us better managed
      Planned_Charge : Boolean := False; -- Leader said it's my turn to charge
      Skip_Queue : Boolean := False; -- I tell leader I want to skip queue
      Multi_Globes : Boolean := False;
      Go_Suicide : Boolean := False; -- Leader tell vehicle to die
      Dying : Boolean := False; -- Vehicle tell leader it is dead (If doesn't dead by luck, leader will know from its newer message)
      Globes_Info : Globes_I_Know; -- Only have message time is not enough
   end record;

end Vehicle_Message_Type;
