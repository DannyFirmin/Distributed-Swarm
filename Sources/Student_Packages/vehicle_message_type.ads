-- Suggestions for packages which might be useful:

with Ada.Real_Time; use Ada.Real_Time;
--  with Swarm_Size; use Swarm_Size;
--  with Vectors_3D; use Vectors_3D;
with Swarm_Structures_Base; use Swarm_Structures_Base;

package Vehicle_Message_Type is

   -- Replace this record definition by what your vehicles need to communicate.
   -- 6611178 means none or invalid or not specified
   type Inter_Vehicle_Messages is record
      Original_Sender : Positive := 6611178; -- If it's 6611178 means it's not a real message. Just create it locally somewhere.
      -- If Original_Sender is manager or Target_Receiver is the leader, highest prority
      Target_Receiver : Positive := 6611178; -- 6611178 is to nobody, ignore.
      -- If it's for me, handle it. If it's for the leader, if newer than local copy, keep passing, otherwise discard.
      Target_Group : Positive := 6611178;
      Manager_ID : Positive := 6611178;
      Turn_Back : Boolean := False; -- Tell the leader to turn back
      Message_Time : Time;
      Predecessor_Positions : Positions;
      Globe_Update_Time : Time;
      Globe_Positions : Positions := (0.0, 0.0, 0.0);

   end record;

end Vehicle_Message_Type;
