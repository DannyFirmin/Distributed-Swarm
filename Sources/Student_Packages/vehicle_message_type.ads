--
-- Danny (u6611178), Australia, September 2019
--

with Ada.Real_Time; use Ada.Real_Time;
-- with Swarm_Size; use Swarm_Size;
--  with Vectors_3D; use Vectors_3D;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
package Vehicle_Message_Type is
   NA : constant Positive := 6611178;
   -- Replace this record definition by what your vehicles need to communicate.
   -- 6611178 means none or invalid or not specified

   package Positive_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Positive);
   use Positive_Sets;

   type Inter_Vehicle_Messages is record
      Original_Sender : Positive := NA; -- If it's 6611178 means it's not a real message. Just create it locally somewhere.
      -- If Original_Sender is leader or Target_Receiver is the leader, highest prority
      Target_Receiver : Positive := NA; -- 6611178 is to nobody, ignore.
      -- If it's for me, handle it. If it's for the leader, if newer than local copy, keep passing, otherwise discard.
      Message_Time : Time;
      New_LeaderID : Positive := NA; -- Employ a leader help us better managed
      Planned_Charge : Boolean := False; -- Leader said it's my turn to charge
      Skip_Queue : Boolean := False;
      Double_Globe : Boolean := False;
      Exile_To_Another_Globe : Boolean := False; -- When found another globe, leader will exile someone there. They will form their own new leader.
      Go_Suicide : Boolean := False; -- Leader tell vehicle to die
      Dying : Boolean := False; -- Vehicle tell leader it is dead (If doesn't dead by luck, leader will know from its newer message)
      Globe_Update_Time : Time; -- One message time is not enough,
      Globe_Positions : Positions := (0.0, 0.0, 0.0);
      Followers : Positive_Sets.Set;
   end record;

end Vehicle_Message_Type;
