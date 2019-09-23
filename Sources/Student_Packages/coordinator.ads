with Swarm_Structures_Base; use Swarm_Structures_Base;
with Ada.Text_IO; use Ada.Text_IO;
package Coordinator is

   protected Master is
      procedure reportGlobe (Globe:Positions);
      function getGlobeLocation return Positions;
      function getGlobeLocationSet return Boolean;
      entry goCharge (Charger_P : out Positions);
   private
      Globe_Location : Positions;
      Globe_Location_Setted : Boolean := False;
   end Master;
   
end Coordinator;
