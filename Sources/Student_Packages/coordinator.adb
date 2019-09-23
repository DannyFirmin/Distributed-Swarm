package body Coordinator is

   -----------------
   -- Coordinator --
   -----------------

   protected body Master is

      procedure reportGlobe (Globe : Positions) is
      begin
         Globe_Location_Setted := True;
         Globe_Location := Globe;
      end reportGlobe;

      function getGlobeLocation return Positions is
      begin
         return Globe_Location;
      end getGlobeLocation;

      function getGlobeLocationSet return Boolean is
      begin
         return Globe_Location_Setted;
      end getGlobeLocationSet;

      entry goCharge(Charger_P : out Positions) when True is
      begin
         Charger_P := Globe_Location;
      end goCharge;


   end Master;

end Coordinator;
