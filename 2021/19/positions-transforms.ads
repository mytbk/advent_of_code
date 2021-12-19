package Positions.Transforms is
   -- (x,y,z)
   function Transform_1(P: Position) return Position is (P);

   -- (x,-y,-z)
   function Transform_2(P: Position) return Position is ((P.X, -P.Y, -P.Z));

   -- (x,z,-y)
   function Transform_3(P: Position) return Position is ((P.X, P.Z, -P.Y));

   -- (x,-z,y)
   function Transform_4(P: Position) return Position is ((P.X, -P.Z, P.Y));

   -- (-x,y,-z)
   function Transform_5(P: Position) return Position is ((-P.X, P.Y, -P.Z));
   
   -- (-x,-y,z)
   function Transform_6(P: Position) return Position is ((-P.X,-P.Y,P.Z));
   
   -- (-x,z,y)
   function Transform_7(P: Position) return Position is ((-P.X,P.Z,P.Y));
   
   -- (-x,-z,-y)
   function Transform_8(P: Position) return Position is ((-P.X,-P.Z,-P.Y));
   
   -- (y,x,-z)
   function Transform_9(P: Position) return Position is ((P.Y,P.X,-P.Z));
   
   -- (y,-x,z)
   function Transform_10(P: Position) return Position is ((P.Y,-P.X,P.Z));
   
   -- (y,z,x)
   function Transform_11(P: Position) return Position is ((P.Y,P.Z,P.X));
   
   -- (y,-z,-x)
   function Transform_12(P: Position) return Position is ((P.Y,-P.Z,-P.X));
   
   -- (-y,x,z)
   function Transform_13(P: Position) return Position is ((-P.Y,P.X,P.Z));
   
   -- (-y,-x,-z)
   function Transform_14(P: Position) return Position is ((-P.Y,-P.X,-P.Z));
   
   -- (-y,z,-x)
   function Transform_15(P: Position) return Position is ((-P.Y,P.Z,-P.X));
   
   -- (-y,-z,x)
   function Transform_16(P: Position) return Position is ((-P.Y,-P.Z,P.X));
   
   -- (z,x,y)
   function Transform_17(P: Position) return Position is ((P.Z,P.X,P.Y));
   
   -- (z,-x,-y)
   function Transform_18(P: Position) return Position is ((P.Z,-P.X,-P.Y));
   
   -- (z,y,-x)
   function Transform_19(P: Position) return Position is ((P.Z,P.Y,-P.X));
   
   -- (z,-y,x)
   function Transform_20(P: Position) return Position is ((P.Z,-P.Y,P.X));
   
   -- (-z,x,-y)
   function Transform_21(P: Position) return Position is ((-P.Z,P.X,-P.Y));
   
   -- (-z,-x,y)
   function Transform_22(P: Position) return Position is ((-P.Z,-P.X,P.Y));
   
   -- (-z,y,x)
   function Transform_23(P: Position) return Position is ((-P.Z,P.Y,P.X));
   
   -- (-z,-y,-x)
   function Transform_24(P: Position) return Position is ((-P.Z,-P.Y,-P.X));
   
   type Transform_Function is access function(P: Position) return Position;
   type Transform_Type is range 1 .. 24;
   
   All_Transforms: constant array (Transform_Type) of Transform_Function :=
     (Transform_1'Access, Transform_2'Access, Transform_3'Access, Transform_4'Access,
      Transform_5'Access, Transform_6'Access, Transform_7'Access, Transform_8'Access,
      Transform_9'Access, Transform_10'Access, Transform_11'Access, Transform_12'Access,
      Transform_13'Access, Transform_14'Access, Transform_15'Access, Transform_16'Access,
      Transform_17'Access, Transform_18'Access, Transform_19'Access, Transform_20'Access,
      Transform_21'Access, Transform_22'Access, Transform_23'Access, Transform_24'Access);
end Positions.Transforms;
