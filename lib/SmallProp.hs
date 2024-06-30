module SmallProp.hs

data SmallProp = 
        T | F 
          | P Prop
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Xor Prop Prop
          | Implies Prop Prop

data VariantType = SPT | SPF | SPP | SPNot | SPAnd | SPOr | SPXor | SPImplies 

smallPropToDepthVector :: SmallProp -> Vector Int
smallPropToDepthVector T = [0]
smallPropToDepthVector F = [0]
smallPropToDepthVector (P prop)              = 0 : smallPropToDepthVector prop
smallPropToDepthVector (Not prop)            = 0 : smallPropToDepthVector prop
smallPropToDepthVector (And propA propB)     = 0 : [smallPropToDepthVector propA, smallPropToDepthVector propB]
smallPropToDepthVector (Or propA propB)      = 0 : [smallPropToDepthVector propA, smallPropToDepthVector propB]
smallPropToDepthVector (Xor propA propB)     = 0 : [smallPropToDepthVector propA, smallPropToDepthVector propB]
smallPropToDepthVector (Implies propA propB) = 0 : [smallPropToDepthVector propA, smallPropToDepthVector propB]

-- TODO
-- smallPropDVectorTypeInf :: SmallProp -> Vector (Int, VariantType)
