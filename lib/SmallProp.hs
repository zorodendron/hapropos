module SmallProp where

data SmallProp = 
        T | F 
          | P SmallProp
          | Not SmallProp
          | And SmallProp SmallProp
          | Or SmallProp SmallProp
          | Xor SmallProp SmallProp
          | Implies SmallProp SmallProp

data VariantData = VT | VF | VP | VN | VA | VO | VX | VI 
