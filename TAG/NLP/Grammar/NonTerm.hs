
--{{{ TESTS

--prop_enumNonTerm a = checkEnum
--    where types = (a::NonTermWrap l) 

-- instance (Language l) => Arbitrary (NonTermWrap l)  where
--     arbitrary = NonTermWrap `liftM` arbitrary
--}}}

-- --{{{ NonTermWrap Classes 
-- instance (Language l) => Show (NonTermWrap l) where
--     show (NonTermWrap nt) = show nt
--     show (NTPOS nt) = show nt

-- instance (Language l) => Eq (NonTermWrap l)
--     (==)  
-- instance (Language l) => Ord (NonTermWrap l)
-- instance (Language l) =>Pretty (NonTermWrap l) where pPrint = text . show  

-- instance (Language l) => Bounded (NonTermWrap l) where
--     minBound = NonTermWrap $ minBound
--     maxBound = NTPOS $ maxBound 

-- instance  (Language l) => Enum (NonTermWrap l) where 
--     fromEnum (NonTermWrap nt) = fromEnum nt
--     fromEnum (NTPOS pos) = fromEnum (maxBound :: NonTerm l) + 1 + fromEnum pos
--     toEnum n = if n > fromEnum (maxBound :: NonTerm l ) then 
--                      NTPOS $ toEnum (n - fromEnum (maxBound :: NonTerm l) -1) 
--                  else NonTermWrap $ toEnum (n ) 


-- --}}} 
