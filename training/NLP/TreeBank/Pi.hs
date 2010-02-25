
type Test m = (m String, m Int)
wrap :: (a -> m a) -> (String, Int) -> Test m
wrap fm (s, i) = (fm s, fm i) 



