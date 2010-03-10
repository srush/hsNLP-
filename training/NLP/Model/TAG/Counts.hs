module NLP.Model.TAG.Counts where 

--{{{  Imports 
import Prelude hiding (catch)
import NLP.Grammar.TAG
import NLP.Model.TAG.Parse 
import NLP.ChartParse.Eisner.Inside
import NLP.ChartParse.Eisner.Outside
import Data.Semiring.Derivation
import NLP.TreeBank.TreeBank
import Control.Exception
import NLP.Model.Distance
import Data.Semiring.ViterbiNBestDerivation
import Debug.Trace
import Helpers.Common
import NLP.Model.TAG.Format
import NLP.Model.CreateableSemi
import NLP.Model.TAG.Adjunction as ADJ
import NLP.Language.SimpleLanguage
import NLP.Probability.Chain
import NLP.Model.TAG.Wrap
import NLP.WordLattice
import NLP.ParseMonad
import NLP.Model.ParseState
import NLP.Grammar.Dependency    
import Helpers.Test
import Control.Monad.Trans
import qualified Data.Map as M
--}}}

readTAG :: String -> IO (ParseMonad TSentence) 
readTAG f = do 
  sent <- readSentence f
  return $ sent >>= toTAGDependency  

readSent :: String -> IO (ParseMonad WordInfoSent)
readSent f =  readSentence f

showCount f obs = do
   t <- readTAG f
   let c = t >>= countTAG obs
   return c
   --putStrLn $ render $ pPrint $ fst $ c

type TAGCountState monoid = AdjState TWord Collins (TAGCountSemi monoid)
type TAGCountSemi monoid = CD monoid Collins
 
type BasicOpts monoid = ParseOpts Collins (TAGCountSemi monoid)  

observation e c =  observe $ chainRule e c
testing e c = case childInd e of  
                Nothing -> (M.singleton (adjSide c, spinePos c, parentInd c) (e,c), M.empty)
                (Just cind) -> (M.empty, M.singleton (cind, parentInd c) (e,c))   

type Collector monoid = FullEvent Collins -> FullContext Collins -> monoid 

makeParseOpts ::  TSentence -> Collector monoid -> ParseMonad (BasicOpts monoid, BasicOpts monoid)
makeParseOpts dsent obs = do 
  left <- mkDistCacheLeft sent
  right <- mkDistCacheRight sent
  return $ 
    (opts{ distanceCache = left } , 
     opts{ distanceCache = right})
          where 
            sent = tSentence dsent
            opts = ParseOpts {useCommaPruning = False,
                              distanceCache = undefined,
                              model = ProbModel {
                                        validity = newValid dsent,
                                        probs = obs
                                      }
                             }
              
newValid sent  event context = 
    valid sent (parentTWord context) (childTWord event) (spinePos context) (fromJustDef Sister $ ADJ.adjType event)

--countTAG :: TSentence  -> ParseMonad TAGDerivation 
countTAG obs dsent   = do 
        (lopts, ropts) <- makeParseOpts dsent obs 
        lstate <- initState lopts [] ALeft
        rstate <- initState ropts [] ARight
        let getFSM = (\ i (Just word) -> (lstate i word, rstate i word))
            (semi,chart) =   eisnerParse getFSM Just sent (\ _ i -> i) id id   
        return $ case semi of 
          Nothing -> trace ("failed to parse" ) (mempty,undefined) -- throw $ AssertionFailed $ show dsent
          Just s -> case s of 
                    (CD (Derivation (Just m))) ->  (m,chart)
                    (CD (Derivation (Nothing))) -> trace ("no derivation") (mempty,undefined)
    where 
      sent = tSentence dsent
    
--{{{  TESTS

testCounts = testGroup "Count tests" [
              testCase "simple sent" test_simpleSent,
              testCase "comma sent" test_commaSent,
              testCase "verb sent" test_verbSent,
              testCase "verb sent3" test_verbSent3,
              testCase "fidelity " test_fidelity
           ]

t_getCounts obs f = 
  liftIO $ do
    t <- readTAG f
    let c = t >>= (countTAG obs)
    dm <- loadDebugMappers 
    return $ runParseMonad c dm

t_getters f = do  
  ((countsEnd, countsAdj),_) <- t_getCounts testing f
  let get = (\(m,h) -> (M.!) countsAdj (m,h) )
      getEmp = (\(s,p,h) -> (M.!) countsEnd (s,p,h) )
  return (get,getEmp)

-- Simple Sent  
-- 1	The	DT	3	HOLDER	*	0	s
-- 2	new	JJ	3	HOLDER	*	0	s
-- 3	rate	NN	4	HOLDER	*+NPB+NP	1	s
-- 4	will	MD	0	HOLDER	*+VP+S	0	s
-- 5	be	VB	4	HOLDER	*+VP	0	s
-- 6	payable	JJ	5	HOLDER	*+ADJP	0	s
-- 7	Feb.	NNP	6	HOLDER	*+NPB+NP	0	s
-- 8	*NUM*	CD	7	HOLDER	*	0	s


test_simpleSent = do 
  (get, getEmp) <- t_getters "CodeTest/Simple.sent"

  assertEqual "" (crossesVerb $ snd $ getEmp (ARight,0,5)) (mkVerbDis False) 
  assertEqual "" (crossesVerb $ snd $ getEmp (ARight,0,4)) (mkVerbDis True) 

  assertEqual "" (parentWord $ snd $ getEmp (ARight,0,4)) (Atom 34)
  assertEqual "" (parentWord $ snd $ getEmp (ALeft,0,3)) (Atom 15)
  assertEqual "" (parentWord $ snd $ getEmp (ARight,0,7)) (Atom 24955)

  assertEqual "" (parentWord $ snd $ get (2,3)) (Atom 167)
  assertEqual "" (parentWord $ snd $ get (1,3)) (Atom 64)


-- Comma Sent
-- 1	Bell	NNP	8	HOLDER	*+NPB+NP	1	s
-- 2	,	,	1	HOLDER	*	1	a
-- 3	based	VBN	1	HOLDER	*+VP	1	s
-- 4	in	IN	3	HOLDER	*+PP	0	s
-- 5	Los	NNP	6	HOLDER	*	0	s
-- 6	Angeles	NNP	4	HOLDER	*+NPB+NP	0	s
-- 7	,	,	8	HOLDER	*	1	s
-- 8	makes	VBZ	0	HOLDER	*+VP+S	0	s
-- 9	and	CC	8	HOLDER	*+VP_CC	0	s
-- 10	distributes	VBZ	9	HOLDER	*	0	s
-- 11	electronic	JJ	16	HOLDER	*+UCP	0	s
-- 12	,	,	11	HOLDER	*	0	s
-- 13	computer	NN	11	HOLDER	*	0	s
-- 14	and	CC	11	HOLDER	*+UCP_CC	0	s
-- 15	building	NN	14	HOLDER	*	0	s
-- 16	products	NNS	8	HOLDER	*+NPB+NP	0	s

test_commaSent = do 
  (get, getEmp) <- t_getters "CodeTest/Comma.sent"

  assertEqual "right comma1" (prevComma $ delta $ snd $ get (3,1)) True 
  assertEqual "" (prevComma $ delta $ snd $ get (9,8)) False 
  assertEqual "right comma2" (prevComma $ delta $ snd $ get (13,11)) True
  assertEqual "right comma3" (prevComma $ delta $ snd $ get (14,11)) False 
  assertEqual "left comma" (prevComma $ delta $ snd $ get (1,8)) True 

  assertEqual "verb" (crossesVerb $  snd $ get (16,8)) (mkVerbDis True) 
  assertEqual "no verb" (crossesVerb $  snd $ get (10,9)) (mkVerbDis False) 
  assertEqual "no verb" (crossesVerb $  snd $ get (1,8)) (mkVerbDis False) 

  assertEqual "regular" (ADJ.adjType $ fst $ get (2,1)) (Just Regular) 
  assertEqual "regular" (ADJ.adjType $ fst $ get (1,8)) (Just Sister) 
  assertEqual "reg" (prevRegular $ snd $ get (3,1)) True 

  assertEqual "" (crossesVerb $ snd $ getEmp (ARight,1,1)) (mkVerbDis True) 
  assertEqual "" (crossesVerb $ snd $ getEmp (ARight,1,8)) (mkVerbDis True) 
  assertEqual "" (crossesVerb $ snd $ getEmp (ARight,0,8)) (mkVerbDis True) 

  assertEqual "" (prevComma $ delta $ snd $ getEmp (ARight,0,11)) False

  assertEqual "" (parentWord $ snd $ getEmp (ARight,1,1)) (Atom 1812)

-- 1	The	DT	2	HOLDER	*	0	s
-- 2	bill	NN	3	HOLDER	*+NPB+NP	1	s
-- 3	intends	VBZ	0	HOLDER	*+VP+S	0	s
-- 4	to	TO	3	HOLDER	*+VP+S	0	s
-- 5	restrict	VB	4	HOLDER	*+VP	0	s
-- 6	the	DT	7	HOLDER	*	0	s
-- 7	RTC	NNP	5	HOLDER	*+NPB+NP	0	s
-- 8	to	TO	5	HOLDER	*+PP	0	s
-- 9	Treasury	NNP	10	HOLDER	*	0	s
-- 10	borrowings	NNS	8	HOLDER	*+NPB+NP	0	s
-- 11	only	RB	10	HOLDER	*	0	s
-- 12	,	,	3	HOLDER	*	0	s
-- 13	unless	IN	3	HOLDER	*+SBAR	0	s
-- 14	the	DT	15	HOLDER	*	0	s
-- 15	agency	NN	16	HOLDER	*+NPB+NP	1	s
-- 16	receives	VBZ	13	HOLDER	*+VP+S	0	s
-- 17	specific	JJ	19	HOLDER	*	0	s
-- 18	congressional	JJ	19	HOLDER	*	0	s
-- 19	authorization	NN	16	HOLDER	*+NPB+NP	0	s

test_verbSent = do 
  (get, getEmp) <- t_getters "CodeTest/Verb.sent"
  
  -- unless -> intends
  assertEqual "" (crossesVerb $ snd $ get (13,3)) (mkVerbDis True) 
  assertEqual "" (crossesVerb $ snd $ get (12,3)) (mkVerbDis True) 
  assertEqual "" (crossesVerb $ snd $ getEmp (ARight,0,3)) (mkVerbDis True) 


-- 1	The	DT	2	HOLDER	*	0	s
-- 2	bill	NN	3	HOLDER	*+NPB+NP	1	s
-- 3	intends	VBZ	0	HOLDER	*+VP+S	0	s
-- 4	to	TO	3	HOLDER	*+VP+S	0	s
-- 5	restrict	VB	4	HOLDER	*+VP	0	s
-- 6	the	DT	7	HOLDER	*	0	s
-- 7	RTC	NNP	5	HOLDER	*+NPB+NP	0	s
-- 8	to	TO	5	HOLDER	*+PP	0	s
-- 9	Treasury	NNP	10	HOLDER	*	0	s
-- 10	borrowings	NNS	8	HOLDER	*+NPB+NP	0	s
-- 11	only	RB	10	HOLDER	*	0	s
-- 12	,	,	3	HOLDER	*	0	s
-- 13	unless	IN	3	HOLDER	*+SBAR	0	s
-- 14	the	DT	15	HOLDER	*	0	s
-- 15	agency	NN	16	HOLDER	*+NPB+NP	1	s
-- 16	receives	VBZ	13	HOLDER	*+VP+S	0	s
-- 17	specific	JJ	19	HOLDER	*	0	s
-- 18	congressional	JJ	19	HOLDER	*	0	s
-- 19	authorization	NN	16	HOLDER	*+NPB+NP	0	s


-- 1	*UNK*	JJ	2	HOLDER	*	0	s
-- 2	members	NNS	10	HOLDER	*+NPB+NP	1	s
-- 3	of	IN	2	HOLDER	*+PP	1	a
-- 4	the	DT	9	HOLDER	*	0	s
-- 5	House	NNP	9	HOLDER	*	0	s
-- 6	Ways	NNPS	9	HOLDER	*	0	s
-- 7	and	CC	9	HOLDER	*	0	s
-- 8	Means	NNP	9	HOLDER	*	0	s
-- 9	Committee	NNP	3	HOLDER	*+NPB+NP	0	s
-- 10	introduced	VBD	0	HOLDER	*+VP+S	0	s
-- 11	legislation	NN	10	HOLDER	*+NPB+NP	0	s
-- 12	that	WDT	11	HOLDER	*+WHNP+SBAR	1	a
-- 13	would	MD	12	HOLDER	*+VP+S	1	s
-- 14	restrict	VB	13	HOLDER	*+VP	0	s
-- 15	how	WRB	14	HOLDER	*+WHADVP+SBAR	0	s
-- 16	the	DT	20	HOLDER	*	0	s
-- 17	new	JJ	20	HOLDER	*	0	s
-- 18	savings-and-loan	JJ	20	HOLDER	*	0	s
-- 19	bailout	NN	20	HOLDER	*	0	s
-- 20	agency	NN	21	HOLDER	*+NPB+NP	1	s
-- 21	can	MD	15	HOLDER	*+VP+S	1	s
-- 22	raise	VB	21	HOLDER	*+VP	0	s
-- 23	capital	NN	22	HOLDER	*+NPB+NP	0	s
-- 24	,	,	14	HOLDER	*	0	s
-- 25	creating	VBG	14	HOLDER	*+VP+S	0	s
-- 26	another	DT	28	HOLDER	*	0	s
-- 27	potential	JJ	28	HOLDER	*	0	s
-- 28	obstacle	NN	25	HOLDER	*+NPB+NP	0	s
-- 29	to	TO	28	HOLDER	*+PP	1	a
-- 30	the	DT	32	HOLDER	*	0	s
-- 31	government	NN	32	HOLDER	*	0	s
-- 32	's	POS	33	HOLDER	*+NPB+NP	0	s
-- 33	sale	NN	29	HOLDER	*+NPB+NP	0	s
-- 34	of	IN	33	HOLDER	*+PP	1	a
-- 35	sick	JJ	36	HOLDER	*	0	s
-- 36	thrifts	NNS	34	HOLDER	*+NPB+NP	0	s
test_verbSent3 = do 
  (get, getEmp) <- t_getters "CodeTest/Verb3.sent"
  
  -- unless -> intends
  assertEqual "" (crossesVerb $ snd $ get (25,14)) (mkVerbDis True) 
  

-- 1	Today	NN	2	HOLDER	*	0	s
-- 2	's	POS	4	HOLDER	*+NPB+NP	0	s
-- 3	Fidelity	NNP	4	HOLDER	*	0	s
-- 4	ad	NN	5	HOLDER	*+NPB+NP	1	s
-- 5	goes	VBZ	0	HOLDER	*+VP+S	0	s
-- 6	a	DT	7	HOLDER	*	0	s
-- 7	step	NN	5	HOLDER	*+NPB+NP	0	s
-- 8	further	RBR	7	HOLDER	*+ADVP	1	a
-- 9	,	,	5	HOLDER	*	0	s
-- 10	encouraging	VBG	5	HOLDER	*+VP+S	0	s
-- 11	investors	NNS	12	HOLDER	*+NPB+NP	1	s
-- 12	to	TO	10	HOLDER	*+VP+S	0	s
-- 13	stay	VB	12	HOLDER	*+VP	0	s
-- 14	in	IN	13	HOLDER	*+PP	0	s
-- 15	the	DT	16	HOLDER	*	0	s
-- 16	market	NN	14	HOLDER	*+NPB+NP	0	s
-- 17	or	CC	12	HOLDER	*+VP_CC	0	a
-- 18	even	RB	19	HOLDER	*+ADVP	0	s
-- 19	to	TO	17	HOLDER	*+VP	0	s
-- 20	plunge	VB	19	HOLDER	*+VP	0	s
-- 21	in	RP	20	HOLDER	*+PRT	0	s
-- 22	with	IN	20	HOLDER	*+PP	0	s
-- 23	Fidelity	NNP	22	HOLDER	*+NPB+NP	0	s
test_fidelity = do 
  (get, getEmp) <- t_getters "CodeTest/Fidelity.sent"
  
  -- unless -> intends
  assertEqual "" (crossesVerb $ snd $ get (17,12)) (mkVerbDis True) 




--}}}

