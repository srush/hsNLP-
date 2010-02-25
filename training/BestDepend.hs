module BestDepend where 


import NLP.Model.Decoding 
import NLP.Language.English
import NLP.TreeBank.TreeBank
import NLP.Model.CreateableSemi
import NLP.Grammar.Dependency

basicParams :: IO (DecodeParams English)
basicParams = 
    readDecodeParams "/tmp/curcounts" "/tmp/cspines" "/tmp/pspines"
                     

decodeBestDer :: String -> IO (TAGDerivation English )
decodeBestDer file =  do 
  params <- basicParams
  sent <- readSentence file
  let (Just der) =decodeSentence params sent 
  return $ getCVDBestDerivation der 
         
