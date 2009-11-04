module NLP.Language.German.NonTerm where 

import qualified NLP.Language.English.NonTerm as J

data NonTerm = AA | AP | AVP | CAC | CAP | CAVP | CCP | CH | CNP | CO | CPP | CS | CVP | CVZ | DL | ISU | MTA | NM | NP | PN | PP | QL | S | VP | VZ | NPB
               deriving (Read, Show, Eq, Ord, Enum, Bounded)

toJoint nt = case nt of 
               AA -> J.ADJP
               AP -> J.ADJP
               AVP -> J.ADVP 
               CAC -> J.PP
               CAP -> J.ADJP
               CAVP -> J.ADVP
               CH -> J.NP -- Chunks are normally FM noun phrases
               CNP -> J.NP
               CO -> J.SBAR
               CVP -> J.VP
               CVZ -> J.VP
               DL ->  J.S -- this does not exist in english
               ISU -> J.NP -- also not there 
               MTA -> J.ADJP
               NM -> J.QP
               NP -> J.NP
               PN -> J.NP
               PP -> J.PP
               QL -> J.FRAG
               S -> J.S 
               VP -> J.VP
               VZ -> J.VP