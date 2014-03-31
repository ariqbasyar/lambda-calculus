module Library where
import Parser

import qualified Data.Map as M
import Data.Map (Map)
import Data.List

---------------------------------------------------------------------------------
--                                 Library                                     --
---------------------------------------------------------------------------------
library = M.fromList [(fromEnum 'I', parseLambda "(\\x.x)"),
                      (fromEnum 'S', parseLambda "(\\wyx.y(wyx))"),
                      (fromEnum '+', parseLambda "(\\ab.(aSb))"), 
                      (fromEnum '*', parseLambda "(\\xyz.x(yz))"),
                      (fromEnum 'T', parseLambda "(\\xy.x)"),
                      (fromEnum 'F', parseLambda "(\\xy.y)"),

                      (fromEnum 'E', parseLambda "(\\xy.xy(\\uv.v))"), -- et
                      (fromEnum 'O', parseLambda "(\\xy.x(\\uv.u)y)"), -- or

                      (fromEnum 'N', parseLambda "(\\x.x(\\uv.v)(\\ab.a))"),
                      (fromEnum 'Z', parseLambda "(\\x.xFNF)"),

                      (fromEnum 'P', parseLambda "(\\nfx.n(\\gh.h(gf))(\\u.x)(\\u.u))"),
                      (fromEnum 'Y', parseLambda "(\\g.((\\x.g(xx))(\\x.g(xx))))"),
                      (fromEnum '-', parseLambda  "(\\ab.(bPa))"), -- take predicesor of 'a' 'b' times --

                      (fromEnum 'R', parseLambda  "Y(\\rn.Zn0(nS(r(Pn))))"),
                      (fromEnum 'A', parseLambda  "Y(\\fn.Zn1(*n(f(Pn))))"),

                      -- (define (my-/ n d) (if (<= (- (+ 1 n) d) 0) 0 (+ 1 (my-/ (- n d) d))))
                      (fromEnum '/', parseLambda  "Y(\\rnd.Z(-(Sn)d)0(S(r(-nd)d)))")]
