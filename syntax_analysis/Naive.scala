object Naive {
    enum LToken {
    case XTok
    case YTok
}

    enum T {
        case XX
        case YX
    }

    enum Result[A] {
        case Failed(msg:String)
        case Ok(v:A)
    }
    import LToken.* 
    import T.* 
    import Result.*

    def item(toks:List[LToken]):Result[(LToken, List[Token])] = toks match {
        case Nil => Failed("item() is called with an empty input")
        case (t::ts) => Ok((t, ts))
    }

    def sat(toks:List[LToken])(p:LToken => Boolean):Result[(LToken, List[Token])] = toks match {
        case Nil => Failed("sat() is called with an empty input")
        case (t::ts) if p(t) => Ok((t, ts))
        case (t::ts)         => Failed("sat() is called with an input that does not satisfy the input predicate.")
    }

    def parseT(toks:List[LToken]):Result[T] = {
        parseXX(toks) match {
            case Failed(_) => parseYX(toks) match {
                case Failed(err) => Failed(err)
                case Ok((t,Nil)) => Ok(t)
                case _ => Failed("the remaining toks are not empty")
            }
            case Ok((t,Nil))  => Ok(t) 
            case _ => Failed("the remaining toks are not empty")
        }
    }

    def parseXX(toks:List[LToken]):(Result[T], List[LToken]) = {
        sat(toks)( t => t match {
            case XTok => true
            case _ => false
        }) match {
            case Failed(err) => Failed(err)
            case Ok((x1,toks1)) => {
                sat(toks1)( s => s match {
                    case XTok => true
                    case _ => false
                }) match {
                    case Failed(err) => Failed(err)
                    case Ok((x2,toks2)) => Ok((XX,toks2))
                }
            } 
        }
    }

    def parseYX(toks:List[LToken]):(Result[T], List[LToken]) = {
        sat(toks)( t => t match {
            case YTok => true
            case _ => false
        }) match {
            case Failed(err) => Failed(err)
            case Ok((y1,toks1)) => {
                sat(toks1)( s => s match {
                    case XTok => true
                    case _ => false
                }) match {
                    case Failed(err) => Failed(err)
                    case Ok((x2,toks2)) => Ok((YX,toks2))
                }
            } 
        }
    }
}