import Monad.* 
import Parsec.* 
import Applicative.* 
import BacktrackParsec.*

object Better {

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

    def parseT:Parser[T] = choice(parseXX)(parseYX)
    
    def parseXX:Parser[T] = for {
        _ <- parseX 
        _ <- parseX 
    } yield XX 

    def parseYX:Parser[T] = for {
        _ <- parseY
        _ <- parseX         
    } yield YX

    def parseX:Parser[LToken] = sat(t => t match {
        case XTok => true
        case _ => false
    }) 

    def parseY:Parser[LToken] = sat(t => t match {
        case YTok => true
        case _ => false
    }) 
    

}