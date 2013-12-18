data Object = Object Int (Int -> Int) ;

result :: Int
result = use obj ;

obj :: Object
obj = Object 1 inc ;

use :: Object -> Int
use o = case o of Object field func -> func field ;
                  
inc :: Int -> Int
inc a = a + 1
