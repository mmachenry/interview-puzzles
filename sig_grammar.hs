import Control.Monad

start = activity ++> frequency
activity = action ++> quantity
action = ["take","drink"]
quantity = number ++> unit
frequency = ["once per day","twice per day"]
number = ["1","2","4"]
unit = ["tablet","ounce"]

(++>) = liftM2 (\a b->a ++ " " ++ b)

