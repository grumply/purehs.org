Implements [RXS-M-XS](https://en.wikipedia.org/wiki/Permuted_congruential_generator#Variants) from the [permuted congruential generator (PCG)](https://en.wikipedia.org/wiki/Permuted_congruential_generator) suite.

Performance is satisfactory, both natively and in the browser.

|            | Throughput (**Gb/s**) |
| --         | :-: |
| native     | 64  |
| Firefox 82 | 12  |
| Safari 14  | 8   |
| node 12    | 4   |
| Chrome 86  | 2.5 |

<pre data-try>
import Pure
import Pure.Random

data Card = Card Int Int

card :: Card -> View
card (Card rank suit) = Span <||> [ fromTxt (r <> s) ]
  where
    r = ["A",2,3,4,5,6,7,8,9,10,"J","Q","K"] !! (rank - 1)
    s = ["♠","♥","♦","♣"] !! (suit - 1)

deck :: [View]
deck = [ card (Card r s) | r <- [1..13] , s <- [1..4] ]

main = do
  s <- newSeed
  inject body $ 
    let cards = shuffle deck s
    in Span <||> take 5 cards
</pre>