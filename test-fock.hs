-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.3554:@thin test-fock.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.3555:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix

import Data.Complex

import Quantum.Euclidean3D
import Quantum.Function
import Quantum.Function.Transformers
import Quantum.Testing

import System.IO.Unsafe
import System.Random

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
-- @nonl
-- @-node:gcross.20091204093401.3555:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3561:Constants
bigOmega :: Complex Double
bigOmega = unsafePerformIO randomIO

om :: Complex Double
om = unsafePerformIO randomIO

om_z :: Complex Double
om_z = unsafePerformIO randomIO

-- @-node:gcross.20091204093401.3561:Constants
-- @+node:gcross.20091204093401.3559:Operators
-- @+node:gcross.20091204093401.3560:x, y, z
x = r_x
y = r_y
z = r_z
-- @-node:gcross.20091204093401.3560:x, y, z
-- @+node:gcross.20091207130316.1277:px, py, pz
px = p_x
py = p_y
pz = p_z
-- @-node:gcross.20091207130316.1277:px, py, pz
-- @+node:gcross.20091204093401.3592:a, b, c
a  = sqrt(om/2)  *|x |+| i*sqrt(1/(2*om))  *|p_x
at = sqrt(om/2)  *|x |-| i*sqrt(1/(2*om))  *|p_x
b  = sqrt(om/2)  *|y |+| i*sqrt(1/(2*om))  *|p_y
bt = sqrt(om/2)  *|y |-| i*sqrt(1/(2*om))  *|p_y
c  = sqrt(om_z/2)*|z |+| i*sqrt(1/(2*om_z))*|p_z
ct = sqrt(om_z/2)*|z |-| i*sqrt(1/(2*om_z))*|p_z
-- @-node:gcross.20091204093401.3592:a, b, c
-- @+node:gcross.20091206113753.1374:g, n
g  = 1/sqrt 2*|(a  |+| i*|b )
gt = 1/sqrt 2*|(at |-| i*|bt)
n  = 1/sqrt 2*|(b  |+| i*|a )
nt = 1/sqrt 2*|(bt |-| i*|at)
-- @-node:gcross.20091206113753.1374:g, n
-- @+node:gcross.20091206205300.1498:j_x, j_y, j_z
j_x = y.p_z |-| z.p_y
j_y = z.p_x |-| x.p_z
j_z = x.p_y |-| y.p_x
-- @-node:gcross.20091206205300.1498:j_x, j_y, j_z
-- @-node:gcross.20091204093401.3559:Operators
-- @+node:gcross.20091204093401.3557:Hamiltonians
-- @+node:gcross.20091204093401.3558:Original
original_hamiltonian :: QuantumOperator
original_hamiltonian =
     1/2 *| (p_x|^2 |+| p_y|^2 |+| p_z|^2)
 |+| bigOmega *| ( x.p_y |-| y.p_x )
 |+| om**2/2 *| ( x|^2 |+| y|^2 )
 |+| om_z**2/2 *| z|^2
-- @-node:gcross.20091204093401.3558:Original
-- @+node:gcross.20091204093401.3595:Ladder
ladder_hamiltonian :: QuantumOperator
ladder_hamiltonian =
     om *| (at.a |+| bt.b |+| id)
 |+| om_z *| (ct.c |+| 1/2*|id)
 |-| i*bigOmega *| (at.b |-| bt.a)
-- @-node:gcross.20091204093401.3595:Ladder
-- @+node:gcross.20091206113753.1494:Diagonalized
diagonalized_hamiltonian :: QuantumOperator
diagonalized_hamiltonian =
     om *| (gt.g |+| nt.n |+| id)
 |+| om_z *| (ct.c |+| 1/2*|id)
 |-| bigOmega *| (gt.g |-| nt.n)
-- @-node:gcross.20091206113753.1494:Diagonalized
-- @-node:gcross.20091204093401.3557:Hamiltonians
-- @+node:gcross.20091204093401.3991:Functions
-- @+node:gcross.20091204093401.3992:makeHOFromCoordinates
makeHOFromCoordinates q p om = 1/2*|p|^2 |+| 1/2*om**2*|q|^2
-- @-node:gcross.20091204093401.3992:makeHOFromCoordinates
-- @+node:gcross.20091204093401.3994:makeHOFromLadders
makeHOFromLadders a at om = om*|(at.a |+| 0.5*|id)
-- @-node:gcross.20091204093401.3994:makeHOFromLadders
-- @+node:gcross.20091206205300.1385:=~=
infix 4 ~=~
(~=~) :: QuantumOperator -> QuantumOperator ->
          Function (ThreeDimensions (Complex Double)) XYZ (Complex Double) ->
          ThreeDimensions (Complex Double) ->
          Bool
(~=~) o1 o2 f v = o1 f $> v ~= o2 f $> v
-- @-node:gcross.20091206205300.1385:=~=
-- @-node:gcross.20091204093401.3991:Functions
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091204093401.3593:<< Tests >>
    -- @+others
    -- @+node:gcross.20091204093401.3987:Ladder operator tests
    [testGroup "Ladder operator tests"
        -- @    @+others
        -- @+node:gcross.20091206113753.1376:Correct form
        [testGroup "Correct form"
            -- @    @+others
            -- @+node:gcross.20091204093401.3988:x coordinate
            [testProperty "x coordinate" $
                makeHOFromCoordinates x p_x om ~=~ makeHOFromLadders a at om
            -- @-node:gcross.20091204093401.3988:x coordinate
            -- @+node:gcross.20091204093401.3990:y coordinate
            ,testProperty "y coordinate" $
                makeHOFromCoordinates y p_y om ~=~ makeHOFromLadders b bt om
            -- @-node:gcross.20091204093401.3990:y coordinate
            -- @+node:gcross.20091204093401.3996:z coordinate
            ,testProperty "z coordinate" $
                makeHOFromCoordinates z p_z om_z ~=~ makeHOFromLadders c ct om_z
            -- @-node:gcross.20091204093401.3996:z coordinate
            -- @-others
            ]
        -- @-node:gcross.20091206113753.1376:Correct form
        -- @+node:gcross.20091206113753.1375:Correct commutator
        ,testGroup "Correct commutator"
            -- @    @+others
            -- @+node:gcross.20091206113753.1377:a
            [testProperty "a" $ (a ~~ at) ~=~ id
            -- @-node:gcross.20091206113753.1377:a
            -- @+node:gcross.20091206113753.1379:b
            ,testProperty "b" $ (b ~~ bt) ~=~ id
            -- @-node:gcross.20091206113753.1379:b
            -- @+node:gcross.20091206113753.1381:c
            ,testProperty "c" $ (c ~~ ct) ~=~ id
            -- @-node:gcross.20091206113753.1381:c
            -- @+node:gcross.20091206113753.1383:g
            ,testProperty "g" $ (g ~~ gt) ~=~ id
            -- @-node:gcross.20091206113753.1383:g
            -- @+node:gcross.20091206113753.1385:n
            ,testProperty "n" $ (n ~~ nt) ~=~ id
            -- @-node:gcross.20091206113753.1385:n
            -- @-others
            ]
        -- @-node:gcross.20091206113753.1375:Correct commutator
        -- @+node:gcross.20091206205300.1382:Correct definition of old in terms of new
        ,testGroup "Correct definition of old in terms of new"
            -- @    @+others
            -- @+node:gcross.20091206205300.1384:x
            [testProperty "x" $ x ~=~ (1/2/sqrt(om)) *| (gt|+|g |+| i*|(nt|-|n) )
            -- @nonl
            -- @-node:gcross.20091206205300.1384:x
            -- @+node:gcross.20091206205300.1493:y
            ,testProperty "y" $ y ~=~ (1/2/sqrt(om)) *| (nt|+|n |+| i*|(gt|-|g) )
            -- @nonl
            -- @-node:gcross.20091206205300.1493:y
            -- @+node:gcross.20091206205300.1495:p_x
            ,testProperty "p_x" $ p_x ~=~ (i*sqrt(om)/2) *| (gt|-|g |+| i*|(nt|+|n) )
            -- @nonl
            -- @-node:gcross.20091206205300.1495:p_x
            -- @+node:gcross.20091206205300.1497:p_y
            ,testProperty "p_y" $ p_y ~=~ (i*sqrt(om)/2) *| (nt|-|n |+| i*|(gt|+|g) )
            -- @nonl
            -- @-node:gcross.20091206205300.1497:p_y
            -- @-others
            ]
        -- @-node:gcross.20091206205300.1382:Correct definition of old in terms of new
        -- @-others
        ]
    -- @-node:gcross.20091204093401.3987:Ladder operator tests
    -- @+node:gcross.20091206113753.1264:Hamiltonian tests
    ,testGroup "Hamiltonian tests"
        -- @    @+others
        -- @+node:gcross.20091206113753.1265:Rewritten with ladder operators
        [testProperty "Rewritten with ladder operators" $
            original_hamiltonian ~=~ ladder_hamiltonian
        -- @-node:gcross.20091206113753.1265:Rewritten with ladder operators
        -- @+node:gcross.20091206113753.1496:Diagonalized
        ,testProperty "Diagonalized" $
            original_hamiltonian ~=~ diagonalized_hamiltonian
        -- @-node:gcross.20091206113753.1496:Diagonalized
        -- @-others
        ]
    -- @-node:gcross.20091206113753.1264:Hamiltonian tests
    -- @+node:gcross.20091206205300.1499:Angular momentum tests
    ,testGroup "Angular momentum tests"
        -- @    @+others
        -- @+node:gcross.20091206205300.1500:j_z
        [testProperty "j_z" $
            j_z ~=~ nt.n |-| gt.g
        -- @-node:gcross.20091206205300.1500:j_z
        -- @-others
        ]
    -- @-node:gcross.20091206205300.1499:Angular momentum tests
    -- @+node:gcross.20091207130316.1274:Miscellaneous expressions
    ,testGroup "Miscellaneous expressions"
        -- @    @+others
        -- @+node:gcross.20091207130316.1276:#0
        [testProperty "#0" $
            \f v -> (x.px.py.y ~=~ x.px.(y.py |-| i*|id)) f v &&
                    (x.px.py.y ~=~ x.px.y.py |+| ((-i)*|x.px)) f v
        -- @-node:gcross.20091207130316.1276:#0
        -- @+node:gcross.20091207130316.1281:#1
        ,testProperty "#1" $
            negosum [x.px.py.y,y.py.px.x] ~=~ (-2)*|x.px.y.py |+| i*|(x.px|+|y.py)
        -- @-node:gcross.20091207130316.1281:#1
        -- @+node:gcross.20091207130316.1283:#2
        ,testProperty "#2" $
            negosum
             [z.pz.(px.x|+|py.y)
             ,pz.z.(x.px|+|y.py)
             ]
            ~=~
            osum
             [(2*i)*|z.pz
             ,(-2)*|z.pz.(x.px|+|y.py)
             ,i*|(x.px|+|y.py)
             ]
        -- @-node:gcross.20091207130316.1283:#2
        -- @+node:gcross.20091207130316.1279:#3
        ,testProperty "#3" $
            negosum
             [z.pz.px.x
             ,x.px.pz.z
             ,y.py.pz.z
             ,z.pz.py.y
             ,x.px.py.y
             ,y.py.px.x
             ]
            ~=~
            osum
             [(2*i)*|(x.px|+|y.py|+|z.pz)
             ,(-2)*|z.pz.(x.px|+|y.py)
             ,(-2)*|x.px.y.py
             ]
        -- @-node:gcross.20091207130316.1279:#3
        -- @-others
        ]
    -- @-node:gcross.20091207130316.1274:Miscellaneous expressions
    -- @-others
    -- @nonl
    -- @-node:gcross.20091204093401.3593:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091204093401.3554:@thin test-fock.hs
-- @-leo
