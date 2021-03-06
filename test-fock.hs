-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.3554:@thin test-fock.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.3555:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix

import Data.Complex
import Data.Function

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Quantum
import Data.Differentiable.Testing

import System.IO.Unsafe
import System.Random

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
-- @-node:gcross.20091204093401.3555:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3561:Constants
bigOmega :: Complex Double
bigOmega = unsafePerformIO $ fmap (:+ 0) randomIO

om :: Complex Double
om = unsafePerformIO $ fmap (:+ 0) randomIO

om_z :: Complex Double
om_z = unsafePerformIO $ fmap (:+ 0) randomIO

-- @-node:gcross.20091204093401.3561:Constants
-- @+node:gcross.20091209132934.1287:Types
-- @+node:gcross.20091209132934.1288:QuantumOperator
type QuantumOperator = DifferentialOperator (Complex Double)
-- @-node:gcross.20091209132934.1288:QuantumOperator
-- @-node:gcross.20091209132934.1287:Types
-- @+node:gcross.20091204093401.3559:Operators
-- @+node:gcross.20091209132934.1289:x, y, z
x, y, z :: QuantumOperator
x = r_ X
y = r_ Y
z = r_ Z
-- @-node:gcross.20091209132934.1289:x, y, z
-- @+node:gcross.20091209132934.1290:px, py, pz
px, py, pz :: QuantumOperator
px = p_ X
py = p_ Y
pz = p_ Z
-- @-node:gcross.20091209132934.1290:px, py, pz
-- @+node:gcross.20091204093401.3592:a, b, c
a  = sqrt(om/2)  *|x + i*sqrt(1/(2*om))  *|px
at = sqrt(om/2)  *|x - i*sqrt(1/(2*om))  *|px
b  = sqrt(om/2)  *|y + i*sqrt(1/(2*om))  *|py
bt = sqrt(om/2)  *|y - i*sqrt(1/(2*om))  *|py
c  = sqrt(om_z/2)*|z + i*sqrt(1/(2*om_z))*|pz
ct = sqrt(om_z/2)*|z - i*sqrt(1/(2*om_z))*|pz
-- @-node:gcross.20091204093401.3592:a, b, c
-- @+node:gcross.20091206113753.1374:g, n
g  = 1/sqrt 2*|(a  + i*|b )
gt = 1/sqrt 2*|(at - i*|bt)
n  = 1/sqrt 2*|(b  + i*|a )
nt = 1/sqrt 2*|(bt - i*|at)
-- @-node:gcross.20091206113753.1374:g, n
-- @+node:gcross.20091206205300.1498:jx, jy, jz
jx = y.pz - z.py
jy = z.px - x.pz
jz = x.py - y.px
-- @-node:gcross.20091206205300.1498:jx, jy, jz
-- @-node:gcross.20091204093401.3559:Operators
-- @+node:gcross.20091204093401.3557:Hamiltonians
-- @+node:gcross.20091204093401.3558:Original
original_hamiltonian :: QuantumOperator
original_hamiltonian =
     1/2 *| (px|^2 + py|^2 + pz|^2)
 + bigOmega *| ( x.py - y.px )
 + om**2/2 *| ( x|^2 + y|^2 )
 + om_z**2/2 *| z|^2
-- @-node:gcross.20091204093401.3558:Original
-- @+node:gcross.20091204093401.3595:Ladder
ladder_hamiltonian :: QuantumOperator
ladder_hamiltonian =
     om *| (at.a + bt.b + id)
 + om_z *| (ct.c + 1/2*|id)
 - i*bigOmega *| (at.b - bt.a)
-- @-node:gcross.20091204093401.3595:Ladder
-- @+node:gcross.20091206113753.1494:Diagonalized
diagonalized_hamiltonian :: QuantumOperator
diagonalized_hamiltonian =
     om *| (gt.g + nt.n + id)
 + om_z *| (ct.c + 1/2*|id)
 - bigOmega *| (gt.g - nt.n)
-- @-node:gcross.20091206113753.1494:Diagonalized
-- @-node:gcross.20091204093401.3557:Hamiltonians
-- @+node:gcross.20091204093401.3991:Functions
-- @+node:gcross.20091212141130.1615:vx, vy, vz
vx, vy, vz :: DifferentiableFunction (Complex Double)
vx = v_ X
vy = v_ Y
vz = v_ Z
-- @-node:gcross.20091212141130.1615:vx, vy, vz
-- @+node:gcross.20091204093401.3992:makeHOFromCoordinates
makeHOFromCoordinates q p om = 1/2*|p|^2 + 1/2*om**2*|q|^2
-- @-node:gcross.20091204093401.3992:makeHOFromCoordinates
-- @+node:gcross.20091204093401.3994:makeHOFromLadders
makeHOFromLadders a at om = om*|(at.a + 0.5*|id)
-- @-node:gcross.20091204093401.3994:makeHOFromLadders
-- @-node:gcross.20091204093401.3991:Functions
-- @+node:gcross.20091207130316.1295:Miscellaneous expressions
-- @+node:gcross.20091207130316.1296:t1/t2
t1 = osum [gt.g,g.gt,nt.n,n.nt]
t2 = nt.gt - n.g
-- @-node:gcross.20091207130316.1296:t1/t2
-- @-node:gcross.20091207130316.1295:Miscellaneous expressions
-- @+node:gcross.20091220115426.1524:States
ground_state = exp $ (om/2) *|| (-vx*vx-vy*vy)
excited_state = nt . ground_state
-- @-node:gcross.20091220115426.1524:States
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
                makeHOFromCoordinates x px om =~= makeHOFromLadders a at om
            -- @nonl
            -- @-node:gcross.20091204093401.3988:x coordinate
            -- @+node:gcross.20091204093401.3990:y coordinate
            ,testProperty "y coordinate" $
                makeHOFromCoordinates y py om =~= makeHOFromLadders b bt om
            -- @nonl
            -- @-node:gcross.20091204093401.3990:y coordinate
            -- @+node:gcross.20091204093401.3996:z coordinate
            ,testProperty "z coordinate" $
                makeHOFromCoordinates z pz om_z =~= makeHOFromLadders c ct om_z
            -- @nonl
            -- @-node:gcross.20091204093401.3996:z coordinate
            -- @-others
            ]
        -- @-node:gcross.20091206113753.1376:Correct form
        -- @+node:gcross.20091206113753.1375:Correct commutator
        ,testGroup "Correct commutator"
            -- @    @+others
            -- @+node:gcross.20091206113753.1377:a
            [testProperty "a" $ (a ~~ at) =~= id
            -- @nonl
            -- @-node:gcross.20091206113753.1377:a
            -- @+node:gcross.20091206113753.1379:b
            ,testProperty "b" $ (b ~~ bt) =~= id
            -- @nonl
            -- @-node:gcross.20091206113753.1379:b
            -- @+node:gcross.20091206113753.1381:c
            ,testProperty "c" $ (c ~~ ct) =~= id
            -- @nonl
            -- @-node:gcross.20091206113753.1381:c
            -- @+node:gcross.20091206113753.1383:g
            ,testProperty "g" $ (g ~~ gt) =~= id
            -- @nonl
            -- @-node:gcross.20091206113753.1383:g
            -- @+node:gcross.20091206113753.1385:n
            ,testProperty "n" $ (n ~~ nt) =~= id
            -- @nonl
            -- @-node:gcross.20091206113753.1385:n
            -- @-others
            ]
        -- @-node:gcross.20091206113753.1375:Correct commutator
        -- @+node:gcross.20091206205300.1382:Correct definition of old in terms of new
        ,testGroup "Correct definition of old in terms of new"
            -- @    @+others
            -- @+node:gcross.20091206205300.1384:x
            [testProperty "x" $ x =~= (1/2/sqrt(om)) *| (gt+g + i*|(nt-n))
            -- @-node:gcross.20091206205300.1384:x
            -- @+node:gcross.20091206205300.1493:y
            ,testProperty "y" $ y =~= (1/2/sqrt(om)) *| (nt+n + i*|(gt-g) )
            -- @nonl
            -- @-node:gcross.20091206205300.1493:y
            -- @+node:gcross.20091207130316.1292:z
            ,testProperty "z" $ z =~= (1/sqrt(2*om_z)) *| (ct+c)
            -- @nonl
            -- @-node:gcross.20091207130316.1292:z
            -- @+node:gcross.20091206205300.1495:p_x
            ,testProperty "px" $ px =~= (i*sqrt(om)/2) *| (gt-g + i*|(nt+n) )
            -- @nonl
            -- @-node:gcross.20091206205300.1495:p_x
            -- @+node:gcross.20091206205300.1497:p_y
            ,testProperty "py" $ py =~= (i*sqrt(om)/2) *| (nt-n + i*|(gt+g) )
            -- @nonl
            -- @-node:gcross.20091206205300.1497:p_y
            -- @+node:gcross.20091207130316.1294:p_z
            ,testProperty "pz" $ pz =~= (i*sqrt(om_z/2)) *| (ct-c)
            -- @nonl
            -- @-node:gcross.20091207130316.1294:p_z
            -- @-others
            ]
        -- @-node:gcross.20091206205300.1382:Correct definition of old in terms of new
        -- @+node:gcross.20091212141130.1304:Correct simplification of the new
        ,testGroup "Correct simplification of the new"
            -- @    @+others
            -- @+node:gcross.20091212141130.1305:nt
            [testProperty "nt" $ nt =~= (sqrt(om)/2) *| (-i*|x - (1/om)*|px + y - (i/om)*|py)
            -- @-node:gcross.20091212141130.1305:nt
            -- @+node:gcross.20091212141130.1806:n
            ,testProperty "nt" $ nt =~= (sqrt(om)/2) *| (-i*|(x - 1/om*|(d X)) + (y - (1/om)*|(d Y)))
            -- @-node:gcross.20091212141130.1806:n
            -- @-others
            ]
        -- @-node:gcross.20091212141130.1304:Correct simplification of the new
        -- @-others
        ]
    -- @-node:gcross.20091204093401.3987:Ladder operator tests
    -- @+node:gcross.20091206113753.1264:Hamiltonian tests
    ,testGroup "Hamiltonian tests"
        -- @    @+others
        -- @+node:gcross.20091206113753.1265:Rewritten with ladder operators
        [testProperty "Rewritten with ladder operators" $
            original_hamiltonian =~= ladder_hamiltonian
        -- @nonl
        -- @-node:gcross.20091206113753.1265:Rewritten with ladder operators
        -- @+node:gcross.20091206113753.1496:Diagonalized
        ,testProperty "Diagonalized" $
            original_hamiltonian =~= diagonalized_hamiltonian
        -- @nonl
        -- @-node:gcross.20091206113753.1496:Diagonalized
        -- @-others
        ]
    -- @-node:gcross.20091206113753.1264:Hamiltonian tests
    -- @+node:gcross.20091206205300.1499:Angular momentum tests
    ,testGroup "Angular momentum tests"
        -- @    @+others
        -- @+node:gcross.20091206205300.1500:jz
        [testProperty "jz" $
            jz =~= nt.n - gt.g
        -- @-node:gcross.20091206205300.1500:jz
        -- @-others
        ]
    -- @-node:gcross.20091206205300.1499:Angular momentum tests
    -- @+node:gcross.20091212141130.1632:Derivatives
    ,testGroup "Derivatives"
        -- @    @+others
        -- @+node:gcross.20091212141130.1634:d exp(-x^2)
        [testProperty "d exp(-x^2)" $
            d X (exp (-vx*vx)) . getArg =~= (const (-2) * vx * exp (-vx*vx)) . getArg
        -- @-node:gcross.20091212141130.1634:d exp(-x^2)
        -- @+node:gcross.20091212141130.1649:nt on ground state
        ,testProperty "nt on ground state" $
            ((=~=) `on` (.getArg)) excited_state ((sqrt(om) *| (y - i*|x)) . ground_state)
        -- @-node:gcross.20091212141130.1649:nt on ground state
        -- @-others
        ]
    -- @-node:gcross.20091212141130.1632:Derivatives
    -- @+node:gcross.20091207130316.1274:Miscellaneous expressions
    ,testGroup "Miscellaneous expressions"
        -- @    @+others
        -- @+node:gcross.20091207130316.1276:#0
        [testProperty "#0" $
            \f -> (x.px.py.y =~= x.px.(y.py - i*|id)) f &&
                  (x.px.py.y =~= x.px.y.py + ((-i)*|x.px)) f
        -- @nonl
        -- @-node:gcross.20091207130316.1276:#0
        -- @+node:gcross.20091207130316.1281:#1
        ,testProperty "#1" $
            negosum [x.px.py.y, y.py.px.x] =~= (-2)*|x.px.y.py + i*|(x.px+y.py)
        -- @nonl
        -- @-node:gcross.20091207130316.1281:#1
        -- @+node:gcross.20091207130316.1283:#2
        ,testProperty "#2" $
            negosum
             [z.pz.(px.x+py.y)
             ,pz.z.(x.px+y.py)
             ]
            =~=
            osum
             [(2*i)*|z.pz
             ,(-2)*|z.pz.(x.px + y.py)
             ,i*|(x.px+y.py)
             ]
        -- @nonl
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
            =~=
            osum
             [(2*i)*|(x.px + y.py + z.pz)
             ,(-2)*|z.pz.(x.px + y.py)
             ,(-2)*|x.px.y.py
             ]
        -- @nonl
        -- @-node:gcross.20091207130316.1279:#3
        -- @+node:gcross.20091207130316.1286:#4
        ,testProperty "#4" $
            osum
             [jx|^2
             ,jy|^2
             ,jz|^2
             ]
            =~=
            osum
             [(z|^2).(px|^2 + py|^2)
             ,(pz|^2).(x|^2 + y|^2)
             ,(x|^2).(py|^2)
             ,(px|^2).(y|^2)
             ,(2*i)*|(x.px + y.py + z.pz)
             ,(-2)*|z.pz.(x.px + y.py)
             ,(-2)*|x.px.y.py
             ]
        -- @nonl
        -- @-node:gcross.20091207130316.1286:#4
        -- @+node:gcross.20091207130316.1288:#5
        ,testProperty "#5" $
            x|^2 + y|^2 =~= (1/(2*om))*|t1 + (i/om)*|t2
        -- @-node:gcross.20091207130316.1288:#5
        -- @+node:gcross.20091207130316.1290:#6
        ,testProperty "#6" $
            px|^2 + py|^2 =~= (om/2)*|t1 - (i*om)*|t2
        -- @-node:gcross.20091207130316.1290:#6
        -- @-others
        ]
    -- @-node:gcross.20091207130316.1274:Miscellaneous expressions
    -- @+node:gcross.20091220115426.1516:Phases
    ,testGroup "Phases"
        -- @    @+others
        -- @+node:gcross.20091220115426.1517:ground state
        [testProperty "ground state" $
            ((=~=) `on` (.map abs.getArg))
                (realPart . abs . log . eval . ground_state)
                (abs . realPart . log . eval . ground_state)
        -- @-node:gcross.20091220115426.1517:ground state
        -- @+node:gcross.20091220115426.1745:excited state
        ,testProperty "excited state" $
            ((=~=) `on` (.map abs.getArg))
                (imagPart . log . eval . nt . ground_state)
                (imagPart . log . eval . (vy - i*||vx))
        -- @-node:gcross.20091220115426.1745:excited state
        -- @+node:gcross.20091220115426.1747:phase X derivative
        ,testProperty "phase X derivative" $
            ((=~=) `on` (.map abs.getArg))
                (imagPart . eval . d X . log . (vy - i*||vx))
                (negate . realPart . eval . recip . (vy - i*||vx))
        -- @-node:gcross.20091220115426.1747:phase X derivative
        -- @+node:gcross.20091220115426.1749:phase Y derivative
        ,testProperty "phase Y derivative" $
            ((=~=) `on` (.map abs.getArg))
                (imagPart . eval . d Y . log . (vy - i*||vx))
                (imagPart . eval . recip . (vy - i*||vx))
        -- @-node:gcross.20091220115426.1749:phase Y derivative
        -- @-others
        ]
    -- @-node:gcross.20091220115426.1516:Phases
    -- @-others
    -- @nonl
    -- @-node:gcross.20091204093401.3593:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091204093401.3554:@thin test-fock.hs
-- @-leo
