-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.2088:@thin test-phase.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.2089:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix

import Data.Complex

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Quantum
import Data.Differentiable.Quantum.MultiParticle
import Data.Differentiable.Testing

import System.IO.Unsafe
import System.Random

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
-- @-node:gcross.20091220080702.2089:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220080702.2090:Constants
bigOmega :: Complex Double
bigOmega = unsafePerformIO randomIO

om :: Complex Double
om = unsafePerformIO randomIO

om_z :: Complex Double
om_z = unsafePerformIO randomIO

number_of_particles = 5
all_variables = [(0 :> X)..((number_of_particles-1) :> Z)]

n = fromIntegral . length $ all_variables
-- @-node:gcross.20091220080702.2090:Constants
-- @+node:gcross.20091220080702.2091:Types
-- @+node:gcross.20091220080702.2092:QuantumOperator
type QuantumOperator = DifferentialOperator (Complex Double)
-- @-node:gcross.20091220080702.2092:QuantumOperator
-- @-node:gcross.20091220080702.2091:Types
-- @+node:gcross.20091220080702.2093:Operators
-- @+node:gcross.20091220080702.2094:x, y, z
x_, y_, z_ :: Int -> QuantumOperator
x_ k = r_ (k :> X)
y_ k = r_ (k :> Y)
z_ k = r_ (k :> Z)
-- @-node:gcross.20091220080702.2094:x, y, z
-- @+node:gcross.20091220080702.2095:px, py, pz
px_, py_, pz_ :: Int -> QuantumOperator
px_ k = p_ (k :> X)
py_ k = p_ (k :> Y)
pz_ k = p_ (k :> Z)
-- @-node:gcross.20091220080702.2095:px, py, pz
-- @+node:gcross.20091220080702.2096:a, b, c
a_  k = sqrt(om/2)  *|x_ k + i*sqrt(1/(2*om))  *|px_ k
at_ k = sqrt(om/2)  *|x_ k - i*sqrt(1/(2*om))  *|px_ k
b_  k = sqrt(om/2)  *|y_ k + i*sqrt(1/(2*om))  *|py_ k
bt_ k = sqrt(om/2)  *|y_ k - i*sqrt(1/(2*om))  *|py_ k
c_  k = sqrt(om_z/2)*|z_ k + i*sqrt(1/(2*om_z))*|pz_ k
ct_ k = sqrt(om_z/2)*|z_ k - i*sqrt(1/(2*om_z))*|pz_ k
-- @-node:gcross.20091220080702.2096:a, b, c
-- @+node:gcross.20091220080702.2097:g, n
g_  k = 1/sqrt 2*|(a_  k + i*|b_  k)
gt_ k = 1/sqrt 2*|(at_ k - i*|bt_ k)
n_  k = 1/sqrt 2*|(b_  k + i*|a_  k)
nt_ k = 1/sqrt 2*|(bt_ k - i*|at_ k)
-- @-node:gcross.20091220080702.2097:g, n
-- @+node:gcross.20091220080702.2098:jx, jy, jz
jx_ k = y_ k.pz_ k - z_ k.py_ k
jy_ k = z_ k.px_ k - x_ k.pz_ k
jz_ k = x_ k.py_ k - y_ k.px_ k
-- @-node:gcross.20091220080702.2098:jx, jy, jz
-- @-node:gcross.20091220080702.2093:Operators
-- @+node:gcross.20091220080702.2147:Harmonic Oscillator
-- @+node:gcross.20091220080702.2150:hamiltonian
hamiltonian = sum [(1/2) *| p_ k.p_ k + (om*om)/2 *| r_ k.r_ k | k <- all_variables]
-- @nonl
-- @-node:gcross.20091220080702.2150:hamiltonian
-- @+node:gcross.20091220080702.2148:ground state
ground_state :: DifferentiableFunction (Complex Double)
ground_state = product [exp ((-om/2) *|| v_ k * v_ k) | k <- all_variables]
-- @-node:gcross.20091220080702.2148:ground state
-- @-node:gcross.20091220080702.2147:Harmonic Oscillator
-- @+node:gcross.20091220080702.2392:Passed tests
-- @-node:gcross.20091220080702.2392:Passed tests
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091220080702.2109:<< Tests >>
    -- @+others
    -- @+node:gcross.20091220080702.2151:Correct ground state
    [testProperty "Correct ground state" $
        hamiltonian . ground_state . getArg =~= ((n*om/2) *|| ground_state) . getArg
    -- @-node:gcross.20091220080702.2151:Correct ground state
    -- @-others
    -- @nonl
    -- @-node:gcross.20091220080702.2109:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091220080702.2088:@thin test-phase.hs
-- @-leo
