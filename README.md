This is an Xcode Playground to play around with elliptic curve algebra. It requires Xcode to run.

Sample usage:

```swift
let curve = EllipticCurve(p: 13, a: 0, b: 1)!
let randomCurve = generateCurve(p: 13)

let allElements = EllipticCurveElement.all(curve: curve)
let traces = allElements.map{ trace(element: $0) }
```

Sample output for `y^2 = x^3 + 1 mod 13`:

```
(0,12) > (0,1) > ∞ [Order: 3]
(0,1) > (0,12) > ∞ [Order: 3]
(2,10) > (0,12) > (12,0) > (0,1) > (2,3) > ∞ [Order: 6]
(2,3) > (0,1) > (12,0) > (0,12) > (2,10) > ∞ [Order: 6]
(4,0) > ∞ [Order: 2]
(5,10) > (0,12) > (4,0) > (0,1) > (5,3) > ∞ [Order: 6]
(5,3) > (0,1) > (4,0) > (0,12) > (5,10) > ∞ [Order: 6]
(6,10) > (0,12) > (10,0) > (0,1) > (6,3) > ∞ [Order: 6]
(6,3) > (0,1) > (10,0) > (0,12) > (6,10) > ∞ [Order: 6]
(10,0) > ∞ [Order: 2]
(12,0) > ∞ [Order: 2]
∞ [Order: 1]
```