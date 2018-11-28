import Foundation

struct Representative: Equatable, Hashable, CustomStringConvertible {

    let r: Int
    let q: Int

    init(_ value: Int, mod q: Int) {
        let remainder = value % q
        self.r = remainder < 0 ? remainder + q : remainder
        self.q = q
    }

    static func all(mod q: Int) -> [Representative] {
        return (0...q-1).map{ Representative($0, mod: q) }
    }

    static func == (lhs: Representative, rhs: Representative) -> Bool {
        return lhs.r == rhs.r
    }

    static prefix func - (e: Representative) -> Representative {
        return Representative(e.q - e.r, mod: e.q)
    }

    static func +(lhs: Representative, rhs: Representative) -> Representative {
        return Representative((lhs.r + rhs.r) % lhs.q, mod: lhs.q)
    }

    static func -(lhs: Representative, rhs: Representative) -> Representative {
        return lhs + (-rhs)
    }

    static func *(lhs: Representative, rhs: Representative) -> Representative {
        return Representative((lhs.r * rhs.r) % lhs.q, mod: lhs.q)
    }

    static func /(lhs: Representative, rhs: Representative) -> Representative {
        return lhs * rhs.pow(rhs.q - 2)
    }

    func times(_ t: Int) -> Representative {
        return t == 1 ? self : ( self + self.times(t-1) )
    }

    func pow(_ e: Int) -> Representative {
        return e == 1 ? self : ( self * self.pow(e-1) )
    }

    var hashValue: Int { return r }

    var description: String { return String(r) }
}

struct EllipticCurve: CustomStringConvertible {

    struct Point: Equatable, Hashable, CustomStringConvertible {

        let x: Representative
        let y: Representative

        static func == (lhs: Point, rhs: Point) -> Bool {
            return lhs.x == rhs.x && lhs.y == rhs.y
        }

        static prefix func - (point: Point) -> Point {
            return Point(x: point.x, y: -point.y)
        }

        var description: String { return "(\(x),\(y))" }
    }

    let a: Representative
    let b: Representative
    let points: [Point]

    init?(p: Int, a: Int, b: Int) {

        let a = Representative(a, mod: p)
        let b = Representative(b, mod: p)

        guard a.pow(3).times(4) != -(b.pow(2).times(27)) else {
            return nil
        }

        let reps = Representative.all(mod: p)

        var roots: [Representative: Representative] = [:]
        reps.forEach{
            let square = $0.pow(2)
            roots[square] = $0
        }
        var points: [Point] = []

        for x in reps {
            let fx = x.pow(3) + x*a + b
            guard let y = roots[fx] else {
                continue
            }
            let p = Point(x: x, y: y)
            points.append(p)
            if p != -p {
                points.append(-p)
            }
        }

        self.a = a
        self.b = b
        self.points = points
    }

    func slope(in p: Point) -> Representative {
        return ( p.x.pow(2).times(3) + a ) / p.y.times(2)
    }

    func slope(between p: Point, and q: Point) -> Representative {
        return (p.y - q.y) / (p.x - q.x)
    }

    var description: String { return "y^2 = x^3 + \(a.description)x + \(b.description) mod \(a.q)" }
}

struct EllipticCurveElement: Equatable, CustomStringConvertible {

    let point: EllipticCurve.Point?
    let curve: EllipticCurve

    static func all(curve: EllipticCurve) -> [EllipticCurveElement] {
        var elements = curve.points.map { EllipticCurveElement(point: $0, curve: curve) }
        elements.append( EllipticCurveElement(point: nil, curve: curve) )
        return elements
    }

    func add(_ e: EllipticCurveElement) -> EllipticCurveElement {

        guard let p = point else {
            return e
        }
        guard let q = e.point else {
            return self
        }
        guard p != -q else {
            return EllipticCurveElement(point: nil, curve: curve)
        }
        let slope = p == q ? curve.slope(in: p) : curve.slope(between: p, and: q)
        let x = slope.pow(2) - p.x - q.x
        let y = ( -slope * (x - p.x) ) - p.y
        let r = EllipticCurve.Point(x: x, y: y)
        return EllipticCurveElement(point: r, curve: curve)
    }

    func times(_ t: Int) -> EllipticCurveElement {
        return t == 1 ? self : self.add( self.times(t-1) )
    }

    static func == (lhs: EllipticCurveElement, rhs: EllipticCurveElement) -> Bool {
        return lhs.point == rhs.point
    }

    var description: String { return point?.description ?? "∞" }
}

func generateCurve(p: Int) -> EllipticCurve {
    while true {
        let a = Int(arc4random_uniform(UInt32(p)))
        let b = Int(arc4random_uniform(UInt32(p)))
        if let curve = EllipticCurve(p: p, a: a, b: b) {
            return curve
        }
    }
}

func trace(element: EllipticCurveElement) -> String {

    var trace = element.description
    var order = 1
    var r = element
    while r.point != nil {
        r = r.add(element)
        order = order + 1
        trace += " > " + r.description
    }
    return trace + " [Order: \(order)]"
}

//let curve = generateCurve(p: 13)
let curve = EllipticCurve(p: 13, a: 0, b: 1)!
print(curve)

let elements = EllipticCurveElement.all(curve: curve)
print("-- \(elements.count) Elements --")

let traces = elements.map{ trace(element: $0) }
traces.forEach{ print($0) }
