
class Chassis {}
class RacingChassis : Chassis {}
class SuperChassis : RacingChassis {}

class Car {
    private var chassis: Chassis? = nil
    func getChassis() -> Chassis? {
        return chassis
    }
    
    func setChassis(chassis: Chassis) {
        self.chassis = chassis
    }
}

class RaceCar: Car {
    private var chassis: RacingChassis {
        get {
            return getChassis() as! RacingChassis
        }
        set {
            setChassis(chassis: newValue)
        }
    }
    
    override init() {
        super.init()
        
        chassis = RacingChassis()
    }
}

class SuperCar: RaceCar {
    private var chassis: SuperChassis {
        get {
            return getChassis() as! SuperChassis
        }
        set {
            setChassis(chassis: newValue)
        }
    }
    
    override init() {
        super.init()
        
        chassis = SuperChassis()
    }
}
protocol Color {
    var color : String { get }
}

protocol RedColor: Color {
    
}

extension Color {
    var color : String {
        get {return "Default color"}
    }
}

extension RedColor {
    var color : String {
        get {return "Red color"}
    }
}

protocol PrintColor {
    func getColor() -> String
}

extension PrintColor where Self: Color {
    func getColor() -> String {
        return color
    }
}

class A : Color, PrintColor {
    
}

class B : RedColor, PrintColor {
    
}

class C : A, RedColor {
    
}

let aaa = A().getColor() // "Default color"
let bbb = B().getColor() // "Red color"
let ccc = C().getColor() // "Default color"

