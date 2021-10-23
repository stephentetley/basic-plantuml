

def main(_args: Array[String]): Int32 & Impure = 
    use PlantUml/Puml.{creole, startuml, enduml};
    use PlantUml/Puml.{<!>};
    use PlantUml/Creole.{strong, text};
    let d1 = startuml() <!> creole(strong(text("strongly emphasized"))) <!> enduml();
    PlantUml/Puml.render(d1) |> println;
    0