use PlantUml/Puml.{Puml};

def main(_args: Array[String]): Int32 & Impure = 
    use PlantUml/Puml.{creole, startuml, enduml};
    use PlantUml/Puml.{<!>};
    use PlantUml/Creole.{strong, text};
    let d1 = startuml() <!> creole(strong(text("strongly emphasized"))) <!> enduml();
    PlantUml/Puml.render(d1) |> println;
    tree1() |> PlantUml/Puml.render |> println;
    0


def tree1(): Puml = 
    use PlantUml/Puml.{startsalt, endsalt, braces, text, vsep, treenode};
    use PlantUml/Puml.{<!>};
    let n1 = treenode(1, text("home"));
    let n2 = treenode(2, text("stephen"));
    startsalt() 
        <!> braces(None, braces(Some("T"), vsep(n1 :: n2 :: Nil)))
        <!> endsalt()