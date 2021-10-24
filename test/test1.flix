use Data.{RoseTree};
use Data.RoseTree.{RoseTree};
use PlantUml/Puml.{Puml};

def main(_args: Array[String]): Int32 & Impure = 
    use PlantUml/Puml.{creole, startuml, enduml};
    use PlantUml/Puml.{<!>};
    use PlantUml/Creole.{monospaced, text};
    let d1 = startuml() <!> creole(monospaced(text("monospaced"))) <!> enduml();
    PlantUml/Puml.render(d1) |> println;
    tree1() |> PlantUml/Puml.render |> println;
    tree2() |> PlantUml/SaltTree.saltTree({printNode = PlantUml/Creole.text}) |> PlantUml/Puml.render |> println;
    0


def tree1(): Puml = 
    use PlantUml/Puml.{startsalt, endsalt, braces, text, vsep, treenode, tree};
    use PlantUml/Puml.{<!>};
    let n1 = treenode(1, text("/home"));
    let n2 = treenode(2, text("/stephen"));
    startsalt() 
        <!> braces(None, 0, tree(n1 :: n2 :: Nil))
        <!> endsalt()

def tree2(): RoseTree[String] = 
    let leaf = x1 -> RoseTree(x1, Nil);
    let node = (x1, xs) -> RoseTree(x1, xs);
    node("/home", leaf("/stephen") :: Nil)
