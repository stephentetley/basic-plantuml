use Data.{RoseTree};
use Data.RoseTree.{RoseTree};
use PlantUml/Puml.{Puml};
use PlantUml/Creole.{Creole};

def main(_args: Array[String]): Int32 & Impure = 
    use PlantUml/Puml.{creole, startuml, enduml};
    use PlantUml/Puml.{<!>};
    use PlantUml/Creole.{monospaced, text};
    let d1 = startuml() <!> creole(monospaced(text("monospaced"))) <!> enduml();
    PlantUml/Puml.render(d1) |> println;
    tree1() |> PlantUml/Puml.render |> println;
    tree2() |> PlantUml/SaltTree.tree({printLabel = PlantUml/Creole.text}) |> PlantUml/Puml.render |> println;
    let headers = List.map(PlantUml/Creole.text, "Name" :: "Props" :: "Size" :: Nil);
    tree3() |> PlantUml/SaltTreeTable.treeTable({printCells = cellPrint}, headers) |> PlantUml/Puml.render |> println;
    0


def tree1(): Puml = 
    use PlantUml/Puml.{startsalt, endsalt, braces, text, vsep, treeNode, tree};
    use PlantUml/Puml.{<!>};
    let n1 = treeNode(1, text("/home"));
    let n2 = treeNode(2, text("/stephen"));
    startsalt() 
        <!> braces(None, 0, tree(n1 :: n2 :: Nil))
        <!> endsalt()

def tree2(): RoseTree[String] = 
    let leaf = x1 -> RoseTree(x1, Nil);
    let node = (x1, xs) -> RoseTree(x1, xs);
    node("/home", leaf("/stephen") :: Nil)


def tree3(): RoseTree[{name :: String, props :: String, size :: Int32}] = 
    let leaf = x1 -> RoseTree(x1, Nil);
    let node = (x1, kids) -> RoseTree(x1, kids);
    node({name = "/home", props = "rwx", size = 145423 }, leaf({name = "/stephen", props = "rwx", size = 1200}) :: Nil)

def cellPrint(x: {name :: String, props :: String, size :: Int32}): List[Creole] = 
    use PlantUml/Creole.{text, asText}; 
    text(x.name) :: text(x.props) :: asText(x.size) :: Nil
