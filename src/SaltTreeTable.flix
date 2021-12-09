/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


namespace PlantUml/SaltTreeTable {

    
    use Data.{RoseTree, RoseForest};
    use Data.RoseTree.{Node};
    use PlantUml/Puml.{Puml};
    use PlantUml/Creole.{Creole};

    
    pub def treeTable(printer: {printCells :: a -> List[Creole]}, headers: List[Creole], x: RoseTree[a]): Puml = 
        use PlantUml/Puml.{startsalt, endsalt, creole, braces, treeTableHeaders, tree};
        use PlantUml/Puml.{<!>};
        let headers1 = List.map(creole, headers) |> treeTableHeaders;
        let mkPuml = xs -> startsalt() <!> braces(None, 0, tree(headers1 :: xs)) <!> endsalt();
        match x {
            case Node(label, kids) => { 
                let root = printTreeTableCells(printer, 1, label);
                saltTreeTableKids(printer, 2, kids, ks -> mkPuml(root :: ks))
            }
        }

    def printTreeTableCells(printer: {printCells :: a -> List[Creole]}, level: Int32, node: a): Puml = 
        use PlantUml/Puml.{treeTableNode, creole};
        let mkCells = printer.printCells;
        treeTableNode(level, List.map(creole, mkCells(node)))

    def saltTreeTableNode(printer: {printCells :: a -> List[Creole]}, level: Int32, node: RoseTree[a], k: Puml -> Puml): Puml = 
        use PlantUml/Puml.{vsep, creole};
        use PlantUml/Puml.{<!>};
        match node {
            case Node(x1, Nil) => { 
                let ans = printTreeTableCells(printer, level, x1);
                k(ans)
            }
            case Node(label, kids) => { 
                let ans = printTreeTableCells(printer, level, label);
                saltTreeTableKids(printer, level + 1, kids, vs -> k(ans <!> vsep(vs)))
            }
        }   

    def saltTreeTableKids(printer: {printCells :: a -> List[Creole]}, level: Int32, nodes: RoseForest[a], k: List[Puml] -> Puml): Puml = match nodes { 
        case Nil => k(Nil)
        case x :: rs => 
            saltTreeTableNode(printer, level, x, v1 -> saltTreeTableKids(printer, level, rs, vs -> k(v1 :: vs)))
    }
        


}
