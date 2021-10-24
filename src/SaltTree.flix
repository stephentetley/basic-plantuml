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


namespace PlantUml/SaltTree {

    
    use Data.{RoseTree, RoseForest};
    use Data.RoseTree.{RoseTree};
    use Data/RoseTree.{Forest};
    use PlantUml/Puml.{Puml};
    use PlantUml/Creole.{Creole};



    pub def tree(printer: {printLabel :: a -> Creole}, x: RoseTree[a]): Puml = 
        use PlantUml/Puml.{startsalt, endsalt, braces, tree};
        use PlantUml/Puml.{<!>};
        let mkPuml = xs -> startsalt() <!> braces(None, 0, tree(xs)) <!> endsalt();
        match x {
            case RoseTree(x1, kids) => { 
                let root = printTreeLabel(printer, 1, x1);
                saltTreeKids(printer, 2, kids, ks -> mkPuml(root :: ks))
            }
        }

    def printTreeLabel(printer: {printLabel :: a -> Creole}, level: Int32, node: a): Puml = 
        use PlantUml/Puml.{treeNode, creole};
        let mkLabel = printer.printLabel;
        treeNode(level, creole(mkLabel(node)))

    def saltTreeNode(printer: {printLabel :: a -> Creole}, level: Int32, node: RoseTree[a], k: Puml -> Puml): Puml = 
        use PlantUml/Puml.{vsep, creole};
        use PlantUml/Puml.{<!>};
        match node {
            case RoseTree(label, Nil) => { 
                let ans = printTreeLabel(printer, level, label);
                k(ans)
            }
            case RoseTree(label, kids) => { 
                let ans = printTreeLabel(printer, level, label);
                saltTreeKids(printer, level + 1, kids, vs -> k(ans <!> vsep(vs)))
            }
        }   

    def saltTreeKids(printer: {printLabel :: a -> Creole}, level: Int32, nodes: RoseForest[a], k: List[Puml] -> Puml): Puml = match nodes { 
        case Nil => k(Nil)
        case x :: rs => 
            saltTreeNode(printer, level, x, v1 -> saltTreeKids(printer, level, rs, vs -> k(v1 :: vs)))
    }
        


}
