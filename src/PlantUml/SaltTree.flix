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

    
    use FlixSandbox.{RoseTree, RoseForest};
    use FlixSandbox.RoseTree.{Node};
    use PlantUml/Puml.{Puml};
    use PlantUml/Puml.{block, repeatString, creole, vsep};
    use PlantUml/Puml.{<<>>, <!>};
    use PlantUml/Creole.{Creole};

    /// Note - it seems like "new style" colours are not supported in Creole



    pub def treeNode(level: Int32, label: Puml): Puml = 
        repeatString(level, "+") <<>> label

    pub def saltTree(nodes: List[Puml]): Puml = 
        block(Some("T"), 1, vsep(nodes))


    pub def printTree(printer: {printLabel :: a -> Creole}, x: RoseTree[a]): Puml = 
        use PlantUml/Puml.{startsalt, endsalt};
        let mkPuml = xs -> startsalt() <!> block(None, 0, saltTree(xs)) <!> endsalt();
        match x {
            case Node(x1, kids) => { 
                let root = printTreeLabel(printer, 1, x1);
                saltTreeKids(printer, 2, kids, ks -> mkPuml(root :: ks))
            }
        }


    def printTreeLabel(printer: {printLabel :: a -> Creole}, level: Int32, node: a): Puml = 
        let mkLabel = printer.printLabel;
        treeNode(level, creole(mkLabel(node)))

    def saltTreeNode(printer: {printLabel :: a -> Creole}, level: Int32, node: RoseTree[a], k: Puml -> Puml): Puml = 
        match node {
            case Node(label, Nil) => { 
                let ans = printTreeLabel(printer, level, label);
                k(ans)
            }
            case Node(label, kids) => { 
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
