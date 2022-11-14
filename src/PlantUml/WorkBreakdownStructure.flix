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


namespace PlantUml/WorkBreakdownStructure {

    
    use DataLib.{RoseTree, RoseForest};
    use DataLib.RoseTree.{Node};
    use PlantUml/Puml.{Puml};
    use PlantUml/Puml.{text, repeatString, vsep};
    use PlantUml/Puml.{<<>>, <!>};

    /// What is different to salttree?


    pub def startwbs(): Puml = text("@startwbs")
    pub def endwbs(): Puml = text("@endwbs")
    
    pub def treeNode(level: Int32, label: Puml): Puml = 
        repeatString(level, "*") <<>> label

    

    pub def printTree(printer: {printLabel = a -> Puml}, x: RoseTree[a]): Puml = 
        let mkPuml = xs -> startwbs() <!> vsep(xs) <!> endwbs();
        match x {
            case Node(x1, kids) => { 
                let root = printTreeLabel(printer, 1, x1);
                wbsTreeKids(printer, 2, kids, ks -> mkPuml(root :: ks))
            }
        }

    def printTreeLabel(printer: {printLabel = a -> Puml}, level: Int32, node: a): Puml = 
        let mkLabel = printer.printLabel;
        treeNode(level, mkLabel(node))

    def wbsTreeNode(printer: {printLabel = a -> Puml}, level: Int32, node: RoseTree[a], k: Puml -> Puml): Puml = 
        match node {
            case Node(label, Nil) => { 
                let ans = printTreeLabel(printer, level, label);
                k(ans)
            }
            case Node(label, kids) => { 
                let ans = printTreeLabel(printer, level, label);
                wbsTreeKids(printer, level + 1, kids, vs -> k(ans <!> vsep(vs)))
            }
        }   

    def wbsTreeKids(printer: {printLabel = a -> Puml}, level: Int32, nodes: RoseForest[a], k: List[Puml] -> Puml): Puml = match nodes { 
        case Nil => k(Nil)
        case x :: rs => 
            wbsTreeNode(printer, level, x, v1 -> wbsTreeKids(printer, level, rs, vs -> k(v1 :: vs)))
    }
        


}
