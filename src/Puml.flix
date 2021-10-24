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

    
namespace PlantUml/Puml {

    use PlantUml/Creole.Creole;

    /// Ideally we should track indentation somewhere...
    pub enum Puml {
        case Empty
        case Text(String)
        case Creole(Creole)
        case HCat(Puml, Puml)
        case VCat(Puml, Puml)
    }



    pub def render(doc: Puml): String = 
        renderHelper(doc) as & Pure

    def renderHelper(doc: Puml): String & Impure = 
        let sb = StringBuilder.new();
        appendPuml!(sb, doc);
        StringBuilder.toString(sb)

    def appendPuml!(sb: StringBuilder, doc: Puml): Unit & Impure = match doc {
        case Empty => () as & Impure
        case Text(s) => StringBuilder.appendString!(sb, s)
        case Creole(c) => PlantUml/Creole.appendCreole!(sb, c) 
        case HCat(d1, d2) => { 
            appendPuml!(sb, d1);
            appendPuml!(sb, d2)
        }
        case VCat(d1, d2) => { 
            appendPuml!(sb, d1);
            StringBuilder.appendLineSeparator!(sb);
            appendPuml!(sb, d2)
        }
    }
    

        // ************************************************************************
    // List concatenation 

    pub def foldPuml(op: (Puml, Puml) -> Puml, docs: List[Puml]): Puml = 
        match docs {
            case Nil => empty()
            case x :: rs => foldPumlHelper(op, x, rs, identity)
        }

    def foldPumlHelper(op: (Puml, Puml) -> Puml, ac: Puml, docs:List[Puml], cont: Puml -> Puml): Puml = 
        match docs {
            case Nil => cont(ac)
            case x :: rs => foldPumlHelper(op, op(ac, x), rs, cont)
        }

    pub def punctuate(sep: Puml, docs: List[Puml]): List[Puml] = punctuateHelper(sep, docs, xs -> xs)


    pub def punctuateHelper(sep: Puml, ds: List[Puml], k: List[Puml] -> List[Puml]): List[Puml] = match ds {
        case Nil => k(Nil)
        case d :: Nil => k(d :: Nil)
        case d :: rs    => punctuateHelper(sep, rs, xs -> k((d <> sep) :: xs))
    }

    pub def vsep(docs: List[Puml]): Puml = foldPuml(x -> y -> x <!> y, docs)

    pub def intersperse(sepa: Puml, docs:List[Puml]): Puml = vsep(punctuate(sepa, docs))

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    ///
    pub def enclose(l: Puml, r: Puml, body: Puml): Puml = l <> body <> r


    pub def <>(x: Puml, y: Puml): Puml = match (x, y) { 
        case (Empty, _) => y
        case (_, Empty) => x 
        case _ => HCat(x, y)
    }

    pub def <<>>(x: Puml, y: Puml): Puml = match (x, y) { 
        case (Empty, _) => y
        case (_, Empty) => x 
        case _ => x <> Text(" ") <> y
    } 

    pub def <!>(x: Puml, y: Puml): Puml = match (x, y) { 
        case (Empty, _) => y
        case (_, Empty) => x 
        case _ => VCat(x, y)
    }

    pub def empty(): Puml = Empty

    pub def text(s: String): Puml = Text(s)

    pub def char(c: Char): Puml = Text("${c}")

    pub def creole(doc: Creole): Puml = Creole(doc)

    pub def repeatString(s: String, n: Int32): Puml = 
        String.repeat(s, n) |> text

    pub def comment(s: String): Puml = 
        String.lines(s) |> List.map(s1 -> text("' ${s1}")) |> vsep

    pub def doubleQuote(s: String): Puml = enclose(char('"'), char('"'), text(s))
        

    pub def startuml(): Puml = Text("@startuml")
    pub def enduml(): Puml = Text("@enduml")

    /// TODO - Salt and wbs etc. in separate namespaces?

    pub def startsalt(): Puml = Text("@startsalt")
    pub def endsalt(): Puml = Text("@endsalt")

    pub def treenode(level: Int32, body: Puml): Puml = 
        repeatString("+", level) <<>> body

    pub def startwbs(): Puml = Text("@startwbs")
    pub def endwbd(): Puml = Text("@endwbs")

    pub def braces(symbol: Option[String],  body: Puml): Puml = match symbol { 
        case Some(s1) => vsep(text("{${s1}") :: body :: text("}") :: Nil)
        case None => vsep(text("{") :: body :: text("}") :: Nil)
    }

}
