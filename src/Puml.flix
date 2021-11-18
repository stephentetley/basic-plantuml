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

    /// Track indentation with Group...
    pub enum Puml {
        case Empty
        case Group(Int32, Puml)
        case Text(String)
        case Creole(Creole)
        case HCat(Puml, Puml)
        case VCat(Puml, Puml)
    }

    ///
    /// Render `doc` to a file.
    ///
    pub def writePuml(path: System.FilePath, cs: Text.Charset, doc: Puml): Result[Unit, String] & Impure =
        use Result.flatMap;
        use System/IO.WriteMode.{Create};
        let str = render(doc);
        System/IO.writeFileWith({path = path, charset = cs, mode = Create}, str)

    pub def render(doc: Puml): String = 
        renderHelper(doc) as & Pure

    def renderHelper(doc: Puml): String & Impure = 
        let sb = StringBuilder.new();
        appendPuml!(sb, 0, doc);
        StringBuilder.toString(sb)

    def appendPuml!(sb: StringBuilder, indent: Int32, doc: Puml): Unit & Impure = match doc {
        case Empty => () as & Impure
        case Text(s) => StringBuilder.appendString!(sb, s)
        case Creole(c) => PlantUml/Creole.appendCreole!(sb, c)
        case Group(i, d1) => appendPuml!(sb, indent + i, d1)
        case HCat(d1, d2) => { 
            appendPuml!(sb, indent, d1);
            appendPuml!(sb, indent, d2)
        }
        case VCat(d1, d2) => { 
            appendPuml!(sb, indent, d1);
            StringBuilder.appendLineSeparator!(sb);
            StringBuilder.appendString!(sb, String.repeat(indent, " "));
            appendPuml!(sb, indent, d2)
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
    pub def hsep(docs: List[Puml]): Puml = foldPuml(x -> y -> x <<>> y, docs)

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

    pub def group(indent: Int32, body: Puml): Puml = Group(indent, body)

    pub def creole(doc: Creole): Puml = Creole(doc)

    pub def repeatString(n: Int32, s: String): Puml = 
        String.repeat(n, s) |> text

    pub def comment(s: String): Puml = 
        String.lines(s) |> List.map(s1 -> text("' ${s1}")) |> vsep

    pub def doubleQuote(s: String): Puml = enclose(char('"'), char('"'), text(s))
        

    pub def startuml(): Puml = Text("@startuml")
    pub def enduml(): Puml = Text("@enduml")

    /// TODO - Salt and wbs etc. in separate namespaces?

    pub def startsalt(): Puml = Text("@startsalt")
    pub def endsalt(): Puml = Text("@endsalt")

    pub def treeNode(level: Int32, label: Puml): Puml = 
        repeatString(level, "+") <<>> label

    pub def tree(nodes: List[Puml]): Puml = 
        braces(Some("T"), 1, vsep(nodes))

    pub def treeTableHeaders(headers: List[Puml]): Puml = 
        text("+") <> hsep(punctuate(text(" |"), headers))

    pub def treeTableNode(level: Int32, cells: List[Puml]): Puml = 
        repeatString(level, "+") <<>> hsep(punctuate(text(" |"), cells))

    pub def treeTable(nodes: List[Puml]): Puml = 
        braces(Some("T"), 1, vsep(nodes))

    pub def startwbs(): Puml = Text("@startwbs")
    pub def endwbd(): Puml = Text("@endwbs")

    pub def braces(symbol: Option[String], indent: Int32, body: Puml): Puml = match symbol { 
        case Some(s1) => group(indent, text("{${s1}") <!> body) <!> text("}")
        case None => group(indent, text("{") <!> body) <!> text("}")
    }

}
