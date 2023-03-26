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


    use Basis/NIO/File.Path
    use Basis/NIO/Charset.Charset

    use PlantUml/Creole.Creole
    use PlantUml/Puml.Puml.{
        Empty, Group, Text, Creole, HCat, VCat
    }

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
    pub def writePuml(path: Path, cs: Charset, doc: Puml): Result[String, Unit] \ IO =
        use Result.flatMap;
        let opts = List.map(Basis/NIO/File/StandardOpenOption.toOpenOption, Basis/NIO/File/StandardOpenOption/StandardOpenOption.Create :: Nil);
        let str = render(doc);
        Basis/NIO/File/Files.writeStringWithCharset!(path, cs, opts, str)

    pub def render(doc: Puml): String = region rc {
        let sb = StringBuilder.new(rc);
        appendPuml!(0, doc, sb);
        StringBuilder.toString(sb)
    }

    def appendPuml!(indent: Int32, doc: Puml, sb: StringBuilder[r]): Unit \ Write(r) = match doc {
        case Empty        => ()
        case Text(s)      => StringBuilder.appendString!(s, sb)
        case Creole(c)    => PlantUml/Creole.appendCreole!(c, sb)
        case Group(i, d1) => appendPuml!(indent + i, d1, sb)
        case HCat(d1, d2) => { 
            appendPuml!(indent, d1, sb);
            appendPuml!(indent, d2, sb)
        }
        case VCat(d1, d2) => { 
            appendPuml!(indent, d1, sb);
            StringBuilder.appendLineSeparator!(sb);
            StringBuilder.appendString!(String.repeat(indent, " "), sb);
            appendPuml!(indent, d2, sb)
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


    pub def startsalt(): Puml = Text("@startsalt")
    pub def endsalt(): Puml = Text("@endsalt")



    // TODO rename to `block`
    pub def block(symbol: Option[String], indent: Int32, body: Puml): Puml = match symbol { 
        case Some(s1) => group(indent, text("{${s1}") <!> body) <!> text("}")
        case None     => group(indent, text("{") <!> body) <!> text("}")
    }

    // Field modifier
    pub def field(body: Puml): Puml = text("{field}") <<>> body

    // Method modifier
    pub def method(body: Puml): Puml = text("{method}") <<>> body

}
