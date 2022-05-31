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

    
namespace PlantUml/Creole {


    pub enum Creole {
        case Text(String)
        case HCat(Creole, Creole)
        case VCat(Creole, Creole)
    }


    pub def render(doc: Creole): String = 
        region r {
            let sb = new StringBuilder(r);
            appendCreole!(doc, sb);
            StringBuilder.toString(sb)
        }

    pub def appendCreole!(doc: Creole, sb: StringBuilder[r]): Unit \ Write(r) = match doc {
        case Text(s) => StringBuilder.appendString!(s, sb) 
        case HCat(d1, d2) => { 
            appendCreole!(d1, sb);
            appendCreole!(d2, sb)
        }
        case VCat(d1, d2) => { 
            appendCreole!(d1, sb);
            StringBuilder.appendLineSeparator!(sb);
            appendCreole!(d2, sb)
        }
    }

    
    pub def text(s: String): Creole = Text(s)
    pub def char(c: Char): Creole = Text("${c}")
    
    pub def asText(x: a): Creole with ToString[a] = Text(ToString.toString(x))

    pub def hcat(d1: Creole, d2: Creole): Creole = HCat(d1, d2)
    pub def <>(d1: Creole, d2: Creole): Creole = hcat(d1, d2)
    pub def <<>>(d1: Creole, d2: Creole): Creole = hcat(d1, hcat(text(" "), d2))
    pub def <!>(x: Creole, y: Creole): Creole = VCat(x, y)

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    ///
    pub def enclose(l: Creole, r: Creole, body: Creole): Creole = l <> body <> r


    pub def italics(doc: Creole): Creole = enclose(text("//"), text("//"), doc)
    pub def bold(doc: Creole): Creole = enclose(text("**"), text("**"), doc)
    pub def monospaced(doc: Creole): Creole = enclose(text("\"\""), text("\"\""), doc)
    pub def stroked(doc: Creole): Creole = enclose(text("--"), text("--"), doc)
    pub def underlined(doc: Creole): Creole = enclose(text("__"), text("__"), doc)
    pub def waved(doc: Creole): Creole = enclose(text("~~"), text("~~"), doc)

    pub def listItem(doc: Creole): Creole = text("*") <<>> doc
    pub def subItem(doc: Creole): Creole = text("**") <<>> doc

    pub def numberedListItem(doc: Creole): Creole = text("#") <<>> doc
    pub def numberedSubItem(doc: Creole): Creole = text("##") <<>> doc

    pub def escaped(doc: Creole): Creole = char('~') <> doc

    pub def hline(): Creole = text("----")
    pub def doubleLine(): Creole = text("====")
    pub def strongLine(): Creole = text("____")
    pub def dottedLine(): Creole = text("....")

    pub def hlineWith(title: String): Creole = text("--${title}--")
    pub def doubleLineWith(title: String): Creole = text("==${title}==")
    pub def strongLineWith(title: String): Creole = text("__${title}__")
    pub def dottedLineWith(title: String): Creole = text("..${title}..")


    pub def color(name: String): Creole = text("<#${name}>")

    pub def oldColor(name: String): Creole = text("<color:${name}>")

    pub def code(body: String): Creole = text("<code>") <!> text(body) <!> text("</code>")


}
