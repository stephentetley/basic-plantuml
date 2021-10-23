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
        renderHelper(doc) as & Pure

    def renderHelper(doc: Creole): String & Impure = 
        let sb = StringBuilder.new();
        appendCreole!(sb, doc);
        StringBuilder.toString(sb)

    pub def appendCreole!(sb: StringBuilder, doc: Creole): Unit & Impure = match doc {
        case Text(s) => StringBuilder.appendString!(sb, s) 
        case HCat(d1, d2) => { 
            appendCreole!(sb, d1);
            appendCreole!(sb, d2)
        }
        case VCat(d1, d2) => { 
            appendCreole!(sb, d1);
            StringBuilder.appendLineSeparator!(sb);
            appendCreole!(sb, d2)
        }
    }

    /// TODO - Creole and Puml in seperate namespaces?

    pub def text(s: String): Creole = Text(s)

    pub def hcat(d1: Creole, d2: Creole): Creole = HCat(d1, d2)
    pub def <>(d1: Creole, d2: Creole): Creole = hcat(d1, d2)
    pub def <<>>(d1: Creole, d2: Creole): Creole = hcat(d1, hcat(text(" "), d2))
    pub def <!>(x: Creole, y: Creole): Creole = VCat(x, y)

    pub def emph(doc: Creole): Creole = text("//") <> doc <> text("//")
    pub def strong(doc: Creole): Creole = text("**") <> doc <> text("**")

    pub def color(name: String): Creole = text("<#${name}>")

}
