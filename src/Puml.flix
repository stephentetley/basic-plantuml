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


    pub enum PumlElement {
        case PumlText(String)
        case PumlComment(String)
        case PumlCreole(Creole)
    }

    pub enum Creole {
        case CreoleText(String)
        case CreoleHCat(Creole, Creole)
        case CreoleVCat(Creole, Creole)
    }


    pub def renderCreole(doc: Creole): String = 
        renderCreoleHelper(doc) as & Pure

    def renderCreoleHelper(doc: Creole): String & Impure = 
        let sb = StringBuilder.new();
        appendCreole!(sb, doc);
        StringBuilder.toString(sb)

    def appendCreole!(sb: StringBuilder, doc: Creole): Unit & Impure = match doc {
        case CreoleText(s) => StringBuilder.appendString!(sb, s) 
        case CreoleHCat(d1, d2) => { 
            appendCreole!(sb, d1);
            appendCreole!(sb, d2)
        }
        case CreoleVCat(d1, d2) => { 
            appendCreole!(sb, d1);
            StringBuilder.appendLineSeparator!(sb);
            appendCreole!(sb, d2)
        }
    }

    /// TODO - Creole and Puml in seperate namespaces?

    pub def text(s: String): Creole = CreoleText(s)

    pub def hcat(d1: Creole, d2: Creole): Creole = CreoleHCat(d1, d2)
    pub def <>(d1: Creole, d2: Creole): Creole = hcat(d1, d2)
    pub def <<>>(d1: Creole, d2: Creole): Creole = hcat(d1, hcat(text(" "), d2))

    pub def emph(doc: Creole): Creole = CreoleText("//") <> doc <> CreoleText("//")
    pub def strong(doc: Creole): Creole = CreoleText("**") <> doc <> CreoleText("**")

}
