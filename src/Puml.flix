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

    pub enum Puml {
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
    
    pub def <!>(x: Puml, y: Puml): Puml = VCat(x, y)

    pub def text(s: String): Puml = Text(s)

    pub def creole(doc: Creole): Puml = Creole(doc)


    pub def startuml(): Puml = Text("@startuml")
    pub def enduml(): Puml = Text("@enduml")

}
