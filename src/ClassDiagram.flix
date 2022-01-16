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


namespace PlantUml/ClassDiagram {

    use PlantUml/Puml.Puml;
    use PlantUml/Puml.{text, char, block};
    use PlantUml/Puml.{<>, <<>>};

    def element(elem: String, name: Puml, body: Puml): Puml = 
        text(elem) <<>> name <<>> block(None, 1, body)

    pub def abstract(name: Puml, body: Puml): Puml = element("abstract", name, body)

    pub def abstractClass(name: Puml, body: Puml): Puml = element("abstract class", name, body)

    pub def annotation(name: Puml, body: Puml): Puml = element("annotation", name, body)

    pub def class_(name: Puml, body: Puml): Puml = element("class", name, body)

    pub def entity(name: Puml, body: Puml): Puml = element("entiry", name, body)

    pub def enum_(name: Puml, body: Puml): Puml = element("enum", name, body)

    pub def interface(name: Puml, body: Puml): Puml = element("interface", name, body)


    pub def relation(classname1: Puml, symbol: String, classname2: Puml): Puml = 
        classname1 <<>> text(symbol) <<>> classname2

    pub def extension(classname1: Puml, classname2: Puml): Puml = relation(classname1, "<|--", classname2)

    pub def composition(classname1: Puml, classname2: Puml): Puml = relation(classname1, "*--", classname2)

    pub def aggregation(classname1: Puml, classname2: Puml): Puml = relation(classname1, "o--", classname2)

    pub def label(body: Puml, lbl: Puml): Puml = body <<>> char(':') <<>> lbl


    pub def private(): Puml = char('-')
    pub def protected(): Puml = char('#')
    pub def packagePrivate(): Puml = char('~')
    pub def public(): Puml = char('+')

    // Field modifier
    pub def field(scope: Option[Puml], body: Puml): Puml = match scope { 
        case Some(d) => d <<>> text("{field}") <<>> body
        case None    => text("{field}") <<>> body
    }

    // Method modifier
    pub def method(scope: Option[Puml], body: Puml): Puml = match scope { 
        case Some(d) => d <<>> text("{method}") <<>> body
        case None    => text("{method}") <<>> body
    }

    // static modifier
    pub def static_(scope: Option[Puml], body: Puml): Puml = match scope { 
        case Some(d) => d <<>> text("{static}") <<>> body
        case None    => text("{static}") <<>> body
    }

    // abstract modifier
    pub def abstractModifier(scope: Option[Puml], body: Puml): Puml = match scope { 
        case Some(d) => d <<>> text("{abstract}") <<>> body
        case None    => text("{abstract}") <<>> body
    }

    pub def dashSep(): Puml = text("--")
    
    pub def dotSep(): Puml = text("..")

    pub def equalsSep(): Puml = text("==")

    pub def underscoreSep(): Puml = text("__")

    def sepTitle(sep: Puml, title: Puml): Puml = sep <<>> title <<>> sep

    pub def dashTitle(title: Puml): Puml = sepTitle(dashSep(), title)

    pub def dotTitle(title: Puml): Puml = sepTitle(dotSep(), title)

    pub def equalsTitle(title: Puml): Puml = sepTitle(equalsSep(), title)
    
    pub def underscoreTitle(title: Puml): Puml = sepTitle(underscoreSep(), title)

}
