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


    
namespace PlantUml/Extra {

    pub type alias PlantUmlArgs = 
        { java :: String        /// "java" / "java.exe"
        , plantUmlJar :: System.FilePath
        , format :: Option[String]
        , sourceFile :: System.FilePath 
        }


    pub def execPlantUml(args: PlantUmlArgs): Result[Int32, String] & Impure = 
        use Result.flatMap;
        let tfmt    = "-t${args.format}";
        let pb      = System/Process.newProcessBuilder(args.java, ["-jar", "${args.plantUmlJar}", tfmt, "${args.sourceFile}"]);
        let* proc   = System/Process.start(pb);
        let* str    = System/Process.getInput(proc, Text/Charset.utf_8());
        let _       = Console.printLine(str);
        let* exitCode   = System/Process.waitFor(proc);
        Ok(exitCode)
    
}
