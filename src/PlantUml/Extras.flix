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

/// Depends on `flix-sandbox`
    
namespace PlantUml/Extras {

    pub type alias PlantUmlArgs = 
        { java :: String        /// "java" / "java.exe"
        , plantUmlJar :: System.FilePath
        , format :: Option[String]
        , sourceFile :: System.FilePath 
        }


    pub def execPlantUml(args: PlantUmlArgs): Result[Int32, String] & Impure = 
        let tfmt    = match args.format { 
            case Some(fmt) => "-t${fmt}"
            case None => ""
        };
        let args1 = 
            { cmd = args.java
            , args = ["-jar", "${args.plantUmlJar}", tfmt, "${args.sourceFile}"]
            , charset = Text/Charset.utf_8()
            };
        process(args1) |> Result.map(x1 -> x1.exitCode)

    /// TODO - to add to System/Process...
    pub type alias ProcessArgs = 
        { cmd :: String
        , args :: Array[String]
        , charset :: Text.Charset
        }


    pub def process(args: ProcessArgs): Result[{exitCode :: Int32, processResponse :: String}, String] & Impure =
        use Result.flatMap;
        let pb      = System/Process.newProcessBuilder(args.cmd, args.args);
        let* proc   = System/Process.start(pb);
        let* resp   = System/Process.getInput(proc, args.charset);
        let* code   = System/Process.waitFor(proc);
        Ok({exitCode = code, processResponse = resp}) 

}
