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

    
namespace PlantUml/Extras {

    use Basis/System.FilePath;

    pub type alias PlantUmlArgs = 
        { java :: String        /// "java" / "java.exe"
        , plantUmlJar :: FilePath
        , format :: Option[String]
        , sourceFile :: FilePath 
        }


    pub def execPlantUml(args: PlantUmlArgs): Result[Int32, String] & Impure = 
        let tfmt    = match args.format { 
            case Some(fmt) => "-t${fmt}"
            case None => ""
        };
        let args1 = 
            { cmd = args.java
            , args = "-jar" :: "${args.plantUmlJar}" :: tfmt :: "${args.sourceFile}" :: Nil
            , charset = Basis/Text/Charset.utf_8()
            };
        Basis/System/Process.processWithExitCode(args1) |> Result.map(x1 -> x1.exitCode)

}
