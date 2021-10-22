

def main(_args: Array[String]): Int32 & Impure = 
    use PlantUml/Puml.{strong, text};
    let d1 = strong(text("strongly emphasized"));
    PlantUml/Puml.renderCreole(d1) |> println;
    0