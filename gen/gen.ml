
let () =
  Swagger.codegen
    ~input:Sys.argv.(1)
    ~output:stdout
    ~path_base:"/"
    ~definition_base:"io.k8s."
    ~reference_base:"#/definitions/io.k8s."
    ~reference_root:"Definitions"

