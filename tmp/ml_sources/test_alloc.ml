open Lang

let alloc e = ref (ref e)

let alloclbl e = ref_lbl "id" e
