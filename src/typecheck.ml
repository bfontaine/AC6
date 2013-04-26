let flag = ref false

let program p = 
  if !flag then 
    failwith "Students, this is your job."
