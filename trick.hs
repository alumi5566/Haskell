--turn float to string
--when facing bmiTell problem, wanna dump the bmi into string, but find no api to do this
f2s_f :: Float -> Int -> String
f2s_f num count
  | count < 1 = show(floor(num))
  | otherwise = show(floor(num)) ++ f2s_f ((num-realToFrac(floor(num)))*10) (count-1)

f2s :: Float -> Int -> String 
f2s num count
  | count < 1 = show(floor(num))
  | otherwise = show(floor(num)) ++ "." ++ f2s_f ((num-realToFrac(floor(num)))*10) (count-1)


--f2s num 0 = show(floor(num))
--f2s num count = show(floor(num)) ++ "." ++ f2s ((num-realToFrac(floor(num)))*10) (count-1)
--floor float->int; show int->string; real2Frac int->float
--still hv some bug
--f2s 3.1415961812345 10
--"3.1415960788" 
--maybe the valid bit of real2Frac?
