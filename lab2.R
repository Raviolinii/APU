library(ahp)

phones_ahp <- Load("./smart.ahp")
#print(phones_ahp)

result <- Calculate(phones_ahp)
#print(result)

analyze <- Analyze(phones_ahp)
#print(analyze)
