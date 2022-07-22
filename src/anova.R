model.sin = 
  aov(df$sincerity ~
        df$intence * df$apology.cost)
summary(model.sin)
lsr::etaSquared(model.sin)

model.for =
  aov(df$forgive ~
        df$intence * df$apology.cost)
summary(model.for)
lsr::etaSquared(model.for)
