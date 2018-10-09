A1 = c(100,200,300,400,500,600,700)
A2 = c(10,20,10,30,20,20,30)
B = c(40,50,50,70,65,65,80)
num = length(B)
VX1 = 0
VX2 = 0
A1bar = 0
A2bar = 0
Bbar = 0
VB = 0
for(I in 1:num){
  VX1 = VX1 + A1[I]
  VX2 =VX2 + A2[I]
  VB = VB + B[I]
}
A1bar = VX1/num
A2bar = VX2/num
Bbar = VB/num
Vx12 = 0
Vx22 = 0
Vx1y = 0
Vx2y = 0
Vx1x2 = 0
for(J in 1:num){
  Vx12 = Vx12 + ((A1[J] - A1bar) ^ 2)
  Vx22 = Vx22 + ((A2[J] - A2bar) ^ 2)
  Vx1y = Vx1y + ((A1[J] - A1bar) * (B[J] - Bbar))
  Vx2y = Vx2y + ((A2[J] - A2bar) * (B[J] - Bbar))
  Vx1x2 = Vx1x2 + ((A1[J] - A1bar) * (A2[J] - A2bar))
}
b1 = ((Vx22 * Vx1y) - (Vx1x2 * Vx2y)) / ((Vx12 * Vx22) - (Vx1x2 ^ 2))
b2 = ((Vx12 * Vx2y) - (Vx1x2 * Vx1y)) / ((Vx12 * Vx22) - (Vx1x2 ^ 2))
b0 = Bbar - (b1 * A1bar) - (b2 * A2bar)
RSS = 0
for(I in 1:num){
  Pvalue = b0 + (b1 * A1[I]) + (b2 * A2[I])
  RSS = RSS + (B[I] - Pvalue) ^ 2
}
MSE = RSS/num-3
VB1 = MSE * (Vx22 / (Vx12 * Vx22 - (Vx1x2 ^ 2)))
VB2 = MSE * (Vx12 / (Vx12 * Vx22 - (Vx1x2 ^ 2)))
VB0 = MSE * (1/num + ((A1bar ^ 2 * Vx22) + (A2bar ^ 2 * Vx12) - (2 * A1bar * A2bar * Vx1x2)) / ((Vx12 * Vx22) - Vx1x2 ^ 2))
SEB0 = sqrt(VB0)
SEB1 = sqrt(VB1)
SEB2 = sqrt(VB2)
cat("\nb0 = " , b0)
cat("\nb1 = " , b1)
cat("\nb2 = " , b2)
cat("\nEquation: B = " , b0, " + ",b1,"(A1) + ", b2 , "(A2)")
cat("\nMSE = " , MSE)
cat("\nV(B0) = " , VB0)
cat("\nV(B1) = " , VB1)
cat("\nV(B2) = " , VB2)
cat("\nSE(B0) = " , SEB0)
cat("\nSE(B1) = " , SEB1)
cat("\nSE(B2) = " , SEB2)