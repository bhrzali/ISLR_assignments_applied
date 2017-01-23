
Power = function(){
    print(2^3)
}
Power()

Power2 = function(x,a){
    print(x^a)
}
Power2(3,8)

Power2(10,3)
Power2(8,17)
Power2(131,3)

Power3 = function(x,a){
    answer = x^a
    return(answer)
}
Power3(2,3)

x = 1:10
y = Power3(x,2)
plot(x,y,main="x^2 vs x",xlab="x",ylab="x^2")

x=1:10
plot(x,Power3(x,2),log="xy", main="log(y) vs log(x)")
plot(x,Power3(x,2),log="x",main="y vs log(x)")

PlotPower = function(x,a){
    y = x^a
    plot(x,y)
}
PlotPower(1:10,3)


