def simple(f : Double => Double , a : Int , b : Int ) : Double = {
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}
val ej1 = simple(x => (-math.pow(x,2) + 8*x  - 12), 3, 5)
val ej2 = simple(x => 3*(math.pow(x,2)), 0, 2)
val ej3 = simple(x => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4)),-1,1)
val ej4 = simple(x => (2*x+1)/((math.pow(x,2))+x) ,1,2) 
val ej5 = simple(x => math.pow((math.E),x) ,0, 1)
val ej6 = simple(x => 1 / math.sqrt(x-1),2,3)
val ej7 = simple(x => 1 / 1 + math.pow(x,2),0,1)

/* ------------------------------------------------------------------------------------------------------------------------------------------------- */
def extendida(f :Double => Double , a : Double , b : Double) : Double = { /* def integracion recibe un Double y saca un double parametro a es entero y b es entero tambien  */
    val n = (2*(b-a)).toInt
    val h = (b-a)/n 
    val i = (1 to n-1 by 2).toList 
    val j = (2 to n-2 by 2).toList
    val sumi =i.map(x=>f(a + x*h)).sum
    val sumj = j.map(x=>f(a + x*h)).sum
   (h/3)*(f(a)+(4*(i.map(x=>f(a + x*h)).sum)+(2*(j.map(x=>f(a + x*h)).sum)+f(b))))
}
val calc2_1= extendida(x => (-math.pow(x,2) + 8*x  - 12), 3, 5)
val calc2_2 = extendida(x => 3*(math.pow(x,2)), 0, 2)
val calc2_3 = extendida(x => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4)),-1,1)
val calc2_4 = extendida(x => (2*x+1)/((math.pow(x,2))+x) ,1,2) 
val calc2_5 = extendida(x => math.pow((math.E),x) ,0, 1)
val calc2_6 = extendida(x => 1 / math.sqrt(x-1),2,3)
val calc2_7 = extendida(x => 1 / 1 + math.pow(x,2),0,1)

/* ------------------------------------------------------------------------------------------------------------------------------------------------- */

def compuesta(f :Double => Double , a : Double , b : Double, n:Int) : Double = {
    val h=((b-a)/n)
    val j=(1 to n/2).toList
    val calc= (a:Double, h:Double,j:Double)=>a+j*h
    (h/3)*j.map(x=>f(calc(a,h,2*x-2)) + 4*f(calc(a,h,2*x-1)) + f(calc(a,h,2*x))).sum
}
val calc3_1= compuesta(x => (-math.pow(x,2) + 8*x  - 12), 3, 5,6)
val calc3_2 = compuesta(x => 3*(math.pow(x,2)), 0, 2,6)
val calc3_3 = compuesta(x => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4)),-1,1,6)
val calc3_4 = compuesta(x => (2*x+1)/((math.pow(x,2))+x) ,1,2,6) 
val calc3_5 = compuesta(x => math.pow((math.E),x) ,0, 1,4)
val calc3_6 = compuesta(x => 1 / math.sqrt(x-1),2,3,6)
val calc3_7 = compuesta(x => 1 / 1 + math.pow(x,2),0,1,6)