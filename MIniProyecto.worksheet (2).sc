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
def calcularError(f : Double => Double , a : Double , b : Double ) : Double = {
    math.abs(a - b )
}
val calc1 = calcularError(x => (-math.pow(x,2) + 8*x  - 12), 7.33, 7.33)
val calc2 = calcularError(x => 3*(math.pow(x,2)), 8, 8)
val calc3 = calcularError(x => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4)),4.6,3.33)
val calc4 = calcularError(x => (2*x+1)/((math.pow(x,2))+x) ,1.38,1.09)
val calc5 = calcularError(x => math.pow((math.E),x) ,1.28, 1.71)
val calc6 = calcularError(x => 1 / math.sqrt(x-1),0.95,0.82)
val calc7 = calcularError(x => 1 / 1 + math.pow(x,2),1.16,0.78)

/* ------------------------------------------------------------------------------------------------------------------------------------------------- */
def extendida(n :Double, f:Double => Double , a : Int , b : Int ) : Double = { /* def integracion recibe un Double y saca un double parametro a es entero y b es entero tambien  */
    val h =((b-a)/n)
    val n = 2*(b-a)
    
    (h/3)*((f(a)+4*verificacion(f(a+verificacion))+2 f(a+(verificacion)*h)+f(b)))
    
}

def verificacion(n:Double):Int=(n % 2)match{ 
    case 0=> 1
    case 1 =>0 
}


def potencia(exponente:Int, base:Int):Int=(1 to exponente).map(elem=>base).reduce(_*_)
potencia(3,4)
