def integracion(f : Double => Double , a : Int , b : Int ) : Double = { /* def integracion recibe un Double y saca un double parametro a es entero y b es entero tambien  */
    val intermedio =((a+b)/2.0)
    val fa=f(a)
    val fi =f(intermedio)
    val fb=f(b)
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}