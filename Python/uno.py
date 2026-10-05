def imprimir_hola_mundo():
    print("Hola mundo")

import math

def raiz(num):
    print(math.sqrt(num))

def perimetro(num)->float:
    res: float = math.pi * 2 * num
    return res


def esMultiplo(n:int,m:int) -> bool:
    if n % m == 0:
        return True
    else:
        return False
    #res:bool = n % n == 0
    #return res

def es_nombre_largo(n:str)-> bool:
    res : bool = 3 <= len(n) <= 8
    return res

def devolver_el_doble_si_es_par(n:int)->int:
    if n % 2 == 0:
        return n*2
    return n

def entre10y40()->None:
    ind:int = 10
    while ind < 41:
        if ind % 2 == 0:
            print(ind)
        ind+=1

def entre1040()->None:
    for num in range(10,41,2):
        print(num)

def cohete(n:int)->None:
    while n > 1:
        print(n)
        n=n-1
    print(1,"\nDespegue")        

    