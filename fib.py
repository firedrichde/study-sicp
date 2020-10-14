#!/usr/env python
class Fib(object):
    """
    docstring
    """
    def __init__(self):
        """
        docstring
        """
        self.__values = {}
        pass

    def get(self,n):
        if n not in self.__values:
            self.__values[n] = self.__calc(1,1,n)
        return self.__calc(1,1,n)

    def __calc(self,a,b,n):
        if n==0:
            return b
        else:
            return self.__calc((a+b),a,(n-1))


if __name__ == "__main__":
    fib = Fib()
    n = 20
    for i in range(n):
        print(fib.get(i))