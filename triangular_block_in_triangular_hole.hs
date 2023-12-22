-- return true if a triangular piece will fit through a triangular hole
data Triangle = Triangle Double Double Double

fits :: Triangle -> Triangle -> Bool
fits hole piece = 
