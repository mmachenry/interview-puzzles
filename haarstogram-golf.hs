import Data.List (transpose)
i=[[8,3,-2,8],[3,7,-2,0],[3,8,9,3],[1,9,9,3]]
c d l=o(sum(take d l))l$drop d l
o t _[]=[t]
o t(p:v)(n:x)=t:o(t-p+n)v x
r d = h d.transpose.h d
h d=map$c d
f m d t=sum$map(length.filter(==t))$r d m
