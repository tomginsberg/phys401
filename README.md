# Phys 401
Wolfram language package for the physics 401 final exam

* Package file `401Lib.wl`
* Examples cam be found in `examples.nb`
* Some final exams in `final.nb`

## Examples 

Compute a time average Poynting vector of an electromagnetic wave

```Mathematica
PoyntingVector[E0 {1, 0, 0} E^(I (k - o t)),B0 {0, 1, 0} E^(I (k - o t))]
{0, 0,(B0 E0 Cos[k - t o]^2)/mu0}
TimeAverage[%]//Last
(B0 E0)/(2 mu0)
```



