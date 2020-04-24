# Phys 401
Wolfram language package for the physics 401 final exam

* Package file `401Lib.wl`
* Examples cam be found in `examples.nb`
* Some final exams in `final.nb`

## Examples 

Compute a time averge Poynting vector of an electromagnetic wave

```Mathematica
PoyntingVector[E_0 {1, 0, 0} E^(I (k - \omega t)),B_0 {0, 1, 0} E^(I (k - \omega t))]
{0, 0,B_0 E^(2 I (k - t \omega)) E_0}
TimeAverage[%]//Last
(B_0 E_0)/(2 \mu_0)
```

