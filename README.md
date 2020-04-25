# Phys 401
Wolfram language package for the physics 401 final exam

* Package file `401Lib.nb`
* Examples can be found in `examples.nb`
* Some final exams in `final.nb`

## Usage
Create a new notebook in this directory, at the top run
```Mathematica
nb = NotebookOpen[NotebookDirectory[] <> "401Lib.nb", 
   CellContext -> $Context, Visible -> False];
NotebookEvaluate[nb, InsertResults -> False];
NotebookClose[nb]
```
*This is a bit hacky, but using a .wl package does not handel symbolized subscripts, so a notebook is the only option*

## Examples 

Compute a time average Poynting vector of an electromagnetic wave

```Mathematica
PoyntingVector[E0 {1, 0, 0} E^(I (k - o t)),B0 {0, 1, 0} E^(I (k - o t))]
{0, 0,(B0 E0 Cos[k - t o]^2)/mu0}
TimeAverage[%]//Last
(B0 E0)/(2 mu0)
```



