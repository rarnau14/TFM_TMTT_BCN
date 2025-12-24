# TFM_TMTT_BCN
TFM amb el títol: Modelització i anàlisi de les víctimes del franquisme pels judicis del TMT Tercer de Barcelona. Basat en una aplicació Shiny per a l’exploració interactiva dels resultats.

## Anàlisi i predicció de la repressió franquista

Aquest repositori conté el codi, les dades processades i l’aplicació interactiva desenvolupats en el marc del Treball Final de Màster en Ciència de Dades.

L’objectiu del projecte és analitzar les característiques sociopolítiques i judicials de les víctimes de la repressió franquista, així com desenvolupar models predictius per estimar la probabilitat d’afusellament en funció de diferents variables individuals i territorials.



## Anàlisi i modelització (Python)

L’anàlisi exploratòria, la neteja de dades, l’enginyeria de característiques i l’entrenament dels models s’han realitzat en Python mitjançant Jupyter notebooks.

Els notebooks estan numerats per indicar l’ordre recomanat d’execució i garantir la traçabilitat del procés analític.

Els models s’entrenen utilitzant diverses tècniques de classificació (Regressió Logística, SVM, LightGBM i Xarxes Neuronals) i es guarden en format `pickle` per a la seva posterior integració a l’aplicació Shiny.


## Aplicació Shiny (R)

L’aplicació Shiny permet:

- Explorar interactiva­ment les dades de les víctimes segons criteris demogràfics, judicials i geogràfics.
- Visualitzar distribucions, tendències temporals i patrons territorials.
- Estimar la probabilitat d’afusellament mitjançant models predictius entrenats en Python.

L’aplicació es troba dins la carpeta `shiny-app/` i està pensada per ser desplegada de manera independent, en aquest cas a Posit Connect Cloud.

---

### Accés a l’aplicació desplegada

https://rarnauag-tfm-tmtt-bcn.share.connect.posit.cloud/


