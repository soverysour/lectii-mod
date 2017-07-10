# Despre #

Scopul proiectului acestuia e de a prezenta material educational elevilor, de orice natura, impreuna cu teste specifice 
materialului, teste cu exercitii de tipul "alege varianta corecta", "completeaza spatiul liber", si "drag and drop".
Se pot crea conturi individuale de elevi, local, pentru a inregistra progresul individual. Testele se proceseaza 
imediat dupa ce au fost date, si se noteaza.

Acesta este scris in Scala ( un limbaj care lucreaza pe JVM, ca si Java, Groovy, Kotlin, etc ) si foloseste scala.swing, 
un wrapper pentru libraria swing din Java. Foloseste Gradle ca si build tool.

# Instalare # 
Dependente: 

    \- Java Runtime Environment ( o versiune cat mai noua )

    \- Scala ( o versiune cat mai noua )

    \- Gradle ( necesar o versiune >= 3.0 )

    \- git ( optional, pentru clonarea directa a repositoriului )


Presupunand ca aveti git:

```
git clone https://bitbucket.org/RedAsATortoise/romana.git
cd romana
gradle run
```

Altfel descarcati manual repositoriul, deschideti cu cmd / un emulator de terminal locatia repositoriului si executati 
comenzile de la `cd romana` in jos.

Comanda `run` data lui gradle este una definita in fisierul de configuratie a lui gradle. Va rezolva toate dependentele 
si va compila sursele, dupa care va rula aplicatia.

Pe langa aplicatia propriu zisa, este nevoie si de un directoriu aflat pe locatia HOME, ( `C:\users\[utilizator\` pentru 
windows sau `/home/[utilizator]/` pentru orice sistem Unix-like ), din care se vor citi toate lucrurile legate de 
aplicatie, precum conturile, lectiile, materialele lectilor, etc.

Directoriul lectii-mod este un "template" care contine teste anterioare date, conturi dummy si materiale. 
Un .jar compilat deja se afla si el in repositoriu, lectii-mod.jar.

Pentru a asambla un nou jar, rulati comanda `gradle build`. Pentru a rula clasele compilate, rulati `gradle run` iar 
pentru a rula jar-ul asamblat, rulati `gradle runJar`.

# Crearea de materiale #

Intai, directoriul `lectii-mod` trebuie sa existe in HOME. Apoi:

    \- Trebuie sa existe fisierul channel.txt cu textul `student`, in acest folder.

    \- Fisierul users.txt trebuie sa existe, fie gol fie cu o lista de utilizatori precedenta

    \- Fisierul mod.txt trebuie sa existe, cu text de forma `NUME_DIRECTORIU####NUME_LECTIE`, unde NUME\_DIRECTORIU este 
numele fix al directoriului lectiei cu numele NUME\_LECTIE.

Apoi, in fiecare directoriu tip lectie, trebuie sa existe: 

    \- `dictionary.txt`, pentru stocarea inregistrarilor testelor precedente, date.

    \- `settings.txt`, cu inregistrari de forma: TIP####NUME\_FISIER####TITLU\_FISIER####NUMAR\_ORDINE, unde TIP poate fi 
M sau T, pentru Material ( lectie ) sau Test, respectiv, NUME\_FISIER este numele fizic al Testului / Lectiei, aflate in 
directoarele test sau material, respectiv, unde TITLU\_FISIER este titlul care va aparea cand programul va fi rulat, 
echivalent cu NUME\_LECTIE, iar NUMAR\_ORDINE este un numar de ordine pentru a ordona casutele in paginile lor din 
program.

    \- Directoarele test, progress si material, cu materiale / teste sau cu rezultatele de la teste.

# Despre Materiale #

Atunci cand un material este specificat in settings.txt, prin NUME\_FISIER, acelasi nume de material trebuie folosit si 
pentru un fisier in directorul material. Materialele pot folosi sintaxa HTML, dupa cum se poate vedea din modele.

Atunci cand o poza locala, in acelasi directoriu, material, este mentionata intr-un material, acea sintaxa HTML trebuie 
modificata, pentru a permite identificarea si construirea URL-ului local, astfel ca, pentru o poza locala folosim:

```
<{img="imagine" [orice alte optiuni]>
```

# Despre Teste # 

Un test poate avea trei "grupe" de exercitii:

    \- CE = Complete the Empty space

    \- CV = Choose the correct Variant

    \- DD = Drag and Drop


Pentru inceperea oricarui chunk, se incepe cu unul dintre identificatorii de mai sus.

Indiferent de tipul de exercitiu, fiecare subexercitiu va incepe la fel, si anume:

```
CERINTA\_ORICAT\_DE\_LUNGA####[solutii]
```

Unde solutiile pot fi:

```
CE

SOL1##SOL2##SOL3
```

Solutiile ce pot fi acceptate fiind despartite printr-un simplu separator.

```
CV

SOL1##SOL2@@##SOL3
```

Solutiile posibile fiind despartite prin acelasi separator, doar ca de data asta, fiecare varianta ce ar trebui aleasa, 
adica fiecare varianta corecta, va primi un sufix, `@@`.

```
DD

VARIANTA1####OPTIUNEA1
VARIANTA2####OPTIUNEA2
null####OPTIUNEA\_CAPCANA
OPTIUNEA_CAPCANA2####null
```

Intr-un exercitiu de tip DD, fiecare subexercitiu doar adauga elemente la coloane. Un element `null` nu face altceva 
decat sa adauge o varianta capcana la cealalta coloana.
