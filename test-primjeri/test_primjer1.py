open('dz.py').read()

test = ['############', '#█*█****█**#', '#₸*█**↑₸**₸#', '#*₸***█**█*#', '#*█**█₸█***#', '#█****█**██#', '############']
pozx = 2
pozy = 6

def izvrsi(funkcije, *argumenti, **globalni):
    print('Main je vratio:', funkcije['main'].pozovi(argumenti, Memorija(globalni)))

proba = P ('''
#funkcija faktorijel koja nam koristi da dolje pokažemo da sve štima s izrazima i da nam rade rekurzije
def Number fakt (Number n) {
        if (n == 1) {return 1;}
        elif (n == 0) {return 1;}
        else {
            Number a = n - 1;
            return (a + 1) * fakt (a);
    }
}
def Number main () {
    
    # dolazimo do prve kutije od interesa
    check (1); forward (1); turn (90); forward (1); forward (100); forward (3);
    # pokazujemo kako radi if grananje i pridruživanje te što vraćaju naredbe turn i forward
    if (turn (90)) {
        Number i = forward (2);
    }
    if (i == 2) {
        turn (270);
    }
    forward (forward (1));
    
    # ako je mjesto ispred popunjeno i kvar, popravi ga (dovoljno je samo isshort ())
    if (not check (1) and isshort ()) {
        Bool b = repair ();
    }
    
    
    # demonstriramo da i ulančavanje naredbe forward radi kako treba
    if (b) {
        forward (forward (forward (1)));
    }
    
    turn (270); repair (); forward (1); pick (); turn (180); forward (1); turn (90); forward (1);
    
    # ovdje može ići i if, no pokazujemo da se while izvrši samo jednom
    while (check (2)) {
        put ();
    }
    
    # for petlja rotira robota za 270 stupnjeva
    i = 0;

    for (i; i < 3; i++) {
        turn (90);
    }

    pick (); forward (1); turn (90); put (); repair (); turn (180); forward (1); turn (90);
    
    /* ako uspijemo podići kutiju (pick isto vraća Bool),
        pomičemo se za jedno mjesto unaprijed te ako je taj
        pomak bio uspješan, okrećemo se i idemo dalje */
    if (pick ()) {
        Number pomak = forward (1);
        if (pomak == 1) {
            turn (90);
            forward (3);
        }
    }

    # popunjavamo slovo I do kraja, provjeravamo je li mjesto slobodno
    turn (180);
    if (not isshort () and not iswall () and not isbox ()) {
        Bool stavi = put ();
    }

    
    # rotiramo se dok ne ugledamo slobodno mjeste na koje se onda pomičemo (check i turn vraćaju Boolove)
    while (not check (1) and turn (90)) {}
    turn (270); forward (1);
    if (isbox ()) {
        pick ();
    }
    forward (1); put ();
    turn (90); repair (); repair (); forward (1);
    pick (); forward (1); turn (270); put (); turn (270); forward (1); turn (270); pick ();
    
    # for petljom dobijamo kut rotacije za tri puta po 90
    i = 0;
    Number kut = 0;
    for (i; i < 90; i+= 1 ) {
        kut = kut + 1;
    }
    turn (kut * 3); forward (2); turn (270);

    #pokušavamo popraviti kvar dok nosimo kutiju, što će biti neuspješno
    while (repair ()) {}
    
    turn (90); forward (1); turn (270); put ();
    turn (270); forward (1);
    while (check (1)) {
        turn (90);
    }
    # kako je robot slobodan, uspijeva popraviti kvar
    while (repair ()) {}

    # ova while petlja nas vodi do prvog kvara i popravlja ga
    while (not repair ()) {
        forward (1);
    }

    turn (270); pick (); turn (270); forward (1); put ();

    # pokazujemo da štimaju bilo kakvi aritmetički izrazi
    turn (fakt (5) - 30); forward (3); turn (90); pick (); turn (90);
    
    # micemo se dok ne dodemo jedno polje ispred slobodnog da imamo mjesta za ostaviti
    while (check (2)) {
        forward (1);
    }
    
    # idemo do zadnje kutije i nosimo je na mjesto koje želimo
    put (); turn (270); forward (3); turn (270);
    while (not isbox ()) {
        forward (1);
    }

    if (isbox ()) {
        pick ();
    }

    turn (270); forward (10); turn (270); put ();

    # rezultat: robot je popravio sve kvarove te
    # posložio kutije u slova IP na ekranu
    
    
    return 0;
}
''')

izvrsi(proba, mapa=mapa, smjerovi=smjerovi, ikone=ikone, kutije=kutije,
       kutija=kutija, smjer=smjer, pozx=pozx, pozy=pozy, senzor=senzor, kvar=kvar, zvuk=zvuk)

