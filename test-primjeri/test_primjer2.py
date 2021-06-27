test = ['############', '#******₸***#', '#♫♫********#', '#******█***#', '#****₸*****#', '#***█↑*****#', '#*█*******█#', '#***♫***₸**#', '#**█****₸**#', '#*█***₸***█#', '############']

proba = P ('''
    def List main () {
        List ret = [];
        Number x = 0; Number y = 0; List temp = []; Number n = 0;

        if (noise () != 0) {
            x = getx (); y = gety (); n = noise (); temp = [x, y, n];
            append (ret, temp);
        }
        
        if (isshort()) {
            repair ();
        }
        # dodaj ovaj repair u listu
        forward (1); Number n1 = n;
        while (noise () < n1 and not iswall ()) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        
        turn (270);
        while (noise () < n1 and not isshort ()) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        repair (); #append
        
        # provjerimo da nema izvora tu gdje želimo staviti sve kutije
        forward (3); turn (270);
        while (noise () == 0 and not iswall () and not isshort () and not isbox ()) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        # sada znamo da tim putem možemo prolaziti
        # prvo spojimo dvije kutije desno
        turn (180); forward (3); turn (270); pick (); forward (1); turn (270); forward (1); put ();
        

        # otprije znamo da je put do ova dva repaira siguran
        turn (270);
        while (not isshort ()) {
            forward (1);
        }
        repair (); turn (90); forward (1); turn (270); repair (); turn (90); forward (1); turn (270);
        
        # ne znamo je li put siguran pa provjeravamo
        while (noise () < 70 and not isshort ()) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        repair ();

        # idemo vidjeti možemo li kutiju prenijeti ovim putem
        while (noise () < 50 and check (1)) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        # shvaćamo da ovim putem nećemo moći prenijeti kutiju pa se vraćamo
        if (noise () >= 50) {
            turn (180);
            forward (100);
            turn (90);
            forward (100);
            turn (90);
        }

        # opet ispipavamo za put s kutijom
        while (noise () < 50 and check (1)) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        if (noise () >= 50) {
            turn (180); forward (1);
        }
        
        # sada smo upravo iznad kutije od koje smo došli
        # pa znamo da se sigurno možemo spustiti po nju pa
        # i nju nosimo na mjesto
        turn (270);
        forward (forward (forward (1))); pick (); turn (90); forward (forward (forward (forward (2))));
        turn (270);
        while (check (2)) {
            forward (1);
        }
        put ();
        # vraćamo se do mjesta na kojem smo spazili kritičnu razinu zvuka
        turn (180); forward (5); turn (90); forward (6); turn (90);
    
        # idemo dok ne dođemo do kritične razine
        forward (2);
        while (noise () < 50 and check (1)) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        turn (270);
        Number noise_curr = noise ();
        forward (1);
        if (noise () < noise_curr) {
            forward (1);
        }

        # vraćamo se sigurnim putem
        turn (90); pick (); turn (90);
        while (noise () < 44 and check (2)) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            forward (1);
        }
        # provjerimo desno iz perspektive robota (lijevo iz perspektive upravljača)
        turn (90); forward (1);
        
        if (noise () <= 44) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            turn (270); forward (1);
            forward (100);
        }
        turn (270); forward (1); put();

        # sada znamo odakle imamo put pa samo dođemo do te točke
        turn (270); forward (8); turn (90);
        while (noise () < 50 and check (1)) {
            forward (1);
        }
        # ispipamo je li ovo siguran put
        if (noise () >= 50) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            turn (180);forward (1);
        }
        # put je ispao nesiguran pa se mičemo još jedno mjesto ulijevo (iz perspektive upravljača)
        turn (90); forward (1);
        if (noise () < 50) {
            if (noise () != 0) {
                x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                append (ret, temp);
            }
            turn (90);
            while (noise () < 70 and check (1)) {
                if (noise () != 0) {
                    x = getx (); y = gety (); n = noise (); temp = [x, y, n];
                    append (ret, temp);
                }
                forward (1);
            } 
        }
        turn (90); pick ();
        # sada kada smo skupili kutiju, znamo da je sigurno i vraćamo se istim putem
        turn (90); forward (3); turn (270); forward (1); turn (90); forward (1); turn (270); forward (1); turn (90); forward (1); turn (270);
        while (forward (1) == 1) {}
        
        # spuštamo i ovu kutiju na hrpu
        turn (270); put ();
        
        

        # ista logika i za predzadnju kutiju, a sada već imamo sve potrebne informacije
        # pa je samo idemo skupiti i donijeti natrag točno istim putem

        turn (270); forward (7); turn (90); forward (1);
        if (noise () != 0) {
            x = getx (); y = gety (); n = noise (); temp = [x, y, n];
            append (ret, temp);
        }
        turn (270); forward (1); turn (90); forward (3); turn (90); pick (); turn (180); forward (1); turn (270); forward (3); turn (270); forward (2); turn (90); forward (1); turn (270);
        while (forward (1) == 1) {}
        turn (90); forward (1); turn (180); put (); turn (270);
        while (forward (1) == 1) {}
        pick (); turn (180);
        while (check (2)) {
            forward (1);
        }
        put ();
        

        return ret;
    }
''')

izvrsi(proba, mapa=mapa, smjerovi=smjerovi, ikone=ikone, kutije=kutije,
       kutija=kutija, smjer=smjer, pozx=pozx, pozy=pozy, senzor=senzor, kvar=kvar, zvuk=zvuk)



