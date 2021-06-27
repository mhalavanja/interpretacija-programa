test = ['############', '#←*********#', '#**********#', '#******█***#', '#**********#', '#***█******#', '#*█*******█#', '#**********#', '#**█*******#', '#*█*******█#', '############']

proba = P ('''
def Bool spirala (Number i, Number smjer, Number stop) {
        
    Number z = 0; Number j = 0; Number pomak = 0; Number pomak1 = 1;
    if (smjer == 1) {
        if (i <= 0) {
            return True;
        }   
        z = 0;
        for (z; z < 2; z++) {
            turn (90);
            j = i;
            for (j; j > 0; j += -1) {
                pomak1 = forward (1);
                if (pomak1 != 1) {
                    turn (180);
                    Number z3 = z; Number j3 = i - j;

                    for (z3; z3 >= 0; z3 += -1) {
                        
                        for (j3; j3 > 0; j3 += -1) {
                            pomak = forward (1);
                        }
                        j3 = i;
                        turn (270);
                    }

                    Bool trash = spirala (i + 1, -1, stop);
                    return False;
                }
            }
        }
        return spirala (i - 1, 1, stop);
    }
    else {
        if (i <= 0) {
            return False;
        }
        if (i == stop + 1) {
            return False;
        }
        z = 0;
        
        for (z; z < 2; z++) {                
            j = i;
            for (j; j > 0; j += -1) {
                pomak = forward (1);
            }
            turn (270);
        }
        return spirala (i + 1, -1, stop);
    }
}    

def Bool main () {
    turn (180);
    # prvo pronađemo dostupnu širinu
    Number sirina = 1;
    while (forward(1) == 1) {
        sirina = sirina + 1;
    }
    turn (180); forward (sirina - 1);
    
    # sada pronađemo dostupnu širinu
    turn (90);
    Number duljina = 1;
    while (forward(1) == 1) {
        duljina = duljina + 1;
    }
    turn (180); forward (duljina - 1);
    turn (90);
    
    Number i = 0; Number j = 0;

    # zelimo slobodni blok velicine 4
    Number velicina = 4;
    for (i; i < duljina - velicina; i++ ) {
            j = 0;
            turn (180);
            Number prostor = forward (1000);
            Number razlika = duljina - velicina;
            if (razlika > prostor) {
                j = j + razlika - prostor;
            }
            turn (180);
            forward (prostor);

            for (j; j < sirina - velicina; j++) {
                Bool pronasaoMjesto = spirala (velicina, 1, velicina);
                forward (1);
                turn (180);
                if (pronasaoMjesto) {
                    return True;
                }
            }

        forward (duljina - velicina + 1); turn (90); forward (1); turn (270);
    }      
    
    
    return False;
}
''')


izvrsi(proba, mapa=mapa, smjerovi=smjerovi, ikone=ikone, kutije=kutije,
       kutija=kutija, smjer=smjer, pozx=pozx, pozy=pozy, senzor=senzor, kvar=kvar, zvuk=zvuk)