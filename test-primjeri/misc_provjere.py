'''
### testovi za FORWARD, TURN i CHECK (bez prepreka) [napomenuti da za testove treba ostaviti neke printove unutar funkcija koje inace ne printaju, npr. check]
    prema gore:
        def Number main () {turn (180); forward (1); forward (2); turn (180); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); forward (1); forward (2); turn (180); forward (2); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}

    prema lijevo:
        def Number main () {turn (180); turn (90); forward (1); forward (2); turn (180); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); turn (90); forward (1); forward (2); turn (180); forward (1); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); turn (90); forward (1); forward (2); turn (180); forward (2); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); turn (90); forward (1); forward (2); turn (180); forward (3); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); turn (90); forward (1); forward (2); turn (180); forward (4); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}

    prema dolje:
        def Number main () {turn (180); turn (180); forward (1); forward (2); turn (180); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (180); turn (180); forward (1); forward (2); turn (180); forward (2); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}

    prema desno:
        def Number main () {turn (90); forward (1); forward (2); turn (180); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (90); forward (1); forward (2); turn (180); forward (1); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (90); forward (1); forward (2); turn (180); forward (2); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (90); forward (1); forward (2); turn (180); forward (3); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}
        def Number main () {turn (90); forward (1); forward (2); turn (180); forward (4); check (1); check (2); check (3); check (4); check (5); check (100); return 0;}

### testovi za FORWARD, TURN i CHECK (prepreka na 2, 2)
    def Number main () {turn (90); forward (2); check (3); return 0;}
    def Number main () {forward (1); turn (90); forward (1); check (1); return 0;}
    def Number main () {forward (1); turn (90); forward (1); turn (90); forward (2); check (1); return 0;}
    def Number main () {forward (1); turn (90); forward (1);  forward (2); turn (90); forward (1); turn (90); check (4); forward (0);forward (1);forward (2);forward (3); return 0;}
    def Number main () {forward (1); turn (90); forward (1);  forward (2); turn (90); forward (1); turn (90); forward (1); turn (270); forward (0); forward (1); turn (90); forward (1); turn (90); check (1); check (2); check (3); check (4); check (100); return 0;}

### testovi za PICK, PUT, REPAIR:
ide uz ovu mapu: ['#######', '#**█**#', '#**↑**#', '#*****#', '#######']
    def Number main () {pick (); turn (270); pick (); put (); pick (); forward (1);turn (270); pick ();  put (); turn (90); put (); turn (270);pick ();turn (90); put (); turn (90); pick (); put (); turn (270); pick (); put (); turn (180); forward (1); forward (100);return 0;}
ide uz ovu mapu:
    def Number main () {check (1); forward (1); turn (90); forward (1); forward (100); pick (); turn (180); forward (3); put (); pick (); turn (270); repair (); turn (270); put (); turn (90); repair ();return 0;}

### testovi za liste:
    def List main () {List a = [[1, 2, 50]]; List v = [2, 3, 70]; add (a, v); return a;}
    def List main () {List a = [1, True, [2, Unknown]]; a = [111]; return a;}




PROVJERA SVIH CASTOVA:
def Bool log (Number n) {
        return False and Unknown;
    }
    def Number aritm (Number n) {
        return 1 - n;
    }
    def List main () {
        # pokazujemo da funkcionira castanje funkcija i složenih logičkih izraza u brojeve
        Bool bul = Unknown and False or True;
        Number i = (Number) bul;
        Number j = (Number) log (5);
        Bool bul2 = 1 < 3;
        Number k = (Number) bul2;

        # pokazujemo da funkcionira castanje funkcija i složenih aritmetičkih izraza u logičke
        Number num = 1 + 2 - 3;
        Bool a = (Bool) num;
        Bool b = (Bool) aritm (0);
        Bool c = (Bool) -1;
        
        # pokazujemo da funkcionira castanje listi u aritmetičke i logičke izraze
        List l1 = [];
        List l2 = [[]];
        Bool bul3 = Unknown and True or False;
        List l3 = [1, True, [[], []], bul3];
        i = (Number) l1;
        j = (Number) l2;
        k = (Number) l3;
        a = (Bool) l1;
        b = (Bool) l2;
        c = (Bool) l3;

        # pokazujemo da funkcionira castanje aritmetičkih i logičkih izraza u liste
        l1 = (List) num;
        Bool bul4 = True or Unknown;
        l2 = (List) bul4;
        l3 = (List) aritm (10);
        List l4 = (List) log (1);
        return l4;
    }

DODATNE PROVJERE CASTOVA I VRAĆANJA:
    def List main(){List x = (List) 5; return x;}

    def List g () {List n = [1,2,False,[[],1,2],True];return n;}
    def List main(){List y = g(); return y;}

    def List g () {List n = [1,2,False,[[],1,2],True];return n;}
    def Bool main(){List y = g(); List v = [y]; Bool x = (Bool) v; return x;}

    def Bool main () {Number n = 0; Bool m = (Bool) n; return m;}

PROVJERA PREKIDA:
    Number ii = 0;
    Number jj = 0;
    for (ii; ii < 10; ii++) {
        if (ii == 5) {
            if (ii == 5) {
                jj = 0;
                for (jj; jj < 2; jj++) {
                    break;
                }
            }
        }
    }
    ii = 0; jj = 0;
    while (ii < 3) {
        ii = ii + 1;
        jj = 0;
        while (jj < 5) {
            jj = jj + 1;
            if (ii == 2) {
                break;
            }
        }
    }

PROVJERA ELIFA:
def Number main () {
    Number n = 5;
    Number i = 0;
    if (n == 1) {
        i = 10;
    }
    elif (n == 2) {
        i = 20;
    }
    elif (n == 3) {
        i = 30;
    }
    elif (n == 4) {
        i = 40;
    }
    else {
        i = 50;
    }
    
    return i;
}
'''