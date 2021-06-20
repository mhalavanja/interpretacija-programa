from vepar import *


class T(TipoviTokena):
    OPEN, CLOSE, COMMA, SEMICOL, OLIST, CLIST, LT, GT, EQ, PLUS, MINUS, TIMES, DIV, EXP, OPENBR, CLOSEBR = "(),;[]<>=+-*/^{}"
    NE, LE, GE, PP, EE, PE = "!=", "<=", ">=", "++", "==", "+="
    IF, ELIF, ELSE, FOR, WHILE, RETURN, DEF = 'if', "elif", 'else', "for", 'while', 'return', "def"
    NOT, AND, OR = 'not', 'and', 'or'
    NUMBERTYPE, BOOLTYPE, LISTTYPE = "Number", "Bool", "List"
    PICK, PUT = "pick", "put"  # pick up, put down

    class NAME(Token):
        # TODO: Ako ne postoji varijabla u lokalnoj memoriji, mozda je u globalnoj
        def value(self, mem, unutar): return mem[self]

        def typeCheck(self, mem): return mem[self].tip

    class NUM(Token):
        def value(self, mem, unutar): return int(self.sadržaj)

        def typeCheck(self, mem): return T.NUMBERTYPE

    class BREAK(Token):
        literal = 'break'

        def run(self, mem, unutar): raise Break

    class TRUE(Token):
        literal = 'True'

        def value(self, mem, unutar): return 1

        def typeCheck(self, mem): return T.BOOLTYPE

    class FALSE(Token):
        literal = 'False'

        def value(self, mem, unutar): return -1

        def typeCheck(self, mem): return T.BOOLTYPE

    class UNKNOWN(Token):
        literal = 'Unknown'

        def value(self, mem, unutar): return 0

        def typeCheck(self, mem): return T.BOOLTYPE

    # Number forward(Number cm) ide u naprijed za cm centimetara
    # Povratna vrijednost je broj centimetara za koje je otisao naprijed
    # (da znamo koliko je otisao naprijed ako ne moze ici za zadani broj)
    class FORWARD(Token):
        literal = "forward"

        def value(self, mem, unutar): return mem[self]

    # Number turn(Number radians) okrece se u smjeru suprotnom od kazaljki na satu za radians radijana
    # Povratna vrijednost je broj radijana za koliko se okrenuo
    # (da znamo koliko se okrenuo ako se ne moze okrenuti za zadani broj)
    class TURN(Token):
        literal = "turn"

        def value(self, mem, unutar): return mem[self]

    # Bool check(Number cm) provjeri vidi li zapreku u narednih cm centimetara
    # Povratna vrijednost označava postoji li zapreka
    class CHECK(Token):
        literal = "check"

        def value(self, mem, unutar): return mem[self]


class Break(NelokalnaKontrolaToka):
    """Signal koji šalje naredba break."""

#TODO: dodati <=, => u lexer
def lexer(lex):
    for znak in lex:
        if znak.isspace():
            lex.zanemari()
        elif znak == '+':
            if lex >= '+':
                yield lex.token(T.PP)
            elif lex >= '=':
                yield lex.token(T.PE)
            else:
                yield lex.token(T.PLUS)
        elif znak == '-':
            yield lex.token(T.MM if lex >= "-" else T.MINUS)
        elif znak == '=':
            yield lex.token(T.EE if lex >= '=' else T.EQ)
        elif znak == '!':
            lex >> '='
            yield lex.token(T.NE)
        elif znak == '/':  # višelinijski komentari koji ako ne završe komentiraju do kraja
            if lex >= "*":
                while True:
                    lex.pročitaj_do("*", više_redova=True)
                    if lex >= "/":
                        lex.zanemari()
                        break
            else:
                yield lex.token(T.DIV)
        elif znak == '#':  # jednolinijski komentari
            lex.pročitaj_do('\n')
            lex.zanemari()
        elif znak.isalpha():
            lex.zvijezda(identifikator)
            yield lex.literal(T.NAME)
        elif znak.isdecimal():
            lex.prirodni_broj(znak)  # TODO: Dodati decimalne brojeve ako stignemo
            yield lex.token(T.NUM)
        else:
            yield lex.literal(T)


### (B)KG
# start -> funkcija start | program
# program -> naredbe program?
# funkcija -> DEF type NAME OPEN parametri* CLOSE blok
# parametri -> type NAME | type NAME COMMA parametri

# naredbe -> condNaredba naredbe? | naredba SEMICOL naredbe?
# blok -> OPENBR naredbe CLOSEBR
# naredba -> inicijaliziraj | azuriraj | NAME poziv | RETURN argument
# condNaredba -> FOR OPEN name# SEMICOL name# usporedba (NUM | NAME | NAME poziv) SEMICOL name# (PP | PE NUM) CLOSE blok
#              | WHILE OPEN logStart CLOSE blok
#              | IF OPEN logStart CLOSE blok (ELIF OPEN logStart CLOSE blok)* (ELSE blok)?

# inicijaliziraj -> NUMBERTYPE NAME EQ aritm | BOOLTYPE NAME EQ logStart | LISTTYPE NAME EQ list
# azuriraj -> NAME EQ (aritm | logStart | list)  [KONTEKST!] prvo mora biti inicijalizirana varijabla

# logStart -> logStart OR disjunkt | disjunkt
# disjunkt -> disjunkt AND konjunk | konjunk
# konjunk -> logiz | NOT konjunk
# logiz -> aritm usporedba aritm | TRUE | FALSE | UNKNOWN | NAME poziv? | OPEN logStart CLOSE |
#          OPEN BOOLTYPE CLOSE NAME poziv? -- eksplicitne pretvorbe
# usporedba -> NE | LE | GE | EE | LT | GT

# list -> OLIST (listArgument | list)? CLIST | NAME poziv? | OPEN LISTTYPE CLOSE NAME poziv?--

# aritm -> aritm PLUS član | aritm MINUS član | član
# član -> član TIMES faktor | član DIV faktor | faktor
# faktor -> baza | baza EXP faktor | MINUS faktor
# baza -> NUM | baza | OPEN aritm CLOSE | NAME poziv? | OPEN NUM CLOSE NAME poziv? --

# poziv -> OPEN argumenti? CLOSE
# argumenti -> argument | argument COMMA argumenti
# argument -> aritm |! logStart |! list  [KONTEKST!] kod poziva funkcija
# listArgument -> list | NUM | TRUE | FALSE | UNKNOWN
# type -> NUMBERTYPE, BOOLTYPE, LISTTYPE

operatoriUsporedbe = {T.EE, T.NE, T.LE, T.LT, T.GE, T.GT}


class P(Parser):
    def program(self):
        self.funkcije = Memorija(redefinicija=False)
        # self.symtab = Memorija()  # globalna memorija za varijable
        # self.funkcije["forward"] = Funkcija([T.NUMBERTYPE, "forward", [T.NUMBERTYPE, "centimeters"]])
        # self.funkcije["turn"] = Funkcija([T.NUMBERTYPE, "turn", [T.NUMBERTYPE, "radians"]])
        # self.funkcije["check"] = Funkcija([T.BOOLTYPE, "check", [T.NUMBERTYPE, "centimeters"]])
        naredbe = funkcije = []
        # TODO: napraviti da funkcije ne moraju biti samo na pocetku koda, ako budemo imali naredbe jos
        while self > T.DEF:
            funkcija = self.funkcija()
            funkcije.append(funkcija.ime)
            self.funkcije[funkcija.ime] = funkcija

        # while not self > KRAJ:
        #     naredbe.append(self.naredba())
        #     self >> {T.SEMICOL, KRAJ} #TODO: Mislim da ovo ne treba zbog petlji i if
        # return Program(funkcije, naredbe, self.funkcije, self.symtab)
        return self.funkcije

    def funkcija(self):
        self.symtab = Memorija(redefinicija=False)
        self >> T.DEF
        [self.tipf, self.imef], self.parametrif = self.tipIme(), self.parametri()
        # u symtab spremamo tipove lokalnih varijabli pa ih provjeravamo za vrijeme parsiranja
        naredbe = self.blok()
        return Funkcija(self.tipf, self.imef, self.parametrif, naredbe)

    def tipIme(self):
        return self >> {T.NUMBERTYPE, T.BOOLTYPE, T.LISTTYPE}, self >> T.NAME

    def parametri(self):
        self >> T.OPEN
        if self >= T.CLOSE: return []
        tip, ime = self.tipIme()
        param = [[tip, ime]]
        self.symtab[ime] = tip
        while self >= T.COMMA:
            tip, ime = self.tipIme()
            self.symtab[ime] = tip
            param.append([tip, ime])
        self >> T.CLOSE
        return param

    def naredba(self):
        if self > T.IF:
            return self.grananje()
        elif self > T.WHILE:
            return self.whilePetlja()
        elif self > T.FOR:
            return self.forPetlja()
        elif self >= T.RETURN:
            return Vrati(self.tipa(self.tipf))
        elif tipVarijable := self >= {T.NUMBERTYPE, T.BOOLTYPE, T.LISTTYPE}:  # inicijalizacija
            ime = self >> T.NAME
            if ime in self.symtab: raise SemantičkaGreška("Varijabla se ne može dva puta inicijalizirati")
            self >> T.EQ
            self.symtab[ime] = tipVarijable
            return Pridruzivanje(tipVarijable, ime, self.tipa(tipVarijable))
        else:  # azuriranje
            ime = self >> T.NAME
            if self >= T.EQ:
                tipVarijable = self.symtab[ime]
                if not tipVarijable: raise SemantičkaGreška("Varijabla se ne može ažurirati prije inicijalizacije")
                return Azuriranje(tipVarijable.tip, ime, self.tipa(tipVarijable))

    def tipa(self, tip):
        if tip ^ T.NUMBERTYPE:
            return self.aritm()
        elif tip ^ T.BOOLTYPE:
            return self.logStart()
        elif tip ^ T.LISTTYPE:
            return self.list()
        else:
            assert False, f'Nepoznat tip {tip}'

    def grananje(self):
        elifNaredbe = elseNaredbe = []
        elifUvjet = Nenavedeno
        self >> T.IF
        self >> T.OPEN
        ifUvjet = self.logStart()
        self >> T.CLOSE
        ifNaredbe = self.blok()
        if self >= T.ELIF:
            self >> T.OPEN
            elifUvjet = self.logStart()
            self >> T.CLOSE
            elifNaredbe = self.blok()
        if self >= T.ELSE: elseNaredbe = self.blok()
        return Grananje(ifUvjet, ifNaredbe, elifUvjet, elifNaredbe, elseNaredbe)

    def whilePetlja(self):
        self >> T.WHILE
        self >> T.OPEN
        logUvjet = self.logStart()
        self >> T.CLOSE
        naredbe = self.blok()
        return WhilePetlja(logUvjet, naredbe)

    def forPetlja(self):
        self >> T.FOR
        self >> T.OPEN
        i = self >> T.NAME
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise SintaksnaGreška("Ime varijable u for petlji mora biti isto")
        stopUsporedba = self >> operatoriUsporedbe
        stopVarijabla = self.aritm()
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise SintaksnaGreška("Ime varijable u for petlji mora biti isto")
        if self >= T.PP:
            inkrement = 1
        else:
            self >> T.PE
            inkrement = self >> T.NUM
        self >> T.CLOSE
        naredbe = self.blok()
        return ForPetlja(i, stopUsporedba, stopVarijabla, inkrement, naredbe)

    def blok(self):
        self >> T.OPENBR
        if self >= T.CLOSEBR: return Blok([])
        n = [self.naredba()]
        while self >= T.SEMICOL and not self > T.CLOSEBR: n.append(self.naredba())
        self >> T.CLOSEBR
        return Blok.ili_samo(n)

    def logStart(self):
        disjunkti = [self.disjunkcija()]
        while self >= T.OR: disjunkti.append(self.disjunkcija())
        return Binarna.ili_samo(disjunkti)

    def disjunkcija(self):
        konjunkti = [self.konjunkcija()]
        while self >= T.AND: konjunkti.append(self.konjunkcija())
        return Binarna.ili_samo(konjunkti)

    def konjunkcija(self):
        while self >= T.NOT:
            return Unarna(T.NOT, self.konjunkcija())

    def logIzraz(self):
        if log := self >= {T.TRUE, T.FALSE, T.UNKNOWN}:
            return log
        if name := self >= T.NAME:
            return self.mozda_poziv(name, T.BOOLTYPE)
        elif self >= T.OPEN:
            if self >= T.BOOLTYPE:
                self >> T.CLOSE
                return self.mozda_poziv(self >> T.NAME, True)  # TODO: Dodati ostale stvari ako stignemo
            if not self > {T.TRUE, T.FALSE, T.UNKNOWN, T.NAME, T.OPEN}:
                SemantičkaGreška("Nakon otvorene zagrade mora doći eksplicitna pretvorba ili logički izraz")
            logStart = self.logStart()
            self >> T.CLOSE
            return logStart
        return Usporedba(self.aritm(), self >> operatoriUsporedbe, self.aritm())

    def mozda_poziv(self, ime, ocekivaniTip, force=False):
        if ime in self.funkcije:
            funkcija = self.funkcije[ime]
            if funkcija.tip != ocekivaniTip: raise SemantičkaGreška(
                f"Povratni tip funkcije {ime} nije očekivanog tipa {ocekivaniTip}")
            return Poziv(funkcija, self.argumenti(funkcija.parametri))
        elif ime == self.imef:
            return Poziv(nenavedeno, self.argumenti(self.parametrif))
        elif ime in self.symtab:
            if self.symtab[ime].tip != ocekivaniTip: raise SemantičkaGreška(
                f"Tip varijable {ime} nije očekivanog tipa {ocekivaniTip}")
            return self.symtab[ime]
        elif force:
            raise SintaksnaGreška("Kriva eksplicitna pretvorba")
        else:
            return ime

    def argumenti(self, parametri):
        arg = []
        self >> T.OPEN
        for i, parametar in enumerate(parametri):
            if i: self >> T.COMMA
            arg.append(self.tipa(parametar))
        self >> T.CLOSE
        return arg

    def aritm(self):
        t = self.clan()
        while op := self >= {T.PLUS, T.MINUS}: t = Binarna(op, t, self.clan())
        return t

    def clan(self):
        t = self.faktor()
        while op := self >= {T.TIMES, T.DIV}: t = Binarna(op, t, self.faktor())
        return t

    def faktor(self):
        if op := self >= T.MINUS: return Unarna(op, self.faktor())
        baza = self.baza()
        if op := self >= T.EXP:
            return Binarna(op, baza, self.faktor())
        else:
            return baza

    def baza(self):
        if self >= T.OPEN:
            if self >= T.BOOLTYPE:
                self >> T.CLOSE
                return self.mozda_poziv(self >> T.NAME, True)
            trenutni = self.aritm()
            self >> T.CLOSE
        elif num := self >= T.NUM: return num
        return self.mozda_poziv(self >> T.NAME, T.NUM)

    # List mojaLista = [[1,2,3],[[true], 1, 2], 3]
    def list(self):
        if self >= T.OPEN:
            self >> T.LISTTYPE
            self >> T.CLOSE
            return self.mozda_poziv(self >> T.NAME, True)
        if name := self >= T.NAME: return self.mozda_poziv(name, T.LISTTYPE)
        self >> T.OLIST
        argumenti = [self.listArgument()]
        while self >= T.COMMA: argumenti.append(self.listArgument())
        self >> T.CLIST
        return Lista(argumenti)

    def listArgument(self):
        if self > T.OLIST:
            return [self.list()]
        elif name := self >> {T.NUM, T.TRUE, T.FALSE, T.UNKNOWN}:
            return name
        return self.mozda_poziv(self >> T.NAME, T.LISTTYPE)

    start = program
    lexer = lexer


def izvrsi(funkcije, *argv):
    print('Program je vratio:', funkcije['program'].pozovi(argv))


def numBoolToToken(num):
    if num == -1:
        return T.FALSE
    elif num == 0:
        return T.UNKNOWN
    elif num == 1:
        return T.TRUE
    else:
        assert False, f"{num} nije vrijednost nikoje instance Bool tipa"


### AST
# Program: funkcije[funkcija] #(za sad nema ovog: naredbe:[naredba])
# Funkcija: tip:TYPE ime:IME parametri:[[tip:TYPE ime:IME]] naredbe:[naredba]
# naredba: Grananje: istinitost:JE|NIJE uvjet:log onda:naredba inače:naredba
#          ForPetlja: varijabla:"" stopUvijet:log inkrement:NUM naredbe:[naredba]
#          WhilePetlja: uvjet:log naredbe:[naredba]
#          Pridruzivanje: tip:TYPE ime:NAME pridruženo:izraz
#          Azuriranje: tip:TYPE ime:NAME pridruženo:izraz
#          Vrati: tip:TYPE što:izraz
# izraz: Usporedba: lijevo:aritm relacija:EE|NE|LT|LE|GT|GE desno:aritm
#        NUM: Token
#        NAME: Token
#        Binarna: op:(aritm:PLUS|MINUS|TIMES|DIV|EXP)|(log:OR|AND) lijevo:izraz desno:izraz
#        Unarna: op:(aritm:MINUS)|(log:NOT) ispod:izraz
#        Poziv: funkcija:Funkcija argumenti:[izraz]

class Funkcija(AST('povratniTip ime parametri naredbe symtab')):
    def pozovi(self, argumenti):
        symtab = Memorija(dict(self.symtab))
        lokalni = Memorija(dict(zip(self.parametri, argumenti)))
        for naredba in self.naredbe:
            try:
                naredba.izvrsi(symtab, lokalni, self)
            except Povratak as exc:
                return exc.preneseno
            # else:
            #     raise GreškaIzvođenja(f'{self.ime} nije ništa vratila')

class Poziv(AST('funkcija argumenti')):
    def vrijednost(self, mem, unutar):
        pozvana = self.funkcija
        if pozvana is nenavedeno: pozvana = unutar  # rekurzivni poziv
        argumenti = [a.vrijednost(mem, unutar) for a in self.argumenti]
        return pozvana.pozovi(argumenti)

class Grananje(AST('istinitost uvjet onda inače')):
    def izvrsi(self, symtab, mem, unutar):
        if ispunjen(self, mem, unutar):
            self.onda.izvrsi(mem, unutar, )
        else:
            self.inače.izvrsi(mem, unutar, )


class Binarna(AST('op lijevo desno')):
    def vrijednost(self, env):
        o,x,y = self.op, self.lijevo.vrijednost(env), self.desno.vrijednost(env)
        try:
            if o ^ T.PLUS:
                return x + y
            elif o ^ T.MINUS:
                return x - y
            elif o ^ T.TIMES:
                return x * y
            elif o ^ T.DIV:
                return x / y
            elif o ^ T.EXP:
                return x ** y
            elif o ^ T.OR:
                return numBoolToToken(max(x, y))
            elif o ^ T.AND:
                return numBoolToToken(min(x, y))
            else:
                assert False, f'nepokriveni slučaj binarnog operatora {o}'
        except ArithmeticError as ex:
            raise o.iznimka(ex)


class Unarna(AST('op ispod')):
    def vrijednost(self, env):
        o, x = self.op, self.ispod.vrijednost(env)
        if o ^ T.MINUS:
            return -x
        elif o ^ T.NOT:
            return numBoolToToken(-x)
        else:
            assert False, f'nepokriveni slučaj unarnog operatora {o}'

    def _asdict(self):  # samo za ispis, da se ne ispiše čitava funkcija
        za_ispis = {'argumenti': self.argumenti}
        if self.funkcija is nenavedeno: za_ispis['*rekurzivni'] = True
        else: za_ispis['*ime'] = self.funkcija.ime
        return za_ispis

def ispunjen(ast, mem, unutar):
    trazeno = {T.JE: True, T.NIJE: False}[ast.istinitost.tip]
    return ast.uvjet.vrijednost(mem, unutar) == trazeno


class Petlja(AST('istinitost uvjet tijelo')):
    def izvrsi(self, symtab, mem, unutar):
        while ispunjen(self, mem, unutar): self.tijelo.izvrsi(mem, unutar, )


class Usporedba(AST('lijevo relacija desno')):
    def vrijednost(self, mem, unutar):
        l = self.lijevo.vrijednost(mem, unutar)
        d = self.desno.vrijednost(mem, unutar)
        if self.relacija ^ T.JEDNAKO: return l == d
        elif self.relacija ^ T.MANJE: return l < d
        else: assert False, f'Nepoznata relacija {self.relacija}'


class Pridruzivanje(AST('varijabla tip vrijednost')):
    def izvrsi(self, symtab, mem, unutar):
        lijevo = symtab[self.varijabla]
        desno = self.vrijednost.provjeri_tip(symtab)
        if not desno <= lijevo:
            raise self.varijabla.krivi_tip(lijevo, desno)
        mem[self.ime] = self.pridruženo.vrijednost(mem, unutar)

class Blok(AST('naredbe')):
    def izvrsi(self, symtab, mem, unutar):
        for naredba in self.naredbe: naredba.izvrsi(mem, unutar, )

class Vrati(AST('što')):
    def izvrsi(self, symtab, mem, unutar):
        raise Povratak(self.što.vrijednost(mem, unutar))

class Povratak(NelokalnaKontrolaToka): """Signal koji šalje naredba vrati."""
