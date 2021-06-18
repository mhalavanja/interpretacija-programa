from vepar import *


class T(TipoviTokena):
    OPEN, CLOSE, COMMA, SEMICOL, OLIST, CLIST, LT, GT, EQ, PLUS, MINUS, TIMES, DIV, EXP, OPENBR, CLOSEBR = "(),;[]<>=+-*/^{}"
    NE, LE, GE, PP, MM, EE, PE = "!=", "<=", ">=", "++", "--", "==", "+="
    IF, ELIF, ELSE, FOR, WHILE, RETURN, DEF = 'if', "elif", 'else', "for", 'while', 'return', "def"
    NOT, AND, OR = 'not', 'and', 'or'
    NUMBER, BOOL, LIST = "Number", "Bool", "List"
    PICK, PUT = "pick", "put"  # pick up, put down

    class NAME(Token):
        #TODO: Ako ne postoji varijabla u lokalnoj memoriji, mozda je u globalnoj
        def value(self, mem, unutar): return mem[self]

        def typeCheck(self, symtab): return symtab[self].tip

    class NUM(Token):
        def value(self, mem, unutar): return int(self.sadržaj)

        def typeCheck(self, symtab): return T.NUMBER

    class BREAK(Token):
        literal = 'break'

        def run(self, mem, unutar): raise Break

    class TRUE(Token):
        literal = 'True'

        def value(self, mem, unutar): return 1

        def typeCheck(self, symtab): return T.BOOL

    class FALSE(Token):
        literal = 'False'

        def value(self, mem, unutar): return -1

        def typeCheck(self, symtab): return T.BOOL

    class UNKNOWN(Token):
        literal = 'Unknown'

        def value(self, mem, unutar): return 0

        def typeCheck(self, symtab): return T.BOOL

    # forward(50)
    class FORWARD(Token):
        literal = "forward"

        def value(self, mem, unutar): return mem[self]

    # turn(90)
    class TURN(Token):
        literal = "turn"

        def value(self, mem, unutar): return mem[self]


class Break(NelokalnaKontrolaToka):
    """Signal koji šalje naredba break."""


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
        elif znak == '/':
            if lex >= "*":
                while True:
                    lex.pročitaj_do("*", više_redova=True)
                    if lex >= "/":
                        lex.zanemari()
                        break
            else:
                yield lex.token(T.DIV)
        elif znak == '#':
            lex.pročitaj_do('\n')
            lex.zanemari()
        elif znak.isalpha():
            lex.zvijezda(Fidentifikator)
            yield lex.literal(T.NAME)
        elif znak.isdecimal():
            lex.prirodni_broj(znak) #TODO: Dodati decimalne brojeve ako stignemo
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

# inicijaliziraj -> NUMBER NAME EQ aritm | BOOL NAME EQ logStart | LIST NAME EQ list
# azuriraj -> NAME EQ (aritm | logStart | list)  [KONTEKST!] prvo mora biti inicijalizirana varijabla

# logStart -> logStart OR disjunkt | disjunkt
# disjunkt -> disjunkt AND konjunk | konjunk
# konjunk -> logiz | NOT konjunk
# logiz -> aritm usporedba aritm | TRUE | FALSE | UNKNOWN | NAME (poziv)? | OPEN logStart CLOSE |
#          OPEN BOOL CLOSE NAME (poziv)? -- eksplicitne pretvorbe
# usporedba -> NE | LE | GE | EE | LT | GT

# list -> OLIST CLIST | OLIST dargumenti CLIST | NAME (poziv)? | OPEN LIST CLOSE NAME --

# aritm -> aritm PLUS član | aritm MINUS član | član
# član -> član TIMES faktor | član DIV faktor | faktor
# faktor -> broj | broj EXP faktor
# broj -> NUM | MINUS broj | broj (PP | MM)? | OPEN aritm CLOSE | NAME (poziv)? | OPEN NUM CLOSE NAME --

# poziv -> OPEN argumenti? CLOSE
# argumenti -> argument | argument COMMA argumenti
# dargumenti -> argument | COMMA | argument COMMA argumenti -- dangling comma
# argument -> aritm |! logStart |! list  [KONTEKST!]
# type -> NUMBER, BOOL, LIST

operatoriUsporedbe = {T.EE, T.NE, T.LE, T.LT, T.GE, T.GT}

class P(Parser):
    def program(self):
        self.funkcije = Memorija(redefinicija=False)
        self.symtab = Memorija() #memorija za varijable
        self.funkcije["forward"] = Funkcija([T.NUMBER, "forward", [T.NUMBER, "centimeters"]])
        self.funkcije["turn"] = Funkcija([T.NUMBER, "turn", [T.NUMBER, "radians"]])
        naredbe = funkcije = []
        while self > T.DEF:
            funkcija = self.funkcija()
            funkcije.append(funkcija.ime)
            self.funkcije[funkcija.ime] = funkcija
        while not self > KRAJ:
            naredbe.append(self.naredba())
            self >> {T.SEMICOL, KRAJ}
        return Program(funkcije, naredbe, self.funkcije, self.symtab)

    def funkcija(self):
        self >> T.DEF
        atributi = self.tipf, self.imef, self.parametrif = self.tipIme(), self.parametri()
        naredbe = self.blok()
        return Funkcija(*atributi, naredbe)

    def tipIme(self): return [self >> {T.NUMBER, T.BOOL, T.LIST}, self >> T.NAME]

    def parametri(self):
        self >> T.OPEN
        if self >= T.CLOSE: return []
        param = [self.tipIme()]
        while self >= T.COMMA: param.append(self.tipIme())
        self >> T.CLOSE
        return param

    def naredba(self):
        if self > T.IF: return self.grananje()
        elif self > T.WHILE: return self.whilePetlja()
        elif self > T.FOR: return self.forPetlja()
        elif self >= T.RETURN: return Vrati(self.tipa(self.tipf))
        elif tipVarijable := self >= {T.NUMBER, T.BOOL, T.LIST}: #inicijalizacija
            ime = self >> T.NAME
            if self.symtab[ime]: raise SintaksnaGreška("Varijabla se ne može dva puta inicijalizirati")
            self >> T.EQ
            pridruzivanje = Pridruzivanje(tipVarijable, ime, self.tipa(tipVarijable))
            self.symtab[ime] = pridruzivanje
            return pridruzivanje
        else: #azuriranje, AST je isti, ali sintaksa je drugacija
            ime = self >> T.NAME
            if self >= T.EQ:
                varijabla = self.symtab[ime]
                if not varijabla: raise SintaksnaGreška("Varijabla se ne može ažurirati prije inicijalizacije")
                azuriranje = Azuriranje(varijabla.tip, ime, self.tipa(varijabla.tip))
                self.symtab[ime] = azuriranje
                return azuriranje

    def tipa(self, tip):
        if tip ^ T.NUMBER:
            return self.aritm()
        elif tip ^ T.BOOL:
            return self.logStart()
        elif tip ^ T.LIST:
            return self.list()  # TODO: implementirat ovo
        else:
            assert False, f'Nepoznat tip {tip}'

    def grananje(self):
        elifNaredbe = elseNaredbe = []
        elifUvijet = Nenavedeno
        self >> T.IF
        self >> T.OPEN
        ifUvijet = self.logStart()
        self >> T.CLOSE
        ifNaredbe = self.blok()
        if self >= T.ELIF:
            self >> T.OPEN
            elifUvijet = self.logStart()
            self >> T.CLOSE
            elifNaredbe = self.blok()
        if self >= T.ELSE: elseNaredbe = self.blok()
        return Grananje(ifUvijet, ifNaredbe, elifUvijet, elifNaredbe, elseNaredbe)

    def whilePetlja(self):
        self >> T.WHILE
        self >> T.OPEN
        logUvijet = self.logStart()
        self >> T.CLOSE
        naredbe = self.blok()
        return WhilePetlja(logUvijet, naredbe)

    def forPetlja(self):
        self >> T.FOR
        self >> T.OPEN
        i = self >> T.NAME
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise SintaksnaGreška("Ime varijable u for petlji mora biti isto")
        stopUsporedba = self >> operatoriUsporedbe
        stopVarijabla = T.NUMBER  # TODO: moze biti i varijabla ili poziv funkcije
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise SintaksnaGreška("Ime varijable u for petlji mora biti isto")
        if self >= T.PP:
            inkrement = 1
        else:
            self >> T.PE
            inkrement = self >> T.NUMBER
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
        return Disjunkcija.ili_samo(disjunkti)

    def disjunkcija(self):
        konjunkti = [self.konjunkcija()]
        while self >= T.AND: konjunkti.append(self.konjunkcija())
        return Konjunkcija.ili_samo(konjunkti)

    def konjunkcija(self):
        brNeg = 0
        while self >= T.NOT: brNeg += 1
        izraz = self.logIzraz()
        return LogickiIzraz(izraz, brNeg)

    def logIzraz(self):
        if log := self >= {T.TRUE, T.FALSE, T.UNKNOWN, T.NAME}: return self.mozda_poziv(log)
        elif self >= T.OPEN:
            if self >= T.BOOL:
                self >> T.CLOSE
                mozda = self.mozda_poziv(self >> T.NAME, True)
                return mozda
            else:
                #TODO: OPEN logStart CLOSE, al treba >>
                if not self > {T.TRUE, T.FALSE, T.UNKNOWN, T.NAME, T.OPEN}:
                    SemantičkaGreška("Nakon otvorene zagrade mora doći eksplicitna pretvorba ili logički izraz")
                logStart = self.logStart()
                return logStart
        return Usporedba(self.aritm(), self >> operatoriUsporedbe, self.aritm())

    def mozda_poziv(self, ime, force=False):
        if ime in self.funkcije:
            funkcija = self.funkcije[ime]
            return Poziv(funkcija, self.argumenti(funkcija.parametri))
        elif ime == self.imef:
            return Poziv(nenavedeno, self.argumenti(self.parametrif))
        elif ime in self.symtab:
            return self.symtab[ime]
        elif force: raise SintaksnaGreška("Kriva eksplicitna pretvorba")
        else:
            return ime

    def argumenti(self, parametri):
        arg = []
        self >> T.OTV
        for i, parametar in enumerate(parametri):
            if i: self >> T.ZAREZ
            arg.append(self.tipa(parametar))
        self >> T.ZATV
        return arg

    def aritm(self):
        članovi = [self.član()]
        while True:
            if self >= T.PLUS:
                članovi.append(self.član())
            elif self >= T.MINUS:
                članovi.append(Suprotan(self.član()))
            else:
                return Zbroj.ili_samo(članovi)

    def član(self):
        faktori = [self.faktor()]
        while self >= T.ZVJEZDICA: faktori.append(self.faktor())
        return Umnožak.ili_samo(faktori)

    def faktor(self):
        if self >= T.MINUS:
            return Suprotan(self.faktor())
        elif aritm := self >= T.AIME:
            return self.mozda_poziv(aritm)
        elif self >= T.OTV:
            u_zagradi = self.aritm()
            self >> T.ZATV
            return u_zagradi
        else:
            return self >> T.BROJ

    start = program
    lexer = lexer

def negacija(log): return -log

### AST
# Program: funkcije[funkcija] naredbe:[naredba]
# Funkcija: tip:TYPE ime:IME parametri:[IME] naredba:naredba
# naredba: Grananje: istinitost:JE|NIJE uvjet:log onda:naredba inače:naredba
#          Petlja: istinitost:JE|NIJE uvjet:log tijelo:naredba
#          Blok: naredbe:[naredba]
#          Pridruzivanje: tip:TYPE ime:NAME pridruženo:izraz
#          Azuriranje: tip:TYPE ime:NAME pridruženo:izraz
#          Vrati: što:izraz
# izraz: log: Disjunkcija: disjunkti:[log]
#             Usporedba: lijevo:aritm relacija:MANJE|JEDNAKO desno:aritm
#        aritm: Zbroj: pribrojnici:[aritm]
#               Suprotan: od:aritm
#               Umnožak: faktori:[aritm]
#        Poziv: funkcija:Funkcija argumenti:[izraz]
