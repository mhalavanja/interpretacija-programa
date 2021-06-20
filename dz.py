from vepar import *


class T(TipoviTokena):
    OPEN, CLOSE, COMMA, SEMICOL, OLIST, CLIST, LT, GT, EQ, PLUS, MINUS, TIMES, DIV, EXP, OPENBR, CLOSEBR = "(),;[]<>=+-*/^{}"
    NE, LE, GE, PP, EE, PE = "!=", "<=", ">=", "++", "==", "+="
    IF, ELIF, ELSE, FOR, WHILE, RETURN, DEF = 'if', "elif", 'else', "for", 'while', 'return', "def"
    NOT, AND, OR = 'not', 'and', 'or'
    NUMBER, BOOL, LIST = "Number", "Bool", "List"
    PICK, PUT = "pick", "put"  # pick up, put down

    class NAME(Token):
        # TODO: Ako ne postoji varijabla u lokalnoj memoriji, mozda je u globalnoj
        def value(self, mem, unutar): return mem[self]

        def typeCheck(self, mem): return mem[self].tip

    class NUM(Token):
        def value(self, mem, unutar): return int(self.sadržaj)

        def typeCheck(self, mem): return T.NUMBER

    class BREAK(Token):
        literal = 'break'

        def run(self, mem, unutar): raise Break

    class TRUE(Token):
        literal = 'True'

        def value(self, mem, unutar): return 1

        def typeCheck(self, mem): return T.BOOL

    class FALSE(Token):
        literal = 'False'

        def value(self, mem, unutar): return -1

        def typeCheck(self, mem): return T.BOOL

    class UNKNOWN(Token):
        literal = 'Unknown'

        def value(self, mem, unutar): return 0

        def typeCheck(self, mem): return T.BOOL

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

# inicijaliziraj -> NUMBER NAME EQ aritm | BOOL NAME EQ logStart | LIST NAME EQ list
# azuriraj -> NAME EQ (aritm | logStart | list)  [KONTEKST!] prvo mora biti inicijalizirana varijabla

# logStart -> logStart OR disjunkt | disjunkt
# disjunkt -> disjunkt AND konjunk | konjunk
# konjunk -> logiz | NOT konjunk
# logiz -> aritm usporedba aritm | TRUE | FALSE | UNKNOWN | NAME poziv? | OPEN logStart CLOSE |
#          OPEN BOOL CLOSE NAME poziv? -- eksplicitne pretvorbe
# usporedba -> NE | LE | GE | EE | LT | GT

# list -> OLIST (listArgument | list)? CLIST | NAME poziv? | OPEN LIST CLOSE NAME poziv?--

# aritm -> aritm PLUS član | aritm MINUS član | član
# član -> član TIMES faktor | član DIV faktor | faktor
# faktor -> baza | baza EXP faktor | MINUS faktor
# baza -> NUM | baza | OPEN aritm CLOSE | NAME poziv? | OPEN NUM CLOSE NAME poziv? --

# poziv -> OPEN argumenti? CLOSE
# argumenti -> argument | argument COMMA argumenti
# argument -> aritm |! logStart |! list  [KONTEKST!] kod poziva funkcija
# listArgument -> list | NUM | TRUE | FALSE | UNKNOWN
# type -> NUMBER, BOOL, LIST

operatoriUsporedbe = {T.EE, T.NE, T.LE, T.LT, T.GE, T.GT}


class P(Parser):
    def program(self):
        self.funkcije = Memorija(redefinicija=False)
        # self.symtab = Memorija()  # globalna memorija za varijable
        self.funkcije["forward"] = Funkcija([T.NUMBER, "forward", [T.NUMBER, "centimeters"]])
        self.funkcije["turn"] = Funkcija([T.NUMBER, "turn", [T.NUMBER, "radians"]])
        self.funkcije["check"] = Funkcija([T.BOOL, "check", [T.NUMBER, "centimeters"]])
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
        self >> T.DEF
        atributi = self.tipf, self.imef, self.parametrif = self.tipIme(), self.parametri()
        # u varTypeMem spremamo tipove lokalnih varijabli pa ih provjeravamo za vrijeme parsiranja
        self.varTypeMem = Memorija()
        naredbe = self.blok()
        return Funkcija(*atributi, naredbe)

    def tipIme(self):
        return self >> {T.NUMBER, T.BOOL, T.LIST}, self >> T.NAME

    def parametri(self):
        self >> T.OPEN
        if self >= T.CLOSE: return []
        param = [self.tipIme()]
        while self >= T.COMMA: param.append(self.tipIme())
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
        elif tipVarijable := self >= {T.NUMBER, T.BOOL, T.LIST}:  # inicijalizacija
            ime = self >> T.NAME
            if self.varTypeMem[ime]: raise SemantičkaGreška("Varijabla se ne može dva puta inicijalizirati")
            self >> T.EQ
            self.varTypeMem[ime] = tipVarijable
            return Pridruzivanje(tipVarijable, ime, self.tipa(tipVarijable))
        else:  # azuriranje
            ime = self >> T.NAME
            if self >= T.EQ:
                tipVarijable = self.varTypeMem[ime]
                if not tipVarijable: raise SemantičkaGreška("Varijabla se ne može ažurirati prije inicijalizacije")
                return Azuriranje(varijabla.tip, ime, self.tipa(varijabla.tip))

    def tipa(self, tip):
        if tip ^ T.NUMBER:
            return self.aritm()
        elif tip ^ T.BOOL:
            return self.logStart()
        elif tip ^ T.LIST:
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
        return Binarna.ili_samo(disjunkti)

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
        if log := self >= {T.TRUE, T.FALSE, T.UNKNOWN}:
            return log
        if name := self >= T.NAME:
            return self.mozda_poziv(name, T.BOOL)
        elif self >= T.OPEN:
            if self >= T.BOOL:
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
        elif ime in self.varTypeMem:
            if funkcija.tip != ocekivaniTip: raise SemantičkaGreška(
                f"Tip varijable {ime} nije očekivanog tipa {ocekivaniTip}")
            return self.varTypeMem[ime]
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
            if self >= T.BOOL:
                self >> T.CLOSE
                return self.mozda_poziv(self >> T.NAME, True)
            trenutni = self.aritm()
            self >> T.CLOSE
        elif num = self >= T.NUMBER: return num
        return self.mozda_poziv(self >> T.NAME, T.NUMBER)

    # List mojaLista = [[1,2,3],[[true], 1, 2], 3]
    def list(self):
        if self >= T.OPEN:
            self >> T.LIST
            self >> T.CLOSE
            return self.mozda_poziv(self >> T.NAME, True)
        if name := self >= T.NAME: return self.mozda_poziv(name, T.LIST)
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
        return self.mozda_poziv(self >> T.NAME, T.LIST)

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

class Funkcija(AST('povratniTip ime parametri naredba')):
    def pozovi(self, argumenti):
        lokalni = Memorija(dict(zip(self.parametri, argumenti)))
        try:
            self.naredba.izvrsi()
        except Povratak as exc:
            return exc.preneseno
        else:
            raise GreškaIzvođenja(f'{self.ime} nije ništa vratila')


class Binarna(AST('op argumenti')):
    def vrijednost(self, env):
        o = self.op
        for arg in self.argumenti:
            arg = arg.vrijednost(env)
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
