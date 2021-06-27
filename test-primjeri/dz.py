from time import sleep

test = ['############', '#******₸***#', '#♫♫********#', '#******█***#', '#****₸*****#', '#***█↑*****#', '#*█*******█#',
        '#***♫***₸**#', '#**█****₸**#', '#*█***₸***█#', '############']

mapa = res = [list(sub) for sub in test]
smjerovi = ['gore', 'lijevo', 'dolje', 'desno']
ikone = ['↑', '←', '↓', '→']
kutije = ['▲', '◄', '▼', '►']
kutija = '█'
kvar = '₸'
smjer = 'gore'
pozx = 5
pozy = 5
zvuk = '♫'
senzor = 3
# print (*mapa, sep = '\n')

# Bool i List se ne mogu usporedivati

from vepar import *
import pprint

pp = pprint.PrettyPrinter()


def nacrtajMapu(mapa, poruka, seconds=0):
    sleep(seconds)
    print(poruka)
    pp.pprint(mapa)
    print()


def updateGlobalni(globalni, pozx, pozy, smjer, mapa):
    globalni["pozx"] = pozx
    globalni["pozy"] = pozy
    globalni["smjer"] = smjer
    globalni["mapa"] = mapa


def raspakirajGlobalni(globalni):
    ret = [int(globalni["pozx"]), int(globalni["pozy"]), globalni["smjer"],
           globalni["mapa"], globalni["smjerovi"]]
    ret.append(globalni["senzor"])

    ret.append(globalni["kutija"]), ret.append(globalni["kutije"]), ret.append(globalni["ikone"]), ret.append(
        globalni["kvar"]), ret.append(globalni["zvuk"])
    return ret


# Ovo je odnos izmedu Token() i .tip,inace pada
# Token(T.NESTO) -> NESTO'Nesto'
# (NESTO'Nesto').tip -> T.NESTO

class T(TipoviTokena):
    OPEN, CLOSE, COMMA, SEMICOL, OLIST, CLIST, LT, GT, EQ, PLUS, MINUS, TIMES, DIV, EXP, OPENBR, CLOSEBR = "(),;[]<>=+-*/^{}"
    NE, LE, GE, PP, EE, PE = "!=", "<=", ">=", "++", "==", "+="
    IF, ELIF, ELSE, FOR, WHILE, RETURN, DEF, APPEND, ADD = 'if', "elif", 'else', "for", 'while', 'return', "def", "append", "add"
    NOT, AND, OR = 'not', 'and', 'or'
    NUMBERTYPE, BOOLTYPE, LISTTYPE = "Number", "Bool", "List"
    FORWARD, CHECK, TURN, PUT, PICK, REPAIR, ISWALL, ISBOX, ISSHORT = "forward", "check", "turn", "put", "pick", "repair", "iswall", "isbox", "isshort"
    NOISE = "noise"
    GETX, GETY = "getx", "gety"

    class NAME(Token):
        def izvrsi(self, mem, globalni, unutar): return mem[self]

    class NUM(Token):
        def izvrsi(self, mem, globalni, unutar): return int(self.sadržaj)

    class TRUE(Token):
        literal = 'True'

        def izvrsi(self, mem, globalni, unutar): return 1

    class FALSE(Token):
        literal = 'False'

        def izvrsi(self, mem, globalni, unutar): return -1

    class UNKNOWN(Token):
        literal = 'Unknown'

        def izvrsi(self, mem, globalni, unutar): return 0

    class BREAK(Token):
        literal = 'break'

        def izvrsi(self, mem, globalni, unutar): raise Prekid

    # Number forward(Number cm) ide u naprijed za cm centimetara
    # Povratna izvrsi je broj centimetara za koje je otisao naprijed
    # (da znamo koliko je otisao naprijed ako ne moze ici za zadani broj)

    # Number turn(Number radians) okrece se u smjeru suprotnom od kazaljki na satu za radians radijana
    # Povratna izvrsi je broj radijana za koliko se okrenuo
    # (da znamo koliko se okrenuo ako se ne moze okrenuti za zadani broj)

    # Bool check(Number cm) provjeri vidi li zapreku u narednih cm centimetara
    # Povratna izvrsi označava postoji li zapreka

    # pick up objekt koji je pickupable :)
    # put down objekt koji robot drzi


class Prekid(NelokalnaKontrolaToka):
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
        elif znak == '>':
            yield lex.token(T.GE if lex >= '=' else T.GT)
        elif znak == '<':
            yield lex.token(T.LE if lex >= '=' else T.LT)
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
            lex.prirodni_broj(znak)
            yield lex.token(T.NUM)
        else:
            yield lex.literal(T)


### (B)KG
# program -> funkcija program | funkcija
# funkcija -> DEF type NAME OPEN parametri? CLOSE blok
# parametri -> type NAME | type NAME COMMA parametri

# blok -> OPENBR naredbe CLOSEBR
# naredbe -> condNaredba naredbe? | naredba SEMICOL naredbe?
# naredba -> inicijaliziraj | azuriraj | RETURN argument | aritmNaredbe | boolNaredbe | break
# inicijaliziraj -> NUMBERTYPE NAME EQ aritm | BOOLTYPE NAME EQ logStart | LISTTYPE NAME EQ list
# azuriraj -> NAME EQ (aritm | logStart | list)  [KONTEKST!] prvo mora biti inicijalizirana varijabla
# condNaredba -> IF OPEN logStart CLOSE blok (ELIF OPEN logStart CLOSE blok)* (ELSE blok)?
#              | WHILE OPEN logStart CLOSE blok
#              | FOR OPEN name# SEMICOL name# usporedba aritm SEMICOL name# (PP | PE aritm) CLOSE blok

# boolNaredbe -> okreni | provjeri | podigni | spusti | popravi | provjeriZid | provjeriKutiju | provjeriKvar
# aritmNaredbe -> naprijed | buka | dohvatix | dohvatiy

# naprijed -> FORWARD OPEN aritm CLOSE
# buka -> NOISE OPEN CLOSE
# dohvatix -> GETX OPEN CLOSE
# dohvatiy -> GETY OPEN CLOSE

# okreni -> TURN OPEN aritm CLOSE
# provjeri -> CHECK OPEN aritm CLOSE
# podigni -> PICK OPEN CLOSE
# spusti -> PUT OPEN CLOSE
# popravi -> REPAIR OPEN CLOSE
# provjeriZid -> ISWALL OPEN CLOSE
# provjeriKutiju -> ISBOX OPEN CLOSE
# provjeriKvar -> ISSHORT OPEN CLOSE

# logStart -> logStart OR disjunkt | disjunkt
# disjunkt -> disjunkt AND konjunk | konjunk
# konjunk -> logIzraz | NOT konjunk
# logIzraz -> aritm usporedba aritm | TRUE | FALSE | UNKNOWN | boolNaredbe | NAME poziv? | OPEN logStart CLOSE |
#          OPEN BOOLTYPE CLOSE (NAME poziv? | aritmNaredbe | boolNaredbe) -- eksplicitne pretvorbe
# usporedba -> NE | LE | GE | EE | LT | GT

# list -> OLIST (listArgument | list)? CLIST | NAME poziv? | NAME (OLIST aritm CLIST)? | OPEN LISTTYPE CLOSE (NAME poziv? | aritmNaredbe | boolNaredbe)--
# listArgument -> list | NUM | TRUE | FALSE | UNKNOWN

# poziv -> OPEN argumenti? CLOSE
# argumenti -> argument | argument COMMA argumenti
# argument -> aritm | logStart | list | boolNaredba | aritmNaredba
# type -> NUMBERTYPE, BOOLTYPE, LISTTYPE

# aritm -> aritm PLUS član | aritm MINUS član | član
# član -> član TIMES faktor | član DIV faktor | faktor
# faktor -> baza | baza EXP faktor | MINUS faktor
# baza -> NUM | OPEN aritm CLOSE | aritmNaredbe | NAME poziv? (OLIST aritm CLIST)? | OPEN NUM CLOSE (NAME poziv? | aritmNaredbe | boolNaredba) --


operatoriUsporedbe = {T.EE, T.NE, T.LE, T.LT, T.GE, T.GT}


class P(Parser):
    def program(self):  # deklaracije drugih funkcija mora prethoditi deklaraciji maina
        self.funkcije = Memorija(redefinicija=False)
        try:
            while self > T.DEF:  # deklaracija funkcija pocinje s def
                funkcija = self.funkcija()
                self.funkcije[funkcija.ime] = funkcija
        except Prekid:
            raise SemantičkaGreška('nedozvoljen break izvan petlje')
        return self.funkcije

    def funkcija(self):
        self.symtab = Memorija(redefinicija=False)  # za spremamanje tipova lokalnih varijabli
        self.listMem = Memorija()  # za spremamanje tipova varijabli u listi
        self >> T.DEF
        # povratni tip, ime, parametri trenutne funkcije
        [self.tipf, self.imef], self.parametrif = self.tipIme(), self.parametri()
        blok = self.blok()
        return Funkcija(self.tipf, self.imef, self.parametrif, blok)

    def tipIme(self):
        return self >> {T.NUMBERTYPE, T.BOOLTYPE, T.LISTTYPE}, self >> T.NAME

    # pokupimo parametre funkcije
    def parametri(self):
        self >> T.OPEN
        if self >= T.CLOSE: return []
        tip, ime = self.tipIme()
        param = [(tip, ime)]
        self.symtab[ime] = tip
        while self >= T.COMMA:
            tip, ime = self.tipIme()
            self.symtab[ime] = tip
            param.append((tip, ime))
        self >> T.CLOSE
        return param

    def blok(self):
        self >> T.OPENBR
        if self >= T.CLOSEBR: return Blok([])
        n = []
        while not self > T.CLOSEBR:
            # nakon tijela ovih naredbi (koje mora biti unutar vitica), ne ide ";"
            if self > {T.IF, T.WHILE, T.FOR}:
                n.append(self.naredba())
            else:
                n.append(self.naredba())
                self >> T.SEMICOL
        self >> T.CLOSEBR
        return Blok.ili_samo(n)

    def naredba(self):
        if self >= T.IF:
            return self.grananje()
        elif self >= T.WHILE:
            return self.whilePetlja()
        elif self >= T.FOR:
            return self.forPetlja()
        elif self >= T.APPEND:
            return self.dodajElement(True)
        elif self >= T.ADD:
            return self.dodajElement(False)
        elif self >= T.RETURN:
            return Vrati(self.tipa(self.tipf))
        elif self >= T.FORWARD:
            return self.naprijed()
        elif self >= T.NOISE:
            return self.buka()
        elif self >= T.GETX:
            return self.dohvatix()
        elif self >= T.GETY:
            return self.dohvatiy()
        elif self >= T.TURN:
            return self.okreni()
        elif self >= T.CHECK:
            return self.provjeri()
        elif self >= T.PICK:
            return self.podigni()
        elif self >= T.PUT:
            return self.spusti()
        elif self >= T.REPAIR:
            return self.popravi()
        elif self >= T.ISWALL:
            return self.provjeriZid()
        elif self >= T.ISBOX:
            return self.provjeriKutiju()
        elif self >= T.ISSHORT:
            return self.provjeriKvar()
        elif br := self >= T.BREAK:
            return br
        elif tipVarijable := self >= {T.NUMBERTYPE, T.BOOLTYPE, T.LISTTYPE}:  # inicijalizacija
            ime = self >> T.NAME
            if ime in self.symtab: raise GreškaIzvođenja("Varijabla se ne može dva puta inicijalizirati")
            self >> T.EQ
            self.symtab[ime] = tipVarijable
            if tipVarijable ^ T.LISTTYPE:
                self.imeListe = ime
                self.listMem[ime] = []
            return Pridruzivanje(ime, self.tipa(tipVarijable))
        else:  # azuriranje
            ime = self >> T.NAME
            if self >= T.EQ:
                if ime not in self.symtab: raise GreškaIzvođenja(
                    "Varijabla se ne može ažurirati prije inicijalizacije")
                tipVarijable = self.symtab[ime]
                if tipVarijable ^ T.LISTTYPE:
                    self.imeListe = ime
                return Azuriranje(ime, self.tipa(tipVarijable))

    def tipa(self, tip):
        if tip ^ T.NUMBERTYPE:
            return self.aritm()
        elif tip ^ T.BOOLTYPE:
            return self.logStart()
        elif tip ^ T.LISTTYPE:
            return self.list()
        else:
            assert False, f'Nepoznat tip {tip}'

    def dodajElement(self, kraj):
        self >> T.OPEN
        listName = self >> T.NAME
        if listName not in self.listMem: raise GreškaIzvođenja("Lista mora biti definirana prije nego se appenda")
        self >> T.COMMA
        varName = self >> T.NAME
        self >> T.CLOSE
        if varName in self.symtab:
            tip = self.symtab[varName]
        elif varName in self.funkcije:
            tip = self.funkcije[varName].povratniTip
        elif varName == self.imef:
            tip = self.tipf
        else:
            raise GreškaIzvođenja(f"Ime {varName} nije niti funkcija niti varijabla")
        self.listMem[listName].append(tip)
        return DodajListi(listName, self.mozda_poziv(varName, tip.tip), kraj)

    def grananje(self):
        duljinaElif = 0
        elifBlokovi = []
        elifUvjeti = []
        elifNaredbe = elseNaredbe = Blok([])
        elifUvjet = False
        self >> T.OPEN
        ifUvjet = self.logStart()
        self >> T.CLOSE
        ifNaredbe = self.blok()
        while self >= T.ELIF:
            self >> T.OPEN
            elifUvjet = self.logStart()
            elifUvjeti.append(elifUvjet)
            self >> T.CLOSE
            elifNaredbe = self.blok()
            elifBlokovi.append(elifNaredbe)
        duljinaElif = elifUvjeti.__sizeof__()
        if self >= T.ELSE: elseNaredbe = self.blok()
        return Grananje(ifUvjet, ifNaredbe, elifUvjeti, elifBlokovi, elseNaredbe)

    def whilePetlja(self):
        self >> T.OPEN
        logUvjet = self.logStart()
        self >> T.CLOSE
        blok = self.blok()
        return WhilePetlja(logUvjet, blok)

    def forPetlja(self):
        self >> T.OPEN
        i = self >> T.NAME  # pridruzivanje se moze izvesti prije
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise GreškaIzvođenja("Ime varijable u for petlji mora biti isto")
        stopUsporedba = self >> operatoriUsporedbe
        stopVar = self.aritm()
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise GreškaIzvođenja("Ime varijable u for petlji mora biti isto")
        if self >= T.PP:
            inkrement = 1
        else:
            self >> T.PE
            inkrement = self.aritm()
        self >> T.CLOSE
        blok = self.blok()
        return ForPetlja(i, stopUsporedba, stopVar, inkrement, blok)

    def naprijed(self):
        self >> T.OPEN
        pomak = self.aritm()
        self >> T.CLOSE
        return Pomak(pomak)

    def buka(self):
        self >> T.OPEN
        self >> T.CLOSE
        return RazinaBuke()

    def dohvatix(self):
        self >> T.OPEN
        self >> T.CLOSE
        return X()

    def dohvatiy(self):
        self >> T.OPEN
        self >> T.CLOSE
        return Y()

    def okreni(self):
        self >> T.OPEN
        kut = self.aritm()
        self >> T.CLOSE
        return Okretaj(kut)

    def provjeri(self):
        self >> T.OPEN
        udaljenost = self.aritm()
        self >> T.CLOSE
        return Provjera(udaljenost)

    def podigni(self):
        self >> T.OPEN
        self >> T.CLOSE
        return Dizanje()

    def spusti(self):
        self >> T.OPEN
        self >> T.CLOSE
        return Spustanje()

    def popravi(self):
        self >> T.OPEN
        self >> T.CLOSE
        return Popravljanje()

    def provjeriZid(self):
        self >> T.OPEN
        self >> T.CLOSE
        return ProvjeraZida()

    def provjeriKutiju(self):
        self >> T.OPEN
        self >> T.CLOSE
        return ProvjeraKutije()

    def provjeriKvar(self):
        self >> T.OPEN
        self >> T.CLOSE
        return ProvjeraKvara()

    def logStart(self):
        t = self.disjunkcija()
        while op := self >= T.OR: t = Binarna(op, t, self.disjunkcija())
        return t

    def disjunkcija(self):
        t = self.konjunkcija()
        while op := self >= T.AND: t = Binarna(op, t, self.konjunkcija())
        return t

    def konjunkcija(self):
        if self >= T.NOT:
            return Unarna(Token(T.NOT), self.konjunkcija())  # bez "Token(..)" je padalo
        return self.logIzraz()

    def pretvorba(self, uTip):
        self >> T.CLOSE
        if self > T.OLIST:
            varijabla = self.list()
            tip = T.LISTTYPE
        elif op := self >= T.MINUS:
            varijabla = Unarna(op, self.faktor())
            tip = T.NUMBERTYPE
        elif varijabla := self >= T.NUM:
            tip = T.NUMBERTYPE
        elif varijabla := self >= {T.TRUE, T.UNKNOWN, T.FALSE}:
            tip = T.BOOLTYPE
        else:
            ime = self >> T.NAME
            tip = None
            if ime in self.symtab:
                tip = (self.symtab[ime].tip)
            elif ime in self.funkcije:
                tip = self.funkcije[ime].povratniTip.tip
            elif ime == self.imef:
                tip = Token(self.tipf)
            varijabla = self.mozda_poziv(ime, tip, True)
        return Pretvorba(Token(tip), varijabla, Token(uTip))

    def logIzraz(self):
        if log := self >= {T.TRUE, T.FALSE, T.UNKNOWN}:
            return log
        if boolNaredba := self >= {T.PICK, T.PUT, T.CHECK, T.TURN, T.REPAIR, T.ISWALL, T.ISBOX, T.ISSHORT}:
            return self.mozda_poziv(boolNaredba, T.BOOLTYPE)
        if name := self >= T.NAME:
            if self.provjeriTip(name, T.BOOLTYPE):
                return self.mozda_poziv(name, T.BOOLTYPE)
            elif self.provjeriTip(name, T.LISTTYPE):
                return self.pristupListi(name, T.BOOLTYPE)
            else:
                self.vrati()
        elif self >= T.OPEN:
            if self >= T.BOOLTYPE: return self.pretvorba(T.BOOLTYPE)
            if not self > {T.TRUE, T.FALSE, T.UNKNOWN, T.NAME, T.OPEN}:
                SemantičkaGreška("Nakon otvorene zagrade mora doći eksplicitna pretvorba ili logički izraz")
            logStart = self.logStart()
            self >> T.CLOSE
            return logStart
        return Usporedba(self.aritm(), self >> operatoriUsporedbe, self.aritm())

    def provjeriTip(self, name, povratniTip):
        return (name in self.symtab and self.symtab[name] ^ povratniTip) or (
                name in self.funkcije and self.funkcije[name].povratniTip ^ povratniTip) or (
                       name == self.imef and self.tipf ^ povratniTip)

    def mozda_poziv(self, ime, ocekivaniTip, force=False):
        if ime in self.funkcije:
            funkcija = self.funkcije[ime]
            if not (funkcija.povratniTip ^ ocekivaniTip): raise SemantičkaGreška(
                f"Povratni tip funkcije {ime} nije očekivanog tipa {ocekivaniTip}")
            return Poziv(funkcija, self.argumenti(funkcija.parametri))
        elif ime == self.imef:
            return Poziv(nenavedeno, self.argumenti(self.parametrif))
        elif ime in self.symtab:
            if not ocekivaniTip == None and not (self.symtab[ime].tip == ocekivaniTip):
                raise SemantičkaGreška(
                    f"Tip varijable {ime} je tipa {self.symtab[ime].tip} nije očekivanog tipa {ocekivaniTip}")
            else:
                return ime
        elif ime.tip in {T.FORWARD, T.NOISE, T.GETX, T.GETY, T.TURN, T.CHECK, T.PICK, T.PUT, T.REPAIR, T.ISWALL,
                         T.ISBOX, T.ISSHORT}:
            self.vrati()
            return self.naredba()
        elif force:
            raise SintaksnaGreška("Kriva eksplicitna pretvorba")
        else:
            return ime

    def argumenti(self, parametri):
        arg = []
        self >> T.OPEN
        for i in range(len(parametri)):
            [tip, _] = parametri[i]
            if i: self >> T.COMMA
            arg.append(self.tipa(tip))
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
            if self >= T.NUMBERTYPE: return self.pretvorba(T.NUMBERTYPE)
            if not self > {T.NUM, T.NAME, T.MINUS, T.OPEN}:
                SemantičkaGreška("Nakon otvorene zagrade mora doći eksplicitna pretvorba ili aritmetički izraz")
            aritm = self.aritm()
            self >> T.CLOSE
            return aritm
        elif num := self >= T.NUM:
            return num
        elif name := self >= T.NAME:
            if self.provjeriTip(name, T.LISTTYPE):
                return self.pristupListi(name, T.NUMBERTYPE)
            return self.mozda_poziv(name, T.NUMBERTYPE)
        return self.mozda_poziv(self >> {T.FORWARD, T.NOISE, T.GETX, T.GETY}, T.NUMBERTYPE)

    def pristupListi(self, name, tip):
        self >> T.OLIST
        index = self.aritm()
        self >> T.CLIST
        return PristupListi(name, index, self.listMem[name], Token(tip))

    def list(self, tipoviArgumenata=None):
        if tipoviArgumenata is None:
            tipoviArgumenata = []
        if self >= T.OPEN:  # pretvorba nekog elementa u listu
            self >> T.LISTTYPE
            pretvorba = self.pretvorba(T.LISTTYPE)
            tip = pretvorba.kojiTip
            if tip ^ T.LISTTYPE:
                tipoviArgumenata = self.listMem[pretvorba.varName]
            else:
                tipoviArgumenata.append(tip)
            self.listMem[self.imeListe] = tipoviArgumenata
            return pretvorba
        elif name := self >= T.NAME:
            if self > T.OLIST:  # dohvat elementa iz liste
                self.listMem[self.imeListe] = self.listMem[name]
                return self.pristupListi(name, T.LISTTYPE)
            else:  # poziv funkcije ili varijable,
                mozda = self.mozda_poziv(name, T.LISTTYPE)
                # ovi slucajevi bi se potencijalno drugacije obradili u buducnosti pa je ovako ostavljeno za sad
                if name == self.imef:
                    self.listMem[self.imeListe] = []
                elif name in self.funkcije:
                    self.listMem[self.imeListe] = []
                elif name in self.symtab:
                    self.listMem[self.imeListe] = []
                return mozda
        else:
            self >> T.OLIST
            if self >= T.CLIST: return Lista([])
            argumenti = [self.listArgument(tipoviArgumenata)]
            while self >= T.COMMA: argumenti.append(self.listArgument(tipoviArgumenata))
            self >> T.CLIST
            if hasattr(self, "imeListe"): self.listMem[self.imeListe] = tipoviArgumenata
            return Lista(argumenti)

    def listArgument(self, tipoviArgumenata):
        if self > T.OLIST:
            tipoviArgumenata.append(T.LISTTYPE)
            return self.list()
        elif num := self >= T.NUM:
            tipoviArgumenata.append(T.NUMBERTYPE)
            return num
        elif bool := self >= {T.TRUE, T.FALSE, T.UNKNOWN}:
            tipoviArgumenata.append(T.BOOLTYPE)
            return bool
        else:
            return self.mozda_poziv(self >> T.NAME, None)

    start = program
    lexer = lexer


def izvrsi(funkcije, *argumenti, **globalni):
    print('Main je vratio:', funkcije['main'].pozovi(argumenti, Memorija(globalni)))


### AST
# Funkcija: povratniTip:tip ime:IME parametri:[(tip:TYPE ime:IME)] blok:Blok #parametri je lista tuplova
# naredba: Grananje: ifUvjet:[log] ifNaredbe:Blok elifUvjeti:[[log]] elifBlokovi:[Blok] elseNaredbe:Blok
#          ForPetlja: i:varijabla stopUsporedba:znakUsporedbe stopVar:aritm inkrement:aritm blok:Blok
#          WhilePetlja: logUvjet:log blok:Blok
#          Pridruzivanje: tip:tip ime:NAME pridruzeno:izraz
#          Azuriranje: ime:NAME izraz:izraz
#          Pretvorba: kojiTip:tip varName:NAME uTip:TYPE
#          Vrati: tip:tip sto:izraz
#          Pomak: pomak:NUM
#          RazinaBuke:
#          Okretaj: kut:NUM
#          Provjera: udaljenost:NUM
#          Dizanje:
#          Spustanje:
#          Popravljanje:
#          ProvjeraZida:
#          ProvjeraKutije:
#          ProvjeraKvara:
#          X:
#          Y:
# Blok: [naredba]
# elifUvijeti: [elifUvijet:[log]]
# elifBlokovi: [elifBlok:blok]
# Poziv: funkcija:Funkcija argumenti:[izraz]
# znakUsporedbe: LT | GT | LE | GE | NE | EE
# izraz: log: Usporedba: lijevo:aritm relacija:znakUsporedbe desno:aritm
#             Binarna: op:OR|AND lijevo:izraz desno:izraz
#             Unarna: op:NOT ispod:izraz
#        aritm: Binarna: op:PLUS|MINUS|TIMES|DIV|EXP lijevo:izraz desno:izraz
#               Unarna: op:MINUS ispod:izraz
#        list: Lista: argumenti:[izraz]
#              DodajListi: name:NAME var:NAME|Poziv kraj:True|False
#
#        mogu vracati (biti) log, aritm, ili list
#
#        Poziv: funkcija:Funkcija argumenti:[izraz]
#        PristupListi: name:NAME index:aritm tipoviUListi:[tip] ocekivaniTip:tip
#        Pretvorba: kojiTip:tip varName:NAME uTip:tip
# tip: NUMBERTYPE | BOOLTYPE | LISTTYPE
class Funkcija(AST('povratniTip ime parametri blok')):
    def pozovi(self, argumenti, globalni):
        parametriImena = []
        for tip, ime in self.parametri:
            parametriImena.append(ime)
        lokalni = Memorija(dict(zip(parametriImena, argumenti)))
        try:
            self.blok.izvrsi(lokalni, globalni, self)
        except Povratak as exc:
            return exc.preneseno
        else:
            raise GreškaIzvođenja(f'{self.ime} nije ništa vratila')


class Poziv(AST('funkcija argumenti')):
    def izvrsi(self, mem, globalni, unutar):
        pozvana = self.funkcija
        if pozvana is nenavedeno: pozvana = unutar  # rekurzivni poziv
        argumenti = [a.izvrsi(mem, globalni, unutar) for a in self.argumenti]
        return pozvana.pozovi(argumenti, globalni)

    def _asdict(self):  # samo za ispis, da se ne ispiše čitava funkcija
        za_ispis = {'argumenti': self.argumenti}
        if self.funkcija is nenavedeno:
            za_ispis['*rekurzivni'] = True
        else:
            za_ispis['*ime'] = self.funkcija.ime
        return za_ispis


class Binarna(AST('op lijevo desno')):
    def izvrsi(self, mem, globalni, unutar):
        o, x, y = self.op, self.lijevo.izvrsi(mem, globalni, unutar), self.desno.izvrsi(mem, globalni, unutar)
        try:
            if o ^ T.PLUS:
                return x + y
            elif o ^ T.MINUS:
                return x - y
            elif o ^ T.TIMES:
                return x * y
            elif o ^ T.DIV:
                if (y != 0):
                    return (int)(x / y)  # dodano tako da nigdje ne baratamo s decimalnima
                else:
                    raise GreškaIzvođenja("Dijeljenje s nulom")
            elif o ^ T.EXP:
                return x ** y
            elif o ^ T.OR:
                return max(x, y)
            elif o ^ T.AND:
                return min(x, y)
            else:
                assert False, f'nepokriveni slučaj binarnog operatora {o}'
        except ArithmeticError as ex:
            raise o.iznimka(ex)


class Unarna(AST('op ispod')):
    def izvrsi(self, mem, globalni, unutar):
        o, x = self.op, self.ispod.izvrsi(mem, globalni, unutar)
        if o ^ T.MINUS:
            return -x
        elif o ^ T.NOT:
            return -x
        else:
            assert False, f'nepokriveni slučaj unarnog operatora {o}'


class Grananje(
    AST("ifUvjet ifNaredbe elifUvjeti elifBlokovi elseNaredbe")):  # Grananje(AST('istinitost uvjet onda inače')):
    def izvrsi(self, mem, globalni, unutar):
        if self.ifUvjet.izvrsi(mem, globalni, unutar) == 1:
            self.ifNaredbe.izvrsi(mem, globalni, unutar)
            return
        for uvjet, naredba in zip(self.elifUvjeti, self.elifBlokovi):
            if uvjet and uvjet.izvrsi(mem, globalni, unutar) == 1:
                naredba.izvrsi(mem, globalni, unutar)
                return

        self.elseNaredbe.izvrsi(mem, globalni, unutar)


class WhilePetlja(AST("logUvjet blok")):
    def izvrsi(self, mem, globalni, unutar):
        while self.logUvjet.izvrsi(mem, globalni, unutar) == 1:
            try:
                self.blok.izvrsi(mem, globalni, unutar)
            except Prekid:
                break


class ForPetlja(AST("i stopUsporedba stopVar inkrement blok")):
    def izvrsi(self, mem, globalni, unutar):
        i = self.i.izvrsi(mem, globalni, unutar)
        stopVar = self.stopVar.izvrsi(mem, globalni, unutar)
        while (usporedi(i, self.stopUsporedba, stopVar)):
            try:
                self.blok.izvrsi(mem, globalni, unutar)
            except Prekid:
                break
            if isinstance(self.inkrement, int):
                i += self.inkrement
            else:
                i += self.inkrement.izvrsi(mem, globalni, unutar)
            mem[self.i] = i


class Pomak(AST('pomak')):
    def izvrsi(self, mem, globalni, unutar):
        pomak = self.pomak.izvrsi(mem, globalni, unutar)
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        n = 0
        jeli_ikona = True if mapa[pozx][pozy] in ikone else False
        mapa[pozx][pozy] = '*'
        if smjer in {'gore', 'dolje'}:
            while mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == '*' and n < pomak:
                pozx += (smjerovi.index(smjer)) - 1
                n += 1
            mapa[pozx][pozy] = (ikone[smjerovi.index(smjer)] if jeli_ikona else kutije[smjerovi.index(smjer)])
        elif smjer in {'lijevo', 'desno'}:
            while mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == '*' and n < pomak:
                pozy += (smjerovi.index(smjer)) - 2
                n += 1
            mapa[pozx][pozy] = (ikone[smjerovi.index(smjer)] if jeli_ikona else kutije[smjerovi.index(smjer)])
        else:
            raise GreškaIzvođenja(f'Smjer {smjer} nije valjan.')
        nacrtajMapu(mapa, f"Pomakni se za {pomak} ({n} odrađeno)")
        updateGlobalni(globalni, pozx, pozy, smjer, mapa)
        return n


class RazinaBuke(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        razina = 100
        euklid_min = 1000
        for i in range(max(0, pozx - math.floor((senzor + 1) / 2)),
                       min(pozx + math.floor((senzor + 1) / 2), len(mapa) - 2) + 1, 1):
            for j in range(max(0, pozy - math.floor((senzor + 1) / 2)),
                           min(pozy + math.floor((senzor + 1) / 2), len(mapa[0]) - 2) + 1, 1):
                if (mapa[i][j] == zvuk):
                    euklid = math.sqrt((pozx - i) ** 2 + (pozy - j) ** 2)
                    if (euklid < euklid_min):
                        euklid_min = euklid
        return ((int)(razina / euklid_min))


class X(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        return pozx


class Y(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        return pozy


class Okretaj(AST('kut')):
    def izvrsi(self, mem, globalni, unutar):
        kut = self.kut.izvrsi(mem, globalni, unutar)
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)

        if (kut % 90):
            raise GreškaIzvođenja(f'Kut {kut} nije višekratnik 90.')

        kut = kut // 90
        smjer = smjerovi[(smjerovi.index(smjer) + kut) % 4]
        if mapa[pozx][pozy] in ikone:
            mapa[pozx][pozy] = ikone[smjerovi.index(smjer)]
        elif mapa[pozx][pozy] in kutije:
            mapa[pozx][pozy] = kutije[smjerovi.index(smjer)]

        nacrtajMapu(mapa, f"Okreni se za {kut * 90} stupnjeva (uspješno)")
        updateGlobalni(globalni, pozx, pozy, smjer, mapa)
        return 1
        # ovde ne triba nista vracat jer ce past na GreskaIzvodenja ako nije visekratnik 90 svakako


class Provjera(AST('udaljenost')):
    def izvrsi(self, mem, globalni, unutar):
        udaljenost = self.udaljenost.izvrsi(mem, globalni, unutar)
        if udaljenost == 0:
            return 1
        elif udaljenost < 0:
            raise GreškaIzvođenja(f"Udaljenost ne može biti negativan broj")
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)

        # ovdje sam globalno (varijabla senzor) definirao udaljenost do koje njegovi senzori rade
        x, y, n = pozx, pozy, 0
        if smjer in {'gore', 'dolje'}:
            while mapa[x + (smjerovi.index(smjer)) - 1][y] == '*' and n < udaljenost:
                x += (smjerovi.index(smjer)) - 1
                if (n == senzor):
                    return 0
                n += 1
        elif smjer in {'lijevo', 'desno'}:
            while mapa[x][y + (smjerovi.index(smjer)) - 2] == '*' and n < udaljenost:
                y += (smjerovi.index(smjer)) - 2
                if (n == senzor):
                    return 0
                n += 1
        else:
            # ovo je stavljeno zbog nas, korisnik ne moze pristupati globalnoj varijabli smjer
            raise GreškaIzvođenja(f'Smjer {smjer} nije valjan')
        if n >= udaljenost:
            return 1
        if n == senzor:
            return 0
        else:
            return -1


class Dizanje(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        flag = -1

        if smjer in {'gore', 'dolje'}:
            if mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == kutija and mapa[pozx][pozy] not in kutije:
                mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] = '*'
                mapa[pozx][pozy] = kutije[smjerovi.index(smjer)]
                flag = 1
        elif smjer in {'lijevo', 'desno'}:
            if mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == kutija and mapa[pozx][pozy] not in kutije:
                mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] = '*'
                mapa[pozx][pozy] = kutije[smjerovi.index(smjer)]
                flag = 1
        por = "Podigni kutiju"
        if flag == 1:
            por += " (uspješno)"
        else:
            por += " (neuspješno)"
        nacrtajMapu(mapa, por)
        updateGlobalni(globalni, pozx, pozy, smjer, mapa)
        return flag


class Spustanje(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        flag = -1

        if smjer in {'gore', 'dolje'}:
            if mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == '*' and mapa[pozx][pozy] in kutije:
                mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] = kutija
                mapa[pozx][pozy] = ikone[smjerovi.index(smjer)]
                flag = 1
        elif smjer in {'lijevo', 'desno'}:
            if mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == '*' and mapa[pozx][pozy] in kutije:
                mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] = kutija
                mapa[pozx][pozy] = ikone[smjerovi.index(smjer)]
                flag = 1

        por = "Spusti kutiju"
        if flag == 1:
            por += " (uspješno)"
        else:
            por += " (neuspješno)"
        nacrtajMapu(mapa, por)
        updateGlobalni(globalni, pozx, pozy, smjer, mapa)
        return flag


class Popravljanje(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        flag = -1

        if smjer in {'gore', 'dolje'}:
            if mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == kvar and mapa[pozx][pozy] in ikone:
                mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] = '*'
                flag = 1
        elif smjer in {'lijevo', 'desno'}:
            if mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == kvar and mapa[pozx][pozy] in ikone:
                mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] = '*'
                flag = 1

        por = "Popravi kvar"
        if flag == 1:
            por += " (uspješno)"
        else:
            por += " (neuspješno)"
        nacrtajMapu(mapa, por)
        updateGlobalni(globalni, pozx, pozy, smjer, mapa)
        return flag


class ProvjeraZida(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        flag = -1

        if smjer in {'gore', 'dolje'}:
            if mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == '#':
                flag = 1
        elif smjer in {'lijevo', 'desno'}:
            if mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == '#':
                flag = 1

        por = "Provjera zida"
        if flag == 1:
            por += " (jest zid)"
        else:
            por += " (nije zid)"
        nacrtajMapu(mapa, por)
        return flag


class ProvjeraKutije(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        flag = -1

        if smjer in {'gore', 'dolje'}:
            if mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == kutija:
                flag = 1
        elif smjer in {'lijevo', 'desno'}:
            if mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == kutija:
                flag = 1

        por = "Provjera kutije"
        if flag == 1:
            por += " (jest kutija)"
        else:
            por += " (nije kutija)"
        nacrtajMapu(mapa, por)
        return flag


class ProvjeraKvara(AST('')):
    def izvrsi(self, mem, globalni, unutar):
        [pozx, pozy, smjer, mapa, smjerovi, senzor, kutija, kutije, ikone, kvar, zvuk] = raspakirajGlobalni(globalni)
        flag = -1

        if smjer in {'gore', 'dolje'}:
            if mapa[pozx + (smjerovi.index(smjer)) - 1][pozy] == kvar:
                flag = 1
        elif smjer in {'lijevo', 'desno'}:
            if mapa[pozx][pozy + (smjerovi.index(smjer)) - 2] == kvar:
                flag = 1

        por = "Provjera kvara"
        if flag == 1:
            por += " (jest kvar)"
        else:
            por += " (nije kvar)"
        nacrtajMapu(mapa, por)
        return flag


def usporedi(l, relacija, d):
    if relacija ^ T.EE:
        return l == d
    elif relacija ^ T.NE:
        return l != d
    elif relacija ^ T.LT:
        return l < d
    elif relacija ^ T.LE:
        return l <= d
    elif relacija ^ T.GT:
        return l > d
    elif relacija ^ T.GE:
        return l >= d
    else:
        assert False, f'Nepoznata relacija {relacija}'


class Usporedba(AST('lijevo relacija desno')):
    def izvrsi(self, mem, globalni, unutar):
        l = self.lijevo.izvrsi(mem, globalni, unutar)
        d = self.desno.izvrsi(mem, globalni, unutar)
        return usporedi(l, self.relacija, d)


class Pridruzivanje(AST('ime pridruzeno')):
    def izvrsi(self, mem, globalni, unutar):
        p = self.pridruzeno.izvrsi(mem, globalni, unutar)
        mem[self.ime] = p


class Azuriranje(AST("ime izraz")):
    def izvrsi(self, mem, globalni, unutar):
        mem[self.ime] = self.izraz.izvrsi(mem, globalni, unutar)


class Lista(AST("argumenti")):
    def izvrsi(self, mem, globalni, unutar):
        lista = []
        for arg in self.argumenti:
            if (isinstance(arg, ListaAST)): arg = Lista(arg)
            lista.append(arg.izvrsi(mem, globalni, unutar))
        return lista


class DodajListi(AST("name var kraj")):
    def izvrsi(self, mem, globalni, unutar):
        lista = mem[self.name]
        var = self.var.izvrsi(mem, globalni, unutar)
        if self.kraj:
            lista.append(var)
        else:
            lista.insert(0, var)
        return lista


class PristupListi(AST("name index tipoviUListi ocekivaniTip")):
    def izvrsi(self, mem, globalni, unutar):
        index = self.index.izvrsi(mem, globalni, unutar)
        lista = mem[self.name]
        tipovi = self.tipoviUListi
        if isinstance(tipovi, list) and not tipovi[index] ^ self.ocekivaniTip.tip:
            raise GreškaIzvođenja(f"Tip varijable na mjestu {index} liste {self.name.sadržaj} nije {self.ocekivaniTip}")
        if index > len(lista) or index < 0: raise GreškaIzvođenja(
            "Index liste mora biti pozitivan broj manji od duljine liste")
        return lista[index]


class Pretvorba(AST("kojiTip varName uTip")):
    def izvrsi(self, mem, globalni, unutar):
        kojiTip = self.kojiTip
        varName = self.varName
        uTip = self.uTip
        vrijednost = varName.izvrsi(mem, globalni, unutar)
        if kojiTip ^ T.LISTTYPE:
            if uTip ^ T.LISTTYPE:
                return vrijednost
            if uTip ^ T.NUMBERTYPE:
                return len(vrijednost)
            if uTip ^ T.BOOLTYPE:
                return len(vrijednost) != 0
        if kojiTip ^ T.NUMBERTYPE:
            if uTip ^ T.NUMBERTYPE:
                return vrijednost
            if uTip ^ T.BOOLTYPE:
                if vrijednost in {-1, 0}:
                    return vrijednost
                else:
                    return 1
            if uTip ^ T.LISTTYPE:
                return [vrijednost]
        if kojiTip ^ T.BOOLTYPE:
            if uTip ^ T.BOOLTYPE:
                return vrijednost
            if uTip ^ T.NUMBERTYPE:
                return vrijednost
            if uTip ^ T.LISTTYPE:
                return [vrijednost]


class Blok(AST('naredbe')):
    def izvrsi(self, mem, globalni, unutar):
        for naredba in self.naredbe: naredba.izvrsi(mem, globalni, unutar)


class Vrati(AST("sto")):
    def izvrsi(self, mem, globalni, unutar):
        sto = self.sto.izvrsi(mem, globalni, unutar)
        raise Povratak(sto)


class Povratak(NelokalnaKontrolaToka): """Signal koji šalje naredba vrati."""


proba = P('''
    def List f(Number a, Number b){
            List c = [a];
            add(c, b);
            return c;
        }
        
    def Number main () { 
        List x = f(2, 2);
        Number d = 3;
        append(x, d);
        Number c = x[0] + x[1] + x[2];
        return c;
    }
''')

prikaz(proba)

izvrsi(proba, mapa=mapa, smjerovi=smjerovi, ikone=ikone, kutije=kutije,
       kutija=kutija, smjer=smjer, pozx=pozx, pozy=pozy, senzor=senzor, kvar=kvar, zvuk=zvuk)