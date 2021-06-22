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
        def vrijednost(self, mem, unutar): return mem[self]

        def typeCheck(self, mem): return mem[self].tip

    class NUM(Token):
        def vrijednost(self, mem, unutar): return int(self.sadržaj)

        def typeCheck(self, mem): return T.NUMBERTYPE

    class BREAK(Token):
        literal = 'break'

        def run(self, mem, unutar): raise Break

    class TRUE(Token):
        literal = 'True'

        def vrijednost(self, mem, unutar): return 1

        def typeCheck(self, mem): return T.BOOLTYPE

    class FALSE(Token):
        literal = 'False'

        def vrijednost(self, mem, unutar): return -1

        def typeCheck(self, mem): return T.BOOLTYPE

    class UNKNOWN(Token):
        literal = 'Unknown'

        def vrijednost(self, mem, unutar): return 0

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

        blok = self.blok()
        return Funkcija(self.tipf, self.imef, self.parametrif, blok, self.symtab)

    def tipIme(self):
        return self >> {T.NUMBERTYPE, T.BOOLTYPE, T.LISTTYPE}, self >> T.NAME

    def parametri(self):
        self >> T.OPEN
        if self >= T.CLOSE: return []
        tip, ime = self.tipIme()
        param = [(tip, ime)]
        # print(tip, ime)
        self.symtab[ime] = tip
        # print(self.symtab[ime])
        while self >= T.COMMA:
            tip, ime = self.tipIme()
            self.symtab[ime] = tip
            param.append((tip, ime))
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
            # return Pridruzivanje(tipVarijable, ime, self.tipa(tipVarijable))
            # TODO: mislim da ne treba proslijedivat tip varijabli
            return Pridruzivanje(ime, self.tipa(tipVarijable))
        else:  # azuriranje
            ime = self >> T.NAME
            if self >= T.EQ:
                if ime not in self.symtab: raise SemantičkaGreška(
                    "Varijabla se ne može ažurirati prije inicijalizacije")
                tipVarijable = self.symtab[ime]
                # return Azuriranje(tipVarijable.tip, ime, self.tipa(tipVarijable))
                # TODO: mislim da ne treba proslijedivat tip varijabli
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

    def grananje(self):
        elifNaredbe = elseNaredbe = Blok([])
        elifUvjet = False
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
        blok = self.blok()
        return WhilePetlja(logUvjet, blok)

    def forPetlja(self):
        self >> T.FOR
        self >> T.OPEN
        i = self >> T.NAME
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise SintaksnaGreška("Ime varijable u for petlji mora biti isto")
        stopUsporedba = self >> operatoriUsporedbe
        stopVar = self.aritm()
        self >> T.SEMICOL
        if not self >> T.NAME == i: raise SintaksnaGreška("Ime varijable u for petlji mora biti isto")
        if self >= T.PP:
            inkrement = 1
        else:
            self >> T.PE
            inkrement = self >> T.NUM
        self >> T.CLOSE
        blok = self.blok()
        return ForPetlja(i, stopUsporedba, stopVar, inkrement, blok)

    def blok(self):
        self >> T.OPENBR
        if self >= T.CLOSEBR: return Blok([])
        n = [self.naredba()]
        while self >= T.SEMICOL and not self > T.CLOSEBR: n.append(self.naredba())
        self >> T.CLOSEBR
        return Blok.ili_samo(n)

    def logStart(self):
        t = self.disjunkcija()
        while op := self >= T.OR: t = Binarna(op, t, self.disjunkcija())
        return t

    def disjunkcija(self):
        t = self.konjunkcija()
        while op := self >= T.AND: t = Binarna(op, t, self.konjunkcija())
        return t

    def konjunkcija(self):
        while self >= T.NOT:
            return Unarna(T.NOT, self.konjunkcija())
        return self.logIzraz()

    def pretvorba(self, uTip):
        self >> T.CLOSE
        if self > T.OLIST:
            varijabla = self.list()
            tip = T.LISTTYPE
        elif varijabla := self >= T.NUM:
            tip = T.NUMBERTYPE
        elif varijabla := self >= {T.TRUE, T.UNKNOWN, T.FALSE}:
            tip = T.BOOLTYPE
        else:
            ime = self >> T.NAME
            tip = self.symtab[ime] if ime in self.symtab else self.funkcije[ime].tip
            varijabla = self.mozda_poziv(ime, tip, True)
        return Pretvorba(tip, varijabla, uTip)

    def logIzraz(self):
        if log := self >= {T.TRUE, T.FALSE, T.UNKNOWN}:
            return log
        if self > T.NAME:
            name = self >= T.NAME
            if (name in self.symtab and self.symtab[name].tip == T.BOOLTYPE) or (
                    name in self.funkcije and self.funkcije[name].tip == T.BOOLTYPE) or (
                    name == self.imef and self.tipf == T.BOOLTYPE):
                return self.mozda_poziv(name, T.BOOLTYPE)
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

    # TODO: mozda da vracamo fale umjesto gresaka, pa da se greske handlaju izvan ove fje
    def mozda_poziv(self, ime, ocekivaniTip, force=False):
        if ime in self.funkcije:
            funkcija = self.funkcije[ime]
            if funkcija.povratniTip != Token(ocekivaniTip): raise SemantičkaGreška(
                f"Povratni tip funkcije {ime} nije očekivanog tipa {ocekivaniTip}")
            return Poziv(funkcija, self.argumenti(funkcija.parametri))
        elif ime == self.imef:
            return Poziv(nenavedeno, self.argumenti(self.parametrif))
        # elif ime in self.symtab:
        #     if self.symtab[ime].tip != ocekivaniTip: raise SemantičkaGreška(
        #         f"Tip varijable {ime} nije očekivanog tipa {ocekivaniTip}")
        #     return self.symtab[ime]
        elif force:
            raise SintaksnaGreška("Kriva eksplicitna pretvorba")
        else:
            return ime

    def argumenti(self, parametri):
        arg = []
        self >> T.OPEN
        # print("AAAAAA")
        # for i, parametar in self.symtab:
            # print("symtab:")
            # print (i, parametar)
        for i, parametar in enumerate(parametri):
            if i: self >> T.COMMA
            # print(i, parametar[1])
            # print(i, parametar[1] in self.symtab)
            arg.append(self.tipa(self.symtab[parametar[1]]))
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
        #TODO: ovo mozda ne radi jer pise NUMBERTYPE umijesto NUM, al mislim da bi zapravo ovako trebalo biti
        return self.mozda_poziv(self >> T.NAME, T.NUMBERTYPE)

    # List mojaLista = [[1,2,3],[[true], 1, 2], 3]
    def list(self):
        if self >= T.OPEN:
            if self >> T.LISTTYPE: return self.pretvorba(T.LISTTYPE)
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
    print('Main je vratio:', funkcije['main'].pozovi(argv))


def numBoolToToken(num):
    if num == -1:
        return T.FALSE
    elif num == 0:
        return T.UNKNOWN
    elif num == 1:
        return T.TRUE
    else:
        assert False, f"{num} nije vrijednost nikoje instance Bool tipa"


# TODO: Dodati AST za eksplicitnu pretvorbu

### AST
# Program: funkcije[funkcija] #(za sad nema ovog: naredbe:[naredba])
# Funkcija: tip:TYPE ime:IME parametri:[[tip:TYPE ime:IME]] naredbe:[naredba]
# naredba: Grananje: istinitost:JE|NIJE uvjet:log onda:naredba inače:naredba
#          ForPetlja: varijabla:"" stopUvjet:log inkrement:NUM naredbe:[naredba]
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

#TODO: Parametri se salje kao tuple tako da kod prikaza ASTa imamo tip i ime varijable
class Funkcija(AST('povratniTip ime parametri blok symtab')):
    def pozovi(self, argumenti):
        parametriImena = []
        for tip, ime in self.parametri:
            parametriImena.append(ime)
        symtab = Memorija(dict(self.symtab))
        lokalni = Memorija(dict(zip(parametriImena, argumenti)))
        try:
            self.blok.izvrsi(symtab, lokalni, self)
        except Povratak as exc:
            return exc.preneseno
        else:
            raise GreškaIzvođenja(f'{self.ime} nije ništa vratila')


class Poziv(AST('funkcija argumenti')):
    def vrijednost(self, mem, unutar):
        pozvana = self.funkcija
        if pozvana is nenavedeno: pozvana = unutar  # rekurzivni poziv
        argumenti = [a.vrijednost(mem, unutar) for a in self.argumenti]
        return pozvana.pozovi(argumenti)

    def _asdict(self):  # samo za ispis, da se ne ispiše čitava funkcija
        za_ispis = {'argumenti': self.argumenti}
        if self.funkcija is nenavedeno:
            za_ispis['*rekurzivni'] = True
        else:
            za_ispis['*ime'] = self.funkcija.ime
        return za_ispis


class Binarna(AST('op lijevo desno')):
    def vrijednost(self, mem, unutar):
        o, x, y = self.op, self.lijevo.vrijednost(mem, unutar), self.desno.vrijednost(mem, unutar)
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
    def vrijednost(self, mem, unutar):
        o, x = self.op, self.ispod.vrijednost(self, mem, unutar)
        if o ^ T.MINUS:
            return -x
        elif o ^ T.NOT:
            return numBoolToToken(-x)
        else:
            assert False, f'nepokriveni slučaj unarnog operatora {o}'


def ispunjen(ast, mem, unutar):
    trazeno = [ast.ifUvjet.tip]
    return ast.uvjet.vrijednost(mem, unutar) == trazeno


class Grananje(
    AST("ifUvjet ifNaredbe elifUvjet elifNaredbe elseNaredbe")):  # Grananje(AST('istinitost uvjet onda inače')):
    def izvrsi(self, symtab, mem, unutar):
        if self.ifUvjet.vrijednost(mem, unutar) == 1:
            self.ifNaredbe.izvrsi(symtab, mem, unutar)
        elif self.elifUvjet and self.elifUvjet.vrijednost(mem, unutar) == 1:
            self.elifNaredbe.izvrsi(symtab, mem, unutar)
        else:
            self.elseNaredbe.izvrsi(symtab, mem, unutar)


class WhilePetlja(AST("logUvjet blok")):
    def izvrsi(self, symtab, mem, unutar):
        while self.logUvjet.vrijednost(self, mem, unutar): self.blok.izvrsi(self, symtab, mem, unutar)


class ForPetlja(AST("i stopUsporedba stopVar inkrement blok")):
    def izvrsi(self, symtab, mem, unutar):
        i = self.i.vrijednost(mem, unutar)
        stopVar = self.stopVar.vrijednost(mem, unutar)
        while (usporedi(i, self.stopUsporedba, stopVar)):
            self.blok.izvrsi(self, symtab, mem, unutar)
            i += self.inkrement
            mem[i] = i


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
    def vrijednost(self, mem, unutar):
        l = self.lijevo.vrijednost(mem, unutar)
        d = self.desno.vrijednost(mem, unutar)
        usporedi(l, self.relacija, d)


class Pridruzivanje(AST('ime pridruzeno')):
    def izvrsi(self, symtab, mem, unutar):
        # TODO: Mislim da svu provjeru tipova vec imamo obavljenu u parseru

        # lijevo = symtab[self.varijabla]
        # desno = self.vrijednost.provjeri_tip(symtab)
        # if not desno <= lijevo:
        #     raise self.varijabla.krivi_tip(lijevo, desno)
        mem[self.ime] = self.pridruzeno.vrijednost(mem, unutar)


class Azuriranje(AST("ime izraz")):
    def izvrsi(self, symtab, mem, unutar):
        mem[self.ime] = self.pridruzeno.vrijednost(mem, unutar)


class Lista(AST("argumenti")):
    def vrijednost(self, mem, unutar):
        return [arg.vrijednost(mem, unutar) for arg in self.argumenti]


class Pretvorba(AST("kojiTip var uTip")):
    def vrijednost(self, mem, unutar):
        kojiTip = self.kojiTip
        var = self.var.vrijednost(mem, unutar)
        uTip = self.uTip
        vrijednost = var.vrijednost()
        if kojiTip == T.LISTTYPE:
            if uTip == T.LISTTYPE:
                return vrijednost
            if uTip == T.NUMBERTYPE:
                return len(vrijednost)
            if uTip == T.BOOLTYPE:
                return len(vrijednost) != 0
        if kojiTip == T.NUMBERTYPE:
            if uTip == T.NUMBERTYPE:
                return vrijednost
            if uTip == T.BOOLTYPE:
                return vrijednost != 0
            if uTip == T.LISTTYPE:
                return [vrijednost]
        if kojiTip == T.BOOLTYPE:
            if uTip == T.BOOLTYPE:
                return vrijednost
            if uTip == T.NUMBERTYPE:
                return vrijednost
            if uTip == T.LISTTYPE:
                return [vrijednost]


class Blok(AST('naredbe')):
    def izvrsi(self, symtab, mem, unutar):
        for naredba in self.naredbe: naredba.izvrsi(symtab, mem, unutar)


class Vrati(AST('sto')):
    def izvrsi(self, symtab, mem, unutar):
        raise Povratak(self.sto.vrijednost(mem, unutar))


class Povratak(NelokalnaKontrolaToka): """Signal koji šalje naredba vrati."""


proba = P('def Number main () {Number c = 1; if (c == 1){ return 1;} else {return 2;}}')
prikaz(proba, 5)
izvrsi(proba)

#TODO: u funkciji argumenti rijesiti da se mogu pozivati funkcije s proizvoljnim imenom

proba2 = P('''\
def Number zbroj (Number n, Number m) {return n+m;}
def Number main () {Number n = 1; Number m = 2; return zbroj(n, m);}
''')
prikaz(proba2, 5)
izvrsi(proba2)

#TODO: rekurzivni pozivi ne rade za sad

# rekurzivna = P('''\
#     def Number fakt(Number n) {if (n == 1){ return 1;} else {n = n-1; return (n+1)*fakt(n);}}
#     def Number main(){return fakt(1);}
# ''')
# prikaz(rekurzivna)
# izvrsi(rekurzivna)
