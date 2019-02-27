# Project for Advanced Programming
In dieser Praktikumsaufgabe soll unter Verwendung von Haskell als Programmiersprache ein Interpreter sowie eine interaktive Umgebung für eine einfache, funktionale Programmiersprache programmiert werden.


*  Es handelt sich um eine ungetypte Sprache. Es findet keine Typprüfung statt und deklarierte Datentypen und angegebene Typsignaturen werden lediglich als Kommentare interpretiert.  
*  Funktionen können zwar via Pattern Matching definiert werden, aber das Pattern Matching...  
    * ist auf höchstens ein Argument pro Regel beschränkt. Weiterhin gilt, dass alle Regeln einer Funktion via Pattern Matching im gleichen Argument definiert sein müssen.  
    *  darf nicht verschachtelt vorkommen.
*  Ausdrücke sind ausschließlich aus Variablen oder Funktions- bzw. Konstruktorapplikationen zusammengesetzt. Es gibt also keine if-then-else-Ausdrücke oder ähnliches.  
*  Es gibt keine Unterstützung für die folgenden, aus Haskell bekannten Sprachfeatures:  
    *  Higher-Order-Funktionen  
    *  Infixkonstruktoren oder -funktionen  
    *  Typ- und Typkonstruktorklassen  
    *  Im- und Export von Modulen  
    *  lokale Deklarationen  