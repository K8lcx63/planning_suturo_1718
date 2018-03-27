Welcome to the planning_suturo_1718 wiki!

### Verzeichnis
* **common** in Common befinden sich Pakete die nicht von Planning_Suturo geschrieben wurde, aber benutzt werden.
* **planning_files** hier befinden sich einige Files die eventuell gebraucht werden.
* **planning_programm** hier befinden sich die Pakete für das Planning_Suturo Programm.

### Installation

* Hat man erfolgreich mit catkin build seine Arbeitsumgebung bauen können, muss man CRAM installieren [klick mich](http://cram-system.org/installation).

* Nun wird die Entwicklungsumgebung installiert mit: sudo apt-get install emacs. Achtung: Bei der Verwendung von Ubuntu 14.04 muss der neueste Lisp 'compiler' noch hinzugefügt werden [klick mich](https://sourceforge.net/projects/sbcl/files/sbcl/1.3.1/) (meistens x86-64 Version). Diese Datei entpacken und im Terminal (im richtigen Ordner) den Befehl: sh install.sh eingeben.


### Launchen

* Zuerst muss im Terminal roslisp\_repl eingeeben werden, daraufhin startet emacs. 

* Nun wird in der Repl (dort wo cl-user steht) ein , (also das Komma auf der Tastertur) eingegeben.

* Darauf folgt r-l-s mit enter und nun muss planning_main_programm mit enter bestätigt werden.

* Zum Schluss wird noch in dem cl-user Fenster (planning-main-programm::main) eingegeben und mit enter bestätigt die Klammern gehören auch dazu.



