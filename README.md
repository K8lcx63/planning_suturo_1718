suturo-planning

### Installation

- Hat man erfolgreich mit \textit{catkin build} seine Arbeitsumgebung vollständig bauen können, muss man CRAM installieren [klick mich](http://cram-system.org/installation).

- Nun wird die Entwicklungsumgebung installiert mit: sudo apt-get install emacs. Achtung: Bei der Verwendung von Ubuntu 14.04 muss der neueste Lisp 'compiler' noch hinzugefügt werden [klick mich](https://sourceforge.net/projects/sbcl/files/sbcl/1.3.1/) (meistens x86-64 Version). Diese Datei entpacken und im Terminal (im richtigen Ordner) den Befehl: sh install.sh eingeben.


### Launchen

- Zuerst muss im Terminal roslisp\_repl eingeeben werden, daraufhin startet emacs. 

-  Nun wird in der Repl (dort wo cl-user steht) ein , (also das Komma auf der Tastertur) eingegeben.

-  Darauf folgt s-b-l mit enter und nun muss planning_main_programm mit enter bestätigt werden.

-  Zum Schluss wird noch in dem cl-user Fenster (planning-main-programm::main) eingegeben und mit enter bestätigt die Klammern gehören auch dazu.

### Verzeichniss


Planning_main_programm:
- main ()

Planning_vision:
- call-vision-point()
- call-vision-pose()
- check-points-is-equal(msg-one msg-two delta)
- askFor()

Planning_knowledge:
- ask-knowledge(point-center-of-object)

Planning_motion:
- call-Motion-Move-Arm()
- call-Motion-Move-To-Point(point-center-of-object)

Planning_move:
- move-Base-To-Point(x y z w)
- find-Object (x z)


