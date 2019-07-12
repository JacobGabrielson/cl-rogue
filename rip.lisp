;;;; File for the fun ends
;;;; Death or a total win
;;;; @(#)rip.c	3.13 (Berkeley) 6/16/81

(in-package :cl-rogue)

(defparameter *rip*
  '("                          ----------"
    "                          /          \\"
    "                         /    REST    \\"
    "                        /      IN      \\"
    "                       /     PEACE      \\"
    "                      /                  \\"
    "                      |                  |"
    "                      |                  |"
    "                      |   killed by a    |"
    "                      |                  |"
    "                      |       1980       |"
    "                     *|     *  *  *      | *"
    "             --------)/\\\\-//(\\/(/\\)/\\//\\/|-)-------"))

(defun killname (monst)
  (if (upper-case-p monst)
      (char-monster-name monst)
      (case monst
        (#\a "arrow")
        (#\d "dart")
        (#\b "bolt")
        (otherwise ""))))

(defun calc-x-position (string)
  (- 28 
     (truncate (/ (1+ (length string))
                  2))))

(defun death (monst)
  "Do something really fun when he dies."
  (cl-charms/low-level:clear)
  (cl-charms/low-level:move 8 0)
  (dolist (dp *rip*)
    (cl-charms/low-level:printw (format nil "~a~%" dp)))
  (cl-charms/low-level:mvaddstr 14 
                        (calc-x-position whoami)
                        whoami)
  (decf purse (truncate (/ purse 10)))
  (let ((buf (format nil "~d Au" purse)))
    (cl-charms/low-level:mvaddstr 15
                          (calc-x-position buf)
                          buf)
    (let ((killer (killname monst)))
      (cl-charms/low-level:mvaddstr 17
                            (calc-x-position killer)
                            killer)
      (cl-charms/low-level:mvaddstr 16 33 (vowelstr killer))
      (cl-charms/low-level:mvaddstr 
       18 26
       (format nil "~4d" (nth-value 5 (decode-universal-time (get-universal-time))))) ; year
      (cl-charms/low-level:move (1- cl-charms/low-level:*lines*) 0)
      (draw cl-charms/low-level:*stdscr*)
      (score purse 0 monst)
      (rogue-done))))

(defun score (&optional amount flags monst)
  "Score -- figure score and post it."
  t)
;; TODO: finish porting
;; char monst;
;; {
;;     static struct sc-ent {
;;         int sc-score;
;;         char sc-name[80];
;;         int sc-flags;
;;         int sc-level;
;;         int sc-uid;
;;         char sc-monster;
;;     } top-ten[10];
;;     register struct sc-ent *scp;
;;     register int i;
;;     register struct sc-ent *sc2;
;;     register FILE *outf;
;;     register char *killer;
;;     register int prflags = 0;
;;     register int fd;
;;     static char *reason[] = {
;;         "killed",
;;         "quit",
;;         "A total winner",
;;     };

;;     if (flags != -1)
;;         endwin();
;;     
;;      * Open file and read list
;;      */

;;     if ((fd = open(SCOREFILE, O-RDWR | O-CREAT, 0666 )) < 0)
;;         return;
;;     outf = (FILE *) fdopen(fd, "w");

;;     for (scp = top-ten; scp <= &top-ten[9]; scp++)
;;     {
;;         scp->sc-score = 0;
;;         for (i = 0; i < 80; i++)
;;             scp->sc-name[i] = rnd(255);
;;         scp->sc-flags = RN;
;;         scp->sc-level = RN;
;;         scp->sc-monster = RN;
;;         scp->sc-uid = RN;
;;     }

;;     signal(SIGINT, SIG-DFL);
;;     if (flags != -1)
;;     {
;;         printf("[Press return to continue]");
;;         fflush(stdout);
;;         fgets(prbuf,80,stdin);
;;     }
;;     if (wizard)
;;         if (strcmp(prbuf, "names") == 0)
;;             prflags = 1;
;;         else if (strcmp(prbuf, "edit") == 0)
;;             prflags = 2;
;;     encread((char *) top-ten, sizeof top-ten, fd);
;;     
;;      * Insert her in list if need be
;;      */
;;     if (!waswizard)
;;     {
;;         for (scp = top-ten; scp <= &top-ten[9]; scp++)
;;             if (amount > scp->sc-score)
;;                 break;
;;         if (scp <= &top-ten[9])
;;         {
;;             for (sc2 = &top-ten[9]; sc2 > scp; sc2--)
;;                 *sc2 = *(sc2-1);
;;             scp->sc-score = amount;
;;             strcpy(scp->sc-name, whoami);
;;             scp->sc-flags = flags;
;;             if (flags == 2)
;;                 scp->sc-level = max-level;
;;             else
;;                 scp->sc-level = level;
;;             scp->sc-monster = monst;
;;             scp->sc-uid = getuid();
;;         }
;;     }
;;     
;;      * Print the list
;;      */
;;     printf("\nTop Ten Adventurers:\nRank\tScore\tName\n");
;;     for (scp = top-ten; scp <= &top-ten[9]; scp++) {
;;         if (scp->sc-score) {
;;             printf("%d\t%d\t%s: %s on level %d", scp - top-ten + 1,
;;                    scp->sc-score, scp->sc-name, reason[scp->sc-flags],
;;                    scp->sc-level);
;;             if (scp->sc-flags == 0) {
;;                 printf(" by a");
;;                 killer = killname(scp->sc-monster);
;;                 if (*killer == 'a' || *killer == 'e' || *killer == 'i' ||
;;                     *killer == 'o' || *killer == 'u')
;;                     putchar('n');
;;                 printf(" %s", killer);
;;             }
;;             if (prflags == 1)
;;             {
;;                 struct passwd *pp, *getpwuid();

;;                 if ((pp = getpwuid(scp->sc-uid)) == NULL)
;;                     printf(" (%d)", scp->sc-uid);
;;                 else
;;                     printf(" (%s)", pp->pw-name);
;;                 putchar('\n');
;;             }
;;             else if (prflags == 2)
;;             {
;;                 fflush(stdout);
;;                 fgets(prbuf,80,stdin);
;;                 if (prbuf[0] == 'd')
;;                 {
;;                     for (sc2 = scp; sc2 < &top-ten[9]; sc2++)
;;                         *sc2 = *(sc2 + 1);
;;                     top-ten[9].sc-score = 0;
;;                     for (i = 0; i < 80; i++)
;;                         top-ten[9].sc-name[i] = rnd(255);
;;                     top-ten[9].sc-flags = RN;
;;                     top-ten[9].sc-level = RN;
;;                     top-ten[9].sc-monster = RN;
;;                     scp--;
;;                 }
;;             }
;;             else
;;                 printf(".\n");
;;         }
;;     }
;;     fseek(outf, 0L, 0);
;;     
;;      * Update the list file
;;      */
;;     encwrite((char *) top-ten, sizeof top-ten, outf);
;;     fclose(outf);
;; }

(defun total-winner ()
  t)
;; TODO: finish porting
;; {
;;     register struct linked-list *item;
;;     register struct object *obj;
;;     register int worth;
;;     register char c;
;;     register int oldpurse;

;;     clear();
;;     standout();
;;     addstr("                                                               \n");
;;     addstr("  @   @               @   @           @          @@@  @     @  \n");
;;     addstr("  @   @               @@ @@           @           @   @     @  \n");
;;     addstr("  @   @  @@@  @   @   @ @ @  @@@   @@@@  @@@      @  @@@    @  \n");
;;     addstr("   @@@@ @   @ @   @   @   @     @ @   @ @   @     @   @     @  \n");
;;     addstr("      @ @   @ @   @   @   @  @@@@ @   @ @@@@@     @   @     @  \n");
;;     addstr("  @   @ @   @ @  @@   @   @ @   @ @   @ @         @   @  @     \n");
;;     addstr("   @@@   @@@   @@ @   @   @  @@@@  @@@@  @@@     @@@   @@   @  \n");
;;     addstr("                                                               \n");
;;     addstr("     Congratulations, you have made it to the light of day!    \n");
;;     standend();
;;     addstr("\nYou have joined the elite ranks of those who have escaped the\n");
;;     addstr("Dungeons of Doom alive.  You journey home and sell all your loot at\n");
;;     addstr("a great profit and are admitted to the fighters guild.\n");
;;     mvaddstr(LINES - 1, 0, "--Press space to continue--");
;;     refresh();
;;     wait-for(' ');
;;     clear();
;;     mvaddstr(0, 0, "   Worth  Item");
;;     oldpurse = purse;
;;     for (c = 'a', item = pack; item != NULL; c++, item = next(item))
;;     {
;;         obj = (struct object *) ldata(item);
;;         switch (obj->o-type)
;;         {
;;             case FOOD:
;;                 worth = 2 * obj->o-count;
;;             when WEAPON:
;;                 switch (obj->o-which)
;;                 {
;;                     case MACE: worth = 8;
;;                     when SWORD: worth = 15;
;;                     when BOW: worth = 75;
;;                     when ARROW: worth = 1;
;;                     when DAGGER: worth = 2;
;;                     when ROCK: worth = 1;
;;                     when TWOSWORD: worth = 30;
;;                     when SLING: worth = 1;
;;                     when DART: worth = 1;
;;                     when CROSSBOW: worth = 15;
;;                     when BOLT: worth = 1;
;;                     when SPEAR: worth = 2;
;;                     otherwise: worth = 0;
;;                 }
;;                 worth *= (1 + (10 * obj->o-hplus + 10 * obj->o-dplus));
;;                 worth *= obj->o-count;
;;                 obj->o-flags |= ISKNOW;
;;             when ARMOR:
;;                 switch (obj->o-which)
;;                 {
;;                     case LEATHER: worth = 5;
;;                     when RING-MAIL: worth = 30;
;;                     when STUDDED-LEATHER: worth = 15;
;;                     when SCALE-MAIL: worth = 3;
;;                     when CHAIN-MAIL: worth = 75;
;;                     when SPLINT-MAIL: worth = 80;
;;                     when BANDED-MAIL: worth = 90;
;;                     when PLATE-MAIL: worth = 400;
;;                     otherwise: worth = 0;
;;                 }
;;                 worth *= (1 + (10 * (a-class[obj->o-which] - obj->o-ac)));
;;                 obj->o-flags |= ISKNOW;
;;             when SCROLL:
;;                 s-know[obj->o-which] = TRUE;
;;                 worth = s-magic[obj->o-which].mi-worth;
;;                 worth *= obj->o-count;
;;             when POTION:
;;                 p-know[obj->o-which] = TRUE;
;;                 worth = p-magic[obj->o-which].mi-worth;
;;                 worth *= obj->o-count;
;;             when RING:
;;                 obj->o-flags |= ISKNOW;
;;                 r-know[obj->o-which] = TRUE;
;;                 worth = r-magic[obj->o-which].mi-worth;
;;                 if (obj->o-which == R-ADDSTR || obj->o-which == R-ADDDAM ||
;;                     obj->o-which == R-PROTECT || obj->o-which == R-ADDHIT)
;;                     if (obj->o-ac > 0)
;;                         worth += obj->o-ac * 20;
;;                     else
;;                         worth = 50;
;;             when STICK:
;;                 obj->o-flags |= ISKNOW;
;;                 ws-know[obj->o-which] = TRUE;
;;                 worth = ws-magic[obj->o-which].mi-worth;
;;                 worth += 20 * obj->o-charges;
;;             when AMULET:
;;                 worth = 1000;
;;         }
;;         mvprintw(c - 'a' + 1, 0, "%c) %5d  %s", c, worth, inv-name(obj, FALSE));
;;         purse += worth;
;;     }
;;     mvprintw(c - 'a' + 1, 0,"   %5d  Gold Peices          ", oldpurse);
;;     refresh();
;;     score(purse, 2, 0);
;;     exit(0);
;; }

