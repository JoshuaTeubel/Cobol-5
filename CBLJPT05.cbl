       identification division.
       program-id. CBLJPT05.
       AUTHOR.     Joshua Teubel.
       DATE-WRITTEN. 1/29/2017.
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RENT-REC
               ASSIGN TO 'C:\Cobol\MONBILLS.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\Cobol\CBLRENT.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.

       data division.
       FILE SECTION.

       FD  RENT-REC
           LABEL RECORD IS STANDARD
           DATA RECORD IS SALES-REC
           RECORD CONTAINS 43 CHARACTERS.
           01  RENTAL-REC.
               05  I-BLD-CODE              PIC XX.
               05  I-UNIT                  PIC 99.
               05  I-TENANTS               PIC 9.
               05  I-ELECTRIC              PIC 999V99.
               05  I-GAS                   PIC 999V99.
               05  I-WATER                 PIC 999V99.
               05  I-GARBAGE               PIC 99V99.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01  PRTLINE             PIC X(132).
       working-storage section.
       01 MISC.
           05 EOF                          PIC X   VALUE 'F'.
           05 CURRENT-DATE-AND-TIME.
               10 CURRENT-YEAR     PIC X(4).
               10 CURRENT-MONTH    PIC XX.
               10 CURRENT-DAY      PIC XX.
               10 CURRENT-TIME     PIC X(11).
           05  C-PCTR              PIC S99     VALUE ZERO.
           05  PREM-CTR            PIC S99     VALUE ZERO.
           05  DISC-CTR            PIC S99     VALUE ZERO.
           05  C-UNIT              PIC 99.
               88 UNIT-1-8
                   VALUE 1 THRU 8.
               88 UNIT-9-16
                   VALUE 9 THRU 16.
               88 UNIT-17-25
                   VALUE 17 THRU 25.
               88 UNIT-23-25
                   VALUE 23 THRU 25.
           05  C-BLD-CODE          PIC XX.
               88 R7YTPP
                   VALUE 'R7', 'YT', 'PP'.
               88 BPCT
                   VALUE 'BP', 'CT'.
           05 TENANT-CHARGE        PIC 999V99.
           05 BASERATE             PIC 999V99.
           05 PREMIUM              PIC 9999V99.
           05 DISCOUNT             PIC 999V99.
           05 C-SUBTOTAL           PIC 99999V99.
           05 UTIL-TOT             PIC 99999V99.
           05 TOT-RENT             PIC 99999V99.
           05 GT-BASE-RENT         PIC 999999V99   VALUE ZERO.
           05 GT-TEN-CHARGE        PIC 999999V99   VALUE ZERO.
           05 GT-PRE-DISC          PIC S9999999V99 VALUE ZERO.
           05 GT-SUBTOTAL          PIC 9999999V99  VALUE ZERO.
           05 GT-TOT-UTIL          PIC 9999999V99  VALUE ZERO.
           05 GT-TOT-RENT          PIC 9999999V99  VALUE ZERO.

       01 COMPANY-LINE.
           05 FILLER               PIC X(6)    VALUE 'DATE: '.
           05 H1-DATE.
               10  H1-MONTH        PIC 99.
               10  FILLER          PIC X       VALUE '/'.
               10  H1-DAY          PIC 99.
               10  FILLER          PIC X       VALUE '/'.
               10  H1-YEAR         PIC 9999.
           05 FILLER               PIC X(42)   VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'FURLY S RENTALS'.
           05 FILLER               PIC X(51)   VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'PAGE: '.
           05 H1-PAGE              PIC Z9.

       01 TITLE-LINE.
           05 FILLER               PIC X(8)    VALUE 'COBJPT05'.
           05 FILLER               PIC X(45)   VALUE SPACES.
           05 FILLER               PIC X(25)   VALUE
               'BILLABLE RENT - TEUBEL S '.
           05 FILLER               PIC X(54)   VALUE SPACES.

       01 FILL-LINE.
           05 FILLER               PIC X(132)  VALUE SPACES.

       01 COL-HEAD1.
           05 FILLER               PIC X(24)   VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE 'BASE'.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'TENANT'.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'TENANT'.
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'PREMIUM/'.
           05 FILLER               PIC X(53)   VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE 'TOTAL'.
           05 FILLER               PIC X(18)   VALUE SPACES.

       01 COL-HEAD2.
           05 FILLER               PIC X(16)   VALUE 'RENTAL BUILDING '.
           05 FILLER               PIC X(6)    VALUE 'RENT  '.
           05 FILLER               PIC X(8)    VALUE 'NUMBER  '.
           05 FILLER               PIC X(6)    VALUE 'CHARGE'.
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'DISCOUNT'.
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'SUBTOTAL'.
           05 FILLER               PIC X(8)    VALUE 'ELECTRIC'.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 FILLER               PIC X(3)    VALUE 'GAS'.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE 'WATER'.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 FILLER               PIC X(7)    VALUE 'GARBAGE'.
           05 FILLER               PIC X(3)    VALUE SPACES.
           05 FILLER               PIC X(9)    VALUE 'UTILITIES'.
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'RENT DUE'.
           05 FILLER               PIC X(3)    VALUE SPACES.

       01 DETAIL-LINE.
           05 BLD-LIT              PIC X(15).
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 D-UNIT               PIC Z9.
           05 FILLER               PIC X(2).
           05 D-RENT               PIC $$$$.99.
           05 FILLER               PIC X(3)    VALUE SPACES.
           05 TENANT-NUM           PIC 9.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 D-T-CHARGE           PIC $$$$.99.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 D-PRE-DSC            PIC +$$,$$$.99.
           05 FILLER               PIC X(3)    VALUE SPACES.
           05 D-SUB                PIC $$,$$$.99.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 D-ELECTRIC           PIC $$$$.99.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 D-GAS                PIC $$$$.99.
           05 FILLER               PIC XX      VALUE SPACES.
           05 D-WATER              PIC $$$$.99.
           05 FILLER               PIC XX      VALUE SPACES.
           05 D-GARBAGE            PIC $$$.99.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 D-TOT-UTIL           PIC $$,$$$.99.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 D-RENT-DUE           PIC $$,$$$.99.
           05 FLAG                 PIC XXX     VALUE '   '.

       01 GT-LINE.
           05 FILLER               PIC X(13)   VALUE 'GRAND TOTALS:'.
           05 FILLER               PIC X(5)    VALUE SPACES.
           05 GT-B-RENT            PIC $$$,$$$.99.
           05 FILLER               PIC X(5).
           05 GT-T-CHARGE          PIC $$$,$$$.99.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 O-GT-PRE-DESC        PIC +$$$$,$$$.99.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-GT-SUBTOTAL        PIC $$$$,$$$.99.
           05 FILLER               PIC X(36)   VALUE SPACES.
           05 O-GT-TOT-UTIL        PIC $$$$,$$$.99.
           05 FILLER               PIC XX      VALUE SPACES.
           05 GT-RENT-DUE          PIC $$$$,$$$.99.
           05 FILLER               PIC XXX     VALUE SPACES.

       01 GT-LINE-2.
           05 FILLER               PIC X(34)   VALUE SPACES.
           05 FILLER               PIC X(19)   VALUE 
               'RENTALS DISCOUNTED '.
           05 GT-DISC              PIC ZZ9.
           05 FILLER               PIC X(76)   VALUE SPACES.

       01 GT-LINE-3.
           05 FILLER               PIC X(37)   VALUE SPACES.
           05 FILLER               PIC X(16)   VALUE
               'PREMIUM RENTALS '.
           05 GT-PRE               PIC ZZ9.
           05 FILLER               PIC X(76)   VALUE SPACES.

       procedure division.
       L1-MAIN.
           PERFORM L2-INIT
           PERFORM L2-MAINLINE
               UNTIL EOF = 'T'.
           PERFORM L2-CLOSING.
           STOP RUN.
       
       L2-INIT.
           MOVE FUNCTION CURRENT-DATE      TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-DAY                TO H1-DAY.
           MOVE CURRENT-MONTH              TO H1-MONTH.
           MOVE CURRENT-YEAR               TO H1-YEAR.
           OPEN INPUT RENT-REC.
           OPEN OUTPUT PRTOUT.
           PERFORM L3-READ.
           PERFORM L3-EVALUATE.
           PERFORM L4-HEADINGS.

       L2-MAINLINE.
           PERFORM L3-CALCS.
           PERFORM L3-MOVES.
           PERFORM L3-READ.
           PERFORM L3-EVALUATE.

       L2-CLOSING.
           MOVE GT-BASE-RENT TO GT-B-RENT.
           MOVE GT-TEN-CHARGE TO GT-T-CHARGE.
           MOVE GT-PRE-DISC TO O-GT-PRE-DESC.
           MOVE GT-SUBTOTAL TO O-GT-SUBTOTAL.
           MOVE GT-TOT-UTIL TO O-GT-TOT-UTIL.
           MOVE GT-TOT-RENT TO GT-RENT-DUE.

           WRITE PRTLINE FROM GT-LINE
               AFTER ADVANCING 3 LINES.

           MOVE DISC-CTR TO GT-DISC.

           WRITE PRTLINE FROM GT-LINE-2
               AFTER ADVANCING 2 LINES.

           MOVE PREM-CTR TO GT-PRE.

           WRITE PRTLINE FROM GT-LINE-3
               AFTER ADVANCING 1 LINES.



           CLOSE RENT-REC.
           CLOSE PRTOUT.

       L3-READ.
           READ RENT-REC
               AT END
                   MOVE 'T' TO EOF.

       L3-EVALUATE.

           EVALUATE I-BLD-CODE
               WHEN 'AA'
                   MOVE 'PALACE PLACE' TO BLD-LIT
               WHEN 'GG'
                   MOVE 'GEROGIA' TO BLD-LIT
               WHEN 'PP'
                   MOVE 'PARK PLACE' TO BLD-LIT
               WHEN 'IA'
                   MOVE 'IOWA CONDO' TO BLD-LIT
               WHEN 'MS'
                   MOVE 'MARKET STREET' TO BLD-LIT
               WHEN 'HH'
                   MOVE 'HIGH TOWER' TO BLD-LIT
               WHEN 'R7'
                   MOVE 'UPTOWN CONDOS' TO BLD-LIT
               WHEN 'GM'
                   MOVE 'GANDER MOUNTAIN' TO BLD-LIT
               WHEN 'BP'
                   MOVE 'BENTON PLACE' TO BLD-LIT
               WHEN 'GA'
                   MOVE 'GRAND AVENUE' TO BLD-LIT
               WHEN 'JK'
                   MOVE 'JACK S PLACE' TO BLD-LIT
               WHEN 'UN'
                   MOVE 'UNDERGROUND SAM' TO BLD-LIT
               WHEN 'YD'
                   MOVE 'YANKEE DOODLE' TO BLD-LIT
               WHEN 'YT'
                   MOVE 'YAHTZEE AVE' TO BLD-LIT
               WHEN 'CP'
                   MOVE 'COURT PLACE' TO BLD-LIT
               WHEN 'NZ'
                   MOVE 'NEW ZOO' TO BLD-LIT
               WHEN 'VV'
                   MOVE 'VERMONT' TO BLD-LIT
               WHEN 'CT'
                   MOVE 'CHINA TOWN' TO BLD-LIT
               WHEN 'YS'
                   MOVE 'YORKSHIRE' TO BLD-LIT
               WHEN 'ME'
                   MOVE 'MAINE APT' TO BLD-LIT
           END-EVALUATE.
           MOVE I-BLD-CODE TO C-BLD-CODE.
           MOVE I-UNIT TO C-UNIT.
           MOVE ZERO TO TENANT-CHARGE.

           EVALUATE TRUE
               WHEN UNIT-1-8
                   BASERATE = 650.00.
                   IF I-TENANTS > THAN 4
                       MOVE 83.45 TO TENANT-CHARGE
                   ELSE
                       IF I-TENANTS > THAN 1
                           TENANT-CHARGE = 25.00 * (I-TENANTS - 1)
                   END-IF
               WHEN UNIT-9-16
                   BASERATE = 700.00.
                   IF I-TENANTS > THAN 4
                       MOVE 135.00 TO TENANT-CHARGE
                   ELSE
                       IF I-TENANTS > THEN 1
                           TENANT-CHARGE = 35.55 * (I-TENANTS - 1)
                   END-IF
               WHEN UNIT-17-25
                   BASERATE = 825.00.
                   IF I-TENANTS > THAN 4
                       MOVE 185.60 TO TENANT-CHARGE
                   ELSE 
                       IF I-TENANTS > THAN 1
                           TENANT-CHARGE = 50.00 * (I-TENANTS - 1)
                   ENF-IF
           END-EVALUATE.

           MOVE ZERO TO PREMIUM.
           MOVE ZERO TO DISCOUNT.

           IF R7YTPP
               IF UNIT-23-25 THAN
                   PREMIUM ROUNDED = (BASERATE + TENANT-CHARGE) * .12
                   PREM-CTR = PREM-CTR + 1
               END-IF
           END-IF.

           IF BPCT THAN
               DISCOUNT ROUNDED = (BASERATE + TENANT-CHARGE) * .33
               DISC-CTR = DISC-CTR + 1.
           END-IF.

       L3-CALCS.
           C-SUBTOTAL ROUNDED
               = BASERATE + TENANT-CHARGE + PREMIUM - DISCOUNT.
           UTIL-TOT ROUNDED = I-WATER + I-GAS + I-ELECTRIC + I-GARBAGE.
           TOT-RENT ROUNDED = C-SUBTOTAL + UTIL-TOT.

           IF TOT-RENT > THAN 1000
               MOVE '***' TO FLAG
           END-IF.
           GT-BASE-RENT = GT-BASE-RENT + BASERATE.
           GT-TEN-CHARGE = GT-TEN-CHARGE + TENANT-CHARGE.
           GT-PRE-DISC = GT-PRE-DISC + PREMIUM + DISCOUNT.
           GT-SUBTOTAL = GT-SUBTOTAL + C-SUBTOTAL.
           GT-TOT-UTIL = GT-TOT-UTIL + UTIL-TOT.
           GT-TOT-RENT = GT-TOT-RENT + TOT-RENT.
       L3-MOVES.

           MOVE I-UNIT TO D-UNIT.
           MOVE BASERATE TO D-RENT.
           MOVE I-TENANTS TO TENANT-NUM.
           MOVE TENANT-CHARGE TO D-T-CHARGE.
           IF PREMIUM > THAN 0
               MOVE PREMIUM TO D-PRE-DSC
           ELSE
               IF DISCOUNT > THAN 0
                   MOVE DISCOUNT TO D-PRE-DSC
           END-IF.
           MOVE C-SUBTOTAL TO D-SUB.
           MOVE I-ELECTRIC TO D-ELECTRIC.
           MOVE I-GAS      TO D-GAS.
           MOVE I-WATER    TO D-WATER.
           MOVE I-GARBAGE  TO D-GARBAGE.
           MOVE UTIL-TOT   TO D-TOT-UTIL.
           MOVE TOT-RENT   TO D-RENT-DUE.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE.
       L4-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO H1-PAGE.
           WRITE PRTLINE FROM COMPANY-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM FILL-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COL-HEAD1
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COL-HEAD2
               AFTER ADVANCING 1 LINE.
       end program CBLJPT05.