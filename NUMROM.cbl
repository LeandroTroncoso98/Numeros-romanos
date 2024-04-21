       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMROM.
       AUTHOR. TRONCOSO LEANDRO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OCC-ROMANO.
       05 OCC-ROMANO OCCURS 8 TIMES.
          10 NUMERO-ROM           PIC X.
       77 NUMERO-IN               PIC 999.
       77 NUMERO-AUX              PIC 999.
          88 NUMERO-AUX-Z         VALUE 0.
       77 INDICE                  PIC 9 VALUE 1.
          88 INDICE-FIN           VALUE 8.
       77 STRING-ROMANO           PIC X(8).

       PROCEDURE DIVISION.
       0100-MAIN-PGM.
           ACCEPT NUMERO-IN FROM SYSIN
           IF NUMERO-IN NOT IS NUMERIC
               DISPLAY "EL DATO NO ES NUMERICO."
               PERFORM 0400-FIN
           END-IF
           IF NUMERO-IN >= 1 OR NUMERO-IN <= 100
              EVALUATE NUMERO-IN
               WHEN 1
                MOVE "I" TO STRING-ROMANO
               WHEN 5
                MOVE "V" TO STRING-ROMANO
               WHEN 10
                MOVE "X" TO STRING-ROMANO
               WHEN 50
                MOVE "L" TO STRING-ROMANO
               WHEN 100
                MOVE "C" TO STRING-ROMANO
               WHEN OTHER
                MOVE NUMERO-IN TO NUMERO-AUX
                PERFORM 0200-GENERAR-N-COMPLEJO UNTIL NUMERO-AUX-Z
                PERFORM 0300-MOSTRAR-NUMERO
              END-EVALUATE
           ELSE
              DISPLAY "NO PUEDE CONVERTIRSE"
           END-IF
           PERFORM 0400-FIN.

      *****************************************************************
      * ESTE PARRAFO ES UN BUCLE PARA IR GENERANDO EL NUMERO ROMANO   *
      * DIGITO POR DIGITO HASTA QUE EL VALOR QUEDE EN 0               *
      *****************************************************************

       0200-GENERAR-N-COMPLEJO.
           EVALUATE TRUE
            WHEN NUMERO-AUX > 50
             SUBTRACT 50 FROM NUMERO-AUX GIVING NUMERO-AUX
             MOVE "L" TO NUMERO-ROM(INDICE)
            WHEN NUMERO-AUX > 10
             SUBTRACT 10 FROM NUMERO-AUX GIVING NUMERO-AUX
             MOVE "X" TO NUMERO-ROM(INDICE)
            WHEN NUMERO-AUX = 9
             MOVE 0 TO NUMERO-AUX
             MOVE "I" TO NUMERO-ROM(INDICE)
             COMPUTE INDICE = INDICE + 1 END-COMPUTE
             MOVE "X" TO NUMERO-ROM(INDICE)
            WHEN NUMERO-AUX > 5
             SUBTRACT 5 FROM NUMERO-AUX GIVING NUMERO-AUX
             MOVE "V" TO NUMERO-ROM(INDICE)
            WHEN NUMERO-AUX = 4
             MOVE 0 TO NUMERO-AUX
             MOVE "I" TO NUMERO-ROM(INDICE)
             COMPUTE INDICE = INDICE + 1 END-COMPUTE
             MOVE "V" TO NUMERO-ROM(INDICE)
            WHEN NUMERO-AUX <= 3
             SUBTRACT 1 FROM NUMERO-AUX GIVING NUMERO-AUX
             MOVE "I" TO NUMERO-ROM(INDICE)
            END-EVALUATE
            COMPUTE INDICE = INDICE + 1 END-COMPUTE.

      *****************************************************************
      * ESTE PARRAFO ATRAVES DE UN BUCLE CONCATENA CADA CARACTER QUE  *
      * ESTE GUARDADO EN EL ARREGLO GUARDANDOLO EN LA VARIABLE        *
      *****************************************************************
       0300-MOSTRAR-NUMERO.
            STRING NUMERO-ROM(1) DELIMITED BY SIZE
               NUMERO-ROM(2) DELIMITED BY SIZE
               NUMERO-ROM(3) DELIMITED BY SIZE
               NUMERO-ROM(4) DELIMITED BY SIZE
               NUMERO-ROM(5) DELIMITED BY SIZE
               NUMERO-ROM(6) DELIMITED BY SIZE
               NUMERO-ROM(7) DELIMITED BY SIZE
               NUMERO-ROM(8) DELIMITED BY SIZE
               INTO STRING-ROMANO
            END-STRING
            DISPLAY "EL NUMERO " NUMERO-IN " EN ROMANO " STRING-ROMANO.

       0400-FIN.
            STOP RUN.
