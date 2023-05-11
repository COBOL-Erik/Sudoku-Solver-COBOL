000100 identification division.
000200 program-id. MDSMSDUS.
000300
000400* Av Erik Weyler 2013-03
000500
000600* RETURN-CODE =                  0       Sudokun löst
000700*                                100     Sudokun saknar lösning
000800*                                200     Ingen lösning funnen
000900*                                3000    Erforderligt minne saknas
001000*                                4000    Internt fel!
001100
001200* Anrop:
001300* CALL 'MDSMSDUS' USING SUDOKU
001400*                       SUDOKUNS-SIDLÄNGD
001500*                       ANTAL-GISSNINGAR-SOM-KRÄVDES-INNAN-LÖSNING
001600* EVALUATE RETURN-CODE !
001700
001800 environment division.
001900 configuration section.
002000 special-names. decimal-point is comma.
002100
002200 data division.
002300 working-storage section.
002400 01  sdu-size   pic 9 comp-3.
002500
002600 01  the-sudoku.
002700     05 rows.
002800        10 row1 pic X(9) value '000000000'.
002900        10 row2 pic X(9) value '000000000'.
003000        10 row3 pic X(9) value '000000000'.
003100        10 row4 pic X(9) value '000000000'.
003200        10 row5 pic X(9) value '000000000'.
003300        10 row6 pic X(9) value '000000000'.
003400        10 row7 pic X(9) value '000000000'.
003500        10 row8 pic X(9) value '000000000'.
003600        10 row9 pic X(9) value '000000000'.
003700     05 rows-nr redefines rows.
003800        10 row-nr pic X(9) occurs 9 times.
003900 
004000 01  ix         pic 99 comp-3.
004100
004200 01  step-square.
004300     05 y                        pic 99 comp-3.
004400     05 x                        pic 99 comp-3.
004500
004600 01  rem-x-y-display.
004700     05 d-rem-x                  pic 99 comp-3.
004800     05 d-rem-y                  pic 99 comp-3.
004900
005000 01  current-sudoku              pic 9(4) comp-3.
005100
005200 01  current-value               pic 9.
005300
005400 01  number-of-sdu               pic 9(4) comp-3.
005500
005600 01  sudokus.
005700     05 sdu occurs 55 times.
005800        10 number-of-numbers-set pic 99 comp-3 value zero.
005900        10 rem-sudoku.
006000           15 rem-x              pic 9  comp-3.
006100           15 rem-y              pic 9  comp-3.
006200        10 rows.
006300           15 row occurs 9 times.
006400              20 columns.
006500                 25 col occurs 9 times.
006600                    30 square.
006700                       35 the-value  pic 9.
006800                       35 candidates.
006900                          40 candidate occurs 9 times.
007000                             45 filler pic X.
007100                                88 possible   value 'Y'.
007200                                88 impossible value 'N'.
007300* Access: candidate(sudoku-nr, row-nr, col-nr, candidate-nr)
007400
007500 01 filler.
007600    05 rem-tried-x-y occurs 55 times.
007700       10 rem-tried-x             pic 9  comp-3.
007800       10 rem-tried-y             pic 9  comp-3.
007900       10 rem-tried-candidate     pic 9  comp-3.
008000       10 filler                  pic X       value 'F'.
008100          88 nr-1-tried                       value 'S'.
008200          88 nr-1-tried-not                   value 'F'.
008300
008400
008500 01 candidate-to-set pic 9.
008600
008700 01 master-square.
008800    05 filler                     pic X.
008900       88 consider-master-square-yes value 'S'.
009000       88 consider-master-square-no  value 'F'.
009100    05 master-square-size         pic 99 comp-3.
009200    05 master-square-x-start      pic 99 comp-3.
009300    05 master-square-x-stop       pic 99 comp-3.
009400    05 master-square-y-start      pic 99 comp-3.
009500    05 master-square-y-stop       pic 99 comp-3.
009600    05 master-square-limit-found-flag.
009700       10 filler                  pic X.
009800          88 x-limits-found     value 'S'.
009900          88 x-limits-found-not value 'F'.
010000       10 filler                  pic X.
010100          88 y-limits-found     value 'S'.
010200          88 y-limits-found-not value 'F'.
010300
010400 01 v-vaxlar.
010500    05 filler                    pic X.
010600* v-done då alla rutor i en sudoku är genomlöpta.
010700       88 v-done                       value 'S'.
010800       88 v-not-done                   value 'F'.
010900    05 filler                    pic X.
011000       88 v-sudoku-init                value 'I'.
011100       88 v-sudoku-done                value 'S' 'F'.
011200       88 v-sudoku-solved              value 'S'.
011300       88 v-no-solution                value 'F'.
011400    05 filler                    pic X.
011500       88 v-number-init                value 'I'.
011600       88 v-number-set                 value 'S'.
011700       88 v-number-set-not             value 'F'.
011800    05 filler                    pic X.
011900       88 candidate-square-found       value 'S'.
012000       88 candidate-square-found-not   value 'F'.
012100    05 filler                    pic X.
012200       88 candidate-number-found       value 'S'.
012300       88 candidate-number-found-not   value 'F'.
012400    05 filler                    pic X.
012500       88 v-done-master-square         value 'S'.
012600       88 v-not-done-master-square     value 'F'.
012700    05 filler                    pic X.
012800       88 find-first-empty-square-init value 'I'.
012900       88 find-first-empty-square-done value 'S'.
013000    05 filler                    pic X.
013100       88 first-candidate-init         value 'I'.
013200       88 first-candidate-found        value 'S'.
013300
013400 01 number-of-guesses            pic 9(4) comp-3.
013500
013600 linkage section.
013700 01 p-sudoku                     pic X(81).
013800 01 p-sdu-size                   pic 99   comp-3.
013900 01 p-nr-of-guesses              pic 9(4) comp-3.
014000
014100 procedure division using p-sudoku
014200                          p-sdu-size
014300                          p-nr-of-guesses.
014400 a-main                              section.
014500* Läs in sudokun + andra initieringar:
014600     perform b-init
014700
014800     perform until v-sudoku-done
014900* Lista alla möjliga siffror i rutorna:
015000        perform bb-init
015100        perform list-candidates until v-done
015200* "Sätt" ensamma kandidater tills alla är satta:
015300        set v-not-done to true
015400        perform set-number until v-number-set-not or v-done
015500* Nu är sudokun löst eller så fanns det inga ensamma kandidat#
015600        if number-of-numbers-set(current-sudoku) >= sdu-size**2
015700           set v-sudoku-solved to true
015800           go to sudoku-solved
015900        end-if
016000* Är inte sudokun löst? Skapa sudoku-kopia och gissa på en siffra:
016100        perform copy-this-sudoku
016200        perform bb-init
016300        perform make-guess
016400     end-perform
016500
016600     goback
016700     .
016800
016900 b-init                              section.
017000     set v-sudoku-init to true
017100     move zero to number-of-numbers-set(1)
017200
017300     move p-sudoku   to the-sudoku
017400     move p-sdu-size to sdu-size
017500* Populera sudoku nr 1 med inskickad sudoku-sträng:
017600     move 1 to y x current-sudoku
017700     perform varying ix from 1 by 1 until ix > sdu-size**2
017800        move the-sudoku(ix:1) to the-value(1,y,x)
017900        if the-value(1,y,x) not = 0
018000           add 1 to number-of-numbers-set(1)
018100        end-if
018200* Alla siffror är kandidater initialt:
018300        move all 'Y' to candidates(1,y,x)
018400        add 1 to x
018500        if function mod(ix,sdu-size) = 0
018600           move 1 to x
018700           add  1 to y
018800        end-if
018900     end-perform
019000
019100* Initiera "gissningsvariabler":
019200     compute number-of-sdu =
019300                 (length of sudokus) / (length of sdu)
019400
019500     perform varying ix from 1 by 1 until ix > number-of-sdu
019600        move zero to rem-tried-y(ix)
019700                     rem-tried-x(ix)
019800                     rem-tried-candidate(ix)
019900     end-perform
020000
020100* Kolla om vi ska bry oss om "master"-rutor (om sqrt(sida) heltal)
020200     set consider-master-square-no to true
020300     if (sdu-size**0,5)**2 = sdu-size
020400        set consider-master-square-yes to true
020500        compute master-square-size = sdu-size**0,5
020600     end-if
020700
020800* Vi håller reda på antalet gissningar som krävs innan lösning:
020900     move zero to number-of-guesses
021000
021100     perform bb-init
021200     .
021300
021400 bb-init section.
021500     set v-not-done       to true
021600     set v-number-init    to true
021700     move 1 to y x
021800     .
021900
022000 list-candidates                     section.
022100     perform until the-value(current-sudoku,y,x) not = 0
022200        perform step-x-y
022300        if v-done
022400           go to section-end
022500        end-if
022600     end-perform
022700* I aktuell ruta är siffran satt (dvs <> 0)
022800     move the-value(current-sudoku,y,x) to current-value
022900     perform impact-analysis
023000     perform step-x-y
023100     .
023200
023300 section-end.
023400     continue
023500     .
023600
023700* Får: x, y, sdu-size
023800* Gör: Går till nästa ruta i sudokun
023900* Ger: Nytt x, Nytt x och y eller v-done om hela sudokun genomlöpt
024000 step-x-y section.
024100     add 1 to x
024200     if x > sdu-size
024300        move 1 to x
024400        add  1 to y
024500        if y > sdu-size
024600* Hela sudokun genomlöpt
024700           set v-done to true
024800        end-if
024900     end-if
025000     .
025100
025200* Får: master-square-x-start, master-square-x-stop,
025300*      master-square-y-start, master-square-y-stop,
025400*      master-square-size
025500* Gör: Går till nästa ruta i "master"-rutan
025600* Ger: Nytt x, Nytt x och y eller v-done-master-square
025700*      om hela "master"-rutan genomlöpt
025800 step-master-square section.
025900     add 1 to x
026000     if x > master-square-x-stop
026100        move master-square-x-start to x
026200        add  1 to y
026300        if y > master-square-y-stop
026400* Hela sudokun genomlöpt
026500           set v-done-master-square to true
026600        end-if
026700     end-if
026800     .
026900
027000* Stryk kandidater alt. notera krasch (typ två 3:or på samma rad)
027100 impact-analysis                    section.
027200     move x to rem-x(current-sudoku)
027300     move y to rem-y(current-sudoku)
027400
027500* Koll på rad:
027600     perform varying x from 1 by 1 until x > sdu-size
027700         if x = rem-x(current-sudoku)
027800            continue
027900         else
028000            if the-value(current-sudoku, y, x) = current-value
028100* Samma värde förekommer två gånger på samma rad. Ajaj.
028200               go to admit
028300            end-if
028400* Stryk aktuellt värde som kandidat i denna ruta:
028500            set impossible(current-sudoku,y,x,current-value)
028600             to true
028700         end-if
028800     end-perform
028900
029000     move rem-x(current-sudoku) to x
029100
029200* Koll på kolumn:
029300     perform varying y from 1 by 1 until y > sdu-size
029400         if y = rem-y(current-sudoku)
029500            continue
029600         else
029700            if the-value(current-sudoku, y, x) = current-value
029800* Samma värde förekommer två gånger i samma kolumn. Ajaj.
029900               go to admit
030000            end-if
030100* Stryk aktuellt värde som kandidat i denna ruta:
030200            set impossible(current-sudoku,y,x,current-value)
030300             to true
030400         end-if
030500     end-perform
030600
030700     move rem-y(current-sudoku) to y
030800
030900     if consider-master-square-no
031000        go to section-end
031100     end-if
031200* Koll inom "master"-rutan (typiskt 3x3):
031300* 1. Hitta de aktuella x-, och y-gränserna i master-rutan:
031400     set x-limits-found-not to true
031500     set y-limits-found-not to true
031600     perform varying ix from 1 by 1
031700       until ix * master-square-size > sdu-size
031800          or (x-limits-found and y-limits-found)
031900             if x-limits-found
032000                continue
032100             else
032200                if x <= ix * master-square-size
032300                   compute master-square-x-start =
032400                           (ix - 1) * master-square-size + 1
032500                   compute master-square-x-stop =
032600                           ix * master-square-size
032700                   set x-limits-found to true
032800                end-if
032900             end-if
033000
033100             if y-limits-found
033200                continue
033300             else
033400                if y <= ix * master-square-size
033500                   compute master-square-y-start =
033600                           (ix - 1) * master-square-size + 1
033700                   compute master-square-y-stop =
033800                           ix * master-square-size
033900                   set y-limits-found to true
034000                end-if
034100             end-if
034200     end-perform
034300* 2. Koll "krockar" i "master"-rutan:
034400     move master-square-x-start    to x
034500     move master-square-y-start    to y
034600     set  v-not-done-master-square to true
034700     perform until v-done-master-square
034800        if x = rem-x(current-sudoku) and y = rem-y(current-sudoku)
034900           continue
035000        else
035100           if the-value(current-sudoku, y, x) = current-value
035200* Samma värde förekommer två gånger i samma "master"-ruta. Ajaj.
035300              go to admit
035400           end-if
035500* Stryk aktuellt värde som kandidat i denna ruta:
035600           set impossible(current-sudoku,y,x,current-value)
035700            to true
035800        end-if
035900        perform step-master-square
036000     end-perform
036100
036200     move rem-x(current-sudoku) to x
036300     move rem-y(current-sudoku) to y
036400
036500     go to section-end
036600     .
036700
036800* Här hamnar man vid krasch.
036900 admit.
037000     subtract 1 from current-sudoku
037100     perform failed-guess-or-sudoku
037200     .
037300
037400 section-end.
037500     continue
037600     .
037700
037800
037900 set-number section.
038000     perform bb-init
038100     set candidate-square-found-not to true
038200     perform until candidate-square-found or v-done
038300         perform find-first-empty-square
038400         if v-not-done
038500            perform find-first-lonely-candidate
038600            if candidate-square-found-not
038700               perform step-x-y
038800            end-if
038900         end-if
039000     end-perform
039100
039200     if candidate-square-found
039300* Sätt siffran här i ledig ruta med endast en kandidatsiffra!
039400        move candidate-to-set to the-value(current-sudoku,y,x)
039500        add 1 to number-of-numbers-set(current-sudoku)
039600        set v-number-set to true
039700        perform list-candidates
039800     end-if
039900     .
040000
040100
040200 find-first-empty-square section.
040300     perform until v-done
040400                or the-value(current-sudoku,y,x) = 0
040500        perform step-x-y
040600     end-perform
040700     .
040800
040900
041000
041100 find-first-lonely-candidate section.
041200     set candidate-number-found-not to true
041300     perform varying ix from 1 by 1 until ix > sdu-size
041400        if possible(current-sudoku,y,x,ix)
041500           if candidate-number-found
041600* Vi har flera kandidatsiffror till denna ruta. Hitta annan ruta!
041700              go to admit
041800           else
041900              set candidate-number-found to true
042000              move ix to candidate-to-set
042100           end-if
042200        end-if
042300     end-perform
042400
042500* I denna ruta fanns endast en kandidatsiffra!
042600     set candidate-square-found to true
042700     go to section-end
042800     .
042900
043000 admit.
043100     set candidate-square-found-not to true
043200     .
043300
043400 section-end.
043500     continue
043600     .
043700
043800
043900 copy-this-sudoku section.
044000     if current-sudoku + 1 > number-of-sdu
044100* Vi klarar inte fler sudoku-kopior
044200        move 3000 to RETURN-CODE
044300        set v-no-solution to true
044400*       display 'Minnet räcker ej! Har ' number-of-sdu ' sudokus.'
044500        goback
044600     else
044700        move sdu(current-sudoku) to sdu(current-sudoku + 1)
044800        add 1 to current-sudoku
044900     end-if
045000     .
045100
045200
045300 set-guess-starting-point section.
045400     move rem-tried-y(current-sudoku)         to y
045500     move rem-tried-x(current-sudoku)         to x
045600     move rem-tried-candidate(current-sudoku) to ix
045700     if x = 0 or y = 0
045800        move 1 to ix
045900        perform bb-init
046000        perform find-first-empty-square
046100     end-if
046200     .
046300
046400 remember-guess section.
046500     move y  to rem-tried-y(current-sudoku)
046600     move x  to rem-tried-x(current-sudoku)
046700     move ix to rem-tried-candidate(current-sudoku)
046800     .
046900
047000 make-guess section.
047100     perform set-guess-starting-point
047200     perform find-first-candidate
047300     if v-done
047400        go to admit
047500     end-if
047600     perform remember-guess
047700
047800* Vi sätter första funna kandidat# i rutan (en gissning)
047900     move ix to the-value(current-sudoku,y,x)
048000     add 1 to number-of-numbers-set(current-sudoku)
048100* Vi håller reda på antal gissningar vi gör:
048200     add 1 to number-of-guesses
048300* Tidigare ignorerad siffra kan nu vara kandidat iom ny gissning:
048400     if current-sudoku < number-of-sdu
048500        move 1 to rem-tried-candidate(current-sudoku + 1)
048600        set nr-1-tried-not(current-sudoku + 1) to true
048700     end-if
048800     .
048900
049000 admit.
049100     continue
049200     .
049300
049400 find-first-candidate section.
049500     set first-candidate-init to true
049600     perform until first-candidate-found
049700        set candidate-number-found-not   to true
049800        perform until candidate-number-found or v-done
049900           perform until possible(current-sudoku,y,x,ix)
050000                      or ix > sdu-size
050100              add 1 to ix
050200           end-perform
050300           if ix > sdu-size
050400                 go to admit
050500           else
050600              set candidate-number-found to true
050700           end-if
050800        end-perform
050900        if v-done
051000* Här hamnar vi om vi inte fann någon tom ruta att gissa i(!)
051100           move 200 to RETURN-CODE
051200           set v-no-solution to true
051300*          display 'Tom ruta för gissning saknas.'
051400*          display 'Sudokun saknar lösning.'
051500*          display 'Antal satta #: '
051600*                  number-of-numbers-set(current-sudoku)
051700           goback
051800        end-if
051900        if (y  = rem-tried-y(current-sudoku) and
052000            x  = rem-tried-x(current-sudoku) and
052100            ix = rem-tried-candidate(current-sudoku))
052200            if ix = 1 and nr-1-tried-not(current-sudoku)
052300               set nr-1-tried(current-sudoku) to true
052400               set first-candidate-found to true
052500            else
052600               add 1 to ix
052700            end-if
052800        else
052900           set first-candidate-found to true
053000        end-if
053100     end-perform
053200
053300     go to section-end
053400     .
053500
053600 admit.
053700     subtract 2 from current-sudoku
053800     perform failed-guess-or-sudoku
053900     .
054000
054100 section-end.
054200     continue
054300     .
054400
054500
054600 failed-guess-or-sudoku section.
054700     set v-done to true
054800     if current-sudoku <= 0
054900         move 100 to RETURN-CODE
055000*        display "Sudokun saknar lösningar!"
055100         goback
055200     end-if
055300     .
055400
055500
055600 display-sudoku section.
055700     perform varying ix from 1 by 1 until ix > sdu-size
055800        display row(current-sudoku,ix) upon console
055900     end-perform
056000     display ' '
056100     .
056200
056300 display-sudoku-clean section.
056400     move y to d-rem-y
056500     move x to d-rem-x
056600     perform sudoku-to-string
056700     perform varying ix from 1 by 1 until ix > sdu-size
056800        display row-nr(ix) upon console
056900     end-perform
057000     display ' ' upon console
057100
057200     move d-rem-x to x
057300     move d-rem-y to y
057400     .
057500
057600 sudoku-solved section.
057700     move 0 to RETURN-CODE
057800     perform sudoku-to-string
057900     move the-sudoku        to p-sudoku
058000     move number-of-guesses to p-nr-of-guesses
058100*    display ' '
058200*    display 'Sudokun är löst!'
058300*    display 'Antal använda sudokukopior: ' current-sudoku
058400*    perform display-sudoku-clean
058500     goback
058600     .
058700
058800* Ger: the-sudoku
058900 sudoku-to-string section.
059000     move 1 to y x ix
059100     set v-not-done to true
059200     perform until v-done
059300        move the-value(current-sudoku,y,x)
059400          to the-sudoku(ix:1)
059500        perform step-x-y
059600        add 1 to ix
059700     end-perform
059800     .
059900
