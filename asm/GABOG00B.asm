*          DATA SET GABOG00B   AT LEVEL 120 AS OF 05/01/02                      
*PHASE TB1800B                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE RANDOM                                                                 
         TITLE 'BOGGLE - WORD GAME'                                             
***********************************************************************         
*--------------------------- MY MAIN PROGRAM -------------------------*         
BOGGMAIN CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BGGLWRKQ-BGGLWORK,GABOG00,R9,CLEAR=YES,RR=R2                     
         USING BGGLWORK,RC         WORKING STORAGE BASE REGISTER.               
         L     RA,4(R1)                                                         
         USING TB18FFD,RA          TWA BASE REGISTER                            
         ST    R2,RELO                                                          
         L     R2,8(R1)            R2=A(COMFACS).                               
         MVC   VCALLOV,CCALLOV-COMFACSD(R2)                                     
*                                   VCALLOV=A(CALLOV)                           
         EJECT                                                                  
BEGIN    NI    BOGSHFLH+1,X'FF'-X'01'    DE-MODIFY FIELD.                       
         NI    BOGSVRQH+1,X'FF'-X'01'    DE-MODIFY FIELD.                       
*                                                                               
         NI    BOGSHFLH+1,X'FF'-X'20'  UNPROTECT SHUFFLE FIELD                  
         OI    BOGSHFLH+6,X'80'                                                 
         NI    BOGGNBDH+1,X'FF'-X'20'  UNPROTECT GNBD FIELD.                    
         OI    BOGGNBDH+6,X'80'                                                 
         NI    BOGSIZEH+1,X'FF'-X'20'  UNPROTECT SIZE FIELD.                    
         OI    BOGSIZEH+6,X'80'                                                 
*                                                                               
         OC    LSTINDSP,LSTINDSP   CHECK FOR VERY FIRST TIME PLAYING.           
         BNZ   BEGI00                                                           
*                                                                               
         LA    R2,BOGSIZEH                                                      
         SR    R2,RA                                                            
         ST    R2,LSTINDSP         LAST DSPLCMNT FROM RA OF CURS INSRT.         
         MVI   KPSHFL,C'Y'                                                      
         MVI   KPGNBD,C'C'                                                      
*                                                                               
* CHECK FOR CORRECT INPUTS                                                      
*                                                                               
BEGI00   MVC   BOGHEAD,BLANKS                                                   
*                                                                               
         CLI   BOGSHFL,C'Y'                                                     
         BE    BEGI01                                                           
         CLI   BOGSHFL,C'N'                                                     
         BE    BEGI01                                                           
         MVC   BOGHEAD(L'ERSHFMSG),ERSHFMSG TELL USER OF ERROR                  
         OI    BOGHEADH+6,X'80'    TRANSMIT MESSAGE.                            
         L     R2,LSTINDSP         CHANGE POS OF CURSOR INSERT.                 
         AR    R2,RA                                                            
         NI    6(R2),X'FF'-X'40'                                                
         LA    R2,BOGSHFLH                                                      
         OI    6(R2),X'40'         POSITION CURSOR AT BOGSHUFL.                 
         SR    R2,RA                                                            
         ST    R2,LSTINDSP         STORE POS. OF CURSOR INSERT.                 
         B     EXIT1                                                            
*                                                                               
BEGI01   CLI   BOGGNBD,C'C'                                                     
         BE    BEGI02                                                           
         CLI   BOGGNBD,C'P'                                                     
         BE    BEGI02                                                           
         MVC   BOGHEAD(L'ERGBDMSG),ERGBDMSG TELL USER OF ERROR                  
         OI    BOGHEADH+6,X'80'    TRANSMIT MESSAGE.                            
         L     R2,LSTINDSP         CHANGE POS OF CURSOR INSERT.                 
         AR    R2,RA                                                            
         NI    6(R2),X'FF'-X'40'                                                
         LA    R2,BOGGNBDH                                                      
         OI    6(R2),X'40'         POSITION CURSOR AT BOGSHUFL.                 
         SR    R2,RA                                                            
         ST    R2,LSTINDSP         STORE POS. OF CURSOR INSERT.                 
         B     EXIT1                                                            
*                                                                               
BEGI02   CLI   BOGSIZE,C'4'                                                     
         BE    BEGI03                                                           
         CLI   BOGSIZE,C'5'                                                     
         BE    BEGI03                                                           
         MVC   BOGHEAD(L'ERSZEMSG),ERSZEMSG TELL USER OF ERROR                  
         OI    BOGHEADH+6,X'80'    TRANSMIT MESSAGE.                            
         L     R2,LSTINDSP         CHANGE POS OF CURSOR INSERT.                 
         AR    R2,RA                                                            
         NI    6(R2),X'FF'-X'40'                                                
         LA    R2,BOGSIZEH                                                      
         OI    6(R2),X'40'         POSITION CURSOR AT BOGSHUFL.                 
         SR    R2,RA                                                            
         ST    R2,LSTINDSP         STORE POS. OF CURSOR INSERT.                 
         B     EXIT1                                                            
*                                                                               
* INPUTS ARE VALID, SEE IF THEY HAVE CHANGED                                    
*                                                                               
BEGI03   CLC   KPGNBD,BOGGNBD      DOES PLAYER WANT A NEW BOARD                 
         BE    BEGI04               GENERATOR?                                  
         MVC   KPGNBD,BOGGNBD                                                   
         OI    FLAG,NEWGAMEQ       TURN NEW-GAME FLAG ON.                       
*                                                                               
BEGI04   CLC   KPSIZE,BOGSIZE      DOES PLAYER WANT A NEW BOARD SIZE?           
         BE    BEGI05                                                           
         MVC   KPSIZE,BOGSIZE                                                   
         OI    FLAG,NEWGAMEQ                                                    
*                                                                               
BEGI05   TM    FLAG,NEWGAMEQ                                                    
         BO    YSHUFFLE                                                         
*                                                                               
         CLI   BOGSHFL,C'Y'        DOES PLAYER WANT BOARD SHUFFLED?             
         BE    YSHUFFLE                                                         
*                                                                               
* RESHUFFLE = NO                                                                
*                                                                               
NSHUFFLE TM    FLAG,NEWGAMEQ       IS THIS A BRAND NEW GAME?                    
         BZ    NOTNEWGM                                                         
*                                                                               
         MVI   BOGSHFL,C'Y'        RESHUFFLE BOARD FOR NEW GAME.                
         OI    FLAG,NEWGAMEQ       TURN NEW GAME FLAG ON.                       
         L     R1,LSTINDSP         PREPARE TO UPDATE LSTINDSP.                  
         AR    R1,RA                                                            
         NI    6(R1),X'BF'                                                      
         LA    R1,BOGSIZEH                                                      
         OI    6(R1),X'40'         TURN ON POSITION-CURSOR HERE                 
         SR    R1,RA                (AT SIZE-INPUT FIELD).                      
         ST    R1,LSTINDSP                                                      
         MVC   BOGHEAD,BLANKS                                                   
         OI    BOGHEADH+6,X'80'                                                 
         MVI   BOGGNBD,C'C'        DEFAULT GENERATOR TO COMPUTER.               
         OI    BOGGNBDH+6,X'80'    TRANSMIT WHO-GENERATES FIELD.                
         B     EXIT1                                                            
*                                                                               
NOTNEWGM CLC   BOGGNBD,KPGNBD      HAS PLAYER CHANGE INPUT                      
         BE    MNSKP1               WHEN THEY ARE NOT SUPPOSED TO?              
         MVC   BOGGNBD,KPGNBD      CHANGE BOGGNBD BACK.                         
         OI    BOGGNBDH+6,X'80'                                                 
         CLC   BOGSIZE,KPSIZE      DO WE HAVE TO CHANGE BOGSIZE TOO?            
         BE    MNSKP2                                                           
*                                                                               
MNSKP1   CLC   BOGSIZE,KPSIZE      DO WE HAVE TO CHANGE BOGSIZE?                
         BE    MNSKP3                                                           
         MVC   BOGSIZE,KPSIZE      CHANGE BOGSIZE BACK.                         
         OI    BOGSIZEH+6,X'80'                                                 
*                                                                               
MNSKP2   MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'ERCHGMSG),ERCHGMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         L     R2,LSTINDSP                                                      
         AR    R2,RA                                                            
         NI    6(R2),X'FF'-X'40'                                                
         LA    R2,BOGSHFLH                                                      
         OI    6(R2),X'40'                                                      
         SR    R2,RA                                                            
         ST    R2,LSTINDSP                                                      
         NI    BOGGUESH+1,X'FF'-X'20'                                           
         OI    BOGSHFLH+1,X'01'    TELL FACPAK SCREEN IS MODIFIED.              
         B     EXIT1                                                            
*                                                                               
MNSKP3   TM    FLAG,NDVLDQ         DO WE HAVE TO VALIDATE BOARD?                
*                                   (DID PLAYER JUST INPUTTED BOARD?)           
         BZ    CKGSSWRD            NO, GO AND CHECK PLAYER'S GUESS.             
*                                                                               
         BAS   RE,VLDTBRD          YES, BOARD HAS TO BE VALIDATED.              
         TM    FLAG,INBDVLDQ       TEST IF BOARD WAS VALID.                     
         BZ    EXIT1               IF NOT, EXIT PRGM FOR RE-INPUT.              
*                                                                               
         NI    FLAG,X'FF'-NDVLDQ   IF BOARD IS VALID, TURN OFF VALIDATE         
         NI    BOGSHFLH+1,X'FF'-X'20'  BIT, UNPROTECT INPUT FIELDS,             
         NI    BOGGNBDH+1,X'FF'-X'20'  AND PERFORM NEXT STEP.                   
         NI    BOGSIZEH+1,X'FF'-X'20'                                           
         NI    BOGGUESH+1,X'FF'-X'20'                                           
         OI    BOGSHFLH+6,X'80'                                                 
         OI    BOGGNBDH+6,X'80'                                                 
         OI    BOGSIZEH+6,X'80'                                                 
         OI    BOGGUESH+6,X'80'                                                 
*                                                                               
         B     GTTABADD             BIT, AND PERFORM NEXT STEP.                 
*                                                                               
         EJECT                                                                  
*                                                                               
* RESHUFFLE = YES                                                               
*                                                                               
YSHUFFLE MVC   BOGGUES,BLANKS                                                   
         OI    BOGGUESH+6,X'80'    CLEAR GUESS FIELD.                           
*                                                                               
         XC    INTBOARD,INTBOARD                                                
         MVI   PWRDCNT,0           ZERO PLAYER'S WORD COUNTER.                  
         MVI   CWRDCNT,0           ZERO COMPUTER'S WORD COUNTER.                
         LA    R1,150                                                           
         LA    R2,DICTBRD                                                       
CLRLOOP1 XC    0(L'DICTBRD,R2),0(R2) CLEAR BOARD'S DICTIONARY.                  
         LA    R2,L'DICTBRD(R2)                                                 
         BCT   R1,CLRLOOP1                                                      
*                                                                               
         LA    R1,50                                                            
         LA    R2,PWORDS                                                        
CLRLOOP2 XC    0(L'PWORDS,R2),0(R2)  CLEAR LIST OF PLAYER'S WORDS.              
         LA    R2,L'PWORDS(R2)                                                  
         BCT   R1,CLRLOOP2                                                      
*                                                                               
         LA    R1,40                                                            
         LA    R2,BOGPWRDH                                                      
CLRLOOP3 XC    8(5,R2),8(R2)       CLEAR PLAYER'S WORDS ON SCREEN.              
         OI    6(R2),X'80'         TRANSMIT CLEARED FIELDS.                     
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         BCT   R1,CLRLOOP3                                                      
*                                                                               
         LA    R1,94                                                            
         LA    R2,BOGCWRDH                                                      
CLRLOOP4 XC    8(5,R2),8(R2)       CLEAR COMPUTER'S WORDS ON SCREEN.            
         OI    6(R2),X'80'         TRANSMIT CLEARED FIELDS.                     
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         BCT   R1,CLRLOOP4                                                      
*                                                                               
         TM    FLAG,NEWGAMEQ       IS THIS A NEW GAME?                          
         BZ    CKWHOGEN                                                         
*                                                                               
         CLI   BOGSIZE,C'4'        HAVE TO SEE WHAT NEW SIZE IS.                
         BNE   CKSIZE5                                                          
*                                                                               
         MVI   BOARDSZE,4          SET BOARD SIZE = 4                           
         MVI   NUMCELL,16          SET # OF CELLS =16                           
         MVI   KPSIZE,C'4'         NOW, PREV SIZE = 4                           
         MVC   LSTDSWRD,=X'0010100F0F0E0E0D0D0A0A00'                            
         B     CKWHOGEN                                                         
*                                                                               
CKSIZE5  CLI   BOGSIZE,C'5'        SEE IF PLAYER WANTS 5X5 BOARD.               
         BNE   INVLDSZE                                                         
*                                                                               
         MVI   BOARDSZE,5          SET BOARD SIZE = 5                           
         MVI   NUMCELL,25          SET # OF CELLS =25                           
         MVI   KPSIZE,C'5'         NOW, PREV SIZE = 5                           
         MVC   LSTDSWRD,=X'001919181817171616151500'                            
         B     CKWHOGEN                                                         
*                                                                               
INVLDSZE MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'ERSZEMSG),ERSZEMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         L     R1,LSTINDSP         SET INSERT CURSOR POSITION                   
         AR    R1,RA                                                            
         NI    6(R1),X'FF'-X'40'                                                
         LA    R1,BOGSIZEH                                                      
         OI    6(R1),X'40'                                                      
         SR    R1,RA                                                            
         ST    R1,LSTINDSP                                                      
         B     EXIT1                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* SEE WHO GENERATES BOARD                                                       
*                                                                               
CKWHOGEN MVI   BOGSHFL,C'N'        CHANGE RESHUFFLE OPTION TO NO.               
         OI    BOGSHFLH+6,X'80'                                                 
         NI    FLAG,X'FF'-NEWGAMEQ TURN OFF NEWGAME FLAG.                       
         CLI   BOARDSZE,4          CHECK WHAT SIZE BOARD TO DRAW.               
         BNE   SKIP1                                                            
*                                                                               
         BAS   RE,MAKE4BRD         DRAW 4X4 GAME BOARD.                         
         BAS   RE,TRANSBRD                                                      
         B     SKIP2                                                            
*                                                                               
SKIP1    BAS   RE,MAKE5BRD         DRAW 5X5 GAME BOARD.                         
         BAS   RE,TRANSBRD                                                      
*                                                                               
SKIP2    CLI   BOGGNBD,C'P'        DOES PLAYER WANT TO GENERATE BOARD?          
         BE    PLYRGEN                                                          
*                                                                               
COMPGEN  NI    FLAG,X'FF'-NDVLDQ   DON'T NEED TO VALIDATE BOARD.                
         CLI   BOGGNBD,C'C'        SEE IF INPUT WAS REALLY VALID.               
         BE    SKIP3                                                            
         MVI   BOGGNBD,C'C'                                                     
         OI    BOGGNBDH+6,X'80'                                                 
*                                                                               
SKIP3    MVI   KPGNBD,C'C'                                                      
         GOTO1 VCALLOV,DMCB,(2,0),0   TABLES ARE IN 2ND OVERLAY.                
         CLI   BOARDSZE,4                                                       
         BNE   SKIP4                                                            
*                                                                               
         L     R2,0(R1)            R2=A(LENGTH OF ADJDIE TABLE).                
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE TABLE).                     
         LA    R2,2(R2)                                                         
         AH    R2,TEMPHLF          R2-->LEN(DICE4BY4 TABLE).                    
         LA    R2,2(R2)            R2-->DICE4BY4 TABLE.                         
         ST    R2,ADDLTRDI         ADDLTRDI=A(LETTERS ON DICE DSTRUCT)          
         B     SKIP5                                                            
*                                                                               
SKIP4    L     R2,0(R1)                                                         
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE4 TABLE).                    
         LA    R2,2(R2)            R2-->ADJ DICE TABLE FOR 4X4.                 
         AH    R2,TEMPHLF          R2-->LENGTH OF DICE4BY4 TABLE.               
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(DICE4BY4 TABLE).                   
         LA    R2,2(R2)            R2-->DICE4BY4 TABLE.                         
         AH    R2,TEMPHLF          R2--LENGTH OF ADJDIE5 TABLE.                 
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE5 TABLE).                    
         LA    R2,2(R2)            R2-->ADJ DICE TABLE FOR 5X5.                 
         AH    R2,TEMPHLF          R2-->LENGTH(DICE5BY5 TABLE).                 
         LA    R2,2(R2)            R2-->DICE5BY5 TABLE.                         
         ST    R2,ADDLTRDI         ADDLTRDI=A(LETTERS ON DICE DSTRUCT).         
*                                                                               
SKIP5    BAS   RE,CMPGENBD         DEFAULT TO COMPUTER W/. INPUT<>'P'.          
         BAS   RE,TRANSBRD                                                      
         B     GTTABADD                                                         
*                                                                               
PLYRGEN  MVI   KPGNBD,C'P'                                                      
         OI    FLAG,NDVLDQ         NEED TO VALIDATE BOARD AFTER PLAYER          
         L     RF,LSTINDSP          INPUTS BOARD.                               
         AR    RF,RA                                                            
         NI    6(RF),X'BF'         UPDATE ADDRESS OF INSERT CURSOR.             
         LA    RF,BOGROW1H         RF-->A WALL FIELD.                           
         ZIC   R1,0(RF)            NEED TO BUMP R8 TO 1ST CUBE FIELD.           
         AR    RF,R1               RF-->CUBE FIELD.                             
         OI    6(RF),X'40'         POSITION CURSOR AT FIRST CUBE.               
         SR    RF,RA                                                            
         ST    RF,LSTINDSP                                                      
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'ENBRDMSG),ENBRDMSG                                     
         OI    BOGHEADH+6,X'80'    TRANSMIT THE MESSAGE.                        
         MVI   DMCB+3,0            0 TO INDICATE UNPROTECT BOARD.               
*******  LA    R1,DMCB                                                          
         MVI   3(R1),0             0 TO INDICATE UNPROTECT BOARD.               
         BAS   RE,BRDPRTCT                                                      
         BAS   RE,TRANSBRD                                                      
         OI    BOGSHFLH+1,X'20'    TURN PROTECT ON.                             
         OI    BOGSHFLH+6,X'80'                                                 
         OI    BOGGNBDH+1,X'20'                                                 
         OI    BOGGNBDH+6,X'80'                                                 
         OI    BOGSIZEH+1,X'20'                                                 
         OI    BOGSIZEH+6,X'80'                                                 
         OI    BOGGUESH+1,X'20'                                                 
         OI    BOGGUESH+6,X'80'                                                 
         OI    BOGSVRQH+1,X'01'    TELL FACPAK SCREEN IS MODIFIED.              
         B     EXIT1               LET PLAYER INPUT BOARD                       
*                                                                               
         EJECT                                                                  
*                                                                               
* TASKS TO DO BEFORE USER GUESSES WORD.                                         
*                                                                               
** GET ADDRESSES OF TABLES IN OVERLAY                                           
*                                                                               
GTTABADD GOTO1 VCALLOV,DMCB,(1,0),0   GET ADDRESS OF DICTIONARY.                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)            R2=A(DICTIONARY)                             
         MVC   DICTLEN,0(R2)       DICTLEN=# OF WORDS IN DICTIONARY.            
         LA    R2,2(R2)            R2-->1ST WORD IN DICTIONARY.                 
         ST    R2,DICTADD          DICTADD=A(FIRST WORD).                       
*                                                                               
         GOTO1 VCALLOV,DMCB,(2,0),0   TABLES ARE IN 2ND OVERLAY.                
         CLI   BOARDSZE,4                                                       
         BNE   GTTB5ADD                                                         
*                                                                               
         L     R2,0(R1)            R2=A(LENGTH OF ADJDIE TABLE).                
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE TABLE).                     
         LA    R2,2(R2)                                                         
         ST    R2,ADDADJDI         ADDADJDI=A(ADJ DICE TABLE FOR 4X4).          
         AH    R2,TEMPHLF          R2-->DICE4BY4 TABLE.                         
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(DICE4BY4 TABLE).                   
         LA    R2,2(R2)                                                         
         ST    R2,ADDLTRDI         ADDLTRDI=A(LETTERS ON DICE DSTRUCT)          
         B     PREGUESS                                                         
*                                                                               
GTTB5ADD L     R2,0(R1)                                                         
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE4 TABLE).                    
         LA    R2,2(R2)            R2-->ADJ DICE TABLE FOR 4X4.                 
         AH    R2,TEMPHLF          R2-->LENGTH OF DICE4BY4 TABLE.               
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(DICE4BY4 TABLE).                   
         LA    R2,2(R2)            R2-->DICE4BY4 TABLE.                         
         AH    R2,TEMPHLF          R2--LENGTH OF ADJDIE5 TABLE.                 
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE5 TABLE).                    
         LA    R2,2(R2)            R2-->ADJ DICE TABLE FOR 5X5.                 
         ST    R2,ADDADJDI         ADDADJDI=A(ADJ DICE TABLE FOR 5X5)           
         AH    R2,TEMPHLF          R2-->LEN(DICE5BY5 TABLE).                    
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(DICE5BY5 TABLE).                   
         LA    R2,2(R2)            R2-->DICE5BY5 TABLE.                         
         ST    R2,ADDLTRDI         ADDLTRDI=A(LETTERS ON DICE DSTRUCT).         
*                                                                               
** PREPARE SCREEN AND BUILD WORDS FROM BOARD                                    
*                                                                               
PREGUESS LA    R1,DMCB                                                          
         MVI   3(R1),1             1 TO INDICATE PROTECT BOARD.                 
         BAS   RE,BRDPRTCT                                                      
         BAS   RE,MVINTBRD         MOVE BOARD INTO INTBOARD.                    
         BAS   RE,BLDWORDS         BUILD DICTBRD FROM BOARD.                    
         BAS   RE,TRANSBRD         TRANSMIT ENTIRE BOARD.                       
         CLI   CWRDCNT,0                                                        
         BH    USRINPT                                                          
*                                                                               
* COMPUTER DID NOT FIND ANY WORDS                                               
*                                                                               
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'NOWD1MSG),NOWD1MSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         MVI   BOGSHFL,C'Y'                                                     
         L     R6,LSTINDSP                                                      
         AR    R6,RA                                                            
         NI    6(R6),X'FF'-X'40'                                                
         LA    R6,BOGSHFLH                                                      
         OI    6(R6),X'C1'                                                      
         SR    R6,RA                                                            
         ST    R6,LSTINDSP                                                      
         OI    FLAG,NEWGAMEQ                                                    
         OI    BOGGUESH+1,X'20'    PROTECT GUESS FIELD.                         
         OI    BOGGUESH+6,X'80'                                                 
         B     EXIT1                                                            
*                                                                               
USRINPT  L     R8,LSTINDSP                                                      
         AR    R8,RA                                                            
         NI    6(R8),X'FF'-X'40'   UPDATE ADDRESS OF INSERT CURSOR.             
         LA    R8,BOGGUESH         R8-->GUESS WORD FIELD.                       
         SR    R8,RA                                                            
         ST    R8,LSTINDSP                                                      
         NI    BOGGUESH+1,X'FF'-X'20'  UNPROTECT GUESSING FIELD.                
         OI    BOGGUESH+6,X'C0'    POSITION CURSOR AT GUESS PLACE.              
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'ENGSSMSG),ENGSSMSG                                     
         OI    BOGHEADH+1,X'08'    HIGH INTENSITY.                              
         OI    BOGHEADH+6,X'80'    TRANSMIT HEADER FIELD.                       
         MVI   BOGSHFL,C'N'                                                     
         OI    BOGSHFLH+6,X'80'    TRANSMIT SHUFFLE FIELD.                      
         B     EXIT1               LET USER INPUT GUESS.                        
*                                                                               
         EJECT                                                                  
*                                                                               
* CHECK WORD USER INPUTTED AGAINST DICTBRD                                      
*                                                                               
CKGSSWRD CLI   BOGGUES,C'#'        DOES PLAYER WANT TO KNOW # OF WORDS          
         BNE   CKGSS2                                                           
*                                                                               
         ZIC   R2,CWRDCNT                                                       
         ZIC   R3,PWRDCNT                                                       
         SR    R2,R3                                                            
         LTR   R2,R2               MAKE SURE R2 IS NOT NEGATIVE.                
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R8,BOGHEAD                                                       
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(7),=C'I HAVE '                                           
         LA    R8,7(R8)                                                         
         EDIT  (R2),(3,(R8)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R8,R0                                                            
         CLI   PWRDCNT,0                                                        
         BE    CKGS01                                                           
         MVC   1(13,R8),=C'WORD(S) LEFT.'                                       
         B     CKGS02                                                           
*                                                                               
CKGS01   MVC   1(8,R8),=C'WORD(S).'                                             
CKGS02   OI    BOGHEADH+6,X'80'                                                 
         B     PREPNXWD                                                         
*                                                                               
CKGSS2   CLI   BOGGUES,C'!'        DOES PLAYER WANT TO GIVE UP?                 
         BE    MAINGVUP                                                         
*                                                                               
         CLI   BOGGUESH+5,5        PLAYER WANTS WORD VALIDATED.                 
         BE    VLDTWRD             CHECK LENGTH OF INPUT.                       
*                                                                               
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'ERLENMSG),ERLENMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         B     PREPNXWD                                                         
*                                                                               
* PLAYER GIVES UP                                                               
*                                                                               
MAINGVUP BAS   RE,SHOWWRDS                                                      
         OI    FLAG,NEWGAMEQ       SET BRAND NEW GAME FLAG TRUE.                
         L     R2,LSTINDSP                                                      
         AR    R2,RA                                                            
         NI    6(R2),X'FF'-X'40'   TURN OFF POSITIONING CURSOR                  
         LA    R2,BOGSHFLH                                                      
         MVI   8(R2),C'Y'          DEFAULT RESHUFFLE TO YES.                    
         OI    6(R2),X'C0'         TRANSMIT AND POSITION CURSOR.                
         SR    R2,RA                                                            
         ST    R2,LSTINDSP                                                      
         MVC   BOGGUES,BLANKS                                                   
         OI    BOGGUESH+1,X'20'    PROTECT GUESS FIELD.                         
         OI    BOGGUESH+6,X'80'    TRANSMIT THE BLANKS IN GUESS FIELD.          
         OI    BOGSHFLH+1,X'01'    TELL FACPAK SCREEN IS MODIFIED.              
         B     EXIT1                                                            
*                                                                               
VLDTWRD  ZIC   R2,CWRDCNT                                                       
         GOTO1 =V(BINSRCH),DMCB,(X'00',BOGGUES),DICTBRD,(R2),          +        
               5,(0,5),75,RR=RELO                                               
         TM    0(R1),X'01'         IS PLAYER'S WORD FOUND?                      
         BO    BADWORD                                                          
*                                                                               
* WORD WAS VALID                                                                
*                                                                               
         ZIC   R2,PWRDCNT                                                       
         GOTO1 =V(BINSRCH),DMCB,(X'01',BOGGUES),PWORDS,(R2),           +        
               5,(0,5),30,RR=RELO   APPEND PLAYER'S WORD TO HIS LIST.           
         TM    0(R1),X'01'                                                      
         BO    ADDPWORD            TESTED TO SEE IF PLAYER                      
*                                   REPEATED WORD.                              
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(L'ERRPTMSG),ERRPTMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         B     PREPNXWD                                                         
*                                                                               
ADDPWORD LA    R2,1(R2)            INCREMENT PWRDCNT ONLY IF WORD               
         STC   R2,PWRDCNT           IS NOT ON LIST YET.                         
         LR    R1,R2               PREPARE TO BUMP TO NEXT AVAILABLE            
         LA    R2,BOGPWRDH          SPOT ON SCREEN TO PUT WORD.                 
*                                                                               
         CLI   PWRDCNT,1                                                        
         BE    PUTPWORD                                                         
*                                                                               
         BCTR  R1,0                                                             
BMPWRDLP ZIC   R0,0(R2)            R0=LENGTH OF FIELD POINTED BY R2.            
         AR    R2,R0                                                            
         BCT   R1,BMPWRDLP                                                      
*                                                                               
** R2-->FIRST AVAILABLE POSITION ON SCREEN.                                     
*                                                                               
PUTPWORD MVC   8(5,R2),BOGGUES     MOVE GUESSED WORD ONTO SCREEN.               
         OI    6(R2),X'80'         TRANSMIT FIELD.                              
         MVC   BOGHEAD,BLANKS                                                   
         CLC   CWRDCNT,PWRDCNT     DID PLAYER GET ALL MY WORDS?                 
         BE    MNSKP4                                                           
*                                                                               
         MVC   BOGHEAD(L'ENGS1MSG),ENGS1MSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         B     PREPNXWD                                                         
*                                                                               
MNSKP4   MVC   BOGHEAD(L'ALWRDMSG),ALWRDMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         B     MAINGVUP                                                         
*                                                                               
* WORD WAS INVALID, NEED TO DETERMINE WHAT KIND IT IS                           
*                                                                               
BADWORD  MVC   BOGHEAD,BLANKS      PREPARE FOR ERROR MESSAGE.                   
*                                                                               
** CHECK FOR INVALID LETTER IN PLAYER'S WORD.                                   
*                                                                               
         LA    R2,BOGGUES          CHECK EACH OF THE 5 CHARACTERS.              
CKLTR1   OI    0(R2),X'40'         "CAPITALIZE" THE CHARACTER.                  
         CLI   0(R2),C'A'                                                       
         BL    INVLDLTR                                                         
         CLI   0(R2),C'I'                                                       
         BNH   NEXTLTR                                                          
         CLI   0(R2),C'J'                                                       
         BL    INVLDLTR                                                         
         CLI   0(R2),C'R'                                                       
         BNH   NEXTLTR                                                          
         CLI   0(R2),C'S'                                                       
         BL    INVLDLTR                                                         
         CLI   0(R2),C'Z'                                                       
         BH    INVLDLTR                                                         
NEXTLTR  LA    R2,1(R2)                                                         
         CLC   0(1,R2),BOGGUES+5                                                
         BNE   CKLTR1                                                           
         B     BADW01              GO CHECK FOR PATTERN FOUND.                  
*                                                                               
INVLDLTR MVC   BOGHEAD(1),0(R2)    INVALID LETTER ENCOUNTERED.                  
         MVC   BOGHEAD+2(L'ERLETMSG),ERLETMSG                                   
         OI    BOGHEADH+6,X'80'                                                 
         B     PREPNXWD                                                         
*                                                                               
** CHECK FOR WORD NOT FOUND ON BOARD ERROR.                                     
*                                                                               
BADW01   NI    FLAG,X'FF'-PTRNFNDQ ASSUME PATTERN NOT FOUND                     
         BAS   RE,CKPTTRN                                                       
         TM    FLAG,PTRNFNDQ                                                    
         BO    BADW02              PATTERN WAS FOUND, CHECK NEXT                
*                                   POSSIBLE ERROR.                             
         MVC   BOGHEAD(5),BOGGUES                                               
         MVC   BOGHEAD+6(L'ERPATMSG),ERPATMSG                                   
         OI    BOGHEADH+6,X'80'                                                 
         B     PREPNXWD                                                         
*                                                                               
** CHECK FOR NOT IN BIG DICTIONARY ERROR                                        
*                                                                               
BADW02   GOTO1 VCALLOV,DMCB,(1,0),0                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)            R2=A(DICTIONARY)                             
         MVC   DICTLEN,0(R2)       DICTLEN=# OF WORDS IN DICTIONARY.            
         LA    R2,2(R2)            R2-->1ST WORD IN DICTIONARY.                 
         ST    R2,DICTADD          DICTADD=A(FIRST WORD).                       
*                                                                               
         LH    R2,DICTLEN                                                       
         GOTO1 =V(BINSRCH),DMCB,(X'00',BOGGUES),DICTADD,(R2),          +        
               5,(0,5),,RR=RELO                                                 
         TM    0(R1),X'01'                                                      
         BO    *+6                                                              
         DC    H'0'              PATTERN SHOULD NOT BE IN DICTIONARY.           
         ZIC   R2,BOGGUESH+5                                                    
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   BOGHEAD(0),BOGGUES  PUT GUESSED WORD IN HEADER.                  
         LA    R2,2(R2)                                                         
         LA    R2,BOGHEAD(R2)                                                   
         MVC   0(L'ERWRDMSG,R2),ERWRDMSG  APPEND MESSAGE AFTER WORD.            
         OI    BOGHEADH+6,X'80'    TRANSMIT HEAD FIELD.                         
*                                                                               
* PREPARE FOR NEXT PLAYER'S ENTRY                                               
*                                                                               
PREPNXWD L     R2,LSTINDSP         PREPARE TO MOVE LSTINDSP                     
         AR    R2,RA                                                            
         NI    6(R2),X'BF'          TO WORD-GUESS FIELD.                        
         LA    R2,BOGGUESH                                                      
         NI    1(R2),X'FF'-X'20'   UNPROTECT GUESS FIELD.                       
         OI    6(R2),X'C0'         POSITION CURSOR TO GUESS FIELD,              
         SR    R2,RA                AND TRANSMIT FIELD.                         
         ST    R2,LSTINDSP         LSTINDSP UPDATED.                            
*                                                                               
         B     EXIT1                                                            
*                                                                               
EXIT1    XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*----------------------------- MAKE4BRD ------------------------------*         
MAKE4BRD NTR1                                                                   
         MVC   BOGTBDR,BLANKS                TOP BORDER                         
         MVC   BOGTBDR(L'TOPBDR4),TOPBDR4                                       
*                                                                               
         LA    R2,BOGROW1H                  DO 1ST ROW.                         
         BAS   RE,BWALLS4                                                       
         MVC   BOGDSH1,BLANKS                                                   
         MVC   BOGDSH1(L'DASH4),DASH4       1ST ROW OF DASHES                   
         LA    R2,BOGROW2H                  DO 2ND ROW.                         
         BAS   RE,BWALLS4                                                       
         MVC   BOGDSH2,BLANKS                                                   
         MVC   BOGDSH2(L'DASH4),DASH4       2ND ROW OF DASHES                   
         LA    R2,BOGROW3H                  DO 3RD ROW.                         
         BAS   RE,BWALLS4                                                       
         MVC   BOGDSH3,BLANKS                                                   
         MVC   BOGDSH3(L'DASH4),DASH4       3RD ROW OF DASHES                   
         LA    R2,BOGROW4H                  DO 4TH ROW.                         
         BAS   RE,BWALLS4                                                       
*                                                                               
         MVC   BOGDSH4,BLANKS                BOTTOM BORDER                      
         MVC   BOGDSH4(L'BOTBDR4),BOTBDR4                                       
*                                                                               
         LA    R2,BOGROW5H         CLEAR BOTTOM OF 5X5 BOARD                    
         LA    R3,11                (IF EXISTED)                                
RECLRFLD ZIC   R5,0(R2)            R5=LENGTH OF FIELD                           
         LR    R1,R5                                                            
         SH    R5,=H'8'            R5=LENGTH OF DATA FIELD.                     
         CH    R5,=H'0'                                                         
         BNE   MISC1                                                            
         DC    F'0'                                                             
MISC1    BCTR  R5,0                PREPARE TO DO EX INSTRUCTION.                
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLANKS      CLEAR OUT DATA FIELD.                        
         OI    6(R2),X'80'                                                      
         AR    R2,R1               BUMP TO NEXT FIELD.                          
         BCT   R3,RECLRFLD                                                      
*                                                                               
         MVC   BOGBBDR,BLANKS      CLEAR BOTTOM BORDER.                         
         OI    BOGBBDRH+6,X'80'                                                 
*                                                                               
         B     EXIT1               EXIT MAKE4BRD SUBROUTINE.                    
         SPACE 4                                                                
*------------------------------ BWALLS4 ------------------------------*         
*                                                                               
BWALLS4  NTR1                                                                   
*                                  ONLY MAKE4BRD CALLS THIS SUBROUTINE.         
*                                  R2-->CORRESPONDING ROW.                      
         MVI   8(R2),C'|'          LEFT WALL                                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO CUBE FIELD.                     
         MVC   9(1,R2),BLANKS      CLEAR CUBE FIELD.                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO WALL FIELD.                     
*                                                                               
         LA    R3,3                                                             
NXTWALL  MVI   8(R2),C'º'                                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO CUBE FIELD.                     
         MVC   9(1,R2),BLANKS      CLEAR CUBE FIELD.                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO WALL FIELD.                     
         BCT   R3,NXTWALL                                                       
*                                                                               
         MVI   8(R2),C'|'          RIGHT WALL                                   
*                                                                               
         ZIC   R1,0(R2)            CLEARING OUT STUFF ON THE RIGHT              
         AR    R2,R1                FROM A 5X5 BOARD (IF EXISTED).              
         MVC   8(2,R2),BLANKS      CLEARED CUBE FIELD.                          
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   8(1,R2),BLANKS      CLEARED WALL FIELD.                          
         OI    6(R2),X'80'                                                      
*                                                                               
         B     EXIT1               EXITS BWALLS4 SUBROUTINE.                    
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*----------------------------- MAKE5BRD ------------------------------*         
MAKE5BRD NTR1                                                                   
         MVC   BOGTBDR,BLANKS                TOP BORDER                         
         MVC   BOGTBDR(L'TOPBDR5),TOPBDR5                                       
*                                                                               
         LA    R2,BOGROW1H                  DO 1ST ROW.                         
         BAS   RE,BWALLS5                                                       
         MVC   BOGDSH1,BLANKS                                                   
         MVC   BOGDSH1(L'DASH5),DASH5       1ST ROW OF DASHES                   
         LA    R2,BOGROW2H                  DO 2ND ROW.                         
         BAS   RE,BWALLS5                                                       
         MVC   BOGDSH2,BLANKS                                                   
         MVC   BOGDSH2(L'DASH5),DASH5       2ND ROW OF DASHES                   
         LA    R2,BOGROW3H                  DO 3RD ROW.                         
         BAS   RE,BWALLS5                                                       
         MVC   BOGDSH3,BLANKS                                                   
         MVC   BOGDSH3(L'DASH5),DASH5       3RD ROW OF DASHES                   
         LA    R2,BOGROW4H                  DO 4TH ROW.                         
         BAS   RE,BWALLS5                                                       
         MVC   BOGDSH4,BLANKS                                                   
         MVC   BOGDSH4(L'DASH5),DASH5       4TH ROW OF DASHES                   
         LA    R2,BOGROW5H                  DO 5TH ROW.                         
         BAS   RE,BWALLS5                                                       
*                                                                               
         MVC   BOGBBDR,BLANKS                BOTTOM BORDER                      
         MVC   BOGBBDR(L'BOTBDR5),BOTBDR5                                       
*                                                                               
         B     EXIT1               EXIT MAKE5BRD SUBROUTINE.                    
         SPACE 4                                                                
*----------------------------- BWALLS5 -------------------------------*         
*                                                                               
BWALLS5  NTR1                                                                   
*                                  ONLY MAKE5BRD CALLS THIS SUBROUTINE.         
*                                  R2-->CORRESPONDING ROW.                      
         MVI   8(R2),C'|'          LEFT WALL                                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO CUBE FIELD.                     
         MVC   9(1,R2),BLANKS      CLEAR CUBE FIELD.                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO WALL FIELD.                     
*                                                                               
         LA    R3,4                                                             
NXTWALL5 MVI   8(R2),C'º'                                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO CUBE FIELD.                     
         MVC   9(1,R2),BLANKS      CLEAR CUBE FIELD.                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 BUMPED TO WALL FIELD.                     
         BCT   R3,NXTWALL5                                                      
*                                                                               
         MVI   8(R2),C'|'          RIGHT WALL                                   
*                                                                               
         B     EXIT1               EXITS BWALLS5 SUBROUTINE.                    
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------- COMPUTER GENERATING BOARD SUBROUTINE ---------------*         
*                                                                               
CMPGENBD NTR1                                                                   
         XC    NDIEUSED,NDIEUSED   ZERO DICE HAVE BEEN USED.                    
         XC    DICEUSED,DICEUSED   NONE OF DICE HAS BEEN USED.                  
*                                                                               
         LA    R2,BOGROW1H                                                      
         BAS   RE,GENROW                                                        
         LA    R2,BOGROW2H                                                      
         BAS   RE,GENROW                                                        
         LA    R2,BOGROW3H                                                      
         BAS   RE,GENROW                                                        
         LA    R2,BOGROW4H                                                      
         BAS   RE,GENROW                                                        
*                                                                               
         CLI   BOARDSZE,4                                                       
         BE    XCMPGNBD            DON'T NEED TO DO 5TH ROW.                    
*                                                                               
         LA    R2,BOGROW5H                                                      
         BAS   RE,GENROW                                                        
*                                                                               
XCMPGNBD B     EXIT1                                                            
         SPACE 4                                                                
*------------------------------- GENROW ------------------------------*         
*                                                                               
* COMPUTER GENERATING ROW OF LETTERS SUBROUTINE *                               
*                                                                               
         USING FLDHDRD,R2                                                       
GENROW   NTR1                                                                   
         ZIC   R3,BOARDSZE                                                      
         ZIC   R1,FLDLEN                                                        
         AR    R2,R1               R2-->1ST CUBE FIELD IN ROW.                  
*                                                                               
* PICK RANDOM DIE                                                               
*                                                                               
RERANDOM ZIC   R5,NUMCELL                                                       
         BCTR  R5,0                                                             
         GOTO1 =V(RANDOM),DMCB,(R5),,RR=RELO                                    
         L     R5,4(R1)                                                         
         LA    R5,1(R5)                                                         
         GOTO1 =V(RANDOM),DMCB,100,,RR=RELO       MAKING NUMBERS                
         L     R4,4(R1)                            REALLY RANDOM.               
         LA    R4,1(R4)                                                         
         MR    R4,R4                                                            
         ZIC   R6,NUMCELL                                                       
         DR    R4,R6               R4=REMAINDER AFTER DIVISION.                 
         LR    R5,R4                                                            
         LA    R5,1(R5)                                                         
         STC   R5,RANDNUM                                                       
*               RANDNUM WILL CONTAIN RANDOM NUMBER, 1-(16 OR 25)                
*                                                                               
         LA    R7,DICEUSED                                                      
         CLI   NDIEUSED,0                                                       
         BE    STOREDIE                                                         
         ZIC   R6,NDIEUSED                                                      
DICELOOP CLC   RANDNUM,0(R7)       SEE IF RANDOM DIE IS ALREADY USED.           
         BE    RERANDOM                                                         
         LA    R7,1(R7)                                                         
         BCT   R6,DICELOOP                                                      
*                                                                               
STOREDIE STC   R5,0(R7)            STORE NEW DIE IN USED-DICE LIST.             
         ZIC   R6,NDIEUSED                                                      
         LA    R6,1(R6)                                                         
         STC   R6,NDIEUSED                                                      
*                                                                               
* BUMP TO SELECTED CUBE                                                         
*                                                                               
         BCTR  R5,0                                                             
         MH    R5,=H'6'            6 FACES PER CUBE.                            
         L     R4,ADDLTRDI         R4=A(CORRESPONDING DICENBYN TABLE)           
         AR    R4,R5               R4-->START OF THE RANDOM DIE.                
*                                                                               
*PICK RANDOM FACE ON DIE                                                        
*                                                                               
         GOTO1 =V(RANDOM),DMCB,5,,RR=RELO                                       
         L     R5,4(R1)                                                         
         AR    R4,R5               R4-->RANDOM LETTER ON RANDOM DIE.            
*                                                                               
         CLI   0(R4),C'Q'                                                       
         BE    SHOWQU                                                           
         MVC   8(1,R2),0(R4)                                                    
         B     *+10                                                             
SHOWQU   MVC   8(2,R2),=X'D8A4'    (CAP)Q(LOWER)U.                              
*                                                                               
         ZIC   R1,FLDLEN                                                        
         AR    R2,R1               R2 BUMPED TO "WALL" FIELD.                   
         ZIC   R1,FLDLEN                                                        
         AR    R2,R1               R2-->A CUBE FIELD.                           
         BCT   R3,RERANDOM                                                      
*                                                                               
         B     EXIT1                                                            
         DROP  R2                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------------- PROTECT/UNPROTECT BOARD ----------------------*         
BRDPRTCT NTR1                                                                   
*               UNPROTECT CELLS OF BOARD SO USER CAN INPUT LETTERS,             
*                OR PROTECT CELLS FROM BEING OVERWRITTEN.                       
*                                  3(R1)=0=>UNPROTECT BOARD.                    
*                                  3(R1)=1=>PROTECT BOARD.                      
*                                                                               
* START PROTECTING/UNPROTECTING ROWS                                            
*                                                                               
         ZIC   R2,BOARDSZE                                                      
         LR    R3,R2                                                            
         LA    R5,BOGROW1H         R5-->WALL FIELD.                             
*                                                                               
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
*                                                                               
OUTLOOP1 CLI   3(R1),1                                                          
         BNE   BRDPRT1                                                          
         OI    1(R5),X'20'         TURN ON PROTECT.                             
         B     *+8                                                              
BRDPRT1  NI    1(R5),X'FF'-X'20'   TURN OFF PROTECT.                            
         ZIC   R0,0(R5)            R0=LENGTH(CUBE FIELD).                       
         AR    R5,R0               R5-->WALL FIELD.                             
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
         BCT   R3,OUTLOOP1                                                      
*                                                                               
         LR    R3,R2                                                            
         LA    R5,BOGROW2H         R5-->WALL FIELD.                             
*                                                                               
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
*                                                                               
OUTLOOP2 CLI   3(R1),1                                                          
         BNE   BRDPRT2                                                          
         OI    1(R5),X'20'         TURN ON PROTECT.                             
         B     *+8                                                              
BRDPRT2  NI    1(R5),X'FF'-X'20'   TURN OFF PROTECT.                            
         ZIC   R0,0(R5)            R0=LENGTH(CUBE FIELD).                       
         AR    R5,R0               R5-->WALL FIELD.                             
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->NEXT CUBE FIELD.                        
         BCT   R3,OUTLOOP2                                                      
*                                                                               
         LR    R3,R2                                                            
         LA    R5,BOGROW3H         R5-->WALL FIELD.                             
*                                                                               
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
*                                                                               
OUTLOOP3 CLI   3(R1),1                                                          
         BNE   BRDPRT3                                                          
         OI    1(R5),X'20'         TURN ON PROTECT.                             
         B     *+8                                                              
BRDPRT3  NI    1(R5),X'FF'-X'20'   TURN OFF PROTECT.                            
         ZIC   R0,0(R5)            R0=LENGTH(CUBE FIELD).                       
         AR    R5,R0               R5-->WALL FIELD.                             
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
         BCT   R3,OUTLOOP3                                                      
*                                                                               
         LR    R3,R2                                                            
         LA    R5,BOGROW4H         R5-->WALL FIELD.                             
*                                                                               
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
*                                                                               
OUTLOOP4 CLI   3(R1),1                                                          
         BNE   BRDPRT4                                                          
         OI    1(R5),X'20'         TURN ON PROTECT.                             
         B     *+8                                                              
BRDPRT4  NI    1(R5),X'FF'-X'20'   TURN OFF PROTECT.                            
         ZIC   R0,0(R5)            R0=LENGTH(CUBE FIELD).                       
         AR    R5,R0               R5-->WALL FIELD.                             
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
         BCT   R3,OUTLOOP4                                                      
*                                                                               
         CLI   BOARDSZE,5          SEE IF WE HAVE TO DO 5TH ROW.                
         BNE   XBRDPRT                                                          
*                                                                               
         LR    R3,R2                                                            
         LA    R5,BOGROW5H         R5-->WALL FIELD.                             
*                                                                               
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
*                                                                               
OUTLOOP5 CLI   3(R1),1                                                          
         BNE   BRDPRT5                                                          
         OI    1(R5),X'20'         TURN ON PROTECT.                             
         B     *+8                                                              
BRDPRT5  NI    1(R5),X'FF'-X'20'   TURN OFF PROTECT.                            
         ZIC   R0,0(R5)            R0=LENGTH(CUBE FIELD).                       
         AR    R5,R0               R5-->WALL FIELD.                             
         ZIC   R0,0(R5)            R0=LENGTH(WALL FIELD).                       
         AR    R5,R0               R5-->CUBE FIELD.                             
         BCT   R3,OUTLOOP5                                                      
*                                                                               
XBRDPRT  B     EXIT1                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------------------- VALIDATE BOARD -------------------------*         
VLDTBRD  NTR1                                                                   
*               VALIDATES BOARD INPUTTED BY USER,                               
*                AND SETS BIT 0 OF FLAG CORRESPONDINGLY.                        
         OI    FLAG,INBDVLDQ       ASSUME VALID BOARD AT FIRST.                 
         CLI   BOARDSZE,4                                                       
         BNE   VLDTE5                                                           
*                                                                               
         LA    R2,BOGROW1H         VALIDATE 1ST ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW2H         VALIDATE 2ND ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW3H         VALIDATE 3RD ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW4H         VALIDATE 4TH ROW.                            
         BAS   RE,VLDTROW                                                       
         B     XVLDTBRD                                                         
*                                                                               
VLDTE5   LA    R2,BOGROW1H         VALIDATE 1ST ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW2H         VALIDATE 2ND ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW3H         VALIDATE 3RD ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW4H         VALIDATE 4TH ROW.                            
         BAS   RE,VLDTROW                                                       
         TM    FLAG,INBDVLDQ                                                    
         BZ    XVLDTBRD                                                         
         LA    R2,BOGROW5H         VALIDATE 5TH ROW.                            
         BAS   RE,VLDTROW                                                       
*                                                                               
XVLDTBRD B     EXIT1                                                            
         SPACE 4                                                                
*---------------------- VALIDATE ROW ---------------------------------*         
VLDTROW  NTR1                                                                   
*               VALIDATES SINGLE ROW IN BOARD, AND SETS BIT 0 OF FLAG.          
*               ONLY CALLED BY VLDTBRD.                                         
*               R2-->ROW                                                        
         ZIC   R3,BOARDSZE                                                      
*                                                                               
         USING FLDHDRD,R2                                                       
         ZIC   R1,FLDLEN                                                        
         AR    R2,R1               R2-->A CUBE FIELD.                           
*                                                                               
VLDTLOOP CLI   FLDILEN,0           SEE IF ANYTHING INPUTTED AT ALL.             
         BE    INVLDBRD                                                         
*                                                                               
CKILEN1  CLI   FLDILEN,1                                                        
         BNE   CHECKQU                                                          
*                                                                               
         LA    R4,ALPHABET                                                      
*                                                                               
VLDT01   CLI   0(R4),C'/'                                                       
         BE    INVLDBRD                                                         
         OI    8(R2),X'40'         CAPITALIZE LETTERS.                          
         CLI   8(R2),C'Q'                                                       
         BNE   VLDT02                                                           
         MVI   9(R2),X'A4'         X'A4'=LOWER CASE 'U'                         
VLDT02   CLC   8(1,R2),0(R4)                                                    
         BE    NXTFIELD                                                         
         LA    R4,1(R4)                                                         
         B     VLDT01                                                           
*                                                                               
CHECKQU  OI    8(R2),X'40'         CAPITALIZE 'Q' (IF ANY)                      
         OI    9(R2),X'40'         CAPITALIZE 'U' (IF ANY)                      
         CLC   8(2,R2),=C'QU'                                                   
         BNE   INVLDBRD                                                         
         MVI   9(R2),X'A4'         LOWER CASE 'U'                               
*                                                                               
NXTFIELD ZIC   R1,FLDLEN                                                        
         AR    R2,R1               R2--> WALL FIELD                             
         ZIC   R1,FLDLEN                                                        
         AR    R2,R1               R2-->CUBE FIELD.                             
         BCT   R3,VLDTLOOP                                                      
*                                                                               
* READING THIS PART => BOARD IS VALID, EXIT SUBROUTINE                          
*                                                                               
         B     XVLDTBRD                                                         
*                                                                               
INVLDBRD NI    FLAG,X'FF'-INBDVLDQ BOARD IS NOT VALID.                          
         L     R4,LSTINDSP                                                      
         AR    R4,RA                                                            
         NI    6(R4),X'FF'-X'40'   TURN OFF POS. CURSOR BIT.                    
         LR    R4,R2               R2-->CUBE FIELD.                             
         NI    6(R4),X'FF'-X'20'   UNPROTECT CUBE FIELD.                        
         OI    6(R4),X'C0'         TURN ON POS. CURSOR BIT AT THE               
         SR    R4,RA                PLACE WHERE INVALIDITY OCCURRED.            
         ST    R4,LSTINDSP                                                      
         MVC   BOGHEAD,BLANKS                                                   
         MVC   BOGHEAD(2),8(R2)                                                 
         MVC   BOGHEAD+3(L'ERLETMSG),ERLETMSG                                   
         OI    BOGHEADH+6,X'80'                                                 
*                                                                               
XVLDTROW B     EXIT1                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*-------------------- MOVE INTO INT(ERNAL)BOARD ----------------------*         
*                                                                               
MVINTBRD NTR1                                                                   
         LA    R2,INTBOARD         INTBOARD IS THE BOARD I USE.                 
         ZIC   R1,BOARDSZE                                                      
*                                                                               
         LA    R3,BOGROW1H         MOVE 1ST ROW.                                
         BAS   RE,MOVEROW                                                       
         AR    R2,R1                                                            
*                                                                               
         LA    R3,BOGROW2H         MOVE 2ND ROW.                                
         BAS   RE,MOVEROW                                                       
         AR    R2,R1                                                            
*                                                                               
         LA    R3,BOGROW3H         MOVE 3RD ROW.                                
         BAS   RE,MOVEROW                                                       
         AR    R2,R1                                                            
*                                                                               
         LA    R3,BOGROW4H         MOVE 4TH ROW.                                
         BAS   RE,MOVEROW                                                       
         AR    R2,R1                                                            
*                                                                               
         CLI   BOARDSZE,4                                                       
         BE    MOVETST                                                          
*                                                                               
         LA    R3,BOGROW5H         MOVE 5TH ROW.                                
         BAS   RE,MOVEROW                                                       
         AR    R2,R1                                                            
*                                                                               
MOVETST  LA    R3,INTBOARD         CHECK TO SEE IF                              
         SR    R2,R3                R2 - NUMCELL = A(INTBOARD).                 
         ZIC   R3,NUMCELL          IF NOT, SOMETHING WENT WRONG.                
         CR    R2,R3                                                            
         BE    EXIT1               RETURN TO WHERE WE CAME FROM.                
         DC    H'0'                                                             
         SPACE 4                                                                
*------------------------- MOVEROW SUBROUTINE ------------------------*         
MOVEROW  NTR1                                                                   
*               MOVES ROWS FROM DISPLAY BOARD TO INTBOARD.                      
*               ONLY CALLED BY MVINTBRD.                                        
*                                  R3-->ROW IN BOARD.                           
*                                  R2-->INTBOARD.                               
         USING FLDHDRD,R3                                                       
         ZIC   R4,BOARDSZE                                                      
*                                                                               
         ZIC   R1,FLDLEN                                                        
         AR    R3,R1               R3-->A CUBE FIELD.                           
*                                                                               
MOVNXDIE MVC   0(1,R2),8(R3)       R2-->INTBOARD.                               
*                                                                               
         LA    R2,1(R2)            BUMP R2 TO NEXT CELL IN INTBOARD.            
*                                                                               
         ZIC   R1,FLDLEN                                                        
         AR    R3,R1               R3-->A FIELD WITH A "WALL".                  
         ZIC   R1,FLDLEN                                                        
         AR    R3,R1               R3-->A CUBE.                                 
*                                                                               
         BCT   R4,MOVNXDIE                                                      
*                                                                               
         DROP  R3                                                               
         XIT1                                                                   
** END OF MOVEROW SUBROUTINE.                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*----------------------------- BUILD WORDS ---------------------------*         
* BUILDING WORDS FROM THE BOARD.                                                
*  WILL ALSO VALIDATE WORD BUILT, AND PUT IT IN DICTBRD.                        
*                                                                               
BLDWORDS NTR1                                                                   
         XC    DICEUSED,DICEUSED   NO DICE HAVE BEEN USED YET.                  
         XC    DSWORD,DSWORD       NO WORD HAS BEEN BUILT YET.                  
         MVI   LTRCNT,0            LETTER COUNT = 0.                            
         XC    LKUPCNT,LKUPCNT     HAVEN'T LOOKED IN DICTIONARY YET.            
         MVI   CWRDCNT,0                                                        
*                                                                               
         LA    R4,DSWORD           R4-->DSWORD.                                 
         USING DSWORDD,R4                                                       
         SR    R5,R5                                                            
*                                                                               
* STARTING TO BUILD WORDS HERE                                                  
*                                                                               
** GET (POINT R3 TO) THE NEXT ADJDIE# IN THE LIST **                            
*                                                                               
GTDIELP  L     R3,ADDADJDI         ADJDSADD=A(ADJDIE DATA STRUCTURE).           
         ZIC   R1,NUMCELL                                                       
         LA    R1,1(R1)                                                         
         STH   R1,TEMPHLF          TEMPHLF=# OF ENTRIES PER ROW                 
         MH    R5,TEMPHLF           IN CORRESPONDING ADJDIE TABLE.              
         AR    R3,R5                                                            
*                                   TABLE.  R5 CONTAINS THE DIE#.               
         CLI   NXTDIE,0            NOW POINT R3 TO THE CURRENT                  
         BE    RECHECK              ADJACENT-DIE# THE R5-TH DIE#.               
*                                                                               
BUMPR3   CLC   NXTDIE,0(R3)                                                     
         BE    NXADJDIE                                                         
         LA    R3,1(R3)            MOVE TO NEXT ADJ DIE#.                       
         B     BUMPR3                                                           
*                                                                               
NXADJDIE LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   RECHECK                                                          
*                                                                               
         MVI   NXTDIE,0                                                         
         B     CNXTDIE0                                                         
*                                                                               
* CHECK IF GOTTEN DIE IS IN DICEUSED                                            
*                                                                               
RECHECK  LA    R6,DICEUSED                                                      
RECHECK1 CLI   0(R6),0                                                          
         BE    CUBEOK              CURRENT CUBE HAS NOT BEEN USED.              
         CLC   0(1,R3),0(R6)       IS R3'S CUBE IN DICEUSED?                    
         BE    NEXTCUBE            YES IT IS, POINT R3 TO NEW CUBE.             
         LA    R6,1(R6)            IF NOT, CHECK REST OF DICEUSED.              
         B     RECHECK1                                                         
*                                                                               
NEXTCUBE LA    R3,1(R3)            POINT R3 TO NEXT ADJACENT DIE.               
         CLI   0(R3),0             IS THERE NO MORE ADJACENT DICE?              
         BNE   RECHECK                                                          
         MVI   NXTDIE,0            IF YES, GET READY TO BUMP BACK               
         B     CNXTDIE0             TO PREVIOUS LETTER.                         
*                                                                               
CUBEOK   MVC   0(1,R6),0(R3)       APPEND DIE ONTO DICEUSED.                    
         ZIC   R1,LTRCNT                                                        
         LA    R1,1(R1)            INCREASE LETTER COUNTER.                     
         STC   R1,LTRCNT                                                        
*                                                                               
         MVC   NXTDIE,0(R3)        PUT INTO WORD DATA STRUCTURE.                
         LA    R4,DSWORDDL(R4)                                                  
FRSTLTR  MVC   DIEPOS,0(R3)                                                     
*                                                                               
         CLI   LTRCNT,5            HAVE WE GOTTEN A FIFTH LETTER YET?           
         BNL   FVECUBES                                                         
*                                                                               
         ZIC   R5,0(R3)            PREPARE TO GET NEXT DIE.                     
         B     GTDIELP                                                          
         DROP  R4                                                               
*                                                                               
* BUILD ACTUAL WORD FROM WORD DATA STRUCTURE.                                   
*                                                                               
FVECUBES LA    R5,DSWORD                                                        
         USING DSWORDD,R5                                                       
         LA    R5,2(R5)            DSWORD HAS A "HEADER" WHICH IS               
         LA    R6,FIVEWORD          TWO BYTES LONG.                             
         MVI   QCOUNT,0            NO 'Q' ENCOUNTERED YET.                      
*                                                                               
         ZIC   R2,LTRCNT                                                        
MOVINWRD ZIC   R7,DIEPOS           R7=POSITION OF CURRENT DIE.                  
         LA    R7,INTBOARD(R7)                                                  
         BCTR  R7,0                R7-->LETTER OF CORRESPONDING DIE.            
         MVC   0(1,R6),0(R7)       MOVE ACTUAL LETTER INTO WORD.                
*                                                                               
         CLI   0(R7),C'Q'                                                       
         BNE   SKIPQCNT                                                         
         ZIC   R1,QCOUNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,QCOUNT                                                        
*                                                                               
SKIPQCNT LA    R5,DSWORDDL(R5)     R5-->NEXT LETTER IN WORD DS.                 
         LA    R6,1(R6)            R6-->NEXT LETTER IN FIVEWORD.                
*                                                                               
         BCT   R2,MOVINWRD                                                      
         DROP  R5                                                               
*                                                                               
** CHECK FOR ANY Q'S IN WORD                                                    
*                                                                               
         CLI   QCOUNT,0                                                         
         BE    LOOKUP              GO STRAIGHT TO LOOK-UP IN DICT.              
*                                                                               
         CLI   QCOUNT,3            CAN'T HAVE MORE THAN 3 Q'S IN WORD.          
         BNL   CNXTDIE0                                                         
*                                                                               
         CLI   FIVEWORD+4,C'Q'                                                  
         BE    CNXTDIE0            CAN'T HAVE 'Q' AS LAST LETTER.               
*                                                                               
         CLI   FIVEWORD,C'Q'       CHECK IF 1ST LETTER IS 'Q'.                  
         BNE   Q2                                                               
         MVC   FIVEWORD+4(1),FIVEWORD+3                                         
         MVC   FIVEWORD+3(1),FIVEWORD+2                                         
         MVC   FIVEWORD+2(1),FIVEWORD+1                                         
         MVI   FIVEWORD+1,C'U'                                                  
         B     Q3                                                               
*                                                                               
Q2       CLI   FIVEWORD+1,C'Q'     CHECK IF 2ND LETTER IS 'Q'.                  
         BNE   Q3                                                               
         MVC   FIVEWORD+4(1),FIVEWORD+3                                         
         MVC   FIVEWORD+3(1),FIVEWORD+2                                         
         MVI   FIVEWORD+2,C'U'                                                  
         B     Q4                                                               
*                                                                               
Q3       CLI   FIVEWORD+2,C'Q'     CHECK IF 3RD LETTER IS 'Q'.                  
         BNE   Q4                                                               
         MVC   FIVEWORD+4(1),FIVEWORD+3                                         
         MVI   FIVEWORD+3,C'U'                                                  
         B     LOOKUP                                                           
*                                                                               
Q4       CLI   FIVEWORD+3,C'Q'     CHECK IF 4TH LETTER IS 'Q'.                  
         BNE   LOOKUP                                                           
         MVI   FIVEWORD+4,C'U'                                                  
*                                                                               
** FIVEWORD NOW EQUALS WORD BUILT, LOOKUP FIVEWORD IN DICTIONARY                
*                                                                               
LOOKUP   LH    R6,DICTLEN                                                       
         GOTO1 =V(BINSRCH),DMCB,(X'00',FIVEWORD),DICTADD,(R6),         +        
               5,(0,5),(R6),RR=RELO                                             
         TM    0(R1),X'01'         CHECK FOR WORD FOUND.                        
         BO    CNXTDIE0            WORD WAS NOT FOUND.                          
*                                                                               
** FOUND A VALID WORD, PUT IT IN DICTBRD **                                     
*                                                                               
         ZIC   R6,CWRDCNT                                                       
         GOTO1 =V(BINSRCH),DMCB,(X'01',FIVEWORD),DICTBRD,(R6),         +        
               5,(0,5),75,RR=RELO                                               
*                                  ADD WORD TO DICTBRD USING BINSRCH.           
         TM    0(R1),X'01'         INCREMENT CWRDCNT.                           
         BZ    CNXTDIE0            IF WORD IS NOT FOUND, IT IS                  
         LA    R6,1(R6)             ADDED TO DICTBRD AND CWRDCNT                
         STC   R6,CWRDCNT           IS INCREMENTED.                             
*                                                                               
* PREPARE TO MAKE NEXT WORD                                                     
*                                                                               
         USING DSWORDD,R4          R4-->WORD DATA STRUCTURE.                    
CNXTDIE0 L     R1,LKUPCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,LKUPCNT          INCREMENT THE PERMUTATION COUNT.             
*                                                                               
         CLC   LSTDSWRD,DSWORD     REACHED THE LAST POSSIBLE WRD BUILT?         
         BE    XBLDWRDS            EXIT BUILDING-WORDS SUBRTNE.                 
         CLI   NXTDIE,0                                                         
         BNE   MOVEON3                                                          
*                                                                               
         LA    R6,DICEUSED                                                      
LOOP1    CLC   DIEPOS,0(R6)                                                     
         BE    FOUND                                                            
         LA    R6,1(R6)                                                         
         B     LOOP1                                                            
*                                                                               
FOUND    MVC   0(1,R6),1(R6)       "POPPING" DICEUSED STACK.                    
         CLI   0(R6),0                                                          
         BE    DONE                DELETED DIE# FROM DICEUSED.                  
         LA    R6,1(R6)                                                         
         B     FOUND                                                            
*                                                                               
DONE     LA    R1,DSWORDDL                                                      
         SR    R4,R1               BUMP DSWORD POINTER TO PREV. LTR.            
         ZIC   R1,LTRCNT                                                        
         BCTR  R1,0                DECREMENT LETTER COUNTER.                    
         STC   R1,LTRCNT                                                        
         B     CNXTDIE0                                                         
*                                                                               
MOVEON3  ZIC   R5,DIEPOS                                                        
         B     GTDIELP                                                          
         DROP  R4                                                               
*                                                                               
XBLDWRDS B     EXIT1                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*----------------------------- TRANSBRD ------------------------------*         
TRANSBRD NTR1                                                                   
*               TRANSMIT THE ENTIRE BOARD (INCLUDING WALLS)                     
         LA    R2,BOGTBDRH         TRANSMIT TOP BORDER                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BOGROW1H         TRANSMIT FIRST ROW.                          
         ZIC   R3,BOARDSZE                                                      
TLOOP1   OI    6(R2),X'80'         TRANSMIT WALL FIELD.                         
         ZIC   R1,0(R2)            R1=LENGTH OF WALL FIELD.                     
         AR    R2,R1               BUMP R2 TO CUBE FIELD.                       
         OI    6(R2),X'80'         TRANSMIT CUBE FIELD                          
         ZIC   R1,0(R2)            R1=LENGTH OF CUBE FIELD.                     
         AR    R2,R1               BUMP R2 TO NEXT WALL FIELD.                  
         BCT   R3,TLOOP1                                                        
         OI    6(R2),X'80'         TRANSMIT "SOLID" WALL FIELD.                 
*                                                                               
         LA    R2,BOGDSH1H         TRANSMIT 1ST ROW OF DASHES.                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BOGROW2H         TRANSMIT SECOND ROW.                         
         ZIC   R3,BOARDSZE                                                      
TLOOP2   OI    6(R2),X'80'         TRANSMIT WALL FIELD.                         
         ZIC   R1,0(R2)            R1=LENGTH OF WALL FIELD.                     
         AR    R2,R1               BUMP R2 TO CUBE FIELD.                       
         OI    6(R2),X'80'         TRANSMIT CUBE FIELD                          
         ZIC   R1,0(R2)            R1=LENGTH OF CUBE FIELD.                     
         AR    R2,R1               BUMP R2 TO NEXT WALL FIELD.                  
         BCT   R3,TLOOP2                                                        
         OI    6(R2),X'80'         TRANSMIT "SOLID" WALL FIELD.                 
*                                                                               
         LA    R2,BOGDSH2H         TRANSMIT 2ND ROW OF DASHES.                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BOGROW3H         TRANSMIT THIRD ROW.                          
         ZIC   R3,BOARDSZE                                                      
TLOOP3   OI    6(R2),X'80'         TRANSMIT WALL FIELD.                         
         ZIC   R1,0(R2)            R1=LENGTH OF WALL FIELD.                     
         AR    R2,R1               BUMP R2 TO CUBE FIELD.                       
         OI    6(R2),X'80'         TRANSMIT CUBE FIELD                          
         ZIC   R1,0(R2)            R1=LENGTH OF CUBE FIELD.                     
         AR    R2,R1               BUMP R2 TO NEXT WALL FIELD.                  
         BCT   R3,TLOOP3                                                        
         OI    6(R2),X'80'         TRANSMIT "SOLID" WALL FIELD.                 
*                                                                               
         LA    R2,BOGDSH3H         TRANSMIT 3RD ROW OF DASHES.                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BOGROW4H         TRANSMIT FOURTH ROW.                         
         ZIC   R3,BOARDSZE                                                      
TLOOP4   OI    6(R2),X'80'         TRANSMIT WALL FIELD.                         
         ZIC   R1,0(R2)            R1=LENGTH OF WALL FIELD.                     
         AR    R2,R1               BUMP R2 TO CUBE FIELD.                       
         OI    6(R2),X'80'         TRANSMIT CUBE FIELD                          
         ZIC   R1,0(R2)            R1=LENGTH OF CUBE FIELD.                     
         AR    R2,R1               BUMP R2 TO NEXT WALL FIELD.                  
         BCT   R3,TLOOP4                                                        
         OI    6(R2),X'80'         TRANSMIT "SOLID" WALL FIELD.                 
*                                                                               
         LA    R2,BOGDSH4H         TRANSMIT 4TH ROW OF DASHES, OR               
         OI    6(R2),X'80'          BOTTOM BORDER OF 4X4 BOARD.                 
*                                                                               
         CLI   BOARDSZE,4                                                       
         BE    XTRANBRD                                                         
*                                                                               
         LA    R2,BOGROW5H         TRANSMIT FIFTH ROW.                          
         ZIC   R3,BOARDSZE                                                      
TLOOP5   OI    6(R2),X'80'         TRANSMIT WALL FIELD.                         
         ZIC   R1,0(R2)            R1=LENGTH OF WALL FIELD.                     
         AR    R2,R1               BUMP R2 TO CUBE FIELD.                       
         OI    6(R2),X'80'         TRANSMIT CUBE FIELD                          
         ZIC   R1,0(R2)            R1=LENGTH OF CUBE FIELD.                     
         AR    R2,R1               BUMP R2 TO NEXT WALL FIELD.                  
         BCT   R3,TLOOP5                                                        
         OI    6(R2),X'80'         TRANSMIT "SOLID" WALL FIELD.                 
*                                                                               
         LA    R2,BOGBBDRH         TRANSMIT BOTTOM BORDER.                      
         OI    6(R2),X'80'                                                      
*                                                                               
XTRANBRD B     EXIT1                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*------------------------------- SHOWWRDS ----------------------------*         
SHOWWRDS NTR1                                                                   
*               DISPLAY PLAYER'S AND COMPUTER'S WORD.                           
*               DICTBRD AND PWORDS SHOULD BE IN ALPHABETICAL ORDER.             
         MVC   BOGHEAD,BLANKS                                                   
*                                                                               
* PLAYER'S WORDS                                                                
*                                                                               
         CLI   PWRDCNT,0           DID PLAYER GET ANY WORD.                     
         BE    S2                                                               
*                                                                               
         LA    R2,BOGPWRDH         PLAYER HAS SOME WORDS.                       
         LA    R4,PWORDS                                                        
         ZIC   R3,PWRDCNT                                                       
S1       MVC   8(L'PWORDS,R2),0(R4)   MOVE WORD ONTO SCREEN.                    
         OI    6(R2),X'80'         TRANSMIT FIELD.                              
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT SPOT ON SCREEN.                 
         LA    R4,L'PWORDS(R4)     BUMP TO PLAYER'S NEXT WORD.                  
         BCT   R3,S1                                                            
*                                                                               
* COMPUTER'S WORDS                                                              
*                                                                               
S2       CLI   CWRDCNT,0           DID COMPUTER GET ANY WORD.                   
         BH    SHOW01                                                           
*                                                                               
* COMPUTER DID NOT FIND ANY WORDS                                               
*                                                                               
         MVC   BOGHEAD(L'NOWD2MSG),NOWD2MSG                                     
         OI    BOGHEADH+6,X'80'    TRANSMIT MESSAGE.                            
         B     XSHWWRDS                                                         
*                                                                               
* DOES PLAYER HAVE ALL OF COMPUTER WORDS                                        
*                                                                               
SHOW01   CLC   CWRDCNT,PWRDCNT                                                  
         BH    SHOW02                                                           
         MVC   BOGHEAD(L'ALWRDMSG),ALWRDMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         B     XSHWWRDS                                                         
*                                                                               
* COMPUTER HAS SOME MORE WORDS                                                  
*                                                                               
SHOW02   MVC   BOGHEAD(L'MRWRDMSG),MRWRDMSG                                     
         OI    BOGHEADH+6,X'80'                                                 
         LA    R2,BOGCWRDH         R2 BUMPS ALONG SCREEN.                       
         LA    R4,DICTBRD          R4 BUMPS ALONG COMPUTER'S WORDS.             
         ZIC   R3,CWRDCNT          R3 USED AS COUNTER FOR LOOP.                 
         ZIC   R5,PWRDCNT                                                       
         SR    R6,R6               R6=# OF WORDS ON SCREEN, MAX=94.             
S3       GOTO1 =V(BINSRCH),DMCB,(X'00',(R4)),PWORDS,(R5),              +        
               5,(0,5),,RR=RELO                                                 
         TM    0(R1),X'01'         DOES PLAYER HAVE THIS WORD?                  
         BZ    SHOW03              YES, DO COMPUTER'S NEXT WORD.                
*                                                                               
         MVC   8(L'DICTBRD,R2),0(R4)   MOVE WORD ONTO SCREEN.                   
         OI    6(R2),X'80'         TRANSMIT FIELD.                              
         LA    R6,1(R6)                                                         
         C     R6,=F'94'           COULD FIT MAX OF 94 WORDS                    
         BE    XSHWWRDS             ON SCREEN.                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT SPOT ON SCREEN.                 
SHOW03   LA    R4,L'DICTBRD(R4)    BUMP TO COMPUTER'S NEXT WORD.                
         BCT   R3,S3                                                            
*                                                                               
XSHWWRDS B     EXIT1                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*----------------------- CHECK PATTERN ON BOARD ----------------------*         
CKPTTRN  NTR1                                                                   
         GOTO1 VCALLOV,DMCB,(2,0),0   GET OVERLAY WITH TABLES                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)            R2=A(LENGTH OF ADJDIE4 TABLE).               
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(ADJDIE4 TABLE).                    
         LA    R2,2(R2)            R2-->ADJ DICE TABLE FOR 4X4.                 
         ST    R2,ADDADJDI                                                      
*                                                                               
         CLI   BOARDSZE,4                                                       
         BE    CKPT01                                                           
*                                                                               
         AH    R2,TEMPHLF          R2-->LENGTH OF DICE4BY4 TABLE.               
         MVC   TEMPHLF,0(R2)       TEMPHLF=L(DICE4BY4 TABLE).                   
         LA    R2,2(R2)            R2-->DICE4BY4 TABLE.                         
         AH    R2,TEMPHLF          R2--LENGTH OF ADJDIE5 TABLE.                 
         LA    R2,2(R2)            R2-->ADJ DICE TABLE FOR 5X5.                 
         ST    R2,ADDADJDI                                                      
*                                                                               
CKPT01   ZIC   R5,NUMCELL                                                       
         XC    DICEUSED,DICEUSED   NO DICE HAVE BEEN USED.                      
         MVI   LTRCNT,0            INITIALIZE LETTERS COUNTER.                  
         SR    R2,R2               R2 IS CELL COUNTER.                          
CKPT02   LA    R3,INTBOARD(R2)     R3-->INTBOARD                                
         CLC   BOGGUES(1),0(R3)    LOOK FOR MATCH ON FIRST LETTER.              
         BNE   CKPT03                                                           
*                                                                               
* MATCHED ON FIRST LETTER                                                       
*                                                                               
         ZIC   R1,LTRCNT                                                        
         CLI   BOGGUES,C'Q'                                                     
         BE    CKPT04                                                           
*                                                                               
         LA    R1,1(R1)                                                         
         B     CKPT05                                                           
*                                                                               
CKPT04   LA    R1,2(R1)            ACCOUNT FOR 'U' IN 'QU'.                     
*                                                                               
CKPT05   STC   R1,LTRCNT                                                        
*                                                                               
         LA    R2,1(R2)            R2 STARTED FROM 0, => DIE # 1.               
         STC   R2,DICEUSED                                                      
         BAS   RE,FNDNXLTR         FNDNXLTR IS A RECURSIVE SBRTNE.              
         TM    FLAG,PTRNFNDQ       WAS PATTERN FOUND?                           
         BO    XCKPTTRN                                                         
*                                                                               
         BCTR  R2,0                                                             
CKPT03   LA    R2,1(R2)                                                         
         CR    R2,R5               R5=NUMCELL.                                  
         BL    CKPT02                                                           
*                                                                               
XCKPTTRN XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*------------------------ FIND NEXT LETTER ---------------------------*         
FNDNXLTR NTR1                                                                   
*               CALLED BY CKPTTRN ONLY.                                         
*                                                                               
* GET LIST OF ADJACENT DICE                                                     
*                                                                               
         LR    R3,R2               R3=DIE # CURRENTLY ON.                       
         ZIC   R1,NUMCELL                                                       
         LA    R1,1(R1)                                                         
         STH   R1,TEMPHLF          TEMPHLF=17 OR 26.                            
         MH    R3,TEMPHLF          EA ROW IN ADJTABLE IS 17 OR 26 LONG.         
         A     R3,ADDADJDI         R3-->BEGINNING OF AN ADJ DICE ROW.           
*                                                                               
* FIND AN UNUSED ADJACENT DIE                                                   
*                                                                               
FNDN01   CLI   0(R3),0             END OF ADJ DICE ROW?                         
         BE    FNDN04              IF YES, THEN EXIT ROUTINE.                   
         LA    R6,DICEUSED                                                      
FNDN00   CLI   0(R6),0                                                          
         BE    FNDN05              DIE WAS NOT USED.                            
         CLC   0(1,R3),0(R6)       IS DIE USED ALREADY?                         
         BE    FNDN02               IF SO, USE NEXT ADJ DIE ON LIST.            
         LA    R6,1(R6)            BUMP TO NEXT DIE# IN DICEUSED.               
         B     FNDN00                                                           
*                                                                               
* TRY TO MATCH LETTER ON ADJ DIE WITH THE NEXT LETTER IN STRING                 
*                                                                               
FNDN05   ZIC   R2,0(R3)            R2=DIE # OF ADJ DIE.                         
         BCTR  R2,0                ADJUST FOR O.B.O.E.                          
         LA    R2,INTBOARD(R2)     R2-->LETTER ON THE ADJ DIE.                  
         ZIC   R6,LTRCNT                                                        
         LA    R6,BOGGUES(R6)      R6-->NEXT LETTER IN GUESSED WORD             
         CLC   0(1,R6),0(R2)        NEEDED TO BE CHECKED.                       
         BNE   FNDN02              NO MATCH, TRY NEXT ADJ DIE.                  
*                                                                               
* A NEXT LETTER WAS FOUND                                                       
*                                                                               
         ZIC   R6,LTRCNT                                                        
         LA    R7,DICEUSED(R6)     R7-->POSITION TO APPEND DIE#.                
         MVC   0(1,R7),0(R3)       R3-->DIE # WE WANT APPENDED.                 
         CLI   0(R2),C'Q'                                                       
         BE    FNDN06                                                           
*                                                                               
         LA    R6,1(R6)            INCREMENT LTRCNT.                            
         B     FNDN07                                                           
*                                                                               
FNDN06   LA    R6,2(R6)                                                         
*                                                                               
FNDN07   STC   R6,LTRCNT           ONCE LTRCNT HITS 5, WE KNOW THAT             
         CLI   LTRCNT,5             THE PATTERN EXISTS IN BOARD.                
         BE    FNDN03              THE WHOLE PATTERN WAS FOUND.                 
*                                                                               
* PREPARE TO FIND NEXT LETTER                                                   
*                                                                               
         ZIC   R2,0(R3)                                                         
         BAS   RE,FNDNXLTR         RECURSING.                                   
         TM    FLAG,PTRNFNDQ       WAS PATTERN FOUND IN BOARD?                  
         BO    XFNDNXLT                                                         
*                                                                               
FNDN02   LA    R3,1(R3)            BUMP R3 TO NEXT ADJ DIE IN LIST.             
         B     FNDN01                                                           
*                                                                               
FNDN03   OI    FLAG,PTRNFNDQ       PATTERN FOUND, EXIT FNDNXLTR                 
         B     XFNDNXLT             ENTIRELY.                                   
*                                                                               
FNDN04   ZIC   R6,LTRCNT                                                        
         BCTR  R6,0                DECREMENT LTRCNT.                            
         STC   R6,LTRCNT                                                        
         LA    R6,DICEUSED(R6)                                                  
         MVI   0(R6),0             REMOVE DIE FROM DICEUSED LIST.               
*                                                                               
XFNDNXLT XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*--------------------------- BOARD CHARACTERS ------------------------*         
TOPBDR4  DC    C'/-------------------\'                                         
DASH4    DC    C'|---- ---- ---- ----|'                                         
BOTBDR4  DC    C'\-------------------/'                                         
TOPBDR5  DC    C'/------------------------\'                                    
DASH5    DC    C'|---- ---- ---- ---- ----|'                                    
BOTBDR5  DC    C'\------------------------/'                                    
*                                                                               
ALPHABET DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ/'                                   
***********************************************************************         
         SPACE 3                                                                
***********************************************************************         
*------------------------- HEADER MESSAGES ---------------------------*         
BLANKS   DC    CL80' '                                                          
*                                                                               
ENBRDMSG DC    C'OKAY, ENTER LETTERS INTO BOARD.'                               
ENGSSMSG DC    C'ENTER A FIVE-LETTER WORD THAT YOU FOUND.'                      
ENGS1MSG DC    C'GOOD WORD.  ENTER ANOTHER FIVE-LETTER WORD.'                   
*                                                                               
ERCHGMSG DC    C'TO CHANGE INPUTS, RESHUFFLE MUST BE "Y".'                      
ERGBDMSG DC    C'BOARD GENERATOR MUST BE "C" OR "P".'                           
ERLENMSG DC    C'PLEASE ENTER A FIVE LETTER WORD.'                              
ERLETMSG DC    C'- CHARACTER NOT A LETTER.  PLEASE RE-ENTER.'                   
ERPATMSG DC    C'IS NOT FOUND ON THE BOARD.  PLEASE RE-ENTER.'                  
ERRPTMSG DC    C'YOU HAVE THAT WORD ALREADY.  PLEASE ENTER ANOTHER WORD+        
               .'                                                               
ERSHFMSG DC    C'RESHUFFLE MUST BE "Y" OR "N".'                                 
ERSZEMSG DC    C'INVALID SIZE FOR BOARD.  PLEASE RE-ENTER'                      
ERWRDMSG DC    C'IS NOT IN DICTIONARY.  PLEASE ENTER ANOTHER WORD'              
*                                                                               
ALWRDMSG DC    C'GOOD WORK, YOU GOT ALL MY WORDS!  RESHUFFLE FOR A NEW +        
               GAME.'                                                           
MRWRDMSG DC    C'GAME OVER.  RESHUFFLE BOARD FOR NEW GAME.'                     
NOWD1MSG DC    C'I DID NOT FIND ANY WORDS.  RESHUFFLE FOR ANOTHER GAME.+        
               '                                                                
NOWD2MSG DC    C'I DID NOT FIND ANY WORDS EITHER.'                              
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ---------------------- BOGGLE WORKING STORAGE ----------------------*         
BGGLWORK DSECT                                                                  
*                                                                               
ADDADJDI DS    A                   A(ADJACENT DICE TABLE).                      
ADDLTRDI DS    A                   A(LETTERS ON DICE FACES).                    
*                                                                               
DICTADD  DS    A                   A(FIRST WORD IN DICTIONARY).                 
DICTLEN  DS    H                   # WORDS IN DICTIONARY.                       
*                                                                               
DICEUSED DS    CL26                TABLE OF DICE ALREADY USED.                  
NDIEUSED DS    X                   NUMBER OF DICE USED.                         
*                                                                               
DMCB     DS    6F                                                               
DSWORD   DS    6CL2                WORD DATA STRUCTURE.                         
DUB      DS    D                   A DOUBLE-WORD WORKING AREA.                  
FULL     DS    F                   FULL WORD FOR TEMPORARY USE.                 
FIVEWORD DS    CL5                 FIVE-LETTER WORD.                            
LKUPCNT  DS    F                   COUNT # OF LOOKING-UPS IN DICT.              
LTRCNT   DS    X                   NUMBER OF LETTERS COUNTER.                   
PRTSWTCH DS    X                   USED IN MY BRDPRTCT SUBROUTINE.              
QCOUNT   DS    X                   COUNT # OF Q'S IN FIVEWORD.                  
RANDNUM  DS    X                   TEMP STORAGE FOR RANDOM NUMBER.              
RELO     DS    F                   RELOCATION FACTOR                            
TEMPHLF  DS    H                   USED FOR TEMPORARY HALFWORD STORAGE.         
VCALLOV  DS    A                   A(CALLOV).                                   
WORK     DS    CL17                USED IN  EDIT  MACRO.                        
BGGLWRKQ EQU   *                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*--------------------- WORD DATA STRUCTURE DSECT ---------------------*         
DSWORDD  DSECT                                                                  
*               DSWORD IS A DATA STRUCTURE REPRESENTING THE WORD                
*                BEING BUILT.  FOR EACH LETTER IN THE WORD, THERE               
*                ARE TWO BYTES REPRESENTING IT.  THEY ARE COVERED               
*                BY THIS DSECT.                                                 
DIEPOS   DS    X                   SHOWS WHICH CUBE# LETTER IS FROM.            
NXTDIE   DS    X                   SHOWS THE CUBE# OF NEXT LETTER.              
DSWORDDL EQU   *-DSWORDD                                                        
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE FLDHDRD                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
***********************************************************************         
*------------------------------ TWA ---------------------------------*          
       ++INCLUDE GABOGFFBD         DSECT FOR TWA.                               
*                                                                               
*                                                                               
BOARDSZE DS    X                   SIZE OF BOARD (4 OR 5)                       
NUMCELL  DS    X                   NUBMER OF DICE (16 OR 25)                    
*                                                                               
KPGNBD   DS    C                   REMEMBERS PREV BOARD GENERATOR.              
KPSHFL   DS    C                   SIGNIFIES WHEN TO SHUFFLE.                   
KPSIZE   DS    X                   REMEMBERS PREV SIZE OF BOARD.                
*                                                                               
CWRDCNT  DS    X                   # OF VALID WORDS FOUND.                      
DICTBRD  DS    150CL5              DICTIONARY FOR A GIVEN BOARD.                
*                                                                               
FLAG     DS    C                                                                
INBDVLDQ EQU   X'80'               X'80'=>PLAYER INPUTTED VALID BOARD.          
NDVLDQ   EQU   X'40'               X'40'=>NEED TO VALIDATE BOARD.               
NEWGAMEQ EQU   X'20'               X'20'=>BRAND NEW GAME.                       
PTRNFNDQ EQU   X'08'               X'08'=>PATTERN FOUND ON BOARD.               
*                                   X'02'=>VALID GENERATOR OF BOARD.            
*                                   X'01'=>VALID SIZE OF BOARD.                 
INTBOARD DS    CL26                INTERNAL BOARD FOR MY USE.                   
LSTINDSP DS    F                   DISPLACEMENT OF PREVIOUS FIELD WHERE         
***                                 X'40' BIT OF FLDOIND IS 1.                  
LSTDSWRD DS    CL12                LAST WORD INDICATOR.                         
*                                                                               
PWORDS   DS    50CL5               PLAYER'S VALID WORDS.                        
PWRDCNT  DS    X                   PLAYER'S VALID-WORDS COUNTER.                
*                                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120GABOG00B  05/01/02'                                      
         END                                                                    
