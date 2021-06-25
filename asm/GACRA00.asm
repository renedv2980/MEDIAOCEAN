*          DATA SET GACRA00    AT LEVEL 183 AS OF 08/22/00                      
*PHASE TB1E00A                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE RANDOM                                                                 
         TITLE  'CRAZY 8'                                                       
CRAZY    CSECT                                                                  
         PRINT  NOGEN                                                           
         NMOD1  CRA8WRKX-CRA8WRK,GACRA00,R9,RR=R2                               
         USING  CRA8WRK,RC            WORKSPACE BASE ADDRESS                    
         L      RA,4(R1)                                                        
         USING  TB1EFFD,RA            TWA BASE ADDRESS                          
         ST     R2,RELO               RELOCATION FACTOR                         
         EJECT                                                                  
*************************************************************                   
*                      MAIN PROGRAM                         *                   
*************************************************************                   
HELLO    CLI   16(RA),0            IS IT THE FIRST TIME?                        
         BNE   GO                  NO                                           
*                                                                               
*----------- VALIDATE DEALER SELECTION INPUT------------------                  
*                                                                               
CHKSELCT CLI   CRADLR,C'C'         CHECK FOR VALID SELECTION                    
         BE    START                                                            
         CLI   CRADLR,C'P'                                                      
         BE    START                                                            
*                                                                               
*............INVLID INPUT SELECTION ..........................                  
*                                                                               
         MVC   CRAMSG,ERRMSGDL     ERROR MESSAGE                                
         OI    CRAMSGH+6,X'80'     TRANSMIT ERROR MESSAGE                       
         OI    CRADLRH+6,X'40'     REPOSITION CURSOR                            
         B     EXIT                                                             
*                                                                               
*------------SET UP NEW GAME -----------------------------------                
*                                                                               
START    BAS   RE,ERASSCRN         CLEAR PREVIOUS SCREEN                        
         BAS   RE,NEWGAME          SET UP GAME AND UPDATE SCREEN                
         B     EXIT                                                             
*                                                                               
*------------ PROCESS PLAYER'S AND THEN COMPUTER'S MOVE ------------            
*                                                                               
GO       MVC   CRAMSG,HEADING      RESTORE TITLE HEADING                        
         OI    CRAMSGH+6,X'80'                                                  
*                                                                               
         BAS   RE,CHKMOVE          VALIDATE PLAYER'S MOVE, IF OK                
*                                  THEN MAKE PLAYER'S AND CPU'S MOVE            
         BAS   RE,DISPLAY          UPDATE SCREEN                                
*                                                                               
*............ CHECK IF THERE'S A WINNER .............................           
*                                                                               
         CLI   HANDPSIZ,0          IS IT END OF GAME?                           
         BNE   WINC                NO                                           
*                                                                               
*.............PLAYER WON ............................................           
*                                                                               
         MVC   CRAMSG,WINMSGP      PLAYER WIN MESSAGE                           
         OI    CRAMSGH+6,X'80'     TRANSMIT WIN MESSAGE                         
         MVC   CRADLRQ,DLRSMSG     DEALER SELECTION QUESTION                    
         OI    CRADLRQH+6,X'80'    TRANSMIT                                     
         OI    CRADLRH+6,X'40'     REPOSITION CURSOR                            
         MVI   16(RA),0            SET NEWGAME FLAG                             
         B     EXIT                                                             
*                                                                               
WINC     CLI   HANDCSIZ,0          IS IT END OF GAME?                           
         BNE   NOWIN               NO                                           
*                                                                               
*............COMPUTER WON .........................................             
*                                                                               
         MVC   CRAMSG,WINMSGC      COMPUTER WIN MESSAGE                         
         OI    CRAMSGH+6,X'80'     TRANSMIT WIN MESSAGE                         
         MVC   CRADLRQ,DLRSMSG     CONTINUE MESSAGE                             
         OI    CRADLRQH+6,X'80'    TRANSMIT                                     
         OI    CRADLRH+6,X'40'     REPOSITION CURSOR                            
         MVI   16(RA),0            SET NEWGAME FLAG                             
         B     EXIT                                                             
*                                                                               
*............ NO WINNER ......................................                  
*                                                                               
NOWIN    BAS   RE,MAKEMOVE         PLAYER'S MOVE SELECTION                      
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
*NEWGAME SUBROUTINE *                                                           
**********************************************************************          
NEWGAME  NTR1                                                                   
*                                                                               
*------------ INITIALIZATION FOR NEW GAME ------------------------              
*                                                                               
         MVC   RANGE,=F'52'        SIZE OF A FULL DECK                          
         MVC   CARDS,CARDSET       LOAD UP A COMPLETE CARD SET                  
         BAS   RE,SHUFFLE          SHUFFLE THE DECK OF CARDS                    
         XC    HANDC,HANDC         CLEAR COMPUTER,S HAND                        
         XC    HANDP,HANDP         CLEAR PLAYER'S HAND                          
         XC    PILE,PILE           CLEAR DISCARD PILE                           
         MVI   DECKSIZE,37         INIT SIZE OF DRAW DECK                       
         MVI   PILESIZE,0          INIT SIZE OF DISCARD PILE                    
         MVI   DECKPTR,30          INIT POINTER FOR DRAW DECK                   
         MVI   PILEPTR,0           INIT POINTER FOR DISCARD PILE                
*                                                                               
*----------- WHO DEALS THE CARD? --------------------------------               
*                                                                               
         CLI   CRADLR,C'P'         IS IT PLAYER DEALS?                          
         BNE   CPUDEAL             NO, COMPUTER IS THE DEALER                   
*                                                                               
*------------ PLAYER IS THE DEALER ------------------------------               
*                                                                               
         MVI   HANDCSIZ,8          # OF CARDS DEALT TO CPU                      
         MVI   HANDPSIZ,7          # OF CARDS DEALT TO PLAYER                   
         MVI   HANDCPTR,14                                                      
         MVI   HANDPPTR,12                                                      
         BAS   RE,DEALERP          PLAYER DEALS THE CARDS OUT                   
         BAS   RE,CPUMOVE          COMPUTER MAKES FIRST MOVE                    
         B     TEST                SHOW THE DECK                                
*                                                                               
*----------- COMPUTER DEALS OUT THE CARDS -------------------------             
*                                                                               
CPUDEAL  MVI   HANDPSIZ,8          # OF CARDS TO BE DEALT TO PLAYER             
         MVI   HANDCSIZ,7          # OF CARDS TO BE DEALT TO CPU                
         MVI   HANDPPTR,14                                                      
         MVI   HANDCPTR,12                                                      
         BAS   RE,DEALERC          COMPUTER DEALS THE GAME                      
*                                                                               
*******************TEST-DISPLAY DRAW DECK**************************             
TEST     LA    R2,37                                                            
         LA    R4,CRADNUM                                                       
         LA    R5,CRADSUI                                                       
         LA    R3,DECK             POINT TO DRAWING DECK                        
         ZIC   R6,DECKPTR          POINT TO TOP OF DRAW DECK                    
         AR    R3,R6                                                            
DKLOOP   MVC   0(1,R4),0(R3)                                                    
         MVC   0(1,R5),1(R3)                                                    
         LA    R3,2(R3)                                                         
         LA    R4,1(R4)            ******TEST*********                          
         LA    R5,1(R5)            *****TEST*****                               
         BCT   R2,DKLOOP                                                        
         OI    CRADNUMH+6,X'80'     *******TEST******************               
         OI    CRADSUIH+6,X'80'     ********TEST****************                
*******************END TEST***************************************              
*                                                                               
DONEDEAL MVC   CRAMSG,HEADING      RESTORE TITLE HEADING                        
         XC    CRADLRQ,CRADLRQ     CLEAR DEALER SELECTION FIELD                 
         XC    CRADLR,CRADLR       CLEAR DEALER FIELD                           
         OI    CRAMSGH+6,X'80'     RESET ERROR MESSAGE                          
         OI    CRADLRQH+6,X'80'    RESET DEALER SELECTION MSG                   
         OI    CRADLRH+6,X'80'     RESET DEALER SELECTION                       
*                                                                               
*------------ PLAYER'S TURN TO SELECT A MOVE ----------------------             
*                                                                               
         BAS   RE,MAKEMOVE         PLAYER'S MOVE                                
         BAS   RE,DISPLAY          UPDATE SCREEN                                
         MVI   16(RA),1            NOT FIRST TIME ANYMORE!                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
********************************************************************            
*PLAYGAME STARTS THE CRAZY 8 CARD GAME                                          
********************************************************************            
MAKEMOVE NTR1                                                                   
         MVC   CRAMOVQ(63),MSGSLCTM   INPUT MESSAGE                             
         OI    CRAMOVQH+6,X'80'       TRANSMIT THE MESSAGE                      
         OI    CRAMOVEH+6,X'40'       REPOSITION CURSOR                         
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
*                 CHECKMOVE SUBROUTINE                             *            
********************************************************************            
CHKMOVE NTR1                                                                    
*                                                                               
*------------ VALIDATE PLAYER'S MOVE -------------------------------            
*                                                                               
         CLI   CRAMOVE,C'D'        IS IT DRAW FROM DECK?                        
         BNE   DISCARD             NO                                           
*                                                                               
*............ PLAYER DRAWS A CARD FROM DECK .......................             
*                                                                               
         BAS   RE,DRAWCARD         DRAW CARD FROM TOP OF DECK                   
*                                                                               
         CLI   CARD,0              ANYMORE CARD(S) TO DRAW FROM?                
         BNE   OKDRAW              YES                                          
*                                                                               
*............ EMPTY DRAW DECK, PLAYER MUST THROW OUT A CARD .........           
*                                                                               
         MVC   CRAMSG,EMPTYMSG     EMPTY DRAW DECK MESSAGE                      
         OI    CRAMSGH+6,X'80'                                                  
         OI    CRAMOVEH+6,X'40'    PUT CURSOR BACK                              
         B     EXIT                                                             
*                                                                               
OKDRAW   ZIC   R2,HANDPSIZ         NUMBER OF CARDS IN PLAYER'S HAND             
         LA    R2,1(R2)            INCREMENT CARD COUNT                         
         STC   R2,HANDPSIZ         UPDATE # CARD IN PLAYER'S HAND               
*                                                                               
         ZIC   R3,HANDPPTR         OFFSET OF LAST CARD IN PLAYER'S HAND         
         LA    R3,2(R3)            BUMP TO NEXT POSITION IN HAND                
         STC   R3,HANDPPTR         UPDATE POINTER                               
*                                                                               
         LA    R4,HANDP(R3)        NEXT AVAILABLE POSITION IN HAND              
         MVC   0(2,R4),CARD        INSERT NEW CARD INTO HAND                    
         MVC   NEWCARD,CARD        SAVE TO SET HIGH INTENSITY                   
*                                                                               
         GOTO1 =V(XSORT),DMCB,(1,HANDP),50,2,1,1,RR=RELO                        
*                                                                               
         B     EXIT                                                             
*                                                                               
DISCARD  ZIC   R3,HANDPSIZ         # OF CARDS IN PLAYER'S HAND                  
         ZIC   R5,PILEPTR          OFFSET FOR TOP OF DISCARD PILE               
         ZIC   R6,HANDPPTR                                                      
*                                                                               
         LA    R2,HANDP            POINT TO PLAYER'S HAND                       
LOOPPH   CLI   CRAMOVE,C'8'        IS INPUT CRAZY 8?                            
         BNE   SKIPP8              NO                                           
*                                                                               
         CLI   0(R2),C'Z'          IS THERE CRAZY 8 IN HAND?                    
         BNE   NOTFOUND            NO CRAZY 8                                   
*                                                                               
         B     CRAZY8              YES                                          
*                                                                               
SKIPP8   CLC   CRAMOVE(1),0(R2)    IS IT SAME NUMBER?                           
         BNE   NOTFOUND            NO                                           
*                                                                               
         CLC   CRAMOVE+1(1),1(R2)  IS IT SAME SUIT?                             
         BE    FOUND               YES                                          
*                                                                               
NOTFOUND LA    R2,2(R2)            TRY NEXT CARD IN HAND                        
         BCT   R3,LOOPPH                                                        
*                                                                               
*CHECKING FOR TYPE OF INPUT ERROR                                               
*                                                                               
         LA    R2,52               52 CARDS IN SET OF CARD                      
         LA    R3,CARDSET          COMPLETE SET OF VALID CARDS                  
LOOPVD   CLC   CRAMOVE(2),0(R3)    IS IT A VALID CARD                           
         BE    ERRMOVE             YES, FOUND A VALID CARD                      
*                                                                               
         LA    R3,2(R3)            TRY NEXT VALID CARD                          
         BCT   R2,LOOPVD                                                        
*                                                                               
*ERROR!  INPUT IS NOT PART OF A VALID CARD SET                                  
*                                                                               
         MVC   CRAMSG,ERRMSGCD     INVALID CARD MESSAGE                         
         OI    CRAMSGH+6,X'80'                                                  
         OI    CRAMOVEH+6,X'40'                                                 
         B     EXIT                                                             
*                                                                               
*ERROR!  INPUT VALID CARD BUT NOT IN PLAYER'S HAND                              
*                                                                               
ERRMOVE  MVC   CRAMSG,ERRMSGMV       ERROR MESSAGE                              
         OI    CRAMSGH+6,X'80'       TRANSMIT MESSAGE                           
         OI    CRAMOVEH+6,X'40'      POSITION CURSOR BACK                       
         B     EXIT                                                             
*                                                                               
*CHECK IF PLAYER'S VALID INPUT IS A LEGAL MOVE                                  
*                                                                               
FOUND    ZIC   R5,PILEPTR                                                       
         LA    R5,PILE(R5)         POINT TO TOP OF DISCARD PILE                 
*                                                                               
         CLI   PILESIZE,0          IS IT 1ST CARD DISCARDED?                    
         BE    VALID1ST            YES, ALL 1ST CARD ARE VALID                  
*                                                                               
         CLC   CRAMOVE+1(1),1(R5)    IS IT SAME SUIT?                           
         BE    VALID                 YES                                        
         CLC   CRAMOVE(1),0(R5)      IS IT SAME NUMBER?                         
         BE    VALID                 YES                                        
*                                                                               
*ERROR! PLAYER'S MOVE IS ILLEGAL                                                
*                                                                               
         MVC   CRAMSG,ERRMSGM2       ERROR MESSAGE                              
         OI    CRAMSGH+6,X'80'       TRANSMIT MESSAGE                           
         OI    CRAMOVEH+6,X'40'      POSITION CURSOR BACK                       
         B     EXIT                                                             
*                                                                               
*PLAYER'S MOVE IS LEGAL                                                         
*                                                                               
CRAZY8   CLI   CRAMOVE+1,C'S'      CHANGE IT TO SPADE?                          
         BE    VALID               YES                                          
         CLI   CRAMOVE+1,C'H'      CHANGE IT TO HEARTS?                         
         BE    VALID               YES                                          
         CLI   CRAMOVE+1,C'C'      CHANGE IT TO CLUBS?                          
         BE    VALID               YES                                          
         CLI   CRAMOVE+1,C'D'      CHANGE IT TO DIAMOND?                        
         BE    VALID               YES                                          
*                                                                               
         MVC   CRAMSG,ERRCRAZ8     INVALID SUIT SELECTION                       
         OI    CRAMSGH+6,X'80'     TRANSMIT IT                                  
         OI    CRAMOVEH+6,X'40'    REPOSITION CURSOR                            
         B     EXIT                                                             
*                                                                               
VALID    ZIC   R5,PILEPTR           UPDATE DISCARD PILE POINTER                 
         LA    R5,2(R5)             OFFSET OF NEXT PILE POSITION                
         STC   R5,PILEPTR                                                       
         LA    R5,PILE(R5)          NEXT POSITION IN DISCARD PILE               
*                                                                               
VALID1ST MVC   0(2,R5),CRAMOVE      UPDATE DISCARD PILE                         
*                                                                               
*BUMP ALL CARD(S) AFTER DISCARDED CARD DOWN ONE POSITION                        
*                                                                               
         CH    R3,=H'1'               IS IT LAST CARD POSITION?                 
         BE    LASTCARD               YES                                       
*                                                                               
         BCTR  R3,0                                                             
BUMPLOOP MVC   0(2,R2),2(R2)       BUMP DOWN CARD                               
         LA    R2,2(R2)                                                         
         BCT   R3,BUMPLOOP                                                      
         XC    0(2,R2),0(R2)       ERASE LAST CARD AFTER BUMPING                
*                                                                               
LASTCARD ZIC   R2,HANDPPTR           UPDATE PLAYER'S HAND POINTER               
         SH    R2,=H'2'                                                         
         STC   R2,HANDPPTR                                                      
*                                                                               
         ZIC   R2,HANDPSIZ         UPDATE HAND SIZE                             
         BCTR  R2,0                                                             
         STC   R2,HANDPSIZ                                                      
*                                                                               
         LTR   R2,R2               IS IT END OF GAME?                           
         BZ    EXIT                YES, PLAYER WINS                             
*                                                                               
         ZIC   R2,PILESIZE         UPDATE DISCARD PILE SIZE                     
         LA    R2,1(R2)                                                         
         STC   R2,PILESIZE                                                      
*                                                                               
         BAS   RE,CPUMOVE          COMPUTER'S MOVE                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*DISPLAY GAME ON SCREEN                                          *              
******************************************************************              
DISPLAY  NTR1                                                                   
         BAS   RE,ERASSCRN         CLEAR PREVIOUS SCREEN FIRST                  
*                                                                               
*DISPLAY PLAYER'S HAND ON SCREEN                                                
*                                                                               
         CLI   HANDPSIZ,0          ANY CARD LEFT TO DISPLAY?                    
         BE    CHECKCPU            NO                                           
*                                                                               
         BAS   RE,INITP            POINT TO PLAYER'S 1ST CARD FIELD             
         ZIC   R2,HANDPSIZ         # OF CARDS IN PLAYER'S HAND                  
         LA    R3,HANDP            POINT TO PLAYER'S HAND                       
         LA    R7,0                FIELD COUNTER                                
LOOPDP   LA    R7,1(R7)            INCREMENT FIELD COUNTER                      
         MVC   CARD,0(R3)          CARDS IN PLAYER'S HAND                       
         L     R6,NUMLINE          POINT TO CURRENT NUMBER FIELD                
*                                                                               
         TM    6(R6),X'08'         IS IT PREVIOUSLY SET TO HI-INTENSITY         
         BZ    NOTHI               NO                                           
*                                                                               
         OI    6(R6),X'04'         RESET TO NORMAL INTENSITY                    
         L     R6,SUITLINE         POINT TO CURRENT SUIT FIELD                  
         OI    6(R6),X'04'         RESET TO NORMAL INTENSITY                    
*                                                                               
NOTHI    CLC   NEWCARD,CARD        IS IT A NEWLY DRAWN CARD                     
         BNE   NOTNEW              NO                                           
*                                                                               
         L     R6,NUMLINE          POINT TO NUMBER FIELD OF CARD                
         OI    6(R6),X'08'         SET IT TO HIGH INTENSITY                     
         L     R6,SUITLINE         POINT TO SUIT FIELD HEADER                   
         OI    6(R6),X'08'         SET IT TO HIGH INTENSITY                     
*                                                                               
NOTNEW   BAS   RE,PUTCARD          DRAW CARD ON THE SCREEN                      
*                                                                               
         CH    R7,EOSCRN           IS IT END OF SCREEN?                         
         BNE   NEXTFLDP            NO                                           
*                                                                               
         BAS   RE,NEXTCARD         BUMP TO NEXT CARD FIELD ON SCREEN            
         BAS   RE,PUTENDCD         LAST CARD IN 1ST ROW OF CARDS                
         BAS   RE,INITP2           YES, POINT TO 2ND ROW OF CARD FIELD          
         B     SKIPP                                                            
*                                                                               
NEXTFLDP BAS   RE,NEXTCARD         BUMP TO NEXT CARD FIELD                      
*                                                                               
SKIPP    LA    R3,2(R3)            NEXT CARD IN PLAYER'S HAND                   
         BCT   R2,LOOPDP                                                        
*                                                                               
         CH    R7,EOSCRN                                                        
         BE    CHECKCPU                                                         
*                                                                               
         BAS   RE,PUTENDCD         DRAW RIGHT BORDER FOR LAST CARD              
*                                                                               
*DISPLAY COMPUTER'S HAND ON SCREEN                                              
*                                                                               
CHECKCPU CLI   HANDCSIZ,0          ANY CARD LEFT TO DISPLAY?                    
         BE    CHKPILE             NO                                           
*                                                                               
         BAS   RE,INITC            POINT TO CPU'S 1ST CARD FIELD                
         ZIC   R2,HANDCSIZ         # OF CARDS IN CPU'S HAND                     
         LA    R3,HANDC            POINT TO COMPUTER'S HAND                     
         LA    R7,0                CARD FIELD COUNTER                           
LOOPDC   LA    R7,1(R7)            INCREMENT CARD FIELD COUNTER                 
         MVC   CARD,0(R3)          CARDS IN COMPUTER'S HAND                     
         BAS   RE,PUTCARD          DRAW CARD ON THE SCREEN                      
*                                                                               
         CH    R7,EOSCRN           IS IT END OF SCREEN?                         
         BNE   NEXTFLDC            NO                                           
*                                                                               
         BAS   RE,NEXTCARD                                                      
         BAS   RE,PUTENDCD                                                      
         BAS   RE,INITC2           POINT TO 2ND ROW OF CARD FIELD               
         B     SKIPC                                                            
*                                                                               
NEXTFLDC BAS   RE,NEXTCARD         BUMP TO NEXT CARD FIELD                      
*                                                                               
SKIPC    LA    R3,2(R3)            NEXT CARD IN COMPUTER'S HAND                 
         BCT   R2,LOOPDC                                                        
*                                                                               
         CH    R7,EOSCRN                                                        
         BE    CHKPILE                                                          
*                                                                               
         BAS   RE,PUTENDCD         DRAW RIGHT BORDER FOR LAST CARD              
*                                                                               
*DISPLAY DISCARD PILE ON SCREEN                                                 
*                                                                               
CHKPILE  CLI   PILESIZE,0          IS DISCARD PILE EMPTY?                       
         BE    EXIT                YES                                          
*                                                                               
         ZIC   R3,PILEPTR          OFFSET FOR TOP OF DISCARD PILE               
         LA    R3,PILE(R3)         POINT TO TOP OF DISCARD PILE                 
         MVC   CRAPI#1+1(1),0(R3)  UPDATE DISCARD PILE                          
         MVC   CRAPI#2+4(1),0(R3)                                               
         MVC   CRAPIS1+1(1),1(R3)                                               
         MVC   CRAPIS2(1),1(R3)                                                 
         OI    CRAPI#1H+6,X'80'                                                 
         OI    CRAPI#2H+6,X'80'                                                 
         OI    CRAPIS1H+6,X'80'                                                 
         OI    CRAPIS2H+6,X'80'                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
*ERASSCRN ERASES ALL CARDS FROM THE SCREEN                        *             
*******************************************************************             
ERASSCRN NTR1                                                                   
*                                                                               
*CLEAR COMPUTER'S HAND FROM SCREEN                                              
*                                                                               
         BAS   RE,INITC            POINT TO CPU'S 1ST CARD FIELD                
         ZIC   R7,HANDCSIZ         # OF CARDS LEFT IN CPU'S HAND                
         LA    R7,2(R7)            ALSO ERASE RIGHT CARD BORDER                 
         LA    R2,0                                                             
ERASEC   LA    R2,1(R2)                                                         
         BAS   RE,ERASCARD         CLEAR ALL COMPUTER'S CARDS                   
*                                                                               
         CH    R2,EOSCRN           IS IT END OF SCREEN?                         
         BNE   NXTFLDC             NO                                           
*                                                                               
         BAS   RE,NEXTCARD         POINT TO RIGHT BORDER                        
         BAS   RE,ERASCARD         ERASE RIGHTMOST CARD'S BORDER                
         BAS   RE,INITC2           POINT TO 2ND ROW                             
         B     SKIPCC                                                           
*                                                                               
NXTFLDC  BAS   RE,NEXTCARD         BUMP TO NEXT CARD FIELD                      
SKIPCC   BCT   R7,ERASEC                                                        
*                                                                               
*CLEAR PLAYER'S HAND FROM SCREEN                                                
*                                                                               
         BAS   RE,INITP            POINT TO PLAYER'S 1ST CARD FIELD             
         ZIC   R7,HANDPSIZ         # OF CARDS LEFT IN PLAYER'S HAND             
         LA    R7,2(R7)            ALSO ERASE RIGHT CARD BORDER                 
         LA    R2,0                                                             
ERASEP   LA    R2,1(R2)            INCREMENT FIELD OFFSET COUNTER               
         BAS   RE,ERASCARD         CLEAR PLAYER'S HAND                          
*                                                                               
         CH    R2,EOSCRN           IS IT END OF SCREEN?                         
         BNE   NXTFLDP             NO                                           
*                                                                               
         BAS   RE,NEXTCARD         POINT TO LAST CARD FIELD ON SCREEN           
         BAS   RE,ERASCARD         ERASE RIGHTMOST BORDER                       
         BAS   RE,INITP2           POINT TO 2ND ROW                             
         B     SKIPPP                                                           
*                                                                               
NXTFLDP  BAS   RE,NEXTCARD         BUMP TO NEXT CARD FIELD                      
SKIPPP   BCT   R7,ERASEP                                                        
*                                                                               
*CLEAR CONTENT OF DISCARD PILE                                                  
*                                                                               
         MVI   CRAPI#1+1,C' '      CLEAR DISCARD PILE                           
         MVI   CRAPI#2+4,C' '                                                   
         MVI   CRAPIS1+1,C' '                                                   
         MVI   CRAPIS2,C' '                                                     
         OI    CRAPI#1H+6,X'80'                                                 
         OI    CRAPI#2H+6,X'80'                                                 
         OI    CRAPIS1H+6,X'80'                                                 
         OI    CRAPIS2H+6,X'80'                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*DEALERC COMPUTER DEAL'S ALL CARDS FOR CRAZY 8 CARD GAME         *              
******************************************************************              
DEALERC  NTR1                                                                   
         XC    HANDP,HANDP         CLEAR PLAYER'S HAND                          
         XC    HANDC,HANDC         CLEAR COMPUTER'S HAND                        
         LA    R3,HANDP            POINT TO PLAYER'S HAND                       
         LA    R6,DECK             POINT TO DRAW DECK                           
         LA    R7,8                PLAYER GETS 8 CARDS                          
LOOPCP   MVC   CARD,0(R6)          PLAYER'S NEXT CARD IN DECK                   
         MVC   0(2,R3),CARD        INSERT NEW CARD INTO PLAYER'S HAND           
*                                                                               
         LA    R3,2(R3)                                                         
         LA    R6,4(R6)            SKIP COMPUTER'S CARD IN DECK                 
         BCT   R7,LOOPCP                                                        
*                                                                               
         LA    R3,HANDC            POINT TO CPU'S HAND                          
         LA    R6,DECK             POINT TO DECK                                
         LA    R7,7                COMPUTER GETS 7 CARDS                        
LOOPCC   MVC   CARD,2(R6)          COMPUTER'S NEXT CARD IN DECK                 
         MVC   0(2,R3),CARD        INSERT NEW CARD INTO CPU'S HAND              
*                                                                               
         LA    R3,2(R3)            NEXT POSITION IN CPU'S HAND                  
         LA    R6,4(R6)            SKIP PLAYER'S CARD IN DECK                   
         BCT   R7,LOOPCC                                                        
*                                                                               
         GOTO1 =V(XSORT),DMCB,(1,HANDP),50,2,1,1,RR=RELO                        
         GOTO1 =V(XSORT),DMCB,(1,HANDC),50,2,1,1,RR=RELO                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*DEALERP                                                                        
******************************************************************              
DEALERP  NTR1                                                                   
         XC    HANDC,HANDC         CLEAR COMPUTER'S HAND                        
         XC    HANDP,HANDP         CLEAR PLAYER'S HAND                          
         LA    R3,HANDC            POINT TO CPU'S HAND                          
         LA    R6,DECK                                                          
         LA    R7,8                COMPUTER GETS 8 CARDS TO START               
LOOPPC   MVC   CARD,0(R6)          COMPUTER'S NEXT CARD IN DECK                 
         MVC   0(2,R3),CARD        INSERT NEW CARD INTO CPU'S HAND              
*                                                                               
         LA    R3,2(R3)            NEXT POSITION IN CPU'S HAND                  
         LA    R6,4(R6)            SKIP PLAYER'S CARD IN DECK                   
         BCT   R7,LOOPPC                                                        
*                                                                               
         LA    R3,HANDP            POINT TO PLAYER'S HAND                       
         LA    R6,DECK             POINT TO DRAW DECK                           
         LA    R7,7                PLAYER GETS 7 CARDS                          
LOOPPP   MVC   CARD,2(R6)          PLAYER'S NEXT CARD IN DECK                   
         MVC   0(2,R3),CARD        INSERT NEW CARD INTO PLAYER'S HAND           
*                                                                               
         LA    R3,2(R3)            NEXT POSITION IN PLAYER'S HAND               
         LA    R6,4(R6)            SKIP COMPUTER'S CARD IN DECK                 
         BCT   R7,LOOPPP                                                        
*                                                                               
         GOTO1 =V(XSORT),DMCB,(1,HANDP),50,2,1,1,RR=RELO                        
         GOTO1 =V(XSORT),DMCB,(1,HANDC),50,2,1,1,RR=RELO                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*INITP INIT ALL POINTERS TO PLAYER'S FIRST CARD FIELD            *              
******************************************************************              
INITP    NTR1                                                                   
         LA    R3,CRALN1PH                                                      
         ST    R3,TOPH                                                          
         LA    R3,CRALN4PH                                                      
         ST    R3,BOTTOMH                                                       
         LA    R3,CRALN1P                                                       
         ST    R3,TOP                                                           
         LA    R3,CRALN2PH                                                      
         ST    R3,NUMLINE                                                       
         LA    R3,CRALN3PH                                                      
         ST    R3,SUITLINE                                                      
         LA    R3,CRALN4P                                                       
         ST    R3,BOTTOM                                                        
         B     EXIT                                                             
****************************************************************                
*INITC INITIALIZE ALL POINTERS TO CPU'S FIRST CARD FIELD       *                
****************************************************************                
INITC    NTR1                                                                   
         LA    R3,CRALN1CH                                                      
         ST    R3,TOPH                                                          
         LA    R3,CRALN4CH                                                      
         ST    R3,BOTTOMH                                                       
         LA    R3,CRALN1C                                                       
         ST    R3,TOP                                                           
         LA    R3,CRALN2CH                                                      
         ST    R3,NUMLINE                                                       
         LA    R3,CRALN3CH                                                      
         ST    R3,SUITLINE                                                      
         LA    R3,CRALN4C                                                       
         ST    R3,BOTTOM                                                        
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
******************************************************************              
*INITP2 INIT POINTERS TO PLAYER'S FIRST CARD FIELD IN 2ND ROW    *              
******************************************************************              
INITP2   NTR1                                                                   
         LA    R3,CRALN5PH                                                      
         ST    R3,TOPH                                                          
         LA    R3,CRALN8PH                                                      
         ST    R3,BOTTOMH                                                       
         LA    R3,CRALN5P                                                       
         ST    R3,TOP                                                           
         LA    R3,CRALN6PH                                                      
         ST    R3,NUMLINE                                                       
         LA    R3,CRALN7PH                                                      
         ST    R3,SUITLINE                                                      
         LA    R3,CRALN8P                                                       
         ST    R3,BOTTOM                                                        
         B     EXIT                                                             
*****************************************************************               
*INITC2 INITIALIZE POINTERS TO CPU'S FIRST CARD FIELD AT 2ND ROW*               
*****************************************************************               
INITC2   NTR1                                                                   
         LA    R3,CRALN5CH                                                      
         ST    R3,TOPH                                                          
         LA    R3,CRALN8CH                                                      
         ST    R3,BOTTOMH                                                       
         LA    R3,CRALN5C                                                       
         ST    R3,TOP                                                           
         LA    R3,CRALN6CH                                                      
         ST    R3,NUMLINE                                                       
         LA    R3,CRALN7CH                                                      
         ST    R3,SUITLINE                                                      
         LA    R3,CRALN8C                                                       
         ST    R3,BOTTOM                                                        
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*CPUMOVE MAKES COMPUTER'S MOVE                                 *                
****************************************************************                
CPUMOVE  NTR1                                                                   
         ZIC   R2,HANDCSIZ                                                      
         ZIC   R5,PILEPTR          OFFSET FOR TOP OF DISCARD PILE               
*                                                                               
         LA    R3,HANDC            POINT TO CPU'S HAND                          
         LA    R5,PILE(R5)         POINT TO TOP OF DISCARD PILE                 
*                                                                               
         CLI   PILESIZE,0          IS DISCARD PILE EMPTY?                       
         BNE   LOOPMV              NO                                           
*                                                                               
LOOP1ST  CLI   1(R3),C'Z'          IS 1ST CARD CRAZY 8?                         
         BNE   THROW1ST            NOT CRAZY 8, OK TO DISCARD                   
*                                                                               
         LA    R3,2(R3)            CHK NEXT CARD IN HAND                        
         B     LOOP1ST             LOOP TILL NOT CRAZY 8                        
*                                                                               
LOOPMV   CLC   1(1,R3),1(R5)       IS IT SAME SUIT AS DISCARD PILE?             
         BE    THROWOUT            YES                                          
*                                                                               
         CLC   0(1,R3),0(R5)       IS IT SAME NUMBER?                           
         BE    THROWOUT            YES                                          
*                                                                               
         LA    R3,2(R3)            CHECK NEXT CARD IN HAND                      
         BCT   R2,LOOPMV                                                        
*                                                                               
         ZIC   R2,HANDCSIZ         # OF CARDS IN CPU'S HAND                     
         LA    R3,HANDC            POINT TO CPU'S HAND                          
LOOPCRAZ CLI   1(R3),C'Z'          IS IT CRAZY 8?                               
         BNE   NXTCD               NO                                           
*                                                                               
         BAS   RE,MOSTSUIT         FINDS SUIT WITH MOST # OF CARDS              
         MVC   1(1,R3),SUIT        CPU SELECTED SUIT                            
         MVI   0(R3),C'8'          TRANSLATE CRAZY 8                            
         B     THROWOUT                                                         
*                                                                               
NXTCD    LA    R3,2(R3)            CHECK NEXT CARD IN HAND                      
         BCT   R2,LOOPCRAZ                                                      
*                                                                               
*CPU HAS NO CARD TO DISCARD, THEREFORE MUST DRAW A CARD                         
*                                                                               
         ZIC   R2,HANDCSIZ                                                      
         ZIC   R6,HANDCPTR         POINT TO CPU'S LAST CARD                     
LOOPDRAW BAS   RE,DRAWCARD         NO MATCH, DRAW A CARD                        
*                                                                               
         CLI   CARD,0              IS DRAW DECK EMPTY?                          
         BNE   DRAWOK              NO                                           
*                                                                               
         MVI   HANDPSIZ,0          CPU HAS NO CARD TO DISCARD...                
         B     EXIT                THEREFORE, PLAYER WINS                       
*                                                                               
DRAWOK   CLI   CARD+1,C'Z'         IS IT CRAZY 8?                               
         BNE   CHECSUIT            NO                                           
*                                                                               
         BAS   RE,MOSTSUIT         FINDS SUITS WITH MOST CARDS                  
         MVI   CARD,C'8'           TRANSLATE CRAZY 8 CARD                       
         MVC   CARD+1(1),SUIT      CPU SELECTED SUIT                            
         B     ENDDRAW                                                          
*                                                                               
CHECSUIT CLC   CARD+1(1),1(R5)     IS IT SAME SUIT?                             
         BE    ENDDRAW                                                          
*                                                                               
         CLC   CARD(1),0(R5)       IS IT SAME NUMBER?                           
         BE    ENDDRAW                                                          
*                                                                               
NXTDRAW  LA    R6,2(R6)            OFFSET OF NEXT AVAILABLE POSITION            
         STC   R6,HANDCPTR         UPDATE THE END POINTER                       
         LA    R8,HANDC(R6)        NEXT AVAILABLE POSITION IN HAND              
         MVC   0(2,R8),CARD        INSERT NEW CARD IN CPU'S HAND                
*                                                                               
         LA    R2,1(R2)            UPDATE CPU'S HAND SIZE                       
         STC   R2,HANDCSIZ                                                      
*                                                                               
         B     LOOPDRAW            DRAW TILL FOUND MATCH                        
*                                                                               
ENDDRAW  ZIC   R5,PILEPTR          UPDATE DISCARD PILE POINTER                  
         LA    R5,2(R5)            OFFSET OF NEXT POSITION IN PILE              
         STC   R5,PILEPTR          UPDATE DISCARD PILE POINTER                  
         LA    R5,PILE(R5)         NEXT POSITION IN DISCARD PILE                
         MVC   0(2,R5),CARD        UPDATE DISCARD PILE                          
*                                                                               
         ZIC   R2,PILESIZE         UPDATE DISCARD PILE SIZE                     
         LA    R2,1(R2)                                                         
         STC   R2,PILESIZE                                                      
*                                                                               
         GOTO1 =V(XSORT),DMCB,(1,HANDC),50,2,1,1,RR=RELO                        
*                                                                               
         B     EXIT                                                             
*                                                                               
*THROW OUT 1ST VALID CARD IN COMPUTER'S HAND                                    
*                                                                               
THROWOUT ZIC   R5,PILEPTR          UPDATE DISCARD PILE POINTER                  
         LA    R5,2(R5)            OFFSET OF NEXT POSITION IN PILE              
         STC   R5,PILEPTR          UPDATE DISCARD PILE POINTER                  
         LA    R5,PILE(R5)         NEXT POSITION IN DISCARD PILE                
THROW1ST MVC   0(2,R5),0(R3)       UPDATE DISCARD PILE                          
*                                                                               
*BUMP ALL CARD(S) AFTER DISCARDED CARD DOWN ONE POSITION                        
*                                                                               
         CH    R2,=H'1'            IS IT END CARD IN HAND?                      
         BE    ENDCARD             YES                                          
*                                                                               
         BCTR  R2,0                                                             
LOOPBUMP MVC   0(2,R3),2(R3)       ALL CARD(S) AFTER DISCARDED CARD             
         LA    R3,2(R3)                                                         
         BCT   R2,LOOPBUMP         ARE BUMP DOWN ONE POSITION                   
         XC    0(2,R3),0(R3)       ERASE LAST CARD AFTER BUMPING                
*                                                                               
ENDCARD  ZIC   R2,HANDCPTR         UPDATE CPU'S HAND POINTER                    
         SH    R2,=H'2'                                                         
         STC   R2,HANDCPTR                                                      
*                                                                               
         ZIC   R2,HANDCSIZ         UPDATE COMPUTER'S HAND SIZE                  
         BCTR  R2,0                                                             
         STC   R2,HANDCSIZ                                                      
*                                                                               
         ZIC   R2,PILESIZE         UPDATE DISCARD PILE SIZE                     
         LA    R2,1(R2)                                                         
         STC   R2,PILESIZE                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**************************************************************                  
*PUTCARD DRAWS A GIVEN CARD AT A GIVEN POSITION ON SCREEN    *                  
**************************************************************                  
PUTCARD  NTR1                                                                   
         CLI   CARD,C'Z'           IS IT CRAZY 8?                               
         BNE   SKIP8               NO                                           
*                                                                               
         MVC   CARD,CARD#8         CRAZY 8 CARD                                 
*                                                                               
SKIP8    L     R2,TOP                                                           
         L     R3,TOPH                                                          
         MVC   0(3,R2),TOPLINE     DRAW TOP LINE OF CARD                        
         OI    6(R3),X'80'         TRANSMIT TOP LINE                            
         L     R2,BOTTOM                                                        
         L     R3,BOTTOMH                                                       
         MVC   0(3,R2),BOTLINE     DRAW BOTTOM LINE OF CARD                     
         OI    6(R3),X'80'         TRANSMIT BOTTOM LINE                         
         L     R2,NUMLINE                                                       
         MVI   8(R2),C'|'          DRAW LEFT BORDER OF CARD                     
         MVC   9(1,R2),CARD        DRAW THE SUIT OF CARD                        
         OI    6(R2),X'80'         TRANSMIT VERTICLE LINE                       
         L     R2,SUITLINE                                                      
         MVI   8(R2),C'|'          DRAW THE LEFT BORDER                         
         MVC   9(1,R2),CARD+1      DRAW THE NUMBER OF CARD                      
         OI    6(R2),X'80'         TRANSMIT VERTICLE LINE                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**************************************************************                  
*PUTENDCD DRAWS RIGHT BORDER ON LAST CARD IN HAND                               
**************************************************************                  
PUTENDCD NTR1                                                                   
         L     R2,TOP                                                           
         L     R3,TOPH                                                          
         MVC   0(3,R2),ENDTOP      DRAW TOP LINE OF CARD                        
         OI    6(R3),X'80'         TRANSMIT TOP LINE                            
         L     R2,BOTTOM                                                        
         L     R3,BOTTOMH                                                       
         MVC   0(3,R2),ENDBOT      DRAW BOTTOM LINE OF CARD                     
         OI    6(R3),X'80'         TRANSMIT BOTTOM LINE                         
         L     R2,NUMLINE                                                       
         MVC   8(2,R2),ENDNUM      DRAW THE LEFT BORDER                         
         OI    6(R2),X'80'         TRANSMIT VERTICLE LINE                       
         L     R2,SUITLINE                                                      
         MVC   8(2,R2),ENDSUI      DRAW THE NUMBER OF CARD                      
         OI    6(R2),X'80'         TRANSMIT VERTICLE LINE                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
*ERASCARD ERASES A CARD FROM SCREEN AT A GIVEN FIELD POSITION      *            
********************************************************************            
ERASCARD NTR1                                                                   
         L     R2,TOP                                                           
         L     R3,TOPH                                                          
         MVC   0(3,R2),BLANKS                                                   
         OI    6(R3),X'80'                                                      
         L     R2,BOTTOM                                                        
         L     R3,BOTTOMH                                                       
         MVC   0(3,R2),BLANKS                                                   
         OI    6(R3),X'80'                                                      
         L     R2,NUMLINE                                                       
         MVC   8(2,R2),BLANKS                                                   
         OI    6(R2),X'80'                                                      
         L     R2,SUITLINE                                                      
         MVC   8(2,R2),BLANKS                                                   
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
*                                                                               
********************************************************************            
*BUMPCARD BUMPS NEXT CARD IN HAND TO CURRENT CARD POSITION         *            
********************************************************************            
BUMPCARD NTR1                                                                   
         L     R2,NUMLINE                                                       
         L     R3,SUITLINE                                                      
         BAS   RE,NEXTCARD         NEXT CARD IN HAND                            
         L     R4,NUMLINE                                                       
         L     R5,SUITLINE                                                      
         MVC   9(1,R2),9(R4)       BUMP TO PREVIOUS CARD FIELD                  
         MVC   9(1,R3),9(R5)                                                    
         OI    6(R2),X'80'                                                      
         OI    6(R3),X'80'                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**************************************************************                  
*RNDNUM GENERATES PSEUDO RANDOM NUMBERS FOR A SPECIFIED RANGE*                  
**************************************************************                  
RNDNUM   NTR1                                                                   
*        L     R5,RN               RANDOM STARTING NUMBER                       
*        M     R4,=F'65541'                                                     
*        ST    R5,RN                                                            
*        M     R4,RANGE                                                         
*        L     R2,RANGE                                                         
*        SRL   R2,1                                                             
*        AR    R4,R2               0 <= R4 <= (RANGE-1)                         
*        STC   R4,RDIGIT           RDIGIT=RANDOM NUMBER                         
         L     R2,RANGE            RANGE OF RANDOM NUMBER                       
         GOTO1 =V(RANDOM),DMCB,(R2),,RR=RELO                                    
         L     R5,4(R1)                                                         
         GOTO1 =V(RANDOM),DMCB,100,,RR=RELO                                     
         L     R7,4(R1)                                                         
         MR    R4,R7                                                            
         D     R4,RANGE            REMAINDER IS IN THE RANGE                    
         STC   R4,RDIGIT           RANDOM NUMBER                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*************************************************************                   
*GETCARD GETS NEXT CARD IN THE SHUFFLED DECK                *                   
*************************************************************                   
GETCARD NTR1                                                                    
LOOPNEXT BAS   RE,RNDNUM                                                        
         ZIC   R6,RDIGIT                                                        
         MH    R6,=H'2'            OFFSET OF CARD IN ARRAY                      
*                                                                               
         CH    R6,=H'104'                                                       
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CARDS(R6)        POINT TO CARD IN ARRAY                       
*                                                                               
         CLI   0(R6),C'8'          IS IT CRAZY 8?                               
         BNE   NOT8                NO                                           
         MVC   0(2,R6),=C'ZZ'      CHANGE IT TO ITS INTERNAL REPR               
*                                                                               
NOT8     MVC   CARD,0(R6)          RANDOM CARD IN ARRAY                         
*                                                                               
         CLI   CARD,C'0'           IS IT A DUPLICATE?                           
         BNE   OK                  NO                                           
*                                                                               
         B     LOOPNEXT                                                         
*                                                                               
OK       MVI   0(R6),C'0'          SET DUPLICATE FLAG                           
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*SHUFFLE                                                                        
******************************************************************              
SHUFFLE  NTR1                                                                   
         L     R2,RANGE            # OF CARDS TO SHUFFLE                        
         LA    R3,DECK             POINT TO CARD DECK                           
LOOPSH   BAS   RE,GETCARD          RANDOMLY SELECTED CARD                       
         MVC   0(2,R3),CARD        PUT CARD INTO DECK                           
         LA    R3,2(R3)            POINT TO NEXT CARD IN DECK                   
         BCT   R2,LOOPSH                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*DRAWCARD DRAWS A CARD FROM THE TOP OF THE DECK                  *              
******************************************************************              
DRAWCARD NTR1                                                                   
         CLI   DECKSIZE,0          IS DRAW DECK EMPTY?                          
         BNE   NOTEMPTY            NO                                           
*                                                                               
         ZIC   R2,PILESIZE         # OF CARDS IN DISCARD PILE                   
*                                                                               
         CH    R2,=H'1'            ANY CARD(S) TO RESHUFFLE?                    
         BH    RESHUFF             YES                                          
*                                                                               
         MVI   CARD,0              NO MORE CARD LEFT TO DRAW                    
         B     EXIT                                                             
*                                                                               
RESHUFF  BCTR  R2,0                KEEP TOP CARD OF DISCARD PILE                
         ST    R2,RANGE                                                         
         STC   R2,DECKSIZE         RESHUFFLED DECK SIZE                         
*                                                                               
         SLL   R2,1                2 BYTES OF STORAGE PER CARD                  
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CARDS(0),PILE       RESHUFFLE DISCARD PILE FOR DECK              
*                                                                               
         BAS   RE,SHUFFLE          RESHUFFLE DRAW DECK                          
*                                                                               
         ZIC   R5,PILEPTR          OFFSET OF TOP OF DISCARD PILE                
         LA    R5,PILE(R5)         POINT TO TOP OF DISCARD PILE                 
         MVC   PILE(2),0(R5)       BUMP IT TO 1ST CARD ON PILE                  
*                                                                               
         MVI   DECKPTR,0           RESET DECK POINTER                           
         MVI   PILESIZE,1          ONE CARD IN DISCARD PILE                     
         MVI   PILEPTR,0           RESET DISCARD PILE POINTER                   
***************TEST-DISPLAY DRAW DECK**********************                     
         XC    CRADNUM,CRADNUM                                                  
         XC    CRADSUI,CRADSUI                                                  
         ZIC   R2,DECKSIZE                                                      
         LA    R4,CRADNUM                                                       
         LA    R5,CRADSUI                                                       
         LA    R3,DECK             POINT TO DRAWING DECK                        
         ZIC   R6,DECKPTR          POINT TO TOP OF DRAW DECK                    
         AR    R3,R6                                                            
LOOPDK   MVC   0(1,R4),0(R3)                                                    
         MVC   0(1,R5),1(R3)                                                    
         LA    R3,2(R3)                                                         
         LA    R4,1(R4)            ******TEST*********                          
         LA    R5,1(R5)            *****TEST*****                               
         BCT   R2,LOOPDK                                                        
         OI    CRADNUMH+6,X'80'     *******TEST******************               
         OI    CRADSUIH+6,X'80'     ********TEST****************                
****************END TEST***************************************                 
NOTEMPTY ZIC   R2,DECKPTR                                                       
         LA    R3,DECK                                                          
         AR    R3,R2                                                            
         MVC   CARD,0(R3)          NEXT CARD IN DECK                            
         LA    R2,2(R2)            UPDATE DECK POINTER                          
         STC   R2,DECKPTR          SAVE POINTER                                 
         ZIC   R2,DECKSIZE                                                      
         BCTR  R2,0                DECREMENT # CARD IN DECK                     
         STC   R2,DECKSIZE         UPDATE # CARD IN DECK                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*************************************************************                   
*NEXTCARD BUMPS TO NEXT CARD POSITION ON SCREEN             *                   
*************************************************************                   
NEXTCARD NTR1                                                                   
         L     R2,TOP              POINTER OF CURRENT FIELD                     
         LA    R2,3(R2)            BUMP TO NEXT CARD POSITION                   
         ST    R2,TOP              UPDATE CURRENT FIELD POINTER                 
         L     R2,BOTTOM                                                        
         LA    R2,3(R2)                                                         
         ST    R2,BOTTOM                                                        
         L     R2,NUMLINE          POINTER TO CURRENT FIELD                     
         ZIC   R3,0(R2)            LENGTH OF FIELD                              
         AR    R2,R3               BUMP TO NEXT FIELD                           
         ST    R2,NUMLINE          SAVE LOCATION FOR THIS TRANSACTION           
         L     R2,SUITLINE                                                      
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         ST    R2,SUITLINE                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*************************************************************                   
*MOSTSUIT FINDS THE SUIT IN CPU'S HAND WITH MOST # OF CARDS *                   
*************************************************************                   
MOSTSUIT NTR1                                                                   
         ZIC   R2,HANDCSIZ         # OF CARDS IN CPU'S HAND                     
         LA    R3,HANDC            POINT TO CPU'S HAND                          
         LA    R5,0                INIT SPADE COUNTER                           
         LA    R6,0                INIT HEARTS COUNTER                          
         LA    R7,0                INIT CLUBS COUNTER                           
         LA    R8,0                INIT DIAMONDS COUNTER                        
LOOPCTR  CLI   1(R3),C'S'          IS IT SPADE?                                 
         BNE   HEARTS              NO                                           
*                                                                               
         LA    R5,1(R5)            YES                                          
         B     NEXTCTR                                                          
*                                                                               
HEARTS   CLI   1(R3),C'H'          IS IT HEARTS?                                
         BNE   CLUBS               NO                                           
*                                                                               
         LA    R6,1(R6)            YES                                          
         B     NEXTCTR                                                          
*                                                                               
CLUBS    CLI   1(R3),C'C'          IS IT CLUBS?                                 
         BNE   DIAMONDS            NO                                           
*                                                                               
         LA    R7,1(R7)            YES                                          
         B     NEXTCTR                                                          
*                                                                               
DIAMONDS CLI   1(R3),C'D'          IS IT DIAMONDS?                              
         BNE   NEXTCTR             NO                                           
*                                                                               
         LA    R8,1(R8)            YES                                          
*                                                                               
NEXTCTR  LA    R3,2(R3)            NEXT CARD IN CPU'S HAND                      
         BCT   R2,LOOPCTR                                                       
*                                                                               
         STH   R5,MOSTCARD         INIT SPADES HAVE THE MOST CARD               
         MVI   SUIT,C'S'                                                        
         CH    R6,MOSTCARD         DOES HEARTS HAVE MORE CARD?                  
         BL    CHKCLUBS            SPADES STILL HAVE MORE CARDS                 
*                                                                               
         STH   R6,MOSTCARD          UPDATE MAX CARDS                            
         MVI   SUIT,C'H'           HEARTS HAS MORE CARDS                        
*                                                                               
CHKCLUBS CH    R7,MOSTCARD         DOES CLUBS HAS MORE CARDS?                   
         BL    CHKDIAM             NO                                           
*                                                                               
         MVI   SUIT,C'C'           CLUBS HAS MORE CARDS                         
         STH   R7,MOSTCARD         UPDATE # OF CARDS                            
*                                                                               
CHKDIAM  CH    R8,MOSTCARD         DOES DIAMONDS HAVE MORE CARD?                
         BL    EXIT                NO                                           
*                                                                               
         MVI   SUIT,C'D'           DIAMONDS HAVE THE MOST CARDS                 
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*                        CONSTANTS                          *                   
*************************************************************                   
RELO     DS    F                     RELOCATION CONSTANT                        
CARDSET  DC    C'2S3S4S5S6S7SZZ9STSJSQSKSAS'                                    
         DC    C'2H3H4H5H6H7HZZ9HTHJHQHKHAH'                                    
         DC    C'2D3D4D5D6D7DZZ9DTDJDQDKDAD'                                    
         DC    C'2C3C4C5C6C7CZZ9CTCJCQCKCAC'                                    
CARDLENQ EQU   *-CARDSET                                                        
HEADING  DC    CL60'C R A Z Y   8'                                              
WINMSGP  DC    CL60'CONGRADULATIONS, PLAYER WINS!'                              
WINMSGC  DC    CL60'SORRY, COMPUTER WINS!  BETTER LUCK NEXT TIME.'              
EMPTYMSG DC    CL60'SORRY, EMPTY DRAW DECK! YOU MUST THROW OUT A CARD.'         
DLRSMSG  DC    CL40'WHO WOULD LIKE TO DEAL(PLAYER/COMPUTER):'                   
ERRMSGDL DC    CL60'INVALID DEALER SELECTION!  PLEASE TRY AGAIN.'               
ERRMSGM2 DC    CL60'INVALID MOVE SELECTION!  PLEASE TRY AGAIN.'                 
ERRMSGMV DC    CL60'INVALID!  PLAYER DOES NOT HAVE SUCH CARD IN HAND.'          
ERRMSGCD DC    CL60'ERROR!  NO SUCH CARD IN DECK.  PLEASE TRY AGAIN.'           
ERRCRAZ8 DC    CL60'ERROR!  NO SUCH SUIT IN DECK.  PLEASE TRY AGAIN.'           
CRAZ8MSG DC    CL40'CRAZY 8!  SELECT YOUR SUIT (S/H/C/D):'                      
MSGSLCTM DC    CL63'SELECT A CARD TO THROW OUT(#S) OR DRAW A CARD FROM +        
               THE DECK(D):'                                                    
RN       DC    F'8193'             STARTING NUMBER FOR RANDOM #                 
TOPLINE DC     CL3' __'            TOP LINE OF CARD                             
BOTLINE  DC    CL3'|__'            BOTTOM LINE OF CARD                          
ENDTOP   DC    C'_  '              RIGHT BORDER OF LAST CARD ON SCREEN          
ENDBOT   DC    C'_| '                                                           
ENDNUM   DC    C' |'                                                            
ENDSUI   DC    C' |'                                                            
BLANKS   DC    C'   '              BLANKS FOR ERASING CARD FIELD                
EOSCRN   DC    H'25'               SCREEN CAN DISPLAY MAX OF 25 CARDS           
CARD#8   DC    CL2'88'             CRAZY 8 CARD                                 
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*************************************************************                   
*                        WORKSPACE                          *                   
*************************************************************                   
CRA8WRK  DSECT                                                                  
DMCB     DS    6F                                                               
WORK     DS    CL17                USED FOR EDIT MACRO                          
DUB      DS    D                                                                
CARD     DS    CL2                 NEXT CARD IN DECK                            
NEWCARD  DS    CL2                 NEW CARD DRAWN FROM DECK                     
SUIT     DS    C                   SUIT IN CPU'S HAND WITH MOST CARDS           
MOSTCARD DS    H                                                                
RANGE    DS    F                   # OF CARDS TO SHUFFLE/RESHUFFLE              
RDIGIT   DS    X                   RANDOM NUMBER GENERATED                      
CARDS    DS    CL104               STORAGE FOR A DECK OF CARDS                  
TOP      DS    A                   POINTER FOR TOP LINE FIELD OF CARD           
BOTTOM   DS    A                   POINTER FOR BOTTOM LINE FIELD                
TOPH     DS    A                   POINTER FOR TOP LINE FIELD HEADER            
BOTTOMH  DS    A                   POINTER FOR BOT LINE FIELD HEADER            
NUMLINE  DS    A                   POINTER FOR NUMBER FIELD OF CARD             
SUITLINE DS    A                   POINTER FOR SUIT FIELD OF CARD               
CRA8WRKX EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*                           TWA                             *                   
*************************************************************                   
       ++INCLUDE GACRAFFD                                                       
HANDP    DS    CL100               PLAYER'S HAND                                
HANDC    DS    CL100               COMPUTER'S HAND                              
DECK     DS    CL104               CARD DECK TO DRAW FROM                       
PILE     DS    CL104               DISCARDED PILE                               
HANDPSIZ DS    X                   # OF CARDS IN PLAYER'S HAND                  
HANDCSIZ DS    X                   # OF CARDS IN COMPUTER'S HAND                
DECKSIZE DS    X                   # OF CARDS IN DRAW DECK                      
PILESIZE DS    X                   # OF CARDS DISCARDED                         
HANDPPTR DS    X                   DISPLACEMENT INTO PLAYER'S HAND              
HANDCPTR DS    X                   DISPLACEMENT INTO CPU'S HAND                 
DECKPTR  DS    X                   DISPLACEMENT INTO DECK                       
PILEPTR  DS    X                   DISPLACEMENT INTO DISCARD PILE               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'183GACRA00   08/22/00'                                      
         END                                                                    
         EJECT                                                                  
