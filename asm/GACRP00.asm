*          DATA SET GACRP00    AT LEVEL 207 AS OF 08/22/00                      
*PHASE TB1A00A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'GAME CRAPS'                                                     
CRAPS    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 CRAPWRKX-CRAPWRK,GACRP00,RR=R2                                   
         USING CRAPWRK,RC            WORKSPACE BASE ADDRESS                     
         L     RA,4(R1)                                                         
         USING TB1AFFD,RA            TWA BASE ADDRESS                           
         ST    R2,RELO               RELOCATION FACTOR                          
         EJECT                                                                  
*************************************************************                   
*                      MAIN PROGRAM                         *                   
*************************************************************                   
*                                                                               
* FIRST VALIDATE INPUT                                                          
         XC    CRPHEAD,CRPHEAD   CLEAR HEADER AREA                              
         OI    CRPHEADH+6,X'80'  SEND HEADER                                    
         CLI   16(RA),0               IS THIS A NEW GAME                        
         BNE   EXIST            NO BET IS ENTERED FOR EXISTING GAME             
         CLI   CRPBETH+5,0            CHECK INPUT LENGTH                        
         BNE   NUMCHECK                                                         
         MVC   CRPHEAD(12),MISSBET                                              
         OI    CRPHEADH+6,X'80'       TRANSMIT ERROR MESSAGE                    
         OI    CRPBETH+6,X'40'        POSITION CURSOR ON ERROR FIELD            
         B     EXIT2                                                            
*                                                                               
* IS THE BET NUMERIC                                                            
NUMCHECK TM    CRPBETH+4,X'08'                                                  
         BO    BETCHECK                                                         
         MVC   CRPHEAD(17),NONNUM         NON NNUMERIC MESSAGE                  
         OI    CRPHEADH+6,X'80'       XMIT MSG FIELD                            
         OI    CRPBETH+6,X'40'          PUT CURSOR ON ERROR FIELD               
         B     EXIT2                                                            
* CHECK USERS BANKROLL. GIVE HIM 10000 TO START.                                
*                                                                               
BETCHECK CLI   MFLAG,X'01'                                                      
         BE    BET                                                              
         L     R0,=F'10000'                                                     
         ST    R0,STATUS                                                        
         MVI   MFLAG,X'01'                                                      
*                                                                               
* CONVERT INPUT BET TO BINARY                                                   
BET      SR    R0,R0                                                            
         C     R0,STATUS           ANY MONEY LEFT IN BANKROLL?                  
         BNE   BINBET                                                           
         MVC   CRPHEAD(18),USEROUT   TELL USER TO EXIT                          
         B     EXIT2                                                            
BINBET   ZIC   R1,CRPBETH+5        INPUT LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+8              ONLY WANT TO PACK THE NUMERIC INPUT          
         B     *+10                                                             
         PACK  DWORD,CRPBET(0)       CONVERT BET                                
         CVB   R4,DWORD              BET IN BINARY                              
         ST    R4,BETHOLD            STORE THE BET AMOUNT                       
         C     R4,STATUS             DOES USER HAVE ENOUGH MONEY?               
         BNH   RCHECK                                                           
         MVC   CRPHEAD(27),OVERMSG                                              
         OI    CRPBETH+6,X'80'                                                  
         OI    CRPBETH+6,X'40'       PUT CURSOR ON BET FIELD                    
         B     EXIT2                                                            
*                                                                               
* VALIDATE THE START NEWGAME FIELD                                              
EXIST    CLI   CRPINEWH+5,0          IF FIELD BLANK, CONTINUE                   
         BE    RCHECK                                                           
         CLI   CRPINEW,C'X'           START A NEW GAME?                         
         BE    RESET                  RESET THE SCREEN                          
         MVC   CRPHEAD(13),INVAL          INVALID INPUT MSG                     
         OI    CRPINEWH,X'40'         POSITION CURSOR ON ERROR FIELD            
         B     EXIT2                                                            
*                                                                               
* IS THE ROLL FIELD BLANK?                                                      
RCHECK   CLI   CRPROLLH+5,0           IS INPUT FIELD BLANK                      
         BNE   ISITR                                                            
         MVC   CRPHEAD(13),MISSROLL                                             
         OI    CRPROLLH+6,X'40'        POSITION CURSOR ON ERROR FIELD           
         B     EXIT2                                                            
*                                                                               
* DID THE USER TYPE AN 'R' IN THE ROLL FIELD?                                   
ISITR    CLI   CRPROLL,C'R'           WAS AN 'R' INPUTTED                       
         BE    SEED                                                             
         MVC   CRPHEAD(13),INVAL      INVALID MESSAGE                           
         OI    CRPROLLH+6,X'40'        POSITION CURSOR ON ERROR FIELD           
         B     EXIT2                                                            
*                                                                               
*   GET THE SEED TO GENERATE RANDOM NUMBERS                                     
         SPACE 3                                                                
SEED     XC    RANDNUM,RANDNUM                                                  
         TIME  TU                                                               
         STH   R0,RANDNUM           GET SEED FROM CLOCK                         
         MVC   DUB(4),RANDNUM                                                   
         OC    DUB(4),=X'80000000'                                              
         CLC   DUB(4),=X'80000000'                                              
         BE    SEED                AVOID BAD VALUE OF SEED OF 0                 
         SPACE 2                                                                
*                                                                               
*   PRODUCE 1ST RANDOM NUMBER FROM THE CLOCK                                    
         L     R3,RANDNUM                                                       
         M     R2,=F'65541'                                                     
         LPR   R3,R3                                                            
         ST    R3,RANDNUM                                                       
         LA    R3,12                                                            
         M     R2,RANDNUM                                                       
         LA    R2,1(R2)            R2 IS NOW BETWEEN 1 AND 6                    
         ST    R2,DICE1            1ST DICE                                     
*                                                                               
*   PRODUCE 2ND RANDOM NUMBER                                                   
         L     R7,RANDNUM                                                       
         M     R6,=F'65541'                                                     
         LPR   R7,R7                                                            
         ST    R7,RANDNUM                                                       
         LA    R7,12                                                            
         M     R6,RANDNUM                                                       
         LA    R6,1(R6)            R6 IS NOW BETWEEN 1 AND 6                    
         ST    R6,DICE2            2ND DICE                                     
         AR    R2,R6                SUM OF DICE                                 
         CLI   16(RA),0              CHECK NEWGAME FLAG                         
         BNE   EXISTING                                                         
*                                                                               
* CHECK ROLLS OF THE DICE                                                       
CHECKRLL CH    R2,=H'7'                                                         
         BE    WIN                                                              
         CH    R2,=H'11'                                                        
         BE    WIN                                                              
         CH    R2,=H'2'                                                         
         BE    LOSE                                                             
         CH    R2,=H'3'                                                         
         BE    LOSE                                                             
         CH    R2,=H'12'                                                        
         BE    LOSE                                                             
*                                                                               
* SET POINT, HAVE TO ROLL AGAIN LATER                                           
SETPOINT STH   R2,POINT             STORE POINT IN TWA                          
HAVEPNT  EDIT  POINT,(2,CRPHEAD)     THE  POINT VALUE                           
         MVC   CRPHEAD+3(24),POINTMS                                            
         XI    CRPBETH+6,X'40'         DON'T INSERT CURSOR ON FIELD             
         OI    CRPBETH+6,X'20'         PROTECT BET FIELD                        
         OI    CRPBETH+6,X'80'                                                  
         XC    CRPROLL,CRPROLL          BLANK OUT R FIELD                       
         OI    CRPROLLH+6,X'80'                                                 
         MVC   CRPNEWG(25),STRTAGA        START OVER LINE                       
         OI    CRPNEWGH+6,X'80'         XMIT START AGAIN MSG                    
         OI    CRPROLLH+6,X'40'        POSITION CURSOR ON ROLL FIELD            
         MVI   16(RA),1              EXISTING GAME                              
         B     ROLLOVER                                                         
*                                                                               
* EXISTING GAME - SEE IF WE MATCH THE POINT                                     
EXISTING CH    R2,POINT                                                         
         BE    WIN                                                              
         CH    R2,=H'7'                                                         
         BE    LOSE                                                             
         B     HAVEPNT                                                          
WIN      MVC   CRPHEAD(10),WINMSG                                               
         L     R5,STATUS                                                        
         A     R5,BETHOLD            ADD BET TO TOTAL                           
         ST    R5,STATUS             KEEP TRACK OF MONEY SO FAR                 
         B     RESET                                                            
LOSE     MVC   CRPHEAD(10),LOSEMSG                                              
         L     R5,STATUS             BANKROLL SO FAR                            
         S     R5,BETHOLD            SUBTRACT BET FROM TOTAL                    
         ST    R5,STATUS             KEEP TRACK OF MONEY SO FAR                 
*                                                                               
* PUT UP ORIGINAL SCREEN FOR NEW GAME                                           
RESET    MVI   16(RA),0              NEW GAME FLAG                              
         XC    CRPNEWG,CRPNEWG       BLANK OUT START OVER MSG.                  
         OI    CRPNEWGH+6,X'80'                                                 
         XC    CRPBET,CRPBET                                                    
         OI    CRPBETH+6,X'80'                                                  
         OI    CRPBETH+6,X'40'        POSITION CURSOR ON BET FIELD              
         XC    CRPROLL,CRPROLL                                                  
         OI    CRPROLLH+6,X'80'                                                 
         CLI   CRPINEW,C'X'          RESET TO NEW GAME?                         
         BE    EXIT                                                             
*                                                                               
* PROCESS THE DICE ROLL                                                         
ROLLOVER EDIT  (R2),(2,CRPDSUM)        FILL IN SUM                              
         OI    CRPDSUMH+6,X'80'       XMIT SUM                                  
         EDIT  DICE1,(1,CRPDI1V)       FILL IN FIRST DIE VALUE                  
         OI    CRPDI1VH+6,X'80'       XMIT FIRST DIE VALUE                      
         EDIT  DICE2,(1,CRPDI2V)       FILL IN SECOND DIE VALUE                 
         OI    CRPDI2VH+6,X'80'       XMIT SEC DIE VALUE                        
         MVC   CRPDIC(11),DICEMSG                                               
         OI    CRPDICH+6,X'80'         XMIT DICE LINE MSG                       
EXIT     EDIT  STATUS,(6,CRPSTTV),ZERO=NOBLANK MONEY WON, LOST SO FAR           
         OI    CRPSTTVH+6,X'80'       XMIT MONEY VALUE                          
         XC    CRPINEW,CRPINEW                                                  
         OI    CRPINEWH+6,X'80'                                                 
EXIT2    XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*                        CONSTANTS                          *                   
*************************************************************                   
LOSEMSG  DC    CL10'- YOU LOSE'                                                 
WINMSG   DC    CL10'- YOU WIN '                                                 
PLAYAGA  DC    CL23'HIT ENTER TO PLAY AGAIN'                                    
STRTAGA  DC    CL25'ENTER AN X TO START AGAIN'                                  
DICEMSG  DC    CL11'DIE1  DIE2'                                                 
STATUSM  DC    CL6'STATUS'                                                      
POINTMS  DC    CL24'IS THE POINT, ROLL AGAIN'                                   
MISSBET  DC    CL12'MISSING BET'                                                
MISSROLL DC    CL13'MISSING ROLL'                                               
NONNUM   DC    CL17'VALUE NOT NUMERIC'                                          
INVAL    DC    CL13'INVALID INPUT'                                              
OVERMSG  DC    CL27'BET IS GREATER THAN BANKROLL'                               
USEROUT  DC    CL18'EXIT, OUT OF MONEY'                                         
RELO     DS    F                                                                
MONEY    DC    F'10000'                                                         
         EJECT                                                                  
*************************************************************                   
*                        WORKSPACE                          *                   
*************************************************************                   
CRAPWRK  DSECT                                                                  
DWORD    DS     D                     FOR PACKING                               
WORK     DS     CL17                  USED FOR EDIT MACRO                       
DUB      DS     D                                                               
DICE1FLG DS     X                                                               
DICE1    DS     F                                                               
RANDNUM  DS     F                                                               
DICE2    DS     F                                                               
DMCB     DS     6F                                                              
CRAPWRKX EQU    *                                                               
         EJECT                                                                  
*************************************************************                   
*                           TWA                             *                   
*************************************************************                   
       ++INCLUDE GACRPFFD                                                       
POINT    DS      H                    POINT VALUE                               
STATUS   DS      F                    MONEY WON OR LOST SO FAR                  
BETHOLD  DS      F                    LAST BET MADE                             
MFLAG    DS     X                    MONEY FLAG                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'207GACRP00   08/22/00'                                      
         END                                                                    
