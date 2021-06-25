*          DATA SET DETN0407   AT LEVEL 030 AS OF 03/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETN047A                                                                 
*INCLUDE DEMTIME                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE LOADER                                                                 
         TITLE 'DEMO CONVERSION'                                                
************************************************************                    
* SEP/03: ADDED VIP SUPERSTATIONS.                   -SEAN                      
* SUPERSTATION:  WIRED CABLE WITH CHANNEL NUMBER ...                            
*                &  DEMO DATA ARE NOT 0'S                                       
*                ***   BYPASS IF DEMO'S ARE 0'S                                 
*                PROCESSED AS 'W' TYPE AND REGULAR '0' TYPE                     
* JAN/03: ADDED SOME CODES TO PUT OUT MREC.                                     
************************************************************                    
*                                                                               
DETN0407 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DETN0407                                                       
         LR    R3,RB                                                            
         LA    R3,2048(RB)                                                      
         LA    R3,2048(R3)                                                      
         LA    RA,2048(R3)                                                      
         LA    RA,2048(RA)                                                      
         USING DETN0407+4096,R3,RA                                              
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)                                                         
         USING QHDSECT,RC                                                       
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
         CLI   RELOFRST,1                                                       
         BNE   OPENOK                                                           
*                                                                               
*                                                                               
LOADTAB  MVC   DUB,DEMTABS                                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CDEMTABS,4(R1)                                                   
*                                                                               
* SETUP FOR LPM PARALLEL BOOK TYPES                                             
*                                                                               
         CLI   BOOKTYPE,C'P'                                                    
         JNE   *+12                                                             
         MVI   BOOKTYP2,C'P'                                                    
         MVI   BOOKTYPE,0                                                       
*                                                                               
         CLI   XSPILL,C'Y'         XTRA SPILL ACTIVATE                          
         BNE   *+8                                                              
         MVI   BOOKTYPE,X'E0'                                                   
         MVI   RELOFRST,0                                                       
         XC    PRVQSTYP,PRVQSTYP                                                
         EJECT                                                                  
OPENOK   L     R4,ARREC                                                         
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),=Y(RRECL+4)                                              
         LA    R4,4(R4)                                                         
*                                                                               
         CLI   BYPREAD,0                                                        
         BNE   READOK                                                           
*                                                                               
         GET   IN1,(R4)                                                         
*                                                                               
*        CLC   QHRCDE,=C'005'      OLD WIRED UNIVERSES                          
*        BNE   *+10                                                             
*        MVC   QHRCDE,=C'002'      MAKE THEM APPEAR AS NORMAL UNIVS             
*                                                                               
         CLI   QHMRI,C'2'          FOR WIRED TYPE, WE DO A FEW CHECKS           
         BH    OPENOK                                                           
         BNE   OPEN10                                                           
         CLC   QHRCDE,=C'002'                                                   
         BE    OPEN10                                                           
         CLI   QHRCDE,C'3'         BYPASS IF RECTYPE IS NOT 300 RANGE           
         BNE   OPENOK                                                           
         CLI   BOOKTYPE,0          BYPASS IF FORCED BOOKTYPE                    
         BNE   OPENOK                                                           
*                                                                               
*&&DO                                                                           
         CLC   QHCHAN(3),=C'000'   BYPASS IF BRDCST AND NOT SO SUPER            
         BNH   OPEN10                                                           
         LA    R1,SUPRTAB                                                       
OPEN05   CLC   QHSCALL(4),0(R1)                                                 
         BE    OPEN10                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    OPENOK                                                           
         B     OPEN05                                                           
*&&                                                                             
*                                                                               
OPEN10   L     RE,ANIINPUT         SET FOR RECORD COUNTS                        
         L     R0,0(RE)                                                         
         AH    R0,=H'1'                                                         
         ST    R0,0(RE)                                                         
*                                                                               
         NI    PIKSW,XFF-PKSWIPTR    RESET INDEX PASSIVE PTR SWITCH             
         CLC   QHMNO,=C'000124'      RESET ATLANTA TO PREV MARKET CODE          
         BNE   *+10                                                             
         MVC   QHMNO,=C'000168'                                                 
*                                                                               
         CLC   QHMNO,=C'000295'      RESET SANTA CLARA TO PREV MKT              
         BNE   *+10                                                             
         MVC   QHMNO,=C'000480'                                                 
*                                                                               
         LA    R1,NEWLORIG           NEW LORIGS MUST REMAIN CABLE               
OPEN20   CLC   QHSCALL(4),0(R1)                                                 
         BNE   *+14                                                             
         MVC   QHSTYP,=C'09'                                                    
         B     READOK                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   OPEN20                                                           
*                                                                               
         LA    R1,OLDLORIG           OLD LORIGS MUST REMAIN BROADCAST           
OPEN30   CLC   QHSCALL(4),0(R1)                                                 
         BNE   OPEN40                                                           
         CLC   QHMNO,4(R1)                                                      
         BNE   OPEN40                                                           
         MVC   QHSTYP,=C'01'                                                    
         B     READOK                                                           
OPEN40   LA    R1,10(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   OPEN30                                                           
*                                                                               
READOK   CLC   QHRCDE,=C'   '         MI#3 WEEKLY MINIMUMS                      
         BE    OPENOK                                                           
         CLC   QHRCDE,=C'003'         MI#3 WEEKLY MINIMUMS                      
         BE    OPENOK                                                           
         CLC   QHRCDE,=C'004'         MI#4 STATION INFORMATION                  
         BE    M4REC                                                            
*                                                                               
         CLC   QHRCDE,=C'002'         MI#2 UNIVERSE ESTIMATES                   
         BNE   *+16                                                             
         CLI   QHMRI,C'2'                                                       
         BE    M5REC                                                            
         B     M2REC                                                            
*                                                                               
         CLC   QHRCDE,=C'001'         MI#1 MARKET INFO/INTABS                   
         BE    M1REC                                                            
         CLC   QHRCDE,=C'300'      QH - FULL SURVEY AVERAGES                    
         BL    OPENOK                                                           
         CLC   QHRCDE,=C'304'      QH - WEEKLY AVERAGES                         
         BH    OPENOK                                                           
         EJECT                                                                  
         TITLE 'QH DEMOGRAPHIC RECORDS'                                         
* CODES 300-304 ARE QH DEMOGRAPHIC RECORDS                                      
*                                                                               
*-------------------------- MARKET NUMBER ----------------------------*         
*                                                                               
         CLC   =C'HUT/PUT',QHSCALL                                              
         BNE   NOHUT                                                            
         CLI   QHMRI,C'2'         SEND WIRED HUTS AS HUTW                       
         BE    HUTWIRED                                                         
         XC    DUB,DUB                                                          
         PACK  DUB,QHMNO                                                        
         CVB   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QHSCALL(4),DUB+5(3)                                              
         MVI   INTSTYP,PARENTE                                                  
         B     NOHUT                                                            
HUTWIRED MVC   QHSCALL(5),=C'HUTWT' SET FOR WIRED HUT                           
         MVC   QHSTYP,=C'09'        AND FORCE TO INCLUDE MARKET                 
*                                                                               
NOHUT    XC    DUB,DUB                                                          
         PACK  DUB,QHMNO          CONVERT MARKET NUMBER                         
         CVB   RE,DUB                                                           
         STCM  RE,3,INTMRKT                                                     
*        BAS   RE,FLTMKT                                                        
         BRAS  RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BNE   OPENOK                                                           
                                                                                
         CLC   MYSVMKT,SAVEMRKT     IF MARKET CHANGES,                          
         BE    *+14                                                             
         NI    PIKSW,XFF-PKSWFFQ     FLAG TO RELEASE DUMMY I-PTR                
         MVC   MYSVMKT,SAVEMRKT      AND UPDATE MKT                             
*                                                                               
*---------------------------- BOOK TYPE ------------------------------*         
*                                                                               
         BAS   RE,GETBTYP                                                       
         BNE   EXIT                                                             
         MVC   INTBTYP,MYBTYP                                                   
*                                                                               
         CLI   LIVEPLSD,1          FOR LIVE+SD CONVERT TO OTHER BTYP            
         BNE   NONPLSD                                                          
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_LS X'36' FOR DMA                                
         CLI   INTBTYP,C'H'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_HS X'38' FOR HISPANIC (NOT AVAIL YET)           
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_LS X'36' FOR CABLE                              
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_WS X'37' FOR WIRED                              
*                                                                               
NONPLSD  CLI   LIVEPLU3,1          FOR LIVE+3 CONVERT TO OTHER BTYP             
         BNE   NONPLU3                                                          
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_L3 X'30' FOR DMA                                
*        CLI   INTBTYP,C'H'                                                     
*        BNE   *+8                                                              
*        MVI   INTBTYP,X'34'       X'34' FOR HISPANIC (NOT AVAIL YET)           
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_C3 X'31' FOR CABLE                              
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_W3 X'32' FOR WIRED                              
*                                                                               
*                                                                               
NONPLU3  CLI   LIVEONLY,1          FOR LIVEONLY CONVERT TO OTHER BTYP           
         BNE   NONLIVE                                                          
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,C'L'                                                     
         CLI   INTBTYP,C'H'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'J'                                                     
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'U'                                                     
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'Z'                                                     
*                                                                               
NONLIVE  CLI   ZEROCELL,1         FOR ZERO CELL CONVER TO OTHER BTYP            
         BNE   NONZERO                                                          
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,C'1'                                                     
         CLI   INTBTYP,C'H'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'2'                                                     
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'3'                                                     
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'4'                                                     
*                                                                               
NONZERO  CLI   BOOKTYP2,C'P'      FOR LPM CONVERT TO OTHER BOOKTYPE             
         BNE   BTPWIRED                                                         
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,C'P'       BASIC                                         
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'F'       CABLE                                         
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'Y'       WIRED                                         
BTPWIRED DS    0C                                                               
*                                    SET UP MSGFLAG AND MREC FOR UNIQUE         
         BRAS  RE,MSGREC             BOOKTYPES.                                 
*                                                                               
         CLI   PREVBTY,0             ONLY DO WIRED FOR BASIC BOOK TYPES         
         BE    WIREDOK                                                          
         CLI   PREVBTY,C'C'           AND CABLE BOOKS                           
         BE    WIREDOK                                                          
         CLI   PREVBTY,C'W'           BECAUSE BYPREAD CAN BE SET                
         BE    WIREDOK                                                          
         CLI   MYBTYP,C'W'            THIS RECORD IS WIRED SO FORGET IT         
         BE    OPENOK                                                           
WIREDOK  DS    0C                                                               
         MVC   PREVBTY,MYBTYP                                                   
                                                                                
*---------------------- STATION CALL LETTERS -------------------------*         
*                                                                               
BTOK1    MVC   INTSTA(4),QHSCALL                                                
         MVI   INTSTA+4,C'T'                                                    
         SPACE 1                                                                
         L     RF,=A(M4STALST)      GET AFFILIATION FROM THE                    
         USING M4STATD,RF           M4REC STATION LIST                          
AFFLOOP  OC    M4TSCDE,M4TSCDE      END OF STATION LIST?                        
         BE    NOAFFL                                                           
         CLC   QHSCDE,M4TSCDE       A MATCH?                                    
         BNE   *+14                                                             
         MVC   INTAFFL,M4TAFFL                                                  
         B     AFFEQU                                                           
         LA    RF,M4STATLN(RF)                                                  
         B     AFFLOOP                                                          
AFFEQU   L     RE,=A(AFFO2N)                                                    
AFFEQU1  CLC   M4TAFFL,0(RE)                                                    
         BE    AFFEQU2                                                          
         LA    RE,9(RE)                                                         
         CLI   0(RE),0                                                          
         BE    NOAFFL                                                           
         B     AFFEQU1                                                          
AFFEQU2  MVC   INTAFFL,SPACE                                                    
         MVC   INTAFFL(2),7(RE)                                                 
         DROP  RF                                                               
         SPACE 1                                                                
*   ANOTHER NSI WIERDO*****************************                             
NOAFFL   CLC   INTSTA(4),=C'WFRV'                                               
         BNE   *+10                                                             
         CLC   INTMRKT,=H'153'                                                  
         BNE   *+10                                                             
         MVC   INTSTA(4),=C'WJMN'                                               
*  *********************************************************                    
         MVC   INTBOOK,NSIBKYM                                                  
*                                                                               
*---------------------------- GET DAYS -------------------------------*         
*                                                                               
         LA    RF,DAYTABL          GET DAYS                                     
GETDAY   CLI   0(RF),0                                                          
         BNE   GETDAY2                                                          
         DC    H'0'                                                             
         DC    CL12'UNKNOWN DAY'                                                
GETDAY2  CLC   QHDCDE,0(RF)                                                     
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     GETDAY                                                           
         MVC   INTDAY,2(RF)                                                     
*                                                                               
*------------------------- QUARTER HOURS -----------------------------*         
*                                                                               
         GOTO1 =V(HRTOQH),DMCB,(1,QHQHCDE),INTSQH                               
         MVC   INTEQH,INTSQH                                                    
*                                                                               
*-------------------------- STATION TYPE -----------------------------*         
*                                                                               
                                                                                
         CLC   QHSTYP,=C'01'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLC   QHSTYP,=C'02'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLC   QHSTYP,=C'00'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLC   QHSTYP,=C'05'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PS1E                                                     
         CLC   QHSTYP,=C'07'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,S1EQU                                                    
         CLC   QHSTYP,=C'09'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,OUTMARE                                                  
* FORCE WFRV P+S1 TO S1 BOOK TYPE SO THINGS DON'T GET MUCKED UP                 
         CLI   INTSTYP,PS1E                                                     
         BNE   STYPOK                                                           
         CLC   INTSTA(4),=C'WFRV'                                               
         BNE   *+12                                                             
         MVI   INTBTYP,C'1'                                                     
         MVI   INTSTYP,PARENTE                                                  
* FORCE KTNC P+S1 TO SPILL FOR SACRAMENTO HISPANIC                              
         CLC   INTMRKT,=H'462'     SACRAMENTO                                   
         BNE   STYPOK                                                           
         CLC   INTSTA(4),=C'KTNC'  NHSI MISCODING - HOME IS SANFRAN             
         BNE   STYPOK                                                           
         MVI   INTSTYP,OUTMARE     FORCE IT TO OUTSIDE                          
*                                                                               
STYPOK   CLC   INTMRKT,=H'114'     CANADIAN VIEWERS                             
         BE    CHKCAN                                                           
         CLC   INTMRKT,=H'481'     CANADIAN VIEWERS                             
         BE    CHKCAN                                                           
         B     CHKMTROB                                                         
*                                                                               
CHKCAN   DS    0C                                                               
         CLI   QHSGI,C'+'                                                       
         BNE   CHKMTROB                                                         
         MVI   INTSTYP,PARENTE                                                  
         MVI   INTBTYP,C'C'                                                     
*                                                                               
*--------------------------- MARKET TYPE -----------------------------*         
*                                                                               
CHKMTROB CLC   INTMRKT,=H'178'     METRO B MARKETS                              
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
         CLC   INTMRKT,=H'253'                                                  
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
         CLC   INTMRKT,=H'254'                                                  
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
*                                                                               
** CHECK FOR FIRST TIME FOR STATION                                             
*                                                                               
         LA    R9,STALIST                                                       
         XC    FRCBTYP,FRCBTYP                                                  
CHKSTA   CLI   0(R9),0             END OF LIST                                  
         BNE   CHKSTA2              NO TRY NEXT                                 
         MVI   BYPREAD,1            YES - INSERT AND SEND 'M' RECORD            
         MVC   0(5,R9),INTSTA                                                   
         MVC   5(2,R9),QHSTYP      SHOULD ALSO CHECK QHSTYP                     
         MVC   7(1,R9),INTBTYP                                                  
         MVI   STASW,1                                                          
         MVC   PRVQSTYP,QHSTYP                                                  
         BRAS  RE,SETKEY                                                        
         B     EXIT                                                             
CHKSTA2  CLC   0(5,R9),INTSTA      STATION IN LIST                              
         BNE   CHKSTA3              YES - DO NORMAL CONVERSION                  
*                                                                               
         CLC   QHDRHH,ZEROS        DMA HOUSEHOLDS                               
         BE    *+14                ADD TEST FOR "W" PTRS                        
         CLC   7(1,R9),INTBTYP     *BUT* ONLY IF THE HOMES ARE NOT '0'          
         BNE   CHKSTA3                                                          
         CLC   5(2,R9),QHSTYP                                                   
         BE    CHKSTA4                                                          
         CLC   PRVQSTYP,=C'02'     IF 02 QHSTYP FOLLOWED BY                     
         BNE   CHKSTA4             05 QHSTYP THEN LET 05 PASS                   
         CLC   QHSTYP,=C'05'       AND SETKEY                                   
         BNE   CHKSTA4                                                          
         CLI   INTBTYP,C' '        IF THERE IS ALREADY A BOOKTYPE               
         BH    *+8                                                              
         MVI   FRCBTYP,C'A'                                                     
CHKSTA3  LA    R9,8(R9)                                                         
         B     CHKSTA                                                           
CHKSTA4  MVI   STASW,0             RESET CREATE 'M' RECORD SWITCHES             
         CLI   BYPREAD,1           MAKE SURE WE'RE RESETTING                    
         BNE   *+8                  BYPREAD AT THE RIGHT PLACE                  
         MVI   BYPREAD,0                                                        
         MVI   INTWEEKS,0          SET UP ACTIVE WEEKS                          
         SR    R9,R9                                                            
         OC    QHNOW(8),ZEROS                                                   
         CLC   QHNOW(2),=C'00'                                                  
         BE    *+12                                                             
         OI    INTWEEKS,X'08'                                                   
         LA    R9,1(R9)                                                         
         CLC   QHNOW+2(2),=C'00'                                                
         BE    *+12                                                             
         OI    INTWEEKS,X'04'                                                   
         LA    R9,1(R9)                                                         
         CLC   QHNOW+4(2),=C'00'                                                
         BE    *+12                                                             
         OI    INTWEEKS,X'02'                                                   
         LA    R9,1(R9)                                                         
         CLC   QHNOW+6(2),=C'00'                                                
         BE    *+12                                                             
         OI    INTWEEKS,X'01'                                                   
         LA    R9,1(R9)                                                         
*                                                                               
*-------------------------- PROGRAM STUFF-----------------------------*         
*                                                                               
         MVC   INTPNAM,QHPN14                                                   
                                                                                
         CLC   QHRCDE,=C'300'            AVG FULL CYCLE                         
         BNE   AVGOK                                                            
         CLI   INTWEEKS,X'0F'            AND 4 WEEKS ACTIVE                     
         BE    AVGOK                     ITS OK                                 
         OI    INTWEEKS,X'0F'             ELSE FORCE 4 WK                       
         STC   R9,INTPNAM+12              AND SAVE #WKS IN PNAME                
         OI    INTPNAM+12,X'F0'                                                 
         MVI   INTPNAM+13,C'W'                                                  
                                                                                
AVGOK    MVC   INTPNAM6,QHPN6                                                   
         MVC   INTPTYP,QHDPTYP     SET PROGRAM TYPE                             
         PACK  DUB,QHPDOM          CONVERT THE PROGRAM NUMBER                   
         CVB   RE,DUB                                                           
         STCM  RE,15,INTPNUM        AND INSERT INTO INT REC                     
         MVI   INTREV,1            REVISION 1 (AS OF 10/93)                     
         MVC   INTPRSRC,QHPRSRC    PROGRAM SOURCE                               
         L     RE,=A(PSRCO2N)                                                   
PSREQU1  CLC   QHPRSRC,0(RE)                                                    
         BE    PSREQU2                                                          
         LA    RE,9(RE)                                                         
         CLI   0(RE),0                                                          
         BE    PSREQUX                                                          
         B     PSREQU1                                                          
PSREQU2  MVC   INTPRSRC,7(RE)                                                   
PSREQUX  DS    0C                                                               
*                                                                               
*--------------------------- CREATE POINTERS -------------------------*         
*                                                                               
         DS    0H                                                               
         LA    R4,CDMA2INT         TABLE CONTROL DMA                            
         LA    R7,QHA1EST          DMAIN                                        
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         LA    R4,CTSA2INT         TABLE CONTROL TSA                            
         LA    R7,QHA2EST          TSA IN                                       
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
         LA    R4,CUNV2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,M2RECA           UNIVERSE IN                                  
         LA    R7,(M2DK25-M2RCODE)(R7)                                          
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
         LA    R4,CUNH2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,M2RECA           UNIVERSE IN                                  
         LA    R7,(M2MA-M2RCODE)(R7)                                            
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
* NOW DO THE HOMES                                                              
         MVC   CRHOMES(HOMESEND-CRHOMES),ZEROS                                  
         MVC   CRHOMES+9-L'QHDRHH(L'QHDRHH),QHDRHH                              
* USUALLY THE RATING BUT CAN BE ALPHA - EG: OFF                                 
         LA    RE,QHDWRTG                                                       
         LA    RF,4                                                             
KILLOFF  TM    0(RE),X'F0'         NUMERIC                                      
         BO    *+10                                                             
         MVC   0(5,RE),=C'00000'   NO - JUST ZERO IT                            
         LA    RE,5(RE)                                                         
         BCT   RF,KILLOFF                                                       
*                                                                               
         MVC   CRHH1+9-L'QHDRHH(L'QHDRHH),QHDWRTG                               
         MVC   CRHH2+9-L'QHDRHH(L'QHDRHH),QHDWRTG+L'QHDRHH                      
         MVC   CRHH3+9-L'QHDRHH(L'QHDRHH),QHDWRTG+L'QHDRHH*2                    
         MVC   CRHH4+9-L'QHDRHH(L'QHDRHH),QHDWRTG+L'QHDRHH*3                    
         MVC   CSHOMES+9-L'QHDRHS(L'QHDRHS),QHDRHS                              
         MVC   CPMETROA+9-L'QHMAHUT(L'QHMAHUT),QHMAHUT                          
         MVC   CSMETROA+9-L'QHMATHS(L'QHMATHS),QHMATHS                          
         MVC   CRMETROA+9-L'QHMARTG(L'QHMARTG),QHMARTG                          
         MVC   CPMETROB+9-L'QHMBHUT(L'QHMBHUT),QHMBHUT                          
         MVC   CSMETROB+9-L'QHMBTHS(L'QHMBTHS),QHMBTHS                          
         MVC   CRMETROB+9-L'QHMBRTG(L'QHMBRTG),QHMBRTG                          
         MVC   CTHOMES+9-L'QHA2HG(L'QHA2HG),QHA2HG                              
         MVC   CDHOMES+9-L'QHDTPH(L'QHDTPH),QHDTPH                              
         MVC   CDMETROA+9-L'QHMATPH(L'QHMATPH),QHMATPH                          
         MVC   CDMETROB+9-L'QHMBTPH(L'QHMBTPH),QHMBTPH                          
                                                                                
*                                                                               
         LA    R4,CHOM2INT         TABLE CONTROL TSA                            
         LA    R7,CRHOMES          TSA IN                                       
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
         TM    INTSTA,X'F0'        CHECK FOR NUMERIC STATION                    
         BO    CPTRX                                                            
         OC    INTPNUM+1(3),INTPNUM+1                                           
         BZ    CPTRX                                                            
         TM    INTSTYP,X'20'       IGNORE SPILL STATIONS                        
         BO    CPTRX                                                            
         CLC   INTPRSRC,=C'PB'     IGNORE PBS STATIONS                          
         BE    CPTRX                                                            
         CLC   INTMRKT,=H'514'                                                  
         BE    CPTRX                                                            
         CLC   INTMRKT,=H'515'                                                  
         BE    CPTRX                                                            
         CLI   INTBTYP,C'O'                                                     
         BE    CPTRX                                                            
         CLI   INTBTYP,C'W'        IGNORE WIRED STATIONS                        
         BE    CPTRX                                                            
         CLI   INTBTYP,C'Y'                                                     
         BE    CPTRX                                                            
                                                                                
         TM    PIKSW,PKSWIPTR      CREATE PROGRAM INDEX RECD YET?               
         BO    CPTR01               YEP                                         
         BAS   RE,SETPIKEY         NO, SET TO CREATE PROG INDEX RECD            
         MVI   BYPREAD,2                                                        
         B     EXIT                                                             
*                                                                               
CPTR01   DS    0H                                                               
         TM    PIKSW,PKSWFFQ       DUMMY I-PTR CREATED FOR MKT YET?             
         BO    CPTR03               YES                                         
                                                                                
         L     R6,AIREC             NO, CREATE ONE FOR THIS MKT                 
         LA    R6,4(R6)                                                         
         USING PIKEY,R6                                                         
         XC    PIKMAJOR,PIKMAJOR                                                
         MVI   PIKMAJOR,XFF                                                     
         MVC   PIKMAJOR+1(L'PIKMAJOR-1),PIKMAJOR                                
         MVI   PIKCODE,PICODEQU                                                 
         DROP  R6                                                               
         MVI   INTRTYP,PICODEQU                                                 
         OI    PIKSW,PKSWFFQ                                                    
         CLI   BYPREAD,2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
CPTR03   DS    0H                                                               
         CLI   BYPREAD,2           MAKE SURE WE'RE RESETTING                    
         BNE   *+8                  BYPREAD AT THE RIGHT PLACE                  
         MVI   BYPREAD,0                                                        
*                                                                               
CPTRX    DS    0H                                                               
*                                                                               
*------------------ FILE CONVERSION ROUTINES GO HERE -----------------*         
*                                                                               
         BRAS  RE,SETKEY                                                        
         TM    INTSTA,X'F0'                                                     
         BNO   EXIT                                                             
         CLI   INTDAY,X'95'        IS IT M-F                                    
         BE    EXIT                 NO                                          
         CLI   INTBTYP,C'W'        DON'T LET WIRED IN HUT SEEDING               
         BE    EXIT                                                             
         CLI   INTBTYP,C'Y'        DON'T LET WIRED IN HUT SEEDING               
         BE    EXIT                                                             
*        BNE   *+12                 NO                                          
*        CLI   INTSQH,37            YES - DONT PUT OUT AFTER 330P               
*        BH    EXIT                                                             
*        CLI   INTDAY,X'55'                                                     
*        BH    *+12                                                             
*        CLI   INTSQH,38           BYPASS M-F INDIV. DAY BEFORE 330P            
*        BL    EXIT                                                             
         GOTO1 VDEMKTAC,DMCB,(C'P',DEMCOND)                                     
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  RC                                                               
***********************************************************************         
         EJECT                                                                  
         TITLE 'MARKET RECORDS'                                                 
***********************************************************************         
*======================= MARKET RECORD ROUTINES ======================*         
*                                                                               
*----------------------------- M1 RECORD -----------------------------*         
*                                                                               
M1REC    L     R5,ARREC                                                         
         LA    R5,4(5)                                                          
         USING M1DSECT,R5                                                       
         LA    RE,STALIST                                                       
         LA    RF,2400                                                          
         XCEF                                                                   
         MVC   NPRES,M1ROUND                                                    
                                                                                
         MVI   SAMPLEBT,0                                                       
         CLC   M1RPTTYP(2),=C'SC'  ALT BOOK TYPE                                
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'C'                                                    
         CLC   M1RPTTYP(2),=C'SB'  ALT BOOK TYPE                                
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'B'                                                    
         CLC   M1RPTTYP(2),=C'SX'  ALT BOOK TYPE                                
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'O'                                                    
         CLC   M1RPTTYP(2),=C'SH'  ALT BOOK TYPE                                
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'H'                                                    
         CLC   M1RPTTYP(2),=C'HH'  ALT BOOK TYPE                                
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'H'                                                    
         CLC   M1RPTTYP(2),=C'ZC'  ZERO CELL BOOK                               
         BNE   *+8                                                              
         MVI   ZEROCELL,1                                                       
         CLC   M1RPTTYP(2),=C'LO'  LIVE ONLY BOOK                               
         BNE   *+8                                                              
         MVI   LIVEONLY,1                                                       
         CLC   M1RPTTYP(2),=C'L3'  LIVE+3 BOOK                                  
         BNE   *+8                                                              
         MVI   LIVEPLU3,1                                                       
         CLC   M1RPTTYP(2),=C'LS'  LIVE+SD BOOK                                 
         BNE   *+8                                                              
         MVI   LIVEPLSD,1                                                       
         CLI   M1SAMTY1,C'S'       SET FOR HISPANIC IF 'S'                      
         BE    *+12                                                             
         CLI   M1SAMTY1,C'H'        OR 'H'                                      
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'H'                                                    
         CLI   M1SAMTY1,C'B'       SET FOR BLACK                                
         BNE   *+8                                                              
         MVI   SAMPLEBT,C'B'                                                    
*        CLI   M1SAMTY1,C'C'       C = WIRED CABLE HOUSEHOLD                    
*        BNE   *+8                                                              
*        MVI   SAMPLEBT,C'W'       WE USED TO PUT C'C' HERE FOR CABLE           
*                                                                               
         PACK  DUB,M1YR                                                         
         CVB   RE,DUB                                                           
         CH    RE,=H'1999'                                                      
         BH    *+12                                                             
         SH    RE,=H'1900'                                                      
         B     *+8                                                              
         SH    RE,=H'2000'                                                      
         CHI   RE,27                Y2K FIX--IF 2-CHAR YEAR < 27,               
         BH    *+8                                                              
         AHI   RE,100                MAKE IT A 21ST CENTURY YEAR                
         STC   RE,NSIBOOKY                                                      
         PACK  DUB,M1MO                                                         
         CVB   RE,DUB                                                           
         STC   RE,NSIBOOKM                                                      
         DROP  R5                                                               
         SPACE 1                                                                
         MVC   NSIBKYM,NSIBOOKY                                                 
         MVC   NSIBKYM+1(1),NSIBOOKM                                            
                                                                                
         CLI   FORCE,C'Y'                                                       
         BNE   *+10                                                             
         MVC   NSIBKYM,FILTBOOK+1  SET FORCE BOOK VALUE                         
                                                                                
         CLI   FRST,1                                                           
         BNE   EXIT                                                             
         GOTO1 VDEMKTAC,DMCB,(C'I',DEMCOND),MODLIST                             
         LA    R9,INTACCS-INTERD                                                
         GOTO1 VDEMKTAC,DMCB,(C'S',DEMCOND),=C'DISP',(R9)                       
         MVI   FRST,0                                                           
         B     OPENOK                                                           
         EJECT                                                                  
*----------------------------- M2 RECORD -----------------------------*         
*                                                                               
M2REC    LA    R1,RRECL                                                         
         LA    R5,M2RECA                                                        
         MOVE  ((R5),(R1)),(R4)                                                 
         USING M2DSECT,R4                                                       
         L     RE,=A(M4STALST)                                                  
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         XC    BTYPTAB,BTYPTAB                                                  
         MVI   BTYPTAB,X'FF'                                                    
         MVC   TEMPMNO,M2MNO       MARKET NUMBER SAVED FOR AMREC                
         MVC   TEMPMKT,M2MN26      MARKET NAME SAVED FOR AMREC                  
*                                                                               
         MVI   INTRTYP,C'U'                                                     
         PACK  DUB,M2MNO                                                        
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMRKT                                                     
         MVC   INTSTA(4),M2MNO+2                                                
         MVI   INTSTA+4,C'U'                                                    
         MVC   INTBOOK(2),NSIBKYM                                               
                                                                                
         LA    R4,CUNV2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,M2RECA           UNIVERSE IN                                  
         LA    R7,(M2DK25-M2RCODE)(R7)                                          
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
                                                                                
         LA    R4,CUNH2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,M2RECA           UNIVERSE IN                                  
         LA    R7,(M2MA-M2RCODE)(R7)                                            
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
         CLI   BYPREAD,3                                                        
         BE    M2REC10                                                          
         MVI   BYPREAD,3           SET SWITCH BEFORE EXITING                    
         B     M2RECX                                                           
*                                                                               
M2REC10  MVI   BYPREAD,0           RESET SWITCH                                 
         XC    INTUNIVS,INTUNIVS   CLEAR DISPL. OF 1ST UNIV DEMO                
         BAS   RE,GETBTYP                                                       
         BNE   EXIT                                                             
         MVC   INTBTYP,MYBTYP                                                   
*                                                                               
         CLI   LIVEONLY,1          FOR LIVEONLY CONVERT TO OTHER BTYP           
         BNE   BTPZERO                                                          
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,C'L'                                                     
         CLI   INTBTYP,C'H'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'J'                                                     
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'U'                                                     
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'Z'                                                     
*                                                                               
BTPZERO  CLI   ZEROCELL,1         FOR ZERO CALL CONVER TO OTHER BTYP            
         BNE   BTPWIRE1                                                         
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,C'1'                                                     
         CLI   INTBTYP,C'H'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'2'                                                     
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'3'                                                     
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'4'                                                     
*                                                                               
BTPWIRE1 CLI   BOOKTYP2,C'P'      FOR LPM CONVERT TO OTHER BOOKTYPE             
         BNE   BTPWIRE2                                                         
         CLI   INTBTYP,0                                                        
         BNE   *+8                                                              
         MVI   INTBTYP,C'P'       BASIC                                         
         CLI   INTBTYP,C'C'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'F'       CABLE                                         
         CLI   INTBTYP,C'W'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'Y'       WIRED                                         
BTPWIRE2 DS    0C                                                               
*                                    SET UP MSGFLAG AND MREC FOR UNIQUE         
                                                                                
         MVI   INTPNAM+1,C' '                                                   
         MVC   INTPNAM+2(L'INTPNAM-2),INTPNAM+1                                 
         MVI   INTPNAM6+1,C' '                                                  
         MVC   INTPNAM6+2(L'INTPNAM6-2),INTPNAM6+1                              
                                                                                
         MVI   STASW,0                                                          
         BRAS  RE,SETKEY                                                        
*                                                                               
         L     R4,ARREC                                                         
         LA    R4,4(R4)                                                         
*                                                                               
*                                                                               
M2RECX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------- M5 RECORD -----------------------------*         
*                                                                               
M5REC    LA    R1,RRECL                                                         
         LA    R5,M5RECA                                                        
         MOVE  ((R5),(R1)),(R4)                                                 
         USING M2DSECT,R4                                                       
         L     RE,=A(M4STALST)                                                  
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVI   INTRTYP,C'R'                                                     
         PACK  DUB,M2MNO                                                        
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMRKT                                                     
         MVC   INTSTA(4),M2MNO+2                                                
         MVI   INTSTA+4,C'U'                                                    
         MVC   INTBOOK(2),NSIBKYM                                               
                                                                                
         LA    R4,CUNV2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,M5RECA           UNIVERSE IN                                  
         LA    R7,(M2DK25-M2RCODE)(R7)                                          
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         LA    R4,CUNH2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,M5RECA           UNIVERSE IN                                  
         LA    R7,(M2MA-M2RCODE)(R7)                                            
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         XC    INTUNIVS,INTUNIVS   CLEAR DISPL. OF 1ST UNIV DEMO                
         MVI   INTBTYP,C'W'                                                     
         CLI   ZEROCELL,1                                                       
         BNE   *+8                                                              
         MVI   INTBTYP,C'4'                                                     
         CLI   LIVEONLY,1                                                       
         BNE   *+8                                                              
         MVI   INTBTYP,C'Z'                                                     
*                                                                               
         MVI   INTPNAM+1,C' '                                                   
         MVC   INTPNAM+2(L'INTPNAM-2),INTPNAM+1                                 
         MVI   INTPNAM6+1,C' '                                                  
         MVC   INTPNAM6+2(L'INTPNAM6-2),INTPNAM6+1                              
*                                                                               
         MVI   STASW,0                                                          
         BRAS  RE,SETKEY                                                        
*                                                                               
         L     R4,ARREC                                                         
         LA    R4,4(R4)                                                         
*                                                                               
M5RECX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
********M4 STATION INFO RECORDS                                                 
M4REC    L     R5,ARREC                                                         
         LA    R5,4(5)                                                          
         USING M4DSECT,R5                                                       
         L     R4,=A(M4STALST)                                                  
         USING M4STATD,R4                                                       
         CLI   0(R4),0                                                          
         JE    *+12                                                             
         LA    R4,M4STATLN(R4)                                                  
         J     *-12                                                             
         MVC   M4TSCDE,M4SCOLD                                                  
         MVC   M4TAFFL,M4AFF1                                                   
         MVC   M4TSTYP,M4STYP                                                   
*                                                                               
M4RECX   B     EXIT                                                             
         DROP  R4                                                               
         DROP  R5                                                               
***********************************************************************         
         EJECT                                                                  
CNVWR    L     R2,ASREC            SET TO SORT RECORD                           
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),C'R'          PURGE RATINGS RECORDS ONLY                   
         BNE   EXIT                                                             
*                                                                               
** RATINGS RECORD **                                                            
*                                                                               
         USING DRKEY,R6                                                         
*                                                                               
*        OC    INTACCS(4),INTACCS                                               
*        BNZ   CNVW10                                                           
*        MVI   INTRTYP,0                                                        
*        B     EXIT                                                             
*                                                                               
CNVW10   CLC   DRSTAT(5),PREVSTAT  SAME STATION                                 
         BNE   GOODREC             NO - CANNOT BE PARENT OF S1                  
         CLC   DRSTYP,PREVSTYP                                                  
         BE    GOODREC                                                          
         CLI   PREVSTYP,1          IF PREV NOT P+S1 MUST BE OK                  
         BNE   GOODREC                                                          
         CLI   DRSTYP,2            IS IT A PARENT                               
         BNE   GOODREC             NO - CANNOT HAVE S1                          
         CLI   INTBTYP,0           FOR BOOKTYPE DATA                            
         BH    CNVWR1               PURGE IT                                    
         MVI   INTBTYP,C'A'        OTHERWISE PAR OF S1 -ALLOW NOV/02            
*                                                                               
         CLI   PARENTF,X'00'                                                    
         BNE   *+18                                                             
         MVI   PARENTF,X'01'       1ST TIME PARENT ONLY FOR THIS STA            
         XC    PREVQH,PREVQH                                                    
         B     GOODREC2                                                         
         CLI   PARENTF,X'01'                                                    
         BNE   *+8                                                              
         MVI   PARENTF,X'02'       MORE THAN 1ST TIME PARENT ONLY               
         B     GOODREC2                                                         
*                                                                               
CNVWR1   MVI   INTRTYP,0           YES - PARENT OF S1 -PURGE                    
         B     EXIT                                                             
GOODREC  MVC   PREVSTYP,DRSTYP                                                  
         MVC   PREVSTAT,DRSTAT                                                  
         MVI   PARENTF,X'00'       SET TO NON PARENT ONLY                       
GOODREC2 TM    INTSTA,X'F0'        CHECK FOR MARKET TOTAL                       
         BNO   *+8                                                              
         OI    INTWEEKS,B'00100000' SET TYPICAL SW                              
         CLC   INTSQH,PREVQH       SAME QTR HR                                  
         BE    CNVWR2              YES SEE IF 2 WK HIGHEST                      
         MVI   HAV2WH,0            RESET 2W HIGHEST SW                          
         MVC   PREVQH,INTSQH                                                    
         CLI   DRHIQHR+1,2         2WK RECORD - MUST HAVE ANOTHER               
         BNE   *+8                                                              
         MVI   HAV2WH,1           SET FOR 2W HIGEST                             
         CLI   DRHIQHR+1,3         IS THIS A TYPICAL                            
         BL    CNVWR3                                                           
         OI    INTWEEKS,B'00100000' SET TYPICAL SW                              
                                                                                
CNVWR2   CLI   DRHIQHR+1,2         AT LEAST 2 WEEKS                             
         BL    CNVWR3              NO-EXIT                                      
         OI    INTWEEKS,B'00100000' YES SET TYP                                 
                                                                                
         CLI   HAV2WH,1            SECOND 2 WEEK RECORD                         
         BNE   CNVWR3                                                           
         SPACE 2                                                                
         CLI   DRHIQHR+1,2         JUST IN CASE WE DONT GET SECOND 2W           
         BNE   *+8                 RECORD -CHECK FOR IT                         
*                                  NOTE - THIS HAS HAPPENED IN THE PAST         
         SPACE 2                                                                
         OI    INTWEEKS,B'00110000' YES -SET TYPICAL                            
         MVI   HAV2WH,0            RESET 2WH                                    
                                                                                
CNVWR3   TM    INTWEEKS,X'20'      IS IT A TYPICAL RECORD                       
         BO    EXIT                YES - EXIT                                   
*        MVI   INTRTYP,0           NO - PURGE IT                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
*------------USE TABLE TO CONVERT FROM RREC TO IREC                             
* R4 = A(CONVERSION TABLE)                                                      
* R7 = A(START OF RATING SERVICE DEMO DATA)                                     
* R6 = A(START OF INTERIM RECORD DEMO DATA)                                     
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,9                                                             
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MH    R5,=H'9'            ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
         CLC   SPACE(9),0(R5)                                                   
         BNE   CNVRTOI3                                                         
         SR    RF,RF                                                            
         B     CNVRTOI4                                                         
CNVRTOI3 PACK  DUB,0(9,R5)         PACK IT                                      
         CVB   RF,DUB              AND CONVERT                                  
CNVRTOI4 ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MH    R5,=H'4'            ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
CNVRTOI5 ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
         B     CNVRTOI1                                                         
*                                                                               
CNVRTOIX XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=================== BUILD PROGRAM INDEX RECORDS =====================*         
*                                                                               
SETPIKEY NTR1                                                                   
         L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         USING PIKEY,R6                                                         
         XC    PIKMAJOR,PIKMAJOR                                                
                                                                                
         MVI   PIKCODE,PICODEQU                                                 
         MVI   INTRTYP,PICODEQU                                                 
         MVI   PIMEDIA,C'T'                                                     
         MVI   PISRC,C'N'                                                       
         MVC   PIBOOK,INTBOOK                                                   
         XC    PIBOOK,=X'FFFF'                                                  
         MVC   PIPNUM,INTPNUM+1                                                 
         MVC   PISTA,INTSTA                                                     
*                                                                               
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   PIDAY,INTDAY                                                     
         MVC   PISQH,INTSQH                                                     
         MVC   PIEQH,INTEQH                                                     
         DROP  R6                                                               
                                                                                
         OI    PIKSW,PKSWIPTR      SET PROG INDEX RECDS SWITCH                  
SETPIKYX XIT                                                                    
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
GETBTYP  NTR1                                                                   
         USING QHDSECT,RC                                                       
*                                                                               
         MVI   MYBTYP,0          RESET THE INTERNAL BOOK TYPE                   
         CLI   QHMRI,C' '        PROCEED NORMALLY                               
         BE    GETB40                                                           
         CLI   QHMRI,C'1'                                                       
         BNE   GETB20                                                           
         CLC   QHCHAN(3),=C'000'                                                
         BNE   GETB10            *-----------------------------------*          
         CLI   BOOKTYPE,0        | SPECIAL BOOKTYPE                  |          
         BNE   GETB10            |                                   |          
*                                |                                   |          
         CLC   QHSTYP(2),=C'01'    HOME MARKET GOES TO REGULAR                  
         BE    GETB10                                                           
*                                |                                   |          
         LA    R1,PXETAB                                                        
GETB02   CLC   QHSCDE,0(R1)                                                     
         BE    GETB05                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GETB02                                                           
*                                |                                   |          
         CLC   QHSCALL(2),=C'PX'                                                
         BE    GETB05              ALL PX AFFILIATES ARE CABLE                  
*                                |                                   |          
         L     RF,=A(M4STALST)   | GET AFFILIATION FROM THE          |          
         USING M4STATD,RF        | M4REC STATION LIST                |          
GETB03   OC    M4TSCDE,M4TSCDE   | END OF STATION LIST?              |          
         BE    GETB05            |                                   |          
         CLC   QHSCDE,M4TSCDE    | STATION FOUND                                
         BE    *+12                                                             
         LA    RF,M4STATLN(RF)   |                                   |          
         B     GETB03            |                                   |          
         CLC   M4TAFFL,SPACE       IS THERE AN AFFILATION?                      
         BNE   GETB10              YES                                          
*                                |                                   |          
GETB05   MVI   MYBTYP,C'C'       | QHMRI = 1, CHANNEL = 0, BTYPE = C |          
         B     GETB30            |                                   |          
GETB10   MVI   MYBTYP,0          | QHMRI = 1, CHANNEL!= 0, BTYPE = 0 |          
         B     GETB40            | REGULAR STILL FOLLOW OLD LOGIC    |          
*                                |                                   |          
GETB20   DS    0H                |                                   |          
         CLI   QHMRI,C'2'        |                                   |          
         BNE   GETB40            |                                   |          
*        CLC   QHCHAN(3),=C'000' |                                   |          
*        BNE   GETB25            |                                   |          
*        MVI   MYBTYP,C'W'       | QHMRI = 2, CHANNEL = 0, BTYPE = W |          
*        B     GETB30            |                                   |          
*ETB25   DS    0H                |                                   |          
         MVI   MYBTYP,C'W'       | QHMRI = 2, CHANNEL!= 0, BTYPE = 0 |          
         B     GETB30            *-----------------------------------*          
*                                                                               
GETB30   DS    0H                                                               
         B     GETBTYPX            CABLE/WIRE BOOKTYPE DETERMINED               
*                                                                               
*********************************************************************           
* ABOVE ROUTINE PROCESSES SPECIAL BOOKTYPES FOR VIP QH REC'S                    
*********************************************************************           
GETB40   GOTO1 VLDCREC,DMCB,0,0    FIND MARKET TABLES                           
         L     RE,DMCB                                                          
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NONE THERE                            
GETBTYP2 CLC   0(2,RE),=C'NT'      FIND THE USA NIELSEN TV ONE                  
         BE    GETBTYP4                                                         
         ICM   RE,7,2(RE)          NF - TRY NEXT ONE                            
         OC    0(2,RE),0(RE)       EOT - DIE SOMETHING WRONG                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     GETBTYP2                                                         
*                                                                               
GETBTYP4 LA    RE,5(RE)            BYPASS HEADER                                
GETBTYP6 OC    0(2,RE),0(RE)       EOT                                          
         BNZ   *+6                 DIE -- PREVENT OVERWRITING                   
         DC    H'0'                 DATA ALREADY LOADED                         
                                                                                
         CLC   0(2,RE),INTMRKT     HAVE IT                                      
         BE    *+12                                                             
         A     RE,DMCB+4           L'ENTRY                                      
         B     GETBTYP6            NEXT ONE                                     
                                                                                
         CLI   2(RE),C' '          DON'T CARE                                   
         BE    GETBTYP7                                                         
*                                                                               
*************NSI STRIKES AGAIN - THIS ATLANTA NORMALLY GETS 'E'                 
*                  BUT IF BOOKTYPE IS FORCED THEN ALLOW THE                     
*                  FORCED BOOK TYPE TO OVERRIDE                                 
*                  REGULAR ATLANTA = MARKET NUMBER 168                          
*                                                                               
         CLC   INTMRKT,=H'124'     ATLANTA EXTENDED                             
         BNE   GETBTYP6A                                                        
         CLI   BOOKTYPE,0          AND BOOK TYPE NOT INPUT                      
         BNE   GETBTYP7                                                         
GETBTYP6A DS   0C                                                               
*                                                                               
         CLI   BOOKTYPE,C'C'       FOR CABLE PROCESSING,                        
         BE    GETBTYP7             OVERRIDE BOOKTYPE FROM MKT TABLE            
*                                                                               
         MVC   MYBTYP,2(RE)        SET BOOK TYPE                                
         CLI   BOOKTYPE,X'E0'      XSPILL                                       
         BNE   GETBTYPX                                                         
         NI    MYBTYP,B'10111111'   MAKE LOWER CASE                             
         B     GETBTYPX                                                         
                                                                                
GETBTYP7 DS    0H                                                               
         CLI   SAMPLEBT,0          IF BOOK TYPE FROM TAPE EXISTS,               
         BE    GETBTYPA                                                         
         CLI   BOOKTYPE,0          CHECK BOTH TYPE AND TYP2                     
         BNE   GETBTYP8                                                         
         CLI   BOOKTYP2,0           MAKE SURE ALSO SPECIFIED                    
         BNE   GETBTYP8                                                         
         LA    R1,1                                                             
         B     ERRFND                                                           
*                                                                               
GETBTYP8 CLC   SAMPLEBT,BOOKTYPE    CORRECTLY IN JCL                            
         BE    GETBTYPC                                                         
         CLI   SAMPLEBT,C'H'       FOR HISPANIC                                 
         BNE   GETBTYP9                                                         
         CLI   BOOKTYP2,C'P'       AND PEOPLE METER                             
         BNE   GETBTYP9                                                         
         MVI   MYBTYP,C'I'                                                      
         B     GETBTYPX                                                         
*                                                                               
GETBTYP9 LA    R1,2                WE ARE MANUALLY DOING ALL BTYPES             
         B     ERRFND                                                           
*                                                                               
GETBTYPA CLI   BOOKTYPE,0          NO SAMPLE (BOOK) TYPE FROM TAPE              
         BE    GETBTYPX                                                         
         CLI   BOOKTYPE,C'B'       DISALLOW OVERRIDE BOOKTYPE TO                
         BE    GETBTYPB                                                         
         CLI   BOOKTYPE,C'H'        BE BLACK OR HISPANIC                        
         BE    GETBTYPB                                                         
         CLI   BOOKTYPE,C'C'        OR CABLE                                    
         BNE   GETBTYPC                                                         
GETBTYPB LA    R1,3                                                             
         B     ERRFND                                                           
GETBTYPC MVC   MYBTYP,BOOKTYPE                                                  
*                                                                               
GETBTYPX SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXIT                                                             
         DROP  RC                                                               
GETBX    XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ ERROR FOUND ============================*         
*                                                                               
ERRFND   DS    0H                                                               
         LR    R0,RE                                                            
         SR    R0,RB               R0=LOCATION CALLING ERRFND                   
         LTR   R5,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R5,0                                                             
         MH    R5,=Y(L'ERRMSG00)                                                
         LA    R5,ERRMSG00(R5)            PRINT ERROR MESSAGE IN                
         L     R4,VCPRINT                                                       
         USING DPRINT,R4                                                        
         MVC   P(L'ERRMSG00),0(R5)                                              
         GOTO1 VPRINTER                    AND OUTPUT,                          
         DROP  R4                                                               
         MVI   INTAPESW,X'02'              SET SWITCH                           
         LTR   R1,R1                                                            
         B     EXIT                        AND EXIT                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== ERROR MESSAGES ==========================*         
*                                                                               
ERRMSG00 DS    0CL50                                                            
ERRMSG01 DC    CL50'**ERROR DEMCON** BOOKTYPE REQUIRED'                         
ERRMSG02 DC    CL50'**ERROR DEMCON** BOOKTYPE MISMATCH'                         
ERRMSG03 DC    CL50'**ERROR DEMCON** INVALID BOOKTYPE SPECIFIED'                
***********************************************************************         
         EJECT                                                                  
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4                                                                
S1EQU    EQU   8                                                                
S2EQU    EQU   16                                                               
OUTMARE  EQU   32                                                               
CANMARE  EQU   64                                                               
METROAE  EQU   1                                                                
METROBE  EQU   2                                                                
GLOBIND  EQU  4                                                                 
XFF      EQU   X'FF'                                                            
SUPRSTAT DS    XL1                                                              
SPACE    DC    CL15' '                                                          
ZEROS    DC    120C'0'                                                          
DEMTABS  DC    CL8'T00AD1'                                                      
CDEMTABS DS    V                                                                
MODLIST  DC    C'JW',C'FN'                                                      
         DC    X'FF'                                                            
SUPRTAB  DC    C'WTBS'                                                          
         DC    C'WGN '                                                          
         DC    X'FF'                                                            
PXETAB   DC    C'7780'                                                          
         DC    C'6788'                                                          
         DC    X'FF'                                                            
CYCTAB   DC    C'02',X'01'           JAN                                        
         DC    C'03',X'02'           FEB                                        
         DC    C'04',X'03'           MAR                                        
         DC    C'05',X'05'           MAY                                        
         DC    C'07',X'07'           JUL                                        
         DC    C'10',X'0A'           OCT                                        
         DC    C'11',X'0B'           NOV                                        
         DC    C'12',X'0C'           DEC                                        
         DC    X'00'                                                            
DAYTABL  DC    C'02',X'10'             MON                                      
         DC    C'03',X'20'             TUE                                      
         DC    C'04',X'30'             WED                                      
         DC    C'05',X'40'             THU                                      
         DC    C'06',X'50'             FRI                                      
         DC    C'07',X'60'             SAT                                      
         DC    C'08',X'70'             SUN                                      
         DC    C'01',X'95'             M-F                                      
         DC    X'00'                                                            
AFFO2N   DS    0CL9                                                             
         DC    CL7'ABC',CL2'A'     ABC NETWORK                                  
         DC    CL7'CBS',CL2'C'     CBS NETWORK                                  
         DC    CL7'FOX',CL2'F'     FOX NETWORK                                  
         DC    CL7'NBC',CL2'N'     NBC NETWORK                                  
         DC    CL7'PAX',CL2'PX'    PAX TV NETWORK                               
         DC    CL7'UPN',CL2'UP'    UPN NETWORK                                  
         DC    CL7'WB',CL2'WB'     THE WB NETWORK                               
         DC    CL7'IND',CL2'I'     INDEPENDENT                                  
         DC    CL7'PBS',CL2'P'     PBS                                          
         DC    CL7'PBS-C',CL2'PC'  PBS COMMERCIAL                               
         DC    CL7'IS',CL2'IS'     INDEPENDENT, SPANISH LANGUAGE                
         DC    CL7'IT',CL2'IT'     TURNER BROADCAST                             
         DC    CL7'TEL',CL2'T'     TELEMUNDO                                    
         DC    CL7'UNI',CL2'U'     UNIVISION                                    
         DC    X'00'                                                            
PSRCO2N  DS    0CL9                                                             
         DC    CL7'ABC',CL2'A'     ABC NETWORK                                  
         DC    CL7'CBS',CL2'C'     CBS NETWORK                                  
         DC    CL7'FOX',CL2'F'     FOX NETWORK                                  
         DC    CL7'L',CL2'L'       LOCAL                                        
         DC    CL7'LM',CL2'LM'     LOCAL MOVIE                                  
         DC    CL7'LN',CL2'LN'     LOCAL NEWS                                   
         DC    CL7'LS',CL2'LS'     LOCAL SPORTS                                 
         DC    CL7'NBC',CL2'N'     NBC NETWORK                                  
         DC    CL7'PAX',CL2'PX'    PAX TV NETWORK                               
         DC    CL7'UPN',CL2'UP'    UPN NETWORK                                  
         DC    CL7'WB',CL2'WB'     THE WB NETWORK                               
         DC    CL7'IND',CL2'I'     INDEPENDENT                                  
         DC    CL7'PBS',CL2'P'     PBS                                          
         DC    CL7'PBS-C',CL2'PC'  PBS COMMERCIAL                               
         DC    CL7'IS',CL2'IS'     INDEPENDENT, SPANISH LANGUAGE                
         DC    CL7'IT',CL2'IT'     TURNER BROADCAST                             
         DC    CL7'TEL',CL2'T'     TELEMUNDO                                    
         DC    CL7'UNI',CL2'U'     UNIVISION                                    
         DC    CL7'SYN',CL2'S'     SYNDICATED                                   
         DC    X'00'                                                            
                                                                                
CRHOMES  DS    CL9                                                              
CRHH1    DS    CL9                                                              
CRHH2    DS    CL9                                                              
CRHH3    DS    CL9                                                              
CRHH4    DS    CL9                                                              
CSHOMES  DS    CL9                                                              
CPMETROA DS    CL9                                                              
CSMETROA DS    CL9                                                              
CRMETROA DS    CL9                                                              
CPMETROB DS    CL9                                                              
CSMETROB DS    CL9                                                              
CRMETROB DS    CL9                                                              
CTHOMES  DS    CL9                                                              
CDHOMES  DS    CL9                                                              
CDMETROA DS    CL9                                                              
CDMETROB DS    CL9                                                              
HOMESEND DS    0C                                                               
*                                                                               
WRHOMES  EQU   1                                                                
WRHH1    EQU   2                                                                
WRHH2    EQU   3                                                                
WRHH3    EQU   4                                                                
WRHH4    EQU   5                                                                
WSHOMES  EQU   6                                                                
WPMETROA EQU   7                                                                
WSMETROA EQU   8                                                                
WRMETROA EQU   9                                                                
WPMETROB EQU   10                                                               
WSMETROB EQU   11                                                               
WRMETROB EQU   12                                                               
WTHOMES  EQU   13                                                               
WDHOMES  EQU   14                                                               
WDMETROA EQU   15                                                               
WDMETROB EQU   16                                                               
                                                                                
* TABLE TO CONVERT UNIVERSES FROM NSI TO DDS INTREC (M2 RECORD)                 
CUNV2INT DS    0C                                                               
         DC    AL1(RRV25,OUV25)                                                 
         DC    AL1(RRV611,OUV611)                                               
         DC    AL1(RRM1217,OUM1217)                                             
         DC    AL1(RRW1217,OUW1217)                                             
         DC    AL1(RRM1820,OUM1820)                                             
         DC    AL1(RRM2124,OUM2124)                                             
         DC    AL1(RRM2534,OUM2534)                                             
         DC    AL1(RRM3549,OUM3549)                                             
         DC    AL1(RRM5054,OUM5054)                                             
         DC    AL1(RRM5564,OUM5564)                                             
         DC    AL1(RRM65O,OUM65O)                                               
         DC    AL1(RRW1820,OUW1820)                                             
         DC    AL1(RRW2124,OUW2124)                                             
         DC    AL1(RRW2534,OUW2534)                                             
         DC    AL1(RRW3549,OUW3549)                                             
         DC    AL1(RRW5054,OUW5054)                                             
         DC    AL1(RRW5564,OUW5564)                                             
         DC    AL1(RRW65O,OUW65O)                                               
         DC    AL1(RRWWRK,OUWWRK)                                               
         DC    X'FF'                                                            
CDMA2INT DS    0C                                                               
         DC    AL1(RRV25,ODV25)                                                 
         DC    AL1(RRV611,ODV611)                                               
         DC    AL1(RRM1217,ODM1217)                                             
         DC    AL1(RRW1217,ODW1217)                                             
         DC    AL1(RRM1820,ODM1820)                                             
         DC    AL1(RRM2124,ODM2124)                                             
         DC    AL1(RRM2534,ODM2534)                                             
         DC    AL1(RRM3549,ODM3549)                                             
         DC    AL1(RRM5054,ODM5054)                                             
         DC    AL1(RRM5564,ODM5564)                                             
         DC    AL1(RRM65O,ODM65O)                                               
         DC    AL1(RRW1820,ODW1820)                                             
         DC    AL1(RRW2124,ODW2124)                                             
         DC    AL1(RRW2534,ODW2534)                                             
         DC    AL1(RRW3549,ODW3549)                                             
         DC    AL1(RRW5054,ODW5054)                                             
         DC    AL1(RRW5564,ODW5564)                                             
         DC    AL1(RRW65O,ODW65O)                                               
         DC    AL1(RRWWRK,ODWWRK)                                               
         DC    X'FF'                                                            
CTSA2INT DS    0C                                                               
         DC    AL1(RRV25,OTV25)                                                 
         DC    AL1(RRV611,OTV611)                                               
         DC    AL1(RRM1217,OTM1217)                                             
         DC    AL1(RRW1217,OTW1217)                                             
         DC    AL1(RRM1820,OTM1820)                                             
         DC    AL1(RRM2124,OTM2124)                                             
         DC    AL1(RRM2534,OTM2534)                                             
         DC    AL1(RRM3549,OTM3549)                                             
         DC    AL1(RRM5054,OTM5054)                                             
         DC    AL1(RRM5564,OTM5564)                                             
         DC    AL1(RRM65O,OTM65O)                                               
         DC    AL1(RRW1820,OTW1820)                                             
         DC    AL1(RRW2124,OTW2124)                                             
         DC    AL1(RRW2534,OTW2534)                                             
         DC    AL1(RRW3549,OTW3549)                                             
         DC    AL1(RRW5054,OTW5054)                                             
         DC    AL1(RRW5564,OTW5564)                                             
         DC    AL1(RRW65O,OTW65O)                                               
         DC    AL1(RRWWRK,OTWWRK)                                               
         DC    X'FF'                                                            
CHOM2INT DS    0C                      CONVERT HOMES TO INTERIM                 
         DC    AL1(WRHOMES,ORHOMES)                                             
         DC    AL1(WRHH1,ORHH1)                                                 
         DC    AL1(WRHH2,ORHH2)                                                 
         DC    AL1(WRHH3,ORHH3)                                                 
         DC    AL1(WRHH4,ORHH4)                                                 
         DC    AL1(WSHOMES,OSHOMES)                                             
         DC    AL1(WPMETROA,OPMETROA)                                           
         DC    AL1(WSMETROA,OSMETROA)                                           
         DC    AL1(WRMETROA,ORMETROA)                                           
         DC    AL1(WPMETROB,OPMETROB)                                           
         DC    AL1(WSMETROB,OSMETROB)                                           
         DC    AL1(WRMETROB,ORMETROB)                                           
         DC    AL1(WTHOMES,OTHOMES)                                             
         DC    AL1(WDHOMES,ODHOMES)                                             
         DC    AL1(WDMETROA,ODMETROA)                                           
         DC    AL1(WDMETROB,ODMETROB)                                           
         DC    X'FF'                                                            
                                                                                
CUNH2INT DS    0C                      CONVERT HOMES TO INTERIM                 
         DC    AL1(UUMETROA,OUMETROA)                                           
         DC    AL1(UUMETROB,OUMETROB)                                           
         DC    AL1(UUHOMES,OUHOMES)                                             
         DC    AL1(UUA1HH,OUA1HH)                                               
         DC    AL1(UUA2HH,OUA2HH)                                               
         DC    AL1(UUA3HH,OUA3HH)                                               
         DC    X'FF'                                                            
UUMETROA EQU   1                                                                
UUMETROB EQU   2                                                                
UUHOMES  EQU   4                                                                
UUA1HH   EQU   6                                                                
UUA2HH   EQU   7                                                                
UUA3HH   EQU   8                                                                
         LTORG                                                                  
         EJECT                                                                  
PASSFLT  DC    X'00'                                                            
PREVQH   DC    X'00'                                                            
HAV2WH   DC    X'00'                                                            
NFRST    DC    X'01'                                                            
FRST     DC    X'01'                                                            
RELOFRST DC    X'01'                                                            
NPRES    DC    X'00'                                                            
BYPREAD  DC    X'00'                                                            
PREVBTY  DC    X'00'                                                            
STASW    DC    X'00'                                                            
SAMPLEBT DC    X'00'                                                            
PIKSW    DC    X'00'               PROG INDEX RECD (I-POINTER) FLAG             
PKSWFFQ  EQU   X'80'                END I-POINTER RELEASED TO SORT              
PKSWIPTR EQU   X'40'                PROGRAM INDEX RECD CREATED                  
PARENTF  DC    X'00'                                                            
BOOKTYP2 DS    X'00'                                                            
ZEROCELL DS    X'00'                                                            
LIVEONLY DS    X'00'                                                            
LIVEPLU3 DS    X'00'                                                            
LIVEPLSD DS    X'00'                                                            
TEMPMNO  DS    CL6                                                              
TEMPMKT  DS    CL26                                                             
TEMPBK   DS    XL3                                                              
BTYPTAB  DS    0XL10                                                            
         DC    X'FF'                                                            
         DS    XL9                                                              
         DC    X'FF'                                                            
*                                                                               
MYBTYP   DS    CL(L'BOOKTYPE)                                                   
VAPROC   DS    F                                                                
VYPROC   DS    F                                                                
STALIST  DS    CL600                                                            
         DS    CL1800              EXTRA ROOM ADDED FOR BKTYP                   
NSIBOOKY DS    CL1                                                              
NSIBOOKM DS    CL1                                                              
NSIBKYM  DS    CL2                                                              
PREVSTYP DS    CL1                                                              
PREVSTAT DS    CL5                                                              
MYSVMKT  DS    XL2                                                              
FRCBTYP  DS    CL1                                                              
PRVQSTYP DS    CL2                                                              
*                                                                               
NEWLORIG DS    0XL4                NEW STATION THAT BECOMES LORIG               
         DC    CL4'LNCH'                                                        
         DC    X'FF'                                                            
*                                                                               
OLDLORIG DS    0XL10               OLD STATION THAT ALWAYS BEEN LORIG           
         DC    CL4'CSNN',CL6'000420'                                            
         DC    CL4'CSCA',CL6'000462'                                            
         DC    X'FF'                                                            
*                                                                               
PPLUSCAB DS    0XL4                NEW PARENT PLUS CABLE STATIONS               
         DC    CL4'N12 '                                                        
         DC    X'FF'                                                            
*                                                                               
M2RECA   DS    (RRECL)C                                                         
M5RECA   DS    (RRECL)C                                                         
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=RRECL,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
IN1A     DS    (10*RRECL)C                                                      
M4STALST DS    CL2000              STATIONS FOR M4 RECORDS                      
         EJECT                                                                  
* INTERIM RECORD DISPLACEMENTS                                                  
ORHOMES  EQU   1                                                                
ORHH1    EQU   2                                                                
ORHH2    EQU   3                                                                
ORHH3    EQU   4                                                                
ORHH4    EQU   5                                                                
OSHOMES  EQU   6                                                                
OPMETROA EQU   7                                                                
OSMETROA EQU   8                                                                
ORMETROA EQU   9                                                                
OPMETROB EQU   10                                                               
OSMETROB EQU   11                                                               
ORMETROB EQU   12                                                               
OTHOMES  EQU   13                                                               
ODHOMES  EQU   14                                                               
OTWWRK   EQU   15                                                               
ODWWRK   EQU   16                                                               
OTW65O   EQU   17                                                               
ODW65O   EQU   18                                                               
OTM65O   EQU   19                                                               
ODM65O   EQU   20                                                               
ODMETROA EQU   21                                                               
ODMETROB EQU   22                                                               
OTM1217  EQU   23                                                               
OTW1217  EQU   24                                                               
OTM1820  EQU   25                                                               
OTW1820  EQU   26                                                               
OTM2124  EQU   27                                                               
OTW2124  EQU   28                                                               
OTM2534  EQU   29                                                               
OTW2534  EQU   30                                                               
OTM3549  EQU   31                                                               
OTW3549  EQU   32                                                               
OTM5564  EQU   33                                                               
OTW5564  EQU   34                                                               
OTM5054  EQU   35                                                               
OTW5054  EQU   36                                                               
OTV25    EQU   37                                                               
ODV25    EQU   38                                                               
OTV611   EQU   39                                                               
ODV611   EQU   40                                                               
OUHOMES  EQU   41                                                               
OUM65O   EQU   42                                                               
OUW65O   EQU   43                                                               
OUWWRK   EQU   44                                                               
OUA1HH   EQU   45                                                               
OUA2HH   EQU   46                                                               
OUA3HH   EQU   47                                                               
OUMETROA EQU   48                                                               
OUMETROB EQU   49                                                               
OUM1217  EQU   50                                                               
OUW1217  EQU   51                                                               
OUM1820  EQU   52                                                               
OUW1820  EQU   53                                                               
OUM2124  EQU   54                                                               
OUW2124  EQU   55                                                               
OUM2534  EQU   56                                                               
OUW2534  EQU   57                                                               
OUM3549  EQU   58                                                               
OUW3549  EQU   59                                                               
OUM5564  EQU   60                                                               
OUW5564  EQU   61                                                               
OUM5054  EQU   62                                                               
OUW5054  EQU   63                                                               
OUV25    EQU   64                                                               
OUV611   EQU   65                                                               
ODM1217  EQU   66                                                               
ODW1217  EQU   67                                                               
ODM1820  EQU   68                                                               
ODW1820  EQU   69                                                               
ODM2124  EQU   70                                                               
ODW2124  EQU   71                                                               
ODM2534  EQU   72                                                               
ODW2534  EQU   73                                                               
ODM3549  EQU   74                                                               
ODW3549  EQU   75                                                               
ODM5564  EQU   76                                                               
ODW5564  EQU   77                                                               
ODM5054  EQU   78                                                               
ODW5054  EQU   79                                                               
OMHOMES  EQU   80                                                               
OMWWRK   EQU   81                                                               
OMW65O   EQU   82                                                               
OMM65O   EQU   83                                                               
OMMETROA EQU   84                                                               
OMMETROB EQU   85                                                               
OMM1217  EQU   86                                                               
OMW1217  EQU   87                                                               
OMM1820  EQU   88                                                               
OMW1820  EQU   89                                                               
OMM2124  EQU   90                                                               
OMW2124  EQU   91                                                               
OMM2534  EQU   92                                                               
OMW2534  EQU   93                                                               
OMM3549  EQU   94                                                               
OMW3549  EQU   95                                                               
OMM5564  EQU   96                                                               
OMW5564  EQU   97                                                               
OMM5054  EQU   98                                                               
OMW5054  EQU   99                                                               
OMV25    EQU   100                                                              
OMV611   EQU   101                                                              
OQHOMES  EQU   102                                                              
OQWWRK   EQU   103                                                              
OQW65O   EQU   104                                                              
OQM65O   EQU   105                                                              
OQM1217  EQU   106                                                              
OQW1217  EQU   107                                                              
OQM1820  EQU   108                                                              
OQW1820  EQU   109                                                              
OQM2124  EQU   110                                                              
OQW2124  EQU   111                                                              
OQM2534  EQU   112                                                              
OQW2534  EQU   113                                                              
OQM3549  EQU   114                                                              
OQW3549  EQU   115                                                              
OQM5564  EQU   116                                                              
OQW5564  EQU   117                                                              
OQM5054  EQU   118                                                              
OQW5054  EQU   119                                                              
OQV25    EQU   120                                                              
OQV211   EQU   121                                                              
ORV2O    EQU   122                                                              
ORV18O   EQU   123                                                              
ORV1234  EQU   124                                                              
ORV1224  EQU   125                                                              
ORV1217  EQU   126                                                              
ORV611   EQU   127                                                              
ORV211   EQU   128                                                              
ORW18O   EQU   129                                                              
ORW1834  EQU   130                                                              
ORW1849  EQU   131                                                              
ORW2549  EQU   132                                                              
ORW2554  EQU   133                                                              
ORW1224  EQU   134                                                              
ORW2564  EQU   135                                                              
ORW1234  EQU   136                                                              
ORM18O   EQU   137                                                              
ORM1834  EQU   138                                                              
ORM1849  EQU   139                                                              
ORM2554  EQU   140                                                              
ORM2564  EQU   141                                                              
ORWWRK   EQU   142                                                              
ORM2549  EQU   143                                                              
ORA1849  EQU   144                                                              
ORA1834  EQU   145                                                              
ORA2554  EQU   146                                                              
OPV2O    EQU   147                                                              
OPV18O   EQU   148                                                              
OPV1234  EQU   149                                                              
OPV1224  EQU   150                                                              
OPV1217  EQU   151                                                              
OPV611   EQU   152                                                              
OPV211   EQU   153                                                              
OPW18O   EQU   154                                                              
OPW1834  EQU   155                                                              
OPW1849  EQU   156                                                              
OPW2549  EQU   157                                                              
OPW2554  EQU   158                                                              
OPW1224  EQU   159                                                              
OPW2564  EQU   160                                                              
OPW1234  EQU   161                                                              
OPM18O   EQU   162                                                              
OPM1834  EQU   163                                                              
OPM1849  EQU   164                                                              
OPM2554  EQU   165                                                              
OPM2564  EQU   166                                                              
OPWWRK   EQU   167                                                              
OPM2549  EQU   168                                                              
OPA1849  EQU   169                                                              
OPA1834  EQU   170                                                              
OPA2554  EQU   171                                                              
         EJECT                                                                  
* FIELD DISPLACEMENTS FOR RATING SERVICE RECORDS                                
RRV25    EQU   1                                                                
RRV611   EQU   2                                                                
RRM1217  EQU   3                                                                
RRW1217  EQU   5                                                                
RRM1820  EQU   7                                                                
RRM2124  EQU   8                                                                
RRM2534  EQU   9                                                                
RRM3549  EQU   10                                                               
RRM5054  EQU   12                                                               
RRM5564  EQU   13                                                               
RRM65O   EQU   14                                                               
RRW1820  EQU   15                                                               
RRW2124  EQU   16                                                               
RRW2534  EQU   17                                                               
RRW3549  EQU   18                                                               
RRW5054  EQU   20                                                               
RRW5564  EQU   21                                                               
RRW65O   EQU   22                                                               
RRWWRK   EQU   23                                                               
*                                                                               
RRECL    EQU   875                                                              
         EJECT                                                                  
         LTORG                                                                  
************************************************************                    
MSGREC   NTR1  BASE=*,LABEL=*                                                   
         LA    RE,BTYPTAB          STORE UNIQUE BOOKTYPES IN TABLE              
MSGR05   CLI   0(RE),X'FF'                                                      
         BNE   MSGR10                                                           
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    MSGRX                                                            
         B     MSGR20                                                           
MSGR10   CLC   INTBTYP,0(RE)        NOT UNIQUE                                  
         BE    MSGRX                                                            
         LA    RE,1(RE)                                                         
         B     MSGR05                                                           
MSGR20   MVC   0(1,RE),INTBTYP                                                  
         MVI   1(RE),X'FF'                                                      
************************************************************                    
         L     R6,AMREC                                                         
         LA    R6,4(R6)                                                         
         MVC   0(3,R6),TEMPMNO+3   MARKET NUMBER                                
         LA    R6,3(R6)                                                         
         MVI   0(R6),C','          ,                                            
         LA    R6,1(R6)                                                         
         LA    R1,TEMPMKT+25       MARKET NAME - SPACES                         
MSGR30   CLI   0(R1),X'40'                                                      
         BNE   MSGR40                                                           
         SHI   R1,1                                                             
         B     MSGR30                                                           
*                                                                               
MSGR40   LA    RF,TEMPMKT          CALCULATE LENGTH                             
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),TEMPMKT                                                  
         AHI   R1,1                                                             
         AR    R6,R1                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         MVC   0(1,R6),INTBTYP     BOOKTYPE                                     
         CLI   INTBTYP,0                                                        
         BE    MSGR70                                                           
*                                                                               
         GOTO1 CDEMTABS,DMCB,SPBOOKTB                                           
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)           LENGTH OF TABLE ENTRIES                       
         USING SPBKTYPD,RE                                                      
*                                                                               
MSGR50   CLI   0(RE),X'FF'        MUST FIND MARKET IN TABLE                     
         BE    MSGR71                                                           
         CLC   INTBTYP,SPBKTYPN                                                 
         BE    *+10                                                             
         AR    RE,RF                                                            
         B     MSGR50                                                           
*                                                                               
         MVC   0(L'SPBKTYPA,R6),SPBKTYPA                                        
         AHI   R6,L'SPBKTYPA                                                    
         B     MSGR80                                                           
*                                                                               
MSGR70   MVC   0(1,R6),=C' '                                                    
MSGR71   AHI   R6,1                                                             
*                                                                               
MSGR80   MVI   0(R6),C','                                                       
         MVC   TEMPBK(2),NSIBKYM                                                
         MVI   TEMPBK+2,X'1'                                                    
         GOTO1 VDATCON,DMCB,(3,TEMPBK),(6,1(R6))                                
         AHI   R6,7                                                             
*                                                                               
         L     RF,AMREC                                                         
         SR    R6,RF                                                            
         STC   R6,1(RF)                                                         
         MVI   MSGFLAG,1                                                        
MSGRX    J     EXIT                                                             
*                                                                               
************************************************************                    
FLTMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         ICM   RF,1,FILTMRKT                                                    
         BZ    FLTMKTX                                                          
         OC    INTMRKT,INTMRKT                                                  
         BZ    FLTMKTX                                                          
         LA    R1,FILTMRKT+1                                                    
*                                                                               
         TM    FLAGS1,NEGATIVE_FILTMRKT                                         
         BZ    FLTMKT10                                                         
FLTMKT05 CLC   INTMRKT,0(R1)                                                    
         BNE   *+12                                                             
         MVI   PASSFLT,C'N'                                                     
         B     FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT05                                                      
         B     FLTMKTX                                                          
*                                                                               
FLTMKT10 CLC   INTMRKT,0(R1)                                                    
         BE    FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT10                                                      
         MVI   PASSFLT,C'N'                                                     
         B     FLTMKTX                                                          
FLTMKTX  J     EXIT                                                             
***********************************************************************         
SETKEY   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   DRCODE,DRCODEQU                                                  
         MVI   INTRTYP,C'R'                                                     
         MVI   DRMEDIA,C'T'                                                     
         MVI   DRSRC,C'N'                                                       
         MVC   DRSTAT,INTSTA                                                    
         TM    INTSTYP,X'20'                                                    
         BZ    *+14                SET FOR SPILL STATION                        
         MVC   DRKMKT,INTMRKT                                                   
         MVI   INTSPILL,C'Y'                                                    
*                                                                               
         LA    R1,PPLUSCAB         NEW PARENT PLUS CABLE NETWORKS               
SK10     CLI   0(R1),X'FF'                                                      
         BE    SK20                                                             
         CLC   INTSTA(4),0(R1)                                                  
         BE    SK15                                                             
         LA    R1,L'PPLUSCAB(R1)                                                
         B     SK10                                                             
SK15     MVI   INTSPILL,C'Y'       SINCE THEY ARE CABLE                         
         MVC   DRKMKT,INTMRKT      THEY ARE SPILL                               
*                                                                               
* ******* THESE NEED TO HAVE A MARKET QUALIFICATION ********                    
SK20     CLC   INTMRKT,=H'514'     NHI BOSTON DMA                               
         BE    *+10                                                             
         CLC   INTMRKT,=H'515'     NHI BOSTON CABLE                             
         BNE   *+14                                                             
         MVC   DRKMKT,INTMRKT                                                   
         MVI   INTSPILL,C'Y'                                                    
*  **********************************************************                   
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
*                                                                               
         OC    FRCBTYP,FRCBTYP                                                  
         BZ    *+8                                                              
         MVI   INTBTYP,C'A'        IF THERE ARE ANY BOOKTYPES FORCED            
*                                                                               
         MVC   DRBTYP,INTBTYP      SET INTERNAL BOOK TYPE                       
*                                                                               
*                                                                               
SETKEY1  CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEY3                                                          
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         ZIC   RF,INTWEEKS                                                      
         SR    RE,RE                                                            
         SLL   RF,28                                                            
         LA    R0,4                                                             
         LA    R1,0                                                             
SETKEY2  SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         LA    R1,1(R1)                                                         
         SR    RE,RE               CLEAR WEEK INDICATOR                         
         BCT   R0,SETKEY2                                                       
         STC   R1,DRHIQHR+1                                                     
         MVC   DRHIQHR+6(1),INTWEEKS                                            
         L     R7,AIREC                                                         
         USING DDSCT,R7                                                         
         MVC   DRHIQHR+2(4),D0140      THOMES                                   
         DROP  R7                                                               
         B     SETKEYX                                                          
*                                                                               
SETKEY3  MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYX             BEFORE SETKEY CALL                           
SETKEYX  J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMFILE                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMCNVD                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMTABD                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= IREC DSECT ============================*         
*          DATA SET DEINTD     AT LEVEL 005 AS OF 11/17/87                      
* DSECT TO COVER INTERIM CONVERSION RECORDS                                     
*                                                                               
INTERD   DSECT                                                                  
INTRECLN DS    F                   RECORD LENGTH                                
INTKEY   DS    CL30                SORT KEY                                     
INTVALS  DS    0CL20               FILTERABLE VALUES                            
INTRTYP  DS    CL1                 RECORD TYPE                                  
INTMRKT  DS    XL2                 MARKET NUMBER                                
INTSTA   DS    CL5                 STATION CALL LETTERS                         
INTBOOK  DS    XL2                 BOOK (BINARY YYMM)                           
INTSTYP  DS    XL1                 STATION TYPE                                 
INTDAYWK DS    XL1                 DAY & WEEK                                   
INTSQH   DS    XL1                 START QTR HR                                 
INTEQH   DS    XL1                 END QTR HR                                   
INTSPILL DS    C                   Y=SPILL MARKET                               
INTADUR  DS    CL1                 ACTUAL DURATION FOR PRGM RECS.               
INTPNO   DS    CL2                 PROG NUMBER(SYND AND NETW PROGS)             
INTBTYP  DS    CL1                 INTERNALLY GENERATED BOOK TYPE               
         DS    CL1                                                              
INTDATA  DS    0C                  ALPHA DATA & BINARY VALUES                   
         SPACE 2                                                                
*          DATA SET DEINTTPT3D AT LEVEL 004 AS OF 08/24/93                      
         SPACE 2                                                                
*                                  TPT FILE CONVERSION FIELDS                   
INTSAT   DS    CL5                 SATELLITE STATION CALL LETTERS               
         ORG   INTDATA                                                          
INTUNIVS DS    0XL4                UNIVERSE VALUES                              
         ORG   INTDATA                                                          
INTMTYP  DS    X                   MARKET TYPE                                  
INTDAY   DS    X                   DAY (START-END)                              
INTWEEKS DS    X                   ACTIVE WEEKS                                 
INTPNAM  DS    CL14                PROGRAM NAME                                 
INTPNAM6 DS    CL6                 6 CHAR PROGRAM NAME                          
INTPNUM  DS    XL4                 PROGRAM NUMBER                               
INTPTYP  DS    CL2                 PROGRAM TYPE                                 
INTAFFL  DS    CL5                 AFFILIATIONS                                 
INTREV   DS    CL1                 REVISION NUMBER                              
INTPRSRC DS    CL2                 PROGRAMMING SOURCE                           
INTACCS  DS    0X                  ACCUMULATOR VALUES                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*==================== MARKET INFORMATION RECORD 1 ====================*         
*                                                                               
M1DSECT  DSECT                                                                  
M1RCDE   DS    CL3                 RECORD ID =001                               
M1MNO    DS    CL6                 MARKET NUMBER                                
M1MRI    DS    CL1                 MULTIPLE RECORD INDICATOR 1-3                
         DS    CL2                 (FOR FUTURE USE) ZERO                        
         DS    CL3                 (FOR FUTURE USE) BLANK                       
         DS    CL5                 (FOR FUTURE USE) ZERO                        
         DS    CL4                 (FOR FUTURE USE) ZERO                        
         DS    CL6                 (FOR FUTURE USE) JIC                         
M1YR     DS    CL4       C         REPORT YEAR                                  
M1MO     DS    CL2       C         REPORT MONTH  1-12,99=DOES NOT APPLY         
         DS    CL4                 JIC                                          
M1START  DS    CL8                 SURVEY START MMDDYYYY                        
M1END    DS    CL8                 SURVEY END MMDDYYYY                          
         DS    CL1                 JIC                                          
M1WKS    DS    CL2                 NUMBER OF WEEKS MEASURED                     
*                                   MAY BE LESS THAT END-START                  
         DS    C                   JIC                                          
M1SDW1   DS    CL8                 WEEK1 MMDDYYYY                               
M1NDW1   DS    C                   NUMBER OF DAYS                               
M1SDW2   DS    CL8                 WEEK2 MMDDYYYY                               
M1NDW2   DS    C                   NUMBER OF DAYS                               
M1SDW3   DS    CL8                 WEEK3 MMDDYYYY                               
M1NDW3   DS    C                   NUMBER OF DAYS                               
M1SDW4   DS    CL8                 WEEK4 MMDDYYYY                               
M1NDW4   DS    C                   NUMBER OF DAYS                               
M1SDW5   DS    CL8                 WEEK5 MMDDYYYY                               
M1NDW5   DS    C                   NUMBER OF DAYS                               
M1SDW6   DS    CL8                 WEEK6 MMDDYYYY                               
M1NDW6   DS    C                   NUMBER OF DAYS                               
M1SDW7   DS    CL8                 WEEK7 MMDDYYYY                               
M1NDW7   DS    C                   NUMBER OF DAYS                               
M1SDW8   DS    CL8                 WEEK8 MMDDYYYY                               
M1NDW8   DS    C                   NUMBER OF DAYS                               
M1SDW9   DS    CL8                 WEEK9 MMDDYYYY                               
M1NDW9   DS    C                   NUMBER OF DAYS                               
M1SDW10  DS    CL8                 WEEK10 MMDDYYYY                              
M1NDW10  DS    C                   NUMBER OF DAYS                               
M1SDW11  DS    CL8                 WEEK11 MMDDYYYY                              
M1NDW11  DS    C                   NUMBER OF DAYS                               
M1SDW12  DS    CL8                 WEEK12 MMDDYYYY                              
M1NDW12  DS    C                   NUMBER OF DAYS                               
         DS    CL9                 JIC                                          
M1TZ     DS    CL2       C         TIME ZONE                                    
*                        01=EASTERN 02=CENTRAL 03=MOUNTAIN 04=PACIFIC           
*                        05=ALASKA  06=HAWAIIAN                                 
M1MTYP   DS    CL2       C         MARKET TYPE                                  
*                                01=SINGLE METRO/METERED 02=MULTI-METER         
*                                05=NON METRO/DIARY      07=MULTI-DIARY         
*                                09=SINGLE METRO/CENTRAL-DIARY                  
*                                99=OTHER                                       
M1SERV   DS    CL5                 NIELSEN SERVICE                              
*                                NSI = NIELSEN STATION INDEX                    
*                                NHI = NIELSEN HOMEVIDEO INDEX                  
*                                NHSI= NIELSEN HISPANIC STA INDEX               
M1RPTTYP DS    CL3                 REPORT TYPE CODE                             
*                                SB=NSI BLACK ETHNIC                            
*                                SC=NSI CABLE(CABLE NETS ONLY)                  
*                                SH=NSI HISPANIC ETHNIC                         
*                                SO=NSI OUTSIDE "SPILL" RPT                     
*                                SR=NSI SRA                                     
*                                SS=NSI STATE SUMMARY                           
*                                ST=NSI TOTAL MARKET(STAND VIP)                 
*                                SX=NSI EXCLUSION                               
*                                SZ=NSI OTHER                                   
*                                CC=NHI CABLE(CABLE HH ONLY)                    
*                                CT=NHI TOT MARKET(ALL HH)                      
*                                CZ=NHI OTHER                                   
*                                HH=NHSI HISPANIC                               
*                                HZ=NHSI OTHER                                  
         DS    C                   JIC                                          
M1SAMTY1 DS    C                   SAMPLE TYPE(GRP1 AREA)                       
*                                B=AFRICAN-AMERICAN                             
*                                C=CABLE                                        
*                                H=HISPANIC                                     
*                                T=TOTAL SAMPLE                                 
*                                Z=OTHER(CUSTOM SAMPLE)                         
M1SAMTY2 DS    C                   SAMPLE TYPE(GRP2 AREA)                       
*                                B=AFRICAN-AMERICAN                             
*                                C=CABLE                                        
*                                H=HISPANIC                                     
*                                T=TOTAL SAMPLE                                 
*                                Z=OTHER(CUSTOM SAMPLE)                         
M1SRCCDE DS    CL2                 BROADCAST/CABLE SOURCE CODE                  
*                                B=BROADCAST ONLY                               
*                                C=CABLE ONLY                                   
*                                O=OUTSIDE  "SPILL"                             
*                                R=BROAD/CAB(STANDARD CASE)                     
*                                Z=CUSTOM                                       
M1MDIND  DS    C                   METER/DIARY INDICATOR                        
*                                  D=DIARY                                      
*                                  M=METER AND/OR METER ADJ                     
*                                  P=PEOPLE METER                               
M1METIND DS    CL1                 METRO INDICATOR                              
*                                  0=NO MET/CENTRAL REPORTED                    
*                                  1=1 MET/CENTRAL REPORTED                     
*                                  2=2 MET/CENTRAL REPORTED                     
*                                  3=3 MET/CENTRAL REPORTED                     
M1ROUND  DS    CL1       C         ROUNDING CONTROL                             
*                                  1=THOUSANDS                                  
*                                  2=HUNDREDS                                   
M1SPACT  DS    CL1                 SPECIAL ACTIVITY IND.(Y=YES)                 
         DS    CL2       C         JIC                                          
M1INTAB  DS    CL(24*12) P         NET-IN-TAB COUNTS BY WEEK                    
M1QHTRND DS    CL24      C         QUARTER HOUR TRENDS (4)                      
M1ADJDMA DS    CL18      P         ADJACENT DMAS (3)                            
         DS    684C                FILLER                                       
         EJECT                                                                  
*==================== MARKET INFORMATION RECORD 2 ====================*         
*                         UNIVERSES                                             
M2DSECT  DSECT                                                                  
M2RCODE  DS    CL3       C         RECORD CODE = 002                            
M2MNO    DS    CL6       C         MARKET NUMBER                                
M2MRI    DS    CL1                 MULTIPLE RECORD INDICATOR 1-3                
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
         DS    CL4                 ZERO                                         
M2MN26   DS    CL26                NSI MARKET NAME SHORT                        
M2MN55   DS    CL55                NSI MARKET NAME LONG                         
         DS    CL44                JIC                                          
*                                                                               
* TV HOUSEHOLDS UNIVERSE ESTIMATES (0)                                          
M2MA     DS    CL9       C         METRO A                                      
M2MB     DS    CL9       C         METRO B                                      
M2MC     DS    CL9       C         METRO C                                      
M2DMA    DS    CL9       C         DMA/SRA                                      
         DS    CL9       C         JIC                                          
M2ADJ1   DS    CL9       C         ADJ DMA 1                                    
M2ADJ2   DS    CL9       C         ADJ DMA 2                                    
M2ADJ3   DS    CL9       C         ADJ DMA 3                                    
         DS    CL158               JIC                                          
M2NSD    DS    C                   NONSTANDARD DEMO INDICATOR                   
*                                C=CUSTOM (DEFINITIONS IN REC 05)               
*                                                                               
* DMA UNIVERSE ESTIMATES (00) OR (000)                                          
* NSI AREA OR GROUP 2 UNIVERSE ESTIMATES (00) OR (000)                          
*                                                                               
M2DK25   DS    CL9       C         KIDS                                         
M2DK611  DS    CL9       C                                                      
*                                  TEENS                                        
M2DM1214 DS    CL9       C                                                      
M2DM1517 DS    CL9       C                                                      
M2DF1214 DS    CL9       C                                                      
M2DF1517 DS    CL9       C                                                      
*                                                                               
M2DM1820 DS    CL9       C         MEN                                          
M2DM2124 DS    CL9       C                                                      
M2DM2534 DS    CL9       C                                                      
M2DM3544 DS    CL9       C                                                      
M2DM4549 DS    CL9       C                                                      
M2DM5054 DS    CL9       C                                                      
M2DM5564 DS    CL9       C                                                      
M2DM65O  DS    CL9       C                                                      
*                                                                               
M2DW1820 DS    CL9       C         WOMEN                                        
M2DW2124 DS    CL9       C                                                      
M2DW2534 DS    CL9       C                                                      
M2DW3544 DS    CL9       C                                                      
M2DW4549 DS    CL9       C                                                      
M2DW5054 DS    CL9       C                                                      
M2DW5564 DS    CL9       C                                                      
M2DW65O  DS    CL9       C                                                      
M2DWWRK  DS    CL9       C                                                      
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
M2NK25   DS    CL9       C         KIDS                                         
M2NK611  DS    CL9       C                                                      
*                                  TEENS                                        
M2NM1214 DS    CL9       C                                                      
M2NM1517 DS    CL9       C                                                      
M2NF1214 DS    CL9       C                                                      
M2NF1517 DS    CL9       C                                                      
*                                                                               
M2NM1820 DS    CL9       C         MEN                                          
M2NM2124 DS    CL9       C                                                      
M2NM2534 DS    CL9       C                                                      
M2NM3544 DS    CL9       C                                                      
M2NM4549 DS    CL9       C                                                      
M2NM5054 DS    CL9       C                                                      
M2NM5564 DS    CL9       C                                                      
M2NM65O  DS    CL9       C                                                      
*                                                                               
M2NW1820 DS    CL9       C         WOMEN                                        
M2NW2124 DS    CL9       C                                                      
M2NW2534 DS    CL9       C                                                      
M2NW3544 DS    CL9       C                                                      
M2NW4549 DS    CL9       C                                                      
M2NW5054 DS    CL9       C                                                      
M2NW5564 DS    CL9       C                                                      
M2NW65O  DS    CL9       C                                                      
M2NWWRK  DS    CL9       C                                                      
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    531C                                                             
*                                                                               
         EJECT                                                                  
*==================== MARKET INFORMATION RECORD 3 ====================*         
*                     MINIMUM SAMPLE SIZES                                      
M3DSECT  DSECT                                                                  
M3RCODE  DS    CL3       C         RECORD CODE = 003                            
M3MNO    DS    CL6       C         MARKET NUMBER                                
M3MRI    DS    CL1                 MULTIPLE RECORD INDICATOR 1-3                
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
         DS    CL4                 ZERO                                         
         DS    CL5                 ZERO                                         
M3MCLASS DS    CL3                 MARKET CLASS                                 
* GROUP 1 AREA DEMOGRAPHICS                                                     
M31K211  DS    CL6       C         KIDS                                         
M31K611  DS    CL6       C                                                      
*                                  TEENS                                        
M31V1217 DS    CL6       C                                                      
M31F1217 DS    CL6       C                                                      
*                                                                               
M31MO18  DS    CL6       C                                                      
M31M1834 DS    CL6       C                                                      
M31M1849 DS    CL6       C         MEN                                          
M31M2149 DS    CL6       C                                                      
M31M2549 DS    CL6       C                                                      
M31M2554 DS    CL6       C                                                      
M31MO50  DS    CL6       C                                                      
*                                                                               
M31WO18  DS    CL6       C                                                      
M31W1224 DS    CL6       C                                                      
M31W1834 DS    CL6       C                                                      
M31W1849 DS    CL6       C         WOMEN                                        
M31W2549 DS    CL6       C                                                      
M31W2554 DS    CL6       C                                                      
M31W2564 DS    CL6       C                                                      
M31WO50  DS    CL6       C                                                      
M31WWORK DS    CL6       C                                                      
*                                  VIEWERS                                      
M31VO2   DS    CL6       C                                                      
M31VO18  DS    CL6       C                                                      
M31V1224 DS    CL6       C                                                      
M31V1234 DS    CL6       C                                                      
M31V1834 DS    CL6       C                                                      
M31V1849 DS    CL6       C                                                      
M31V2149 DS    CL6       C                                                      
M31V2554 DS    CL6       C                                                      
M31VO35  DS    CL6       C                                                      
M31V3564 DS    CL6       C                                                      
M31VO50  DS    CL6       C                                                      
         DS    CL102     C JIC                                                  
         DS    CL12      C JIC                                                  
* GROUP 1 AREA DEMOGRAPHICS                                                     
M32K211  DS    CL6       C         KIDS                                         
M32K611  DS    CL6       C                                                      
*                                  TEENS                                        
M32V1217 DS    CL6       C                                                      
M32F1217 DS    CL6       C                                                      
*                                                                               
M32MO18  DS    CL6       C                                                      
M32M1834 DS    CL6       C                                                      
M32M1849 DS    CL6       C         MEN                                          
M32M2149 DS    CL6       C                                                      
M32M2549 DS    CL6       C                                                      
M32M2554 DS    CL6       C                                                      
M32MO50  DS    CL6       C                                                      
*                                                                               
M32WO18  DS    CL6       C                                                      
M32W1224 DS    CL6       C                                                      
M32W1834 DS    CL6       C                                                      
M32W1849 DS    CL6       C         WOMEN                                        
M32W2549 DS    CL6       C                                                      
M32W2554 DS    CL6       C                                                      
M32W2564 DS    CL6       C                                                      
M32WO50  DS    CL6       C                                                      
M32WWORK DS    CL6       C                                                      
*                                  VIEWERS                                      
M32VO2   DS    CL6       C                                                      
M32VO18  DS    CL6       C                                                      
M32V1224 DS    CL6       C                                                      
M32V1234 DS    CL6       C                                                      
M32V1834 DS    CL6       C                                                      
M32V1849 DS    CL6       C                                                      
M32V2149 DS    CL6       C                                                      
M32V2554 DS    CL6       C                                                      
M32VO35  DS    CL6       C                                                      
M32V3564 DS    CL6       C                                                      
M32VO50  DS    CL6       C                                                      
         DS    CL114     C ANY DEMO                                             
         DS    CL12      C JIC                                                  
* MINIMUM IN TABS FOR DAYPART                                                   
M3MITDM  DS    CL3                 METRO                                        
M3MITDD  DS    CL3                 DMA (GROUP 1 AREA)                           
M3MITDN  DS    CL3                 NSI (GROUP 2 AREA)                           
* MINIMUM IN TABS FOR M-F (ALL MULTI-WEEK COMBINATIONS)                         
M3MITMWM DS    CL3                 METRO                                        
M3MITMWD DS    CL3                 DMA (GROUP 1 AREA)                           
M3MITMWN DS    CL3                 NSI (GROUP 2 AREA)                           
* MINIMUM IN TABS FOR M-F (1-WEEK)                                              
M3MIT1WM DS    CL3                 METRO                                        
M3MIT1WD DS    CL3                 DMA (GROUP 1 AREA)                           
M3MIT1WN DS    CL3                 NSI (GROUP 2 AREA)                           
* MINIMUM IN TABS FOR INDIVIDUAL DAY (1WEEK AND ALL WEEK COMBINATIONS)          
M3MIT1DM DS    CL3                 METRO                                        
M3MIT1DD DS    CL3                 DMA (GROUP 1 AREA)                           
M3MIT1DN DS    CL3                 NSI (GROUP 2 AREA)                           
         DS    CL702     C SPARE                                                
* MARKET INFORMATION *4                                                         
M4DSECT  DSECT                                                                  
M4RCODE  DS    CL3       C         RECORD CODE = 004                            
M4MNO    DS    CL6       C         MARKET NUMBER                                
M4ADDREC DS    CL1                 ADDITIONAL RECORD INDICATOR  1-9             
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
M4SEQSR  DS    CL4                 STATION REPORT SEQUENCE                      
         DS    CL1                 JIC                                          
M4SCOLD  DS    CL4                 STATION CODE (OLD) 1-9095                    
M4SIDCDE DS    CL10                STATION ID CODE                              
M4SCALL  DS    CL12                STATION CALL LETTERS/GROUP NAME              
M4SGRPI  DS    CL1                 STATION GROUP INDICATOR(+=GRP INCL)          
M4ORIGD  DS    CL6                 DMA OF ORIGIN                                
M4CHAN   DS    CL7                 BROADCAST CHANNEL NUMBER                     
M4STYP   DS    CL2                 STATION TYPE CODE                            
*                                  01=REGULAR,02=PARENT                         
*                                  03=PARENT+SAT/CHILD STN GRP                  
*                                  07=SAT/CHILD STATION IN MKT                  
*                                  09 OUTSIDE STATION                           
M4RSTAT  DS    C                   REPORTABILTY STATUS                          
         DS    CL3                 JIC                                          
M4AFF1   DS    CL7                 PRIMARY AFFILIATION                          
M4AFF2   DS    CL7                 SECONDARY AFFILIATION                        
M4AFF3   DS    CL7                 TERTIARY AFFILIATION                         
M4PSCO   DS    CL4                 PARENT STA CODE(OLD)                         
M4PSID   DS    CL10                PARENT STATION ID CODE                       
M4PCALL  DS    CL12                PARENT STATION CALL/GROUP                    
M4S1PSCO DS    CL4                 SAT 1 STA CODE(OLD)                          
M4S1PSID DS    CL10                SAT 1 STATION ID CODE                        
M4S1PCAL DS    CL12                SAT 1 STATION CALL/GROUP                     
M4S2PSCO DS    CL4                 SAT 2 STA CODE(OLD)                          
M4S2PSID DS    CL10                SAT 2 STATION ID CODE                        
M4S2PCAL DS    CL12                SAT 2 STATION CALL/GROUP                     
M4S3PSCO DS    CL4                 SAT 3 STA CODE(OLD)                          
M4S3PSID DS    CL10                SAT 3 STATION ID CODE                        
M4S3PCAL DS    CL12                SAT 3 STATION CALL/GROUP                     
M4SATTAB DS    (26*17)C            REPEAT SAT FIELDS 17 TIMES                   
M4STNAM  DS    CL40                EXPANDED STATION NAME                        
M4DRIND  DS    C                   DATA RECORD IND (Y OR N)                     
         DS    727C                SPARE                                        
                                                                                
* MARKET INFORMATION *5                                                         
M5DSECT  DSECT                                                                  
M5RCODE  DS    CL3       C         RECORD CODE = 002                            
M5MNO    DS    CL6       C         MARKET NUMBER                                
M5ADDREC DS    CL1                 ADDITIONAL RECORD INDICATOR  1-9             
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
         DS    CL41                ZERO                                         
M5A1DEF  DS    CL14                GROUP1 AREA DEFINITION                       
M5A2DEF  DS    CL14                GROUP2 AREA DEFINITION                       
         DS    290C                JIC                                          
M5CUSTOM DS    C                   C=NON-STANDARD DEMO                          
*                                  USE DEMO DEFS BELOW                          
M5A1DNAM DS    CL(9*27)            AREA1 CUSTOM DEMO NAMES                      
M5A2DNAM DS    CL(9*27)            AREA2 CUSTOM DEMO NAMES                      
         DS    CL375               SPARE                                        
* *************REST OF DEFINITION REMAINS TO BE DONE************                
         EJECT                                                                  
*======================== TIME PERIOD RECORD =========================*         
*                                                                               
QHDSECT  DSECT                                                                  
QHRCDE   DS    CL3       C         RECORD CODE 300=FULL SURVEY                  
*                                   3XX XX=FIRST WEEK OF PROGRAM                
QHMNO    DS    CL6       C         MARKET NUMBER                                
QHMRI    DS    CL1       C         MULTIPLE RECORD INDICATOR                    
QHDCDE   DS    CL2       C         DAY SEQUENCE 01=M-F 02-08=DAY                
QHDALF   DS    CL3       C         DAY CODE ALPHA                               
QHQHCDE  DS    CL5       C         HH:MM:A/P MID=1200A                          
QHSSEQ   DS    CL4       C         REPORT SEQ                                   
         DS    C                                                                
QHSCDE   DS    CL4       C         STATION CODE(OLD)                            
QHSCDEN  DS    CL10      C         STATION CODE(NEW)                            
QHSCALL  DS    CL12      C         STATION CALL LETTERS/GROUP NAME              
         DS    CL6                 SPARE                                        
QHSGI    DS    CL1       C         STATION GROUP INDICATOR                      
*                                   +=STATION GROUP DEMOS                       
QHCHAN   DS    CL7                 CHANNEL XXX-YYY                              
*                                   XXX=PRIMARY,YYY=SECONDARY                   
QHSTYP   DS    CL2       C         STATION TYPE                                 
*                                   00=HUT/PUT 01=LOCAL                         
*                                   02=PARENT ONLY                              
*                                   05=PARENT+SAT/CHILD                         
*                                   07=SAT/CHILD                                
*                                   09=OUTSIDE                                  
         DS    CL4                                                              
QHSGINAM DS    CL12                PARENT GROUP NAME FOR SATS                   
         DS    CL5       C                                                      
QHPN14   DS    CL14      C         PROGRAM NAME (14)                            
QHPN6    DS    CL6       C         PROGRAM NAME (6)                             
QHNOW    DS    CL24      C         NUMBER OF WEEKS                              
QHDAYT   DS    CL2       C         NUMBER OF TELECAST                           
QHPDOM   DS    CL10      C         PROGRAM CODE DOMINANT                        
QHDMATND DS    CL20      C         TREND DMA SHARES OR HUT                      
QHDPTYP  DS    CL8       C         PROGRAM TYPE                                 
         DS    CL2       C                                                      
QHPNI1   DS    CL1       C         1=PROG NAMES DIFFER PAR/SAT/CHILD            
QHPNI2   DS    CL1       C         1=OTHER PROMMING INCLUDED IN AVG             
QHPNI3   DS    CL1       C         1=OFF AIR ONE OR MORE QH                     
QHPNI4   DS    CL1       C         1=ONE OR MORE QH WITHHELD                    
         DS    CL5                                                              
QHPRSRC  DS    CL7       C         PROGRAM SOURCE                               
         DS    CL29                (FOR FUTURE USE)                             
         SPACE 1                                                                
QHMARTG  DS    CL5       P         METRO A HH RTG                               
QHMAHUT  DS    CL5       P         METRO A HUT                                  
QHMATHS  DS    CL5       P         METRO A HOUSEHOLD SHARE                      
QHMATPH  DS    CL9       P         METRO A TOTAL PROJECTED HH (0)               
QHMBRTG  DS    CL5       P         METRO B HH RTG                               
QHMBHUT  DS    CL5       P         METRO B HUT                                  
QHMBTHS  DS    CL5       P         METRO B HOUSEHOLD SHARE                      
QHMBTPH  DS    CL9       P         METRO B TOTAL PROJECTED HH (0)               
QHMCRTG  DS    CL5       P         METRO C HH RTG                               
QHMCHUT  DS    CL5       P         METRO C HUT                                  
QHMCTHS  DS    CL5       P         METRO C HOUSEHOLD SHARE                      
QHMCTPH  DS    CL9       P         METRO C TOTAL PROJECTED HH (0)               
QHDRHH   DS    CL5       P         DMA HH RTG                                   
QHDHUT   DS    CL5       P         DMA HUT                                      
QHDRHS   DS    CL5       P         DMA HH SHARE                                 
QHDTPH   DS    CL9       P         DMA TOTAL PROJECTED HH (0)                   
QHDWRTG  DS    CL(12*5)  P         WEEKLY RATINGS                               
         DS    CL5                                                              
QHA1EST  DS    CL(27*9)  P         AREA1 DEMOS IN IMPRESSIONS(DMA)              
QHA2HG   DS    CL9                 TSA HOMES                                    
QHA2EST  DS    CL(27*9)  P         AREA2 DEMOS IN IMPRESSIONS(TSA)              
***********************************************************************         
         EJECT                                                                  
*                                                                               
AMRECD   DSECT                                                                  
AMMNO    DS    CL6                                                              
         DS    CL1                                                              
AMMKT    DS    CL26                                                             
         DS    CL1                                                              
AMBTYP   DS    CL1                                                              
AMRECL   EQU   *-AMRECD+4                                                       
*                                                                               
INTABD   DSECT                     DSECT FOR NET-IN-TAB-COUNTS                  
ITMETA   DS    CL3       P         METRO A                                      
ITMETB   DS    CL3       P         METRO B                                      
ITDMA    DS    CL3       P         DMA                                          
ITNTA    DS    CL3       P         NSI AREA                                     
*                                                                               
QHTD     DSECT                     DSECT FOR QUARTER-HOUR TRENDS                
QHTMO    DS    CL2       C         REPORT PERIOD                                
QHTYR    DS    CL2       C         REPORT YEAR                                  
*                                                                               
M4STATD  DSECT                     DSECT FOR REPORTABLE STATIONS                
M4TSCDE  DS    CL4       C         STATION CODE                                 
M4TSTATS DS    CL1       C         REPORTABILITY STATUS                         
M4TAFFL  DS    CL7       C         NETWORK AFFILIATION                          
M4TSTYP  DS    CL1       C         STATION TYPE                                 
M4STATLN EQU   *-M4TSCDE                                                        
         EJECT                                                                  
*          DATA SET DDDPRINT   AT LEVEL 001 AS OF 02/19/88                      
         SPACE 1                                                                
DPRINT   DSECT                                                                  
P        DS    CL132               USERS PRINT LINE - WILL BE CLEARED           
*                                  WITH SPACES AFTER PRINTING                   
HEAD1    DS    0CL132                                                           
HEADDATE DS    CL14                DATE DD MMM YY                               
         DS    CL5                                                              
HEADTIME DS    CL10                TIME HH.MM                                   
         DS    CL5                                                              
TITLE    DS    CL60                ENTRIES IN THIS FIELD ARE UNDERLINED         
         DS    CL5                                                              
HEADPAGE DS    CL09                PAGE NNNN                                    
         DS    CL5                                                              
HEADUSER DS    CL19                AVAILABLE FOR USER                           
         SPACE 1                                                                
MID1     DS    CL132               NON SPACE MID-HEADING LINES                  
MID2     DS    CL132               WILL BE PRINTED AFTER TITLE                  
MID3     DS    CL132                                                            
MID4     DS    CL132                                                            
         SPACE 1                                                                
SUB1     DS    CL132               NON SPACE SUB-HEADING LINES                  
SUB2     DS    CL132               WILL BE PRINTED AFTER MIDS                   
SUB3     DS    CL132                                                            
SPACES   DS    CL132               PRESET TO SPACES                             
         SPACE 1                                                                
SPACING  DS    CL4                 PRESET TO PRINT AND SINGLE SPACE             
*                                  (BL01) BYTES 3/4 CAN BE CHANGED              
         SPACE 1                                                                
LINE     DS    PL2                 LINE COUNT - PRESET TO PL2'75'               
*                                  HEADLINE PRINTING CAN BE FORCED BY           
*                                  SETTING THIS GREATER THAN MAXLINE            
         SPACE 1                                                                
MAXLINE  DS    PL2                 PRESET TO 60 LINES PER PAGE                  
         SPACE 1                                                                
PAGE     DS    PL4                 PAGE NUMBER - PACKED DECIMAL                 
         SPACE 1                   PRESET TO 1 - CAN BE RESET BY USER           
MONTHS   DS    CL36                12 X 3 BYTES  (JAN - DEC)                    
SPECDATE DS    CL12                FILL WITH DATE=YYMMDD FOR OVERRIDE           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ADSCT    DSECT                                                                  
A0000    DS    0C                                                               
         ORG   *+0089                                                           
A0090    DS    CL0003              RMETROA                                      
A0093    DS    CL0003              PMETROA                                      
A0096    DS    CL0003              SMETROA                                      
A0099    DS    CL0005              DMETROA                                      
A0104    DS    CL0005              *                                            
A0109    DS    CL0003              RMETROB                                      
A0112    DS    CL0003              PMETROB                                      
A0115    DS    CL0003              SMETROB                                      
A0118    DS    CL0005              DMETROB                                      
A0123    DS    CL0005              *                                            
A0128    DS    CL0003              RHOMES                                       
A0131    DS    CL0003              PHOMES                                       
A0134    DS    CL0003              SHOMES                                       
A0137    DS    CL0005              DHOMES                                       
A0142    DS    CL0005              *                                            
A0147    DS    CL0003              RHH1                                         
A0150    DS    CL0003              RHH2                                         
A0153    DS    CL0003              RHH3                                         
A0156    DS    CL0003              RHH4                                         
A0159    DS    CL0005              THOMES                                       
A0164    DS    CL0019              *                                            
A0183    DS    CL0004              TV25                                         
A0187    DS    CL0004              TV611                                        
A0191    DS    CL0004              *                                            
A0195    DS    CL0004              TM1217                                       
A0199    DS    CL0004              TW1217                                       
A0203    DS    CL0012              *                                            
A0215    DS    CL0004              TM1820                                       
A0219    DS    CL0004              TM2124                                       
A0223    DS    CL0004              TM2534                                       
A0227    DS    CL0004              TM3549                                       
A0231    DS    CL0004              TM5054                                       
A0235    DS    CL0004              TM5564                                       
A0239    DS    CL0004              TM65+                                        
A0243    DS    CL0004              *                                            
A0247    DS    CL0004              TW1820                                       
A0251    DS    CL0004              TW2124                                       
A0255    DS    CL0004              TW2534                                       
A0259    DS    CL0004              TW3549                                       
A0263    DS    CL0004              TW5054                                       
A0267    DS    CL0004              TW5564                                       
A0271    DS    CL0004              TW65+                                        
A0275    DS    CL0004              *                                            
A0279    DS    CL0004              TWWRK                                        
A0283    DS    CL0020              *                                            
A0303    DS    CL0004              DV25                                         
A0307    DS    CL0004              DV611                                        
A0311    DS    CL0004              *                                            
A0315    DS    CL0004              DM1217                                       
A0319    DS    CL0004              DW1217                                       
A0323    DS    CL0012              *                                            
A0335    DS    CL0004              DM1820                                       
A0339    DS    CL0004              DM2124                                       
A0343    DS    CL0004              DM2534                                       
A0347    DS    CL0004              DM3549                                       
A0351    DS    CL0004              DM5054                                       
A0355    DS    CL0004              DM5564                                       
A0359    DS    CL0004              DM65+                                        
A0363    DS    CL0008              *                                            
A0371    DS    CL0004              DW1820                                       
A0375    DS    CL0004              DW2124                                       
A0379    DS    CL0004              DW2534                                       
A0383    DS    CL0004              DW3549                                       
A0387    DS    CL0004              DW5054                                       
A0391    DS    CL0004              DW5564                                       
A0395    DS    CL0004              DW65+                                        
A0399    DS    CL0004              *                                            
A0403    DS    CL0004              DWWRK                                        
         LTORG                                                                  
         EJECT                                                                  
BDSCT    DSECT                                                                  
B0000    DS    0C                                                               
         ORG   *+0052                                                           
B0053    DS    CL0004              UMETROA                                      
B0057    DS    CL0004              UMETROB                                      
B0061    DS    CL0004              UHOMES                                       
B0065    DS    CL0004              *                                            
B0069    DS    CL0004              UA1HH                                        
B0073    DS    CL0004              UA2HH                                        
B0077    DS    CL0004              UA3HH                                        
B0081    DS    CL0220              *                                            
B0301    DS    CL0004              UV25                                         
B0305    DS    CL0004              UV611                                        
B0309    DS    CL0004              *                                            
B0313    DS    CL0004              UM1217                                       
B0317    DS    CL0004              UW1217                                       
B0321    DS    CL0012              *                                            
B0333    DS    CL0004              UM1820                                       
B0337    DS    CL0004              UM2124                                       
B0341    DS    CL0004              UM2534                                       
B0345    DS    CL0004              UM3549                                       
B0349    DS    CL0004              UM5054                                       
B0353    DS    CL0004              UM5564                                       
B0357    DS    CL0004              UM65+                                        
B0361    DS    CL0008              *                                            
B0369    DS    CL0004              UW1820                                       
B0373    DS    CL0004              UW2124                                       
B0377    DS    CL0004              UW2534                                       
B0381    DS    CL0004              UW3549                                       
B0385    DS    CL0004              UW5054                                       
B0389    DS    CL0004              UW5564                                       
B0393    DS    CL0004              UW65+                                        
B0397    DS    CL0004              *                                            
B0401    DS    CL0004              UWWRK                                        
         LTORG                                                                  
         EJECT                                                                  
CDSCT    DSECT                                                                  
C0000    DS    0C                                                               
         ORG   *+0071                                                           
C0072    DS    CL0002              RMETROA                                      
C0074    DS    CL0002              PMETROA                                      
C0076    DS    CL0002              SMETROA                                      
C0078    DS    CL0004              DMETROA                                      
C0082    DS    CL0002              RMETROB                                      
C0084    DS    CL0002              PMETROB                                      
C0086    DS    CL0002              SMETROB                                      
C0088    DS    CL0004              DMETROB                                      
C0092    DS    CL0002              RHOMES                                       
C0094    DS    CL0002              PHOMES                                       
C0096    DS    CL0002              SHOMES                                       
C0098    DS    CL0004              DHOMES                                       
C0102    DS    CL0002              RHH1                                         
C0104    DS    CL0002              RHH2                                         
C0106    DS    CL0002              RHH3                                         
C0108    DS    CL0002              RHH4                                         
C0110    DS    CL0004              THOMES                                       
C0114    DS    CL0004              TV25                                         
C0118    DS    CL0004              TV611                                        
C0122    DS    CL0004              TM1217                                       
C0126    DS    CL0004              TW1217                                       
C0130    DS    CL0004              TM1820                                       
C0134    DS    CL0004              TM2124                                       
C0138    DS    CL0004              TM2534                                       
C0142    DS    CL0004              TM3549                                       
C0146    DS    CL0004              TM5054                                       
C0150    DS    CL0004              TM5564                                       
C0154    DS    CL0004              TM65+                                        
C0158    DS    CL0004              TW1820                                       
C0162    DS    CL0004              TW2124                                       
C0166    DS    CL0004              TW2534                                       
C0170    DS    CL0004              TW3549                                       
C0174    DS    CL0004              TW5054                                       
C0178    DS    CL0004              TW5564                                       
C0182    DS    CL0004              TW65+                                        
C0186    DS    CL0004              TWWRK                                        
C0190    DS    CL0004              DV25                                         
C0194    DS    CL0004              DV611                                        
C0198    DS    CL0004              DM1217                                       
C0202    DS    CL0004              DW1217                                       
C0206    DS    CL0004              DM1820                                       
C0210    DS    CL0004              DM2124                                       
C0214    DS    CL0004              DM2534                                       
C0218    DS    CL0004              DM3549                                       
C0222    DS    CL0004              DM5054                                       
C0226    DS    CL0004              DM5564                                       
C0230    DS    CL0004              DM65+                                        
C0234    DS    CL0004              DW1820                                       
C0238    DS    CL0004              DW2124                                       
C0242    DS    CL0004              DW2534                                       
C0246    DS    CL0004              DW3549                                       
C0250    DS    CL0004              DW5054                                       
C0254    DS    CL0004              DW5564                                       
C0258    DS    CL0004              DW65+                                        
C0262    DS    CL0004              DWWRK                                        
         LTORG                                                                  
         EJECT                                                                  
DDSCT    DSECT                                                                  
D0000    DS    0C                                                               
         ORG   *+0091                                                           
D0092    DS    CL0004              RHOMES                                       
D0096    DS    CL0004              RHH1                                         
D0100    DS    CL0004              RHH2                                         
D0104    DS    CL0004              RHH3                                         
D0108    DS    CL0004              RHH4                                         
D0112    DS    CL0004              SHOMES                                       
D0116    DS    CL0004              PMETROA                                      
D0120    DS    CL0004              SMETROA                                      
D0124    DS    CL0004              RMETROA                                      
D0128    DS    CL0004              PMETROB                                      
D0132    DS    CL0004              SMETROB                                      
D0136    DS    CL0004              RMETROB                                      
D0140    DS    CL0004              THOMES                                       
D0144    DS    CL0004              DHOMES                                       
D0148    DS    CL0004              TWWRK                                        
D0152    DS    CL0004              DWWRK                                        
D0156    DS    CL0004              TW65+                                        
D0160    DS    CL0004              DW65+                                        
D0164    DS    CL0004              TM65+                                        
D0168    DS    CL0004              DM65+                                        
D0172    DS    CL0004              DMETROA                                      
D0176    DS    CL0004              DMETROB                                      
D0180    DS    CL0004              TM1217                                       
D0184    DS    CL0004              TW1217                                       
D0188    DS    CL0004              TM1820                                       
D0192    DS    CL0004              TW1820                                       
D0196    DS    CL0004              TM2124                                       
D0200    DS    CL0004              TW2124                                       
D0204    DS    CL0004              TM2534                                       
D0208    DS    CL0004              TW2534                                       
D0212    DS    CL0004              TM3549                                       
D0216    DS    CL0004              TW3549                                       
D0220    DS    CL0004              TM5564                                       
D0224    DS    CL0004              TW5564                                       
D0228    DS    CL0004              TM5054                                       
D0232    DS    CL0004              TW5054                                       
D0236    DS    CL0004              TV25                                         
D0240    DS    CL0004              DV25                                         
D0244    DS    CL0004              TV611                                        
D0248    DS    CL0004              DV611                                        
D0252    DS    CL0004              UHOMES                                       
D0256    DS    CL0004              UM65+                                        
D0260    DS    CL0004              UW65+                                        
D0264    DS    CL0004              UWWRK                                        
D0268    DS    CL0004              UA1HH                                        
D0272    DS    CL0004              UA2HH                                        
D0276    DS    CL0004              UA3HH                                        
D0280    DS    CL0004              UMETROA                                      
D0284    DS    CL0004              UMETROB                                      
D0288    DS    CL0004              UM1217                                       
D0292    DS    CL0004              UW1217                                       
D0296    DS    CL0004              UM1820                                       
D0300    DS    CL0004              UW1820                                       
D0304    DS    CL0004              UM2124                                       
D0308    DS    CL0004              UW2124                                       
D0312    DS    CL0004              UM2534                                       
D0316    DS    CL0004              UW2534                                       
D0320    DS    CL0004              UM3549                                       
D0324    DS    CL0004              UW3549                                       
D0328    DS    CL0004              UM5564                                       
D0332    DS    CL0004              UW5564                                       
D0336    DS    CL0004              UM5054                                       
D0340    DS    CL0004              UW5054                                       
D0344    DS    CL0004              UV25                                         
D0348    DS    CL0004              UV611                                        
D0352    DS    CL0004              DM1217                                       
D0356    DS    CL0004              DW1217                                       
D0360    DS    CL0004              DM1820                                       
D0364    DS    CL0004              DW1820                                       
D0368    DS    CL0004              DM2124                                       
D0372    DS    CL0004              DW2124                                       
D0376    DS    CL0004              DM2534                                       
D0380    DS    CL0004              DW2534                                       
D0384    DS    CL0004              DM3549                                       
D0388    DS    CL0004              DW3549                                       
D0392    DS    CL0004              DM5564                                       
D0396    DS    CL0004              DW5564                                       
D0400    DS    CL0004              DM5054                                       
D0404    DS    CL0004              DW5054                                       
D0408    DS    CL0004              MHOMES                                       
D0412    DS    CL0004              MWWRK                                        
D0416    DS    CL0004              MW65+                                        
D0420    DS    CL0004              MM65+                                        
D0424    DS    CL0004              MMETROA                                      
D0428    DS    CL0004              MMETROB                                      
D0432    DS    CL0004              MM1217                                       
D0436    DS    CL0004              MW1217                                       
D0440    DS    CL0004              MM1820                                       
D0444    DS    CL0004              MW1820                                       
D0448    DS    CL0004              MM2124                                       
D0452    DS    CL0004              MW2124                                       
D0456    DS    CL0004              MM2534                                       
D0460    DS    CL0004              MW2534                                       
D0464    DS    CL0004              MM3549                                       
D0468    DS    CL0004              MW3549                                       
D0472    DS    CL0004              MM5564                                       
D0476    DS    CL0004              MW5564                                       
D0480    DS    CL0004              MM5054                                       
D0484    DS    CL0004              MW5054                                       
D0488    DS    CL0004              MV25                                         
D0492    DS    CL0004              MV611                                        
D0496    DS    CL0004              QHOMES                                       
D0500    DS    CL0004              QWWRK                                        
D0504    DS    CL0004              QW65+                                        
D0508    DS    CL0004              QM65+                                        
D0512    DS    CL0004              QM1217                                       
D0516    DS    CL0004              QW1217                                       
D0520    DS    CL0004              QM1820                                       
D0524    DS    CL0004              QW1820                                       
D0528    DS    CL0004              QM2124                                       
D0532    DS    CL0004              QW2124                                       
D0536    DS    CL0004              QM2534                                       
D0540    DS    CL0004              QW2534                                       
D0544    DS    CL0004              QM3549                                       
D0548    DS    CL0004              QW3549                                       
D0552    DS    CL0004              QM5564                                       
D0556    DS    CL0004              QW5564                                       
D0560    DS    CL0004              QM5054                                       
D0564    DS    CL0004              QW5054                                       
D0568    DS    CL0004              QV25                                         
D0572    DS    CL0004              QV211                                        
         LTORG                                                                  
         EJECT                                                                  
ZDSCT    DSECT                                                                  
         ORG   *+0000                                                           
         DC    X'00'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030DETN0407  03/21/14'                                      
         END                                                                    
