*          DATA SET DEBBM9I    AT LEVEL 031 AS OF 03/12/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEBBM9IA                                                                 
         TITLE 'BBM METERED TAPE CONVERSION'                                    
DEBBM9I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEBBM9I,RA                                                     
         USING DEMCOND,R8                                                       
         L     RC,ARREC            RC -> BBM INPUT RECD                         
         LA    RC,4(RC)                                                         
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
*                                                                               
         B     READ                GET INPUT1 (ARREC - INT )                    
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
READ     CLI   DONEPROG,C'Y'                                                    
         BE    READ2                                                            
         CLI   INTAPESW,1                                                       
         BE    OPENOK1                                                          
*                                                                               
         OPEN  (INPROG,(INPUT))                                                 
         L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'416'     HEADER                                       
*                                                                               
* A "PUTBUFF" BUFFER IS DEFINED STATICALLY WITHIN DEDEMCNV, BUT AS OF           
* NOW (MAR/2014) IT ISN'T LARGE ENOUGH FOR THIS CONVERSION. RATHER THAN         
* ENLARGE THAT BUFFER IN DEDEMCNV, WE JUST ALLOCATE A NEW BUFFER HERE,          
* AND OVERWRITE THE VPUTBUFF ADCON. THIS UPDATED ADCON IS THEN PICKED           
* UP AUTOMATICALLY BY THE CONVERSION OPHASE, AND ALL IS TRANSPARENT.            
*                                                                               
         L     R3,=A(PUTBUFF_MAX_ACCUMS)                                        
         MHI   R3,4                X L'FULLWORD                                 
         LA    R3,8(R3)            FOR EYE-CATCHER(8)                           
         STORAGE OBTAIN,LENGTH=(R3)                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*PUTBUF*'  R1 = A(OBTAINED STORAGE)                   
         LA    R1,8(R1)            BUMP PAST EYE-CATCHER                        
         ST    R1,VPUTBUFF         OVERWRITE DEMCON'S ADDRESS                   
*                                                                               
OPENOK1  DS    0H                                                               
         GET   INPROG,(RC)             READ IN RECD FROM TAPE                   
*                                                                               
         LA    RE,OUTREC           CLEAR TEMP OUTPUT BUFFER AREA                
         XCEF  (RE),1004                                                        
*                                                                               
         MVC   MYRECTY,0(RC)       FIRST BYTE  OF INPUT REC                     
         CLI   MYRECTY,C'1'        STATION AVG RECDS???                         
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'2'        STATION AVG RECDS???                         
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'3'        UNIVERSE RECORD???                           
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'5'        STATION AVG RECDS???                         
         BE    OPENOK1                                                          
************************************************************                    
* NOT ALL THE MARKETS GIVEN TO US ARE SUPPORTED                                 
* HOME STATIONS ETC WILL NEED TO BE UPDATED WITH EACH MARKET                    
************************************************************                    
         CLC   =C'001',1(RC)       VANCOUVER/VICTORIA (EM)                      
         BE    OPENOK3                                                          
         CLC   =C'103',1(RC)       TORONTO/HAMILTON (EM)                        
         BE    OPENOK3                                                          
         CLC   =C'300',1(RC)       CALGARY (EM)                                 
         BE    OPENOK3                                                          
         CLC   =C'350',1(RC)       EDMONTON (EM)                                
         BE    OPENOK3                                                          
         CLC   =C'275',1(RC)       MONTREAL ANGLO (EM)                          
         BE    OPENOK3                                                          
         CLC   =C'250',1(RC)       MONTREAL FRANCO (EM)                         
         BE    OPENOK3                                                          
         CLC   =C'100',1(RC)       ONTARIO                                      
         BE    OPENOK3                                                          
         CLC   =C'200',1(RC)       QUEBEC FRANCO                                
         BE    OPENOK3                                                          
         CLC   =C'998',1(RC)       ENGLISH CANADA                               
         BNE   OPENOK1                                                          
* NO LONGER SUPPORT MARKET 999 - AGENCIES DO NOT WANT TOTAL CANADA UNV          
***      BE    OPENOK3                                                          
***      CLC   =C'999',1(RC)       TOTAL CANADA (AKA "NATIONAL")                
***      BNE   OPENOK1                                                          
************************************************************                    
OPENOK3  MVC   SVPNC,1(RC)         SAVE THE PANEL CODE                          
****                                                                            
* CONVERT THE STATION CODE TO CALL LETTERS                                      
         USING PROGNAMD,RC                                                      
*                                                                               
*******  CLC   =C'275',1(RC)       ALL MONTREAL ANGLO FORCE SPILL               
*******  BNE   *+8                                                              
*******  MVI   INTSPILL,C'Y'                                                    
*                                                                               
         CLC   =C'250',1(RC)       ALL MONTREAL FR FORCE SPILL                  
         BNE   *+8                                                              
         MVI   INTSPILL,C'Y'                                                    
*                                                                               
         LA    RE,STATTAB                                                       
READP05  CLC   =X'FFFF',0(RE)                                                   
         BE    READP13                                                          
         CLC   PROGSTTCD,0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'STATTAB(RE)                                                 
         B     READP05                                                          
         MVC   MYCALL,3(RE)                                                     
         B     READP15                                                          
*                                                                               
READP13  MVI   MYCALL,C'B'         SET FOR BBM UNKNOWN                          
         MVC   MYCALL+1(3),PROGSTTCD                                            
         MVI   INTSPILL,C'Y'       AND FORCE TO SPILL                           
         B     READP15                                                          
*                                                                               
READP15  CLI   BYPREAD,1           RESTORE STATION CALL LETTERS                 
         BNE   *+10                IF NOT ORIGINAL READ                         
         MVC   MYCALL(5),PREVSTAT                                               
*                                                                               
         CLC   MYCALL(3),=C'ALL'  BYPASS ALL AND OTHER RECDS                    
         BE    OPENOK1                                                          
         CLC   MYCALL(5),=C'OTHER'                                              
         BE    OPENOK1                                                          
         CLC   MYCALL(5),=C'CKPG+'                                              
         BNE   *+8                                                              
         MVI   MYCALL+3,C'Z'                                                    
         CLI   MYCALL+3,C'+'                                                    
         BNE   *+8                                                              
         MVI   MYCALL+3,C' '                                                    
         CLC   MYCALL(5),=C'R-CAN'                                              
         BNE   *+10                                                             
         MVC   MYCALL(4),=C'RCAN'                                               
         MVI   MYCALL+4,C'T'                                                    
         MVC   PREVSTAT,MYCALL                                                  
         MVC   INTSTA,MYCALL                                                    
         MVC   SVCALL,INTSTA                                                    
*                                                                               
****  FIGURE OUT START QTR HOUR 6PM = ZERO QTR                                  
         MVC   INTPNAM,PROGTITLE                                                
         OC    INTPNAM,=CL24' '                                                 
*  THE START TIME IS IN MILITARY CHARS CONVERT TO BINARY                        
         CLI   MYRECTY,C'2'                                                     
         BE    READP30                                                          
         PACK  DUB(8),PROGSTTM(4)                                               
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH                                         
*                                                                               
         SR    R4,R4                                                            
         PACK  DUB(8),PROGDUR(4)                                                
         CVB   R5,DUB                                                           
         STCM  R5,3,HALF                                                        
         LA    RE,15                                                            
         DR    R4,RE                                                            
         OR    R4,R4                                                            
         BZ    *+8                                                              
         AHI   R5,1                                                             
         ZIC   RE,INTSQH                                                        
         AR    R5,RE                                                            
         STC   R5,INTEQH                                                        
*                                                                               
*  GET  INTDAY                                                                  
         LA    RE,DAYTAB           START DAY                                    
READP20  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID START DAY ON INPUT RECD              
         CLC   PROGDAY,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'DAYTAB(RE)                                                  
         B     READP20                                                          
         MVC   INTDAY,1(RE)                                                     
*                                                                               
READP30  DS    0H                                                               
         MVI   STASW,0             SWITCH 3 FOR PROG RECS                       
         CLI   MYRECTY,C'2'        UNIV RECS                                    
         BNE   *+8                                                              
         MVI   STASW,2             SWITCH 2 FOR UNIV PROG RECS                  
*                                                                               
         BAS   RE,SETKEYP                                                       
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*   =============== BBM QTRFILE ======================                          
**********************************************************************          
         DROP  RC                                                               
READ2    DS    0H                                                               
         L     RC,ARREC            RC -> BBM INPUT RECD                         
         LA    RC,4(RC)                                                         
*                                                                               
         L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'606'     HEADER                                       
*                                                                               
OPENOK   DS    0H                                                               
         CLI   BYPREAD,0                                                        
         BNE   OPENOK10                                                         
         GET   IN,(RC)             READ IN RECD FROM TAPE                       
OPENOK10 LA    RE,OUTREC           CLEAR TEMP OUTPUT BUFFER AREA                
         XCEF  (RE),1004                                                        
*                                                                               
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
*                                                                               
         MVC   MYRECTY,0(RC)       FIRST BYTE OF INPUT REC                      
         CLI   MYRECTY,C'1'        HEADER RECORD                                
         BE    HEADER                                                           
         CLI   MYRECTY,C'5'        TRAILER RECORD                               
         BE    OPENOK              YES: IGNORE IT                               
*                                                                               
         CLC   =C'001',1(RC)       VANCOUVER/VICTORIA (EM)                      
         BE    OPENOK20                                                         
         CLC   =C'103',1(RC)       TORONTO/HAMILTON (EM)                        
         BE    OPENOK20                                                         
         CLC   =C'300',1(RC)       CALGARY (EM)                                 
         BE    OPENOK20                                                         
         CLC   =C'350',1(RC)       EDMONTON (EM)                                
         BE    OPENOK20                                                         
         CLC   =C'275',1(RC)       MONTREAL ANGLO (EM)                          
         BE    OPENOK20                                                         
         CLC   =C'250',1(RC)       MONTREAL FRANCO (EM)                         
         BE    OPENOK20                                                         
         CLC   =C'100',1(RC)       ONTARIO                                      
         BE    OPENOK20                                                         
         CLC   =C'200',1(RC)       QUEBEC FRANCO                                
         BE    OPENOK20                                                         
         CLC   =C'998',1(RC)       ENGLISH CANADA                               
         BE    OPENOK20                                                         
         B     OPENOK                                                           
* NO LONGER SUPPORT MARKET 999 - AGENCIES DO NOT WANT TOTAL CANADA UNV          
*********BE    *+10                                                             
*********CLC   =C'999',1(RC)       TOTAL CANADA (AKA "NATIONAL")                
*********BNE   OPENOK                                                           
*                                                                               
OPENOK20 DS    0H                                                               
         CLI   MYRECTY,C'2'        UNIV RECS                                    
         BE    UNIVS                                                            
         CLI   MYRECTY,C'3'        STATION AVG RECDS???                         
         BE    OPENOK                                                           
         CLI   MYRECTY,C'4'        STATION AVG RECDS???                         
         BE    OUTRTN                                                           
*                                                                               
         B     OPENOK              BYPASS UNKOWN RECD TYPES                     
         EJECT                                                                  
*                                                                               
**********************************************************************          
HEADER   DS    0H                                                               
         USING HEADERD,RC                                                       
         XC    DUB,DUB                                                          
         PACK  DUB,HEADYR+2(2)                                                  
         CVB   RE,DUB                                                           
         CLI   HEADYR,C'2'         Y2K ADJUST                                   
         BNE   *+8                                                              
         LA    RE,100(RE)                                                       
         STC   RE,SVBOOK           YEAR FROM HEADER RECORD                      
         MVC   TEMPDATE(2),HEADYR+2                                             
         MVC   TEMPDATE+2(2),HEADDM+2                                           
         MVC   TEMPDATE+4(2),HEADDM                                             
         GOTO1 VNETWEEK,DMCB,TEMPDATE,VGETDAY,VADDAY                            
         ZIC   RE,8(R1)                                                         
         STC   RE,SVWEEK           WEEK FROM HEADER RECORD                      
         STC   RE,SVBOOK+1         WEEK FROM HEADER RECORD                      
         ZIC   RE,4(R1)                                                         
         STC   RE,SVBOOK                                                        
         B     OPENOK              BYPASS UNKOWN RECD TYPES                     
         DROP  RC                                                               
*                                                                               
**********************************************************************          
*UNIVS - SLOT UNIVERSES FROM MARKET LEVEL RECD RECTYPE=9 INTO                   
*        SAVUNIV BUFFER TO COPY INTO EACH STATION RECD LATER                    
**********************************************************************          
UNIVS    DS    0H                                                               
         BAS   RE,BLDHOM           ON RETURN, HOMETAB IS FILLED                 
UNIV40   LA    RE,STALIST          CLEAR TEMP OUTPUT BUFFER AREA                
         XCEF  (RE),L'STALIST                                                   
         BAS   RE,SLOTDEMS         SAVE UNIVS IN SAVUNIV BUFFER                 
         B     OPENOK                                                           
         EJECT                                                                  
*                                                                               
**********************************************************************          
* OUTRTN: BUILD RECORD INTO INTERIM RECORD                                      
**********************************************************************          
*                                                                               
OUTRTN   DS    0C                                                               
         USING QTRHOURD,RC                                                      
*                                                                               
* CONVERT THE STATION CODE TO CALL LETTERS                                      
*                                                                               
******   CLC   =C'275',1(RC)       ALL MONTREAL FR FORCE SPILL                  
******   BNE   *+8                                                              
******   MVI   INTSPILL,C'Y'                                                    
*                                                                               
         CLC   =C'250',1(RC)       ALL MONTREAL FR FORCE SPILL                  
         BNE   *+8                                                              
         MVI   INTSPILL,C'Y'                                                    
*                                                                               
         LA    RE,STATTAB                                                       
OUTRN05  CLC   =X'FFFF',0(RE)                                                   
         BE    OUTRN12                                                          
         CLC   QTRSTACD,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,L'STATTAB(RE)                                                 
         B     OUTRN05                                                          
         MVC   MYCALL,3(RE)                                                     
*                                                                               
         LA    RE,HOMETAB                                                       
OUTRN07  CLI   0(RE),0                                                          
         BE    OUTRN08                                                          
         CLC   MYCALL(4),0(RE)                                                  
         BE    OUTRN15                                                          
         LA    RE,5(RE)                                                         
         B     OUTRN07                                                          
*                                                                               
OUTRN08  MVI   INTSPILL,C'Y'       SINCE SPILL STATION WE WILL SET              
         B     OUTRN15             FLAG FOR SPILL                               
*                                                                               
OUTRN12  MVI   MYCALL,C'B'                                                      
         MVC   MYCALL+1(3),QTRSTACD                                             
         MVI   INTSPILL,C'Y'                                                    
         B     OUTRN15                                                          
*                                                                               
OUTRN15  CLI   BYPREAD,1           RESTORE STATION CALL LETTERS                 
         BNE   *+10                IF NOT ORIGINAL READ                         
         MVC   MYCALL(5),PREVSTAT                                               
         MVC   PREVSTAT,MYCALL                                                  
         MVC   INTSTA,MYCALL                                                    
*                                                                               
         CLC   MYCALL(3),=C'ALL'  BYPASS ALL AND OTHER RECDS                    
         BE    OPENOK                                                           
         CLC   MYCALL(5),=C'OTHER'                                              
         BE    OPENOK                                                           
         CLC   MYCALL(5),=C'CKPG+'                                              
         BNE   *+8                                                              
         MVI   MYCALL+3,C'Z'                                                    
         CLI   MYCALL+3,C'+'                                                    
         BNE   *+8                                                              
         MVI   MYCALL+3,C' '                                                    
         CLC   MYCALL(5),=C'R-CAN'                                              
         BNE   *+10                                                             
         MVC   MYCALL(4),=C'RCAN'                                               
         MVI   MYCALL+4,C'T'                                                    
         MVC   SVCALL,INTSTA                                                    
         MVC   INTMRKT,MKT                                                      
         MVC   INTPNAM(9),=C'NOT GIVEN'                                         
*                                                                               
*  GET INTSDAY INTO INTDAY                                                      
         LA    RE,DAYTAB           START DAY                                    
OUT20    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID START DAY ON INPUT RECD              
         CLC   QTRDAY,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,L'DAYTAB(RE)                                                  
         B     OUT20                                                            
*                                                                               
         MVC   INTDAY,1(RE)                                                     
         MVC   INTSQH,QTRQHR                                                    
         MVC   INTEQH,QTRQHR                                                    
         MVC   INTWEEKS,SVWEEK                                                  
         MVI   INTWEEKS,X'21'      FORCE 1 WEEK LOOKUP                          
         MVI   INTBTYP,C'W'                                                     
         MVI   INTSTYP,PARENTE                                                  
         CLI   GLOBAL,C'Y'                                                      
         BE    GETHOM2                                                          
*                                                                               
         CLI   INTSPILL,C'Y'                                                    
         BNE   GETHOM2                                                          
         OI    INTSTYP,OUTMARE                                                  
GETHOM2  DS    0H                                                               
         MVC   INTSTA,MYCALL                                                    
         LA    R9,STALIST          1ST TIME FOR STATION?                        
CHKSTA   CLI   0(R9),0             END OF LIST                                  
         BNE   CHKSTA2             NO TRY NEXT                                  
         MVI   BYPREAD,1           YES - INSERT AND SEND 'M' RECORD             
         MVC   0(5,R9),INTSTA                                                   
         C     R9,=A(STALISTX)                                                  
         BL    *+6                                                              
         DC    H'0'                INCREASE MAX_STATIONS                        
         MVI   STASW,1                                                          
         B     OUT300              SETKEY AND EXIT                              
*                                                                               
CHKSTA2  CLC   0(5,R9),INTSTA      STATION IN LIST                              
         BE    *+12                                                             
         LA    R9,5(R9)                                                         
         B     CHKSTA                                                           
*                                                                               
         MVI   STASW,0             RESET CREAT 'M' RECORD SWITCHES              
         MVI   BYPREAD,0                                                        
         BAS   RE,SLOTDEMS                                                      
*                                                                               
OUT300   BAS   RE,SETKEY           BUILD KEY AND RELEASE RECD                   
         B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*SLOTDEMS - PICK UP DEMOS FROM TAPE AND SLOT INTO APPROPRIATE BUCKETS           
**********************************************************************          
*                                                                               
SLOTDEMS NTR1                                                                   
*                                                                               
         LA    R9,RECTYP2                                                       
         LA    R6,24(RC)           DEMOS R 24 BEYOND FOR QTR AND UNIV           
         CLI   MYRECTY,C'2'        SLOT DEMOS INTO OUTPUT BUCKETS               
         BE    SLOT10               DEPENDING ON RECD TYPE                      
         LA    R9,RECTYP3                                                       
         CLI   MYRECTY,C'4'                                                     
         BE    SLOT10                                                           
         DC     H'0'                                                            
*                                                                               
SLOT10   DS    0H                                                               
*                                                                               
         LA    R5,OUTDATA           IN OUTDEMOS                                 
         USING OUTRECD,R5                                                       
         LA    R7,OUTDEMOS                                                      
*                                                                               
SLOT20   CLI   0(R9),X'FF'         END OF RECTYP TABLE                          
         BE    SLOT60                                                           
         MVI   DUB,0               LEADING BLANK FLAG                           
         XC    WORK,WORK                                                        
         XC    0(4,R7),0(R7)       INITIALIZE OUTPUT DEMO BUCKET TO 0           
         ICM   R1,15,0(R9)         L'INPUT BUCKET                               
         LA    RE,WORK             RE=TEMP OUTPUT WORK AREA                     
         LR    RF,R6               RF=CURRENT INPUT DEMO BUCKET (TAPE)          
*                                                                               
SLOT30   CLI   0(RF),C' '          BLANKS IN FIELD?                             
         BNE   SLOT35                                                           
         CLI   DUB,0               LEAD BLANKS OKAY                             
         BNE   SLOT50              IF BLK IN MIDDLE OF FIELD, INVALID           
         B     SLOT40              IGNORE LEADING BLANKS                        
SLOT35   TM    0(RF),X'F0'         NUMERIC FIELD?                               
         BNO   SLOT50              INVALID DATA IN FIELD, BYPASS IT             
         MVC   0(1,RE),0(RF)       MOVE NUMBER INTO WORK                        
         MVI   DUB,1               MARK START OF VALUE                          
         LA    RE,1(RE)            NEXT DIGIT IN WORK                           
SLOT40   LA    RF,1(RF)            NEXT DIGIT TO VALIDATE ON INPUT              
         BCT   R1,SLOT30                                                        
*                                                                               
         CLI   DUB,0               ENTIRE FIELD IS BLANKED OUT?                 
         BE    SLOT50               YES                                         
         LA    RF,WORK                                                          
         SR    RE,RF               CALCULATE ACTUAL L'DATA IN WORK              
         BCTR  RE,0                -1 FOR EX PACK                               
         XC    DUB,DUB                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),WORK(0)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R7)                                                      
*                                                                               
SLOT50   DS    0H                                                               
         LA    R7,4(R7)            BUMP OUTPUT DEMO AREA BUCKET                 
         ICM   RE,15,0(R9)         RE=L'FIELD IN INPUT                          
         AR    R6,RE               BUMP TO NEXT DEMO BUCKET ON IN TAPE          
         ZIC   RE,4(R9)            PICK UP ANY ADDL DISPLACEMENT                
         AR    R6,RE               R6 -> NEXT DEMO BUCKET ON TAPE               
         LA    R9,5(R9)            NEXT ENTRY IN RECTYP TABLE                   
         B     SLOT20                                                           
*                                                                               
SLOT60   LA    RE,OUTDEMOS                                                      
         LR    RF,R7                                                            
         SR    RF,RE               LENGTH OF DEMOS                              
         LA    RE,OUTHDLEN                                                      
         AR    RE,RF               LENGTH OF RECORD                             
         LA    RE,4(RE)                                                         
         STCM  RE,3,OUTHEAD                                                     
*                                                                               
*                                  NOW SEED INTO INTACCS                        
         LA    RE,INTACCS          DESTINATION OF OUTPUT DEMOS                  
*                                                                               
         LA    R7,OUTDEMOS         SOURCE IN OUTREC                             
*                                                                               
         LAY   R9,OUTDEMS2         DEMO DISP TABLE REC TYPE 2                   
         CLI   MYRECTY,C'2'                                                     
         BE    SLOT80                                                           
         LAY   R9,OUTDEMS3                                                      
         CLI   MYRECTY,C'3'                                                     
         BE    SLOT80                                                           
*                                                                               
SLOT80   CLI   0(R9),X'FF'         END OF TABLE?--DONE SLOTTING DEMOS           
         BE    SLOT90              GO DO PUTS                                   
         ZIC   RF,0(R9)            SOURCE BUCKET (INPUT)                        
         MHI   RF,4                RF=SOURCE 4BYTE BUCKETS                      
         AR    RF,R7               SOURCE                                       
         ZIC   R1,1(R9)            DESTINATION BUCKET POSITION (OUTPUT)         
         MHI   R1,4                R1=DESTIN 4BYTE BUCKETS                      
         AR    R1,RE               DESTIN IN INTACCS                            
         MVC   0(4,R1),0(RF)       MOVE DEMO TO APPROPRIATE OUTPUT SLOT         
         LA    R9,2(R9)            DO NEXT DEMO CATEGORY                        
         B     SLOT80                                                           
*                                                                               
SLOT90   DS    0H                                                               
         CLI   MYRECTY,C'2'        UNIV GET SAVED AWAY FOR LATER                
         BNE   SLOT95                                                           
         MVC   SAVUNIV,INTACCS+UNVQ  SAVE UNIVS                                 
         B     SLOTX                                                            
SLOT95   MVC   INTACCS+UNVQ(UNVQLN),SAVUNIV  PUT UNIVS IN EA RECD               
*                                                                               
SLOTX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
*SETKEY - BUILD R-KEY FOR RECORD                                                
*********************************************************************           
SETKEY   NTR1                                                                   
         L     R6,AIREC                                                         
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'R'                                                     
         MVI   DRCODE,C'R'                                                      
         MVC   INTBOOK,FILTBOOK+1                                               
         MVC   INTBOOK,SVBOOK                                                   
         MVC   DRBOOK,INTBOOK                                                   
*                                                                               
         MVI   DRBTYP,C'W'                                                      
*                                                                               
         MVI   DRMEDIA,C'C'                                                     
         MVI   DRSRC,C'A'                                                       
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         MVC   DRHIQHR+6(L'INTWEEKS),INTWEEKS                                   
         CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEY3                                                          
*        MVC   DRHIQHR+2(4),SITOTVWR                                            
         B     SETKEYX                                                          
         SPACE 2                                                                
SETKEY3  MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYX                                                          
SETKEYX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
SETKEYP  NTR1                                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'R'                                                     
         MVI   DRCODE,C'R'                                                      
         MVC   INTBOOK,FILTBOOK+1                                               
         MVC   INTBOOK,SVBOOK                                                   
         MVC   DRBOOK,INTBOOK                                                   
*                                                                               
         MVI   DRBTYP,C'P'                                                      
*                                                                               
         MVI   DRMEDIA,C'C'                                                     
         MVI   DRSRC,C'A'                                                       
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
         CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEYP3                                                         
         CLI   STASW,2             CREAT 'U' RECORD SWITCH                      
         BE    SETKEYP4                                                         
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         MVC   DRHIQHR+6(L'INTWEEKS),INTWEEKS                                   
*        MVC   DRHIQHR+2(4),SITOTVWR                                            
         B     SETKEYPX                                                         
         SPACE 2                                                                
SETKEYP3 MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYPX                                                         
*  PROGRAM NAME USES UNIVERSE TYPE TO INIT SOME STUFF                           
SETKEYP4 MVI   DRCODE,C'A'                                                      
         MVI   INTRTYP,C'A'                                                     
         B     SETKEYPX                                                         
*                                                                               
SETKEYPX XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         XIT1                                                                   
**********************************************************************          
BLDHOM   NTR1                                                                   
         LA    RE,METMKT                                                        
BH01     CLI   0(RE),X'FF'                                                      
         BE    OPENOK                                                           
         CLC   1(3,RC),0(RE)                                                    
         BE    BH05                                                             
         LA    RE,L'METMKT(RE)                                                  
         B     BH01                                                             
BH05     MVC   MKT,3(RE)                                                        
*                                                                               
         XC    HOMETAB,HOMETAB     SET UP HOME STATIONS FOR MKT                 
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,HOMESTA                                                
         ICM   R5,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)                                                         
         USING HOMESTAD,R5                                                      
         LA    R4,HOMETAB                                                       
*                                                                               
BH10     CLC   HOMEMKT,=X'FFFF'    BLD HOME STATIONS LIST                       
         BE    BHX                                                              
         CLC   HOMEMKT,MKT         FIND STATIONS FOR THIS MKT IN TABLE          
         BE    BH15                                                             
         AR    R5,R0                                                            
         B     BH10                                                             
*                                                                               
BH15     OC    HOMEBK,HOMEBK       CHECK EFFECTIVE DATE                         
         BZ    BH20                                                             
         MVC   DUB(2),HOMEBK                                                    
         MVI   DUB+2,15                                                         
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 VGETBRD,DMCB,(X'01',WORK),WORK+6,VGETDAY,VADDAY                  
         GOTO1 VNETWEEK,DMCB,WORK+6,VGETDAY,VADDAY                              
         MVC   HALF(1),DMCB+4                                                   
         MVC   HALF+1(1),DMCB+8                                                 
         CLC   SVBOOK,HALF         BYPASS IF PRIOR TO EFFECTIVE DATE            
         BL    BH30                                                             
*                                                                               
BH20     MVC   0(5,R4),HOMECALL                                                 
         LA    R4,5(R4)                                                         
         C     R4,=A(HOMETABX)                                                  
         BL    *+6                                                              
         DC    H'0'                INCREASE MAX_HOME_STATIONS                   
*                                                                               
BH30     AR    R5,R0                                                            
         B     BH10                                                             
BHX      XIT1                                                                   
         DROP  R5                                                               
**********************************************************************          
*CNVWR - SORT RECORD HANDLING BEFORE OUTPUT PHASE                               
**********************************************************************          
CNVWR    DS    0H                                                               
         L     R2,ASREC            POINT TO SORT RECORD                         
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         CLI   0(R6),C'R'          BYPASS IF NOT RATINGS RECORD                 
         BNE   EXIT                                                             
*                                                                               
         CLC   INTSQH,PREVSQH      FORCE DIFFERENT PROG NAMES                   
         BNE   PNAMOK              IF NUMBER OF WEEKS DIFFERS                   
         CLC   INTPNAM,PREVPNAM                                                 
         BNE   PNAMOK                                                           
         MVC   INTPNAM+13(1),DRHIQHR+1                                          
         OI    INTPNAM+13,X'F0'                                                 
*                                                                               
PNAMOK   MVC   PREVSQH,INTSQH                                                   
         MVC   PREVPNAM,INTPNAM                                                 
         CLI   DRHIQHR+1,2                                                      
         BL    *+8                                                              
         OI    INTWEEKS,B'00100000' SET TYPICAL                                 
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
MORET    CLOSE (IN,REWIND)                                                      
         MVC   NRECS,=F'0'                                                      
                                                                                
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
*                                                                               
MOREP    MVI   DONEPROG,C'Y'                                                    
         MVI   BYPREAD,0                                                        
         CLOSE (INPROG,REWIND)                                                  
         OPEN  (IN,(INPUT))                                                     
*                                                                               
         B     READ2                                                            
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PARENTE  EQU   2                                                                
OUTMARE  EQU   32                                                               
         EJECT                                                                  
BYPREAD  DC    X'00'                                                            
STASW    DC    X'00'                                                            
MKT      DS    XL2                                                              
PREVSTAT DS    CL5                                                              
PREVSQH  DS    X                                                                
PREVPNAM DS    CL14                                                             
MYCALL   DS    CL5                                                              
SVBOOK   DS    XL2                                                              
SVWEEK   DS    X                                                                
MYRECTY  DS    C                                                                
DONEPROG DS    CL1                                                              
TEMPDATE DS    CL6                                                              
*                                                                               
         DS    0F                                                               
MAX_HOME_STATIONS EQU 40                                                        
HOMETAB  DS    CL(MAX_HOME_STATIONS*5)                                          
HOMETABX EQU   *                                                                
*                                                                               
SAVUNIV  DS    CL((UNIVEND+1)*4)                                                
*                                                                               
         EJECT                                                                  
*********************************************************************           
*RECORD TYPE 2 - UNIVERSE DEMO DISPLACEMENTS                                    
*********************************************************************           
* INPUT TYPE 2                                                                  
IUVM2O   EQU   0                                                                
IUVM12O  EQU   1                                                                
IUVM18O  EQU   2                                                                
IUVM25O  EQU   3                                                                
IUVM35O  EQU   4                                                                
IUVM50O  EQU   5                                                                
IUVM55O  EQU   6                                                                
IUVM60O  EQU   7                                                                
IUVM65O  EQU   8                                                                
IUVW2O   EQU   9                                                                
IUVW12O  EQU   10                                                               
IUVW18O  EQU   11                                                               
IUVW25O  EQU   12                                                               
IUVW35O  EQU   13                                                               
IUVW50O  EQU   14                                                               
IUVW55O  EQU   15                                                               
IUVW60O  EQU   16                                                               
IUVW65O  EQU   17                                                               
*                                                                               
* INPUT TYPE 3                                                                  
IQVM2O   EQU   0                                                                
IQVM12O  EQU   1                                                                
IQVM18O  EQU   2                                                                
IQVM25O  EQU   3                                                                
IQVM35O  EQU   4                                                                
IQVM50O  EQU   5                                                                
IQVM55O  EQU   6                                                                
IQVM60O  EQU   7                                                                
IQVM65O  EQU   8                                                                
IQVW2O   EQU   9                                                                
IQVW12O  EQU   10                                                               
IQVW18O  EQU   11                                                               
IQVW25O  EQU   12                                                               
IQVW35O  EQU   13                                                               
IQVW50O  EQU   14                                                               
IQVW55O  EQU   15                                                               
IQVW60O  EQU   16                                                               
IQVW65O  EQU   17                                                               
*                                                                               
*********************************************************************           
*OUTPUT RECORD DISPLACEMENTS                                                    
*********************************************************************           
*                                                                               
OUVM2O   EQU   0                                                                
OUVM12O  EQU   1                                                                
OUVM18O  EQU   2                                                                
OUVM25O  EQU   3                                                                
OUVM35O  EQU   4                                                                
OUVM50O  EQU   5                                                                
OUVM55O  EQU   6                                                                
OUVM60O  EQU   7                                                                
OUVM65O  EQU   8                                                                
OUVW2O   EQU   9                                                                
OUVW12O  EQU   10                                                               
OUVW18O  EQU   11                                                               
OUVW25O  EQU   12                                                               
OUVW35O  EQU   13                                                               
OUVW50O  EQU   14                                                               
OUVW55O  EQU   15                                                               
OUVW60O  EQU   16                                                               
OUVW65O  EQU   17                                                               
UNIVEND  EQU   17                                                               
*                                                                               
OQVM2O   EQU   18                                                               
OQVM12O  EQU   19                                                               
OQVM18O  EQU   20                                                               
OQVM25O  EQU   21                                                               
OQVM35O  EQU   22                                                               
OQVM50O  EQU   23                                                               
OQVM55O  EQU   24                                                               
OQVM60O  EQU   25                                                               
OQVM65O  EQU   26                                                               
OQVW2O   EQU   27                                                               
OQVW12O  EQU   28                                                               
OQVW18O  EQU   29                                                               
OQVW25O  EQU   30                                                               
OQVW35O  EQU   31                                                               
OQVW50O  EQU   32                                                               
OQVW55O  EQU   33                                                               
OQVW60O  EQU   34                                                               
OQVW65O  EQU   35                                                               
*                                                                               
UNVQ     EQU   OUVM2O*4              DISP TO UNIVS IN INTACCS                   
UNVQLN   EQU   (UNIVEND-OUVM2O+1)*4   L'UNIVERSES                               
*                                                                               
********************************************************************            
*                                                                               
OUTDEMS2 DS   0XL2                                                              
         DC   AL1(IUVM2O,OUVM2O)                                                
         DC   AL1(IUVM12O,OUVM12O)                                              
         DC   AL1(IUVM18O,OUVM18O)                                              
         DC   AL1(IUVM25O,OUVM25O)                                              
         DC   AL1(IUVM35O,OUVM35O)                                              
         DC   AL1(IUVM50O,OUVM50O)                                              
         DC   AL1(IUVM55O,OUVM55O)                                              
         DC   AL1(IUVM60O,OUVM60O)                                              
         DC   AL1(IUVM65O,OUVM65O)                                              
         DC   AL1(IUVW2O,OUVW2O)                                                
         DC   AL1(IUVW12O,OUVW12O)                                              
         DC   AL1(IUVW18O,OUVW18O)                                              
         DC   AL1(IUVW25O,OUVW25O)                                              
         DC   AL1(IUVW35O,OUVW35O)                                              
         DC   AL1(IUVW50O,OUVW50O)                                              
         DC   AL1(IUVW55O,OUVW55O)                                              
         DC   AL1(IUVW60O,OUVW60O)                                              
         DC   AL1(IUVW65O,OUVW65O)                                              
         DC   X'FF',X'FFFF'                                                     
*                                                                               
OUTDEMS3 DS   0XL2                                                              
         DC   AL1(IQVM2O,OQVM2O)                                                
         DC   AL1(IQVM12O,OQVM12O)                                              
         DC   AL1(IQVM18O,OQVM18O)                                              
         DC   AL1(IQVM25O,OQVM25O)                                              
         DC   AL1(IQVM35O,OQVM35O)                                              
         DC   AL1(IQVM50O,OQVM50O)                                              
         DC   AL1(IQVM55O,OQVM55O)                                              
         DC   AL1(IQVM60O,OQVM60O)                                              
         DC   AL1(IQVM65O,OQVM65O)                                              
         DC   AL1(IQVW2O,OQVW2O)                                                
         DC   AL1(IQVW12O,OQVW12O)                                              
         DC   AL1(IQVW18O,OQVW18O)                                              
         DC   AL1(IQVW25O,OQVW25O)                                              
         DC   AL1(IQVW35O,OQVW35O)                                              
         DC   AL1(IQVW50O,OQVW50O)                                              
         DC   AL1(IQVW55O,OQVW55O)                                              
         DC   AL1(IQVW60O,OQVW60O)                                              
         DC   AL1(IQVW65O,OQVW65O)                                              
         DC   X'FF',X'FFFF'                                                     
         EJECT                                                                  
SVCALL   DS    CL(L'INTSTA)                                                     
SVPNC    DS    CL3                                                              
INREC    DS    CL606                                                            
OUTREC   DS    0XL1004       MAX LENGTH 1004 WITH LENGTH                        
OUTHEAD  DS    XL4                                                              
OUTDATA  DS    XL1000                                                           
*                                                                               
*********************************************************************           
*          DATA SET DETMPRD    AT LEVEL 001 AS OF 12/17/98                      
*********************************************************************           
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
IN       DCB   DDNAME=FILEIN1,                                         X        
               DSORG=PS,                                               X        
               EODAD=MORET,                                            X        
               RECFM=FB,                                               X        
               LRECL=00200,                                            X        
               MACRF=GM                                                         
*                                                                               
INPROG   DCB   DDNAME=FILEIN2,                                         X        
               DSORG=PS,                                               X        
               EODAD=MOREP,                                            X        
               RECFM=FB,                                               X        
               LRECL=0459,                                             X        
               MACRF=GM                                                         
         EJECT                                                                  
*                                                                               
DAYTAB   DS    0CL2                                                             
         DC    CL1'1',X'10'                                                     
         DC    CL1'2',X'20'                                                     
         DC    CL1'3',X'30'                                                     
         DC    CL1'4',X'40'                                                     
         DC    CL1'5',X'50'                                                     
         DC    CL1'6',X'60'                                                     
         DC    CL1'7',X'70'                                                     
         DC    AL1(0)                                                           
*********************************************************************           
* R = CENTRAL MARKET (CM)                                                       
* I = FULL COVERAGE  (FC)                                                       
* E = EXTENDED MARKET(EM)                                                       
* B = CENTRAL SHARE  (CS)                                                       
*********************************************************************           
RECTYP2  DS   0CL5                                                              
         DC   AL4(8),AL1(0)       M2+                                           
         DC   AL4(8),AL1(0)       M12+                                          
         DC   AL4(8),AL1(0)       M18+                                          
         DC   AL4(8),AL1(0)       M25+                                          
         DC   AL4(8),AL1(0)       M35+                                          
         DC   AL4(8),AL1(0)       M50+                                          
         DC   AL4(8),AL1(0)       M55+                                          
         DC   AL4(8),AL1(0)       M60+    ***                                   
         DC   AL4(8),AL1(0)       M65+                                          
         DC   AL4(8),AL1(0)       W2+                                           
         DC   AL4(8),AL1(0)       W12+                                          
         DC   AL4(8),AL1(0)       W18+                                          
         DC   AL4(8),AL1(0)       W25+                                          
         DC   AL4(8),AL1(0)       W35+                                          
         DC   AL4(8),AL1(0)       W50+                                          
         DC   AL4(8),AL1(0)       W55+                                          
         DC   AL4(8),AL1(0)       W60+    ***                                   
         DC   AL4(8),AL1(0)       W65+                                          
         DC   X'FFFF'                                                           
*                                                                               
RECTYP3  DS   0CL5                                                              
         DC   AL4(8),AL1(0)       M2+                                           
         DC   AL4(8),AL1(0)       M12+                                          
         DC   AL4(8),AL1(0)       M18+                                          
         DC   AL4(8),AL1(0)       M25+                                          
         DC   AL4(8),AL1(0)       M35+                                          
         DC   AL4(8),AL1(0)       M50+                                          
         DC   AL4(8),AL1(0)       M55+                                          
         DC   AL4(8),AL1(0)       M60+    ***                                   
         DC   AL4(8),AL1(0)       M65+                                          
         DC   AL4(8),AL1(0)       W2+                                           
         DC   AL4(8),AL1(0)       W12+                                          
         DC   AL4(8),AL1(0)       W18+                                          
         DC   AL4(8),AL1(0)       W25+                                          
         DC   AL4(8),AL1(0)       W35+                                          
         DC   AL4(8),AL1(0)       W50+                                          
         DC   AL4(8),AL1(0)       W55+                                          
         DC   AL4(8),AL1(0)       W60+    ***                                   
         DC   AL4(8),AL1(0)       W65+                                          
         DC   X'FFFF'                                                           
* ***NEW-PPM***                                                                 
METMKT   DS    0CL5                                                             
* CL3 BBM MARKET NUMBER                                                         
* AL2 INTERNAL MARKET NUMBER (SEE DEDEMTABOF)                                   
*   !!!! ALSO SEE TABLE BBMMTRL IN DEGETTP                                      
         DC    CL3'001',AL2(9109)  VANCOUVER/VICTORIA (EM)                      
         DC    CL3'100',AL2(0003)  ONTARIO                                      
         DC    CL3'103',AL2(5199)  TORONTO/HAMILTON (EM)                        
         DC    CL3'200',AL2(0004)  QUEBEC FRANCO                                
         DC    CL3'250',AL2(4481)  MONTREAL FRANCO (EM)                         
         DC    CL3'275',AL2(4480)  MONTREAL ANGLO (EM)                          
         DC    CL3'300',AL2(8069)  CALGARY (EM)                                 
         DC    CL3'350',AL2(8119)  EDMONTON (EM)                                
         DC    CL3'999',AL2(0002)  TOTAL CANADA (AKA "NATIONAL")                
* 998 - ENGLISH CANADA POPULATION MARKET                                        
* WE WILL BE LOADING THESE UNDER THE SAME NATIONAL MARKET NUMBER 2              
* WE SHOULD IN THEORY NOT RUN FILES WITH 999 WHEN WE HAVE 998 FILES             
* NUMERIS WILL PROVIDE A NEW SET OF FILES FOR 998 INSTEAD OF 999                
         DC    CL3'998',AL2(0002)  ENGLISH CANADA                               
         DC    X'FFFF'                                                          
*                                                                               
STATTAB  DS   0CL8                                                              
* CL3 BBM STATION CODE                                                          
* CL5 OUR STATION CALL LETTERS (PROVIDED BY GWEN)                               
         DC   CL3'001',CL5'CBUT '                                               
         DC   CL3'003',CL5'CHEK '                                               
         DC   CL3'004',CL5'KCTS '                                               
         DC   CL3'005',CL5'KOMO '                                               
         DC   CL3'006',CL5'CHAN '                                               
         DC   CL3'007',CL5'KVOS '                                               
         DC   CL3'008',CL5'CKVU '                                               
         DC   CL3'009',CL5'KSTW '                                               
         DC   CL3'010',CL5'KIRO '                                               
         DC   CL3'011',CL5'KING '                                               
         DC   CL3'012',CL5'CBUFT'                                               
         DC   CL3'013',CL5'KCPQ '                                               
         DC   CL3'014',CL5'NOW  '                                               
         DC   CL3'015',CL5'CIVI '                                               
         DC   CL3'017',CL5'CHNM '                                               
         DC   CL3'019',CL5'CIVT '                                               
         DC   CL3'026',CL5'CBRT '                                               
         DC   CL3'029',CL5'CFCN '                                               
         DC   CL3'030',CL5'CHCA '                                               
         DC   CL3'045',CL5'DYSC '                                               
         DC   CL3'046',CL5'DSCH '                                               
         DC   CL3'053',CL5'KAYU '                                               
         DC   CL3'056',CL5'KHQ  '                                               
         DC   CL3'057',CL5'KREM '                                               
         DC   CL3'058',CL5'KSPS '                                               
         DC   CL3'059',CL5'KXLY '                                               
         DC   CL3'064',CL5'CKAL '                                               
         DC   CL3'148',CL5'CKXT '                                               
         DC   CL3'149',CL5'OMNT '                                               
*********DC   CL3'149',CL5'CJMT '                                               
         DC   CL3'150',CL5'CBLT '                                               
         DC   CL3'151',CL5'CBLF '                                               
         DC   CL3'152',CL5'CFMT '                                               
         DC   CL3'153',CL5'CFTO '                                               
         DC   CL3'154',CL5'CHCH '                                               
         DC   CL3'155',CL5'CIII '                                               
         DC   CL3'156',CL5'CITY '                                               
         DC   CL3'157',CL5'CKVR '                                               
         DC   CL3'158',CL5'CHEX '                                               
         DC   CL3'159',CL5'CKCO '                                               
         DC   CL3'160',CL5'CBEF '                                               
         DC   CL3'161',CL5'CBET '                                               
         DC   CL3'162',CL5'CBFT '                                               
         DC   CL3'163',CL5'CBLN '                                               
         DC   CL3'164',CL5'CBMT '                                               
         DC   CL3'165',CL5'CBOF '                                               
         DC   CL3'166',CL5'CBOT '                                               
         DC   CL3'167',CL5'CBWF '                                               
         DC   CL3'168',CL5'CBWT '                                               
         DC   CL3'169',CL5'CFCF '                                               
         DC   CL3'170',CL5'CFCL '                                               
         DC   CL3'171',CL5'CFGS '                                               
         DC   CL3'172',CL5'CFJP '                                               
         DC   CL3'173',CL5'CFPL '                                               
         DC   CL3'174',CL5'CFTM '                                               
         DC   CL3'175',CL5'CHBX '                                               
         DC   CL3'176',CL5'CHFD '                                               
         DC   CL3'177',CL5'CHNB '                                               
         DC   CL3'178',CL5'CHOT '                                               
         DC   CL3'180',CL5'CHRO '                                               
         DC   CL3'181',CL5'CHWI '                                               
         DC   CL3'182',CL5'CICI '                                               
         DC   CL3'183',CL5'CITO '                                               
         DC   CL3'184',CL5'CJBN '                                               
         DC   CL3'185',CL5'CJIC '                                               
         DC   CL3'186',CL5'CJOH '                                               
         DC   CL3'190',CL5'CKMI '                                               
         DC   CL3'191',CL5'CKNC '                                               
         DC   CL3'192',CL5'CKNX '                                               
         DC   CL3'193',CL5'CKNY '                                               
         DC   CL3'194',CL5'CKPR '                                               
         DC   CL3'195',CL5'CKWS '                                               
         DC   CL3'196',CL5'KARE '                                               
         DC   CL3'197',CL5'TFO  '                                               
         DC   CL3'198',CL5'TVO  '                                               
         DC   CL3'199',CL5'WCAX '                                               
         DC   CL3'200',CL5'WCCO '                                               
         DC   CL3'201',CL5'WDIV '                                               
         DC   CL3'202',CL5'WDWB '                                               
         DC   CL3'203',CL5'WEYI '                                               
         DC   CL3'204',CL5'WGKI '                                               
         DC   CL3'205',CL5'WGRZ '                                               
         DC   CL3'206',CL5'WGTQ '                                               
         DC   CL3'207',CL5'WHEC '                                               
         DC   CL3'208',CL5'WICU '                                               
         DC   CL3'209',CL5'WIVB '                                               
*****    DC   CL3'210',CL5'WIXT '   AS PER PATTY THIS 210 SHOULD BE CBC         
         DC   CL3'210',CL5'CBC  '   AS OF 2017                                  
         DC   CL3'211',CL5'WJBK '                                               
         DC   CL3'212',CL5'WJET '                                               
         DC   CL3'213',CL5'WJRT '                                               
         DC   CL3'214',CL5'WKBD '                                               
         DC   CL3'215',CL5'WKBW '                                               
         DC   CL3'216',CL5'WNED '                                               
         DC   CL3'217',CL5'WNEQ '                                               
         DC   CL3'218',CL5'WNPI '                                               
         DC   CL3'219',CL5'WNWO '                                               
         DC   CL3'220',CL5'WNYO '                                               
         DC   CL3'221',CL5'WPBS '                                               
         DC   CL3'222',CL5'WPTZ '                                               
         DC   CL3'223',CL5'WQLN '                                               
         DC   CL3'224',CL5'WRKO '                                               
         DC   CL3'225',CL5'WROC '                                               
         DC   CL3'226',CL5'WSEE '                                               
         DC   CL3'227',CL5'WSTM '                                               
         DC   CL3'228',CL5'WSYT '                                               
         DC   CL3'229',CL5'WTOL '                                               
         DC   CL3'230',CL5'WTVG '                                               
         DC   CL3'231',CL5'WTVH '                                               
         DC   CL3'232',CL5'WTVS '                                               
         DC   CL3'233',CL5'WUAB '                                               
         DC   CL3'234',CL5'WUHF '                                               
         DC   CL3'235',CL5'WUTV '                                               
         DC   CL3'236',CL5'WVNY '                                               
         DC   CL3'237',CL5'WWJ  '                                               
         DC   CL3'238',CL5'WWNY '                                               
         DC   CL3'239',CL5'WWTI '                                               
         DC   CL3'240',CL5'WWUP '                                               
         DC   CL3'241',CL5'WXXI '                                               
         DC   CL3'242',CL5'WXYZ '                                               
         DC   CL3'251',CL5'H2+  '                                               
         DC   CL3'252',CL5'CINV '                                               
         DC   CL3'263',CL5'DEJA '                                               
         DC   CL3'284',CL5'MTME '                                               
         DC   CL3'296',CL5'BBCC '                                               
         DC   CL3'298',CL5'DIY  '                                               
         DC   CL3'301',CL5'NATG '                                               
         DC   CL3'302',CL5'ACTN '                                               
         DC   CL3'303',CL5'LIFE '                                               
*********DC   CL3'252',CL5'MYST '                                               
*********DC   CL3'263',CL5'DEJA '                                               
         DC   CL3'304',CL5'ELLE '                                               
         DC   CL3'325',CL5'NGEW '                                               
         DC   CL3'340',CL5'CFEM '                                               
         DC   CL3'345',CL5'CFVS '                                               
         DC   CL3'352',CL5'CJNT '                                               
         DC   CL3'356',CL5'CKRN '                                               
         DC   CL3'362',CL5'WCFE '                                               
         DC   CL3'364',CL5'WETK '                                               
         DC   CL3'365',CL5'WFFF '                                               
         DC   CL3'371',CL5'TALK '                                               
         DC   CL3'373',CL5'METM '                                               
         DC   CL3'374',CL5'MUSX '     WAS MUSI                                  
***      DC   CL3'374',CL5'MUSI '                                               
         DC   CL3'375',CL5'SUPE '                                               
         DC   CL3'377',CL5'EVAS '                                               
         DC   CL3'378',CL5'SERI '                                               
         DC   CL3'379',CL5'HISF '                                               
*********DC   CL3'381',CL5'CIVM '                                               
         DC   CL3'381',CL5'TLQ  '                                               
         DC   CL3'382',CL5'WCFE '                                               
         DC   CL3'385',CL5'SAVR '                                               
         DC   CL3'386',CL5'LCN  '                                               
         DC   CL3'388',CL5'CANZ '                                               
         DC   CL3'519',CL5'TSN  '                                               
         DC   CL3'520',CL5'NWS  '                                               
         DC   CL3'521',CL5'CNBC '                                               
         DC   CL3'522',CL5'WTN  '                                               
         DC   CL3'523',CL5'WNW  '                                               
         DC   CL3'524',CL5'VTV  '                                               
         DC   CL3'525',CL5'YTV  '                                               
         DC   CL3'526',CL5'CBNW '                                               
         DC   CL3'527',CL5'MUCH '                                               
         DC   CL3'528',CL5'TSN  '                                               
         DC   CL3'529',CL5'AEN  '                                               
         DC   CL3'530',CL5'TNN  '                                               
         DC   CL3'531',CL5'CNN  '                                               
         DC   CL3'532',CL5'TLC  '                                               
***      DC   CL3'533',CL5'LCD  '                                               
         DC   CL3'533',CL5'CAND '  WAS LCD                                      
         DC   CL3'534',CL5'TV5  '  WAS  TVC                                     
         DC   CL3'535',CL5'HLN  '                                               
         DC   CL3'536',CL5'CMT  '                                               
         DC   CL3'537',CL5'SHOW '                                               
         DC   CL3'538',CL5'BRAV '                                               
*****    DC   CL3'539',CL5'LIFE ' DEFINED ALREADY IN 303                        
         DC   CL3'539',CL5'SLCE ' SLICE AS PER PATTY                            
         DC   CL3'540',CL5'DSCY '                                               
         DC   CL3'541',CL5'VRAK '                                               
*****    DC   CL3'542',CL5'SPN  '                                               
         DC   CL3'542',CL5'SPNO '                                               
         DC   CL3'543',CL5'VIE  '                                               
         DC   CL3'544',CL5'CPNW '                                               
         DC   CL3'545',CL5'RDI  '                                               
         DC   CL3'546',CL5'WTBS '                                               
         DC   CL3'547',CL5'CTS  '                                               
         DC   CL3'548',CL5'MOVP '                                               
***      DC   CL3'549',CL5'PLUS '                                               
***      DC   CL3'549',CL5'MUSP '  WAS PLUS - RETIRED AUG25/19                  
         DC   CL3'550',CL5'OLEG '                                               
         DC   CL3'551',CL5'RDS  '                                               
         DC   CL3'552',CL5'DYVL '                                               
         DC   CL3'553',CL5'TSTV '                                               
         DC   CL3'556',CL5'FAM  '                                               
         DC   CL3'557',CL5'TMN  '                                               
         DC   CL3'558',CL5'FAIR '                                               
         DC   CL3'560',CL5'CTSW '                                               
         DC   CL3'562',CL5'FAMW '                                               
         DC   CL3'563',CL5'GAME '                                               
         DC   CL3'564',CL5'TONW '                                               
         DC   CL3'570',CL5'CFTM '                                               
         DC   CL3'571',CL5'HIST '                                               
         DC   CL3'572',CL5'SPAC '                                               
         DC   CL3'573',CL5'HGTV '                                               
         DC   CL3'574',CL5'PRME '                                               
         DC   CL3'575',CL5'OLN  '                                               
         DC   CL3'576',CL5'TOON '                                               
         DC   CL3'578',CL5'GOLF '                                               
         DC   CL3'579',CL5'SCOR '                                               
         DC   CL3'580',CL5'SPED '                                               
         DC   CL3'581',CL5'COME '                                               
         DC   CL3'582',CL5'BET  '                                               
         DC   CL3'583',CL5'MMM  '                                               
         DC   CL3'586',CL5'KTLA '                                               
         DC   CL3'587',CL5'WGN  '                                               
         DC   CL3'588',CL5'WPIX '                                               
         DC   CL3'589',CL5'CPAC '                                               
         DC   CL3'590',CL5'WSBK '                                               
         DC   CL3'591',CL5'APTV '                                               
****     DC   CL3'592',CL5'ROB  '  DELETE ENDED AS OF AUG2015                   
         DC   CL3'593',CL5'CLC  '                                               
         DC   CL3'594',CL5'STAR '                                               
         DC   CL3'595',CL5'ATN  '                                               
         DC   CL3'596',CL5'TREE '                                               
         DC   CL3'597',CL5'FOOD '                                               
         DC   CL3'598',CL5'TLN  '                                               
         DC   CL3'599',CL5'ARTV '                                               
         DC   CL3'676',CL5'TELE '                                               
         DC   CL3'700',CL5'CSPX '                                               
         DC   CL3'701',CL5'USPX '                                               
         DC   CL3'702',CL5'CCOX '                                               
         DC   CL3'703',CL5'UCOX '                                               
         DC   CL3'705',CL5'KVAN '                                               
         DC   CL3'710',CL5'CTV  '                                               
         DC   CL3'711',CL5'CBC  '                                               
         DC   CL3'712',CL5'CIND '                                               
         DC   CL3'713',CL5'TVA  '                                               
         DC   CL3'714',CL5'TQS  '                                               
         DC   CL3'715',CL5'SRC  '                                               
         DC   CL3'716',CL5'ABC  '                                               
         DC   CL3'717',CL5'CBS  '                                               
         DC   CL3'718',CL5'FOX  '                                               
         DC   CL3'719',CL5'UIND '                                               
         DC   CL3'720',CL5'NBC  '                                               
         DC   CL3'721',CL5'PBS  '                                               
         DC   CL3'722',CL5'CHUM '                                               
         DC   CL3'723',CL5'GLBL '                                               
         DC   CL3'724',CL5'TGCA '                                               
*        DC   CL3'730',CL5'CSPX ' RETIRED AUG25/19                              
*        DC   CL3'731',CL5'CSPE ' RETIRED AUG25/19                              
*        DC   CL3'732',CL5'CSPF ' RETIRED AUG25/19                              
         DC   CL3'733',CL5'CSPO '                                               
         DC   CL3'734',CL5'USPX '                                               
         DC   CL3'735',CL5'CCOX '                                               
         DC   CL3'736',CL5'UCOX '                                               
         DC   CL3'737',CL5'CCOX '                                               
         DC   CL3'738',CL5'CCOF '                                               
         DC   CL3'739',CL5'CSNA '                                               
         DC   CL3'740',CL5'CSEN '                                               
         DC   CL3'741',CL5'CSFR '                                               
         DC   CL3'746',CL5'HMOS '                                               
         DC   CL3'747',CL5'HVAS '                                               
         DC   CL3'748',CL5'HTOS '                                               
         DC   CL3'749',CL5'HMF  '                                               
         DC   CL3'750',CL5'CTVO '                                               
         DC   CL3'751',CL5'CBCO '                                               
         DC   CL3'752',CL5'YTV  '                                               
         DC   CL3'753',CL5'COME '                                               
         DC   CL3'754',CL5'PRME '                                               
         DC   CL3'755',CL5'TOON '                                               
         DC   CL3'756',CL5'FAM  '                                               
         DC   CL3'757',CL5'SHOW '                                               
****     DC   CL3'758',CL5'SPNO '                                               
         DC   CL3'758',CL5'SPN  '                                               
         DC   CL3'759',CL5'WNET '                                               
         DC   CL3'762',CL5'PFTM '                                               
         DC   CL3'770',CL5'CICT '                                               
         DC   CL3'777',CL5'MULM '                                               
*        DC   CL3'797',CL5'CNDE ' RETIRED AUG25/19                              
*        DC   CL3'798',CL5'CNDF ' RETIRED AUG25/19                              
         DC   CL3'799',CL5'DIGI '                                               
         DC   CL3'800',CL5'TOTX '                                               
         DC   CL3'801',CL5'TOTE '                                               
         DC   CL3'802',CL5'TOTF '                                               
         DC   CL3'803',CL5'SPNE '                                               
         DC   CL3'900',CL5'OSTX '                                               
         DC   CL3'901',CL5'OTVX '                                               
         DC   CL3'912',CL5'UNMT '                                               
         DC   CL3'920',CL5'NXVA '                                               
         DC   CL3'921',CL5'KNOW '                                               
         DC   CL3'922',CL5'OVAN '                                               
         DC   CL3'923',CL5'WVAN '                                               
         DC   CL3'924',CL5'VVAN '                                               
         DC   CL3'925',CL5'YVAN '                                               
         DC   CL3'926',CL5'NVAN '                                               
         DC   CL3'927',CL5'MVAN '                                               
         DC   CL3'928',CL5'SVAN '                                               
         DC   CL3'929',CL5'AEN  '                                               
         DC   CL3'930',CL5'TNN  '                                               
         DC   CL3'931',CL5'CNN  '                                               
         DC   CL3'932',CL5'TLC  '                                               
         DC   CL3'935',CL5'HLN  '                                               
         DC   CL3'936',CL5'YXVA '                                               
         DC   CL3'937',CL5'HVAN '                                               
         DC   CL3'938',CL5'BVAN '                                               
         DC   CL3'939',CL5'LVAN '                                               
         DC   CL3'940',CL5'DVAN '                                               
         DC   CL3'942',CL5'JXVA '                                               
*********DC   CL3'942',CL5'SPNP '                                               
         DC   CL3'943',CL5'ACCE '                                               
         DC   CL3'944',CL5'CKCS '                                               
         DC   CL3'945',CL5'CJCO '                                               
         DC   CL3'946',CL5'TACH '                                               
         DC   CL3'947',CL5'TCIT '                                               
         DC   CL3'948',CL5'TECA '                                               
         DC   CL3'950',CL5'OTHR '                                               
         DC   CL3'951',CL5'ADDK '                                               
         DC   CL3'953',CL5'CASA '                                               
         DC   CL3'954',CL5'PRIS '                                               
         DC   CL3'956',CL5'FAMX '                                               
         DC   CL3'957',CL5'SUPX '                                               
         DC   CL3'959',CL5'NICK '                                               
         DC   CL3'962',CL5'TSN2 '                                               
         DC   CL3'965',CL5'CBXT '  CBC EDMONTON (CBXT)+                         
         DC   CL3'966',CL5'CKEM '  CITYTV EDMONTON (CKEM)+                      
         DC   CL3'967',CL5'CKES '  CTS EDMONTON (CKES)+                         
         DC   CL3'968',CL5'CFRN '  CTV EDMONTON (CFRN)+                         
         DC   CL3'969',CL5'CITV '  GLOBAL EDMONTON (CITV)+                      
*********DC   CL3'970',CL5'TVAN '                                               
         DC   CL3'970',CL5'CJEO '  OMNI EDMONTON (CJEO)+                        
         DC   CL3'971',CL5'HXVA '                                               
         DC   CL3'972',CL5'SXVA '                                               
****     DC   CL3'973',CL5'HGTV ' THINK THIS IS WRONG DEFINED IN 573            
         DC   CL3'973',CL5'CHRG '                                               
         DC   CL3'974',CL5'PXVA '                                               
         DC   CL3'975',CL5'UXVA '                                               
         DC   CL3'976',CL5'LXVA '                                               
         DC   CL3'978',CL5'TVAS '                                               
         DC   CL3'979',CL5'JVAN '                                               
         DC   CL3'980',CL5'SPDX '                                               
         DC   CL3'981',CL5'CXVA '                                               
         DC   CL3'983',CL5'QXVA '                                               
         DC   CL3'990',CL5'NVST '                                               
         DC   CL3'992',CL5'CART '                                               
         DC   CL3'996',CL5'TREE '                                               
         DC   CL3'997',CL5'FOOD '                                               
         DC   X'FFFF'                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
MAX_STATIONS EQU 1000                                                           
STALIST  DS    CL(MAX_STATIONS*5)                                               
STALISTX EQU   *                                                                
*                                                                               
         DS    D                                                                
         EJECT                                                                  
* DEDEMCNVD                                                                     
* DEINTD                                                                        
* DEINTTPTD                                                                     
* DEBBCNVD                                                                      
* DEDEMFILE                                                                     
* DDCOMFACS                                                                     
* DEDEMTABD                                                                     
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTTPTD                                                      
       ++INCLUDE DEBBCNVD                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         EJECT                                                                  
**********************************************                                  
*                                                                               
*  DSECT OF HEADER REC                                                          
HEADERD  DSECT                                                                  
         DS    CL1                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL22                                                             
HEADDM   DS    CL4                                                              
HEADYR   DS    CL4                                                              
*                                                                               
*  DSECT OF OUTPUT REC                                                          
OUTRECD  DSECT                                                                  
OUTMKTN   DS    XL5                                                             
OUTCALL   DS    CL6                                                             
OUTSDAY   DS    CL1                                                             
OUTEDAY   DS    CL1                                                             
OUTSTIME  DS    XL2                                                             
OUTETIME  DS    XL2                                                             
OUTWEEKS  DS    CL4                                                             
OUTTITLE  DS    CL16                                                            
OUTHDLEN  EQU   *-OUTRECD                                                       
OUTDEMOS  DS    0C                                                              
*                                                                               
QTRHOURD DSECT                                                                  
QTRRECTY  DS    CL1                                                             
QTRPANCD  DS    CL3                                                             
QTRAGBWK  DS    CL4                                                             
QTRDAY    DS    CL1                                                             
QTRSTACD  DS    CL3                                                             
          DS    CL1                                                             
QTRACTV   DS    CL1                                                             
QTRCATCD  DS    CL6                                                             
QTRSTHR   DS    CL2                                                             
QTRQHR    DS    CL1                                                             
          DS    CL1                                                             
QTRAUDS   DS    0CL294                                                          
QTRAUD    DS    49CL6                                                           
*                                                                               
PROGNAMD DSECT                                                                  
PROGNTYP  DS    CL1                                                             
PROGPNC   DS    CL3                                                             
PROGAGBW  DS    CL4                                                             
PROGDAY   DS    CL1                                                             
PROGSTTCD DS    CL3                                                             
          DS    CL5                                                             
PROGSTTM  DS    CL4                                                             
          DS    CL3                                                             
PROGDUR   DS    CL4                                                             
PROGTITLE DS    CL20                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031DEBBM9I   03/12/20'                                      
         END                                                                    
