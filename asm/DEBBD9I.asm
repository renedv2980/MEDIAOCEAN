*          DATA SET DEBBD9I    AT LEVEL 098 AS OF 04/02/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEBBD9IA                                                                 
         TITLE 'BBM JAN/04 NEW FMT TAPE CONVERSION'                             
DEBBDRYI CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEBBDRYI,RA                                                    
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
         STORAGE OBTAIN,LENGTH=(R3) RETURNS A(STORAGE) IN R1                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*PUTBUF*'  EYE-CATCHER                                
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
         CLI   MYRECTY,0           PRE-HEADER RECORD?                           
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'1'        HEADER RECORD?                               
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'2'        STATION AVG RECDS???                         
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'3'        UNIVERSE RECORD???                           
         BE    OPENOK1                                                          
         CLI   MYRECTY,C'5'        STATION AVG RECDS???                         
         BE    OPENOK1                                                          
****                                                                            
* CONVERT THE STATION CODE TO CALL LETTERS                                      
         USING PROGNAMD,RC                                                      
*                                                                               
         LA    RE,DIAMKT           DIARY MARKET TABLE                           
READP12  CLC   =X'FFFF',0(RE)                                                   
         BE    OPENOK1                                                          
         CLC   1(3,RC),0(RE)                                                    
         BNE   *+14                                                             
         MVC   INTMRKT,3(RE)                                                    
         B     *+12                                                             
         LA    RE,L'DIAMKT(RE)                                                  
         B     READP12                                                          
*                                                                               
         BAS   RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BNE   OPENOK1                                                          
*                                                                               
READP14  LA    RE,DIARYSTA                                                      
READP14A CLC   =X'FFFF',0(RE)                                                   
         BE    READP14Q                                                         
         CLC   PROGSTTCD,0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'DIARYSTA(RE)                                                
         B     READP14A                                                         
         MVC   MYCALL,3(RE)                                                     
         B     READP15                                                          
*                                                                               
READP14Q MVI   MYCALL,C'B'                                                      
         MVC   MYCALL+1(3),PROGSTTCD                                            
         MVI   INTSPILL,C'Y'                                                    
*                                                                               
READP15  CLC   MYCALL(4),=C'CIHF'                                               
         BNE   READP18                                                          
         LA    RE,CIHFTAB                                                       
READP16  CLC   PROGPNC,0(RE)                                                    
         BE    READP17                                                          
         LA    RE,L'CIHFTAB(RE)                                                 
         CLI   0(RE),X'FF'         BYPASS CIHF DATA NOT IN THESE MKTS           
         BE    OPENOK1                                                          
         B     READP16                                                          
*                                                                               
READP17  CLC   PROGSTTCD,3(RE)                                                  
         BNE   OPENOK1                                                          
*                                                                               
READP18  CLI   BYPREAD,1           RESTORE STATION CALL LETTERS                 
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
         CLI   MYRECTY,0           PRE-HEADER RECORD                            
         BE    PHEADER                                                          
         CLI   MYRECTY,C'1'        HEADER RECORD                                
         BE    HEADER                                                           
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
PHEADER  DS    0H                                                               
         MVC   DUB,PHDBK-PHEADERD(RC)    AS DERIVED BY PRE-PROCESSOR            
         MVC   DUB+6(2),=C'01'     FOR DATCON                                   
         GOTO1 VDATCON,DMCB,(9,DUB),(3,DUB1)    ** Y2K **                       
         MVC   SAVEBOOK,DUB1                                                    
         B     OPENOK              BYPASS UNKOWN RECD TYPES                     
*                                                                               
**********************************************************************          
HEADER   DS    0H                                                               
         USING HEADERD,RC                                                       
         MVC   DUB(4),HEADYR                                                    
         MVC   DUB+4(2),HEADMM                                                  
         MVC   DUB+6(2),HEADDD                                                  
         GOTO1 VDATCON,DMCB,(9,DUB),(3,DUB1)    ** Y2K **                       
         MVC   SVBOOK(1),DUB1      BINARY YEAR FROM HEADER RECORD               
         GOTO1 VDATCON,DMCB,(3,DUB1),DUB                                        
         GOTO1 VNETWEEK,DMCB,DUB,VGETDAY,VADDAY                                 
         MVC   SVBOOK+1(1),8(R1)   WEEK FROM NETWEEK                            
         B     OPENOK              BYPASS UNKOWN RECD TYPES                     
*                                                                               
**********************************************************************          
*UNIVS - SLOT UNIVERSES FROM MARKET LEVEL RECD RECTYPE=9 INTO                   
*        SAVUNIV BUFFER TO COPY INTO EACH STATION RECD LATER                    
**********************************************************************          
UNIVS    DS    0H                                                               
         USING UNIVD,RC                                                         
*                                                                               
         LA    RE,DIAMKT           DIARY MARKET TABLE                           
UNIV10   CLC   =X'FFFF',0(RE)      IF MARKET NOT FOUND, WE DONT PROC            
         BE    OPENOK                                                           
         CLC   1(3,RC),0(RE)                                                    
         BNE   *+14                                                             
         MVC   MKT,3(RE)                                                        
         B     *+12                                                             
         LA    RE,L'DIAMKT(RE)                                                  
         B     UNIV10                                                           
*                                                                               
         XC    HOMETAB,HOMETAB     SET UP HOME STATIONS FOR MKT                 
         BAS   RE,BLDHSTA                                                       
*                                                                               
UNIV20   DS    0H                                                               
*                                                                               
UNIV40   LA    RE,STALIST          CLEAR TEMP OUTPUT BUFFER AREA                
         XCEF  (RE),1000                                                        
         BAS   RE,SLOTDEMS         SAVE UNIVS IN SAVUNIV BUFFER                 
         B     OPENOK              GO READ NEXT INPUT RECD FROM TAPE            
         EJECT                                                                  
*                                                                               
**********************************************************************          
* OUTRTN: BUILD RECORD INTO INTERIM RECORD                                      
**********************************************************************          
*                                                                               
OUTRTN   DS    0C                                                               
         USING QTRHOURD,RC                                                      
*                                                                               
         LA    R5,OUTDATA                                                       
         USING OUTRECD,R5                                                       
* CONVERT THE STATION CODE TO CALL LETTERS                                      
*                                                                               
OUTRN14  LA    RE,DIARYSTA                                                      
OUTRN14A CLC   =X'FFFF',0(RE)                                                   
         BE    OUTRN14Q                                                         
         CLC   QTRSTACD,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,L'DIARYSTA(RE)                                                
         B     OUTRN14A                                                         
         MVC   MYCALL,3(RE)                                                     
         B     OUTRN15                                                          
OUTRN14Q MVC   MYCALL+1(3),QTRSTACD                                             
         MVI   INTSPILL,C'Y'                                                    
*                                                                               
OUTRN15  CLC   MYCALL(4),=C'CIHF'                                               
         BNE   OUTRN15X                                                         
         LA    RE,CIHFTAB                                                       
OUTRN15A CLC   QTRPANCD,0(RE)                                                   
         BE    OUTRN15B                                                         
         LA    RE,L'CIHFTAB(RE)                                                 
         CLI   0(RE),X'FF'         BYPASS CIHF DATA NOT IN THESE MKTS           
         BE    OPENOK                                                           
         B     OUTRN15A                                                         
OUTRN15B CLC   QTRSTACD,3(RE)                                                   
         BNE   OPENOK                                                           
*                                                                               
OUTRN15X CLI   BYPREAD,1           RESTORE STATION CALL LETTERS                 
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
*                                                                               
         LA    RE,DIAMKT           DIARY MARKET TABLE                           
OUTRN16  CLC   =X'FFFF',0(RE)                                                   
         BE    OPENOK                                                           
         CLC   1(3,RC),0(RE)                                                    
         BNE   *+14                                                             
         MVC   INTMRKT,3(RE)                                                    
         B     *+12                                                             
         LA    RE,L'DIAMKT(RE)                                                  
         B     OUTRN16                                                          
*                                                                               
         BAS   RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BNE   OPENOK                                                           
*                                                                               
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
         MVC   INTDAY,1(RE)                                                     
*                                                                               
         MVC   INTSQH,QTRQHR                                                    
         MVC   INTEQH,QTRQHR                                                    
*                                                                               
         MVI   INTWEEKS,X'21'      FORCE 1 WEEK LOOKUP                          
         MVI   INTBTYP,C'W'                                                     
*                                                                               
         MVI   INTSTYP,PARENTE                                                  
         CLI   GLOBAL,C'Y'                                                      
         BE    GETHOM2                                                          
         LA    RE,HOMETAB                                                       
GETHOME  CLC   INTSTA(5),0(RE)     CHECK ALL 5 CHAR                             
         BE    GETHOM2                                                          
         CLC   INTSTA(4),0(RE)     ONLY CHECK 4 BBM MESSES UP THE 5TH           
         BE    GETHOM2                                                          
         LA    RE,5(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   GETHOME                                                          
         OI    INTSTYP,OUTMARE                                                  
GETHOM2  DS    0H                                                               
         MVC   INTSTA,MYCALL                                                    
         LA    R9,STALIST          1ST TIME FOR STATION?                        
CHKSTA   CLI   0(R9),0             END OF LIST                                  
         BNE   CHKSTA2             NO TRY NEXT                                  
         MVI   BYPREAD,1           YES - INSERT AND SEND 'M' RECORD             
         MVC   0(5,R9),INTSTA                                                   
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
         EJECT                                                                  
*                                                                               
**********************************************************************          
*BLDHSTA - BUILD HOME STATION TABLE BY AREA CODE                                
* RC : POINTING AT THE RECORD                                                   
* R5 : HOME STATION TABLE                                                       
* R4 : HOMETAB                                                                  
**********************************************************************          
BLDHSTA  NTR1                                                                   
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
BH15     OC    HOMEBK,HOMEBK                                                    
         BZ    BH20                                                             
         MVC   DUB(L'HOMEBK),HOMEBK                                             
         MVI   DUB+2,15                                                         
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 VGETBRD,DMCB,(X'01',WORK),WORK+6,VGETDAY,VADDAY                  
         GOTO1 VNETWEEK,DMCB,WORK+6,VGETDAY,VADDAY                              
         MVC   HALF(1),DMCB+4                                                   
         MVC   HALF+1(1),DMCB+8                                                 
         CLC   SVBOOK,HALF                                                      
         BL    BH30                                                             
BH20     MVC   0(5,R4),HOMECALL                                                 
         LA    R4,5(R4)                                                         
BH30     AR    R5,R0                                                            
         B     BH10                                                             
*                                                                               
BHX      XIT1                                                                   
************************************************************                    
         EJECT                                                                  
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
SLOT15   LA    R5,OUTDATA           IN OUTDEMOS                                 
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
SLOT70   DS    0H                  NOW SEED INTO INTACCS                        
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
         SPACE 1                                                                
*                                                                               
SLOT90   DS    0H                                                               
*                                                                               
         CLI   MYRECTY,C'2'        UNIV GET SAVED AWAY FOR LATER                
         BNE   SLOT95                                                           
         MVC   SAVUNIV,INTACCS+UNVQ  SAVE UNIVS                                 
         B     SLOTX                                                            
SLOT95   MVC   INTACCS+UNVQ(UNVQLN),SAVUNIV  PUT UNIVS IN EA RECD               
*                                                                               
SLOTX    B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*SETKEY - BUILD R-KEY FOR RECORD                                                
*********************************************************************           
SETKEY   NTR1                                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'R'                                                     
         MVI   DRCODE,C'R'                                                      
         XC    INTSPILL,INTSPILL                                                
         MVC   INTBOOK,SAVEBOOK                                                 
         OC    INTBOOK,INTBOOK                                                  
         BNZ   *+10                                                             
         MVC   INTBOOK,SVBOOK                                                   
         MVC   DRBOOK,INTBOOK                                                   
*                                                                               
         MVI   DRBTYP,C'W'                                                      
*                                                                               
         MVI   DRMEDIA,C'C'                                                     
         MVI   DRSRC,C'A'                                                       
         MVC   DRSTAT,INTSTA                                                    
         TM    INTSTYP,X'20'                                                    
         BZ    *+14                                                             
         MVC   DRKMKT,INTMRKT      SET STATYP IN KEY                            
         MVI   INTSPILL,C'Y'                                                    
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
         CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEY3                                                          
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         MVC   DRHIQHR+6(L'INTWEEKS),INTWEEKS                                   
         B     SETKEYX                                                          
         SPACE 2                                                                
SETKEY3  MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYX                                                          
SETKEYX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
SETKEYP  NTR1                                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'R'                                                     
         MVI   DRCODE,C'R'                                                      
         MVC   INTBOOK,SAVEBOOK                                                 
         OC    INTBOOK,INTBOOK                                                  
         BNZ   *+10                                                             
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
         EJECT                                                                  
         XIT1                                                                   
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
GOODREC  DS    0H                                                               
         CLC   INTSQH,PREVSQH      FORCE DIFFERENT PROG NAMES                   
         BNE   PNAMOK              IF NUMBER OF WEEKS DIFFERS                   
         CLC   INTPNAM,PREVPNAM                                                 
         BNE   PNAMOK                                                           
         MVC   INTPNAM+13(1),DRHIQHR+1                                          
         OI    INTPNAM+13,X'F0'                                                 
PNAMOK   MVC   PREVSQH,INTSQH                                                   
         MVC   PREVPNAM,INTPNAM                                                 
         CLI   DRHIQHR+1,2                                                      
         BL    *+8                                                              
         OI    INTWEEKS,B'00100000' SET TYPICAL                                 
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*FLTMKT - APPLY JCL MARKET FILTER TO INPUT RECORD.  ONLY KEEP MKT               
*        REQUESTED IN PARAMETER CARD                                            
*********************************************************************           
FLTMKT   DS    0H                                                               
         SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         BR    RE                  SKIPPING CONVERSION FILTER                   
*                                  LET DEMCNV DO THE FILTERING                  
         ICM   RF,1,FILTMRKT       ANY JCL MKT FILTERING?                       
         BZR   RE                  NO, JUST EXIT                                
         OC    INTMRKT,INTMRKT     RECORD'S MKT NUMBER                          
         BZR   RE                                                               
         LA    R1,FILTMRKT+1                                                    
         CLC   INTMRKT,0(R1)                                                    
         BER   RE                                                               
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,*-12                                                          
         MVI   PASSFLT,C'N'                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ASK OPERATOR FOR MORE INPUT                                                   
*                                                                               
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN,REWIND)                                                      
         MVC   NRECS,=F'0'                                                      
                                                                                
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
MOREP    DS    0H'0'                                                            
         MVI   DONEPROG,C'Y'                                                    
         MVI   BYPREAD,0                                                        
         CLOSE (INPROG,REWIND)                                                  
         OPEN  (IN,(INPUT))                                                     
         B     READ2                                                            
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
METROA   EQU   1                                                                
METROB   EQU   2                                                                
GLOBIND  EQU   4                                                                
         EJECT                                                                  
PASSFLT  DC    X'00'                                                            
BYPREAD  DC    X'00'                                                            
STASW    DC    X'00'                                                            
MKT      DS    XL2                                                              
PREVSTAT DS    CL5                                                              
PREVSQH  DS    CL1                                                              
PREVPNAM DS    CL14                                                             
MYCALL   DS    CL5                                                              
SVBOOK   DS    XL2                                                              
SAVEBOOK DS    XL2                 AS DERIVED BY PRE-PROCESSOR                  
MYRECTY  DS    X                                                                
DONEPROG DS    CL1                                                              
         DS    0F                                                               
HOMETAB  DS    CL200                                                            
STALIST  DS    CL1000                                                           
SAVUNIV  DS    CL((UNIVEND+1)*4)                                                
*                                                                               
*          DATA SET DEBBM9I    AT LEVEL 017 AS OF 08/06/09                      
* INPUT TYPE 2                                                                  
IUVM2O   EQU   0                                                                
IUVM12O  EQU   1                                                                
IUVM18O  EQU   2                                                                
IUVM25O  EQU   3                                                                
IUVM35O  EQU   4                                                                
IUVM50O  EQU   5                                                                
IUVM55O  EQU   6                                                                
IUVM60O  EQU   7                                                                
IUVM65O  EQU   8      * BLANK FOR DIARY                                         
IUVW2O   EQU   9                                                                
IUVW12O  EQU   10                                                               
IUVW18O  EQU   11                                                               
IUVW25O  EQU   12                                                               
IUVW35O  EQU   13                                                               
IUVW50O  EQU   14                                                               
IUVW55O  EQU   15                                                               
IUVW60O  EQU   16                                                               
IUVW65O  EQU   17     * BLANK FOR DIARY                                         
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
IQVM65O  EQU   8      * BLANK FOR DIARY                                         
IQVW2O   EQU   9                                                                
IQVW12O  EQU   10                                                               
IQVW18O  EQU   11                                                               
IQVW25O  EQU   12                                                               
IQVW35O  EQU   13                                                               
IQVW50O  EQU   14                                                               
IQVW55O  EQU   15                                                               
IQVW60O  EQU   16                                                               
IQVW65O  EQU   17     * BLANK FOR DIARY                                         
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
OUVM65O  EQU   8      * BLANK FOR DIARY                                         
OUVW2O   EQU   9                                                                
OUVW12O  EQU   10                                                               
OUVW18O  EQU   11                                                               
OUVW25O  EQU   12                                                               
OUVW35O  EQU   13                                                               
OUVW50O  EQU   14                                                               
OUVW55O  EQU   15                                                               
OUVW60O  EQU   16                                                               
OUVW65O  EQU   17     * BLANK FOR DIARY                                         
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
OQVM65O  EQU   26     * BLANK FOR DIARY                                         
OQVW2O   EQU   27                                                               
OQVW12O  EQU   28                                                               
OQVW18O  EQU   29                                                               
OQVW25O  EQU   30                                                               
OQVW35O  EQU   31                                                               
OQVW50O  EQU   32                                                               
OQVW55O  EQU   33                                                               
OQVW60O  EQU   34                                                               
OQVW65O  EQU   35     * BLANK FOR DIARY                                         
*                                                                               
UNVQ     EQU   OUVM2O*4              DISP TO UNIVS IN INTACCS                   
UNVQLN   EQU   (UNIVEND-OUVM2O+1)*4   L'UNIVERSESS                              
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
         DC   AL1(IUVM65O,OUVM65O)     * BLANK FOR DIARY                        
         DC   AL1(IUVW2O,OUVW2O)                                                
         DC   AL1(IUVW12O,OUVW12O)                                              
         DC   AL1(IUVW18O,OUVW18O)                                              
         DC   AL1(IUVW25O,OUVW25O)                                              
         DC   AL1(IUVW35O,OUVW35O)                                              
         DC   AL1(IUVW50O,OUVW50O)                                              
         DC   AL1(IUVW55O,OUVW55O)                                              
         DC   AL1(IUVW60O,OUVW60O)                                              
         DC   AL1(IUVW65O,OUVW65O)     * BLANK FOR DIARY                        
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
         DC   AL1(IQVM65O,OQVM65O)     * BLANK FOR DIARY                        
         DC   AL1(IQVW2O,OQVW2O)                                                
         DC   AL1(IQVW12O,OQVW12O)                                              
         DC   AL1(IQVW18O,OQVW18O)                                              
         DC   AL1(IQVW25O,OQVW25O)                                              
         DC   AL1(IQVW35O,OQVW35O)                                              
         DC   AL1(IQVW50O,OQVW50O)                                              
         DC   AL1(IQVW55O,OQVW55O)                                              
         DC   AL1(IQVW60O,OQVW60O)                                              
         DC   AL1(IQVW65O,OQVW65O)     * BLANK FOR DIARY                        
         DC   X'FF',X'FFFF'                                                     
         EJECT                                                                  
SVCALL   DS    CL(L'INTSTA)                                                     
OUTREC   DS    0XL1004       MAX LENGTH 1004 WITH LENGTH                        
OUTHEAD  DS    XL4                                                              
OUTDATA  DS    XL1000                                                           
*                                                                               
*********************************************************************           
*          DATA SET DETMPRD    AT LEVEL 001 AS OF 12/17/98                      
*********************************************************************           
*                                                                               
         EJECT                                                                  
IN       DCB   DDNAME=FILEIN1,                                         X        
               DSORG=PS,                                               X        
               EODAD=MORET,                                            X        
               RECFM=FB,                                               X        
               LRECL=00200,                                            X        
               MACRF=GM                                                         
         EJECT        PROG                                                      
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
*                                                                               
CIHFTAB  DS    0CL6                                                             
         DC    CL3'001',CL3'210'        FULL COVERAGE-CIHF                      
         DC    CL3'004',CL3'024'        CHARLOTTETOWN-CIHFNS                    
         DC    CL3'006',CL3'210'        ATLANTIC-CIHF                           
         DC    CL3'008',CL3'024'        SYDNEY-CIHFNS                           
         DC    CL3'010',CL3'024'        HALIFAX-CIHFNS                          
         DC    CL3'012',CL3'016'        STJ/MONCTON-CIHFNB                      
         DC    X'FF'                                                            
*                                                                               
DIAMKT   DS    0CL5                                                             
         DC    CL3'001',AL2(0001)       FULL COVERAGE                           
*        DC    CL3'002',AL2(0009)       ST JOHN'S CM                            
         DC    CL3'003',AL2(0009)       ST JOHN'S-CRBRK EM                      
         DC    CL3'004',AL2(1021)       CHARLOTTETOWN EM                        
         DC    CL3'006',AL2(1031)       MARITIMES EM                            
*        DC    CL3'007',AL2(2011)       SYDNEY CA NOV/04                        
         DC    CL3'008',AL2(2010)       SYD-GLACE EM                            
         DC    CL3'010',AL2(2080)       HALIFAX CO                              
         DC    CL3'012',AL2(3011)       ST JOHN-MONTON EM                       
         DC    CL3'013',AL2(3111)       CARLETON EM                             
         DC    CL3'014',AL2(4061)       RIM-MAT-SEPT-IL EM                      
         DC    CL3'015',AL2(4101)       RIVIERE-DU-LOUP EM                      
         DC    CL3'017',AL2(4120)       CHICOUTEMI CO                           
         DC    CL3'019',AL2(4199)       QUEBEC EM                               
         DC    CL3'020',AL2(4351)       SHERBROOKE                              
         DC    CL3'022',AL2(4479)       MONTREAL EM                             
         DC    CL3'024',AL2(4480)       MTL EM ANGLO                            
         DC    CL3'026',AL2(4481)       MTL EM FRANCO                           
         DC    CL3'027',AL2(4661)       TROIS-RIVIERES EM                       
         DC    CL3'028',AL2(4723)       ROUYN EM                                
         DC    CL3'030',AL2(5069)       OTTAWA-HL EM                            
         DC    CL3'032',AL2(5071)       OTT-GAT EM ANGLO                        
         DC    CL3'034',AL2(5072)       OTT-GAT EM FRANCO                       
         DC    CL3'035',AL2(5100)       PEMBROKE CM                             
         DC    CL3'037',AL2(5109)       KINGSTON EM                             
         DC    CL3'038',AL2(5145)       EAST CTRL ONT EM                        
         DC    CL3'040',AL2(5159)       PETERBORO EM                            
         DC    CL3'042',AL2(5199)       TORONTO EM                              
         DC    CL3'043',AL2(5243)       BARRIE EM                               
         DC    CL3'044',AL2(5269)       HAMILTON CMA                            
         DC    CL3'045',AL2(5336)       KITCHENER-LOND EM                       
         DC    CL3'047',AL2(5339)       KITCHENER EM                            
         DC    CL3'049',AL2(5369)       LONDON EM                               
         DC    CL3'051',AL2(5409)       WINDSOR CM                              
         DC    CL3'052',AL2(5479)       SUDBURY                                 
         DC    CL3'053',AL2(5461)       SUD-TIM-NB EM                           
         DC    CL3'055',AL2(5511)       SUD-TIM-NB/SSM EM                       
         DC    CL3'056',AL2(5531)       ALGOMA DISTRICT                         
         DC    CL3'058',AL2(5539)       THUNDER BAY EM                          
         DC    CL3'059',AL2(5565)       KENORA EM                               
         DC    CL3'060',AL2(6061)       BRANDON EM                              
         DC    CL3'062',AL2(6119)       WINNIPEG EM                             
         DC    CL3'063',AL2(7011)       YORKTON EM                              
         DC    CL3'066',AL2(7071)       REG-MOOSE EM                            
         DC    CL3'068',AL2(7109)       SASKATOON EM                            
         DC    CL3'069',AL2(7153)       PR ALBERT EM                            
         DC    CL3'070',AL2(8010)       MEDICINE HAT EM                         
         DC    CL3'071',AL2(9763)       CALGARY USA FC                          
         DC    CL3'072',AL2(8069)       CALGARY EM                              
         DC    CL3'073',AL2(8078)       RED DEER EM                             
         DC    CL3'074',AL2(8091)       LLOYDMINSTER EM                         
         DC    CL3'076',AL2(8119)       EDMONTON EM                             
         DC    CL3'077',AL2(9071)       OKAN-KAMLPS EM                          
         DC    CL3'079',AL2(9109)       VANCOUVER EM                            
         DC    CL3'081',AL2(9301)       TERRACE-KITIMAT EM                      
         DC    CL3'082',AL2(9341)       PR GEORGE+TERR  EM                      
         DC    CL3'083',AL2(9351)       PRINCE GEORGE EM                        
         DC    CL3'084',AL2(9363)       DAWSON CREEK EM                         
         DC    CL3'088',AL2(9580)       CHCH CANCOM EM                          
         DC    CL3'092',AL2(9590)       CHAN CANCOM EM                          
         DC    CL3'093',AL2(9710)       CTV SASKATCHWN                          
         DC    CL3'094',AL2(9802)       NOVA SCOTIA                             
         DC    CL3'096',AL2(9600)       VANCOUVER FC                            
         DC    CL3'108',AL2(9231)       KELOWNA                                 
         DC    CL3'109',AL2(9331)       PRINCE GEORGE-KAMLOOPS                  
         DC    X'FFFF'                                                          
*                                                                               
DIARYSTA DS    0CL9                                                             
         DC    CL3'001',CL6'ALL'                                                
         DC    CL3'002',CL6'OTHERS'                                             
         DC    CL3'003',CL6'ASN'                                                
         DC    CL3'004',CL6'CBNT+'                                              
         DC    CL3'005',CL6'CITV'                                               
         DC    CL3'006',CL6'CJON'                                               
         DC    CL3'007',CL6'TSN'                                                
         DC    CL3'008',CL6'WBZ'                                                
         DC    CL3'009',CL6'WCVB'                                               
         DC    CL3'010',CL6'WHDH'                                               
         DC    CL3'011',CL6'WTBS'                                               
         DC    CL3'012',CL6'WTVS'                                               
         DC    CL3'013',CL6'WUHF'                                               
         DC    CL3'014',CL6'YTV'                                                
         DC    CL3'015',CL6'CBCT'                                               
         DC    CL3'016',CL6'CIHFNB'                                             
         DC    CL3'017',CL6'CKCW+'                                              
         DC    CL3'018',CL6'WGBH'                                               
         DC    CL3'019',CL6'ATV'                                                
         DC    CL3'020',CL6'AEN'                                                
         DC    CL3'021',CL6'CBHT'                                               
         DC    CL3'022',CL6'CBIT'                                               
         DC    CL3'023',CL6'CHCH'                                               
         DC    CL3'024',CL6'CIHFNS'                                             
         DC    CL3'025',CL6'CJCB'                                               
         DC    CL3'026',CL6'CJCH'                                               
         DC    CL3'027',CL6'TNN'                                                
         DC    CL3'028',CL6'CBAFT'                                              
         DC    CL3'029',CL6'CBAT'                                               
         DC    CL3'030',CL6'WLBZ'                                               
         DC    CL3'031',CL6'WTOL'                                               
         DC    CL3'032',CL6'WXYZ'                                               
         DC    CL3'033',CL6'CFJP+'                                              
         DC    CL3'034',CL6'CFTF'                                               
         DC    CL3'035',CL6'CFTM+'                                              
         DC    CL3'036',CL6'CHAU'                                               
         DC    CL3'037',CL6'CIVM+'                                              
         DC    CL3'038',CL6'CJBRT+'                                             
         DC    CL3'039',CL6'VRAKTV'                                             
         DC    CL3'040',CL6'RDS'                                                
         DC    CL3'041',CL6'CFER'                                               
         DC    CL3'042',CL6'TOON F'                                             
         DC    CL3'043',CL6'CFCM'                                               
         DC    CL3'044',CL6'CIMT'                                               
         DC    CL3'045',CL6'CKRT'                                               
         DC    CL3'046',CL6'ECRAN'                                              
         DC    CL3'047',CL6'CFRS'                                               
         DC    CL3'048',CL6'CJPM'                                               
         DC    CL3'049',CL6'CKRS'                                               
         DC    CL3'050',CL6'CBVT'                                               
         DC    CL3'051',CL6'CFAP'                                               
         DC    CL3'052',CL6'CHLT'                                               
         DC    CL3'053',CL6'CFKS'                                               
         DC    CL3'054',CL6'CKSH'                                               
         DC    CL3'055',CL6'WCAX'                                               
         DC    CL3'056',CL6'CBFT'                                               
         DC    CL3'057',CL6'CBMT'                                               
         DC    CL3'058',CL6'CFCF'                                               
         DC    CL3'059',CL6'CJOH'                                               
         DC    CL3'060',CL6'CKMI'                                               
         DC    CL3'061',CL6'WCFE'                                               
         DC    CL3'062',CL6'WETK'                                               
         DC    CL3'063',CL6'WFFF'                                               
         DC    CL3'064',CL6'WPTZ'                                               
         DC    CL3'065',CL6'WVNY'                                               
         DC    CL3'066',CL6'CFKM'                                               
         DC    CL3'067',CL6'CHEM'                                               
         DC    CL3'068',CL6'CKTM'                                               
         DC    CL3'069',CL6'RDI'                                                
         DC    CL3'070',CL6'CFEM'                                               
         DC    CL3'071',CL6'CFVS'                                               
         DC    CL3'072',CL6'CKRN'                                               
         DC    CL3'073',CL6'CBOFT'                                              
         DC    CL3'074',CL6'CBOT'                                               
         DC    CL3'075',CL6'CFGS'                                               
         DC    CL3'076',CL6'CFMT'                                               
         DC    CL3'077',CL6'CHOT'                                               
         DC    CL3'078',CL6'CHRO'                                               
         DC    CL3'079',CL6'CICO E'                                             
         DC    CL3'080',CL6'CIII'                                               
         DC    CL3'081',CL6'CITY'                                               
         DC    CL3'082',CL6'NEWSWD'                                             
         DC    CL3'083',CL6'WPBS'                                               
         DC    CL3'084',CL6'WROC'                                               
         DC    CL3'085',CL6'WUTV'                                               
         DC    CL3'086',CL6'TOON E'                                             
         DC    CL3'087',CL6'WHEC'                                               
         DC    CL3'088',CL6'WOKR'                                               
         DC    CL3'089',CL6'CKWS'                                               
         DC    CL3'090',CL6'WSTM'                                               
         DC    CL3'091',CL6'WSYT'                                               
         DC    CL3'092',CL6'WWNY'                                               
         DC    CL3'093',CL6'WWTI'                                               
         DC    CL3'094',CL6'CECO'                                               
         DC    CL3'095',CL6'CFTO'                                               
         DC    CL3'096',CL6'CBLT'                                               
         DC    CL3'097',CL6'CHEX+'                                              
         DC    CL3'098',CL6'CKVR'                                               
         DC    CL3'099',CL6'CBLFT'                                              
         DC    CL3'100',CL6'CICO F'                                             
         DC    CL3'101',CL6'WGRZ'                                               
         DC    CL3'102',CL6'WIVB'                                               
         DC    CL3'103',CL6'WKBW'                                               
         DC    CL3'104',CL6'WNED'                                               
         DC    CL3'105',CL6'CKCO+'                                              
         DC    CL3'106',CL6'CBLN'                                               
         DC    CL3'107',CL6'CFPL+'                                              
         DC    CL3'108',CL6'WDIV'                                               
         DC    CL3'109',CL6'WSEE'                                               
         DC    CL3'110',CL6'CBEFT'                                              
         DC    CL3'111',CL6'CBET'                                               
         DC    CL3'112',CL6'TMN'                                                
         DC    CL3'113',CL6'WDWB'                                               
         DC    CL3'114',CL6'WJBK'                                               
         DC    CL3'115',CL6'WKBD'                                               
         DC    CL3'116',CL6'WWJ'                                                
         DC    CL3'117',CL6'CICI+'                                              
         DC    CL3'118',CL6'CKNC+'                                              
         DC    CL3'119',CL6'WWUP'                                               
         DC    CL3'120',CL6'CHBX'                                               
         DC    CL3'121',CL6'CICI++'                                             
         DC    CL3'122',CL6'CKNC++'                                             
         DC    CL3'123',CL6'CJIC'                                               
         DC    CL3'124',CL6'WEYI'                                               
         DC    CL3'125',CL6'WFQX'                                               
         DC    CL3'126',CL6'WJRT'                                               
         DC    CL3'127',CL6'CHFD'                                               
         DC    CL3'128',CL6'CKPR'                                               
         DC    CL3'129',CL6'CBWT'                                               
         DC    CL3'130',CL6'CJBN'                                               
         DC    CL3'131',CL6'KARE'                                               
         DC    CL3'132',CL6'TLC'                                                
         DC    CL3'133',CL6'WCCO'                                               
         DC    CL3'134',CL6'CHMI'                                               
         DC    CL3'135',CL6'CKND'                                               
         DC    CL3'136',CL6'CKX'                                                
         DC    CL3'137',CL6'CKY'                                                
         DC    CL3'138',CL6'CBWFT'                                              
         DC    CL3'139',CL6'WDAZ'                                               
         DC    CL3'140',CL6'CICC'                                               
         DC    CL3'141',CL6'CKCK'                                               
         DC    CL3'142',CL6'CKOS'                                               
         DC    CL3'143',CL6'CFQC'                                               
         DC    CL3'144',CL6'CJFB'                                               
         DC    CL3'145',CL6'CBKFT'                                              
         DC    CL3'146',CL6'CBKT'                                               
         DC    CL3'147',CL6'CFRE'                                               
         DC    CL3'148',CL6'CBKST'                                              
         DC    CL3'149',CL6'CFSK'                                               
         DC    CL3'150',CL6'CIPA'                                               
         DC    CL3'151',CL6'CKBI'                                               
         DC    CL3'152',CL6'CFCN+'                                              
         DC    CL3'153',CL6'CHAT'                                               
         DC    CL3'154',CL6'FAMILY'                                             
         DC    CL3'155',CL6'KHQ'                                                
         DC    CL3'156',CL6'KREM'                                               
         DC    CL3'157',CL6'KSPS'                                               
         DC    CL3'158',CL6'KXLY'                                               
         DC    CL3'159',CL6'ACCESS'                                             
         DC    CL3'160',CL6'CBRT'                                               
         DC    CL3'161',CL6'CICT'                                               
         DC    CL3'162',CL6'CKAL'                                               
         DC    CL3'163',CL6'CHCA'                                               
         DC    CL3'164',CL6'KAYU'                                               
         DC    CL3'165',CL6'CFRN+'                                              
         DC    CL3'166',CL6'CITL'                                               
         DC    CL3'167',CL6'CKSA'                                               
         DC    CL3'168',CL6'CBXFT'                                              
         DC    CL3'169',CL6'CBXT'                                               
         DC    CL3'170',CL6'CKEM'                                               
         DC    CL3'171',CL6'CHAN'                                               
         DC    CL3'172',CL6'CHBC+'                                              
         DC    CL3'173',CL6'KCTS'                                               
         DC    CL3'174',CL6'KING'                                               
         DC    CL3'175',CL6'KIRO'                                               
         DC    CL3'176',CL6'KOMO'                                               
         DC    CL3'177',CL6'CBUFT'                                              
         DC    CL3'178',CL6'CBUT'                                               
         DC    CL3'179',CL6'CHEK'                                               
         DC    CL3'180',CL6'CIVT'                                               
         DC    CL3'181',CL6'CKVU'                                               
         DC    CL3'182',CL6'KCPQ'                                               
         DC    CL3'183',CL6'KSTW'                                               
         DC    CL3'184',CL6'KVOS'                                               
         DC    CL3'185',CL6'CFTK'                                               
         DC    CL3'186',CL6'SUPER'                                              
         DC    CL3'187',CL6'CBC NW'                                             
         DC    CL3'188',CL6'CKPZ'                                               
         DC    CL3'189',CL6'CKPG'                                               
         DC    CL3'190',CL6'CJDC'                                               
         DC    CL3'191',CL6'CBC '                                               
         DC    CL3'192',CL6'WGTQ'                                               
         DC    CL3'193',CL6'WGN'                                                
         DC    CL3'194',CL6'KRTV'                                               
         DC    CL3'195',CL6'KNOW'                                               
         DC    CL3'196',CL6'CANALD'                                             
         DC    CL3'197',CL6'WICU'                                               
         DC    CL3'198',CL6'WUAB'                                               
         DC    CL3'199',CL6'KGFE'                                               
         DC    CL3'200',CL6'WGTU'                                               
         DC    CL3'201',CL6'KTLA'                                               
         DC    CL3'202',CL6'WSBK'                                               
         DC    CL3'203',CL6'PRIME'                                              
         DC    CL3'204',CL6'SNET'                                               
         DC    CL3'205',CL6'CTV SK'                                             
         DC    CL3'206',CL6'WMEM'                                               
         DC    CL3'207',CL6'WIXT'                                               
         DC    CL3'208',CL6'SPACE'                                              
         DC    CL3'209',CL6'TV5'                                                
         DC    CL3'210',CL6'CIHF'                                               
         DC    CL3'211',CL6'CKTV'                                               
         DC    CL3'212',CL6'VIE'                                                
         DC    CL3'213',CL6'HGTV'                                               
         DC    CL3'214',CL6'TREE'                                               
         DC    CL3'215',CL6'SERIES'                                             
         DC    CL3'216',CL6'CNN'                                                
         DC    CL3'217',CL6'M PIX'                                              
         DC    CL3'218',CL6'WPBN'                                               
         DC    CL3'219',CL6'FAIRTV'                                             
         DC    CL3'221',CL6'LATINO'                                             
         DC    CL3'222',CL6'DSCVRY'                                             
         DC    CL3'223',CL6'CJNT'                                               
         DC    CL3'224',CL6'WPIX'                                               
         DC    CL3'225',CL6'CIVI'                                               
         DC    CL3'226',CL6'TVA NB'                                             
         DC    CL3'227',CL6'3ABN'                                               
         DC    CL3'228',CL6'CBNT'                                               
         DC    CL3'229',CL6'CHWI'                                               
         DC    CL3'230',CL6'SCN'                                                
         DC    CL3'231',CL6'BRAVO'                                              
         DC    CL3'232',CL6'WEATHR'                                             
         DC    CL3'233',CL6'WNET+'                                              
         DC    CL3'234',CL6'BBCCA'                                              
         DC    CL3'235',CL6'CKXT'                                               
         DC    CL3'236',CL6'HISTTV'                                             
         DC    CL3'237',CL6'CBC NE'                                             
         DC    CL3'238',CL6'CP 24'                                              
         DC    CL3'239',CL6'CMT'                                                
         DC    CL3'240',CL6'SPIKE'                                              
         DC    CL3'241',CL6'WFXT'                                               
         DC    CL3'242',CL6'SHWCSE'                                             
         DC    CL3'243',CL6'OLN'                                                
         DC    CL3'244',CL6'CLT'                                                
         DC    CL3'246',CL6'SCORE'                                              
         DC    CL3'247',CL6'FOOD'                                               
         DC    CL3'248',CL6'CJMT'                                               
         DC    CL3'249',CL6'CHNM'                                               
         DC    CL3'252',CL6'CTVNNT'                                             
         DC    CL3'253',CL6'LCN'                                                
         DC    CL3'254',CL6'CTS'                                                
         DC    CL3'256',CL6'VCR'                                                
         DC    CL3'261',CL6'CIIT'                                               
         DC    CL3'262',CL6'CBCN'                                               
         DC    CL3'263',CL6'PVR'                                                
         DC    CL3'267',CL6'ARTV'                                               
         DC    CL3'270',CL6'CKES'                                               
         DC    CL3'273',CL6'HISTFR'                                             
         DC    CL3'274',CL6'CJEO'                                               
         DC    CL3'276',CL6'ZTELE'                                              
         DC    CL3'283',CL6'CHBC'                                               
         DC    CL3'347',CL6'INTV'                                               
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
* DEDEMCNVD                                                                     
* DEINTD                                                                        
* DEINTTPTD                                                                     
* DEBBCNVD                                                                      
* DEDEMFILE                                                                     
* DEDEMTABD                                                                     
* DDCOMFACS                                                                     
* DDDPRINT                                                                      
       ++INCLUDE DEDEMCNVD                                                      
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTTPTD                                                      
       ++INCLUDE DEBBCNVD                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         EJECT                                                                  
         TITLE                     'BBM TAPE FORMAT NOV/78'                     
**********************************************                                  
*  DSECT OF HEADER REC                                                          
HEADERD  DSECT                                                                  
HDRECTY  DS    CL1                 ALWAYS C'1'                                  
         DS    CL3                                                              
HDWEEKNO DS    CL4                 WEEK ID FROM WEEK.TXT FILE                   
         DS    CL22                                                             
HEADSTRT DS    0CL8                START DAY DATE                               
HEADDD   DS    CL2                                                              
HEADMM   DS    CL2                                                              
HEADYR   DS    CL4                                                              
HDFILENM DS    CL11                FILENAME OF THE QHR FILE                     
HDFILLER DS    0C                                                               
*                                                                               
*  PRE-HEADER RECORD (GENERATED BY PRE-PROCESSOR)                               
PHEADERD DSECT                                                                  
PHDRECTY DS    XL30                ALWAYS NULLS (SO IT SORTS FIRST)             
PHDBK    DS    0CL6                OUR DERIVED MONTHLY BOOK: YYMM               
*                                   FALL   --> NOV                              
*                                   SPRING --> MAR                              
*                                   SUMMER --> JUL                              
PHDBKYR  DS    CL4                 YEAR                                         
PHDBKMM  DS    CL2                 MONTH                                        
*                                                                               
*                                                                               
*                                                                               
*  DSECT OF OUTPUT REC                                                          
OUTRECD   DSECT                                                                 
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
*                                                                               
UNIVD     DSECT                                                                 
UNIVRECTY DS    CL1                                                             
UNIVPANCD DS    CL3                                                             
UNIVAGBWK DS    CL4                                                             
          DS    CL15                                                            
UNIVSEQN  DS    CL1                                                             
UNIVERSES DS    0CL285                                                          
UNIVERSE  DS    57CL5                                                           
UNIVCAT1  DS    CL5                                                             
UNIVCAT2  DS    CL5                                                             
UNIVCAT3  DS    CL5                                                             
UNIVCAT4  DS    CL5                                                             
UNIVCAT5  DS    CL5                                                             
UNIVCAT6  DS    CL5                                                             
UNIVCAT7  DS    CL5                                                             
UNIVCAT8  DS    CL5                                                             
UNIVCAT9  DS    CL5                                                             
UNIVCAT10 DS    CL5                                                             
UNIVCAT11 DS    CL5                                                             
UNIVCAT12 DS    CL5                                                             
UNIVCAT13 DS    CL5                                                             
UNIVCAT14 DS    CL5                                                             
UNIVCAT15 DS    CL5                                                             
UNIVCAT16 DS    CL5                                                             
UNIVCAT17 DS    CL5                                                             
UNIVCAT18 DS    CL5                                                             
UNIVCAT19 DS    CL5                                                             
UNIVCAT20 DS    CL5                                                             
UNIVCAT21 DS    CL5                                                             
UNIVCAT22 DS    CL5                                                             
UNIVCAT23 DS    CL5                                                             
UNIVCAT24 DS    CL5                                                             
UNIVCAT25 DS    CL5                                                             
UNIVCAT26 DS    CL5                                                             
UNIVCAT27 DS    CL5                                                             
UNIVCAT28 DS    CL5                                                             
UNIVCAT29 DS    CL5                                                             
UNIVCAT30 DS    CL5                                                             
UNIVCAT31 DS    CL5                                                             
UNIVCAT32 DS    CL5                                                             
UNIVCAT33 DS    CL5                                                             
UNIVCAT34 DS    CL5                                                             
UNIVCAT35 DS    CL5                                                             
UNIVCAT36 DS    CL5                                                             
UNIVCAT37 DS    CL5                                                             
UNIVCAT38 DS    CL5                                                             
UNIVCAT39 DS    CL5                                                             
UNIVCAT40 DS    CL5                                                             
UNIVCAT41 DS    CL5                                                             
UNIVCAT42 DS    CL5                                                             
UNIVCAT43 DS    CL5                                                             
UNIVCAT44 DS    CL5                                                             
UNIVCAT45 DS    CL5                                                             
UNIVCAT46 DS    CL5                                                             
UNIVCAT47 DS    CL5                                                             
UNIVCAT48 DS    CL5                                                             
UNIVCAT49 DS    CL5                                                             
UNIVCAT50 DS    CL5                                                             
UNIVCAT51 DS    CL5                                                             
UNIVCAT52 DS    CL5                                                             
UNIVCAT53 DS    CL5                                                             
UNIVCAT54 DS    CL5                                                             
UNIVCAT55 DS    CL5                                                             
UNIVCAT56 DS    CL5                                                             
UNIVCAT57 DS    CL5                                                             
*                                                                               
QTRHOURD  DSECT                                                                 
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
TRAILERD  DSECT                                                                 
TRARECTYP DS    CL1                                                             
          DS    CL23                                                            
TRAILCNT  DS    CL7                                                             
*                                                                               
PROGNAMD  DSECT                                                                 
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
**PAN#1  DC    CL21'098DEBBD9I   04/02/14'                                      
         END                                                                    
