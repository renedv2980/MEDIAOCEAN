*          DATA SET ACINQ06    AT LEVEL 062 AS OF 05/01/02                      
*PHASE T60606A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - JOB DETAIL - T60606'                      
T60606   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'JOB DETAIL' IN ACCOUNT ENQUIRY PROGRAM          
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60606)                                               
         DC    A(FILTABLE-T60606)                                               
         DC    A(KNTRYPNT-T60606)                                               
         DC    A(FNTRYPNT-T60606)                                               
         DC    A(DNTRYPNT-T60606)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
KNTRYPNT DS    0H                                                               
         NMOD1 0,**INQ6**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60606,RB                                                        
         USING T606TWA,RA                                                       
         MVI   OPTN,0                                                           
         XIT1                                                                   
*                                                                               
*                                                                               
*              BILLED FILTER MUST ALWAYS DROP WC99 TRANSACTIONS                 
*                                                                               
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ6**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60606,RB                                                        
         USING T606TWA,RA                                                       
         USING FTBD,R6                                                          
         USING FILTERSD,R4                                                      
*                                                                               
         CLC   FILFULKW,CTIME      TIME=B/N/R OPTION                            
         BNE   FN10                                                             
         MVI   FTBSTAT,1           DISABLE BASE FILTER ROUTINE                  
         CLI   FTBVAL,C'B'         SET FILTER                                   
         BNE   *+12                                                             
         OI    OPTN,TIMEB                                                       
         B     FNOKX                                                            
         CLI   FTBVAL,C'N'                                                      
         BNE   *+12                                                             
         OI    OPTN,TIMEN                                                       
         B     FNOKX                                                            
         CLI   FTBVAL,C'R'                                                      
         BNE   FNERX                                                            
         OI    OPTN,TIMER                                                       
         B     FNOKX                                                            
*                                                                               
FN10     CLC   FILFULKW,CTTYPE     TTYPE=T/M/A                                  
         BNE   FN20                                                             
         MVI   FTBSTAT,1           DISABLE BASE FILTER ROUTINE                  
         CLI   FTBVAL,C'T'         SET FILTER                                   
         BNE   *+12                                                             
         OI    OPTN,TTYPET                                                      
         B     FNOKX                                                            
         CLI   FTBVAL,C'M'                                                      
         BNE   *+12                                                             
         OI    OPTN,TTYPEM                                                      
         B     FNOKX                                                            
         CLI   FTBVAL,C'A'                                                      
         BNE   FNERX                                                            
         OI    OPTN,TTYPEA                                                      
         B     FNOKX                                                            
*                                                                               
FN20     L     R7,AIO                                                           
         USING ACKEYD,R7                                                        
         LA    R2,WORK                                                          
         MVI   WORK,0                                                           
         CLC   ACKEYWRK,=C'99'                                                  
         BE    FN40                                                             
         OC    ACDTUSED,ACDTUSED                                                
         BZ    FN30                                                             
         MVI   WORK,1                                                           
         B     FXIT                                                             
*                                                                               
FN30     BAS   RE,BILLSTAT                                                      
         B     FXIT                                                             
FN40     CLI   FTBSIGN,C'P'        INVERT FOR WRK CODE 99                       
         BE    *+8                                                              
         MVI   WORK,1                                                           
FXIT     XIT1  REGS=(R2)                                                        
*                                                                               
FNERX    MVI   DMCB,1                                                           
         B     *+8                                                              
FNOKX    MVI   DMCB,0                                                           
         XIT1                                                                   
         DROP  R6,R7                                                            
         EJECT                                                                  
*              MAIN PROCESS                                                     
*                                                                               
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ6**,RR=R9                                                 
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60606,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         ST    R9,MYRELO                                                        
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
*                                                                               
         CLI   ACKEYACC,RUNLAST    END                                          
         BE    T01                                                              
         CLI   ACKEYDTE,C' '       MUST BE TRANSACTION                          
         BE    TNEXT                                                            
*                                                                               
T00A     TM    OPTIONS,MOSRANGE    HANDLE MOS RANGE FILTER                      
         BZ    T00B                                                             
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         TM    FTBSTAT,5           DISABLED+SUBRECORD = MUST BE MOSRNGE         
         BO    *+12                IN WHICH CASE FTBVAL(12),FTBVAL+2(2)         
         LA    R6,FTBTBLEN(R6)     CONTAIN LOW & HIGH MOS CODES                 
         B     *-12                                                             
         ICM   R4,15,ATRN                                                       
         BZ    TNEXT                                                            
         USING TRANSD,R4                                                        
         GOTO1 CONVMOS,DMCB,TRANSD,WORK                                         
         CLC   WORK(2),FTBVAL+0                                                 
         BL    TNEXT                                                            
         CLC   WORK(2),FTBVAL+2                                                 
         BH    TNEXT                                                            
         DROP  R4,R6                                                            
*                                                                               
T00B     EQU   *                                                                
         TM    OPTN,TIMEB+TIMEN+TIMER TIME=B/N/R                                
         BZ    T00C                OPTION NOT SELECTED                          
         ICM   R4,15,APER                                                       
         BZ    TNEXT               NO PERSONNEL ELEMENT                         
         USING ACPERSD,R4                                                       
         TM    OPTN,TIMEB          TIME=B                                       
         BNO   *+12                                                             
         TM    ACPSSTAT,ACPSBIL    IS IT B TIME                                 
         BNO   TNEXT                                                            
         TM    OPTN,TIMEN          TIME=N                                       
         BNO   *+12                                                             
         TM    ACPSSTAT,ACPSNOT    IS IT N TIME                                 
         BNO   TNEXT                                                            
         TM    OPTN,TIMER          TIME=R                                       
         BNO   *+12                                                             
         TM    ACPSSTAT,ACPSRTE    IS IT R TIME                                 
         BNO   TNEXT                                                            
*                                                                               
T00C     TM    OPTN,TTYPET+TTYPEM+TTYPEA TTYPE=T/M/A                            
         BZ    T01                 OPTION NOT SELECTED                          
         ICM   R4,15,ATRS                                                       
         BZ    TNEXT               NO STATUS ELEMENT                            
         USING TRSELD,R4                                                        
         TM    OPTN,TTYPET         TTYPE=T                                      
         BNO   *+12                                                             
         TM    TRSSTAT2,TRSSTIME   IS IT TYPE T                                 
         BNO   TNEXT                                                            
         TM    OPTN,TTYPEM         TTYPE=M                                      
         BNO   *+12                                                             
         TM    TRSSTAT2,TRSSTMSS   IS IT TYPE M                                 
         BNO   TNEXT                                                            
         TM    OPTN,TTYPEA         TTYPE=A                                      
         BNO   *+12                                                             
         TM    TRSSTAT2,TRSSTADJ   IS IT TYPE A                                 
         BNO   TNEXT                                                            
         DROP  R4                                                               
*                                                                               
T01      CLI   VIRGIN,C'H'                                                      
         BE    T1                                                               
         EJECT                                                                  
*              FIRST TIME ACTIONS                                               
*                                                                               
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         MVI   VIRGIN,C'H'                                                      
*                                                                               
         CLI   LASTKMK,0           FIRST TIME THROUGH SAVE WORKCODE             
         BNE   T05                 AND CLEAR WORKCODE AND ACCOUNT TOTS          
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND1                                                            
         MVC   SAVEWC,ACKEYWRK                                                  
         GOTO1 =A(INIT),DMCB,(RC),VALCOUNT,RR=MYRELO                            
*                                                                               
T05      CLI   LINE+1,2                                                         
         BE    T06                                                              
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
T06      L     R2,=A(HEADING)                                                   
         A     R2,MYRELO                                                        
         MVC   INFDAT3,0(R2)                                                    
         OI    INFDAT3H+6,X'80'                                                 
*                                                                               
         LA    R2,2*L'HEADING(R2)  ADDRESS HEADING2                             
         MVC   INFDAT4,0(R2)                                                    
         OI    INFDAT4H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
         CLC   SAVEWC,=C'99'       WORKCODE 99 SPECIAL ACTION                   
         BNE   T2                                                               
         MVC   INFDAT3+41(15),=C'BILLING DETAILS'                               
         MVC   INFDAT4+52(4),=4C'-'                                             
         MVI   INFDAT3+65,C' '     CLEAR BILLED MARKER HEAD                     
         MVI   INFDAT4+65,C' '                                                  
         MVC   KEYWORDS(30),PRESETS                                             
*&&UK                                                                           
         LA    R2,KEYWORDS+10      SET UP VAT RATE KEYWORDS FOR UK              
         LA    R3,SAVEVATR                                                      
         LA    R4,5                                                             
T08      MVC   0(10,R2),SPACES                                                  
         MVC   7(3,R2),=C'VAT'                                                  
         EDIT  (2,0(R3)),(5,0(R2)),2,FILL=0                                     
         MVI   5(R2),C'%'                                                       
         LA    R2,10(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,T08                                                           
*&&                                                                             
         B     T2                                                               
         EJECT                                                                  
*              EACH TIME ACTIONS                                                
*                                                                               
T1       CLI   LINE+1,18           CHECK FOR SCREEN FULL                        
         BH    TFULL                                                            
         TM    OPTIONS,DDSONLY+XDETAIL                                          
         BZ    *+12                                                             
         CLI   LINE+1,18                                                        
         BNL   TFULL                                                            
*                                                                               
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MHI   R6,86                                                            
         LA    R6,INFDATAH(R6)                                                  
*                                                                               
         USING LINED,R6                                                         
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND                                                             
*                                                                               
         CLC   ACKEYWRK,SAVEWC     WORKCODE CONTROL BREAK                       
         BE    T27                                                              
         GOTO1 =A(WORKTOTS),DMCB,(RC),RR=MYRELO                                 
         BM    TFULL                NO ROOM                                     
         MVC   SAVEWC,ACKEYWRK                                                  
         CLC   SAVEWC,=C'99'       NEW PAGE FOR WC 99                           
         BE    TFULL                                                            
         B     T1                                                               
*                                                                               
         USING ACKEYD,R5                                                        
T27      L     R5,AIO                                                           
         TM    OPTIONS,PEELED      SKIP PEELED UNLESS PEELED=YES                
         BZ    T2A                 IN WHICH CASE SHOW ONLY PEELED               
         OC    ACDTPEEL,ACDTPEEL                                                
         BZ    TNEXT                                                            
         B     *+14                                                             
T2A      OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   TNEXT                                                            
*                                                                               
         USING TRANSD,R5                                                        
         ICM   R5,15,ATRN                                                       
         BZ    TNEXT                                                            
         CLC   SAVEWC,=C'99'       WC 99 NARRATIVE                              
         BNE   T28                                                              
         MVI   CALLTYPE,DETAIL                                                  
         GOTO1 =A(DISPWC99),DMCB,(RC),RR=MYRELO                                 
         BM    TFULL                                                            
         LH    R9,LINE                                                          
         B     T5                                                               
*                                                                               
T28      ZIC   R4,TRNSLEN          OTHER WC NARRATIVE                           
         L     RE,AIOB                                                          
         MVI   0(RE),C' '                                                       
         MVC   1(250,RE),0(RE)                                                  
         SH    R4,=H'29'                                                        
         BM    T28A                                                             
         EX    R4,*+8                                                           
         B     T28A                                                             
         MVC   0(0,RE),TRNSNARR                                                 
         LA    R4,1(R4)                                                         
T28A     AH    R4,=H'1'            FIND NEXT AVAILABLE 25 BYTE CHUNK            
         A     R4,AIOB             POINT R2 AT IT & SET R4 TO LENGTH            
         L     RF,AIOB             SO FAR (TO FORCE NEXT NARRATIVE TO A         
         SR    R4,RF               NEW DISPLAY LINE)                            
         SR    R0,R0                                                            
         LR    R1,R4                                                            
         D     R0,=F'25'                                                        
         LTR   R0,R0                                                            
         BZ    *+10                                                             
         A     R4,=F'25'                                                        
         SR    R4,R0                                                            
         LR    R2,R4                                                            
         A     R2,AIOB                                                          
         ICM   R8,15,ATRT                                                       
         BZ    T28B                                                             
*                                                                               
         USING TRTRANSD,R8                                                      
         LR    R3,R2                                                            
         MVC   0(11,R2),=C'TRANSFERRED'                                         
         CLI   TRTRTYPE,C'F'                                                    
         BE    *+18                                                             
         MVC   12(2,R2),=C'TO'                                                  
         LA    R2,15(R2)                                                        
         B     *+14                                                             
         MVC   12(4,R2),=C'FROM'                                                
         LA    R2,17(R2)                                                        
         MVC   0(14,R2),TRTRACC+1                                               
         LA    R2,13(R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(2,R2),=C'ON'                                                   
         GOTO1 VDATCON,DMCB,(1,TRTRDATE),(8,5(R2))                              
         LA    R4,13(R2)                                                        
         L     R0,AIOB                                                          
         SR    R4,R0               CALCULATE TOTAL LENGTH                       
*                                                                               
         LA    R2,45(R3)           RESET R2                                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)            POINT TO NEXT AVAILABLE BYTE                 
*                                                                               
         USING ACNOD,R8                                                         
T28B     ICM   R8,15,AXNO          ORDER NUMBER                                 
         BZ    T28C                                                             
         MVC   0(4,R2),=C'ORD='                                                 
         ZIC   R1,ACNOLEN                                                       
         SH    R1,=H'3'                                                         
         BM    T28C                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R2),ACNO                                                     
         LA    R4,5(R1,R4)                                                      
         LA    R2,11(R2)                                                        
*                                                                               
         USING ACOTHERD,R8                                                      
T28C     ICM   R8,15,AOTH                                                       
         BZ    T28D                                                             
         MVC   0(7,R2),=C'SUBREF='                                              
         MVC   7(6,R2),ACOTNUM                                                  
         LA    R4,14(R4)                                                        
         LA    R2,14(R2)                                                        
         DROP  R8                                                               
*                                                                               
         USING TRANSD,R5                                                        
T28D     ICM   R5,15,ATRN                                                       
         TM    TRNSSTAT,X'04'                                                   
         BZ    T28D1                                                            
         MVC   0(4,R2),=C'HELD'                                                 
         LA    R2,5(R2)                                                         
         LA    R4,5(R4)                                                         
T28D1    CLI   INVREG,C'Y'         SHOW AUTH/UNAUTH IF USING INV REG            
         BNE   T28F                                                             
         MVC   0(4,R2),=C'AUTH'                                                 
         TM    TRNSSTAT,X'08'                                                   
         BZ    T28E                                                             
         LA    R2,5(R2)                                                         
         LA    R4,5(R4)                                                         
         B     T28F                                                             
T28E     MVC   0(6,R2),=C'UNAUTH'                                               
         LA    R2,7(R2)                                                         
         LA    R4,7(R4)                                                         
*                                                                               
T28F     ICM   R8,15,ACSH          SHOW UK HOURS                                
         BZ    T28G                                                             
         USING TRCASHD,R8                                                       
         CLI   TRCSTYPE,C'T'                                                    
         BNE   T28G                                                             
         MVC   0(6,R2),=C'HOURS='                                               
         EDIT  TRCSAMNT,(7,6(R2)),2,MINUS=YES,ALIGN=LEFT                        
         LA    R2,7(R2)                                                         
         AR    R2,R0                                                            
         LA    R4,7(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
T28G     ICM   R8,15,APER          PERSONELL ELEMENT                            
         BZ    T28H                                                             
*                                                                               
         BCTR  R2,0                MAKE SURE YOU HAVE A BLANK                   
         CLI   0(R2),C' '          BEFORE HOURS                                 
         LA    R2,1(R2)                                                         
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
*                                                                               
         USING ACPERSD,R8                                                       
         MVC   0(6,R2),=C'HOURS='                                               
         EDIT  ACPSHOUR,(9,6(R2)),2,ALIGN=LEFT,FLOAT=-                          
         LA    R2,7(R2)                                                         
         AR    R2,R0                                                            
         LA    R4,7(R4)                                                         
         AR    R4,R0                                                            
         TM    ACPSSTAT,ACPSBIL+ACPSNOT+ACPSRTE                                 
         BZ    T28H                                                             
*                                                                               
         MVC   0(5,R2),=C'TIME='                                                
         MVI   5(R2),C'B'                                                       
         TM    ACPSSTAT,ACPSBIL    IS IT BILLABLE ?                             
         BO    T28G1                                                            
*                                                                               
         MVI   5(R2),C'R'                                                       
         TM    ACPSSTAT,ACPSRTE    SPECIAL NOT BILLABLE ?                       
         BO    T28G1                                                            
*                                                                               
         MVI   5(R2),C'N'          ELSE, NOT BILLABLE                           
*                                                                               
T28G1    LA    R2,7(R2)                                                         
         LA    R4,7(R4)                                                         
*                                                                               
T28H     ICM   R8,15,AUNT          UNIT ELEMENT                                 
         BZ    T28I                                                             
*                                                                               
         BCTR  R2,0                MAKE SURE YOU HAVE A BLANK                   
         CLI   0(R2),C' '          BEFORE UNITS                                 
         LA    R2,1(R2)                                                         
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
*                                                                               
         USING UNPELD,R8                                                        
         MVC   0(6,R2),=C'UNITS='                                               
         EDIT  UNPUNIT,(9,6(R2)),0,ALIGN=LEFT,FLOAT=-                           
         LA    R2,7(R2)                                                         
         AR    R2,R0                                                            
         LA    R4,7(R4)                                                         
         AR    R4,R0                                                            
         LA    R2,7(R2)                                                         
         LA    R4,7(R4)                                                         
*                                                                               
         USING TRSELD,R8                                                        
T28I     ICM   R8,15,ATRS          TRANSACTION STATUS                           
         BZ    T29                                                              
         TM    TRSSTAT2,TRSSTADJ+TRSSTMSS+TRSSTIME                              
         BZ    T29                                                              
         MVC   0(6,R2),=C'TTYPE='                                               
         TM    TRSSTAT2,TRSSTADJ   ADJUSTED                                     
         BNO   *+8                                                              
         MVI   6(R2),C'A'                                                       
         TM    TRSSTAT2,TRSSTMSS   MISSING                                      
         BNO   *+8                                                              
         MVI   6(R2),C'M'                                                       
         TM    TRSSTAT2,TRSSTIME   REGULAR TIME                                 
         BNO   *+8                                                              
         MVI   6(R2),C'T'                                                       
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         DROP  R8                                                               
*                                                                               
T29      LTR   R4,R4                                                            
         BZ    T5                                                               
         GOTO1 VCHOPPER,DMCB,((R4),AIOB),(24,SCANBLCK),12                       
         ICM   R4,15,DMCB+8                                                     
         BZ    T5                                                               
         AR    R9,R4                                                            
         CH    R9,=H'20'                                                        
         BNL   TFULL                                                            
         TM    OPTIONS,DDSONLY+XDETAIL                                          
         BZ    T3                                                               
         CH    R9,=H'19'                                                        
         BNL   TFULL                                                            
T3       BCTR  R9,0                                                             
         LR    R8,R6                                                            
*                                                                               
TMP      USING LINED,R8                                                         
         LA    R3,SCANBLCK                                                      
T4       OI    TMP.LINEHDR+6,X'80'                                              
         MVC   TMP.LINEDATA+41(24),0(R3)                                        
         LA    R3,24(R3)                                                        
         LA    R8,86(R8)                                                        
         BCT   R4,T4                                                            
         DROP  TMP                                                              
*                                                                               
T5       MVC   LINEDATA+34(6),TRNSBTCH                                          
         LA    R8,LINEDATA+66                                                   
*                                                                               
T50      CLC   SAVEWC,=C'**'       UNMATCHED ORDER WORKCODE - SHOW              
         BNE   T52                 O/S EST AMT IN BRACKETS                      
         ICM   RF,15,AOAM                                                       
         BZ    T5A                                                              
         USING ACOAMTD,RF                                                       
         SR    RE,RE                                                            
T51      AP    TRNSAMNT,ACOAMT     EST                                          
         SP    TRNSAMNT,ACOAIVAL   MINUS INVOICED TO DATE                       
         IC    RE,ACOALEN                                                       
         AR    RF,RE                                                            
         CLI   0(RF),X'68'                                                      
         BE    T51                                                              
         SH    R8,=H'2'                                                         
         CP    TRNSAMNT,=P'0'                                                   
         BNL   T51A                                                             
         ZAP   TRNSAMNT,=P'0'                                                   
T51A     EDIT  TRNSAMNT,(12,0(R8)),2,MINUS=YES,BRACKET=YES                      
         B     T54                                                              
         DROP  RF                                                               
*                                                                               
T52      EDIT  TRNSAMNT,(10,0(R8)),2,MINUS=YES                                  
         MVC   LINEDATA+76(2),=C'CR'                                            
         CLC   SAVEWC,=C'99'                                                    
         BE    T53                                                              
         L     RE,AIO                                                           
         USING ACKEYD,RE                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    T52A                                                             
         MVI   LINEDATA+65,C'*'    BILL MARKER                                  
         B     T53                                                              
*                                                                               
T52A     BAS   RE,BILLSTAT                                                      
         CLI   WORK,0                                                           
         BE    T53                                                              
         MVI   LINEDATA+65,C'*'                                                 
         CLI   WORK,1                                                           
         BE    T53                                                              
         MVI   LINEDATA+65,C'P'                                                 
         DROP  RE                                                               
*                                                                               
T53      TM    TRNSSTAT,X'80'                                                   
         BO    *+14                                                             
         SP    WRKAMNT,TRNSAMNT                                                 
         B     T5A                                                              
         MVI   LINEDATA+76,C'D'                                                 
T54      AP    WRKAMNT,TRNSAMNT                                                 
*                                                                               
T5A      L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         MVC   LINEDATA(2),ACKEYWRK                                             
         LA    R3,13                                                            
         LA    R4,ACKEYCON+1                                                    
T6       CLI   0(R4),C' '                                                       
         BH    T7                                                               
         LA    R4,1(R4)                                                         
         BCT   R3,T6                                                            
T7       EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LINEDATA+3(0),0(R4)                                              
         GOTO1 VDATCON,DMCB,(1,ACKEYDTE),(8,LINEDATA+18)                        
*&&UK*&& OI    LINEDATA+18,X'F0'                                                
         MVC   LINEDATA+27(6),ACKEYREF                                          
*                                                                               
         OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         STH   R9,LINE                                                          
         TM    OPTIONS,DDSONLY+XDETAIL                                          
         BZ    TNEXT                                                            
         BAS   RE,DDSXTRAS                                                      
*                                                                               
TNEXT    LTR   RB,RB                                                            
         B     TXIT                                                             
         EJECT                                                                  
*              RUNLAST ACTIONS                                                  
*                                                                               
TEND     OC    SAVEWC,SAVEWC       HAVE FINAL WC TOTALS BEEN DISPLAYED          
         BZ    TEND0               IF NOT DISPLAY THEM                          
         GOTO1 =A(WORKTOTS),DMCB,(RC),RR=MYRELO                                 
         BM    TFULL                                                            
         XC    SAVEWC,SAVEWC                                                    
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         LH    R6,LINE                                                          
         MHI   R6,86                                                            
         LA    R6,INFDATAH(R6)                                                  
*                                                                               
TEND0    MVC   LINEDATA+3(13),=C'ACCOUNT TOTAL'                                 
         LA    RF,LINEDATA+64                                                   
         EDIT  TOTAMNT,(12,0(RF)),2,MINUS=YES                                   
         OI    LINEHDR+6,X'80'                                                  
*                                                                               
TEND1    SR    RF,RF               SET CC TO EQU FOR END                        
         B     TXIT                                                             
*                                                                               
TFULL    LNR   RB,RB               SET CC TO NEGATIVE FOR SCREEN FULL           
         MVI   LINE+1,2            AND SAVE SOME HEADS                          
TXIT     XIT1                                                                   
         EJECT                                                                  
*              DETERMINE IS TRANSACTION IF BILLED, UNBILLED OR PARTIAL          
*                                                                               
BILLSTAT NTR1                                                                   
         L     R7,ALOCAL                                                        
         MVI   WORK,0                                                           
         ZAP   DUB,=P'0'                                                        
         ICM   R5,15,ATRN                                                       
         BZ    BILLSX                                                           
         L     R5,AIO                                                           
*                                                                               
         GOTO1 PRORATA,DMCB,(X'80',(R5)),0,ACOMFACS,0,PROBLK,0                  
*                                                                               
         USING TRANSD,R5                                                        
         USING PRORATAD,RF                                                      
         LA    RF,PROBLK                                                        
         ICM   RE,15,APER                                                       
         BZ    BILLS02                                                          
*                                                                               
         TM    PRTSTAT-PRTELD(RE),PRTSBILQ  BILLABLE TIME?                      
         BZ    BILLS02                                                          
*                                                                               
         ICM   R5,15,ATRN                                                       
         CP    TRNSAMNT,=P'0'                                                   
         BNE   BILLS02                                                          
*                                                                               
         OC    PG$LBLDT,PG$LBLDT                                                
         BZ    BILLSX                                                           
*                                                                               
BILLS02  TM    PG$STAT,PG$FULLB                                                 
         BZ    *+12                                                             
         MVI   WORK,1                                                           
         B     BILLSX                                                           
*                                                                               
         TM    PG$STAT,PG$PARTB                                                 
         BZ    BILLSX                                                           
         MVI   WORK,2                                                           
*                                                                               
BILLSX   XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
*              SET UP DISPLAY LINE FOR DDS=YES, XDETAIL=YES OPTIONS             
*                                                                               
DDSXTRAS NTR1                                                                   
         LH    R6,LINE             R6 = A(LINE HEADER) - WE ALREADY             
         MH    R6,=H'86'           KNOW THERE'S ROOM FOR IT                     
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         MVI   UNSCANBK,C' '                                                    
         MVC   UNSCANBK+1(199),UNSCANBK                                         
         SR    R3,R3               R3 = COUNT OF UNSCAN FIELDS                  
         LA    R4,UNSCANBK         R4 = A(UNSCAN BLOCK)                         
         L     R5,ATRN                                                          
         USING TRANSD,R5                                                        
*                                                                               
DX1      CLI   TRNSTYPE,0          TYPE                                         
         BE    DX2                                                              
         MVC   0(2,R4),=C'TY'                                                   
         EDIT  TRNSTYPE,(4,10(R4)),ALIGN=LEFT,ZERO=BLANK                        
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
DX2      DS    0H                                                               
         TM    OPTIONS,DDSONLY     STATUS FOR DDS ONLY                          
         BZ    DX3                                                              
         MVC   0(2,R4),=C'ST'      STATUS IN HEX                                
         GOTO1 VHEXOUT,DMCB,TRNSSTAT,10(R4),1,=C'MIX'                           
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
DX3      ICM   R5,15,AOTH          OTHERS                                       
         BZ    DX6                                                              
         USING ACOTHERD,R5                                                      
         CLC   ACOTNUM(L'ACOTNUM+L'ACOTPROF),SPACES                             
         BE    DX6                                                              
         MVC   0(2,R4),=C'OT'                                                   
         LA    R8,10(R4)                                                        
         ZIC   R1,1(R5)            LENGTH OF ELEMENT                            
         SH    R1,=H'2'            LESS FIRST TWO BYTES                         
         LA    R5,2(R5)            BUMP TO BEGINNING OF DATA                    
         CLI   0(R5),C' '                                                       
         BE    DX4E                                                             
DX4A     XR    RF,RF               RF=LENGTH FOR EXECUTED MOVE                  
         LA    RE,1(R5)            RE=LOCATION IN ELEMENT                       
         BCTR  R1,0                                                             
DX4C     CLI   0(RE),C' '          SEARCH FOR END OF STRING                     
         BNH   *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,DX4C                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R5)                                                    
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         LR    R8,R4                                                            
         LTR   R1,R1                                                            
         BZ    DX6                                                              
         LR    R5,RE                                                            
DX4E     CLI   0(R5),C' '          SCAN FOR NEXT FIELD (IF ANY)                 
         BH    DX4A                                                             
         LA    R5,1(R5)                                                         
         BCT   R1,DX4E                                                          
*                                                                               
DX6      ICM   R5,15,ACSH          SUBSIDIARY CASH, ETC                         
         BZ    DX8                                                              
*                                                                               
         XR    R1,R1                                                            
DX6A     CLI   0(R5),X'50'                                                      
         BNE   DX8                                                              
*                                                                               
         USING TRCASHD,R5                                                       
         LA    RF,CASHTAB          SEE IF THIS TYPE GETS DISPLAYED              
         LA    R1,CASHTNUM                                                      
*                                                                               
DX6B     CLC   TRCSTYPE,0(RF)      IS THIS TYPE IN TABLE                        
         BE    DX6C                YES, PRINT FROM TABLE                        
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R1,DX6B                                                          
         B     DX6D                CASH TYPE NOT DISPLAYED                      
*                                                                               
DX6C     MVC   0(2,R4),=C'AM'      DISPLAY AS AM=NNN.NN,T(YPE)                  
         BAS   RE,DXEDCASH                                                      
         MVC   20(1,R4),TRCSTYPE                                                
         LA    R3,2(R3)                                                         
         LA    R4,2*L'UNSCANBK(R4)                                              
*                                                                               
DX6D     IC    R1,1(R5)            GET NEXT CASH EL                             
         LA    R5,0(R1,R5)                                                      
         B     DX6A                                                             
*                                                                               
*                                                                               
         USING TRSTATD,R5                                                       
DX8      ICM   R5,15,ATRS          ACTIVITY DATE                                
         BZ    DX9                                                              
         MVC   0(2,R4),=C'DA'                                                   
         LA    R2,TRSTDATE                                                      
         BAS   RE,DXDATCON                                                      
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
DX9      L     R5,AIO              USED DATE                                    
         USING ACKEYD,R5                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    DX10                                                             
         MVC   0(2,R4),=C'US'                                                   
         LA    R2,ACDTUSED                                                      
         BAS   RE,DXDATCON                                                      
         LA    R4,L'UNSCANBK(R4)                                                
         LA    R3,1(R3)                                                         
*                                                                               
DX10     OC    ACDTPEEL,ACDTPEEL   PEEL DATE                                    
         BZ    DX11                                                             
         MVC   0(2,R4),=C'PE'                                                   
         LA    R2,ACDTPEEL                                                      
         BAS   RE,DXDATCON                                                      
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
DX11     ICM   R5,15,APER          PERSONEL ELEMENT                             
         BZ    DX13                                                             
         USING ACPERSD,R5                                                       
         MVC   0(4,R4),=C'TIME'                                                 
         MVI   10(R4),C'B'                                                      
         TM    ACPSSTAT,ACPSBIL    IS IT BILLABLE ?                             
         BO    DX11A               YES                                          
         MVI   10(R4),C'R'         NO                                           
         TM    ACPSSTAT,ACPSRTE    IS IT SPECIAL RATE ?                         
         BO    DX11A               YES                                          
         MVI   10(R4),C'N'         NO, MUST BE NON-BILLABLE                     
*                                                                               
DX11A    LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
         USING TRSELD,R5                                                        
DX13     ICM   R5,15,ATRS          TRANSACTION STATUS                           
         BZ    DX15                                                             
         TM    TRSSTAT2,TRSSTADJ+TRSSTMSS+TRSSTIME                              
         BZ    DX15                                                             
         MVC   0(5,R4),=C'TTYPE'                                                
         TM    TRSSTAT2,TRSSTADJ   ADJUSTED                                     
         BNO   *+8                                                              
         MVI   10(R4),C'A'                                                      
         TM    TRSSTAT2,TRSSTMSS   MISSING                                      
         BNO   *+8                                                              
         MVI   10(R4),C'M'                                                      
         TM    TRSSTAT2,TRSSTIME   REGULAR TIME                                 
         BNO   *+8                                                              
         MVI   10(R4),C'T'                                                      
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
DX15     LTR   R3,R3               ANYTHING TO UNSCAN                           
         BZ    DXIT                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 VUNSCAN,DMCB,((R3),UNSCANBK),(C'C',WORK)                         
         OI    LINEHDR+6,X'80'                                                  
         MVI   LINEDATA,C'('       PARENTHESES ROUND IT                         
         MVC   LINEDATA+1(77),WORK                                              
         LA    R4,LINEDATA+76                                                   
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         LH    R1,LINE             BUMP LINE NUMBER                             
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
DXIT     XIT1                                                                   
*                                                                               
DXDATCON EQU   *                                                                
         ST    RE,FULL                                                          
         GOTO1 VDATCON,DMCB,(2,(R2)),(X'20',10(R4))                             
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         USING TRCASHD,R5                                                       
DXEDCASH EQU   *                                                                
         EDIT  (P6,TRCSAMNT),(10,10(R4)),2,MINUS=YES,ALIGN=LEFT                 
         BR    RE                                                               
         EJECT                                                                  
VALCOUNT DS    0H                  MAX NUMBER OF VALUES                         
*&&UK*&& DC    H'6'                                                             
*&&US*&& DC    H'3'                                                             
*                                                                               
*        TABLE OF CASH ELEMENT TYPES WHICH PRINT WHEN DD=Y                      
CASHTAB  DS    0CL1                                                             
         DC    CL1'D',CL1'S'                                                    
CASHTNUM EQU   *-CASHTAB                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              LOCAL WS AND EQUATES                                             
*                                                                               
PRESETS  DS    0H                  PRESET KEYWORDS                              
         DC    CL10'COMMISSION'                                                 
         DC    CL10'CASH DSCNT'                                                 
         DC    CL10'PAYABLE'                                                    
         SPACE 3                                                                
HEADING  DC    CL39'WC CONTRA         DATE     REF    BATCH'                    
         DC    CL39'  DESCRIPTION             B   AMOUNT'                       
HEADING2 DC    CL39'-- ------         ----     ---    -----'                    
         DC    CL39'  -----------             -   ------'                       
CTIME    DC    CL(L'FILFULKW)'TIME'                                             
CTTYPE   DC    CL(L'FILFULKW)'TTYPE'                                            
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
*                                                                               
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    XL2'FF00'                                                        
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODUNIT-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODLEDG-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(27)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
*                                                                               
FILTABLE DC    CL10'AMOUNT'                                                     
         DC    CL2'AM'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSAMNT-TRANSD)                                             
         DC    AL1(10)                                                          
         DC    AL2(EDITCASH-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'BATCH'                                                      
         DC    CL2'BA'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(L'TRNSBTCH)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'BILLED'                                                     
         DC    CL2'BI'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'01'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACDTUSED-ACKEYD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'             OMIT WC99                                    
*                                                                               
         DC    CL10'CREDIT'                                                     
         DC    CL2'CR'                                                          
         DC    X'02'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DEBIT'                                                      
         DC    CL2'DR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DDS'                                                        
         DC    CL2'DD'                                                          
         DC    X'80'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'48'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'HELD'                                                       
         DC    CL2'HE'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'04'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MOS'                                                        
         DC    CL2'MO'                                                          
         DC    X'20'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ORDER'                                                      
         DC    CL2'OR'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AXNO-GWS)                                                    
         DC    AL1(ACNO-ACNOD)                                                  
         DC    AL1(6)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'PEELED'                                                     
         DC    CL2'PE'                                                          
         DC    X'80'                                                            
         DC    CL8'YES'                                                         
         DC    X'20'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'REFERENCE'                                                  
         DC    CL2'RE'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYREF-ACKEYD)                                             
         DC    AL1(L'ACKEYREF)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'REVERSALS'                                                  
         DC    CL2'RV'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'20'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'50'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSTYPE-TRANSD)                                             
         DC    AL1(3)                                                           
         DC    AL2(EDITYPE-GWS)                                                 
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'URGENT'                                                     
         DC    CL2'UR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'40'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WC'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYWRK-ACKEYD)                                             
         DC    AL1(L'ACKEYWRK)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'XDETAIL'                                                    
         DC    CL2'XD'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'04'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'AUTHORISE'                                                  
         DC    CL2'AU'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'08'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'TIME'                                                       
         DC    CL2'TI'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    XL2'FF00'                                                        
         DC    XL2'FF00'                                                        
*                                                                               
         DC    CL10'TTYPE'                                                      
         DC    CL2'TT'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    XL2'FF00'                                                        
         DC    XL2'FF00'                                                        
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
         EJECT                                                                  
*              SET UP SPECIAL NARRATIVE DISPLAY FOR WORKCODE 99                 
*              ON ENTRY CALLTYPE = DETAIL OR WORKCODE                           
*                       KEYWORDS = SET OF KEYWORDS FOR VALUES                   
*                       VALCOUNT = MAX NUMBER OF VALUES                         
*                       R6       = A(LINE HEADER) COVERED BY LINED              
*              ON EXIT  LINE     = NEW LINE NUMBER MINUS ONE                    
*                       CC       = NEGATIVE FOR NOT ENOUGH ROOM                 
*                                                                               
DISPWC99 NMOD1 0,*WC99*            BUILD SET OF KEYWORD=VALUE IN BUFFER         
         L     RC,0(R1)                                                         
         LA    R2,KEYWORDS                                                      
         L     RF,=A(VALCOUNT)                                                  
         A     RF,MYRELO                                                        
         LH    R3,0(RF)                                                         
         LA    R4,SCANBLCK                                                      
         SR    R9,R9               R9 = NUMBER IN SET                           
*                                                                               
         CLI   CALLTYPE,DETAIL     DETAIL HAS BILL TYPE AS FIRST IN SET         
         BNE   DW1                                                              
         ICM   R5,15,ATRN                                                       
         USING TRANSD,R5                                                        
         MVC   0(22,R4),SPACES                                                  
         MVC   0(5,R4),=C'TYPE='                                                
         MVC   6(15,R4),TRNSNARR                                                
*                                                                               
         CLI   1(R5),X'39'         IS THIS A SHORT BILL                         
         BNH   DWXIT               YES, THERE ARE NO PACKED BUCKETS             
*                                                                               
         LA    R4,22(R4)                                                        
         LA    R9,1                                                             
         LA    R8,TRNSNARR+15                                                   
*&&UK*&& MVC   TRNSNARR+21(5*6),TRNSNARR+63  SHIFT VAT FORWARD                  
         B     DW3                                                              
*                                                                               
DW1      CLI   CALLTYPE,WORKCODE   WORKCODE TOTALS                              
         BNE   DWXIT                                                            
         LA    R8,WRKCOMM                                                       
*                                                                               
DW3      CP    0(6,R8),=P'0'                                                    
         BE    DW4                                                              
         MVC   0(10,R4),0(R2)                                                   
         MVI   10(R4),C'='                                                      
         MVC   11(11,R4),SPACES                                                 
         BAS   RE,DWEDIT                                                        
         LA    R4,22(R4)                                                        
         LA    R9,1(R9)                                                         
DW4      LA    R2,10(R2)                                                        
         LA    R8,6(R8)                                                         
         BCT   R3,DW3                                                           
*                                                                               
         CLI   CALLTYPE,WORKCODE   IF TOTALS, DO TAX TOTALS                     
         BNE   DW4A                                                             
         GOTO1 =A(TXXDISP),DMCB,(RC),RR=MYRELO                                  
         B     DW5                                                              
*                                                                               
         USING VBIELD,R5                                                        
DW4A     SR    R2,R2               CHECK FOR VAT ON THIS BILL                   
DW4C     IC    R2,1(R5)                                                         
         AR    R5,R2                                                            
         CLI   0(R5),0             END OF RECORD                                
         BE    DW5                 YES                                          
*                                                                               
         CLI   0(R5),VBIELQ        IS THIS A VBI ELEMENT                        
         BE    DW4D                YES                                          
*                                                                               
         CLI   0(R5),PBIELQ        OR A PBI ELEMENT                             
         BNE   DW4C                NO                                           
*                                                                               
DW4D     GOTO1 =A(TXXDET),DMCB,(RC),RR=MYRELO                                   
         LA    R4,22(R4)                                                        
         LA    R9,1(R9)                                                         
         B     DW4C                                                             
*                                                                               
DW5      LH    R2,LINE             HAVE WE ROOM FOR THE SET                     
         LTR   R9,R9                                                            
         BZ    DWXIT                                                            
         AR    R2,R9                                                            
         CH    R2,=H'20'                                                        
         BNL   DWFULL                                                           
         CLI   CALLTYPE,DETAIL                                                  
         BNE   DW6                                                              
         TM    OPTIONS,DDSONLY+XDETAIL                                          
         BZ    DW6                                                              
         CH    R2,=H'19'                                                        
         BNL   DWFULL                                                           
         B     DW6                                                              
*                                                                               
DW6      BCTR  R2,0                INCREMENT LINE  BY NO IN SET MINUS 1         
         STH   R2,LINE                                                          
         LA    R4,SCANBLCK         SET UP DISPLAY ON SCREEN                     
DW7      MVC   LINEDATA+41(22),0(R4)                                            
         OI    LINEHDR+6,X'80'                                                  
         LA    R4,22(R4)                                                        
         LA    R6,86(R6)                                                        
         BCT   R9,DW7                                                           
*                                                                               
DW8      CLI   CALLTYPE,DETAIL     IF DETAIL UPDATE WC TOTALS                   
         BNE   DWXIT                                                            
         ICM   R5,15,ATRN                                                       
         USING TRANSD,R5                                                        
         L     RF,=A(VALCOUNT)                                                  
         A     RF,MYRELO                                                        
         LH    R3,0(RF)                                                         
         LA    R6,WRKCOMM                                                       
         LA    R8,TRNSNARR+15                                                   
DW9      AP    0(6,R6),0(6,R8)                                                  
         LA    R6,6(R6)                                                         
         LA    R8,6(R8)                                                         
         BCT   R3,DW9                                                           
         GOTO1 =A(TXXPUT),DMCB,(RC),RR=MYRELO                                   
         B     DWXIT                                                            
*                                                                               
DWFULL   LNR   RB,RB                                                            
DWXIT    LTR   RB,RB                                                            
         XIT1                                                                   
DWEDIT   EQU   *                                                                
         ST    RE,FULL                                                          
         EDIT  (P6,0(R8)),(11,11(R4)),2,MINUS=YES,ALIGN=LEFT                    
         L     RE,FULL                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*        PUT A TAX CODE FOUND ON A WC 99 OUT TO THE TABLE                       
*                                                                               
TXXPUT   NMOD1 0,*TXXPUT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING VBIELD,R5                                                        
         SR    R3,R3               CHECK FOR VAT ON THIS BILL                   
TXXP05   IC    R3,1(R5)                                                         
         AR    R5,R3                                                            
         CLI   0(R5),0             END OF RECORD                                
         BE    TXXPX               YES                                          
*                                                                               
         CLI   0(R5),VBIELQ        IS THIS A VBI ELEMENT                        
         BNE   TXXP10              NO                                           
         USING VBIELD,R5                                                        
         MVI   FULL,TXXTGST        BUILD KEY FOR TXXTAB LOOK UP                 
         MVC   FULL+1(1),VBITYPE                                                
         MVC   FULL+2(2),VBIRATE                                                
         XC    HALF,HALF           NO PROVINCE IN VBI ELEMENT                   
         BAS   RE,TXXLOOK          IS THIS TYPE/CODE/RATE IN TABLE              
         BE    TXXP07              YES, ACCUMULATE AMOUNT                       
         BAS   RE,TXXBUMP          BUMP NUMBER IN TABLE                         
*                                                                               
         USING TXXTABD,R2          R2 SET BY TXXLOOK                            
         MVI   TXXTYPE,TXXTGST                                                  
         MVC   TXXCODE,VBITYPE                                                  
         XC    TXXPRV,TXXPRV                                                    
         MVC   TXXRATE,VBIRATE                                                  
TXXP07   AP    TXXAMNT,VBIVAT                                                   
         B     TXXP05                                                           
*                                                                               
TXXP10   CLI   0(R5),PBIELQ        IS THIS A PBI ELEMENT                        
         BNE   TXXP05              N, GET NEXT EL                               
*                                                                               
         USING PBIELD,R5                                                        
         MVI   FULL,TXXTPST        BUILD KEY FOR TXXTAB LOOK UP                 
         MVC   FULL+1(1),PBITYPE                                                
         MVC   FULL+2(2),PBIRATE                                                
         MVC   HALF,PBIPRV         PASS PROVINCE IN HALF                        
         BAS   RE,TXXLOOK          IS THIS TYPE/CODE/RATE IN TABLE              
         BE    TXXP20              YES, ACCUMULATE AMOUNT                       
         BAS   RE,TXXBUMP          BUMP NUMBER IN TABLE                         
*                                  R2 SET BY TXXLOOK                            
         MVI   TXXTYPE,TXXTPST                                                  
         MVC   TXXCODE,PBITYPE                                                  
         MVC   TXXRATE,PBIRATE                                                  
         XC    TXXPRV,PBIPRV                                                    
*                                                                               
TXXP20   AP    TXXAMNT,PBIPST                                                   
         B     TXXP05                                                           
*                                                                               
TXXPX    XIT1                                                                   
         DROP  R2                                                               
*----------------------------------------------------------------------         
*        LOOK UP A TAX TYPE, CODE AND RATE BUILT IN FULL                        
*        AND THE PROVINCE IN HALF                                               
*        RETURNS EQ IF FOUND, R2 POINTS TO A(FOUND TABLE ENTRY) OR              
*        A(NEXT FREE TABLE ENTRY) OR A(EOT) IF AT MAX AND NOT FOUND             
*----------------------------------------------------------------------         
*                                                                               
         USING TXXTABD,R2                                                       
TXXLOOK  LA    R2,TXXTAB                                                        
         ZIC   R1,TXXNUM                                                        
         LTR   R1,R1               TABLE EMPTY                                  
         BZ    TXXLNO              YES, RETURN NOT EQUAL                        
*                                                                               
TXXL10   CLC   TXXTYPE,FULL        CODE IN TABLE                                
         BNE   TXXL30              NO, GET NEXT                                 
*                                                                               
         CLC   TXXCODE,FULL+1      CODE IN TABLE                                
         BNE   TXXL30              NO, GET NEXT                                 
*                                                                               
         CLC   TXXPRV,HALF         PROVINCE OR BINARY ZEROS                     
         BNE   TXXL30              NO, GET NEXT                                 
*                                                                               
         CLC   TXXRATE,FULL+2      SAME RATE?                                   
         BE    TXXLYES             YES, RETURN EQUAL AND R2                     
*                                                                               
TXXL30   LA    R2,TXXTABLN(R2)                                                  
         BCT   R1,TXXL10                                                        
*                                                                               
TXXLNO   CR    R1,RE                                                            
         BR    RE                                                               
*                                                                               
TXXLYES  CR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
TXXBUMP  XR    R1,R1               BUMP TXX NUM, DIE IF GT MAX                  
         IC    R1,TXXNUM           ADD CODE TO TABLE                            
         LA    R0,TXXMAX                                                        
         CR    R0,R1                                                            
         BH    *+6                                                              
         DC    H'0'                TOO MANY TXX CODES ON THIS JOB               
*                                                                               
         LA    R1,1(R1)                                                         
         STC   R1,TXXNUM                                                        
         BR    RE                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*        DISPLAY TAX TOTALS FOR THIS JOB AT 0(R4)                               
*        R9 IS LINE COUNTER, AND IS PASSED BACK TO CALLER                       
*                                                                               
TXXDISP  NMOD1 0,*TXXDISP                                                       
         L     RC,0(R1)                                                         
         USING TXXTABD,R2                                                       
         LA    R2,TXXTAB                                                        
         ZIC   R5,TXXNUM                                                        
         LTR   R5,R5                                                            
         BZ    TXXDX                                                            
         ZAP   WRKCOMM,=P'0'       THIS HAS BEEN ALREADY PRINTED                
*                                                                               
TXXD20   MVC   0(22,R4),SPACES                                                  
         MVC   0(4,R4),=C'GST '                                                 
         MVC   4(1,R4),TXXCODE                                                  
         CLI   TXXTYPE,TXXTPST                                                  
         BNE   TXXD30                                                           
         MVC   0(2,R4),TXXPRV      OUTPUT PROVINCE/PST CODE                     
         MVI   2(R4),C'/'                                                       
         MVC   3(1,R4),TXXCODE                                                  
         MVI   4(R4),C' '                                                       
*                                                                               
TXXD30   MVI   5(R4),C'='                                                       
         LA    R3,TXXRATE                                                       
         EDIT  (2,0(R3)),(5,6(R4)),2,FILL=0                                     
         MVI   11(R4),C'/'                                                      
         LA    R8,TXXAMNT                                                       
         EDIT  (P6,0(R8)),(10,12(R4)),2,MINUS=YES,ALIGN=LEFT                    
*                                                                               
         AP    WRKCOMM(6),TXXAMNT                                               
         LA    R4,22(R4)                                                        
         LA    R9,1(R9)                                                         
         LA    R2,TXXTABLN(R2)                                                  
         BCT   R5,TXXD20                                                        
*                                                                               
         MVC   0(11,R4),=CL11'TOTAL TAX'                                        
         MVI   10(R4),C'='                                                      
         MVC   11(11,R4),SPACES                                                 
         EDIT  (P6,WRKCOMM),(11,11(R4)),2,MINUS=YES,ALIGN=LEFT                  
         LA    R9,1(R9)                                                         
TXXDX    XIT1  REGS=(R9)                                                        
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*    OUTPUT THE DATA FROM EITHER A VBI OR PBI ELEMENT IN 0(R5)                  
*----------------------------------------------------------------------         
TXXDET   NMOD1 0,*TXXDET                                                        
         L     RC,0(R1)                                                         
         MVC   11(22,R4),SPACES                                                 
         CLI   0(R5),PBIELQ                                                     
         BE    TXXD50                                                           
         USING VBIELD,R5                                                        
         MVC   0(4,R4),=C'GST '                                                 
         MVC   4(1,R4),VBITYPE                                                  
         LA    R3,VBIRATE                                                       
         LA    R2,VBIVAT                                                        
         LA    R4,5(R4)                                                         
         BAS   RE,TXXDEDIT                                                      
         B     TXXDETX                                                          
*                                                                               
         USING PBIELD,R5                                                        
TXXD50   EQU   *                                                                
         MVC   0(2,R4),PBIPRV                                                   
         MVI   2(R4),C'/'                                                       
         MVC   3(1,R4),PBITYPE                                                  
         LA    R4,5(R4)                                                         
         LA    R3,PBIRATE                                                       
         LA    R2,PBIPST                                                        
         BAS   RE,TXXDEDIT                                                      
*                                                                               
TXXDETX XIT1                                                                    
*                                                                               
TXXDEDIT MVI   0(R4),C'='                                                       
         EDIT  (2,0(R3)),(5,1(R4)),2,FILL=0                                     
         MVI   6(R4),C'/'                                                       
         EDIT  (P6,0(R2)),(10,7(R4)),2,MINUS=YES,ALIGN=LEFT                     
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DISPLAY WORKCODE TOTALS AND UPDATE ACCOUNT TOTALS                
*              ON ENTRY SAVEWC   = WORKCODE                                     
*                       R6       = A(LINE HEADER)                               
*              ON EXIT  LINE     = UPDATED LINE NUMBER                          
*                       CC       = NEGATIVE FOR NOT ENOUGH ROOM                 
*                                                                               
WORKTOTS NMOD1 0,**WORK**                                                       
         L     RC,0(R1)                                                         
         CLC   SAVEWC,=C'99'                                                    
         BNE   WT1                                                              
         MVI   CALLTYPE,WORKCODE                                                
         GOTO1 =A(DISPWC99),DMCB,(RC),RR=MYRELO                                 
         BM    WTXIT                                                            
WT1      LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         MVC   LINEDATA(2),SAVEWC                                               
         MVC   LINEDATA+3(14),=C'WORKCODE TOTAL'                                
         LA    RF,LINEDATA+64                                                   
WT2      CLC   SAVEWC,=C'**'       UNMATCHED ORDERS IN BRACKETS AND NOT         
         BNE   WT3                 INCLUDED IN ACCOUNT TOTAL                    
         SH    RF,=H'2'                                                         
         EDIT  WRKAMNT,(14,0(RF)),2,MINUS=YES,BRACKET=YES                       
         B     WT4                                                              
WT3      EDIT  WRKAMNT,(12,0(RF)),2,MINUS=YES                                   
         AP    TOTAMNT,WRKAMNT                                                  
WT4      OI    LINEHDR+6,X'80'                                                  
         ZAP   WRKAMNT,=P'0'                                                    
         LTR   RB,RB                                                            
WTXIT    XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
*        INIT TOTAL BUCKEST, STATUS BYTES, ETC                                  
*----------------------------------------------------------------------         
INIT     NMOD1 0,**INIT**                                                       
         L     RC,0(R1)                                                         
         L     R1,4(R1)                                                         
         LH    R1,0(R1)                                                         
         LA    R1,2(R1)                                                         
         LA    R2,TOTALS                                                        
         ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         XC    TXXNUM,TXXNUM       INIT TABLE OF TAX CODES ON THIS JOB          
         LA    R1,TXXMAX                                                        
         USING TXXTABD,R2                                                       
         LA    R2,TXXTAB                                                        
*                                                                               
IB10     XC    TXXKEY(TXXKLN),TXXKEY                                            
         ZAP   TXXAMNT,=P'0'                                                    
         LA    R2,TXXTABLN(R2)                                                  
         BCT   R1,IB10                                                          
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
LOCALD   DSECT                                                                  
MYRELO   DS    A                                                                
CALLTYPE DS    C                                                                
KEYWORDS DS    CL80                                                             
*                                                                               
PEELED   EQU   X'20'               PEELED=YES OPTION IN USE                     
XDETAIL  EQU   X'04'               XDETAIL=YES OPTION IN USE                    
MOSRANGE EQU   X'02'                                                            
DETAIL   EQU   C'D'                                                             
WORKCODE EQU   C'W'                                                             
PROBLK   DS    CL(PR$LNQ)          PRORATA BLOCK                                
         EJECT                                                                  
*        DSECT TO COVER TABLE OF TAX TOTALS  ON WC 99                           
*                                                                               
TXXTABD  DSECT                                                                  
TXXKEY   DS    0C                                                               
TXXTYPE  DS    CL1                                                              
TXXTGST  EQU   C'G'                                                             
TXXTPST  EQU   C'P'                                                             
TXXCODE  DS    CL1                                                              
TXXPRV   DS    CL2                 PROVINCE (PST ONLY)                          
TXXRATE  DS    XL2                                                              
TXXKLN   EQU   *-TXXTABD                                                        
TXXAMNT  DS    PL6                                                              
TXXTABLN EQU   *-TXXTABD                                                        
         EJECT                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
SAVEWC   DS    CL2       C         SAVED WORKCODE                               
TOTALS   DS    0C                  WORKCODE AND ACCOUNT TOTALS                  
TOTAMNT  DS    PL6       P         TOTAL TRNSAMNT FOR ACCOUNT                   
WRKAMNT  DS    PL6       P         TOTAL TRNSAMNT FOR WORKCODE                  
WRKCOMM  DS    8PL6      P         WORKCODE 99 TOTALS FOR WORKCODE              
OPTN     DS    XL1                 LOCAL OPTION                                 
TIMEB    EQU   X'40'                                                            
TIMER    EQU   X'20'                                                            
TIMEN    EQU   X'10'                                                            
TTYPET   EQU   X'08'                                                            
TTYPEM   EQU   X'04'                                                            
TTYPEA   EQU   X'02'                                                            
TXXNUM   DS    CL1                 NUMBER OF GST CODES USED IN THIS JOB         
TXXTAB   DS    (TXXMAX*TXXTABLN)C  TABLE OF GST CODES/AMOUNTS                   
TXXMAX   EQU   20                                                               
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062ACINQ06   05/01/02'                                      
         END                                                                    
