*          DATA SET ACINQ04    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T60604A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - BUDGET - T60604'                          
T60604   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'BUDGET' IN ACCOUNT ENQUIRY PROGRAM              
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60604)                                               
         DC    A(FILTABLE-T60604)                                               
         DC    A(KNTRYPNT-T60604)                                               
         DC    A(FNTRYPNT-T60604)                                               
         DC    A(DNTRYPNT-T60604)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(17)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ABUD-GWS)                                                    
         DC    AL1(ACBDSTRT-ACBUDGD)                                            
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ABUD-GWS)                                                    
         DC    AL1(ACBDEND-ACBUDGD)                                             
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    X'04'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ABUD-GWS)                                                    
         DC    AL1(ACBDTYPE-ACBUDGD)                                            
         DC    AL1(L'ACBDTYPE)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              HANDLE CONTRA-ACCOUNT FILTER                                     
*              (ROUTINE CALLED BY FILTER ROUTINE IN ROOT)                       
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ4**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60604,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         LA    R2,WORK                                                          
         ICM   R9,15,ABUD                                                       
         USING ACBUDGD,R9                                                       
         BCTR  R3,0                R3=LENGTH MINUS ONE                          
         EX    R3,VCONTMV1         EQUATE COMPARANDS                            
         SPACE 1                                                                
         CLC   CONFILT,SPACES      HAVE WE GOT A FIX ON THE FILTER              
         BE    VCONT1              IF SO USE IT                                 
         L     R4,CONFLEN                                                       
         EX    R4,VCONTCL0                                                      
         BNE   VCONTNO                                                          
         SPACE 1                                                                
VCONT1   LA    R4,1                OTHERWISE TRY VARIOUS START POINTS           
         EX    R3,VCONTCL1                                                      
         BE    VCONT4                                                           
         LA    R4,2                                                             
         EX    R3,VCONTCL2                                                      
         BE    VCONT4                                                           
         LA    R4,3                                                             
         EX    R3,VCONTCL3                                                      
         BNE   VCONTNO                                                          
         SPACE 1                                                                
VCONT4   AR    R4,R3               THE FILTER FITS                              
         EX    R4,VCONTMV2         SAVE ITS FULL VALUE AND LENGTH -1            
         ST    R4,CONFLEN          FOR FUTURE USE                               
         B     VCONTX                                                           
         SPACE 1                                                                
VCONTNO  MVI   WORK,0              IT DOESNT FIT SO UNEQUATE COMPARANDS         
         SPACE 1                                                                
VCONTX   LA    R3,1(R3)                                                         
         LA    R5,INFCAC                                                        
         XIT1  REGS=(R2,R5)                                                     
         SPACE 1                                                                
VCONTMV1 MVC   WORK(0),INFCAC      EXECUTED INSTRUCTIONS                        
VCONTMV2 MVC   CONFILT(0),ACBDSBAC                                              
VCONTCL0 CLC   CONFILT(0),ACBDSBAC                                              
VCONTCL1 CLC   INFCAC(0),ACBDSBAC+1                                             
VCONTCL2 CLC   INFCAC(0),ACBDSBAC+2                                             
VCONTCL3 CLC   INFCAC(0),ACBDSBAC+3                                             
         DROP  R9                                                               
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ4                                                         
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60604,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         L     R7,ATIA             R7 = A(TABLE ENTRY)                          
         MVC   TABWIDTH(8),0(R7)   TABLE ENTRY WIDTH & MAX NO ENTRIES           
         LA    R7,8(R7)                                                         
         USING TABLED,R7                                                        
         L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         SPACE 1                                                                
         CLI   LASTKMK,0           CONTINUATION OR PREVIOUS SCREEN              
         BNE   TDISP                                                            
         SPACE 1                                                                
TFIRST   CLI   VIRGIN,C'2'         FIRST REC - INITIALISE TABLE & COUNT         
         BNE   TFIRST0                                                          
         CLI   ACKEYACC,RUNLAST                                                 
         BNE   TUPDATE2                                                         
         CLI   TABLE,X'FF'                                                      
         BE    TEND                                                             
         B     TDISP                                                            
TFIRST0  DS    0H                                                               
         MVI   TABLE,X'FF'                                                      
         MVC   RECNUM,=F'1'                                                     
         XC    COUNT,COUNT                                                      
         ZAP   ACTUAL,=P'0'                                                     
         MVC   CONNAME,SPACES                                                   
         MVI   VIRGIN,C'2'                                                      
         MVC   SAVEKEY,ACKEYD                                                   
         MVC   CONFILT,SPACES                                                   
         ST    R7,ATABLE                                                        
         MVI   TABTYPE,0           SET INITIAL TABLE WIDTH = TYPE 0             
         MVC   TABWIDTH(8),TABPROFS                                             
         B     TUPDATE                                                          
         EJECT                                                                  
*              UPDATE TABLE OF CONTRA/START/END/TYPE ENTRIES FROM               
*              BUDGET ELEMENTS                                                  
         SPACE 1                                                                
TUPDATE  ICM   R9,15,ABUD                                                       
         BZ    TEND                                                             
         USING ACBUDGD,R9                                                       
         SR    R3,R3                                                            
         B     T2                                                               
         SPACE 1                                                                
T1       IC    R3,1(R9)            BUMP TO NEXT BUDGET ELEMENT                  
         AR    R9,R3                                                            
         CLI   0(R9),0                                                          
         BE    T3                                                               
         CLI   0(R9),X'34'                                                      
         BNE   T1                                                               
         ST    R9,ABUD             SAVE ITS ADDRESS FOR FILTER                  
         SPACE 1                                                                
T2       MVI   DMCB,0                                                           
         L     RF,AFILTER                                                       
         BASR  RE,RF                                                            
         BZ    T1                                                               
         MVC   CONCODE,ACBDSBAC                                                 
         MVC   START(4),ACBDSTRT                                                
         MVC   TYPE,ACBDTYPE                                                    
         MVC   BUDGET,ACBDBUDG                                                  
         MVI   CALLTYPE,ADD                                                     
         BAS   RE,FINDIT                                                        
         B     T1                                                               
         SPACE 1                                                                
T3       L     R7,ATABLE           AT END OF RECORD CHECK FOR ANY ENTRY         
         CLI   0(R7),X'FF'                                                      
         BE    TEND                IF NOT NO DISPLAY                            
         B     TNEXT                                                            
         EJECT                                                                  
*              UPDATE TABLE WITH ACTUALS AND CONTRA NAMES FROM CONTRA           
*              HEADERS                                                          
         SPACE 1                                                                
TUPDATE2 DS    0H                                                               
         USING ACKEYD,R9                                                        
         MVC   CONCODE,ACKEYCON                                                 
         CLC   SAVEKEY+1(2),=C'1C' CONTRA SUBSTITUTION 13N FOR 1PXXXN           
         BNE   TUP21                                                            
         CLC   ACKEYCON+1(2),=C'1P'                                             
         BNE   TUP21                                                            
         LA    R1,ACKEYCON+14                                                   
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVI   CONCODE+2,C'3'                                                   
         MVC   CONCODE+3(1),0(R1)                                               
         MVC   CONCODE+4(11),SPACES                                             
TUP21    DS    0H                                                               
         CLC   SAVECACN,SPACES     SAVE NAME IF WE HAVENT GOT IT AND            
         BNE   TUPDATE3            FILTER FITS IN FULL                          
         CLC   CONCODE,CONFILT                                                  
         BNE   TUPDATE3                                                         
         BAS   RE,NAMTOWRK                                                      
         MVC   SAVECACN,WORK                                                    
         SPACE 1                                                                
TUPDATE3 MVC   CONNAME,SPACES      LOOK FOR FIRST ENTRY FOR THIS CONTRA         
         MVI   CALLTYPE,FIND                                                    
         BAS   RE,FINDIT                                                        
         TM    DMCB,1                                                           
         BO    TUPDATE4            THERE ARENT ANY                              
         CLI   TABTYPE,0           ARE WE HOLDING NAMES IN THE TABLE            
         BNE   *+14                                                             
         BAS   RE,NAMTOWRK                                                      
         MVC   CONNAME,WORK                                                     
         BAS   RE,ADACTUAL         UPDATE ALL ENTRIES WITH THIS CONCODE         
         SPACE 1                                                                
TUPDATE4 L     R7,ATABLE           UPDATE ACCOUNT LEVEL ENTRIES IF ANY          
         CLC   TABCON,SPACES                                                    
         BNE   TNEXT                                                            
         MVC   CONCODE(15),SPACES                                               
         MVC   CONNAME(36),SPACES                                               
         BAS   RE,ADACTUAL                                                      
         B     TNEXT                                                            
         EJECT                                                                  
*              UPDATE ALL TABLE ENTRIES FOR A GIVEN CONTRA WITH NAME            
*              AND ACTUALS (DEBITS OR CREDITS) FROM A GIVEN HEADER REC.         
*              ON ENTRY CONCODE  = CONTRA-ACCOUNT CODE OR SPACES IF A/C         
*                       CONNAME  = CONTRA NAME OR SPACES                        
*                       R7       = A(1ST TABLE ENTRY FOR CONTRA) COVERD         
*                                  BY TABLED                                    
*                       AHST     = A(1ST HISTORY ELEMENT IN HEADER REC)         
         SPACE 1                                                                
ADACTUAL NTR1                                                                   
         SR    R3,R3                                                            
ADACT1   ICM   R9,15,AHST                                                       
         BZ    ADACTX              NO HISTORY ELEMENTS                          
         USING TRHISTD,R9                                                       
         CLI   TABTYPE,0           ARE NAMES HELD IN THE TABLE                  
         BNE   *+10                                                             
         MVC   TABNAME,CONNAME     IF SO BUNG IT IN                             
         SPACE 1                                                                
ADACT2   CLC   TABSTART,TRHSYEAR   MATCH ON DATE AND UPDATE                     
         BH    ADACT4                                                           
         CLC   TABEND,TRHSYEAR                                                  
         BL    ADACT5                                                           
         LA    R4,TRHSDR                                                        
         CLI   TABBTYPE,C'D'       DEBITS                                       
         BE    ADACT3              OR                                           
         CLI   TABBTYPE,C'C'       CREDITS                                      
         BNE   ADACT4                                                           
         LA    R4,TRHSCR                                                        
ADACT3   AP    TABACT,0(6,R4)                                                   
         SPACE 1                                                                
ADACT4   IC    R3,1(R9)            BUMP TO NEXT HISTORY EL                      
         AR    R9,R3                                                            
         CLI   0(R9),0                                                          
         BE    ADACT5                                                           
         CLI   0(R9),X'45'                                                      
         BNE   ADACT4                                                           
         B     ADACT2                                                           
         SPACE 1                                                                
ADACT5   A     R7,TABWIDTH         BUMP TO NEXT TAB ENTRY                       
         CLC   CONCODE,TABCON      FOR SAME CONTRA                              
         BE    ADACT1                                                           
ADACTX   XIT1                                                                   
         EJECT                                                                  
*              MOVE NAME INTO WORK FROM HEADER                                  
         SPACE 1                                                                
NAMTOWRK NTR1                                                                   
         MVC   WORK(36),SPACES                                                  
         ICM   R9,15,ASUB                                                       
         BZ    NMX                                                              
         USING TRSUBHD,R9                                                       
         ZIC   R3,TRSBLEN                                                       
         SH    R3,=H'18'                                                        
         BM    NMX                                                              
         EX    R3,*+8                                                           
         B     NMX                                                              
         MVC   WORK(0),TRSBNAME                                                 
NMX      XIT1                                                                   
         DROP  R9                                                               
         EJECT                                                                  
*              SET UP DISPLAY FROM TABLE                                        
         SPACE 1                                                                
TDISP    MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,4                                                         
         BE    T32                                                              
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
         MVC   INFDAT3,HEADING     AND HEADING                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT4,HEADING2                                                 
         OI    INFDAT4H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
T32      MVI   LASTKMK,0                                                        
         LH    R5,COUNT            R5 = COUNT OF TABLE ENTRIES DISPLAYD         
         LA    R6,INFDAT5H         R6 = A(LINE HEADER)                          
         USING LINED,R6                                                         
         MVC   CONCODE,SPACES                                                   
         MVC   CONNAME,SPACES                                                   
         SPACE 1                                                                
         CLC   INFKEY(4),=C'PREV'  ADJUST COUNT IN R5 FOR PREVIOUS              
         BNE   T34                                                              
         SR    R2,R2                                                            
         LR    R3,R5                                                            
         D     R2,=F'15'                                                        
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,15                                                            
         LA    R2,15(R2)                                                        
         SR    R5,R2                                                            
T34      LR    R1,R5               POINT TO NEXT ENTRY                          
         M     R0,TABWIDTH                                                      
         AR    R7,R1                                                            
         B     T4                                                               
HEADING  DC    CL39'CONTRA-ACCOUNT              TYPE START'                     
         DC    CL39'  END   BUDGET  ACTUAL BALANCE VARIANCE'                    
HEADING2 DC    CL39'--------------              ---- -----'                     
         DC    CL39'  ---   ------  ------ ------- --------'                    
         SPACE 1                                                                
T4       CLI   TABLE,X'FF'         DISPLAY A LINE FOR A BUDGET                  
         BE    TFINAL                                                           
         CLC   LINE,=H'19'                                                      
         BE    TFULL                                                            
         BAS   RE,GETNAME          CONTRA CODE/NAME                             
         CLC   CONCODE,SPACES                                                   
         BE    T5                                                               
         MVC   WORK(53),SPACES                                                  
         MVC   WORK(14),CONCODE+1                                               
         MVI   WORK+15,C'/'                                                     
         MVC   WORK+17(36),CONNAME                                              
         GOTO1 VSQASHER,DMCB,WORK,53                                            
         MVC   LINEDATA(28),WORK                                                
         SPACE 1                                                                
T5       LA    R1,BTYPETAB         BUDGET TYPE                                  
T6       CLI   0(R1),0                                                          
         BE    T7                                                               
         CLC   0(1,R1),TABBTYPE                                                 
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     T6                                                               
         MVC   LINEDATA+29(2),1(R1)                                             
         B     T7                                                               
BTYPETAB DC    C'CCRDDROOREE ',X'00'                                            
         SPACE 1                                                                
T7       MVC   WORK(2),TABSTART                                                 
         MVI   WORK+2,1                                                         
         GOTO1 VDATCON,DMCB,(1,WORK),(9,LINEDATA+33)                            
         MVC   WORK(2),TABEND                                                   
         GOTO1 VDATCON,DMCB,(1,WORK),(9,LINEDATA+40)                            
         SPACE 1                                                                
         LA    R1,TABBUDG          ROUND BUDGET AND ACTUAL IN SITU              
         BAS   RE,ROUNDIT                                                       
         LA    R1,TABACT                                                        
         LA    RE,T8                                                            
ROUNDIT  AP    0(6,R1),=P'50'                                                   
         NI    4(R1),X'F0'         CLEAR PENNY NIBBLES                          
         NI    5(R1),X'0F'                                                      
         BR    RE                                                               
         SPACE 1                                                                
T8       LA    R2,TABBUDG          DISPLAY BUDGET, ACTUAL AND BALANCE           
         LA    R3,LINEDATA+46                                                   
         BAS   RE,DISPVAL                                                       
         LA    R2,TABACT                                                        
         LA    R3,LINEDATA+54                                                   
         BAS   RE,DISPVAL                                                       
         MVC   WORK+20(6),TABBUDG                                               
         SP    WORK+20(6),TABACT                                                
         LA    R2,WORK+20                                                       
         LA    R3,LINEDATA+62                                                   
         BAS   RE,DISPVAL                                                       
         SPACE 1                                                                
         XC    DUB,DUB             CALCULATE AND DISPLAY VARIANCE IE            
         MVC   DUB+2(6),TABBUDG                                                 
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BZ    T9                     ( ACTUAL X 100.00 )  - 100.00             
         MVC   DUB+2(6),TABACT          ------                                  
         CVB   R3,DUB                   BUDGET                                  
         M     R2,=F'10000'                                                     
         SLDA  R2,1                                                             
         DR    R2,R4                                                            
         LTR   R3,R3                                                            
         BM    *+8                                                              
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         S     R3,=F'10000'                                                     
         LA    RF,LINEDATA+71                                                   
         EDIT  (R3),(7,0(RF)),2,MINUS=YES                                       
         CLI   LINEDATA+77,C'-'                                                 
         BE    *+8                                                              
         MVI   LINEDATA+77,C'+'                                                 
         SPACE 1                                                                
T9       OI    LINEHDR+6,X'80'                                                  
         LA    R5,1(R5)            BUMP COUNT                                   
         LH    R1,LINE                  SCREEN LINE                             
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         LA    R6,LINELEN(R6)                                                   
         A     R7,TABWIDTH              TABLE                                   
         B     T4                                                               
         EJECT                                                                  
*              FINAL ACTIONS FOR SCREEN FULL AND END OF TABLE                   
         SPACE 1                                                                
TFULL    OI    LASTKMK,X'80'       CONTINUATION BIT                             
         SPACE 1                                                                
TFINAL   STH   R5,COUNT                                                         
         C     R5,=F'15'                                                        
         BNH   *+8                                                              
         OI    LASTKMK,X'40'       PREVIOUS SCREEN MARKER                       
         SPACE 1                                                                
         CLI   LASTKMK,0           COULD THERE BE A 'PREV' OR 'NEXT'            
         BE    TXIT                ENQUIRY                                      
         GOTO1 AWRITIA             IF SO SAVE TABLE                             
         MVI   LINE+1,4            AND HEADLINES                                
         TM    LASTKMK,X'80'                                                    
         BZ    TXIT                                                             
         LNR   RF,RF               CC = NEQ FOR SCREEN FULL                     
         B     TXIT                                                             
TNEXT    L     R7,ATIA                                                          
         MVC   0(8,R7),TABWIDTH                                                 
         LTR   RB,RB               CC = POS FOR NEXT RECORD PLEASE              
         B     TXIT                                                             
         SPACE 1                                                                
TEND     SR    RB,RB               CC = EQU FOR END                             
TXIT     XIT1                                                                   
         EJECT                                                                  
*              GET A CONTRA-ACCOUNT NAME INTO CONNAME FOR DISPLAY               
*              ON ENTRY R7      = A(TABLE ENTRY) COVERED BY TABLED              
*              ON EXIT  CONNAME = NAME(36) OR SPACES                            
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         CLC   TABCON,CONCODE                                                   
         BE    GETNX                                                            
         MVC   CONCODE,TABCON                                                   
         MVC   CONNAME,SPACES                                                   
         CLC   TABCON,SPACES                                                    
         BE    GETNX                                                            
         CLC   TABNAME,SPACES                                                   
         BE    GETN2                                                            
         MVC   CONNAME(29),TABNAME                                              
         B     GETNX                                                            
         SPACE 1                                                                
GETN2    LA    R4,KEYB                                                          
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES       IF NO NAME IN TABLE READ A/C RECORD          
         MVC   ACTKCULA,TABCON                                                  
         L     R9,AIOB                                                          
         GOTO1 AREADB                                                           
         BZ    GETNX                                                            
         MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETEL                                                         
         BNZ   GETNX                                                            
         USING ACNAMED,R9                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         BM    GETNX                                                            
         EX    R3,*+8                                                           
         B     GETNX                                                            
         MVC   CONNAME(0),ACNMNAME                                              
         SPACE 1                                                                
GETNX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY A VALUE IN $'S                                           
*              ON ENTRY R2 = A(P6 VALUE)                                        
*                       R3 = A(DISPLAY POSITION)                                
         SPACE 1                                                                
DISPVAL  EDIT  (P6,0(R2)),(11,WORK+40),2,MINUS=YES                              
         MVC   WORK+47(1),WORK+50                                               
         MVC   0(8,R3),WORK+40                                                  
         BR    RE                                                               
         EJECT                                                                  
*              FIND OR ADD A TABLE ENTRY                                        
*              ON ENTRY CALLTYPE = FIND OR ADD (EQUATES)                        
*                       IF FIND                                                 
*                       CONCODE  = CONTRA CODE FOR WHICH CALLER WANTS           
*                                  FIRST ENTRY ADDRESS PASSED BACK              
*                       IF ADD                                                  
*                       CONCODE  = *                                            
*                       START    = * KEY FOR ADDITIONAL ENTRY                   
*                       END      = *                                            
*                       TYPE     = *                                            
*                       BUDGET   =  *                                           
*                       ACTUAL   =  * REMAINDER OF ENTRY                        
*                       CONNAME  =  *                                           
*              ON EXIT  R7       = A(NEW OR FOUND TABLE ENTRY)                  
*                       DMCB     = 1 IF RECORD NOT FOUND                        
*                       CC       = EQU IF TABLE IS FULL                         
         SPACE 1                                                                
FINDIT   NTR1                                                                   
         XC    DMCB+16(3),DMCB+16  ADJUST KEY LENGTH FOR CALLTYPE               
         MVI   DMCB+19,15          FIND - CONTRA                                
         CLI   CALLTYPE,FIND                                                    
         BE    FIND2                                                            
         MVI   DMCB+19,20          ADD  - CONTRA/START/END/TYPE                 
         SPACE 1                                                                
FIND2    LM    R4,R6,RECNUM                                                     
         GOTO1 VBINSRCH,DMCB,(CALLTYPE,CONCODE),ATABLE,(R4),(R5),,(R6)          
         MVC   RECNUM,DMCB+8                                                    
         ICM   R7,15,DMCB                                                       
         BNZ   FINDX                                                            
         SPACE 1                                                                
         ZIC   R1,TABTYPE          TABLE IS FULL SO COMPRESS IT BY              
         LA    R1,1(R1)            INCREMENTING THE TABLE TYPE (PRE-SET         
         LR    R0,R1               TO ZERO), USING THIS TO INDEX INTO           
         SLL   R1,3                A SET OF ALTERNATIVE TABLE PROFILES          
         LA    R1,TABPROFS(R1)     CONSISTING OF REDUCED TABLE ENTRY            
         CLI   0(R1),X'FF'         WIDTHS AND INCREASED MAX. NOS., AND          
         BE    FINDX               COMPRESSING THE TABLE ACCORDINGLY.           
         STC   R0,TABTYPE                                                       
         L     R7,ATABLE                                                        
         L     R5,RECNUM                                                        
         M     R4,TABWIDTH                                                      
         AR    R5,R7                                                            
         BCTR  R5,0                                                             
         L     R4,TABWIDTH                                                      
         MVC   TABWIDTH(8),0(R1)                                                
         LR    R1,R7                                                            
         L     R2,TABWIDTH                                                      
         LR    R6,R2                                                            
         BCTR  R6,0                                                             
         L     R3,RECNUM                                                        
FIND4    EX    R6,MOVNTRY                                                       
         AR    R1,R2                                                            
         BXLE  R7,R4,FIND4                                                      
         ST    R3,RECNUM                                                        
         B     FIND2                                                            
MOVNTRY  MVC   0(0,R1),0(R7)                                                    
         SPACE 1                                                                
FINDX    XIT1  REGS=(R7)                                                        
         SPACE 1                                                                
TABPROFS DC    F'61',F'37'         INITIAL PROFILE - 29BYTE CONTRA NAME         
         DC    F'32',F'72'         MINIMUM PROFILE - NO     CONTRA NAME         
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R9,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              DSECT TO COVER LOCAL WORKING STORAGE                             
LOCALD   DSECT                                                                  
RECNUM   DS    F         X         NUMBER OF TABLE ENTRIES SO FAR               
TABWIDTH DS    F         X         CURRENT SIZE OF AN ENTRY (26 TO 62)          
TABMAX   DS    F         X         CURRENT MAX NUMBER OF ENTRIES                
ATABLE   DS    A         X         ADDRESS OF TABLE                             
TABTYPE  DS    C         X         TABLE TYPE - INDEX INTO TABPROFS             
CONFILT  DS    CL15      C         CONTRA FILTER ONCE IT IS 'FIXED'             
CONFLEN  DS    F         X         LENGTH OF 'FIXED' CONTRA FILTER              
CALLTYPE DS    C         X         PASSED TO FINDIT ROUTINE (ADD/FIND)          
CONCODE  DS    CL15      C         CONTRA CODE FOR BUDGET ENTRY                 
START    DS    CL2       P         START MONTH/YEAR                             
END      DS    CL2       P         END   MONTH/YEAR                             
TYPE     DS    CL1       C         BUDGET TYPE                                  
BUDGET   DS    PL6       P         BUDGET AMOUNT                                
ACTUAL   DS    PL6       P         ACTUAL FOR PERIOD                            
CONNAME  DS    CL36      C         CONTRA NAME                                  
         SPACE 1                                                                
*              DSECT TO COVER A TABLE ENTRY IN TIA                              
TABLED   DSECT                                                                  
TABLE    DS    0C                                                               
TABCON   DS    CL15      C         CONTRA-ACCOUNT CODE = KEY 1                  
TABSTART DS    CL2       P         START MONTH/YEAR    = KEY 2                  
TABEND   DS    CL2       P         END   MONTH/YEAR    = KEY 3                  
TABBTYPE DS    CL1       C         BUDGET TYPE         = KEY 4                  
TABBUDG  DS    PL6       P         BUDGET AMOUNT                                
TABACT   DS    PL6       P         ACTUAL FOR PERIOD                            
TABNAME  DS    CL29      C         CONTRA NAME                                  
TABLEN   EQU   *-TABLED                                                         
         SPACE 1                                                                
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
SAVEKEY  DS    CL42      C         SAVED ACCOUNT KEY                            
         SPACE 1                                                                
ADD      EQU   1                                                                
FIND     EQU   0                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACINQ04   05/01/02'                                      
         END                                                                    
