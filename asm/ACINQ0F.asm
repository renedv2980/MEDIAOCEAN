*          DATA SET ACINQ0F    AT LEVEL 015 AS OF 05/01/02                      
*PHASE T6060FA,*                                                                
         TITLE 'ACCOUNT ENQUIRY MK2 - NEW BUDGETS (NB) - T6060F'                
T6060F   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'NEW BUDGET' IN ACCOUNT ENQUIRY PROGRAM          
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6060F)                                               
         DC    A(FILTABLE-T6060F)                                               
         DC    A(KNTRYPNT-T6060F)                                               
         DC    A(FNTRYPNT-T6060F)                                               
         DC    A(DNTRYPNT-T6060F)                                               
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
FILTABLE DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ABUD-GWS)                                                    
         DC    AL1(ACBAMNTH-ACBAD)                                              
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'MONTH'                                                      
         DC    CL2'MO'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ABUD-GWS)                                                    
         DC    AL1(ACBAMNTH-ACBAD)                                              
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ABUD-GWS)                                                    
         DC    AL1(ACBAMNTH-ACBAD)                                              
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    X'01'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACBTKBNO-ACKEYD)                                             
         DC    AL1(L'ACBTKCOD)                                                  
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              HANDLE CONTRA-ACCOUNT FILTER ON BUDGET RECS                      
*              (ROUTINE CALLED BY FILTER ROUTINE IN ROOT)                       
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQF**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060F,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         OC    RECOUNT,RECOUNT                                                  
         BE    VTYP1               VALIDATE TYPE FILTER                         
         LA    R2,WORK                                                          
         ICM   R9,15,AKEY                                                       
         USING ACKEYD,R9                                                        
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
VCONTMV2 MVC   CONFILT(0),ACBTKCON                                              
VCONTCL0 CLC   CONFILT(0),ACBTKCON                                              
VCONTCL1 CLC   INFCAC(0),ACBTKCON+1                                             
VCONTCL2 CLC   INFCAC(0),ACBTKCON+2                                             
VCONTCL3 CLC   INFCAC(0),ACBTKCON+3                                             
         DROP  R9                                                               
         SPACE 2                                                                
*              VALIDATE TYPE FILTER                                             
         SPACE 1                                                                
VTYP1    DS    0H                  FIND BUDGET TYPE REC                         
         USING FTBD,R6                                                          
         USING ACKEYD,R5                                                        
         LA    R5,KEYB                                                          
         XC    ACBTKEY,ACBTKEY                                                  
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,MYCO                                                    
         ZIC   R4,FTBLEN                                                        
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACBTKCOD(0),FTBVAL                                               
         GOTO1 AHIGHB                                                           
         MVI   DMCB,1                                                           
         BE    TXIT                                                             
         L     R5,AIOB                                                          
         LA    R4,(ACBTKCOD-ACBTKEY)(R4)                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   KEYB(0),0(R5)                                                    
         MVI   ERROR,100           TYPE DOESNT EXIST                            
         BNE   TXIT                                                             
         MVI   FTBLEN,2                                                         
         MVC   FTBVAL(2),ACBTKNO2                                               
         MVI   FTBSIGN,C'P'        (IN CASE IT WAS OVERWRITTEN)                 
         OI    OPTIONS,TYPFILT                                                  
         MVC   TYPECODE,ACBTKCOD                                                
         ST    R6,ATYPFILT                                                      
         MVI   DMCB,0                                                           
         B     TXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQF                                                         
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060F,RB                                                        
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
         CLI   ACKEYD,RUNLAST                                                   
         BNE   *+16                                                             
         CLI   KEYCHK,ACBTKTEQ                                                  
         BE    TBUDLAST            END OF BUDGETS                               
         B     TDISP               END OF CONTRAS                               
         CLI   VIRGIN,2                                                         
         BL    TFIRST              FIRST (ACCOUNT) REC                          
         BE    TBUDFRST            FIRST BUDGET REC                             
         CLI   KEYCHK,ACBTKTEQ                                                  
         BE    TUPDATE             EACH BUDGET REC                              
         B     TUPDATE2            EACH CONTRA HEADER                           
         EJECT                                                                  
*              FIRST (ACCOUNT) RECORD ACTIONS                                   
         SPACE 1                                                                
TFIRST   DS    0H                  INITIALISE                                   
         MVI   TABLE,X'FF'                                                      
         XC    RECNUM,RECNUM                                                    
         XC    COUNT,COUNT                                                      
         ZAP   BUDGET,=P'0'                                                     
         ZAP   ACTUAL,=P'0'                                                     
         MVI   VIRGIN,2                                                         
         MVC   SAVEKEY,ACKEYD                                                   
         MVC   CONFILT,SPACES                                                   
         ST    R7,ATABLE                                                        
         MVI   TABTYPE,0           SET INITIAL TABLE WIDTH = TYPE 0             
         MVC   TABWIDTH(8),TABPROFS                                             
         SPACE 1                                                                
TFIRST2  MVI   KEYMASK+15,X'FF'    RESET KEY CONTROLS TO GIVE BUDGETS           
         XC    KEYMASK+32(3),KEYMASK+32                                         
         MVC   WORK(15),KEYCHK                                                  
         MVI   KEYCHK,ACBTKTEQ                                                  
         MVC   KEYCHK+1(15),WORK                                                
         XC    KEYCHK+32(10),KEYCHK+32                                          
         MVC   ACKEYD(L'ACCKEY),KEYCHK                                          
         SPACE 1                                                                
TFIRST4  LA    R6,FILTAB           RESET FILTERS FOR BUDGET RECORDS             
         USING FTBD,R6                                                          
         CLI   INFCAC,C' '         CONTRA                                       
         BE    TFIRST6                                                          
         MVI   FTBDISP,(ACBTKCON-ACBTKEY)    DISP INTO KEY                      
         NI    FTBSTAT,X'FB'       FILTERING IN ROOT (VIA S/R IN OLAY)          
TFIRST6  TM    OPTIONS,TYPFILT     TYPE                                         
         BNO   TNEXT                                                            
         L     R6,ATYPFILT                                                      
         MVI   FTBSTAT,0                                                        
         B     TNEXT                                                            
         EJECT                                                                  
*              FIRST BUDGET RECORD ACTIONS                                      
         SPACE 1                                                                
TBUDFRST MVI   VIRGIN,3            CREATE A TYPE FILTER IF WE HAVENT            
         TM    OPTIONS,TYPFILT     GOT ONE                                      
         BO    TUPDATE                                                          
         LA    R6,FILTAB                                                        
         CLI   0(R6),X'FF'                                                      
         BE    *+12                                                             
         LA    R6,FTBTBLEN(R6)                                                  
         B     *-12                                                             
         XC    0(FTBTBLEN,R6),0(R6)                                             
         LA    RF,AKEY                                                          
         ST    RF,FTBELMNT                                                      
         MVI   FTBDISP,(ACBTKBNO-ACBTKEY)                                       
         MVI   FTBLEN,L'ACBTKBNO                                                
         MVI   FTBMARK,C'C'                                                     
         MVI   FTBSIGN,C'P'                                                     
         MVC   FTBVAL(2),ACBTKBNO                                               
         MVI   FTBTBLEN(R6),X'FF'                                               
         ST    R6,ATYPFILT                                                      
         SPACE 1                                                                
TBUDF02  LA    R4,KEYB             AND GET ITS CODE                             
         USING BUDRECD,R4                                                       
         XC    BUDKEY,BUDKEY                                                    
         MVC   BUDKEY(2),ACKEYD                                                 
         MVC   BUDKNO1,FTBVAL                                                   
         GOTO1 AHIGHB                                                           
         BE    TEND                                                             
         L     R4,AIOB                                                          
         MVC   TYPECODE,BUDKCOD                                                 
         B     TUPDATE                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              UPDATE TABLE OF CONTRA ENTRIES FROM BUDGET RECORDS               
         SPACE 1                                                                
TUPDATE  MVI   ELCODE,ACBAELEQ                                                  
         L     R7,ATABLE                                                        
         BAS   RE,GETEL                                                         
         BNE   TNEXT                                                            
         SPACE 1                                                                
TUP01    ST    R9,ABUD             HANDLE A BUDGET AMOUNT EL                    
         MVI   DMCB,0                                                           
         GOTO1 AFILTER             DATE FILTERS                                 
         BZ    TUP03                                                            
         CLI   0(R7),X'FF'                                                      
         BNE   TUP02                                                            
         USING TABLED,R7                                                        
         L     RE,AIO                                                           
         MVC   TABCON,ACBTKCON-ACBTKEY(RE)                                      
         USING ACBAD,R9                                                         
         MVC   TABSTART,ACBAMNTH                                                
         ZAP   TABBUDG,=P'0'                                                    
         ZAP   TABACT,=P'0'                                                     
         MVC   TABNAME,SPACES                                                   
TUP02    MVC   TABEND,ACBAMNTH                                                  
         AP    TABBUDG,ACBABUDG                                                 
         AP    BUDGET,ACBABUDG     ADD TO ACC TOT                               
TUP03    BAS   RE,NEXTEL                                                        
         BE    TUP01                                                            
         SPACE 1                                                                
TUP04    CLI   0(R7),X'FF'                                                      
         BE    TNEXT                                                            
         L     R9,AIO                                                           
         MVI   ELCODE,ACNMELQ      MOVE IN BUDGET NAME IF ANY                   
         BAS   RE,GETEL                                                         
         BNE   TUP0X                                                            
         USING ACNAMED,R9                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         BM    TUP0X                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TABNAME(0),ACNMNAME                                              
TUP0X    A     R7,TABWIDTH                                                      
         MVI   0(R7),X'FF'                                                      
         ST    R7,ATABLE                                                        
         L     RF,RECNUM                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RECNUM                                                        
         B     TNEXT                                                            
         EJECT                                                                  
*              END OF BUDGET RECORDS ACTIONS                                    
         SPACE 1                                                                
TBUDLAST CLI   TABLE,X'FF'         CHECK FOR ANY BUDGETS IN TABLE               
         BE    TEND                NO                                           
         SPACE 1                                                                
TBUD02   MVI   KEYMASK+15,0        RESET KEY CONTROLS FOR CONTRA HEADS          
         MVC   KEYMASK+32(3),KEYMASK                                            
         MVC   KEYCHK(16),KEYCHK+1                                              
         MVC   KEYCHK+32(10),SPACES                                             
         ZIC   R1,INFKEYH+5        A/C LEVELS BELOW REQUESTED LEVEL             
         LA    RF,KEYMASK+1(R1)    ARE VARIABLE SUBKEYS                         
         XC    0(15,RF),0(RF)                                                   
         LA    RF,KEYCHK+1(R1)                                                  
         XC    0(15,RF),0(RF)                                                   
         L     RE,AIO                                                           
         MVC   0(L'ACCKEY,RE),SAVEKEY                                           
         CLI   INFCAC,C' '         ADJUST CONTRA FILTER                         
         BE    TBUD03                                                           
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         MVI   FTBDISP,17                                                       
         MVC   FTBSR,EDITCONT                                                   
TBUD03   L     R6,ATYPFILT         DISABLE TYPE FILTER                          
         MVI   FTBSTAT,1                                                        
         B     TNEXT                                                            
         EJECT                                                                  
*              UPDATE TABLE WITH ACTUALS AND CONTRA NAMES FROM CONTRA           
*              HEADERS                                                          
         SPACE 1                                                                
TUPDATE2 DS    0H                  SELECT CONTRA HEADERS                        
         OC    AHST,AHST                                                        
         BZ    TNEXT               NO HISTORY ELS                               
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
         BNE   TUP23               FILTER FITS IN FULL                          
         CLC   CONCODE,CONFILT                                                  
         BNE   TUP23                                                            
         BAS   RE,NAMTOWRK                                                      
         MVC   SAVECACN,WORK                                                    
         SPACE 1                                                                
TUP23    LA    R2,15               FIND MATCHING TABLE ENTRY                    
         LA    RF,TABCON(R2)                                                    
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         BCT   R2,*-14                                                          
         B     TUP25                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   TABCON(0),ACKEYCON                                               
         BE    TUP25                                                            
         BH    TNEXT                                                            
         A     R7,TABWIDTH                                                      
         CLI   0(R7),X'FF'                                                      
         BE    TNEXT                                                            
         B     TUP23                                                            
         SPACE 1                                                                
TUP25    CLC   TABCON,CONCODE      MOVE CONTRA NAME TO TABLE IF WE              
         BNE   TUP27               DIDNT HAVE A BUDGET NAME AND CONTRAS         
         CLI   TABTYPE,0           MATCH IN FULL                                
         BNE   TUP27                                                            
         CLC   TABNAME,SPACES                                                   
         BNE   TUP27                                                            
         BAS   RE,NAMTOWRK                                                      
         MVC   TABNAME,WORK                                                     
         SPACE 1                                                                
TUP27    SR    R3,R3               UPDATE ACTUALS                               
         ICM   R9,15,AHST                                                       
         USING TRHISTD,R9                                                       
         SPACE 1                                                                
ADACT2   L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         CLC   TABSTART,TRHSYEAR   MATCH ON DATE AND UPDATE                     
         BH    ADACT4                                                           
         CLC   TABEND,TRHSYEAR                                                  
         BL    TNEXT                                                            
         CLC   ACKEYACC+1(2),=C'28'  SOME LEDGERS ONLY HAVE CREDITS             
         BE    ADACT3A                                                          
         CLC   ACKEYACC+1(2),=C'29'                                             
         BE    ADACT3A                                                          
         CLC   ACKEYACC+1(2),=C'13'                                             
         BE    ADACT3A                                                          
         CLC   ACKEYACC+1(2),=C'1C'                                             
         BNE   ADACT3B                                                          
         CLC   ACKEYCON+1(2),=C'11'                                             
         BE    ADACT3B                                                          
         CLC   ACKEYCON+2(2),=C'12'                                             
         BE    ADACT3B                                                          
ADACT3A  MP    TRHSCR,=P'-1'                                                    
ADACT3B  DS    0H                                                               
         AP    TABACT,TRHSDR                                                    
         SP    TABACT,TRHSCR                                                    
         AP    ACTUAL,TRHSDR                                                    
         SP    ACTUAL,TRHSCR                                                    
         SPACE 1                                                                
ADACT4   IC    R3,1(R9)            BUMP TO NEXT HISTORY EL                      
         AR    R9,R3                                                            
         CLI   0(R9),0                                                          
         BE    TNEXT                                                            
         CLI   0(R9),X'45'                                                      
         BNE   ADACT4                                                           
         B     ADACT2                                                           
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
         LA    RF,INFDAT2+77       AND BUDGET TYPE CODE                         
         LR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RF,RE               RF = SPACE AVAILABLE IN LINE 2               
         LA    R1,L'TYPECODE                                                    
         LA    R2,TYPECODE-1(R1)                                                
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-12                                                          
         LA    R1,8(R1)            R1 = SPACE REQUIRED                          
         SR    RF,R1                                                            
         BNM   *+6                                                              
         AR    RE,RF                                                            
         MVC   1(8,RE),=C' / TYPE='                                             
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   9(0,RE),TYPECODE                                                 
         MVC   INFDAT3,HEADING     AND HEADING                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT4,HEADING2                                                 
         OI    INFDAT4H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
T32      MVI   LASTKMK,0                                                        
         LH    R5,COUNT            R5 = COUNT OF TABLE ENTRIES DISPLAYD         
         LA    R6,INFDAT5H         R6 = A(LINE HEADER)                          
         USING LINED,R6                                                         
         CH    R5,=H'-1'                                                        
         BE    TTOT                ONLY ACC TOTAL TO COME                       
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
HEADING  DC    CL39'CONTRA-ACCOUNT                START   E'                    
         DC    CL39'ND     BUDGET   ACTUAL  BALANCE VARNCE '                    
HEADING2 DC    CL39'--------------                -----   -'                    
         DC    CL39'--     ------   ------  ------- ------ '                    
         SPACE 1                                                                
T4       CLI   TABLE,X'FF'         DISPLAY A LINE FOR A BUDGET                  
         BE    TTOT                                                             
         CLC   LINE,=H'19'                                                      
         BE    TFULL                                                            
         BAS   RE,GETNAME          CONTRA CODE/NAME                             
         CLC   CONCODE,SPACES                                                   
         BE    T7                                                               
         MVC   WORK(53),SPACES                                                  
         MVC   WORK(14),CONCODE+1                                               
         CLC   CONNAME,SPACES                                                   
         BE    *+14                                                             
         MVI   WORK+15,C'/'                                                     
         MVC   WORK+17(36),CONNAME                                              
         GOTO1 VSQASHER,DMCB,WORK,53                                            
         MVC   LINEDATA(29),WORK                                                
         SPACE 1                                                                
T7       MVC   WORK(2),TABSTART                                                 
         MVI   WORK+2,1                                                         
         GOTO1 VDATCON,DMCB,(1,WORK),(9,LINEDATA+30)                            
         MVC   WORK(2),TABEND                                                   
         GOTO1 VDATCON,DMCB,(1,WORK),(9,LINEDATA+37)                            
         SPACE 1                                                                
T7A      LA    R1,TABBUDG          ROUND BUDGET AND ACTUAL IN SITU              
         BAS   RE,ROUNDIT                                                       
         LA    R1,TABACT                                                        
         LA    RE,T8                                                            
ROUNDIT  CP    0(6,R1),=P'0'                                                    
         BNH   *+10                                                             
         AP    0(6,R1),=P'50'                                                   
         NI    4(R1),X'F0'         CLEAR PENNY NIBBLES                          
         NI    5(R1),X'0F'                                                      
         BR    RE                                                               
         SPACE 1                                                                
T8       LA    R2,TABBUDG          DISPLAY BUDGET, ACTUAL AND BALANCE           
         LA    R3,LINEDATA+44                                                   
         BAS   RE,DISPVAL                                                       
         LA    R2,TABACT                                                        
         LA    R3,LINEDATA+53                                                   
         BAS   RE,DISPVAL                                                       
         MVC   WORK+20(6),TABBUDG                                               
         SP    WORK+20(6),TABACT                                                
         LA    R2,WORK+20                                                       
         LA    R3,LINEDATA+62                                                   
         BAS   RE,DISPVAL                                                       
         SPACE 1                                                                
         XC    DUB,DUB             CALCULATE AND DISPLAY VARIANCE IE            
         MVC   DUB+3(5),TABBUDG                                                 
         MVN   DUB+7(1),TABBUDG+5  CONVERT TO POUNDS/DOLLARS                    
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BZ    T9                     ( ACTUAL X 100.00 )  - 100.00             
         MVC   DUB+3(5),TABACT          ------                                  
         MVN   DUB+7(1),TABACT+5        BUDGET                                  
         ZAP   PL13,DUB                                                         
         MP    PL13,=P'20000'                                                   
         CVD   R4,DUB                                                           
         DP    PL13,DUB+2(6)                                                    
         CP    PL13(7),=P'0'                                                    
         BL    *+10                                                             
         AP    PL13(7),=P'1'                                                    
         ZAP   DUB,PL13(7)                                                      
         ZAP   PL13,DUB                                                         
         DP    PL13,=P'2'                                                       
         SP    PL13(12),=P'10000'                                               
         ZAP   DUB2,PL13(12)                                                    
         LA    RF,LINEDATA+71                                                   
         EDIT  (P8,DUB2),(7,0(RF)),2,MINUS=YES                                  
         CLI   LINEDATA+77,C'-'                                                 
         BE    *+8                                                              
         MVI   LINEDATA+77,C'+'                                                 
         SPACE 1                                                                
T9       OI    LINEHDR+6,X'80'                                                  
         CH    R5,=H'-1'                                                        
         BNE   *+10                                                             
         SR    R5,R5               TOTALS DISPLAYED                             
         B     TFINAL              SO GO TO FINISH                              
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
TTOT     SR    R5,R5               ACC TOTAL PRINTING                           
         BCTR  R5,0                                                             
         CLC   LINE,=H'19'                                                      
         BE    TFULL                                                            
         MVC   LINEDATA+38(5),=C'TOTAL'                                         
         LA    R7,BUDGET-(TABBUDG-TABLED)                                       
         B     T7A                                                              
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
         CLC   RECNUM,TABMAX       IF SO SAVE TABLE                             
         BNH   TFINAL2             COMPRESSED IF NEC                            
         ZIC   R1,TABTYPE          TABLE IS FULL SO COMPRESS IT BY              
         LA    R1,1(R1)            INCREMENTING THE TABLE TYPE (PRE-SET         
         LR    R0,R1               TO ZERO), USING THIS TO INDEX INTO           
         SLL   R1,3                A SET OF ALTERNATIVE TABLE PROFILES          
         LA    R1,TABPROFS(R1)     CONSISTING OF REDUCED TABLE ENTRY            
         CLI   0(R1),X'FF'         WIDTHS AND INCREASED MAX. NOS., AND          
         BNE   *+12                COMPRESSING THE TABLE ACCORDINGLY.           
         MVI   LASTKMK,0                                                        
         B     TEND                CANT COMPRESS ANY MORE SO END                
         STC   R0,TABTYPE                                                       
         L     R7,ATIA                                                          
         LA    R7,8(R7)                                                         
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
         MVI   0(R1),X'FF'         NEW END OF TABLE                             
         BXLE  R7,R4,FIND4                                                      
         ST    R3,RECNUM                                                        
         B     TFINAL2                                                          
MOVNTRY  MVC   0(0,R1),0(R7)                                                    
TFINAL2  L     R7,ATIA                                                          
         MVC   0(8,R7),TABWIDTH                                                 
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
         BNE   *+14                                                             
         MVC   CONCODE+1(3),=C'ALL'                                             
         B     GETNX                                                            
         CLI   TABTYPE,0           TEST WIDE TABLE                              
         BNE   GETN2                                                            
         CLC   TABNAME,SPACES                                                   
         BE    GETN2                                                            
         MVC   CONNAME(29),TABNAME                                              
         B     GETNX                                                            
         SPACE 1                                                                
GETN2    LA    R4,KEYB             IF NO NAME IN TABLE READ A/C RECORD          
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
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
DISPVAL  EDIT  (P6,0(R2)),(12,WORK+40),2,MINUS=YES                              
         MVC   WORK+48(1),WORK+51                                               
         MVC   0(9,R3),WORK+40                                                  
         BR    RE                                                               
         EJECT                                                                  
TABPROFS DC    F'60',F'100'        INITIAL PROFILE - 29BYTE CONTRA NAME         
         DC    F'31',F'180'        MINIMUM PROFILE - NO     CONTRA NAME         
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
         GETEL R9,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              DSECT TO COVER LOCAL WORKING STORAGE                             
LOCALD   DSECT                                                                  
TABWIDTH DS    F         X         CURRENT SIZE OF AN ENTRY (26 TO 62)          
TABMAX   DS    F         X         CURRENT MAX NUMBER OF ENTRIES                
ATABLE   DS    A         X         ADDRESS OF TABLE                             
CONFILT  DS    CL15      C         CONTRA FILTER ONCE IT IS 'FIXED'             
CONFLEN  DS    F         X         LENGTH OF 'FIXED' CONTRA FILTER              
CONCODE  DS    CL15      C         CONTRA CODE FOR BUDGET ENTRY                 
CONNAME  DS    CL36      C         CONTRA NAME                                  
TYPECODE DS    CL10      C         BUDGET TYPE CODE                             
ATYPFILT DS    A         A         A(BUDGET TYPE FILTAB ENTRY)                  
DUB2     DS    D                                                                
PL13     DS    PL13                                                             
         SPACE 1                                                                
*              DSECT TO COVER A TABLE ENTRY IN TIA                              
TABLED   DSECT                                                                  
TABLE    DS    0C                                                               
TABCON   DS    CL15      C         CONTRA-ACCOUNT CODE                          
TABSTART DS    CL2       P         START MONTH/YEAR                             
TABEND   DS    CL2       P         END   MONTH/YEAR                             
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
         SPACE 2                                                                
SAVEKEY  DS    CL42      C         SAVED ACCOUNT KEY                            
BUDGET   DS    PL6       P         ACCOUNT TOTAL                                
ACTUAL   DS    PL6       P         ACCOUNT TOTAL                                
RECNUM   DS    F         X         NUMBER OF TABLE ENTRIES SO FAR               
TABTYPE  DS    C         X         TABLE TYPE - INDEX INTO TABPROFS             
         SPACE 1                                                                
TYPFILT  EQU   1                   OPTIONS SETTING FOR BUDGET TYPE              
*                                  FILTER INPUT                                 
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACINQ0F   05/01/02'                                      
         END                                                                    
