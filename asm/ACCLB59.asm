*          DATA SET ACCLB59    AT LEVEL 161 AS OF 08/16/00                      
*PHASE T62159A                                                                  
CLB59    TITLE '- PC COMMS - MATCH'                                             
CLB59    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB59**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
         L     R6,AOVERWRK                                                      
         USING MWORKD,R6                                                        
         USING POSTVALS,MPOSTVAL                                                
M        USING TLREC,BCLSTMAT                                                   
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
                                                                                
RCVJOB   MVC   THISJOB,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVDA    MVC   THISDA,DATA                                                      
         B     EXITY                                                            
                                                                                
RCVNMAT  ZAP   MATCHED,DATA(L'PTANET)                                           
         B     EXITY                                                            
                                                                                
RCVCMAT  ZAP   COMMATCH,DATA(L'PTANET)                                          
         B     BLDTSAR                                                          
                                                                                
RCVMCHDA MVC   M.TLDA,DATA                                                      
         BAS   RE,GETMTC                                                        
         BAS   RE,CLRTOTS                                                       
         B     EXITY                                                            
                                                                                
RCVINCAC MVC   M.TLMINCAC(1),CUABIN                                             
         MVC   M.TLMINCAC+1(L'TLMINCAC-1),DATA                                  
         B     EXITY                                                            
                                                                                
RCVBILNO MVC   M.TLMBILL,DATA                                                   
         B     EXITY                                                            
                                                                                
RCVBILDT MVC   M.TLMDATC,DATA                                                   
         GOTO1 VDATCON,BOPARM,(2,M.TLMDATC),(1,M.TLMDATP)                       
         B     EXITY                                                            
                                                                                
RCVRATE  MVC   EXCHRAT,DATA                                                     
         LA    RE,EXRULE                                                        
         ST    RE,MAEXC                                                         
         OI    MINDS,MIFCB                                                      
         B     EXITY                                                            
                                                                                
RCVCUR   MVC   BILCUR,DATA                                                      
         B     EXITY                                                            
                                                                                
RCVACTN  MVI   MINDS,0                                                          
         XC    EXRULE,EXRULE                                                    
         XC    MAEXC,MAEXC                                                      
         XC    NUMTSAR,NUMTSAR                                                  
*        ZAP   SVNETMA,BCPZERO                                                  
*        ZAP   SVNETMB,BCPZERO                                                  
*        ZAP   SVCOMMA,BCPZERO                                                  
*        ZAP   SVCOMMB,BCPZERO                                                  
*        ZAP   SVHOURM,BCPZERO                                                  
                                                                                
         CLI   DATA,ACTDRAQ                                                     
         BNE   *+8                                                              
         OI    MINDS,MIDRAFT                                                    
         CLI   DATA,ACTUPDQ                                                     
         BNE   *+8                                                              
         OI    MINDS,MILIVE                                                     
         B     EXITY                                                            
                                                                                
RCVBREF  MVC   OSREF#,DATA                                                      
         B     EXITY                                                            
                                                                                
RCVBMON  BAS   RE,VALMON                                                        
         BNE   EXITN                                                            
         TM    MINDS,MILIVE                                                     
         BO    UPDATE                                                           
         B     DRAFT                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
                                                                                
SNDMATCH DS    0H                                                               
         OC    THISJOB,THISJOB     IF NO JOB MUST BE DRAFT/UPDATE               
         BZ    SNDMATUP                                                         
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING PRORATAD,LSPRATA                                                 
         USING TLSTD,LSTLST                                                     
                                                                                
         GOTO1 SETBILL             BUILD BILL TABLE                             
                                                                                
         USING TRNRECD,IOKEY                                                    
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,THISJOB                                                  
         LA    R1,IOHIGH+IOACCDIR                                               
         B     GNEXT04                                                          
GETNEXT  GOTO1 GETITEM,1           GET NEXT BILL# FOR RECORD                    
         BE    GNEXT08                                                          
GNEXT02  LA    R1,IOSEQ+IOACCDIR                                                
GNEXT04  GOTO1 AIO                                                              
         BNE   GETNEXTX                                                         
         CLC   TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                                 
         BNE   GETNEXTX                                                         
         OC    TRNKDATE,TRNKDATE   ENSURE HAVE TRANSACTION RECORD               
         BZ    GNEXT02                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    GNEXT02                                                          
         TM    TRNKSTAT,TRNSREVS   EXCLUDE REVERSALS                            
         BNZ   GNEXT02                                                          
         CLI   TRNKSTYP,99         INCLUDE ADVANCES                             
         BE    GNEXT06                                                          
         TM    TRNKSTAT,TRNSDRFT   EXCLUDE OTHER DRAFTS                         
         BO    GNEXT02                                                          
         CLC   TRNKWORK,ORDWC      INCLUDE ORDERS                               
         BNE   GNEXT02                                                          
GNEXT06  MVC   LASTKEY,TRNKEY                                                   
         OI    LSSTAT1,LSSORDER                                                 
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT                                                          
         NI    LSSTAT1,X'FF'-LSSORDER                                           
                                                                                
         MVC   IODAOVER,TRNKDA                                                  
         MVC   THISDA,TRNKDA                                                    
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         CP    TRNAMNT,BCPZERO                                                  
         BNE   GNEXT02                                                          
                                                                                
         GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GNEXT02                                                          
         MVI   TLMINDS,TLMIADV     TEST ADVANCE/ORDER                           
         CLI   TRNTYPE,99                                                       
         BE    *+8                                                              
         MVI   TLMINDS,TLMIORD                                                  
         GOTO1 GETITEM,0                                                        
         BE    GNEXT07                                                          
         MVC   IOKEY,LASTKEY                                                    
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         B     GNEXT02                                                          
                                                                                
GNEXT07  L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   TLMINCAC,GOINCAC-GOBBLOCK(RF)                                    
         MVC   STLREC,TLREC                                                     
                                                                                
GNEXT08  L     R2,AIO1                                                          
         GOTO1 ASNDHDR,BODMCB,MH#MAT                                            
         GOTO1 ASNDDATA,BODMCB,1,THISDA                                         
         GOTO1 (RF),(R1),2,TRNKWORK                                             
         GOTO1 (RF),(R1),3,(L'TRNKULC,TRNKULC)                                  
         GOTO1 (RF),(R1),4,TRNKDATE                                             
         GOTO1 (RF),(R1),5,TRNKREF                                              
         GOTO1 (RF),(R1),7,TLMINCAC+1                                           
         GOTO1 (RF),(R1),8,TLMBILL                                              
         GOTO1 (RF),(R1),9,TLMDATC                                              
         GOTO1 (RF),(R1),10,TLMCUR                                              
         L     R2,ALSTPTA                                                       
         USING PTAELD,R2                                                        
         GOTO1 (RF),(R1),11,PTANET                                              
         GOTO1 (RF),(R1),12,PTARCOM                                             
         OC    TLMXEXC,TLMXEXC     FOREIGN CURRENCY INFO?                       
         BZ    GNEXT10                                                          
         GOTO1 (RF),(R1),13,TLMXEXC                                             
         GOTO1 (RF),(R1),14,PTANETF     SEND FC AMOUNTS ASWELL                  
         GOTO1 (RF),(R1),15,PTARFCOM                                            
         DROP  R2                                                               
GNEXT10  MVC   IOKEY,LASTKEY                                                    
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    GETNEXT                                                          
         DC    H'0'                                                             
                                                                                
GETNEXTX B     EXITY                                                            
         SPACE 1                                                                
                                                                                
***********************************************************************         
* ROUTINE TO NEXT UPDATED ITEM ON RECORD                              *         
* NTRY: R1 = ZERO TO GET FIRST ITEM ELSE GET NEXT ITEM                *         
* EXIT: CC = NOT EQUAL IF NO MORE ITEMS ON RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
GETITEM  NTR1  ,                                                                
         XR    R0,R0                                                            
         LTR   R1,R1               TEST GET NEXT ITEM                           
         BZ    GITEM02                                                          
         MVC   TLREC(TLMT1LNQ),STLREC                                           
         L     R3,ALSTPTA                                                       
         B     GITEM08                                                          
                                                                                
GITEM02  L     R3,ALSTPTA                                                       
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         TM    TLMINDS,TLMIORD     TEST AN ORDER                                
         BZ    GITEM04             YES - MAY BE PARTLY MATCHED                  
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO4                                  
         L     R3,AIO4                                                          
         USING PTAELD,R3                                                        
                                                                                
GITEM04  CLI   PTAEL,0             TEST EOR                                     
         BE    GETITEMN                                                         
         CLI   PTAEL,PTAELQ        TEST REGULAR ALLOCATION PTAEL                
         BNE   GITEM08                                                          
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GITEM08                                                          
         TM    PTASTAT1,PTASPEND+PTASREVS+PTASREVD+PTASREVU                     
         TM    PTASTAT1,PTASPEND                                                
         BZ    GITEM10             TEST NOT PENDING                             
GITEM08  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     GITEM04                                                          
                                                                                
GITEM10  MVC   TLMBILL,PTARBLNO    SAVE BILL#                                   
         MVC   TLMDATC,PTARBLDT    SAVE BILL DATE                               
         GOTO1 VDATCON,BOPARM,(2,TLMDATC),(1,TLMDATP)                           
         MVC   TLMCUR,PTACUR       SAVE CURRENCY CODE                           
         NI    TLMINDS,FF-(TLMICPY+TLMISEC)                                     
         CLC   TLMCUR,CSCPYCUR                                                  
         BNE   *+8                                                              
         OI    TLMINDS,TLMICPY                                                  
         CLC   TLMCUR,BCCPYSEC                                                  
         BNE   *+8                                                              
         OI    TLMINDS,TLMISEC                                                  
                                                                                
         TM    TLMINDS,TLMICPY     TEST FOR COMPANY CURRENCY                    
         BZ    GITEM12                                                          
         MVC   TLMCTAB,CSCURCPY    SET COMPANY CURRENCY                         
         XC    TLMX,TLMX                                                        
         B     GETITEMY                                                         
                                                                                
GITEM12  DS    0H                                                               
         TM    TLMINDS,TLMISEC    TEST FOR SECONDARY CURRENCY                   
         BZ    GITEM14                                                          
         XC    TLMX,TLMX                                                        
         B     GITEM16                                                          
                                                                                
GITEM14  DS    0H                                                               
         GOTO1 GETBILL             GET EXCHANGE RATE OF BILL                    
         BNE   GETITEMN                                                         
                                                                                
GITEM16  DS    0H                                                               
         GOTO1 GETCUR,BOPARM,TLMCUR,TLMCTAB                                     
         BE    GETITEMY                                                         
         GOTO1 AIO,IOREAD+IOACCDIR                                              
                                                                                
GETITEMY ST    R3,ALSTPTA                                                       
         B     EXITY                                                            
*                                                                               
GETITEMN XC    ALSTPTA,ALSTPTA                                                  
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT CURRENCY CODE INTO TABLE ENTRY                   *         
* NTRY: P1=A(CURRENCY CODE)                                           *         
*       P2=A(CURRENCY TABLE ENTRY)                                    *         
* EXIT: CC=NOT EQUAL IF VBLDCUR CALLED                                *         
***********************************************************************         
         SPACE 1                                                                
GETCUR   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
                                                                                
         LA    R4,CODTAB                                                        
         USING CODTABD,R4                                                       
         LA    R0,CODTABL                                                       
         LA    R1,CODTABX-1                                                     
GCUR02   CLI   CODTABD,EOT                                                      
         BE    GCUR04                                                           
         CLC   CODTCOD,0(R2)                                                    
         BNE   *+14                                                             
         MVC   0(L'CODTTAB,R3),CODTTAB                                          
         B     EXITY                                                            
         BXLE  R4,R0,GCUR02                                                     
         SR    R4,R0               USE LAST ENTRY IF TABLE FULL                 
                                                                                
GCUR04   MVC   CODTCOD,0(R2)                                                    
         GOTO1 VBLDCUR,BOPARM,CODTCOD,(X'80',CODTTAB),ACOM                      
         MVC   0(L'CODTTAB,R3),CODTTAB                                          
         MVI   CODTABD+CODTABL,EOT                                              
         B     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOOK UP EXCHANGE RATE TABLE  - BILL                      *         
* NTRY: TLMBILL = BILL#                                               *         
*       TLMDATP = BILL DATE                                           *         
* EXIT: TLMX     = EXCHANGE RATE                                      *         
***********************************************************************         
         SPACE 1                                                                
GETBILL  NTR1  ,                                                                
         L     R3,AIO2                                                          
X        USING TLSTD,R3                                                         
         XC    X.TLSTD(TLBLNQ),X.TLSTD  READ TSAR RECORD                        
         MVI   X.TLKSES,TLKSTEMP                                                
         MVC   X.TLBDATE,TLMDATP                                                
         MVC   X.TLBREF#,TLMBILL                                                
         GOTO1 ATSARIO,BOPARM,('TSARDH',X.TLSTD)                                
         CLC   X.TLBDATE,TLMDATP                                                
         BE   *+6                                                               
         DC    H'0'                                                             
         CLC   X.TLBREF#,TLMBILL                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    X.TLBX,X.TLBX       TEST ALREADY GOT EXCHANGE RATE               
         BNZ   GBILL10                                                          
         MVC   IODAOVER,X.TLBDA    NO - GO GET IT                               
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3                                                          
         LA    R4,TRNRFST-TRNRECD(R4)                                           
         USING AFCELD,R4                                                        
         XR    RF,RF                                                            
GBILL02  CLI   AFCEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R4,RF,GBILL02                                                    
         MVC   X.TLBX,AFCX         WRITE BACK TSAR RECORD                       
         GOTO1 ATSARIO,BOPARM,('TSAPUT',X.TLSTD)                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GBILL10  MVC   TLMX,X.TLBX                                                      
         B     EXITY                                                            
         DROP  X,R4                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP BILL TABLE                                        *         
***********************************************************************         
         SPACE 1                                                                
SETBILL  NTR1  ,                                                                
         L     R2,AIO2                                                          
X        USING TLSTD,R2                                                         
         XC    X.TLSTD(TLBLNQ),X.TLSTD                                          
         MVC   X.TLRLEN,=AL2(TLBLNQ)                                            
         MVI   X.TLKSES,TLKSTEMP                                                
                                                                                
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,THISJOB                                                  
         MVC   TRNKWORK,BILLWC                                                  
                                                                                
         LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
SBILL02  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   SBILL04                                                          
         CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                                 
         BNE   SBILL04                                                          
         OC    TRNKDATE,TRNKDATE                                                
         BZ    SBILL02                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    SBILL02                                                          
                                                                                
         MVC   X.TLBDATE,TRNKDATE                                               
         MVC   X.TLBREF#,TRNKREF                                                
         MVC   X.TLBDA,TRNKDA                                                   
         XC    X.TLBX,X.TLBX                                                    
         GOTO1 ATSARIO,BOPARM,('TSAADD',X.TLSTD)                                
         B     SBILL02                                                          
                                                                                
SBILL04  DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  X                                                                
         EJECT                                                                  
                                                                                
SNDMATUP DS    0H                                                               
         TM    MINDS,MIPOST                                                     
         BO    SNDUP02                                                          
         GOTO1 VGETTXT,BOPARM,AI$NOPMM,(L'WORK,WORK),('GTMINF',0),,    *        
               ('GT1NOREF+GT1OWRK',0)                                           
         ZIC   R2,BOPARM+4         R2=L'MESSAGE                                 
         B     SNDUP10                                                          
                                                                                
SNDUP02  L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVC   WORK,REPSUBID                                                    
         MVI   WORK+L'REPSUBID,C','                                             
         LA    RF,WORK+L'REPSUBID+1                                             
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT,WRK=BOWORK2                     
         LA    R2,7                                                             
         DROP  R3                                                               
                                                                                
SNDUP10  GOTO1 ASNDHDR,BODMCB,MH#MAT                                            
         GOTO1 ASNDDATA,BODMCB,19,((R2),WORK)                                   
         B     EXITY                                                            
                                                                                
***********************************************************************         
BLDTSAR  DS    0H                                                               
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING PRORATAD,LSPRATA                                                 
         USING TLSTD,LSTLST                                                     
         MVC   IODAOVER,THISDA                                                  
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
                                                                                
         XC    TLST1,TLST1         SET UP TSAR KEY                              
         XC    TLST2,TLST2                                                      
         XC    TLST3,TLST3                                                      
         LA    R1,TLMT2LNQ                                                      
         STCM  R1,3,TLRLEN                                                      
         OI    TLKSES,TLKSTEMP                                                  
         MVC   TLRECACT,CSRECACT                                                
         MVC   TLKWC,TRNKWORK                                                   
         MVC   TLKULC,TRNKCULC                                                  
         MVC   TLKDATE,TRNKDATE                                                 
         MVC   TLKREF,TRNKREF                                                   
         MVC   TLKSBR,TRNKSBR                                                   
         MVC   TLDA,THISDA                                                      
         DROP  R2                                                               
                                                                                
         BAS   RE,SETLST           FILL IN RECORD VALUES                        
                                                                                
         ZAP   TLIHRM,BCPZERO      NO HOURS YET                                 
                                                                                
         ZAP   TLINMB,MATCHED      SET MATCHED AMOUNT (BILL CUR)                
         AP    SVNETMB,TLINMB      AND ADD TO TOTAL MATCHED AMOUNT              
         SP    SVNETAB,TLINMB      AND SUBTRACT FROM AVAILABLE                  
         ZAP   TLICMB,COMMATCH                                                  
         AP    SVCOMMB,TLICMB                                                   
         SP    SVCOMAB,TLICMB                                                   
                                                                                
         TM    MINDS,MIFCB         CALCUALTE AGENCY NET AMOUNT                  
         BNO   BLDTS02                                                          
         ZAP   BODUB1,SVNETTA                                                   
         ZAP   BODUB2,SVNETTB                                                   
         ZAP   BOPL81(16),TLINMB   BOPL81(16)=BILLING AMOUNT                    
         MP    BOPL81(16),BODUB1             *AGENCY AVAILABLE                  
         SRP   BOPL81(16),2,0                *100                               
         DP    BOPL81(16),BODUB2             /BILLING AVAILABLE                 
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLINMA,BOPL81                                                    
         AP    SVNETMA,TLINMA                                                   
         SP    SVNETAA,TLINMA                                                   
                                                                                
         ZAP   BODUB1,SVCOMTA      CALCULATE AGENCY COM AMOUNT                  
         ZAP   BODUB2,SVCOMTB                                                   
         ZAP   BOPL81(16),TLICMB   BOPL81(16)=BILLING AMOUNT                    
         MP    BOPL81(16),BODUB1             *AGENCY AVAILABLE                  
         SRP   BOPL81(16),2,0                *100                               
         DP    BOPL81(16),BODUB2             /BILLING AVAILABLE                 
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLICMA,BOPL81                                                    
         AP    SVCOMMA,TLICMA                                                   
         SP    SVCOMAA,TLICMA                                                   
         B     BLDTS04                                                          
                                                                                
BLDTS02  ZAP   TLINMA,TLINMB       MAKE AGY AMOUNTS THE SAME IF NOT FC          
         AP    SVNETMA,TLINMA                                                   
         SP    SVNETAA,TLINMA                                                   
         ZAP   TLICMA,TLICMB                                                    
         AP    SVCOMMA,TLICMA                                                   
         SP    SVCOMAA,TLICMA                                                   
                                                                                
BLDTS04  XC    TLIX,TLIX                                                        
         XR    RE,RE               BUMP NUMBER OF TSARS ADDED                   
         ICM   RE,3,NUMTSAR                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,NUMTSAR                                                     
                                                                                
         GOTO1 ATSARIO,TSAADD                                                   
         BE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO SET UP THINGS IN TLSTD                                   *         
* NTRY: IO1=TRANSACTION RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
SETLST   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         TM    TRNRSTAT,TRNSREVS   TEST RECORD HAS BEEN REVERSED                
         BZ    SETLST10              (I.E. REVALUED)                            
         MVC   IOKEY(L'TRNKEY),TRNKEY                                           
         LA    R1,IOREAD+IOACCDIR                                               
         B     *+8                                                              
SETLST02 LA    R1,IOSEQ+IOACCDIR   FIND REVALUED RECORD                         
         GOTO1 AIO                                                              
         CLC   IOKEY(TRNKSBR-1-TRNRECD),TRNKEY                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    IOKEY+(TRNKSTA-TRNRECD),TRNSREVS                                 
         BO    SETLST02                                                         
         MVC   TLDA,IOKEY+(TRNKDA-TRNRECD)                                      
         MVC   IODAOVER,TLDA                                                    
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETLST10 L     RE,AGOPBLK                                                       
         ZAP   TLICOMR,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RE)                          
*                                  APRORATA CALL MAY CONVERT RECORD             
         MVC   BOWORK1(L'CSBILCUR),CSBILCUR  SAVE CSBILCUR                      
         OC    BILCUR,BILCUR                                                    
         BZ    *+10                                                             
         MVC   CSBILCUR,BILCUR     CSBILCUR USED BYAPRORATA                     
         GOTO1 APRORATA,BOPARM,AIO1,AGOPBLK,ACOM,MAEXC,LSPRATA,0                
         MVC   CSBILCUR,BOWORK1                                                 
         ZAP   TLINAA,PM$ANVBL                                                  
         ZAP   TLINAB,PM$ANVBL                                                  
         ZAP   TLIHRA,PM$HRVBL                                                  
         MVI   TLIINDS,0                                                        
         TM    MINDS,MIFCB         TEST IF FOREIGN CUR BILL                     
         BZ    SETLSTX                                                          
         ZAP   TLINAB,PM$FNVBL                                                  
* SVNETTA/B NOT SET YET                                                         
*        CP    TLINAA,SVNETTA      TEST AGENCY AMOUNT = MATCH AGENCY            
*        BNE   *+10                                                             
*        ZAP   TLINAB,SVNETTB      YES - SET FOREIGN AMOUNTS =                  
                                                                                
         LA    R3,TRNRFST                                                       
         USING AFCELD,R3           FIND AFCELD ON MATCHER                       
         XR    RF,RF                                                            
SETLST12 CLI   AFCEL,0                                                          
         BE    SETLSTX                                                          
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R3,RF,SETLST12                                                   
         CLC   AFCCURR,BILCUR      TEST SAME FOREIGN CURRENCY                   
         BNE   SETLSTX                                                          
         OI    TLIINDS,TLIISFC                                                  
         MVC   TLIX,AFCX                                                        
         CLC   TLIXEXC,EXCHRAT   TEST DIFFERENT EXCHANGE RATE                   
         BE    SETLSTX                                                          
         OI    TLIINDS,TLIIDEX                                                  
         CP    PP$AALLO,BCPZERO    TEST ANYTHING PENDING                        
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CLC   PP$AALLO(PM$VALS-PP$ACOMM),PP$ACOMM                              
         BNE   SETLSTX                                                          
         CP    PA$GRSBL,BCPZERO    TEST ANYTHING BILLED                         
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CP    PA$WOFAM,BCPZERO    TEST ANYTHING WRITTEN OFF                    
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CP    PA$XFRAM,BCPZERO    TEST ANYTHING TRANSFERRED                    
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CLI   P#FREVFI,C'N'       TEST REVALUE TO BE FORCED                    
         BE    SETLSTX                                                          
         OI    TLIINDS,TLIIREV     SET REVALUE REQUIRED                         
         DROP  R3                                                               
                                                                                
SETLSTX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
PFKRTN   CLI   BCPFKEY,PFKMUPDQ                                                 
         BE    UPDATE                                                           
         CLI   BCPFKEY,PFKMDFTQ                                                 
         BE    DRAFT                                                            
         DC    H'0'                                                             
                                                                                
UPDATE   GOTO1 PROC,MILIVE                                                      
         BNE   EXITN                                                            
         TM    MINDS,MIPOST                                                     
         BZ    UPDATE02                                                         
         L     RE,AGOPBLK          PRINTING CORRUPTS GO BLOCK                   
         XC    8(GOADM+8-GOBLOCK,RE),8(RE)                                      
         B     EXITY                                                            
UPDATE02 MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$MINUP)                                           
         B     EXITY                                                            
                                                                                
DRAFT    GOTO1 PROC,MIDRAFT                                                     
         BNE   EXITN                                                            
         TM    MINDS,MIPOST                                                     
         BZ    DRAFT02                                                          
         L     RE,AGOPBLK          PRINTING CORRUPTS GO BLOCK                   
         XC    8(GOADM+8-GOBLOCK,RE),8(RE)                                      
         B     EXITY                                                            
DRAFT02  MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NOPMM)                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS RECORDS FOR DRAFT/UPDATE                         *         
*                                                                     *         
* NTRY: R1 = MILIVE / MIDRAFT                                         *         
***********************************************************************         
         SPACE 1                                                                
PROC     NTR1  ,                                                                
         STC   R1,BOBYTE1                                                       
         OC    MINDS,BOBYTE1                                                    
                                                                                
                                                                                
         GOTO1 POSTINIT                                                         
                                                                                
         L     R1,AIOMCH                                                        
         BAS   RE,GETMTC                                                        
         GOTO1 GETPTA,TRNRFST-TRNRECD(R1)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AMCHPTA                                                       
         MVC   MCHPTA,0(R1)                                                     
MCH      USING PTAELD,MCHPTA                                                    
*                                  VAT A/C FOR INCOME A/C ATTRIBUTE             
         PUSH  USING                                                            
         USING VTCD,R1                                                          
         L     R1,AIO3                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUABIN                                                    
         TM    BCCPYST1,CPYSOROE   TEST ON OFFICE                               
         BNO   *+10                                                             
         MVC   VTCOFFC,CSOFFICE                                                 
         L     RE,AGOPBLK                                                       
         MVC   VTCTYPE,GOTAXCOD-GOBLOCK(RE)                                     
         MVC   VTCCOMF,ACOM                                                     
         MVC   VTCINVD,MCH.PTARBLDT                                             
         GOTO1 VVATICAN                                                         
         MVC   VATACCT,VTCACT+1                                                 
         DROP  R1                                                               
         POP   USING                                                            
                                                                                
         SP    MCH.PTANET,SVNETMA  ADJUST MATCH RECORD                          
         SP    MCH.PTARCOM,SVCOMMA                                              
         TM    MINDS,MIFCB                                                      
         BZ    *+16                                                             
         SP    MCH.PTANETF,SVNETMB                                              
         SP    MCH.PTARFCOM,SVCOMMB                                             
         CP    SVHOURT,BCPZERO     ADJUST HOURS PROPORTIONATELY                 
         BE    PROC10                                                           
         CP    SVNETTA,BCPZERO                                                  
         BE    PROC10                                                           
         ZAP   BOPL81(16),MCH.PTANET                                            
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),SVHOURT                                               
         DP    BOPL81(16),SVNETTA                                               
         SRP   BOPL81,64-2,5                                                    
         CVB   RE,BOPL81                                                        
         STH   RE,MCH.PTAHOURS                                                  
                                                                                
PROC10   L     R5,ALSVALS                                                       
         XC    TLNUM,TLNUM         START FOM FIRST TSAR                         
PROC12   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,NUMTSAR                                                    
         BH    PROC30                                                           
         GOTO1 ATSARIO,TSAGET                                                   
         CP    TLINMB,BCPZERO                                                   
         BE    PROC12                                                           
                                                                                
         MVC   IODAOVER,TLDA                                                    
         L     R1,=A(IOINV)                                                     
         LA    R1,IOGET+IOACCMST(R1)                                            
         TM    MINDS,MILIVE                                                     
         BZ    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    M.TLMINDS,TLMISEC   CONVERT IF SECONDARY CURRENCY                
         BZ    PROC14                                                           
         GOTO1 TOBSEC,BOPARM,AIOINV                                             
                                                                                
PROC14   BAS   RE,TSPOST           ADD/UPDATE ANY POSTING TSAR RECORDS          
                                                                                
         TM    MINDS,MIDRAFT       TEST DRAFTING ONLY                           
         BO    PROC12                                                           
                                                                                
         L     R1,AIOINV                                                        
         GOTO1 GETPTA,TRNRFST-TRNRECD(R1)                                       
         BNE   PROC18                                                           
         ST    R1,AINVPTA                                                       
         MVC   INVPTA,0(R1)                                                     
INV      USING PTAELD,INVPTA                                                    
         XR    RF,RF                                                            
         AP    INV.PTANET,TLINMA   ADD TO EXISTING AGENCY TOTALS                
         AP    INV.PTARCOM,TLICMA                                               
         ZAP   BODUB1,TLIHRM                                                    
         CVB   RE,BODUB1                                                        
         AH    RE,INV.PTAHOURS                                                  
         STH   RE,INV.PTAHOURS                                                  
         TM    MINDS,MIFCB                                                      
         BZ    PROC16                                                           
         AP    INV.PTANETF,TLINMB ADD TO EXISTING BILLING TOTALS                
         AP    INV.PTARFCOM,TLICMB                                              
PROC16   GOTO1 ATOBCHA,BOPARM,AIOINV,AINVPTA,INVPTA                             
         B     PROC26                                                           
         DROP  INV                                                              
                                                                                
INV      USING PTAELD,BOELEM                                                    
PROC18   IC    RF,MCH.PTALN        ADD NEW ELEMENT                              
         EX    RF,*+4                                                           
         MVC   INV.PTAELD(0),MCH.PTAELD                                         
         ZAP   INV.PTANET,TLINMA   SET AGENCY TOTALS FOR NEW ELEMENT            
         ZAP   INV.PTARCOM,TLICMA                                               
         ZAP   BODUB1,TLIHRM                                                    
         CVB   RE,BODUB1                                                        
         STH   RE,INV.PTAHOURS                                                  
         TM    MINDS,MIFCB                                                      
         BZ    PROC20                                                           
         ZAP   INV.PTANETF,TLINMB SET BILLING CURRENCY TOTALS                   
         ZAP   INV.PTARFCOM,TLICMB                                              
PROC20   GOTO1 ATOBCHA,BOPARM,AIOINV,0,INV.PTAELD                               
         DROP  INV                                                              
PROC26   TM    M.TLMINDS,TLMISEC   RE-CONVERT RECORD IF NEC.                    
         BZ    PROC28                                                           
         GOTO1 TOBBASE,BOPARM,AIOINV                                            
PROC28   L     R1,=A(IOINV)                                                     
         GOTO1 AIO,IOWRITE+IOACCMST(R1)                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PROC12                                                           
                                                                                
PROC30   BAS   RE,POST             MAKE POSTINGS FROM POSTING BUFFER            
                                                                                
         TM    MINDS,MIDRAFT       TEST DRAFT ONLY                              
         BO    PROCX                                                            
                                                                                
*                                    UPDATE TSAR RECORDS                        
         XC    TLNUM,TLNUM           START FOM 0                                
PROC32   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,NUMTSAR                                                    
         BH    PROC40                                                           
         GOTO1 ATSARIO,TSAGET                                                   
         CP    TLINMB,BCPZERO                                                   
         BE    PROC32                                                           
         ZAP   MNET,BCPZERO                                                     
         GOTO1 MATCHNET,1                                                       
         ZAP   MCOM,BCPZERO                                                     
         GOTO1 MATCHCOM,1                                                       
         GOTO1 ATSARIO,TSAPUT                                                   
         B     PROC32                                                           
********?? MAYBE DONT NEED TO CALC NEW TOTALS                                   
PROC40   ZAP   SVNETTA,SVNETAA     NEW TOTALS=AMOUNT AVAILABLE                  
         ZAP   SVNETTB,SVNETAB                                                  
         ZAP   SVCOMTA,SVCOMAA                                                  
         ZAP   SVCOMTB,SVCOMAB                                                  
         ZAP   SVHOURT,SVHOURA     TEST HOURS GONE NEGATIVE                     
         BNM   *+10                                                             
         ZAP   SVHOURT,BCPZERO                                                  
                                                                                
         CP    MCH.PTANET,BCPZERO  TEST FULLY MATCHED                           
         BNE   *+14                                                             
         CP    MCH.PTARCOM,BCPZERO                                              
         BE    PROC42                                                           
         GOTO1 ATOBCHA,BOPARM,AIOMCH,AMCHPTA,MCHPTA                             
         B     PROC50                                                           
                                                                                
PROC42   DS    0H                  FULLY MATCHED - DELETE ELEMENT               
         GOTO1 ATOBCHA,BOPARM,AIOMCH,AMCHPTA,0                                  
         DROP  MCH                                                              
                                                                                
         L     R3,AIOMCH           TEST ANY MORE ACTIVE PTAELS                  
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
PROC44   CLI   PTAEL,0                                                          
         BE    PROC48                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   PROC46                                                           
         OC    PTANET,PTANET                                                    
         BZ    PROC46                                                           
         CP    PTANET,BCPZERO                                                   
         BNE   PROC50                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   PROC46                                                           
         CP    PTARCOM,BCPZERO                                                  
         BNE   PROC50                                                           
PROC46   IC    RF,PTALN                                                         
         BXH   R3,RF,PROC44                                                     
         DROP  R3                                                               
*                                                                               
PROC48   L     R2,AIOMCH                                                        
         USING TRNRECD,R2                                                       
         CLC   ORDWC,TRNKWORK      TEST MATCHING AN ORDER                       
         BE    PROC50                                                           
         OI    MINDS,MIDEL         NO - DELETE RECORD                           
         OI    TRNRSTA,TRNSDELT    SET DELETE BIT ON FILE RECORD                
         MVC   IOKEY,TRNKEY        DELETE THE DIRECTORY RECORD                  
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         OI    IOKEY+(TRNKSTA-TRNRECD),TRNSDELT                                 
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         DROP  R2                                                               
PROC50   TM    M.TLMINDS,TLMISEC   RE-CONVERT RECORD IF NECC.                   
         BZ    PROC51                                                           
         GOTO1 TOBBASE,BOPARM,AIOMCH                                            
PROC51   L     R1,=A(IOMCH)                                                     
         GOTO1 AIO,IOWRITE+IOACCMST(R1)                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CLRTOTS                                                       
*                                                                               
         TM    MINDS,MIDEL         TEST ADVANCE ITEM DELETED                    
         BNO   PROCX                                                            
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO2                                                          
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         SR    R0,R0                                                            
PROC52   IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),ASTELQ                                                     
         BE    PROC54                                                           
         CLI   0(RE),JCBELQ                                                     
         BE    PROC56                                                           
         CLI   0(RE),0                                                          
         BE    PROC58                                                           
         B     PROC52                                                           
*                                                                               
PROC54   SR    RF,RF               PICK UP DRAFT COUNT                          
         ICM   RF,7,ASTDRAFT-ASTELD(RE)                                         
         BZ    *+6                 DO NOT SET NEGATIVE                          
         BCTR  RF,0                                                             
         STCM  RF,7,ASTDRAFT-ASTELD(RE)                                         
         B     PROC52                                                           
*                                                                               
PROC56   SR    RF,RF               PICK UP ADVANCE COUNT                        
         ICM   RF,3,JCBADV-JCBELD(RE)                                           
         BZ    *+6                 DO NOT SET NEGATIVE                          
         BCTR  RF,0                                                             
         STCM  RF,3,JCBADV-JCBELD(RE)                                           
         B     PROC52                                                           
*                                                                               
PROC58   GOTO1 AIO,IOWRITE+IOACCMST+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROCX    LA    RF,BASACTH                                                       
         ST    RF,FVADDR                                                        
         TM    MINDS,MIPOST                                                     
         BZ    EXITY                                                            
         GOTO1 ABLDTRN,BOPARM,(X'FF',POSTVALS)                                  
         L     R4,AREP                                                          
         USING REPD,R4                                                          
         MVI   REPACTN,REPACLO                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   FVOMTYP,GTMINF      REPORT SPOOLED MESSAGE                       
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1           
                                                                                
         B     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR POSTINGS                                             *         
***********************************************************************         
         SPACE 1                                                                
POSTINIT NTR1  ,                                                                
         MVC   POSTOFFC,CSOFFICE                                                
         MVC   POSTREF,M.TLMBILL                                                
         MVC   POSTDATE,M.TLMDATP                                               
         MVI   POSTTYPE,POSTMADJ                                                
         MVI   POSTMODE,POSTDFQ                                                 
         MVC   POSTCCPY,CUABIN                                                  
         TM    MINDS,MILIVE                                                     
         BZ    *+8                                                              
         MVI   POSTMODE,POSTLVQ                                                 
         TM    M.TLMINDS,TLMISEC   TELL BLDTRN IF WORKING IN 2ND CURR           
         BZ    *+8                                                              
         OI    POSTSTA2,POSTSEC                                                 
         MVI   XELBLK,0            INITIALISE EXTRA ELEMENT BLOCK               
         LA    RE,XELBLK                                                        
         ST    RE,POSTXTRA         AND SET ADDRESS                              
         XC    REUSE,REUSE         CLEAR NAME TABLE REUSE VALUE                 
         LA    R0,NAMTAB           CLEAR NAME TABLE                             
         LA    R1,NAMTABL*NAMTABN                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
POSTINIX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD/UPDATE TSAR POSTING RECORDS                          *         
***********************************************************************         
         SPACE 1                                                                
TSPOST   NTR1  ,                                                                
         CLI   P#XFRINC,C'Y'       TEST TRANSFERRING INCOME                     
         BNE   TSPOST20                                                         
         CLC   M.TLMINCAC,TLIINCAC TEST DIFFERENT INCOME ACCOUNTS               
         BE    TSPOST20                                                         
P        USING TLSTD,BOELEM                                                     
         XC    P.TLKEY,P.TLKEY     BUILD KEY FOR INCOME TSAR RECORD             
         MVI   P.TLKSES,TLKMPOQ                                                 
         MVI   P.TLKMPT,TLKMPINQ   INCOME                                       
         MVC   P.TLKMPACT,TLIINCAC                                              
         MVC   P.TLKMPWC,TLKWC                                                  
         GOTO1 ATSARIO,BOPARM,('TSARDH',P.TLSTD)                                
         BE    TSPOST02                                                         
         XC    P.TLKEY,P.TLKEY     NOT FOUND - ADD                              
         MVI   P.TLKSES,TLKMPOQ                                                 
         MVI   P.TLKMPT,TLKMPINQ   INCOME                                       
         MVC   P.TLKMPACT,TLIINCAC                                              
         MVC   P.TLKMPWC,TLKWC                                                  
         ZAP   P.TLDMNET,TLINMA                                                 
         ZAP   P.TLDMCOM,TLICMA                                                 
         MVC   P.TLRLEN,=AL2(TLMPOLNQ)                                          
         GOTO1 ATSARIO,BOPARM,('TSAADD',P.TLSTD)                                
         BE    TSPOST20                                                         
         DC    H'0'                                                             
                                                                                
TSPOST02 AP    P.TLDMNET,TLINMA    FOUND - ACCUMULATE                           
         AP    P.TLDMCOM,TLICMA                                                 
         GOTO1 ATSARIO,BOPARM,('TSAWRT',P.TLSTD)                                
         BE    TSPOST20                                                         
         DC    H'0'                                                             
                                                                                
TSPOST20 XC    SKULACT,SKULACT     CLEAR INCOME SUSPENSE ACCOUNT                
         L     R2,AIOINV                                                        
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         CLI   TRNTYPE,49          TEST A TIMESHEET                             
         BE    TSPOST24                                                         
TSPOST22 CLC   UL1R,TRNKULC        WAS 1R CONTRA USED                           
         BE    TSPOST24            CHECK FOR SK ATTRIBUTE                       
         CLC   UL1P,TRNKULC        ALSO IF CONTRA IS 1P                         
         BE    TSPOST24                                                         
         CLC   ULSK,TRNKULC        ELSE CHECK IF CONTRA IS SK                   
         BNE   TSPOST40                                                         
         MVC   SKULACT,TRNKULC     SO USE THAT                                  
         B     TSPOST32                                                         
                                                                                
TSPOST24 XR    RF,RF               TEST FOR SK ATTRIBUTE                        
         LA    R3,TRNRFST                                                       
         USING APEELD,R3                                                        
TSPOST26 CLI   APEEL,0                                                          
         BE    TSPOST40                                                         
         IC    RF,APELN            RF=THIS ELEMENT LENGTH                       
         CLI   APEEL,APEELQ        LOOK FOR ANALYSIS POINTER                    
         BNE   TSPOST28                                                         
         CLC   ULSK,APENACT                                                     
         BNE   TSPOST30                                                         
         MVC   SKULACT,BCSPACES                                                 
         SH    RF,=Y(APENACT+1-APEELD)                                          
         EX    RF,*+4                                                           
         MVC   SKULACT(0),APENACT                                               
         B     TSPOST32                                                         
                                                                                
         USING SPDEL,R3                                                         
TSPOST28 CLI   SPDEL,SPDELQ        OR SUBSIDIARY POSTING (TMS)                  
         BNE   TSPOST30                                                         
         CLC   ULSK,SPDACCS                                                     
         BNE   TSPOST30                                                         
         MVC   SKULACT,BCSPACES                                                 
         SH    RF,=Y(SPDACCS+1-SPDELD)                                          
         EX    RF,*+4                                                           
         MVC   SKULACT(0),SPDACCS                                               
         B     TSPOST32                                                         
                                                                                
TSPOST30 BXH   R3,RF,TSPOST26                                                   
         DROP  R3                                                               
                                                                                
TSPOST32 OC    SKULACT,SKULACT     TEST SUSPENSE ACCOUNT FOUND                  
         BZ    TSPOST40                                                         
         XC    P.TLKEY,P.TLKEY     BUILD KEY FOR SUSPENSE TSAR RECORD           
         MVI   P.TLKSES,TLKMPOQ                                                 
         MVI   P.TLKMPT,TLKMPSUQ   SUSPENSE                                     
         MVC   P.TLKMPULA,SKULACT                                               
         GOTO1 ATSARIO,BOPARM,('TSARDH',P.TLSTD)                                
         BE    TSPOST34                                                         
         XC    P.TLKEY,P.TLKEY     NOT FOUND - ADD                              
         MVI   P.TLKSES,TLKMPOQ                                                 
         MVI   P.TLKMPT,TLKMPSUQ   SUSPENSE                                     
         MVC   P.TLKMPULA,SKULACT                                               
         XC    P.TLKMPWC,P.TLKMPWC                                              
         ZAP   P.TLDMNET,TLINMA                                                 
         ZAP   P.TLDMCOM,TLICMA                                                 
         MVC   P.TLRLEN,=AL2(TLMPOLNQ)                                          
         GOTO1 ATSARIO,BOPARM,('TSAADD',P.TLSTD)                                
         BE    TSPOST40                                                         
         DC    H'0'                                                             
                                                                                
TSPOST34 AP    P.TLDMNET,TLINMA    FOUND - ACCUMULATE                           
         AP    P.TLDMCOM,TLICMA                                                 
         GOTO1 ATSARIO,BOPARM,('TSAWRT',P.TLSTD)                                
         BE    TSPOST40                                                         
         DC    H'0'                                                             
                                                                                
TSPOST40 DS    0H                                                               
         B     EXIT                                                             
                                                                                
         DROP  P                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MAKE POSTINGS FROM TSAR POSTING RECORDS                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PPRELD,BCCMPPRF                                                  
POST     NTR1  ,                                                                
         XC    TLIINCAC,TLIINCAC   CLEAR INCOME ACCOUNT                         
         XC    SKULACT,SKULACT     CLEAR SUSPENSE ACCOUNT                       
P        USING TLSTD,BOELEM                                                     
         XC    P.TLKEY,P.TLKEY     BUILD KEY FOR INCOME TSAR RECORDS            
         MVI   P.TLKSES,TLKMPOQ                                                 
         MVI   P.TLKMPT,TLKMPINQ   INCOME (FOLLOWED BY SUSPENSE)                
         GOTO1 ATSARIO,BOPARM,('TSARDH',P.TLSTD)                                
         BL    POST16                                                           
         B     POST04                                                           
POST02   GOTO1 ATSARIO,BOPARM,('TSANXT',P.TLSTD)                                
         BL    POST16                                                           
POST04   CLI   P.TLKSES,TLKMPOQ    MATCH POSTING                                
         BNE   POST16                                                           
         CLI   P.TLKMPT,TLKMPINQ   INCOME                                       
         BNE   POST16                                                           
         OC    TLIINCAC,TLIINCAC   TEST FIRST TIME                              
         BNZ   POST06                                                           
         MVC   TLIINCAC,P.TLKMPACT SET VALUES                                   
         ZAP   TLINMA,P.TLDMNET                                                 
         ZAP   TLICMA,P.TLDMCOM                                                 
         B     POST08                                                           
POST06   CLC   TLIINCAC,P.TLKMPACT TEST SAME ACCOUNT                            
         BNE   POST16                                                           
         AP    TLINMA,P.TLDMNET    ACCUMULATE VALUES                            
         AP    TLICMA,P.TLDMCOM                                                 
POST08   LA    R1,IWCTAB                                                        
         USING IWCTABD,R1                                                       
         LA    R0,IWCTABN                                                       
POST10   OC    IWCTWC,IWCTWC       NEW ENTRY                                    
         BZ    POST12                                                           
         CLC   IWCTWC,P.TLKMPWC    TEST MATCHING ENTRY                          
         BE    POST14                                                           
         LA    R1,IWCTABL(R1)                                                   
         BCT   R0,POST10                                                        
         DC    H'0'                INCOME BY W/C TABLE FULL                     
POST12   MVC   IWCTWC,P.TLKMPWC    ESTABLISH NEW ENTRY                          
         ZAP   IWCTNET,P.TLDMNET                                                
         ZAP   IWCTCOM,P.TLDMCOM                                                
         B     POST02              GET NEXT POSTING RECORD                      
POST14   AP    IWCTNET,P.TLDMNET                                                
         AP    IWCTCOM,P.TLDMCOM                                                
         B     POST02              GET NEXT POSTING RECORD                      
                                                                                
POST16   OC    TLIINCAC,TLIINCAC   ANYTHING FOUND                               
         BZ    POST24              SUSPENSE POSTINGS                            
         ZAP   BODUB3,BCPZERO      (CLEAR CONTROL TEST NET)                     
         ZAP   BODUB4,BCPZERO      (CLEAR CONTROL TEST COMMISSION)              
         LA    R2,XELBLK                                                        
         USING SCIELD,R2                                                        
         MVI   SCIEL,SCIELQ        GROSS SCIEL                                  
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS    TAKE OUT ORIGINAL GROSS                      
         ZAP   SCIAMNT,BCPZERO                                                  
         SP    SCIAMNT,TLINMA      -NET                                         
         SP    SCIAMNT,TLICMA      -COMMISSION                                  
         LA    R2,SCILN1Q(R2)                                                   
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITANAL    TAKE OUT ORIGINAL INCOME BY W/C              
         ZAP   SCIAMNT,BCPZERO                                                  
         SP    SCIAMNT,TLINMA      -NET                                         
         ZAP   SCIADMN,BCPZERO                                                  
         SP    SCIADMN,TLICMA      -COMMISSION                                  
         MVC   SCISUBTY,BCSPACES                                                
         MVC   SCISUBWC,M.TLKWC    MATCH WORKCODE                               
         MVI   SCIELD+SCILN3Q,0                                                 
I        USING TRNKEY,M.TLMINCAC                                                
         GOTO1 BLDTRN,BTPARM,=C'C-',                                   *        
               I.TRNKUNT,I.TRNKACT,0,                                  *        
               BCCPYPRD,BCPROCOD,BCPRONAM,TLICMA                                
*                                                                               
         LA    R2,XELBLK                                                        
         MVI   SCIEL,SCIELQ        GROSS SCIEL                                  
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,TLINMA      NET +                                        
         AP    SCIAMNT,TLICMA      COMMISSION                                   
         LA    R2,SCILN1Q(R2)                                                   
         LA    R1,IWCTAB           GET INCOME BY WORKCODE                       
         USING IWCTABD,R1                                                       
         LA    R0,IWCTABN                                                       
POST18   MVI   SCIEL,0             NEW EOR EACH TIME                            
         OC    IWCTWC,IWCTWC                                                    
         BZ    POST20                                                           
         MVI   SCIEL,SCIELQ        BUILD INCOME BY WORKCODE BUCKETS             
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITANAL                                                 
         ZAP   SCIAMNT,IWCTNET     NET                                          
         ZAP   SCIADMN,IWCTCOM     COMMISSION                                   
         MVC   SCISUBTY,BCSPACES                                                
         MVC   SCISUBWC,IWCTWC     SET INVOICE WORKCODE                         
         AP    BODUB3,IWCTNET      (CONTROL TEST NET)                           
         AP    BODUB4,IWCTCOM      (CONTROL TEST COMMISSION)                    
         XC    IWCTABD(IWCTABL),IWCTABD CLEAR THIS ENTRY                        
         LA    R2,SCILN3Q(R2)                                                   
         LA    R1,IWCTABL(R1)                                                   
         BCT   R0,POST18                                                        
         MVI   SCIEL,0             SET EOR                                      
I        USING TRNKEY,TLIINCAC                                                  
POST20   GOTO1 BLDTRN,BTPARM,=C'C+',                                   *        
               I.TRNKUNT,I.TRNKACT,0,                                  *        
               ,,,TLICMA                                                        
         DROP  I                                                                
         CP    BODUB3,TLINMA       CONTROL TEST - NET                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    BODUB4,TLICMA       CONTROL TEST - COMMISSION                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETCOST,TLIINCAC                                                 
         CLC   MANC,MATCHANC       TEST CHANGE OF COSTING CODES                 
         BE    POST24                                                           
         CLC   MATCHANC,BCSPACES                                                
         BNH   POST22                                                           
         ZAP   BODUB1,TLINMA                                                    
         AP    BODUB1,TLICMA                                                    
         GOTO1 BLDTRN,BTPARM,=C'D-',                                   *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL11,MATCHANC,0,BODUB1                                           
         ZAP   BODUB1,TLINMA                                                    
         AP    BODUB1,TLICMA                                                    
         GOTO1 (RF),(R1),=C'C-',                                       *        
               UL11,MATCHANC,0,                                        *        
               PPRCOSTU,PPRCOSTA,0,BODUB1                                       
         GOTO1 (RF),(R1),=C'D-',                                       *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL12,MATCHANC,0,TLICMA                                           
         GOTO1 (RF),(R1),=C'C-',                                       *        
               UL12,MATCHANC,0,                                        *        
               PPRCOSTU,PPRCOSTA,0,TLICMA                                       
                                                                                
POST22   CLC   MANC,BCSPACES                                                    
         BNH   POST24                                                           
         ZAP   BODUB1,TLINMA                                                    
         AP    BODUB1,TLICMA                                                    
         GOTO1 BLDTRN,BTPARM,=C'D+',                                   *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL11,MANC,0,BODUB1                                               
         ZAP   BODUB1,TLINMA                                                    
         AP    BODUB1,TLICMA                                                    
         GOTO1 (RF),(R1),=C'C+',                                       *        
               UL11,MANC,0,                                            *        
               PPRCOSTU,PPRCOSTA,0,BODUB1                                       
         GOTO1 (RF),(R1),=C'D+',                                       *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL12,MANC,0,TLICMA                                               
         GOTO1 (RF),(R1),=C'C+',                                       *        
               UL12,MANC,0,                                            *        
               PPRCOSTU,PPRCOSTA,0,TLICMA                                       
                                                                                
POST24   XC    TLIINCAC,TLIINCAC   CLEAR INCOME ACCOUNT                         
         L     R1,ATSABLK          TEST FINISHED WITH POSTINGS                  
         TM    TSERRS-TSARD(R1),TSEEOF                                          
         BO    POST30                                                           
         CLI   P.TLKSES,TLKMPOQ    TEST FINISHED WITH POSTINGS                  
         BNE   POST30                                                           
         CLI   P.TLKMPT,TLKMPINQ   TEST INCOME POSTING                          
         BE    POST04              ACCUMULATE THIS POSTING RECORD               
         CLI   P.TLKMPT,TLKMPSUQ   TEST SUSPENSE POSTING                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    SKULACT,SKULACT     TEST FIRST TIME                              
         BNZ   POST28                                                           
         MVC   SKULACT,P.TLKMPULA  SET VALUES                                   
         ZAP   TLINMA,P.TLDMNET                                                 
         ZAP   TLICMA,P.TLDMCOM                                                 
         B     POST02              GET NEXT POSTING RECORD                      
POST28   CLC   SKULACT,P.TLKMPULA  TEST SAME ACCOUNT                            
         BNE   POST30                                                           
         AP    TLINMA,P.TLDMNET    ACCUMULATE VALUES                            
         AP    TLICMA,P.TLDMCOM                                                 
         B     POST02              GET NEXT POSTING RECORD                      
*                                                                               
POST30   OC    SKULACT,SKULACT     TEST ANY SUSPENSE TO POST                    
         BZ    POST32                                                           
         GOTO1 BLDTRN,BTPARM,=C'D+',                                   *        
               SKUL,SKACT,0,                                           *        
               BCCPYPRD,BCJOBCOD,BCJOBNAM,TLINMA                                
         GOTO1 (RF),(R1),=C'C+',                                       *        
               ULSI,SKACT,0,                                           *        
               BCCPYPRD,BCPROCOD,BCPRONAM,TLINMA                                
                                                                                
         MVC   BOWORK1(L'TRNKCPY),CUABIN                                        
         MVC   BOWORK1+L'TRNKCPY(L'POSTACT),POSTACT                             
         GOTO1 GETCOST,BOWORK1                                                  
         CLC   MANC,BCSPACES                                                    
         BNH   POST32                                                           
         GOTO1 BLDTRN,BTPARM,=C'D+',                                   *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL12,MANC,0,TLINMA                                               
         GOTO1 (RF),(R1),=C'C+',                                       *        
               UL12,MANC,0,                                            *        
               PPRCOSTU,PPRCOSTA,0,TLINMA                                       
*                                                                               
POST32   XC    SKULACT,SKULACT     CLEAR SUSPENSE ACCOUNT                       
*                                                                               
         L     R1,ATSABLK          TEST FINISHED WITH POSTINGS                  
         TM    TSERRS-TSARD(R1),TSEEOF                                          
         BO    POST34                                                           
         CLI   P.TLKSES,TLKMPOQ                                                 
         BNE   POST34                                                           
         CLI   P.TLKMPT,TLKMPSUQ   TEST SUSPENSE POSTING                        
         BE    POST04              ACCUMULATE THIS POSTING RECORD               
         DC    H'0'                                                             
*                                                                               
POST34   B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL BLDTRN                                              *         
*                                                                     *         
* NTRY: P1 = A(HALF WORD) - BYTE 1 = 'C'REDIT / 'D'EBIT               *         
*                           BYTE 2 = '+' / '-'                        *         
*       P2 = A(ACCOUNT UNIT/LEDGER)                                   *         
*       P3 = A(ACCOUNT CODE)                                          *         
*       P4 = A(ACCOUNT NAME) / 0                                      *         
*       P5 = A(CONTRA UNIT/LEDGER)                                    *         
*       P6 = A(CONTRA CODE)                                           *         
*       P7 = A(CONTRA NAME) / 0                                       *         
*       P8 = A(PL8 POSTING AMOUNT)                                    *         
***********************************************************************         
         SPACE 1                                                                
BLDTRN   NTR1  ,                                                                
         L     RF,BTPARM1                                                       
         MVC   BTHALF,0(RF)                                                     
         LM    R2,R3,BTPARM2       SET ACCOUNT DETAILS                          
         MVC   POSTAUL,0(R2)                                                    
         MVC   POSTAACT,0(R3)                                                   
         CLI   CUCTRY,CTRYGER      TEST THIS IS A GERMAN USER                   
         BNE   BTRN01                                                           
         CLC   POSTAUL,ULSI        TEST INCOME POSTING                          
         BNE   BTRN01                                                           
         LA    RF,XELBLK           ADD VAT ATTRIBUTE                            
         SR    R0,R0                                                            
BTRN00   CLI   0(RF),0                                                          
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     BTRN00                                                           
         USING APEELD,RF                                                        
         MVI   APEEL,APEELQ          SET VAT ATTRIBUTE                          
         MVI   APENUM,1                                                         
         MVC   APENACT,VATACCT                                                  
         MVI   APENACT+(L'APENACT+1),0                                          
         LA    R1,APENACT+L'APENACT-1                                           
         LA    RE,APELN1Q(RF)      GET LENGTH OF ACCOUNT                        
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         MVI   0(R1),0                                                          
         BCT   R1,*-12                                                          
         LA    R1,1(R1)                                                         
         SR    R1,RE                                                            
         STC   R1,APENLEN                                                       
         LA    R1,APELN1Q(R1)                                                   
         STC   R1,APELN                                                         
         DROP  RF                                                               
BTRN01   ICM   R2,15,BTPARM4                                                    
         BZ    *+14                                                             
         MVC   POSTACTN,0(R2)                                                   
         B     BTRN02                                                           
         GOTO1 GETNAM,BOPARM,POSTAUL,POSTACTN                                   
                                                                                
BTRN02   LM    R2,R3,BTPARM5       SET CONTRA DETAILS                           
         MVC   POSTCUL,0(R2)                                                    
         MVC   POSTCACT,0(R3)                                                   
         ICM   R2,15,BTPARM7                                                    
         BZ    *+14                                                             
         MVC   POSTCACN,0(R2)                                                   
         B     BTRN04                                                           
         GOTO1 GETNAM,BOPARM,POSTCUL,POSTCACN                                   
*                                                                               
BTRN04   L     RF,BTPARM8                                                       
         ZAP   BTAMNT,0(8,RF)                                                   
         TM    MINDS,MIPOST        TEST ANY POSTINGS MADE YET                   
         BNZ   BTRN10                                                           
*??      GOTO1 VALTRN     BATCH REF/NOA VALIDATED ELSEWHERE                     
*        BE    BTRN06                                                           
*        TM    MINDS,MILIVE                                                     
*        BZ    *+8                                                              
*        OI    CSINDSG1,CSINDUNW                                                
*        L     RD,BCSVRD           RETURN TO ROOT                               
*        L     RD,8(RD)                                                         
*        B     EXIT                                                             
*                                                                               
BTRN06   OI    MINDS,MIPOST                                                     
         GOTO1 PRTINIT             INITIALIZE PRINTING                          
         MVC   POSTBTRF,OSREF#                                                  
         MVC   POSTBTMC,OSMONC                                                  
         TM    MINDS,MILIVE                                                     
         BZ    BTRN10                                                           
         GOTO1 AADDOBH,BOPARM,('POSTMADJ',OSREF#),0                             
         GOTO1 AINIADT                                                          
*                                                                               
BTRN10   MVI   POSTSTAT,0                                                       
         CLI   BTHALF,C'D'                                                      
         BNE   *+8                                                              
         MVI   POSTSTAT,TRNSDR                                                  
         ZAP   BODUB1,BTAMNT                                                    
         CLI   BTHALF+1,C'-'                                                    
         BNE   *+10                                                             
         MP    BODUB1,PMINUS1                                                   
         ZAP   POSTAMNT,BODUB1                                                  
*                                                                               
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR PRINTING                                             *         
***********************************************************************         
         SPACE 1                                                                
PRTINIT  NTR1  ,                                                                
         L     R4,AREP                                                          
         USING REPD,R4                                                          
*                                                                               
         LA    R0,REPSPEC          INITIALIZE FOR PRINTING                      
         ST    R0,REPAPHS                                                       
         LA    RE,=C'AMD'          ??                                           
         CLI   BCPFKEY,PFKRLIVQ                                                 
         BNE   *+8                                                              
         LA    RE,=C'AMV'          ??                                           
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID                                                 
         BNZ   *+10                                                             
         MVC   REPSUBID,0(RE)                                                   
         MVCDD REPDESC,AC#DRMAT                                                 
         MVI   REPSUBPG,REPSDPST                                                
         TM    MINDS,MILIVE                                                     
         BZ    *+14                                                             
         MVCDD REPDESC,AC#LIMAT                                                 
         MVI   REPSUBPG,REPSLPST                                                
         GOTO1 VDICTAT,BOPARM,C'SL  ',REPDESC                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTO1 (RF),(R1)                                                        
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
         LA    R3,REPH3                                                         
         USING HLINE,R3                                                         
         MVC   HDATA1(L'CUAALF),CUAALF                                          
         L     RF,AGOPBLK                                                       
         L     RF,GOACOMP-GOBLOCKD(RF)                                          
         GOTO1 AGETELS,BOPARM,ACCORFST(RF)                                      
         MVC   HNAME1,ACNAME                                                    
         MVC   HDATA2(L'BCUSERID),BCUSERID                                      
*                                                                               
         LA    R3,REPH4                                                         
         MVC   HDATA1(L'CSOFFICE),CSOFFICE                                      
         LA    R2,IOKEY                                                         
         USING OGRRECD,R2                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(L'OGRKUNT+L'OGRKLDG),BCCPYPRD                            
         MVC   OGRKOFC,CSOFFICE                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         L     R2,AIO1                                                          
         GOTO1 AGETELS,BOPARM,OGRRFST                                           
         MVC   HNAME1,ACNAME                                                    
         DROP  R2                                                               
         MVC   HDATA2(L'CSBPID),CSBPID                                          
*                                                                               
         LA    R3,REPH5                                                         
         MVC   HDATA1,BCCLICOD                                                  
         MVC   HNAME1,BCCLINAM                                                  
*                                                                               
         LA    R3,REPH6                                                         
         XR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RF,L'BCPROCOD-1                                                  
         SR    RF,RE                                                            
         LA    RE,BCPROCOD(RE)                                                  
         EX    RF,*+4                                                           
         MVC   HDATA1(0),0(RE)                                                  
*        MVC   HNAME1,BCPRONAM                                                  
                                                                                
         LA    R3,REPH7                                                         
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RF,L'BCJOBCOD-1                                                  
         SR    RF,RE                                                            
         LA    RE,BCJOBCOD(RE)                                                  
         EX    RF,*+4                                                           
         MVC   HDATA1(0),0(RE)                                                  
         MVC   HNAME1,BCJOBNAM                                                  
         DROP  R3                                                               
*                                                                               
PRTINITX B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET COSTING CODE FROM INCOME ACCOUNT                     *         
*                                                                     *         
* NTRY: R1 = A(INCOME ACCOUNT)                                        *         
* EXIT: MINCAC = INOCME ACCOUNT                                       *         
*       MANC   = ANALYSIS COSTING ACCOUNT                             *         
***********************************************************************         
         SPACE 1                                                                
GETCOST  NTR1  ,                                                                
         CLC   MINCAC,0(R1)        TEST ALREADY HAVE INCOME ACCOUNT             
         BE    EXIT                                                             
         MVC   MINCAC,0(R1)                                                     
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'MINCAC),MINCAC                                           
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MANC,BCSPACES                                                    
         MVC   MANC(L'ACCOST),ACCOST                                            
*&&US                                                                           
         CLC   MANC,BCSPACES                                                    
         BH    GCOST04                                                          
         ICM   R3,15,ACASPA                                                     
         BZ    GCOST04                                                          
         USING SPAELD,R3                                                        
         XR    RF,RF                                                            
GCOST02  CLI   SPAEL,SPAELQ                                                     
         BNE   GCOST04                                                          
         CLI   SPATYPE,SPATANAL                                                 
         BE    *+12                                                             
         IC    RF,SPALN                                                         
         BXH   R3,RF,GCOST02                                                    
         MVC   MANC,SPAAANAL                                                    
         DROP  R3                                                               
*&&                                                                             
GCOST04  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ACCOUNT NAME                                         *         
*                                                                     *         
* NTRY: P1=A(ACCOUNT CODE)                                            *         
*       P2=A(ACCOUNT NAME)                                            *         
***********************************************************************         
         SPACE 1                                                                
GETNAM   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LA    R4,NAMTAB           LOOK FOR NAME IN TABLE                       
         LA    R0,NAMTABN                                                       
         USING NAMTABD,R4                                                       
GETNAM02 OC    NAMTULA,NAMTULA                                                  
         BZ    GETNAM04                                                         
         CLC   NAMTULA,0(R2)                                                    
         BE    GETNAM10                                                         
         LA    R4,NAMTABL(R4)                                                   
         BCT   R0,GETNAM02                                                      
         LH    R1,REUSE            TABLE FULL - RE-USE AN ENTRY                 
         LA    R4,NAMTAB(R1)                                                    
         LA    R1,L'NAMTAB(R1)                                                  
         CH    R1,=Y(NAMTABN)      TEST RE-USED ALL ENTRIES                     
         BL    *+6                                                              
         SR    R1,R1               CLEAR RE-USE VALUE                           
         STH   R1,REUSE            SET TO REUSE NEXT ENTRY                      
*                                                                               
GETNAM04 MVC   NAMTULA,0(R2)       NEW NAME TABLE ENTRY                         
         MVC   NAMTNAM,BCSPACES                                                 
                                                                                
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,CUABIN                                                 
         MVC   K.TRNKUNT(L'NAMTULA),NAMTULA                                     
         DROP  K                                                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R1,RF,*-12                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   NAMTNAM(0),NAMEREC                                               
         DROP  R1                                                               
                                                                                
GETNAM10 MVC   0(L'NAMTNAM,R3),NAMTNAM                                          
*                                                                               
GETNAMX  DS    0H                                                               
         B     EXITY                                                            
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET MATCH RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETMTC   NTR1  ,                                                                
         MVC   IODAOVER,M.TLDA                                                  
         L     R1,=A(IOMCH)                                                     
         LA    R1,IOACCMST+IOGET(R1)                                            
         TM    MINDS,MILIVE        TEST UPDATING                                
         BNO   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    M.TLMINDS,TLMISEC   CONVERT RECORD IF SECONDARY CURR.            
         BZ    GETMTC02                                                         
         GOTO1 TOBSEC,BOPARM,AIOMCH                                             
*                                                                               
GETMTC02 DS    0H                                                               
         ZAP   SVNETTB,BCPZERO                                                  
         ZAP   SVCOMTB,BCPZERO                                                  
         ZAP   SVHOURT,BCPZERO                                                  
         ZAP   SVNETTA,BCPZERO                                                  
         ZAP   SVCOMTA,BCPZERO                                                  
*                                                                               
         L     R3,AIOMCH                                                        
         GOTO1 GETCOST,M.TLMINCAC                                               
         MVC   MATCHANC,MANC                                                    
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         TM    M.TLMINDS,TLMIORD   TEST AN ORDER                                
         BZ    GETMTC04            YES - MAY BE PARTLY MATCHED                  
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO1                                  
         L     R3,AIO1                                                          
GETMTC04 GOTO1 GETPTA,(R3)                                                      
         BNE   GETMTCX                                                          
         USING PTAELD,R1                                                        
         ZAP   SVNETTA,PTANET                                                   
         ZAP   SVCOMTA,PTARCOM                                                  
         ZAP   SVNETTB,PTANET                                                   
         ZAP   SVCOMTB,PTARCOM                                                  
         LH    RE,PTAHOURS                                                      
         CVD   RE,BODUB1                                                        
         ZAP   SVHOURT,BODUB1                                                   
         TM    MINDS,MIFCB                                                      
         BZ    GETMTCX                                                          
         ZAP   SVNETTB,PTANETF                                                  
         ZAP   SVCOMTB,PTARFCOM                                                 
         DROP  R1                                                               
*                                                                               
GETMTCX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PTA ELEMENT FOR BILL                                 *         
*                                                                     *         
* NTRY: R1 = A(FIRST ELEMENT)                                         *         
* EXIT: R1 = A(PTA ELEMENT)                                           *         
***********************************************************************         
         SPACE 1                                                                
GETPTA   XR    RF,RF                                                            
         USING PTAELD,R1                                                        
GPTA02   CLI   PTAEL,0                                                          
         BE    GETPTAN                                                          
         CLI   PTAEL,PTAELQ        FIND UPDATED ALLOCATION                      
         BNE   GPTA08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GPTA08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    GPTA08                                                           
         CLC   PTARBLNO,M.TLMBILL  MATCH ON BILL #                              
         BNE   GPTA08                                                           
         CLC   PTARBLDT,M.TLMDATC  MATCH ON BILL DATE                           
         BE    GETPTAY                                                          
GPTA08   IC    RF,PTALN                                                         
         BXH   R1,RF,GPTA02                                                     
*                                                                               
GETPTAY  CR    RE,RE               SET CC = EQUAL                               
         BR    RE                                                               
*                                                                               
GETPTAN  XR    R1,R1                                                            
         LTR   RE,RE               SET CC = NO                                  
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MATCH NET AMOUNT                                                    *         
*                                                                     *         
* NTRY:      R1 = 0 TO UPDATE MATCHED AND AVAILABLE TOTALS            *         
*            R1 = NON-0 TO UPDATE MATCHED TOTALS ONLY                 *         
*          MNET = NET AMOUNT BILLED IN BILLING CURRENCY               *         
* EXIT: TLINMB = NET AMOUNT BILLED IN BILLING CURRENCY                *         
*       TLINMA = NET AMOUNT BILLED IN AGENCY CURRENCY                 *         
*       SVNETAB,SVNETMB,SVNETAA,SVNETMA TOTALS ARE UPDATED            *         
*       SVHOURA, SVHOURM TOTALS ARE UPDATED                           *         
***********************************************************************         
         SPACE 1                                                                
MATCHNET NTR1  ,                                                                
         LTR   R2,R1                                                            
         BNZ   MNET02                                                           
         AP    SVNETAB,TLINMB      ADJUST TOTALS FOR OLD VALUES                 
         AP    SVNETAA,TLINMA                                                   
         AP    SVHOURA,TLIHRM                                                   
MNET02   SP    SVNETMA,TLINMA                                                   
         SP    SVNETMB,TLINMB                                                   
         SP    SVHOURM,TLIHRM                                                   
*                                                                               
         ZAP   TLINMB,MNET         UPDATE BILLING CURRENCY                      
         ZAP   TLINMA,TLINMB       TEST ZERO                                    
         BZ    MNET06                                                           
         TM    MINDS,MIFCB         TEST SAME CURRENCY                           
         BZ    MNET06                                                           
*                                                                               
         CP    TLINMB,TLINAB       IF INVOICE FULLY MATCHED (BILLING)           
         BNE   *+14                  FULLY MATCH INVIOCE (CURRENCY)             
         ZAP   TLINMA,TLINAA                                                    
         B     MNET04                                                           
*                                                                               
         ZAP   BODUB1,SVNETAA                                                   
         ZAP   BODUB2,SVNETAB                                                   
         BNZ   *+16                                                             
         ZAP   BODUB1,SVNETTA                                                   
         ZAP   BODUB2,SVNETTB                                                   
*                                                                               
         ZAP   BOPL81(16),TLINMB   BOPL81(16)=BILLING AMOUNT                    
         MP    BOPL81(16),BODUB1             *AGENCY AVAILABLE                  
         SRP   BOPL81(16),2,0                *100                               
         DP    BOPL81(16),BODUB2             /BILLING AVAILABLE                 
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLINMA,BOPL81                                                    
         BNE   *+10                IF MATCHED(AGENCY)=ZERO                      
         ZAP   TLINMB,BCPZERO        MATCHED(BILLING)=ZERO                      
*                                                                               
MNET04   CP    TLINMA,SVNETAA      IF MATCHED(AGENCY) > AVAILABLE               
         BNH   *+10                  MATCHED(AGENCY)=AVAILABLE                  
         ZAP   TLINMA,SVNETAA                                                   
*                                                                               
MNET06   CP    SVNETAB,TLINMB      IF BILLING FULLY MATCHED                     
         BNE   MNET08                                                           
         CP    SVNETAA,TLINMA        IF AGENCY NOT FULLY MATCHED                
         BE    MNET08                                                           
         ZAP   TLINMA,SVNETAA          FULLY MATCH AGENCY                       
*                                                                               
MNET08   DS    0H                  ESTABLISH HOURS IN PROPORTION TO NET         
         CP    TLIHRA,BCPZERO                                                   
         BE    MNET10                                                           
         CP    TLINAA,BCPZERO                                                   
         BE    MNET10                                                           
         ZAP   BOPL81(16),TLINMA             AGENCY NET MATCHED                 
         SRP   BOPL81(16),2,0                *100                               
         MP    BOPL81(16),TLIHRA             *HOURS AVAILABLE                   
         DP    BOPL81(16),TLINAA             /AGENCY NET AVAILABLE              
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLIHRM,BOPL81                                                    
*                                                                               
MNET10   LTR   R2,R2                                                            
         BNZ   MNET12                                                           
         SP    SVNETAB,TLINMB      ADJUST TOTALS FOR NEW VALUES                 
         SP    SVNETAA,TLINMA                                                   
         SP    SVHOURA,TLIHRM                                                   
MNET12   AP    SVNETMA,TLINMA                                                   
         AP    SVNETMB,TLINMB                                                   
         AP    SVHOURM,TLIHRM                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MATCH COMMISSION AMOUNT                                             *         
*                                                                     *         
* NTRY:      R1 = 0 TO UPDATE MATCHED AND AVAILABLE TOTALS            *         
*            R1 = NON-0 TO UPDATE MATCHED TOTALS ONLY                 *         
*          MCOM =  COMMISSION AMOUNT BILLED IN BILLING CURRENCY       *         
* EXIT: TLICMB = COMMISSION AMOUNT BILLED IN BILLING CURRENCY         *         
*       TLICMA = COMMISSION AMOUNT BILLED IN AGENCY CURRENCY          *         
*       SVCOMAB,SVCOMMB,SVCOMAA,SVCOMMA TOTALS ARE UPDATED            *         
***********************************************************************         
         SPACE 1                                                                
MATCHCOM NTR1  ,                                                                
         LTR   R2,R1                                                            
         BNZ   *+16                                                             
         AP    SVCOMAB,TLICMB      ADJUST TOTALS FOR OLD VALUES                 
         AP    SVCOMAA,TLICMA                                                   
         SP    SVCOMMA,TLICMA                                                   
         SP    SVCOMMB,TLICMB                                                   
*                                                                               
         ZAP   TLICMB,MCOM         UPDATE BILLING CURRENCY                      
         ZAP   TLICMA,TLICMB       TEST ZERO                                    
         BZ    MCOM02                                                           
         TM    MINDS,MIFCB         TEST SAME CURRENCY                           
         BZ    MCOM02                                                           
*                                                                               
         ZAP   BODUB1,SVCOMAA                                                   
         ZAP   BODUB2,SVCOMAB                                                   
         BNZ   *+16                                                             
         ZAP   BODUB1,SVCOMTA                                                   
         ZAP   BODUB2,SVCOMTB                                                   
*                                                                               
         ZAP   BOPL81(16),TLICMB   BOPL81(16)=BILLING AMOUNT                    
         MP    BOPL81(16),BODUB1             *AGENCY AVAILABLE                  
         SRP   BOPL81(16),2,0                *100                               
         DP    BOPL81(16),BODUB2             /BILLING AVAILABLE                 
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLICMA,BOPL81                                                    
         BNE   *+10                IF MATCHED(AGENCY)=ZERO                      
         ZAP   TLICMB,BCPZERO        MATCHED(BILLING)=ZERO                      
*                                                                               
MCOM02   CP    SVCOMAB,TLICMB      IF BILLING FULLY MATCHED                     
         BNE   MCOM04                                                           
         CP    SVCOMAA,TLICMA        IF AGENCY NOT FULLY MATCHED                
         BE    MCOM04                                                           
         ZAP   TLICMA,SVCOMAA          FULLY MATCH AGENCY                       
*                                                                               
MCOM04   LTR   R2,R2                                                            
         BNZ   *+16                                                             
         SP    SVCOMAB,TLICMB      ADJUST TOTALS FOR NEW VALUES                 
         SP    SVCOMAA,TLICMA                                                   
         AP    SVCOMMB,TLICMB                                                   
         AP    SVCOMMA,TLICMA                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT RECORD VIA TOBACCO TO SECONDARY CURRENCY         *         
* NTRY: P1 = A(RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
TOBSEC   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         GOTO1 VTOBACCO,BOPARM,('TOBAACVS',TOBCUR),(R2),ACOM,0,0                
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CONVERT RECORD VIA TOBACCO BACK TO BASE CURRENCY         *         
* NTRY: P1 = A(RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
TOBBASE  NTR1  ,                                                                
         L     R2,0(R1)                                                         
         GOTO1 VTOBACCO,BOPARM,('TOBAACVB',TOBCUR),(R2),ACOM,0,0                
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
***********************************************************************         
* VALIDATE THE MONTH FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALMON   NTR1  ,                                                                
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         LA    R3,BOWORK1                                                       
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,BOPARM,(DATALEN+1,DATA),(79,ACOM),             *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         MVC   OSMONP,BMOMOSP                                                   
         MVC   OSMONC,BMOMOSC                                                   
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
*                                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR TOTALS                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLRTOTS  ZAP   SVNETMA,BCPZERO                                                  
         ZAP   SVNETMB,BCPZERO                                                  
         ZAP   SVHOURM,BCPZERO                                                  
         ZAP   SVNETAA,SVNETTA                                                  
         ZAP   SVNETAB,SVNETTB                                                  
         ZAP   SVHOURA,SVHOURT                                                  
         ZAP   SVCOMMA,BCPZERO                                                  
         ZAP   SVCOMMB,BCPZERO                                                  
         ZAP   SVCOMAA,SVCOMTA                                                  
         ZAP   SVCOMAB,SVCOMTB                                                  
         ZAP   SVCOMREM,BCPZERO                                                 
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
ACCMST   DC    C'ACCMST '                                                       
FF       EQU   X'FF'                                                            
BILLWC   DC    C'99'                                                            
ORDWC    DC    C'**'                                                            
UL11     DC    C'11'                                                            
UL12     DC    C'12'                                                            
ULSI     DC    C'SI'                                                            
ULSK     DC    C'SK'                                                            
UL1P     DC    C'1P'                                                            
UL1R     DC    C'1R'                                                            
PPLUS1   DC    P'1'                                                             
PMINUS1  DC    P'-1'                                                            
ACTDRAQ  EQU   11                                                               
ACTUPDQ  EQU   12                                                               
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                  ** REVALUE CURRENCIES **                     
MTEL     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#MAT)         ELEMENT CODE                                 
         DC    AL2(MTELX+1-MTEL)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDMATCH-CLB59) SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(255)            MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKACT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB59)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DA'             TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDA-CLB59)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WC'             TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKWORK)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'SUPPL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'DATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'REF'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKREF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'INDS'           TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'INCAC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOINCAC-1)    DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVINCAC-CLB59) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'BILL#'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTARBLNO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBILNO-CLB59) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'BLDAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTARBLDT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBILDT-CLB59) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'CURR'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTACUR)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCUR-CLB59)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'NET  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTANET)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVNMAT-CLB59)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'COM  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTARCOM)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVCMAT-CLB59)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'EXCHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'AFCXRATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRATE-CLB59)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'FCNET'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTANETF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(15)             MAPPING CODE                                 
         DC    CL5'FCCOM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTARFCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
*        DC    AL1(MDELDL2)        ITEM LENGTH                                  
*        DC    AL2(15)             MAPPING CODE                                 
*        DC    CL5'AGAMT'          TEXT IDENTIFIER                              
*        DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
*        DC    AL1(L'MATCHED)      DATA LENGTH                                  
*        DC    AL1(0)              DATA DISPLACEMENT                            
*        DC    AL1(MDINNULL)       INDICATORS                                   
*        DC    AL2(RCVNMAT-CLB59)  RECEIVE ROUTINE                              
*        DC    AL2(0)              SEND ROUTINE                                 
*        DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(16)             MAPPING CODE                                 
         DC    CL5'BREF'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(4)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBREF-CLB59)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(17)             MAPPING CODE                                 
         DC    CL5'BMON'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(5)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBMON-CLB59)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(18)             MAPPING CODE                                 
         DC    CL5'ACTN'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVACTN-CLB59)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(19)             MAPPING CODE                                 
         DC    CL5'REPID'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE                                 
         DC    CL5'MTCHDA'         TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVMCHDA-CLB59) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
MTELX    DC    XL1'00'             END OF ELEMENT FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
*                                  * REPSUBPG EQUATES *                         
REPSDPST EQU   03                  DRAFT POSTINGS                               
REPSLPST EQU   04                  LIVE POSTINGS                                
*                                                                               
WORK     EQU   BOWORK1                                                          
DUB      EQU   BODUB1                                                           
*                                                                               
REPSPEC  DS    0X                                                               
         SPROG 3,4                                                              
         SPEC  H1,001,RUN                                                       
         SPEC  H1,073,PAGE                                                      
         SPEC  H3,001,AC#CPY,8,L                                                
         SPEC  H3,061,AC#USRID,10,L                                             
         SPEC  H4,001,AC#OFF,8,L                                                
         SPEC  H4,061,AC#REQR,10,L                                              
         SPEC  H5,001,AC#CLINT,8,L                                              
         SPEC  H6,001,AC#PRO,8,L                                                
         SPEC  H7,001,AC#JOB,8,L                                                
         SPROG 3                                                                
         SPEC  H1,020,AC#DPOS,40,C                                              
         SPEC  H2,020,AC#DPOS,40,CU                                             
         SPROG 4                                                                
         SPEC  H1,020,AC#APOS,40,C                                              
         SPEC  H2,020,AC#APOS,40,CU                                             
*                                                                               
         SPEC  END                                                              
         EJECT                                                                  
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    XL(OSVALSL-(*-OSVALS))                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   AIO9                                                             
AIOMCH   DS    A                   A(MATCH RECORD)                              
IOMCH    EQU   IO9                                                              
         ORG   AIOA                                                             
AIOINV   DS    A                   A(INVOICE RECORD)                            
IOINV    EQU   IOA                                                              
                                                                                
*                                                                               
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
DMCB     DS    6A                                                               
*                                                                               
THISJOB  DS    XL(L'TRNKACT)                                                    
THISDA   DS    XL(L'TRNKDA)        TRANSACTION DA                               
MATCHED  DS    PL8                 NET MATCHED AGY CUR                          
COMMATCH DS    PL8                 COMMISSION MATCHED AGY CUR                   
BILCUR   DS    CL3                                                              
                                                                                
EXRULE   DS    0XL7                                                             
EXIND    DS    XL1                                                              
EXCHRAT  DS    XL5                                                              
EXSHIFT  DS    XL1                                                              
                                                                                
BILLREF  DS    CL6                                                              
BILLDATE DS    PL3                                                              
NUMTSAR  DS    XL2                 NUMBER OF TSAR RECORDS ADDED                 
REPID    DS    CL7                                                              
*                                                                               
OSREF#   DS    CL4                 BATCH REF                                    
OSMONP   DS    PL2                                                              
OSMONC   DS    CL2                                                              
*                                                                               
LASTKEY  DS    XL(L'TRNKEY)                                                     
ALSTPTA  DS    A                   A(LAST PTA ELEMENT ON ITEM)                  
STLREC   DS    XL(TLMT1LNQ)        SAVED LAST TIME TSAR RECORD                  
CODTAB   DS    10XL(CODTABL)                                                    
CODTABN  EQU   (*-CODTAB)/CODTABL                                               
CODTABX  DS    XL1                                                              
*                                                                               
SVNETTB  DS    PL8                 NET TOTAL (BILLING CURRENCY)                 
SVNETTA  DS    PL8                 NET TOTAL (AGENCY CURRENCY)                  
SVNETAB  DS    PL8                 NET AVAILABLE (BILLING CURRENCY)             
SVNETAA  DS    PL8                 NET AVAILABLE (AGENCY CURRENCY)              
SVNETMB  DS    PL8                 NET MATCHED (BILLING CURRENCY)               
SVNETMA  DS    PL8                 NET MATCHED (AGENCY CURRENCY)                
SVCOMTB  DS    PL8                 COMMISSION TOTAL (BILLING CURRENCY)          
SVCOMTA  DS    PL8                 COMMISSION TOTAL (AGENCY CURRENCY)           
SVCOMAB  DS    PL8                 COMMISSION AVAILABLE (BILLING)               
SVCOMAA  DS    PL8                 COMMISSION AVAILABLE (AGENCY)                
SVCOMMB  DS    PL8                 COMMISSION MATCHED (BILLING)                 
SVCOMMA  DS    PL8                 COMMISSION MATCHED (AGENCY)                  
SVCOMREM DS    PL8                 COMMISION REMAINDER                          
SVHOURT  DS    PL8                 HOURS TOTAL                                  
SVHOURA  DS    PL8                 HOURS AVAILABLE                              
SVHOURM  DS    PL8                 HOURS MATCHED                                
*                                                                               
         DS    (L'OVRWS-(*-OVRWS))X                                             
*                                                                               
MWORKD   DSECT                                                                  
MAEXC    DS    A                   A(EXCHANGE RATE RULE)                        
MAPCT    DS    A                   A(COMMISION FIELD IF % ENTERED)              
*                                                                               
REUSE    DS    H                   NAME TABLE RE-USE VALUE                      
*                                                                               
MPCT     DS    PL8                 % ENTERED IN COMMISSION FIELD                
MNET     DS    PL8                 NET AMOUNT FOR MATCHING                      
MCOM     DS    PL8                 COMMISSION AMOUNT FOR MATCHING               
*                                                                               
MINDS    DS    XL1                 INDICATOR BYTE                               
MIFCB    EQU   X'80'               FOREIGN CURRENCY BIILL                       
MINET    EQU   X'40'               INPUT TO NET COLUMN                          
MICOM    EQU   X'20'               INPUT TO COMMISSION COLUMN                   
MIDEL    EQU   X'10'               ADVANCE TRANSACTION WAS DELETED              
MILIVE   EQU   X'08'               LIVE POSTINGS REQUIRED                       
MIDRAFT  EQU   X'04'               DRAFT POSITNGS REQUIRED                      
MIPOST   EQU   X'02'               POSTINGS MADE                                
*                                                                               
MINCAC   DS    CL15                CURRENTLY SAVED INCOME ACCOUNT               
MANC     DS    CL12                ANALYSIS CODE                                
*                                                                               
MATCHANC DS    CL12                MATCH ANALYSIS CODE                          
*                                                                               
SKULACT  DS    0CL14                                                            
SKUL     DS    CL2                                                              
SKACT    DS    CL12                                                             
*                                                                               
VATACCT  DS    CL14                                                             
*                                                                               
BTHALF   DS    H                                                                
BTAMNT   DS    PL8                                                              
BTPARM   DS    8A                                                               
         ORG   BTPARM                                                           
BTPARM1  DS    A                                                                
BTPARM2  DS    A                                                                
BTPARM3  DS    A                                                                
BTPARM4  DS    A                                                                
BTPARM5  DS    A                                                                
BTPARM6  DS    A                                                                
BTPARM7  DS    A                                                                
BTPARM8  DS    A                                                                
*                                                                               
SCRAMT1  DS    CL14                                                             
SCRAMT2  DS    CL14                                                             
SCRAMT3  DS    CL14                                                             
SCRNET1  DS    CL14                                                             
SCRNET2  DS    CL14                                                             
SCRNET3  DS    CL14                                                             
SCRCOM1  DS    CL14                                                             
SCRCOM2  DS    CL14                                                             
SCRCOM3  DS    CL14                                                             
*                                                                               
AMCHPTA  DS    A                   A(MATCH PTA ELEMENT)                         
MCHPTA   DS    XL256               COPY OF MATCH PTA ELEMENT                    
AINVPTA  DS    A                   A(INVOICE PTA ELEMENT)                       
INVPTA   DS    XL256               COPY OF INVOICE PTA ELEMENT                  
MPOSTVAL DS    XL(POSTVALL)                                                     
*                                                                               
SCANBYTE DS    CL1                                                              
SCANBLK  DS    5CL(SCBLKLQ)                                                     
*                                                                               
IWCTAB   DS    XL(IWCTABL*IWCTABN)                                              
*                                                                               
NAMTAB   DS    XL(NAMTABL*NAMTABN)                                              
*                                                                               
XELBLK   DS    XL500                                                            
*                                                                               
         SPACE 1                                                                
*                                                                               
CODTABD  DSECT                     ** CURRENCY CODE/TABLE TABLE **              
CODTCOD  DS    CL3                 CODE                                         
CODTTAB  DS    XL(CURTABL)         TABLE ENTRY                                  
CODTABL  EQU   *-CODTABD                                                        
*                                                                               
TLSTD    DSECT                     ** TEMPORARY TSAR RECORDS                    
         ORG   TLKSRT                    CONTAINING BILL INFO **                
TLBDATE  DS    PL3                 BILL DATE                                    
TLBREF#  DS    CL6                 BILL REFERENCE NUMBER                        
TLBDA    DS    XL4                 DISK ADDRESS                                 
         ORG   TLDATA                                                           
TLBX     DS    XL(L'TLMX)          EXCHANGE RATE                                
TLBLNQ   EQU   *-TLSTD                                                          
                                                                                
HLINE    DSECT                     ** HEAD LINE **                              
         ORG   HLINE+01                                                         
HWORD1   DS    CL08,CL1                                                         
HDATA1   DS    CL08,CL1                                                         
HNAME1   DS    CL36                                                             
         ORG   HLINE+61                                                         
HWORD2   DS    CL10,CL1                                                         
HDATA2   DS    CL08                                                             
         SPACE 1                                                                
IWCTABD  DSECT                     ** INCOME BY WORKCODE TABLE **               
IWCTWC   DS    XL2                                                              
IWCTNET  DS    PL8                                                              
IWCTCOM  DS    PL8                                                              
IWCTABL  EQU   *-IWCTABD                                                        
IWCTABN  EQU   50                                                               
         SPACE 1                                                                
NAMTABD  DSECT                     ** NAME TABLE **                             
NAMTULA  DS    XL14                                                             
NAMTNAM  DS    CL36                                                             
NAMTABL  EQU   *-NAMTABD                                                        
NAMTABN  EQU   15                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSES                                                           
TLKMPOQ  EQU   X'F2'               TSAR RECORD FOR MATCH POSTINGS               
         ORG   TLKSRT                                                           
TLKMPT   DS    XL1                 POSTING TYPE                                 
TLKMPINQ EQU   1                   INCOME POSTINGS                              
TLKMPSUQ EQU   2                   SUSPENSE TRANSFER POSTINGS                   
TLKMPACT DS    CL15                C/U/L/ACCOUNT (INCOME)                       
         ORG   TLKMPACT                                                         
TLKMPULA DS    CL14                U/L/ACCOUNT (SUSPENSE)                       
         ORG   TLKMPACT+L'TLKMPACT                                              
TLKMPWC  DS    CL2                 WORKCODE (NULL IF SUSPENSE)                  
         ORG   TLDATA                                                           
TLDMNET  DS    PL8                 NET MATCHED                                  
TLDMCOM  DS    PL8                 COMMISSION MATCHED                           
TLMPOLNQ EQU   *-TLREC                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'161ACCLB59   08/16/00'                                      
         END                                                                    
