*          DATA SET ACREPP302  AT LEVEL 056 AS OF 04/27/07                      
*PHASE ACP302A                                                                  
*INCLUDE ACCEDIT                                                                
         TITLE 'UNMATCHED ORDER LIST'                                           
ACP302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP3**,R9,RR=R5                                              
         L     RA,0(R1)            RA=A(ACWORKD)                                
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(PROGRAM W/S)                            
         USING ACP3D,RC                                                         
         ST    R5,RELO                                                          
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   ACP8                                                             
*                                                                               
         L     R1,ADCMPEL                                                       
         USING CPYELD,R1                                                        
         MVI   NEWOFF,C'Y'                                                      
         TM    CPYSTAT4,CPYSOFF2   TEST NEW OFFICES                             
         BO    *+8                                                              
         MVI   NEWOFF,C'N'                                                      
         DROP  R1                                                               
*                                                                               
         L     RF,ADACC                                                         
         MVC   SAVEACC,0(RF)                                                    
         MVC   IOB(42),SPACES                                                   
         L     RF,ADCOMP           READ PRODUCTION LEDGER                       
         MVC   IOB(1),0(RF)        FOR HEIRARCHY ELEMENT                        
         MVC   IOB+1(2),=C'SJ'                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,IOB                                                           
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
         USING ACLELD,RE                                                        
ACP2     CLI   0(RE),0                                                          
         BE    ACP7                                                             
         CLI   0(RE),ACLELQ                                                     
         BE    ACP6                                                             
*                                                                               
ACP4     IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ACP2                                                             
*                                                                               
ACP6     MVC   SAVE16,ACLEL                                                     
         B     ACP4                                                             
         DROP  RE                                                               
*                                                                               
ACP7     MVI   BLDTBL,C'N'         INDICATE TABLE NOT BUILT YET                 
         BAS   RE,OFFLMT                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',SAVEACC,IOB                   
         B     EXIT                                                             
         EJECT                                                                  
ACP8     CLI   MODE,REQFRST                                                     
         BNE   ACP10                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         ZAP   REQORDT,=P'0'                                                    
         ZAP   REQINVT,=P'0'                                                    
         XC    PSTART,PSTART                                                    
         MVC   PEND,=3X'FF'                                                     
         CLI   NEWOFF,C'Y'         TEST NEW OFFICES                             
         BNE   ACP8A               NO                                           
         L     R1,ADOFFALD                                                      
         USING OFFALD,R1                                                        
         CLC   OFFAOFFC,SPACES     TEST REQUEST BY OFFICE                       
         BE    ACP8C               NO                                           
         B     ACP8B               YES                                          
         DROP  R1                                                               
*                                                                               
ACP8A    CLI   QOFFICE,C' '         IF REQUEST IS BY OFFICE AND TABLE           
         BE    ACP8C                HAS NOT YET BEEN BUILT - BUILD IT           
*                                                                               
ACP8B    CLI   BLDTBL,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,OFFLMT                                                        
*                                                                               
ACP8C    CLC   QSTART,SPACES                                                    
         BE    ACP9                                                             
         GOTO1 DATCON,DMCB,(0,QSTART),(1,PSTART)                                
*                                                                               
ACP9     CLC   QEND,SPACES                                                      
         BE    ACP9A                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
*                                                                               
ACP9A    CLI   QOPT1,C' '          SORTING OPTION                               
         BE    EXIT                                                             
         BAS   RE,SETSORT                                                       
         MVC   HEADSORT,SPACES                                                  
         MVC   HEADSORT(8),=C'DUE DATE'                                         
         CLI   QOPT1,C'U'                                                       
         BE    EXIT                                                             
         MVC   HEADSORT(10),=C'ORDER DATE'                                      
         CLI   QOPT1,C'D'                                                       
         BE    EXIT                                                             
         MVC   HEADSORT(10),=C'AUTHORIZER'                                      
         CLI   QOPT1,C'A'                                                       
         BE    EXIT                                                             
         MVC   HEADSORT(13),=C'SUPPLIER CODE'                                   
         MVI   RCSUBPRG,1          SORTING HEADLINE FOR SUPPLIER SORT           
         MVC   LASTSUP,SPACES      FOR FIRST TIME TEST IN PRINT                 
         B     EXIT                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ACP10    CLI   MODE,PROCORD        READ AN ORDER RECORD                         
         BNE   ACP20                                                            
         L     R2,ADACC                                                         
         CLC   4(6,R2),=C'000000'  SKIP CONTROL RECORD                          
         BE    EXIT                                                             
         USING ORDRECD,R2                                                       
         TM    ORDRSTAT,ORDSFMCH+ORDSLDEL+ORDSDEL                               
         BNZ   EXIT                SKIP DELETED AND MATCHED ORDERS              
         MVI   WANT,C'Y'                                                        
         BAS   RE,BUILDREC                                                      
         CLI   WANT,C'N'           DO WE WANT THIS                              
         BE    EXIT                                                             
         CLI   QOPT1,C' '                                                       
         BNE   ACP12                                                            
         BAS   RE,PRINTREC         PRINT IF STRAIGHT                            
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
ACP12    BAS   RE,PUTSORT          OR TUCK AWAY IF SORTING                      
         B     EXIT                                                             
         EJECT                                                                  
ACP20    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         CLI   QOPT1,C' '                                                       
         BE    ACP30               IF NOT SORTING-FINISH                        
         CLI   SORTACTV,C'Y'       DID WE WRITE ANYTHING OUT                    
         BNE   ACP28                                                            
         LA    R6,IOA                                                           
*                                                                               
ACP22    BAS   RE,GETSORT          ELSE READ SORTED RECORDS                     
         CLC   IOA(L'SORTKEY),SPACES                                            
         BE    ACP28                                                            
         BAS   RE,PRINTREC         AND PRINT THEM                               
         B     ACP22                                                            
*                                                                               
ACP28    BAS   RE,ENDSORT                                                       
*                                                                               
ACP30    BAS   RE,MYREPORT                                                      
         MVC   P+50(13),=C'REQUEST TOTAL'                                       
         EDIT  REQORDT,(15,P+88),2,MINUS=YES                                    
         EDIT  REQINVT,(15,P+104),2,MINUS=YES                                   
         BAS   RE,MYREPORT                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              BUILD A STANDARD RECORD FOR PRINT OR SORT                        
*                                                                               
BUILDREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         LR    RE,R6                                                            
         LA    RF,L'SORTREC                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R2,ADACC                                                         
         MVC   SORDKEY,2(R2)       ORDER NO PART OF KEY                         
         AH    R2,DATADISP                                                      
*                                                                               
BLDR2    CLI   0(R2),0                                                          
         BE    BLDR22                                                           
         CLI   0(R2),X'67'                                                      
         BE    BLDR6                                                            
         CLI   0(R2),X'68'                                                      
         BE    BLDR18                                                           
*                                                                               
BLDR4    ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     BLDR2                                                            
*                                                                               
         USING ORDELD,R2                                                        
BLDR6    CLI   BLDTBL,C'Y'         WAS A TABLE BUILT ?                          
         BNE   BLDR10              NO, DON'T TRY TO READ FROM IT THEN           
         BAS   RE,FINOF                                                         
         OC    OFFICE,OFFICE                                                    
         BZ    BLDR8                                                            
         CLI   OFFWANT,C'Y'                                                     
         BNE   BLDR24                                                           
*                                                                               
BLDR8    DS    0H                                                               
         CLI   NEWOFF,C'Y'         TEST NEW OFFICES                             
         BE    BLDR9               YES                                          
         CLI   QOFFICE,C' '                                                     
         BE    BLDR10                                                           
         CLC   QOFFICE,SELOFF       IT MUST BE INCLUDE OFFICE IF:               
         BE    BLDR10               OFFICES MATCH OR X'40' BIT IS ON            
         TM    QOFFICE,X'40'        IT IS EXCLUDE IF X'40' BIT IS OFF           
         BO    BLDR24                                                           
         MVC   BYTE,QOFFICE                                                     
         OI    BYTE,X'40'                                                       
         CLC   BYTE,SELOFF                                                      
         BE    BLDR24                                                           
         B     BLDR10                                                           
*                                                                               
BLDR9    L     R1,ADOFFALD                                                      
         USING OFFALD,R1                                                        
         CLC   OFFAOFFC,SPACES     TEST OFFICE FILTER REQUEST                   
         BE    BLDR10              NO                                           
         L     RF,OFFAREQL         RF=A(REQUEST OFFICE LIST)                    
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)          R0=COUNT OF VALID OFFICES                    
         LA    RF,2(RF)            RF=A(VALID OFFICE)                           
         CLC   SELOFF,0(RF)        TEST FOR VALID OFFICE                        
         BE    BLDR10              YES                                          
         LA    RF,2(RF)                                                         
         BCT   R0,*-14                                                          
         B     BLDR24              NO                                           
         DROP  R1                                                               
*                                                                               
BLDR10   CLC   ORDDATE,PSTART      APPLY DATE FILTERS                           
         BL    BLDR24                                                           
         CLC   ORDDATE,PEND                                                     
         BH    BLDR24                                                           
         CLI   QOPT2,C' '                                                       
         BE    BLDR14                                                           
         CLI   QOPT2,C'P'          TYPE FILTER,P=PRODN E=EXPENSE                
         BE    BLDR12                                                           
         CLC   ORDEXP+1(2),=C'SJ'                                               
         BE    BLDR24                                                           
         B     BLDR14                                                           
*                                                                               
BLDR12   CLC   ORDJOB+1(2),=C'SJ'                                               
         BNE   BLDR24                                                           
*                                                                               
BLDR14   CLC   QSELECT,SPACES      CLIENT CODE IN QSELECT                       
         BE    BLDR16                                                           
         LA    R1,QSELECT-1                                                     
         LA    R3,L'QSELECT                                                     
         AR    R1,R3               R1 TO END OF QSELECT                         
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R3,*-10                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   ORDJOB+3(0),QSELECT                                              
         BNE   BLDR24              EXCLUDE ALL BUT THIS CLIENT                  
         B     BLDR16                                                           
*                                                                               
BLDR16   MVC   SSUPKEY,ORDSUP                                                   
         XC    SORTMAJ,SORTMAJ                                                  
         MVC   SORTMAJ(3),ORDDDTE  DUE DATE TO SORT KEY                         
         CLI   QOPT1,C'U'                                                       
         BE    BLDR4                                                            
         MVC   SORTMAJ(3),ORDDATE  ORDER DATE TO SORT KEY                       
         CLI   QOPT1,C'D'                                                       
         BE    BLDR4                                                            
         MVC   SORTMAJ,ORDAUTH     AUTHORISER TO SORT KEY                       
         CLI   QOPT1,C'A'                                                       
         BE    BLDR4                                                            
         MVC   SORTMAJ,ORDSUP      SUPPLIER CODE TO SORT KEY                    
         B     BLDR4                                                            
         DROP  R2                                                               
*                                                                               
         USING OAMELD,R2                                                        
BLDR18   CLI   QOPT3,C' '          INVOICED AMOUNT OPTION                       
         BE    BLDR4                                                            
         CLI   QOPT3,C'I'          ORDER MUST HAVE BEEN INVOICED                
         BE    BLDR20                                                           
         CP    OAMIVAL,=P'0'       OR NOT                                       
         BE    BLDR4                                                            
         B     BLDR24                                                           
*                                                                               
BLDR20   CP    OAMIVAL,=P'0'                                                    
         BE    BLDR24                                                           
         B     BLDR4                                                            
*                                                                               
BLDR22   LA    RE,SORTDATA         CLEAR SORTDATA                               
         LHI   RF,L'SORTDATA                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ADACC            A(ORDER)                                     
         LA    R0,SORTDATA         A(SORTDATA)                                  
         LH    RF,ACCORLEN(RE)     LENGTH OF ORDER                              
         SH    RF,DATADISP         LESS LENGTH OF DATADISP                      
         AH    RE,DATADISP         BUMP TO 1ST ELEMENT                          
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY ORDER INTO SORTDATA                     
         B     EXIT                                                             
*                                                                               
BLDR24   XC    SORDKEY,SORDKEY                                                  
         MVI   WANT,C'N'                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              PRINT A STANDARD RECORD                                          
         SPACE 1                                                                
PRINTREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         CLI   QOPT1,C'S'          SORTING BY SUPPLIER                          
         BE    PRT2                YES-BRANCH                                   
         MVC   P+1(6),SORDKEY+2    ORDER NUMBER                                 
         LA    R2,P+8                                                           
         BAS   RE,SUPNAME          SUPPLIER NAME AND CODE                       
         B     PRT20                                                            
*                                                                               
PRT2     CLC   LASTSUP,SPACES                                                   
         BE    PRT4                                                             
         CLC   LASTSUP,SSUPKEY+3                                                
         BE    PRT6                                                             
*                                                                               
PRT4     LA    R2,P+1                                                           
         BAS   RE,SUPNAME          NAME-ONCE ONLY PER SUPPLIER                  
*                                                                               
PRT6     DS    0H                                                               
         MVC   P+27(6),SORDKEY+2                                                
         MVC   LASTSUP,SSUPKEY+3                                                
         B     PRT20                                                            
*                                                                               
PRT20    LA    R2,SORTDATA         REST OF PRINT LINE                           
         ZAP   ORDAMT,=P'0'                                                     
         ZAP   INVAL,=P'0'                                                      
         MVC   WCODE,SPACES                                                     
*                                                                               
PRT22    CLI   0(R2),0                                                          
         BE    PRT40                                                            
         CLI   0(R2),X'67'                                                      
         BE    PRT26                                                            
         CLI   0(R2),X'68'                                                      
         BE    PRT28                                                            
*                                                                               
PRT24    ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     PRT22                                                            
*                                                                               
         USING ORDELD,R2                                                        
PRT26    CLC   ORDJOB+1(2),=C'SJ'  STRAIGHT PRINT IF EXPENSE                    
         BE    PRT27                                                            
         MVC   P+35(12),ORDJOB+3                                                
         B     PRT27A                                                           
*                                                                               
PRT27    MVC   WORK(15),ORDJOB     CLI/PRD/JOB                                  
         MVC   WORK+20(20),SPACES                                               
         GOTO1 =V(ACCEDIT),DMCB,(0,WORK),SAVE16,WORK+20,RR=RB                   
         L     RF,DMCB                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+35(0),WORK+20                                                  
*                                                                               
PRT27A   GOTO1 DATCON,DMCB,(1,ORDDATE),(8,P+63)                                 
         GOTO1 (RF),(R1),(1,ORDDDTE),(8,P+51)                                   
         MVC   P+72(L'ORDAUTH),ORDAUTH                                          
         B     PRT24                                                            
         DROP  R2                                                               
*                                                                               
         USING OAMELD,R2                                                        
PRT28    AP    ORDAMT,OAMAMNT                                                   
         AP    INVAL,OAMIVAL                                                    
         CLC   WCODE,SPACES                                                     
         BNE   PRT24                                                            
         MVC   WCODE,OAMWORK       FIRST W-CODE IS THE MAJOR                    
         B     PRT24                                                            
*                                                                               
PRT40    MVC   P+60(2),WCODE       COMPLETE THE LINE                            
         EDIT  ORDAMT,(11,P+92),2,MINUS=YES                                     
         AP    REQORDT,DUB                                                      
         EDIT  INVAL,(11,P+108),2,MINUS=YES                                     
         AP    REQINVT,DUB                                                      
         BAS   RE,MYREPORT                                                      
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
MYREPORT NTR1                                                                   
         CLI   QOPT1,C' '                                                       
         BE    MYRP10                                                           
         MVC   HEAD4+84(9),=C'SORTED BY'                                        
         MVC   HEAD4+94(L'HEADSORT),HEADSORT                                    
*                                                                               
MYRP10   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              GET SUPPLIER NAME AND CHOP TO PRINTLINE                          
*                                                                               
SUPNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         L     RF,ADACC                                                         
         MVC   SAVEACC,0(RF)                                                    
         MVC   IOB(42),SPACES                                                   
         MVC   IOB(15),SSUPKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
         MVC   WORK(14),SSUPKEY+1                                               
         CLI   DMCB+8,0                                                         
         BE    SUPN2                                                            
         MVC   WORK+15(7),=C'MISSING'                                           
         B     SUPN8                                                            
*                                                                               
SUPN2    LA    R3,IOB                                                           
         AH    R3,DATADISP                                                      
         SR    RE,RE                                                            
*                                                                               
         USING NAMELD,R3                                                        
SUPN4    CLI   0(R3),0                                                          
         BE    EXIT                                                             
         CLI   0(R3),NAMELQ                                                     
         BE    SUPN6                                                            
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     SUPN4                                                            
*                                                                               
SUPN6    ZIC   R4,NAMLN                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+15(0),NAMEREC                                               
         DROP  R3                                                               
*                                                                               
SUPN8    GOTO1 ADSQUASH,DMCB,WORK,60                                            
         L     R4,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R4),WORK),(26,0(R2)),(C'P',3)                     
         MVC   WORK(26),132(R2)                                                 
         MVC   132(26,R2),SPACES                                                
         MVC   133(26,R2),WORK     INDENT SECOND LINE                           
         MVC   WORK(26),264(R2)                                                 
         MVC   264(26,R2),SPACES                                                
         MVC   265(26,R2),WORK     AND THIRD                                    
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',SAVEACC,IOB                   
         B     EXIT                                                             
         EJECT                                                                  
*              DEAL WITH OFFICES                                                
* CALLED AT RUNFRST OR REQFRST                                                  
* BUILDS A TABLE OF CLIENT CODES AND THEIR OFFICES WHICH PASS LIMIT             
* ACCESS                                                                        
*                                                                               
OFFLMT   NTR1                                                                   
         CLI   MODE,REQFRST                                                     
         BE    OFL4                                                             
         L     RF,ADCOMP                                                        
         MVC   COMPANY,0(RF)                                                    
         MVC   COMFACS,DATAMGR                                                  
         CLI   NEWOFF,C'Y'                                                      
         BE    OFL2                ON NEW OFFICES                               
         XC    OFFICE,OFFICE                                                    
         L     R1,ADMASTC                                                       
         USING MASTD,R1                                                         
         CLI   MCIDACCS,C'*'                                                    
         BE    *+12                                                             
         CLI   MCIDACCS,C'$'                                                    
         BNE   OFL4                                                             
         MVC   OFFICE,MCIDACCS                                                  
         B     OFL4                                                             
*                                                                               
OFL2     L     R1,ADOFFALD                                                      
         USING OFFALD,R1                                                        
         OC    OFFICE,OFFANEWA                                                  
         BZ    EXIT                                                             
         DROP  R1                                                               
*                                                                               
OFL4     L     RF,RELO             CLEAR CLIENT TABLE                           
         L     RE,=A(CLILIST)                                                   
         AR    RE,RF                                                            
         ST    RE,ACLILIST                                                      
         L     RF,MAXCLIS                                                       
         SLL   RF,3                * 8                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
OFL10    CLI   NEWOFF,C'Y'                                                      
         BNE   OFL11                                                            
         L     R1,ADOFFALD                                                      
         USING OFFALD,R1                                                        
         MVC   SVOFFC,OFFAOFFC     SAVE REQUESTED OFFICE                        
         MVC   SVINDS,OFFAINDS     SAVE OFFAL INDICATOR BYTE                    
*                                                                               
OFL11    LA    RF,SAVE16                                                        
         USING ACLELD,RF                                                        
         MVC   CLEN,ACLELLVA       SAVE CLIENT LENGTH                           
         LA    R2,IOB                                                           
         USING ACCRECD,R2                                                       
         MVC   ACCKEY,SPACES                                                    
         L     RF,ADCOMP           READ PRODUCTION LEDGER                       
         MVC   ACCKEY(1),0(RF)     FOR CLIENTS IN CORRECT OFFICE                
         MVC   ACCKEY+1(2),=C'SJ'                                               
         MVI   ACCKEY+3,X'41'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
*                                                                               
OFL12    CLC   ACCKEY+1(2),=C'SJ'                                               
         BNE   OFLX                                                             
         LA    R3,IOB                                                           
         AH    R3,DATADISP         LOOK AT CLIENT RECORD                        
         SR    RE,RE                                                            
*                                                                               
OFL14    CLI   0(R3),0                                                          
         BE    OFL22                                                            
         CLI   0(R3),PPRELQ        AND FIND PROFILE ELEMENT                     
         BE    OFL15                                                            
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     OFL14                                                            
*                                                                               
         USING PPRELD,R3                                                        
OFL15    CLI   NEWOFF,C'Y'         TEST NEW OFFICES                             
         BE    OFL18                                                            
         OC    OFFICE,OFFICE                                                    
         BZ    OFL20                                                            
         L     R1,ADOFFLST         R1=A(OFFICE LIST)                            
         LA    R0,32               R0=LOOP COUNTER                              
*                                                                               
OFL16    CLI   0(R1),0                                                          
         BE    OFL17                                                            
         CLI   0(R1),C'0'                                                       
         BE    OFL17                                                            
         CLC   PPRGAOFF(1),0(R1)                                                
         BE    OFL20                                                            
*                                                                               
OFL17    LA    R1,1(R1)                                                         
         BCT   R0,OFL16                                                         
         B     OFL22                                                            
*                                                                               
OFL18    L     R1,ADOFFALD                                                      
         USING OFFALD,R1                                                        
         LA    R0,IOB                                                           
         ST    R0,OFFAREC                                                       
         MVC   OFFAOFFC,PPRGAOFF                                                
         MVI   OFFAOPOS,C'C'                                                    
         OI    OFFAINDS,OFFAIOFF   CHECK OFFICE SECURITY                        
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 ADOFFAL                                                          
         BNE   OFL22                                                            
*                                                                               
OFL20    BAS   RE,ADDOFL                                                        
*                                                                               
OFL22    ZIC   RE,CLEN                                                          
         ZIC   RF,ACCKEY+2(RE)     GET NEXT CLIENT                              
         LA    RF,1(RF)                                                         
         STC   RF,ACCKEY+2(RE)                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCFIL',IOB,IOB                       
         B     OFL12                                                            
*                                                                               
OFLX     CLI   NEWOFF,C'N'         TEST OLD OFFICES                             
         BE    EXIT                                                             
         L     R1,ADOFFALD                                                      
         MVC   OFFAOFFC,SVOFFC     RESTORE REQUESTED OFFICE                     
         MVC   OFFAINDS,SVINDS     AND INDICATOR BYTE                           
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
ADDOFL   NTR1                                                                   
         L     RF,ACLILIST         ADD CLIENT CODE TO LIST                      
         L     RE,MAXCLIS                                                       
*                                                                               
ADDO2    OC    0(8,RF),0(RF)                                                    
         BZ    ADDO4                                                            
         LA    RF,8(RF)                                                         
         BCT   RE,ADDO2                                                         
         DC    H'0'                TABLE FULL-INCREASE CLILIST/MAXCLIS          
*                                                                               
ADDO4    MVC   0(6,RF),ACCKEY+3                                                 
         MVC   6(2,RF),PPRGAOFF    MOVE OFFICE CODE TO TABLE ALSO               
         MVI   BLDTBL,C'Y'         INDICATE A TABLE IS BUILT                    
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
FINOF    NTR1                                                                   
         L     RF,ACLILIST         FIND CLIENT CODE IN LIST                     
         USING ORDELD,R2                                                        
         ZIC   RE,CLEN                                                          
         BCTR  RE,0                                                             
         MVI   OFFWANT,C'N'                                                     
         XC    SELOFF,SELOFF       CLEAR SELECTED OFFICE FIELD                  
         L     R1,MAXCLIS                                                       
*                                                                               
FIN02    OC    0(8,RF),0(RF)                                                    
         BZ    EXIT                WE RAN OFF THE END                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),ORDJOB+3    TEST EACH AGAINST ORDER CLIENT               
         BE    FIN04                                                            
         LA    RF,8(RF)                                                         
         BCT   R1,FIN02                                                         
         B     EXIT                                                             
*                                                                               
FIN04    MVI   OFFWANT,C'Y'                                                     
         MVC   SELOFF,6(RF)                                                     
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              SORTER INTERFACE                                                 
*                                                                               
         USING SORTRECD,R6                                                      
SETSORT  NTR1                                                                   
         GOTO1 ADSORTER,DMCB,SORTCARD,RECDCARD                                  
         MVI   SORTACTV,C'N'                                                    
         B     EXIT                                                             
*                                                                               
PUTSORT  NTR1                                                                   
         GOTO1 ADSORTER,DMCB,=C'PUT',IOA                                        
         MVI   SORTACTV,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
GETSORT  NTR1                                                                   
         GOTO1 ADSORTER,DMCB,=C'GET',0                                          
         MVC   IOA(L'SORTKEY),SPACES                                            
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         MVC   0(250,R6),0(R1)                                                  
         MVC   250(250,R6),250(R1)                                              
         MVC   500(250,R6),500(R1)                                              
         MVC   750(250,R6),750(R1)                                              
         MVC   1000(250,R6),1000(R1)                                            
         B     EXIT                                                             
*                                                                               
ENDSORT  NTR1                                                                   
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(01,23,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=(2038)'                                
*                                                                               
MAXCLIS  DC    F'7000'                                                          
COMFACS  DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR STANDARD SORT-RECORD                                   
*                                                                               
SORTRECD DSECT                                                                  
SORTREC  DS    0CL2038                                                          
SORTKEY  DS    0CL23                                                            
SORTMAJ  DS    CL15                SUPPLIER OR DATE OR SPACES                   
SORDKEY  DS    CL8                 BYTES 3-10 OF ORDER KEY                      
SSUPKEY  DS    CL15                SUPPLIER                                     
SORTDATA DS    CL2000                                                           
*                                                                               
*              GENERAL W/S DSECT                                                
*                                                                               
ACP3D    DSECT                                                                  
RELO     DS    F                                                                
ACLILIST DS    A                                                                
SORTACTV DS    CL1                 SORTER ACTIVITY SWITCH                       
SAVEACC  DS    CL15                                                             
LASTSUP  DS    CL12                                                             
WCODE    DS    CL2                 IN US -MAJOR WORK-CODE                       
ORDAMT   DS    PL6                                                              
INVAL    DS    PL6                                                              
PEND     DS    CL3                                                              
PSTART   DS    CL3                                                              
WANT     DS    CL1                                                              
COMPANY  DS    CL1                                                              
OFFWANT  DS    CL1                                                              
NEWOFF   DS    CL1                 Y=COMPANY ON NEW OFFICES                     
OFFICE   DS    CL2                 OFFICE LIMIT ACCESS                          
SVOFFC   DS    CL2                 SAVED REQUESTED OFFICE CODE                  
SVINDS   DS    CL1                 OFFAL INDICATOR BYTE                         
BLDTBL   DS    CL1                 INDICATOR THAT CLIENT TABLE IS BUILT         
SELOFF   DS    CL2                 OFFICE CODE FROM CLIENT LIST                 
CLEN     DS    CL1                                                              
HEADSORT DS    CL15                                                             
REQORDT  DS    PL8                                                              
REQINVT  DS    PL8                                                              
SAVE16   DS    CL70                                                             
IOA      DS    2023C                                                            
IOB      DS    2000C                                                            
         EJECT                                                                  
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACOFFALD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
CLILIST  CSECT                                                                  
         DS    56000C              56000 BYTES FOR CLIENT TABLE                 
*                                  7000 ENTRIES, 8 BYTES LONG                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACREPP302 04/27/07'                                      
         END                                                                    
