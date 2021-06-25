*          DATA SET ACREP0703  AT LEVEL 035 AS OF 03/06/03                      
*PHASE AC0703A,*,NOAUTO                                                         
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE ACSAVE                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLAC                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'COKE EXPENDITURE JOURNAL'                                       
AC0703   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 500,*COKE*,=V(REGSAVE),RA                                        
         USING AC0703D,RC                                                       
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         SPACE 2                                                                
         GOTO1 =V(DATCON),DMCB,(5,0),(1,TODAY)                                  
INIT2    GOTO1 =V(CARDS),DMCB,WORK,=C'RE00'                                     
         CLC   =C'/*',WORK                                                      
         BE    INIT6                                                            
         CLC   =C'FILE=',WORK      FILE=ACC1 ETC                                
         BE    INIT4                                                            
         CLC   WORK(5),=C'DATE='                                                
         BE    INIT5                                                            
         B     INIT2                                                            
**                                                                              
INIT4    MVC   FILNO,WORK+8                                                     
         B     INIT2                                                            
*                                                                               
INIT5    GOTO1 =V(DATVAL),DMCB,(0,WORK+5),DUB      DATE=MM/DD/YY                
         OC    DMCB(4),DMCB                                                     
         BZ    BADDATE                                                          
         MVC   SPECDATE(5),=C'DATE='                                            
         MVC   SPECDATE+5(6),DUB        YYMMDD                                  
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,TODAY)                                
         B     INIT2                                                            
         SPACE 1                                                                
BADDATE  MVC   P(20),=CL20'BAD DATE CARD'                                       
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
INIT6    PACK  DUB,FILNO                                                        
         CVB   R3,DUB                                                           
         STC   R3,FILNOB                                                        
         L     RF,=V(UTL)                                                       
         MVI   4(RF),10                                                         
         GOTO1 =V(DATAMGR),DMCB,=C'OPEN',=C'CONTROL',=C'NCTFILE X'              
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CTWREC,R5                                                        
         XC    CTWKEY,CTWKEY       BUILD SYSLIST KEY (SPECIFIC SYSTEM)          
         MVI   CTWKTYP,CTWKTYPQ    RECORD TYPE C'W' (SYSLIST)                   
         MVI   CTWKREC,CTWKRSYS    SUB-TYPE C'S' (SYSTEMS)                      
         MVI   CTWKSYSN,CTWKACC    MAJOR SYSTEM NUMBER                          
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE  ',KEY,IOAREA1,0           
         CLC   KEY(L'CTWKEY),IOAREA1                                            
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND SYSLIST                           
         SPACE 1                                                                
         LA    R5,IOAREA1                                                       
         LA    R5,CTWDATA                                                       
         SPACE 1                                                                
INIT7    CLI   0(R5),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'A4'         SYSTEM LIST ELEMENT                          
         BNE   INIT8                                                            
         USING CTLSTD,R5                                                        
         CLC   FILNOB,CTLSTDTA+9   MATCH SYSTEM                                 
         BNE   INIT8                                                            
         MVC   ACCSE,CTLSTDTA+8                                                 
         B     INIT9                                                            
         SPACE 1                                                                
INIT8    ZIC   R2,1(R5)                                                         
         AR    R5,R2                                                            
         B     INIT7                                                            
         DROP  R5                                                               
*                                                                               
INIT9    L     RF,=V(UTL)                                                       
         MVC   4(1,RF),ACCSE                                                    
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,DMSYS,DMFLIST                            
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,(40,V(SORTAREA))                
*                                                                               
         XC    ID,ID               BUILD A SKELETON KEY FOR WORKER              
         MVC   ID+2(3),=C'ACE'                                                  
         MVC   ID+6(1),TODAY+2                                                  
         MVI   ID+7,C'P'                                                        
*                                                                               
         LA    RF,6                                                             
         LA    RE,TOT1                                                          
CE2      ZAP   0(6,RE),=P'0'                                                    
         LA    RE,6(RE)                                                         
         BCT   RF,CE2                                                           
*                                                                               
         MVC   TITLE+23(24),=C'COKE EXPENDITURE JOURNAL'                        
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         LA    R8,IOAREA1                                                       
         USING RECDS,R8                                                         
         MVI   COMPANY,FRST                                                     
         EJECT                                                                  
* READ RECOVERY RECORDS, FILTER COKE POSTINGS AND WRITE TO SORTER               
*                                                                               
         LA    RF,4(R8)                                                         
         LA    R3,L'RECVHDR(RF)                                                 
         USING ACTRECD,R3                                                       
CE4      GOTO1 =V(DATAMGR),DMCB,(X'FF',DMRSEQ),DMRFILE,DMDA,4(R8),     X        
               A(TRKBUFF)                                                       
         LH    RE,DMCB+18                                                       
         LA    RE,4(RE)                                                         
         XC    0(4,R8),0(R8)                                                    
         STH   RE,0(R8)                                                         
         TM    DMCB+8,X'80'        EOF                                          
         BO    CE20                                                             
         TM    DMCB+8,0            ERRORS                                       
         BO    CE6                                                              
         CLI   RFILTY,DMFEQU       ACCOUNT MAST                                 
         BNE   CE4                                                              
         CLI   RSIN,X'FF'          SKIP IF TRANSACTION DIED                     
         BE    CE4                                                              
         CLI   RPRG,X'15'          IGNORE ALL BUT COKE EXPENDITURE              
         BE    CE5                                                              
         CLI   RPRG,X'13'          SPOT PAY                                     
         BNE   CE4                                                              
         CLI   0(R3),X'CC'         ONLY COKE                                    
         BNE   CE4                                                              
CE5      CLI   ACTRFST,X'44'                                                    
         BNE   CE4                                                              
         TM    ACTRSTA,X'80'      IGNORE DELETED RECORDS                        
         BO    CE4                                                              
         USING TRNELD,R2                                                        
         LA    R2,ACTRFST                                                       
         CLI   RRECTY,X'01'        COPIES                                       
         BNE   *+10                                                             
         MP    TRNAMNT,=P'-1'                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R8)                                     
         B     CE4                                                              
         EJECT                                                                  
* HANDLE RECOVERY READ ERRORS                                                   
CE6      DS    0H                                                               
         MVC   P(28),=C'DISK ERROR ON RECOVERY FILE='                           
         MVC   P+28(8),DMRFILE                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'DA=XXXXXXXX,DMCB='                                      
         GOTO1 =V(HEXOUT),WORK,DMDA,P+34,,=C'TOG'                               
         GOTO1 (RF),(R1),DMCB,P+17,20,=C'TOG'                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(14),=C'RUN CONTINUING'                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         LH    R3,DMDA             BUMP TO NEXT TRACK                           
         LA    R3,1(R3)                                                         
         STH   R3,DMDA                                                          
         MVI   DMDA+2,0                                                         
         B     CE4                                                              
         DROP  R8                                                               
*                                                                               
DMFILE   DC    CL8'ACCOUNT'                                                     
DMRFILE  DC    C'ACCRCVR'                                                       
DMFLIST  DC    C'NACCDIR NACCMST NACCRCV X'                                     
DMFEQU   EQU   X'6A'                                                            
DMSEQU   EQU   X'06'                                                            
DMSYS    DC    C'ACCOUNT'                                                       
DMOPEN   DC    C'OPEN'                                                          
DMRSEQ   DC    C'DMRSEQ'                                                        
DMREAD   DC    C'DMREAD'                                                        
GETREC   DC    C'GETREC'                                                        
ACDIR    DC    C'ACCDIR '                                                       
ACMST    DC    C'ACCMST '                                                       
DMDA     DC    F'0'                                                             
DA       DC    F'0'                                                             
*                                                                               
SORTCARD DC    C'SORT FIELDS=(29,42,A,23,3,A,9,4,A),FORMAT=BI,WORK=1 '          
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(1100,,,,)  '                             
         EJECT                                                                  
* READ SORTED RECORDS AND PRINT AGENCY/DDS PAGES                                
*                                                                               
CE20     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R8,15,4(R1)                                                      
         BZ    CEEOJ                                                            
         B     *+8                                                              
         BAS   RE,DMPGET                                                        
         USING RECDS,R8                                                         
         LA    R3,RRECORD                                                       
         USING ACTRECD,R3                                                       
         LA    R2,ACTRFST                                                       
         USING TRNELD,R2                                                        
         CLI   COMPANY,FRST                                                     
         BE    CE22                                                             
         CLC   COMPANY,ACTKEY     SAME COMPANY                                  
         BE    CE24                                                             
         BAS   RE,COMPEND                                                       
CE22     MVC   COMPANY,ACTKEY                                                   
         BAS   RE,COMPSTRT                                                      
CE24     DS    0H                                                               
         MVC   COMPANY,ACTKEY                                                   
         LA    RE,TOT1             DEBITS                                       
         LA    RF,TOT3                                                          
         TM    TRNSTAT,X'80'                                                    
         BO    *+12                                                             
         LA    RE,TOT2             CREDITS                                      
         LA    RF,TOT4                                                          
         AP    0(6,RE),TRNAMNT                                                  
         AP    0(6,RF),TRNAMNT                                                  
         BAS   RE,OUTDET                                                        
         B     CE20                                                             
*                                                                               
*                                                                               
CEEOJ    DS    0H                                                               
         CLI   COMPANY,FRST                                                     
         BE    *+8                                                              
         BAS   RE,COMPEND                                                       
         MVC   MID1,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   MID1+1(10),=C'DDS TOTALS'                                        
         MVC   MID2+25(6),=C'DEBITS'                                            
         MVC   MID2+37(7),=C'CREDITS'                                           
         EDIT  TOT3,(12,P+20),2,MINUS=YES                                       
         EDIT  TOT4,(12,P+33),2,MINUS=YES                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   COMPANY,FRST                                                     
         BE    *+8                                                              
         BAS   RE,CLOSWRK                                                       
         XBASE                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT TRANSACTION DETAILS                             
         SPACE 1                                                                
         USING TRNELD,R2                                                        
         USING TRNRECD,R3                                                       
         USING PRINTD,R6                                                        
         USING RECDS,R8                                                         
OUTDET   NTR1                                                                   
         CLI   RRECTY,1            IGNORE COPIES                                
         BE    XIT                                                              
         LA    R6,P                                                             
         MVI   PACT,C'A'           FLAG ADD                                     
         CLI   RRECTY,3                                                         
         BE    *+8                                                              
         MVI   PACT,C'C'           FLAG CHANGE                                  
         MVC   PACCOUNT,TRNKEY+1                                                
         MVC   PCNTRA,TRNKULC                                                   
         GOTO1 =V(DATCON),DMCB,(1,TRNKDATE),(8,PDATE)                           
         MVC   PREF,TRNKREF                                                     
         ZIC   RF,TRNKSBR                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSUB,DUB+6(2)                                                    
         SPACE 1                                                                
         EDIT  (B2,RTRM),(4,PTRM)                                               
         TM    TRNSTAT,X'80'      DEBIT                                         
         BZ    OUT2                                                             
         AP    DRTOT,TRNAMNT                                                    
         EDIT  TRNAMNT,(12,PDR),2,MINUS=YES                                     
         B     OUT4                                                             
         SPACE 1                                                                
OUT2     AP    CRTOT,TRNAMNT                                                    
         EDIT  TRNAMNT,(12,PCR),2,MINUS=YES                                     
         SPACE 1                                                                
OUT4     GOTO1 =V(PRINTER)                                                      
         B     XIT                                                              
         EJECT                                                                  
* HANDLE END-OF-COMPANY BREAK                                                   
*                                                                               
         USING TRNRECD,R3                                                       
COMPSTRT NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         BAS   RE,GETNAME                                                       
         MVC   MID1,SPACES                                                      
         MVC   MID1+1(7),=C'COMPANY'                                            
         MVC   MID1+15(36),WORK                                                 
         MVC   SUB1,MYSUB1                                                      
         MVC   SUB2,MYSUB2                                                      
         B     XIT                                                              
*                                                                               
COMPEND  NTR1                                                                   
         MVC   P+1(14),=C'COMPANY TOTALS'                                       
         LA    R6,P                                                             
         USING PRINTD,R6                                                        
         EDIT  DRTOT,(12,PDR),2,MINUS=YES                                       
         EDIT  CRTOT,(12,PCR),2,MINUS=YES                                       
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
*                                                                               
         LA    R4,IOAREA1+4                                                     
         USING PSSUBFD,R4                                                       
         MVC   PSSBEL(2),=X'521D'                                               
         MVC   PSSBDESC,=CL15'EXPENDITURE'                                      
         ZAP   PSSBRECS,TOT1                                                    
         ZAP   PSSBCASH,TOT2                                                    
         MVI   PSSBCASH+6,0                                                     
         MVC   IOAREA1(4),=X'00220000'                                          
*        MVC   ID+5(1),COMPANY     BUILD REST OF KEY                            
         MVC   ID(2),COMPID                                                     
         BAS   RE,ADDWRK                                                        
         ZAP   TOT2,=P'0'                                                       
         ZAP   TOT1,=P'0'                                                       
         ZAP   DRTOT,=P'0'                                                      
         ZAP   CRTOT,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
* READ ACCOUNT FILE FOR NAME                                                    
*                                                                               
GETNAME  NTR1                                                                   
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACDIR,KEY,DIRIO                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,DIRIO                                                         
         USING TRNRECD,R3                                                       
         MVC   DA,TRNKDA                                                        
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACMST,DA,IOAREA1,DMWRK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IOAREA1                                                       
         SR    RF,RF                                                            
         LA    RE,TRNRFST                                                       
GETNAME2 CLI   0(RE),0                                                          
         BE    XIT                                                              
         CLI   0(RE),X'20'                                                      
         BE    GETNAME4                                                         
         CLI   0(RE),X'10'                                                      
         BE    GETNAME6                                                         
GETNAME3 IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETNAME2                                                         
*                                                                               
         USING NAMELD,RE                                                        
GETNAME4 MVC   WORK(36),SPACES                                                  
         IC    RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     GETNAME3                                                         
         MVC   WORK(0),NAMEREC                                                  
         DROP  RE                                                               
*                                                                               
         USING CPYELD,RE                                                        
GETNAME6 MVC   COMPID,CPYUID                                                    
         B     GETNAME3                                                         
         DROP  RE                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* WORKER INTERFACE                                                              
*                                                                               
ADDWRK   MVC   COMMAND,=CL6'ADD'                                                
         B     FILE                                                             
         SPACE 1                                                                
CLOSWRK  MVC   COMMAND,=CL6'CLOSE'                                              
         B     FILE                                                             
         SPACE 1                                                                
FILE     NTR1                                                                   
         L     R4,=A(POSTBUFF)                                                  
         LA    R3,IOAREA1                                                       
         GOTO1 =V(WORKER),DMCB,COMMAND,(R4),ID,(R3)                             
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
DMPGET   NTR1                                                                   
         LA    R7,=C'GET'                                                       
         MVC   HALF,0(R8)                                                       
         LH    R3,HALF                                                          
         SPACE 1                                                                
         GOTO1 =V(PRNTBL),DMCB,(3,(R7)),(R8),C'DUMP',(R3),=C'2D'                
         B     XIT                                                              
*                                                                               
MYSUB1   DC    CL132' ACT TERM  ACCOUNT        CONTRA/ACCOUNT  DATE    X        
                REFERENCE SUB/REF         DEBIT        CREDIT   '               
MYSUB2   DC    CL132' --- ----  -------------- --------------  ------- X        
                --------- -------      ------------ ------------'               
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
AC0703D  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
TODAY    DS    CL3                 PACKED                                       
DMCB     DS    6F                                                               
DMWRK    DS    12D                                                              
ID       DS    CL16                                                             
FILNO    DS    CL1                                                              
FILNOB   DS    XL1                                                              
ACCSE    DS    XL1                                                              
COMPID   DS    CL2                                                              
COMMAND  DS    CL6                                                              
WORK     DS    CL100                                                            
TOT1     DS    PL6                 AGENCY DEBITS                                
TOT2     DS    PL6                 AGENCY CREDITS                               
TOT3     DS    PL6                 RUN DEBITS                                   
TOT4     DS    PL6                 CREDITS                                      
DRTOT    DS    PL6                                                              
CRTOT    DS    PL6                                                              
COMPANY  DS    CL1                                                              
DIRIO    DS    CL64                                                             
KEY      DS    CL42                                                             
IOAREA1  DS    2200C                                                            
*                                                                               
*                                                                               
FRST     EQU   0                                                                
         EJECT                                                                  
PRINTD   DSECT                     DSECT TO COVER DETAIL PRINT LINE             
         DS    CL2                                                              
PACT     DS    CL2                 A=ADD, C=CHANGE                              
         DS    CL1                                                              
PTRM     DS    CL4                 TERMINAL NUMBER                              
         DS    CL2                                                              
PACCOUNT DS    CL14                U/L ACCOUNT                                  
         DS    CL1                                                              
PCNTRA   DS    CL14                U/L CONTRA ACOUNT                            
         DS    CL2                                                              
PDATE    DS    CL8                 DATE                                         
         DS    CL1                                                              
PREF     DS    CL6                 REFERENCE                                    
         DS    CL4                                                              
PSUB     DS    CL3                 SUB-REF                                      
         DS    CL10                                                             
PDR      DS    CL12                DEBITS                                       
         DS    CL1                                                              
PCR      DS    CL12                CREDITS                                      
         EJECT                                                                  
* DSECT FOR RECOVERY HEADER                                                     
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    CL4                                                              
* DMRCVRHDR                                                                     
       ++INCLUDE DMRCVRHDR                                                      
RRECORD  DS    0C                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
TRKBUFF  CSECT                                                                  
         DS    50000C                                                           
SORTAREA CSECT                                                                  
         DS    41000C                                                           
POSTBUFF CSECT                                                                  
         DC    4500X'00'                                                        
         DC    4500X'00'                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACREP0703 03/06/03'                                      
         END                                                                    
