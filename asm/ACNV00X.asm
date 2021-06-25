*          DATA SET ACNV00X    AT LEVEL 043 AS OF 09/12/00                      
*PHASE ACNV00A                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE BUFFAHI                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE CARDS                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE CHOPPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DECODE                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE XSORT                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE KHDUMMY                                                                
*ENTRY ACNV00                                                                   
         TITLE 'ACCPAK CONVERSION CONTROLLER'                                   
ACNV00   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**CNVC**,WORK=V(REGSAVE),RA                                    
         L     R9,=A(ACNVC)                                                     
         USING ACNVC,R9                                                         
*                                                                               
         GOTO1 AINIT               READ CARDS AND INITILIZE                     
*                                                                               
         GOTO1 STXITER,DMCB,DMPL1                                               
         XC    DKEY,DKEY                                                        
         CLI   DUPERR,0            BAD TABLE                                    
         BNE   LAST                GET OUT                                      
         CLI   ACTNSW,ACTNCON      CONVERSION ?                                 
         BE    ACNV2                                                            
         CLI   ACTNSW,ACTNCHG      CHANGE ?                                     
         BE    ACNV2                                                            
         L     RF,ACNVKEYS                                                      
         CLI   ACTNSW,ACTNKEY      TRANSACTION KEY SORT ?                       
         BE    ACNV1                                                            
         L     RF,ACNVMRGE                                                      
         CLI   ACTNSW,ACTNMRG      MERGE ?                                      
         BE    ACNV1                                                            
         DC    H'0'                UNKNOWN ACTION                               
*                                                                               
ACNV1    BASR  RE,RF                                                            
         B     XALL                                                             
         EJECT                                                                  
***********************************************************************         
* GET INPUT - PROCESS THE RECORDS                                     *         
***********************************************************************         
                                                                                
ACNV2    TM    CCNTL,CCNTLS        TEST PARTIAL 'COPY'                          
         BNO   *+8                                                              
         BAS   RE,PCPY             COPY PART OF TEST COMPANY                    
*                                                                               
ACNV3    BAS   RE,GETIN            GET INPUT RECORD                             
         TM    DMBYTE,X'80'        TEST EOF                                     
         BO    LAST                                                             
         BAS   RE,SETIN            SET INPUT RECORD TYPE                        
         BNE   ACNV5                                                            
         CLC   CURCOMP,LASTACT     CHANGE OF COMPNAY                            
         BE    ACNV4                                                            
         BAS   RE,PUTCHD           PUT LAST CONTRA-HEADERS                      
         BAS   RE,PUTOFA           AND OFFICE/ACCOUNT FOR LAST COMPANY          
*                                                                               
ACNV4    TM    CCNTL,CCNTLS        TEST PARTIAL 'COPY'                          
         BO    ACNV7               ALREADY GOT IT                               
         CLC   CURCOMP,XCOMPANY    TEST COMPANY BEING COPIED                    
         BNE   ACNV7                                                            
         BAS   RE,COPY             COPY TO TAPE                                 
         TM    XSTAT,XSTAMRG       IF MERGING DON'T KEEP OLD RECORD             
         BO    ACNV3                                                            
*                                                                               
ACNV5    TM    OUTPUT,TEST         ONLY OUTPUT TEST AGENCY                      
         BNO   ACNV6                                                            
         CLI   RECTYP,ACRTOTHR                                                  
         BNL   ACNV3                                                            
         CLC   CURCOMP,HICOMP      TEST PASSED CONVERSION COMPANIES             
         BH    LAST                                                             
ACNV6    BAS   RE,PUT2             PUT TO TAPE                                  
         AP    CNTOTH,PONE         AND TO NON-CONVERSION                        
         B     ACNV3                                                            
*                                                                               
ACNV7    L     R2,AOUT                                                          
         USING ACCRECD,R2                                                       
         MVI   ERRNUM,0            CLEAR ERROR SWITCH                           
         MVI   TRCCNT,0            CLEAR TRACE COUNT                            
         MVI   ITEMERR,C'N'        SET NO ERRORS                                
         TM    ACCRSTA,X'80'       TEST ACCOUNT DELETED                         
         BNO   ACNV9                                                            
         BAS   RE,DDEL             COUNT AND DUMP DELETED RECORDS               
         B     ACNV3                                                            
*                                                                               
ACNV9    MVI   MODE,PROCBFR        HOOK BEFORE CONVERT                          
         BAS   RE,GOHOOK                                                        
         BAS   RE,KEYC             KEY CONVERSION                               
         BNE   ACNV3               DELETE RECORD                                
         GOTO1 ACNVELM             ELEMENT CONVERSION ROUTINE                   
         MVI   MODE,PROCAFT        HOOK AFTER                                   
         BAS   RE,GOHOOK                                                        
*                                                                               
         CLI   ITEMERR,C'N'        TEST ERRORS                                  
         BE    ACNV11                                                           
         BAS   RE,DNOK             DUMP NOT OK RECORDS                          
         B     ACNV13                                                           
*                                                                               
ACNV11   BAS   RE,DOK              DUMP SOME OK RECORDS                         
*                                                                               
ACNV13   BAS   RE,PUT              BUILD OUTPUT FILES                           
         B     ACNV3                                                            
         EJECT                                                                  
***********************************************************************         
* PASS CONTROL TO HOOK                                                *         
***********************************************************************         
                                                                                
GOHOOK   LR    R0,RE                                                            
         XC    HOOKCB,HOOKCB                                                    
         TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNOR  RE                  NO HOOK                                      
         GOTO1 ACNVHOOK            CALL USER HOOK                               
         OC    HKNREC,HKNREC       TEST NEW RECORD TO BE ADDED                  
         BZ    *+8                                                              
         BAS   RE,NEWR             NEW RECORD TO BE ADDED                       
         TM    HKSTA,HKSDEL        TEST DELETE                                  
         BNO   GOHOOKX                                                          
         BAS   RE,DDEL             DUMP DELETED RECORDS                         
         LA    R0,ACNV3            SET NEW RETURN ADDRESS                       
GOHOOKX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LAST ROUTINES                                                       *         
***********************************************************************         
                                                                                
LAST     DS    0H                                                               
         BAS   RE,PUTCHD           PUT LAST CONTRA-HEADERS                      
         BAS   RE,PUTOFA           PUT LAST OFFICE/ACCOUNT RECORDS              
         GOTO1 ARPT                PRINT THE REPORTS                            
                                                                                
LAST3    CLOSE (TINT)                                                           
         CLOSE (TOUT)                                                           
         CLI   ACTNSW,ACTNCHG      TEST ACCOUNT CHANGE                          
         BE    XALL                NO OTHER FILES                               
         CLOSE (TTRN)                                                           
         CLOSE (TACC)                                                           
         CLOSE (TKEY)                                                           
XALL     XBASE                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* KEY CHANGE ROUTINES                                                 *         
***********************************************************************         
                                                                                
KEYC     NTR1  ,                                                                
         TM    OUTPUT,DISKO        TEST WRITING TO DISK                         
         BNO   KEYC1                                                            
         MVC   OLDKEY,0(R2)        KEYS CAN'T CHANGE                            
         MVC   NEWKEY,0(R2)                                                     
         BAS   RE,SETLGR           SET LEDGER TYPE                              
         B     XIT                                                              
*                                                                               
KEYC1    LA    RF,TRNC                                                          
         CLI   RECTYP,ACRTTIM      TIME RECORD                                  
         BE    KEYC3                                                            
         LA    RF,SPLC                                                          
         CLI   RECTYP,ACRTOTHR     TEST SPECIAL RECORDS                         
         BNL   KEYC3                                                            
         SR    RF,RF                                                            
         IC    RF,RECTYP           ROUTINE BASED ON RECORD TYPE                 
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LA    RE,KEYTAB(RF)                                                    
         ICM   RF,15,0(RE)                                                      
KEYC3    BASR  RE,RF                                                            
         B     XIT                                                              
                                                                                
KEYTAB   DC    A(CPYC)             1  COMPANY                                   
         DC    A(UNTC)             2  UNIT                                      
         DC    A(LDGC)             3  LEDGER                                    
         DC    A(ACTHC)            4  ACCOUNT HIGH                              
         DC    A(ACTLC)            5  ACCOUNT LOW                               
         DC    A(OFAC)             6  OFFICE/ACCOUNT                            
         DC    A(0)                7  CONTRA/HEADER (PASSIVE)                   
         DC    A(CACC)             8  CONTRA/HEADER (REAL)                      
         DC    A(CACC)             9  CONTRA/HEADER (BUCKETS)                   
         DC    A(TRNC)             10 TRANSACTION                               
         DC    A(TRNC)             11 TRANSACTION (ARCHIVED)                    
         EJECT                                                                  
***********************************************************************         
* COMPANY RECORDS                                                     *         
***********************************************************************         
                                                                                
         USING CPYRECD,R2                                                       
CPYC     NTR1  ,                                                                
         MVC   OLDKEY,CPYKEY       SAVE COMPANY KEY                             
         MVC   NEWKEY,CPYKEY                                                    
         B     KEYX                                                             
         EJECT                                                                  
***********************************************************************         
* UNIT  RECORDS                                                       *         
***********************************************************************         
                                                                                
         USING UNTRECD,R2                                                       
UNTC     NTR1  ,                                                                
         MVC   OLDKEY,UNTKEY       SAVE LEDGER KEY                              
         MVC   NEWKEY,UNTKEY                                                    
         B     KEYX                                                             
         EJECT                                                                  
***********************************************************************         
* LEDGER RECORDS                                                      *         
***********************************************************************         
                                                                                
         USING LDGRECD,R2                                                       
LDGC     NTR1  ,                                                                
         MVC   OLDKEY,LDGKEY       SAVE LEDGER KEY                              
         MVC   NEWKEY,LDGKEY                                                    
         BAS   RE,SETLGR           SET LEDGER RECORD                            
         B     KEYX                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT HIGH                                                        *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
ACTHC    NTR1  ,                                                                
         MVC   OLDKEY,ACTKEY       SAVE ACCOUNT KEY                             
         MVC   NEWKEY,ACTKEY                                                    
*                                                                               
         CLC   LGRCDE,ACTKULA      TEST CORRECT LEDGER                          
         BE    *+8                                                              
         BAS   RE,SETLGR                                                        
*                                                                               
         TM    LGRTYP,LTPROD+LTPCON   TEST PRODUCTION & PC                      
         BNZ   PRODC                                                            
         B     KEYX                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LOW                                                         *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
ACTLC    NTR1  ,                                                                
         BAS   RE,PUTCHD           PUT LAST CONTRA-HEADERS                      
         BAS   RE,PUTOFA           PUT LAST OFFICE/ACCOUNT RECORDS              
*                                                                               
         MVC   OLDKEY,ACTKEY       SAVE ACCOUNT KEY                             
         MVC   NEWKEY,ACTKEY                                                    
         CLC   LGRCDE,ACTKULA      TEST CORRECT LEDGER                          
         BE    *+8                                                              
         BAS   RE,SETLGR                                                        
*                                                                               
         TM    LGRTYP,LTPROD+LTPCON TEST PRODUCTION & PC                        
         BNZ   PRODC                                                            
         TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSIONS                     
         BNO   KEYX                                                             
         MVC   SRCHARG(L'ACCO),OLDULA                                           
         GOTO1 ASRCH,SRCHA         SEARCH ACCOUNT TABLE                         
         BNE   KEYX                                                             
         MVC   NEWULA,ACCN         MOVE IN NEW ACCOUNT                          
         B     KEYX                                                             
         EJECT                                                                  
***********************************************************************         
* PRODUCTION CONVERSION                                               *         
***********************************************************************         
                                                                                
PRODC    TM    CNVSW,CNVSCC        TEST CLIENT CODE CONVERSION                  
         BNO   PRODC3                                                           
         MVC   SRCHARG(L'CLIO),OLDCC                                            
         GOTO1 ASRCH,SRCHCC        SEARCH FOR CLIENT CODE                       
         BNE   PRODC3                                                           
         MVC   NEWCC,CLIN          NEW CLIENT CODE                              
*                                                                               
PRODC3   TM    CNVSW,CNVSMC        TEST MEDIA CODE CONVERSION                   
         BNO   KEYX                                                             
         CLI   OLDMC,C' '          TEST LOW LEVEL ACCOUNT                       
         BNH   KEYX                                                             
         MVC   SRCHARG(L'MEDO),OLDMC                                            
         GOTO1 ASRCH,SRCHMC        SEARCH FOR MEDIA CODE                        
         BNE   KEYX                                                             
         MVC   NEWMC,MEDN          NEW MEDIA CODE                               
         B     KEYX                                                             
         EJECT                                                                  
***********************************************************************         
* OFFICE/ACCOUNT                                                      *         
***********************************************************************         
                                                                                
         USING OFARECD,R2                                                       
OFAC     NTR1  ,                                                                
         TM    ACTSTA,ACTSDEL      TEST ACCOUNT DELETED                         
         BO    DELX                                                             
         TM    COMPSTA,COMPNOFF    TEST ON NEW OFFICES                          
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   OLDULC,OFAKEY       TEST SAME ACCOUNT                            
         BE    *+6                                                              
         DC    H'0'                EXPECTED SAME ACCOUNT                        
         MVC   OLDKEY,OFAKEY       SAVE OLD KEY                                 
         MVC   NEWOFF(L'NEWOFF+L'NEWCDTL),OLDOFF SAVE REST OF KEY               
*                                                                               
         TM    CNVSW,CNVSCO        TEST CLIENT/OFFICE CONVERSION                
         BZ    OFAC3                                                            
         BAS   RE,UPBAL            ADD BBF TO OFFICE/ACCOUNT TABLE              
         B     DELX                MUST REBUILD ALL OFFICE/ACCOUNTS             
*                                                                               
OFAC3    TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNO   KEYX                                                             
         MVC   SRCHARG(L'NEWOFF),NEWOFF                                         
         GOTO1 ASRCH,SRCHO         SEARCH FOR OFFICE                            
         BNE   KEYX                                                             
         MVC   NEWOFF,OFFN         NEW OFFICE                                   
         B     KEYX                                                             
         EJECT                                                                  
***********************************************************************         
* CONTRA ACCOUNT                                                      *         
***********************************************************************         
                                                                                
         USING CACRECD,R2                                                       
CACC     NTR1  ,                                                                
         CLI   RECTYP,ACRTCHDH     TEST REAL CONTRA-HEADER                      
         BNE   *+8                                                              
         BAS   RE,PUTCHD           BUILD OFFICE-CONTRA RECORDS FOR LAST         
*                                                                               
         TM    ACTSTA,ACTSDEL      TEST ACCOUNT DELETED                         
         BO    DELX                                                             
         CLC   OLDULC,CACKEY       TEST SAME ACCOUNT                            
         BE    *+6                                                              
         DC    H'0'                EXPECTED SAME ACCOUNT                        
*                                                                               
         MVC   OLDKEY,CACKEY              SAVE CONTRA                           
*****    TM    COMPSTA,COMPNOFF    TEST ON NEW OFFICES                          
*****    BO    *+10                                                             
*****    MVC   NEWOFF,SPACE                                                     
*****    MVC   NEWCCMP(L'NEWCDTL),OLDCCMP SAVE REST OF KEY                      
         MVC   NEWOFF(L'NEWCDTL+L'NEWOFF),OLDOFF  SAVE REST OF KEY              
         TM    LGRTYP,LTPROD+LTPCON TEST  PRODUCTION LEDGER                     
         BZ    CACC3                                                            
         TM    CNVSW,CNVSWC        TEST WORKCODE CONVERSION                     
         BNO   CACC3                                                            
         CLI   OLDWC,C' '                                                       
         BNH   CACC3                                                            
         MVC   SRCHARG(L'WRKO),OLDWC                                            
         GOTO1 ASRCH,SRCHWC        SEARCH FOR WORKCODE                          
         BNE   *+10                                                             
         MVC   NEWWC,WRKN                                                       
*                                                                               
CACC3    MVI   CLGTYP,0                                                         
         LA    RF,LGRTTAB                                                       
CACC5    CLC   0(2,RF),CACKCUNT    TEST CONTRA LEDGER CODE                      
         BNE   *+14                                                             
         OC    CLGTYP,2(RF)        SET CONTRA LEDGER TYPE                       
         B     CACC7                                                            
         CLI   0(RF),EOT                                                        
         BE    CACC7                                                            
         LA    RF,L'LGRTTAB(RF)                                                 
         B     CACC5                                                            
*                                                                               
CACC7    MVC   NEWCULC,CACKCULC                                                 
         TM    LGRTYP,LTRECV       TEST RECEIVABLE LEDGER                       
         BNO   CACC9                                                            
         TM    CNVSW,CNVSBS        TEST CHANGING BILLING SOURCE                 
         BNO   CACC9                                                            
         CLC   CACKCCPY,CACKCPY    TEST CONTRA COMPANY                          
         BE    CACC9                                                            
         MVC   SRCHARG(L'BLSO),OLDCACC  OLD BILLING SOURCE                      
         GOTO1 ASRCH,SRCHBS        SEARCH FOR BILL SOURCE                       
         BNE   CACC9                                                            
         MVC   NEWCACC,BLSN        REPLACE SOURCE                               
*                                                                               
CACC9    TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNO   CACC11                                                           
         CLC   CACKCCPY,CACKCPY    TEST CONTRA COMPANY                          
         BNE   CACC11                                                           
         MVC   SRCHARG(L'CONO),OLDCULA                                          
         GOTO1 ASRCH,SRCHC         SEARCH FOR CONTRA                            
         BNE   CACC11                                                           
         MVC   NEWCULA,CONN                                                     
         BAS   RE,FIXNAM           FIX CONTRA NAME ELEMENT                      
*                                                                               
CACC11   TM    CLGTYP,LTPROD+LTPCON TEST CONTRA IS SJ OR 1J                     
         BZ    CACC21                                                           
         TM    CNVSW,CNVSCC        TEST CLIENT CODE CONVERSION                  
         BNO   CACC13                                                           
         MVC   SRCHARG(L'CLIO),OLDCCC                                           
         GOTO1 ASRCH,SRCHCC        SEARCH FOR CLIENT CODE                       
         BNE   CACC13                                                           
         MVC   NEWCCC,CLIN         NEW CLIENT CODE                              
*                                                                               
CACC13   TM    CNVSW,CNVSMC        TEST MEDIA CODE CONVERSION                   
         BNO   CACC21                                                           
         CLI   OLDCMC,C' '                                                      
         BNH   CACC21                                                           
         MVC   SRCHARG(L'MEDO),OLDCMC                                           
         GOTO1 ASRCH,SRCHMC        SEARCH FOR MEDIA CODE                        
         BNE   CACC21                                                           
         MVC   NEWCMC,MEDN         NEW  MEDIA CODE                              
*                                                                               
CACC21   DS    0H                                                               
         LA    R3,CACRFST          FIX CONTRA ELEMENT                           
         USING CACELD,R3                                                        
         CLI   CACEL,CACELQ                                                     
         BNE   KEYX                                                             
         MVC   CACCNT,NEWCULC                                                   
         B     KEYX                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TRANSACTION                                                         *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
TRNC     NTR1  ,                                                                
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         TM    ACTSTA,ACTSDEL      TEST ACCOUNT DELETED                         
         BO    DELX                                                             
         CLC   OLDKEY(32),TRNKEY   TEST ACC/CONTRA                              
         BE    TRNC1                                                            
         CLC   TRNKWORK,=C'**'                                                  
         BE    TRNC1                                                            
         CP    TRNAMNT,=P'0'                                                    
         BE    *+10                                                             
         NOP   DELX                                                             
         DC    H'0'                EXPECTED SAME ACCOUNT AND CONTRA             
*                                                                               
         MVC   OLDCULC,TRNKCULC                                                 
         MVC   NEWCULC,OLDCULC                                                  
         TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNO   TRNC1                                                            
         CLC   TRNKCCPY,TRNKCPY    TEST CONTRA COMPANY                          
         BNE   TRNC1                                                            
         MVC   SRCHARG(L'CONO),OLDCULA                                          
         GOTO1 ASRCH,SRCHC         SEARCH FOR CONTRA                            
         BNE   TRNC1                                                            
         MVC   NEWCULA,CONN                                                     
*                                                                               
TRNC1    MVC   OLDKEY,TRNKEY                                                    
*                                                                               
         CLC   NEWCULC,SPACE       IF NO CONTRA HEADER                          
         BNE   *+10                                                             
         MVC   NEWCULC,OLDCULC     RESTORE OLD CONTRA ACCOUNT                   
         MVC   NEWDTRFS,OLDDTRFS   SAVE DATE, REF, SUB                          
         SR    R0,R0                                                            
*                                                                               
         CLI   TRNEL,TRNELQ                                                     
         BNE   *+12                                                             
         LA    RF,TRNOFFC                                                       
         B     TRNC5                                                            
*                                                                               
         USING TIMELD,R3                                                        
TRNC3    CLI   TIMEL,TIMELQ        TMS ELEMENT                                  
         BNE   *+12                                                             
         LA    RF,TIMOFF                                                        
         B     TRNC5                                                            
         CLI   TIMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,TIMLN                                                         
         AR    R3,R0                                                            
         B     TRNC3                                                            
*                                                                               
         USING TRNELD,R3                                                        
TRNC5    TM    LGRTYP,LTPROD+LTPCON  TEST PRODUCTION OR PC                      
         BZ    TRNC7                                                            
         MVC   TRNANAL,NEWWC       SET WORDCODE FROM KEY                        
         B     KEYX                                                             
*                                                                               
TRNC7    TM    COMPSTA,COMPNOFF    ALREADY ON NEW OFFICES                       
         BNO   TRNC8                                                            
         CLC   0(2,RF),=C'**'      LEAVE ORDERS ALONE                           
         BE    KEYX                                                             
         MVC   0(2,RF),NEWOFF      SET OFFICE FROM KEY                          
         B     KEYX                                                             
*                                                                               
TRNC8    MVC   OFFICE,0(RF)        OLD OFFICE FROM TRANSACTION                  
         TM    CNVSW,CNVSCO        TEST CLIENT/OFFICE CONVERSION                
         BZ    TRNC10                                                           
         LA    R4,OLDCCC                                                        
         TM    CLGTYP,LTPROD+LTPCON TEST CONTRA PROD OR PROJ. CONTROL           
         BNZ   TRNC9                                                            
         LA    R4,OLDCACC+9                                                     
         TM    LGRTYP,LTMPAY       MEDIA PAYABLE                                
         BNO   TRNC10                                                           
*                                                                               
TRNC9    CLI   0(R4),C' '          TEST CLIENT CODE                             
         BNH   TRNC10                                                           
         MVC   SRCHARG(L'CLOC),0(R4)                                            
         GOTO1 ASRCH,SRCHCO        SEARCH FOR CLIENT CODE                       
         BNE   TRNC9A                                                           
         MVC   OFFICE,CLOO         SET NEW OFFICE CODE                          
         B     TRNC13                                                           
*                                                                               
TRNC9A   TM    ERRCB+((ERRCLO*2)+1),ERRIGN  TEST 'IGNORE' NOT FOUND             
         BO    TRNC10                       CHECK OFFICE CONVERT                
         B     TRNC13                                                           
*                                                                               
TRNC10   TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNO   TRNC13                                                           
         MVC   SRCHARG(L'OFFICE),OFFICE                                         
         GOTO1 ASRCH,SRCHO         SEARCH FOR OFFICE                            
         BE    TRNC11                                                           
         CLI   DFLTOFF,C' '        TEST DEFAULT                                 
         BNH   TRNC13                                                           
         MVC   OFFICE,DFLTOFF      USE DEFAULT                                  
         B     TRNC13                                                           
*                                                                               
TRNC11   MVC   OFFICE,OFFN         SET NEW OFFICE                               
*                                                                               
TRNC13   MVC   TRNOFFC,OFFICE      SET OFFICE IN ELEMENT                        
TRNC15   TM    COMPSTA,COMPOTON    OLD TO NEW OFFICES                           
         BZ    KEYX                                                             
         MVC   NEWOFF,OFFICE       SET OFFICE IN KEY                            
         CLI   NEWOFF,C' '                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   NEWOFF+1,C' '                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         B     KEYX                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SPECIAL RECORD CONVERSION                                           *         
***********************************************************************         
                                                                                
SPLC     NTR1  ,                                                                
         L     R2,AOUT                                                          
         MVC   OLDKEY,0(R2)                                                     
         MVC   NEWKEY,0(R2)                                                     
*                                                                               
         TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNO   *+8                                                              
         BAS   RE,ACCK                                                          
         TM    CNVSW,CNVSOF        TEST OFFICE CONVERSION                       
         BNO   *+8                                                              
         BAS   RE,OFFK                                                          
         TM    CNVSW,CNVSMC        TEST MEDIA CONVERSION                        
         BNO   *+8                                                              
         BAS   RE,MEDK                                                          
         TM    CNVSW,CNVSWC        TEST WORKCODE CONVERSION                     
         BNO   *+8                                                              
         BAS   RE,WRKK                                                          
         TM    CNVSW,CNVSCC        TEST CLIENT CODE CONVERSION                  
         BNO   *+8                                                              
         BAS   RE,CLIK                                                          
         TM    CNVSW,CNVSCO        TEST CLIENT OFFICE CONVERSION                
         BNO   *+8                                                              
         BAS   RE,CLOK                                                          
         CLI   XCOMPANY,0          TEST CHANGING COMPANY CODE                   
         BE    *+8                                                              
         BAS   RE,CMPK                                                          
         CLI   KTABSP1R,0          TEST PERSON ACCOUNTS                         
         BE    *+8                                                              
         BAS   RE,PERK                                                          
         EJECT                                                                  
***********************************************************************         
* FINAL KEY AND ELEMENT CHECK                                         *         
***********************************************************************         
                                                                                
*                                                                               
KEYX     L     R2,AOUT            NEW KEY TO RECORD                             
         USING ACCRECD,R2                                                       
         MVC   ACCKEY(L'NEWKEY),NEWKEY                                          
         CLI   RECTYP,ACRTCHDH     TEST REAL CONTRA-HEADER                      
         BNE   *+8                                                              
         BAS   RE,CONP             SAVE CURRENT CONTRA                          
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
DELX     BAS   RE,DDEL             COUNT AND DUMP DELETED                       
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT ACCOUNT CODE - SPECIAL RECORDS                              *         
***********************************************************************         
                                                                                
ACCK     NTR1  ,                                                                
         LA    R3,KTABUL           FIRST ACCOUNT                                
         BAS   RE,ACCK3                                                         
         LA    R3,KTABUL2          SECOND ACCOUNT                               
         BAS   RE,ACCK3                                                         
         B     XIT                                                              
*                                                                               
ACCK3    LR    R0,RE                                                            
         SR    R1,R1                                                            
         MVC   SRCHARG(L'KTABUL),KTABUL-KTABS(R3) DEFAULT U/L                   
         CLC   SRCHARG(L'KTABUL),SPACE                                          
         BNE   ACCK5                                                            
         ICM   R1,1,KTABDUL-KTABS(R3)   DISP. TO U/L                            
         BZ    ACCKX                                                            
         LA    R2,OLDKEY(R1)                                                    
         MVC   SRCHARG(L'KTABUL),0(R2)         OLD U/L                          
*                                                                               
ACCK5    ICM   R1,1,KTABDAC-KTABS(R3)   DISPLACEMENT TO ACCOUNT                 
         BZ    ACCKX                                                            
         LA    R2,OLDKEY(R1)                                                    
         MVC   SRCHARG+L'KTABUL(12),0(R2)      ACCOUNT CODE                     
         GOTO1 ASRCH,SRCHA              SEARCH FOR NEW ACOUNT                   
         BNE   ACCKX                                                            
         SR    R1,R1                                                            
         ICM   R1,1,KTABDUL-KTABS(R3)   DISP. TO U/L                            
         BZ    ACCK7                                                            
         LA    R2,NEWKEY(R1)                                                    
         MVC   0(2,R2),ACCN                                                     
*                                                                               
ACCK7    ICM   R1,1,KTABDAC-KTABS(R3)   DISP. ACCOUNT                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R2,NEWKEY(R1)                                                    
         MVC   0(12,R2),ACCN+2     REPLACE WITH NEW                             
*                                                                               
ACCKX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT OFFICE CODES - SPECIAL RECORDS                              *         
***********************************************************************         
                                                                                
OFFK     NTR1  ,                                                                
         LA    R3,KTABOF           R3=DISP. TO FIRST OFFICE                     
         BAS   RE,OFFK3                                                         
         LA    R3,KTABOF2          R3=DISP. TO SECOND OFFICE                    
         BAS   RE,OFFK3                                                         
         B     XIT                                                              
*                                                                               
OFFK3    LR    R0,RE                                                            
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          DISP. TO OFFICE                              
         BZ    OFFKX                                                            
         LA    R2,OLDKEY(R1)                                                    
         MVC   SRCHARG(L'OFFO),0(R2)    OLD OFFICE                              
         CLI   SRCHARG,C' '        TEST OFFICE IN RECORD                        
         BNH   OFFKX                                                            
         CLI   SRCHARG,X'FF'       ALL OFFICES                                  
         BE    OFFKX                                                            
         GOTO1 ASRCH,SRCHO         SEARCH FOR NEW ACOUNT                        
         BNE   OFFKX                                                            
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          DISP. TO OFFICE                              
         LA    R2,NEWKEY(R1)                                                    
         MVC   0(2,R2),OFFN                                                     
OFFKX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MEDIA CODE - IN SPECIAL RECORDS                                     *         
***********************************************************************         
                                                                                
MEDK     NTR1  ,                                                                
         LA    R3,KTABMC           MEDIA CODE                                   
         CLI   0(R3),0                                                          
         BE    *+8                                                              
         BAS   RE,MEDK3                                                         
         LA    R3,KTABJC           JOB CODE(MEDIA IS FIRST BYTE)                
         CLI   0(R3),0                                                          
         BE    *+8                                                              
         BAS   RE,MEDK3                                                         
         B     XIT                                                              
*                                                                               
MEDK3    LR    R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R2,OLDKEY(R1)                                                    
         CLI   0(R2),C' '                                                       
         BNH   MEDKX                                                            
         MVC   SRCHARG(L'MEDO),0(R2)    OLD CODE                                
         GOTO1 ASRCH,SRCHMC                                                     
         BNE   MEDKX                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R2,NEWKEY(R1)                                                    
         MVC   0(1,R2),MEDN        MOVE IN NEW CODE                             
*                                                                               
MEDKX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* WORKCODE - IN SPECIAL RECORDS                                       *         
***********************************************************************         
                                                                                
WRKK     NTR1  ,                                                                
         CLI   RECTYP,ACRTWCO      TEST WORKCODE RECORD                         
         BNE   *+14                                                             
         USING WCORECD,R2                                                       
         CLC   WCOKUNT(2),=C'SJ'   TEST U/L SJ                                  
         BNE   XIT                                                              
         DROP  R2                                                               
*                                                                               
         LA    R3,KTABWC           WORK CODE                                    
         CLI   0(R3),0                                                          
         BE    *+8                                                              
         BAS   RE,WRKK3                                                         
         B     XIT                                                              
*                                                                               
WRKK3    LR    R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R2,OLDKEY(R1)                                                    
         CLI   0(R2),C' '                                                       
         BNH   WRKKX                                                            
         MVC   SRCHARG(L'WRKO),0(R2)    OLD CODE                                
         GOTO1 ASRCH,SRCHWC                                                     
         BNE   WRKKX                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R2,NEWKEY(R1)                                                    
         MVC   0(2,R2),WRKN        MOVE IN NEW CODE                             
*                                                                               
WRKKX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CLIENT CODE IN - SPECIAL RECORDS                                    *         
***********************************************************************         
                                                                                
CLIK     NTR1  ,                                                                
         LA    R3,KTABCC           CLIENT CODE                                  
         CLI   0(R3),0                                                          
         BE    *+8                                                              
         BAS   RE,CLIK3                                                         
         B     XIT                                                              
*                                                                               
CLIK3    LR    R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R2,OLDKEY(R1)                                                    
         CLI   0(R2),C' '                                                       
         BNH   CLIKX                                                            
         MVC   SRCHARG(L'CLIO),0(R2)    OLD CODE                                
         GOTO1 ASRCH,SRCHCC                                                     
         BNE   CLIKX                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R2,NEWKEY(R1)                                                    
         MVC   0(3,R2),CLIN        MOVE IN NEW CODE                             
*                                                                               
CLIKX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CLIENT OFFICE  - SPECIAL RECORDS                                    *         
***********************************************************************         
                                                                                
CLOK     LR    R0,RE                                                            
         CLI   RECTYP,ACRTPOP      OPTIONS                                      
         BNE   CLOKX                                                            
         LA    R3,NEWKEY                                                        
         USING POPRECD,R3                                                       
         OC    POPKOFC,POPKOFC     TEST ANY OFFICE                              
         BZ    CLOKX                                                            
         OC    POPKCLI,POPKCLI     TEST ANY CLIENT                              
         BZ    CLOKX                                                            
         MVC   SRCHARG(L'CLOC),POPKCLI                                          
         GOTO1 ASRCH,SRCHCO        SEARCH FOR CLIENT                            
         BNE   CLOKX                                                            
         MVC   POPKOFC,CLOO        NEW OFFICE TO KEY                            
         MVC   SRCHARG(L'OFFO),POPKOFC                                          
         GOTO1 ASRCH,SRCHO         SEARCH FOR OFFICE                            
         BNE   CLOKX                                                            
         MVC   POPKOFC,OFFN        NEW OFFICE TO KEY                            
CLOKX    LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SECOND COMPANY CODE IN KEY OF SPECIAL RECORD                        *         
***********************************************************************         
                                                                                
CMPK     CLI   KTABCP2,0           TEST SECOND COMPANY IN KEY                   
         BER   RE                                                               
         CLI   XCOMPANY,0          TEST COPY/MERGE COMPANY                      
         BER   RE                                                               
         SR    R1,R1                                                            
         IC    R1,KTABCP2          R1=DISP. TO COMPANY CODE                     
         LA    R3,OLDKEY(R1)                                                    
         CLC   0(1,R3),XCOMPANY                                                 
         BNER  RE                                                               
         MVC   0(1,R3),COMPANY     REPLACE IT                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PERSON KEY - PARSE INTERFACE - OFF/DEPT/SUB/PERSON                  *         
***********************************************************************         
                                                                                
PERK     NTR1  ,                                                                
         TM    CNVSW,CNVSAC        TEST ACCOUNT CONVERSION                      
         BNO   XIT                                                              
         L     R5,APERT            TABLE OF PERSON KEYS                         
PERK3    CLC   RECTYP,0(R5)        MATCH RECORD TYPE                            
         BE    PERK5                                                            
         CLI   0(R5),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                MUST DEFINE PERSON KEY                       
         LA    R5,5(R5)                                                         
         B     PERK3                                                            
*                                                                               
PERK5    LA    R5,1(R5)                                                         
         ST    R5,PARCTAB          SET A(PARSE TABLE)                           
         L     R2,AINP                                                          
         ST    R2,PARINP           SET A(INPUT KEY)                             
         LA    R2,NEWKEY                                                        
         ST    R2,PAROUT           SET A(OUTPUT KEY)                            
         GOTO1 APAR                PARSE THE 1R KEY                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET LEDGER ENTRY                                                    *         
***********************************************************************         
                                                                                
         USING LDGRECD,R2                                                       
SETLGR   NTR1  ,                                                                
         MVI   LGRTYP,0                                                         
         LA    RF,LGRTTAB                                                       
SETLGR3  CLC   0(2,RF),LDGKUNT     TEST LEDGER CODE                             
         BNE   *+14                                                             
         OC    LGRTYP,2(RF)        SET LEDGER TYPE                              
         B     SETLGR5                                                          
         CLI   0(RF),EOT                                                        
         BE    SETLGR5                                                          
         LA    RF,L'LGRTTAB(RF)                                                 
         B     SETLGR3                                                          
*                                                                               
SETLGR5  MVC   SRCHARG(L'OLDUL),OLDUL                                           
         GOTO1 ASRCH,SRCHL         SEARCH FOR LEDGER ENTRY                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE BALANCE ENTRY IN OFFICE ACCOUNT TABLE                        *         
***********************************************************************         
                                                                                
UPBAL    NTR1  ,                                                                
         L     R2,AOUT             GET BALANCE ELEMENT                          
         USING OFARECD,R2                                                       
         LA    R4,OFARFST                                                       
         SR    R0,R0                                                            
UPBAL3   CLI   0(R4),0             EOR?                                         
         BE    XIT                 JUST LEAVE IF NO BALANCE ELEMENT             
         USING ABLELD,R4                                                        
         CLI   0(R4),ABLELQ                                                     
         BE    UPBAL5                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     UPBAL3                                                           
*                                                                               
UPBAL5   CP    ABLFRWD,=P'0'        TEST ANY BALANCE                            
         BE    XIT                                                              
         USING OFATABD,R5                                                       
         L     R5,AOFATAB           SEE IF ENTRY EXISTS IF OFF\ACCT             
*                                                                               
UPBAL7   CLI   0(R5),OFATEOTQ                                                   
         BE    UPBAL9                                                           
         CLC   OFATOFFC,OFAKOFF                                                 
         BE    UPBAL11                                                          
         LA    R5,OFATABLN(R5)                                                  
         B     UPBAL7                                                           
*                                                                               
UPBAL9   MVC   OFATOFFC,OFAKOFF     ADD NEW ENTRY                               
         ZAP   OFATBALF,=P'0'       INITIALIZE BBF                              
         ZAP   OFATTOTD,=P'0'       INITIALIZE BBF                              
         ZAP   OFATTOTC,=P'0'       INITIALIZE BBF                              
         MVC   OFATLMOS,EFFS                                                    
         XC    OFATHMOS,OFATHMOS                                                
         MVI   OFATABLN(R5),X'FF'   MARK NEW END OF TABLE                       
*                                                                               
UPBAL11  AP    OFATBALF,ABLFRWD     ACCUMULATE BBF                              
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* FIX CONTRA ACCOUNT NAME ELEMENTS                                    *         
***********************************************************************         
                                                                                
FIXNAM   NTR1  ,                                                                
         CLI   CONNME,X'40'        TEST NAME PRESENT                            
         BNH   XIT                                                              
         L     R2,AOUT                                                          
         USING CACRECD,R2                                                       
         LA    R3,CACRFST                                                       
         USING CACELD,R3                                                        
         CLI   CACEL,CACELQ        TEST CONTRA ELEMENT                          
         BNE   XIT                                                              
         XC    ELEMENT,ELEMENT                                                  
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ELEMENT(0),0(R3)                                                 
         MVI   0(R3),X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',(R2)),0                          
         LA    R3,ELEMENT                                                       
         MVC   CACNAME,CONNME                                                   
         LA    R1,CACNAME+L'CACNAME-1                                           
         LA    RF,L'CACNAME                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                BAD NAME                                     
         LA    RF,CACLN1Q(RF)                                                   
         STC   RF,CACLN                                                         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3),0                             
         LA    R3,CACRFST                                                       
         CLI   CACEL,CACELQ        TEST CONTRA ELEMENT STILL THERE              
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE TO OUTPUT FILES                                               *         
***********************************************************************         
                                                                                
PUT      NTR1  ,                                                                
PUT1     LM    R2,R3,AOUTL         R2=A(LENGTH), R3=A(RECORD)                   
         SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R3)                                         
         AH    RF,=H'4'                                                         
         STCM  RF,3,0(R2)                                                       
         CLI   ACTNSW,ACTNCON      TEST ACCOUNT CONVERSION                      
         BE    PUT3                                                             
         BAS   RE,PUT2             CHANGES ACCOUNTS                             
         B     XIT                                                              
*                                                                               
PUT2     NTR1  ,                                                                
         L     R0,AOUTL                                                         
         L     R1,ATOUT                                                         
         PUT   (R1),(R0)           ALL  OUTPUT TO TOUT FOR CHANGE               
         GOTO1 ACNVCNT,DMCB,('RACTOUT',AOUT)                                    
         AP    CNTOUT,PONE         ADD TO TOTAL OUTPUT                          
         B     XIT                                                              
*                                                                               
PUT3     CLI   RECTYP,ACRTNBT      TEST NEW BATCH RECORDS                       
         BNE   PUT5                                                             
         BAS   RE,NBTP             WRITE NEW BATCH RECORDS TO KEY FILE          
         B     PUT13                                                            
*                                                                               
PUT5     CLI   RECTYP,ACRTACTL     TEST ACCOUNT LOW                             
         BNE   PUT7                                                             
         MVC   LASTACT,0(R3)                                                    
         B     PUT9                                                             
*                                                                               
PUT7     CLI   RECTYP,ACRTCHDH     CONTRA HEADER(REAL)                          
         BNE   PUT11                                                            
*                                                                               
PUT9     BAS   RE,WTACC            WRITE OUTPUT TAPE                            
         B     XIT                                                              
*                                                                               
PUT11    TM    RECSTA,RECTRN       TEST TRANSACTION LIKE                        
         BNO   PUT9                                                             
         BAS   RE,TRNP             TRANSACTION & KEY FILE                       
         L     RF,AINP             KEEP OLD KEY FOR TRANSACTIONS                
         MVC   0(L'TRNKEY,R3),0(RF) UNTIL KEYSORT PHASE                         
*                                                                               
PUT13    L     R1,ATTRN                                                         
         L     R0,AOUTL                                                         
         ST    R0,AUPREC                                                        
         PUT   (R1),(R0)                                                        
         GOTO1 ACNVCNT,DMCB,('RACTOUT',(R3))                                    
         AP    CNTTRN,PONE                                                      
         AP    CNTOUT,PONE                                                      
         BAS   RE,UPDN             UPDATE THE FILE                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* NEW BATCH RECORDS TO OUTPUT FILES                                   *         
***********************************************************************         
                                                                                
         USING TBARECD,R3                                                       
NBTP     NTR1  ,                                                                
         LA    R4,KEYWRK                                                        
         USING KEYCD,R4                                                         
         XC    KEYCD(KEYCLNQ),KEYCD                                             
         MVI   KEYTYP,KEYTBAT                                                   
         MVC   KEYREC,TBAKEY       BATCH HEADER KEY                             
         SR    R0,R0                                                            
         LA    R5,TBARFST                                                       
         USING ASKELD,R5                                                        
*                                                                               
NBTP3    CLI   0(R5),0              KEY CHANGE RECORD FOR EACH KEY              
         BE    XIT                                                              
         CLI   0(R5),ASKELQ                                                     
         BNE   NBTP5                                                            
         MVC   KEYTBO,ASKKEY                                                    
         BAS   RE,WTKEY            WRITE THE KEY FILE                           
*                                                                               
NBTP5    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     NBTP3                                                            
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* CONTRA HEADER TO OUTPUT FILES                                      *          
***********************************************************************         
                                                                                
CONP     NTR1  ,                                                                
         LM    R2,R3,AOUTL         R2=A(LENGTH), R3=A(RECORD)                   
         SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R3)                                         
         AH    RF,=H'4'                                                         
         STCM  RF,3,0(R2)                                                       
*                                                                               
         L     RE,AOUTL                                                         
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         L     R0,ACHDRL                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE CURRENT CONTRA HEADER                   
         L     R3,ACHDR                                                         
         AH    R3,=Y(CHDRFST-CHDRECD)                                           
         CLI   0(R3),CACELQ        TEST CONTRA ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT                                                                    
         EJECT                                                                  
***********************************************************************         
* BUILD OFFICE CONTRA HEADER RECORDS FROM CHDTAB                      *         
***********************************************************************         
                                                                                
PUTCHD   NTR1  ,                                                                
         TM    OUTPUT,DISKO                                                     
         BO    XIT                                                              
         L     R3,ACHDR                                                         
         USING CHDRECD,R3                                                       
         L     R5,ACHDTAB                                                       
*                                                                               
PUTCHD2  OC    0(L'CHDKOFF,R5),0(R5)                                            
         BZ    PUTCHDX                                                          
         MVC   CHDKOFF,0(R5)                                                    
         L     R2,ACHDRL                                                        
         BAS   RE,WTACC            WRITE OUTPUT TAPE                            
         BAS   RE,DNEW             DUMP NEW RECORDS                             
         LA    R5,L'CHDKOFF(R5)                                                 
         B     PUTCHD2                                                          
*                                                                               
PUTCHDX  L     R5,ACHDTAB          CLEAR TABLE                                  
         XC    0(L'CHDKOFF,R5),0(R5)                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD OFFICE/ACCOUNT RECORDS FROM OFFICE TABLE                      *         
***********************************************************************         
                                                                                
PUTOFA   NTR1  ,                                                                
         TM    OUTPUT,DISKO                                                     
         BO    XIT                                                              
         L     R5,AOFATAB          BUILD OFFICE/ACCOUNT RECORDS                 
         USING OFATABD,R5                                                       
         L     R3,AIO3                                                          
         USING OFARECD,R3                                                       
         MVC   OFAKEY,SPACE                                                     
         MVC   OFAKCULA,LASTACT                                                 
*                                                                               
PUTOFA2  CLI   OFATABD,OFATEOTQ    TEST END OF OFFICE TABLE                     
         BE    PUTOFAX                                                          
         MVC   OFAKOFF,OFATOFFC    BUILD REST OF OFFICE/ACCOUNT KEY             
         XC    OFARSTA(256),OFARSTA                                             
         OI    OFARSTA,ACTSABLP                                                 
         MVC   OFARLMOS,OFATLMOS                                                
         MVC   OFARHMOS,OFATHMOS                                                
         LA    R1,OFARFST                                                       
         USING ABLELD,R1           BUILD BALANCE ELEMENT                        
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN1Q                                                    
         ZAP   ABLFRWD,OFATBALF                                                 
         ZAP   ABLDR,OFATTOTD                                                   
         ZAP   ABLCR,OFATTOTC                                                   
         LA    R1,ABLLN1Q(R1)                                                   
         MVI   0(R1),0             SET END OF RECORD                            
         LA    R1,1(R1)                                                         
         LA    R0,OFARECD                                                       
         SR    R1,R0                                                            
         STCM  R1,3,OFARLEN        SET RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         L     R2,AIO3L                                                         
         XC    0(4,R2),0(R2)                                                    
         STCM  R1,3,0(R2)          SET OUTPUT RECORD LENGTH                     
         BAS   RE,WTACC            WRITE OUTPUT TAPE                            
         BAS   RE,DNEW             SET OFFICE ACCOUNT                           
         LA    R5,OFATABLN(R5)     BUMP TO NEXT TABLE ENTRY                     
         B     PUTOFA2                                                          
*                                                                               
PUTOFAX  L     R5,AOFATAB          BUILD OFFICE/ACCOUNT RECORDS                 
         MVI   0(R5),OFATEOTQ      CLEAR OFFICE TABLE                           
         B     XIT                                                              
         DROP  R1,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* TRANSACTIONS TO OUTPUT FILES                                        *         
***********************************************************************         
                                                                                
TRNP     NTR1  ,                                                                
         LA    R4,KEYWRK           BUILD A KEY CHANGE FILE                      
         USING KEYCD,R4                                                         
         XC    KEYCD(KEYCLNQ),KEYCD                                             
         MVI   KEYTYP,KEYTTRN                                                   
         L     RF,AINP                                                          
         MVC   KEYOLD,0(RF)        OLD TRANSACTION KEY                          
         MVC   KEYREC,0(RF)                                                     
         L     RF,AOUT                                                          
         MVC   KEYNEW(41),0(RF)    NEW TRANSACTION KEY                          
         BAS   RE,WTKEY            TRANSACTION KEY CHANGE                       
         BAS   RE,BOFC             BUILD OFFICE/CONTRA ENTRIES                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD TO OFFICE/ACCOUNT AND OFFICE/CONTRA TABLES                      *         
***********************************************************************         
                                                                                
BOFC     TM    LGRTYP,LTPROD+LTPCON TEST PRODUCTION OR PC                       
         BNZR  RE                                                               
         TM    COMPSTA,COMPOTON    TEST NEW OFFICES                             
         BNOR  RE                                                               
         NTR1  ,                                                                
         L     R2,AOUT                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         SR    R0,R0                                                            
*                                                                               
BOFC3    CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),TIMELQ                                                     
         BE    BOFC4                                                            
         CLI   0(R3),TRSELQ                                                     
         BE    BOFC5                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BOFC3                                                            
*                                                                               
         USING TIMELD,R3                                                        
BOFC4    MVC   PMOS,TIMMOA         SAVE MOS                                     
         B     BOFC6                                                            
*                                                                               
         USING TRSELD,R3                                                        
BOFC5    MVC   PMOS,TRSPMOS        SAVE MOS                                     
BOFC6    L     RE,ACHDTAB                                                       
*                                                                               
BOFC8    CLC   TRNKOFF,0(RE)                                                    
         BE    BOFC12                                                           
         OC    0(L'TRNKOFF,RE),0(RE)                                            
         BZ    *+12                                                             
         LA    RE,L'TRNKOFF(RE)                                                 
         B     BOFC8                                                            
         MVC   0(L'TRNKOFF,RE),TRNKOFF                                          
         XC    L'TRNKOFF(L'TRNKOFF,RE),L'TRNKOFF(RE)                            
*                                                                               
BOFC12   L     RE,AOFATAB                                                       
         USING OFATABD,RE                                                       
BOFC14   CLI   OFATABD,OFATEOTQ    POST TRNSACTION TO OFFICE TABLE              
         BE    BOFC16                                                           
         CLC   OFATOFFC,TRNKOFF    MATCH ON OFFICE CODE                         
         BE    BOFC18                                                           
         LA    RE,OFATABLN(RE)     BUMP TO NEXT TABLE ENTRY                     
         B     BOFC14                                                           
*                                                                               
BOFC16   MVC   OFATOFFC,TRNKOFF    CREATE AN OFFICE TABLE ENTRY                 
         ZAP   OFATBALF,=P'0'                                                   
         ZAP   OFATTOTD,=P'0'                                                   
         ZAP   OFATTOTC,=P'0'                                                   
         MVC   OFATLMOS,EFFS                                                    
         XC    OFATHMOS,OFATHMOS                                                
         MVI   OFATABD+OFATABLN,OFATEOTQ                                        
*                                                                               
BOFC18   LA    RF,OFATTOTD         UPDATE OFFICE TABLE ENTRY                    
         CLI   RECTYP,ACRTTIM                                                   
         BE    BOFC19                                                           
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RF,OFATTOTC                                                      
         AP    0(L'OFATTOTD,RF),TRNAMNT                                         
*                                                                               
BOFC19   CLC   OFATLMOS,PMOS                                                    
         BL    *+10                                                             
         MVC   OFATLMOS,PMOS                                                    
         CLC   OFATHMOS,PMOS                                                    
         BH    *+10                                                             
         MVC   OFATHMOS,PMOS                                                    
         B     XIT                                                              
         DROP  R2,R3,RE                                                         
         EJECT                                                                  
***********************************************************************         
* WRITE RECORDS TO ACCOUNT OUTPUT FILE                                *         
***********************************************************************         
                                                                                
WTACC    NTR1  ,                                                                
         L     R1,ATACC                                                         
         ST    R2,AUPREC                                                        
         PUT   (R1),(R2)                                                        
         GOTO1 ACNVCNT,DMCB,('RACTOUT',4(R2))                                   
         AP    CNTACC,PONE                                                      
         AP    CNTOUT,PONE                                                      
         BAS   RE,UPDN             UPDATE NOW                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE RECORDS TO TRANSACTION KEY FILE                               *         
***********************************************************************         
                                                                                
WTKEY    NTR1  ,                                                                
         L     R1,ATKEY                                                         
         PUT   (R1),KEYWRK                                                      
         AP    CNTKFL,PONE                                                      
         ICM   RE,15,MAXKFL        TEST KEY DUMP COUNT                          
         BZ    XIT                 NO MORE DUMPS                                
         BCTR  RE,0                REDUCE THE COUNT                             
         STCM  RE,15,MAXKFL                                                     
         MVC   PCAP1,=CL8'KEY FILE'                                             
         MVC   PCAP2,SPACE                                                      
         MVC   PCAP3,SPACE                                                      
         MVC   PCAP4,SPACE                                                      
         LA    RE,KEYWRK           SET RECORD ADDRESS                           
         ST    RE,PDMP2                                                         
         LA    RE,L'KEYWRK         AND LENGTH                                   
         ST    RE,PDMP4                                                         
         GOTO1 PRNTBL,PDMP                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE FILE NOW                                                     *         
***********************************************************************         
                                                                                
*                                                                               
UPDN     TM    OUTPUT,DISKO                                                     
         BNOR  RE                                                               
         CLI   ITEMERR,C'N'        TEST ANY ERRORS                              
         BNER  RE                                                               
         TM    OUTPUT,CHNGO        TEST OUTPUT CHANGED                          
         BNOR  RE                                                               
*                                                                               
UPDN1    NTR1  ,                                                                
         L     R2,AUPREC           R2=A(RECORD TO BE UPDATED/ADDED)             
         LA    R2,4(R2)                                                         
         MVC   SVDIR,DIR           SAVE CURRENT DIRECTORY KEY                   
         MVC   DKEY,0(R2)          SET TO READ UPDATE KEY                       
         GOTO1 ADMGR,DMRD                                                       
         CLC   DKEY,DIR            TEST RECORD FOUND                            
         BNE   UPDN7                                                            
         LA    R3,DIR                                                           
         TM    ACTKSTAT-ACTRECD(R3),ACTSDELT                                    
         BNO   UPDN5                                                            
         NI    ACTKSTAT-ACTRECD(R3),X'FF'-(ACTSDELT)                            
         GOTO1 ADMGR,DMWRT                                                      
*                                                                               
UPDN5    L     R2,AIO3             GET THE OLD RECORD                           
         GOTO1 ADMGR,DMGET                                                      
         L     R2,AUPREC           R2=A(RECORD TO BE UPDATED)                   
         LA    R2,4(R2)                                                         
         GOTO1 ADMGR,DMPUT                                                      
         B     UPDN9                                                            
*                                                                               
UPDN7    DC    H'0'                FOR NOW WE WONT ADD THEM                     
         GOTO1 ADMGR,DMADD         ADD NEW RECORD                               
*                                                                               
UPDN9    MVC   DIR,SVDIR           RESTORE DIRECTORY                            
         TM    INPUT,DISKI                                                      
         BNO   XIT                                                              
         MVC   DKEY,DIR                                                         
         GOTO1 ADMGR,DMRD                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRNTBL OF SOME OUTPUT RECORDS OK RECORDS                            *         
***********************************************************************         
                                                                                
DOK      L     R2,AINP             TEST RECORD CHANGED                          
         SR    R3,R3                                                            
         ICM   R3,3,ACTRLEN-ACTRECD(R2)                                         
         L     R4,AOUT                                                          
         LR    R5,R3                                                            
         CLCL  R2,R4                                                            
         BE    *+12                                                             
         OI    OUTPUT,CHNGO        SET OUTPUT CHANGED                           
         BNE   DOK1                                                             
         CLI   RDDMPUN,0           RECORD UNCHANGED                             
         BNE   DOK5                                                             
         TM    HKSTA2,HKSDMP       DUMP REQUESTED BY HOOK                       
         BNOR  RE                                                               
DOK1     CLI   RDDMPOK,0           TEST DUMP REQUIRED                           
         BER   RE                                                               
*                                                                               
DOK3     NTR1  ,                                                                
         MVC   PCAP1,RDNME                                                      
         MVC   PCAP2,=CL8'INPUT'                                                
         MVC   PCAP3,RDLNME                                                     
         MVC   PCAP4,DAOUT                                                      
         L     RE,AINPL                                                         
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RDDMPOK                                                       
         BCTR  RF,0                REDUCE DUMP COUNT                            
         STC   RF,RDDMPOK          UPDATE WORKING COPY                          
         L     RE,RDMPADDR                                                      
         STC   RF,RDDMPOK-RDMPS(RE)                                             
*                                                                               
         MVC   PCAP2,=CL8'OUTPUT'                                               
         MVC   PCAP4,SPACE                                                      
         LM    R2,R3,AOUTL         R2=A(LENGTH), R3=A(RECORD)                   
         SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R3)                                         
         AH    RF,=H'4'                                                         
         STCM  RF,3,0(R2)                                                       
         L     RE,AOUTL                                                         
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRNTBL OF SOME UNCHANGED INPUT RECORDS                              *         
***********************************************************************         
                                                                                
DOK5     NTR1  ,                                                                
         MVC   PCAP1,RDNME                                                      
         MVC   PCAP2,=CL8'SAME'                                                 
         MVC   PCAP3,RDLNME                                                     
         MVC   PCAP4,DAOUT                                                      
         L     RE,AINPL                                                         
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RDDMPUN                                                       
         BCTR  RF,0                REDUCE DUMP COUNT                            
         STC   RF,RDDMPUN          UPDATE WORKING COPY                          
         L     RE,RDMPADDR                                                      
         STC   RF,RDDMPUN-RDMPS(RE)                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRNTBL OF SOME NOT OK RECORDS                                       *         
***********************************************************************         
                                                                                
DNOK     CLI   RDDMPNO,0           TEST DUMP REQUIRED                           
         BNE   DNOK1                                                            
         TM    SRCHFLG,SRCHTRC     TEST TRACE SEARCH DATA                       
         BNOR  RE                                                               
         CP    TRCARGN,=P'50'                                                   
         BHR   RE                                                               
         AP    TRCARGN,=P'1'                                                    
DNOK1    NTR1  ,                                                                
         TM    SRCHFLG,SRCHTRC                                                  
         BO    DNOK2                                                            
         SR    RF,RF                                                            
         IC    RF,RDDMPNO                                                       
         BCTR  RF,0                REDUCE DUMP COUNT                            
         STC   RF,RDDMPNO          UPDATE WORKING COPY                          
         L     RE,RDMPADDR                                                      
         STC   RF,RDDMPNO-RDMPS(RE)                                             
DNOK2    MVC   PCAP1,RDNME                                                      
         MVC   PCAP2,=CL8'*ERROR* '                                             
         MVC   PCAP3,RDLNME                                                     
         MVC   PCAP4,DAOUT                                                      
*                                                                               
         L     RE,AINPL                                                         
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
         MVC   P+8(L'TRCBLK),TRCBLK                                             
         GOTO1 PRINTER                                                          
         MVC   P,SPACES                                                         
         MVC   TRCBLK,SPACES                                                    
         B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* PRNTBL OF SOME COPIED RECORDS                                       *         
***********************************************************************         
                                                                                
DCPY     CLI   RDDMPCP,0           TEST ANYMORE TO DUMP                         
         BER   RE                                                               
DCPY1    NTR1  ,                                                                
         SR    RF,RF                                                            
         IC    RF,RDDMPCP                                                       
         BCTR  RF,0                REDUCE DUMP COUNT                            
         STC   RF,RDDMPCP                                                       
         L     RE,RDMPADDR                                                      
         STC   RF,RDDMPCP-RDMPS(RE)                                             
         MVC   PCAP1,RDNME         RECORD EQUATE                                
         MVC   PCAP2,=CL8'*COPIED*'                                             
         MVC   PCAP3,RDLNME        RECORD NAME                                  
         MVC   PCAP4,DAOUT                                                      
*                                                                               
         L     RE,AOUTL                                                         
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRNTBL OF SOME DELETED RECORDS                                      *         
***********************************************************************         
                                                                                
DDEL     NTR1  ,                                                                
         AP    CNTDEL,PONE         DELETE                                       
         GOTO1 ACNVCNT,DMCB,('RACTDEL',AINP)                                    
         CLI   RDDMPDL,0           TEST ANYMORE TO DUMP                         
         BE    XIT                                                              
         SR    RF,RF                                                            
         IC    RF,RDDMPDL                                                       
         BCTR  RF,0                REDUCE DUMP COUNT                            
         STC   RF,RDDMPDL                                                       
         L     RE,RDMPADDR                                                      
         STC   RF,RDDMPDL-RDMPS(RE)                                             
         MVC   PCAP1,RDNME         RECORD EQUATE                                
         MVC   PCAP2,=CL8'DELETED'                                              
         MVC   PCAP3,RDLNME        RECORD NAME                                  
         MVC   PCAP4,DAOUT                                                      
*                                                                               
         L     RE,AOUTL                                                         
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD NEW RECORD FROM HOOK                                            *         
***********************************************************************         
                                                                                
NEWR     NTR1  ,                                                                
         L     R2,HKNREC                                                        
         BAS   RE,DNEW             DUMP OF NEW RECORD                           
         L     R1,ATOUT                                                         
         PUT   (R1),(R2)           PUT NEW RECORD                               
         AP    CNTACC,PONE                                                      
         AP    CNTOUT,PONE                                                      
         GOTO1 ACNVCNT,DMCB,('RACTOUT',4(R2))                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRNTBL OF NEW RECORDS                                               *         
***********************************************************************         
                                                                                
DNEW     NTR1  ,                                                                
         STCM  R2,15,PDMP2         SET RECORD ADDRESS                           
         MVC   SAVTYP,RECTYP       SAVE THE RECORD TYPE                         
         GOTO1 ACRECTYP,DMCB,(C'D',4(R2))   GET RECORD TYPE                     
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         MH    RE,=Y(RDMPLNQ)                                                   
         A     RE,ARECT            A(RECORD DUMP TABLE)                         
*                                                                               
         CLI   RDDMPNW-RDMPS(RE),0   TEST DUMP COUNT                            
         BE    DNEW3                                                            
         SR    RF,RF                                                            
         IC    RF,RDDMPNW-RDMPS(RE)                                             
         BCTR  RF,0                                                             
         STC   RF,RDDMPNW-RDMPS(RE)                                             
         MVC   PCAP1,RDNME-RDMPS(RE)                                            
         MVC   PCAP2,=CL8'* NEW * '                                             
         MVC   PCAP3,RDLNME-RDMPS(RE)                                           
         MVC   PCAP4,SPACE                                                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
*                                                                               
DNEW3    GOTO1 ACNVCNT,DMCB,('RACTNEW',4(R2))                                   
         MVC   RECTYP,SAVTYP       RESTORE RECORD TYPE                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PARTIAL COPY OF TEST COMPANY                                        *         
***********************************************************************         
                                                                                
PCPY     NTR1  ,                                                                
         OI    CCNTL,CCNTLM        SET COPY MODE                                
         CLI   CKEYEND,X'FF'       COPY END KEY                                 
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE END CARD FOR PARIAL COPY           
*                                                                               
PCPY3    BAS   RE,GETIN            GET RECORD                                   
         TM    DMBYTE,X'80'        TEST EOF                                     
         BO    PCPYX                                                            
         BAS   RE,SETIN            SET RECORD TYPE TABLES                       
         BAS   RE,COPY             COPY TO OUTPUT                               
         B     PCPY3                                                            
*                                                                               
PCPYX    NI    CCNTL,ALL-CCNTLM                                                 
         NI    INPUT,ALL-NEXT                                                   
         MVI   DMBYTE,0                                                         
         XC    DIR,DIR                                                          
         XC    DKEY,DKEY                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET INPUT RECORD                                                    *         
***********************************************************************         
                                                                                
GETIN    ST    RE,GETRTN                                                        
         NI    OUTPUT,ALL-CHNGO                                                 
*                                                                               
         L     R2,AINPL            CLEAR INPUT AREA                             
         LH    R3,=Y(L'INP)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE                                                            
         L     R2,AOUTL            CLEAR OUTPUT AREA                            
         LH    R3,=Y(L'OUT)                                                     
         MVCL  R2,RE               CLEAR INPUT                                  
*                                                                               
         TM    INPUT,DISKI         TEST INPUT=DISK                              
         BO    GETIN3                                                           
         TM    CCNTL,CCNTLM        TEST PARTIAL COPY MODE                       
         BO    GETIN3              MUST BE FROM DISK                            
         L     R3,ATINT                                                         
         L     R5,AINPL                                                         
         GET   (R3),(R5)                                                        
         B     GETIN11                                                          
*                                                                               
GETIN3   TM    INPUT,NEXT          TEST FIRST TIME                              
         BO    GETIN5                                                           
         MVC   DAOUT(3),=C'DA='                                                 
         LA    RF,KEYSTRT                                                       
         TM    CCNTL,CCNTLM        TEST COPY MODE                               
         BNO   *+8                                                              
         LA    RF,CKEYSTRT                                                      
         MVC   DKEY,0(RF)                                                       
GETIN4   GOTO1 ADMGR,DMHI                                                       
         OI    INPUT,NEXT                                                       
         B     GETIN7                                                           
*                                                                               
GETIN5   GOTO1 ADMGR,DMSEQ                                                      
GETIN7   TM    DMBYTE,X'80'        TEST EOF                                     
         BO    GETIN13                                                          
         LA    RF,KEYEND                                                        
         TM    CCNTL,CCNTLM        TEST COPY MODE                               
         BNO   *+8                                                              
         LA    RF,CKEYEND                                                       
         CLC   0(42,RF),DIR                                                     
         BH    GETIN9                                                           
         OI    DMBYTE,X'80'        SET EOF                                      
         B     GETIN13                                                          
         EJECT                                                                  
*                                                                               
GETIN9   GOTO1 ACRECTYP,DMCB,(C'I',DIR)                                         
         BAS   RE,SETCPY           SET COMPANY RECORD TYPE                      
         CLI   RECTYP,0            TEST RECORD TYPE                             
         BE    GETIN5                                                           
         CLI   RECTYP,ACRTHDRA     PROCESS                                      
         BL    *+12                                                             
         CLI   RECTYP,ACRTTRLB     OR TRAILERS                                  
         BNH   GETIN5                                                           
*                                                                               
         CLI   DIR,X'40'                                                        
         BH    *+12                                                             
         CLI   COMPDSP,0           DISPLACEMENT TO COMPANY                      
         BE    GETIN5                                                           
*                                                                               
         CLC   CURCOMP,COMPANY     TEST COMPANY BEING CONVERTED                 
         BE    GETIN10                                                          
         CLC   CURCOMP,XCOMPANY    TEST COPY/MERGE COMPANY                      
         BE    GETIN10                                                          
         BAS   RE,SKIP             SET KEY TO FOR READ HIGH                     
         TM    DMBYTE,X'80'                                                     
         BO    GETIN13                                                          
         B     GETIN4                                                           
*                                                                               
GETIN10  L     R2,AINP                                                          
         GOTO1 ADMGR,DMGET         GET THE RECORD                               
         GOTO1 HEXOUT,DMCB,DA,DAOUT+3,4,0                                       
         CLC   0(42,R2),DIR        TEST RECORD = KEY                            
         BNE   GETIN5              MUST BE PASSIVE                              
         SR    RE,RE                                                            
         ICM   RE,3,ACCRLEN-ACCRECD(R2)                                         
         LA    RE,4(RE)            SET LENGTH                                   
         L     RF,AINPL                                                         
         STCM  RE,3,0(RF)                                                       
*                                                                               
GETIN11  AP    CNTIN,PONE          COUNT NUMBER OF RECORDS                      
GETIN13  L     RE,GETRTN                                                        
         BR    RE                                                               
GETRTN   DC    F'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SKIP TO NEXT RECORDS                                                *         
***********************************************************************         
                                                                                
SKIP     LR    R0,RE                                                            
         MVC   DKEY,DIR                                                         
*                                                                               
*        BAS   RE,DMPIN                                                         
*                                                                               
         CLI   RECTYP,ACRTOFF      TEST SPECIAL RECORD                          
         BL    SKIP9                                                            
*                                                                               
         TM    RDINDS,RECIPAS      TEST PASSIVE POINTER                         
         BNO   SKIP5               TEST PASSIVE POINTERS                        
         SR    RF,RF               SKIP TO NEXT RECORD TYPE                     
         IC    RF,DKEY                                                          
         LA    RF,1(RF)                                                         
         STC   RF,DKEY                                                          
         XC    DKEY+1(L'ACCKEY-1),DKEY+1                                        
         B     SKIPX                                                            
*                                                                               
SKIP5    SR    RF,RF                                                            
         IC    RF,COMPDSP          RF=DISPLACEMENT TO COMPANY                   
         LA    R3,DKEY(RF)         R3=A(COMPANY CODE)                           
         LA    RE,LOCOMP           RE=A(LOCOMP)                                 
         CLC   0(1,R3),0(RE)       TEST UP TO LOW COMPANY                       
         BL    SKIP7                                                            
         LA    RE,HICOMP           RE=A(HICOMP)                                 
         CLC   0(1,R3),0(RE)       TEST FINISHED SPECIAL RECORDS                
         BL    SKIP7                                                            
         LA    RE,=X'FF'                                                        
*                                                                               
SKIP7    MVC   0(1,R3),0(RE)       SET NEW COMPANY                              
         LA    RE,L'ACCKEY-2       RE=LENGTH OF KEY                             
         SR    RE,RF               RE=REMAINING LENGTH                          
         EX    RE,*+4                                                           
         XC    1(0,R3),1(R3)       CLEAR END OF KEY                             
         B     SKIPX                                                            
*                                                                               
SKIP9    CLC   DKEY(1),HICOMP      PAST THE LAST COMPANY                        
         BNH   *+12                                                             
         OI    DMBYTE,X'80'        SET EOF FLAG                                 
         B     SKIPX                                                            
         LA    RE,LOCOMP           SET COMPANY FOR READ                         
         CLC   DKEY(1),0(RE)                                                    
         BL    *+8                                                              
         LA    RE,HICOMP                                                        
         MVC   DKEY(1),0(RE)                                                    
         XC    DKEY+1(L'DKEY-1),DKEY+1                                          
*                                                                               
*                                                                               
SKIPX    DS    0H                                                               
*        BAS   RE,DMPOUT                                                        
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
DMPIN    NTR1  ,                                                                
         LA    R0,L'DKIN                                                        
         LA    RF,DKIN                                                          
         GOTO1 PRNTBL,DMCB,((R0),(RF)),DKEY,C'DUMP',42,=C'2D'                   
         B     XIT                                                              
*                                                                               
DMPOUT   NTR1  ,                                                                
         LA    R0,L'DKOUT                                                       
         LA    RF,DKOUT                                                         
         GOTO1 PRNTBL,DMCB,((R0),(RF)),DKEY,C'DUMP',42,=C'2D'                   
         B     XIT                                                              
*                                                                               
DKIN     DC    C'DKEY IN'                                                       
DKOUT    DC    C'DKEY OUT'                                                      
         EJECT                                                                  
***********************************************************************         
* SET RECORD TYPE                                                     *         
***********************************************************************         
                                                                                
SETIN    LR    R0,RE                                                            
         L     R2,AOUTL            MOVE TO OUT FROM INPUT                       
         LH    R3,=Y(L'INP)                                                     
         L     RE,AINPL                                                         
         ICM   RF,3,0(RE)                                                       
         MVCL  R2,RE                                                            
         GOTO1 ACRECTYP,DMCB,(C'D',AINP)                                        
         GOTO1 ACNVCNT,DMCB,('RACTINP',AINP)                                    
         BAS   RE,SETCPY           SET COMPANY, RECORD TYPE                     
         CLI   RECTYP,0            TEST BAD RECORD                              
         BE    SETINNO                                                          
         CLI   RECTYP,ACRTGBC      TEST RECORD IN TABLE                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   CURCOMP,COMPANY     TEST COMPANY BEING CONVERTED                 
         BE    SETIN1                                                           
         CLC   CURCOMP,XCOMPANY    COPY/MERGE COMPANY                           
         BNE   SETINNO                                                          
*                                                                               
SETIN1   CLI   RECTYP,ACRTACTL     TEST ACCOUNT LOW                             
         BNE   SETIN3                                                           
         L     R2,AOUT                                                          
         MVI   ACTSTA,0            ACCOUNT STATUS                               
         TM    ACCRSTA-ACCRECD(R2),X'80'                                        
         BNO   *+8                                                              
         OI    ACTSTA,ACTSDEL      SET ACCOUNT DELETED                          
*                                                                               
SETIN3   SR    R1,R1                                                            
         ICM   R1,7,RDFLDT         R1=A(KEY FIELD TABLE)                        
         BNZ   *+8                                                              
         L     R1,AXXXK            A(DEFAULT KEY TAB)                           
         MVC   KTABS,0(R1)         SET KEY TABLE                                
**                                                                              
         CLI   KTABOF,C'X'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
**                                                                              
SETINYES LR    RE,R0                                                            
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
SETINNO  LR    RE,R0                                                            
         LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET COMPANY - RECORD TYPE - COPY DATA                               *         
***********************************************************************         
                                                                                
SETCPY   MVC   COMPDSP,2(R1)       SAVE DISPLACEMENT TO COMPANY                 
         MVC   RECTYP,0(R1)        RECORD TYPE                                  
         MVC   CURCOMP,1(R1)       CURRENT COMPANY                              
*                                                                               
         NI    RECSTA,ALL-RECTRN                                                
         CLI   RECTYP,ACRTTIM      TREAT TMS LIKE TRANSACTIONS                  
         BNE   *+8                                                              
         OI    RECSTA,RECTRN                                                    
         CLI   RECTYP,ACRTTRN      ALSO TRANSACTIONS                            
         BNE   *+8                                                              
         OI    RECSTA,RECTRN                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,RECTYP                                                      
         MH    R1,=Y(RDMPLNQ)                                                   
         A     R1,ARECT            A(RECORD DUMP TABLE)                         
         ST    R1,RDMPADDR                                                      
         MVC   RDMPS,0(R1)         SAVE CURRENT ENTRY                           
*                                                                               
         XC    XCOPMRG,XCOPMRG     CLEAR COPY/MERGE CONTROL                     
         LA    R2,XTAB                                                          
SETCPY1  CLI   0(R2),0             END OF XTAB                                  
         BER   RE                                                               
         CLC   XCOMPANY-XCOPMRG(L'XCOMPANY,R2),CURCOMP  TEST COMPANY            
         BE    *+12                                                             
         LA    R2,L'XCOPMRG(R2)                                                 
         B     SETCPY1                                                          
         MVC   XCOPMRG,0(R2)       SET COPY/MERGE DATA                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* COPY NEW COMPANY DATA                                               *         
***********************************************************************         
                                                                                
COPY     NTR1  ,                                                                
         L     R2,AOUT                                                          
         SR    R3,R3                                                            
         IC    R3,COMPDSP                                                       
         AR    R3,R2                                                            
         CLC   0(1,R3),XCOMPANY                                                 
         BNE   *+10                                                             
         MVC   0(1,R3),COMPANY     REPLACE COMPANY CODE IN KEY                  
         CLI   RECTYP,ACRTOTHR     TEST FOR SPECIAL                             
         BH    COPY3                                                            
         LA    R3,TRNKCCPY-TRNKEY(R2)                                           
         CLC   0(1,R3),XCOMPANY    TEST COMPANY CODE IN CONTRA                  
         BNE   *+10                                                             
         MVC   0(1,R3),COMPANY     REPLACE WITH NEW COMPANY                     
         TM    RECSTA,RECTRN       TEST TRANSACTION LIKE                        
         BO    *+8                                                              
         OI    TRNKSBR-TRNKEY(R2),X'01' COPIED SORT LAST                        
*                                                                               
COPY3    TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNO   COPY4                                                            
         MVI   MODE,PROCCPY                                                     
         GOTO1 ACNVHOOK            CALL USER HOOK                               
*                                                                               
COPY4    MVI   MODE,CHNGCMP        SET MODE FOR ELEMENT ROUTINE                 
         GOTO1 ACNVELM             ELEMENT ROUTINE DOES THE REST                
         GOTO1 ACNVCNT,DMCB,('RACTNEW',0(R2))                                   
         AP    CNTCPY,PONE         COUNT NUMBER COPIED IN                       
         BAS   RE,DCPY             DUMP COPY                                    
         BAS   RE,PUT              PUT TO TAPE                                  
COPYX    XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
                                                                                
                                                                                
LGRTTAB  DS    0CL3                LEDGER TYPE TABLE                            
         DC    C'SJ',AL1(LTPROD)   PRODUCTION                                   
         DC    C'1J',AL1(LTPCON)   PROJECT CONTROL                              
         DC    C'SP',AL1(LTMPAY)   MEDIA PAYABLE                                
         DC    C'SQ',AL1(LTMPAY)                                                
         DC    C'SS',AL1(LTMPAY)                                                
         DC    C'ST',AL1(LTMPAY)                                                
         DC    C'ST',AL1(LTMPAY)                                                
         DC    C'SU',AL1(LTMPAY)                                                
         DC    C'SV',AL1(LTPPAY)   PRODUCTION PAYABLE                           
         DC    C'SW',AL1(LTPPAY)                                                
         DC    C'SX',AL1(LTXPAY)   EXPENSE PAYABLE                              
         DC    C'SY',AL1(LTXPAY)                                                
         DC    C'SR',AL1(LTRECV)                                                
         DC    X'FF'                                                            
*                                                                               
SVDIR    DS    CL(L'DIR)           SAVE DIRECTORY KEY                           
OFFICE   DC    CL2' '                                                           
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION - READ AND VALIDATE CONTROL CARDS                    *         
**********************************************************************          
                                                                                
         ENTRY INIT                                                             
INIT     DS    0D                                                               
         NMOD1 0,*INIT*,RA                                                      
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
*                                                                               
INIT1    GOTO1 CARDS,DMCB,CARDIO,=C'RE00'                                       
         MVC   P(L'CARDIO),CARDIO                                               
         GOTO1 PRINTER                                                          
         CLC   CARDIO(2),=C'/*'                                                 
         BE    INIT15                                                           
         CLC   CARDIO(6),=C'PATCH '                                             
         BNE   *+12                                                             
         BAS   RE,VPATCH                                                        
         B     INIT1                                                            
*                                                                               
         CLC   CARDIO(5),=C'START'                                              
         BNE   *+12                                                             
         BAS   RE,VSTART                                                        
         B     INIT1                                                            
*                                                                               
         CLC   CARDIO(3),=C'END'                                                
         BNE   *+12                                                             
         BAS   RE,VEND                                                          
         B     INIT1                                                            
*                                                                               
         LA    R0,L'CARDIO                                                      
         LA    RF,CARDIO+L'CARDIO-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,CARDH+5          SET LENGTH OF INPUT                          
         AH    R0,=Y(L'CARDH)                                                   
         STC   R0,CARDH                                                         
         L     R3,AIO3                                                          
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(20,CARDH),(10,SCAND),0                             
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          SET NUMBER OF PARAMETERS                     
         BNZ   *+6                                                              
         DC    H'0'                BAD CARD                                     
         DROP  R8                                                               
*                                                                               
INIT3    LA    R2,OPTTAB           LIST OF VALID INPUT FIELDS                   
         SR    R1,R1                                                            
*                                                                               
INIT5    IC    R1,0(R2)            LENGTH FOR COMPARE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCANLFT(0),1(R2)    MATCH CARD FIELD TO TABLE                    
         BE    INIT7                                                            
         LA    R2,L'OPTTAB(R2)                                                  
         CLI   0(R2),EOT                                                        
         BNE   INIT5                                                            
         DC    H'0'                INVALID INPUT OPTION                         
*                                                                               
INIT7    SR    RF,RF               GET VALIDATION ROUTINE                       
         ICM   RF,3,11(R2)                                                      
         AR    RF,RB                                                            
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         LA    R3,SCANLNQ(R3)                                                   
         BCT   R0,INIT3                                                         
         B     INIT1               GET NEXT CARD                                
         DROP  R3                                                               
*                                                                               
INIT15   BAS   RE,LTAB             LOAD PHASES & TABLES                         
         BAS   RE,ZAP              APPLY PATCHES                                
         BAS   RE,ZCNT             SET DUMP RECORD COUNTS                       
         BAS   RE,OPCTF            OPEN CONTROL FILE                            
         BAS   RE,OPACF            OPEN ACCOUNT FILE                            
         CLI   ACTNSW,ACTNKEY      KEY CHANGE ?                                 
         BE    INITX                                                            
         CLI   ACTNSW,ACTNMRG      MERGE ?                                      
         BE    INITX                                                            
*                                                                               
         XC    HOOKCB,HOOKCB                                                    
         MVI   MODE,INITTAB        HOOK TO INIT TABLE                           
         TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNO   INIT17              NO HOOK                                      
         GOTO1 ACNVHOOK            CALL USER HOOK                               
*                                                                               
INIT17   BAS   RE,OPINF            OPEN AND LOAD INPUT FILES                    
         BAS   RE,BLDLG            BUILD THE LEDGER CONTROL TABLES              
         BAS   RE,DUPCK            CHECK TABLES FOR DUPLICATES                  
         BAS   RE,SETPER           SET 1R LEDGER LENGTHS                        
*                                                                               
         TM    INPUT,DISKI         TEST INPUT=DISK                              
         BO    INIT21                                                           
         OPEN  (TINT,(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT21   OPEN  (TOUT,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   ACTNSW,ACTNCHG      TEST ACTION CHANGE                           
         BE    INIT23                                                           
         OPEN  (TTRN,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (TACC,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (TKEY,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
INIT23   GOTO1 BUFFALO,DMCB,=C'SET',ABUFC                                       
INITX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY CODE                                                *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VAGNY    DS    0H                                                               
         MVC   ALPHA,SCANRHT       AGENCY=XX                                    
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* VALIDATE COPY AGENCY                                                *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VCOPY    MVI   BYTE,XSTACOP        SET COPY BIT                                 
         B     *+8                                                              
VMERG    MVI   BYTE,XSTAMRG        SET MERGE BIT                                
         NTR1  ,                                                                
         LA    R0,5                                                             
         LA    R1,XTAB             FIND AN EMPTY SLOT                           
         CLI   XALPHA-XCOPMRG(R1),0                                             
         BE    *+14                                                             
         LA    R1,L'XCOPMRG(R1)                                                 
         BCT   R0,*-12                                                          
         DC    H'0'                TOO MANY COPY/MERGES                         
*                                                                               
         MVC   XALPHA-XCOPMRG(L'XALPHA,R1),SCANRHT  COPY(MERGE)=XX              
         OC    XSTAT-XCOPMRG(L'XSTAT,R1),BYTE       SET STATUS                  
         B     INITX                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* VALIDATE INPUT TYPE                                                *          
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VINPT    NI    INPUT,ALL-(TAPE+DISKI)       INPUT=DISK(TAPE)                    
         CLC   SCANRHT(4),=C'TAPE'                                              
         BNE   *+10                                                             
         OI    INPUT,TAPE                                                       
         BR    RE                                                               
         CLC   SCANRHT(4),=C'DISK'                                              
         BNE   *+10                                                             
         OI    INPUT,DISKI                                                      
         BR    RE                                                               
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOOK CARD                                                 *          
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VHOOK    MVC   PHASE5(8),SCANRHT   HOOK=ACNV05XX                                
         OI    HOOKSW,HOOKIT       SET HOOK SWITCH                              
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* VALIDATE TABLES CARD                                              *           
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VTABL    MVC   PHASE6(8),SCANRHT   TABLE=ACNV06XX                               
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ADD DEFAULT OFFICE                                                  *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VDEFOF   MVC   DFLTOFF,SCANRHT     DFLTOFF=XX                                   
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ADD TRCAE DATA                                                      *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VTRACE   MVC   TRCARG,SCANRHT      TRACE=XXXX                                   
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE SOME CARDS TILL PHASES ARE LOADED                              *         
***********************************************************************         
                                                                                
VPATCH   NTR1  ,                   SAVE PATCH CARDS                             
         L     R1,AHCARD                                                        
         LA    R0,MXHCRD           MAX NUMBER OF PATCHES                        
         CLI   0(R1),C' '                                                       
         BNH   *+14                                                             
         LA    R1,80(R1)                                                        
         BCT   R0,*-12                                                          
         DC    H'0'                TOO MANY ENTRIES                             
         MVC   0(80,R1),CARDIO                                                  
         B     INITX                                                            
                                                                                
***********************************************************************         
* APPLY THE PATCH CARDS                                               *         
***********************************************************************         
                                                                                
ZAP      NTR1  ,                   PATCH 00 000000 HHHHHH                       
         L     R6,AHCARD           R6=PATCH CARDS                               
         LA    R7,MXHCRD           R7=MAX NUMBER OF PATCHES                     
ZAP3     CLC   0(5,R6),=C'PATCH'                                                
         BNE   INITX                                                            
         MVC   CARDIO(80),0(R6)                                                 
         GOTO1 HEXIN,DMCB,CARDIO+6,BYTE,2,0  CONVERT OVERLAY #                  
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         IC    R2,BYTE                                                          
         SLL   R2,2                                                             
         L     R3,=A(ACNV00)                                                    
         LTR   R2,R2                                                            
         BZ    *+12                                                             
         LA    RF,ACNVPHS-4(R2)                                                 
         L     R3,0(RF)                                                         
*                                                                               
         GOTO1 HEXIN,DMCB,CARDIO+9,FULL,6,0  CONVERT LOCATION                   
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                BAD LOCATION                                 
         SR    RF,RF                                                            
         ICM   RF,7,FULL                                                        
         AR    R3,RF               R3=LOCATION TO BE PATCHED                    
*                                                                               
         LA    RF,CARDIO+16       GET LENGTH OF PATCH                           
         SR    R0,R0                                                            
         CLI   0(RF),C' '                                                       
         BE    *+16                                                             
         AH    R0,=H'1'                                                         
         LA    RF,1(RF)                                                         
         B     *-16                                                             
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                BAD PATCH                                    
         GOTO1 HEXIN,DMCB,CARDIO+16,(R3),(R0),0                                 
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                BAD PATCH                                    
         LA    R6,80(R6)                                                        
         BCT   R7,ZAP3                                                          
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE SOME SCANNER ENTRIES                                           *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VRECT    NTR1  ,                                                                
         L     R1,AHSCAN                                                        
         LA    R0,MXHSCN           MAX NUMBER OF SCANNER ENTRIES                
         CLI   SCANLFT-SCAND(R1),C' '                                           
         BNH   *+14                                                             
         LA    R1,SCANLNQ(R1)                                                   
         BCT   R0,*-12                                                          
         DC    H'0'                TOO MANY ENTRIES                             
         MVC   0(SCANLNQ,R1),SCAND                                              
         B     INITX                                                            
                                                                                
***********************************************************************         
* ZAP IN THE DUMP COUNTS                                             *          
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
ZCNT     NTR1  ,                                                                
         L     R3,AHSCAN           R3=SCANNER DATA                              
         LA    R0,MXHSCN           R7=MAX NUMBER OF LINES                       
ZCNT3    CLC   SCANLFT,SPACE                                                    
         BE    INITX                                                            
         L     RF,ARECT            ACRTXX=999                                   
         SR    R1,R1                                                            
         IC    R1,SCANLLN          SET LENGTH OF LEFT SIDE                      
         SH    R1,=H'2'                                                         
         MVC   DUB(2),=C'AC'       BUILD ACRTXX                                 
         MVC   DUB+2(6),SCANLFT+3                                               
         LA    R7,RDDMPOK-RDMPS    CHARTXX=9(99)   CHANGED OK                   
         CLC   SCANLFT(5),=C'CHART'                                             
         BE    ZCNT4                                                            
         LA    R7,RDDMPUN-RDMPS    UNCRTXX=9(99)   UNCHANGED OK                 
         CLC   SCANLFT(5),=C'UNCRT'                                             
         BE    ZCNT4                                                            
         LA    R7,RDDMPNO-RDMPS    NOTRTXX=9(99)   NOT OK (ERROR)               
         CLC   SCANLFT(5),=C'NOTRT'                                             
         BE    ZCNT4                                                            
         LA    R7,RDDMPCP-RDMPS    COPRTXX=9(99)   COPIED RECORDS               
         CLC   SCANLFT(5),=C'COPRT'                                             
         BE    ZCNT4                                                            
         LA    R7,RDDMPDL-RDMPS    DELRTXX=9(99)   DELETED RECORDS              
         CLC   SCANLFT(5),=C'DELRT'                                             
         BE    ZCNT4                                                            
         LA    R7,RDDMPNW-RDMPS    NEWRTXX=9(99)   NEW RECORDS                  
         CLC   SCANLFT(5),=C'NEWRT'                                             
         BE    ZCNT4                                                            
         DC    H'0'                                                             
ZCNT4    SR    RE,RE                                                            
         ICM   RE,15,SCANRBV       BINARY VALUE                                 
         BNZ   *+6                                                              
         DC    H'0'                INVALID NUMERIC                              
*                                                                               
ZCNT5    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),RDNME-RDMPS(RF) TEST RECORD NAME                          
         BNE   *+12                                                             
         LA    R6,0(R7,RF)                                                      
         STC   RE,0(R6)                SET DUMP COUNT                           
         LA    RF,RDMPLNQ(RF)                                                   
         CLI   0(RF),EOT                                                        
         BNE   ZCNT5                                                            
         LA    R3,SCANLNQ(R3)                                                   
         BCT   R0,ZCNT3                                                         
         B     INITX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MAXERROR AND MAXKEY                                        *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VMXER    LA    RF,MAXERR           SET MAXIMUM ERROR COUNT                      
         B     *+8                                                              
VMXKY    LA    RF,MAXKFL           SET MAXIMUM KEY DUMP                         
         OC    SCANRBV,SCANRBV     TEST VALID NUMNER                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,RF),SCANRBV                                                  
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
         USING SCAND,R3            FILES=                                       
VFLES    DS    0H                  NUMBER OF INPUT FILES                        
         OC    SCANRBV,SCANRBV     TEST VALID NUMNER                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   NFLS,SCANRBV+3                                                   
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* VALIDATE DISPLACEMENT TO OLD /NEW DATA                              *         
***********************************************************************         
                                                                                
*                                  OLDISP=99,NEWDISP=99                         
         USING SCAND,R3                                                         
VOLDD    LH    R7,=Y(FDOD-FPRMS)   DISPLACEMENT TO OLD FIELD                    
         B     *+8                                                              
VNEWD    LH    R7,=Y(FDND-FPRMS)   DISP. TO NEW FIELD                           
         A     R7,CURPRM                                                        
         OC    SCANRBV,SCANRBV     TEST VALIDATE NUMBER                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R7),SCANRBV+3                                                
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE START / END CARD                                           *         
***********************************************************************         
                                                                                
VSTART   NTR1  ,                                                                
         CLC   CARDIO(6),=C'STARTC' STARTC=XXXXX                                
         BNE   VSTRC1                                                           
         OI    CCNTL,CCNTLS                                                     
         LA    R4,CKEYSTRT                                                      
         LA    R3,CARDIO+7                                                      
         B     VKEY                                                             
VSTRC1   OI    INPUT,START          START=XXXXXXXX                              
         LA    R4,KEYSTRT                                                       
         LA    R3,CARDIO+6                                                      
         B     VKEY                                                             
*                                                                               
VEND     NTR1  ,                                                                
         CLC   CARDIO(4),=C'ENDC'  ENDC=XXXXX                                   
         BNE   VENDC1                                                           
         LA    R4,CKEYEND                                                       
         LA    R3,CARDIO+5                                                      
         B     VKEY                                                             
VENDC1   LA    R4,KEYEND           END=XXXXXXXX                                 
         LA    R3,CARDIO+4                                                      
*                                                                               
VKEY     GOTO1 DECODE,DMCB,(42,(R3)),(X'00',(R4))                               
         CLI   8(R1),0                                                          
         BE    INITX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FILES CARDS                                          *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VACCT    MVC   ACCDCB,SCANRHT      ACCOUNT=XXXX                                 
         LA    RF,ACCPRMS                                                       
         B     VALLT                                                            
*                                                                               
VOFFT    MVC   OFFDCB,SCANRHT      OFFICE=XXXX                                  
         LA    RF,OFFPRMS                                                       
         B     VALLT                                                            
*                                                                               
VWRKT    MVC   WRKDCB,SCANRHT      WORKCODE=XXXX                                
         LA    RF,WRKPRMS                                                       
         B     VALLT                                                            
*                                                                               
VBLST    MVC   BLSDCB,SCANRHT      BLSOURCE=XXXX                                
         LA    RF,BLSPRMS                                                       
         B     VALLT                                                            
*                                                                               
VMEDT    MVC   MEDDCB,SCANRHT      MEDIA=XXXX                                   
         LA    RF,MEDPRMS                                                       
         B     VALLT                                                            
*                                                                               
VCLIT    MVC   CLIDCB,SCANRHT      CLIENT=XXXX                                  
         LA    RF,CLIPRMS                                                       
         B     VALLT                                                            
*                                                                               
VCLOT    MVC   CLODCB,SCANRHT      CLIOFF=XXXX                                  
         LA    RF,CLOPRMS                                                       
VALLT    ST    RF,CURPRM                                                        
         BR    RE                                                               
*                                                                               
VOUTPUT  CLC   SCANRHT(4),=C'DISK' OUTPUT=DISK                                  
         BNE   VOUTDIR                                                          
         OI    OUTPUT,DISKO                                                     
         BR    RE                                                               
*                                                                               
VOUTDIR  CLC   SCANRHT(4),=C'DIR' OUTPUT=DIR                                    
         BNE   VOUTTEST                                                         
         OI    OUTPUT,DIRO                                                      
         BR    RE                                                               
*                                                                               
VOUTTEST CLC   SCANRHT(4),=C'TEST' OUTPUT=TEST                                  
         BNER  RE                                                               
         OI    OUTPUT,TEST                                                      
         BR    RE                                                               
*                                                                               
VWRITEN  CLC   SCANRHT(2),=C'NO'   WRITE=NO                                     
         BNER  RE                                                               
         OI    OUTPUT,WRTNO                                                     
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION CARD                                                *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VACTN    CLC   SCANRHT(8),=C'CONVERT ' FILE CONVERSION                          
         BNE   *+8                                                              
         MVI   ACTNSW,ACTNCON                                                   
         CLC   SCANRHT(8),=C'MERGE   '   ACCOUNT MERGE                          
         BNE   *+8                                                              
         MVI   ACTNSW,ACTNMRG                                                   
         CLC   SCANRHT(8),=C'KEYSORT ' TRANSACTION KEY SORT                     
         BNE   *+8                                                              
         MVI   ACTNSW,ACTNKEY                                                   
         CLC   SCANRHT(8),=C'CHANGE  '  ACCOUNT/DATA CHANGE                     
         BNE   *+8                                                              
         MVI   ACTNSW,ACTNCHG                                                   
         CLI   ACTNSW,0                                                         
         BNER  RE                                                               
         DC    H'0'                INVALID ACTION                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* STRIPES - OTHER OPTIONS                                             *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VSTRI    OI    OPTSW,STRIPES       STRIPES                                      
         BR    RE                                                               
*                                                                               
VNOANAL  OI    OPTSW,NOANAL        NOANAL - NOT CONVERT ANALYSIS                
         BR    RE                                                               
*                                                                               
VNOLIST  OI    OPTSW,NOLIST        NOLIST - SKIP PRINTING LISTS                 
         BR    RE                                                               
*                                                                               
VNOCAER  OI    ERRCB+((ERRCAC*2)+1),ERRIGN   DON'T ADD MISSING CONTRA           
         OI    ERRCB+((ERRACE*2)+1),ERRIGN     OR ELEMENT                       
         BR    RE                            TO NUMBER OF ERRORS.               
*                                                                               
VNOMEER  OI    ERRCB+((ERRMED*2)+1),ERRIGN     SAME FOR MEDIA                   
         BR    RE                                                               
*                                                                               
VNOWCER  OI    ERRCB+((ERRWRK*2)+1),ERRIGN     WORKCODE                         
         BR    RE                                                               
*                                                                               
VNOBSER  OI    ERRCB+((ERRBLS*2)+1),ERRIGN     BILL SOURCE                      
         BR    RE                                                               
*                                                                               
VNOCCER  OI    ERRCB+((ERRCLI*2)+1),ERRIGN     CLIENT CODE                      
         BR    RE                                                               
*                                                                               
VNOCOER  OI    ERRCB+((ERRCLO*2)+1),ERRIGN     CLIENT OFFICE                    
         BR    RE                                                               
*                                                                               
VNOACER  OI    ACCERSW,SUPACER                 SUPPRESS ACC NOT ON FILE         
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PHASES AND TABLES                                              *         
***********************************************************************         
                                                                                
LTAB     NTR1  ,                                                                
         LA    R2,PHASES                                                        
         LA    R4,ACNVPHS                                                       
*                                                                               
LTAB3    CLI   0(R2),C' '                                                       
         BNH   LTAB7                                                            
         MVC   DUB,0(R2)           LOAD IN PHASE                                
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)       TEST LOAD WAS OK                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            R0=START OF PHASE                            
         ST    R0,0(R4)            SAVE IN PHASE LIST                           
         OC    DMPL2(4),DMPL2      TEST FIRST TIME                              
         BNZ   *+8                                                              
         ST    R0,DMPL2            SAVE START IN DUMP LIST                      
         CLM   R0,15,DMPL2                                                      
         BH    *+8                                                              
         ST    R0,DMPL2            SAVE LOWEST                                  
         A     R0,0(R1)            +LENGTH                                      
         CLM   R0,7,DMPL2+5                                                     
         BNH   *+8                                                              
         STCM  R0,7,DMPL2+5        SAVE END OF DUMP                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,8(R2)          LENGTH OF ENTRY POINTS                       
         BZ    LTAB7                                                            
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,9(R2)          RF=W/S ADDRESS                               
         ICM   RE,15,0(R4)         RE=PHASE ENTRY POINTS                        
         EX    R1,*+4                                                           
         MVC   0(0,RF),0(RE)       SAVE ENTRY POINTS                            
*                                                                               
LTAB7    LA    R4,4(R4)                                                         
         LA    R2,L'PHASES(R2)     LOAD NEXT PHASE                              
         CLI   0(R2),EOT           TEST END OF TABLE                            
         BNE   LTAB3                                                            
*                                                                               
         ICM   R3,15,DMPL2                                                      
         AR    R3,R0                                                            
         STCM  R3,7,DMPL2+5        R3=END ADDRESS                               
         ICM   RE,15,ACNVLIST      RE=A(TABLE),A(NUMBER OF ENTRIES)             
         BZ    INITX                                                            
         LA    R3,AGYLST                                                        
         SR    RF,RF                                                            
*                                                                               
LTAB9    ICM   RF,7,1(R3)            RF=A(TABLE PARAMETERS)                     
         MVC   FTAB-FPRMS(L'FTAB,RF),0(RE)   START                              
         MVC   FNUM-FPRMS(L'FNUM,RF),4(RE)   NUMBER                             
         LA    RE,8(RE)                                                         
         LA    R3,4(R3)                                                         
         CLI   0(R3),EOT                                                        
         BNE   LTAB9                                                            
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN CONTROL FILE AND GET SE NUMBER                                 *         
***********************************************************************         
                                                                                
OPCTF    NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,OPEN,CONTROL,CTFILEL                                
         LA    R2,DKEY             READ ACCESS RECORD                           
         USING CT5REC,R2                                                        
         LA    R4,ALPHA            MAIN COMPANY BEING CONVERTED                 
         LA    R5,COMPANY                                                       
         BAS   RE,OPCTF2           GET HEX VALUE FROM CONTROL FILE              
         MVC   LOCOMP,COMPANY                                                   
         MVC   HICOMP,COMPANY                                                   
         LA    R1,XTAB             TABLE OF COPY/MERGE COMPANIES                
OPCTF1   CLI   0(R1),0                                                          
         BE    INITX                                                            
         LA    R4,XALPHA-XCOPMRG(R1)                                            
         LA    R5,XCOMPANY-XCOPMRG(R1)                                          
         BAS   RE,OPCTF2           GET HEX VALUE FROM CONTROL FILE              
         MVC   HICOMP,0(R5)                                                     
         CLC   LOCOMP,HICOMP                                                    
         BNH   *+22                                                             
         XC    LOCOMP,HICOMP       SWITCH THEM                                  
         XC    HICOMP,LOCOMP                                                    
         XC    LOCOMP,HICOMP                                                    
         LA    R1,L'XCOPMRG(R1)                                                 
         B     OPCTF1                                                           
*                                                                               
OPCTF2   NTR1  ,                                                                
         XC    DKEY,DKEY                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R4)                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,AIO3                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
         USING CTSYSD,R3                                                        
OPCTF3   CLI   0(R3),0             FIND SYSTEM ELEMENT                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTSYSELQ                                                   
         BNE   *+12                                                             
         CLI   CTSYSNUM,X'06'      TEST ACCOUNT FILE                            
         BE    OPCTF5                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     OPCTF3                                                           
*                                                                               
OPCTF5   MVC   SENUM,CTSYSSE       GET  SE NUMBER                               
         MVC   0(L'COMPANY,R5),CTSYSAGB    GET COMPANY CODE                     
         B     INITX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN ACCOUNT FILE AND GET COMPANY RECORD                            *         
***********************************************************************         
                                                                                
OPACF    NTR1  ,                                                                
         MVC   SE,SENUM            SET SE NUMBER                                
         GOTO1 DATAMGR,DMCB,OPEN,ACCOUNT,ACFILEL                                
         LA    R2,DKEY             READ COMPANY RECORD                          
         USING CPYRECD,R2                                                       
         MVC   DKEY,SPACE                                                       
         MVC   CPYKCPY,COMPANY                                                  
         GOTO1 ADMGR,DMRD                                                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         GOTO1 ADMGR,DMGET                                                      
         LA    R3,CPYRFST                                                       
         SR    R0,R0                                                            
*                                                                               
OPACF3   CLI   0(R3),0                                                          
         BE    INITX                                                            
         CLI   0(R3),CPYELQ        TEST COMPANY ELEMENT                         
         BE    OPACF5                                                           
         CLI   0(R3),NAMELQ        TEST NAME ELEMENT                            
         BE    OPACF7                                                           
OPACF4   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     OPACF3                                                           
*                                                                               
         USING CPYELD,R3                                                        
OPACF5   CLC   CPYALPHA,ALPHA      TEST ALPHA CODE                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CPYSTAT4,CPYSOFF2                                                
         BNO   *+8                                                              
         OI    COMPSTA,COMPNOFF                                                 
         CLI   CPYLN,CPYLN3Q                                                    
         BL    OPACF4                                                           
         CLI   CPYTCMP,X'40'       TEST COMPANY                                 
         BL    OPACF4                                                           
         MVC   LKCOMP,CPYTCMP      SAVE THE LINKED COMPANY                      
         B     OPACF4                                                           
*                                                                               
         USING NAMELD,R3                                                        
OPACF7   SR    R1,R1               GET COMPANY NAME                             
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   COMPNME(0),NAMEREC                                               
         B     OPACF4                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN AND LOAD INPUT FILES                                           *         
***********************************************************************         
                                                                                
OPINF    NTR1  ,                                                                
         LA    R7,AGYLST                                                        
OPINF1   SR    RF,RF                                                            
         ICM   RF,7,1(R7)                                                       
         MVC   FPRMS(FPLNQ),0(RF)                                               
         CLI   FDCB,C' '           TEST INPUT FILE                              
         BNH   OPINF11                                                          
         L     R3,ACINP                                                         
         L     R2,AXINP                                                         
         MVC   0(XINPLNQ,R3),0(R2)                                              
         USING IHADCB,R3                                                        
         MVC   DCBDDNAM,FDCB                                                    
         LA    RE,OPINF5                                                        
         STCM  RE,7,DCBEODA        SET EOF ADDRESS                              
         OPEN  ((R3),(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SR    R8,R8                                                            
         L     R5,AIO3                                                          
*                                                                               
OPINF3   GET   (R3),(R5)                                                        
         AH    R8,=H'1'            COUNT # RECORDS                              
         B     OPINF3                                                           
*                                                                               
OPINF5   L     R3,ACINP                                                         
         CLOSE ((R3))                                                           
         ST    R8,FNUM             SAVE # OF RECORDS                            
         MH    R8,FRLN+2           # * LENGTH                                   
         LTR   R0,R8               TEST NUMBER OF BYTES REQUIRED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AH    R0,=H'100'                                                       
         GETMAIN R,LV=(0)          GET THE STORAGE                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,FTAB             SAVE START OF TABLE                          
         B     OPINF6              DON'T DUMP THE TABELS                        
*                                                                               
         OC    DMPL3,DMPL3                                                      
         BZ    *+12                                                             
         CLM   R1,15,DMPL3                                                      
         BNL   *+8                                                              
         ST    R1,DMPL3                                                         
         ICM   RF,7,DMPL3+5                                                     
         AR    RF,R0               ADD LENGTH OF TABLE                          
         STCM  RF,7,DMPL3+5                                                     
         NI    DMPL2+4,X'FF'-X'80'                                              
*                                                                               
OPINF6   L     R3,ACINP                                                         
         L     R2,AXINP                                                         
         MVC   0(XINPLNQ,R3),0(R2)                                              
         MVC   DCBDDNAM,FDCB                                                    
         LA    RE,OPINF9                                                        
         STCM  RE,7,DCBEODA        SET EOF ADDRESS                              
         OPEN  ((R3),(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3             R5=IO AREA                                   
         L     R6,FTAB             R6=START OF TABLE                            
         SR    R2,R2                                                            
         IC    R2,FDPK             DISPLACEMENT TO PACKED FIELD                 
         L     R3,ACINP            R3=DCB                                       
*                                                                               
OPINF7   GET   (R3),(R5)                                                        
         LR    RF,R5                                                            
         SR    RE,RE                                                            
         IC    RE,FDOD             DISPLACEMENT TO OLD DATA                     
         AR    RF,RE                                                            
         L     R4,FKLN             L'KEY(OLD DATA)                              
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R6),0(RF)       OLD DATA TO TABLE                            
         LA    R6,1(R4,R6)                                                      
*                                                                               
         LR    RF,R5                                                            
         L     RE,FKLN             DEFAULT DISP TO NEW DATA                     
         CLI   FDND,0              TEST ANY OVERRIDE                            
         BE    *+8                                                              
         IC    RE,FDND                                                          
         AR    RF,RE                                                            
         L     R4,FRLN             R4=LENGTH OF RECORD                          
         S     R4,FKLN             LESS KEY                                     
         LTR   R2,R2               TEST PACKED FIELD                            
         BZ    *+8                                                              
         SH    R4,=H'5'            LESS PACKED FIELD LENGTH                     
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R6),0(RF)       NEW DATA TO TABLE                            
         LA    R6,1(R4,R6)                                                      
*                                                                               
         LTR   R2,R2                                                            
         BZ    *+14                                                             
         ZAP   0(5,R6),=P'0'       SET THE PACKED FIELD                         
         LA    R6,5(R6)                                                         
         B     OPINF7                                                           
*                                                                               
OPINF9   CLOSE ((R3))                                                           
*                                                                               
OPINF11  OC    FNUM,FNUM           TEST ANY RECORDS                             
         BZ    OPINF13                                                          
         OC    CNVSW,0(R7)         SET CONVERSION ACTIVE BIT                    
         GOTO1 XSORT,FTAB                                                       
         BAS   RE,TABSET                                                        
*                                                                               
OPINF13  SR    RF,RF                                                            
         ICM   RF,7,1(R7)                                                       
         MVC   0(FPLNQ,RF),FPRMS   SAVE UPDATED PARAMETERS                      
         LA    R7,4(R7)                                                         
         CLI   0(R7),EOT                                                        
         BNE   OPINF1                                                           
         B     INITX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD UNIT/LEDGER/ACCOUNT CONTROL                                   *         
***********************************************************************         
                                                                                
BLDLG    NTR1  ,                                                                
         TM    CNVSW,CNVSAC        TEST CONVERTING ACCOUNTS                     
         BNO   INITX                                                            
         XC    FPRMS(FPLNQ),FPRMS                                               
         MVC   FRLN,ACCRLN         RECORD LENGTH                                
         MVC   FKLN,ACCKLN         KEY LENGTH                                   
         MVC   FDSP,ACCDSP         DISPLACEMENT TO KEY                          
         L     R3,ALGRTAB          R3=LEDGER TABLE                              
         ST    R3,LGRTAB                                                        
         L     R1,ACCTAB           R1=ACCOUNT TABLE                             
         L     R4,ACCNUM           R4=NUMBER                                    
         MH    R4,ACCRLN+2         X LENGTH                                     
         AR    R4,R1               R4=END OF TABLE +1                           
*                                                                               
BLDLG3   ST    R1,FTAB             START OF TABLE                               
         MVC   LGRCDE-LGRS(L'LGRCDE,R3),0(R1)                                   
         LA    R0,1                R0=ITEM COUNT                                
BLDLG5   BAS   RE,ADDNAM           ADD ACCOUNT NAME TO THE RECORD               
         CLI   ACTNSW,ACTNCHG      TEST ACTION CHANGE                           
         BNE   *+8                                                              
         BAS   RE,CHKDUP           CHECK FOR A DUPLICATE NAME                   
         LA    R2,ACCLNQ(R1)       R2=NEXT RECORD                               
         CR    R2,R4               TEST EOT                                     
         BNL   BLDLG7                                                           
         CLC   0(2,R2),0(R1)       TEST SAME LEDGER                             
         BNE   BLDLG7                                                           
         AH    R0,=H'1'            COUNT NUMBER OF ITEMS                        
         LR    R1,R2               R1=NEXT ITEM                                 
         B     BLDLG5                                                           
*                                                                               
BLDLG7   ST    R0,FNUM             STORE THE NUMBER                             
         BAS   RE,TABSET                                                        
         MVC   LGRREG-LGRS(L'FBINL,R3),FBIN  SAVE SEARCH REGISTERS              
         LA    R3,LGRLNQ(R3)                                                    
         SR    RF,RF                                                            
         ICM   RF,15,LGRNUM        NUMBER OF LEDGER ENTRIES                     
         AH    RF,=H'1'                                                         
         STCM  RF,15,LGRNUM                                                     
         CR    R2,R4               TEST EOT                                     
         BNL   BLDLG9                                                           
         LR    R1,R2                                                            
         B     BLDLG3                                                           
*                                                                               
BLDLG9   MVC   FPRMS(FPLNQ),LGRPRMS                                             
         BAS   RE,TABSET           SET LEDGER TABLE                             
         MVC   LGRPRMS(FPLNQ),FPRMS    SAVE LEDGER INFO                         
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* ADD NAME TO ACCOUNT ENTRY                                           *         
***********************************************************************         
                                                                                
ADDNAM   NTR1  ,                                                                
         LR    R3,R1                                                            
         MVC   ACCNME-ACCS(L'ACCNME,R3),SPACE                                   
         CLI   LKCOMP,X'40'        IS THERE A LINKED COMPANY                    
         BNH   INITX                                                            
         MVC   DKEY,SPACE                                                       
         MVC   DKEY(1),LKCOMP      USE LINKED COMPANY                           
         MVC   DKEY+1(14),ACCN-ACCS(R3)                                         
         GOTO1 ADMGR,DMHI          READ HIGH FOR ACCOUNT                        
         CLC   DKEY,DIR                                                         
         BNE   INITX                                                            
         L     R2,AIO3                                                          
         GOTO1 ADMGR,DMGET         GET THE RECORD                               
         USING ACTRECD,R2                                                       
         SR    R1,R1                                                            
         LA    R5,ACTRFST                                                       
*                                                                               
         USING NAMELD,R5                                                        
ADDNAM3  IC    R1,NAMLN                                                         
         CLI   0(R5),0                                                          
         BE    INITX                                                            
         CLI   0(R5),NAMELQ                                                     
         BE    ADDNAM4                                                          
         AR    R5,R1                                                            
         B     ADDNAM3                                                          
*                                                                               
ADDNAM4  SH    R1,=H'3'            ADD NAME TO TABLE ENTRY                      
         EX    R1,*+4                                                           
         MVC   ACCNME-ACCS(0,R3),NAMEREC                                        
         B     INITX                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK FOR A DUPLICATE ACCOUNT                                       *         
***********************************************************************         
                                                                                
CHKDUP   NTR1  ,                                                                
         LR    R3,R1                                                            
         MVC   DKEY,SPACE                                                       
         MVC   DKEY(1),COMPANY     USE LINKED COMPANY                           
         MVC   DKEY+1(14),ACCN-ACCS(R3)                                         
         GOTO1 ADMGR,DMHI          READ HIGH FOR ACCOUNT                        
         CLC   DKEY,DIR                                                         
         BNE   INITX                                                            
         LA    R2,DIR                                                           
         USING ACTRECD,R2                                                       
         TM    ACTKSTAT,ACTSDELT   TEST DELETED                                 
         BO    INITX                                                            
         OI    DUPERR,CNVSAC                                                    
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
         L     R7,BOXAREA                                                       
         USING BOXD,R7                                                          
         MVC   P+2(14),DKEY+1                                                   
         MVC   P+17(25),=CL25'ACCOUNT ALREADY EXISTS'                           
         GOTO1 PRINTER                                                          
         B     INITX                                                            
         DROP  R2,R7,R8                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE SEARCH PARAMETERS                                       *         
***********************************************************************         
                                                                                
TABSET   NTR1  ,                                                                
         L     R1,FNUM             # RECORDS                                    
         L     R3,FRLN             R3=L'RECORD                                  
         MR    R0,R3                                                            
         L     R2,FTAB             R2=A(START OF TABLE)                         
         AR    R1,R2                                                            
         BCTR  R1,0                R1=END OF TABLE                              
         LR    R0,R3               R0=L'RECORD                                  
         SR    R2,R0               R2=A(START LESS LENGTH OF ENTRY)             
         SR    R1,R2               R1=LENGTH OF TABLE PLUS AN ENTRY             
         BXLE  R0,R0,*             R0=LOWEST POWER OF 2 GE R1                   
         AR    R1,R2               R1=END OF TABLE                              
         L     R4,FKLN             R4=L'KEY                                     
         STM   R0,R4,FBIN          SAVE SEARCH PARAMETERS                       
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* BROWSE TABLES FOR DUPLICATE ENTRIES                                 *         
***********************************************************************         
                                                                                
DUPCK    NTR1  ,                                                                
         LA    R7,AGYLST                                                        
DUPCK3   SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    CNVSW,0             TEST ANY DATA                                
         BNO   DUPCK9                                                           
         SR    RF,RF                                                            
         ICM   RF,7,1(R7)                                                       
         MVC   FPRMS(FPLNQ),0(RF)                                               
         L     R1,FTAB             R1=A(TABLE)                                  
         LR    R5,R1                                                            
         SR    R0,R0                                                            
         IC    R0,FDPK             R0=DISPL. TO COUNT FIELD                     
         AR    R5,R0               R5=A(PACKED FIELD)                           
         L     R4,FNUM             R4=NUMBER                                    
         MH    R4,FRLN+2           X LENGTH                                     
         AR    R4,R1               R4=END OF TABLE +1                           
         A     R1,FDSP             R1=A(KEY)                                    
         L     R3,FKLN                                                          
         BCTR  R3,0                R3=KEY LENGTH                                
*                                                                               
DUPCK5   LR    R2,R1                                                            
         A     R2,FRLN             R2=NEXT ITEM                                 
         CR    R2,R4               TEST EOT                                     
         BNL   DUPCK9                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R1)       TEST TABLE IN SEQUENCE                       
         BH    *+16                                                             
         OC    DUPERR,0(R7)        SET ERROR BIT                                
         ZAP   0(5,R5),=P'1'                                                    
         A     R1,FRLN                                                          
         A     R5,FRLN                                                          
         B     DUPCK5                                                           
*                                                                               
DUPCK9   LA    R7,4(R7)                                                         
         CLI   0(R7),EOT                                                        
         BNE   DUPCK3                                                           
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* SET 1R LEDGER LENGTHS FORM LEDGER RECORDS                           *         
***********************************************************************         
                                                                                
SETPER   NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACE        READ 1R LEDGER RECORD                        
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),=C'1R'                                                
         GOTO1 ADMGR,DMRD                                                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,OLD1RLVS                                                      
         BAS   RE,SETPERL          SET LENGTHS                                  
         MVC   NEW1RLVS,OLD1RLVS                                                
         LA    R1,XTAB                                                          
         CLI   0(R1),0                                                          
         BE    INITX                                                            
*                                                                               
         MVC   LDGKCPY,XCOMPANY-XCOPMRG(R1) COPY/MERGE COMPANY                  
         GOTO1 ADMGR,DMRD                                                       
         CLI   8(R1),0                                                          
         BNE   INITX                                                            
         LA    R4,NEW1RLVS                                                      
         BAS   RE,SETPERL          SET LENGTHS                                  
         B     INITX                                                            
*                                                                               
SETPERL  NTR1  ,                                                                
         L     R2,AIO3             GET THE RECORD                               
         GOTO1 ADMGR,DMGET                                                      
         LA    R3,LDGRFST                                                       
         SR    R0,R0                                                            
*                                                                               
SETPERL3 CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),ACLELQ        LENGTHS ELEMENT                              
         BE    SETPERL5                                                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     SETPERL3                                                         
*                                                                               
         USING ACLELD,R3                                                        
SETPERL5 LA    R5,ACLVALS                                                       
         LA    R0,4                                                             
         MVC   0(1,R4),0(R5)       SAVE THE LENGTHS                             
         LA    R4,1(R4)                                                         
         LA    R5,L'ACLVALS(R5)                                                 
         BCT   R0,*-14                                                          
         B     INITX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
OPTTAB   DS    0CL13               CONTROL CARDS                                
         DC    AL1(5),CL10'ACTION    ',AL2(VACTN-INIT)                          
         DC    AL1(5),CL10'AGENCY    ',AL2(VAGNY-INIT)                          
         DC    AL1(3),CL10'COPY      ',AL2(VCOPY-INIT)                          
         DC    AL1(4),CL10'MERGE     ',AL2(VMERG-INIT)                          
         DC    AL1(4),CL10'INPUT     ',AL2(VINPT-INIT)                          
         DC    AL1(4),CL10'FILES     ',AL2(VFLES-INIT)                          
         DC    AL1(3),CL10'CHART     ',AL2(VRECT-INIT)                          
         DC    AL1(3),CL10'NOTRT     ',AL2(VRECT-INIT)                          
         DC    AL1(3),CL10'UNCRT     ',AL2(VRECT-INIT)                          
         DC    AL1(3),CL10'COPRT     ',AL2(VRECT-INIT)                          
         DC    AL1(3),CL10'DELRT     ',AL2(VRECT-INIT)                          
         DC    AL1(3),CL10'NEWRT     ',AL2(VRECT-INIT)                          
         DC    AL1(5),CL10'MAXERR    ',AL2(VMXER-INIT)                          
         DC    AL1(5),CL10'MAXKEY    ',AL2(VMXKY-INIT)                          
         DC    AL1(4),CL10'START     ',AL2(VSTART-INIT)                         
         DC    AL1(2),CL10'END       ',AL2(VEND-INIT)                           
         DC    AL1(5),CL10'STARTC    ',AL2(VSTART-INIT)                         
         DC    AL1(3),CL10'ENDC      ',AL2(VEND-INIT)                           
         DC    AL1(6),CL10'ACCOUNT   ',AL2(VACCT-INIT)                          
         DC    AL1(5),CL10'OFFICE    ',AL2(VOFFT-INIT)                          
         DC    AL1(7),CL10'WORKCODE  ',AL2(VWRKT-INIT)                          
         DC    AL1(7),CL10'BLSOURCE  ',AL2(VBLST-INIT)                          
         DC    AL1(4),CL10'MEDIA     ',AL2(VMEDT-INIT)                          
         DC    AL1(5),CL10'CLIENT    ',AL2(VCLIT-INIT)                          
         DC    AL1(5),CL10'CLIOFF    ',AL2(VCLOT-INIT)                          
         DC    AL1(4),CL10'PATCH     ',AL2(VPATCH-INIT)                         
         DC    AL1(3),CL10'HOOK      ',AL2(VHOOK-INIT)                          
         DC    AL1(5),CL10'TABLES    ',AL2(VTABL-INIT)                          
         DC    AL1(6),CL10'OLDDISP   ',AL2(VOLDD-INIT)                          
         DC    AL1(6),CL10'NEWDISP   ',AL2(VNEWD-INIT)                          
         DC    AL1(6),CL10'DFLTOFF   ',AL2(VDEFOF-INIT)                         
         DC    AL1(6),CL10'STRIPES   ',AL2(VSTRI-INIT)                          
         DC    AL1(5),CL10'NOANAL    ',AL2(VNOANAL-INIT)                        
         DC    AL1(5),CL10'NOLIST    ',AL2(VNOLIST-INIT)                        
         DC    AL1(5),CL10'OUTPUT    ',AL2(VOUTPUT-INIT)                        
         DC    AL1(4),CL10'WRITE     ',AL2(VWRITEN-INIT)                        
         DC    AL1(6),CL10'NOERRCA   ',AL2(VNOCAER-INIT)                        
         DC    AL1(6),CL10'NOERRMC   ',AL2(VNOMEER-INIT)                        
         DC    AL1(6),CL10'NOERRWC   ',AL2(VNOWCER-INIT)                        
         DC    AL1(6),CL10'NOERRBS   ',AL2(VNOBSER-INIT)                        
         DC    AL1(6),CL10'NOERRCC   ',AL2(VNOCCER-INIT)                        
         DC    AL1(6),CL10'NOERRCO   ',AL2(VNOCOER-INIT)                        
         DC    AL1(6),CL10'NOERRAOF  ',AL2(VNOACER-INIT)                        
         DC    AL1(4),CL10'TRACE     ',AL2(VTRACE-INIT)                         
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0F                                                               
PHASES   DS    0CL12                                                            
PHASE1   DC    CL8'ACNV01  ',AL4(0)                   ELEMENT ROUTINES          
PHASE2   DC    CL8'ACNV02  ',AL4(0)                   KEY SORT                  
PHASE3   DC    CL8'ACNV03  ',AL4(0)                   MERGE                     
PHASE4   DC    CL8'ACNV04  ',AL4(0)                   RECORD COUNT              
PHASE5   DC    CL8'        ',AL4(0)                   HOOK                      
PHASE6   DC    CL8'        ',AL4(0)                   CONVERSION LISTS          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                                  
***********************************************************************         
                                                                                
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* SEARCH ROUTINES                                                     *         
***********************************************************************         
                                                                                
         ENTRY SRCH                                                             
SRCH     DS    0D                                                               
         NMOD1 0,*SRCH*,RA                                                      
         NI    SRCHFLG,ALL-SRCHTRC                                              
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     LGRSR               SEARCH LEDGER                                
         B     ACCSR                      ACCOUNT                               
         B     CONSR                      CONTRA                                
         B     OFFSR                      OFFICE                                
         B     ACCSE                      ACCOUNT(IN ELEMENT)                   
         B     OFFSE                      OFFICE (IN ELEMENT)                   
         B     CLISR                      CLIENT                                
         B     MEDSR                      MEDIA                                 
         B     WRKSR                      WORKCODE                              
         B     CLOSR                      CLIENT/ OFFICE                        
         B     BLSSR                      BILLING SOURCE                        
*                                                                               
SRCHX    MVC   OLDARG,SRCHARG      SAVE OLD SEARCH ARGUMENT                     
         MVC   SRCHARG,SPACE       CLEAR FOR NEXT                               
         CLI   TRCARG,C' '                                                      
         BNH   *+18                                                             
         CLC   OLDARG,TRCARG                                                    
         BNE   *+8                                                              
         OI    SRCHFLG,SRCHTRC     FORCE TRACE MISSING DATA                     
         CLI   FOUND,FND           TEST FOUND                                   
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LEDGER SEARCH ROUTINE                                               *         
***********************************************************************         
                                                                                
LGRSR    CLC   LGRCDE,SRCHARG      TEST SAME LEDGER                             
         BE    LGRSRX                                                           
         BAS   R8,LGRSR3                                                        
LGRSRX   MVC   FOUND,LGRFND        SET FOUND/NOT FND INDICATOR                  
         B     SRCHX                                                            
*                                                                               
LGRSR3   XC    LGRS,LGRS                                                        
         MVC   LGRCDE,SRCHARG      SAVE CURRENT SEARCH                          
         MVI   LGRFND,NFND         SET NOT FOUND                                
         OC    LGRNUM,LGRNUM       ANYTHING IN TABLE                            
         BZR   R8                                                               
         LA    RF,LGRBIN           LOAD LEDGER PARAMETERS                       
         BAS   RE,SRCHALL          SEARCH TABLE                                 
         BNER  R8                                                               
         MVC   LGRS,0(R2)          MOVE TO LOCAL AREA                           
         ST    R2,LGRADDR          SAVE THE ADDRESS                             
         MVI   LGRFND,FND          SET ITEM FOUND                               
         BR    R8                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT SEARCH ROUTINE                                              *         
***********************************************************************         
                                                                                
ACCSR    CLC   ACCO,SRCHARG        TEST SAME ACCOUNT                            
         BE    ACCSRX                                                           
         XC    ACCS,ACCS                                                        
         MVC   ACCO,SRCHARG                                                     
         MVI   ACCFND,NFND         SET NOT FOUND                                
         LA    RE,ACCN                                                          
         ST    RE,HKNEWA           SET ADDRESS FOR HOOK                         
         BAS   RE,HOOKER           TEST THE HOOK                                
         BNE   ACCSR1                                                           
         MVI   ACCFND,FND          SET ITEM FOUND                               
         B     ACCSRX                                                           
*                                                                               
ACCSR1   CLC   ACCO(2),LGRCDE      TEST CORRECT LEDGER                          
         BE    ACCSR3                                                           
         BAS   R8,LGRSR3           GET LEDGER ENTRY                             
*                                                                               
ACCSR3   CLI   LGRFND,FND          TEST LEDGER FOUND                            
         BNE   ACCSRX              LEDGER NOT FOUND                             
         CLI   SRCHARG+2,C' '      NO ACCOUNT CODE                              
         BH    *+14                                                             
         MVC   ACCN,ACCO           SEND BACK SAME U/L                           
         B     ACCSR4                                                           
*                                                                               
         LA    RF,LGRREG           RF=A(REGS FOR ACCTS IN THIS U/L)             
         BAS   RE,SRCHALL                                                       
         BNE   ACCSR5              NOT FOUND SET ERROR                          
         MVC   ACCS,0(R2)                                                       
         ST    R2,ACCADDR                                                       
         AP    ACCH-ACCS(L'ACCH,R2),PONE   COUNT HITS                           
ACCSR4   MVI   ACCFND,FND          SET ITEM FOUND                               
         B     ACCSRX                                                           
*                                                                               
ACCSR5   MVI   ERRNUM,ERRACC       SET ACCOUNT KEY ERROR                        
         BAS   RE,POSTERR                                                       
*                                                                               
ACCSRX   MVC   FOUND,ACCFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* CONTRA SEARCH ROUTINE                                               *         
***********************************************************************         
                                                                                
CONSR    CLC   CONO,SRCHARG        TEST SAME CONTRA                             
         BE    CONSRX                                                           
         XC    CONS,CONS           NOW LOOK FOR CONTRA ACCOUNT                  
         MVC   CONO,SRCHARG                                                     
         MVI   CONFND,NFND         SET NOT FOUND                                
         LA    RE,CONN                                                          
         ST    RE,HKNEWA           SET ADDRESS FOR HOOK                         
         BAS   RE,HOOKER           TEST THE HOOK                                
         BNE   CONSR1                                                           
         MVI   CONFND,FND          SET ITEM FOUND                               
         B     CONSRX                                                           
*                                                                               
CONSR1   CLC   CONO(2),CLGCDE      TEST CORRECT CONTRA/LEDGER ENTRY             
         BE    CONSR3                                                           
         MVC   CLGCDE,SRCHARG                                                   
         MVI   CLGFND,NFND                                                      
         OC    LGRNUM,LGRNUM       ANYTHING IN TABLE                            
         BZ    CONSRX                                                           
         LA    RF,LGRBIN           LOAD LEDGER PARAMETERS                       
         BAS   RE,SRCHALL          SEARCH LEDGER TABLE                          
         BNE   CONSRX                                                           
         MVC   CLGS,0(R2)          SAVE CONTRA LEGDER DATA                      
         ST    R2,CLGADDR                                                       
         MVI   CLGFND,FND                                                       
*                                                                               
CONSR3   CLI   CLGFND,FND                                                       
         BNE   CONSRX                                                           
         LA    RF,CLGREG                                                        
         CLI   SRCHARG+2,C' '      NO ACCOUNT                                   
         BH    *+14                                                             
         MVC   CONN,CONO           SEND BACK SAME U/L                           
         B     CONSR4                                                           
*                                                                               
         BAS   RE,SRCHALL                                                       
         BNE   CONSR5                                                           
         MVC   CONS,0(R2)                                                       
         ST    R2,CONADDR                                                       
CONSR4   MVI   CONFND,FND                                                       
         B     CONSRX                                                           
*                                                                               
CONSR5   MVI   ERRNUM,ERRCAC       CONTRA ACCOUNT                               
         BAS   RE,POSTERR                                                       
*                                                                               
CONSRX   MVC   FOUND,CONFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* OFFICE SEARCH ROUTINE                                               *         
***********************************************************************         
                                                                                
OFFSR    CLC   OFFO,SRCHARG        TEST SAME OFFICE                             
         BE    OFFSRX                                                           
         XC    OFFS,OFFS                                                        
         MVC   OFFO,SRCHARG        SAVE CURRENT SEARCH                          
         MVI   OFFFND,NFND                                                      
         LA    RF,OFFBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   OFFSR5                                                           
         MVC   OFFS,0(R2)                                                       
         ST    R2,OFFADDR                                                       
         MVI   OFFFND,FND                                                       
         AP    OFFH-OFFS(L'OFFH,R2),PONE COUNT NUMBER OF HITS                   
         B     OFFSRX                                                           
*                                                                               
OFFSR5   MVI   ERRNUM,ERROFF      OFFICE ERROR IN KEY                           
         BAS   RE,POSTERR                                                       
*                                                                               
OFFSRX   MVC   FOUND,OFFFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT SEARCH ROUTINE (ELEMENT)                                    *         
***********************************************************************         
                                                                                
ACCSE    CLC   ACEO,SRCHARG        TEST SAME ACCOUNT                            
         BE    ACCSEX                                                           
         XC    ACES,ACES                                                        
         MVC   ACEO,SRCHARG                                                     
         MVI   ACEFND,NFND                                                      
         LA    RE,ACEN                                                          
         ST    RE,HKNEWA           SET ADDRESS FOR HOOK                         
         BAS   RE,HOOKER           TEST THE HOOK                                
         BNE   ACCSE1                                                           
         MVI   ACEFND,FND          SET ITEM FOUND                               
         B     ACCSEX                                                           
*                                                                               
ACCSE1   CLC   ACEO(2),ELGCDE      TEST CORRECT ELEMENT/LEDGER ENTRY            
         BE    ACCSE3                                                           
         MVC   ELGCDE,SRCHARG                                                   
         MVI   ELGFND,NFND                                                      
         OC    LGRNUM,LGRNUM       ANYTHING IN TABLE                            
         BZ    ACCSEX                                                           
         LA    RF,LGRBIN           LOAD LEDGER PARAMETERS                       
         BAS   RE,SRCHALL          SEARCH LEDGER TABLE                          
         BNE   ACCSEX                                                           
         MVC   ELGS,0(R2)          SAVE ELEMENT LEGDER DATA                     
         ST    R2,ELGADDR                                                       
         MVI   ELGFND,FND                                                       
*                                                                               
ACCSE3   CLI   ELGFND,FND                                                       
         BNE   ACCSEX                                                           
         LA    RF,ELGREG                                                        
         CLI   SRCHARG+2,C' '      NO ACCOUNT                                   
         BH    *+14                                                             
         MVC   ACEN,ACEO           SEND BACK SAME U/L                           
         B     ACCSE4                                                           
*                                                                               
         BAS   RE,SRCHALL                                                       
         BNE   ACCSE5                                                           
         MVC   ACES,0(R2)                                                       
         ST    R2,ACEADDR                                                       
ACCSE4   MVI   ACEFND,FND                                                       
         B     ACCSEX                                                           
*                                                                               
ACCSE5   MVI   ERRNUM,ERRACE       SET ACCOUNT                                  
         BAS   RE,POSTERR                                                       
*                                                                               
ACCSEX   MVC   FOUND,ACEFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* OFFICE SEARCH ROUTINE (ELEMENT)                                     *         
***********************************************************************         
                                                                                
OFFSE    CLC   OFEO,SRCHARG        TEST SAME OFFICE                             
         BE    OFFSEX                                                           
         XC    OFES,OFES                                                        
         MVC   OFEO,SRCHARG                                                     
         MVI   OFEFND,NFND                                                      
         LA    RF,OFFBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   OFFSE3                                                           
         MVC   OFES,0(R2)                                                       
         ST    R2,OFEADDR                                                       
         MVI   OFEFND,FND                                                       
         B     OFFSEX                                                           
*                                                                               
OFFSE3   MVI   ERRNUM,ERROFE       OFFICE MISSING                               
         BAS   RE,POSTERR                                                       
*                                                                               
OFFSEX   MVC   FOUND,OFEFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* CLIENT SEARCH ROUTINE                                               *         
***********************************************************************         
                                                                                
CLISR    CLC   CLIO,SRCHARG        TEST SAME CLIENT                             
         BE    CLISRX                                                           
         XC    CLIS,CLIS                                                        
         MVC   CLIO,SRCHARG                                                     
         MVI   CLIFND,NFND                                                      
         LA    RF,CLIBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   CLISR5                                                           
         MVC   CLIS,0(R2)                                                       
         ST    R2,CLIADDR                                                       
         MVI   CLIFND,FND                                                       
         AP    CLIH-CLIS(L'CLIH,R2),PONE COUNT NUMBER OF HITS                   
         B     CLISRX                                                           
*                                                                               
CLISR5   MVI   ERRNUM,ERRCLI         CLIENT CODE                                
         BAS   RE,POSTERR                                                       
*                                                                               
CLISRX   MVC   FOUND,CLIFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* MEDIA CODE SEARCH ROUTINE                                           *         
***********************************************************************         
                                                                                
MEDSR    CLC   MEDO,SRCHARG        TEST SAME MEDIA                              
         BE    MEDSRX                                                           
         XC    MEDS,MEDS                                                        
         MVC   MEDO,SRCHARG                                                     
         MVI   MEDFND,NFND                                                      
         LA    RF,MEDBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   MEDSR5                                                           
         MVC   MEDS,0(R2)                                                       
         ST    R2,MEDADDR                                                       
         MVI   MEDFND,FND                                                       
         AP    MEDH-MEDS(L'MEDH,R2),PONE COUNT NUMBER OF HITS                   
         B     MEDSRX                                                           
*                                                                               
MEDSR5   MVI   ERRNUM,ERRMED         MEDIA ERROR                                
         BAS   RE,POSTERR                                                       
*                                                                               
MEDSRX   MVC   FOUND,MEDFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* WORKCODE SEARCH ROUTINE                                             *         
***********************************************************************         
                                                                                
WRKSR    CLC   WRKO,SRCHARG        TEST SAME WORKCODE                           
         BE    WRKSRX                                                           
         XC    WRKS,WRKS                                                        
         MVC   WRKO,SRCHARG                                                     
         MVI   WRKFND,NFND                                                      
         LA    RF,WRKBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   WRKSR5                                                           
         MVC   WRKS,0(R2)                                                       
         ST    R2,WRKADDR                                                       
         MVI   WRKFND,FND                                                       
         AP    WRKH-WRKS(L'WRKH,R2),PONE COUNT NUMBER OF HITS                   
         B     WRKSRX                                                           
*                                                                               
WRKSR5   MVI   ERRNUM,ERRWRK                                                    
         BAS   RE,POSTERR                                                       
*                                                                               
WRKSRX   MVC   FOUND,WRKFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* CLIENT / OFFICE SEARCH                                              *         
***********************************************************************         
                                                                                
CLOSR    CLC   CLOC,SRCHARG        TEST SAME CLIENT                             
         BE    CLOSRX                                                           
         XC    CLOS,CLOS                                                        
         MVC   CLOC,SRCHARG                                                     
         MVI   CLOFND,NFND                                                      
         LA    RF,CLOBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   CLOSR5                                                           
         MVC   CLOS,0(R2)                                                       
         ST    R2,CLOADDR                                                       
         MVI   CLOFND,FND                                                       
         AP    CLOH-CLOS(L'CLOH,R2),PONE COUNT NUMBER OF HITS                   
         B     CLOSRX                                                           
*                                                                               
CLOSR5   MVI   ERRNUM,ERRCLO                                                    
         BAS   RE,POSTERR                                                       
*                                                                               
CLOSRX   MVC   FOUND,CLOFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* BILLING SOURCE  SEARCH                                              *         
***********************************************************************         
                                                                                
BLSSR    CLC   BLSO,SRCHARG        TEST SAME SOURCE                             
         BE    BLSSRX                                                           
         XC    BLSS,BLSS                                                        
         MVC   BLSO,SRCHARG                                                     
         MVI   BLSFND,NFND                                                      
         LA    RF,BLSBIN                                                        
         BAS   RE,SRCHALL                                                       
         BNE   BLSSR5                                                           
         MVC   BLSS,0(R2)                                                       
         ST    R2,BLSADDR                                                       
         MVI   BLSFND,FND                                                       
         AP    BLSH-BLSS(L'BLSH,R2),PONE COUNT NUMBER OF HITS                   
         B     BLSSRX                                                           
*                                                                               
BLSSR5   MVI   ERRNUM,ERRBLS                                                    
         BAS   RE,POSTERR                                                       
*                                                                               
BLSSRX   MVC   FOUND,BLSFND                                                     
         B     SRCHX                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL SEARCH ROUTINE                                              *         
***********************************************************************         
                                                                                
SRCHALL  LM    R0,R4,0(RF)         LOAD SEARCH REGISTERS                        
         BCTR  R4,0                                                             
*                                                                               
SRCH8    SRL   R0,1                1/2 REMAINING TABLE LENGTH                   
         CR    R0,R3               TEST IF LESS THAN AN ENTRY LENGTH            
         BLR   RE                  NOT FOUND - AT END OF TABLE                  
         BXH   R2,R0,SRCH11        COMPUTE NEW TABLE START ADDRESS              
         EX    R4,*+8              SEARCH ACCOUNT VS. TABLE                     
         B     *+10                                                             
         CLC   SRCHARG(0),0(R2)                                                 
         BE    SRCH12              GOT A MATCH                                  
         BH    SRCH8                                                            
SRCH11   SR    R2,R0                                                            
         B     SRCH8                                                            
                                                                                
SRCH12   CR    RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LINK TO THE HOOK TO TEST ACCOUNT CHANGES                            *         
***********************************************************************         
                                                                                
HOOKER   LR    R0,RE                                                            
         TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNO   HOOKX                                                            
         MVC   SVMODE,MODE         SAVE MODE                                    
         MVI   MODE,CHNGACC        SET NEW MODE FOR ACCOUNT CHANGE              
         GOTO1 ACNVHOOK                                                         
         MVC   MODE,SVMODE                                                      
*                                                                               
HOOKX    LR    RE,R0               RESTORE RETURN ADDRESS                       
         L     RF,HKNEWA           TEST THE NEW ACCOUNT FIELD                   
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         CR    RB,RB               RETURN OK                                    
         BR    RE                                                               
         LTR   RB,RB               RETURN NOT OK                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* POST ERRORS TO BUFFALO                                              *         
***********************************************************************         
                                                                                
POSTERR  TM    SRCHFLG,SRCHIGN     IGNORE (DON'T POST ERROR)                    
         BOR   RE                                                               
         B     POSTERR1                                                         
*                                                                               
         CLI   ERRNUM,ERRACE       IGNORE MISSING ACCOUNT IN ELEMENT            
         BER   RE                                                               
         CLI   ERRNUM,ERRCAC       IGNORE MISSING ACCOUNT IN CONTRA             
         BER   RE                                                               
         CLI   ERRNUM,ERRACC       IGNORE MISSING ACCOUNT HIGH                  
         BNE   POSTERR1                                                         
         CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
POSTERR1 NTR1  ,                                                                
         XC    BUFREC(BUFRLNQ),BUFREC                                           
         MVC   BUFDATA,SRCHARG     MISSING DATA                                 
         SR    R1,R1                                                            
         IC    R1,ERRNUM           ERROR NUMBER                                 
         SLL   R1,1                X 2                                          
         LA    R1,ERRCB(R1)        R1=ERROR CONTROL BLOCK ENTRY                 
*                                                                               
         TM    1(R1),ERRIGN        OPTION TO IGNORE THIS ERROR                  
         BO    POSTX                                                            
         BAS   RE,TRCERR           TRACE THE ERROR                              
*                                                                               
         MVC   BUFRTYP,RECTYP      RECORD TYPE                                  
         MVC   BUFETYP,0(R1)       ERROR TYPE                                   
         LA    RF,BUFBUCK                                                       
         TM    1(R1),ERRCON        TEST ADDING TO CONTRA ACCUMULATOR            
         BZ    *+8                                                              
         LA    RF,4(RF)                                                         
         TM    1(R1),ERRELM        TEST ADDING TO ELEMENT ACCUMULATOR           
         BZ    *+8                                                              
         LA    RF,8(RF)                                                         
*                                                                               
         MVC   0(4,RF),FONE        ADD 1 TO BUCKET                              
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFREC,1                              
POSTX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* TRACE ERRORS                                                        *         
***********************************************************************         
                                                                                
TRCERR   NTR1  ,                                                                
         CLI   ACTNSW,ACTNCHG      TEST ACTION CHANGE                           
         BE    POSTX                                                            
*                                                                               
         LA    RE,SRCHARG+L'SRCHARG-1  GET LENGTH OF DATA                       
         LA    R1,L'SRCHARG                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         B     POSTX                                                            
*                                                                               
         MVI   ITEMERR,C'Y'        CRITICAL ERROR                               
         AP    CNTERR,PONE         ERROR COUNT                                  
         L     RF,TRCNEXT          RF=NEXT AREA                                 
         SR    R0,R0                                                            
         ICM   R0,1,TRCCNT         COUNT NUMBER OF ERRORS                       
         BNZ   *+14                TEST FIRST TIME                              
         MVC   TRCBLK,SPACE        CLEAR THE BLOCK                              
         LA    RF,TRCBLK           RF=START OF DATA                             
         LA    R0,1(R0)            COUNT ERRORS                                 
         STC   R0,TRCCNT                                                        
*                                                                               
         LA    RE,4(R1,RF)         PLUS TAG (ACC=XXX) RE=END OF DATA            
         LA    R0,TRCBLK+L'TRCBLK-1 R0=END OF BLOCK                             
         CR    RE,R0               TEST PASSED END                              
         BH    POSTX               DON'T DO IT                                  
         LA    RE,1(RE)                                                         
         ST    RE,TRCNEXT          SAVE NEXT ADDRESS                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   4(0,RF),SRCHARG     MOVE DATA TO BLOCK                           
         SR    R1,R1                                                            
         IC    R1,ERRNUM           ERROR NUMBER                                 
         BCTR  R1,0                                                             
         MH    R1,=Y(L'TRCTAB)                                                  
         LA    R1,TRCTAB(R1)       R1=TRACE CODE                                
         MVC   0(3,RF),0(R1)       ACC=XXXX                                     
         MVI   3(RF),C'='                                                       
         B     POSTX                                                            
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS AND LITERAL POOL                                     *         
***********************************************************************         
                                                                                
TRCTAB   DS    0CL3                                                             
         DC    C'OFF'             OFFICE                                        
         DC    C'ACC'             ACCOUNT                                       
         DC    C'CLI'             CLIENT CODE                                   
         DC    C'MED'             MEDIA CODE                                    
         DC    C'WRK'             WORKCODE                                      
         DC    C'CLO'             CLIENT OFFICE                                 
         DC    C'BLS'             BILLING SOURCE                                
         DC    C'CAC'             CONTRA ACCOUNT                                
         DC    C'ACE'             ACCOUNT IN ELEMENT                            
         DC    C'OFE'             OFFICE IN ELEMENT                             
*                                                                               
SVMODE   DS    XL1                 SAVE PROCESSING MODE                         
                                                                                
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PARSE 1R ACCOUNT                                         *         
***********************************************************************         
                                                                                
         ENTRY PAR                                                              
PAR      DS    0D                                                               
         NMOD1 0,*PAR**,RA                                                      
*                                                                               
         L     R5,PARCTAB          R5=PARSE CONTROL TABLE                       
         SR    R8,R8                                                            
         ICM   R8,1,0(R5)          R8=DISP. TO START OF OFC/DEPT/SUB            
         MVC   ACCO,SPACE                                                       
         MVC   ACCO(2),=C'1R'      BUILD 1R KEY FORM CHARGE RATE KEY            
         LA    RE,ACCO+2           RE=ACCOUNT FIELD                             
         LA    RF,OLD1RLVS         RF=1R LENGTHS                                
         LA    R0,4                R0=NUMBER OF LEVELS                          
         SR    R1,R1                                                            
         SR    R3,R3                                                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         IC    R1,0(RF)            R1=LENGTH OF LEVEL                           
*                                                                               
PAR3     SR    R2,R2                                                            
         ICM   R2,1,0(R5)          DISP. TO KEY FIELD                           
         BNZ   PAR5                TEST OFC/DEPT/SUB ARE CONTIGUOUS             
         LR    R2,R6               R2=LENGTH OF PREVIOUS LEVEL                  
         AR    R2,R8               ADD DISP. TO OFC/DEPT/SUB                    
*                                                                               
PAR5     A     R2,PARINP           R2=INPUT DATA                                
         CLI   0(R2),C' '          TEST ANY DATA                                
         BNH   PAR7                END OF PERSON OFF/DEP/SUB/PER DATA           
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(R2)                                                    
         LA    R7,1(R7)            COUNT LEVELS                                 
         LA    R1,1(R1)            R1=LENGTH OF FIELD                           
         LR    R6,R1               R6=SAVE LENGTH OF PREVIOUS                   
         AR    R3,R1               R3=TOTAL LENGTH SO FAR                       
         LA    RE,0(R1,RE)         RE=NEXT FIELD                                
         LA    RF,1(RF)            RF=NEXT LEVEL                                
         IC    R1,0(RF)            R1=LENGTH TO END OF THIS LEVEL               
         SR    R1,R3               R1=LENGTH OF THIS FIELD                      
         LA    R5,1(R5)                                                         
         BCT   R0,PAR3                                                          
*                                                                               
PAR7     LTR   R7,R7                                                            
         BZ    PARX                                                             
         CH    R7,=H'4'            MUST BE 4 LEVELS                             
         BNE   PARX                                                             
*                                                                               
         LA    RF,L'ACCO                                                        
         LA    R1,ACCO+(L'ACCO-1)                                               
         CLI   0(R1),C' '          FIND ACCOUNT LENGTH                          
         BH    *+12                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         L     R3,ACCTAB                                                        
         L     R4,ACCNUM                                                        
*                                                                               
PAR9     EX    RF,*+8              MATCH TO ACCOUNT TABLE                       
         B     *+10                                                             
         CLC   0(0,R3),ACCO                                                     
         BE    PAR11                                                            
         LA    R3,ACCLNQ(R3)                                                    
         BCT   R4,PAR9                                                          
         B     PARX                                                             
*                                                                               
PAR11    MVC   ACCN,ACCN-ACCS(R3)                                               
         L     R5,PARCTAB          R5=DISP. TABLE FOR THIS RECORD               
         LA    RE,ACCN+2           RE=ACCOUNT FIELD                             
         LA    RF,NEW1RLVS         RF=1R LENGTHS                                
         LR    R0,R7               R0=NUMBER OF LEVELS                          
         SR    R1,R1                                                            
         SR    R6,R6                                                            
         SR    R3,R3                                                            
         IC    R1,0(RF)            R1=LENGTH OF LEVEL                           
*                                                                               
PAR13    SR    R2,R2                                                            
         ICM   R2,1,0(R5)          DISP. TO KEY FIELD                           
         BNZ   PAR15               TEST OFC/DEPT/SUB ARE CONTIGUOUS             
         LR    R2,R6               R2=LENGTH OF PREVIOUS LEVEL                  
         AR    R2,R8               ADD DISP. TO OFC/DEPT/SUB                    
*                                                                               
PAR15    A     R2,PAROUT           R2=A(OUTPUT DATA)                            
         BCTR  R1,0                                                             
         CLI   0(R2),C' '          TEST ANY DATA                                
         BNH   *+14                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),0(RE)                                                    
         LA    R1,1(R1)            R1=LENGTH OF FIELD                           
         LR    R6,R1                                                            
         AR    R3,R1               R3=TOTAL LENGTH SO FAR                       
         LA    RE,0(R1,RE)         RE=NEXT FIELD                                
         LA    RF,1(RF)            RF=NEXT LEVEL                                
         IC    R1,0(RF)            R1=LENGTH TO END OF THIS LEVEL               
         SR    R1,R3               R1=LENGTH OF THIS FIELD                      
         LA    R5,1(R5)                                                         
         BCT   R0,PAR13                                                         
*                                                                               
PARX     XIT1                                                                   
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT ROUTINES                                                      *         
***********************************************************************         
                                                                                
         ENTRY RPT                                                              
RPT      DS    0D                                                               
         NMOD1 0,*RPT**,RA                                                      
         LR    R3,R1                                                            
         XC    RDMPS,RDMPS                                                      
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
         L     R7,BOXAREA                                                       
         USING BOXD,R7                                                          
         TM    OPTSW,NOLIST        SKIP THE REPORTS                             
         BO    RPT3                                                             
         BAS   RE,RPTACC           PRINT ACCOUNT TABLE                          
         BAS   RE,RPTOFF           PRINT OFFICE  TABLE                          
         BAS   RE,RPTCLI           PRINT CLIENT TABLE                           
         BAS   RE,RPTMED           PRINT MEDIA TABLE                            
         BAS   RE,RPTWRK           PRINT WORKCODE TABLE                         
         BAS   RE,RPTCLO           PRINT CLIENT OFFICE TABLE                    
         BAS   RE,RPTBLS           PRINT BILLING SOURCE TABLE                   
RPT3     BAS   RE,RPTEND           PRINT ERROR AND RECORD COUNTS                
*                                                                               
RPTX     XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT ACCOUNT CONVERSION LIST                                       *         
***********************************************************************         
                                                                                
RPTACC   NTR1  ,                                                                
         OC    ACCNUM,ACCNUM                                                    
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES         ACCOUNT LIST                                 
         MVC   WORK(L'ACLTIT),ACLTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING ACLRD,R5                                                         
         MVI   ACLRL,C'L'                                                       
         MVI   ACLRC1,C'C'                                                      
         MVI   ACLRC2,C'C'                                                      
         MVI   ACLRC3,C'C'                                                      
         MVI   ACLRC4,C'C'                                                      
         MVI   ACLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   ACLROLD(11),=C'OLD ACCOUNT'                                      
         MVC   ACLRONME(8),=C'OLD NAME'                                         
         MVC   ACLRNEW(11),=C'NEW ACCOUNT'                                      
         MVC   ACLRNNME(8),=C'NEW NAME'                                         
         MVC   ACLRHIT(10),=C'  ACCOUNT '                                       
         LA    R5,SUB2                                                          
         MVC   ACLRHIT(10),=C'  MATCHES '                                       
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,ACCTAB           R2= A(OFFICE TABLE)                          
         L     R0,ACCNUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTACC3  MVC   ACCS,0(R2)                                                       
         MVC   ACLROLD(14),ACCO    OLD ACCOUNT                                  
         MVC   ACLRNEW(14),ACCN    NEW ACCOUNT                                  
         MVC   ACLRNNME,ACCNME     NEW ACCOUNT NAME                             
         EDIT  ACCH,(10,ACLRHIT)   NUMBER                                       
         CP    ACCH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   ACLRHIT,DCDUP                                                    
         LA    R3,ACLROLD                                                       
         LA    R4,ACLRONME                                                      
         BAS   RE,GETNME           GET ACCOUNT NAME                             
         GOTO1 PRINTER                                                          
         LA    R2,ACCLNQ(R2)                                                    
         BCT   R0,RPTACC3                                                       
         B     RPTBXX                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT OFFICE LIST                                                   *         
***********************************************************************         
                                                                                
RPTOFF   NTR1  ,                                                                
         OC    OFFNUM,OFFNUM       TEST NUMBER OF ENTRIES                       
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES         OFFICE LIST                                  
         MVC   WORK(L'OFLTIT),OFLTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DCLRD,R5                                                         
         MVI   DCLRL,C'L'                                                       
         MVI   DCLRC1,C'C'                                                      
         MVI   DCLRC2,C'C'                                                      
         MVI   DCLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DCLROLD,DCOLD                                                    
         MVC   DCLRNEW,DCNEW                                                    
         MVC   DCLRHIT,DCOFF                                                    
         LA    R5,SUB2                                                          
         MVC   DCLROLD,DCOFF                                                    
         MVC   DCLRNEW,DCOFF                                                    
         MVC   DCLRHIT,DCMAT                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,OFFTAB           R2= A(OFFICE TABLE)                          
         L     R0,OFFNUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTOFF3  MVC   OFFS,0(R2)                                                       
         MVC   DCLROLD+4(1),OFFO    OLD OFFICE                                  
         MVC   DCLRNEW+4(2),OFFN    NEW OFFICE                                  
         EDIT  OFFH,(10,DCLRHIT)    NUMBER                                      
         CP    OFFH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   DCLRHIT,DCDUP                                                    
         BASR  RE,RF                                                            
         LA    R2,OFFLNQ(R2)                                                    
         BCT   R0,RPTOFF3                                                       
         B     RPTBXX              CLOSE BOX                                    
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT CLIENT LIST                                                   *         
***********************************************************************         
                                                                                
RPTCLI   NTR1  ,                                                                
         OC    CLINUM,CLINUM       TEST NUMBER OF ENTRIES                       
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES         OFFICE LIST                                  
         MVC   WORK(L'CLITIT),CLITIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DCLRD,R5                                                         
         MVI   DCLRL,C'L'                                                       
         MVI   DCLRC1,C'C'                                                      
         MVI   DCLRC2,C'C'                                                      
         MVI   DCLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DCLROLD,DCOLD                                                    
         MVC   DCLRNEW,DCNEW                                                    
         MVC   DCLRHIT,DCCLI                                                    
         LA    R5,SUB2                                                          
         MVC   DCLROLD,DCCLI                                                    
         MVC   DCLRNEW,DCCLI                                                    
         MVC   DCLRHIT,DCMAT                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,CLITAB           R2= A(CLIENT TABLE)                          
         L     R0,CLINUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTCLI3  MVC   CLIS,0(R2)                                                       
         MVC   DCLROLD+4(3),CLIO    OLD CLIENT                                  
         MVC   DCLRNEW+4(3),CLIN    NEW CLIENT                                  
         EDIT  CLIH,(10,DCLRHIT)    NUMBER                                      
         CP    CLIH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   DCLRHIT,DCDUP                                                    
         BASR  RE,RF                                                            
         LA    R2,CLILNQ(R2)                                                    
         BCT   R0,RPTCLI3                                                       
         B     RPTBXX              CLOSE BOX                                    
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA  LIST                                                   *         
***********************************************************************         
                                                                                
RPTMED   NTR1  ,                                                                
         OC    MEDNUM,MEDNUM       TEST NUMBER OF ENTRIES                       
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES         OFFICE LIST                                  
         MVC   WORK(L'MEDTIT),MEDTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DCLRD,R5                                                         
         MVI   DCLRL,C'L'                                                       
         MVI   DCLRC1,C'C'                                                      
         MVI   DCLRC2,C'C'                                                      
         MVI   DCLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DCLROLD,DCOLD                                                    
         MVC   DCLRNEW,DCNEW                                                    
         MVC   DCLRHIT,DCMED                                                    
         LA    R5,SUB2                                                          
         MVC   DCLROLD,DCMED                                                    
         MVC   DCLRNEW,DCMED                                                    
         MVC   DCLRHIT,DCMAT                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,MEDTAB           R2= A(MEDIA TABLE)                           
         L     R0,MEDNUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTMED3  MVC   MEDS,0(R2)                                                       
         MVC   DCLROLD+4(1),MEDO    OLD MEDIA                                   
         MVC   DCLRNEW+4(1),MEDN    NEW MEDIA                                   
         EDIT  MEDH,(10,DCLRHIT)    NUMBER                                      
         CP    MEDH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   DCLRHIT,DCDUP                                                    
         BASR  RE,RF                                                            
         LA    R2,MEDLNQ(R2)                                                    
         BCT   R0,RPTMED3                                                       
         B     RPTBXX              CLOSE BOX                                    
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT WORKCODE LIST                                                 *         
***********************************************************************         
                                                                                
RPTWRK   NTR1  ,                                                                
         OC    WRKNUM,WRKNUM       TEST NUMBER OF ENTRIES                       
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES         OFFICE LIST                                  
         MVC   WORK(L'WRKTIT),WRKTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DCLRD,R5                                                         
         MVI   DCLRL,C'L'                                                       
         MVI   DCLRC1,C'C'                                                      
         MVI   DCLRC2,C'C'                                                      
         MVI   DCLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DCLROLD,DCOLD                                                    
         MVC   DCLRNEW,DCNEW                                                    
         MVC   DCLRHIT,DCWRK                                                    
         LA    R5,SUB2                                                          
         MVC   DCLROLD,DCWRK                                                    
         MVC   DCLRNEW,DCWRK                                                    
         MVC   DCLRHIT,DCMAT                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,WRKTAB           R2= A(W/CODE TABLE)                          
         L     R0,WRKNUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTWRK3  MVC   WRKS,0(R2)                                                       
         MVC   DCLROLD+4(2),WRKO    OLD W/CODE                                  
         MVC   DCLRNEW+4(2),WRKN    NEW W/CODE                                  
         EDIT  WRKH,(10,DCLRHIT)    NUMBER                                      
         CP    WRKH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   DCLRHIT,DCDUP                                                    
         BASR  RE,RF                                                            
         LA    R2,WRKLNQ(R2)                                                    
         BCT   R0,RPTWRK3                                                       
         B     RPTBXX              CLOSE BOX                                    
         EJECT                                                                  
***********************************************************************         
* PRINT CLIENT/OFFICE LIST                                            *         
***********************************************************************         
                                                                                
RPTCLO   NTR1  ,                                                                
         OC    CLONUM,CLONUM       TEST NUMBER OF ENTRIES                       
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES         OFFICE LIST                                  
         MVC   WORK(L'CLOTIT),CLOTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DCLRD,R5                                                         
         MVI   DCLRL,C'L'                                                       
         MVI   DCLRC1,C'C'                                                      
         MVI   DCLRC2,C'C'                                                      
         MVI   DCLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DCLROLD,DCCLI                                                    
         MVC   DCLRNEW,DCOFF                                                    
         MVC   DCLRHIT,DCCLI                                                    
         LA    R5,SUB2                                                          
         MVC   DCLRHIT,DCMAT                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,CLOTAB           R2= A(W/CODE TABLE)                          
         L     R0,CLONUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTCLO3  MVC   CLOS,0(R2)                                                       
         MVC   DCLROLD+4(3),CLOC    CLIENT CODE                                 
         MVC   DCLRNEW+4(2),CLOO    NEW OFFICE                                  
         EDIT  CLOH,(10,DCLRHIT)    NUMBER                                      
         CP    CLOH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   DCLRHIT,DCDUP                                                    
         BASR  RE,RF                                                            
         LA    R2,CLOLNQ(R2)                                                    
         BCT   R0,RPTCLO3                                                       
         B     RPTBXX              CLOSE BOX                                    
         EJECT                                                                  
***********************************************************************         
* PRINT BILLING SOURCE LIST                                           *         
***********************************************************************         
                                                                                
RPTBLS   NTR1  ,                                                                
         OC    BLSNUM,BLSNUM       TEST NUMBER OF ENTRIES                       
         BZ    RPTBXX                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BLSTIT),BLSTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DCLRD,R5                                                         
         MVI   DCLRL,C'L'                                                       
         MVI   DCLRC1,C'C'                                                      
         MVI   DCLRC2,C'C'                                                      
         MVI   DCLRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DCLROLD,DCOLD                                                    
         MVC   DCLRNEW,DCNEW                                                    
         MVC   DCLRHIT,DCBLS                                                    
         LA    R5,SUB2                                                          
         MVC   DCLROLD,DCBLS                                                    
         MVC   DCLRNEW,DCBLS                                                    
         MVC   DCLRHIT,DCMAT                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         L     R2,BLSTAB           R2= A(W/CODE TABLE)                          
         L     R0,BLSNUM           R0= NUMBER OF ENTRIES                        
         LA    R5,P                                                             
*                                                                               
RPTBLS3  MVC   BLSS,0(R2)                                                       
         MVC   DCLROLD(L'BLSO),BLSO    OLD SOUCES                               
         MVC   DCLRNEW(L'BLSN),BLSN    NEW SOURCE                               
         EDIT  BLSH,(10,DCLRHIT)       NUMBER                                   
         CP    BLSH,=P'0'                                                       
         BE    *+18                                                             
         CLI   DUPERR,0                                                         
         BE    *+10                                                             
         MVC   DCLRHIT,DCDUP                                                    
         BASR  RE,RF                                                            
         LA    R2,BLSLNQ(R2)                                                    
         BCT   R0,RPTBLS3                                                       
         B     RPTBXX              CLOSE BOX                                    
         EJECT                                                                  
***********************************************************************         
* PRINT END  REPORTS                                                  *         
***********************************************************************         
                                                                                
RPTEND   NTR1  ,                                                                
         CLI   ACTNSW,ACTNCHG      TEST ACTION CHANGE                           
         BE    RPTEND5                                                          
         L     R3,ARECT            PRINT ERRORS BY RECORD TYPE                  
RPTEND1  MVC   RDMPS,0(R3)                                                      
         BAS   RE,ACCERR           ACCOUNT ERRORS                               
         LA    R2,MISTAB           OTHER DATA                                   
RPTEND3  MVC   MISREC,0(R2)                                                     
         BAS   RE,MISERR           MISSING REPORTS                              
         LA    R2,MISLNQ(R2)                                                    
         CLI   0(R2),EOT                                                        
         BNE   RPTEND3                                                          
         LA    R3,RDMPLNQ(R3)      NEXT RECORD TYPE                             
         CLI   0(R3),EOT                                                        
         BNE   RPTEND1                                                          
*                                                                               
RPTEND5  XC    RDMPS,RDMPS         CLEAR RECORD NAMES                           
         GOTO1 ACNVCNT,DMCB,('RACTPRT',0)                                       
         BAS   RE,RECCNT           RECORD COUNTS                                
         B     RPTX                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT ACCOUNT ERROR REPORT                                          *         
***********************************************************************         
                                                                                
ACCERR   NTR1  ,                                                                
         XC    BUFREC(BUFRLNQ),BUFREC                                           
         MVC   BUFRTYP,RDCDE       RECORD TYPE                                  
         MVI   BUFETYP,BUFTACC                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFC,BUFREC,1                             
         CLI   8(R1),0                                                          
         BNE   RPTX                                                             
         CLC   BUFRTYP,RDCDE       RECORD TYPE                                  
         BNE   RPTX                                                             
         CLI   BUFETYP,BUFTACC                                                  
         BNE   RPTX                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'ACETIT),ACETIT       ACCOUNT ERRORS                       
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING AELRD,R5                                                         
         MVI   AELRL,C'L'                                                       
         MVI   AELRC1,C'C'                                                      
         MVI   AELRC2,C'C'                                                      
         MVI   AELRC3,C'C'                                                      
         MVI   AELRC4,C'C'                                                      
         MVI   AELRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   AELRCDE,DCOLDACC                                                 
         MVC   AELRNME,DCACCNME                                                 
         MVC   AELRACC,DCACCNT                                                  
         MVC   AELRCON,DCCNTRA                                                  
         MVC   AELRELM,DCELMNT                                                  
         LA    R5,SUB2                                                          
         MVC   AELRACC,DCMISACC                                                 
         MVC   AELRCON,DCMISCON                                                 
         MVC   AELRELM,DCMISELM                                                 
         GOTO1 PRINTER                                                          
         SR    R3,R3               COUNT ERRORS                                 
         LA    R5,P                                                             
         B     ACCERR5                                                          
*                                                                               
ACCERR3  GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFREC,1                              
         CLI   8(R1),0                                                          
         BNE   RPTBXX                                                           
         CLC   BUFRTYP,RDCDE       RECORD TYPE                                  
         BNE   RPTBXX                                                           
         CLI   BUFETYP,BUFTACC                                                  
         BNE   RPTBXX                                                           
*                                                                               
ACCERR5  MVC   AELRCDE,BUFDATA                                                  
         EDIT  (B4,BUFBACC),(8,AELRACC),ZERO=BLANK                              
         EDIT  (B4,BUFBCON),(8,AELRCON),ZERO=BLANK                              
         EDIT  (B4,BUFBELM),(8,AELRELM),ZERO=BLANK                              
         LA    R3,AELRCDE                                                       
         LA    R4,AELRNME                                                       
         BAS   RE,GETNME                                                        
         TM    ACCERSW,SUPACER                                                  
         BZ    ACCERR8                                                          
         CLC   AELRNME(36),=CL36' ** ACCOUNT NOT ON FILE **'                    
         BNE   ACCERR8                                                          
         MVC   P,SPACES                                                         
         B     ACCERR9                                                          
ACCERR8  GOTO1 PRINTER                                                          
         AH    R3,=H'1'               COUNT ERRORS                              
ACCERR9  OC    MAXERR,MAXERR          TEST MAX SET                              
         BZ    ACCERR3                                                          
         C     R3,MAXERR                                                        
         BNH   ACCERR3                                                          
         B     RPTBXX                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT MISSING DATA REPORT                                           *         
***********************************************************************         
                                                                                
MISERR   NTR1  ,                                                                
         XC    BUFREC(BUFRLNQ),BUFREC                                           
         MVC   BUFRTYP,RDCDE       RECORD TYPE                                  
         MVC   BUFETYP,MISTYP                                                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFC,BUFREC,1                             
         CLI   8(R1),0             TEST ANY ERRORS                              
         BNE   RPTX                                                             
         CLC   BUFRTYP,RDCDE       RECORD TYPE                                  
         BNE   RPTX                                                             
         CLC   BUFETYP,MISTYP                                                   
         BNE   RPTX                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'MISTIT),MISTIT    TITLE                                   
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING DELRD,R5                                                         
         MVI   DELRL,C'L'                                                       
         MVI   DELRC1,C'C'                                                      
         MVI   DELRC2,C'C'                                                      
         MVI   DELRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   DELRCDE,MISCOL      SET COLUMN CAPTIONS                          
         MVC   DELRKEY,MISKEY1                                                  
         MVC   DELRELM,MISELM1                                                  
         LA    R5,SUB2                                                          
         MVC   DELRKEY,MISKEY2                                                  
         MVC   DELRELM,MISELM2                                                  
         GOTO1 PRINTER                                                          
         LA    R5,P                                                             
         B     MISERR5                                                          
*                                                                               
MISERR3  GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFREC,1                              
         CLI   8(R1),0                                                          
         BNE   RPTBXX                                                           
         CLC   BUFRTYP,RDCDE       RECORD TYPE                                  
         BNE   RPTBXX                                                           
         CLC   BUFETYP,MISTYP                                                   
         BNE   RPTBXX                                                           
*                                                                               
MISERR5  MVC   DELRCDE(L'BUFDATA),BUFDATA                                       
         EDIT  (B4,BUFBKEY),(8,DELRKEY),ZERO=BLANK                              
         EDIT  (B4,BUFBELM),(8,DELRELM),ZERO=BLANK                              
         GOTO1 PRINTER                                                          
         B     MISERR3                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT RECORD COUNTS                                                 *         
***********************************************************************         
                                                                                
RECCNT   NTR1  ,                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'RECTIT),RECTIT                                            
         BAS   RE,SETBOX                                                        
         LA    R5,BOXCOLS                                                       
         USING CNTRD,R5                                                         
         MVI   CNTRL,C'L'                                                       
         MVI   CNTRC1,C'C'                                                      
         MVI   CNTRR,C'R'                                                       
         LA    R5,SUB1                                                          
         MVC   CNTRDSC(20),=CL20'RECORD DESCRIPTION'                            
         MVC   CNTRCNT+3(6),=C'NUMBER'                                          
         GOTO1 PRINTER                                                          
         LA    R2,CNTS                                                          
         LA    R5,P                                                             
*                                                                               
RECCNT3  MVC   CNTRDSC,5(R2)                                                    
         EDIT  (P5,0(R2)),(10,CNTRCNT)                                          
         GOTO1 PRINTER                                                          
         LA    R2,L'CNTS(R2)                                                    
         CLI   0(R2),EOT                                                        
         BNE   RECCNT3                                                          
*                                                                               
RPTBXX   MVI   BOXREQ,C'C'         CLOSE BOX                                    
         GOTO1 PRINTER                                                          
         MVI   BOXYORN,C'N'                                                     
         B     RPTX                                                             
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT NAME                                                    *         
***********************************************************************         
                                                                                
GETNME   NTR1 ,                                                                 
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),COMPANY                                                  
         MVC   DKEY+1(14),0(R3)    SET ACCOUNT KEY                              
         GOTO1 ADMGR,DMHI          READ HIGH FOR ACCOUNT                        
         CLC   DKEY,DIR                                                         
         BNE   GETNME5                                                          
         L     R2,AIO3                                                          
         GOTO1 ADMGR,DMGET         GET THE RECORD                               
         USING ACTRECD,R2                                                       
         SR    R1,R1                                                            
         LA    R5,ACTRFST                                                       
*                                                                               
         USING NAMELD,R5                                                        
GETNME3  IC    R1,NAMLN                                                         
         CLI   0(R5),0                                                          
         BE    GETNME5                                                          
         CLI   0(R5),NAMELQ                                                     
         BE    GETNME4                                                          
         AR    R5,R1                                                            
         B     GETNME3                                                          
*                                                                               
GETNME4  SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,R4),NAMEREC                                                  
         B     RPTX                                                             
*                                                                               
GETNME5  MVC   0(36,R4),=CL36' ** ACCOUNT NOT ON FILE **'                       
         B     RPTX                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRINT AND BOX                                            *         
***********************************************************************         
                                                                                
SETBOX   NTR1  ,                                                                
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVI   MID1,0                                                           
         MVI   MID2,0                                                           
         MVI   MID3,0                                                           
         MVI   MID4,0                                                           
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         ZAP   MAXLINE,=P'57'                                                   
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVC   TITLE,WORK                                                       
         LA    R0,L'TITLE                                                       
         GOTO1 CENTER,DMCB,TITLE,(R0)                                           
         LA    R3,MID2+(TITLE-HEADDATE)                                         
         MVC   0(L'COMPNME,R3),COMPNME     COMPANY NAME                         
         LA    R0,L'TITLE                                                       
         GOTO1 CENTER,DMCB,(R3),(R0)                                            
*                                                                               
         CLI   RDNME,C' '           RECORD EQUATE                               
         BNH   SETBOX3                                                          
         LA    R3,132(R3)                                                       
         MVC   0(L'RDNME,R3),RDNME                                              
         GOTO1 CENTER,DMCB,(R3),(R0)                                            
         CLI   RDLNME,C' '          RECORD NAME                                 
         BNH   SETBOX3                                                          
         LA    R3,132(R3)                                                       
         MVC   0(L'RDLNME,R3),RDLNME                                            
         GOTO1 CENTER,DMCB,(R3),(R0)                                            
*                                                                               
SETBOX3  MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXMAXL,57                                                       
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         TM    OPTSW,STRIPES                                                    
         BNO   *+12                                                             
         MVI   BOXSHADE,4                                                       
         MVI   BOXSHCH1,X'42'                                                   
         B     RPTX                                                             
         DROP  R7,R8                                                            
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
OFLTIT   DC    C'OFFICE CONVERSION LIST'                                        
ACLTIT   DC    C'ACCOUNT CONVERSION LIST'                                       
CLITIT   DC    C'CLIENT CONVERSION LIST'                                        
MEDTIT   DC    C'MEDIA CONVERSION LIST'                                         
WRKTIT   DC    C'WORKCODE CONVERSION LIST'                                      
CLOTIT   DC    C'CLIENT/OFFICE CONVERSION LIST'                                 
BLSTIT   DC    C'BILLING SOURCE CONVERSION LIST'                                
ACETIT   DC    C'MISSING ACCOUNT CODES'                                         
RECTIT   DC    C'RECORD SUMMARY'                                                
*                                                                               
MISREC   DS    0CL(MISLNQ)         MISSING DATA REPORTS                         
MISTYP   DS    XL1                 BUFFALO TYPE                                 
MISTIT   DS    CL36                TITLE FOR REPORT                             
MISCOL   DS    CL13                CAPTION FOR COLUMN                           
MISKEY1  DS    CL8                 CAPTION FOR KEY  COLUMNS                     
MISKEY2  DS    CL8                                                              
MISELM1  DS    CL8                 CAPTION FOR ELEMENT COLUMNS                  
MISELM2  DS    CL8                                                              
MISLNQ   EQU   *-MISTYP                                                         
*                                                                               
MISTAB   DS    0H                                                               
OFFMIS   DC    AL1(BUFTOFF)                                                     
         DC    CL36'MISSING OFFICE CODES'                                       
         DC    CL13'OFFICE'                                                     
         DC    CL8' KEY    '                                                    
         DC    CL8' MISSES '                                                    
         DC    CL8'ELEMENT '                                                    
         DC    CL8' MISSES '                                                    
*                                                                               
CLOMIS   DC    AL1(BUFTCLO)                                                     
         DC    CL36'MISSING CLIENT/ OFFICE'                                     
         DC    CL13'CLIENT'                                                     
         DC    CL8' KEY    '                                                    
         DC    CL8' MISSES '                                                    
         DC    CL8'ELEMENT '                                                    
         DC    CL8' MISSES '                                                    
*                                                                               
CLIMIS   DC    AL1(BUFTCLI)                                                     
         DC    CL36'MISSING CLIENT CODES'                                       
         DC    CL13'CLIENT'                                                     
         DC    CL8' KEY    '                                                    
         DC    CL8' MISSES '                                                    
         DC    CL8'ELEMENT '                                                    
         DC    CL8' MISSES '                                                    
*                                                                               
MEDMIS   DC    AL1(BUFTMED)                                                     
         DC    CL36'MISSING MEDIA CODES'                                        
         DC    CL13'MEDIA '                                                     
         DC    CL8' KEY    '                                                    
         DC    CL8' MISSES '                                                    
         DC    CL8'ELEMENT '                                                    
         DC    CL8' MISSES '                                                    
*                                                                               
WRKMIS   DC    AL1(BUFTWRK)                                                     
         DC    CL36'MISSING WORKCODES'                                          
         DC    CL13'WORKCODE'                                                   
         DC    CL8' KEY    '                                                    
         DC    CL8' MISSES '                                                    
         DC    CL8'ELEMENT '                                                    
         DC    CL8' MISSES '                                                    
*                                                                               
BLSMIS   DC    AL1(BUFTBLS)                                                     
         DC    CL36'MISSING BILLING SOURCE'                                     
         DC    CL13'BILL SOURCE'                                                
         DC    CL8' KEY    '                                                    
         DC    CL8' MISSES '                                                    
         DC    CL8'ELEMENT '                                                    
         DC    CL8' MISSES '                                                    
         DC    X'FF'                                                            
*                                                                               
DCOLDACC DC    CL(L'AELRCDE)'OLD ACCOUNT'                                       
DCACCNME DC    CL(L'AELRNME)'NAME'                                              
DCACCNT  DC    CL(L'AELRACC)'ACCOUNT'                                           
DCMISACC DC    CL(L'AELRACC)'MISSES'                                            
DCCNTRA  DC    CL(L'AELRCON)'CONTRA'                                            
DCMISCON DC    CL(L'AELRCON)'MISSES'                                            
DCELMNT  DC    CL(L'AELRELM)'ELEMENT'                                           
DCMISELM DC    CL(L'AELRELM)'MISSES'                                            
*                                                                               
DCOLD    DC    CL(L'DCLROLD)'    OLD      '                                     
DCNEW    DC    CL(L'DCLRNEW)'    NEW      '                                     
DCOFF    DC    CL(L'DCLRHIT)'   OFFICE    '                                     
DCCLI    DC    CL(L'DCLRHIT)'   CLIENT    '                                     
DCMED    DC    CL(L'DCLRHIT)'   MEDIA     '                                     
DCWRK    DC    CL(L'DCLRHIT)'  WORKCODE   '                                     
DCBLS    DC    CL(L'DCLRHIT)' BILL SOURCE '                                     
DCMAT    DC    CL(L'DCLRHIT)'   MATCHES   '                                     
DCDUP    DC    CL(L'DCLRHIT)'  DUPLICATE  '                                     
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
         ENTRY DMGR                                                             
DMGR     DS    0D                                                               
         NMOD1 0,*DMGR*,RA                                                      
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     DMGREAD                                                          
         B     DMGHIGH                                                          
         B     DMGSEQ                                                           
         B     DMGGET                                                           
         B     DMGWRT                                                           
         B     DMGPUT                                                           
         B     DMGADD                                                           
*                                                                               
DMGREAD  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         B     DMGNXT                                                           
*                                                                               
DMGHIGH  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,DKEY,DIR                      
         B     DMGNXT                                                           
*                                                                               
DMGSEQ   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),ACCDIR,DKEY,DIR                      
DMGNXT   MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         OC    DA,DA                                                            
         BZ    DMGSEQ                                                           
         B     DMERR                                                            
*                                                                               
DMGGET   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMGWRT   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         TM    OUTPUT,WRTNO                                                     
         BO    *+6                                                              
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMGPUT   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
         ORG   *-2                                                              
         TM    OUTPUT,WRTNO                                                     
         BO    *+6                                                              
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMGADD   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         ORG   *-2                                                              
         TM    OUTPUT,WRTNO                                                     
         BO    *+6                                                              
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMERR    MVC   DMBYTE,8(R1)                                                     
         NI    DMBYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED             
         TM    DMBYTE,X'80'        PASS BACK EOF                                
         BO    DMXIT                                                            
         CLI   DMBYTE,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DMXIT    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE                                                      *         
***********************************************************************         
                                                                                
         ORG   ACNV00+(4096*5)                                                  
ACNVC    CSECT                                                                  
         ENTRY UTL                                                              
       ++INCLUDE ACNVWORK                                                       
         ORG   ACNVC+4096                                                       
         EJECT                                                                  
***********************************************************************         
*  RECORD TABLE                                                       *         
***********************************************************************         
         ENTRY RECT                                                             
RECT     DS    0D                                                               
         DC    AL1(ACRTUNKN),AL3(XXXK),CL8'ACRTUNKN'                            
         DC    CL(L'RECNAME)'Unknown'                                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCPY),AL3(0),CL8'ACRTCPY'                                 
         DC    CL(L'RECNAME)'Company'                                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTUNT),AL3(0),CL8'ACRTUNT'                                 
         DC    CL(L'RECNAME)'Unit'                                              
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTLDG),AL3(0),CL8'ACRTLDG'                                 
         DC    CL(L'RECNAME)'Ledger'                                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTACTH),AL3(0),CL8'ACRTACTH'                               
         DC    CL(L'RECNAME)'High Account'                                      
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTACTL),AL3(0),CL8'ACRTACTL'                               
         DC    CL(L'RECNAME)'Low Account'                                       
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOFA),AL3(0),CL8'ACRTOFA'                                 
         DC    CL(L'RECNAME)'Account Office'                                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCHDP),AL3(0),CL8'ACRTCHDP'                               
         DC    CL(L'RECNAME)'Contra-account Passive'                            
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCHDH),AL3(0),CL8'ACRTCHDH'                               
         DC    CL(L'RECNAME)'Contra-account Header'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCAC),AL3(0),CL8'ACRTCAC'                                 
         DC    CL(L'RECNAME)'History Bucket'                                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTRN),AL3(0),CL8'ACRTTRN'                                 
         DC    CL(L'RECNAME)'ACCMST Transaction'                                
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTRNA),AL3(0),CL8'ACRTTRNA'                               
         DC    CL(L'RECNAME)'ACCARC Transaction'                                
         DC    AL1(RECIDIR+RECIARC)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOTHR),AL3(0),CL8'ACRTOTHR'                               
         DC    CL(L'RECNAME)'Other'                                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOFF),AL3(0),CL8'ACRTOFF'                                 
         DC    CL(L'RECNAME)'X''01'' Office'                                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTMIN),AL3(0),CL8'ACRTMIN'                                 
         DC    CL(L'RECNAME)'X''09'' Production Media'                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPMD),AL3(PMDK),CL8'ACRTPMD'                              
         DC    CL(L'RECNAME)'X''08'' Media Interface'                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTWCO),AL3(WCOK),CL8'ACRTWCO'                              
         DC    CL(L'RECNAME)'X''0A'' Analysis Code'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTBAT),AL3(0),CL8'ACRTBAT'                                 
         DC    CL(L'RECNAME)'X''0B'' Batch'                                     
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSCM),AL3(0),CL8'ACRTSCM'                                 
         DC    CL(L'RECNAME)'X''0C'' Comment'                                   
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTORD),AL3(0),CL8'ACRTORD'                                 
         DC    CL(L'RECNAME)'X''1A'' Order'                                     
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTBUD),AL3(BUDK),CL8'ACRTBUD'                              
         DC    CL(L'RECNAME)'X''1B'' Budget'                                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTLST),AL3(0),CL8'ACRTLST'                                 
         DC    CL(L'RECNAME)'X''1D'' List'                                      
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTFEEC),AL3(0),CL8'ACRTFEEC'                               
         DC    CL(L'RECNAME)'X''1F'' Old Artist Fee Control'                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTFEEA),AL3(0),CL8'ACRTFEEA'                               
         DC    CL(L'RECNAME)'X''20'' Old Artist Fee Area'                       
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTFEEP),AL3(0),CL8'ACRTFEEP'                               
         DC    CL(L'RECNAME)'X''21'' Old Artist Fee Percent'                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPCR),AL3(PCHK),CL8'ACRTPCR'                              
         DC    CL(L'RECNAME)'X''2A'' Charge Rate'                               
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPRL),AL3(0),CL8'ACRTPRL'                                 
         DC    CL(L'RECNAME)'X''1C'' Price List'                                
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSBL),AL3(GSBK),CL8'ACRTSBL'                              
         DC    CL(L'RECNAME)'X''2E'' Split Billing'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOGRG),AL3(0),CL8'ACRTOGRG'                               
         DC    CL(L'RECNAME)'X''2C02'' Office Group'                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOGRO),AL3(OGROK),CL8'ACRTOGRO'                           
         DC    CL(L'RECNAME)'X''2C04'' Office'                                  
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTMGR),AL3(0),CL8'ACRTMGR'                                 
         DC    CL(L'RECNAME)'X''2C06'' Media Group'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTWGR),AL3(0),CL8'ACRTWGR'                                 
         DC    CL(L'RECNAME)'X''2C08'' Workcode Group'                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTUFS),AL3(UFSK),CL8'ACRTUFS'                              
         DC    CL(L'RECNAME)'X''2C10'' User Field'                              
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPOP),AL3(POPK),CL8'ACRTPOP'                              
         DC    CL(L'RECNAME)'X''2C20'' Options'                                 
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSCH),AL3(0),CL8'ACRTSCH'                                 
         DC    CL(L'RECNAME)'X''2C30'' Scheme Header'                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCAT),AL3(0),CL8'ACRTCAT'                                 
         DC    CL(L'RECNAME)'X''2C32'' Category'                                
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPAN),AL3(0),CL8'ACRTPAN'                                 
         DC    CL(L'RECNAME)'X''2C34'' Panel'                                   
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTEVE),AL3(EVEK),CL8'ACRTEVE'                              
         DC    CL(L'RECNAME)'X''2C36'' Estimate Version'                        
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTXT),AL3(TXTK),CL8'ACRTTXT'                              
         DC    CL(L'RECNAME)'X''2C38'' Text'                                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSUT),AL3(0),CL8'ACRTSUT'                                 
         DC    CL(L'RECNAME)'X''2D01'' Sales Tax'                               
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTRES),AL3(0),CL8'ACRTRES'                                 
         DC    CL(L'RECNAME)'X''2D02'' Scribe Format'                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTINT),AL3(INTK),CL8'ACRTINT'                              
         DC    CL(L'RECNAME)'X''2D03'' Intagy Estimate'                         
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTIDJ),AL3(IDJK),CL8'ACRTIDJ'                              
         DC    CL(L'RECNAME)'X''2D04'' Intagy Journal Passive'                  
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTRAT),AL3(0),CL8'ACRTRAT'                                 
         DC    CL(L'RECNAME)'X''2D05'' Interest Rate'                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTDAT),AL3(DATK),CL8'ACRTDAT'                              
         DC    CL(L'RECNAME)'X''2C40'' Project Date'                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
*                                                                               
         DC    AL1(ACRTTAX),AL3(TAXK),CL8'ACRTTAX'                              
         DC    CL(L'RECNAME)'X''05'' Tax Rules'                                 
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTNBT),AL3(0),CL8'ACRTNBT'                                 
         DC    CL(L'RECNAME)'X''03'' New Batch'                                 
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTNBP),AL3(0),CL8'ACRTNBP'                                 
         DC    CL(L'RECNAME)'X''04'' New Batch Passive'                         
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTBSC),AL3(0),CL8'ACRTBSC'                                 
         DC    CL(L'RECNAME)'X''06'' Billing Source'                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTMPD),AL3(MPDK),CL8'ACRTMPD'                              
         DC    CL(L'RECNAME)'X''2F00'' Media Detail'                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTMPR),AL3(MPRK),CL8'ACRTMPR'                              
         DC    CL(L'RECNAME)'X''2F01'' Media Rules'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPAR),AL3(PARK),CL8'ACRTPAR'                              
         DC    CL(L'RECNAME)'X''19'' Adjustment Rate'                           
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTGRB),AL3(GRBK),CL8'ACRTGRB'                              
         DC    CL(L'RECNAME)'X''2C3A'' Group Bill'                              
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOAP),AL3(0),CL8'ACRTOAP'                                 
         DC    CL(L'RECNAME)'X''30'' Office/Account Passive'                    
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTANC),AL3(0),CL8'ACRTANC'                                 
         DC    CL(L'RECNAME)'X''31'' Acct/Name Change Passive'                  
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTAJN),AL3(AJNK),CL8'ACRTAJN'                              
         DC    CL(L'RECNAME)'X''2C22'' Auto Job Number'                         
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSRM),AL3(SRMK),CL8'ACRTSRM'                              
         DC    CL(L'RECNAME)'X''3F01'' Stored Request'                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPBA),AL3(PBRK),CL8'ACRTPBA'                              
         DC    CL(L'RECNAME)'X''3301'' Active Prod. Bills'                      
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPBP),AL3(0),CL8'ACRTPBP'                                 
         DC    CL(L'RECNAME)'X''3302'' Passive Prod. Bills'                     
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPTA),AL3(PTAK),CL8'ACRTPTA'                              
         DC    CL(L'RECNAME)'X''34'' Prod. Trans. Activity'                     
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTMRH),AL3(0),CL8'ACRTMRH'                                 
         DC    CL(L'RECNAME)'X''07'' Dutch Reconcile Passive'                   
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSES),AL3(SESK),CL8'ACRTSES'                              
         DC    CL(L'RECNAME)'X''2C3C'' Session Estimate'                        
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCKA),AL3(0),CL8'ACRTCKA'                                 
         DC    CL(L'RECNAME)'X''10'' Check Authorization'                       
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSRC),AL3(0),CL8'ACRTSRC'                                 
         DC    CL(L'RECNAME)'X''32'' Name Search Passive'                       
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTADV),AL3(0),CL8'ACRTADV'                                 
         DC    CL(L'RECNAME)'X''11'' Advertiser'                                
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTACP),AL3(0),CL8'ACRTACP'                                 
         DC    CL(L'RECNAME)'X''11'' Advertiser Passive'                        
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTAGR),AL3(0),CL8'ACRTAGR'                                 
         DC    CL(L'RECNAME)'X''12'' Account Group'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTAGP),AL3(0),CL8'ACRTAGP'                                 
         DC    CL(L'RECNAME)'X''13'' Account Group Passive'                     
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTRAP),AL3(0),CL8'ACRTRAP'                                 
         DC    CL(L'RECNAME)'X''14'' Activity Passive'                          
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTJCB),AL3(JCBK),CL8'ACRTJCB'                              
         DC    CL(L'RECNAME)'X''2C3E'' Job Cycle Bill'                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSTU),AL3(0),CL8'ACRTSTU'                                 
         DC    CL(L'RECNAME)'X''2C23'' Studio Type'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPRC),AL3(PRCK),CL8'ACRTPRC'                              
         DC    CL(L'RECNAME)'X''2C24'' Unit Pricing'                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSPO),AL3(SPOK),CL8'ACRTSPO'                              
         DC    CL(L'RECNAME)'X''3502'' Studio PO Passive'                       
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTAPO),AL3(APOK),CL8'ACRTAPO'                              
         DC    CL(L'RECNAME)'X''3504'' Agency PO Passive'                       
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTKWD),AL3(0),CL8'ACRTKWD'                                 
         DC    CL(L'RECNAME)'X''2D06'' Scribe Keyword'                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCASP),AL3(0),CL8'ACRTCASP'                               
         DC    CL(L'RECNAME)'X''3E0C'' Cost Calendar Passive'                   
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPID),AL3(0),CL8'ACRTPID'                                 
         DC    CL(L'RECNAME)'X''3E12'' Cost Person Id Passive'                  
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCCP),AL3(CCPK),CL8'ACRTCCP'                              
         DC    CL(L'RECNAME)'X''3E0A'' Costing Client Profile'                  
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTAPG),AL3(0),CL8'ACRTAPG'                                 
         DC    CL(L'RECNAME)'APG'                                               
         DC    AL1(RECIFILE+RECIDIR+RECIMST)                                    
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(79),AL3(0),CL8'Not Used'                                     
         DC    CL(L'RECNAME)'Not Used'                                          
         DC    AL1(RECIDIR+RECIMST)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(80),AL3(0),CL8'Not Used'                                     
         DC    CL(L'RECNAME)'Not Used'                                          
         DC    AL1(RECIDIR+RECIMST)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTHDRA),AL3(0),CL8'ACRTHDRA'                               
         DC    CL(L'RECNAME)'ACCMST Header'                                     
         DC    AL1(RECIFILE+RECIDIR+RECIMST)                                    
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTHDRB),AL3(0),CL8'ACRTHDRB'                               
         DC    CL(L'RECNAME)'ACCARC Header'                                     
         DC    AL1(RECIFILE+RECIDIR+RECIARC)                                    
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTRLA),AL3(0),CL8'ACRTTRLA'                               
         DC    CL(L'RECNAME)'ACCMST Trailer'                                    
         DC    AL1(RECIFILE+RECIDIR+RECIMST)                                    
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTRLB),AL3(0),CL8'ACRTTRLB'                               
         DC    CL(L'RECNAME)'ACCARC Trailer'                                    
         DC    AL1(RECIFILE+RECIDIR+RECIARC)                                    
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPER),AL3(0),CL8'ACRTPER'                                 
         DC    CL(L'RECNAME)'X''0F'' Person'                                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCAH),AL3(CAHK),CL8'ACRTCAH'                              
         DC    CL(L'RECNAME)'X''3E01'' Cost Allocation Hist'                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCMT),AL3(0),CL8'ACRTCMT'                                 
         DC    CL(L'RECNAME)'X''3E02'' Cost Allocation Method'                  
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPAY),AL3(0),CL8'ACRTPAY'                                 
         DC    CL(L'RECNAME)'X''3E03'' Cost Payroll Code'                       
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPHI),AL3(PHIK),CL8'ACRTPHI'                              
         DC    CL(L'RECNAME)'X''3E05'' Cost Payroll History'                    
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCPR),AL3(CPRK),CL8'ACRTCPR'                              
         DC    CL(L'RECNAME)'X''3E07'' Cost Personal Rates'                     
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCAP),AL3(CAPK),CL8'ACRTCAP'                              
         DC    CL(L'RECNAME)'X''3E09'' Cost Profile'                            
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCAS),AL3(CASK),CL8'ACRTCAS'                              
         DC    CL(L'RECNAME)'X''3E0B'' Cost Calendar '                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSTD),AL3(STDK),CL8'ACRTSTD'                              
         DC    CL(L'RECNAME)'X''3E0D'' Cost Standard Hours'                     
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPBC),AL3(0),CL8'ACRTPBC'                                 
         DC    CL(L'RECNAME)'X''3300'' Prod. Bill Control'                      
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTIM),AL3(0),CL8'ACRTTIM'                                 
         DC    CL(L'RECNAME)'Time Management   '                                
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTTH),AL3(TTHK),CL8'ACRTTTH'                              
         DC    CL(L'RECNAME)'X''3E0E'' Time Total         '                     
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTSL),AL3(TSLK),CL8'ACRTTSL'                              
         DC    CL(L'RECNAME)'X''2C42'' Time Sheet List'                         
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTGIN),AL3(0),CL8'ACRTGIN'                                 
         DC    CL(L'RECNAME)'X''1E'' Group Invoice'                             
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTBDP),AL3(XXXK),CL8'ACRTBDP'                              
         DC    CL(L'RECNAME)'X''2B'' Bill/Debtor Passive'                       
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTSW),AL3(TSWK),CL8'ACRTTSW'                              
         DC    CL(L'RECNAME)'X''3E0F'' Timesheet Wkly Passive'                  
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTEDT),AL3(EDTK),CL8'ACRTEDT'                              
         DC    CL(L'RECNAME)'X''3E10'' Edit Hours'                              
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTORES),AL3(0),CL8'ACRTORES'                               
         DC    CL(L'RECNAME)'X''22'' Order Reservation'                         
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTSSAV),AL3(TSAVK),CL8'ACRTSSAV'                           
         DC    CL(L'RECNAME)'X''3E11'' Timesheet Save Record '                  
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTPOX),AL3(TPOXK),CL8'ACRTTPOX'                           
         DC    CL(L'RECNAME)'X''3E13'' Timesheet Tempo X-Ref'                   
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCONT),AL3(0),CL8'ACRTCONT'                               
         DC    CL(L'RECNAME)'X''3D01'' Account Contract Record'                 
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTCONP),AL3(0),CL8'ACRTCONP'                               
         DC    CL(L'RECNAME)'X''3D02'' Account Contract Passive'                
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTINV),AL3(0),CL8'ACRTINV'                                 
         DC    CL(L'RECNAME)'X''02'' Invoice# Passive'                          
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTMOS),AL3(0),CL8'ACRTMOS'                                 
         DC    CL(L'RECNAME)'X''15'' Mos Passive'                               
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTPCRT),AL3(0),CL8'ACRTPCRT'                               
         DC    CL(L'RECNAME)'X''29'' Charge Rate (TMS)'                         
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTRSF),AL3(0),CL8'ACRTRSF'                                 
         DC    CL(L'RECNAME)'Record Name Filter Value'                          
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTOCO),AL3(0),CL8'ACRTOCO'                                 
         DC    CL(L'RECNAME)'Override control'                                  
         DC    AL1(RECIMST+RECIDIR)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTTCP),AL3(0),CL8'ACRTTCP'                                 
         DC    CL(L'RECNAME)'Trans./Contra Passive'                             
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    AL1(ACRTGBC),AL3(0),CL8'ACRTGBC'                                 
         DC    CL(L'RECNAME)'German Bank/Client Ptr.'                           
         DC    AL1(RECIDIR+RECIPAS)                                             
         DC    AL4(0)                                                           
         DC    XL17'00'                                                         
*                                                                               
         DC    X'FF'                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
*  FIELD TABLE                                                        *         
***********************************************************************         
                                                                                
         ENTRY XXXK                   UNKNOW                                    
XXXK     DS    0F                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    AL1(0)                  DISP. MEDIA CODE                         
         DC    AL1(0)                  DISP. WORK CODE                          
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     PRODUCTION MEDIA                          
PMDK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(PMDKMED-PMDRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(0)                  WORK CODE                                
         DC    AL1(0)                  CLIENT CODE                              
         DC    AL1(0)                  JOB CODE                                 
*                                                                               
*                                     WORKCODE                                  
WCOK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(WCOKWRK-WCORECD)    DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     BUDGET                                    
BUDK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(BUDKUNT-BUDRECD)    DISP. TO U/L                             
         DC    AL1(BUDKACT-BUDRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(BUDKCUNT-BUDRECD)   DISP. TO 2ND U/L                         
         DC    AL1(BUDKCACT-BUDRECD)   DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(BUDKCCPY-BUDRECD)   DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. MEDIA CODE                         
         DC    AL1(0)                  DISP. WORK CODE                          
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     PRODUCTION CHARGE RATE                    
PCHK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(PCHKOFF-PCHRECD)    DISP. TO OFFICE                          
         DC    AL1(PCHKDOF-PCHRECD)    DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    all ACCOUNT                              
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(PCHKTSK-PCHRECD)    TASK/WORK CODE                           
         DC    AL1(PCHKCLI-PCHRECD)    CLIENT CODE                              
         DC    AL1(0)                  JOB                                      
*                                                                               
*                                     GERMAN SPLIT BILLING                      
GSBK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(GSBKRUNT-GSBRECD)   DISP. TO U/L                             
         DC    AL1(GSBKRACT-GSBRECD)   DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    X'00'                   SPLIT 1R ACCOUNT                         
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  TASK/WORK CODE                           
         DC    AL1(GSBKCLI-GSBRECD)    CLIENT CODE                              
         DC    AL1(0)                  JOB                                      
*                                                                               
*                                     OFFICE                                    
OGROK    DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(OGRKOFC-OGRRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     USER FIELD                                
UFSK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(UFSKOFC-UFSRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(UFSKMED-UFSRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(UFSKCLI-UFSRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(UFSKJOB-UFSRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     PRODUCTION ORDERS                         
POPK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(POPKOFC-POPRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(POPKMED-POPRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(POPKWRK-POPRECD)    DISP. TO WORKCODE                        
         DC    AL1(POPKCLI-POPRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(POPKJOB-POPRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     ESTIMATE VERSION                          
EVEK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(EVEKWC-EVERECD)     DISP. TO WORKCODE                        
         DC    AL1(EVEKCLI-EVERECD)    DISP. TO CLIENT CODE                     
         DC    AL1(EVEKJOB-EVERECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     PRODUCTION TEXT                           
TXTK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(TXTKOFC-TXTRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(TXTKMED-TXTRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(TXTKWRK-TXTRECD)    DISP. TO WORKCODE                        
         DC    AL1(TXTKCLI-TXTRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(TXTKJOB-TXTRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     INTERAGENCY                               
INTK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(INTKUNT-INTRECD)    DISP. TO U/L                             
         DC    AL1(INTKACT-INTRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(INTKCLT-INTRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB CODE                        
*                                                                               
*                                     INTERCOMPAY DJ                            
IDJK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(IDJKUNT-IDJRECD)    DISP. TO U/L                             
         DC    AL1(IDJKACT-IDJRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(IDJKCLT-IDJRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB CODE                        
*                                                                               
*                                     PRODUCTION DATE SCHEME                    
DATK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(DATKUNT-DATRECD)    DISP. TO U/L                             
         DC    AL1(DATKACT-DATRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     TAX/GST RULES                             
TAXK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(TAXKOFF-TAXRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     MEDIA POSTING DETAIL                      
MPDK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(MPDKMED-MPDRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(MPDKCLI-MPDRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB CODE                        
*                                                                               
*                                     MEDIA POSTING RULES                       
MPRK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
*        DC    AL1(MPRKOFC-MPRRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  IGNORE MEDIA OFFICE                      
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(MPRKMED-MPRRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(MPRKCLI-MPRRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB CODE                        
*                                                                               
*                                     PRODUCTION ADJUST. RATE                   
PARK     DS    0X                                                               
         DC    C'SJ'                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(PAJKCLI-PAJRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(PAJKOFF-PAJRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    CL1'Y'                  SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(PAJKTSK-PAJRECD)    DISP. TO WORKCODE                        
         DC    AL1(PAJKCLI-PAJRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(PAJKJOB-PAJRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     GROUP BILL                                
GRBK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(GRBKCLI-GRBRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB CODE                        
*                                                                               
*                                     AUTO JOB NUMBER                           
AJNK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(AJNKOFC-AJNRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(AJNKMED-AJNRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(AJNKCLI-AJNRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB CODE                        
*                                                                               
*                                     STORED REQUEST                            
SRMK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(SRMKUNT-SRMRECD)    DISP. TO U/L                             
         DC    AL1(SRMKACT-SRMRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     PRODUCTION BILL                           
PBRK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(PBRKJOB-PBRRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(PBRKJOB+6-PBRRECD)  DISP. TO JOB CODE                        
*                                                                               
*                                     PRODUCTION TRNS ACTIVIY                   
PTAK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(PTAKJOB-PTARECD)    DISP. TO CLIENT CODE                     
         DC    AL1(PTAKJOB+6-PTARECD)  DISP. TO JOB CODE                        
*                                                                               
*                                     SESSION ESTIMATE                          
SESK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(SESKMED-SESRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(SESKCLI-SESRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(SESKJOB-SESRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     JOB CYCLE BILLING                         
JCBK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(JCBKCLI-JCBRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(JCBKJOB-JCBRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     UNIT PRICE                                
PRCK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(PRCKOFC-PRCRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(PRCKMED-PRCRECD)    DISP. TO MEDIA CODE                      
         DC    AL1(PRCKWRK-PRCRECD)    DISP. TO WORKCODE                        
         DC    AL1(PRCKCLI-PRCRECD)    DISP. TO CLIENT CODE                     
         DC    AL1(PRCKJOB-PRCRECD)    DISP. TO JOB CODE                        
*                                                                               
*                                     STUDIO POINTER                            
SPOK     DS    0X                                                               
         DC    C'SJ'                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(STCKSJB-STCRECD)    DISP. TO ACCOUNT                         
         DC    C'SJ'                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(STCKAJB-STCRECD)    DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     AGENCY POINTER                            
APOK     DS    0X                                                               
         DC    C'SJ'                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(SACKAJB-SACRECD)    DISP. TO ACCOUNT                         
         DC    C'SJ'                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(SACKSJB-SACRECD)    DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     CLIENT PROFILE                            
CCPK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(CCPKUNT-CCPRECD)    DISP. TO U/L                             
         DC    AL1(CCPKACT-CCPRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     COST ALLOCATION HISTORY                   
CAHK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(CAHKOFC-CAHRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     PAYROLL HISTORY                           
PHIK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(PHIKOFC-PHIRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     COST PERSONAL RATES                       
CPRK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(CPRKUNT-CPRRECD)    DISP. TO U/L                             
         DC    AL1(CPRKACT-CPRRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     COST ALLOCATION PROFILE                   
CAPK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(CAPKOFC-CAPRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     COST TIMESHEET PERIOD                     
CASK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(CASKOFC-CASRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     STANDARD HOURS                            
STDK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(STDKOFC-STDRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     TOTAL TIME HOURS                          
TTHK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(TTHKUNT-TTHRECD)    DISP. TO U/L                             
         DC    AL1(TTHKACT-TTHRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     TOTAL TIME HOURS                          
TSLK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(TSLKUNT-TSLRECD)    DISP. TO U/L                             
         DC    AL1(TSLKACT-TSLRECD)    DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     TIMESHEET WEEKLY TOTAL                    
TSWK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(TSWKULC-TSWRECD)    DISP. TO U/L                             
         DC    AL1(TSWKULC+2-TSWRECD)  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(0)                  DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    XL1'00'                 SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     EDIT HOURS                                
EDTK     DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(EDTKOFC-EDTRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     TIME SHEET SAVE                           
TSAVK    DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(TSSKODS-TSSRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
*                                                                               
*                                     TEMPO X-REF                               
TPOXK    DS    0X                                                               
         DC    C'  '                   U/L (NOT IN ACCOUNT CODE)                
         DC    AL1(0)                  DISP. TO U/L                             
         DC    AL1(0)                  DISP. TO ACCOUNT                         
         DC    C'  '                   U/L (SECOND ACCOUNT)                     
         DC    AL1(0)                  DISP. TO 2ND U/L                         
         DC    AL1(0)                  DISP. TO 2ND ACCOUNT                     
         DC    AL1(TSXKODS-TSXRECD)    DISP. TO OFFICE                          
         DC    AL1(0)                  DISP. TO 2ND OFFICE                      
         DC    AL1(0)                  DISP. TO SECOND COMPANY                  
         DC    C'Y'                    SPLIT 1R                                 
         DC    AL1(0)                  DISP. TO MEDIA CODE                      
         DC    AL1(0)                  DISP. TO WORKCODE                        
         DC    AL1(0)                  DISP. TO CLIENT CODE                     
         DC    AL1(0)                  DISP. TO JOB                             
         EJECT                                                                  
***********************************************************************         
*  TABLE TO CONTROL PARSING 1R ACCOUNTS                               *         
***********************************************************************         
                                                                                
         ENTRY PERT                                                             
PERT     DS    0XL5                     SPLIT PERSON KEY                        
         DC    AL1(ACRTPCR)             PRODUCTION CHARGE RATE                  
         DC    AL1(PCHKDOF-PCHRECD)     OFFICE                                  
         DC    AL1(PCHKDEP-PCHRECD)     DEPT                                    
         DC    AL1(PCHKSUB-PCHRECD)     SUB-DEPT                                
         DC    AL1(PCHKSTF-PCHRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTPAR)             PRODUCTION ADJ. RATE                    
         DC    AL1(PAJKDOF-PAJRECD)     OFFICE                                  
         DC    AL1(PAJKDEP-PAJRECD)     DEPT                                    
         DC    AL1(PAJKSUB-PAJRECD)     SUB-DEPT                                
         DC    AL1(PAJKSTF-PAJRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTPHI)             PAYROLL HISTORY                         
         DC    AL1(PHIKOFC-PHIRECD)     OFFICE                                  
         DC    AL1(PHIKDPT-PHIRECD)     DEPT                                    
         DC    AL1(PHIKSBD-PHIRECD)     SUB-DEPT                                
         DC    AL1(PHIKPER-PHIRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTSTD)             STANDARD HOURS                          
         DC    AL1(STDKOFC-STDRECD)     OFFICE                                  
         DC    AL1(STDKDPT-STDRECD)     DEPT                                    
         DC    AL1(STDKSBD-STDRECD)     SUB-DEPT                                
         DC    AL1(STDKPER-STDRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTCAP)             COST ALLOCATION PROFILE                 
         DC    AL1(CAPKOFC-CAPRECD)     OFFICE                                  
         DC    AL1(CAPKDPT-CAPRECD)     DEPT                                    
         DC    AL1(CAPKSDT-CAPRECD)     SUB-DEPT                                
         DC    AL1(CAPKPER-CAPRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTEDT)             EDIT HOURS                              
         DC    AL1(EDTKOFC-EDTRECD)     OFFICE                                  
         DC    AL1(EDTKDPT-EDTRECD)     DEPT                                    
         DC    AL1(EDTKSBD-EDTRECD)     SUB-DEPT                                
         DC    AL1(EDTKPER-EDTRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTSSAV)            TIME SHEET SAVE                         
         DC    AL1(TSSKODS-TSSRECD)     OFFICE                                  
         DC    AL1(0)                   DEPT                                    
         DC    AL1(0)                   SUB-DEPT                                
         DC    AL1(TSSKPER-TSSRECD)     STAFF                                   
*                                                                               
         DC    AL1(ACRTTPOX)            TEMP X-REF                              
         DC    AL1(TSXKODS-TSXRECD)     OFFICE                                  
         DC    AL1(0)                   DEPT                                    
         DC    AL1(0)                   SUB-DEPT                                
         DC    AL1(TSXKPER-TSXRECD)     STAFF                                   
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DCB'S / BUFFALO                                                     *         
***********************************************************************         
                                                                                
* INPUT CONVERSION FILES                                                        
         ENTRY CINP                                                             
CINP     DCB   DDNAME=CINP,DSORG=PS,MACRF=(GM),EODAD=OPINF5                     
* DCB USED TO REFRESH INPUT DCB                                                 
         ENTRY XINP                                                             
XINP     DS    0C                                                               
         DCB   DDNAME=XXXX,DSORG=PS,MACRF=(GM),EODAD=OPINF5                     
XINPLNQ  EQU   *-XINP                                                           
                                                                                
* INPUT ACCOUNT FILE                                                            
         ENTRY TINT                                                             
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=LAST,             *        
               RECFM=VB,LRECL=2048                                              
                                                                                
* OUTPUT ACCOUNT FILE (NO CHANGES)                                              
         ENTRY TOUT                                                             
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
                                                                                
* OUTPUT ACCOUNT FILE (TRANSACTIONS & SPECIAL)                                  
         ENTRY TTRN                                                             
TTRN     DCB   DDNAME=TTRN,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
                                                                                
* OUTPUT ACCOUNT FILE (ACCOUNT & HISTORIES)                                     
         ENTRY TACC                                                             
TACC     DCB   DDNAME=TACC,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
                                                                                
* OUTPUT KEY CHANGE FILE                                                        
         ENTRY TKEY                                                             
TKEY     DCB   DDNAME=TKEY,DSORG=PS,MACRF=(PM),                        *        
               RECFM=FB,LRECL=130,BLKSIZE=32500                                 
*                                                                               
         BUFF  LINES=500,ROWS=1,COLUMNS=3,FLAVOR=B,KEYLIST=(16,A)               
          EJECT                                                                 
***********************************************************************         
* IO AREA'S & TABLES                                                  *         
***********************************************************************         
         ENTRY INPL,INP                                                         
INPL     DC    F'0'                                                             
INP      DS    XL2004                                                           
*                                                                               
         ENTRY OUTL,OUT                                                         
OUTL     DC    F'0'                                                             
OUT      DS    XL2004                                                           
*                                                                               
         ENTRY IO3L,IO3                                                         
IO3L     DC    F'0'                                                             
IO3      DS    XL2004                                                           
*                                                                               
         ENTRY CHDRL,CHDR                                                       
CHDRL    DC    F'0'                                                             
CHDR     DS    XL2004                                                           
*                                                                               
         ENTRY LGRTABL                                                          
LGRTABL  DC    (100)XL(LGRLNQ)'00'                                              
*                                                                               
                                                                                
         ENTRY CHDTABL                                                          
CHDTABL  DC    (OFATMAXN)XL(L'CHDKOFF)'00'                                      
*                                                                               
         ENTRY OFATABL                                                          
OFATABL  DC    AL1(EOT)                                                         
         DC    (OFATMAXN)XL(OFATABLN)'00'                                       
*                                                                               
         ENTRY HCARD                                                            
MXHCRD   EQU   10                                                               
HCARD    DC    (MXHCRD)CL80' ' HOLD SOME CARDS TILL PHASES ARE LOADED           
*                                                                               
         ENTRY HSCAN                                                            
MXHSCN   EQU   10                                                               
HSCAN    DC    (MXHCRD)CL(SCANLNQ)' ' HOLD SOME SCANNER FORMATS                 
         EJECT                                                                  
         ENTRY SSB                                                              
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOSTAT2                                                         
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR STORAGE                                                   *         
***********************************************************************         
                                                                                
       ++INCLUDE ACNVDSECT                                                      
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DCBD                                                                          
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043ACNV00X   09/12/00'                                      
         END                                                                    
