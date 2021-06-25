*          DATA SET ACREPCQ02  AT LEVEL 040 AS OF 11/06/13                      
*PHASE ACCQ02A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE CHOPCON                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE GETCAP                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
* DCUR LVL 34 - CHANGED THE CALL TO EDPACK IN BUILDBUF ROUTINE.  WAS            
*               PASSING HARDCODED LENGTH OF 5 WHEN SHOULD HAVE BEEN             
*               USING L'DUB.                                                    
*                                                                               
         TITLE 'SALARY INTERFACE TAPE'                                          
ACCQ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCQ**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACCP02D,RC          RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVI   BIT,0                                                            
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
         MVI   ELEMBIT,0                                                        
*                                                                               
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
*              READ PAYROLL CODE RECORD / BUILD PAYROLL CODE TABLE              
*                                                                               
         USING PYCODED,R5                                                       
RUNF10   L     R5,APYTABLE                                                      
*                                                                               
         USING PAYRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    X'3E'                                        
         MVI   PAYKSUB,PAYKSUBQ    X'03'                                        
         MVC   PAYKCPY,RCCOMPFL                                                 
         MVI   PAYKSEQ,0                                                        
*                                                                               
RUNF100  MVC   SVKEY,0(R4)                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BO    RUNF200                                                          
         CLC   PAYKEY(PAYKSEQ-PAYKEY),SVKEY                                     
         BNE   RUNF200                                                          
*                                                                               
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
RUNF125  CLI   0(R4),0                                                          
         BE    RUNF175                                                          
         CLI   0(R4),PAYELQ        X'84' ELEMENT                                
         BE    *+16                                                             
RUNF150  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     RUNF125                                                          
*                                                                               
         USING PAYELD,R4                                                        
         CLI   PAYNUM,MAXPCODE                                                  
         BNH   *+6                                                              
         DC    H'0'                MUST EXPAND TABLE                            
         MVC   PYNUM,PAYNUM        NUMBER (SYSTEM ASSIGNED)                     
         MVC   PYCODE,PAYCODE      PAYROLL CODE                                 
         MVC   PYDESC,PAYDESC      DESCRIPTION                                  
         MVC   PYREV,PAYREV        REVERSAL                                     
         MVC   PYNUMPC1,PAYPC1     PC1 PAY NUMBER                               
         MVC   PYCODPC1,SPACES                                                  
         MVC   PYNUMPC2,PAYPC2     PC2 PAY NUMBER                               
         MVC   PYCODPC2,SPACES                                                  
         MVC   PYSTAT,PAYSTAT      STATUS                                       
         TM    PYSTAT,PAYADJRT    IS AN ADJ RATE SET UP                         
         BZ    *+8                                                              
         OI    BIT,ADJRATE                                                      
         LA    R5,PYLNQ(R5)        NEXT TABLE ENTRY                             
         B     RUNF150                                                          
*                                                                               
         USING PAYRECD,R4                                                       
RUNF175  L     R4,AIO1             CHECK FOR LINKED RECORDS                     
         MVC   PAYKEY,SVKEY                                                     
         ZIC   R1,PAYKSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,PAYKSEQ                                                       
         B     RUNF100                                                          
*                                                                               
RUNF200  MVI   0(R5),X'FF'         MARK END OF PAYCODE TABLE                    
*                                                                               
         USING PYCODED,R5                                                       
         L     R5,APYTABLE         GO BACK FOR PC1/PC2 CODES                    
RUNF210  CLI   PYNUM,X'FF'                                                      
         BE    RUNF240                                                          
         CLI   PYNUMPC1,0          NEED TO LOOKUP CODE?                         
         BH    RUNF215                                                          
         CLI   PYNUMPC2,0                                                       
         BE    *+8                                                              
RUNF215  BAS   RE,RUNF230          YES- READ TABLE FOR CODE                     
         LA    R5,PYLNQ(R5)                                                     
         B     RUNF210                                                          
*                                                                               
*                                                                               
RUNF230  DS    0H                  REREAD TABLE FOR PCT PAY TYPES               
         L     RF,APYTABLE                                                      
RUNF232  CLI   0(RF),X'FF'                                                      
         BER   RE                                                               
         CLC   PYNUM-PYCODED(L'PYNUM,RF),PYNUMPC1                               
         BNE   *+10                                                             
         MVC   PYCODPC1,PYCODE-PYCODED(RF)   SAVE CODE IN TABLE                 
         CLC   PYNUM-PYCODED(L'PYNUM,RF),PYNUMPC2                               
         BNE   *+10                                                             
         MVC   PYCODPC2,PYCODE-PYCODED(RF)                                      
         LA    RF,PYLNQ(RF)                                                     
         B     RUNF232                                                          
*                                                                               
RUNF240  DS    0H                                                               
         L     RE,ACOBLOCK         CLEAR THE COBLOCK                            
         L     RF,=A(COBLOCKX-COBLOCK)                                          
         XCEFL                                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
*                                                                               
REQF     GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         GOTO1 DATCON,DMCB,(5,TODAYP),(1,TODAYP)                                
*                                                                               
         CLI   PROGPROF+4,C'N'     SORT BY DEFAULT METHOD?                      
         BE    REQF100                                                          
         CLI   PROGPROF+4,C'0'     SORT BY DEFAULT METHOD?                      
         BE    REQF100                                                          
         CLI   PROGPROF+4,C' '     SORT BY DEFAULT METHOD?                      
         BE    REQF100                                                          
         CLI   PROGPROF+4,0        SORT BY DEFAULT METHOD?                      
         BE    REQF100                                                          
         OI    BIT,SORTPROF        THEN SORTING BY NAME                         
         B     REQF100                                                          
*                                                                               
REQF100  GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         USING ACCOMPD,R4                                                       
REQF110  L     R4,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         MVC   CMPNAME,SPACES                                                   
         L     R4,ADCMPNAM                                                      
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
         EX    R1,*-6                                                           
*                                                                               
         L     R4,ACOBLOCK         INITIALIZE COBLOCK FOR METHOD                
         USING COBLOCKD,R4                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(8),COBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER           
         LA    RE,COBLOCK          CLEAR THE BLOCK                              
         L     RF,=A(COBLOCKX-COBLOCK)                                          
         XCEFL                                                                  
         MVC   COBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                   
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,RCCOMPFL     COMPANY                                      
         MVI   COKMTHD,X'40'       COSTING METHOD NOT NEEDED                    
         MVC   COACOVL,COVAIL      ADDR OF COVAIL                               
         MVC   COABINSR,BINSRCH    ADDR OF BINSRCH                              
         MVC   COADM,DATAMGR       ADDR OF DATAMGR                              
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
LDGF     DS    0H                                                               
         USING ACHEIRD,R4                                                       
         L     R4,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVC   LEVELA,ACHRLEVA     LEVEL LENGTHS                                
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVC   LEVELD,ACHRLEVD                                                  
         MVC   LEVELANM,ACHRDESA   LEVEL NAMES                                  
         MVC   LEVELBNM,ACHRDESB                                                
         MVC   LEVELCNM,ACHRDESC                                                
         MVC   LEVELDNM,ACHRDESD                                                
         XC    SVACT,SVACT         CLEAR SAVE ACCOUNT                           
         DROP  R4                                                               
*                                                                               
         USING LDGELD,R4                                                        
         TM    LDGOPOS,X'40'       ON 2 BYTE OFFICES?                           
         BO    *+8                                                              
         OI    BIT,ONEBYTOF                                                     
*                                                                               
         LA    RF,COUNTERS         CLEAR COUNTERS                               
         LA    R0,COUNT#                                                        
         ZAP   0(L'COUNTERS,RF),=P'0'                                           
         LA    RF,L'COUNTERS(RF)                                                
         BCT   R0,*-10                                                          
*                                                                               
         OPEN  (INTAPE,(INPUT))    OPEN INPUT TAPE                              
         USING TAPED,R3                                                         
         L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
         GET   INTAPE,(R3)                                                      
         CLC   TAPEREC+10(L'TAPEREC-11),SPACES     CHECK FOR HEADER             
         BNE   LDGF100A                                                         
*                                                                               
LDGF100  L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
         GET   INTAPE,(R3)                                                      
LDGF100A MVI   TAPESTAT,0                                                       
         GOTO1 =A(CONVERS),DMCB,(RC)    CONVERSIONS                             
         AP    COUNTIN,=P'1'                                                    
         GOTO1 SORTER,DMCB,=C'PUT',(R3)   PUT REC TO SORTER                     
*                                                                               
         GOTO1 =A(REVERSAL),DMCB,(RC)  CHECK FOR REVERSAL CODE                  
         BE    LDGF100B                                                         
         GOTO1 =A(BENEFIT),DMCB,(RC) CHECK FOR BENEFIT PERCENTS                 
         BNE   LDGF100                                                          
LDGF100B DS    0H                                                               
*        GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         B     LDGF100                                                          
*                                                                               
LDGF200  CLOSE (INTAPE)            CLOSE INPUT TAPE                             
         XC    ALSORT,ALSORT                                                    
         GOTO1 =A(GETSORT),DMCB,(RC)   1ST SORT READ                            
         L     RF,ALSTWRK                                                       
         CLC   TAPEACT-TAPED(L'TAPEACT,RF),SPACES DO I HAVE ONE SAVED?          
         BNE   LDGF300A                                                         
         B     LDGF300A            YES - PROCESS IT                             
*                                                                               
LDGF300  OC    ALSORT,ALSORT       WAS THAT THE LAST SORT REC?                  
         BZ    LDGFX               YES - WE ARE DONE                            
         GOTO1 =A(GETSORT),DMCB,(RC)  NO - GEWT ANOTHER SORT REC                
LDGF300A GOTO1 =A(BUILDBUF),DMCB,(RC)  BUILD BUFFALO RECORD                     
         BAS   RE,VALIDATE         VALIDATE ACCOUNT LEVELS                      
         BNE   LDGF300                                                          
LDGF400  BAS   RE,HISTORY          BUILD/UPDATE PAYROLL HISTORY                 
         CLI   BUFSTAT,0           IF WE GOT SOME ERROR DON'T CHANGE            
         BNZ   *+16                                                             
         CLI   BUFSTAT2,0                                                       
         BNZ   *+8                                                              
         MVI   BUFTYPE,BUFREP1     SHOW ITEM ON REPORT                          
         GOTO1 =A(PUTBUF),DMCB,(RC)                                             
         B     LDGF300                                                          
*                                                                               
LDGFX    GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE ACCOUNT LEVELS                                *         
***********************************************************************         
*                                                                               
VALIDATE NTR1                                                                   
*                                                                               
         NI    BIT,X'FF'-NONAMES                                                
*                                                                               
*              VALIDATE OFFICE LEVEL                                            
*                                                                               
         USING ACTRECD,R4                                                       
VAL100   DS    0H                                                               
         MVI   NOMATCH,C'N'                                                     
*                                                                               
         L     R4,AIO1             R4=A(IO1 BUFFER)                             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELA                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,VALMOVE          LOOK BELOW                                   
*                                                                               
         BAS   RE,VALCMPLV                                                      
         BNE   VAL150                                                           
         MVC   BUFNMLV1,SAVNMLV1                                                
         B     VAL200                                                           
*                                                                               
VAL150   GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    BUFSTAT,BUFIOFF+BUFIDPT+BUFISDPT+BUFISTAF                        
         B     VAL510                                                           
         GOTO1 =A(GETNAME),DMCB,(RC)                                            
         MVC   BUFNMLV1,WORK       SAVE OFFICE NAME                             
         MVC   SAVNMLV1,WORK       SAVE OFFICE NAME                             
*                                                                               
*              VALIDATE DEPARTMENT LEVEL                                        
*                                                                               
VAL200   DS    0H                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELB                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,VALMOVE                                                       
*                                                                               
         CLI   NOMATCH,C'Y'                                                     
         BE    VAL250                                                           
         BAS   RE,VALCMPLV                                                      
         BNE   VAL250                                                           
         MVC   BUFNMLV2,SAVNMLV2                                                
         B     VAL300                                                           
*                                                                               
VAL250   GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    BUFSTAT,BUFIDPT+BUFISDPT+BUFISTAF                                
         B     VAL510                                                           
         GOTO1 =A(GETNAME),DMCB,(RC)                                            
         MVC   BUFNMLV2,WORK       SAVE DEPARTMENT NAME                         
         MVC   SAVNMLV2,WORK       SAVE DEPARTMENT NAME                         
*                                                                               
*              VALIDATE SUBDEPARTMENT LEVEL                                     
*                                                                               
VAL300   DS    0H                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELC                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,VALMOVE                                                       
*                                                                               
         CLI   NOMATCH,C'Y'                                                     
         BE    VAL350                                                           
         BAS   RE,VALCMPLV                                                      
         BNE   VAL350                                                           
         MVC   BUFNMLV3,SAVNMLV3                                                
         B     VAL400                                                           
*                                                                               
VAL350   GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    BUFSTAT,BUFISDPT+BUFISTAF                                        
         B     VAL510                                                           
         GOTO1 =A(GETNAME),DMCB,(RC)                                            
         MVC   BUFNMLV3,WORK       SAVE SUB-DEPARTMENT NAME                     
         MVC   SAVNMLV3,WORK       SAVE SUB-DEPARTMENT NAME                     
*                                                                               
*              VALIDATE PERSON LEVEL                                            
*                                                                               
VAL400   DS    0H                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELD                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,VALMOVE                                                       
*                                                                               
         CLI   NOMATCH,C'Y'                                                     
         BE    VAL450                                                           
         BAS   RE,VALCMPLV                                                      
         BNE   VAL450                                                           
         B     VAL510                                                           
*                                                                               
VAL450   GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+8                                                              
         OI    BUFSTAT,BUFISTAF                                                 
*                                                                               
VAL500   DS    0H                                                               
         CLI   BUFSTAT,0                                                        
         BE    VAL502                                                           
         OI    BIT,NONAMES         1R HAS ERRORS-CANNOT UPDATE NAME             
         MVI   BUFTYPE,BUFERROR    SHOW ON ERROR REPORT                         
         MVI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         OI    BUFSTAT,BUFISTAF    INVALID STAFF                                
         B     VAL510                                                           
*                                                                               
VAL502   MVI   RECORD,RECSTAFF                                                  
         BAS   RE,PROCREC          UPDATE ACC RECORDS                           
*                                                                               
*              VALIDATE PERSON RECORD                                           
*                                                                               
         USING PERRECD,R4                                                       
VAL510   L     R4,AIO1                                                          
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,QCOMPANY                                                 
         ZIC   R1,LEVELD                                                        
         ZIC   R0,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   PERKCODE(0),BUFPERSN                                             
         EX    R1,*-6                                                           
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    VAL550                                                           
         MVI   BUFTYPE,BUFERROR    SHOW ON ERROR REPORT                         
         MVI   BUFTTYPE,BUFTITEM   ITEM REPORT                                  
         OI    BUFSTAT,BUFIPERS                                                 
         MVI   NOMATCH,C'Y'        ERRORS                                       
         B     VAL600                                                           
*                                                                               
VAL550   GOTO1 =A(VALDATE),DMCB,(RC)  VALIDATE CHK DATE WITH LOCATION           
         BE    VAL560                                                           
         MVI   BUFTYPE,BUFERROR    SHOW ON ERROR REPORT                         
         MVI   BUFTTYPE,BUFTITEM   ITEM REPORT                                  
         TM    BUFSTAT,BUFICKDT    CHECK DATE ERROR                             
         BO    *+8                 NOT PERSON DATE ERROR                        
         OI    BUFSTAT,BUFIDATE                                                 
         MVI   NOMATCH,C'Y'        ERRORS                                       
         B     VAL600                                                           
*                                                                               
VAL560   DS    0H                                                               
         TM    BIT,NONAMES        CAN I UPDATE THE NAME?                        
         BO    VAL600              NO                                           
         CLI   BUFSTAT,0                                                        
         BNE   VAL600              CANNOT PROCESS-ERRORS                        
         CLI   BUFSTAT2,0                                                       
         BNE   VAL600                                                           
*                                                                               
VAL570   MVI   RECORD,RECPERSN                                                  
         BAS   RE,PROCREC                                                       
*                                                                               
*              VALIDATE PAYROLL CODE                                            
*                                                                               
         USING PYCODED,R5                                                       
VAL600   L     R5,APYTABLE                                                      
VAL610   OC    BUFCODE,SPACES      MAKE SURE CODE IS UPPER CASE                 
         CLC   PYCODE,BUFCODE      PAYROLL CODE                                 
         BNE   VAL620                                                           
*                                                                               
         TM    BIT,OUTLOCDT        ARE WE OUTSIDE THE LOC DATE?                 
         BZ    VAL615                                                           
         TM    PYSTAT,PYADJRTE     THEN CAN ONLY ACCEPT ADJ PAYRATE             
         BZ    *+12                                                             
         MVI   BUFPSTAT,PDESADJ                                                 
         B     VAL615                                                           
         OI    BUFSTAT,BUFICODE                                                 
         MVI   NOMATCH,C'Y'                                                     
         B     VALX                                                             
*                                                                               
VAL615   MVC   BUFNUM,PYNUM        PAYROLL CODE NUMBER                          
         MVC   BUFDESC,PYDESC      PAYROLL CODE DESCRIPTION                     
         TM    PYSTAT,PYHRTE                                                    
         BZ    *+8                                                              
         MVI   BUFPSTAT,PDESHRTE   PAYROLL STATUS (HRLY RATE?)                  
         B     VALX                                                             
VAL620   CLI   0(R5),X'FF'                                                      
         BE    *+12                                                             
         LA    R5,PYLNQ(R5)        NEXT TABLE ENTRY                             
         B     VAL610                                                           
         OI    BUFSTAT,BUFICODE                                                 
         MVI   NOMATCH,C'Y'                                                     
*                                                                               
VALX     DS    0H                                                               
         CLI   NOMATCH,C'Y'        NO MATCHES                                   
         BE    *+10                                                             
         MVC   BUFSTAT,SAVSTAT                                                  
*                                                                               
         MVC   SAVSTAT,BUFSTAT                                                  
         CLI   BUFSTAT,0                                                        
         BNE   *+12                                                             
         CLI   BUFSTAT2,0                                                       
         BE    VALX1                                                            
         MVC   MYKEY,SPACES                                                     
         MVC   MYPERSN,SPACES                                                   
         GOTO1 =A(PUTBUF),DMCB,(RC)                                             
         CLI   BUFSTAT,0                                                        
         BNE   VALX1                                                            
         CLI   BUFSTAT2,0                                                       
         BNE   VALX2                                                            
VALX1    CLI   BUFSTAT,0           SET CONDITION FLAG                           
         B     EXIT                                                             
VALX2    CLI   BUFSTAT2,0          SET CONDITION FLAG                           
         B     EXIT                                                             
*                                                                               
         USING ACTRECD,R4                                                       
VALCMPLV LA    R1,3(R1)            COMP + C'1R'                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   MYKEY(0),ACTKEY                                                  
         BNE   VALCMP5                                                          
         CR    RE,RE                                                            
         BR    RE                                                               
VALCMP5  MVC   MYKEY(3),ACTKEY                                                  
         MVC   MYKEY+3(38),BUFACT                                               
         MVI   NOMATCH,C'Y'                                                     
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
VALMOVE  MVC   ACTKACT(0),BUFACT                                                
         EJECT                                                                  
***********************************************************************         
*              PROCESS INCOMING RECORD                                *         
***********************************************************************         
*                                                                               
PROCREC  NTR1                                                                   
         MVI   ACTION,UPDATE                                                    
*                                                                               
         USING ELTABD,R1           LOOP THROUGH ELEMENT TABLE AND               
PROC400X L     R1,AELTAB           PROCESS ALL ELEMENTS FOR THIS                
         LA    R0,ELTAB#           RECORD/ACTION COMBINATION                    
PROC500  MVC   BYTE,RECORD                                                      
         NC    BYTE,ELTRECRD       MUST MATCH RECORD                            
         BZ    PROC550                                                          
         SR    RF,RF                                                            
         ICM   RF,15,ELTADDR       RF=A(ELEMENT ROUTINE)                        
         BASR  RE,RF               BRANCH TO ELEMENT ROUTINE                    
PROC550  LA    R1,L'ELTAB(R1)                                                   
         BCT   R0,PROC500                                                       
*                                                                               
         CLI   QOPT1,C'D'          OPT1=D --> DUMP RECORDS                      
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
*                                                                               
         CLI   RCWRITE,C'N'        ACTION=UPDATE --> WRITE RECORD BACK          
         BE    PROCX                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,AIO1,AIO1                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROCX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS PAYROLL HISTORY RECORDS                        *         
***********************************************************************         
*                                                                               
HISTORY  NTR1                                                                   
         GOTO1 =A(ELEM86),DMCB,(RC)  BUILD NEW 86 ELEMENT                       
         GOTO1 =A(ELEMF1),DMCB,(RC)  BUILD F1 ACTIVITY ELEMENT                  
*                                                                               
*              READ FOR PAYROLL HISTORY RECORD                                  
*                                                                               
         USING PHIRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E'                                        
         MVI   PHIKSUB,PHIKSUBQ    X'05'                                        
         MVC   PHIKCPY,QCOMPANY                                                 
         MVC   PHIKOFC,BUFOFFC                                                  
         MVC   PHIKDPT,BUFDPT                                                   
         MVC   PHIKSBD,BUFSDPT                                                  
         MVC   PHIKPER,BUFPERSN                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,BUFCKDT),(1,DUB)                                  
         SR    R1,R1                                                            
         ICM   R1,3,DUB                                                         
         LNR   R1,R1                                                            
         STCM  R1,3,PHIKMOA        MOA (2'S COMPLIMENT)                         
         MVI   PHIKSEQ,0                                                        
*                                                                               
         MVC   SVKEY,0(R4)                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCFIL,AIO1,AIO1                     
         CLI   DMCB+8,0            REC FOUND                                    
         BE    HIST010             UPDATE                                       
         CLI   DMCB+8,2            REC FOUND IS DELETED?                        
         BNE   *+12                                                             
         NI    PHIRSTAT,X'FF'-X'80'   UNDELETE RECORD                           
         B     HIST010                                                          
*                                                                               
         BAS   RE,HISTADD             ADD NEW HISTORY AND EXIT                  
         B     EXIT                                                             
*                                                                               
HIST010  AP    COUNTUPD,=P'1'      KEEP UPDATE COUNT AS PER PSHA 4/97           
*                                                                               
*              UPDATE EXISTING PAYROLL RECORD                                   
*                                                                               
HIST100  DS    0H                  SCAN RECORD LOOKING FOR A MATCHING           
         L     R4,AIO1             HISTORY ELEMENT.  IF ONE IS FOUND            
         AH    R4,DATADISP         THEN REPLACE ELEMENT                         
HIST110  CLI   0(R4),0                                                          
         BE    HIST120                                                          
         CLI   0(R4),PDEELQ        X'86' ELEMENT                                
         BE    *+16                                                             
HIST115  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HIST110                                                          
*                                                                               
         USING PDEELD,R4                                                        
         CLC   PDEEL(PDENUM-PDEEL+1),ELEM                                       
         BNE   HIST115                                                          
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,DELEL            DELETE OLD X'86' ELEMENT                     
         LA    R5,ELEM                                                          
         BAS   RE,ADDEL            ADD NEW 86 ELEMENT                           
*                                                                               
         USING ACTVD,R4                                                         
HIST15A  L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
HIST116  CLI   0(R4),0             LOOK FOR F1 ELEM                             
         BE    HIST16A                                                          
         CLI   0(R4),X'F1'                                                      
         BE    HIST16B                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HIST116                                                          
*                                                                               
HIST16A  LA    R5,ELEM2            NOT THERE SO ADD                             
         BAS   RE,ADDEL                                                         
         B     HIST117                                                          
*                                                                               
HIST16B  MVC   SVADDATE,ACTVADDT   SAVE DATE REC ADDED                          
         MVC   SVCHANUM,ACTVCHNM   SAVE # OF LAST CHANGE                        
         TM    ACTVADFL,X'80'      SAVE IF ADD FLAG IS ON                       
         BZ    *+8                                                              
         OI    ELEMBIT,ADDFLAG                                                  
         TM    ACTVCHFL,X'80'      SAVE IF CHANGE FLAG IS ON                    
         BZ    *+8                                                              
         OI    ELEMBIT,CHAFLAG                                                  
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,DELEL              DELETE OLD F1 ELEM                         
         BAS   RE,ELF1CHA          REBUILD F1 ELEM WITH SAVED INFO              
         LA    R5,ELEM2                                                         
         BAS   RE,ADDEL            ADD REBUILT F1 ELEM                          
         DROP  R4                                                               
*                                                                               
HIST117  CLI   RCWRITE,C'N'                                                     
         BE    HIST118                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,AIO1,AIO1                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
HIST118  CLI   QOPT1,C'D'          OPT1=D --> DUMP RECORDS                      
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
         B     EXIT                                                             
*                                                                               
HIST120  LA    R1,MAXACC           AIO1 ---> AIO2                               
         L     R0,AIO1             (SAVE LAST RECORD READ)                      
         LA    RF,MAXACC                                                        
         L     RE,AIO2                                                          
         MVCL  RE,R0                                                            
*                                                                               
         USING PHIRECD,R4                                                       
         L     R4,AIO1                                                          
         ZIC   R1,PHIKSEQ          BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,PHIKSEQ                                                       
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCFIL,AIO1,AIO1                     
         CLI   DMCB+8,0                                                         
         BE    HIST100                                                          
         CLI   DMCB+8,2            REC FOUND IS DELETED?                        
         BNE   *+12                                                             
         NI    PHIRSTA,X'FF'-X'80' UNDELETE                                     
         B     HIST100                                                          
*                                                                               
         LA    R1,MAXACC           AIO2 ---> AIO1                               
         L     R0,AIO2             (RESTORE LAST READ)                          
         LA    RF,MAXACC                                                        
         L     RE,AIO1                                                          
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,ELEM             ELEM--> '86'                                 
         BAS   RE,ADDEL                                                         
         CLI   DMCB+12,0                                                        
         BE    *+18                                                             
         MVC   SVKEY,0(R4)                                                      
         BAS   RE,HISTADD                                                       
         B     EXIT                                                             
*                                                                               
         USING ACTVD,R4                                                         
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
HIST121  CLI   0(R4),0             LOOK FOR F1 ELEM                             
         BE    HIST122                                                          
         CLI   0(R4),X'F1'                                                      
         BE    HIST123                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HIST121                                                          
*                                                                               
HIST122  LA    R5,ELEM2            NOT THERE SO ADD                             
         BAS   RE,ADDEL                                                         
         B     HIST23A                                                          
*                                                                               
HIST123  MVC   SVADDATE,ACTVADDT   SAVE DATE REC ADDED                          
         MVC   SVCHANUM,ACTVCHNM   SAVE # OF LAST CHANGE                        
         TM    ACTVADFL,X'80'      SAVE IF ADD FLAG IS ON                       
         BZ    *+8                                                              
         OI    ELEMBIT,ADDFLAG                                                  
         TM    ACTVCHFL,X'80'      SAVE IF CHANGE FLAG IS ON                    
         BZ    *+8                                                              
         OI    ELEMBIT,CHAFLAG                                                  
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,DELEL              DELETE OLD F1 ELEM                         
         BAS   RE,ELF1CHA          REBUILD F1 ELEM WITH SAVED INFO              
         LA    R5,ELEM2                                                         
         BAS   RE,ADDEL            ADD REBUILT F1 ELEM                          
         DROP  R4                                                               
*                                                                               
HIST23A  CLI   RCWRITE,C'N'                                                     
         BE    HIST124                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,AIO1,AIO1                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
HIST124  CLI   QOPT1,C'D'          OPT1=D --> DUMP RECORDS                      
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ADD NEW PAYROLL HISTORY RECORD                         *         
***********************************************************************         
*                                                                               
HISTADD  NTR1                                                                   
*                                                                               
         L     R4,AIO1                                                          
         BAS   RE,XCIO                                                          
         MVC   0(L'SVKEY,R4),SVKEY                                              
         SR    R1,R1                                                            
         ICM   R1,3,DATADISP                                                    
         STCM  R1,3,ACCORLEN(R4)                                                
         LA    R5,ELEM             ADD '86' ELEM                                
         BAS   RE,ADDEL                                                         
         LA    R5,ELEM2            ADD 'F1' ELEM                                
         BAS   RE,ADDEL                                                         
         AP    COUNTADD,=P'1'                                                   
         CLI   RCWRITE,C'N'                                                     
         BE    HISTA02                                                          
         GOTO1 DATAMGR,DMCB,DMADD,ACCFIL,AIO1,AIO1                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
HISTA02  CLI   QOPT1,C'D'          OPT1=D --> DUMP RECORDS                      
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              BUILD NAME ELEMENT - X'20'                             *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW NAME ELEMENT                         
*              ACTION=UPDATE --> DELETE OLD NAME ELEMENT & ADD NEW ONE          
*                                                                               
ELEM20   NTR1                                                                   
         USING NAMELD,R5                                                        
         CLI   PROGPROF,C'Y'       UPDATE NAMES PROFILE SET?                    
         BNE   EXIT                                                             
*                                                                               
         CLC   BUFLNAM,SPACES      MAKE SURE BOTH LAST AND FIRST NAMES          
         BE    *+14                EXIST                                        
         CLC   BUFFNAM,SPACES                                                   
         BNE   EL2010                                                           
         MVI   BUFTYPE,BUFERROR    SHOW ON ERROR REPORT                         
         MVI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         OI    BUFSTAT2,BUFINAME   INVALID STAFF                                
         OI    BIT,ERRNAME                                                      
         B     EXIT                                                             
*                                                                               
EL2010   CLI   ACTION,UPDATE                                                    
         BNE   *+12                                                             
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,DELEL            DELETE OLD NAME ELEMENT                      
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ        X'20' ELEMENT                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         OC    WORK(L'BUFLNAM),SPACES                                           
*                                                                               
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
*                                                                               
         MVC   WORK+L'BUFLNAM(L'BUFFNAM+L'BUFMNAM),BUFFNAM                      
         OC    WORK+L'BUFLNAM(L'BUFFNAM+L'BUFMNAM),SPACES                       
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         MVC   NAMEREC(0),WORK                                                  
         EX    R1,*-6                                                           
         SR    R3,R3                                                            
         ICM   R3,15,DMCB+4        LENGTH OF NAME                               
         LA    R4,NAMEREC                                                       
         BAS   RE,UPCASE                                                        
         ICM   R1,15,DMCB+4                                                     
         LA    R1,NAMLN1Q(R1)                                                   
         STC   R1,NAMLN                                                         
         LA    R5,ELEM                                                          
         BAS   RE,ADDEL            ADD NEW NAME ELEMENT TO RECORD               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD NAME ELEMENT - X'5A'                             *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW NAME ELEMENT                         
*              ACTION=UPDATE --> DELETE OLD NAME ELEMENTS AND THEN              
*                                ADD 2 NEW ONES (FIRST, LAST)                   
*                                                                               
ELEM5A   NTR1                                                                   
         USING GPNELD,R5                                                        
         CLI   PROGPROF,C'Y'       UPDATE NAMES PROFILE SET?                    
         BNE   EXIT                                                             
*                                                                               
         TM    BIT,ERRNAME         WAS FIRST AND LAST NAMES NOT FOUND           
         BO    EXIT                IN ELEM20 ROUTINE?                           
         CLC   BUFLNAM,SPACES      MAKE SURE BOTH LAST AND FIRST NAMES          
         BE    *+14                EXIST                                        
         CLC   BUFFNAM,SPACES                                                   
         BNE   EL5A10                                                           
         MVI   BUFTYPE,BUFERROR    SHOW ON ERROR REPORT                         
         MVI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         OI    BUFSTAT2,BUFINAME   INVALID STAFF                                
         OI    BIT,ERRNAME                                                      
         B     EXIT                                                             
*                                                                               
EL5A10   CLI   ACTION,UPDATE                                                    
         BNE   *+12                                                             
         MVI   ELCODE,GPNELQ                                                    
         BAS   RE,DELEL            DELETE OLD NAME ELEMENT                      
*                                                                               
*              ADD NAME ELEMENT - (LAST NAME)                                   
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ        X'5A' ELEMENT                                
         MVI   GPNTYP,GPNTLST      LAST NAME                                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         OC    WORK(L'BUFLNAM),SPACES                                           
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         MVC   GPNNME(0),WORK                                                   
         EX    R1,*-6                                                           
         SR    R3,R3                                                            
         ICM   R3,15,DMCB+4        LENGTH OF NAME                               
         LA    R4,GPNNME                                                        
         BAS   RE,UPCASE                                                        
         ICM   R1,15,DMCB+4                                                     
         LA    R1,GPNLNQ(R1)                                                    
         STC   R1,GPNLN                                                         
         LA    R5,ELEM                                                          
         BAS   RE,ADDEL                                                         
*                                                                               
*              ADD NAME ELEMENT - (FIRST NAME & M.I.)                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ        X'5A' ELEMENT                                
         MVI   GPNTYP,GPNTFST      FIRST NAME                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFFNAM+L'BUFMNAM),BUFFNAM                                
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         MVC   GPNNME(0),WORK                                                   
         EX    R1,*-6                                                           
         SR    R3,R3                                                            
         ICM   R3,15,DMCB+4        LENGTH OF NAME                               
         LA    R4,GPNNME                                                        
         BAS   RE,UPCASE                                                        
         ICM   R1,15,DMCB+4                                                     
         LA    R1,GPNLNQ(R1)                                                    
         STC   R1,GPNLN                                                         
         LA    R5,ELEM                                                          
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CHANGE FIELD AT R4 TO UPPERCASE FOR LEN IN R3                          
***********************************************************************         
*                                                                               
UPCASE   NTR1                                                                   
UP10     CLI   0(R4),X'81'                                                      
         BL    UP20                                                             
         CLI   0(R4),X'A9'                                                      
         BH    UP20                                                             
         OI    0(R4),X'40'         MAKE UPPER CASE                              
UP20     LA    R4,1(R4)                                                         
         BCT   R3,UP10                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REBUILD F1 ELEM                                        *         
***********************************************************************         
*                                                                               
ELF1CHA  NTR1                                                                   
         USING ACTVD,R5                                                         
         LA    R5,ELEM2                                                         
         XC    ELEM2,ELEM2                                                      
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,ACTVLENQ                                                 
         MVC   ACTVADDT,SVADDATE   SAVED ADD DATE                               
         TM    ELEMBIT,ADDFLAG     IS FLAG FOR ADD ON?                          
         BZ    *+8                                                              
         OI    ACTVADFL,X'80'      THEN TURN ON IN ELEM                         
         GOTO1 DATCON,DMCB,(5,0),(3,ACTVCHDT) DATE LAST CHA                     
         TM    ELEMBIT,CHAFLAG     IS FLAG FOR CHA ON?                          
         BZ    *+8                                                              
         OI    ACTVCHFL,X'80'      THEN TURN ON IN ELEM                         
         ZIC   R1,SVCHANUM         BUMP # OF LAST CHANGE                        
         LA    R1,1(R1)                                                         
         STC   R1,SVCHANUM                                                      
         MVC   ACTVCHNM,SVCHANUM   # OF LAST CHANGE                             
         MVC   ACTVSCID,=C'**TAPE**'      FROM SALARY TAPE                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              ADD ELEMENT TO RECORD IN AIO1                          *         
*              NTRY--R5 POINTS TO ELEM (OR ELEM2)                     *         
***********************************************************************         
*                                                                               
ADDEL    NTR1                                                                   
         L     R4,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCBIG),(R4),(R5)                               
         B     EXIT                                                             
         SPACE 3                                                                
***********************************************************************         
*              DELETE ELEMENT IN AIO1                                 *         
***********************************************************************         
*                                                                               
DELEL    NTR1                                                                   
         L     R4,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'D',ACCBIG),(ELCODE,(R4)),0                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR CURRENT AIO                                      *         
***********************************************************************         
*                                                                               
XCIO     NTR1                                                                   
         LR    R0,R4                                                            
         LA    R1,MAXACC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         CLI   QOPT2,C' '          DEFAULT(SUMMARY)?                            
         BE    REQL10                                                           
         CLI   QOPT2,C'S'          SUMMARY REPORT?                              
         BE    REQL10                                                           
         TM    BIT,SORTPROF        SET NAME SORT PROFILE?                       
         BZ    *+12                                                             
         BAS   RE,SORTRPT          PRINT SORTED DETAIL REPORT                   
         B     REQL20                                                           
         BAS   RE,REPORT           PRINT OFFC/DPT/SDPT/PER REPORT               
         CLI   QOPT2,C'B'          PRINT BOTH TYPES OF REPORTS?                 
         BE    REQL15                                                           
         B     REQL50                                                           
REQL10   TM    BIT,SORTPROF        SORT PROFILE IN USE?                         
         BZ    *+12                                                             
         BAS   RE,SORTPER          PRINT SORTED SUMMARY REPORT                  
         B     REQL50                                                           
REQL15   BAS   RE,PERSON           PRINT PERSON SUMMARY REPORT                  
         B     REQL50                                                           
*                                                                               
REQL20   CLI   QOPT2,C'B'          PRINT BOTH SORT DETAIL AND                   
         BNE   REQL50              SORT SUMMARY REPORT                          
         BAS   RE,SORTPER                                                       
*                                                                               
REQL50   BAS   RE,ERROR            PRINT ERROR REPORT                           
         BAS   RE,RECAP            PRINT RECORD RECAP REPORT                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT ITEM REPORT                                      *         
***********************************************************************         
*                                                                               
REPORT   NTR1                                                                   
         MVI   RCSUBPRG,0          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    SVACT,SVACT         CLEAR HEADLINE SAVE ACCOUNT                  
         XC    SVPERSN,SVPERSN                                                  
         ZAP   LASTTOT,=P'0'                                                    
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFREP1                                                  
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
REP100   MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    EXIT                                                             
         NI    BIT,X'FF'-PGBRKUP                                                
         CLI   BUFTYPE,BUFREP1                                                  
         BNE   REP100                                                           
         CLI   PROGPROF+2,3        BREAK ON SUBDEPT                             
         BE    REP120              NO NEED TO PRINT HEADINGS                    
         CLI   PROGPROF+2,C' '                                                  
         BE    REP120                                                           
         CLI   PROGPROF+2,0                                                     
         BE    REP120                                                           
         CLI   PROGPROF+2,1                                                     
         BNE   REP112                                                           
         ZIC   R1,LEVELA                                                        
         B     REP113                                                           
*                                                                               
REP112   CLI   PROGPROF+2,2                                                     
         BNE   REP115                                                           
         ZIC   R1,LEVELB                                                        
*                                                                               
REP113   BCTR  R1,0                                                             
         EXCLC R1,BUFACT,SVACT     WILL THERE BE A PAGE BREAK NEXT              
         BE    REP115                                                           
         OI    BIT,PGBRKUP         YES-DO NOT PRINT HEADINGS                    
         B     REP120              WAIT FOR NEXT PAGE                           
*                                                                               
REP115   ZIC   R1,LEVELC                                                        
         BCTR  R1,0                                                             
         EXCLC R1,BUFACT,SVACT                                                  
         BE    REP120                                                           
         GOTO1 =A(PRNTHED),DMCB,(RC)  PRNT HEADINGS FOR CHA IN ACCNT            
*                                                                               
REP120   CLI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         BE    REP200                                                           
         XC    SVPERSN,SVPERSN                                                  
         CLI   BUFTTYPE,BUFTPERS   PERSON TOTAL RECORD                          
         BE    REP300                                                           
         CLI   BUFTTYPE,BUFTSDPT   SUBDEPT TOTAL RECORD                         
         BE    REP400                                                           
         CLI   BUFTTYPE,BUFTDPT    DEPT TOTAL RECORD                            
         BE    REP500                                                           
         CLI   BUFTTYPE,BUFTOFFC   OFFICE TOTAL RECORD                          
         BE    REP600                                                           
         CLI   BUFTTYPE,BUFTREQ    REQUEST TOTAL RECORD                         
         BE    REP700                                                           
         CLI   BUFTTYPE,BUFTCNTL   CONTROL TOTAL RECORD                         
         BE    REP800                                                           
         DC    H'0'                                                             
*                                                                               
*              PRINT ITEM LINE                                                  
*                                                                               
REP200   TM    BIT,PGBRKUP                                                      
         BZ    REP205                                                           
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTXP                                                        
         GOTO1 =A(PRNTHED),DMCB,(RC)                                            
*                                                                               
         USING PLINED,R6                                                        
REP205   LA    R6,XP                                                            
         ZAP   LASTTOT,=P'0'                                                    
         CLC   SVPERSN,BUFPERSN                                                 
         BE    REP210                                                           
         MVC   SVPERSN,BUFPERSN                                                 
         MVC   PLINSTAF,BUFPERSN   EMPLOYEE NUMBER & NAME                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINNAME,WORK                                                    
*                                                                               
REP210   GOTO1 DATCON,DMCB,(0,BUFCKDT),(5,PLINCKDT)                             
         MVC   PLINCODE,BUFCODE                                                 
         MVC   PLINDESC,BUFDESC                                                 
         EDIT  (P8,BUFCKAMT),(14,PLINAMNT),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
*                                                                               
*              PRINT PERSON TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
REP300   CP    BUFACCUM,=P'1'                                                   
         BE    REP100                                                           
         LA    R6,XP                                                            
         MVC   PLINTOT$,=C'--------------'                                      
         BAS   RE,PRNTXP                                                        
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         BAS   RE,PRNTXP                                                        
         ZAP   LASTTOT,BUFCKAMT                                                 
         B     REP100                                                           
*                                                                               
*              PRINT SUBDEPARTMENT TOTAL                                        
*                                                                               
         USING PLINED,R6                                                        
REP400   CP    BUFACCUM,=P'1'      SUBDEPT LEVEL                                
         BE    REP100                                                           
         CP    LASTTOT,BUFCKAMT                                                 
         BE    REP100                                                           
         ZAP   LASTTOT,BUFCKAMT                                                 
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFSDPT),BUFSDPT                                       
         MVC   WORK+20(L'BUFNMLV3),BUFNMLV3                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINTOT,WORK                                                     
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
*                                                                               
*              PRINT DEPARTMENT TOTAL                                           
*                                                                               
         USING PLINED,R6                                                        
REP500   CP    BUFACCUM,=P'1'      DEPT LEVEL                                   
         BE    REP100                                                           
         CP    LASTTOT,BUFCKAMT                                                 
         BE    REP100                                                           
         ZAP   LASTTOT,BUFCKAMT                                                 
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFDPT),BUFDPT                                         
         MVC   WORK+20(L'BUFNMLV2),BUFNMLV2                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINTOT,WORK                                                     
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
*                                                                               
*              PRINT OFFICE TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
REP600   CP    BUFACCUM,=P'1'                                                   
         BE    REP100                                                           
         CP    LASTTOT,BUFCKAMT                                                 
         BE    REP100                                                           
         ZAP   LASTTOT,BUFCKAMT                                                 
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFOFFC),BUFOFFC                                       
         MVC   WORK+20(L'BUFNMLV1),BUFNMLV1                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINTOT,WORK                                                     
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
*                                                                               
*              POSTING TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
REP700   BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PLINTOT(20),=CL20'TOTAL FOR POSTING'                             
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
*                                                                               
*              CONTROL TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
REP800   BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PLINTOT(20),=CL20'TOTAL FOR CONTROL'                             
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
         EJECT                                                                  
***********************************************************************         
*              PRINT SORTED DETAIL REPORT                             *         
***********************************************************************         
*                                                                               
SORTRPT  NTR1                                                                   
         MVI   RCSUBPRG,0          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    SVACT,SVACT         CLEAR HEADLINE SAVE ACCOUNT                  
         XC    SVPERSN,SVPERSN                                                  
         ZAP   LASTTOT,=P'0'                                                    
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFNMRP1                                                 
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
SORT100  MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    EXIT                                                             
         CLI   BUFTYPE,BUFNMRP1                                                 
         BNE   SORT100                                                          
*                                                                               
SORT140  CLI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         BE    SORT200                                                          
         XC    SVPERSN,SVPERSN                                                  
         CLI   BUFTTYPE,BUFTPERS   PERSON TOTAL RECORD                          
         BE    SORT300                                                          
         CLI   BUFTTYPE,BUFTSDPT   SUBDEPT TOTAL RECORD                         
         BE    SORT400                                                          
         CLI   BUFTTYPE,BUFTDPT    DEPT TOTAL RECORD                            
         BE    SORT500                                                          
         CLI   BUFTTYPE,BUFTOFFC   OFFICE TOTAL RECORD                          
         BE    SORT600                                                          
         CLI   BUFTTYPE,BUFTREQ    REQUEST TOTAL RECORD                         
         BE    SORT700                                                          
         CLI   BUFTTYPE,BUFTCNTL   CONTROL TOTAL RECORD                         
         BE    SORT800                                                          
         DC    H'0'                                                             
*                                                                               
*              PRINT ITEM LINE                                                  
*                                                                               
         USING PLINED,R6                                                        
SORT200  LA    R6,XP                                                            
         ZAP   LASTTOT,=P'0'                                                    
         CLC   SVPERSN,BUFPERSN                                                 
         BE    SORT220                                                          
         MVC   SVPERSN,BUFPERSN                                                 
         CLI   PROGPROF+4,C'1'     SORT ON NAME BY OFFICE                       
         BNE   SORT206                                                          
         TM    BIT,ONEBYTOF        ONE BYTE OFFICES                             
         BZ    *+14                                                             
         MVC   PSTSTAF3(2),BUFACT+1    SHOW DPT/SDPT/EMP #                      
         B     *+10                                                             
         MVC   PSTSTAF3(2),BUFACT+2                                             
         TM    BIT,ONEBYTOF                                                     
         BZ    *+14                                                             
         MVC   PSTSTAF3+2(2),BUFACT+3                                           
         B     *+10                                                             
         MVC   PSTSTAF3+2(2),BUFACT+4                                           
         MVC   PSTSTAF3+4(8),BUFPERSN                                           
         B     SORT215                                                          
SORT206  CLI   PROGPROF+4,C'2'     SORT ON NAME BY DEPT                         
         BNE   SORT209                                                          
         TM    BIT,ONEBYTOF        ONE BYTE OFFICES                             
         BZ    SORT207                                                          
         MVC   PSTSTAF3(2),BUFACT+3    SHOW SDPT/EMP #                          
         MVC   PSTSTAF3+2(8),BUFPERSN                                           
         B     SORT215                                                          
SORT207  MVC   PSTSTAF3(2),BUFACT+4                                             
         MVC   PSTSTAF3+2(8),BUFPERSN                                           
         B     SORT215                                                          
SORT209  MVC   PSTSTAFF,BUFPERSN   JUST SHOW EMP NUMBER                         
SORT215  MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         CLI   PROGPROF+4,C'1'          SORT ON NAME BY OFFICE                  
         BNE   *+14                                                             
         MVC   PSTNAME3,WORK                                                    
         B     SORT220                                                          
         CLI   PROGPROF+4,C'2'     SORT ON NAME BY DEPT                         
         BNE   *+14                                                             
         MVC   PSTNAME2,WORK                                                    
         B     *+10                                                             
         MVC   PSTNAME,WORK                                                     
SORT220  GOTO1 DATCON,DMCB,(0,BUFCKDT),(5,PSTCKDT)                              
         MVC   PSTCODE,BUFCODE                                                  
         MVC   PSTDESC,BUFDESC                                                  
         EDIT  (P8,BUFCKAMT),(14,PSTAMNT),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORT100                                                          
*                                                                               
*              PRINT PERSON TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
SORT300  CP    BUFACCUM,=P'1'                                                   
         BE    SORT100                                                          
         LA    R6,XP                                                            
         MVC   PSTTOT$,=C'--------------'                                       
         BAS   RE,PRNTXP                                                        
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         BAS   RE,PRNTXP                                                        
         ZAP   LASTTOT,BUFCKAMT                                                 
         B     SORT100                                                          
*                                                                               
*              PRINT SUBDEPARTMENT TOTAL                                        
*                                                                               
         USING PLINED,R6                                                        
SORT400  CP    BUFACCUM,=P'1'                                                   
         BE    SORT100                                                          
         CP    LASTTOT,BUFCKAMT                                                 
         BE    SORT100                                                          
         ZAP   LASTTOT,BUFCKAMT                                                 
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFSDPT),BUFSDPT                                       
         MVC   WORK+20(L'BUFNMLV3),BUFNMLV3                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PSTTOT,WORK                                                      
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORT100                                                          
*                                                                               
*              PRINT DEPARTMENT TOTAL                                           
*                                                                               
         USING PLINED,R6                                                        
SORT500  CP    BUFACCUM,=P'1'                                                   
         BE    SORT100                                                          
         CP    LASTTOT,BUFCKAMT                                                 
         BE    SORT100                                                          
         ZAP   LASTTOT,BUFCKAMT                                                 
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFDPT),BUFDPT                                         
         MVC   WORK+20(L'BUFNMLV2),BUFNMLV2                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PSTTOT,WORK                                                      
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORT100                                                          
*                                                                               
*              PRINT OFFICE TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
SORT600  CP    BUFACCUM,=P'1'                                                   
         BE    SORT100                                                          
         CP    LASTTOT,BUFCKAMT                                                 
         BE    SORT100                                                          
         ZAP   LASTTOT,BUFCKAMT                                                 
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFOFFC),BUFOFFC                                       
         MVC   WORK+20(L'BUFNMLV1),BUFNMLV1                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PSTTOT,WORK                                                      
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORT100                                                          
*                                                                               
*              POSTING TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
SORT700  BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PSTTOT(20),=CL20'TOTAL FOR POSTING'                              
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORT100                                                          
*                                                                               
*              CONTROL TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
SORT800  BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PSTTOT(20),=CL20'TOTAL FOR CONTROL'                              
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORT100                                                          
                                                                                
***********************************************************************         
*              PRINT SORTED SUMMARY REPORT                            *         
***********************************************************************         
*                                                                               
SORTPER  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1          SET CORRECT HEADINGS                         
         MVC   PAGE,=H'1'                                                       
         XC    SVACT,SVACT         CLEAR HEADLINE SAVE ACCOUNT                  
         XC    SVPERSN,SVPERSN                                                  
         ZAP   LASTTOT,=P'0'                                                    
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFNMRP2                                                 
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
SORTP10  MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    EXIT                                                             
         CLI   BUFTYPE,BUFNMRP2                                                 
         BNE   SORTP10                                                          
*                                                                               
SORTP20  CLI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         BE    SORTP30                                                          
         CLI   BUFTTYPE,BUFTDPT    DEPT TOTAL RECORD                            
         BE    SORTP60                                                          
         CLI   BUFTTYPE,BUFTOFFC   OFFICE TOTAL RECORD                          
         BE    SORTP90                                                          
         CLI   BUFTTYPE,BUFTREQ    REQUEST TOTAL RECORD                         
         BE    SORTP120                                                         
         CLI   BUFTTYPE,BUFTCNTL   CONTROL TOTAL RECORD                         
         BE    SORTP150                                                         
         DC    H'0'                                                             
*                                                                               
         USING PLINED,R6                                                        
SORTP30  LA    R6,XP                                                            
         CLI   PROGPROF+4,C'1'     SORT ON NAME BY OFFICE                       
         BNE   SORTP32                                                          
         TM    BIT,ONEBYTOF        ONE BYTE OFFICES                             
         BZ    *+14                                                             
         MVC   PSTSTAF3(2),BUFACT+1    SHOW DPT/SDPT/EMP #                      
         B     *+10                                                             
         MVC   PSTSTAF3(2),BUFACT+2                                             
         TM    BIT,ONEBYTOF                                                     
         BZ    *+14                                                             
         MVC   PSTSTAF3+2(2),BUFACT+3                                           
         B     *+10                                                             
         MVC   PSTSTAF3+2(2),BUFACT+4                                           
         MVC   PSTSTAF3+4(8),BUFPERSN                                           
         B     SORTP40                                                          
SORTP32  CLI   PROGPROF+4,C'2'     SORT ON NAME BY DEPT                         
         BNE   SORTP36                                                          
         TM    BIT,ONEBYTOF        ONE BYTE OFFICES                             
         BZ    SORTP34                                                          
         MVC   PSTSTAF3(2),BUFACT+3    SHOW SDPT/EMP #                          
         MVC   PSTSTAF3+2(8),BUFPERSN                                           
         B     SORTP40                                                          
SORTP34  MVC   PSTSTAF3(2),BUFACT+4                                             
         MVC   PSTSTAF3+2(8),BUFPERSN                                           
         B     SORTP40                                                          
SORTP36  MVC   PSTSTAFF,BUFPERSN   JUST SHOW EMP NUMBER                         
SORTP40  MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         CLI   PROGPROF+4,C'1'          SORT ON NAME BY OFFICE                  
         BNE   *+14                                                             
         MVC   PSTNAME3,WORK                                                    
         B     SORTP50                                                          
         CLI   PROGPROF+4,C'2'     SORT ON NAME BY DEPT                         
         BNE   *+14                                                             
         MVC   PSTNAME2,WORK                                                    
         B     *+10                                                             
         MVC   PSTNAME,WORK                                                     
SORTP50  EDIT  (P8,BUFCKAMT),(14,PSTAMNT),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORTP10                                                          
*                                                                               
*              PRINT DEPARTMENT TOTAL                                           
*                                                                               
         USING PLINED,R6                                                        
SORTP60  LA    R6,XP                                                            
         BAS   RE,PRNTXP                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFDPT),BUFDPT                                         
         MVC   WORK+20(L'BUFNMLV2),BUFNMLV2                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PSTTOT,WORK                                                      
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORTP10                                                          
*                                                                               
*              PRINT OFFICE TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
SORTP90  LA    R6,XP                                                            
         BAS   RE,PRNTXP                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFOFFC),BUFOFFC                                       
         MVC   WORK+20(L'BUFNMLV1),BUFNMLV1                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PSTTOT,WORK                                                      
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORTP10                                                          
*                                                                               
*              POSTING TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
SORTP120 BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PSTTOT(20),=CL20'TOTAL FOR POSTING'                              
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORTP10                                                          
*                                                                               
*              CONTROL TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
SORTP150 BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PSTTOT(20),=CL20'TOTAL FOR CONTROL'                              
         EDIT  (P8,BUFCKAMT),(14,PSTTOT$),2,FLOAT=-                             
         BAS   RE,PRNTXP                                                        
         B     SORTP10                                                          
***********************************************************************         
*        PRINT PERSON SUMMARY REPORT                                            
***********************************************************************         
*                                                                               
PERSON   NTR1                                                                   
         XC    SVACT,SVACT         CLEAR HEADLINE SAVE ACCOUNT                  
         MVI   RCSUBPRG,1          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFREP2                                                  
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
PER100   MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    EXIT                                                             
         NI    BIT,X'FF'-PGBRKUP                                                
         CLI   BUFTYPE,BUFREP2                                                  
         BNE   PER100                                                           
*                                                                               
         CLI   PROGPROF+2,3        BREAK ON SUBDEPT                             
         BE    PER120              NO NEED TO PRINT HEADINGS                    
         CLI   PROGPROF+2,C' '                                                  
         BE    PER120                                                           
         CLI   PROGPROF+2,0                                                     
         BE    PER120                                                           
         CLI   PROGPROF+2,1                                                     
         BNE   PER112                                                           
         ZIC   R1,LEVELA                                                        
         B     PER113                                                           
*                                                                               
PER112   CLI   PROGPROF+2,2                                                     
         BNE   PER115                                                           
         ZIC   R1,LEVELB                                                        
*                                                                               
PER113   BCTR  R1,0                                                             
         EXCLC R1,BUFACT,SVACT     WILL THERE BE A PAGE BREAK NEXT              
         BE    PER115                                                           
         OI    BIT,PGBRKUP         YES-DO NOT PRINT HEADINGS                    
         B     PER120              WAIT FOR NEXT PAGE                           
*                                                                               
PER115   ZIC   R1,LEVELC                                                        
         BCTR  R1,0                                                             
         EXCLC R1,BUFACT,SVACT                                                  
         BE    PER120                                                           
         GOTO1 =A(PRNTHED),DMCB,(RC) PRNT HEADINGS FOR CHA IN ACCNT             
*                                                                               
PER120   CLI   BUFTTYPE,BUFTITEM   ITEM RECORD                                  
         BE    PER200                                                           
         CLI   BUFTTYPE,BUFTDPT    DEPT TOTAL RECORD                            
         BE    PER300                                                           
         CLI   BUFTTYPE,BUFTOFFC   OFFICE TOTAL RECORD                          
         BE    PER400                                                           
         CLI   BUFTTYPE,BUFTREQ    REQUEST TOTAL RECORD                         
         BE    PER500                                                           
         CLI   BUFTTYPE,BUFTCNTL   CONTROL TOTAL RECORD                         
         BE    PER600                                                           
         DC    H'0'                                                             
*                                                                               
*              PRINT ITEM LINE                                                  
*                                                                               
PER200   TM    BIT,PGBRKUP                                                      
         BZ    PER205                                                           
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTXP                                                        
         GOTO1 =A(PRNTHED),DMCB,(RC)                                            
*                                                                               
         USING PLINED,R6                                                        
PER205   LA    R6,XP                                                            
         MVC   PLINSTAF,BUFPERSN   EMPLOYEE NUMBER & NAME                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINNAME,WORK                                                    
         EDIT  (P8,BUFCKAMT),(14,PLINAMNT),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     PER100                                                           
*                                                                               
*              PRINT DEPARTMENT TOTAL                                           
*                                                                               
         USING PLINED,R6                                                        
PER300   LA    R6,XP                                                            
         BAS   RE,PRNTXP                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFDPT),BUFDPT                                         
         MVC   WORK+20(L'BUFNMLV2),BUFNMLV2                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINTOT,WORK                                                     
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     PER100                                                           
*                                                                               
*              PRINT OFFICE TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
PER400   LA    R6,XP                                                            
         BAS   RE,PRNTXP                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'BUFOFFC),BUFOFFC                                       
         MVC   WORK+20(L'BUFNMLV1),BUFNMLV1                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINTOT,WORK                                                     
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     PER100                                                           
*                                                                               
*              POSTING TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
PER500   BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PLINTOT(20),=CL20'TOTAL FOR POSTING'                             
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     PER100                                                           
*                                                                               
*              CONTROL TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
PER600   BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PLINTOT(20),=CL20'TOTAL FOR CONTROL'                             
         EDIT  (P8,BUFCKAMT),(14,PLINTOT$),2,FLOAT=-                            
         BAS   RE,PRNTXP                                                        
         B     PER100                                                           
         EJECT                                                                  
***********************************************************************         
*              PRINT ERROR REPORT                                     *         
***********************************************************************         
*                                                                               
ERROR    NTR1                                                                   
         MVI   RCSUBPRG,2          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFERROR                                                 
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
ERR100   MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    ERRX                                                             
         CLI   BUFTYPE,BUFERROR                                                 
         BNE   ERR100                                                           
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
         MVC   WORK,SPACES                                                      
*        MVC   WORK(48),BUFOFFC                                                 
         MVC   WORK(36),BUFOFFC                                                 
         MVC   WORK+36(12),BUFPERSN                                             
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PERRACT,WORK                                                     
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PERRNAME,WORK                                                    
         GOTO1 DATCON,DMCB,(0,BUFCKDT),(5,PERRCKDT)                             
         MVC   PERRPAYC,BUFCODE                                                 
         EDIT  (P8,BUFCKAMT),(14,PERRAMNT),2,FLOAT=-                            
         TM    BUFSTAT,BUFIOFF     INVALID OFFICE                               
         BZ    *+8                                                              
         MVI   PERROFFC,C'I'                                                    
         TM    BUFSTAT,BUFIDPT     INVALID DEPARTMENT                           
         BZ    *+8                                                              
         MVI   PERRDPT,C'I'                                                     
         TM    BUFSTAT,BUFISDPT    INVALID SUB DEPARTMENT                       
         BZ    *+8                                                              
         MVI   PERRSDPT,C'I'                                                    
         TM    BUFSTAT,BUFISTAF    INVALID 1R STAFF RECORD                      
         BZ    *+8                                                              
         MVI   PERRSTAF,C'I'                                                    
         TM    BUFSTAT,BUFIPERS    INVALID PERSON RECORD                        
         BZ    *+8                                                              
         MVI   PERRPERS,C'I'                                                    
         TM    BUFSTAT,BUFIDATE    INVALID CHECK DATE                           
         BZ    *+8                                                              
         MVI   PERRPERS,C'D'                                                    
         TM    BUFSTAT,BUFICKDT    INVALID CHECK DATE                           
         BZ    *+8                                                              
         MVI   PERRPERS,C'C'                                                    
         TM    BUFSTAT,BUFICODE    INVALID PAYROLL CODE                         
         BZ    *+8                                                              
         MVI   PERRCODE,C'I'                                                    
         TM    BUFSTAT2,BUFINAME   INVALID NAME(S)                              
         BZ    *+8                                                              
         MVI   PERRPERS,C'N'                                                    
         AP    COUNTERR,=P'1'                                                   
         BAS   RE,PRNTXP                                                        
         B     ERR100                                                           
*                                                                               
ERRX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT RECAP REPORT                                     *         
***********************************************************************         
*                                                                               
RECAP    NTR1                                                                   
         MVI   RCSUBPRG,3          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
         LA    R2,COUNTERS                                                      
         LA    R3,RECAPMSG                                                      
         LA    R4,COUNT#                                                        
*                                                                               
RECAP100 MVC   PXDESC,0(R3)                                                     
         CLC   =C'**',PXDESC       BREAKER LINE - NO TOTALS                     
         BE    RECAP110                                                         
         EDIT  (P8,0(R2)),(7,PXCOUNT),ZERO=NOBLANK                              
RECAP110 BAS   RE,PRNTXP                                                        
         LA    R2,L'COUNTERS(R2)                                                
         LA    R3,L'PXDESC(R3)                                                  
         BCT   R4,RECAP100                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT A LINE                                           *         
***********************************************************************         
*                                                                               
PRNTXP   NTR1                                                                   
         MVC   XHEAD2+12(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+21(L'CMPNAME),CMPNAME                                     
*                                                                               
         CLI   RCSUBPRG,1          REGULAR REPORT BOXES                         
         BH    PRNTX                                                            
*                                                                               
         TM    BIT,PGBRKUP                                                      
         BO    PRNTX20                                                          
*                                                                               
* SORTING OPTION TAKES PRECEDENCE OVER LEV FOR PAGE BREAK                       
*                                                                               
         SR    R1,R1               NEW PAGE ON CHANGE OF OFF/DPT/SDPT           
         CLI   PROGPROF+4,C'1'     SORT BY NAME AT LEV 1                        
         BNE   *+12                                                             
         ICM   R1,1,LEVELA                                                      
         B     PRNTX10                                                          
         CLI   PROGPROF+4,C'2'     SORT BY NAME AT LEV 2                        
         BNE   *+12                                                             
         ICM   R1,1,LEVELB                                                      
         B     PRNTX10                                                          
         CLI   PROGPROF+4,C'3'     SORT BY NAME AT LEV 3                        
         BNE   *+12                                                             
         ICM   R1,1,LEVELC                                                      
         B     PRNTX10                                                          
*                                                                               
         CLI   PROGPROF+2,1     PAGE BREAK AT LEVEL 1                           
         BNE   *+12                                                             
         ICM   R1,1,LEVELA                                                      
         B     PRNTX10                                                          
         CLI   PROGPROF+2,2     PAGE BREAK AT LEVEL 2                           
         BNE   *+12                                                             
         ICM   R1,1,LEVELB                                                      
         B     PRNTX10                                                          
         ICM   R1,1,LEVELC         PAGE BREAK AT LEVEL 3 IS DEFAULT             
*                                                                               
PRNTX10  DS    0H                                                               
         SH    R1,=H'1'                                                         
         EXCLC R1,BUFACT,SVACT                                                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING PLINED,R6                                                        
PRNTX20  MVC   SVACT,BUFACT                                                     
         MVC   XHEAD4(L'LEVELANM),LEVELANM           OFFICE                     
         MVC   XHEAD4+12(6),BUFOFFC                                             
         MVC   XHEAD4+21(L'BUFNMLV1),BUFNMLV1                                   
         MVC   XHEAD5,XSPACES                                                   
         MVC   XHEAD6,XSPACES                                                   
         TM    BIT,SORTPROF                                                     
         BZ    PRNTX25                                                          
         CLI   PROGPROF+4,C'1'                                                  
         BE    PRNTX                                                            
         B     PRNTX30                                                          
PRNTX25  CLI   PROGPROF+2,1        IF BREAK ON OFFICE-DONE                      
         BE    PRNTX                                                            
*                                                                               
PRNTX30  MVC   XHEAD5(L'LEVELBNM),LEVELBNM           DEPT                       
         MVC   XHEAD5+12(6),BUFDPT                                              
         MVC   XHEAD5+21(L'BUFNMLV2),BUFNMLV2                                   
         MVC   XHEAD6,XSPACES                                                   
         TM    BIT,SORTPROF                                                     
         BZ    PRNTX35                                                          
         CLI   PROGPROF+4,C'2'     IF BREAK ON DEPT-DONE                        
         BE    PRNTX                                                            
         B     PRNTX40                                                          
PRNTX35  CLI   PROGPROF+2,2        IF BREAK ON DEPT-DONE                        
         BE    PRNTX                                                            
*                                                                               
PRNTX40  MVC   XHEAD6(L'LEVELCNM),LEVELCNM           SUBDEPT                    
         MVC   XHEAD6+12(6),BUFSDPT                                             
         MVC   XHEAD6+21(L'BUFNMLV3),BUFNMLV3                                   
*                                                                               
PRNTX    GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DUMP RECORDS                                           *         
***********************************************************************         
*                                                                               
DUMP     NTR1                                                                   
         L     R4,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(R4)                                                
         LA    R2,=C'UPDATE REC'                                                
         CLI   ACTION,BUILD                                                     
         BNE   *+8                                                              
         LA    R2,=C'BUILD REC '                                                
         GOTO1 PRNTBL,DMCB,(10,(R2)),(R4),C'DUMP',(RF),=C'2D'                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              EXTERNAL ADDRESS LIST                                  *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(DATVAL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(GETCAP)                                                        
         DC    V(COVAIL)                                                        
         DC    V(CHOPCON)                                                       
         DC    V(HELLO)                                                         
         DC    A(ELTAB)                                                         
         DC    A(BETAB)                                                         
         DC    A(CONVTAB)                                                       
         DC    A(BUFFALOC)                                                      
         DC    A(IO1)                                                           
         DC    A(IO2)                                                           
         DC    A(IOTAPE)                                                        
         DC    A(LSTWRK)                                                        
         DC    A(PYTABLE)                                                       
         DC    A(CCOBLOCK)                                                      
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLES                                         *         
***********************************************************************         
*                                                                               
*        DS    CL1 = RECORD TYPES WHERE THIS ELEM MUST BE BUILT/UPDATED         
*                    X'10' = 1R RECORD     (CPY/U/L/OF/DPT/SDPT/PER)            
*                    X'01' = PERSON RECORD (X'0F'/CPY/PERSON)                   
*        DS    AL4 = ADDRESS OF ROUTINE TO HANDLE ELEMENT                       
*        DS    AL1 = ACTIONS WHEN THIS ROUTINE SHOULD BE EXECUTED               
*                    BUILD  = ONLY EXECUTE IF BUILDING A NEW RECORD             
*                    UPDATE = ONLY EXECUTE IF UPDATING AN OLD RECORD            
*                                                                               
*                                                                               
ELTAB    DS    0XL6                                                             
         DC    X'10',AL4(ELEM20),AL1(BUILD+UPDATE)                              
         DC    X'11',AL4(ELEM5A),AL1(BUILD+UPDATE)                              
ELTAB#   EQU   (*-ELTAB)/L'ELTAB                                                
         EJECT                                                                  
***********************************************************************         
*              BENEFIT TABLE                                          *         
***********************************************************************         
*                                                                               
BETAB    DS    0H                                                               
         DC    X'73',AL2(58),CL5'BEN  ',PL4'18000',AL1(9)                       
         DC    CL5'REG  '                                                       
         DC    CL5'VACA '                                                       
         DC    CL5'SICK '                                                       
         DC    CL5'OT   '                                                       
         DC    CL5'DT   '                                                       
         DC    CL5'HOL  '                                                       
         DC    CL5'BONUS'                                                       
         DC    CL5'COMM '                                                       
         DC    CL5'SPCL '                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              CONVERSION TABLE                                                 
***********************************************************************         
CONVTAB  DS    0H                                                               
         DC    X'FF'                                                            
         DC    AL1(0)                                                           
         DC    AL1(LEVELA-LEVELS)                                               
         DC    AL1(0)                                                           
         DC    AL1(LEVELA-LEVELS)                                               
         DC    AL1(2)                                                           
         DC    CL(L'TAPEACT)'QQ',CL(L'TAPEACT)'04'                              
         DC    CL(L'TAPEACT)'RR',CL(L'TAPEACT)'02'                              
         DC    X'FF'                                                            
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
RECAPMSG DC    CL35'RECORDS IN FROM TAPE'                                       
         DC    CL35'RECORDS FROM TAPE W/ERRORS  (-)'                            
         DC    CL35'RECORDS ADDED FOR REVERSALS (+)'                            
         DC    CL35'RECORDS ADDED FOR BENEFITS  (+)'                            
         DC    CL35'RECORDS ADDED FOR PERCENTS  (+)'                            
         DC    CL35'*******************************'                            
         DC    CL35'RECORDS UPDATED ON FILE     (+)'                            
         DC    CL35'RECORDS ADDED TO ACCFILE'                                   
*                                                                               
EFFS     DC    48X'FF'                                                          
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCBIG   DC    CL8'ACCBIG  '                                                    
*                                                                               
MYKEY    DC    CL41' '                                                          
MYPERSN  DC    CL41' '                                                          
NOMATCH  DC    CL1'N'                                                           
SAVNMLV1 DC    CL36' '             OFFICE NAME                                  
SAVNMLV2 DC    CL36' '             DEPARTMENT NAME                              
SAVNMLV3 DC    CL36' '             SUBDEPARTMENT NAME                           
SAVSTAT  DC    X'00'               STATUS, LOOK @ BUFSTAT                       
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=70'                                    
SORTCARD DC    CL80'SORT FIELDS=(1,12,A,50,10,A),FORMAT=BI,WORK=1'              
*                                                                               
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,MACRF=GM,EODAD=LDGF200,          X        
               RECFM=FB,BUFNO=1                                                 
*                                                                               
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(74,A),                                         X        
               COMMENT=197,                                            X        
               LINES=100,                                              X        
               COLUMNS=2,                                              X        
               ROWS=1                                                           
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              PUT RECORD TO BUFFALO                                  *         
***********************************************************************         
*                                                                               
PUTBUF   NMOD1 0,*PUTBUF*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING TAPED,R3            DONT PUT REVERSALS TO REPORT                 
         L     R3,ALSTWRK                                                       
         TM    TAPESTAT,TAPEREV                                                 
         BO    PUTX                                                             
*                                                                               
*              ERROR REPORT ITEM                                                
*                                                                               
         CLI   BUFTYPE,BUFERROR                                                 
         BNE   PUT100                                                           
         MVI   BUFTTYPE,BUFTITEM                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         MVI   BUFTTYPE,BUFTCNTL                                                
         MVC   BUFOFFC,EFFS                                                     
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFDPT,EFFS                                                      
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFPERSN,EFFS                                                    
*        MVC   BUFKACT,EFFS                                                     
         MVI   BUFNUM,X'FF'                                                     
         MVC   BUFDATE,EFFS                                                     
*                                                                               
         TM    BIT,SORTPROF        SORTING BY NAME PROFILE ON?                  
         BZ    PUT025                                                           
         CLI   QOPT2,C' '                                                       
         BE    PUT030                                                           
         CLI   QOPT2,C'S'                                                       
         BE    PUT030                                                           
         MVI   BUFTYPE,BUFNMRP1                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         CLI   QOPT2,C'B'          BOTH TYPES?                                  
         BNE   PUTX                                                             
PUT030   MVI   BUFTYPE,BUFNMRP2                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         B     PUTX                                                             
*                                                                               
PUT025   CLI   QOPT2,C' '          DEFULT(SUMMARY)?                             
         BE    PUT050                                                           
         CLI   QOPT2,C'S'          SUMMARY REPORT                               
         BE    PUT050                                                           
         MVI   BUFTYPE,BUFREP1                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         CLI   QOPT2,C'B'          BOTH TYPES?                                  
         BNE   PUTX                                                             
PUT050   MVI   BUFTYPE,BUFREP2                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         B     PUTX                                                             
*                                                                               
*              DETAIL PERSON ITEM                                               
*                                                                               
PUT100   MVC   WORK(L'BUFKEY),BUFKEY                                            
         TM    BIT,SORTPROF        SET NAME SORT PROFILE?                       
         BO    PUT300                                                           
         CLI   QOPT2,C' '          DEFAUT(SUMMARY)?                             
         BE    PUT200                                                           
         CLI   QOPT2,C'S'          SUMMARY REPORT?                              
         BE    PUT200                                                           
*                                                                               
         MVI   BUFTYPE,BUFREP1                                                  
         MVI   BUFTTYPE,BUFTITEM                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL PERSON TOTAL RECORD                                       
*                                                                               
         MVI   BUFNUM,X'FF'                                                     
         MVC   BUFDATE,EFFS                                                     
         MVI   BUFTTYPE,BUFTPERS                                                
         MVI   BUFTYPE,BUFREP1                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL DEPT TOTAL RECORD                                         
*                                                                               
         MVI   BUFTTYPE,BUFTDPT                                                 
*        MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFPERSN,EFFS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL OFFICE TOTAL RECORD                                       
*                                                                               
         MVI   BUFTTYPE,BUFTOFFC                                                
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFDPT,EFFS                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL REQUEST TOTAL RECORD                                      
*                                                                               
         MVI   BUFTTYPE,BUFTREQ                                                 
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFOFFC,EFFS                                                     
         MVI   BUFOFFC+11,X'FE'                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL CONTROL TOTAL RECORD, REQUEST + ERRORS                    
*                                                                               
         MVI   BUFTTYPE,BUFTCNTL                                                
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFOFFC,EFFS                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         CLI   QOPT2,C'B'          BOTH REPORT TYPES?                           
         BNE   PUTX                                                             
*              SUMMARY PERSON ITEM                                              
*                                                                               
PUT200   MVC   BUFKEY,WORK                                                      
         MVI   BUFTYPE,BUFREP2                                                  
         MVI   BUFTTYPE,BUFTITEM                                                
         MVI   BUFNUM,X'FF'                                                     
         MVC   BUFDATE,EFFS                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY DEPT TOTAL RECORD                                        
*                                                                               
         MVI   BUFTTYPE,BUFTDPT                                                 
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFPERSN,EFFS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY OFFICE TOTAL RECORD                                      
*                                                                               
         MVI   BUFTTYPE,BUFTOFFC                                                
         MVC   BUFDPT,EFFS                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY REQUEST TOTAL RECORD                                     
*                                                                               
         MVI   BUFTTYPE,BUFTREQ                                                 
         MVC   BUFOFFC,EFFS                                                     
         MVI   BUFOFFC+11,X'FE'                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY CONTROL TOTAL RECORD, REQUEST + ERRORS                   
*                                                                               
         MVI   BUFTTYPE,BUFTCNTL                                                
         MVC   BUFOFFC,EFFS                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         B     PUTX                                                             
*                                                                               
PUT300   CLI   QOPT2,C' '          SUMMARY REPORT?                              
         BE    PUT400                                                           
         CLI   QOPT2,C'S'          SUMMARY REPORT?                              
         BE    PUT400                                                           
*                                                                               
*              NAME SORT REPORT                                                 
*                                                                               
         MVI   BUFTYPE,BUFNMRP1                                                 
         MVI   BUFTTYPE,BUFTITEM                                                
         CLI   PROGPROF+4,C'3'     SORT BY NAME AT SUBDPT LVL                   
         BNE   *+14                                                             
         MVC   BUFKLNAM,EFFS                                                    
         B     PUT305                                                           
         MVC   BUFSDPT,EFFS                                                     
         CLI   PROGPROF+4,C'2'     SORT BY NAME AT DEPT LVL                     
         BE    PUT305                                                           
         MVC   BUFDPT,EFFS                                                      
PUT305   GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL PERSON TOTAL RECORD                                       
*                                                                               
         MVI   BUFNUM,X'FF'                                                     
         MVC   BUFDATE,EFFS                                                     
         MVI   BUFTTYPE,BUFTPERS                                                
         MVI   BUFTYPE,BUFNMRP1                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL DEPT TOTAL RECORD                                         
*                                                                               
         CLI   PROGPROF+4,C'1'     SORT ON NAME BY OFFICE LEVEL?                
         BE    PUT310              THEN DEPT TOTALS NOT APPLICABLE              
*                                                                               
         MVI   BUFTTYPE,BUFTDPT                                                 
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFPERSN,EFFS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL OFFICE TOTAL RECORD                                       
*                                                                               
PUT310   MVI   BUFTTYPE,BUFTOFFC                                                
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFDPT,EFFS                                                      
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFPERSN,EFFS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL REQUEST TOTAL RECORD                                      
*                                                                               
         MVI   BUFTTYPE,BUFTREQ                                                 
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFOFFC,EFFS                                                     
         MVI   BUFOFFC+11,X'FE'                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              DETAIL CONTROL TOTAL RECORD, REQUEST + ERRORS                    
*                                                                               
         MVI   BUFTTYPE,BUFTCNTL                                                
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFOFFC,EFFS                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         CLI   QOPT2,C'B'          WANT BOTH REPORTS?                           
         BNE   PUTX                                                             
*                                                                               
* NAME SORT SUMMARY REPORT                                                      
*                                                                               
PUT400   MVC   BUFKEY,WORK                                                      
         MVI   BUFTYPE,BUFNMRP2                                                 
         MVI   BUFTTYPE,BUFTITEM                                                
         MVI   BUFNUM,X'FF'                                                     
         MVC   BUFDATE,EFFS                                                     
         CLI   PROGPROF+4,C'3'     SORT BY NAME AT SUBDPT LVL                   
         BNE   *+14                                                             
         MVC   BUFKLNAM,EFFS                                                    
         B     PUT405                                                           
         MVC   BUFSDPT,EFFS                                                     
         CLI   PROGPROF+4,C'2'     SORT BY NAME AT DEPT LVL                     
         BE    PUT405                                                           
         MVC   BUFDPT,EFFS                                                      
PUT405   GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         CLI   PROGPROF+4,C'1'     SORT ON NAME BY OFFICE LEV                   
         BE    PUT410              THAN DON'T SHOW DEPT TOTALS                  
*                                                                               
*              SUMMARY DEPT TOTAL RECORD                                        
*                                                                               
         MVI   BUFTTYPE,BUFTDPT                                                 
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFPERSN,EFFS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY OFFICE TOTAL RECORD                                      
*                                                                               
PUT410   MVI   BUFTTYPE,BUFTOFFC                                                
         MVC   BUFPERSN,EFFS                                                    
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFSDPT,EFFS                                                     
         MVC   BUFDPT,EFFS                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY REQUEST TOTAL RECORD                                     
*                                                                               
         MVI   BUFTTYPE,BUFTREQ                                                 
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFOFFC,EFFS                                                     
         MVI   BUFOFFC+11,X'FE'                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*              SUMMARY CONTROL TOTAL RECORD, REQUEST + ERRORS                   
*                                                                               
         MVI   BUFTTYPE,BUFTCNTL                                                
         MVC   BUFKLNAM,EFFS                                                    
         MVC   BUFOFFC,EFFS                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
PUTX     XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BUILD PAYROLL DETAIL ELEMENT - X'86'                   *         
***********************************************************************         
*                                                                               
ELEM86   NMOD1 0,*ELEM86*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING PDEELD,R5                                                        
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   PDEEL,PDEELQ        X'86' ELEMENT                                
         MVI   PDELN,PDELNQ                                                     
         GOTO1 DATCON,DMCB,(0,BUFCKDT),(1,PDEDTE)                               
         MVC   PDENUM,BUFNUM       PAYROLL CODE NUMBER                          
         MVC   PDESTAT,BUFPSTA1                                                 
         MVC   PDESTAT2,BUFPSTAT   HRLY STATUS                                  
         ZAP   PDEAMT,BUFCKAMT                                                  
         ZAP   PDEADJ,=P'0'                                                     
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R5                                                               
***********************************************************************         
*              EXTRACT NAME FROM X'20' ELEMENT                        *         
***********************************************************************         
*                                                                               
GETNAME  NMOD1 0,*GETNAME                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R4,AIO1             R4=A(IO1 BUFFER)                             
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,NAMELQ       X'20'                                        
         BAS   RE,GETEL                                                         
         BNE   GETX                                                             
*                                                                               
         USING NAMELD,R4                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETX                                                             
         MVC   WORK(0),NAMEREC                                                  
         EX    R1,*-6                                                           
GETX     XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
*              VALIDATE PERSON LOCATION FOR CHECK DATE                          
***********************************************************************         
*                                                                               
VALDATE  NMOD1 0,*VALDTE*                                                       
         L     RC,0(R1)                                                         
*                                                                               
* FIRST SEE IF PAYCODE IS AN YTD ADJUSTMENT TYPE                                
*                                                                               
         NI    BIT,X'FF'-OUTLOCDT                                               
         USING PYCODED,R5                                                       
         L     R5,APYTABLE                                                      
VALDA    CLC   PYCODE,BUFCODE      PAYROLL CODE                                 
         BNE   VALDB                                                            
*                                                                               
         TM    PYSTAT,PYADJRTE     IS THIS AN ADJUSTMENT RATE TYPE              
         BZ    *+8                                                              
         OI    BIT,OUTLOCDT                                                     
*                                                                               
VALDB    CLI   0(R5),X'FF'                                                      
         BE    *+12                                                             
         LA    R5,PYLNQ(R5)        NEXT TABLE ENTRY                             
         B     VALDA                                                            
         DROP  R5                                                               
*                                                                               
         USING LOCELD,R4                                                        
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(0,BUFDATE),(10,WORK) MM/DD/YY                       
         GOTO1 DATVAL,DMCB,(X'00',WORK),WORK+10                                 
         CLI   DMCB+3,X'00'        VALID DATE?                                  
         BNE   *+12                                                             
         OI    BUFSTAT,BUFICKDT                                                 
         B     VALDNO                                                           
         GOTO1 DATCON,DMCB,(0,BUFDATE),(1,WORK)                                 
         L     R4,AIO1                                                          
         MVI   ELCODE,LOCELQ       X'83'                                        
         BAS   RE,GETEL                                                         
         BNE   VALDNO                                                           
         B     *+8                                                              
VALD10   BAS   RE,NEXTEL                                                        
         BNE   VALDNO                                                           
*                                                                               
         TM    BIT,OUTLOCDT        SPECIAL RULES FOR ADJ SALARY                 
         BO    VALD50                                                           
*                                                                               
         CLC   WORK(3),LOCSTART    DATE MUST NOT BE < START                     
         BL    VALD10                                                           
         OC    LOCSALKD,LOCSALKD   SALARY LOCK DATE                             
         BZ    VALD20                                                           
         CLC   WORK(3),LOCSALKD                                                 
         BH    VALD10              DATE MUST NOT BE > SAL LOCK                  
         B     VALD30                                                           
VALD20   OC    LOCEND,LOCEND                                                    
         BZ    VALD30                                                           
         CLC   WORK(3),LOCEND      OR END DATE                                  
         BH    VALD10                                                           
*                                                                               
VALD30   CLC   LOCOFF,BUFOFFC      MAKE SURE RIGHT LOCATION                     
         BNE   VALDNO                                                           
         CLC   LOCDEPT,BUFDPT                                                   
         BNE   VALDNO                                                           
         CLC   LOCSUB,BUFSDPT                                                   
         BNE   VALDNO                                                           
         B     VALDYES                                                          
*                                                                               
VALD50   CLC   WORK(3),LOCSTART    DATE MUST NOT BE < START                     
         BL    VALD10                                                           
*                                                                               
         CLC   LOCOFF,BUFOFFC      MAKE SURE RIGHT LOCATION                     
         BNE   VALD10                                                           
         CLC   LOCDEPT,BUFDPT                                                   
         BNE   VALD10                                                           
         CLC   LOCSUB,BUFSDPT                                                   
         BNE   VALD10                                                           
*                                                                               
VALD100  TM    BIT,OUTLOCDT        IS THIS AN ADJ RATE PAYTYPE                  
         BZ    VALD10              NO THEN TRY NEXT ELEM                        
*                                                                               
VALDYES  SR    RC,RC                                                            
VALDNO   LTR   RC,RC                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   BUILD ACTIVITY ELEMENT 'F1'   (FILL IN WHAT I CAN)                *         
***********************************************************************         
*                                                                               
ELEMF1   NMOD1 0,*ELEMF1*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING ACTVD,R5                                                         
         LA    R5,ELEM2                                                         
         XC    ELEM2,ELEM2                                                      
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,ACTVLENQ                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,ACTVADDT) DATE ADDED                        
         MVC   ACTVCHDT,ACTVADDT              DATE CHANGED                      
         MVC   ACTVSCID,=C'**TAPE**'          FROM SALARY TAPE                  
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT HEADLINES                                                  
***********************************************************************         
*                                                                               
PRNTHED  NMOD1 0,*PRNTHD*                                                       
         L     RC,0(R1)            RESTORE RC                                   
*                                                                               
         MVC   XHEAD2+12(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+21(L'CMPNAME),CMPNAME                                     
*                                                                               
         DROP  R6                                                               
         USING HEDLINED,R6                                                      
         LA    R6,XP                                                            
*                                                                               
         TM    BIT,PGBRKUP         IF PAGE BREAK DON'T SKIP A LINE              
         BO    PRNT05                                                           
         GOTO1 ACREPORT                                                         
*                                                                               
PRNT05   CLI   PROGPROF+2,2                   PAGE BREAK ON DEPT?               
         BE    PRNT10              THEN ONLY NEED TO SHOW CHA OF SUB            
         CLI   PROGPROF+4,C'2'                                                  
         BE    PRNT10                                                           
*                                                                               
*        MVC   HED1NME(L'LEVELANM),LEVELANM        OFFICE                       
*        MVC   HED1OFF(6),BUFOFFC                                               
*        MVC   HED1DSC(L'BUFNMLV1),BUFNMLV1                                     
*        GOTO1 ACREPORT                                                         
         MVC   HED2NME(L'LEVELBNM),LEVELBNM        DEPT                         
         MVC   HED2DPT(6),BUFDPT                                                
         MVC   HED2DSC(L'BUFNMLV2),BUFNMLV2                                     
*                                                                               
PRNT10   GOTO1 ACREPORT                                                         
         MVC   HED3NME(L'LEVELCNM),LEVELCNM        SUBDEPT                      
         MVC   HED3SUB(6),BUFSDPT                                               
         MVC   HED3DSC(L'BUFNMLV3),BUFNMLV3                                     
*                                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R6                                                               
***********************************************************************         
*              BUILD BUFFALO RECORD                                   *         
***********************************************************************         
*                                                                               
BUILDBUF NMOD1 0,*BLDBUF*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING TAPED,R3                                                         
         L     R3,ALSTWRK                                                       
*                                                                               
         LA    R0,BUFREC           CLEAR BUFFALO RECORD AREA                    
         LA    R1,BUFLNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   BUFTYPE,BUFERROR    ASSUME ITEM WILL HAVE AN ERROR               
         MVC   BUFDATA,TAPEREC                                                  
         MVC   BUFDATE,TAPECKDT    CHECK DATE IN BUF KEY                        
         ZAP   BUFACCUM,=P'1'                                                   
*        PACK  DUB(5),TAPEAMNT(9)  CHECK AMOUNT                                 
         GOTO1 EDPACK,DMCB,(RC),(L'DUB,DUB),(L'TAPEAMNT,TAPEAMNT)               
         ZAP   BUFCKAMT,DUB(8)                                                  
         CLI   QOPT3,C'Z'                                                       
         BNE   *+10                                                             
         ZAP   BUFCKAMT,=P'0'      ZERO OUT ALL AMOUNTS                         
         MVC   BUFKLNAM,SPACES     ZERO OUT LAST NAME IN KEY                    
         TM    BIT,SORTPROF        SORTING BY LAST NAME                         
         BZ    *+10                                                             
         MVC   BUFKLNAM,TAPELNAM   LAST NAME                                    
         MVC   BUFPSTA1,TAPESTAT   MOVE IN STATUS BYTE                          
*                                                                               
         LA    RF,TAPEACT          ISOLATE OFFICE                               
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         MVC   BUFOFFC(0),0(RF)                                                 
         EX    R1,*-6                                                           
         OC    BUFOFFC,SPACES                                                   
*                                                                               
         LA    RF,TAPEACT          ISOLATE DEPARTMENT                           
         ZIC   R0,LEVELA                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   BUFDPT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         OC    BUFDPT,SPACES                                                    
*                                                                               
         LA    RF,TAPEACT          ISOLATE SUBDEPARTMENT                        
         ZIC   R0,LEVELB                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   BUFSDPT(0),0(RF)                                                 
         EX    R1,*-6                                                           
         OC    BUFSDPT,SPACES                                                   
*                                                                               
         LA    RF,TAPEACT          ISOLATE PERSON                               
         ZIC   R0,LEVELC                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   BUFPERSN(0),0(RF)                                                
         EX    R1,*-6                                                           
         OC    BUFPERSN,SPACES                                                  
*                                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET A COMBINED SORT RECORD                                  
*        ON EXIT, ALSTWRK HAS ADDR OF RECORD                                    
***********************************************************************         
*                                                                               
         USING TAPED,R3                                                         
GETSORT  NMOD1 0,*GETSORT                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R3,AIOTAPE                                                       
         L     R4,ALSTWRK                                                       
         OC    ALSORT,ALSORT        1ST TIME THRU?                              
         BNZ   GETS00                                                           
         MVC   TAPEREC,SPACES       CLEAR BOTH AREAS                            
*        MVI   TAPESTAT,0                                                       
         MVC   TAPEREC-TAPED(TAPELNQ,R4),SPACES                                 
*        MVI   TAPESTAT-TAPED(R4),0                                             
*                                                                               
GETS00   MVC   0(TAPELNQ,R4),TAPED                                              
GETS02   GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,4(R1)                                                         
         ST    R2,ALSORT           ADDRESS OF LAST SORT REC                     
         LTR   R2,R2                                                            
         BZ    GETSX               SORT IS DONE                                 
*                                                                               
         MVC   TAPED(TAPELNQ),0(R2)               SAVE CURRENT SRT REC          
         CLC   TAPEACT-TAPED(L'TAPEACT,R4),SPACES DO I HAVE ONE SAVED?          
         BE    GETS00                             NO - GO SAVE                  
*                                                                               
         CLC   TAPEACT,TAPEACT-TAPED(R4)          COMPARE                       
         BNE   GETSX                                                            
         CLC   TAPECKDT(L'TAPECKDT+L'TAPECODE),TAPECKDT-TAPED(R4)               
         BNE   GETSX                                                            
*                                                                               
*        PACK  DUB,TAPEAMNT                                                     
         GOTO1 EDPACK,DMCB,(RC),(L'DUB,DUB),(L'TAPEAMNT,TAPEAMNT)               
*        ZAP   DUB,DUB                                                          
*        PACK  DUB2,TAPEAMNT-TAPED(L'TAPEAMNT,R4)                               
         LA    RF,TAPEAMNT-TAPED(R4)                                            
         GOTO1 EDPACK,DMCB,(RC),(L'DUB2,DUB2),(L'TAPEAMNT,(RF))                 
         AP    DUB2,DUB                           ADD 'EM                       
         UNPK  TAPEAMNT-TAPED(L'TAPEAMNT,R4),DUB2                               
         B     GETS02                             AND GET THE NEXT ONE          
*                                                                               
GETSX    XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R3                                                               
***********************************************************************         
*              CHECK FOR REVERSAL CODE BY SALARY TYPE                 *         
***********************************************************************         
*                                                                               
*        IF PAYTYPE HAS AN ASSOCIATED REVERSAL TYPE THEN MAKE AN                
*        ADDITIONAL POSTING TO NEXT MONTH FOR THE REVERSAL TYPE WITH            
*        AN OPPOSITE AMOUNT.                                                    
*                                                                               
REVERSAL NMOD1 0,*REVERS*                                                       
         L     RC,0(R1)            RESET RC                                     
         USING TAPED,R3                                                         
         L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
*                                                                               
         USING PYCODED,R5                                                       
         L     R5,APYTABLE                                                      
REV100   CLC   PYCODE,TAPECODE     PAYROLL CODE                                 
         BE    REV200                                                           
         CLI   0(R5),X'FF'                                                      
         BE    REVNO                                                            
         LA    R5,PYLNQ(R5)        NEXT TABLE ENTRY                             
         B     REV100                                                           
*                                                                               
REV200   CLC   PYREV,SPACES                                                     
         BNH   REVNO                                                            
         MVC   TAPECKDT+4(2),=C'01'                                             
         GOTO1 ADDAY,DMCB,TAPECKDT,TAPECKDT,F'31'                               
         MVC   TAPECKDT+4(2),=C'01'                                             
         MVC   TAPECODE,PYREV                                                   
*        PACK  DUB(8),TAPEAMNT(9)                                               
         GOTO1 EDPACK,DMCB,(RC),(8,DUB),(L'TAPEAMNT,TAPEAMNT)                   
         MP    DUB(8),=P'-1'                                                    
         UNPK  TAPEAMNT(9),DUB(8)                                               
         OI    TAPESTAT,TAPEREV                                                 
*                                                                               
REVYES   DS    0H                                                               
         AP    COUNTREV,=P'1'      KEEP REVERSAL CNT AS PER PSHA 4/97           
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         CR    RB,RB               SET CC=EQ                                    
         B     REVX                                                             
*                                                                               
REVNO    LTR   RB,RB               SET CC=NE                                    
*                                                                               
REVX     XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*        FIX ACCOUNT (FOR BBDO) -READ PERSON REC & VAL ACCNT FOR SUBDPT         
*        REPLACE INTN'L OFFICE CODE WITH DDS OFFICE CODE FOR BBDO               
*        FIX KEY FOR OGILVY                                                     
***********************************************************************         
*                                                                               
CONVERS  NMOD1 0,*CONVERS                                                       
         L     RC,0(R1)            RESET RC                                     
*                                                                               
*** REMOVED THIS CALL BECAUSE OGILVY CHANGED THEIR TAPE FORMAT ***              
*        BAS   RE,OGILVY           FIX KEY FOR OGILVY                           
*** REMOVED THIS CALL BECAUSE OGILVY CHANGED THEIR TAPE FORMAT ***              
*                                                                               
         BAS   RE,CHAACCNT              CHANGE PARTS OF THE 1R                  
*                                                                               
         USING TAPED,R3                                                         
         L     R3,AIOTAPE                                                       
         USING PERRECD,R4                                                       
         L     R4,AIO2                                                          
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,QCOMPANY                                                 
         LA    RF,TAPEACT                                                       
         ZIC   R0,LEVELC                                                        
         AR    RF,R0               GET TO PERSON LEVEL                          
         ZIC   R1,LEVELD                                                        
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,PERKCODE,0(RF)                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO2,AIO2                             
         CLI   DMCB+8,0                                                         
         BE    *+8                                                              
         B     CONVX               LET VALDATE FILL IN ANY ERRORS               
*                                                                               
         USING LOCELD,R4                                                        
         GOTO1 DATCON,DMCB,(0,TAPECKDT),(1,WORK)                                
         L     R4,AIO2                                                          
         MVI   ELCODE,LOCELQ       X'83'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CONV10   BAS   RE,NEXTEL                                                        
         BNE   CONVX               LET VALDATE FILL IN ANY ERRORS               
*                                                                               
         CLC   WORK(3),LOCSTART    DATE MUST NOT BE < START                     
         BL    CONV10                                                           
         OC    LOCSALKD,LOCSALKD   SALARY LOCK DATE                             
         BZ    CONV20                                                           
         CLC   WORK(3),LOCSALKD                                                 
         BH    CONV10             DATE MUST NOT BE > SAL LOCK                   
         B     CONV30                                                           
CONV20   OC    LOCEND,LOCEND                                                    
         BZ    CONV30                                                           
         CLC   WORK(3),LOCEND      OR END DATE                                  
         BH    CONV10                                                           
*                                                                               
CONV30   DS    0H                                                               
         CLI   PROGPROF+3,C'P'     VALIDATE AT JUST PERSON                      
         BE    CONV100                                                          
         LA    RF,TAPEACT          ACCOUNT FROM TAPE                            
         ZIC   R1,LEVELA                                                        
         BCTR  R1,0                                                             
         EXCLC R1,LOCOFF,0(RF)                                                  
         BNE   CONVX                                                            
         CLI   PROGPROF+3,C'1'     VALIDATE AT OFFICE LEVEL                     
         BE    CONV100             YES SO DONE                                  
*                                                                               
         LA    RF,TAPEACT                                                       
         ZIC   R0,LEVELA                                                        
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         AR    RF,R0                                                            
         BCTR  R1,0                                                             
         EXCLC R1,LOCDEPT,0(RF)                                                 
         BNE   CONVX                                                            
         CLI   PROGPROF+3,C'2'     VALIDATE AT DEPT LEVEL                       
         BE    CONV100             YES SO DONE                                  
*                                                                               
         LA    RF,TAPEACT                                                       
         ZIC   R0,LEVELB                                                        
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         AR    RF,R0                                                            
         BCTR  R1,0                                                             
         EXCLC R1,LOCSUB,0(RF)     MUST BE VALIDATE AT SUBDPT LEVEL             
         BNE   CONVX                                                            
*                                                                               
CONV100  DS    0H                                                               
         LA    RF,TAPEACT                                                       
         ZIC   R1,LEVELA           LENGTH TO MOVE                               
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),LOCOFF     MOVE IN OFFICE                               
         ZIC   R0,LEVELA                                                        
         ZIC   R1,LEVELB                                                        
         SR    R1,R0               LENGTH TO MOVE                               
         AR    RF,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),LOCDEPT    MOVE IN DEPT                                 
         LA    RF,TAPEACT                                                       
         ZIC   R0,LEVELB                                                        
         ZIC   R1,LEVELC                                                        
         SR    R1,R0               LENGTH TO MOVE                               
         AR    RF,R0               POSITION R3 TO SUBDEPT IN ACCOUNT            
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),LOCSUB     AND MOVE IN FROM LOCATION ELEM               
*                                                                               
CONVX    XMOD1                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              FIX KEY FOR OGILVY                                     *         
***********************************************************************         
*                                                                               
OGILVY   NTR1                                                                   
         CLI   QCOMPANY,X'B3'                                                   
         BE    *+12                                                             
         CLI   QCOMPANY,X'C3'                                                   
         BNE   OGX                                                              
*                                                                               
         USING TAPED,R3                                                         
         L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(7),TAPEACT+5                                                
         MVC   WORK+7(5),TAPEACT                                                
         MVC   TAPEACT,WORK                                                     
         B     OGX                                                              
*                                                                               
OGX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        CONVERT PARTS OF THE 1R ACCOUNT BASED ON INFO IN TABLE                 
***********************************************************************         
*                                                                               
CHAACCNT NTR1                                                                   
*                                                                               
         USING TAPED,R3                                                         
         L     R3,AIOTAPE                                                       
         USING CONVTABD,R4                                                      
         L     R4,ACONVTAB                                                      
*                                                                               
CHA05    LA    R3,TAPEACT          POINT TO ACCOUNT                             
CHA10    CLI   0(R4),X'FF'                                                      
         BE    CHAX                                                             
*                                                                               
         CLC   COCOMP,RCCOMPFL     MATCH ON COMPANY                             
         BE    CHA20                                                            
         LA    R4,COLNQ(R4)       BUMP TABLE                                    
         B     CHA10                                                            
*                                                                               
CHA20    LA    RE,COENTRY                                                       
         USING COENTRY,RE                                                       
         ZIC   RF,CODISPC          DISPL INTO ACCOUNT FOR COMPARE               
         AR    R3,RF                                                            
         ZIC   R0,CONENT           NUMBER OF ENTRIES                            
         LA    RF,LEVELS           POINT TO RIGHT LEVEL TO                      
         ZIC   R1,COCLENC          PICK UP LENGTH OF COMPARE                    
         AR    RF,R1                                                            
         ZIC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
CHA30    EXCLC R1,0(R3),COOLDACC                                                
         BE    CHA40               EQUAL-MAKE THE SWITCH                        
         LA    RE,COELNQ(RE)       BUMP TO NEXT ACCOUNT ENTRY                   
         BCT   R0,CHA30                                                         
         LA    R4,COLNQ(R4)                                                     
         B     CHA05                                                            
*                                                                               
CHA40    LA    R3,TAPEACT                                                       
         ZIC   RF,CODISPM          DISPLACEMENT INTO ACC FOR MOVE               
         AR    R3,RF                                                            
         LA    RF,LEVELS           POINT TO RIGHT LEVEL TO                      
         ZIC   R1,COCLENM          PICK UP LENGTH OF MOVE                       
         AR    RF,R1                                                            
         ZIC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),CONEWACC    MOVE IN NEW ACC PIECE                       
         LA    R4,COLNQ(R4)       BUMP TABLE                                    
         B     CHA05               ANY MORE                                     
*                                                                               
CHAX     XIT1                                                                   
         DROP  R3,R4,RE                                                         
         EJECT                                                                  
***********************************************************************         
*              CHECK BENEFIT PERCENTS                                 *         
***********************************************************************         
*                                                                               
*                                                                               
BENEFIT  NMOD1 0,*BENEFIT                                                       
         L     RC,0(R1)            RESTORE RC                                   
         USING TAPED,R3                                                         
         L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
         CLI   PROGPROF+1,C'Y'     PROFILE SET FOR BEN LOOKUP?                  
         BE    *+12                                                             
         CLI   QOPT4,C'B'          LOOKUP AND POST PROFILE RATES?               
         BNE   *+12                                                             
         BAS   RE,BPCT                                                          
         B     BENX                                                             
*                                                                               
         USING BETABD,R5                                                        
         L     R5,ABETAB                                                        
BEN100   CLI   BECOMP,X'FF'        END OF TABLE                                 
         BE    BENNO                                                            
         CLC   BECOMP,RCCOMPFL     MATCH COMPANY                                
         BNE   BEN104                                                           
         SR    R1,R1                                                            
         IC    R1,BENUM            NUMBER OF PAYTYPES                           
         LA    RE,BEENTRY                                                       
         USING BEENTRY,RE                                                       
BEN102   CLC   BEPCODE,TAPECODE                                                 
         BE    BEN200                                                           
         LA    RE,BEELNQ(RE)       NEXT PAY TYPE                                
         BCT   R1,BEN102                                                        
BEN104   SR    R1,R1                                                            
         ICM   R1,3,BELEN          LENGTH OF THE ENTRY                          
         AR    R5,R1                                                            
         B     BEN100                                                           
*                                                                               
BEN200   MVC   TAPECODE,BEBCODE    BENEFIT PAY TYPE                             
*        PACK  DUB,TAPEAMNT                                                     
         GOTO1 EDPACK,DMCB,(RC),(L'DUB,DUB),(L'TAPEAMNT,TAPEAMNT)               
*        ZAP   DUB,DUB             CHANGE X'F' SIGN TO X'C'                     
         ZAP   DIVWRK,BEPCT        BENEFIT PERCENTAGE                           
         MP    DIVWRK,DUB                                                       
         SRP   DIVWRK,64-5,5       SHIFT 5 (3 FOR PCT + 2 FOR $'S)              
         ZAP   DUB,DIVWRK                                                       
         UNPK  TAPEAMNT,DUB                                                     
*                                                                               
BENYES   DS    0H                                                               
         AP    COUNTBEN,=P'1'      KEEP BENEFITS CNT AS PER PSHA 4/97           
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         CR    RB,RB               SET CC=EQ                                    
         B     BENX                                                             
*                                                                               
BENNO    LTR   RB,RB               SET CC=NE                                    
         B     BENX                                                             
BENX     XMOD1                                                                  
         DROP  R3,R5,RE                                                         
         EJECT                                                                  
***********************************************************************         
*              READ COST PROFILES FOR PERCENTS                        *         
***********************************************************************         
*                                                                               
*                                                                               
BPCT     NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
         L     R6,ACOBLOCK                                                      
         USING COBLOCKD,R6                                                      
         CLC   SVACT,TAPEACT       IF SAME PERSON YOU HAVE PROFILE              
         BE    BPCT02                                                           
*                                                                               
         MVC   COKOFC,SPACES                                                    
         LA    RF,TAPEACT          ISOLATE OFFICE                               
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         MVC   COKOFC(0),0(RF)                                                  
         EX    R1,*-6                                                           
         OC    COKOFC,SPACES                                                    
*                                                                               
         MVC   COKDPT,SPACES                                                    
         LA    RF,TAPEACT          ISOLATE DEPARTMENT                           
         ZIC   R0,LEVELA                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   COKDPT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         OC    COKDPT,SPACES                                                    
*                                                                               
         MVC   COKSDT,SPACES                                                    
         LA    RF,TAPEACT          ISOLATE SUBDEPARTMENT                        
         ZIC   R0,LEVELB                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   COKSDT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         OC    COKSDT,SPACES                                                    
*                                                                               
         MVC   COKPER,SPACES                                                    
         LA    RF,TAPEACT          ISOLATE PERSON                               
         ZIC   R0,LEVELC                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         MVC   COKPER(0),0(RF)                                                  
         EX    R1,*-6                                                           
         OC    COKPER,SPACES                                                    
*                                                                               
         GOTO1 GETCAP,DMCB,ACOBLOCK                                             
         OC    COSTATUS,COSTATUS                                                
         BZ    *+14                                                             
         TM    COSTATUS,COLEVMIS   HIGHER LEVELS MISSING?                       
         BO    BPCTX               YES-DON'T DIE                                
         DC    H'0'                                                             
         MVC   SVACT,TAPEACT       SAVE CURRENT PERSON                          
BPCT02   CP    COPC1,=P'0'         ANY BENEFIT PCTS?                            
         BNE   *+14                                                             
         CP    COPC2,=P'0'                                                      
         BE    BPCTX                                                            
*                                                                               
         USING PYCODED,R5                                                       
         L     R5,APYTABLE                                                      
BPCT04   CLI   PYCODE,X'FF'                                                     
         BE    BPCTX                                                            
         CLC   PYCODE,TAPECODE     PAYROLL CODE                                 
         BE    *+12                                                             
         LA    R5,PYLNQ(R5)        NEXT TABLE ENTRY                             
         B     BPCT04                                                           
*                                  SAVE ORIGINAL TAPE AMNT IN DUB2              
         GOTO1 EDPACK,DMCB,(RC),(8,DUB2),(L'TAPEAMNT,TAPEAMNT)                  
         CLC   PYCODPC1,SPACES     HAVE A BEN PCT 1?                            
         BNH   BPCT06                                                           
         MVC   TAPECODE,PYCODPC1                                                
         ZAP   DUB,COPC1                                                        
         BAS   RE,BPCTUP                                                        
BPCT06   CLC   PYCODPC2,SPACES     HAVE A BEN PCT 2?                            
         BNH   BPCTX                                                            
         MVC   TAPECODE,PYCODPC2                                                
         ZAP   DUB,COPC2                                                        
         BAS   RE,BPCTUP                                                        
BPCTX    XIT1                                                                   
*                                                                               
*                                                                               
BPCTUP   NTR1                                                                   
         DS    0H                                                               
*        ST    RE,SVRE                                                          
         ZAP   DIVWRK,DUB                                                       
         MP    DIVWRK,DUB2                                                      
         CP    DIVWRK,=P'0'                                                     
         BE    BPCTUPX                                                          
         SRP   DIVWRK,64-6,5       SHIFT AND ROUND                              
         ZAP   DUB,DIVWRK                                                       
         UNPK  TAPEAMNT,DUB                                                     
         AP    COUNTPCT,=P'1'      KEEP PERCENTS CNT AS PER PSHA 4/97           
         CLC   PYCODPC1,SPACES     HAVE A BEN PCT 1?                            
         BNH   *+12                                                             
         OI    TAPESTAT,PDEPC1     TURN ON PC1 BIT FOR SORTER                   
         B     BPCTUP5                                                          
         CLC   PYCODPC2,SPACES     HAVE A BEN PCT 2?                            
         BH    *+6                                                              
         DC    H'0'                IF WE GET HERE ONE MUST BE SET               
         OI    TAPESTAT,PDEPC2     TURN ON PC2 BIT FOR SORTER                   
BPCTUP5  GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
BPCTUPX  XIT1                                                                   
                                                                                
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        PACK NUMBERS IF NEGATIVE OR POSTIVE                          *         
*        P1   = L'FIELD TO PACK TO                                    *         
*             = A(FIELD TO PACK TO)                                   *         
*        P2   = L'FIELD TO PACK FROM                                  *         
*             = A(FIELD TO PACK FROM)                                 *         
***********************************************************************         
*                                                                               
EDPACK   NMOD1 0,*EDPACK*                                                       
         L     RC,0(R1)            RESTORE RC                                   
*                                                                               
         L     R5,4(R1)            PACK FIELD FOR ANSWER                        
         SR    R4,R4                                                            
         ICM   R4,1,4(R1)          LENGTH OF FIELD                              
         BNZ   *+6                                                              
         DC    H'00'               NO LENGTH SUPPLIED                           
         SH    R4,=H'01'           REDUCE BY ONE FOR EXECUTE                    
         SLL   R4,4                SHIFT TO HOB                                 
*                                                                               
         L     R3,8(R1)            FIELD TO PACK FROM                           
         SR    R2,R2                                                            
         ICM   R2,1,8(R1)          GET LENGTH OF FIELD                          
         BNZ   *+6                                                              
         DC    H'00'               NO LENGTH SUPPLIED                           
*                                                                               
         MVI   SIGN,0              RESET FLAG SIGN                              
         LR    RE,R3               RE-LOAD START OF FIELD                       
         LR    RF,R2               LENGTH OF FIELD                              
         CLI   0(RE),C'-'          CHECK LEADING MINUS SIGN                     
         BE    EDPACK3                                                          
         LA    RE,1(RE)            LEADING MINUS SIGN                           
         BCT   RF,*-12                                                          
         LR    RF,R2               RESTORE LENGTH                               
         LR    RE,R3               RE-STORE ADDRESS                             
         B     EDPACK5             NO NEGATIVE                                  
*                                                                               
EDPACK3  LA    RE,1(RE)            BUMP PAST MINUS SIGN                         
         OI    SIGN,NEGLEAD        LEADING MINUS SIGN                           
         SH    RF,=H'01'           LESS ONE FOR MINUS SIGN                      
         BNZ   EDPACK5                                                          
         LR    RF,R2               RESTORE LENGTH                               
         NI    SIGN,TURNOFF-NEGLEAD                                             
         OI    SIGN,NEGTRAIL       NO TRIALING                                  
         SH    RF,=H'01'           LESS ONE FOR MINUS SIGN                      
         LR    RE,R3               RESTORE ADDRESS                              
*                                                                               
EDPACK5  SH    RF,=H'01'           REDUCE FOR EXECUTE                           
         OR    RF,R4               RF = HOB(L'FIELD 1) LOB(L'FIELD 2)           
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  0(0,R5),0(0,RE)                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         TP    0(0,R5)             TEST IF PACKED VALUE                         
         BZ    EDPACK6                                                          
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
         L     RE,ALSTWRK                                                       
         LA    RF,TAPECKDT-TAPEREC                                              
         SHI   RF,1                                                             
         MVC   PLINSTAF(0),0(RE)          MOVE IN ACCOUNT & NAME                
         EX    RF,*-6                                                           
         MVC   PLINCKDT,=CL8'********'                                          
         MVC   PLINCODE(23),=CL23'DISPACEMENT MAY BE OFF'                       
         MVC   PLINAMNT,=CL14'****INVALID***'                                   
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
EDPACK6  LR    RF,R4                                                            
         SRL   RF,4                SHIFT TO LOB                                 
         OR    RF,R4               POINT TO END-1                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R5),0(0,R5)     CHANGE X'0F' SIGN TO X'0C'                   
         TM    SIGN,NEGTRAIL+NEGLEAD                                            
         BZ    EDX                                                              
         SRL   RF,4                CLEAR LOB                                    
         SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MP    0(0,R5),=P'-1'      MAKE NEGATIVE                                
*                                                                               
EDX      XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*              BOX HOOK                                               *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXCOLS,C'L'                                                     
*                                                                               
         CLI   RCSUBPRG,0          REGULAR REPORT BOXES                         
         BNE   BX100                                                            
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+66,C'C'                                                  
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+109,C'R'                                                 
         B     BX500                                                            
*                                                                               
BX100    CLI   RCSUBPRG,1          PERSON REPORT BOXES                          
         BNE   BX200                                                            
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+109,C'R'                                                 
         B     BX500                                                            
*                                                                               
BX200    CLI   RCSUBPRG,2          ERROR REPORT BOXES                           
         BNE   BX300                                                            
         MVI   BOXCOLS+22,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+66,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+91,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+101,C'C'                                                 
         MVI   BOXCOLS+106,C'C'                                                 
         MVI   BOXCOLS+111,C'C'                                                 
         MVI   BOXCOLS+116,C'C'                                                 
         MVI   BOXCOLS+121,C'R'                                                 
         B     BX500                                                            
*                                                                               
BX300    CLI   RCSUBPRG,3          RECAP BOXES                                  
         BNE   BX500                                                            
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+55,C'R'                                                  
*                                                                               
BX500    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BUFFERS                                                *         
***********************************************************************         
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
MAXACC   EQU   *-IO1                                                            
*                                                                               
IO2      DS    0D                  IOAREA #2                                    
         DS    2000C                                                            
*                                                                               
IOTAPE   DS    0D                  IO AREA FOR IN-TAPE                          
         DS    500C                                                             
*                                                                               
LSTWRK   DS    0D                  IO AREA FOR IN-TAPE                          
         DS    500C                                                             
*                                                                               
PYTABLE  DS    0D                  PAYCODE TABLE                                
         DS    5850C               MAX 150 ENTRIES                              
         DS    0D                  ALIGNMENT                                    
CCOBLOCK DS    CL(COBLOCKX-COBLOCK)   COBLOCK FOR GETCAP                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLE DSECT                                    *         
***********************************************************************         
*                                                                               
PYCODED  DSECT                                                                  
PYNUM    DS    CL1                 PAYROLL NUMBER                               
PYCODE   DS    CL5                 PAYROLL CODE                                 
PYDESC   DS    CL15                DESCRIPTION                                  
PYREV    DS    CL5                 REVERSAL CODE                                
PYNUMPC1 DS    CL1                 PAYROLL NUMBER OF PC1 PAY CODE               
PYCODPC1 DS    CL5                 PAYROLL CODE OF PC1 PAY CODE                 
PYNUMPC2 DS    CL1                 PAYROLL NUMBER OF PC2 PAY CODE               
PYCODPC2 DS    CL5                 PAYROLL CODE OF PC2 PAY CODE                 
PYSTAT   DS    XL1                 STATUS                                       
PYHRTE   EQU   PAYSHRTE            X'80'FROM PAYELX'84'                         
PYADJRTE EQU   PAYADJRT            X'40'FROM PAYELX'84'                         
PYLNQ    EQU   *-PYCODED                                                        
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLE DSECT                                    *         
***********************************************************************         
*                                                                               
ELTABD   DSECT                                                                  
ELTRECRD DS    XL1                 RECORD TYPE                                  
ELTADDR  DS    AL4                 A(ELEMENT ROUTINE)                           
ELTACTN  DS    XL1                 ACTION                                       
ELTABQ   EQU   *-ELTABD                                                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*              BENEFIT TABLE DSECT                                    *         
***********************************************************************         
*                                                                               
BETABD   DSECT                                                                  
BECOMP   DS    XL1                 COMPANY CODE                                 
BELEN    DS    AL2                 LENGTH OF THE ENTRY                          
BEBCODE  DS    CL5                 BENEFIT PAY CODE                             
BEPCT    DS    PL4                 BENEFIT PERCENT                              
BENUM    DS    AL1                 NUMBER OF CODES IN LIST                      
BEENTRY  EQU   *                                                                
BEPCODE  DS    CL5                 PAYCODES                                     
BEELNQ   EQU   *-BEENTRY                                                        
         EJECT                                                                  
***********************************************************************         
*              CONVERSION TABLE DSECT                                           
***********************************************************************         
CONVTABD DSECT                                                                  
COCOMP   DS    XL1                 COMPANY CODE                                 
CODISPC  DS    XL1                 DISPLACEMENT INTO ACC FOR COMPARE            
COCLENC  DS    XL1                 DISPL INTO LEVELS FOR COMPARE LEN            
CODISPM  DS    XL1                 DISPLACEMENT INTO ACC FOR MOVE               
COCLENM  DS    XL1                 DISPL INTO LEVELS FOR MOVE LEN               
CONENT   DS    XL1                 NUMBER OF ENTRIES                            
COENTRY  EQU   *                                                                
COOLDACC DS    CL(L'TAPEACT)       OLD ACCOUNT                                  
CONEWACC DS    CL(L'TAPEACT)       NEW ACCOUNT                                  
COELNQ   EQU   *-COENTRY           LENGTH OF ACCOUNT ENTRY                      
COLNQ    EQU   *-COCOMP            LENGTH OF TABLE ENTRY                        
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
* DETAIL OR PERSON REPORT LAYOUT                                                
*                                                                               
PLINED   DSECT                                                                  
PLIN     DS    0C                  DETAIL REPORT                                
         DS    CL2                                                              
PLINSTAF DS    CL8                 EMPLOYEE NUMBER                              
         DS    CL2                                                              
PLINNAME DS    0CL42               NAME (LAST, FIRST M.)                        
PLINTOT  DS    CL42                TOTAL FIELD                                  
         DS    CL3                                                              
PLINCKDT DS    CL8                 CHECK DATE YYMMDD                            
         DS    CL3                                                              
PLINCODE DS    CL5                 PAY TYPE                                     
         DS    CL3                                                              
PLINDESC DS    CL15                PAY CODE DESCRIPTION                         
         DS    CL3                                                              
PLINAMNT DS    0CL14               CHECK AMOUNT                                 
PLINTOT$ DS    CL14                TOTAL AMOUNT                                 
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
* ERROR REPORT LAYOUT                                                           
*                                                                               
         ORG   PLIN                                                             
         DS    CL2                                                              
PERRACT  DS    CL15                OF/DEPT/SDPT/PERSON                          
         DS    CL7                                                              
PERRNAME DS    CL30                NAME (LAST, FIRST M.)                        
         DS    CL3                                                              
PERRCKDT DS    CL8                 CHECK DATE YYMMDD                            
         DS    CL3                                                              
PERRPAYC DS    CL5                 PAY TYPE                                     
         DS    CL3                                                              
PERRAMNT DS    CL14                CHECK AMOUNT                                 
         DS    CL3                                                              
PERROFFC DS    CL1                                                              
         DS    CL4                                                              
PERRDPT  DS    CL1                                                              
         DS    CL4                                                              
PERRSDPT DS    CL1                                                              
         DS    CL4                                                              
PERRSTAF DS    CL1                                                              
         DS    CL4                                                              
PERRPERS DS    CL1                                                              
         DS    CL4                                                              
PERRCODE DS    CL1                                                              
*                                                                               
* SORT DETAIL REPORT                                                            
*                                                                               
         ORG   PLIN                                                             
         DS    CL2                                                              
PSTSTAFF DS    CL7                 EMPLOYEE NUMBER                              
         DS    CL6                                                              
PSTNAME  DS    0CL39                                                            
         ORG   PSTSTAFF                                                         
PSTSTAF2 DS    CL9                 SUBDPT/EMPLOYEE NUMBER                       
         DS    CL4                                                              
PSTNAME2 DS    0CL39                                                            
         ORG   PSTSTAFF                                                         
PSTSTAF3 DS    CL11                DEPT/SUBDPT/EMPLOYEE #                       
         DS    CL2                                                              
PSTNAME3 DS    0CL39                                                            
PSTTOT   DS    CL39                                                             
         DS    CL3                                                              
PSTCKDT  DS    CL8                                                              
         DS    CL3                                                              
PSTCODE  DS    CL5                                                              
         DS    CL3                                                              
PSTDESC  DS    CL15                                                             
         DS    CL3                                                              
PSTAMNT  DS    0CL14                                                            
PSTTOT$  DS    CL14                                                             
*                                                                               
* RECAP REPORT LAYOUT                                                           
*                                                                               
         ORG   PLIN                                                             
         DS    CL2                                                              
PXDESC   DS    CL35                                                             
         DS    CL10                                                             
PXCOUNT  DS    CL7                                                              
         EJECT                                                                  
***********************************************************************         
*              HEADLINE DSECT                                                   
***********************************************************************         
HEDLINED DSECT                                                                  
         DS    CL5                                                              
HED1NME  DS    CL(L'LEVELANM)                                                   
         DS    CL2                                                              
HED1OFF  DS    CL6                                                              
         DS    CL3                                                              
HED1DSC  DS    CL(L'BUFNMLV1)                                                   
         ORG   HED1NME                                                          
HED2NME  DS    CL(L'LEVELBNM)                                                   
         DS    CL2                                                              
HED2DPT  DS    CL6                                                              
         DS    CL3                                                              
HED2DSC  DS    CL(L'BUFNMLV2)                                                   
         ORG   HED1NME                                                          
HED3NME  DS    CL(L'LEVELCNM)                                                   
         DS    CL2                                                              
HED3SUB  DS    CL6                                                              
         DS    CL3                                                              
HED3DSC  DS    CL(L'BUFNMLV3)                                                   
         EJECT                                                                  
***********************************************************************         
*              INPUT TAPE DSECT                                       *         
***********************************************************************         
*                                                                               
TAPED    DSECT                                                                  
TAPEREC  DS    0CL70                                                            
TAPEACT  DS    CL12                1R STRUCTURE FOR OFF/DPT/SUB/EMPL            
*                                                                               
TAPELNAM DS    CL18                LAST NAME                                    
TAPEFNAM DS    CL18                FIRST NAME                                   
TAPEMNAM DS    CL1                 MIDDLE INITIAL                               
*                                                                               
TAPECKDT DS    CL6                 CHECK DATE YYMMDD                            
TAPECODE DS    CL5                 PAY TYPE                                     
TAPEAMNT DS    CL9                 AMOUNT                                       
TAPESTAT DS    XL1                 STATUS (SAME AS PDESTAT IN 86 ELEM)          
TAPEREV  EQU   PDERVRSL            AUTO REVERSAL                                
TAPEPC1  EQU   PDEPC1              GENERATED BY PC1                             
TAPEPC2  EQU   PDEPC2              GENERATED BY PC2                             
TAPELNQ  EQU   *-TAPED                                                          
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACCP02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
PRNTBL   DS    A                   PRNTBL                                       
DATVAL   DS    A                   DATE VALIDATION                              
SQUASHER DS    A                   SQUASHER                                     
GETCAP   DS    A                   GET COST ALLOCATION PROFILES                 
COVAIL   DS    A                   COVAIL                                       
CHOPCON  DS    A                   CHOPPER                                      
HELLO    DS    A                   HELLO                                        
AELTAB   DS    A                   ELEMENT TABLE                                
ABETAB   DS    A                   BENEFIT TABLE                                
ACONVTAB DS    A                   CONVERSION TABLE                             
ABUFF    DS    A                   BUFFALO                                      
AIO1     DS    A                   IO AREA #1 (2000 BYTES)                      
AIO2     DS    A                   IO AREA #2 (2000 BYTES)                      
AIOTAPE  DS    A                   IO AREA FOR IN-TAPE                          
ALSTWRK  DS    A                   SAVED SORT RECORD                            
APYTABLE DS    A                   PAYCODE TABLE                                
ACOBLOCK DS    A                   COBLOCK FOR GETTING COST PROFILES            
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
ALSORT   DS    A                   ADDR OF LAST SORT RECORD READ                
SVRE     DS    A                   SAVED RE                                     
SAVERC   DS    F                                                                
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
ELCODE   DS    XL1                                                              
FIRST    DS    CL1                                                              
OFFDISP  DS    XL1                 DISPL INTO ACC OF OFFICES                    
NUMLEVS  DS    XL1                 NUMBER OF LEVELS                             
ABCDLEN  DS    XL4                 INDIVIDUAL LEVEL LENGTHS                     
BIT      DS    XL1                                                              
ONEBYTOF EQU   X'80'               AGY ON OLD OFFICES                           
NONAMES  EQU   X'40'               CANNOT UPDATE NAMES-INVALID REC              
PGBRKUP  EQU   X'20'               PAGE BRK COMING UP-HOLD OFF PRINTING         
ADJRATE  EQU   X'10'               ADJ RATE IS SET UP                           
OUTLOCDT EQU   X'08'               SALARY OUTSIDE LOC DATE                      
SORTPROF EQU   X'04'               USING SORTING PROFILE                        
ERRNAME  EQU   X'02'               NAME ERROR                                   
*                                                                               
COMMAND  DS    CL8                 USED IN DATAMGR CALL                         
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
ELEM     DS    CL255               ELEMENT BUFFER                               
ELEM2    DS    CL255               ELEMENT BUFFER                               
*                                                                               
SVACT    DS    CL12                                                             
SVPERSN  DS    CL12                                                             
SVKEY    DS    CL42                                                             
SVTAMNT  DS    CL(L'TAPEAMNT)      AMOUNT                                       
LASTTOT  DS    PL8                                                              
DIVWRK   DS    PL16                                                             
DUB2     DS    PL8                                                              
*                                                                               
COUNTERS DS    0PL8                                                             
COUNTIN  DS    PL8                 NUMBER OF INPUT RECORDS FROM TAPE            
COUNTERR DS    PL8                 NUMBER OF INPUT RECORDS WITH ERRORS          
COUNTREV DS    PL8                 NUMBER OF ADDL POSITIONS-REVERSALS           
COUNTBEN DS    PL8                 NUMBER OF ADDL POSITIONS-BENEFITS            
COUNTPCT DS    PL8                 NUMBER OF ADDL POSITIONS-PERCENTS            
         DS    PL8                 SPARE - MATCHES BREAKER LINE                 
COUNTUPD DS    PL8                 NUMBER OF RECORDS ON FILE UPDATED            
COUNTADD DS    PL8                 NUMBER OF RECORDS ADDED TO FILE              
COUNT#   EQU   (*-COUNTERS)/8                                                   
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
LEVELNM  DS    0CL15                                                            
LEVELANM DS    CL15                LEDGER LEVEL NAMES (HIERARCHY)               
LEVELBNM DS    CL15                                                             
LEVELCNM DS    CL15                                                             
LEVELDNM DS    CL15                                                             
*                                                                               
RECORD   DS    XL1                 RECORD TYPE                                  
RECSTAFF EQU   X'10'               1R STAFF RECORD                              
RECPERSN EQU   X'01'               X'0F' PERSON RECORD                          
RECHIST  EQU   X'02'               SALARY HISTORY RECORD                        
*                                                                               
SIGN     DS    XL1                 SIGN OF PACKED NUMBER                        
NEGLEAD  EQU   X'80'                                                            
NEGTRAIL EQU   X'40'                                                            
TURNOFF  EQU   X'FF'                                                            
*                                                                               
ACTION   DS    XL1                 MODE FOR CURRENT INPUT RECORD                
BUILD    EQU   X'80'               BUILD A NEW RECORD                           
UPDATE   EQU   X'40'               UPDATE AN OLD RECORD                         
*                                                                               
ELEMBIT  DS    XL1                 BITS FOR F1 ELEM                             
ADDFLAG  EQU   X'80'               FLAG FOR ADD IS ON                           
CHAFLAG  EQU   X'40'               FLAG FOR CHANGE IS ON                        
*                                                                               
PCTBYTE  DS    XL1                 BYTE FOR PERCENT FLAGS                       
PC1ATTR  EQU   X'80'               PC1                                          
PC2ATTR  EQU   X'40'               PC2                                          
*                                                                               
SVBFTTYP DS    XL1                 SAVE TYPE OF RECORD                          
SVADDATE DS    CL3                 SAVED ADD DATE FROM F1 ELEM (BIN)            
SVCHANUM DS    CL1                 SAVED # OF LAST CHA FROM F1                  
SVPCODE  DS    XL1                 SAVED PAYROLL CODE                           
*              BUFFALO RECORD                                                   
*                                                                               
BUFREC   DS    0CL266                                                           
BUFKEY   DS    0CL74               *** BUFFALO KEY ***                          
BUFTYPE  DS    CL1                                                              
BUFREP1  EQU   X'01'               SHOW ON DETAIL REPORT                        
BUFREP2  EQU   X'02'               SHOW ON PERSON REPORT                        
BUFERROR EQU   X'04'               SHOW ON ERROR REPORT                         
BUFNMRP1 EQU   X'05'               SHOW ON NAME SORT DETAIL REPORT              
BUFNMRP2 EQU   X'06'               SHOW ON NAME SORT PERSON REPORT              
BUFOFFC  DS    CL12                                                             
BUFDPT   DS    CL12                                                             
BUFSDPT  DS    CL12                                                             
BUFKLNAM DS    CL18                LAST NAME                                    
BUFPERSN DS    CL12                                                             
BUFNUM   DS    XL1                 PAYROLL CODE #                               
BUFDATE  DS    CL6                 CHECK DATE YYMMDD                            
*                                                                               
BUFDATA  DS    0CL69               *** BUFFALO DATA ***                         
BUFACT   DS    CL12                                                             
BUFLNAM  DS    CL18                LAST NAME                                    
BUFFNAM  DS    CL18                FIRST NAME                                   
BUFMNAM  DS    CL1                 MIDDLE INITIAL                               
BUFCKDT  DS    CL6                 CHECK DATE YYMMDD                            
BUFCODE  DS    CL5                 PAYROLL CODE                                 
BUFAMNT  DS    CL9                 AMOUNT                                       
*                                                                               
BUFNAMES DS    0CL108                                                           
BUFNMLV1 DS    CL36                OFFICE NAME                                  
BUFNMLV2 DS    CL36                DEPARTMENT NAME                              
BUFNMLV3 DS    CL36                SUBDEPARTMENT NAME                           
BUFDESC  DS    CL15                PAYROLL CODE DESCRIPTION                     
BUFPSTA1 DS    XL1                 PAYROLL STATUS 1                             
BUFREV   EQU   PDERVRSL            AUTO REVERSAL                                
BUFPC1   EQU   PDEPC1              GENERATED BY PC1                             
BUFPC2   EQU   PDEPC2              GENERATED BY PC2                             
*                                                                               
BUFPSTAT DS    XL1                 PAYROLL STATUS 2                             
BUFPHRTE EQU   PDESHRTE            FROM PDEEL X'86' HOURLY RATE X'40'           
BUFADJRT EQU   PDESADJ             FROM X'86' ADJUSTMENT RATE X'20'             
*                                                                               
BUFSTAT  DS    XL1                                                              
BUFIOFF  EQU   X'01'               INVALID OFFICE                               
BUFIDPT  EQU   X'02'               INVALID DEPARTMENT                           
BUFISDPT EQU   X'04'               INVALID SUB DEPARTMENT                       
BUFISTAF EQU   X'08'               INVALID 1R STAFF                             
BUFIPERS EQU   X'10'               INVALID PERSON RECORD                        
BUFICODE EQU   X'20'               INVALID PAYROLL CODE                         
BUFIDATE EQU   X'40'               INVALID CHECK DATE - LOCATION DATE           
BUFICKDT EQU   X'80'               INVALID CHECK DATE                           
*                                                                               
BUFSTAT2 DS    XL1                                                              
BUFINAME EQU   X'80'               INVALID NAME(S)                              
*                                                                               
BUFTTYPE DS    XL1                                                              
BUFTITEM EQU   X'00'               ITEM RECORD                                  
BUFTOFFC EQU   X'01'               OFFICE TOTAL RECORD                          
BUFTDPT  EQU   X'02'               DEPARTMENT TOTAL RECORD                      
BUFTSDPT EQU   X'04'               SUBDEPARTMENT TOTAL RECORD                   
BUFTPERS EQU   X'08'               PERSON TOTAL RECORD                          
BUFTREQ  EQU   X'10'               REQUEST TOTAL RECORD                         
BUFTCNTL EQU   X'20'               CONTROL TOTAL RECORD/ REQ+ERR                
*                                                                               
BUFACCUM DS    PL8                 *** BUFFALO ACCUMS ***                       
BUFCKAMT DS    PL8                 CHECK AMOUNT                                 
BUFLNQ   EQU   *-BUFREC                                                         
*                                                                               
WRK2     DS    CL120                                                            
MAXPCODE EQU   150                 MAX # OF PAYCODES FOR TABLE                  
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACREPCQ02 11/06/13'                                      
         END                                                                    
