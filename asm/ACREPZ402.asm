*          DATA SET ACREPZ402  AT LEVEL 051 AS OF 02/14/01                      
*PHASE ACZ402A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'MOVE PAYABLE RECORDS TO NEW ACCOUNT'                            
         PRINT NOGEN                                                            
ACZ402   CSECT                                                                  
         NMOD1 0,**ACZ4**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ4D,RC                                                         
         MVI   FCSUPOFC,C'Y'                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RNF                                                              
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RNL                                                              
XIT      XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
                                                                                
RNF      DS    0H                                                               
         OPEN  (TINT,(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SR    R4,R4                                                            
         L     R3,=A(FXTAB)                                                     
*                                                                               
RNF3     GET   TINT,(R3)                                                        
         LA    R3,L'FXTAB(R3)                                                   
         LA    R4,1(R4)                                                         
         CHI   R4,5000                                                          
         BNH   RNF3                                                             
         DC    H'0'                                                             
*                                                                               
RNF5     CLOSE (TINT)                                                           
         ST    R4,NUMACC                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
RQF      MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
                                                                                
LDGF     L     R3,ADLEDGER                                                      
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(3),0(R3)                                                    
         BAS   RE,DMRD             READ LEDGER                                  
*                                                                               
LDGF3    BAS   RE,DMSEQ            READ ACCOUNTS                                
         CLC   DIR(3),DKEY         SAME C/U/L                                   
         BNE   XIT                                                              
LDGF5    LA    R6,DIR                                                           
         USING ACTRECD,R6                                                       
         CLI   ACTKACT,C' '                                                     
         BH    XIT                 PASSED THE BAD SHIT                          
         CLI   ACTKACT+1,C' '                                                   
         BNH   LDGF3                                                            
*                                                                               
         MVC   LREC,0(R6)                                                       
         MVC   OLDACC,0(R6)        SAVE ACCOUNT                                 
         BAS   RE,ACC              FIX THIS ACCOUNT                             
*                                                                               
         MVC   DKEY,SPACES         RESET KEY                                    
         MVC   DKEY(L'ACTKCULA),OLDACC                                          
         MVI   DKEY+(L'ACTKCULA-1),X'FF'                                        
         BAS   RE,DMHGH            LOOK FOR NEXT                                
         B     LDGF5                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
                                                                                
         USING ACTRECD,R6                                                       
ACC      NTR1  ,                                                                
         L     R2,=A(FXTAB)                                                     
         L     R0,NUMACC                                                        
ACC3     CLC   ACTKCPY(3),0(R2)       MATCH C/U/L                               
         BNE   *+14                                                             
         CLC   ACTKACT+1(11),ACTKACT-ACTRECD+1(R2)                              
         BE    ACC4                                                             
         LA    R2,L'FXTAB(R2)                                                   
         BCT   R0,ACC3                                                          
         DC    H'0'                                                             
*                                                                               
ACC4     MVC   NEWACC,0(R2)        SAVE NEW ACCOUNT                             
         MVC   DKEY,0(R6)                                                       
         BAS   RE,DMRD             READ BAD ACCOUNT                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            INTO IO1                                     
         AP    CNTAPRC,=P'1'       ACCOUNTS PROCESSED                           
         BAS   RE,DMGETR                                                        
         DROP  R6                                                               
*                                                                               
         L     R4,AIO1             R4=A(BAD RECORD)                             
         USING ACTRECD,R4                                                       
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R4               R3=BAD 32 ELEMENT                            
*                                                                               
B        USING ABLELD,R3                                                        
*                                                                               
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(L'ACTKCULA),NEWACC  SET GOOD ACCOUNT                        
         BAS   RE,DMRD             READ GOOD ACCOUNT                            
         BE    ACC7                FOUND -  IT                                  
         BAS   RE,ADDR             NOT FOUND - ADD IT                           
         B     ACC11                                                            
*                                                                               
ACC7     MVC   AIO,AIO2            GOOD ACCOUNT INTO IO2                        
         BAS   RE,DMGETR                                                        
*                                                                               
         L     R4,AIO2             R4=A(GOOD RECORD)                            
         USING ACTRECD,R4                                                       
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R4               R5=GOOD 32 ELEMENT                           
*                                                                               
         USING ABLELD,R5                                                        
ACC9     AP    ABLFRWD,B.ABLFRWD   ADD TO CORRECT ACCOUNT                       
         AP    ABLDR,B.ABLDR                                                    
         AP    ABLCR,B.ABLCR                                                    
         BAS   RE,DMPUTR                                                        
         DROP  R5,B                                                             
*                                                                               
ACC11    BAS   RE,NEXT             GET NEXT "BAD' RECORD                        
         BNE   ACC13               END OF ACCOUNT                               
*                                                                               
         L     R4,AIO1                                                          
         LA    R5,ACTRFST                                                       
         CLI   0(R5),ABLELQ        TEST OFFICE ACCOUNT                          
         BNE   *+12                                                             
         BAS   RE,OFAC             FIX OFFICE ACCOUNT                           
         B     ACC11               GET NEXT                                     
*                                                                               
         CLI   0(R5),CACELQ        TEST CONTRA HEADER                           
         BNE   *+12                                                             
         BAS   RE,CONT                                                          
         B     ACC11                                                            
*                                                                               
         CLI   0(R5),BUKELQ        TEST BUCKET  RECORD                          
         BNE   *+12                                                             
         BAS   RE,BUCK                                                          
         B     ACC11                                                            
*                                                                               
         CLI   0(R5),TRNELQ        TEST TRANSACTION RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TRNS                                                          
         B     ACC11                                                            
*                                                                               
ACC13    MVC   DKEY,SPACES         DELETE BAD ACCOUNT                           
         MVC   DKEY(L'ACTKCULA),OLDACC                                          
         BAS   RE,DMRD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
ACC15    BAS   RE,DMGETR                                                        
         LA    R4,DIR                                                           
         OI    ACTKSTA,X'80'         DELETE OLD RECORDS                         
         BAS   RE,DMWRTD                                                        
*                                                                               
         L     R4,AIO                                                           
         OI    ACTRSTA,X'80'                                                    
         BAS   RE,DMPUTR                                                        
*                                                                               
         BAS   RE,DMSEQ                                                         
         CLC   DIR(L'ACTKCULA),OLDACC  TEST SAME ACCOUNT                        
         BE    ACC15                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RNL      MVI   FORCEHED,C'Y'                                                    
         LA    R2,CNTS                                                          
RNL3     EDIT  (P5,0(R2)),(6,P+1)                                               
         MVC   P+10(20),5(R2)                                                   
         GOTO1 ACREPORT                                                         
         LA    R2,L'CNTS(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   RNL3                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OFFICE ACCOUNT RECORDS                                              *         
***********************************************************************         
                                                                                
OFAC     NTR1  ,                                                                
         L     R4,AIO1             SET RECORD KEY                               
         MVC   DKEY,0(R4)                                                       
         MVC   DKEY(L'ACTKCULA),NEWACC  SET GOOD ACCOUNT                        
         BAS   RE,DMRD                                                          
         BE    OFAC5               FOUND IT                                     
         BAS   RE,ADDR             NOT FOUND ADD IT                             
         B     XIT                                                              
*                                                                               
OFAC5    MVC   AIO,AIO2                                                         
         BAS   RE,DMGETR           GET GOOD RECORD                              
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R4               R3=BAD 32 ELEMENT                            
B        USING ABLELD,R3                                                        
*                                                                               
         L     R4,AIO2                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R6,R4               R6=GOOD 32 ELEMENT                           
         USING ABLELD,R6                                                        
*                                                                               
         AP    ABLFRWD,B.ABLFRWD                                                
         AP    ABLDR,B.ABLDR                                                    
         AP    ABLCR,B.ABLCR                                                    
*                                                                               
         MVC   AIO,AIO2                                                         
         BAS   RE,DMWRTD           WRITE GOOD RECORD                            
         B     XIT                                                              
         DROP  B,R6                                                             
         EJECT                                                                  
***********************************************************************         
* CONTRA HEADER                                                       *         
***********************************************************************         
                                                                                
CONT     NTR1  ,                                                                
         L     R4,AIO1             SET RECORD KEY                               
         MVC   DKEY,0(R4)                                                       
         MVC   DKEY(L'ACTKCULA),NEWACC  SET GOOD ACCOUNT                        
         BAS   RE,DMRD                                                          
         BE    XIT                 FOUND IT - OK                                
         BAS   RE,ADDR             NOT FOUND ADD IT                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUCKET RECORD                                                       *         
***********************************************************************         
                                                                                
BUCK     NTR1  ,                                                                
         L     R4,AIO1             SET RECORD KEY                               
         MVC   DKEY,0(R4)                                                       
         MVC   DKEY(L'ACTKCULA),NEWACC  SET GOOD ACCOUNT                        
         BAS   RE,DMRD                                                          
         BE    BUCK3               FOUND IT - OK                                
         BAS   RE,ADDR             NOT FOUND ADD IT                             
         B     XIT                                                              
*                                                                               
BUCK3    MVC   AIO,AIO2                                                         
         BAS   RE,DMGETR           GET GOOD RECORD                              
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,BUKELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+10                                                             
BUCK5    LR    R4,R3                                                            
         BAS   RE,NEXTEL                                                        
         BNE   BUCK13                                                           
         LR    R3,R4                                                            
B        USING BUKELD,R3                                                        
*                                                                               
         L     R4,AIO2             R4=GOOD RECORD                               
         BAS   RE,GETEL                                                         
         B     *+10                                                             
BUCK7    LR    R4,R6                                                            
         BAS   RE,NEXTEL                                                        
         BNE   BUCK9                                                            
         LR    R6,R4               R6=GOOD 45 ELEMENT                           
         USING BUKELD,R6                                                        
         CLC   BUKMOS,B.BUKMOS     MATCH MONTH                                  
         BNE   BUCK7                                                            
         AP    BUKDR,B.BUKDR       ADD BAD TO GOOD                              
         AP    BUKCR,B.BUKCR                                                    
         B     BUCK5                                                            
*                                                                               
BUCK9    GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO2,(R3),0                             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BUCK5                                                            
*                                                                               
BUCK13   MVC   AIO,AIO2            WRITE BACK THE RECORD                        
         BAS   RE,DMPUTR                                                        
         B     XIT                                                              
         DROP  B,R6                                                             
         EJECT                                                                  
***********************************************************************         
* TRANSACTIONS                                                        *         
***********************************************************************         
                                                                                
TRNS     NTR1  ,                                                                
         L     R4,AIO1             SET RECORD KEY                               
         MVC   DKEY,0(R4)                                                       
         MVC   DKEY(L'ACTKCULA),NEWACC  SET GOOD ACCOUNT                        
         MVI   INCR,0              CLEAR INCREMENT                              
TRNS3    BAS   RE,DMRD                                                          
         BNE   TRNS5               FOUND IT - NOT GOOD                          
         LA    R4,DKEY                                                          
         USING TRNRECD,R4                                                       
         SR    R0,R0                                                            
         IC    R0,INCR                                                          
         AH    R0,=H'2'            ADD TWO TO THE INCREMENT                     
         STC   R0,INCR                                                          
         SR    R1,R1                                                            
         IC    R1,TRNKSBR          FIND NEXT AVAILABLE SUB REF                  
         AR    R1,R0               ADD THE INCREMENT                            
         STC   R1,TRNKSBR                                                       
         B     TRNS3                                                            
*                                                                               
TRNS5    L     R4,AIO1                                                          
         MVC   AIO,AIO1                                                         
         MVC   TRNKEY,DKEY         SET CORRECT KEY                              
         LA    R1,TRNRFST                                                       
         USING TRNELD,R1                                                        
         MVC   TRNSUB,TRNKSBR                                                   
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BNO   TRNS7                                                            
         MVI   ELCODE,MPYELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   TRNS7                                                            
         USING MPYELD,R4                                                        
         CLI   MPYLN,MPYLN2Q       TEST ELEMENT HAS SUB REFERENCE               
         BL    TRNS7                                                            
         SR    R1,R1                                                            
         IC    R1,MPYSUB           UPDATE SUB REFERENCE                         
         SR    R0,R0                                                            
         IC    R0,INCR                                                          
         AR    R1,R0                                                            
         STC   R1,MPYSUB                                                        
*                                                                               
TRNS7    BAS   RE,DMADDR                                                        
         MVC   SDA,DA                                                           
         BAS   RE,TMOSP            FIX TRANSACTION MOS PASSIVE                  
         B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
***********************************************************************         
* FIX TRANSACTION MOS PASSIVE                                         *         
***********************************************************************         
                                                                                
TMOSP    NTR1  ,                                                                
         XC    MOSKEY,MOSKEY                                                    
         LA    R3,MOSKEY                                                        
         USING MOSPASD,R3                                                       
         LA    R4,LREC                                                          
         USING TRNRECD,R4                                                       
         MVI   MOSPTYP,MOSPTYPQ    BUILD KEY FOR PASSIVE POINTER                
         MVC   MOSPCPY,TRNKCPY                                                  
         MVC   MOSPMOS,TRNKSMOS                                                 
         MVC   MOSPULA,TRNKULA                                                  
         MVC   MOSPOFF,TRNKOFF                                                  
         MVC   MOSPCULC,TRNKCULC                                                
         MVC   MOSPDATE,TRNKDATE                                                
         MVC   MOSPDA,TRNKDA                                                    
         MVC   DKEY,MOSKEY                                                      
         BAS   RE,DMRD             READ FOR PASSIVE                             
         BNE   XIT                 NOT FOUND - NO PROBLEM                       
         LA    R3,DIR                                                           
         OI    MOSPKSTA,X'80'      DELETE IT                                    
         BAS   RE,DMWRTD                                                        
         NI    MOSPKSTA,X'FF'-X'80'                                             
         MVC   MOSPDA,SDA          ADD NEW PASSIVE                              
         MVC   MOSPKDA,SDA                                                      
         MVC   MOSPACT,NEWACC+3                                                 
         BAS   RE,DMADDD                                                        
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET NEXT "BAD" RECORD INTO IO1                                      *         
***********************************************************************         
                                                                                
NEXT     NTR1  ,                                                                
         MVC   DKEY,LREC          SET LAST KEY                                  
*                                                                               
NEXT3    BAS   RE,DMRD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMSEQ                                                         
         CLC   DIR(L'ACTKCULA),DKEY                                             
         BNE   XIT                                                              
         MVC   AIO,AIO1            GET NEXT INTO IO1                            
         BAS   RE,DMGETR                                                        
         MVC   LREC,DIR            SAVE FOR NEXT TIME                           
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
         GETEL R4,FRSTEL,ELCODE                                                 
         EJECT                                                                  
***********************************************************************         
* ADD NEW RECORD TO FILE                                              *         
***********************************************************************         
                                                                                
ADDR     NTR1  ,                                                                
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKCULA,NEWACC      CHANGE TO NEW ACC                           
         BAS   RE,DMADDR                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(DMINB,DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(DMINB,DMRDHI),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(DMINB,DMRSEQ),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMADDD   ST    RE,SAVRE                                                         
         LA    RE,CNTDADD                                                       
         CLI   DIR,C' '                                                         
         BH    *+8                                                              
         LA    RE,CNTDPAS                                                       
         AP    0(L'CNTDADD,RE),=P'1'                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMWRTD   ST    RE,SAVRE                                                         
         LA    RE,CNTDWRT                                                       
         TM    DIR+(ACTKSTA-ACTRECD),X'80'                                      
         BNO   *+8                                                              
         LA    RE,CNTDDEL                                                       
         AP    0(L'CNTDWRT,RE),=P'1'                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(DMINB,GETREC),ACCMST,DA,AIO,DMWORK                 
         B     DMERR                                                            
*                                                                               
DMADDR   ST    RE,SAVRE                                                         
         AP    CNTDADD,=P'1'                                                    
         AP    CNTMADD,=P'1'                                                    
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO,DMWORK                         
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMPUTR   ST    RE,SAVRE                                                         
         LA    RE,CNTMPUT                                                       
         L     RF,AIO                                                           
         TM    ACTRSTA-ACTRECD(RF),X'80'                                        
         BNO   *+8                                                              
         LA    RE,CNTMDEL                                                       
         AP    0(L'CNTMPUT,RE),=P'1'                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO,DMWORK                         
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
*                                                                               
DMERR    MVC   DMBYTE,8(R1)                                                     
         BAS   RE,TRACE                                                         
         L     RE,SAVRE                                                         
         TM    DMBYTE,(X'80'+X'40'+X'20') TEST EOF/DISK ERR/DUP                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMBYTE,X'02'               TEST DELETED                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMBYTE,X'10'               TEST RNF                              
         BO    DMXN                                                             
         CR    RB,RB                                                            
         BR    RE                                                               
DMXN     LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TRACE IO'S                                                          *         
***********************************************************************         
                                                                                
TRACE    NTR1  ,                                                                
         CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         LR    R2,R1                                                            
         L     RF,12(R2)                RF=A(IO)                                
         LA    R6,DIRLNQ                R6=LENGTH OF DIRECTORY                  
         ICM   R3,15,4(R2)                                                      
         CLC   ACCDIR,0(R3)             TEST DIRECTORY READ                     
         BE    TRACE3                                                           
         ICM   R6,3,ACTRLEN-ACTRECD(RF) R6=LENGTH OF RECORD                     
*                                                                               
TRACE3   ICM   R3,15,0(R2)              R3=A(COMMAND)                           
         GOTO1 PRNTBL,PARM,(8,0(R3)),(RF),C'DUMP',(R6),=C'2D'                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS & STORAGE                                                 *         
***********************************************************************         
                                                                                
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
*                                                                               
FRSTEL   DC    Y(ACTRFST-ACTRECD)                                               
*                                                                               
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCMST   DC    CL8'ACCMST'                                                      
ACCDIR   DC    CL6'ACCDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
DMINB    DC    X'00'                                                            
DMBYTE   DC    X'00'                                                            
*                                                                               
CNTS     DS    0XL25                                                            
CNTAPRC  DC    PL5'0',CL20'ACCOUNTS PROCESSED'                                  
CNTDADD  DC    PL5'0',CL20'DIRECTORY ADDS'                                      
CNTDPAS  DC    PL5'0',CL20'DIRECTORY PASSIVES'                                  
CNTDWRT  DC    PL5'0',CL20'DIRECTORY WRITES'                                    
CNTDDEL  DC    PL5'0',CL20'DIRECTORY DELETES'                                   
CNTMADD  DC    PL5'0',CL20'MASTER ADDS'                                         
CNTMPUT  DC    PL5'0',CL20'MASTER PUTS'                                         
CNTMDEL  DC    PL5'0',CL20'MASTER DELETES'                                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL AND IO AREAS                                           *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
IO1      DS    CL2048                                                           
IO2      DS    CL2048                                                           
                                                                                
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=RNF5,             X        
               RECFM=FB,LRECL=15,BLKSIZE=1500                                   
*                                                                               
FXTAB    DS    5000XL15                                                         
*                                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT                                                               *         
***********************************************************************         
                                                                                
*                                                                               
ACZ4D    DSECT                                                                  
AIO      DS    A                                                                
DKEY     DS    CL(L'ACCKEY)                                                     
*                                                                               
DIR      DS    CL(ACCKLEN)                                                      
         ORG   DIR                                                              
DACC     DS    CL(L'ACCKEY)                                                     
DSTA     DS    XL(L'ACCKSTA)                                                    
DA       DS    XL(L'ACCKDA)                                                     
DIRLNQ   EQU   *-DIR                                                            
*                                                                               
SDA      DS    XL4                                                              
LREC     DS    CL(DIRLNQ)                                                       
MOSKEY   DS    CL(DIRLNQ)                                                       
*                                                                               
ELCODE   DS    X                                                                
INCR     DS    X                                                                
SAVRE    DS    F                                                                
*                                                                               
NUMACC   DS    F                                                                
NEWACC   DS    CL15                                                             
OLDACC   DS    CL15                                                             
PARM     DS    6F                                                               
*                                                                               
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
*                                                                               
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051ACREPZ402 02/14/01'                                      
         END                                                                    
