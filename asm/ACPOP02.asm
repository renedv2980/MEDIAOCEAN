*          DATA SET ACPOP02    AT LEVEL 010 AS OF 06/07/13                      
*PHASE T60A02A                                                                  
*INCLUDE ACRAPPER                                                               
         TITLE 'ACPOP02 - PRODUCTION ORDERS PLUS- OPEN/CLOSE'                   
ACPOP02  CSECT                                                                  
         PRINT NOGEN               ON ENTRY TO OVERLAY ORDER RECORD IS          
         NMOD1 PROGDX,**POP2**,CLEAR=YES,RR=R2   LOCKED IN AIOAREA1             
         LR    RA,RC                                                            
         USING PROGD,RC                                                         
         USING POPWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     R5,AIOAREA1                                                      
         USING ORDRECD,R5                                                       
         MVI   FERN,OK                                                          
         L     RE,=V(ACRAPPER)                                                  
         AR    RE,R2                                                            
         ST    RE,VRAPPER                                                       
         MVC   SVORDER,ORDKORD                                                  
                                                                                
         CLI   EBUYFLAG,C'Y'       IS THIS A BRANDO/EBUYER ORDER?               
         BNE   ORDELET             NO, BYPASS THIS                              
         LA    R6,ORDRECD+ACCORFST                                              
         USING ORDELD,R6                                                        
         XR    R0,R0                                                            
ORDPID   CLI   ORDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                GET THE PID FROM THE ORDER RECORD            
         CLI   ORDEL,ORDELQ                                                     
         BE    *+14                                                             
         IC    R0,ORDLN                                                         
         AR    R6,R0                                                            
         B     ORDPID                                                           
*                                                                               
         MVC   SVORDPID,ORDCPID    SAVE THE PID                                 
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LOGICALLY DELETE ORDER RECORD - X'20'                        *         
***********************************************************************         
                                                                                
ORDELET  CLI   ACTION,DEL          TEST ACTION DELETE                           
         BNE   ORDCLOSD                                                         
                                                                                
         MVC   BIGKEY,0(R5)        ORIGINAL ORDER                               
         GOTOR ARDHID,0                                                         
                                                                                
         LA    R5,BIGKEY                                                        
         CLC   ORDKEY(ORDKSEQ-ORDKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,DISKADD        SAVE THE ADDRESS                             
                                                                                
         L     RE,AIOAREA1                                                      
         LHI   RF,LIOAREAS                                                      
         L     R0,AIOAREA4                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,DMCB                                                          
         GOTO1 VACCEMU,DMCB,=C'OLDN',,AIOAREA4,AIOAREA4                         
                                                                                
         USING CPTRBLK,XTCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIOAREA4),(C'K',CPTRBLK),SVDA,0,     +        
               ACOMFACS                                                         
                                                                                
         L     R5,AIOAREA1                                                      
         MVI   ORDRSTAT,X'20'      SET LOGICAL DELETE BIT                       
         MVC   SVSTATUS,ORDRSTAT   SAVE STATUS                                  
         GOTO1 VDATCON,DMCB,(1,TODAYP),(2,WORK)                                 
         MVC   ORDRECD+ACCOUSED(ACCOULEN),WORK                                  
         B     ORDATE                                                           
                                                                                
***********************************************************************         
*        MARK ORDER AS CLOSED/FULFILLED - X'40'                       *         
***********************************************************************         
                                                                                
ORDCLOSD CLI   ACTION,CLO          TEST ACTION CLOSE                            
         BNE   ORDREST                                                          
         MVI   ORDRSTAT,X'40'      SET FULLY MATCHED BIT (CLOSED)               
         MVC   SVSTATUS,ORDRSTAT   SAVE STATUS                                  
         XC    ORDRECD+ACCOUSED(ACCOULEN),ORDRECD+ACCOUSED                      
         BAS   RE,PARTIAL                                                       
         CLI   PRESFLAG,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ACTIVE                                                        
         B     ORDATE                                                           
                                                                                
***********************************************************************         
*        RESTORE ORDER - MARK STATUS AS X'00'                         *         
***********************************************************************         
                                                                                
ORDREST  CLI   ACTION,RES          TEST ACTION RESTORE                          
         BNE   ORDOPEN                                                          
         MVI   ORDRSTAT,0                                                       
         MVC   SVSTATUS,ORDRSTAT   SAVE STATUS                                  
         XC    ORDRECD+ACCOUSED(ACCOULEN),ORDRECD+ACCOUSED                      
         B     ORDATE                                                           
                                                                                
***********************************************************************         
*        OPEN ORDER - MARK STATUS AS X'00'                            *         
***********************************************************************         
                                                                                
ORDOPEN  CLI   ACTION,OPE          TEST ACTION OPEN                             
         BE    *+6                                                              
         DC    H'0'                INVALID ACTION                               
         MVI   ORDRSTAT,0          CLEAR ORDER STATUS                           
         MVC   SVSTATUS,ORDRSTAT   SAVE STATUS                                  
         XC    ORDRECD+ACCOUSED(ACCOULEN),ORDRECD+ACCOUSED                      
         BAS   RE,REOPEN                                                        
         CLI   PRESFLAG,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ACTIVE                                                        
                                                                                
         USING GDAELD,R6                                                        
ORDATE   LA    R6,ELEMENT          BUILD A DATE ELEMENT                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATOSTA                                                 
         MVC   GDADATE,TODAYP                                                   
         GOTOR ADDELEM,AIOAREA1    ADD IT TO THE ORDER                          
         DROP  R6                                                               
                                                                                
         GOTO1 AWRITE,AIOAREA1     WRITE THE ORDER RECORD BACK                  
         BNE   EXIT                                                             
                                                                                
         CLI   ACTION,DEL          TEST ACTION DELETE                           
         BNE   ORD00                                                            
                                                                                
         L     R5,AIOAREA1                                                      
         MVC   BIGKEY,0(R5)        UPDATED ORDER                                
         GOTOR ARDHID,0                                                         
                                                                                
         LA    R5,BIGKEY                                                        
         CLC   ORDKEY(ORDKSEQ-ORDKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,DISKADD        SAVE THE ADDRESS                             
                                                                                
         L     RE,AIOAREA1                                                      
         LHI   RF,LIOAREAS                                                      
         L     R0,AIOAREA4                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,DMCB                                                          
         GOTO1 VACCEMU,DMCB,=C'OLDN',,AIOAREA4,AIOAREA4                         
                                                                                
         USING CPTRBLK,XTCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIOAREA4),(C'K',CPTRBLK),SVDA,0,     +        
               ACOMFACS                                                         
         DROP  R5                                                               
                                                                                
ORD00    CLI   PRESFLAG,C'Y'       TEST PRESTO ORDER                            
         BNE   ORD02               NO                                           
         CLI   ACTION,CLO          TEST CLOSE                                   
         BE    *+12                YES                                          
         CLI   ACTION,OPE          TEST OPEN                                    
         BNE   ORD02               NO                                           
                                                                                
         BAS   RE,POINT            HANDLE PRESTO ACTIVITY POINTERS              
         EJECT                                                                  
***********************************************************************         
*        MARK JOB TRANSACTIONS                                        *         
*                                                                     *         
*        ACTION DELETE & CLOSE MARKS TRANSACTIONS AS X'80'            *         
*        ACTION RESTORE & OPEN UNMARKS OR REBUILDS THEM.              *         
***********************************************************************         
                                                                                
ORD02    L     RE,AIOAREA2         CLEAR AIOAREA2                               
         LHI   RF,LIOAREAS                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R6,AIOAREA1         R6=A(ORDER RECORD)                           
         AHI   R6,ACCORFST         R6=1ST ELEMENT                               
         SR    R0,R0                                                            
                                                                                
ORD04    CLI   0(R6),0             END OF RECORD?                               
         BE    ORD14                                                            
         CLI   0(R6),ORDELQ        ORDER ELEMENT                                
         BE    ORD08                                                            
         CLI   0(R6),OAMELQ        ORDER AMOUNT ELEMENT                         
         BE    ORD12                                                            
                                                                                
ORD06    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ORD04               LOOP FOR NEXT ELEMENT                        
                                                                                
         USING TRNRECD,R5                                                       
         USING ORDELD,R6                                                        
ORD08    LA    R5,KEY              BUILD KEY OF JOB RECORD                      
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCULA,ORDJOB     MOVE IN JOB                                  
         MVC   JOBACCT,ORDJOB       AND SAVE IT FOR LATER                       
         MVC   TRNKWORK,=2C'*'     WORKCODE IS '**'                             
         MVC   TRNKCULC,ORDSUP                                                  
         CLI   TYPE,PRODN                                                       
         BE    ORD10                                                            
         MVC   TRNKWORK,SPACES     FOR NON-PROD CLEAR WORK CODE                 
         MVI   TRNKCCPY,C' '       AND FIRST BYTE OF CONTRA ACCOUNT             
                                                                                
ORD10    MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,APONUM      MOVE ORDER NUMBER                            
                                                                                
         L     R3,AIOAREA2         MOVE JOB KEY TO AIO2                         
         MVC   0(L'TRNKEY,R3),TRNKEY                                            
         DROP  R5                                                               
                                                                                
         USING TRNELD,R3                                                        
         LA    R3,ELEMENT          BUILD TRANSACTION ELEMENT                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVC   TRNDATE,ORDDATE                                                  
         MVC   TRNREF,APONUM                                                    
         MVI   TRNSTAT,TRNSDR                                                   
         MVI   TRNTYPE,TRNTORD                                                  
         MVC   TRNMOS,THISMON                                                   
         MVC   TRNBREF,SPACES                                                   
         ZAP   TRNAMNT,=P'0'                                                    
         MVC   TRNANAL,=2C'*'                                                   
         MVI   TRNNARR,C' '                                                     
         GOTOR ADDELEM,AIOAREA2    ADD TO RECORD IN AIO2                        
         DROP  R3                                                               
                                                                                
         USING TRSELD,R3                                                        
         LA    R3,ELEMENT          BUILD STATUS ELEMENT                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         GOTO1 VDATCON,DMCB,(1,TODAYP),(2,TRSDATE)                              
         GOTOR ADDELEM,AIOAREA2    ADD TO RECORD IN AIO2                        
         B     ORD06               LOOP BACK UP FOR 68 ELEMENTS                 
         DROP  R3                                                               
                                                                                
         USING OAMELD,R3                                                        
ORD12    LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         SR    R1,R1                                                            
         IC    R1,1(R6)            GET LENGTH OF 68 ELEMENT                     
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OAMEL(0),0(R6)      COPY 68 ELEMENT FROM ORDER RECORD            
         ZAP   OAMTVAL,OAMIVAL                                                  
         GOTOR ADDELEM,AIOAREA2    ADD TO RECORD IN AIO2                        
         B     ORD06               LOOP BACK UP MORE                            
                                                                                
ORD14    GOTO1 AREADL,AIOAREA1     READ RECORD BASED ON KEY                     
         TM    DMCB+8,X'FD'        ACCEPT DELETES                               
         BNZ   ORD16               IF RECORD IS NOT THERE THEN BUILD            
*                                  A NEW RECORD FOR OPEN & RESTORE              
         USING TRNRECD,R5                                                       
         L     R5,AIOAREA1                                                      
         MVI   TRNRSTAT,0                                                       
         CLI   ACTION,OPE          OPEN/RESTORE UNDELETES TRANSACTION           
         BE    *+8                                                              
         CLI   ACTION,RES                                                       
         BE    *+8                                                              
         MVI   TRNRSTAT,X'80'      DELETE/CLOSE DELETES TRANSACTION             
         GOTO1 AWRITE                                                           
         BNE   EXIT                                                             
         B     OKEND                                                            
         DROP  R5                                                               
                                                                                
ORD16    CLI   ACTION,OPE          ONLY REBUILD TRANS FOR OPEN/RESTORE          
         BE    ORD18                                                            
         CLI   ACTION,RES                                                       
         BE    ORD18                                                            
         B     OKEND                                                            
                                                                                
ORD18    MVC   KEY,SPACES          READ THE JOB RECORD                          
         MVC   KEY(15),JOBACCT                                                  
         MVC   TEMPKEY,KEY                                                      
         GOTO1 ARDHI,AIOAREA3                                                   
         CLC   TEMPKEY,KEY                                                      
         BE    ORD20                                                            
         MVI   FERN,NOTFOUND                                                    
         B     EXIT                                                             
                                                                                
ORD20    L     R4,AIOAREA3         AIOAREA3 = JOB RECORD                        
         AHI   R4,ACCORFST                                                      
         SR    R1,R1                                                            
                                                                                
ORD22    CLI   0(R4),RSTELQ         CHECK IF JOB IS CLOSED                      
         BE    ORD24                                                            
         CLI   0(R4),0              END OF RECORD                               
         BNE   *+6                                                              
         DC    H'0'                 MUST FIND A 30 ELEMENT                      
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     ORD22                                                            
                                                                                
         USING RSTELD,R4                                                        
ORD24    CLI   RSTSTAT,RSTSACIC    IS JOB CLOSED?                               
         BNE   ORD26                                                            
         MVI   FERN,CLOSED                                                      
         B     EXIT                                                             
                                                                                
ORD26    CLI   RSTSTAT,RSTSACIL    IS ACCOUNT LOCKED                            
         BNE   ORD28                                                            
         MVI   FERN,LOCKED                                                      
         B     EXIT                                                             
                                                                                
ORD28    MVI   FERN,NOREST                                                      
         GOTO1 AADD,AIOAREA2       ADD THE TRANSACTION RECORD                   
         BNE   EXIT                                                             
                                                                                
OKEND    CLI   EBUYFLAG,C'Y'       DO FOR BRANDO ONLY                           
         BNE   *+8                                                              
         BAS   RE,UPPIDR                                                        
         MVI   FERN,OK                                                          
         MVC   MSG,=CL60'ACTION COMPLETED - ENTER NEXT'                         
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        TURN OFF PARTIAL PAY FLAG                                    *         
*        AIOAREA1 = ORDER RECORD                                      *         
***********************************************************************         
                                                                                
PARTIAL  NTR1                                                                   
         L     R6,AIOAREA1                                                      
         AHI   R6,ACCORFST         SEARCH ORDER RECORD                          
         SR    R0,R0                                                            
                                                                                
PART2    CLI   0(R6),0                                                          
         BE    PARTX                                                            
         CLI   0(R6),ORDELQ        FIND ORDER ELEMENT                           
         BE    PART4                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PART2                                                            
                                                                                
         USING ORDELD,R6                                                        
PART4    CLI   ORDLN,ORDLN2Q       CHECK FOR STATUS BYTE AND CHANGE             
         BL    *+8                                                              
         NI    ORDSTAT,X'FF'-ORDSPART                                           
                                                                                
PARTX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        TURN OFF FULLY MATCHED AND TURN ON PARTIAL FLAG              *         
*        AIOAREA1 = ORDER RECORD                                      *         
***********************************************************************         
                                                                                
REOPEN   NTR1                                                                   
         L     R6,AIOAREA1                                                      
         AHI   R6,ACCORFST         SEARCH ORDER RECORD                          
         SR    R0,R0                                                            
                                                                                
REOPEN2  CLI   0(R6),0                                                          
         BE    REOPENX                                                          
         CLI   0(R6),ORDELQ        FIND ORDER ELEMENT                           
         BE    REOPEN4                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     REOPEN2                                                          
                                                                                
         USING ORDELD,R6                                                        
REOPEN4  CLI   ORDLN,ORDLN2Q       CHECK FOR STATUS BYTE AND CHANGE             
         BL    REOPENX                                                          
         TM    ORDSTAT,ORDSMNUP+ORDSPART                                        
         BZ    REOPENX             LEAVE AS IS IF NOT BATCHED TO YET            
         NI    ORDSTAT,X'FF'-ORDSMNUP                                           
         OI    ORDSTAT,ORDSPART                                                 
                                                                                
REOPENX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD POINTER ELEMENT FOR RECORD ACTIVITY KEY                  *         
***********************************************************************         
                                                                                
ACTIVE   NTR1                                                                   
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM                                                 
         MVC   RAPCPY,COMPANY                                                   
         MVI   RAPRTYP,RAPKRORD    RECORD TYPE = ORDER                          
         MVI   RAPEMU,C'Y'                                                      
         MVC   RAPAREC,AIOAREA1                                                 
         MVC   RAPACOM,ACOMFACS                                                 
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    ACTIVEX                                                          
         DC    H'0'                                                             
                                                                                
ACTIVEX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        MAINTAIN RECORD ACTIVITY POINTERS                            *         
***********************************************************************         
                                                                                
POINT    NTR1                                                                   
         MVI   RAPACTN,RAPAPTR     POINTER MAINTENANCE                          
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    POINTX                                                           
         DC    H'0'                                                             
                                                                                
POINTX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        UPDATE PIDRECD WITH STATUS                                   *         
***********************************************************************         
UPPIDR   NTR1                                                                   
         USING PIDRECD,RF                                                       
         LA    RF,KEY              UPDATE PASSIVES (DOES IT'S OWN IOS           
         XC    PIDKEY,PIDKEY       AS THIS SN'T SUPPORTED IN =ORD YET)          
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,COMPANY                                                  
         MVC   PIDKPID,SVORDPID                                                 
         MVI   PIDKSTYP,PIDKORDQ                                                
         MVC   PIDKORD,SVORDER                                                  
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMREAD  '),=C'ACCDIR  ',KEY,KEY          
         LA    RF,KEY                                                           
         MVC   PIDKSTA(1),SVSTATUS                                              
         GOTO1 VDATAMGR,DMCB,=C'DMWRT   ',=C'ACCDIR  ',KEY,KEY                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        ADD ELEMENT TO IO PASSED IN R1                               *         
***********************************************************************         
                                                                                
ADDELEM  NTR1                                                                   
         CLI   ELEMENT,0                                                        
         BE    EXIT                                                             
         L     RF,0(R1)                                                         
         GOTO1 VHELLO,DMCB,(C'P',ACCFIL),(RF),ELEMENT,0                         
         CLI   DMCB+12,0                                                        
         BE    EXIT                                                             
         MVI   FERN,TOOLONG        DID RECORD GET TOO LONG                      
         CLI   DMCB+12,5                                                        
         BE    EXIT                                                             
         DC    H'0'                OTHER ERRORS UNACCEPTABLE                    
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
                                                                                
ACCFIL   DC    CL8'ACCFIL'                                                      
         LTORG                                                                  
         EJECT                                                                  
PROGD    DSECT                     LOCAL STORAGE                                
VRAPPER  DS    V                                                                
JOBACCT  DS    CL15                                                             
TEMPKEY  DS    CL42                                                             
ELEMENT  DS    CL256               NEW ELEMENT WORK AREA                        
SVORDER  DS    CL6                 SAVE THE ORDER NUMBER                        
SVORDPID DS    XL2                 SAVE ORDER PID                               
SVSTATUS DS    XL1                 SAVE ORDER STATUS                            
XTCPTRBK DS    XL128               USED FOR PADDLE CPTRBLK CALLS                
SVDA     DS    XL4                 SAVED DISK ADDRESS                           
       ++INCLUDE ACRAPPERD                                                      
PROGDX   EQU   *-PROGD                                                          
         EJECT                                                                  
       ++INCLUDE ACPOPDSECT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACPOP02   06/07/13'                                      
         END                                                                    
