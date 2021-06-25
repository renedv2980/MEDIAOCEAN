*          DATA SET ACORD02    AT LEVEL 053 AS OF 10/17/18                      
*PHASE T60F02A                                                                  
*INCLUDE ACRAPPER                                                               
         TITLE 'ACORD02 - PRODUCTION ORDERS - OPEN/CLOSE'                       
ACORD02  CSECT                                                                  
         PRINT NOGEN               ON ENTRY TO OVERLAY ORDER RECORD IS          
         NMOD1 PROGDX,**ORD2**,CLEAR=YES,RR=R2   LOCKED IN AIOAREA1             
         LR    RA,RC                                                            
         USING PROGD,RC                                                         
         USING ORDWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     R5,AIOAREA1                                                      
         USING ACKEYD,R5                                                        
         MVI   FERN,OK                                                          
         L     RE,=V(ACRAPPER)                                                  
         AR    RE,R2                                                            
         ST    RE,VRAPPER                                                       
         EJECT                                                                  
*---------------------------------------------------------                      
*        LOGICALLY DELETE ORDER RECORD - X'20'                                  
*---------------------------------------------------------                      
*                                                                               
ORDELET  CLI   ACTION,DEL          TEST ACTION DELETE                           
         BNE   ORDCLSE                                                          
         MVI   ACSTATUS,X'20'      SET LOGICAL DELETE BIT                       
         GOTO1 VDATCON,DMCB,(1,TODAYP),(2,ACDTUSED)                             
         B     ORDATE                                                           
*                                                                               
*---------------------------------------------------------                      
*        MARK ORDER AS CLOSED/FULFILLED - X'40'                                 
*---------------------------------------------------------                      
*                                                                               
ORDCLSE  CLI   ACTION,CLO          TEST ACTION CLOSE                            
         BNE   ORDREST                                                          
         MVI   ACSTATUS,X'40'      SET FULLY MATCHED BIT (CLOSED)               
         XC    ACDTUSED,ACDTUSED                                                
         BAS   RE,PARTIAL                                                       
         CLI   PRESFLAG,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ACTIVE                                                        
         B     ORDATE                                                           
*                                                                               
*---------------------------------------------------------                      
*        RESTORE ORDER - MARK STATUS AS X'00'                                   
*---------------------------------------------------------                      
*                                                                               
ORDREST  CLI   ACTION,RES          TEST ACTION RESTORE                          
         BNE   ORDOPEN                                                          
         MVI   ACSTATUS,0                                                       
         XC    ACDTUSED,ACDTUSED                                                
         B     ORDATE                                                           
*                                                                               
*---------------------------------------------------------                      
*        OPEN ORDER - MARK STATUS AS X'00'                                      
*---------------------------------------------------------                      
*                                                                               
ORDOPEN  CLI   ACTION,OPE          TEST ACTION OPEN                             
         BE    *+6                                                              
         DC    H'0'                INVALID ACTION                               
         MVI   ACSTATUS,0          CLEAR ORDER STATUS                           
         XC    ACDTUSED,ACDTUSED                                                
         BAS   RE,REOPEN                                                        
         CLI   PRESFLAG,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ACTIVE                                                        
*                                                                               
*                                                                               
         USING GDAELD,R6                                                        
ORDATE   LA    R6,NGDAEL           BUILD A DATE ELEMENT                         
         XC    NGDAEL,NGDAEL                                                    
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATOSTA                                                 
         MVC   GDADATE,TODAYP                                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCFIL'),AIOAREA1,NGDAEL,0                  
         CLI   DMCB+12,0           EVERYTHING OK?                               
         BE    ORD00               YES                                          
         MVI   FERN,BIGERR                                                      
         CLI   DMCB+12,5           RECORD TOO BIG?                              
         BE    EXIT                YES                                          
         DC    H'0'                                                             
*                                                                               
ORD00    GOTO1 AWRITE,AIOAREA1     WRITE BACK RECORD                            
         BNE   EXIT                                                             
*                                                                               
         CLI   PRESFLAG,C'Y'       TEST PRESTO ORDER                            
         BNE   ORD02               NO                                           
         CLI   ACTION,CLO          TEST CLOSE                                   
         BE    *+12                YES                                          
         CLI   ACTION,OPE          TEST OPEN                                    
         BNE   ORD02               NO                                           
*                                                                               
         BAS   RE,POINT            HANDLE PRESTO ACTIVITY POINTERS              
         EJECT                                                                  
*---------------------------------------------------------                      
*        MARK JOB TRANSACTIONS                                                  
*---------------------------------------------------------                      
*                                                                               
*        ACTION DELETE & CLOSE MARKS TRANSACTIONS AS X'80'                      
*        ACTION RESTORE & OPEN UNMARKS OR REBUILDS THEM.                        
*                                                                               
ORD02    L     R4,AIOAREA2                                                      
         LA    R2,10                                                            
ORD03    XC    0(100,R4),0(R4)     CLEAR OUT IO AREA                            
         LA    R4,100(R4)                                                       
         BCT   R2,ORD03                                                         
*                                                                               
         L     R3,AIOAREA2                                                      
         L     R6,AIOAREA1         POINT R6 AT ORDER ELEMENT                    
         AH    R6,DATADISP                                                      
*                                                                               
ORD04    CLI   0(R6),0             END OF RECORD?                               
         BE    ORD80                                                            
         CLI   0(R6),X'67'         ORDER ELEMENT                                
         BE    ORD10                                                            
         CLI   0(R6),X'68'         ORDER AMOUNT ELEMENT                         
         BE    ORD40                                                            
ORD06    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ORD04               LOOP FOR NEXT ELEMENT                        
*                                                                               
         USING ACORDRD,R6                                                       
ORD10    LA    R5,KEY              BUILD JOB/EXP TRANSACTION KEY                
         XC    KEY,KEY             CLEAR OUT KEY                                
         MVC   ACKEYACC,ACORJOB    JOB ACCNT                                    
         MVC   JOBACCT,ACORJOB     SAVE JOB FOR LATER ON                        
         MVC   ACKEYWRK,=2C'*'     WORKCODE IS '**'                             
         MVC   ACKEYCON,ACORSUPP                                                
         CLI   TYPE,PRODN                                                       
         BE    ORD20                                                            
         MVC   ACKEYWRK,SPACES     FOR NON-PROD CLEAR WORK CODE                 
         MVI   ACKEYCON,C' '       AND FIRST BYTE OF CONTRA ACCOUNT             
ORD20    MVC   ACKEYDTE,ACORDATE                                                
         MVC   ACKEYREF,APONUM     STORE ORDER NUMBER                           
*                                                                               
*                                  BUILD TRANS RECORD IN IO2                    
         MVC   0(L'KEY,R3),KEY                                                  
         AH    R3,DATADISP                                                      
         USING TRANSD,R3           BUILD 44 ELEMENT                             
         MVC   TRNSEL(2),=X'441D'                                               
         MVC   TRNSDATE,ACORDATE                                                
         MVC   TRNSREF,APONUM                                                   
         MVI   TRNSSTAT,X'80'                                                   
         MVI   TRNSTYPE,X'0C'                                                   
         MVC   TRNSBTCH(2),THISMON                                              
         MVC   TRNSBTCH+2(4),SPACES                                             
         ZAP   TRNSAMNT,=P'0'      HAS NO AMOUNT                                
         MVC   TRNSANAL,=2C'*'                                                  
         MVI   TRNSNARR,C' '                                                    
         ZIC   R1,TRNSLEN                                                       
         AR    R3,R1                                                            
*                                                                               
         USING TRSELD,R3                                                        
         XC    0(TRSLNQ,R3),0(R3)  ADD TRANSACTION STATUS                       
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         GOTO1 VDATCON,DMCB,(1,TODAYP),(2,TRSDATE)                              
         SR    R1,R1                                                            
         IC    R1,TRSLN                                                         
         AR    R3,R1                                                            
         MVI   0(R3),0                                                          
         B     ORD06               LOOP BACK UP FOR 68 ELEMENTS                 
*                                                                               
         USING ACOAMTD,R3                                                       
ORD40    ZIC   R1,1(R6)            GET LENGTH OF 68 ELEMENT                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)       COPY OVER 68 ELEMENT                         
         ZAP   ACOPAMT,ACOAIVAL                                                 
         LA    R1,1(R1)                                                         
         AR    R3,R1               BUMP TO NEXT AVAILABLE SPOT                  
         MVI   0(R3),0             SET AS END OF RECORD JUST IN CASE            
         B     ORD06                                                            
*                                                                               
ORD80    GOTO1 AREADL,AIOAREA1     READ IT                                      
         TM    DMCB+8,X'FD'        ACCEPT DELETES                               
         BNZ   ORD100              IF RECORD IS NOT THERE THEN BUILD            
*                                  A NEW RECORD FOR OPEN & RESTORE              
         L     R5,AIOAREA1                                                      
         MVI   ACSTATUS,0                                                       
         CLI   ACTION,OPE          OPEN/RESTORE UNDELETES TRANSACTION           
         BE    *+8                                                              
         CLI   ACTION,RES                                                       
         BE    *+8                                                              
         MVI   ACSTATUS,X'80'      DELETE/CLOSE DELETES TRANSACTION             
         GOTO1 AWRITE                                                           
         BNE   EXIT                                                             
         B     OKEND                                                            
*                                                                               
ORD100   CLI   ACTION,OPE          ONLY REBUILD TRANS FOR OPEN/RESTORE          
         BE    ORD110                                                           
         CLI   ACTION,RES                                                       
         BE    ORD110                                                           
         B     OKEND                                                            
         DROP  R5                                                               
*                                                                               
ORD110   DS    0H                  INCOMPLETE RECORD IS IN IOAREA2              
         L     R3,AIOAREA2                                                      
         AH    R3,DATADISP                                                      
*                                                                               
ORD115   ZIC   R1,1(R3)                                                         
         CLI   1(R3),0             REACH END OF RECORD YET?                     
         BE    ORD120                                                           
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         B     ORD115                                                           
*                                                                               
ORD120   L     R4,AIOAREA2         POINT TO START                               
         USING ACKEYD,R4                                                        
         SR    R3,R4               GET LENGTH OF RECORD                         
         LA    R3,1(R3)                                                         
         STCM  R3,3,ACLENGTH       STORE RECORD LENGTH                          
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBACCT                                                  
         MVC   TEMPKEY,KEY                                                      
         GOTO1 ARDHI,AIOAREA3                                                   
         CLC   TEMPKEY,KEY                                                      
         BE    ORD123                                                           
         MVI   FERN,NOTFOUND                                                    
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
ORD123   L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
ORD125   CLI   0(R4),X'30'          CHECK IF JOB IS CLOSED                      
         BE    ORD130                                                           
         CLI   0(R4),0              END OF RECORD                               
         BNE   *+6                                                              
         DC    H'0'                 MUST FIND A 30 ELEMENT                      
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     ORD125                                                           
*                                                                               
         USING ACSTATD,R4                                                       
ORD130   DS    0H                                                               
         CLI   ACSTSTAT,X'40'      IS JOB CLOSED?                               
         BNE   ORD140                                                           
         MVI   FERN,CLOSED                                                      
         B     EXIT                                                             
*                                                                               
ORD140   DS    0H                                                               
         CLI   ACSTSTAT,X'20'      IS ACCOUNT LOCKED                            
         BNE   ORD150                                                           
         MVI   FERN,LOCKED                                                      
         B     EXIT                                                             
*                                                                               
ORD150   MVI   FERN,NOREST                                                      
         GOTO1 AADD,AIOAREA2       ADD THE RECORD                               
         BNE   EXIT                                                             
*                                                                               
OKEND    MVI   FERN,OK                                                          
         MVC   MSG,=CL60'ACTION COMPLETED - ENTER NEXT'                         
EXIT     XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------                      
*        TURN OFF PARTIAL PAY FLAG                                              
*---------------------------------------------------------                      
*                                                                               
PARTIAL  NTR1  ,                                                                
         L     R6,AIOAREA1                                                      
         AH    R6,DATADISP         SEARCH ORDER RECORD                          
         SR    R0,R0                                                            
*                                                                               
PARTIAL2 CLI   0(R6),0                                                          
         BE    PARTIALX                                                         
         CLI   0(R6),X'67'         TEST FOR ORDER ELEMENT                       
         BE    PARTIAL4            YES                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PARTIAL2                                                         
*                                                                               
         USING ACORDRD,R6                                                       
PARTIAL4 CLI   ACORLEN,ACORSTAT-ACORDRD  TEST ELEMENT HAS STATUS BYTE           
         BNH   *+8                                                              
         NI    ACORSTAT,X'FF'-ACORSPAR                                          
*                                                                               
PARTIALX B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
*---------------------------------------------------------                      
*        TURN OFF FULLY MATCHED AND TURN ON PARTIAL                             
*---------------------------------------------------------                      
*                                                                               
REOPEN   NTR1  ,                                                                
         L     R6,AIOAREA1                                                      
         AH    R6,DATADISP         SEARCH ORDER RECORD                          
         SR    R0,R0                                                            
*                                                                               
REOPEN2  CLI    0(R6),0                                                         
         BE    REOPENX                                                          
         CLI   0(R6),X'67'         TEST FOR ORDER ELEMENT                       
         BE    REOPEN4             YES                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     REOPEN2                                                          
*                                                                               
         USING ACORDRD,R6                                                       
REOPEN4  CLI   ACORLEN,ACORSTAT-ACORDRD  TEST ELEMENT HAS STATUS BYTE           
         BNH   REOPENX                                                          
         TM    ACORSTAT,ACORDMCH+ACORSPAR                                       
         BZ    REOPENX                                                          
         NI    ACORSTAT,X'FF'-ACORDMCH                                          
         OI    ACORSTAT,ACORSPAR                                                
*                                                                               
REOPENX B      EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
*---------------------------------------------------------                      
*        ADD POINTER ELEMENT FOR RECORD ACTIVITY KEY                            
*---------------------------------------------------------                      
*                                                                               
ACTIVE   NTR1  ,                                                                
*                                                                               
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
*                                                                               
ACTIVEX  B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        MAINTAIN RECORD ACTIVITY POINTERS                                      
*---------------------------------------------------------                      
*                                                                               
POINT    NTR1  ,                                                                
*                                                                               
         MVI   RAPACTN,RAPAPTR     POINTER MAINTENANCE                          
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    POINTX                                                           
         DC    H'0'                                                             
*                                                                               
POINTX   B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
* CONSTANTS                                                                     
*---------------------------------------------------------------                
*                                                                               
ACCOUNT  DC    C'ACCOUNT'                                                       
         LTORG                                                                  
         EJECT                                                                  
PROGD    DSECT                     LOCAL STORAGE                                
VRAPPER  DS    V                                                                
JOBACCT  DS    CL15                                                             
TEMPKEY  DS    CL42                                                             
NGDAEL   DS    CL(GDALNQ)          GENERAL DATE ELEMENT                         
       ++INCLUDE ACRAPPERD                                                      
PROGDX   EQU   *-PROGD                                                          
         EJECT                                                                  
       ++INCLUDE ACORDDSECT                                                     
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053ACORD02   10/17/18'                                      
         END                                                                    
