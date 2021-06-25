*          DATA SET ACBAT05    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T61B05A                                                                  
         TITLE 'MULTIPLE JOURNAL ENTRY'                                         
T61B05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT05**,RR=R5,CLEAR=YES                           
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
         EJECT                                                                  
*-------------------------------------------------------------                  
*        MULTIPLE JOURNAL ENTRY                                                 
*-------------------------------------------------------------                  
*                                                                               
         LA    R2,MULDOCH                                                       
         BAS   RE,ANY                                                           
         MVC   SAVEDOC,MULDOC                                                   
         OC    SAVEDOC,SPACES                                                   
         XC    SAVECAC,SAVECAC                                                  
         LA    R2,MULDATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   MULDATH+5,0                                                      
         BNE   MUL2                                                             
         BAS   RE,GETODAY                                                       
         B     MUL4                                                             
*                                                                               
MUL2     GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
MUL4     GOTO1 DATCON,DMCB,(0,WORK),(1,DATE3)                                   
         CLI   DATE3,X'70'                                                      
         BL    ERROR                                                            
         GOTO1 DATECHK,DMCB,DATE3                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        VALIDATE AND SAVE THE ACCOUNTS AND AMOUNTS                             
*-------------------------------------------------------------                  
*                                                                               
         LA    R2,MULFRSTH                                                      
         BAS   RE,ANY                                                           
         L     R4,=A(TABLE)                                                     
         A     R4,PRELOC                                                        
         USING POSTD,R4                                                         
         ST    R4,ADDTAB                                                        
*                                                                               
MUL10    MVC   KEY,SPACES                                                       
         ZIC   RF,5(R2)            READ ACCOUNT                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),8(R2)                                                   
         MVC   KEY(1),COMPANY                                                   
         SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC                                                        
         MVC   POSTACC,ACCTNUM                                                  
         MVC   POSTACCN,ACCTNAME                                                
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      BAL EL                                       
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'30'      LOCKED OR CLOSED                             
         BNZ   ERROR                                                            
*&&US                                                                           
         TM    ACCTSTAT,X'48'      DEPT/STAFF FLAGS                             
         BNZ   ERROR                                                            
         CLI   ACCTCOST,0                                                       
         BE    MUL11                                                            
         CLI   ACCTCOST,X'40'      ANALYSIS FILTER                              
         BE    MUL11                                                            
         CLI   ACCTCOST,C')'       OLD-STYLE COSTING GROUPS                     
         BE    MUL11                                                            
         CLI   ACCTCOST,C'('                                                    
         BNE   ERROR                                                            
*&&                                                                             
MUL11    DS    0H                                                               
         ST    R2,SAVER2                                                        
*                                  TRY TO READ CONTRA-ACCOUNT                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   5(R2),0             IF NO CONTRA A/C INPUT                       
         BNE   MUL11A                                                           
         OC    SAVECAC,SAVECAC     USE SAVED CONTRA IF PRESENT                  
         BNZ   *+8                                                              
         BAS   RE,ANY                                                           
         MVC   POSTCAC,SAVECAC                                                  
         MVC   POSTCACN,SAVECACN                                                
*&&US                                                                           
         MVI   ERRNUM,2                                                         
         CLC   POSTACC+1(2),=C'SR'  SAVED C/A CAN ONLY BE USED IF               
         BE    MUL11AA            BOTH PREVIOUS AND CURRENT ACCOUNTS            
         CLI   POSTCAC,X'40'      ARE SR OR IF NEITHER IS SR                    
         BE    ERROR                                                            
         B     MUL21                                                            
*                                                                               
MUL11AA  CLC   POSTCAC(3),SPACES                                                
         BNE   ERROR                                                            
*&&                                                                             
         B     MUL21                                                            
*                                                                               
MUL11A   DS    0H                                                               
         CLI   8(R2),X'41'                                                      
         BL    ERROR               THEY STARTED INPUT W. A BLANK                
         MVC   POSTCACN,SPACES                                                  
         MVC   WORK(14),8(R2)                                                   
         CLC   POSTACC+1(2),=C'SR'                                              
         BE    MUL11B                                                           
*&&UK                                                                           
         CLC   POSTACC+1(2),=C'SF'                                              
         BE    MUL11C                                                           
*&&                                                                             
         BNE   MUL11P                                                           
*                                                                               
MUL11B   MVI   ERRNUM,2                                                         
         CLC   WORK(3),=C'***'                                                  
         BNE   ERROR                                                            
         CLI   WORK+4,X'41'        MUST HAVE INPUT BEYOND ***                   
         BL    ERROR                                                            
*&&UK                                                                           
         B     MUL11P                                                           
*                                                                               
MUL11C   MVI   ERRNUM,2            UK MEDIA PAYEES                              
         CLI   WORK,C'A'                                                        
         BE    MUL12                                                            
         CLI   WORK,C'S'                                                        
         BNE   MUL11D                                                           
         CLI   WORK+1,C'F'         ALLOW SF,SC AND SI AS CONTRAS                
         BE    MUL12                                                            
         CLI   WORK+1,C'C'                                                      
         BE    MUL12                                                            
         CLI   WORK+1,C'I'                                                      
         BE    MUL12               BUT APART FOM THOSE                          
         CLI   WORK+1,C'E'         AND SE,SQ                                    
         BE    MUL12                                                            
         CLI   WORK+1,C'Q'                                                      
         BE    MUL12                                                            
*                                                                               
MUL11D   CLC   WORK(3),=C'***'     CONTRA MUST START WITH ***                   
         BNE   ERROR                                                            
         CLI   WORK+3,X'41'                                                     
         BL    ERROR               AND THERE MUST BE SOME INPUT                 
         MVC   POSTCAC,SPACES                                                   
         MVC   POSTCAC+3(11),WORK+3                                             
         OC    POSTCAC+3(11),SPACES                                             
         B     MUL20                                                            
*&&                                                                             
MUL11P   DS    0H                                                               
         CLC   WORK(3),=C'***'     ONLY ALLOW *** FOR SR CONTRA                 
         BNE   MUL12                                                            
         CLC   POSTACC+1(2),=C'SR'                                              
         BNE   ERROR                                                            
         MVC   POSTCAC,SPACES                                                   
         MVC   POSTCAC+3(11),WORK+3                                             
         OC    POSTCAC+3(11),SPACES                                             
         B     MUL20                                                            
*                                                                               
MUL12    DS    0H                                                               
         MVC   KEY+1(41),SPACES                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),WORK       ASSUME NO COMPANY INPUT                      
         BAS   RE,HIGH                                                          
         MVC   POSTCAC,KEYSAVE                                                  
         CLC   KEY(15),KEYSAVE                                                  
         BE    MUL14                                                            
*&&UK                                                                           
         CLC   POSTACC+1(2),=C'SR' CONTRA ACCOUNT MUST EXIST                    
         BE    POST13                                                           
         CLC   POSTACC+1(2),=C'29' EXCEPT FOR SR,29,2P                          
         BE    POST13                                                           
         CLC   POSTACC+1(2),=C'2P'                                              
         BE    POST13                                                           
         MVI   ERRNUM,17                                                        
         B     ERROR                                                            
POST13   DS    0H                                                               
*&&                                                                             
         MVC   POSTCAC,SPACES                                                   
         MVC   POSTCAC(14),KEYSAVE+1                                            
         B     MUL20                                                            
*                                                                               
MUL14    DS    0H                                                               
         BAS   RE,GETACC                                                        
         MVC   POSTCACN,ACCTNAME                                                
*                                                                               
MUL20    MVC   SAVECAC,POSTCAC                                                  
         MVC   SAVECACN,POSTCACN                                                
*                                                                               
MUL21    ZIC   RF,0(R2)            DR/CR                                        
         AR    R2,RF                                                            
         BAS   RE,ANY                                                           
         MVI   POSTDC,C'D'                                                      
         CLI   8(R2),C'D'                                                       
         BE    MUL21A                                                           
         MVI   POSTDC,C'C'                                                      
         MVI   ERRNUM,2                                                         
         CLI   8(R2),C'C'                                                       
         BNE   ERROR                                                            
*                                                                               
MUL21A   MVI   HALF,C'Y'                                                        
         BAS   RE,TESTIT           TEST EXCLUSIONS                              
         CLI   HALF,C'Y'                                                        
         BE    MUL21B                                                           
         L     R2,SAVER2                                                        
         MVI   ERRNUM,18                                                        
         B     ERROR                                                            
*                                                                               
MUL21B   CLI   POSTDC,C'D'         IS THIS A DEBIT?                             
         BNE   MUL22               NO                                           
         BAS   RE,CHKXJOB          YES, CHECK FOR AN XJOB                       
         CLI   ERRNUM,45           IS ACCOUNT AN XJOB                           
         BNE   MUL22                                                            
         L     R2,SAVER2                                                        
         B     ERROR               YES                                          
*                                                                               
MUL22    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   POSTCODE,SPACES                                                  
         CLC   POSTACC+1(2),PRODUL                                              
         BE    MUL22C                                                           
         TM    COMPSTAT,X'20'      OFFICE AGENCY NEEDS ANAL/OFFC INPT           
         BZ    MUL22A                                                           
         BAS   RE,ANY                                                           
         OC    8(2,R2),SPACES                                                   
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         BNE   ERROR                                                            
*                                                                               
MUL22A   DS    0H                                                               
*&&UK                                                                           
         CLI   TWAACCS,C'*'        OFFICE LOGON                                 
         BNE   MUL23                                                            
         CLC   POSTACC+1(2),PRODUL                                              
         BE    MUL23                                                            
         MVI   ERRNUM,SECLOCK                                                   
         CLC   8(1,R2),TWAACCS+1   SO TEST INPUT IS CORRECT OFFICE              
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1             AND 1-LONG                                   
         BNE   ERROR                                                            
*&&                                                                             
         B     MUL23                                                            
*                                                                               
MUL22C   BAS   RE,ANY                                                           
         MVI   ERRNUM,2                                                         
         CLC   =C'99',8(R2)                                                     
         BE    ERROR                                                            
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),=C'SJ'                                                  
         MVC   KEY+4(2),8(R2)                                                   
         BAS   RE,READ                                                          
*                                                                               
MUL23    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    MUL24                                                            
         CLC   =C'**',8(R2)                                                     
         BE    ERROR                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   POSTCODE(0),8(R2)                                                
*                                                                               
MUL24    ZIC   RF,0(R2)            CASH FIELD                                   
         AR    R2,RF                                                            
         ZIC   R3,5(R2)                                                         
         MVI   ERRNUM,25                                                        
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   POSTCASH,0(8,RF)                                                 
*                                                                               
         ZIC   RF,0(R2)            OUTPUT NAME OF ACCOUNT                       
         AR    R2,RF                                                            
         L     R3,SAVER2                                                        
         TM    4(R3),X'20'                                                      
         BO    MUL30                                                            
         OI    4(R3),X'20'                                                      
         GOTO1 CHOPPER,DMCB,(36,POSTACCN),(24,8(R2)),1                          
         FOUT  (R2)                                                             
*                                                                               
MUL30    AH    R4,TABWDTH                                                       
         MVI   0(R4),X'FF'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),10            TAB FIELD                                    
         BL    MUL60                                                            
         CLI   5(R2),0             ANY MORE                                     
         BNE   MUL10                                                            
         B     MUL60                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        NOW SEE IF DEBITS EQUAL CREDITS                                        
*-------------------------------------------------------------                  
*                                                                               
MUL60    L     R4,ADDTAB                                                        
         ZAP   DEBITS,=P'0'                                                     
         ZAP   CREDITS,=P'0'                                                    
*                                                                               
MUL62    LA    RF,DEBITS                                                        
         CLI   POSTDC,C'D'                                                      
         BE    *+8                                                              
         LA    RF,CREDITS                                                       
         AP    0(6,RF),POSTCASH                                                 
         AH    R4,TABWDTH                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   MUL62                                                            
*                                                                               
         CP    CREDITS,DEBITS                                                   
         BE    MUL70                                                            
         MVI   ERRNUM,X'FE'                                                     
         LA    R2,MULFCASH                                                      
         MVC   MSG,SPACES                                                       
         MVC   MSG(35),=C'ERROR - DEBITS DO NOT EQUAL CREDITS'                  
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*              NOW BUILD POSTING RECORD                                         
*-------------------------------------------------------------                  
*                                                                               
MUL70    LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         MVI   DLDSEL,X'64'        DESCRIPTION ELEMENT                          
         MVC   DLDSREF,SAVEDOC                                                  
         MVC   DLDSDATE,DATE3                                                   
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORISE                                    
         XC    DLDSNARR,DLDSNARR                                                
         MVI   DLDSNARR,C' '                                                    
         LA    RF,1                                                             
         CLI   MULNARH+5,0                                                      
         BE    MUL71                                                            
         IC    RF,MULNARH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),MULNAR                                               
         LA    RF,1(RF)                                                         
MUL71    LA    R3,DLDSNARR                                                      
         SR    R3,R7                                                            
         AR    R3,RF                                                            
         STH   R3,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         EJECT                                                                  
         AR    R7,R3               POSTING ELEMENTS                             
         USING DLPOSTD,R7                                                       
         L     R4,ADDTAB                                                        
*                                                                               
         USING POSTD,R4                                                         
MUL72    MVI   DLPSLEN,X'71'                                                    
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,POSTCASH                                                
         MVC   DLPSANAL,POSTCODE                                                
*                                                                               
         CLI   POSTDC,C'C'                                                      
         BE    MUL74                                                            
         MVC   DLPSDBAC(51),POSTACC                                             
         MVC   DLPSCRAC(51),POSTCAC                                             
         MVI   DLPSEL,X'69'        DEBIT                                        
         B     MUL76                                                            
*                                                                               
MUL74    MVC   DLPSCRAC(51),POSTACC                                             
         MVC   DLPSDBAC(51),POSTCAC                                             
         MVI   DLPSEL,X'6A'        CREDIT                                       
*                                                                               
MUL76    ZIC   RF,DLPSLEN                                                       
         AR    R7,RF                                                            
         AH    R4,TABWDTH                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   MUL72               DO ANOTHER                                   
*                                                                               
         MVI   0(R7),0             END OF RECORD                                
         LA    R3,IOAREA-1                                                      
         SR    R7,R3                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF      FINAL LENGTH                                 
         BAS   RE,PUTDAY                                                        
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),SAVEDOC     REFERENCE                                    
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,DEBITS                                                  
         BAS   RE,ADTWA1                                                        
         LA    R2,MULDOCH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*        TEST LOGICAL EXCLUSIONS                                                
*-------------------------------------------------------------                  
*                                                                               
TESTIT   NTR1                                                                   
         LA    RF,DLIST                                                         
         CLI   POSTDC,C'D'                                                      
         BE    *+8                                                              
         LA    RF,CLIST                                                         
*                                                                               
TEST2    CLI   0(RF),X'FF'                                                      
         BE    TEST4                                                            
         CLC   0(2,RF),POSTACC+1                                                
         BE    TESTNO                                                           
         LA    RF,2(RF)                                                         
         B     TEST2                                                            
*                                                                               
TEST4    CLC   =C'SE',POSTACC+1    TEST FURTHER FOR SE                          
         BNE   TESTYES                                                          
*                                                                               
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         GOTO1 VCATCALL                                                         
         CLI   CATPST,C'N'                                                      
         BE    TESTYES                                                          
*                                                                               
TESTNO   MVI   HALF,C'N'                                                        
*                                                                               
TESTYES  B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SEE IF GETACC HAS PUT AN XJOB IN ACCTNUM                               
*        RETURNS ERRNUM SET TO 45 IF AN XJOB                                    
*--------------------------------------------------------------                 
CHKXJOB  NTR1                                                                   
         MVI   ERRNUM,0                                                         
*                                                                               
         CLC   PRODUL,POSTACC+1    PRODUCTION LEDGER?                           
         BNE   CHKXX                                                            
*                                                                               
         LA    R3,POSTACC+3                                                     
         ZIC   R1,PRDLNGTH                                                      
         LR    R0,R1                                                            
         LA    R3,0(R1,R3)                                                      
         IC    R1,JOBLNGTH                                                      
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BNP   CHKXX               NO JOBS !!!!                                 
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES      IS THIS A JOB?                               
         BNH   CHKXX               NO                                           
*                                                                               
         LA    R3,POSTACC                                                       
         GOTO1 ASETJOB,DMCB,(R3)                                                
         TM    ACOPSTAT,ACOXJOB                                                 
         BNO   CHKXX                                                            
         MVI   ERRNUM,45                                                        
CHKXX    XIT1                                                                   
         EJECT                                                                  
XIT      XIT1                                                                   
*                                                                               
DLIST    DS    0H                                                               
*&&UK                                                                           
         DC    C'SVSXSFSIST'                                                    
*&&                                                                             
*&&US                                                                           
         DC    C'SKSISNSPSQSSSTSUSVSWSXSY'                                      
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
CLIST    DS    0H                                                               
*&&UK                                                                           
         DC    C'SESJ'                                                          
*&&                                                                             
*&&US                                                                           
         DC    C'SJ'                                                            
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
TABWDTH  DC    H'111'                                                           
         DS    0D                                                               
CATBLK   DS    XL(CATLNQ)                                                       
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ACBATCODE                                                              
*-------------------------------------------------------------                  
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ACBATDSECT                                                             
*-------------------------------------------------------------                  
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ACBATFAD                                                               
*-------------------------------------------------------------                  
*                                                                               
       ++INCLUDE ACBATFAD                                                       
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOCAL WORKING STORAGE                                                  
*-------------------------------------------------------------                  
*                                                                               
PROGD    DSECT                                                                  
SAVER2   DS    F                                                                
ADDTAB   DS    F                                                                
PRELOC   DS    F                                                                
DEBITS   DS    PL6                                                              
CREDITS  DS    PL6                                                              
SAVEDOC  DS    CL6                                                              
DATE3    DS    CL3                                                              
SAVECAC  DS    CL15                                                             
SAVECACN DS    CL36                                                             
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        DSECT FOR LINE IN POSTING TABLE                                        
*-------------------------------------------------------------                  
*                                                                               
POSTD    DSECT                                                                  
POSTACC  DS    CL15                                                             
POSTACCN DS    CL36                                                             
POSTCAC  DS    CL15                                                             
POSTCACN DS    CL36                                                             
POSTDC   DS    CL1                                                              
POSTCODE DS    CL2                                                              
POSTCASH DS    PL6                                                              
         EJECT                                                                  
*-------------------------------------------------------------                  
*        OTHER INCLUDES                                                         
*-------------------------------------------------------------                  
*                                                                               
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
*                                                                               
TABLE    CSECT                                                                  
         DS    1100C                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACBAT05   05/01/02'                                      
         END                                                                    
