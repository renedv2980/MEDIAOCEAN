*          DATA SET ACBAT02    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T61B02A                                                                  
         TITLE 'JOURNAL ENTRY'                                                  
T61B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT02,CLEAR=YES                                   
         USING TWAD,RA             RA=A(TWA0)                                   
*        USING T602D,R9            TWA1                                         
         L     R9,4(R1)            PASSED BY ROOT                               
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         EJECT                                                                  
*-------------------------------------------------------------                  
*        VALIDATE DEBIT & CREDIT ACCOUNTS                                       
*-------------------------------------------------------------                  
*                                                                               
         MVC   KEY,SPACES                                                       
         SR    R6,R6               NO PROFILES                                  
         MVC   KEY(1),COMPANY                                                   
         LA    R2,TRADACH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,17                                                        
         SR    R3,R3                                                            
         IC    R3,TRADACH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),TRADAC                                                  
         BAS   RE,GETACC                                                        
         MVC   DRNAME,ACCTNAME                                                  
         MVC   DRNUM,ACCTNUM                                                    
         MVI   ERRNUM,18                                                        
         CLC   DRNUM+1(2),=C'SR'                                                
         BE    ERROR                                                            
*&&US                                                                           
         CLC   DRNUM+1(2),=C'SS'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'ST'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'SP'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'SQ'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'SV'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'SW'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'SX'                                                
         BE    ERROR                                                            
         CLC   DRNUM+1(2),=C'SY'                                                
         BE    ERROR                                                            
*&&                                                                             
         TM    ACCTSTAT,X'80'      CAN WE POST TO THIS ACCOUNT                  
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      LOCKED ACCOUNT                               
         BO    ERROR                                                            
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'      CLOSED JOB                                   
         BO    ERROR                                                            
         MVI   ERRNUM,18                                                        
         MVI   SW,C'Y'                                                          
         CLC   COUNTRY,=C'US'                                                   
         BE    TRAN1                                                            
         CLI   DRNUM,C'1'          JWT LONDON                                   
         BNE   TRAN1                                                            
         CLC   DRNUM+1(2),=C'SI'   SKIP TEST IF BOTH 'SI'                       
         BNE   TRAN1                                                            
         CLC   TRACAC(2),=C'SI'    SKIP TEST IF BOTH 'SI'                       
         BE    *+12                                                             
*                                                                               
TRAN1    DS    0H                                                               
         MVI   WORK,C'D'                                                        
         BAS   RE,TESTIT                                                        
         CLI   SW,C'Y'                                                          
         BNE   ERROR                                                            
         TM    TRADACH+4,X'20'                                                  
         BO    TRAN2                                                            
         MVC   TRADACN,DRNAME                                                   
         OI    TRADACNH+6,X'80'                                                 
         OI    TRADACH+4,X'20'                                                  
*                                                                               
TRAN2    LA    R2,TRACACH                                                       
         BAS   RE,ANY                                                           
         MVC   KEY+1(14),SPACES                                                 
         MVI   ERRNUM,17                                                        
         IC    R3,TRACACH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),TRACAC                                                  
         BAS   RE,GETACC                                                        
         MVC   CRNAME,ACCTNAME                                                  
         MVC   CRNUM,ACCTNUM                                                    
         MVI   ERRNUM,18                                                        
         CLC   CRNUM+1(2),=C'SR'                                                
         BE    ERROR                                                            
         TM    ACCTSTAT,X'80'      CAN WE POST TO THIS ACCOUNT                  
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      LOCKED ACCOUNT                               
         BO    ERROR                                                            
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'      CLOSED JOB                                   
         BO    ERROR                                                            
         MVI   ERRNUM,18                                                        
         CLC   COUNTRY,=C'US'      JWT LONDON                                   
         BE    TRAN2A                                                           
         CLI   CRNUM,C'1'                                                       
         BNE   TRAN2A                                                           
         CLC   CRNUM+1(2),=C'SI'   SKIP FOR 'SI'                                
         BE    *+12                                                             
*                                                                               
TRAN2A   MVI   WORK,C'C'                                                        
         BAS   RE,TESTIT                                                        
         CLI   SW,C'Y'                                                          
         BNE   ERROR                                                            
         TM    TRACACH+4,X'20'                                                  
         BO    TRAN2B                                                           
         MVC   TRACACN,CRNAME                                                   
         OI    TRACACNH+6,X'80'                                                 
         OI    TRACACH+4,X'20'                                                  
*                                                                               
TRAN2B   LA    R2,TRADACH          NOW TEST THE UNITS                           
         CLI   TRADAC,C'1'                                                      
         BL    TRAN3                                                            
         CLC   TRADAC(1),TRACAC    IF UNITS ARE 1,2 OR 3,THEY                   
         BNE   ERROR               MUST BE BOTH THE SAME UNITS                  
         B     TRAN4                                                            
*                                                                               
TRAN3    CLI   TRACAC,C'G'         DR UNIT IS G OR S                            
         BE    TRAN4                                                            
         CLI   TRACAC,C'S'         AND CR UNIT MUST BE G OR S ALSO              
         BE    TRAN4                                                            
         CLC   COUNTRY,=C'US'                                                   
         BE    TRAN3A                                                           
         CLI   TRACAC,C'A'         PLUS UNIT A FOR UK                           
         BE    TRAN4                                                            
*                                                                               
TRAN3A   DS    0H                                                               
         LA    R2,TRACACH                                                       
         B     ERROR                                                            
*                                                                               
TRAN4    DS    0H                                                               
         TM    COMPSTAT,X'20'      OFFICE AGENCY MUST HAVE WORK-CODES           
         BZ    TRAN4A                                                           
         LA    R2,TRAWRKDH                                                      
         BAS   RE,ANY                                                           
         LA    R2,TRAWRKCH                                                      
         BAS   RE,ANY                                                           
*                                                                               
TRAN4A   DS    0H                                                               
*&&UK                                                                           
         CLI   TWAACCS,C'*'        FOR OFFICE LOGON-TEST CORRECT INPUT          
         BNE   TRAN4B                                                           
         MVI   ERRNUM,SECLOCK                                                   
         CLC   PRODUL,TRADAC       BUT NOT FOR JOBS                             
         BE    TRAN4AA                                                          
         LA    R2,TRAWRKDH                                                      
         CLC   8(1,R2),TWAACCS+1                                                
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
*                                                                               
TRAN4AA  LA    R2,TRAWRKCH                                                      
         MVI   ERRNUM,SECLOCK                                                   
         CLC   8(1,R2),TWAACCS+1                                                
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
*&&                                                                             
*                                                                               
TRAN4B   LA    R4,COMPEL           NOW DEAL WITH WORK-CODES                     
         USING ACCOMPD,R4                                                       
         CLC   ACMPJOB,TRADAC                                                   
         BNE   TRAN4G                                                           
         LA    R2,TRAWRKDH                                                      
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),DRNUM                                                   
         MVC   KEY+4(2),TRAWRKD                                                 
         MVI   ERRNUM,19           INVALID WORKCODE IF ERROR                    
         BAS   RE,READ                                                          
         LA    R5,IOAREA                                                        
*                                                                               
TRAN4C   CLI   0(R5),0                                                          
         BE    TRAN4G                                                           
         CLI   0(R5),X'12'                                                      
         BE    TRAN4E                                                           
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     TRAN4C                                                           
*                                                                               
         USING ACANALD,R5                                                       
TRAN4E   MVC   TRADWCN,ACANDESC                                                 
         OI    TRADWCNH+6,X'80'                                                 
*                                                                               
TRAN4G   DS    0H                                                               
         LA    R2,TRACACH          CAN'T POST CREDITS TO JOBS                   
         MVI   ERRNUM,2                                                         
         CLC   ACMPJOB,TRACAC                                                   
         BE    ERROR                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD '64' ELEMENT (DESCRIPTION)                                       
*-------------------------------------------------------------                  
*                                                                               
TRAN5    LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         LA    R2,TRADOCH                                                       
         BAS   RE,ANY                                                           
         MVC   DLDSREF,SPACES                                                   
         IC    R3,TRADOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),TRADOC                                                
         LA    R2,TRADATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   TRADATH+5,0         ANY DATE INPUT                               
         BNE   TRAN6               IF THERE WAS BRANCH                          
         BAS   RE,GETODAY                                                       
         B     TRAN8                                                            
*                                                                               
TRAN6    DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   WORK(2),=C'70'                                                   
         BL    ERROR                                                            
*                                                                               
TRAN8    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,DLDSDATE)                                
         GOTO1 DATECHK,DMCB,DLDSDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORISE                                    
*                                                                               
         LA    R2,TRANARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               LENGTH OF ELEMENT - NARRATIVE                
         AH    R5,=H'2'            2 BYTE LENGTH FIELD                          
         AR    R5,R6               R6 = LENGTH OF NARRATIVE                     
         STH   R5,HALF                                                          
         MVC   IOAREA(2),HALF      RUNNING TOTAL                                
         SH    R5,=H'2'            REMOVE LENGTH OF LENGTH FIELD                
         STH   R5,FULL                                                          
         MVC   DLDSLEN,FULL+1                                                   
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD '68' ELEMENT                                                     
*-------------------------------------------------------------                  
*                                                                               
         AR    R8,R5               BUMP TO NEXT ELEMENT                         
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'68'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,DRNUM                                                   
         MVC   DLPSDBNM,DRNAME                                                  
         MVC   DLPSCRAC,CRNUM                                                   
         MVC   DLPSCRNM,CRNAME                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SPACES                                                  
*                                                                               
         LA    R2,TRAAMTH                                                       
         SR    R3,R3                                                            
         IC    R3,TRAAMTH+5                                                     
         MVI   ERRNUM,25                                                        
         GOTO1 AMTVAL,DMCB,TRAAMT,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)                                                         
         ZAP   DLPSAMNT,0(8,R4)                                                 
         ZAP   TRANSAMT,DLPSAMNT                                                
*                                                                               
         MVC   HALF,IOAREA         LENGTH OF FIRST ELEMENT                      
         LH    R4,HALF                                                          
         IC    R3,DLPSLEN          LENGTH OF SECOND ELEMENT                     
         AR    R8,R3                                                            
         MVI   0(R8),0             END OF RECORD                                
         LA    R3,1(R3)                                                         
         AR    R4,R3                                                            
         STH   R4,HALF                                                          
         MVC   IOAREA(2),HALF      LENGTH OF TOTAL                              
         CLC   TRADAC(2),=C'SJ'                                                 
         BE    TRAN10                                                           
         CLC   TRACAC(2),=C'SJ'                                                 
         BE    TRAN10                                                           
         CLI   TRAWRKDH+5,0        ANY WORK CODE INPUT                          
         BNE   TRAN10              GO AND BUILD 69 & 6A ELEMENTS                
         CLI   TRAWRKCH+5,0                                                     
         BNE   TRAN10                                                           
         B     TRAN12                                                           
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD 69 & 6A ELEMENTS                                                 
*-------------------------------------------------------------                  
*                                                                               
TRAN10   DS    0H                                                               
         LR    R7,R8                                                            
         BCTR  R3,R0                                                            
         SR    R8,R3               RESET TO START OF '68' ELEMENT               
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R8)       DUPLICATE 68 ELEMENT                         
         LA    R3,1(R3)            R3 = DLPSLEN AGAIN                           
         MVI   DLPSEL,X'69'        CONVERT 68 TO A 69                           
         MVI   ERRNUM,19                                                        
         LA    R2,TRAWRKDH         PLAY AROUND WITH WORK CODE (DEBIT)           
         CLC   TRAWRKD,=C'99'                                                   
         BE    ERROR                                                            
         CLI   TRAWRKDH+5,0                                                     
         BE    *+10                                                             
         MVC   DLPSANAL,TRAWRKD                                                 
         OC    DLPSANAL,SPACES                                                  
         DROP  R8                  USE R7 FOR SECOND ELEMENT                    
*                                                                               
         USING DLPOSTD,R7          CONVERT SECOND 68 TO A 6A                    
         MVI   DLPSEL,X'6A'                                                     
         LA    R2,TRAWRKCH                                                      
         CLC   TRAWRKC,=C'99'                                                   
         BE    ERROR                                                            
         MVC   DLPSANAL,SPACES                                                  
         CLI   TRAWRKCH+5,0                                                     
         BE    *+10                                                             
         MVC   DLPSANAL,TRAWRKC                                                 
         OC    DLPSANAL,SPACES                                                  
*                                                                               
         AR    R7,R3                                                            
         MVI   0(R7),0             NEW END OF RECORD                            
         LH    R4,HALF             LENGTH OF FIRST TWO ELEMENTS + 3             
         AR    R4,R3               R3 CONTAINS DLPSLEN                          
         STH   R4,HALF                                                          
         MVC   IOAREA(2),HALF      NEW TOTAL                                    
TRAN12   BAS   RE,PUTDAY           ADD TO ACCDAY                                
         EJECT                                                                  
*-------------------------------------------------------------                  
*        BUILD TWA1 RECORD & PUT IT THERE                                       
*-------------------------------------------------------------                  
*                                                                               
         XC    WORK,WORK                                                        
         IC    R3,TRADOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TRADOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         LA    R2,TRADOCH                                                       
         MVI   ERRNUM,X'FF'        RETURN 'OK' TO ROOT                          
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOOK FOR ACCOUNT FLAGS TO DISQUALIFY POSTING                           
*-------------------------------------------------------------                  
*                                                                               
TESTIT   NTR1                                                                   
         CLI   ACCTCOST,C' '       ANALYSIS FLAG                                
         BE    TEST2                                                            
         CLI   ACCTCOST,0                                                       
         BE    TEST2                                                            
         CLI   ACCTCOST,C'('       SPECIAL FLAG FOR COSTING CLIENTS             
         BE    TEST2                                                            
         CLI   ACCTCOST,C')'                                                    
         BNE   NOEXIT                                                           
*                                                                               
TEST2    DS    0H                                                               
         TM    ACCTSTAT,X'40'      PERSONAL EXPENSE ITEM                        
         BO    NOEXIT                                                           
         TM    ACCTSTAT,X'08'      DEPARTMENTAL FLAG                            
         BO    NOEXIT                                                           
*                                                                               
         LA    RF,DLIST            NOW TEST DEBIT/CREDIT EXCLUSIONS             
         CLI   WORK,C'D'                                                        
         BE    *+8                                                              
         LA    RF,CLIST                                                         
*                                                                               
TEST4    CLI   0(RF),X'FF'                                                      
         BE    YESEXIT                                                          
         CLC   0(2,RF),8(R2)                                                    
         BE    NOEXIT                                                           
         LA    RF,2(RF)                                                         
         B     TEST4                                                            
*                                                                               
NOEXIT   MVI   SW,C'N'                                                          
YESEXIT  XIT1                                                                   
*                                                                               
DLIST    DS    0H                                                               
*&&UK                                                                           
         DC    C'SVSXSFSIST'                                                    
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
CLIST    DS    0H                                                               
*&&UK                                                                           
         DC    C'SE'                                                            
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ACBATCODE                                                              
*-------------------------------------------------------------                  
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ACBATDSECT                                                             
*-------------------------------------------------------------                  
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*-------------------------------------------------------------                  
*        ACBATFDD                                                               
*-------------------------------------------------------------                  
*                                                                               
       ++INCLUDE ACBATFDD                                                       
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOCAL WORKING STORAGE                                                  
*-------------------------------------------------------------                  
*                                                                               
PROGD    DSECT                                                                  
SW       DS    CL1                                                              
DRNUM    DS    CL15                                                             
DRNAME   DS    CL36                                                             
CRNUM    DS    CL15                                                             
CRNAME   DS    CL36                                                             
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
* ACGENBOTH                                                                     
* ACGENDAY                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACBAT02   05/01/02'                                      
         END                                                                    
