*          DATA SET DDDT02TAB  AT LEVEL 003 AS OF 02/08/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DT02TABA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'BUILD CONVERSION LAYOUT TABLES FOR EXTERNS'                     
DT02TAB  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         NBASE 0,DT02TAB,=V(REGSAVE),R9                                         
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
         IF (NE)                                                                
           MVC   P(33),=C'*** ERROR: INVALID PARAMETER CARD'                    
           GOTO1 =V(PRINTER)                                                    
           LHI   RF,20             SET BAD RETURN CODE                          
         ELSE ,                                                                 
           BAS   RE,BUILD_PAN1_CARDS BUILD PAN#1 SYSIN CARDS                    
           MVC   P(40),=C'CALLING ICETOOL TO BUILD LAYOUT TABLE(S)'             
           GOTO1 =V(PRINTER)                                                    
           SR    R1,R1             USING TOOLIN INTERFACE:                      
           LINK  EP=ICETOOL         CALL ICETOOL TO BUILD LAYOUT TABLES         
         ENDIF ,                                                                
*                                                                               
         ST    RF,RETCODE          HIGHEST ICETOOL OPERATOR RETURN CODE         
         MVC   P(22),=C'ICETOOL RETURN CODE = '                                 
         EDIT  RETCODE,(2,P+22),ALIGN=LEFT,ZERO=NOBLANK                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* RC = 0: NO ERRORS FOUND, BUT THERE MAY OR MAY NOT BE ANY DATES                
* RC = 4: DFSORT WARNING (USUALLY NOT A PROBLEM)                                
* RC = 8: WE FOUND USER ERRORS DURING LAYOUT TABLE GENERATION                   
*          (OR NO DATE FIELDS FOUND TO CONVERT)                                 
* RC > 8: SERIOUS DFSORT OR ICETOOL ERROR                                       
*                                                                               
         SELECT CLC,RETCODE,EQ                                                  
           WHEN (=F'0')                                                         
             LA    R1,ICEPAR1      COUNT OPERATOR VIA PARAMETER LIST            
             LINK  EP=ICETOOL      CONFIRM WE GENERATED A TABLE                 
             IF (CHI,RF,GT,8)      SUCCESSFUL ICETOOL CALL?                     
               J *+2               NO: HOW CAN A COUNT OPERATOR FAIL?!?         
             ELSEIF (CHI,RF,EQ,8)                                               
               MVC  P(36),=C'*** ERROR: NO COMPRESSED DATES FOUND'              
               MVC  RETCODE,=F'8'  FORCE RETURN CODE TO 8                       
             ELSE ,                                                             
               MVC  P(34),=C'SUCCESSFUL LAYOUT TABLE GENERATION'                
             ENDIF ,                                                            
             GOTO1 =V(PRINTER)                                                  
           WHEN (=F'4')                                                         
             MVC   P(37),=C'NON-FATAL DFSORT WARNING(S) GENERATED'              
             GOTO1 =V(PRINTER)                                                  
           WHEN (=F'8')                                                         
             MVC  P(29),=C'*** USER ERROR(S) ENCOUNTERED'                       
             GOTO1 =V(PRINTER)                                                  
           OTHRWISE ,                                                           
             MVC P(44),=C'*** SEVERE DFSORT/ICETOOL ERROR(S): SEE DEIS'         
             GOTO1 =V(PRINTER)                                                  
             J   *+2                FORCE ABEND                                 
         ENDSEL ,                                                               
*                                                                               
         MVC   P(20),=C'PROCESSING COMPLETED'                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
*                                                                               
READCRDS NTR1  ,                                                                
*                                                                               
         MVC   TITLE(26),=C'BUILD RECORD LAYOUT TABLES'                         
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXTCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    YES                 ALL CARDS ARE VALID                          
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    NEXTCARD            YES                                          
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   *+18                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      DDSIO= OVERRIDE                              
         B     NEXTCARD                                                         
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     NEXTCARD                                                         
*                                                                               
         CLC   =C'MAPMEMBER=',CARD                                              
         BNE   NO                  INVALID PARAMETER CARD                       
         MVC   MAPMEMBR(L'MAPMEMBR+1),CARD+10  PAN OUTPUT MEMBER NAME           
         CLI   MAPMEMBRX,C' '      MEMBERNAME EXCEEDS 10 CHAR. MAX?             
         BE    NEXTCARD            NO                                           
         MVC   P(33),=C'*** ERROR: INVALID MAPMEMBER NAME'                      
         GOTO1 =V(PRINTER)                                                      
         B     NO                                                               
*                                                                               
         ANSR  ,                                                                
         EJECT                                                                  
*                                                                               
BUILD_PAN1_CARDS NTR1 ,                                                         
*                                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* CALL PANIC TO SEE IF THE OUTPUT MEMBER EXISTS IN THE DEVELOPER'S              
* PERSONAL PAN LIBRARY                                                          
*                                                                               
         OPEN  (PAN1CRDS,OUTPUT)                                                
         MVC   CARDOUT(18),=C'++OPTION INPUT,SEQ'                               
         PUT   PAN1CRDS,CARDOUT                                                 
         MVC   CARDOUT,SPACES                                                   
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'20',=C'READ'),=C'DIR',MAPMEMBR,CARD            
*                                                                               
* GENERATE PAN#1 CONTROL CARDS                                                  
*                                                                               
         IF (TM,DMCB+8,X'10',Z)    IF MEMBER ALREADY EXISTS:                    
           MVC   CARDOUT(8),=C'++UPDATE'         UPDATE IT                      
           MVC   CARDOUT+9(L'MAPMEMBR),MAPMEMBR                                 
           LA    R2,CARDOUT+9                                                   
           CLI   0(R2),C' '                                                     
           BE    *+12                                                           
           LA    R2,1(R2)                                                       
           B     *-12                                                           
           MVC   0(6,R2),=C',0,ALL'                                             
         ELSE  ,                   OTHERWISE:                                   
           MVC   CARDOUT(5),=C'++ADD'            ADD IT                         
           MVC   CARDOUT+6(L'MAPMEMBR),MAPMEMBR                                 
           LA    R2,CARDOUT+6                                                   
           CLI   0(R2),C' '                                                     
           BE    *+12                                                           
           LA    R2,1(R2)                                                       
           B     *-12                                                           
           MVC   0(4,R2),=C',BAL'                                               
         ENDIF ,                                                                
         PUT   PAN1CRDS,CARDOUT                                                 
*                                                                               
         MVC   CARDOUT,SPACES                                                   
         MVC   CARDOUT(9),=C'++COMMENT'  ADD FIXED PANVALET COMMENT             
         MVC   CARDOUT+10(L'MAPMEMBR),MAPMEMBR                                  
         LA    R2,CARDOUT+10                                                    
         CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVC   0(35,R2),=C',AUTO-GENERATED RECORD LAYOUT TABLE'                 
         PUT   PAN1CRDS,CARDOUT                                                 
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'20',=C'CLOSE'),=C'PAN',0,0                     
         CLOSE PAN1CRDS                                                         
*                                                                               
         XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
PAN1CRDS DCB   DDNAME=PAN1CRDS,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM              
         SPACE 3                                                                
DUB      DS    D                                                                
RETCODE  DC    F'0'                RETURN CODE FROM THIS PROGRAM                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
CARD     DS    CL80                                                             
CARDOUT  DC    CL80' '                                                          
MAPMEMBR DS    CL10                PANVALET OUTPUT MEMBER NAME                  
MAPMEMBRX DC   C' '                FOR VALIDITY CHECK                           
         EJECT                                                                  
ICEPAR1  DC    A(0)                ICETOOL PARAMETER LIST                       
         DC    A(COUNTOPR)         A(COUNT STATEMENT)                           
         DC    A(0)                NO RETURN AREA NEEDED                        
         DC    X'FFFFFFFF'         EOL                                          
*                                                                               
* NOTE: "ALLDATES" IS GENERATED BY THE ICETOOL TABLE GENERATION                 
*   PROCESS. IF IT IS EMPTY, THEN THERE ARE NO ELIGIBLE DATES FOR               
*   ANY LAYOUT TABLE.                                                           
COUNTOPR DC    AL2(80)             LENGTH OF ONE ICETOOL CONTROL CARD           
         DC    CL80'COUNT FROM(ALLDATES) EMPTY RC8'                             
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 2                                                                
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDDT02TAB 02/08/21'                                      
         END                                                                    
