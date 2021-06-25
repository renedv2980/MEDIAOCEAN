*          DATA SET ACREP7002  AT LEVEL 013 AS OF 09/17/02                      
*PHASE AC7002A                                                                  
         TITLE 'USE AND SALES TAX RATE LISTING'                                 
         PRINT NOGEN                                                            
AC7002   CSECT                                                                  
         NMOD1 0,**AC70**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC70D,RC                                                         
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
         USING ACKEYD,R4                                                        
         LA    R4,RECORD                                                        
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES  CLEAR KEY                    
         MVI   ACUTTYPE,X'2D'        BUILD TAX KEY                              
         MVI   ACUTSREC,X'01'                                                   
         MVC   ACUTCMP(1),RCCOMPFL                                              
         SPACE 1                                                                
         BAS   RE,HIGH                                                          
         B     READAA                                                           
READREC  LA    R4,RECORD                                                        
         MVC   RECORD(42),SAVEKEY    RESET KEY FOR NEXT READ                    
         BAS   RE,READREAD                                                      
         BAS   RE,READSEQ                                                       
READAA   MVC   SAVEKEY(42),RECORD    SAVE OLD KEY FOR READSEQ                   
         CLI   ACUTTYPE,X'2D'        STILL READING TAX RECORDS                  
         BNE   XIT                                                              
         CLI   ACUTSREC,X'01'        STILL SUB RECORD X'01'                     
         BNE   XIT                                                              
         CLC   ACUTCMP,RCCOMPFL      STILL READING SAME COMPANY                 
         BNE   XIT                                                              
         SPACE 1                                                                
         USING REPORTD,R7            DSECT FOR PRINT LNE                        
READA    LA    R7,P                                                             
         BAS   RE,GETNAME            RETURNS NAME IN WORK                       
         LA    RF,L'RENAME                                                      
         GOTO1 CHOPPER,DMCB,(36,WORK),((RF),RENAME),(C'P',2)                    
         MVC   RELOCAL,ACUTLOC                                                  
         SPACE 1                                                                
         USING ACTAXD,R4                                                        
         AH    R4,DATADISP           GET INTO RECORD FOR NEXTEL                 
READ01   MVI   ELCODE,X'5F'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   READX                 GET NEXT REC                               
         SPACE 1                                                                
         CLI   ACTAXLEN,X'20'        DOES THIS ONE HAVE THE CRED ACCT           
         BNE   READ01A               NO JUST RATE AND DATE                      
         SPACE 1                                                                
         MVC   CREDACC(14),ACTAXACC  FOR CREDNAME AND REPORT                    
         SPACE 1                                                                
READ01A  EQU   *                                                                
         ZAP   FULL,ACTAXRTE                                                    
         EDIT  (P4,FULL),(8,RERATE),4,DROP=3,ZERO=NOBLANK                       
         GOTO1 DATCON,DMCB,(1,ACTAXEFF),(5,REDATE) CONVERT FROM YMD             
         LA    R7,L'P(R7)            BUMP TO NEXT P AREA                        
         B     READ01                                                           
         SPACE 1                                                                
READX    BAS   RE,CREDNAME           GET CREDIT ACCT NAME                       
         LA    RF,L'RECRENM                                                     
         LA    R7,P                  POINT TO FIRST P AREA                      
         GOTO1 CHOPPER,DMCB,(36,WORK),((RF),RECRENM),(C'P',2)                   
         MVC   RECRED(14),CREDACC                                               
         SPACE 1                                                                
         CLI   SAVEKEY+12,0          IS THIS A BAD KEY                          
         BE    READX01               THEN DONT REPORT                           
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
READX01  B     READREC                                                          
         SPACE 1                                                                
         EJECT                                                                  
CREDNAME NTR1                                                                   
         USING ACKEYD,R4             GET CREDIT ACCOUNT NAME                    
         LA    R4,RECORD                      SPACE TO BUILD KEY                
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES  CLEAR KEY                    
         MVC   ACKEYACC(1),RCCOMPFL           HEX COMP FROM MONAC               
         MVC   ACKEYACC+1(14),CREDACC                                           
         BAS   RE,READREAD           EXACT READ                                 
         BAS   RE,GETNAME                                                       
         MVC   SVCREDNM(36),WORK                                                
         B     XIT                                                              
         SPACE 2                                                                
*              READ ACC FILE                                                    
HIGH     EQU   *                                                                
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     GTREC                                                            
READSEQ  EQU   *                                                                
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     GTREC                                                            
READREAD EQU   *                                                                
         MVC   COMMAND,=CL8'DMREAD'                                             
GTREC    EQU   *                                                                
         ST    RE,RESAVE                                                        
         LA    R4,RECORD                                                        
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT ',(R4),(R4)                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,RESAVE                                                        
         BR    RE                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
*              GET A COMPANY NAME FROM AN ACC RECORD                            
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVI   ELCODE,X'20'                    FOR NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                            NO NAME                          
         USING ACNAMED,R4                                                       
         MVC   WORK(36),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME              SAVE NAME AFTER LENGTH             
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
AC70D    DSECT                                                                  
RESAVE   DS    A                              AREA TO SAVE RE                   
ELCODE   DS    CL1                            FOR GETEL                         
PRTSTAT  DS    CL1                                                              
COMMAND  DS    CL8                            FOR DATAMGR                       
SAVEKEY  DS    CL42                           AREA TO BUILD KEY                 
CREDACC  DS    CL14                           SAVED JOB                         
SVDATE   DS    CL6                            SAVED DATE IN YYMMDD              
SVCOMPNM DS    CL36                           COMPANY NAME                      
SVCREDNM DS    CL36                           CREDIT ACCT NAME                  
SVLOCANM DS    CL36                           LOCALITY NAME                     
RECORD   DS    0D                             SPACE FOR DATAMGR READ            
         DS    CL1000                                                           
         SPACE 4                                                                
REPORTD  DSECT                       DSECT TO COVER PRINT LINE IN               
         DS    CL4                                                              
RELOCAL  DS    CL8                   REPORT                                     
         DS    CL3                                                              
RENAME   DS    CL20                                                             
         DS    CL5                                                              
REDATE   DS    CL8                                                              
         DS    CL4                                                              
RERATE   DS    CL8                                                              
         DS    CL6                                                              
RECRED   DS    CL14                                                             
         DS    CL2                                                              
RECRENM  DS    CL20                                                             
         EJECT                                                                  
       ++INCLUDE ACMASTD                                                        
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        LOGOD                                                                  
*        REPMASTD                                                               
*        REMOTED                                                                
*        REPXTRAD                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREP7002 09/17/02'                                      
         END                                                                    
