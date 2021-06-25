*          DATA SET PPREP03020 AT LEVEL 031 AS OF 05/01/00                      
*PHASE PP03020,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
* THIS PROGRAM WILL LOOKUP ESTIMATE/PUB UPLOAD REOCRD                           
* AND PRINT OUT UNIQUE INSERTION ID IN X'90' ELEMENT                            
*                                                                               
* QOPT5  Y= TEST RUN (DON'T MARK FILE)                                          
* QOPT6  Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                            
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PEUPREC,RC                                                       
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
*                                                                               
         ZAP   INCNT,=P'0'         AGYHDR RECS READ                             
         ZAP   DUMPCNT,=P'0'       RECORDS PRINTED                              
         ZAP   RPRCCNT,=P'0'       RECORDS PROCESSED                            
         ZAP   X90CNT,=P'0'        X90 ELEM PRINTED                             
         ZAP   YFFCNT,=P'0'        XFF FOUND                                    
         ZAP   NFFCNT,=P'0'        NO XFF FOUND                                 
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         LA    R0,PEUPREC                                                       
         ST    R0,AREC                                                          
                                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'90'          ESTIMATE UPLOAD RECORDS                     
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    DS    0H                                                               
*                                                                               
         CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              DONE                                         
         AP    INCNT,=P'1'                                                      
*                                                                               
         GOTO1 GETPRT              GET AGENCY REC                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   PROC10              NO                                           
*                                                                               
         AP    DUMPCNT,=P'1'                                                    
         BAS   RE,DMPKEY                                                        
         BAS   RE,DMPREC                                                        
*                                                                               
PROC10   DS    0H                                                               
*                                                                               
         XC    WKTEMP,WKTEMP                                                    
         CLI   KEY+18,X'FF'                                                     
         BE    PROC15                                                           
         AP    NFFCNT,=P'1'                                                     
         MVI   WKTEMP,C'*'         X'FF' NOT FOUND IN KEY+18                    
         B     *+10                                                             
PROC15   AP    YFFCNT,=P'1'                                                     
*                                                                               
*******  BAS   RE,DMPKEY                                                        
*                                                                               
         L     R2,AREC                                                          
         LA    R2,33(R2)                                                        
         USING PEUPMEL,R2                                                       
         MVI   ELCODE,X'90'        LOOKING FOR INSERTION ELEM                   
PROC30   BAS   RE,NEXTEL                                                        
         BNE   PROC35              LOOP UNTIL NO MORE                           
         AP    X90CNT,=P'1'                                                     
         MVC   P+01(20),=C'UNIQUE INSERTION ID '                                
         MVC   P+21(L'PEUPUNIQ),PEUPUNIQ                                        
*******  BAS   RE,RPRT                                                          
         B     PROC30                                                           
*                                                                               
PROC35   DS    0H                                                               
*                                                                               
         AP    RPRCCNT,=P'1'       RECORDS PROCESSED                            
*                                                                               
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC40              NO                                           
*                                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
PROC40   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC50              NO                                           
*                                                                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,DMPREC                                                        
*                                                                               
PROC50   DS    0H                                                               
*                                                                               
         B     PROC3               NEXT SEQ                                     
         DROP  R2                                                               
*                                                                               
PROC80  DS    0H                                                                
*                                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'         SKIP TO NEXT AGY/MED                         
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'90'         ESTIMATE UPLOAD RECORDS                      
         B     PROC2                                                            
*                                                                               
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(25),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+27(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,CNTLENQ(R4)      NEXT COUNTER IN TABLE                        
         B     RUNL5                                                            
*                                                                               
RUNL10   DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25               KEY LENGTH                                   
*                                                                               
         CLI   WKTEMP,C'*'         SEE IF X'FF' IS FOUND IN KEY+18              
         BNE   DKEY50                                                           
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEY **',(R5),C'DUMP',(R2),=C'1D'              
         B     DKEYXX                                                           
*                                                                               
DKEY50   GOTO1 =V(PRNTBL),DMCB,=C'KEY   ',(R5),C'DUMP',(R2),=C'1D'              
*                                                                               
DKEYXX   B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)         RECORD LENGTH                                
         SR    R2,R2                                                            
         LH    R2,HALF                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'REC   ',(R5),C'DUMP',(R2),=C'1D'              
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
CNTLENQ  EQU   33                                                               
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DS    PL8                                                              
         DC    CL25'RECORD(S) READ'                                             
RPRCCNT  DS    PL8                                                              
         DC    CL25'RECORDS(S) PROCESSED'                                       
YFFCNT   DS    PL8                                                              
         DC    CL25'FF IN PEUPEKEY'                                             
NFFCNT   DS    PL8                                                              
         DC    CL25'FF NOT IN PEUPEKEY'                                         
X90CNT   DS    PL8                                                              
         DC    CL25'X90 ELEM PRINTED'                                           
         DC    X'FF'                                                            
*                                                                               
DUMPCNT  DS    PL8                                                              
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
WKTEMP   DS    D                                                                
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PEUPLREC                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031PPREP0302005/01/00'                                      
         END                                                                    
