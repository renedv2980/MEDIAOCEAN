*          DATA SET PPREP0302V AT LEVEL 021 AS OF 07/06/00                      
*PHASE PP0302V,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
* THIS PROGRAM WILL CLEAR LAST 8 BYTES OF ACCOUNT NUMBER IN                     
* AGENCY HEADER RECORD (PREPARE IT FOR RFP ID USES)                             
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
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9       NOTICE USE OF R9 AS 2ND BASE REG             
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
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'01'          AGENCY RECORDS                              
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              END OF MEDIA GO DO NEXT                      
         AP    INCNT,=P'1'                                                      
*                                                                               
         GOTO1 GETPRT              GET AGENCY REC                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   PROC10              NO                                           
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'50'      50 RECORDS DUMPED ?                          
         BH    PROC10              YES                                          
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+54(16),=C'*** BEFORE *** #'                                    
         EDIT  (P8,DUMPCNT),(8,P+70),ALIGN=LEFT,COMMAS=YES,ZERO=BLANK           
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,DMPREC                                                        
         BAS   RE,RPRT             SKIP 2 LINES                                 
         BAS   RE,RPRT                                                          
*                                                                               
PROC10   DS    0H                                                               
*                                                                               
         LA    R2,PAGYREC+33                                                    
         CLI   0(R2),X'01'         FIRST AGENCY ELEMENT CODE                    
         BE    *+6                                                              
         DC    H'0'                ELEMENT MUST BE THERE!                       
*                                                                               
         USING PAGYELEM,R2                                                      
         XC    PAGYACCT+10(8),PAGYACCT+10                                       
         AP    RPRCCNT,=P'1'                                                    
*                                                                               
         SR    R5,R5               CLEAR END OF RECORD                          
         IC    R5,PAGYREC+25                                                    
         SLL   R5,8                                                             
         IC    R5,PAGYREC+26                                                    
         SR    RE,RE                                                            
         LA    RE,PAGYREC                                                       
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PAGYREC                                                       
         LA    RF,2000(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC40              NO                                           
*                                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
PROC40   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC50              NO                                           
         CP    DUMPCNT,=P'50'      50 RECORDS DUMPED ?                          
         BH    PROC50              YES                                          
*                                                                               
         MVC   P+55(15),=C'*** AFTER *** #'                                     
         EDIT  (P8,DUMPCNT),(8,P+70),ALIGN=LEFT,COMMAS=YES,ZERO=BLANK           
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,DMPREC                                                        
         BAS   RE,RPRT             SKIP 2 LINES                                 
         BAS   RE,RPRT                                                          
*                                                                               
PROC50   DS    0H                                                               
*                                                                               
         B     PROC3               NEXT SEQ                                     
         DROP  R2                                                               
*                                                                               
PROC80   DS    0H                                                               
*                                                                               
         CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'               END OF FILE                              
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'01'         AGENCY RECORDS                               
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
         GOTO1 =V(PRNTBL),DMCB,=C'KEY   ',(R5),C'DUMP',(R2),=C'1D'              
*                                                                               
         B     EXIT                                                             
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
         GOTO1 =V(PRNTBL),DMCB,=C'AGYREC',(R5),C'DUMP',(R2),=C'1D'              
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
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREP0302V07/06/00'                                      
         END                                                                    
