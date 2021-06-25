*          DATA SET ACREPXJ02  AT LEVEL 012 AS OF 08/16/00                      
*PHASE ACXJ02A,+0                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DDS BILLING DOWNLOAD RECORDS'                                   
ACXJ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXJ**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXJD,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,ACCFRST                                                     
         BE    ACCFST                                                           
         CLI   MODE,PROCTRNS                                                    
         BE    PRCTRN                                                           
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  RUN FIRST                                                                    
******************************************************************              
*                                                                               
RUNF     MVC   VTYPES(VTYPLNQ),ADCONS     RELOCATE ADDRESSES                    
*                                                                               
         ZAP   CNTJOBS,=P'0'                                                    
         ZAP   CNTTRAN,=P'0'                                                    
*                                                                               
RUNFX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REQUEST FIRST                                                                
******************************************************************              
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         XC    PRTFLG,PRTFLG                                                    
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  LEVEL A FIRST                                                                
******************************************************************              
*                                                                               
ACCFST   DS    0H                                                               
*                                                                               
         OI    PRTFLG,NEWJOB                                                    
*                                                                               
ACCFSTX  B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PROCESS TRANSACTIONS                                                         
******************************************************************              
*                                                                               
PRCTRN   DS    0H                                                               
         L     R2,ADTRANS                                                       
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        X'44' TRANSACTION ELEMENT                    
         BNE   PRTRX                                                            
*                                                                               
         CLI   TRNTYPE,TRNTWRTO    TYPE 57 - WRITEOFF OR RECOVERY               
         BNE   PRTRX                                                            
*                                                                               
         LR    R3,R2               GET TO TRAN REC                              
         SH    R3,DATADISP                                                      
         USING TRNRECD,R3                                                       
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,TRNRLEN                                                     
         CHI   R5,TRNLN1Q          IS THERE A NARRATIVE?                        
         BNH   PRTRX                NO                                          
*                                                                               
         CLC   TRNNARR+9(8),=C'FEB17/93' -  THIS WAS A BAD DAY                  
         BNE   PRTRX                                                            
*       --------------------------------------                                  
         OI    PRTFLG,NEWTRAN                                                   
*                                                                               
         LR    R6,R2                                                            
*                                                                               
PRTR10   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R6)                    "                                    
         AR    R6,RF                       "                                    
*                                                                               
         CLI   0(R6),0             END OF RECORD ?                              
         BE    PRTRX               YES                                          
         CLI   0(R6),BNDELQ        X'4B' - BILL ELEMENT                         
         BNE   PRTR10                                                           
*                                                                               
         USING BNDELD,R6                                                        
         ICM   R1,15,BNDAMNT                                                    
         LTR   R1,R1                                                            
         BZ    PRTR10                                                           
*                                                                               
         CVD   R1,DUB              IN '4B', THEN SOMETHING IS WRONG             
         MP    DUB,=P'-1'                                                       
         CP    DUB,TRNAMNT                                                      
         BNE   PRTR10                                                           
*                                                                               
         AP    CNTTRAN,=P'1'                                                    
*       ----------------------------------------                                
         CLI   RCWRITE,C'N'                                                     
         BE    PRTR90                                                           
         CVB   R1,DUB                                                           
         STCM  R1,15,BNDAMNT                                                    
         MVI   MODE,WRITRANS                                                    
*       ----------------------------------------                                
*                                                                               
PRTR90   TM    PRTFLG,NEWJOB                                                    
         BNO   PRTR100                                                          
         NI    PRTFLG,X'FF'-NEWJOB                                              
         AP    CNTJOBS,=P'1'                                                    
*                                                                               
PRTR100  DS    0H                                                               
*                                                                               
         USING PLINED,P                                                         
         MVC   PLCLI,TRNKACT                                                    
         MVC   PLPRO,TRNKACT+3                                                  
         MVC   PLJOB,TRNKACT+6                                                  
         MVC   PLOFF,TRNKOFF                                                    
         MVC   PLCONTRA,TRNKULC                                                 
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(11,PLDATE)                             
         MVC   PLREF,TRNKREF                                                    
         EDIT  TRNSUB,PLSUB                                                     
         EDIT  TRNAMNT,PLAMOUNT,2,ZERO=NOBLANK,COMMAS=Y,MINUS=YES               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         AHI   R1,-(TRNLN1Q+1)                                                  
         BNP   PRTR110                                                          
         EX    R1,*+4                                                           
         MVC   PLNARR(0),TRNNARR                                                
*                                                                               
PRTR110  GOTO1 ACREPORT                                                         
*                                                                               
         B     PRTR10                                                           
*       ----------------------------------------                                
*                                                                               
PRTRX    B     EXIT                                                             
*                                                                               
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
******************************************************************              
*  REQUEST LAST                                                                 
******************************************************************              
*                                                                               
REQL     DS    0H                                                               
*                                                                               
         MVC   P(12),=C'JOBS      - '                                           
         EDIT  (P6,CNTJOBS),(10,P+12)                                           
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P(12),=C'TRANSACTIONS'                                           
         EDIT  (P6,CNTTRAN),(10,P+12)                                           
         GOTO1 ACREPORT                                                         
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(DUMP)                                                          
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
*                                                                               
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         LA    R5,=C'2D'                                                        
*                                                                               
         GOTO1 PRNTBL,DMCB,0,(R3),C'DUMP',(R4),(R5),(C'P',PRINT)                
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(ADDRESS OF DATA),(LENGTH OF DATA)               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*              WORKING STORAGE DSECT                                            
********************************************************************            
*                                                                               
ACXJD    DSECT                     WORKING STORAGE DSECT                        
*                                                                               
VTYPES   DS    0A                                                               
ADUMP    DS    A                                                                
PRNTBL   DS    V                   PRINT DATA                                   
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
CNTJOBS  DS    PL6                                                              
CNTTRAN  DS    PL6                                                              
*                                                                               
PRTFLG   DS    X                   TURN ON WHEN NEW JOB                         
NEWJOB   EQU   X'80'                                                            
NEWTRAN  EQU   X'40'                                                            
*                                                                               
MSG      DS    CL10                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
PLINED   DSECT                                                                  
PLCLI    DS    CL3                                                              
         DS    CL1                                                              
PLPRO    DS    CL3                                                              
         DS    CL1                                                              
PLJOB    DS    CL6                                                              
         DS    CL1                                                              
PLOFF    DS    CL2                                                              
         DS    CL1                                                              
PLCONTRA DS    CL14                                                             
         DS    CL1                                                              
PLDATE   DS    CL8                                                              
         DS    CL1                                                              
PLREF    DS    CL6                                                              
         DS    CL2                                                              
PLSUB    DS    CL3                                                              
         DS    CL2                                                              
PLAMOUNT DS    CL12                                                             
         DS    CL2                                                              
PLNARR   DS    CL40                                                             
***********************************************************************         
*                                                                               
*                                                                               
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*                                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*                                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
*        DDDLCB                                                                 
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
*                                                                               
*        ACREPPROFD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPXJ02 08/16/00'                                      
         END                                                                    
