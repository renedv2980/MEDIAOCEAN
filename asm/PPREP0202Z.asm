*          DATA SET PPREP0202Z AT LEVEL 002 AS OF 07/08/02                      
*PHASE PP0202Z                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        PROGRAM WILL DELETE ALL X'61' RECORDS                                  
*                                                                               
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         ZAP   CNTCOUNT,=P'0'      CONTRACTS COUNTER                            
         ZAP   AMEDCNT,=P'0'       MEDIA "A" COUNTER                            
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
PROC     DS    0H                                                               
*                                                                               
PROC10   XC    KEY,KEY                                                          
*        MVC   KEY(2),QAGENCY                                                   
*        MVC   KEY+2(1),QMEDIA                                                  
*        MVI   KEY+3,X'10'         RECORD CODE (CONTRACT RECORDS)               
*                                                                               
PROC20   GOTO1 HIGH                                                             
         B     PROC40                                                           
*                                                                               
PROC30   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PROC40   DS    0H                                                               
*                                                                               
*                                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         CLC   KEY+2(2),=X'C161'                                                
         BNE   PROC45                                                           
         AP    AMEDCNT,=P'1'                                                    
         B     PROC30                                                           
*                                                                               
PROC45   DS    0H                                                               
         CLI   KEY+3,X'61'                                                      
         BNE   PROC30                                                           
*                                                                               
PROC50   DS    0H                                                               
*                                                                               
         MVC   P(25),KEY                                                        
         GOTO1 REPORT                                                           
         OI    KEY+25,X'80'                                                     
         AP    CNTCOUNT,=P'1'      NUMBER OF X'61' RECS                         
*        GOTO1 WRT                                                              
*                                                                               
PROCXX   B     PROC30              NEXT RECORD                                  
*                                                                               
*                                                                               
*                                                                               
CNTCOUNT DS    PL8                                                              
AMEDCNT  DS    PL8                                                              
ELCODE   DS    X                                                                
WKFLAG   DS    X                                                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(10),=C'RUN TOTALS'                                             
         GOTO1 REPORT                                                           
         MVC   P(10),=C'----------'                                             
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(25),=C'TOTAL NUMBER OF 61 RECS: '                              
         EDIT  (P8,CNTCOUNT),(10,P+29),COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
         MVC   P(27),=C'TOTAL NUMBER OF MEDIA "A": '                            
         EDIT  (P8,AMEDCNT),(10,P+29),COMMAS=YES,ALIGN=LEFT                     
         GOTO1 REPORT                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
ABUFFC   DS    A                                                                
PPBVWORK DS    0D                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0202Z07/08/02'                                      
         END                                                                    
