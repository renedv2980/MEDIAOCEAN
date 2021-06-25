*          DATA SET PPREP0202X AT LEVEL 002 AS OF 07/08/02                      
*PHASE PP0202X                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*                                                                               
*                                                                               
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
         ZAP   CNTCOUNT,=P'0'                                                   
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
PROC     DS    0H                                                               
*                                                                               
PROC10   XC    KEY,KEY                                                          
**       MVC   KEY(2),QAGENCY                                                   
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
         CLI   KEY+3,X'20'                                                      
         BNE   PROC30              END OF CONTRACTS                             
*                                                                               
PROC50   DS    0H                                                               
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETBUY                                                           
*                                                                               
         MVI   WKFLAG,0            CLEAR FLAG                                   
*                                                                               
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL            GET ALL X'84' ELEMENTS                       
*                                                                               
*                                                                               
         CLI   WKFLAG,C'Y'                                                      
         BNE   *+10                                                             
         AP    CNTCOUNT,=P'1'      NUMBER OF MATCHED CONTRACT RECS              
*                                                                               
PROCXX   B     PROC30              NEXT RECORD                                  
*                                                                               
*                                                                               
*                                                                               
CNTCOUNT DS    PL8                                                              
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
         MVC   P(31),=C'TOTAL NUMBER OF BUY      RECS: '                        
         EDIT  (P8,CNTCOUNT),(10,P+31),COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
*                                                                               
GETEL    NTR1                                                                   
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PBYPSTEL,R2                                                      
         SR    R0,R0                                                            
GETEL10  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BE    GETEL40                                                          
         CLI   0(R2),0                                                          
         BE    GETELX              NO MORE ELEMENTS                             
         B     GETEL10                                                          
*                                                                               
GETEL40  DS    0H                                                               
         CLC   PBYPSTC,=10X'00'                                                 
         BE    GETELX              CRITERIA DON'T MATCH, TRY NEXT ELEM          
*                                                                               
         LA    R3,0                                                             
         LA    R7,10                                                            
         LA    R6,2(R2)                                                         
CNT00    CLI   0(R6),X'00'                                                      
         BE    CNT01                                                            
         AHI   R3,1                                                             
*                                                                               
CNT01    LA    R6,1(R6)                                                         
         BCT   R7,CNT00                                                         
*                                                                               
         CHI   R3,1                                                             
         BE    GETELX                                                           
         GOTO1 HEXOUT,DMCB,KEY,P,25,=C'N'                                       
         GOTO1 HEXOUT,DMCB,ELCODE,P+54,1,=C'N'                                  
         GOTO1 HEXOUT,DMCB,PBYPSTC,P+60,10,=C'N'                                
         GOTO1 REPORT                                                           
*                                                                               
         MVI   WKFLAG,C'Y'         MATCHED CONTRACT REC IS FOUND                
*                                                                               
         B     GETELX                                                           
*                                                                               
*                                                                               
         DC    H'0'                NO OTHER ELEMS ARE SEARCHED                  
*                                                                               
         DROP  R2                                                               
*                                                                               
GETELX   B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
       ++INCLUDE PBYPSTEL                                                       
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
**PAN#1  DC    CL21'002PPREP0202X07/08/02'                                      
         END                                                                    
