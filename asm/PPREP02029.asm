*          DATA SET PPREP02029 AT LEVEL 005 AS OF 05/04/99                      
*PHASE PP0202K,+0,NOAUTO                                                        
*INCLUDE HEXOUT                                                                 
*INCLUDE SIXPACK                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM SEARCH CONTRACT RECORDS AND PRTINTS RECORDS               
*        WITH AGENCY=WI MEDIA=N AND OPEN RATE IS NOT ZERO                       
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
         B     EXIT                                                             
*                                                                               
         ZAP   CNTCOUNT,=P'0'      CONTRACTS COUNTER                            
         ZAP   X20COUNT,=P'0'      ELEMENT X'20' COUNTER                        
         ZAP   X21COUNT,=P'0'      ELEMENT X'21' COUNTER                        
         ZAP   X22COUNT,=P'0'      ELEMENT X'22' COUNTER                        
         ZAP   X24COUNT,=P'0'      ELEMENT X'24' COUNTER                        
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
PROC     DS    0H                                                               
         LA    R5,TABLE                                                         
PROC5    CLI   0(R5),X'FF'        END                                           
         BE    PROCX                                                            
         MVC   P(8),0(R5)                                                       
         GOTO1 =V(SIXPACK),DMCB,0(R5),P+10,8                                    
         GOTO1 HEXOUT,DMCB,P+10,P+25,6,=C'N'                                    
         GOTO1 REPORT                                                           
         LA    R5,8(R5)                                                         
         B     PROC5                                                            
*                                                                               
PROCX    B     EXIT                                                             
*                                                                               
TABLE    DC    C'      30'                                                      
         DC    C'      38'                                                      
         DC    C'      40'                                                      
         DC    C'     173'                                                      
         DC    C'     243'                                                      
         DC    C'      92'                                                      
         DC    C'     123'                                                      
         DC    C'     288'                                                      
         DC    X'FF'                                                            
*                                                                               
PROC10   XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'10'         RECORD CODE (CONTRACT RECORDS)               
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
         CLC   KEY(4),KEYSAVE      AGY/MED/CLT                                  
         BNE   EXIT                END OF CONTRACTS                             
*                                                                               
PROC50   DS    0H                                                               
*                                                                               
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVI   WKFLAG,0            CLEAR FLAG                                   
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            GET ALL X'20' ELEMENTS                       
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL            GET ALL X'21' ELEMENTS                       
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL            GET ALL X'22' ELEMENTS                       
*                                                                               
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL            GET ALL X'24' ELEMENTS                       
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
X20COUNT DS    PL8                                                              
X21COUNT DS    PL8                                                              
X22COUNT DS    PL8                                                              
X24COUNT DS    PL8                                                              
ELCODE   DS    X                                                                
WKFLAG   DS    X                                                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(10),=C'RUN TOTALS'                                             
         GOTO1 REPORT                                                           
         MVC   P(10),=C'----------'                                             
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(31),=C'NUMBER OF X20 ELEM(S) PRINTED: '                        
         EDIT  (P8,X20COUNT),(10,P+31),COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
         MVC   P(31),=C'NUMBER OF X21 ELEM(S) PRINTED: '                        
         EDIT  (P8,X21COUNT),(10,P+31),COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
         MVC   P(31),=C'NUMBER OF X22 ELEM(S) PRINTED: '                        
         EDIT  (P8,X22COUNT),(10,P+31),COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
         MVC   P(31),=C'NUMBER OF X24 ELEM(S) PRINTED: '                        
         EDIT  (P8,X24COUNT),(10,P+31),COMMAS=YES,ALIGN=LEFT                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(31),=C'TOTAL NUMBER OF CONTRACT RECS: '                        
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
         LA    R2,PCONREC+33                                                    
         USING PRBELEM,R2                                                       
         SR    R0,R0                                                            
GETEL10  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BE    GETEL40                                                          
         CLI   0(R2),0                                                          
         BE    GETELX              NO MORE ELEMENTS                             
         B     GETEL10                                                          
*                                                                               
GETEL40  CLC   =PL5'0',PRBOPEN     COMPARING OPEN RATE                          
         BE    GETEL10             CRITERIA DON'T MATCH, TRY NEXT ELEM          
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,25,=C'N'                                       
         GOTO1 HEXOUT,DMCB,ELCODE,P+54,1,=C'N'                                  
         GOTO1 HEXOUT,DMCB,PRBOPEN,P+60,5,=C'N'                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVI   WKFLAG,C'Y'         MATCHED CONTRACT REC IS FOUND                
*                                                                               
         CLI   ELCODE,X'20'                                                     
         BNE   *+14                                                             
         AP    X20COUNT,=P'1'      PRINTED A MATCHED X'20' ELEM                 
         B     GETEL60                                                          
*                                                                               
         CLI   ELCODE,X'21'                                                     
         BNE   *+14                                                             
         AP    X21COUNT,=P'1'      PRINTED A MATCHED X'21' ELEM                 
         B     GETEL60                                                          
*                                                                               
         CLI   ELCODE,X'22'                                                     
         BNE   *+14                                                             
         AP    X22COUNT,=P'1'      PRINTED A MATCHED X'22' ELEM                 
         B     GETEL60                                                          
*                                                                               
         CLI   ELCODE,X'24'                                                     
         BNE   *+14                                                             
         AP    X24COUNT,=P'1'      PRINTED A MATCHED X'24' ELEM                 
         B     GETEL60                                                          
*                                                                               
         DC    H'0'                NO OTHER ELEMS ARE SEARCHED                  
*                                                                               
GETEL60  B     GETEL10             CASE OF MULTIPLE SAME ELEM CODES             
         DROP  R2                                                               
*                                                                               
GETELX   B     EXIT                                                             
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
**PAN#1  DC    CL21'005PPREP0202905/04/99'                                      
         END                                                                    
