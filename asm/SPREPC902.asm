*          DATA SET SPREPC902  AT LEVEL 001 AS OF 02/18/98                      
*PHASE SPC902A                                                                  
         TITLE 'SPC902 - COKE BUY LOAD TURNAROUND'                              
SPC902   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPC902                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPC902,R8,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    R7,P                MAPPING PRINT LINE DSECT                     
         USING PLINED,R7                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQFRST                                                           
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
         CLI   MODE,STAFRST                                                     
         BE    STAF                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    RQLAST                                                           
*                                                                               
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
*                                                                               
RQFRST   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
CLTF     DS    0H                                                               
         MVC   CLTCODE,CLT                                                      
         B     EXIT                                                             
*                                                                               
PRDF     DS    0H                                                               
         MVC   PRTPRD,PRD                                                       
         MVC   PRTPRDNM,PRDNM                                                   
         B     EXIT                                                             
*                                                                               
ESTF     DS    0H                                                               
         MVC   PRTEST,EST                                                       
         MVC   PRTESTNM,ESTNM                                                   
         B     EXIT                                                             
*                                                                               
MKTF     DS    0H                                                               
         MVC   PRTMKT,MKT                                                       
         MVC   PRTMKTNM,MKTNM                                                   
         B     EXIT                                                             
*                                                                               
STAF     DS    0H                                                               
         MVC   PRTSTA,STAPRINT                                                  
         CLC   MYCLT,=X'000000'                                                 
         BE    STAF10                                                           
         MVI   FORCEHED,C'N'                                                    
         CLC   MYCLT,CLTCODE                                                    
         BE    STAF10                                                           
         MVI   FORCEHED,C'Y'                                                    
STAF10   GOTO1 REPORT              PRINT THE LINE                               
         MVC   MYCLT,CLTCODE                                                    
         GOTO1 AENDREQ             FORCE TO NEXT REQUEST                        
         B     EXIT                                                             
*                                                                               
RQLAST   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R7                  USED FOR MAPPING PRINT LINE                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
*                                                                               
MYCLT    DC    X'000000'           USED FOR FORCING PAGE BREAK                  
CLTCODE  DS    CL3                 SAVES CLIENT CODE FROM CLTFRST               
*                                                                               
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
*                                                                               
PLINED   DSECT                                                                  
*                                                                               
PRTPRD   DS    CL3                                                              
         DS    CL2                                                              
PRTPRDNM DS    CL20                                                             
         DS    CL2                                                              
PRTEST   DS    CL3                                                              
         DS    CL2                                                              
PRTESTNM DS    CL20                                                             
         DS    CL2                                                              
PRTMKT   DS    CL4                                                              
         DS    CL4                                                              
PRTMKTNM DS    CL20                                                             
         DS    CL2                                                              
PRTSTA   DS    CL7                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPC902 02/18/98'                                      
         END                                                                    
