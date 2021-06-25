*          DATA SET PPREP02MC  AT LEVEL 005 AS OF 03/16/87                      
*PHASE PP0202M,+0,NOAUTO                                                        
         TITLE 'PP0202 - GENERATE COKE ACCOUNT RECORDS FROM PRINT FILE'         
PP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0202                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
         SPACE 1                                                                
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
RUNF     DS    0H                                                               
         MVI   RCRQONLY,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QMEDIA                                                    
         GOTO1 HIGHPUB                                                          
         B     PROC4                                                            
*                                                                               
PROC2    DS    0H                                                               
         GOTO1 SEQPUB                                                           
*                                                                               
PROC4    DS    0H                                                               
         CLC   KEY(1),KEYSAVE                                                   
         BNE   EXIT                                                             
         CLI   KEY+9,X'81'                                                      
         BNE   PROC2                                                            
         CLC   KEY+7(2),QAGENCY                                                 
         BNE   PROC2                                                            
*                                                                               
         GOTO1 GETNAME                                                          
*                                                                               
*                                  ADD YOUR CODE HERE                           
         B     PROC2               GET NEXT                                     
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
PP02WRKD DSECT                                                                  
PP02WRKL EQU   *-PP02WRKD                                                       
         EJECT                                                                  
* PPGENFILE                                                                     
* PPREPWORK                                                                     
* PPMODEQU                                                                      
* ACGENBOTH                                                                     
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREP02MC 03/16/87'                                      
         END                                                                    
