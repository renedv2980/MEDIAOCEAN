*          DATA SET DEISPKZIP  AT LEVEL 002 AS OF 08/17/00                      
*PHASE DEISZIPA                                                                 
         TITLE 'DEIS TEST PKZIP'                                                
DEISZIP  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**ZIP***,=A(R13CHAIN),R9                                       
*                                                                               
         MVC   PARMLEN,=Y(L'ZIPPARMS)                                           
         LH    RF,=Y(L'ZIPPARMS)                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PARMCARD(0),ZIPPARMS                                             
*                                                                               
         MVC   PARMADDR,=A(PARMLIST)                                            
*                                                                               
         LA    R1,PARMADDR                                                      
         XC    TASKECB,TASKECB                                                  
*                                                                               
         ATTACH EPLOC=PKZIP,ECB=TASKECB,SZERO=NO                                
         ST    R1,TASKTCB                                                       
         OC    TASKTCB,TASKTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH (NO TCB)                 
*                                                                               
         WAIT  ECB=TASKECB         WAIT FOR PKZIP TO COMPLETE                   
*                                                                               
         TM    TASKECB,X'40'       DID SUBTASK COMPLETE?                        
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         DETACH TASKTCB                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PKZIP    DC    CL8'PKZIP'                                                       
TASKECB  DS    F                   ECB FOR ATTACHED SUBTASK                     
TASKTCB  DS    F                   TCB FOR ATTACHED SUBTASK                     
PARMADDR DS    F                   A(PARMLIST)                                  
PARMLIST DS    0F                                                               
PARMLEN  DS    H                                                                
PARMCARD DC    CL100' '                                                         
ZIPPARMS DC    C'-NOSYSIN -ARCHVOL(TSOCCC) -BINARY -ARCHIVE(DEIS.ZIP) D+        
               EIS.SCP'                                                         
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DEISPKZIP 08/17/00'                                      
         END                                                                    
