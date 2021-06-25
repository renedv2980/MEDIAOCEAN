*          DATA SET CTLDEXTXX  AT LEVEL 001 AS OF 05/04/03                      
*          DATA SET CTLDEXTANN AT LEVEL 031 AS OF 05/01/02                      
*PHASE CONDEL                                                                   
***********************************************************************         
***********************************************************************         
         TITLE 'REMOVE 221F FROM CTFILE'                                        
CONDEL   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONDEL                                                        
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         USING CTIREC,R2                                                        
M15      CLI   0(R2),C'I'          TEST ID RECORD                               
         BNE   EXIT                NO - LEAVE ALONE                             
         CLC   =X'221F',CTIKNUM                                                 
         BNE   EXIT                NO - LEAVE ALONE                             
*                                                                               
         OI    CTISTAT,X'80'       IS THE FUCKING THING DELETED?                
         B     EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
COUNT1   DC    F'0'                CHANGED RECS                                 
COUNT2   DC    F'0'                ALL=800F                                     
COUNT3   DC    F'0'                RRGO=                                        
COUNT4   DC    F'0'                DELETED C'5'S                                
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTLDEXTXX 05/04/03'                                      
         END                                                                    
