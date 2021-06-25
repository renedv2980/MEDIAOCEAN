*          DATA SET CTCONDARE2 AT LEVEL 006 AS OF 01/20/99                      
*PHASE CONDARE                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'33' ELS TO       *         
* EXPAND DARE PARTNER CODE TO TWO BYTES, AND REMOVE USELESS FLAG BYTE *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONDARE - CHANGE DARE (X''33'') ELEMS IN ID RECORDS'          
CONDARE  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONDARE                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),C'I'          ID RECORD?                                   
         BNE   EXIT                NO -- IGNORE IT                              
*                                                                               
         TM    27(R2),X'80'        IS THE FUCKING THING DELETED?                
         BO    EXIT                YES -- IGNORE IT                             
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'33'                                                     
         BAS   RE,GETEL            DARE ELEMENT PRESENT?                        
         BNE   EXIT                NO -- IGNORE IT                              
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',(R2),C'DUMP',1000,=C'2D',    +        
               (C'P',VPRINT)                                                    
*                                                                               
         MVC   ROUTCODE,3(R6)      DARE ROUTING CODE                            
         MVC   4(L'ROUTCODE,R6),ROUTCODE  SHIFT RIGHT BY ONE BYTE               
         MVC   3(1,R6),2(R6)       SHIFT PARTNER CODE RIGHT BY ONE BYTE         
         MVI   2(R6),0             HOB OF PARTNER CODE BECOMES ZERO             
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',(R2),C'DUMP',1000,=C'2D',     +        
               (C'P',VPRINT)                                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         GETEL R6,28,ELCODE                                                     
*                                                                               
ELCODE   DS    X                                                                
ROUTCODE DS    CL5                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTCONDARE201/20/99'                                      
         END                                                                    
