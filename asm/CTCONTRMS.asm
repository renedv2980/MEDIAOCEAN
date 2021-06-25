*          DATA SET CTCONTRMS  AT LEVEL 006 AS OF 11/07/01                      
*PHASE CONTTRMA CONTTRMS                                                        
***********************************************************************         
* MODULE CALLED TO STRIP OFF TERMINAL PASSIVES AND DELETE 03 ELEMENTS *         
* FROM ACTIVES                                                        *         
***********************************************************************         
         TITLE 'CTCONTRMS - STRIP TERMINAL RECORDS'                             
CONTRMS  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONTRMS                                                       
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
MAIN     L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
         USING CTTREC,R2                                                        
         CLI   CTTKTYP,CTTKTYPQ    TERMINAL REC?                                
         BNE   EXIT                                                             
         TM    CTTSTAT,X'80'       IGNORE DELETED                               
         BNZ   EXIT                                                             
*                                                                               
         MVI   WRITE,X'FF'         SET DELETE THIS ONE                          
         OC    CTTKTID(16),CTTKTID THIS A TCID PASSIVE?                         
         BZ    EXIT                YES                                          
*                                                                               
         MVI   WRITE,X'00'         SET KEEP THIS ONE                            
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'03',(R2)),0                     
         CLI   DMCB+12,0           DELETE 03 ELEMENT                            
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*CTCONWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTCONTRMS 11/07/01'                                      
         END                                                                    
