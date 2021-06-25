*          DATA SET CTCONSC1   AT LEVEL 077 AS OF 05/02/00                      
*PHASE CONSC1,                                                                  
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO PICK OUT "CNN" FORMULAS   *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONSC1 - DUMPING CNN RECS TO DATASET'                         
CONSC1   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONSC1*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         MVC   P(9),=C'COPIED:  '                                               
         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
*                                                                               
         MVI   WRITE,X'FF'         FIRST SET EVERYTHING TO DELETE!!!            
*                                                                               
         CLI   0(R2),C'G'          FORMULA RECS ARE G'RECS                      
         BNE   EXIT                                                             
*                                                                               
         USING CTGREC,R2                                                        
         CLC   CTGKFILE(3),=C'CNN' CNN SPECIFIC                                 
         BNZ   EXIT                                                             
*                                                                               
         MVI   WRITE,X'00'         WE KEEP THE G-CNN REC'S                      
*                                                                               
         L     R1,COUNT1           BUMP THE COUNT                               
         LA    R1,1(R1)                                                         
         ST    R1,COUNT1                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
*                                                                               
ELEM     DS    CL255                                                            
COUNT1   DS    F'0'                                                             
FLAG     DS    X                                                                
FOUND5   EQU   X'80'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077CTCONSC1  05/02/00'                                      
         END                                                                    
