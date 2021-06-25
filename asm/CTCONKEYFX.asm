*          DATA SET CTCONKEYFX AT LEVEL 046 AS OF 04/18/94                      
*PHASE KEYFIX,*                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE KEYS ON FAX RECS   *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONKEYFX - FIX KEYS ON FAX RECORDS'                           
KEYFIX   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*KEYFIX*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),C'9'          FAX RECORD?                                  
         BNE   EXIT                                                             
         USING CTFXREC,R2                                                       
         MVC   CTFXCODE+5(02),=C'  '                                            
         DROP  R2                                                               
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENFAX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046CTCONKEYFX04/18/94'                                      
         END                                                                    
