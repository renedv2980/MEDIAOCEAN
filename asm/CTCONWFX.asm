*          DATA SET CTCONWFX   AT LEVEL 001 AS OF 08/14/02                      
*PHASE CONWFXA                                                                  
*INCLUDE HEXOUT                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO EXTRACT SPOT WRITER FMTS  *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONWFX - WRITER FORMAT EXTRACT'                               
CONWFXA  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONWFX*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
         MVI   WRITE,X'FF'                                                      
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         USING CTLKEYD,R2                                                       
         CLI   0(R2),CTLKIDQ       PROGRAM RECORD?                              
         BNE   EXIT                NO                                           
*                                                                               
         CLC   =C'TM',CTLAGID      AGENCY TM?                                   
         BNE   EXIT                NO                                           
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,CTLDCODQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING CTLDESCD,R6                                                      
         CLC   =C'QA-',CTLDDATA                                                 
         BNE   EXIT                                                             
         DROP  R6                                                               
         MVI   WRITE,0                                                          
***                                                                             
* PRINT OUT RECORD KEY...                                                       
***                                                                             
         LA    R3,KEYLEN                                                        
         GOTO1 =V(HEXOUT),DMCB,0(R2),P,(R3)                                     
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
ELEM     DS    CL255                                                            
KEYLEN   EQU   CTLDATA2-CTLKID                                                  
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
*        PRINT ON                                                               
CTLKEYD  DSECT                                                                  
CTLKID   DS    XL1                                                              
CTLKIDQ  EQU   X'01'                                                            
         DS    CL11                                                             
CTLAGID  DS    CL2                                                              
CTLSYS   DS    XL1                                                              
CTLPRG   DS    XL1                                                              
CTLPHAS  DS    XL1                                                              
CTLNAME  DS    CL8                                                              
CTLEN    DS    XL2    <--- CHANGED NAME...WAS DEFINED IN LIBRARY RECS           
CTLSTATS DS    XL1    <--- CHANGED NAME...WAS DEFINED IN LIBRARY RECS           
CTLDATA2 DS    0X     <--- CHANGED NAME...WAS DEFINED IN LIBRARY RECS           
*                                                                               
CTLDESCD DSECT                                                                  
CTLDCOD  DS    XL1                                                              
CTLDCODQ EQU   X'01'                                                            
CTLDLEN  DS    XL1                                                              
CTLDDATA DS    0X                                                               
*                                                                               
CTLFIELD DSECT                                                                  
CTLFCOD  DS    XL1                                                              
CTLFCODQ EQU   X'02'                                                            
CTLFLEN  DS    XL1                                                              
CTLFID   DS    XL1                                                              
CTLFSEQ  DS    XL1                                                              
CTLFDATA DS    0X                                                               
*                                                                               
CTLFILTD DSECT                                                                  
CTLLCOD  DS    XL1                                                              
CTLLCODQ EQU   X'10'                                                            
CTLLLEN  DS    XL1                                                              
CTLLFILT DS    CL4                                                              
CTLLLENQ EQU   *-CTLFILTD                                                       
*                                                                               
* THE FOLLOWING ONLY PRESENT IF OUTPUT FIELD STARTS WITH '@' OR '/'             
CTLOUTD  DSECT         OUTPUT & DEST FIELDS                                     
CTLOCOD  DS    XL1                                                              
CTLOCODQ EQU   X'20'                                                            
CTLOLEN  DS    XL1                                                              
CTLOOUT  DS    CL8                                                              
CTLODEST DS    CL8                                                              
CTLOLENQ EQU   *-CTLOUTD                                                        
*                                                                               
CTLPIDD  DSECT         SECURITY ELEMENT                                         
CTLPCOD  DS    XL1                                                              
CTLPCODQ EQU   X'FE'                                                            
CTLPLEN  DS    XL1                                                              
CTLPID   DS    XL2                 PERSONAL ID OF LAST TO CHANGE                
CTLPDATE DS    XL3                 DATE OF LAST CHANGE                          
CTLPLENQ EQU   *-CTLPIDD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTCONWFX  08/14/02'                                      
         END                                                                    
