*          DATA SET CTCONCPPRG AT LEVEL 005 AS OF 08/16/00                      
*PHASE COPYPROA COPYPROG                                                        
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CREATE OUTPUT TAPE OF     *         
* PROGRAM RECS FOR MERGE INTO CTFIL LOAD                              *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONCPPRG - OUTPUT PROGRAM RECORDS'                            
COPYPROG CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CPYPRG*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
         USING CTLKEYD,R2                                                       
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
M10      DS    0H                                                               
         MVI   WRITE,X'FF'         DEFAULT TO DELETE ALL OUTPUT                 
         CLI   CTLKID,CTLKIDQ      PROGRAM REC?                                 
         BNE   EXIT                                                             
         CLC   CTLAGID,=C'JW'      FOR JWNY                                     
         BE    M12                                                              
         CLC   CTLAGID,=C'OM'      FOR OMNY                                     
         BNE   EXIT                                                             
M12      CLI   CTLSYS,3            NETWORK?                                     
         BNE   EXIT                                                             
         MVC   CTLAGID,=C'H7'                                                   
*                                                                               
         SR    R0,R0                                                            
         LA    R6,CTLDATA                                                       
*                                                                               
M14      CLI   0(R6),X'01'                                                      
         BE    M16                                                              
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     M14                                                              
*                                                                               
         USING CTLDESCD,R6                                                      
M16      DS    0H                                                               
         CLI   CTLDDATA,C'*'                                                    
         BNE   EXIT                                                             
         MVI   WRITE,X'00'         KEEP THIS REC                                
*                                                                               
         MVC   P(8),CTLNAME        PRINT OUT SOME INFO ABOUT IT                 
         ZIC   RE,CTLDLEN                                                       
         AHI   RE,-3                                                            
         EX    RE,*+4                                                           
         MVC   P+10(0),CTLDDATA                                                 
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
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
ELEM     DS    CL255                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*          DATA SET GEGENPRG   AT LEVEL 014 AS OF 11/16/99                      
CTLKEYD  DSECT                                                                  
CTLKID   DS    XL1                                                              
CTLKIDQ  EQU   X'01'                                                            
         DS    CL11                                                             
CTLAGID  DS    CL2                                                              
CTLSYS   DS    XL1                                                              
CTLPRG   DS    XL1                                                              
CTLPHAS  DS    XL1                                                              
CTLNAME  DS    CL8                                                              
CTLLEN   DS    XL2                                                              
CTLSTAT  DS    XL1                                                              
CTLDATA  DS    0X                                                               
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
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTCONCPPRG08/16/00'                                      
         END                                                                    
