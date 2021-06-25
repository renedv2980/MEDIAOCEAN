*          DATA SET CTCONPROF  AT LEVEL 079 AS OF 08/10/00                      
*PHASE CONPROFA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'49' PQ RETAIN    *         
* ELEMS FROM 'P' TO 'G'                                               *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONPROF - CHANGE PQ RETAINS OF P TO G'                        
CONPROF  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONPROF                                                       
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
         MVC   P(10),=C'CHANGED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
*                                                                               
         USING CTPREC,R2                                                        
         CLI   CTPKTYP,CTPKTYPQ    PROFILE REC?                                 
         BNE   EXIT                                                             
         TM    CTPSTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'49'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING CTPQCD,R6                                                        
         CLI   CTPQCLAS,C'P'                                                    
         BNE   *+12                                                             
         MVI   CTPQCLAS,C'G'                                                    
         OI    FLAG,FLCHNG         SET CHANGED FLAG                             
*                                                                               
         CLI   CTPQCLA2,C'P'                                                    
         BNE   *+12                                                             
         MVI   CTPQCLA2,C'G'                                                    
         OI    FLAG,FLCHNG         SET CHANGED FLAG                             
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
         TM    FLAG,FLCHNG                                                      
         BZ    EXIT                                                             
         NI    FLAG,X'FF'-FLCHNG   RESET CHANGED FLAG                           
         MVC   P(3),CTPKSYS                                                     
         GOTO1 VPRINTER                                                         
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
         L     R1,COUNT1                                                        
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
FLCHNG   EQU   X'80'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079CTCONPROF 08/10/00'                                      
         END                                                                    
