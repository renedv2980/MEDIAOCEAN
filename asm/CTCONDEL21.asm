*          DATA SET CTCONDEL21 AT LEVEL 045 AS OF 02/03/94                      
*PHASE COND21,*                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO DELETE X'21' ELEMS FOR    *         
* SYSTEM STRF (X'0D') IF DEFAULT IS ALL=N AND THERE ARE NO OVERRIDES. *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONDEL21 - DELETE SYS AUTH (X''21'') ELEMS'                   
COND21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*COND21*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BE    EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
         CLI   0(R2),C'T'          TERMINAL REC?                                
         BE    M14                                                              
         CLI   0(R2),C'I'          ID REC?                                      
         BE    M14                                                              
         CLI   0(R2),C'0'          AUTH REC?                                    
         BE    M14                                                              
         CLI   0(R2),C'5'          ACCESS REC?                                  
         BNE   EXIT                                                             
*                                                                               
         USING CTTREC,R2                                                        
M14      TM    CTTSTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
* TOO LONG NOT AN ISSUE FOR DEL...                                              
*                                                                               
*         CLC   CTTLEN,=H'950'      REC >= 990 BYTES?                           
*         BNL   PRNERR                                                          
*                                                                               
         BAS   RE,CHA21            MAKE CHANGE IF NECESSARY                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* CHA21: SEE IF X'21' EL EXISTS FOR STRAFFIC.  IF SO, AND ALL=N,                
*        DELETE ELEM.                                                           
*                                                                               
CHA21    NTR1                                                                   
*                                                                               
* SEE IF SYS AUTH EL EXISTS FOR STRAF                                           
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'21',(R2)),=X'0D'                
         CLI   DMCB+12,X'06'       FOUND?                                       
         BE    EXIT                 NO                                          
         L     R6,DMCB+12          A(ELEM)                                      
         USING CTSYSD,R6                                                        
         OC    CTSYSALL,CTSYSALL   IS ALL=N?                                    
         BNZ   EXIT                 NO - LEAVE IT ALONE                         
*                                                                               
* CHECK IF ANY PGM OVERRIDES                                                    
         CLI   CTSYSLEN,CTSYSL1Q                                                
         BNE   EXIT                 YES - LEAVE IT ALONE                        
*                                                                               
* HAD TRAF ELEM OF ALL=N, SO DELETE                                             
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'21',(R2)),=X'0D'                
         CLI   DMCB+12,0                                                        
         BE    *+6                 NO ERRORS, SET LENGTH AND EXIT               
         DC    H'0'                ELSE DIE                                     
*                                                                               
*  UPDATE BY USING ACTUAL LENGTH IN KEY...                                      
         SR    R1,R1                                                            
         ICM   R1,3,CTTLEN                                                      
         AH    R1,=H'4'                                                         
         L     R3,AIOAREA                                                       
         STH   R1,0(R3)                                                         
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
*         GOTO1 VHEXOUT,DMCB,0(R2),P,25,=C'TOG'                                 
*         GOTO1 VPRINTER                                                        
*         MVC   P(25),0(R2)                                                     
*         GOTO1 VPRINTER                                                        
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
*         L     R1,COUNT1                                                       
*         LA    R1,1(R1)                                                        
*         ST    R1,COUNT1                                                       
          B     EXIT                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045CTCONDEL2102/03/94'                                      
         END                                                                    
