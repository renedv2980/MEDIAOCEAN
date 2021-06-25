*          DATA SET CTCONAUTHU AT LEVEL 034 AS OF 05/01/02                      
*PHASE CONNAUTA CONNAUTH                                                        
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO RESTORE X'21' ELEMS       *         
* SET BY CTCONAUTH                                                    *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONAUTH - CHANGE SYS AUTH (X''21'') ELEMS'                    
CONAUTH  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONAUTH                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(10),=C'CHANGED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'ALL=800F: '                                             
         GOTO1 VHEXOUT,DMCB,COUNT2,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'RRGO=:    '                                             
         GOTO1 VHEXOUT,DMCB,COUNT3,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'DELETED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT4,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         DROP  R8                                                               
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
M15      CLI   0(R2),C'I'          TEST ID RECORD                               
         BNE   EXIT                NO - LEAVE ALONE                             
         OC    1(24,R2),1(R2)      NULL KEY? (HI TERM REC?)                     
         BZ    EXIT                                                             
*                                                                               
         USING CTIREC,R2                                                        
         TM    CTISTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                TERM HAS NO VALID ID                         
         CLC   =C'YN',2(R6)                                                     
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,CHA21            MAKE CHANGE IF NECESSARY                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* CHA21: SEE IF X'21' EL EXISTS FOR 'F' SYSTEM - IF SO, DELETE AND              
*        RESTORE OLD SYSTEM                                                     
*                                                                               
CHA21    NTR1                                                                   
         LA    R7,SYSPRG                                                        
*                                                                               
* SEE IF SYS AUTH EL EXISTS FOR 'F' SYSTEM                                      
CHA1     GOTO1 VHELLO,DMCB,(C'G',=C'CTFBIG'),(X'21',(R2)),(1,1(R7))             
         CLI   DMCB+12,X'06'       ELEM NOT FOUND?                              
         BE    CHA2                                                             
*                                                                               
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* NOW DELETE 'NEW' ELEM                                                         
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFBIG'),(X'21',(R2)),(1,(R7))              
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* GO FIND 'F' ELEM AGAIN                                                        
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFBIG'),(X'21',(R2)),(1,1(R7))             
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,DMCB+12                                                       
         NI    2(R1),X'0F'         'RESTORE' OLD ELEMENT                        
*                                                                               
CHA2     AHI   R7,2                                                             
         CLI   0(R7),X'FF'                                                      
         BNE   CHA1                                                             
*                                                                               
*  UPDATE BY USING ACTUAL LENGTH IN KEY...                                      
         SR    R1,R1                                                            
         ICM   R1,3,CTILEN                                                      
         AHI   R1,4                                                             
         L     R3,AIOAREA                                                       
         STH   R1,0(R3)                                                         
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         GOTO1 VHEXOUT,DMCB,0(R2),P,25,=C'TOG'                                  
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         DROP  R8                                                               
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
         L     R1,COUNT1                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNT1                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PRNERR   DS    0H                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
ENDSYS   AHI   R7,1                R7=1ST PROGRAM                               
NS2      CLI   0(R7),0             END OF SYSTEM?                               
         BER   RE                                                               
         AHI   R7,3                                                             
         B     NS2                                                              
                                                                                
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
COUNT1   DC    F'0'                CHANGED RECS                                 
COUNT2   DC    F'0'                ALL=800F                                     
COUNT3   DC    F'0'                RRGO=                                        
COUNT4   DC    F'0'                DELETED C'5'S                                
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
SYSPRG   DS    0H                                                               
         DC    X'06F6'             ACC SYSTEM                                   
         DC    X'02F2'             SPOT SYSTEM                                  
         DC    X'04F4'             PRINT SYSTEM                                 
         DC    X'03F3'             NET SYSTEM                                   
         DC    X'FF'               EOFT                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034CTCONAUTHU05/01/02'                                      
         END                                                                    
