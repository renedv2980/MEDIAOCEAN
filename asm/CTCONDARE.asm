*          DATA SET CTCONDARE  AT LEVEL 020 AS OF 01/25/99                      
*PHASE CONDARE                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'33' ELS TO       *         
* EXPAND LENGTH BY TWO BYTES                                          *         
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
         CLI   0(R2),X'FF'         TRAILER RECORD?                              
         BE    NOMORE              YES -- PRINT TOTALS                          
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
         CLI   1(R6),11            NEW ELEMENT LENGTH?                          
         BE    EXIT                YES -- LEAVE IT ALONE                        
*                                                                               
*****    CLC   25(2,R2),=H'998'    REC > 998 BYTES?                             
*****    BH    TOOBIG              YES -- WOULD GET TOO BIG                     
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       CURRENT X'33' ELEMENT                        
         MVI   ELEM+1,11           NEW ELEMENT LENGTH IS 11                     
*                                                                               
         L     R5,AIOAREA                                                       
         LH    R5,0(R5)                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',AIOAREA,C'DUMP',(R5),=C'2D', +        
               (C'P',VPRINT)                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'33',(R2)),0                     
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'33',(R2)),ELEM                  
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)         RECORD LENGTH                                
         LA    R1,4(R1)            PLUS 4 FOR LENGTH OF RDW                     
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)          UPDATED SORT RECORD LENGTH IN RDW            
*                                                                               
         L     R5,AIOAREA                                                       
         LH    R5,0(R5)                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',AIOAREA,C'DUMP',(R5),=C'2D',  +        
               (C'P',VPRINT)                                                    
*                                                                               
         L     R0,TOTRECS                                                       
         AHI   R0,1                                                             
         ST    R0,TOTRECS                                                       
*                                                                               
         B     EXIT                                                             
*                                                                               
TOOBIG   MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,28,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         L     R0,TOTERR                                                        
         AHI   R0,1                                                             
         ST    R0,TOTERR                                                        
         B     EXIT                                                             
*                                                                               
NOMORE   BC    0,EXIT              ** SELF-MODIFYING CODE **                    
         MVI   *-3,X'F0'           ONLY PRINT TOTALS ONCE                       
         GOTO1 VPRINTER                                                         
         MVC   P(38),=C'TOTAL RECORDS CHANGED BY DEIS EXTERN: '                 
         EDIT  TOTRECS,(7,P+38),ALIGN=LEFT,ZERO=NOBLANK                         
         GOTO1 VPRINTER                                                         
         MVC   P(39),=C'TOTAL RECORDS IN ERROR IN DEIS EXTERN: '                
         EDIT  TOTERR,(7,P+39),ALIGN=LEFT,ZERO=NOBLANK                          
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         GETEL R6,28,ELCODE                                                     
*                                                                               
TOTRECS  DC    F'0'                                                             
TOTERR   DC    F'0'                                                             
ELCODE   DS    X                                                                
ELEM     DS    XL255                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTCONDARE 01/25/99'                                      
         END                                                                    
