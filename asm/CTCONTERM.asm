*          DATA SET CTCONTERM  AT LEVEL 022 AS OF 08/10/00                      
*PHASE CONTERMA                                                                 
*INCLUDE BINSRCH2                                                               
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO RENUMBER TERMINAL RECORDS *         
* NOTE - BE SURE TO MANUALLY UPDATE HIGH TERMINAL NUMBER VIA PFM      *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONTERM - RENUMBER TERMINAL RECORDS'                          
CONTERM  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONAPL*                                                       
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
         BNE   M10                                                              
         MVC   P(14),=C'NEW HIGH TERM:'                                         
         GOTO1 VHEXOUT,DMCB,HIGHTERM,P+14,2,=C'TOG'                             
         MVC   P+19(09),=C'(MINUS 1)'                                           
         GOTO1 VPRINTER                                                         
         BE    EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
         CLI   0(R2),C'T'          TERMINAL REC?                                
         BNE   EXIT                                                             
*                                                                               
         USING CTTREC,R2                                                        
         TM    CTTSTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
*                                                                               
         OC    CTTKSPAR(22),CTTKSPAR   THIS A TNUM PASSIVE?                     
         BNZ   M14                      NO                                      
         OC    CTTKEY+23(2),CTTKEY+23  THIS HIGH TERM REC?                      
         BZ    EXIT                     YES - IGNORE IT                         
         MVC   OLDTNUM,CTTKEY+23                                                
         MVC   NEWTNUM,HIGHTERM                                                 
         XC    PASSWORD,PASSWORD                                                
         LR    R6,R2                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPASD,R6                                                        
         MVC   LUID,CTPASDTA                                                    
         CLI   CTPASLEN,X'14'                                                   
         BL    *+10                 NO PASSWORD                                 
         MVC   PASSWORD,CTPASDTA+8                                              
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,(X'01',XREFREC),XREFTAB,XREFENT,       X        
               XREFLQ,(0,L'OLDTNUM),65535                                       
         CLI   0(R1),1             REC NOT FOUND?                               
         BE    *+6                  YES - THIS IS GOOD                          
         DC    H'0'                                                             
         MVC   XREFENT,DMCB+8      UPDATE COUNT                                 
*                                                                               
         LH    R1,HIGHTERM                                                      
         STCM  R1,3,CTTKEY+23                                                   
         LA    R1,1(R1)                                                         
         STH   R1,HIGHTERM                                                      
         B     EXIT                                                             
*                                                                               
M14      TM    CTTSTAT,X'0C'       IS THIS A PRINTER?                           
         BZ    M16                  NO                                          
         LR    R6,R2                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                COOL - LEAVE IT ALONE                        
         B     PRNERR              PRINTERS SHOULDN'T HAVE X'03'S               
*                                                                               
M16      DS    0H                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPASD,R6                                                        
         GOTO1 =V(BINSRCH),DMCB,(0,CTPASDTA),XREFTAB,XREFENT,XREFLQ,   X        
               (0,L'OLDTNUM),65535                                              
         CLI   0(R1),1             REC NOT FOUND?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZICM  R3,1(R1),3                                                       
         MVC   XREFREC(XREFLQ),0(R3)                                            
         CLC   LUID,CTTKTID        MAKE SURE SAME LUID                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PASSWORD,CTTKPASS                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CTPASDTA(2),NEWTNUM                                              
         B     EXIT                                                             
*&&DO                                                                           
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
         OC    CTTKTID,CTTKTID     THIS A PASSIVE?                              
         BZ    *+14                 YES                                         
         MVC   P(8),7(R2)                                                       
         B     ADD40                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'03',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,DMCB+12          A(ELEM)                                      
         MVC   P(8),2(R3)                                                       
ADD40    GOTO1 VPRINTER                                                         
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
         L     R1,COUNT1                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNT1                                                        
         B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
*                                                                               
PRNERR   DS    0H                                                               
         MVC   P(14),=C'SOME   ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         DS    0D                                                               
HIGHTERM DC    H'1'                                                             
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
*                                                                               
ELEM     DS    CL255                                                            
COUNT1   DC    F'0'                                                             
FLAG     DS    X                                                                
FOUND5   EQU   X'80'                                                            
*                                                                               
XREFREC  DS    0D                                                               
OLDTNUM  DS    H                                                                
NEWTNUM  DS    H                                                                
LUID     DS    CL8                                                              
PASSWORD DS    CL10                                                             
XREFLQ   EQU   *-XREFREC                                                        
*                                                                               
XREFENT  DS    A                   N'RECS                                       
MAXRECS  EQU   65535                                                            
*                                                                               
         DS    0D                                                               
XREFTAB  DS    (MAXRECS)CL(XREFLQ)                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022CTCONTERM 08/10/00'                                      
         END                                                                    
