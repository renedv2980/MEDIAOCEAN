*          DATA SET CTCONCID2  AT LEVEL 070 AS OF 08/10/00                      
*PHASE CONCID2A                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE VALID ID'S         *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONCID - CHANGE VALID IDS'                                    
CONCID2  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONCID*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVI   DATADISP+1,28                                                    
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
         USING CTTREC,R2                                                        
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BE    DONE                 YES                                         
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
         TM    27(R2),X'80'        IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
*                                                                               
* SKIP HIGH ID/TERM REC                                                         
         OC    1(24,R2),1(R2)                                                   
         BZ    EXIT                                                             
*                                                                               
         CLI   0(R2),C'T'          TERM REC?                                    
         BNE   EXIT                                                             
*                                                                               
         USING CTTREC,R2                                                        
M1A      TM    27(R2),X'04'        PRINTER OR SHUTTLE?                          
         BNZ   EXIT                LEAVE IT ALONE                               
*                                                                               
         OC    CTTKTID,CTTKTID     THIS A PASSIVE?                              
         BZ    M14                  YES                                         
         OC    CTTKPASS,CTTKPASS   PASSWORD REC?                                
         BNZ   EXIT                 YES - SKIP                                  
*        LA    R3,CTTKTID                                                       
         B     M16                                                              
*                                                                               
M14      DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'03',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,DMCB+12                                                       
         CLI   1(R3),X'0A'         PASSWORD REC?                                
         BH    EXIT                 YES - SKIP                                  
*        LA    R3,2(R3)                                                         
*                                                                               
* SEE IF ONE WE WANT                                                            
M16      LR    R6,R2                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
M18      BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         CLC   =C'KRGSEC    ',2(R6)                                             
         BNE   *+12                                                             
         BAS   RE,CHAID                                                         
         B     M20                                                              
*                                                                               
         OC    2(2,R6),2(R6)       IS IT AN ID LIST?                            
         BNZ   M18                  NO                                          
         CLC   =C'FTSSTA  ',4(R6)                                               
         BNE   M18                                                              
         BAS   RE,CHAID2                                                        
*                                                                               
* PRINT OUT CHANGES                                                             
M20      TM    FLAGS,IDADDED       ANY CHANGES?                                 
         BZ    EXIT                 NO                                          
         LA    R3,P                                                             
         MVC   0(08,R3),=C'TERMINAL'                                            
         LA    R3,9(R3)                                                         
         OC    CTTKTID,CTTKTID                                                  
         BNZ   M30                                                              
         GOTO1 VHEXOUT,DMCB,23(R2),0(R3),2,=C'TOG'                              
         LA    R3,5(R3)                                                         
         B     M40                                                              
*                                                                               
M30      MVC   0(08,R3),CTTKTID                                                 
         LA    R3,9(R3)                                                         
         OC    CTTKPASS,CTTKPASS   ANY PASSWORD?                                
         BZ    *+14                                                             
         MVC   0(10,R3),CTTKPASS                                                
         LA    R3,11(R3)                                                        
         DROP  R2                                                               
*                                                                               
M40      MVC   0(14,R3),=C'HAD ID''S ADDED'                                     
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
DONE     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHAID    NTR1                      ADD NEW ID ABC*                              
         CLC   =H'980',25(R2)      REC >= 980 BYTES                             
         BNH   PRNERR               YES                                         
         NI    FLAGS,X'FF'-IDADDED                                              
         SPACE                                                                  
* MAKE SURE NOT ALREADY THERE...                                                
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'20',(R2)),=C'ABC*'              
         CLI   DMCB+12,6           ELEM NOT FOUND?                              
         BNE   EXIT                 NO - IT'S ALREADY HERE                      
         SPACE                                                                  
* ADD NEW X'20' ELEMS                                                           
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'200C'                                                 
         MVC   ELEM+2(10),=C'ABC*      '                                        
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'20',(R2)),ELEM,        X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0           ANY OTHER ERRORS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         SPACE                                                                  
         ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
         OI    FLAGS,IDADDED                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHAID2   NTR1                      ADD NEW ID LISTS FTSST2 & FTSNA2             
         CLC   =H'970',25(R2)      REC >= 970 BYTES                             
         BNH   PRNERR               YES                                         
         NI    FLAGS,X'FF'-IDADDED                                              
         SPACE                                                                  
* MAKE SURE NOT ALREADY THERE...                                                
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'20',(R2)),             X        
               =X'0000C6E3E2E2E3F24040'                                         
         CLI   DMCB+12,6           ELEM NOT FOUND?                              
         BNE   CID220               NO - IT'S ALREADY HERE                      
         SPACE                                                                  
* ADD NEW X'20' ELEMS                                                           
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'200C'                                                 
         MVC   ELEM+4(8),=C'FTSST2  '                                           
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'20',(R2)),ELEM,        X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0           ANY OTHER ERRORS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
* MAKE SURE NOT ALREADY THERE...                                                
         SPACE                                                                  
CID220   GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'20',(R2)),             X        
               =X'0000C6E3E2D5C1F24040'                                         
         CLI   DMCB+12,6           ELEM NOT FOUND?                              
         BNE   CID230               NO - IT'S ALREADY HERE                      
         SPACE                                                                  
* ADD NEW X'20' ELEMS                                                           
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'200C'                                                 
         MVC   ELEM+4(8),=C'FTSNA2  '                                           
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'20',(R2)),ELEM,        X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0           ANY OTHER ERRORS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         SPACE                                                                  
CID230   ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
         OI    FLAGS,IDADDED                                                    
         B     EXIT                                                             
         EJECT                                                                  
* PRINT ERROR RECS *                                                            
         SPACE                                                                  
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VARIOUS TEST CODE                                                             
*                                                                               
*         MVC   P(25),0(R2)                                                     
*         GOTO1 VPRINTER                                                        
*         GOTO1 VHEXOUT,DMCB,0(R6),P,60,=C'TOG'                                 
*         GOTO1 VPRINTER                                                        
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
         ANSR  X=N                                                              
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
FLAGS    DS    X                                                                
IDADDED  EQU   X'80'               ID ADDED TO TERM                             
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070CTCONCID2 08/10/00'                                      
         END                                                                    
