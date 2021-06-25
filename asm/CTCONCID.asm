*          DATA SET CTCONCID   AT LEVEL 082 AS OF 08/10/00                      
*PHASE CONCIDA                                                                  
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE VALID ID'S         *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONCID - CHANGE VALID IDS'                                    
CONCID   CSECT                                                                  
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
**NOP         BE    M1A                                                         
**NOP         CLI   0(R2),C'I'          ID REC?                                 
**NOP         BNE   EXIT                                                        
*                                                                               
*&&DO                                                                           
*                                                                               
* GET 2-CHAR AGY CODE FROM ID REC                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'06',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
**NOP    BE    *+6                                                              
**NOP    DC    H'0'                                                             
         BNE   EXIT                                                             
         L     R3,DMCB+12                                                       
         USING CTAGYD,R3                                                        
         LA    RF,AGYTABLE                                                      
*                                                                               
* SEE IF THIS IS ONE WE WANT                                                    
         CLC   CTAGYID,0(RF)                                                    
         BE    M24                                                              
         LA    RF,L'AGYTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-18                                                             
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
*&&                                                                             
         USING CTTREC,R2                                                        
M1A      TM    27(R2),X'04'        PRINTER OR SHUTTLE?                          
         BNZ   EXIT                LEAVE IT ALONE                               
*                                                                               
         OC    CTTKTID,CTTKTID     THIS A PASSIVE?                              
         BZ    M14                  YES                                         
         OC    CTTKPASS,CTTKPASS   PASSWORD REC?                                
         BNZ   EXIT                 YES - SKIP                                  
         LA    R3,CTTKTID                                                       
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
         LA    R3,2(R3)                                                         
*                                                                               
M16      LA    RF,LUIDLIST                                                      
         CLC   0(L'LUIDLIST,RF),0(R3)    LISTED TERMINAL?                       
         BE    M24                                                              
         LA    RF,L'LUIDLIST(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-18                                                             
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
M24      BAS   RE,CHAID                                                         
*                                                                               
* PRINT OUT CHANGES                                                             
         TM    FLAGS,IDADDED       ANY CHANGES?                                 
         BZ    EXIT                 NO                                          
         CLI   0(R2),C'T'          TERM REC?                                    
         BNE   M52                                                              
         USING CTTREC,R2                                                        
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
         USING CTIREC,R2                                                        
M52      DS    0H                  PRINT OUT CHANGED ID'S                       
         LA    R3,P                                                             
         MVC   0(02,R3),=C'ID'                                                  
         LA    R3,3(R3)                                                         
         OC    CTIKID(8),CTIKID                                                 
         BNZ   M60                                                              
         GOTO1 VHEXOUT,DMCB,23(R2),0(R3),2,=C'TOG'                              
         LA    R3,5(R3)                                                         
         B     M40                                                              
M60      MVC   0(10,R3),CTIKID                                                  
         LA    R3,11(R3)                                                        
         B     M40                                                              
         DROP  R2                                                               
*                                                                               
DONE     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHAID    NTR1                                                                   
         NI    FLAGS,X'FF'-IDADDED                                              
         SPACE                                                                  
* CHECK REC NOT TOO BIG                                                         
         CLC   =H'970',25(R2)      REC >= 970 BYTES                             
         BNH   PRNERR               YES                                         
         SPACE                                                                  
* REMOVE ALL X'20' ELEMS                                                        
         SPACE                                                                  
***********************************************************************         
***********************************************************************         
*                                                                     *         
* BEFORE DELETING THE NEXT LINE, ADD CODE TO MAKE SURE THE FUCKING    *         
* NEW ELEMENT DOESN'T FUCKING EXIST                                   *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                                                                               
         CLC   =C'JW',0(R3)                                                     
         BNE   CI10                ONLY DELETE ALL ID'S FOR JW                  
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'20',(R2))                       
         LA    R5,JWID                                                          
         B     CI20                                                             
*                                                                               
* FOR JB & TO, SKIP IF L=INFOFF IS ALREADY PRESENT                              
*                                                                               
CI10     GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'34',(R2)),LINFOFF               
         CLI   DMCB+12,X'06'       TEST FOR ELEM NOT FOUND                      
         BNE   FOUND               ELEM ALREADY THERE                           
         LA    R5,IDLIST                                                        
*                                                                               
* ADD NEW X'20' ELEMS                                                           
*                                                                               
CI20     XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'200C'                                                 
*                                                                               
CI22     TM    0(R5),X'80'         THIS AN ID LIST?                             
         BZ    CI30                 NO                                          
         XC    ELEM+2(2),ELEM+2                                                 
         MVC   ELEM+4(8),1(R5)                                                  
         B     *+10                                                             
*                                                                               
CI30     MVC   ELEM+2(10),1(R5)                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'20',(R2)),ELEM,        X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,5           REC TOO LARGE?                               
         BE    PRNERR                                                           
         CLI   DMCB+12,0           ANY OTHER ERRORS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,L'IDLIST(R5)                                                  
         CLI   0(R5),X'FF'         EOT?                                         
         BNZ   CI22                 NO                                          
*                                                                               
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         SPACE                                                                  
         CLC   =H'999',25(R2)      REC >= 999 BYTES                             
         BNH   PRNERR               YES                                         
         ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
         OI    FLAGS,IDADDED                                                    
         B     EXIT                                                             
         EJECT                                                                  
* PRINT ERROR RECS *                                                            
         SPACE                                                                  
PRNERR   DS    0H                                                               
         USING CTTREC,R2                                                        
         MVC   P(14),=C'LENGTH ERROR: '                                         
         OC    CTTKTID(8),CTTKTID                                               
         BNZ   PRNERR10                                                         
         EDIT  (B2,23(R2)),(5,P+15)                                             
         B     PRNERR20                                                         
*                                                                               
PRNERR10 MVC   P+15(10),CTTKTID                                                 
*                                                                               
PRNERR20 GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         SPACE                                                                  
FOUND    DS    0H                                                               
         USING CTTREC,R2                                                        
         MVC   P(14),=C'HAD INFOFF:   '                                         
         OC    CTTKTID(8),CTTKTID                                               
         BNZ   FOUND10                                                          
         EDIT  (B2,23(R2)),(5,P+15)                                             
         B     FOUND20                                                          
*                                                                               
FOUND10  MVC   P+15(10),CTTKTID                                                 
*                                                                               
FOUND20  GOTO1 VPRINTER                                                         
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
LINFOFF  DS    X'00',C'INFOFF'                                                  
*                                                                               
LUIDLIST DS    0CL2                                                             
         DC    C'TO'                                                            
         DC    C'JB'                                                            
         DC    C'JW'                                                            
         DC    X'FF'                                                            
*                                                                               
JWID     DS    0CL11                                                            
         DC    X'00',C'JWTSEC    '                                              
         DC    X'80',C'JWIDS3    '                                              
         DC    X'80',C'FDMIDS    '                                              
         DC    X'80',C'JWBCID    '                                              
         DC    X'80',C'JWFIID    '                                              
         DC    X'80',C'JWDDS     '                                              
         DC    X'80',C'JWIDS4    '                                              
         DC    X'FF'                                                            
*                                                                               
IDLIST   DS    0CL11                                                            
         DC    X'80',C'INIOFF    '                                              
         DC    X'FF'                                                            
*                                                                               
AGYTABLE DS    0CL2                                                             
         DC    C'AR'                                                            
         DC    C'YF'                                                            
         DC    C'BU'                                                            
         DC    C'YP'                                                            
         DC    C'CH'                                                            
         DC    C'YS'                                                            
         DC    C'WW'                                                            
         DC    C'YY'                                                            
         DC    C'YE'                                                            
         DC    C'Y1'                                                            
         DC    C'YN'                                                            
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082CTCONCID  08/10/00'                                      
         END                                                                    
