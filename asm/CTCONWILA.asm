*          DATA SET CTCONWILA  AT LEVEL 068 AS OF 08/10/00                      
*PHASE CONWILAA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE VALID ID'S         *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONCID - CHANGE VALID IDS'                                    
CONWILA  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONWILA                                                       
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
         BE    M1A                                                              
         B     EXIT                                                             
*                                                                               
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
         B     M16                                                              
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
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'21',(R2)),TALENT,      X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'21',(R2)),CONTROL,     X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =H'990',25(R2)      REC >= 990 BYTES                             
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* PRINT OUT CHANGES                                                             
         TM    FLAGS,IDADDED       ANY CHANGES?                                 
         BZ    EXIT                 NO                                          
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
DONE     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHAID    NTR1                                                                   
         NI    FLAGS,X'FF'-IDADDED                                              
         CLC   =H'900',25(R2)      REC >= 900 BYTES                             
         BNH   PRNERR               YES                                         
         SPACE                                                                  
* REMOVE ALL X'20' ELEMS                                                        
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'20',(R2))                       
         SPACE                                                                  
* ADD NEW X'20' ELEMS                                                           
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'200C'                                                 
         LA    R5,IDLIST                                                        
*                                                                               
CI20     TM    0(R5),X'80'         THIS AN ID LIST?                             
         BZ    CI30                 NO                                          
         XC    ELEM+2(2),ELEM+2                                                 
         MVC   ELEM+4(8),1(R5)                                                  
         B     *+10                                                             
*                                                                               
CI30     MVC   ELEM+2(10),1(R5)                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'20',(R2)),ELEM,        X        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,L'IDLIST(R5)                                                  
         CLI   0(R5),X'FF'         EOT?                                         
         BNZ   CI20                 NO                                          
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
TALENT   DC    X'2110070000800000000000000000000F'                              
CONTROL  DC    X'21130A000080000000000000000000000C000F'                        
*                                                                               
FLAGS    DS    X                                                                
IDADDED  EQU   X'80'               ID ADDED TO TERM                             
*                                                                               
LUIDLIST DS    0CL4                                                             
         DC    C'WIN1'                                                          
         DC    C'WIL1'                                                          
         DC    X'FF'                                                            
*                                                                               
IDLIST   DS    0CL11                                                            
         DC    X'00',C'WILSEC    '                                              
         DC    X'80',C'WIOFF2    '                                              
         DC    X'80',C'WTIDS2    '                                              
         DC    X'80',C'WIIDS2    '                                              
         DC    X'00',C'SJW       '                                              
         DC    X'00',C'SJR       '                                              
         DC    X'00',C'SJX       '                                              
         DC    X'00',C'SRW       '                                              
         DC    X'00',C'DDSLA     '                                              
         DC    X'00',C'HDTO      '                                              
         DC    X'80',C'WRIDS2    '                                              
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068CTCONWILA 08/10/00'                                      
         END                                                                    
