*          DATA SET CTTERMATT  AT LEVEL 009 AS OF 08/10/00                      
*PHASE TERMATTA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE TERM ATTRIBUTES    *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTTERMATT - CHANGE TERM ATTRIBUTES'                             
TERMATT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TRMATT*                                                       
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
         BE    EXIT                 YES                                         
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
         CLI   0(R2),C'T'          TERM REC?                                    
         BNE   EXIT                 NO                                          
*                                                                               
         TM    27(R2),X'80'        IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
*                                                                               
         TM    27(R2),X'04'        PRINTER OR SHUTTLE?                          
         BNZ   EXIT                LEAVE IT ALONE                               
         BAS   RE,CHAATT                                                        
         BNE   EXIT                                                             
* PRINT OUT CHANGES                                                             
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
*                                                                               
M40      MVC   0(14,R3),=C'HAD ATT CHANGED'                                     
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHAATT   NTR1                                                                   
         OC    CTTKTID,CTTKTID                                                  
         BZ    CI05                                                             
         OC    CTTKPASS,CTTKPASS   PASSWORD REC?                                
         BNZ   NO                   YES                                         
         LA    R3,CTTKTID                                                       
         B     CI10                                                             
*                                                                               
CI05     GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'03',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,DMCB+12          A(ELEM)                                      
         CLI   1(R3),X'0A'         PASSWORD REC                                 
         BH    NO                   YES                                         
         LA    R3,2(R3)                                                         
*                                                                               
CI10     CLC   =C'BD',0(R3)        THIS LINE?                                   
         BNE   NO                   ONLY FOR BD TERMS                           
         CLC   =H'990',25(R2)      REC >= 990 BYTES                             
         BNH   PRNERR               YES                                         
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'25',(R2)),0                     
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,DMCB+12          A(ELEM)                                      
         USING CTTRMD,R6                                                        
         SPACE                                                                  
         MVI   CTTRMDEV,X'02'      DEVICE=PC                                    
         OI    CTTRMAT1,X'80'      ATT1=A                                       
         SPACE                                                                  
         B     YES                                                              
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
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009CTTERMATT 08/10/00'                                      
         END                                                                    
