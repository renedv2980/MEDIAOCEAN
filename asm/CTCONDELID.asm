*          DATA SET CTCONDELID AT LEVEL 071 AS OF 08/10/00                      
*PHASE CONDIDA                                                                  
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO DELETE VALID ID'S         *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONDID - DELETE VALID IDS'                                    
CONDID   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONDID*                                                       
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
         BE    *+12                                                             
         CLI   0(R2),C'I'          ID REC?                                      
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,DELID                                                         
*                                                                               
* PRINT OUT CHANGES                                                             
         TM    FLAGS,IDDEL         ANY CHANGES?                                 
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
M40      MVC   0(16,R3),=C'HAD ID''S DELETED'                                   
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
DELID    NTR1                                                                   
         NI    FLAGS,X'FF'-IDDEL                                                
         SPACE                                                                  
* REMOVE ALL X'20' ELEMS                                                        
         SPACE                                                                  
         LR    R6,R2                                                            
         MVI   ELCODE,X'20'                                                     
         USING CTIDD,R6                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DELID10  BAS   RE,NEXTEL                                                        
         BNE   DELID20                                                          
         LA    RF,IDLIST                                                        
DELID11  TM    0(RF),X'80'         ID LIST?                                     
         BNZ   *+18                                                             
         CLC   CTID,1(RF)                                                       
         BNE   DELID14                                                          
         B     *+14                                                             
*                                                                               
         CLC   CTID+2(8),1(RF)                                                  
         BNE   DELID14                                                          
*                                                                               
         MVI   0(R6),X'FF'                                                      
         OI    FLAGS,IDDEL                                                      
         B     DELID10                                                          
*                                                                               
DELID14  LA    RF,L'IDLIST(RF)                                                  
         CLI   0(RF),X'FF'                                                      
         BE    DELID10                                                          
         B     DELID11                                                          
*                                                                               
         DROP  R6                                                               
         SPACE                                                                  
DELID20  TM    FLAGS,IDDEL                                                      
         BZ    EXIT                                                             
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'FF',(R2)),0,0                   
         SPACE                                                                  
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         SPACE                                                                  
*        CLC   =H'999',25(R2)      REC >= 999 BYTES                             
*        BNH   PRNERR               YES                                         
         ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
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
IDDEL    EQU   X'80'               ID DELETED                                   
*                                                                               
IDLIST   DS    0CL11                                                            
         DC    X'00',C'LATMI     '                                              
         DC    X'80',C'HNOFF     '                                              
         DC    X'80',C'MMROFF    '                                              
         DC    X'80',C'CMBOFF    '                                              
         DC    X'80',C'TOROFF    '                                              
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071CTCONDELID08/10/00'                                      
         END                                                                    
