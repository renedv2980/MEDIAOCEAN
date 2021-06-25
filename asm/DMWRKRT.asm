*          DATA SET DMWRKRT    AT LEVEL 003 AS OF 03/13/91                      
*PHASE WKTEST                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'WKTEST - PROGRAM TO TEST NEW WKFILE'                            
         PRINT NOGEN                                                            
WKTEST   CSECT                                                                  
         NBASE 0,WKTEST,WORK=A(WKWORK)                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(16),=C'WORKER FILE TEST'                                   
*                                                                               
         GOTO1 =V(CARDS),PARM,C,=C'RE00'                                        
         MVC   WKFILE,C                                                         
         L     R1,=A(UTL)                                                       
         CLC   WKFILE,=CL8'WKFILE'                                              
         BNE   *+12                                                             
         MVI   4(R1),01                                                         
         B     WKTEST1                                                          
         CLC   WKFILE,=CL8'ACCWRK'                                              
         BNE   *+12                                                             
         MVI   4(R1),06                                                         
         B     WKTEST1                                                          
*&&UK                                                                           
         CLC   WKFILE,=CL8'MEDWRK'                                              
         BNE   *+12                                                             
         MVI   4(R1),04                                                         
         B     WKTEST1                                                          
*&&                                                                             
*&&US                                                                           
         CLC   WKFILE,=CL8'REPWRK'                                              
         BNE   *+12                                                             
         MVI   4(R1),08                                                         
         B     WKTEST1                                                          
*&&                                                                             
         DC    H'0'                                                             
WKTEST1  MVC   TITLE+17(8),WKFILE                                               
         CLI   4(R1),1                                                          
         BE    WKTEST2                                                          
         L     RE,=V(UPDID)                                                     
         MVC   0(2,RE),=C'XX'                                                   
*                                                                               
WKTEST2  L     R3,=A(WKREC)        R3=A(WKFILE RECORD)                          
         L     R4,=A(WKBUFF)       R4=A(WKFILE BUFFER)                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',WKFILE,WKINDEX,(R3),(R4)               
*                                                                               
         L     R7,8(R4)            R7=A(WKFILE BUFFER SAVE AREA)                
         EJECT                                                                  
WKNEXT   GOTO1 =V(CARDS),PARM,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    WKEOJ                                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
WKWHAT   CLI   C,C'A'              ADD A WORK FILE                              
         BE    ADD                                                              
         CLI   C,C'B'              ADD A LIBRARY WORK FILE                      
         BE    ADD                                                              
         CLI   C,C'I'              READ THE INDEX                               
         BE    NDX                                                              
         CLI   C,C'L'              LIBRARY FUNCTION                             
         BE    LIB                                                              
         CLI   C,C'Q'              SEQUENTIAL READ                              
         BE    SEQ                                                              
         CLI   C,C'R'              READ DATA                                    
         BE    RDF                                                              
         CLI   C,C'S'              CHANGE STATUS                                
         BE    STA                                                              
         B     WKNEXT                                                           
*                                                                               
WKEOJ    XBASE                                                                  
         EJECT                                                                  
ADD      XC    WKINDEX,WKINDEX     BUILD KEY FOR DDS1 USER ID                   
         CLI   C,C'B'                                                           
         BNE   *+8                                                              
         OI    WKINDEX+13,X'02'    SET LIBRARY BOOK FLAG                        
         MVC   WKINDEX(2),USERID                                                
         MVC   WKINDEX+2(6),C+1                                                 
         L     RE,=A(WKREC)        CLEAR RECORD                                 
         LA    RF,1024                                                          
         XCEF                                                                   
*                                                                               
         OC    C+7(4),=C'0000'     GET NUMBER OF RECORDS                        
         PACK  DUB,C+7(4)                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                                                             
         SR    R2,R2                                                            
*                                                                               
         OC    C+11(4),=C'0000'    GET RECORD LENGTH                            
         PACK  DUB,C+11(4)                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'4'                                                         
         BNL   *+8                                                              
         LH    RE,=H'4'                                                         
         L     R6,=A(WKREC)        SET IN START OF RECORD                       
         STH   RE,0(R6)                                                         
         LA    R5,28(R6)           R5=A(AREA TO PASS PARM DATA)                 
*                                                                               
ADDA     OC    C+15(4),=C'0000'    GET RETENTION                                
         PACK  DUB,C+15(4)                                                      
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         BZ    ADDB                                                             
         STH   RE,DUB                                                           
         MVC   37(2,R5),DUB                                                     
         OI    WKINDEX+13,X'10'                                                 
*                                                                               
ADDB     CLI   C+19,C' '           GET COMMENT                                  
         BE    ADD0                                                             
         MVC   48(16,R5),C+19                                                   
         OI    WKINDEX+13,X'08'                                                 
*                                                                               
ADD0     GOTO1 =V(DATAMGR),DMCB,=C'OPEN'                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    4(256,R6),4(R6)                                                  
*                                                                               
ADD2     LA    R2,1(R2)            BUMP REC NUM AND ADD TO FILE                 
         L     RF,=A(WKREC)                                                     
         CLC   0(2,RF),=H'8'                                                    
         BNH   *+18                                                             
         CVD   R2,DUB                                                           
         UNPK  4(4,RF),DUB                                                      
         OI    7(RF),X'F0'                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'ADD'                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R0,ADD2                                                          
*                                                                               
ADD4     GOTO1 =V(DATAMGR),DMCB,=C'CLOSE'                                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADD6     GOTO1 =V(HEXOUT),PARM,WKINDEX,P,16,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,(R4),P,60,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,8(R7),P,60,=C'TOG'                               
         GOTO1 =V(PRINTER)                                                      
         B     WKNEXT                                                           
         EJECT                                                                  
NDX      XC    WKINDEX,WKINDEX     BUILD KEY IF FILE DATA INPUT                 
         CLC   C+1(6),=CL6' '                                                   
         BE    NDX2                                                             
         MVC   WKINDEX(2),USERID                                                
         MVC   WKINDEX+2(6),C+1                                                 
*                                                                               
NDX2     GOTO1 =V(DATAMGR),DMCB,(X'80',=C'INDEX')                               
         CLI   8(R1),0                                                          
         BE    NDX4                                                             
         TM    8(R1),X'80'                                                      
         BO    NDX6                                                             
         DC    H'0'                                                             
*                                                                               
NDX4     GOTO1 =V(HEXOUT),PARM,WKINDEX,P,16,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         CLC   C+1(6),=CL6' '                                                   
         BNE   NDX6                                                             
         B     NDX2                                                             
*                                                                               
NDX6     B     WKNEXT                                                           
         EJECT                                                                  
SEQ      XC    WKINDEX,WKINDEX     SET KEY                                      
         CLI   C+7,C'D'                                                         
         BNE   *+8                                                              
         OI    WKINDEX+13,X'80'    SET DATA READ                                
         CLC   C+1(6),=CL6' '                                                   
         BE    SEQ2                                                             
         MVC   WKINDEX(2),USERID                                                
         MVC   WKINDEX+2(6),C+1                                                 
*                                                                               
SEQ2     GOTO1 =V(DATAMGR),DMCB,=C'SEQ'                                         
         CLI   8(R1),0                                                          
         BE    SEQ4                                                             
         TM    8(R1),X'80'                                                      
         BO    SEQ10                                                            
         DC    H'0'                                                             
*                                                                               
SEQ4     L     R2,=A(WKREC)                                                     
         GOTO1 =V(HEXOUT),PARM,(R2),P,4,=C'TOG'                                 
         CLC   4(4,R2),=C'*SOF'                                                 
         BE    *+14                                                             
         CLC   4(4,R2),=C'*EOF'                                                 
         BNE   SEQ6                                                             
         MVC   P+8(8),4(R2)                                                     
         CLC   4(4,R2),=C'*EOF'                                                 
         BE    SEQ8                                                             
         GOTO1 =V(HEXOUT),PARM,12(R2),P+16,36                                   
         TM    WKINDEX+13,X'80'                                                 
         BZ    SEQ8                                                             
         GOTO1 =V(HEXOUT),PARM,28(R2),P+16,58                                   
         B     SEQ8                                                             
*                                                                               
SEQ6     LH    R1,0(R2)                                                         
         SH    R1,=H'5'                                                         
         BM    SEQ8                                                             
         CH    R1,=H'123'                                                       
         BNH   *+8                                                              
         LH    R1,=H'123'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),4(R2)                                                     
*                                                                               
SEQ8     GOTO1 =V(PRINTER)                                                      
         B     SEQ2                                                             
*                                                                               
SEQ10    B     WKNEXT                                                           
         EJECT                                                                  
RDF      GOTO1 =V(DATAMGR),DMCB,=C'READ'                                        
         CLI   8(R1),0                                                          
         BE    RDF2                                                             
         TM    8(R1),X'80'                                                      
         BO    RDF6                                                             
         DC    H'0'                                                             
*                                                                               
RDF2     L     R2,=A(WKREC)                                                     
         GOTO1 =V(HEXOUT),PARM,(R2),P,4,=C'TOG'                                 
         LH    R1,0(R2)                                                         
         SH    R1,=H'5'                                                         
         BM    RDF4                                                             
         CH    R1,=H'123'                                                       
         BNH   *+8                                                              
         LH    R1,=H'123'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),4(R2)                                                     
*                                                                               
RDF4     GOTO1 =V(PRINTER)                                                      
         B     RDF                                                              
*                                                                               
RDF6     GOTO1 =V(HEXOUT),PARM,(R4),P,60,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,8(R7),P,60,=C'TOG'                               
         GOTO1 =V(PRINTER)                                                      
         B     WKNEXT                                                           
         EJECT                                                                  
STA      CLC   C+1(6),=CL6' '                                                   
         BE    STA4                                                             
         XC    WKINDEX,WKINDEX                                                  
         MVC   WKINDEX(2),USERID                                                
         MVC   WKINDEX+2(6),C+1                                                 
*                                                                               
STA2     GOTO1 =V(DATAMGR),DMCB,(X'80',=C'INDEX')                               
         CLI   8(R1),0                                                          
         BE    STA4                                                             
         TM    8(R1),X'80'                                                      
         BO    WKNEXT                                                           
         DC    H'0'                                                             
*                                                                               
STA4     LA    R0,=C'DELETE'                                                    
         CLI   C+7,C'D'                                                         
         BE    STA6                                                             
         LA    R0,=C'HOLD'                                                      
         CLI   C+7,C'H'                                                         
         BE    STA6                                                             
         LA    R0,=C'KEEP'                                                      
         CLI   C+7,C'K'                                                         
         BE    STA6                                                             
         LA    R0,=C'PURGE'                                                     
         CLI   C+7,C'P'                                                         
         BE    STA6                                                             
         LA    R0,=C'RESTORE'                                                   
         CLI   C+7,C'R'                                                         
         BE    STA6                                                             
         LA    R0,=C'UNKEEP'                                                    
         CLI   C+7,C'U'                                                         
         BE    STA6                                                             
         B     WKNEXT                                                           
*                                                                               
STA6     GOTO1 =V(DATAMGR),DMCB,(R0)                                            
         CLI   8(R1),0                                                          
         BE    STA8                                                             
         DC    H'0'                                                             
*                                                                               
STA8     GOTO1 =V(HEXOUT),PARM,(R4),P,60,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,8(R7),P,60,=C'TOG'                               
         GOTO1 =V(PRINTER)                                                      
         B     WKNEXT                                                           
         EJECT                                                                  
LIB      LA    R6,WKINDEX+16                                                    
         XC    0(16,R6),0(R6)                                                   
         MVC   0(3,R6),C+1                                                      
         OC    C+4(4),=C'0000'                                                  
         PACK  DUB,C+4(4)                                                       
         CVB   R0,DUB                                                           
         ST    R0,4(R6)                                                         
         OC    C+8(4),=C'0000'                                                  
         PACK  DUB,C+8(4)                                                       
         CVB   R0,DUB                                                           
         ST    R0,8(R6)                                                         
         L     RF,=A(WKREC)                                                     
         SLL   R0,16                                                            
         ST    R0,0(RF)                                                         
         MVC   4(4,RF),C+4                                                      
         MVC   8(8,RF),C+12                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'LIBRARY')                             
         MVC   3(1,R6),DMCB+8                                                   
         GOTO1 =V(HEXOUT),PARM,WKINDEX,P,32,=C'TOG'                             
         L     RF,=A(WKREC)                                                     
         MVC   P+66(12),4(RF)                                                   
         GOTO1 =V(PRINTER)                                                      
         B     WKNEXT                                                           
         EJECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
C        DS    CL80                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&UK                                                                           
USERID   DC    H'38'                                                            
*&&                                                                             
*&&US                                                                           
USERID   DC    H'43'                                                            
*&&                                                                             
         DS    0D                                                               
WKFILE   DC    CL8' '                                                           
         DC    C'**WKNDX*'                                                      
WKINDEX  DC    XL32'00'                                                         
         DS    0D                                                               
         DC    C'**WKREC*'                                                      
WKREC    DC    1024X'00'                                                        
         DS    0D                                                               
         DC    C'**WKBUF*'                                                      
WKBUFF   DC    4000X'00'                                                        
         DS    0D                                                               
         DC    C'**WKWRK*'                                                      
WKWORK   DC    5000D'0'                                                         
*                                                                               
UTL      CSECT                                                                  
         DC    F'0',X'01000000'                                                 
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMWRKRT   03/13/91'                                      
         END                                                                    
