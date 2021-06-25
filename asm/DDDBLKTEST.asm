*          DATA SET DDDBLKTEST AT LEVEL 001 AS OF 02/16/82                      
*PHASE DBLKTEST,*                                                               
*INCLUDE DBLKTAPE                                                               
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DBLKTEST - PROGRAM TO TEST DBLKTAPE MODULE'                     
         PRINT NOGEN                                                            
DBLKTEST CSECT                                                                  
         NBASE 0,DBLKTEST                                                       
         L     RD,=A(DBLKWORK)                                                  
*                                                                               
DBLK0    GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BNE   DBLK1                                                            
EOJ      EOJ                                                                    
*                                                                               
DBLK1    XC    P1(16),P1                                                        
         LA    R1,REC                                                           
         ST    R1,P1                                                            
         MVC   P4(1),C                                                          
         LA    R1,C+1                                                           
         STCM  R1,7,P4+1                                                        
*                                                                               
         OC    C+8(2),=C'0000'     DADDS DISP TO LINK                           
         PACK  DUB,C+8(2)                                                       
         CVB   R0,DUB                                                           
         STC   R0,P2                                                            
*                                                                               
         OC    C+10(4),=C'0000'    DADDS BLOCK LENGTH                           
         PACK  DUB,C+10(4)                                                      
         CVB   R0,DUB                                                           
         STH   R0,P5                                                            
         LA    R0,P5                                                            
         STCM  R0,7,P2+1                                                        
*                                                                               
         CLI   C+14,C' '           PHYSICAL I/O TRACE                           
         BE    *+10                                                             
         MVC   P3,=C'TRAC'                                                      
*                                                                               
         OC    C+16(4),=C'0000'    MAX RECORDS                                  
         PACK  DUB,C+16(4)                                                      
         CVB   R5,DUB                                                           
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R5,20                                                            
*                                                                               
DBLKA    GOTO1 =V(DBLKTAPE),P1                                                  
         OC    P1,P1                                                            
         BZ    DBLK0                                                            
*                                                                               
DBLKB    SR    R6,R6               GET RECORD LENGTH                            
         ICM   R6,3,REC-4                                                       
         SH    R6,=H'4'                                                         
         CLI   C+15,C' '           TEST TO PRINT                                
         BE    DBLKC                                                            
         GOTO1 =V(PRNTBL),DMCB,0,REC,C'DUMP',(R6),=C'2D'                        
*                                                                               
DBLKC    BCT   R5,DBLKA                                                         
         B     DBLK0                                                            
         EJECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
*                                                                               
C        DS    CL80                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DC    4X'00'                                                           
REC      DC    2000X'00'                                                        
*                                                                               
DBLKWORK DC    500D'0'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDDBLKTEST02/16/82'                                      
         END                                                                    
