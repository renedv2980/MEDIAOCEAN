*          DATA SET DDEDICTDL2 AT LEVEL 028 AS OF 02/25/97                      
*PHASE EDICTDL2                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'EDICTDL2 - DUMP EDICT FILE, DOUBLING LRECL'                     
EDICTDL2 CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDICTDL2,=V(REGSAVE)                                           
*==================================                                             
*                                                                               
* GET THIS SYMBOL CORRECT!!!                                                    
*                                                                               
TKPERDAY EQU   160                 NUMBER OF TRACKS/DAY                         
*==================================                                             
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         EJECT                                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         MVC   RECORD1+6(2),=Y(TKPERDAY)                                        
*                                                                               
         GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
*                                                                               
         GOTO1 =V(DADDS),DMCB,DARPT,,F'14336'                                   
         LH    R4,DMCB+10          RECORDS (BLOCKS) PER TRACK                   
         CH    R4,=H'3'                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE 3 RECORDS PER TRACK                  
*                                                                               
         OPEN  (TAPEFILE,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,1                FIRST NEW TRACK NUMBER                       
         LA    R7,1                FIRST NEW BLOCK NUMBER                       
*                                                                               
         LA    R2,1                                                             
         MVI   BLOCK+3,0                                                        
*                                                                               
DUMP5    LR    RF,R2                                                            
         BCTR  RF,0                                                             
         AH    RF,=Y(HALFDAY)                                                   
         STH   RF,MAXTRACK                                                      
*                                                                               
DUMP10   STH   R2,BLOCK            TRACK NUMBER                                 
*                                                                               
         LA    R3,1                RESET BLOCK NUMBER                           
DUMP20   STC   R3,BLOCK+2          BLOCK NUMBER                                 
*                                                                               
         LA    R1,31               DAYS/MONTH                                   
         MH    R1,=Y(TKPERDAY)     TRACKS/DAY                                   
         CR    R6,R1               R1 = HIGHEST POSSIBLE TRACK NUMBER           
         BH    DUMP30              NO MORE TO WRITE                             
*                                                                               
         MVC   P(22),=C'READING DISK ADDRESS: '                                 
         GOTO1 =V(HEXOUT),DMCB,BLOCK,P+22,4,=C'TOG'                             
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DADDS),DMCB,RDID,BLOCK+4,0,EDICTFL,BLOCK,0                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BNE   DUMP30              EOF                                          
*                                                                               
         LA    R8,BLOCK+4                                                       
         L     R9,ANEWBLK                                                       
         LA    R9,4(R9)            BUMP PAST D/A                                
         LA    R0,56                                                            
DUMP25   MVC   0(128,R9),0(R8)                                                  
         XC    128(128,R9),128(R9)                                              
         LA    R8,128(,R8)                                                      
         LA    R9,256(,R9)                                                      
         BCT   R0,DUMP25                                                        
*                                                                               
         L     R9,ANEWBLK                                                       
         STH   R6,0(R9)            NEW TRACK NUMBER                             
         STC   R7,2(R9)            NEW BLOCK NUMBER                             
         MVI   3(R9),0                                                          
*                                                                               
         CLC   =X'00010100',0(R9)  PUT CORRECT NUMBERS IN HEADER RECORD         
         BNE   *+10                                                             
         MVC   4(L'RECORD1,R9),RECORD1                                          
*                                                                               
         MVC   P+35(22),=C'WRITING DISK ADDRESS: '                              
         GOTO1 =V(HEXOUT),DMCB,0(R9),P+57,4,=C'TOG'                             
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         PUT   TAPEFILE,(9)                                                     
         LA    R7,1(R7)            BUMP BLOCK NUMBER                            
         CH    R7,=H'3'            STILL ROOM ON THIS TRACK?                    
         BNH   *+12                                                             
         LA    R7,1                NO -- BUMP TO NEXT TRACK                     
         LA    R6,1(R6)                                                         
*                                                                               
         L     R9,ANEWBLK                                                       
         LA    R9,4(R9)            BUMP PAST D/A                                
         LA    R0,56                                                            
DUMP27   MVC   0(128,R9),0(R8)                                                  
         XC    128(128,R9),128(R9)                                              
         LA    R8,128(,R8)                                                      
         LA    R9,256(,R9)                                                      
         BCT   R0,DUMP27                                                        
*                                                                               
         L     R9,ANEWBLK                                                       
         STH   R6,0(R9)            NEW TRACK NUMBER                             
         STC   R7,2(R9)            NEW BLOCK NUMBER                             
         MVI   3(R9),0                                                          
*                                                                               
         MVC   P+35(22),=C'WRITING DISK ADDRESS: '                              
         GOTO1 =V(HEXOUT),DMCB,0(R9),P+57,4,=C'TOG'                             
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         PUT   TAPEFILE,(9)                                                     
         LA    R7,1(R7)            BUMP BLOCK NUMBER                            
         CH    R7,=H'3'            STILL ROOM ON THIS TRACK?                    
         BNH   *+12                                                             
         LA    R7,1                NO -- BUMP TO NEXT TRACK                     
         LA    R6,1(R6)                                                         
*                                                                               
         LA    R3,1(R3)            BUMP OLD BLOCK NUMBER                        
         CR    R3,R4                                                            
         BNH   DUMP20                                                           
*                                                                               
         LA    R2,1(R2)            BUMP OLD TRACK NUMBER                        
         CH    R2,MAXTRACK                                                      
         BNH   DUMP10                                                           
*                                                                               
         AH    R2,=Y(HALFDAY)      SKIP UPPER HALF OF DAY'S TRACKS              
         B     DUMP5                                                            
*                                                                               
DUMP30   CLOSE TAPEFILE                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
HALFDAY  EQU   TKPERDAY/2                                                       
MAXTRACK DS    H                                                                
ANEWBLK  DC    A(NEWBLOCK)                                                      
*                                                                               
DUB      DS    D                                                                
DMCB     DS    8F                                                               
*                                                                               
UTL      DC    F'0',F'0'                                                        
SSB      DC    F'0'                                                             
*                                                                               
WORK     DS    CL17                                                             
*                                                                               
RECORD1  DC    XL256'00'                                                        
         ORG   RECORD1                                                          
         DC    X'000000FF00FF0000380100'   NEW FIRST RECORD                     
         ORG                                                                    
*                                                                               
EDICTFL  DMDA                                                                   
*                                                                               
TAPEFILE DCB   DDNAME=TAPEFILE,DSORG=PS,LRECL=14340,BLKSIZE=14340,     +        
               RECFM=FB,MACRF=(GM,PM)                                           
*                                                                               
         DS    0D                                                               
         DC    C'*BLOCK**'                                                      
BLOCK    DC    14340X'00'          FIRST FOUR BYTES ARE DISK ADDRESS            
*                                                                               
         DS    0D                                                               
         DC    C'NEWBLOCK'                                                      
NEWBLOCK DC    14340X'00'          FIRST FOUR BYTES ARE DISK ADDRESS            
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028DDEDICTDL202/25/97'                                      
         END                                                                    
