*          DATA SET DDEDICTCMP AT LEVEL 022 AS OF 08/13/00                      
*PHASE EDICTCMA EDICTCMP                                                        
*INCLUDE REGSAVE                                                                
         TITLE 'EDICTCMP -- TAPE TO TAPE EDICT FILE BLOCKSIZE CHANGE'           
EDICTCMP CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,EDICTCMP,=V(REGSAVE)                                           
*                                                                               
* GET THIS RIGHT!!!                                                             
*                                                                               
TKPERDAY EQU   160                 TRACKS/DAY                                   
*                                                                               
         MVC   RECORD1+6(2),=Y(TKPERDAY)                                        
*                                                                               
         OPEN  TAPEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,1                START WITH DAY 1                             
         LA    R8,1                START WITH TRACK 1                           
*                                                                               
         LA    R2,TKPERDAY         TRACKS PER DAY. . .                          
         MH    R2,=H'3'            . . . TIMES BLOCKS PER TRACK. . .            
         MH    R2,=H'18432'        . . . TIMES BLOCKSIZE = TABLE SIZE           
         ST    R2,TABLELEN                                                      
         LA    R2,8(R2)            ROOM FOR LABEL                               
         STORAGE OBTAIN,LENGTH=(2)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,R1),=C'*TABLE**'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,ATABLE           A(START OF TABLE)                            
*                                                                               
NEXTDAY  L     RE,ATABLE           CLEAR TABLE                                  
         L     RF,TABLELEN         SET LENGTH TO CLEAR                          
         SR    R0,R0               SET FROM ADDRESS                             
         SR    R1,R1               SET FROM LEN                                 
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,ATABLE                                                        
         LA    R3,TKPERDAY                                                      
         MH    R3,=H'3'            R3 = BLOCKS/DAY                              
*                                                                               
GETBLOCK GET   TAPEIN,BLOCK                                                     
*                                                                               
         LR    RE,R2               A(PLACE TO PUT BLOCK IN TABLE)               
         LH    RF,=H'14336'        SET LENGTH TO MOVE                           
         LA    R0,BLOCK+4          A(14K BLOCK)                                 
         LR    R1,RF               SET FROM LEN                                 
         MVCL  RE,R0                                                            
*                                                                               
         BC    0,*+14              ADJUST THE VERY FIRST RECORD                 
         MVI   *-3,X'F0'                                                        
         MVC   0(L'RECORD1,R2),RECORD1                                          
*                                                                               
         AH    R2,=H'14336'        BUMP TABLE POINTER                           
         BCT   R3,GETBLOCK                                                      
*                                                                               
         L     R2,ATABLE                                                        
         LA    R5,TKPERDAY                                                      
         MVI   BLOCK+3,0                                                        
*                                                                               
NEXTTRK  STH   R8,BLOCK            TRACK NUMBER                                 
*                                                                               
         LA    R3,1                RESET BLOCK NUMBER                           
NEXTBLK  STC   R3,BLOCK+2          BLOCK NUMBER                                 
         LA    R6,BLOCK+4                                                       
*                                                                               
         LA    R0,72               LOGICAL RECORD COUNTER                       
NEXTREC  MVC   0(256,R6),0(R2)     MOVE IN ONE LOGICAL RECORD                   
         LA    R2,256(R2)          BUMP TABLE POINTER                           
         LA    R6,256(R6)          BUMP BLOCK POINTER                           
         BCT   R0,NEXTREC                                                       
*                                                                               
         PUT   TAPEOUT,BLOCK                                                    
         LA    R3,1(R3)            BUMP BLOCK NUMBER                            
         CH    R3,=H'3'            3 BLOCKS/TRACK                               
         BNH   NEXTBLK                                                          
*                                                                               
         LA    R8,1(R8)            BUMP TRACK NUMBER                            
         BCT   R5,NEXTTRK                                                       
*                                                                               
         LA    R7,1(R7)            BUMP DAY NUMBER                              
         CH    R7,=H'31'           31 DAYS/MONTH                                
         BNH   NEXTDAY                                                          
*                                                                               
         CLOSE TAPEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE TAPEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
WORK     DS    CL17                                                             
ATABLE   DS    A                                                                
TABLELEN DS    F                                                                
*                                                                               
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,LRECL=14340,BLKSIZE=14340,       +        
               RECFM=FB,MACRF=GM                                                
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,LRECL=18436,BLKSIZE=18436,      +        
               RECFM=FB,MACRF=PM                                                
*                                                                               
RECORD1  DC    XL256'00'                                                        
         ORG   RECORD1                                                          
         DC    X'000000FF00FF000048010003'   NEW FIRST RECORD                   
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'***I/O**'                                                      
BLOCK    DC    18436X'00'          FIRST FOUR BYTES ARE DISK ADDRESS            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022DDEDICTCMP08/13/00'                                      
         END                                                                    
