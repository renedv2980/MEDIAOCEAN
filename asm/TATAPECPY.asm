*          DATA SET TATAPECPY  AT LEVEL 016 AS OF 05/01/02                      
*PHASE TACOPYA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE TINVCON                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'TATAPECPY - COPY AND EDIT TAPES'                                
TAPECPY  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,TAPECPY,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING TAPECPY+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,TAPECPY          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (TAPEIN,(INPUT))                                                 
         OPEN  (TAPEOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(24),=C'TALENT TAPE COPY UTILITY'                           
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
         LA    R3,RECORD           R3=A(RECORD)                                 
*                                                                               
MN10     GET   TAPEIN,RECORD       GET A RECORD                                 
*                                                                               
         CLI   0(R3),C'I'          IF THIS IS 'I' RECORD                        
         BNE   MN20                                                             
         AP    DROPCNT,=P'1'       COUNT IT BUT                                 
         B     MN10                DON'T PUT IT ON NEW TAPE                     
*                                                                               
MN20     CLC   0(2,R3),=C'RT'      IF THIS IS TOTAL RECORD                      
         BE    MN60                PRINT IT OUT                                 
         CLC   0(2,R3),=C'RF'      IF THIS IS FINAL RECORD                      
         BE    MN60                PRINT IT OUT                                 
*                                                                               
         CLC   0(2,R3),=C'RW'      IF THIS IS 'RW' RECORD                       
         BNE   MN90                                                             
         CLC   87(22,R3),=22X'00'  IF 2ND ADDRESS LINE IS ZEROS                 
         BNE   MN90                                                             
         MVC   87(22,R3),=22C' '   SET IT TO SPACES                             
         CP    OUTCNT,=P'10'       PRINT OUT SOME OF THE CHANGED RW'S           
         BH    MN90                                                             
*                                                                               
MN60     BAS   RE,PRINTREC                                                      
*                                                                               
MN90     PUT   TAPEOUT,RECORD      WRITE THE RECORD                             
         AP    OUTCNT,=P'1'        ADD TO OUTPUT COUNT                          
         B     MN10                                                             
         EJECT                                                                  
PRINTREC NTR1                                                                   
         GOTO1 =V(PRNTBL),DMCB,0,RECORD,C'DUMP',512,=C'1D'                      
         B     XIT                                                              
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE TAPEIN                                                           
         CLOSE TAPEOUT                                                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         EDIT  (P4,DROPCNT),(10,P+1),ZERO=NOBLANK,COMMAS=YES                    
         MVC   P+12(12),=C'RECS DROPPED'                                        
         GOTO1 =V(PRINTER)                                                      
         EDIT  (P4,OUTCNT),(10,P+1),ZERO=NOBLANK,COMMAS=YES                     
         MVC   P+12(16),=C'RECS ON NEW TAPE'                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,RECFM=FB,LRECL=512,              X        
               MACRF=GM,EODAD=ENDIN,BLKSIZE=23040                               
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,RECFM=FB,LRECL=512,             X        
               MACRF=PM,BLKSIZE=23040,BUFNO=2                                   
         EJECT                                                                  
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
OUTCNT   DC    PL4'0'                                                           
DROPCNT  DC    PL4'0'                                                           
DUB      DS    D                                                                
WORK     DS    XL64                                                             
*                                                                               
RECORD   DC    512C' '                                                          
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016TATAPECPY 05/01/02'                                      
         END                                                                    
