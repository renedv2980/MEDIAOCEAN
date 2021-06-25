*          DATA SET DDEDICTDL4 AT LEVEL 033 AS OF 03/26/02                      
*PHASE EDICTDBG                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'EDICTDBG - DUMP EDICT FILE, DOUBLE TRACKS/DAY'                  
EDICTDBG CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDICTDBG,=V(REGSAVE)                                           
*==================================                                             
*        GET THIS RIGHT !!!                                                     
TKPERDAY EQU   640                 OLD NUMBER OF TRACKS/DAY                     
*                                                                               
* THE PROGRAM DDEDICTDL MUST ALSO BE CHANGED. FILETAB CONTAINS                  
* THE HARD-CODED FILE HEADER RECORDS, AND MUST REFLECT THE CORRECT              
* NUMBER OF TRACKS PER DAY.                                                     
*==================================                                             
*                                                                               
HIGHTRK  EQU   31*TKPERDAY                                                      
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         EJECT                                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         LHI   RF,TKPERDAY                                                      
         MHI   RF,2                DOUBLE TRACKS/DAY                            
         STCM  RF,3,RECORD1+6                                                   
*                                                                               
         GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
*                                                                               
         GOTO1 =V(DADDS),DMCB,DARPT,,F'18432'                                   
         LH    R4,DMCB+10          RECORDS (BLOCKS) PER TRACK                   
         CHI   R4,3                                                             
         BE    *+6                                                              
         DC    H'0'                MUST BE 3 RECORDS PER TRACK                  
*                                                                               
         OPEN  (TAPEFILE,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R2,1                                                             
         MVI   BLOCK+3,0                                                        
         MVI   NEWDSKAD+3,0                                                     
*                                                                               
DUMP10   STH   R2,BLOCK            TRACK NUMBER                                 
         LR    RF,R2                                                            
         BCTR  RF,0                                                             
         SR    RE,RE               PREPARE FOR DIVIDE                           
         LHI   R0,TKPERDAY         OLD NUMBER OF TRACKS/DAY                     
         DR    RE,R0               RE = (OLD TRACK #) MOD (TRACKS/DAY)          
         AHI   RE,1                BASED FROM 1                                 
         LR    RF,R2                                                            
         MHI   RF,2                DOUBLE THE OLD TRACK NUMBER                  
         SR    RF,RE                                                            
         STH   RF,NEWDSKAD         NEW TRACK NUMBER                             
*                                                                               
         C     RF,=X'0000FFFF'                                                  
         BNH   *+6                                                              
         DC    H'0'                NEW TRACK NUMBER EXCEEDS HALFWORD!           
*                                                                               
         LHI   R3,1                RESET BLOCK NUMBER                           
DUMP20   STC   R3,BLOCK+2          BLOCK NUMBER                                 
         STC   R3,NEWDSKAD+2       NEW BLOCK NUMBER                             
*                                                                               
         GOTO1 =V(DADDS),DMCB,RDID,BLOCK+4,0,EDICTFL,BLOCK,0                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                EOF                                          
*                                                                               
         MVC   P(10),=C'OLD D/A = '                                             
         GOTO1 =V(HEXOUT),DMCB,BLOCK,P+10,4,=C'TOG'                             
         MVC   P+25(10),=C'NEW D/A = '                                          
         GOTO1 =V(HEXOUT),DMCB,NEWDSKAD,P+35,4,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ICM   R6,15,BLOCK         SAVE OLD DISK ADDRESS                        
         MVC   BLOCK(4),NEWDSKAD   NEW DISK ADDRESS                             
*                                                                               
         CLC   =X'00010100',BLOCK  PUT CORRECT NUMBERS IN HEADER RECORD         
         BNE   *+10                                                             
         MVC   BLOCK+4(L'RECORD1),RECORD1                                       
*                                                                               
         PUT   TAPEFILE,BLOCK                                                   
         STCM  R6,15,BLOCK         RESTORE OLD DISK ADDRESS                     
*                                                                               
         AHI   R3,1                BUMP BLOCK NUMBER                            
         CR    R3,R4                                                            
         BNH   DUMP20                                                           
*                                                                               
         AHI   R2,1                BUMP TRACK NUMBER                            
         CHI   R2,HIGHTRK                                                       
         BNH   DUMP10              DO THE NEXT TRACK                            
*                                                                               
         CLOSE TAPEFILE                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    8F                                                               
*                                                                               
UTL      DC    F'0',F'0'                                                        
         SPACE 3                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
WORK     DS    CL17                                                             
NEWDSKAD DS    F                                                                
*                                                                               
RECORD1  DC    XL256'00'                                                        
         ORG   RECORD1                                                          
         DC    X'000000FF00FF000048010003' NEW FIRST RECORD                     
         ORG                                                                    
*                                                                               
EDICTFL  DMDA                                                                   
*                                                                               
TAPEFILE DCB   DDNAME=TAPEFILE,DSORG=PS,LRECL=18436,BLKSIZE=18436,     +        
               RECFM=FB,MACRF=PM                                                
*                                                                               
         DS    0D                                                               
         DC    C'***I/O**'                                                      
BLOCK    DC    18436X'00'          FIRST FOUR BYTES ARE DISK ADDRESS            
         EJECT                                                                  
* ++INCLUDE DMGREQUS                                                            
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033DDEDICTDL403/26/02'                                      
         END                                                                    
