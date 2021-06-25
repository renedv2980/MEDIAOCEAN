*          DATA SET DDEDICTDL  AT LEVEL 008 AS OF 10/12/00                      
*PHASE EDICTDLA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
         TITLE 'EDICTDL - DUMP/LOAD EDICT FILE'                                 
EDICTDL  CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,*EDICTDL,=V(REGSAVE),R9,R8,R7,R6,R5                            
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'DUMP',CARD                                                    
         BE    DUMP                                                             
         CLC   =C'LOAD=',CARD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TYPE,CARD+5                                                      
         B     LOAD                                                             
         EJECT                                                                  
DUMP     GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
*                                                                               
         GOTO1 =V(DADDS),DMCB,DARPT,,F'18432'                                   
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
         LA    R2,1                                                             
         MVI   BLOCK+3,0                                                        
*                                                                               
DUMP10   STH   R2,BLOCK            TRACK NUMBER                                 
*                                                                               
         LA    R3,1                RESET BLOCK NUMBER                           
DUMP20   STC   R3,BLOCK+2          BLOCK NUMBER                                 
*                                                                               
         GOTO1 =V(DADDS),DMCB,RDID,BLOCK+4,0,EDICTFL,BLOCK,0                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BNE   DUMP30              EOF                                          
*                                                                               
         PUT   TAPEFILE,BLOCK                                                   
*                                                                               
         LA    R3,1(R3)            BUMP BLOCK NUMBER                            
         CR    R3,R4                                                            
         BNH   DUMP20                                                           
*                                                                               
         LA    R2,1(R2)            BUMP TRACK NUMBER                            
         B     DUMP10                                                           
*                                                                               
DUMP30   CLOSE TAPEFILE                                                         
         LTR   RF,RF                                                            
         BZ    GOODBYE                                                          
         DC    H'0'                                                             
         EJECT                                                                  
* FIRST FORMAT THE FILE (INITIALIZE ALL TRACKS TO NULLS)                        
*                                                                               
LOAD     GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
         LA    RF,EDICTFL                                                       
         MVC   DNEXT-DTFPHD(4,RF),=X'00010000'                                  
*                                                                               
         GOTO1 =V(DADDS),DMCB,DARPT,,F'18432'                                   
         LH    R1,DMCB+10          RECORDS (BLOCKS) PER TRACK                   
         CH    R1,=H'3'                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE 3 RECORDS PER TRACK                  
*                                                                               
         BCTR  R1,0                DYNAMICALLY BUILD CCW CHAIN                  
         LA    R2,BUF                                                           
         MVC   12(12,R2),0(R2)                                                  
         LA    R2,12(R2)                                                        
         BCT   R1,*-10                                                          
*                                                                               
         LA    R2,12(R2)           COMPUTE LENGTH OF CCW CHAIN                  
         S     R2,=A(BUF)                                                       
*                                                                               
LOAD10   GOTO1 =V(DADDS),DMCB,WTTRK,CCW,(R2),EDICTFL,ADDR,(X'FF',BUF)           
         TM    9(R1),X'04'                                                      
         BO    LOAD20                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    LOAD10                                                           
         DC    H'0'                                                             
*                                                                               
LOAD20   GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
*                                                                               
* NOW GET THE APPROPRIATE CONTROL INFO FOR THIS PARTICULAR EDICT FILE           
*                                                                               
         LA    RF,FILETAB          EDICT FILES AND CONTROL RECORDS              
         USING FILETABD,RF                                                      
LOAD25   CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                YES -- UNKNOWN FILE TYPE                     
         CLC   FILETYPE,TYPE                                                    
         BE    *+12                                                             
         LA    RF,FILETABL(,RF)    BUMP TO NEXT TABLE ENTRY                     
         B     LOAD25                                                           
         MVC   BUFDATA(L'FILEDATA),FILEDATA PUT CONTROL DATA IN 1ST REC         
         DROP  RF                                                               
*                                                                               
* NOW WRITE THE FIRST BLOCK (INCLUDING THE CONTROL INFO)                        
*                                                                               
         GOTO1 =V(DADDS),DMCB,WTID,BUFDATA,F'18432',EDICTFL,           +        
               =X'00010100',0                                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                 FIRST RECORD WRITTEN OK                      
         DC    H'0'                                                             
*                                                                               
* OPEN THE DUMP TAPE                                                            
*                                                                               
         OPEN  (TAPEFILE,INPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOAD30   GET   TAPEFILE,BLOCK                                                   
*                                                                               
* IF TAPEFILE IS DUMMY, THEN WE GO DIRECTLY TO EOF, BUT THAT'S OK,              
* BECAUSE WE'VE ALREADY CLEARED THE FILE.                                       
*                                                                               
* FIRST FOUR BYTES OF EACH RECORD ARE THE DISK ADDRESS.                         
*                                                                               
         GOTO1 =V(DADDS),DMCB,WTID,BLOCK+4,F'18432',EDICTFL,BLOCK,0             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    LOAD30                                                           
         DC    H'0'                                                             
*                                                                               
LOAD40   CLOSE TAPEFILE                                                         
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    8F                                                               
ADDR     DS    F                                                                
TYPE     DS    CL3                 ADV/REP/TST (SEE FILETAB)                    
*                                                                               
FILETAB  DS    0C                                                               
         DC    C'ADV',X'000000FF00FF028048010003'                               
         DC    C'REP',X'000000FF00FF028048010003'                               
         DC    C'TST',X'000000FF00FF000848010003'                               
         DC    X'FF'                                                            
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
         DS    0D                                                               
CCW      DS    256X                                                             
CARD     DS    CL80                                                             
WORK     DS    CL17                                                             
*                                                                               
EDICTFL  DMDA                                                                   
*                                                                               
TAPEFILE DCB   DDNAME=TAPEFILE,DSORG=PS,LRECL=18436,BLKSIZE=18436,     +        
               RECFM=FB,MACRF=(GM,PM),EODAD=LOAD40                              
*                                                                               
         DS    0D                                                               
         DC    C'***I/O**'                                                      
BLOCK    DC    18436X'00'          FIRST FOUR BYTES ARE DISK ADDRESS            
*                                                                               
         DS    0D                                                               
BUF      DC    6X'00'              COUNT FIELD                                  
         DC    AL2(18432)                                                       
         DC    A(BUFDATA)          ADDRESS OF DATA AREA                         
         DS    200X                                                             
LBUF     DS    F                                                                
BUFDATA  DC    60000X'00'                                                       
         EJECT                                                                  
FILETABD DSECT                                                                  
FILETYPE DS    CL3                 ADV/REP/TST                                  
FILEDATA DS    XL12                CONTROL INFO (SEE DDEDICTFIL)                
FILETABL EQU   *-FILETABD                                                       
         SPACE 3                                                                
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DDEDICTDL 10/12/00'                                      
         END                                                                    
