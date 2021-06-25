*          DATA SET SPLFM25    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T21925A,+0                                                               
         TITLE 'SPLFM25 - SYN RECORD  T21925'                                   
T21925   CSECT                                                                  
         NMOD1 0,T21925                                                         
         PRINT NOGEN                                                            
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING REPRECD,R8                                                       
         CLI   SVFMTSW,0                     TEST FORMAT OR EDIT                
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         FOUT  REPNAMEH,RNAME,22                                                
         FOUT  REPSTADH,R1LINE,24                                               
         XC    WORK(10),WORK                                                    
         OC    RGDEMO,RGDEMO                                                    
         BZ    FMT3                                                             
         MVI   WORK,C'$'                                                        
         CLI   RGDEMO+1,X'FF'                                                   
         BE    FMT3                                                             
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DMCB+4(4),=X'D9000AE0' DEMOCON                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'         PHASE NOT FOUND                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,RGDEMO),(2,WORK),(C'S',DBLOCK),REC2                 
FMT3     FOUT  REPGOALH,WORK,7                                                  
         FOUT  REPOCLTH,RCLTOWN,3                                               
         XC    WORK(20),WORK                                                    
         OC    RCLTOWN,RCLTOWN                                                  
         BZ    FMT4                                                             
         LA    R2,REPOCLTH                                                      
         BAS   R9,FMTCLT                                                        
*                                                                               
FMT4     FOUT  REPONAMH,WORK,20                                                 
*                                                                               
FMT10    XC    REPPCT,REPPCT                                                    
         CLC   RCADJF,=C'  '                                                    
         BE    FMT15                                                            
         OC    RCADJF,RCADJF                                                    
         BZ    FMT15                                                            
         EDIT  (B2,RCADJF),(6,REPPCT),2,ALIGN=LEFT                              
*                                                                               
FMT15    FOUT  REPPTYPH,RPTYPE,1                                                
         FOUT  REPPCTH                                                          
         B     EXXMOD                                                           
*                                                                               
FMTCLT   XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         XC    DUB,DUB                                                          
         MVC   DUB(3),8(R2)                                                     
         OI    DUB+2,X'40'                                                      
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),DUB,DUB+3                                              
         CLI   0(R1),0                                                          
         BNE   NOCLT                                                            
         MVC   KEY+2(2),DUB+3                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY                                                   
         BNE   NOCLT                                                            
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         USING CLTHDRD,R7                                                       
         MVC   WORK(20),CNAME                                                   
         ST    R8,AREC                                                          
         BR    R9                                                               
*                                                                               
NOCLT    MVC   WORK(20),=CL20'** CLT NOT FOUND **'                              
         BR    R9                                                               
*                                                                               
         EJECT                                                                  
EDT      CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
EDT0     LA    R2,REPNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   RNAME,8(R2)                                                      
         LA    R2,REPSTADH                                                      
         GOTO1 ANY                                                              
         MVC   R1LINE,8(R2)                                                     
EDT3     LA    R2,REPGOALH                                                      
         MVI   ERRCD,INVERR                                                     
         GOTO1 ANY                                                              
         XC    RGDEMO,RGDEMO                                                    
         CLI   5(R2),1                                                          
         BNE   EDT3B                                                            
         CLI   REPGOAL,C'$'                                                     
         BNE   EDT3B                                                            
         MVI   RGDEMO+1,X'FF'                                                   
         B     EDT4                                                             
*                                                                               
EDT3B    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DMCB+4(4),=X'D9000AD9' DEMOVAL                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   DMCB,X'FF'          PHASE NOT FOUND                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,0(R2)),(1,WORK),(C'S',DBLOCK),REC2                  
         CLI   DMCB+4,0                                                         
         BE    LFMERR                                                           
         MVC   RGDEMO,WORK                                                      
         DROP  R5                                                               
EDT4     LA    R2,REPOCLTH                                                      
         XC    WORK(20),WORK                                                    
         XC    RCLTOWN,RCLTOWN                                                  
         CLI   5(R2),0                                                          
         BE    EDT5                                                             
         CLI   5(R2),1                                                          
         BNH   LFMERR                                                           
         BAS   R9,GETCLT                                                        
         MVC   RCLTOWN,REPOCLT                                                  
         OC    RCLTOWN,SPACES                                                   
         B     EDT5                                                             
*                                                                               
EDT5     FOUT  REPONAMH,WORK,20                                                 
*                                                                               
EDT10    DS    0H                                                               
         LA    R2,REPPCTH                                                       
         XC    RCADJF,RCADJF                                                    
         CLI   5(R2),0                                                          
         BE    EDT10X                                                           
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,REPPCT),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    LFMERR                                                           
         L     R5,DMCB+4                                                        
         C     R5,=F'0'                                                         
         BNH   LFMERR                                                           
         C     R5,=F'16448'        WILL BE X'4040' - LIKE SPACES                
         BE    LFMERR                                                           
         C     R5,=F'20000'        CAN'T EXCEED 200.00 PCT                      
         BH    LFMERR                                                           
         STH   R5,HALF                                                          
         MVC   RCADJF,HALF                                                      
*                                                                               
EDT10X   B     EDT15                                                            
*                                                                               
EDT15    DS    0H                                                               
         MVI   RPTYPE,0                                                         
         LA    R2,REPPTYPH                                                      
         CLI   5(R2),0                                                          
         BE    EDT15X                                                           
         CLI   8(R2),C'O'          OTO                                          
         BE    EDT15D                                                           
         CLI   8(R2),C'W'          WEEKLY                                       
         BNE   LFMERR                                                           
*                                                                               
EDT15D   MVC   RPTYPE,8(R2)                                                     
EDT15X   B     EDTX                                                             
*                                                                               
*                                                                               
*                                                                               
GETCLT   XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         XC    DUB,DUB                                                          
         MVC   DUB(3),8(R2)                                                     
         OI    DUB+2,X'40'                                                      
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),DUB,DUB+3                                              
         CLI   0(R1),0                                                          
         BNE   NOCLT                                                            
         MVC   KEY+2(2),DUB+3                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY                                                   
         BNE   CLTNOF                                                           
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         USING CLTHDRD,R7                                                       
         MVC   WORK(20),CNAME                                                   
         ST    R8,AREC                                                          
         BR    R9            RETURN                                             
*                                                                               
         DROP  R7                                                               
*                                                                               
CLTNOF   MVI   ERRCD,NOCLTERR                                                   
         B     LFMERR                                                           
*                                                                               
EDTX     MVI   RSYND,C'S'                                                       
         MVC   R3LINE(8),SPACES                                                 
         ST    R8,AREC             RESET AREC TO                                
         MVC   KEY,SVKEY                                                        
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   WRITE                                                            
         MVC   REC(17),SVKEY                                                    
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
* 10/22/92   KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            SPREP-TYPE RECORDS IS TO BE 144.                                   
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         GOTO1 CNADDSTA                                                         
         B     REQREC                                                           
*                                                                               
WRITE    LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA               REREAD REC BEFORE WRITE                      
         TM    DMCB+8,X'50'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
* 10/22/92   KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            SPREP-TYPE RECORDS IS TO BE 144.                                   
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         ST    R8,AREC                                                          
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 STA                                                              
         GOTO1 CNCHASTA                                                         
         B     REQREC                                                           
         EJECT                                                                  
REQREC   XC    REC(110),REC             GENERATE REQUEST RECORD                 
         MVI   REC+10,45                                                        
         MVI   REC+14,106                                                       
         MVI   REC+26,X'40'                                                     
         MVC   REC+27(79),REC+26                                                
         MVC   REC+26(2),=C'45'                                                 
         MVC   REC+28(2),AGYALPHA                                               
         MVC   REC+30(1),SVEBCMED                                               
         MVC   REC+36(1),SVKEY                                                  
         MVC   REC+44(5),SVKEY+2                                                
         MVC   REC+94(7),=C'CONTROL'                                            
         MVC   REC+93(1),SVACT                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
*                                                                               
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFME5                                                                        
       ++INCLUDE SPLFME5D                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         SPACE 2                                                                
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPLFM25   05/01/02'                                      
         END                                                                    
