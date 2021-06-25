*          DATA SET PPLFM05    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T40405A,+0  ******** NOTE A APPENDED TO PHASE NAME ****                  
         TITLE 'PPLFM05 - ESTIMATE HEADER COPY'                                 
*                                                                               
* SMYE   10/17/97  CHANGED MVI.. TO OI.. TO SET PESTTEST TO TEST                
*                  STATUS IN EDT10 PROC                                         
*                                                                               
* SMYE   09/12/97  SET PASS DELETE SWITCH OFF IMMEDIATELY AFTER READ            
*                  HIGH FOR ESTIMATE IN PROC EDT3                               
*                                                                               
         PRINT NOGEN                                                            
T40405   CSECT                                                                  
         NMOD1 0,T40405                                                         
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         USING   T404FFD,RA                                                     
         EJECT                                                                  
         LA    R4,IOAREA                                                        
         LH    R5,=H'2000'                                                      
         BAS   RE,CLEARWRK                                                      
         CLI   SCRNUM,X'F5'                                                     
         BE    EST5                                                             
         MVI   DSPSW,1             SET TO FORMAT                                
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F5'                                 
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'F5'                                                     
EST5     MVC   KEY+27(4),ESTADDR                                                
         BAS   RE,GETREC                                                        
         XC    PESTZZZ,PESTZZZ     CLEAR ALLOCATIONS                            
ESTSCRN  CLI   DSPSW,0                                                          
         BNE   FORMATE                                                          
EDT      DS    0H                                                               
         LA    R2,ESTPRD1H                                                      
*                                                                               
EDT2     CLI   5(R2),0                                                          
         BE    EDT4                                                             
         GOTO1 MOVE                                                             
         CLC   WORK(3),=C'ZZZ'     CAN'T COPY ZZZ                               
         BNE   EDT3                                                             
         LA    R3,POLERR                                                        
         B     ERROR                                                            
*                                                                               
EDT3     EQU   *                                                                
         BAS   RE,GETPRD                                                        
         MVC   KEY,PESTKEY                                                      
         MVC   KEY+7(3),WORK                                                    
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'       SET TO NOT PASS DELETES                
         CLC   KEY(12),KEYSAVE                                                  
         BNE   EDT4                                                             
         LA    R3,DUPERR                                                        
         TM    KEY+25,X'80'        TEST DELETED                                 
         BZ    ERROR                                                            
         LA    R3,DELERR                                                        
         B     ERROR                                                            
*                                                                               
*                                                                               
EDT4     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNE   EDT2                                                             
         EJECT                                                                  
* OK TO ADD ESTIMATES                                                           
         MVC   SVESTTST,PESTTEST    SAVE TEST STATUS                            
*                                   OF ORIGINAL EST                             
         LA    R2,ESTPRD1H                                                      
EDT10    CLI   5(R2),0                                                          
         BE    EDT14                                                            
         GOTO1 MOVE                                                             
         BAS   RE,GETPRD                                                        
         MVC   PESTKPRD,WORK                                                    
         MVC   KEY(25),PESTKEY                                                  
         CLI   OANSW,1           SEE IF OAN PRODUCT                             
         BNE   EDT10C                                                           
*****    MVI   PESTTEST,X'80'         MUST SET TO TEST ESTIMATE                 
         OI    PESTTEST,X'80'         MUST SET TO TEST ESTIMATE                 
*                                                                               
EDT10C   GOTO1 ADDREC                                                           
         MVC   PESTTEST,SVESTTST   RESTORE TEST STATUS FOR NEXT EST             
*                                                                               
REQ      DS    0H                                                               
         MVC   PRDTAB(250),PESTKEY     SAVE EST REC                             
         MVC   PRDTAB+250(250),PESTKEY+250                                      
*              ADD EMPTY BUCKET REC                                             
*                                                                               
         MVC   KEY(25),PESTKEY                                                  
         MVI   KEY+3,X'09'                                                      
         MVI   PESTKRCD,X'09'                                                   
         XC    PESTELEM(2),PESTELEM                                             
         MVC   PESTLEN,=X'0021'                                                 
         GOTO1 ADDREC                                                           
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'41'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),HDRMED                                                
         MVC   QAREA+5(3),HDRCLT                                                
         ST    R2,FULL             SAVE R2                                      
         LA    R2,HDRESTH                                                       
         BAS   RE,PACK                                                          
         L     R2,FULL             RESTORE R2                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+20(3),DUB                                                  
         MVC   QAREA+11(3),8(R2)                                                
         OI    QAREA+13,C' '                                                    
REQ4     DS    0H                                                               
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    EDT12                                                            
         SR    R3,R3                                                            
         B     ERROR                                                            
*                                                                               
*                                  RESTORE EST REC                              
EDT12    MVC   PESTKEY(250),PRDTAB                                              
         MVC   PESTKEY+250(250),PRDTAB+250                                      
EDT14    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNE   EDT10                                                            
         B     FMT2                GO DISPLAY PRD LISTS                         
         SPACE 2                                                                
GETPRD   DS    0H                                                               
         MVI   OANSW,0                                                          
         MVC   PRDTAB(250),PESTKEY     SAVE EST REC                             
         MVC   PRDTAB+250(250),PESTKEY+250                                      
         ST    RE,FULL                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(7),PESTKEY                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    GETPX                                                            
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
GETPX    GOTO1 GETREC                                                           
         CLI   PPRDOAN,C' '                                                     
         BNH   GETPX8                                                           
         MVI   OANSW,1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(12),PRDTAB        ESTIMATE KEY                               
         MVC   KEY+7(3),=C'ZZZ'      MUST SEE IF ZZZ EST IS LIVE                
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   GETPX8               NO ZZZ ESTIMATE                             
         GOTO1 GETREC                                                           
         TM    PESTTEST,X'80'        ZZZ MUST BE TEST                           
         BO    GETPX8                                                           
         LA    R3,POLERR             SET INVALID PRODUCT                        
         B     ERROR                                                            
*                                                                               
GETPX8   MVC   PESTKEY(250),PRDTAB                                              
         MVC   PESTKEY+250(250),PRDTAB+250      RESTORE ESTIMATE                
GETPXX   L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
FORMATE  DS    0H                                                               
*                                                                               
FMT2     LA    R2,ESTDSPH          CLEAR PROTECTED FIELDS                       
FMT4A    ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'            SET FOR EX                                   
         EX    RE,FMTOC                                                         
         BE    FMT4B                                                            
         EX    RE,FMTXC                                                         
         FOUT  (R2)                                                             
FMT4B    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),0                                                          
         BNE   FMT4A                                                            
         B     FMT4X                                                            
FMTOC    OC    8(0,R2),8(R2)                                                    
FMTXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
* BUILD LIST OF BRANDS THAT ARE OPEN                                            
*                                                                               
FMT4X    XC    KEY,KEY                                                          
         LA    R4,PRDTAB                                                        
         LH    R5,=H'2000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R6,PRDTAB                                                        
         MVC   KEY(7),PESTKEY                                                   
         MVI   KEY+3,X'06'                                                      
FMT5     GOTO1 HIGH                                                             
         B     FMT5B                                                            
FMT5A    GOTO1 SEQ                                                              
FMT5B    CLC   KEY(7),KEYSAVE                                                   
         BNE   FMT5X                                                            
         MVC   0(3,R6),KEY+7                                                    
         MVI   3(R6),X'FF'                                                      
         LA    R6,4(R6)                                                         
         B     FMT5A                                                            
*                                                                               
FMT5X    LA    R6,PRDTAB                                                        
*                                                                               
FMT6     XC    KEY,KEY                                                          
         MVC   KEY(12),PESTKEY                                                  
         MVC   KEY+7(3),0(R6)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+8                                                              
         MVI   3(R6),0             SET BRAND CODE TO 0 IF NOT OPEN              
*                                                                               
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   FMT6                                                             
         EJECT                                                                  
         LA    R2,ESTDSPH                                                       
         MVC   8(21,R2),=C'** BRANDS NOT OPEN **'                               
         FOUT  (R2)                                                             
*                                                                               
         ZAP   HALF,=P'14'                                                      
         LA    R4,32(R2)                                                        
         LA    R6,PRDTAB                                                        
FMT8     CLI   3(R6),0             TEST OPEN                                    
         BNE   FMT8E               YES - SKIP                                   
         BAS   RE,FMTPRD                                                        
FMT8E    LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   FMT8                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BE    FMT10X              EXIT                                         
         MVC   8(25,R2),=C'** BRANDS ALREADY OPEN **'                           
         FOUT  (R2)                                                             
*                                                                               
         ZAP   HALF,=P'13'                                                      
         LA    R4,36(R2)                                                        
         LA    R6,PRDTAB                                                        
*                                                                               
FMT10    CLI   3(R6),0                                                          
         BE    *+8                                                              
         BAS   RE,FMTPRD                                                        
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   FMT10                                                            
* TEST FOR INPUT TO SEND PROPER MESSAGE                                         
FMT10X   LA    R2,ESTPRD1H                                                      
FMT12    CLI   5(R2),0                                                          
         BNE   FMT14                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNE   FMT12                                                            
         LA    R2,ESTPRD1H                                                      
         MVC   HDRMSG(25),=C'** ENTER PRODUCT CODES **'                         
         B     FMTX                                                             
*                                                                               
FMT14    LA    R2,HDRRECH                                                       
         MVC   HDRMSG(22),=C'** ACTION COMPLETED **'                            
*                                                                               
FMTX     OI    6(R2),X'40'         INSERT CURSOR                                
         MVI   ERRAREA,X'FF'       TELL BASE WE SET MESSAGE                     
         B     EXIT                                                             
         EJECT                                                                  
FMTPRD   CP    HALF,=P'0'                                                       
         BNE   FMTPRD2                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BE    FMT10X              EXIT                                         
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'20'                                                      
*                                                                               
FMTPRD2  MVC   0(3,R4),0(R6)                                                    
         LA    R4,4(R4)                                                         
         SP    HALF,=P'1'                                                       
         FOUT  (R2)                                                             
         BR    RE                                                               
         EJECT                                                                  
DUPERR   EQU   52                  DUPLICATE KEY ON ADD                         
DELERR   EQU   56                  RECORD IS DELETED                            
NOFNDERR EQU   41                  PRODUCT NOT FOUND                            
POLERR   EQU   15                  INVALID PRODUCT                              
*                                                                               
OANSW    DC    X'00'               SET TO X'01' IF OAN PRD IN GETPRD            
SVESTTST DC    X'00'               USED TO SAVE PESTTEST                        
ZEROS    DC    40C'0'                                                           
SPACES   DC    CL40' '                                                          
SAVEKEY  DS    CL32                                                             
DMWORK1  DS    12D                                                              
VIRERR   DC    H'0'                                                             
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
*                                                                               
         ORG   HDRLAST                                                          
*                                                                               
       ++INCLUDE PPLFMF5D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPLFM05   05/01/02'                                      
         END                                                                    
