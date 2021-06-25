*          DATA SET SPLFM1C    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T2191CA                                                                  
         TITLE 'SPLFM1C - ESTHDR COPY   T2191C'                                 
         PRINT NOGEN                                                            
T2191C   CSECT                                                                  
NOPOLCPY EQU   250                 MAY NOT ADD POL EST WITH ACTN COPY           
         NMOD1 0,T2191C                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING ESTHDRD,R8                                                       
*                                                                               
         LR    RE,RC                                                            
         A     RE,=A(DEMAREA-GENOLD)                                            
         ST    RE,ADRCLT                                                        
         USING CLTHDRD,RE                                                       
         LA    R6,CLIST                                                         
         ST    R6,ADRCLIST                                                      
         DROP  RE                                                               
*                                                                               
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
LFMERR   GOTO1 ERROR               (DOES NOT RETURN HERE)                       
         EJECT                                                                  
FMT      MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,ESTKEX2H                                                      
         MVC   8(20,R2),EDESC                                                   
         GOTO1 VDATCON,DMCB,ESTART,(5,WORK)                                     
         GOTO1 (RF),(R1),EEND,(5,WORK+9)                                        
         MVI   WORK+8,C'-'                                                      
         MVC   30(17,R2),WORK                                                   
         FOUT  (R2)                                                             
*                                                                               
         MVI   ERRCD,MSTREST                                                    
         CLI   EMSTRIND,0                                                       
         BNE   LFMERR                                                           
*                                                                               
FMT2     LA    R2,ESTDSPH          CLEAR PROTECTED FIELDS                       
FMT4A    ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'            SET FOR EX                                   
         EX    RE,FMTOC                                                         
         BE    FMT4B                                                            
         EX    RE,FMTXC                                                         
         FOUT  (R2)                                                             
FMT4B    LA    R2,9(RE,R2)                                                      
         LA    R0,ESTLSTH                                                       
         CR    R2,R0                                                            
         BNH   FMT4A                                                            
         B     FMT4X                                                            
FMTOC    OC    8(0,R2),8(R2)                                                    
FMTXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
* BUILD LIST OF BRANDS THAT ARE OPEN                                            
*                                                                               
FMT4X    XC    KEY,KEY                                                          
         MVC   KEY+14(4),SVCLTDA   READ CLTHDR                                  
         MVC   AREC,ADRCLT                                                      
         GOTO1 GETREC                                                           
         ST    R8,AREC             RESOTRE I/O ADDRESS                          
*                                                                               
         L     R6,ADRCLIST                                                      
*                                                                               
FMT6     MVC   KEY,SVKEY                                                        
         MVC   KEY+4(3),0(R6)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
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
         ZAP   HALF,=P'0'                                                       
         L     R6,ADRCLIST                                                      
FMT8     CLI   3(R6),0             TEST OPEN                                    
         BNE   FMT8E               YES - SKIP                                   
         MVC   KEY,SVKEY           TRY TO READ PRODUCT                          
         MVC   KEY+4(3),0(R6)                                                   
         XC    KEY+7(6),KEY+7                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FMT8E                   PRD NOT ON FILE - SKIP                   
*                                                                               
FMT8B    DS     0H                                                              
         BAS   RE,FMTPRD                                                        
FMT8E    LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   FMT8                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(25,R2),=C'** BRANDS ALREADY OPEN **'                           
         FOUT  (R2)                                                             
*                                                                               
         ZAP   HALF,=P'0'                                                       
         L     R6,ADRCLIST                                                      
*                                                                               
FMT10    CLI   3(R6),0                                                          
         BE    *+8                                                              
         BAS   RE,FMTPRD                                                        
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   FMT10                                                            
* TEST FOR INPUT TO SEND PROPER MESSAGE                                         
         LA    R2,ESTPRD1H                                                      
FMT12    CLI   5(R2),0                                                          
         BNE   FMT14                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST END OF PRDS                             
         BNE   FMT12                                                            
         LA    R2,ESTPRD1H                                                      
         MVC   LFMMSG(25),=C'** ENTER PRODUCT CODES **'                         
         B     FMTX                                                             
*                                                                               
FMT14    LA    R2,LFMRECH          POSITION CURSOR                              
         MVC   LFMMSG(22),=C'** ACTION COMPLETED **'                            
*                                                                               
FMTX     OI    6(R2),X'40'         INSERT CURSOR                                
         MVI   ERRAREA,X'FF'       TELL BASE WE SET MESSAGE                     
         B     EXIT                                                             
         EJECT                                                                  
FMTPRD   CP    HALF,=P'0'                                                       
         BNE   FMTPRD2                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,ESTLSTH                                                       
         CR    R2,R0               TEST PAST EOS                                
         BHR   RE                  YES - RETURN                                 
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'20'                                                      
*                                                                               
FMTPRD2  MVC   0(3,R4),0(R6)                                                    
         LA    R4,4(R4)                                                         
         SP    HALF,=P'1'                                                       
         FOUT  (R2)                                                             
         BR    RE                                                               
         EJECT                                                                  
EDT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERRCD,MSTREST                                                    
         CLI   EMSTRIND,0                                                       
         BNE   LFMERR                                                           
*                                                                               
* MAKE SURE POL ESTIMATE HAS CORRECT FILTERS VS F0 PROFILE                      
*                                                                               
         CLI   SVF0PROF+2,C'Y'                                                  
         BNE   EDT1                                                             
         CLI   EPROF,C' '                                                       
         BNH   F0ERR                                                            
*                                                                               
EDT1     CLI   SVF0PROF+3,C'Y'                                                  
         BNE   EDT1A                                                            
         CLI   EPROF+1,C' '                                                     
         BNH   F0ERR                                                            
*                                                                               
EDT1A    CLI   SVF0PROF+4,C'Y'                                                  
         BNE   EDT1B                                                            
         CLI   EPROF+2,C' '                                                     
         BNH   F0ERR                                                            
*                                                                               
EDT1B    DS    0H                                                               
* VALIDATE BRAND CODES - MAKE SURE EST NOT ON FILE                              
         MVC   AREC,ADRCLT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),SVCLTDA                                                
         GOTO1 GETREC                                                           
         ST    R8,AREC             RESTORE I/O ADDRESS                          
*                                                                               
         LA    R2,ESTPRD1H                                                      
*                                                                               
EDT2     CLI   5(R2),0                                                          
         BE    EDT4                                                             
         GOTO1 MOVE                                                             
         BAS   RE,GETPRD                                                        
         MVI   ERRCD,NOPOLCPY                                                   
         CLC   =C'POL',WORK                                                     
         BE    LFMERR                                                           
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOTRDCPY)                                            
         CLI   WORK+2,C'#'         TEST TRADE PRODUCT                           
         BE    LFMERR                                                           
* CHECK EST ON FILE                                                             
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(3),0(R6)                                                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDT4                                                             
         MVI   ERRCD,DUPERR                                                     
         TM    KEY+13,X'80'                                                     
         BZ    LFMERR                                                           
         MVI   ERRCD,DELERR                                                     
         B     LFMERR                                                           
*                                                                               
EDT4     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST EOL                                     
         BNE   EDT2                                                             
         EJECT                                                                  
* OK TO ADD ESTIMATES                                                           
         LA    R2,ESTPRD1H                                                      
EDT10    CLI   5(R2),0                                                          
         BE    EDT12                                                            
         GOTO1 MOVE                                                             
         BAS   RE,GETPRD                                                        
         MVC   EKEY+4(3),0(R6)                                                  
         MVC   EPRDCD+1(1),3(R6)                                                
         XC    ECURPDN,ECURPDN                                                  
         XC    EAUTHN,EAUTHN                                                    
         XC    EORDN,EORDN                                                      
         XC    EPAIDN,EPAIDN                                                    
         XC    ETYPE,ETYPE                                                      
         MVC   KEY(13),EKEY                                                     
         GOTO1 ADDREC                                                           
*                                                                               
         GOTO1 CNADDSPT                                                         
*                                                                               
REQREC   XC    REC2(150),REC2                                                   
         LA    R1,REC2                                                          
         MVI   10(R1),132             L2 REQ                                    
         MVI   14(R1),106                                                       
         LA    R1,REC2+26                                                       
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L2'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   11(3,R1),EKEY+4         PRODUCT                                  
         SR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVI   61(R1),C'N'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   65(R1),C'A'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC2,REC2                    
*                                                                               
EDT12    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST EOL                                     
         BNE   EDT10                                                            
         B     FMT2                GO DISPLAY PRD LISTS                         
         SPACE 2                                                                
GETPRD   L     R6,ADRCLIST                                                      
GETP1    CLC   0(3,R6),WORK                                                     
         BE    GETP5                                                            
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   GETP1                                                            
GETPERR  MVI   ERRCD,NOPRDERR                                                   
         B     LFMERR                                                           
*                                                                               
GETP5    DS    0H                                                               
         ST    RE,FULL             SAVE RE FOR RETURN                           
         MVC   KEY,SVKEY           TRY TO READ PRODUCT                          
         MVC   KEY+4(3),0(R6)                                                   
         XC    KEY+7(6),KEY+7                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPERR                 PRD NOT FOUND                            
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
F0ERR    MVC   LFMMSG(29),=C'FILTERS DONT MATCH F0 PROFILE'                     
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         LA    R2,LFMKEYH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
         LTORG                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMFCD                                                       
         SPACE 2                                                                
GENOLD   DSECT                                                                  
*                                                                               
         ORG   ELEM                                                             
ADRCLT   DS    A                   A(CLTHDR)                                    
ADRCLIST DS    A                   A(CLIST)                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
* SPGENCLT                                                                      
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
* SPGENEST                                                                      
       ++INCLUDE SPGENEST                                                       
 END                                                                            
