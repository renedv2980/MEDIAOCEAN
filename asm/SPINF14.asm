*          DATA SET SPINF14    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T21A14A,+0,NOAUTO                                                        
*INCLUDE CLUNPK                                                                 
         TITLE 'T21A14 - PGROUP DISPLAY'                                        
T21A14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 30,T21A14                                                        
         LR    R3,RC                                                            
         USING MODWORK,R3                                                       
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA(5),=C'GROUP'                                             
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(5),=C'ID/NO'                                             
         LA    R5,KEY                                                           
         USING PRGRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SVAGYMD                                                 
         MVC   PRGKCLT,SVCLT                                                    
         MVC   PRGKID,SVEST                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         LA    R8,PRGEL                                                         
         USING PRGEL01,R8                                                       
         MVC   FLDDATA+6(12),PRGBK1                                             
         MVC   FLDDATA+31(12),PRGBK2                                            
         MVC   FLDDATA+56(12),PRGBK3                                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(5),DASH                                                  
         MVC   FLDDATA+6(12),DASH                                               
         XC    PLFORMAT(64),PLFORMAT                                            
         CLI   PRGBK2,0                                                         
         BE    *+10                                                             
         MVC   FLDDATA+31(12),DASH                                              
         CLI   PRGBK3,0                                                         
         BE    *+10                                                             
         MVC   FLDDATA+56(12),DASH                                              
         DROP  R8                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVI   PRODSW,0                                                         
         GOTO1 USER1,DUB,(64,SINIFLT),(5,=C'DATA=')                             
         OC    4(4,R1),4(R1)                                                    
         BZ    FLT2                                                             
         LA    RE,5                                                             
         MVC   WORK(5),=C'DATA='                                                
         L     R4,4(R1)                                                         
         CLC   5(4,R4),=C'PROD'                                                 
         BE    PRDFOK                                                           
         CLC   5(3,R4),=C'PRD'                                                  
         BE    PRDFOK                                                           
         B     FLTERR                                                           
PRDFOK   MVI   PRODSW,1                                                         
FLT2     DS    0H                                                               
         LA    R5,KEY                                                           
         USING PRGRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SVAGYMD                                                 
         MVC   PRGKCLT,SVCLT                                                    
         LA    R4,14                                                            
         MVC   PRGKID,SVEST                                                     
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY(13),PREVKEY                                                  
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
         B     HAVKEY                                                           
PGSEQ    GOTO1 SEQ                                                              
HAVKEY   LA    R5,KEY                                                           
         CLC   SVAGYMD,PRGKAGMD                                                 
         BNE   PGEND                                                            
         CLC   PRGKTYP,=X'0D01'                                                 
         BNE   PGEND                                                            
         CLC   SVCLT,PRGKCLT                                                    
         BNE   PGEND                                                            
         OC    PRGKGRP,PRGKGRP                                                  
         BZ    PGSEQ                                                            
         MVC   WORK,KEY                                                         
         OC    SVEST(3),SVEST                                                   
         BZ    *+14                                                             
         CLC   PRGKID,SVEST                                                     
         BNE   PGSEQ                                                            
         OC    SVEST+1(2),SVEST+1                                               
         BZ    HAVREC                                                           
         MVC   FULL(2),SVEST+1                                                  
         MVC   FULL+2(2),PRGKGRP                                                
         BAS   RE,CHKGRP                                                        
         CLI   FULL,0                                                           
         BNE   PGSEQ                                                            
HAVREC   GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         LA    R8,PRGEL                                                         
         USING PRGEL10,R8                                                       
         MVC   PSVID,PRGKID        SAVE GROUP ID AND NAMES                      
         UNPK  DUB,PRGKGRP(3)                                                   
         MVC   PSVGRP,DUB+3                                                     
         MVC   PSVN1,PRGNAM1                                                    
         MVC   PSVN2,PRGNAM2                                                    
         MVC   PSVN3,PRGNAM3                                                    
         CLI   PRODSW,0            PRODUCT DISPLAY                              
         BE    SEND                 NO - SEND NAMES                             
         LA    RE,REC2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R7,0                                                             
         LA    R6,REC2                                                          
         MVC   WORK(13),KEY        GET PRODUCTS                                 
         LA    R5,KEY                                                           
         OI    PRGPTYP+1,X'80'                                                  
         XC    PRGPPRD,PRGPPRD                                                  
GETPRD   GOTO1 HIGH                                                             
         B     HAVPRDK                                                          
GETPRDS  GOTO1 SEQ                                                              
HAVPRDK  CLC   KEY+2(6),WORK+2                                                  
         BNE   ENDPRD                                                           
         MVC   0(3,R6),PRGPPRD                                                  
         LA    R6,3(R6)                                                         
         LA    R7,1(R7)                                                         
         B     GETPRDS                                                          
ENDPRD   MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         LR    RF,R7                                                            
         CH    RF,=H'180'                                                       
         BL    *+8                                                              
         LA    R4,1(R4)                                                         
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'15'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         LA    RF,1(RF)                                                         
         CR    RF,R4                                                            
         BNL   PROEND                                                           
         MVC   PREVKEY,KEY                                                      
SEND     DS    0H                                                               
         MVC   FLDDATA(1),PSVID                                                 
         MVC   FLDDATA+2(3),PSVGRP                                              
         MVI   FLDDATA+1,C'/'                                                   
         MVC   FLDDATA+6(24),PSVN1                                              
         MVC   FLDDATA+31(24),PSVN2                                             
         MVC   FLDDATA+56(23),PSVN3                                             
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCTR  R4,0                                                             
         CLI   PRODSW,0                                                         
         BE    SENDEND                                                          
         LA    R6,REC2                                                          
         LA    R7,FLDDATA+18                                                    
         MVC   FLDDATA+8(9),=C'PRODUCTS='                                       
SENDPRD  CLI   0(R6),0             END OF PRODUCTS                              
         BE    SENDEND              YES                                         
         LA    R9,15                                                            
SENDPRD1 MVC   0(3,R7),0(R6)                                                    
         LA    R7,4(R7)                                                         
         LA    R6,3(R6)                                                         
         CLI   0(R6),0                                                          
         BE    SENDPRD2                                                         
         BCT   R9,SENDPRD1                                                      
SENDPRD2 FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCTR  R4,0                                                             
         LA    R7,FLDDATA+18                                                    
         B     SENDPRD                                                          
         EJECT                                                                  
PROEND   SR    R4,R4                                                            
         B     MODEXIT                                                          
*                                                                               
SENDEND  MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         LTR   R4,R4                                                            
         BNZ   PGSEQ                                                            
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BNE   PGEND                                                            
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
PGEND    XC    PREVKEY,PREVKEY                                                  
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT2 OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
CHKGRP   UNPK  DUB,FULL(3)                                                      
         EJECT                                                                  
         MVC   FLTR,DUB+3                                                       
         UNPK  DUB,FULL+2(3)                                                    
         MVC   CURR,DUB+3                                                       
         LA    R1,FLTR+3                                                        
         LA    R9,CURR+3                                                        
         LA    R0,4                                                             
CHKGRP1  CLI   0(R1),C'0'                                                       
         BNE   CHKGRP2                                                          
         MVI   0(R9),C'0'                                                       
         BCTR  R1,0                                                             
         BCTR  R9,0                                                             
         BCT   R0,CHKGRP1                                                       
CHKGRP2  MVI   FULL,0                                                           
         CLC   FLTR,CURR                                                        
         BER   RE                                                               
         MVI   FULL,1                                                           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SEND FILTER ERROR MESSAGE                                                     
*                                                                               
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)     *EXECUTED*                                   
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'INVALID FILTER FIELD'                                
         LA    R2,SINIFLTH                                                      
         FOUT  (R2)                                                             
         MVI   ERRCD,X'FF'                                                      
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT2                                                         
LINLEN   EQU   88                                                               
DASH     DC    40C'-'                                                           
         LTORG                                                                  
         EJECT                                                                  
MODWORK  DSECT                                                                  
PRODSW   DS    CL1                 PRODUCT DISPLAY SWITCH                       
PSVID    DS    CL1                 SAVE GROUP ID                                
PSVGRP   DS    CL3                 SAVE GROUP NUMBER                            
PSVN1    DS    CL24                SAVE NAME 1                                  
PSVN2    DS    CL24                SAVE NAME 2                                  
PSVN3    DS    CL24                SAVE NAME 3                                  
PLFORMAT DS    16F                 PRODUCT LIST AREA                            
CURR     DS    CL4                                                              
FLTR     DS    CL4                                                              
         EJECT                                                                  
*SPINFWORK                                                                      
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
*SPGENPRG                                                                       
       ++INCLUDE SPGENPRG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPINF14   05/01/02'                                      
         END                                                                    
