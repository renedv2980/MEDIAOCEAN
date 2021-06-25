*          DATA SET SPINF16    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T21A16A                                                                  
         TITLE 'T21A16 - MGROUP DISPLAY'                                        
         PRINT NOGEN                                                            
T21A16   CSECT                                                                  
         NMOD1 MODWORKX-MODWORK,T21A16                                          
         LR    R3,RC                                                            
         USING MODWORK,R3                                                       
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA(11),=C' PGRP  MGRP'                                      
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(11),=C'ID/NO ID/NO'                                      
         LA    R5,KEY                                                           
         USING MKGRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SVAGYMD                                                 
         MVC   MKGKCLT,SVCLT                                                    
         MVC   MKGKPID(3),SVKEY+5                                               
         MVC   MKGKMID,SVKEY+8                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         LA    R8,MKGEL                                                         
         USING MKGEL01,R8                                                       
         MVC   FLDDATA+12(7),=C'BREAK 1'                                        
         MVC   FLDDATA+34(7),=C'BREAK 2'                                        
         MVC   FLDDATA+56(7),=C'BREAK 3'                                        
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(5),DASH                                                  
         MVC   FLDDATA+6(5),DASH                                                
         MVC   FLDDATA+12(12),DASH                                              
         MVC   FLDDATA+34(12),DASH                                              
         MVC   FLDDATA+56(12),DASH                                              
         DROP  R5,R8                                                            
*                                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         MVI   MKTSW,0                                                          
         GOTO1 USER1,DUB,(64,SINIFLT),(5,=C'DATA=')                             
         OC    4(4,R1),4(R1)                                                    
         BZ    FLT2                                                             
         LA    RE,5                                                             
         MVC   WORK(5),=C'DATA'                                                 
         L     R4,4(R1)                                                         
         CLI   5(R4),C'M'                                                       
         BE    MKTFOK                                                           
         B     FLTERR                                                           
         SPACE 2                                                                
MKTFOK   MVI   MKTSW,1                                                          
FLT2     DS    0H                                                               
         LA    R5,KEY                                                           
         USING MKGRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SVAGYMD                                                 
         MVC   MKGKCLT,SVCLT                                                    
         MVC   MKGKPID(3),SVKEY+5                                               
         MVC   MKGKMID,SVKEY+8                                                  
         LA    R4,14                                                            
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY(13),PREVKEY                                                  
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
         B     HAVKEY                                                           
*                                                                               
MGSEQ    GOTO1 SEQ                                                              
*                                                                               
HAVKEY   LA    R5,KEY                                                           
         USING MKGRECD,R5                                                       
         CLC   SVAGYMD,MKGKAGMD                                                 
         BNE   MGEND                                                            
         CLC   MKGKTYP,=X'0D02'                                                 
         BNE   MGEND                                                            
         CLC   MKGKCLT,SVCLT                                                    
         BNE   MGSEQ                                                            
         OC    MKGKMGRP,MKGKMGRP                                                
         BZ    MGSEQ                                                            
         MVC   WORK,KEY                                                         
         OC    SVKEY+8(1),SVKEY+8                                               
         BZ    *+14                                                             
         CLC   MKGKMID,SVKEY+8                                                  
         BNE   MGSEQ                                                            
         OC    SVKEY+9(2),SVKEY+9                                               
         BZ    NOMGNUM                                                          
         MVC   FULL(2),SVKEY+9                                                  
         MVC   FULL+2(2),MKGKMGRP                                               
         BAS   RE,CHKGRP                                                        
         CLI   FULL,0                                                           
         BNE   MGSEQ                                                            
*                                                                               
NOMGNUM  OC    SVKEY+6(2),SVKEY+6                                               
         BZ    NOPGNUM                                                          
         MVC   FULL(2),SVKEY+6                                                  
         MVC   FULL+2(2),MKGKPGRP                                               
         BAS   RE,CHKGRP                                                        
         CLI   FULL,0                                                           
         BNE   MGSEQ                                                            
*                                                                               
NOPGNUM  DS    0H                                                               
         OC    SVKEY+5(3),SVKEY+5                                               
         BZ    *+14                                                             
         CLC   MKGKPID,SVKEY+5                                                  
         BNE   MGSEQ                                                            
* CHECK HAVE RIGHT MGRID RECORD                                                 
         BAS   RE,GETID                                                         
*                                                                               
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         LA    R8,MKGEL                                                         
         USING MKGEL01,R8                                                       
         USING MKGEL10,R8                                                       
         MVC   PSVID,MKGKPID       SAVE IDS AND NAMES                           
         UNPK  DUB,MKGKPGRP(3)                                                  
         MVC   PSVGRP,DUB+3                                                     
         MVC   MSVID,MKGKMID                                                    
         UNPK  DUB,MKGKMGRP(3)                                                  
         MVC   MSVGRP,DUB+3                                                     
         MVC   MSVN1,MKGNAM1                                                    
         MVC   MSVN2,MKGNAM2                                                    
         MVC   MSVN3,MKGNAM3                                                    
         CLI   MKTSW,0             MARKET DISPLAY                               
         BE    SEND                 NO - SEND IDS AND NAMES                     
         LA    RE,REC2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R7,0                                                             
         LA    R6,REC2                                                          
         MVC   WORK(13),KEY                                                     
         LA    R5,KEY                                                           
         OI    MKGPTYP+1,X'80'                                                  
         XC    MKGPMKT,MKGPMKT                                                  
GETMKT   GOTO1 HIGH                                                             
         B     HAVMKTK                                                          
GETMKTS  GOTO1 SEQ                                                              
HAVMKTK  CLC   KEY(2),=X'0D82'                                                  
         BNE   ENDMKT                                                           
         CLC   KEY+2(9),WORK+2                                                  
         BNE   ENDMKT                                                           
         MVC   HALF,MKGPMKT                                                     
         LH    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R6),DUB                                                      
         LA    R6,4(R6)                                                         
         LA    R7,1(R7)                                                         
         C     R7,=F'192'                                                       
         BH    ENDMKT                                                           
         B     GETMKTS                                                          
         SPACE 2                                                                
ENDMKT   MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         LR    RF,R7                                                            
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'17'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         LA    RF,2(RF)                                                         
         CR    RF,R4                                                            
         BH    EMARDISP                                                         
         MVC   PREVKEY,KEY                                                      
*                                                                               
SEND     DS    0H                                                               
         CLI   PSVID,0                                                          
         BE    *+16                                                             
         MVC   FLDDATA(1),PSVID                                                 
         MVC   FLDDATA+1(4),PSVGRP                                              
         MVC   FLDDATA+6(1),MSVID                                               
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MYMGRLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA+7(0),MSVGRP                                              
*                                                                               
         MVC   FLDDATA+12(21),MSVN1                                             
         MVC   FLDDATA+34(21),MSVN2                                             
         MVC   FLDDATA+56(21),MSVN3                                             
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCTR  R4,0                                                             
         CLI   MKTSW,0                                                          
         BE    SENDEND                                                          
         LA    R6,REC2                                                          
         LA    R7,FLDDATA+14                                                    
         MVC   FLDDATA+6(8),=C'MARKETS='                                        
         LA    R9,13                                                            
SENDMKT  CLI   0(R6),0             END OF MARKETS                               
         BE    SENDEND              YES                                         
SENDMKT1 MVC   0(4,R7),0(R6)                                                    
         LA    R7,5(R7)                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BE    SENDMKT2                                                         
         BCT   R9,SENDMKT1                                                      
SENDMKT2 FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCTR  R4,0                                                             
         LA    R7,FLDDATA+4                                                     
         LA    R9,15                                                            
         B     SENDMKT                                                          
         SPACE 2                                                                
CHKGRP   UNPK  DUB,FULL(3)                                                      
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
EMARDISP MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
         EJECT                                                                  
SENDEND  MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         LTR   R4,R4                                                            
         BNZ   MGSEQ                                                            
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   MGEND                                                            
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
MGEND    XC    PREVKEY,PREVKEY                                                  
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT2 OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* SEND FILTER ERROR MESSAGE                                                     
*                                                                               
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)     * EXECUTED *                                 
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
         EJECT                                                                  
*==============================================================*                
* GET SUM OF BREAK LENGTHS FOR CURRENT MGRID                   *                
* NOTE THAT MGRD REC HAS CLIENT 0000 AND PGRID 0000            *                
* EXCEPT FOR MGRPS A-F                                                          
*==============================================================*                
         SPACE 1                                                                
GETID    NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT KEY                             
         LA    R5,KEY                                                           
         USING MKGRECD,R5                                                       
*                                                                               
         CLC   MYMGRID,MKGKMID     TEST HAVE RIGHT MGRID                        
         BE    EXIT                                                             
*                                                                               
         MVC   MYMGRID,MKGKMID       SAVE GROUP ID                              
         MVC   MYCLT,MKGKCLT         SAVE CLIENT CODE                           
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SVAGYMD                                                 
         MVC   MKGKMID(1),MYMGRID                                               
         CLI   MKGKMID,C'F'                                                     
         BH    *+10                                                             
         MVC   MKGKCLT,MYCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AREC                                                          
         LA    R8,MKGEL                                                         
         USING MKGEL01,R8                                                       
         SR    R1,R1                                                            
         IC    R1,MKGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,MKGBK2LN                                                      
         AR    R1,R0                                                            
         IC    R0,MKGBK3LN                                                      
         AR    R1,R0                                                            
         STC   R1,MYMGRLN                                                       
* RESTORE CURRENT KEY VALUE                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(13),MYKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
EXIT     XIT1                                                                   
         DROP  R5,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
MODWORK  DSECT                                                                  
MKTSW    DS    CL1                 MARKET DISPLAY SWITCH                        
PSVID    DS    CL1                                                              
PSVGRP   DS    CL4                                                              
MSVID    DS    CL1                                                              
MSVGRP   DS    CL4                                                              
MSVN1    DS    CL21                                                             
MSVN2    DS    CL21                                                             
MSVN3    DS    CL21                                                             
FLTR     DS    CL4                                                              
CURR     DS    CL4                                                              
MYKEY    DS    XL13                                                             
MYMGRID  DS    XL1                                                              
MYMGRLN  DS    XL1                                                              
MYCLT    DS    XL2                                                              
MODWORKX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
       ++INCLUDE SPGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPINF16   05/01/02'                                      
         END                                                                    
