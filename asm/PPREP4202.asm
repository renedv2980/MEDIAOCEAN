*          DATA SET PPREP4202  AT LEVEL 026 AS OF 03/15/10                      
*PHASE PP4202A,+0                                                               
         TITLE 'PP4202  DIV-REG-DST LISTING'                                    
PP4202   CSECT                                                                  
         NMOD1 0,PP4202                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
*                                                                               
         MVI   RC2DSECT,C'Y'                                                    
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R5,RC                                                            
         A     R5,=F'4096'                                                      
         USING PPFILED,RC,R5                                                    
CKCLT    CLI   MODE,PROCCLI                                                     
         BNE   EXT                                                              
CKCLT1   MVC   PPGKEY,KEY                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*                                                                               
SWSET    LA    R4,DMRDHI                                                        
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+7(3),=3C'0'                                                  
         CLI   QDIV,C'0'           CHECK ONE DIV                                
         BL    RDDIV               NO                                           
         MVC   KEY+7(3),QDIV                                                    
*                                                                               
RDDIV    MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCBA,(DMINBTS,(R4)),PRTDIR,KEY,KEY,(0,0)                
         CLI   DMCBA+8,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SAVEKEY(7),KEY                                                   
         BE    RDDIV1                                                           
         B     PPGEXT                                                           
*                                                                               
RDDIV1   DS    0H                                                               
         CLI   QDIV,C'0'           CHECK ONE DIV                                
         BL    PRTDIV              NO                                           
         CLC   SAVEKEY(10),KEY                                                  
         BNE   PPGEXT                                                           
         B     PRTDIV                                                           
*                                                                               
PRTDIV   GOTO1 DATAMGR,DMCBA,(DMINBTS,GETREC),PRTFILE,KEY+27,PDIVREC,  X        
               (0,DMWORKA)                                                      
         CLI   DMCBA+8,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R7,LINE                                                          
         AH    R7,=H'4'                                                         
         STC   R7,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   PRTDIV1                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     PRTDIV2                                                          
PRTDIV1  BAS   R8,PRINTIT                                                       
PRTDIV2  MVC   P1+1(8),=C'DIVISION'                                             
         MVC   P1+11(3),PDIVKDIV                                                
         MVC   P1+16(L'PDIVNAME),PDIVNAME                                       
         CLI   PDIVREP,C' '      IS A BILLING REP PRESENT?                      
         BNH   PRTDIV5                                                          
         LA    R4,PDIVREP                                                       
         BAS   RE,PRNTREP        PRINT REP INFO                                 
*                                                                               
PRTDIV5  DS    0H                                                               
         MVC   P2+1(13),=13C'-'                                                 
         MVI   SPACING,2                                                        
         BAS   R8,PRINTIT                                                       
         LA    R4,DMRDHI                                                        
         MVI   KEY+3,X'04'                                                      
         MVC   KEY+10(3),=C'000'                                                
         XC    KEY+13(18),KEY+13                                                
         CLI   QREGION,C'A'                                                     
         BL    NEXTDIV             NO REGS                                      
         BE    RDREG               ALL REGS                                     
         MVC   KEY+10(3),QREGION                                                
         B     RDREG                                                            
*                                                                               
RDREG    MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCBA,(DMINBTS,(R4)),PRTDIR,KEY,KEY,(0,0)                
         CLI   DMCBA+8,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SAVEKEY(10),KEY                                                  
         BE    RDREG1                                                           
         B     NEXTDIV                                                          
*                                                                               
RDREG1   DS    0H                                                               
         CLI   QREGION,C'0'        CHECK ONE REG                                
         BL    PRTREG              NO                                           
         CLC   SAVEKEY(13),KEY                                                  
         BNE   PPGEXT                                                           
         B     PRTREG                                                           
*                                                                               
PRTREG   GOTO1 DATAMGR,DMCBA,(DMINBTS,GETREC),PRTFILE,KEY+27,PREGREC,  X        
               (0,DMWORKA)                                                      
         CLI   DMCBA+8,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R7,LINE                                                          
         AH    R7,=H'2'                                                         
         STC   R7,SAVELINE                                                      
         CLC   SAVELINE(1),MAXLINES                                             
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P1+4(6),=C'REGION'                                               
         MVC   P1+12(3),PREGKREG                                                
         MVC   P1+17(L'PREGNAME),PREGNAME                                       
         CLI   PREGREP,C' '      IS A BILLING REP PRESENT?                      
         BNH   PRTREG5                                                          
         LA    R4,PREGREP                                                       
         BAS   RE,PRNTREP        PRINT REP INFO                                 
*                                                                               
PRTREG5  DS    0H                                                               
         MVC   P2+4(11),=11C'-'                                                 
         BAS   R8,PRINTIT                                                       
         LA    R4,DMRDHI                                                        
         MVI   KEY+3,X'05'                                                      
         MVC   KEY+13(3),=C'000'                                                
         XC    KEY+16(15),KEY+16                                                
         CLI   QDIST,C'A'                                                       
         BL    NEXTREG             NO DSTS                                      
         BE    RDDST               ALL DTST                                     
         MVC   KEY+13(3),QDIST                                                  
         B     RDDST                                                            
*                                                                               
RDDST    MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCBA,(DMINBTS,(R4)),PRTDIR,KEY,KEY,(0,0)                
         CLI   DMCBA+8,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SAVEKEY(13),KEY                                                  
         BE    RDDST1                                                           
         B     NEXTREG                                                          
RDDST1   DS    0H                                                               
         CLI   QDIST,C'0'          CHECK ONE DST                                
         BL    PRTDST              NO                                           
         CLC   SAVEKEY(16),KEY                                                  
         BNE   PPGEXT                                                           
         B     PRTDST                                                           
*                                                                               
PRTDST   GOTO1 DATAMGR,DMCBA,(DMINBTS,GETREC),PRTFILE,KEY+27,PDSTREC,  X        
               (0,DMWORKA)                                                      
         CLI   DMCBA+8,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R7,LINE                                                          
         AH    R7,=H'1'                                                         
         STC   R7,SAVELINE                                                      
         CLC   SAVELINE(1),MAXLINES                                             
         BNH   PRTDST1                                                          
         MVI   FORCEHED,C'Y'                                                    
         CLI   QREGION,C' '                                                     
         BE    PRTDST1                                                          
         MVC   P1+4(6),=C'REGION'                                               
         MVC   P1+12(3),PREGKREG                                                
         MVC   P1+17(11),=C'(CONTINUED)'                                        
         BAS   R8,PRINTIT                                                       
         MVC   P1+4(11),=11C'-'                                                 
         BAS   R8,PRINTIT                                                       
*                                                                               
PRTDST1  MVC   P1+8(8),=C'DISTRICT'                                             
         MVC   P1+18(3),PDSTKDST                                                
         MVC   P1+23(L'PDSTNAME),PDSTNAME                                       
         BAS   R8,PRINTIT                                                       
         LA    R4,DMRSEQ                                                        
         CLI   QDIST,C'A'                                                       
         BE    RDDST                                                            
         B     PPGEXT                                                           
*                                                                               
*                                                                               
NEXTREG  DS    0H                                                               
         CLI   QREGION,C'A'                                                     
         BNE   PPGEXT                                                           
         MVC   KEY,PREGREC                                                      
         MVI   KEY+3,X'04'                                                      
         PACK  DUB,KEY+10(3)                                                    
         CP    DUB,=P'999'      LAST REGION                                     
         BE    NEXTDIV                                                          
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+10(3),DUB+6(2)                                               
         XC    KEY+13(18),KEY+13                                                
         LA    R4,DMRDHI                                                        
         B     RDREG                                                            
*                                                                               
NEXTDIV  DS    0H                                                               
         CLI   QDIV,C'A'                                                        
         BNE   PPGEXT                                                           
         MVC   KEY,PDIVREC                                                      
         MVI   KEY+3,X'03'                                                      
         PACK  DUB,KEY+7(3)                                                     
         CP    DUB,=P'999'       LAST DIVISION                                  
         BE    PPGEXT                                                           
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+7(3),DUB+6(2)                                                
         XC    KEY+10(21),KEY+10                                                
         LA    R4,DMRDHI                                                        
         B     RDDIV                                                            
*                                                                               
PRINTIT  DS    0H                                                               
*                                                                               
PRINTX   GOTO1 REPORT                                                           
         BR    R8                                                               
         EJECT                                                                  
PRNTREP  NTR1                                                                   
         MVC   P1+40(12),=C'BILLING REP='                                       
         MVC   P1+53(4),0(R4)                                                   
         MVC   MYSKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(3),MYSKEY       AGY/MED                                      
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),0(R4)      BILLING REP CODE                             
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    PRNTR5                                                           
         MVC   P2+53(19),=C'*** NOT ON FILE ***'                                
         B     PRNTRX                                                           
*                                                                               
PRNTR5   LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   P2+53(L'PREPNAME),PREPNAME                                       
         MVC   P3+53(L'PREPLIN1),PREPLIN1                                       
         MVC   P4+53(L'PREPLIN2),PREPLIN2                                       
         OC    PREPATTN,PREPATTN                                                
         BZ    PRNTRX                                                           
         MVC   P5+53(5),=C'ATTN:'                                               
         MVC   P5+59(L'PREPATTN),PREPATTN                                       
PRNTRX   MVC   KEY,MYSKEY                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DMWORKA  DS    12D                                                              
DMCBA    DS    6F                                                               
*                                                                               
SAVELINE DS    CL1                                                              
MYSKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
PPGKEY   DS    CL32                                                             
*                                                                               
PPGEXT   MVC   KEY,PPGKEY                                                       
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEY,KEY,(0,0)               
         CLI   DMCB+8,0                                                         
         BE    EXT                                                              
         DC    H'0'                                                             
EXT      XMOD1 1                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
****** ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026PPREP4202 03/15/10'                                      
         END                                                                    
