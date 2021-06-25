*          DATA SET SPINF43    AT LEVEL 001 AS OF 08/06/97                      
*PHASE T21A43A,+0,NOAUTO                                                        
         TITLE 'T21A43 - SPOTPAK INFO NETWORK DEFINITION'                       
*                                                                               
*        SVKEY HAS FOLLOWING UPON ENTRY                                         
*              0-1  X'0D11'                                                     
*              2-3  ALPHA AGENCY                                                
*              4-7  NETWORK /ALL=ALL NETWORKS                                   
*              8-11 CLIENT / 0=DEFAULTS / ALL=ALL CLIENTS                       
*                12 REGION / 0 = ALL                                            
*                                                                               
T21A43   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 60,T21A43                                                        
         USING FLDHDRD,R2                                                       
         LR    R8,RC                                                            
         USING NTWRKD,R8                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    RE,REC2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R7,REC2                                                          
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING NDEFRECD,R5                                                      
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,SVKEY+2                                                 
         CLC   SVKEY+4(3),=C'ALL'                                               
         BE    *+10                                                             
         MVC   NDEFKNET,SVKEY+4                                                 
         OC    PREVKEY,PREVKEY                                                  
         BNZ   *+10                                                             
         XC    SVSTA,SVSTA                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY(13),PREVKEY                                                  
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
         LA    R2,SINHDRH                                                       
         LA    RE,FLDDATA               HL1                                     
         LA    RF,LINLEN(RE)            HL2                                     
         LA    R9,LINLEN(RF)            HL3                                     
         MVC   ELCNT,=F'28'                                                     
         MVC   0(4,RF),=C'NWRK'                                                 
         MVC   0(4,R9),DASH                                                     
         MVC   5(3,RF),=C'CLT'                                                  
         MVC   5(3,R9),DASH                                                     
         MVC   9(7,RF),=C'STATION'                                              
         MVC   9(7,R9),DASH                                                     
         MVC   18(4,RE),=C'COST'                                                
         MVC   18(4,RF),=C'PCNT'                                                
         MVC   18(4,R9),DASH                                                    
         MVC   25(4,RE),=C'TIME'                                                
         MVC   24(6,RF),=C'OFFSET'                                              
         MVC   24(6,R9),DASH                                                    
         MVC   32(6,RF),=C'REGION'                                              
         MVC   32(6,R9),DASH                                                    
         MVC   40(39,RE),0(RE)                                                  
         MVC   40(39,RF),0(RF)                                                  
         MVC   40(39,R9),0(R9)                                                  
         LA    R3,3                                                             
         LA    R4,30                                                            
         LA    R6,REC2                                                          
SENDTIT  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCT   R3,SENDTIT                                                       
         B     HAVREC                                                           
NDSEQ    GOTO1 SEQ                                                              
HAVREC   CLC   KEY(4),KEYSAVE                                                   
         BNE   NDEND1                                                           
         LA    R5,KEY                                                           
         CLI   SVKEY+4,0                                                        
         BE    NDPROC2                                                          
         CLC   SVKEY+4(3),=C'ALL'       FILTER NETWORK                          
         BE    NDPROC2                                                          
         CLC   NDEFKNET,SVKEY+4                                                 
         BNE   NDSEQ                                                            
         SPACE 2                                                                
NDPROC2  CLC   SVKEY+8(3),=C'ALL'       FILTER CLIENT                           
         BE    NDPROC3                                                          
         CLI   SVKEY+8,0                                                        
         BE    NDPROC3                                                          
         MVC   SVEBCCLT,SPACES                                                  
         LA    RE,3                                                             
         CLI   SVKEY+10,0                                                       
         BNE   *+8                                                              
         LA    RE,2                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVEBCCLT(0),SVKEY+8                                              
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),SVEBCCLT,SVCLT                                         
         CLC   NDEFKCLT,SVCLT                                                   
         BNE   NDSEQ                                                            
         SPACE 2                                                                
NDPROC3  GOTO1 GETREC                                                           
         MVC   NCLTHLD,NDEFKCLT                                                 
         MVC   NNETHLD,NDEFKNET                                                 
         L     R5,AREC                                                          
         LA    R7,NDEFEL                                                        
         USING NDEFEL01,R7                                                      
NDPROC3A CLI   0(R7),1                                                          
         BNE   NDNXTEL                                                          
         CLI   SVKEY+11,0          FILTER REGIONS                               
         BE    NDSVEL                                                           
         LA    RE,NDEFRGN                                                       
         LA    RF,4                                                             
         CLI   SVKEY+11,C'0'       ALPHA REGION FILTER                          
         BL    NDPROC5                                                          
NDPROC4  CLI   0(RE),C'0'          NUMERIC FILTER                               
         BL    NDPROC4A                                                         
         CLC   0(1,RE),SVKEY+11                                                 
         BE    NDSVEL                                                           
NDPROC4A LA    RE,1(RE)                                                         
         BCT   RF,NDPROC4                                                       
NDNXTEL  ZIC   RF,1(R7)                                                         
         AR    R7,RF                                                            
         CLI   0(R7),0                                                          
         BNE   NDPROC3A                                                         
         B     NDSEQ                                                            
NDPROC5  CLI   0(RE),C'A'          ALPHA REGIONS                                
         BL    *+14                                                             
         CLC   0(1,RE),SVKEY+11    ALPHA REGIONS                                
         BNH   NDSVEL                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,NDPROC5                                                       
         B     NDNXTEL                                                          
         SPACE 2                                                                
*        SAVE ELEMENT                                                           
NDSVEL   OC    SVSTA,SVSTA         PREVIOUS PROCCESSING                         
         BZ    NDSVEL2                                                          
         CLC   SVSTA(4),NDEFSTA     YES - BYPASS PROCESSED STATIONS             
         BNE   NDNXTEL                                                          
         XC    SVSTA,SVSTA                                                      
         B     NDNXTEL                                                          
NDSVEL2  MVC   0(16,R6),NDEFEL01                                                
         MVC   16(6,R6),NNETHLD                                                 
         MVC   WORK(5),NDEFSTA                                                  
         LA    R6,22(R6)                                                        
         L     RE,ELCNT                                                         
         SH    RE,=H'1'                                                         
         ST    RE,ELCNT                                                         
         MVC   PREVKEY,KEY                                                      
         LTR   RE,RE                                                            
         BZ    NDSEND                                                           
         B     NDNXTEL                                                          
         EJECT                                                                  
NDSEND   CLI   REC2,0              ANY DATA                                     
         BNE   FORMAT                                                           
         MVI   ERRCD,NOFNDERR       NO - SEND MESSAGE                           
         GOTO1 ERROR                                                            
         B     MODEXIT                                                          
NDEND1   XC    PREVKEY,PREVKEY                                                  
         B     NDSEND                                                           
*                                                                               
FORMAT   MVC   SVSTA,NDEFSTA                                                    
         CLI   SVKEY+11,0                                                       
         BE    FRMTGO                                                           
         OC    ELCNT,ELCNT         TABLE FULL                                   
         BNZ   PCNT3                NO - ALL STATIONS ARE THERE                 
         LA    RE,REC2                                                          
         MVC   WORK(6),16(RE)      SAVE FIRST KEY                               
         XC    FULL,FULL           SAVE FIRST KEY ADDRESS                       
PCNT1    CLI   22(RE),0                                                         
         BE    PCNT2                                                            
         LA    RE,22(RE)                                                        
         CLC   WORK+16(6),16(RE)   SAME CLIENT/NETWORK                          
         BE    PCNT1                                                            
         MVC   WORK(6),16(RE)       NO - SAVE CLIENT/NETWORK                    
         ST    RE,FULL                                                          
         B     PCNT1                                                            
         SPACE 2                                                                
PCNT2    OC    FULL,FULL                                                        
         BZ    PCNT3                                                            
         L     RE,FULL                                                          
         XC    SVSTA,SVSTA         RESET TO PROCESS ENTIRE NETWORK              
         XC    0(22,RE),0(RE)      CLEAR LAST NETWORK/CLIENT                    
         SPACE 2                                                                
* ADJUST REGIONAL PERCENTAGES                                                   
PCNT3    LA    RE,REC2                                                          
PCNT4    SR    R1,R1                                                            
         MVC   WORK(6),16(RE)                                                   
         ST    RE,FULL                                                          
PCNT5    MVC   DUB(4),6(RE)        TOTAL PERCENTAGES                            
         A     R1,DUB                                                           
         LA    RE,22(RE)                                                        
         CLC   WORK(6),16(RE)                                                   
         BE    PCNT5                                                            
         ST    RE,DUB+4            SAVE END ADDRESS                             
         SPACE 2                                                                
         L     RE,FULL             GET START OF REGION                          
PCNT6    CLI   0(RE),0                                                          
         BE    FRMTGO                                                           
         C     RE,DUB+4                                                         
         BE    PCNT4                                                            
         SR    R9,R9                                                            
         SR    R8,R8                                                            
         MVC   DUB(4),6(RE)                                                     
         L     R9,DUB                                                           
         M     R8,=F'100000'        ADJUST REGIONAL PERCENT                     
         LTR   R9,R9                                                            
         BZ    PCNT7                                                            
         SLDA  R8,1                                                             
         DR    R8,R1                                                            
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         ST    R9,DUB                                                           
         MVC   6(4,RE),DUB                                                      
PCNT7    LA    RE,22(RE)                                                        
         B     PCNT6                                                            
         EJECT                                                                  
FRMTGO   XC    DMWORK(20),DMWORK                                                
         LA    R6,REC2                                                          
         LA    R5,0                                                             
FORMAT1  CLI   0(R6),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2                                                          
         LA    R6,22(R6)                                                        
         LA    R5,1(R5)                                                         
         B     FORMAT1                                                          
*                                                                               
FORMAT2  GOTO1 USER2,DMCB,(22,REC2),(R5),14,(2,DMWORK)                          
FORMAT3  LA    R6,DMWORK                                                        
         LA    R4,FLDDATA+1                                                     
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(4,R4),16(R7)                                                   
         MVC   HALF,20(R7)         CLIENT                                       
         OC    HALF,HALF                                                        
         BZ    FORMAT5                                                          
         MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         LA    R8,5(R4)                                                         
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),HALF,(R8)                                              
FORMAT5  DS    0H                                                               
         MVC   9(4,R4),NDEFSTA                                                  
         MVC   FULL,NDEFPCT                                                     
         L     RF,FULL                                                          
         LPR   RF,RF                                                            
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,17(R4)),2                                                
         EDIT  NDEFOSET,(4,25(R4))                                              
         MVC   33(4,R4),NDEFRGN                                                 
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,22(R5)           BUMP TO NEXT ENTRY                           
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)            DECREMENT COLUMN COUNT                       
         LA    R6,4(R6)                                                         
         LA    R4,40(R4)                                                        
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FRMTEND  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT  OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         LTORG                                                                  
LINLEN   EQU   88                                                               
DASH     DC    40C'-'                                                           
         SPACE 2                                                                
NTWRKD   DSECT                                                                  
ELCNT    DS    F                                                                
NNETHLD  DS    CL4                                                              
NCLTHLD  DS    CL2                                                              
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPINFWORK                                                      
