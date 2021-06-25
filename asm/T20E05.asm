*          DATA SET T20E05     AT LEVEL 036 AS OF 05/01/02                      
*PHASE T20E05C,+0,NOAUTO                                                        
*INCLUDE XSORT                                                                  
         PRINT NOGEN                                                            
         TITLE 'LIST MARKETS ON BOOK'                                           
T20E05   CSECT                                                                  
         NMOD1 1000,T20E05,RR=R8                                                
         LR    R4,RC                                                            
         USING MKTALPH,R4                                                       
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    R8,RELO                                                          
         B     ST                                                               
RELO     DC    A(0)                                                             
ST       L     RC,0(R1)                                                         
         L     RF,=V(XSORT)                                                     
         A     RF,RELO                                                          
         ST    RF,XSORT                                                         
         ST    R4,MAPOINT                                                       
         XC    MACOUNT,MACOUNT                                                  
         XC    DATALIN,DATALIN                                                  
         LA    R2,DMCHDC0H                                                      
         MVC   FLDDATA(6),=C'MARKET'                                            
*        MVC   FLDDATA+35(4),=C'MRKT'                                           
         CLI   FNO,1                                                            
         BNE   MSH1                                                             
         MVC   FLDDATA+40(39),FLDDATA                                           
MSH1     FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(18),=C'NUMBER MARKET NAME'                               
*        MVC   FLDDATA+35(4),=C'TYPE'                                           
         MVC   FLDDATA+40(8),=C'STATIONS'                                       
         CLI   FNO,1                                                            
         BNE   MSH2                                                             
         MVC   FLDDATA+40(39),FLDDATA                                           
MSH2     FOUT  (R2)                                                             
         LA    R2,88(R2)                                                        
         MVC   FLDDATA(35),=C'------ --------------------------- ----'          
         MVC   FLDDATA+40(8),=C'--------'                                       
         CLI   FNO,1                                                            
         BNE   MSH3                                                             
         MVC   FLDDATA+40(39),FLDDATA                                           
MSH3     FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    RE,WORKEND                                                       
         LH    RF,=H'1600'                                                      
         XCEF                                                                   
         XC    KEY,KEY                                                          
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DMKEY,R3                                                         
         MVI   DMKEY,C'A'                                                       
         MVC   DMMEDIA,CMEDIA                                                   
         MVI   DMSRC,C'A'                                                       
         CLI   CSOURCE,1                                                        
         BE    *+8                                                              
         MVI   DMSRC,C'N'                                                       
         BAS   R9,RDHID                                                         
         CLC   KEY(3),IOAREA                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    IOAREA+19(4),IOAREA+19                                           
         BAS   R9,RHI                                                           
         B     *+8                                                              
BLDALPH  BAS   R9,RSEQ                                                          
         LA    R3,IOAREA                                                        
         CLC   IOAREA(3),KEY                                                    
         BNE   SRTALPH                                                          
         LA    R5,DMFRSTEL                                                      
         USING DMELEM,R5                                                        
         XC    MKTNAM,MKTNAM                                                    
         ZIC   RE,DMLEN                                                         
         SH    RE,=H'4'                                                         
         CH    RE,=H'15'                                                        
         BL    *+8                                                              
         LA    RE,15                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MKTNAM(0),DMMNAME                                                
         MVC   MANUM,DMMNO                                                      
         MVI   MAFLAG,0                                                         
         MVC   MAALPH,MKTNAM                                                    
         LA    R4,MALEN(R4)                                                     
         XC    MANUM(14),MANUM                                                  
         L     RE,MACOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MACOUNT                                                       
         B     BLDALPH                                                          
SRTALPH  L     R4,MAPOINT          BYPASS COMPLETED MARKETS                     
SRTALPH1 OC    SAVDYTM(10),SAVDYTM                                              
         BZ    SRTALPH2                                                         
         CLC   MAALPH,SAVDYTM                                                   
         BH    *+8                                                              
         MVI   MAFLAG,X'FF'                                                     
         LA    R4,MALEN(R4)                                                     
         OC    MANUM,MANUM                                                      
         BNZ   SRTALPH1                                                         
SRTALPH2 DS    0C                                                               
         L     R4,MAPOINT                                                       
         L     R9,MACOUNT                                                       
         GOTO1 XSORT,DMCB,(R4),(R9),14,12,2                                     
         DS    0H                                                               
         EJECT                                                                  
* MARKETS ARE IN SEQUENCE-NOW FIND ACTIVES                                      
         XC    MACOUNT,MACOUNT                                                  
         L     R4,MAPOINT                                                       
         LA    RE,WORKEND                                                       
         L     RF,=F'1000'                                                      
         XCEF                                                                   
         XC    MACOUNT,MACOUNT                                                  
GETMKT   OC    MANUM,MANUM                                                      
         BZ    GMEXIT                                                           
         CLI   MAFLAG,X'FF'                                                     
         BNE   *+12                                                             
         LA    R4,MALEN(R4)                                                     
         B     GETMKT                                                           
         LA    R3,KEY                                                           
         USING MLKEY,R3            GET ACTIVE MARKETS                           
         XC    KEY,KEY                                                          
         MVI   MLCODE,MLCODEQU                                                  
         MVC   MLMEDIA,CMEDIA                                                   
         MVI   MLSRC,C'A'                                                       
         MVC   MLBOOK+1(1),B1                                                   
         NI    MLBOOK+1,X'0F'                                                   
         ZIC   R9,B1                                                            
         SRL   R9,4                                                             
         LA    R9,80(R9)                                                        
         STC   R9,MLBOOK                                                        
         CLI   CSOURCE,1                                                        
         BE    *+8                                                              
         MVI   MLSRC,C'N'                                                       
         XC    MLBOOK,=X'FFFF'                                                  
         MVC   MLRMKT,MANUM                                                     
         MVC   MLSTAT(4),STAT                                                   
         BAS   R9,RDHID                                                         
         MVI   MAFLAG,X'FF'                                                     
         LA    R3,IOAREA                                                        
         OC    STAT,STAT                                                        
         BZ    *+14                                                             
         CLC   MLSTAT(4),STAT                                                   
         BNE   GETMKT                                                           
         CLI   FNO,1                                                            
         BNE   GETSTA                                                           
         CLC   KEY(8),IOAREA                                                    
         BNE   GETMKT                                                           
         L     RE,MACOUNT                                                       
         SLL   RE,1                                                             
         LA    RF,WORKEND(RE)                                                   
         MVC   0(2,RF),MANUM                                                    
         L     RE,MACOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MACOUNT                                                       
         C     RE,=F'28'                                                        
         BNE   GETMKT                                                           
GMEXIT   LA    R6,WORKEND          SEND MARKET NAMES AND NUMBERS                
         LA    R7,WORKEND+28                                                    
PUTMKT   OC    0(2,R6),0(R6)                                                    
         BZ    EXIT05                                                           
         MVC   DUB2(2),0(R6)                                                    
         BAS   RE,RDMKT                                                         
         LH    R9,0(R6)                                                         
         EDIT  (R9),(4,FLDDATA)                                                 
         MVC   FLDDATA+5(30),MKTNAM                                             
         CLC   SAVDYTM(10),MKTNAM                                               
         BH    *+10                                                             
         MVC   SAVDYTM(10),MKTNAM                                               
         OC    0(2,R7),0(R7)                                                    
         BZ    PUTMKT2                                                          
         MVC   DUB2(2),0(R7)                                                    
         BAS   RE,RDMKT                                                         
         LH    R9,0(R7)                                                         
         LA    RF,FLDDATA+40                                                    
         EDIT  (R9),(4,(RF))                                                    
         MVC   FLDDATA+45(30),MKTNAM                                            
         MVC   SAVDYTM(10),MKTNAM                                               
PUTMKT2  LA    R6,2(R6)                                                         
         XC    0(2,R7),0(R7)                                                    
         LA    R7,2(R7)                                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     PUTMKT                                                           
         EJECT                                                                  
GETSTA   LA    R6,FLDDATA+40                                                    
         CLC   KEY(8),IOAREA                                                    
         BNE   GETMKT                                                           
         MVC   KEYSAVE,KEY                                                      
         MVC   DUB2,MANUM                                                       
         BAS   RE,RDMKT                                                         
         MVC   KEY,KEYSAVE                                                      
         EDIT  (2,MANUM),(4,FLDDATA)                                            
         MVC   FLDDATA+5(30),MKTNAM                                             
         B     GETSTA2                                                          
GETSTA1  BAS   R9,RDSEQ                                                         
GETSTA2  CLC   KEY(8),IOAREA                                                    
         BNE   GETSTA3                                                          
         LA    R3,IOAREA                                                        
         TM    MLSTAT,X'F0'                                                     
         BO    GETSTA1                                                          
         CLI   MLSTAT,C'U'                                                      
         LA    R3,IOAREA                                                        
         MVC   0(5,R6),MLSTAT                                                   
         LA    R6,8(R6)                                                         
         LA    R7,FLDDATA+78                                                    
         FOUT  (R2)                                                             
         CR    R6,R7                                                            
         BL    GETSTA1                                                          
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R6,FLDDATA+40                                                    
         L     RE,MACOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MACOUNT                                                       
         CH    RE,=H'13'                                                        
         BE    EXIT05                                                           
         B     GETSTA1                                                          
GETSTA3  MVC   SAVDYTM,MKTNAM                                                   
         B     GETMKT                                                           
LINLEN   EQU   88                                                               
EXIT05   LA    R2,DEMPAGH                                                       
         OC    MANUM,MANUM                                                      
         BNZ   EXIT                                                             
         LA    R2,DEMBOOKH                                                      
         XC    SAVDYTM(30),SAVDYTM                                              
         B     EXIT                                                             
         EJECT                                                                  
RHI      GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMFIL',SVDADR,IOAREA                
         BR    R9                                                               
*                                                                               
RSEQ     GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'DEMFIL',SVDADR,IOAREA                
         BR    R9                                                               
RDSEQ    GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',KEY,IOAREA                   
         BR    R9                                                               
RDHID    GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA                   
         CLI   IOAREA,C'A'                                                      
         BNER  R9                                                               
         MVC   SVDADR,IOAREA+19                                                 
         BR    R9                                                               
RDMKT    NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DMKEY,R3                                                         
         MVI   DMKEY,C'A'                                                       
         MVC   DMMEDIA,CMEDIA                                                   
         MVI   DMSRC,C'A'                                                       
         CLI   CSOURCE,1                                                        
         BE    *+8                                                              
         MVI   DMSRC,C'N'                                                       
         MVC   DMRMKT,DUB2                                                      
         MVC   IOAREA(20),KEY                                                   
         BAS   R9,RHI                                                           
         LA    R3,IOAREA                                                        
         LA    R5,DMFRSTEL                                                      
         XC    MKTNAM,MKTNAM                                                    
         ZIC   RE,DMLEN                                                         
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MKTNAM(0),DMMNAME                                                
         MVC   KEY,KEYSAVE                                                      
         BAS   R9,RDHID                                                         
         XIT1                                                                   
         BR    R9                                                               
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
MKTALPH  DSECT                                                                  
MANUM    DS    CL2                                                              
MAFLAG   DS    CL1                                                              
MAALPH   DS    CL11                                                             
MALEN    EQU   14                                                               
         TITLE 'RECORDS DSECTS'                                                 
       ++INCLUDE DMDEMFILE                                                      
         TITLE 'WORK AREA DSECTS'                                               
       ++INCLUDE GENOLD                                                         
       ++INCLUDE T20EWORK                                                       
MACOUNT  DS    F                                                                
MAPOINT  DS    F                                                                
SVDADR   DS    F                                                                
XSORT    DS    F                                                                
MKTNAM   DS    CL30                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036T20E05    05/01/02'                                      
         END                                                                    
