*          DATA SET ACREP7A02  AT LEVEL 072 AS OF 06/03/15                      
*PHASE AC7A02A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CONVERSION ACCOUNT LISTING'                                     
AC7A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC7A**,RR=R4                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING LWSD,RC                                                          
         ST    R4,RELO                                                          
         L     R9,=V(SORTER)                                                    
         A     R9,RELO                                                          
         ST    R9,SORTER                                                        
         L     R9,=A(IO1)                                                       
         A     R9,RELO                                                          
         ST    R9,AIO1                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
         CLI   MODE,RUNFRST                                                     
         BNE   RQF010                                                           
         MVI   FILESW,0                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        REQUEST FIRST                                                          
*        QOPT1 O=(DEFAULT)OLD ACCOUNT ORDER, N=NEW ACCOUNT ORDER                
*        QOPT2 A=(DEFAULT)ALL ACCOUNTS, L=LINKED ONLY                           
*              M=MISSING CONVERSION ACCOUNTS ONLY                               
*        QOPT3 D=(DEFAULT)DRAFT RUN, L=LIVE RUN(CREATE ACCOUNT TABLE)           
*        QOPT4 A=(DEFAULT)ALL ACCOUNTS, B=BALANCE ACCOUNTS ONLY                 
*--------------------------------------------------------------------*          
RQF010   CLI   MODE,REQFRST                                                     
         BNE   PAC010                                                           
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   SVLACC,SPACES                                                    
         MVI   AGYSET,C'Y'                                                      
                                                                                
         MVI   SVTSTCMP,0                                                       
         L     R4,ADCOMP                                                        
         AH    R4,DATADISP                                                      
RQF15    CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),CPYELQ                                                     
         BE    RQF20                                                            
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     RQF15                                                            
         USING CPYELD,R4                                                        
RQF20    MVC   SVTSTCMP,CPYTCMP                                                 
         CLI   SVTSTCMP,0                                                       
         BNE   RQF30                                                            
         MVC   P+10(35),=C'** COMPANY NOT SET UP TO CONVERT **'                 
         GOTO1 ACREPORT                                                         
         MVI   AGYSET,C'N'                                                      
         MVI   MODE,REQLAST                                                     
         B     XIT                                                              
                                                                                
RQF30    CLI   QOPT1,C'N'                                                       
         BNE   RQF40                                                            
         MVI   RCSUBPRG,1                                                       
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
                                                                                
RQF40    CLI   QOPT3,C'L'                                                       
         BNE   RQF50                                                            
         TM    FILESW,FLOPEN                                                    
         BO    RQF50                                                            
         OPEN  (TABOUT,OUTPUT)                                                  
         OI    FILESW,FLOPEN                                                    
                                                                                
RQF50    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS POSTING LEVEL ACCOUNT RECORDS                                  
*--------------------------------------------------------------------*          
PAC010   CLI   MODE,PROCACC                                                     
         BNE   RLST10                                                           
         CLI   AGYSET,C'N'                                                      
         BE    XIT                                                              
                                                                                
         CLI   QOPT4,C'B'                                                       
         BNE   PAC030                                                           
         L     R4,ADACC                                                         
         AH    R4,DATADISP                                                      
PAC020   CLI   0(R4),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ABLELQ                                                     
         BE    PAC025                                                           
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PAC020                                                           
                                                                                
         USING ABLELD,R4                                                        
PAC025   ZAP   DUB,ABLFRWD                                                      
         AP    DUB,ABLDR                                                        
         SP    DUB,ABLCR                                                        
         CP    DUB,=P'0'                                                        
         BE    XIT                                                              
                                                                                
         USING ACTRECD,R2                                                       
PAC030   L     R2,ADACC                                                         
         MVC   TABREC,SPACES                                                    
         MVC   TABACC1,ACTKULA                                                  
                                                                                
         CLI   QOPT1,C'N'                                                       
         BE    PAC100                                                           
                                                                                
         CLC   SVLACC,SPACES                                                    
         BE    PAC045                                                           
         CLC   SVLACC(2),1(R2)                                                  
         BE    PAC045                                                           
         MVI   FORCEHED,C'Y'                                                    
PAC045   MVC   SVLACC,1(R2)                                                     
                                                                                
         USING PRNTD,R3                                                         
         LA    R3,P                                                             
         MVC   PACC1,ACTKULA                                                    
         MVC   PACC2,=C'** MISSING ** '                                         
         MVC   TABACC2,=C'** MISSING ** '                                       
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
PAC050   CLI   0(R4),0                                                          
**       BE    PAC090                                                           
         BNE   PAC052                                                           
         CLI   QOPT2,C'L'                                                       
         BNE   PAC090                                                           
         MVC   P,SPACES                                                         
         B     XIT                                                              
PAC052   CLI   0(R4),FFTELQ                                                     
         BE    PAC070                                                           
PAC053   ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PAC050                                                           
                                                                                
         USING FFTELD,R4                                                        
PAC070   CLI   FFTTYPE,FFTTCONV                                                 
         BNE   PAC053                                                           
         CLI   QOPT2,C'M'                                                       
         BNE   PAC073                                                           
         MVC   P,SPACES                                                         
         B     XIT                                                              
                                                                                
PAC073   MVC   PACC2,FFTDATA                                                    
         MVC   TABACC2,FFTDATA                                                  
         MVC   PACCNAME,=C'**  ACCOUNT MISSING FROM TEST ID  **'                
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),SVTSTCMP                                                
         MVC   MYKEY+1(14),FFTDATA                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,AIO1                   
         CLI   8(R1),0                                                          
         BNE   PAC090                                                           
         MVC   PACCNAME,SPACES                                                  
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
PAC075   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),NAMELQ                                                     
         BE    PAC080                                                           
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PAC075                                                           
         USING NAMELD,R4                                                        
PAC080   ZIC   R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PACCNAME(0),NAMEREC                                              
                                                                                
PAC090   MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         CLI   QOPT3,C'L'                                                       
         BNE   XIT                                                              
         CLC   TABACC2,=C'** MISSING ** '                                       
         BE    XIT                                                              
         LA    R4,TABREC                                                        
         PUT   TABOUT,(R4)                                                      
         B     XIT                                                              
                                                                                
PAC100   DS    0H                                                               
         MVC   SRTACCO,ACTKULA                                                  
         MVC   SRTACCN,=C'** MISSING ** '                                       
         MVC   TABACC2,SRTACCN                                                  
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
PAC120   CLI   0(R4),0                                                          
         BE    PAC190                                                           
         CLI   0(R4),FFTELQ                                                     
         BE    PAC130                                                           
PAC122   ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PAC120                                                           
         USING FFTELD,R4                                                        
PAC130   CLI   FFTTYPE,FFTTCONV                                                 
         BNE   PAC122                                                           
         MVC   SRTACCN,FFTDATA                                                  
         MVC   TABACC2,SRTACCN                                                  
                                                                                
PAC190   CLI   QOPT2,C'L'                                                       
         BNE   PAC192                                                           
         CLC   SRTACCN,=C'** MISSING ** '                                       
         BE    XIT                                                              
PAC192   CLI   QOPT2,C'M'                                                       
         BNE   PAC194                                                           
         CLC   SRTACCN,=C'** MISSING ** '                                       
         BNE   XIT                                                              
PAC194   LA    R4,SRTIO                                                         
         GOTO1 SORTER,DMCB,=C'PUT',(R4)                                         
         CLI   QOPT3,C'L'                                                       
         BNE   XIT                                                              
         CLC   SRTACCN,=C'** MISSING ** '                                       
         BNE   XIT                                                              
         LA    R4,TABREC                                                        
         PUT   TABOUT,(R4)                                                      
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        REQUEST LAST                                                           
*--------------------------------------------------------------------*          
RLST10   CLI   MODE,REQLAST                                                     
         BNE   RNL10                                                            
         CLI   QOPT1,C'N'                                                       
         BNE   RLST50                                                           
         CLI   AGYSET,C'N'                                                      
         BE    RLST50                                                           
                                                                                
RLST20   LA    R4,SRTIO                                                         
         GOTO1 SORTER,DMCB,=C'GET',(R4)                                         
         L     R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    RLST30                                                           
         MVC   SRTIO,0(R4)                                                      
         BAS   RE,PRTSORT                                                       
         B     RLST20                                                           
                                                                                
RLST30   GOTO1 SORTER,DMCB,=C'END'                                              
                                                                                
RLST50   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        AT RUN LAST CLOSE DATA SET IF IT WAS OPEN                              
*--------------------------------------------------------------------*          
RNL10    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         TM    FILESW,FLOPEN                                                    
         BZ    XIT                                                              
         CLOSE (TABOUT)                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PRINT RECORDS FROM SORT                                                
*--------------------------------------------------------------------*          
PRTSORT  NTR1                                                                   
         USING PRNTD,R3                                                         
         LA    R3,P                                                             
         MVC   PACC1,SRTACCN                                                    
         MVC   PACC2,SRTACCO                                                    
         CLC   PACC1,=C'** MISSING ** '                                         
         BE    PRTS90                                                           
         CLC   SVLACC,SPACES                                                    
         BE    PRTS20                                                           
         CLC   SVLACC(2),SRTACCN                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
                                                                                
PRTS20   MVC   PACCNAME,=C'**  ACCOUNT MISSING FROM TEST ID  **'                
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),SVTSTCMP                                                
         MVC   MYKEY+1(14),SRTACCN                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,AIO1                   
         CLI   8(R1),0                                                          
         BNE   PRTS90                                                           
         MVC   PACCNAME,SPACES                                                  
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
PRTS35   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),NAMELQ                                                     
         BE    PRTS40                                                           
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PRTS35                                                           
         USING NAMELD,R4                                                        
PRTS40   ZIC   R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PACCNAME(0),NAMEREC                                              
                                                                                
                                                                                
PRTS90   MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         MVC   SVLACC,SRTACCN                                                   
         B     XIT                                                              
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        LITERAL POOL                                                           
*--------------------------------------------------------------------*          
AGYSET   DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,28,A),FORMAT=CH,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(28,,,,)'                              
                                                                                
SORTER   DS    A                                                                
AIO1     DS    A                                                                
                                                                                
TABOUT   DCB   DDNAME=TABOUT,DSORG=PS,MACRF=(PM),RECFM=FB,             X        
               LRECL=28,BLKSIZE=28000                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DSECT FOR MODULE                                                       
*--------------------------------------------------------------------*          
LWSD     DSECT                                                                  
RELO     DS    F                                                                
MYKEY    DS    CL42                                                             
SVLACC   DS    CL14                                                             
SVTSTCMP DS    CL1                                                              
FILESW   DS    CL1                                                              
FLOPEN   EQU   X'80'                                                            
                                                                                
SRTIO    DS    0CL28                                                            
SRTACCN  DS    CL14                                                             
SRTACCO  DS    CL14                                                             
                                                                                
TABREC   DS    0CL28                                                            
TABACC1  DS    CL14                                                             
TABACC2  DS    CL14                                                             
                                                                                
LWDSLEN  EQU   *-LWSD                                                           
                                                                                
PRNTD    DSECT                                                                  
         DS    0CL132                                                           
         DS    CL9                                                              
PACC1    DS    CL14                                                             
         DS    CL6                                                              
PACC2    DS    CL14                                                             
         DS    CL7                                                              
PACCNAME DS    CL36                                                             
                                                                                
                                                                                
         CSECT                                                                  
         DS    0D                                                               
IO1      DS    CL2000                                                           
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        INCLUDED DSECTS                                                        
*--------------------------------------------------------------------*          
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACREP7A02 06/03/15'                                      
         END                                                                    
