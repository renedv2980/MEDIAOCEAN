*          DATA SET SPREPTS02  AT LEVEL 008 AS OF 03/24/15                      
*PHASE SPTS02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
SPTS02   TITLE 'CREATE TBS BUCKET RECORDS FROM BUY RECORDS'                     
SPTS02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   *-8192                                                           
         NMOD1 0,SPTS02                                                         
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY        PROCESS                                      
         BE    TS30                                                             
         CLI   MODE,REQFRST        OPEN                                         
         BE    TS15                                                             
         CLI   MODE,REQLAST        CLOSE                                        
         BE    TS100                                                            
         CLI   MODE,RUNFRST                                                     
         BE    TS10                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*==============================================================*                
* RUNFRST - LOAD CORE-RESIDENT PHASE GETCTA                    *                
*==============================================================*=*              
TS10     DS    0H                                                               
         GOTO1 LOADER,DMCB,=CL8'T00A7C'                                         
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   GETCTA,4(R1)                                                     
*                                                                               
         MVI   RQNOPSSV,C'Y'       SET FOR NO PASSIVE POINTERS                  
         B     EXIT                                                             
         SPACE 2                                                                
*================================================================*              
* REQFRST - INITIALIZE CIBBLK AND SORT                           *              
*================================================================*              
         SPACE 1                                                                
TS15     DS    0H                                                               
         MVI   RQRDPOL,C'Y'        SET TO READ POL POINTERS                     
         XC    CIBBLK,CIBBLK                                                    
*                                                                               
         LA    R2,CIBBLK                                                        
         USING CIBBLKD,R2                                                       
*                                                                               
         MVI   CIBACT,CIBADDQ      TREAT ALL RECORDS AS 'ADDS'                  
         MVI   CIBFLAGS,CIBNUPDQ                                                
         MVC   CIBAGYA,QAGY                                                     
         MVC   CIBACOMF,ACOMFACS                                                
         MVC   CIBARCUP,RECUP                                                   
         MVC   CIBABUY,ADBUY                                                    
         CLI   RCWRITE,C'N'                                                     
         BE    *+8                                                              
         OI    CIBFLAGS,CIBNOWRT                                                
*                                                                               
         LA    R0,L'SRTKEY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(2),DUB                                               
         LA    R0,L'SRTREC                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         EJECT                                                                  
* DELETE ALL CTA USAGE ELEMENTS                                                 
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTARECD,R3          RD CTAREC TO DEL USAGE ELEMS                 
         MVI   CTAKTYP,CTAKTYPQ                                                 
         MVI   CTAKSUB,CTAKSUBQ                                                 
         MVC   CTAKAGMD,SVAGYMD                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 HIGH                                                             
TS18     CLC   KEY(3),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         LR    R8,R6                                                            
         USING CTARECD,R8                                                       
*                                                                               
         LA    R6,CTAEL                                                         
         MVI   ELCDLO,X'06'        USAGE ELEMENT                                
         MVI   ELCDHI,X'06'                                                     
*                                                                               
TS20     BAS   RE,NEXTEL2                                                       
         BNE   TS25                                                             
*                                  REMOVE ALL USAGE ELEMENTS                    
         MVC   P(11),=C'DEL CTAUSEL'                                            
         GOTO1 HEXOUT,DMCB,(R6),P+13,20                                         
         GOTO1 REPORT                                                           
         GOTO1 RECUP,DMCB,(C'S',(R8)),(R6)                                      
         B     TS20                                                             
*                                                                               
TS25     DS    0H                                                               
         GOTO1 PUT                                                              
         GOTO1 SEQ                                                              
         B     TS18                                                             
*                                                                               
         DROP  R2                                                               
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=36'                                    
         EJECT                                                                  
*================================================================*              
* PROCBUY                                                        *              
* CHECK FOR CHANGE OF STATION/ESTIMATE TO CREATE SORT OUTPUT     *              
* LOOK FOR CONTRACT ELEMENT IN BUY RECORD AND PROCESS IF THERE   *              
*================================================================*              
         SPACE 1                                                                
                                                                                
TS30     DS    0H                                                               
         LA    R2,CIBBLK                                                        
         USING CIBBLKD,R2                                                       
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R8,ADBUY            POINT TO RECORD WE JUST READ                 
         USING BUYRECD,R8                                                       
*                                                                               
         TM    BDCIND2,X'02'       IS IT A TRADE BUY                            
         BNO   EXIT                NO                                           
*                                                                               
         MVI   CIBERR,0            RESET                                        
         MVI   ERRCD,NOCONT        SET LOCAL ERROR VALUE                        
         MVI   ELCDLO,X'70'        SEARCH FOR CONTRACT ELEMENT                  
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+12                                                             
         BAS   RE,TSERR                                                         
         B     EXIT                                                             
*                                                                               
* << CALL SPGETCTA >>                                                           
*                                                                               
         BAS   RE,TSTBRK           CHECK FOR CONTROL BREAK                      
*                                                                               
         GOTO1 GETCTA,CIBBLK       EXTRACT BUY VALUES INTO CIBNEW               
         CLI   CIBERR,0                                                         
         BE    *+8                                                              
         BAS   RE,TSERR                                                         
         B     EXIT                                                             
         DROP  R2,R8                                                            
         EJECT                                                                  
*================================================================*              
* TEST FOR CHANGE OF CLIENT/PRODUCT/ESTIMATE/STATION/CONTRACT    *              
* SRTKEY CONTAINS VALUES FOR PRESENT DATA IN BUFFER              *              
* ON ENTRY R6 POINTS TO ID ELEMENT IN CURRENT BUY RECORD         *              
*================================================================*              
         SPACE 2                                                                
TSTBRK   DS    0H                                                               
*                                                                               
         LA    R2,CIBBLK                                                        
         USING CIBBLKD,R2                                                       
*                                                                               
         L     R8,ADBUY            POINT TO RECORD WE JUST READ                 
         USING BUYRECD,R8                                                       
*                                                                               
         MVC   THISPRD,=C'POL'                                                  
         CLI   BUYKPRD,X'FF'                                                    
         BE    TSTBRK2                                                          
*                                                                               
         LR    R0,RE                                                            
         BAS   RE,GETPRDCD                                                      
         LR    RE,R0                                                            
*                                                                               
TSTBRK2  CLC   SRTCON,3(R6)                                                     
         BNE   TSTBRK10                                                         
         CLC   SRTCLT,BUYKCLT                                                   
         BNE   TSTBRK10                                                         
         CLC   SRTPRD,THISPRD                                                   
         BNE   TSTBRK10                                                         
         CLC   SRTSTA,BUYMSTA+2                                                 
         BNE   TSTBRK10                                                         
         CLC   SRTEST,BUYKEST                                                   
         BNE   TSTBRK10                                                         
         BR    RE                  NO CONTROL BREAK                             
*                                                                               
TSTBRK10 NTR1                                                                   
*                                                                               
         OC    CIBNEW,CIBNEW                                                    
         BZ    TSTBRK12                                                         
*                                                                               
         XC    SRTDATA,SRTDATA     BUILD CTAUSEL                                
         LA    R3,SRTDATA                                                       
         USING CTAUSELD,R3                                                      
*                                                                               
         MVI   CTAUSEL,CTAUSELQ                                                 
         MVI   CTAUSELN,CTAUSLNQ                                                
         MVC   CTAUSSTA,SRTSTA     STATION                                      
         MVC   CTAUSCLT,SRTCLT     CLIENT                                       
*                                                                               
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
PRDCD2   CLC   SRTPRD,3(RF)                                                     
         BE    PRDCD4                                                           
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   PRDCD2                                                           
         ZIC   R0,SRTPRD                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CTAUSPRD,DUB                                                     
         B     *+10                                                             
*                                                                               
PRDCD4   MVC   CTAUSPRD,0(RF)                                                   
*                                                                               
         MVC   CTAUSEST,SRTEST     ESTIMATE                                     
         MVC   CTAUSOGR(8),CIBNEW+4  MOVE ORDERED/PAID DOLLARS                  
         EJECT                                                                  
         MVI   SORTSW,C'Y'         INIDICATE SORT OUTPUT                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTKEY                                   
         MVC   P(3),=C'PUT'                                                     
         GOTO1 HEXOUT,DMCB,SRTKEY,P+4,36,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
TSTBRK12 XC    CIBNEW,CIBNEW       CLEAR ACCUMULATORS                           
         MVC   SRTCON,3(R6)                                                     
         MVC   SRTSTA,BUYMSTA+2                                                 
         MVC   SRTCLT,BUYKCLT                                                   
         MVC   SRTPRD,BUYKPRD                                                   
         MVC   SRTEST,BUYKEST                                                   
         B     EXIT                                                             
         DROP  R2,R3,R8                                                         
         EJECT                                                                  
* LOOK UP PRODUCT CODE FROM CLTHDR                                              
*                                                                               
GETPRDCD L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
         L     R8,ADBUY            POINT TO RECORD WE JUST READ                 
         USING BUYRECD,R8                                                       
*                                                                               
GETPRD2  CLC   BUYKPRD,3(RF)                                                    
         BE    GETPRD4                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   GETPRD2                                                          
         ZIC   R0,BUYKPRD                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISPRD,DUB                                                      
         BR    RE                                                               
*                                                                               
GETPRD4  MVC   THISPRD,0(RF)                                                    
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
*===============================================================*               
* REQLAST                                                       *               
* RETRIEVE RECORDS FROM SORT AND UPDATE CONTRACT RECORDS        *               
*===============================================================*               
         SPACE 1                                                                
TS100    DS    0H                                                               
*                                                                               
         LA    R2,CIBBLK                                                        
         USING CIBBLKD,R2                                                       
         OC    CIBNEW,CIBNEW                                                    
         BZ    TS102                                                            
         L     R8,ADBUY            POINT TO RECORD WE JUST READ                 
         USING BUYRECD,R8                                                       
         XC    SRTDATA,SRTDATA     BUILD CTAUSEL                                
         LA    R3,SRTDATA                                                       
         USING CTAUSELD,R3                                                      
*                                                                               
         MVI   CTAUSEL,CTAUSELQ                                                 
         MVI   CTAUSELN,CTAUSLNQ                                                
         MVC   CTAUSSTA,SRTSTA     STATION                                      
         MVC   CTAUSCLT,SRTCLT     CLIENT                                       
         MVC   CTAUSPRD,BUYKPRD    PRODUCT                                      
         MVC   CTAUSEST,SRTEST     ESTIMATE                                     
         MVC   CTAUSOGR(8),CIBNEW+4  MOVE ORDERED/PAID DOLLARS                  
*                                                                               
         MVI   SORTSW,C'Y'         INIDICATE SORT OUTPUT                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTKEY                                   
         MVC   P(3),=C'PUT'                                                     
         GOTO1 HEXOUT,DMCB,SRTKEY,P+4,36,=C'TOG'                                
         GOTO1 REPORT                                                           
         XC    CIBNEW,CIBNEW                                                    
         DROP  R2                                                               
*                                                                               
TS102    CLI   SORTSW,C'Y'         TEST ANY RECORDS PUT TO SORT                 
         BE    TS105                                                            
         MVC   P(32),=C'** ERROR ** NO RECORDS PROCESSED'                       
         GOTO1 REPORT                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     EXIT                                                             
*                                                                               
TS105    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   REC1,0(R5)        FIRST TIME - SAVE RECORD                       
         MVC   P(3),=C'GET'                                                     
         GOTO1 HEXOUT,DMCB,REC1,P+4,36,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     R7,=A(ELEMBUFF)                                                  
         USING SRTREC,R7                                                        
         LA    R3,SRTDATA                                                       
         USING CTAUSEL,R3                                                       
*                                                                               
         MVC   0(L'SRTREC,R7),REC1      SAVE ELEMENT & SORT DATA                
         LA    R7,L'SRTREC(R7)                                                  
         ST    R7,NEXTBUFF                                                      
*                                                                               
TS110    GOTO1 =V(SORTER),DMCB,=C'GET'  GET NEXT SORT RECORD                    
         ICM   R5,15,4(R1)              TEST EOF                                
         BNZ   TS112                                                            
         MVI   REC2,X'FF'             SET EOF FLAG                              
         B     TS120                                                            
*                                                                               
TS112    MVC   REC2,0(R5)        MOVE RECORD TO VISIBLE STORAGE                 
         MVC   P(3),=C'GET'                                                     
         GOTO1 HEXOUT,DMCB,REC2,P+4,36,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
         CLC   REC1(16),REC2       TEST FOR SAME KEY                            
         BNE   TS120                                                            
*                                                                               
* KEYS EQUAL, ADD DOLLARS                                                       
*                                                                               
         LA    RF,REC2                                                          
         L     R1,28(RF)                                                        
         L     R0,CTAUSOGR                                                      
         AR    R0,R1                                                            
         ST    R0,CTAUSOGR                                                      
*                                                                               
         LA    RF,REC2                                                          
         L     R1,32(RF)                                                        
         L     R0,CTAUSPGR                                                      
         AR    R0,R1                                                            
         ST    R0,CTAUSPGR                                                      
         B     TS110                                                            
         DROP  R3                                                               
         EJECT                                                                  
* CHANGE OF KEY - SAVE CTAUSEL TILL CHANGE OF CONTRACT                          
*                                                                               
TS120    DS    0H                                                               
         CLC   REC1(5),REC2        TEST FOR CHANGE OF CONTRACT                  
         BNE   TS130                                                            
*                                                                               
TS122    DS    0H                                                               
         L     R7,NEXTBUFF                                                      
         MVC   0(L'SRTREC,R7),REC2      SAVE ELEMENT & SORT DATA                
         LA    R3,SRTDATA                                                       
         LA    R7,L'SRTREC(R7)                                                  
         ST    R7,NEXTBUFF                                                      
*                                                                               
         MVC   REC1,REC2           SAVE NEW SRTREC                              
         B     TS110                                                            
         DROP  R7                                                               
         EJECT                                                                  
*================================================================*              
* CHANGE OF CONTRACT - PRINT DETAILS AND UPDATE RECORD           *              
* NOTE THAT SRTREC HAS NOT YET BEEN UPDATED                      *              
*================================================================*              
         SPACE 1                                                                
TS130    DS    0H                                                               
*                                                                               
         LA    R2,CIBBLK                                                        
         USING CIBBLKD,R2                                                       
*                                                                               
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*                                                                               
         MVC   PPRD,THISPRD        BEFORE THE USING FOR R7                      
*                                                                               
         L     R7,=A(ELEMBUFF)                                                  
         USING SRTREC,R7                                                        
         LA    R3,SRTDATA                                                       
         USING CTAUSELD,R3                                                      
*                                                                               
         MVC   PCON,SRTCON                                                      
*                                                                               
TS132    DS    0H                                                               
*                                                                               
         GOTO1 MSUNPK,DMCB,CTAUSSTA-2,WORK,PSTA                                 
*                                                                               
         GOTO1 CLUNPK,DMCB,CTAUSCLT,PCLT                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,CTAUSEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         EDIT  CTAUSOGR,(11,PGORD),2,COMMAS=YES                                 
         EDIT  CTAUSPGR,(11,PGPAID),2,COMMAS=YES                                
         GOTO1 REPORT                                                           
         DROP  R3                                                               
*                                                                               
* NOW UPDATE CTA RECORD                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTARECD,R3          RD CTAREC BUILT FROM SRTREC                  
         MVI   CTAKTYP,CTAKTYPQ                                                 
         MVI   CTAKSUB,CTAKSUBQ                                                 
         MVC   CTAKAGMD,SVAGYMD                                                 
         PACK  FULL,SRTCON         CONTRACT #                                   
         ZAP   WORD,=P'999999'                                                  
         SP    WORD,FULL           FIND 9'S COMPLEMENT OF CONTRACT NO.          
         SRP   WORD,1,0            SHIFT LEFT 1 DIGIT                           
         MVC   CTAKYR(3),WORD                                                   
         DROP  R3,R8                                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                CAN'T BE A SRTREC W/OUT A CTAREC             
*                                                                               
         L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         GOTO1 GET                                                              
         L     R8,AREC                                                          
         USING CTARECD,R8                                                       
*                                                                               
*  BUILD THE USAGE ELEMENT                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(20),SRTDATA  CTAUSEL                                        
*                                                                               
         MVC   P(4),=C'ELEM'                                                    
         GOTO1 HEXOUT,DMCB,ELEM,P+5,20                                          
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',SPTFILE),(R8),ELEM,0                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUT                                                              
*                                                                               
         LA    R7,L'SRTREC(R7)                                                  
         LA    R3,SRTDATA                                                       
         CLI   0(R7),0                                                          
         BNE   TS132                                                            
*                                                                               
TS135    L     R7,=A(ELEMBUFF)     CLEAR OUT ELEMBUFF                           
         LR    R0,R7                                                            
         LA    R1,ELEMBUFX-ELEMBUFF                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   REC1,REC2                                                        
         CLI   REC2,X'FF'                                                       
         BE    TS140                                                            
         XC    REC2,REC2                                                        
*                                                                               
         LA    R3,SRTDATA                                                       
*                                                                               
         MVC   0(L'SRTREC,R7),REC1      SAVE ELEMENT & SORT DATA                
         LA    R7,L'SRTREC(R7)                                                  
         ST    R7,NEXTBUFF                                                      
*                                                                               
         B     TS110                                                            
*                                                                               
TS140    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         DROP  R7                                                               
         EJECT                                                                  
*===============================================================*               
* THESE ERRORS ARE ASSUMED TO OCCUR DURING BUYREC PROCESSING    *               
*===============================================================*               
         SPACE 1                                                                
NOCONT   EQU   254                                                              
*                                                                               
TSERR    NTR1                                                                   
         MVC   BYTE,CIBERR                                                      
         DROP  R2                                                               
         CLI   BYTE,0                                                           
         BNE   *+10                                                             
         MVC   BYTE,ERRCD          USE LOCAL ERROR CODE                         
         L     RE,=A(ERRLIST)                                                   
         SR    RF,RF                                                            
TSERR2   DS    0H                                                               
         CLC   BYTE,0(RE)                                                       
         BE    TSERR10                                                          
         IC    RF,1(RE)                                                         
         LA    RE,2(RE,RF)                                                      
         CLI   0(RE),X'FF'                                                      
         BNE   TSERR2                                                           
         DC    H'0'                                                             
*                                                                               
TSERR10  MVC   P(11),=C'** ERROR **'                                            
         IC    RF,1(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+12(0),2(RE)                                                    
         GOTO1 REPORT                                                           
         BAS   RE,PRTBUY                                                        
         B     EXIT                                                             
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R8,ADBUY            POINT TO RECORD WE JUST READ                 
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R4,P                                                             
         MVC   0(1,R4),QMED                                                     
         MVC   2(3,R4),CLT         CURRENT CLIENT CODE                          
         MVC   6(3,R4),THISPRD                                                  
         GOTO1 MSUNPK,DMCB,BUYMSTA,10(R4),15(R4)                                
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
         MVI   26(R4),C'-'                                                      
         ZIC   R0,BUYKEY+11                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  27(3,R4),DUB                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
PRTCON   NTR1                                                                   
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,=C'CTAREC',(R6),C'DUMP',(R0),=C'1D00'                     
         B     EXIT                                                             
PRTSORT  NTR1                                                                   
         LA    R0,L'SRTREC                                                      
         GOTO1 PRNTBL,=C'SRTREC',SRTREC,C'DUMP',(R0),=C'1D00'                   
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
FAKEKEY  DS    CL13                                                             
REC1     DS    CL36                                                             
REC2     DS    CL36                                                             
         DS    0D                                                               
         DC    CL8'**ELEM**'                                                    
ELEM     DS    XL64                                                             
         DC    CL8'*CIBBLK*'                                                    
CIBBLK   DS    XL64                                                             
         DS    0D                                                               
         DC    CL8'*SRTREC*'                                                    
SRTREC   DS    0CL36                                                            
SRTKEY   DS    0CL16                                                            
SRTCON   DS    CL5                 EBCDIC CONTRACT                              
SRTSTA   DS    XL3                                                              
SRTCLT   DS    CL2                                                              
SRTPRD   DS    CL3                 EBCDIC PRODUCT                               
SRTEST   DS    XL1                                                              
         DS    XL2                                                              
*                                                                               
SRTDATA  DS    XL20                CONTAINS CTAUSEL                             
*                                                                               
SRTNEW   DS    XL36                                                             
*                                                                               
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SORTSW   DS    C                                                                
THISPRD  DS    CL3                                                              
ERRCD    DS    X                                                                
         DS    X                                                                
GETCTA   DS    A                                                                
NEXTBUFF DS    A                                                                
*                                                                               
         DS    0D                                                               
         EJECT                                                                  
*                                                                               
ELEMBUFF DS    CL2000                                                           
ELEMBUFX EQU   *                                                                
*                                                                               
         DS    0D                                                               
*                                                                               
         SPACE 2                                                                
ERRLIST  DS    0D                                                               
         DC    AL1(001),AL1(L'ERR001)                                           
ERR001   DC    C'INVALID CONTRACT IN BUY RECORD'                                
         DC    AL1(002),AL1(L'ERR002)                                           
ERR002   DC    C'MAX DOLLARS EXCEEDED IN CONTRACT'                              
         DC    AL1(254),AL1(L'ERR254)                                           
ERR254   DC    C'MISSING CONTRACT IN BUY RECORD'                                
         DC    X'FF'               E-O-L FLAG                                   
*                                                                               
         EJECT                                                                  
PLINED   DSECT                                                                  
PCON     DS    CL5                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PGORD    DS    CL11                                                             
         DS    CL2                                                              
PGPAID   DS    CL11                                                             
         EJECT                                                                  
       ++INCLUDE SPCIBBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCTA                                                       
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPTS02 03/24/15'                                      
         END                                                                    
