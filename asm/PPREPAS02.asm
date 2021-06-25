*          DATA SET PPREPAS02  AT LEVEL 004 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PPAS02A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE OFFOUT                                                                 
         TITLE 'PPAS02  CHANGE LOG'                                             
*                                                                               
         PRINT NOGEN                                                            
         TITLE 'PPAS02 - AGENCY COPY PROGRAM'                                   
PPAS02   CSECT                                                                  
         NMOD1 0,PPAS02,RR=R9                                                   
*                                                                               
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,=V(ASWORK)                                                    
         AR    R5,R9                                                            
         USING ASWORKD,R5,R6                                                    
         LA    R6,4095(R5)                                                      
         LA    R6,1(R6)                                                         
         ST    R9,RELO                                                          
         LA    RF,PRDTAB                                                        
         ST    RF,APRDTAB                                                       
         DROP  R6                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CKMODE                                                           
         BAS   R9,INITIAL                                                       
         GOTO1 VBLDMLST              BUILD MTH LIST                             
         B     EXT                                                              
*                                                                               
CKMODE   CLI   MODE,FBUYCLI                                                     
         BNE   CKM2                                                             
         GOTO1 VCLIFRST                                                         
         B     EXT                                                              
*                                                                               
CKM2     DS    0H                                                               
*                                                                               
CKM3     CLI   MODE,FBUYPUB                                                     
         BNE   CKM4                                                             
         GOTO1 VPUBFRST                                                         
         B     EXT                                                              
*                                                                               
CKM4     CLI   MODE,PROCBUY                                                     
         BNE   CKM6                                                             
         B     PROCESS                                                          
*                                                                               
CKM6     CLI   MODE,LBUYPUB                                                     
         BNE   CKM8                                                             
         GOTO1 VPUBEND                                                          
         B     EXT                                                              
*                                                                               
CKM8     CLI   MODE,LBUYPRO                                                     
         BNE   CKM9                                                             
         GOTO1 VPRDEND                                                          
         B     EXT                                                              
*                                                                               
CKM9     CLI   MODE,LBUYCLI                                                     
         BNE   CKM11                                                            
         GOTO1 VCLTEND                                                          
         B     EXT                                                              
*                                                                               
CKM11    CLC   QSORT,=C'08'                                                     
         BE    CKM11A                                                           
         CLC   QSORT,=C'09'                                                     
         BNE   CKM12                                                            
*                                                                               
CKM11A   CLI   MODE,LBUYREP                                                     
         BNE   CKM12                                                            
         GOTO1 VREPEND                                                          
         B     EXT                                                              
*                                                                               
CKM12    CLI   MODE,FBUYPRO                                                     
         BNE   CKM14                                                            
         CLI   PRDSW,1                                                          
         BNE   EXT                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXT                                                              
*                                                                               
CKM14    CLI   MODE,RUNFRST                                                     
         BNE   EXT                                                              
         XC    SVMEDCLI,SVMEDCLI                                                
         B     EXT                                                              
*                                                                               
         EJECT                                                                  
PROCESS  EQU   *                                                                
         OC    KEY+21(3),KEY+21       IGNORE PASSIVE POINTERS                   
         BNZ   EXT                                                              
*                                                                               
PROC10   CLI   PUBSW,0             SEE IF DOING THIS PUB                        
         BNE   EXT                 NO                                           
PROC12   DS    0H                                                               
         CLI   QBPDATE,C'B'        SEE IF REPORTING BILLABLE MONTHS             
         BNE   PROC12D                                                          
         GOTO1 VMTHEND             MUST ALWAYS GOT TO MTHEND                    
         B     PROC12X                                                          
*                                                                               
PROC12D  CLC   LASTYM,PBUYKDAT     CHECK FOR CHANGE OF MONTH                    
         BE    *+10                                                             
         GOTO1 VMTHEND                                                          
         MVC   LASTYM,PBUYKDAT                                                  
*                                                                               
PROC12X  XC    THISGST,THISGST                                                  
         XC    PAIDGST,PAIDGST                                                  
         XC    UPAIDGST,UPAIDGST                                                
         XC    GSTCOMM,GSTCOMM                                                  
         CLI   PAGYNAT,C'C'        CANADIAN                                     
         BNE   CKQOPT1                                                          
*                                                                               
         PRINT  GEN                                                             
         MVC   FWORK,=C'CRATE'                                                  
         LA    R1,PBUYREC                                                       
         ST    R1,DMCB                                                          
         CLI   ASOFDTE,0                                                        
         BE    DOGOTO                                                           
*        *                                                                      
         XC    GSTDTS,GSTDTS                                                    
         MVC   GSTDTS+3(3),ASOFDTE                                              
         LA    R1,GSTDTS                                                        
         ST    R1,DMCB+12                                                       
         MVI   DMCB,C'P' PASS BACK PAID DATA BETWEEN BOTH DATES                 
DOGOTO   LA    RF,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    DOGOTOO                                                          
         LA    RF,KEY+13                                                        
*        *                                                                      
DOGOTOO  GOTO1 GETINS,DMCB,,FWORK,(RF),,=C'BOTH'                                
         PRINT NOGEN                                                            
*                                                                               
         XC    THISGST,THISGST     INIT GST FIELDS                              
         XC    GSTCOMM,GSTCOMM                                                  
         XC    PAIDGST,PAIDGST                                                  
         XC    UPAIDGST,UPAIDGST                                                
*                                                                               
         CLI   PBDCOSIN,C'C'       COMMISSION ONLY BUY                          
         BE    CKQOPT1             NO GST                                       
*                                                                               
         USING GVALUESD,R1                                                      
         L     R1,16(R1)                                                        
*                                                                               
         TM    PBUYCNTL,X'80'        IF DELETED                                 
         BNO   GSTDELX                                                          
*                                                                               
         XC    GSTTAX,GSTTAX            CLEAR ORDERED GST                       
*                                                                               
         LA    R0,10                    TEN PROVINCES                           
         LA    RF,PSTAREA               PST SAVEAREA                            
         USING PSTAREA,RF               ESTABLISH PSTAREA                       
*                                                                               
         XC    PSTTAX,PSTTAX            CLEAR GROSS PST                         
         LA    RF,PSTAREAL(RF)          BUMP POINTER                            
         BCT   R0,*-10                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
GSTDELX  DS    0H                                                               
*                                                                               
         MVC   THISGST,GSTTAX      COPY GST AND PAID GST                        
         MVC   PAIDGST,GSTTAXPD                                                 
*                                                                               
*        ADD CORRESPONDING PST AMOUNTS TO GST                                   
*                                                                               
         LA    R0,10                    TEN PROVINCES                           
         LA    RF,PSTAREA               PST SAVEAREA                            
         USING PSTAREA,RF               ESTABLISH PSTAREA                       
*                                                                               
         ICM   RE,15,THISGST                                                    
         ICM   R2,15,PAIDGST                                                    
*                                                                               
         A     RE,PSTTAX                ADD PST TO GST BUCKET                   
         A     R2,PSTTAXPD              ADD PST TO GST PAID BUCKET              
         LA    RF,PSTAREAL(RF)          BUMP POINTER                            
         BCT   R0,*-12                                                          
*                                                                               
         STCM  RE,15,THISGST                                                    
         STCM  R2,15,PAIDGST                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   GSTCOMM,THISGST        PREPARE FOR GST ACCUMULATION              
*                                                                               
         L     RF,THISGST                                                       
         L     RE,PAIDGST                                                       
         SR    RF,RE               UNPAID GST                                   
         ST    RF,UPAIDGST                                                      
*                                                                               
         DROP  R1                                                               
         B     CKQOPT1                                                          
*                                                                               
GSTDTS   DS    CL6                                                              
*                                                                               
*                                                                               
CKQOPT1  DS    0H                                                               
         CLI   QOPT1,C' '      SEE IF DOING PAID OR UNPAID REQ                  
         BNE   CKOPT1A                                                          
         CLI   PROGPROF+5,C'Y' SUPPRESSING FREE BUYS ON ALL ITEM REQ            
         BNE   CKOPT1A                                                          
         OC    GROSS(12),GROSS                                                  
         BZ    NEXTBUY                                                          
*                                                                               
CKOPT1A  MVI   PAIDSW,0       WILL BE SET TO X'01'                              
*                             IF DATED PAY ELEM IS FOUND                        
         LA    R3,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
CKQOPT1A BAS   RE,NEXTEL                                                        
         BNE   CKQOPT1D                                                         
         USING PPDUMD03,R3                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    CKQOPT1A                                                         
         CLI   ASOFDTE,0             SEE IF I HAVE AN AS OF DATE                
         BE    CKQOPT1C                                                         
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1A                                                         
*                                                                               
CKQOPT1C MVI   PAIDSW,X'01'       SET DATE PAY ELEM FOUND                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1E         GO PROCESS THIS ELEM                            
*                                                                               
CKQOPT1D DS    0H        I GET HERE IF NO VALID PAY ELEMS                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1X                                                         
*                                                                               
CKQOPT1E DS    0H                                                               
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1F                                                         
         LM    R7,R9,PGROSS                                                     
         A     R7,PPGROSS                                                       
         A     R8,PPAGYCOM                                                      
         A     R9,PPCSHDSC                                                      
         STM   R7,R9,PGROSS                                                     
         LM    R7,R9,PPGROSS                                                    
         SR    R7,R8                                                            
         SR    R7,R9                                                            
         L     R6,PAID                                                          
         AR    R6,R7                                                            
         ST    R6,PAID                                                          
CKQOPT1F BAS   RE,NEXTEL                                                        
         BE    CKQOPT1E                                                         
         B     CKQOPT1X                                                         
*                                                                               
*                                                                               
NEXTEL   DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
CKQOPT1X TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BZ    CKPD                                                             
         OC    PGROSS(12),PGROSS                                                
         BZ    NEXTBUY              NOT PAID SO IGNORE                          
         XC    GROSS(20),GROSS                                                  
CKPD     CLI   QOPT1,C'P'                                                       
         BNE   CKUNPD                                                           
         OC    PGROSS(12),PGROSS                                                
         BNZ   CKPD5                                                            
         CLI   PAIDSW,X'01'       SEE IF I FOUND A DATED PAY ELEM               
         BNE   NEXTBUY                                                          
*                                                                               
         OC    GROSS(12),GROSS    SEE IF FREE BUY                               
         BNZ   CKPD5                                                            
*                                                                               
         CLI   PROGPROF+4,C'Y'  SUPPRESSING "FREE" BUYS ON PAID REQ             
         BE    NEXTBUY                                                          
*                                                                               
CKPD5    L     RF,GSTCOMM                                                       
         S     RF,UPAIDGST                                                      
         ST    RF,GSTCOMM                                                       
         MVC   BUYGST,UPAIDGST IF PAID REQUESTED WANT TO SHOW UNPD    X         
         B     BUYOKK                                                           
*                                                                               
CKUNPD   CLI   QOPT1,C'U'                                                       
         BNE   BUYOK        DOING ALL ITEMS                                     
         CLC   GROSS(12),PGROSS                                                 
         BNE   CKUNPD5                                                          
         OC    GROSS(12),GROSS     SEE IF "FREE" BUY                            
         BNE   NEXTBUY                                                          
         CLI   PAIDSW,X'01'        SEE IF I HAVE A DATED PAY ELEM               
         BE    NEXTBUY             YES THEN SKIP                                
         CLC   PBUYKDAT,=X'5C0901'  SEE IF BEFORE SEP01/92                      
         BL    NEXTBUY                                                          
         CLI   PROGPROF+3,C'Y'  SEE IF SKIPPING "FREE" ON UNPAID REQ            
         BE    NEXTBUY                                                          
*                                                                               
CKUNPD5  MVC   BUYGST,PAIDGST      PAID GST                           X         
         L     RF,GSTCOMM                                                       
         S     RF,PAIDGST                                                       
         ST    RF,GSTCOMM                                                       
         B     BUYOKK                                                           
*                                                                               
BUYOK    DS    0H                                                               
         MVC   BUYGST,THISGST                                         X         
BUYOKK   DS    0H                                                               
         CLC   SAVEPUB,PBUYKPUB                                                 
         BE    BUYOK5                                                           
         GOTO1 VPRNTPUB                                                         
BUYOK5   CLC   SAVEPRD,PBUYKPRD                                                 
         BE    BUYOK6                                                           
         GOTO1 =A(PROFRST)                                                      
BUYOK6   DS    0H                                                               
*                                                                               
BUYOK10  MVC   MTHACT(5),=5C'Y'    SET ACTIVITY SWITCHES                        
*                                                                               
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
         MVC   P+4(8),PBYOINS                                                   
         CLI   PBYOINS2,C' '                                                    
         BE    PPBY00                                                           
         MVI   PSECOND+3,C'+'                                                   
         MVC   PSECOND+4(8),PBYOINS2                                            
         B     PPBYX                                                            
PPBY00   DS    0H                                                               
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    PPBYX                                                            
         MVC   PSECOND+4(11),PBYOISNM                                           
         B     PPBYX                                                            
PPBYX    DS    0H                                                               
         TM    PBUYCNTL,X'80'           SEE IF DELETED                          
         BZ    PRTN1                                                            
         MVI   P+12,C'D'                                                        
         MVC   P+53(15),=C'CLEARED+DELETED'                                     
         B     PRTN2                                                            
PRTN1    MVC   P+53(14),PBYOGRS                                                 
PRTN2    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMAG                                                           
         MVC   P+25(8),PBYOUR                                                   
         CLI   PBYOSPC,C' '                                                     
         BE    PRTN5                                                            
         MVC   P+34(10),PBYOSPC                                                 
         MVC   PSECOND+34(11),PBYOPRM                                           
         B     PRTN10                                                           
*                                                                               
PRTN5    MVC   P+34(11),PBYOPRM                                                 
*                                                                               
PRTN10   MVC   P+45(7),PBYOUNTS                                                 
         LA    R3,5                                                             
         LA    R4,P+45                                                          
PRTN20   CLI   P+51,C' '                                                        
         BNE   PRTEST                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),PBYOUNTS                                                 
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R3,PRTN20                                                        
         B     PRTEST                                                           
*                                                                               
PRTMAG   DS    0H                                                               
         MVC   P+28(20),PBYOSPC                                                 
         MVC   PSECOND+28(20),PBYOSPC2                                          
*                                                                               
PRTEST   CLI   QOPT5,C'Y'      SEE IF FLAGGING BILLED/TRAFFICED ITEMS           
         BNE   *+8                                                              
         BAS   RE,TSTBLTR                                                       
         SR    R0,R0                                                            
         IC    R0,PBUYKEST                                                      
         SLL   R0,8                                                             
         IC    R0,PBUYKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(3),DUB+6(2)                                                 
*        GOTO1 DTCNV,DMCB,(1,PBDPDATE),(4,P+14)                                 
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(7,P+14)                                
         LM    R6,R9,GROSS                                                      
         STM   R6,R8,BUYGO                                                      
         A     R9,THISGST                                             X         
         ST    R9,BUYNP                                                         
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
         EDIT  BUYGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  BUYCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND1                                                          
         EDIT  THISGST,(12,(R3)),2,COMMAS=YES,MINUS=YES               X         
         LA    R3,14(R3)                                                        
*                                                                               
NTCAND1  DS    0H                                                               
*                                                                               
         EDIT  BUYNP,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PRTE20                                                           
         CLI   PBYOZZZ,C' '                                                     
         BE    PRTE20                                                           
         CLC   PSECOND+28(10),SPACES        SEE IF PSECOND IS USED              
         BNE   PRTE10              YES                                          
         MVC   PSECOND+25(50),PBYOZZZ                                           
         CLI   QMEDIA,C'N'                                                      
         BE    PRTE5                                                            
         MVC   PSECOND+28(50),PBYOZZZ                                           
         MVC   PSECOND+25(3),SPACES                                             
PRTE5    GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE10   GOTO1 VPRINTIT                                                         
         MVC   P+25(50),PBYOZZZ                                                 
         CLI   QMEDIA,C'N'                                                      
         BE    PRTE15                                                           
         MVC   P+28(50),PBYOZZZ                                                 
         MVC   P+25(3),SPACES                                                   
PRTE15   GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE20   GOTO1 VPRINTIT                                                         
PRTEX    OC    PGROSS(12),PGROSS                                                
         BZ    LTSTIO                   ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   PRTPPD                                                           
         CLC   GROSS(12),PGROSS                                                 
         BE    LTSTIO              TOTALLY PAID SO ADD TO MTH ACCUMS            
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
*                                                                               
*              SUBTRACT PGROSS FROM GROSS AND PUT IN PGROSS SO THAT             
*              PGROSS,PAGYCOM,PCSHDSC,PAID WILL REFLECT UNPAID AMTS             
*                                                                               
         L     R3,GROSS                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,PGROSS                                                        
         L     R3,AGYCOM                                                        
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         L     R3,CSHDSC                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,PCSHDSC                                                       
         L     R3,PYABLE                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,PAID                                                          
******   L     R3,THISGST                                                       
******   L     R4,PAIDGST                                                       
******   SR    R3,R4                                                            
******   ST    R3,THISGST        PAYABLE                                        
ADDINGST L     R3,BUYGST                                              X         
         A     R3,PAID           INCLUDE GST IN PAYABLE                         
         ST    R3,PAID                                                          
         B     PRTPPD1                                                          
PRTPPD   CLI   QOPT1,C'U'                                                       
         BNE   LTSTIO       IF DOING ALL ITEMS - NO ADJUSTMENT                  
         MVC   P+35(18),=C'PREVIOUSLY CLEARED'                                  
         B     ADDINGST                                               X         
PRTPPD1  EDIT  PGROSS,(14,P+53),2,COMMAS=YES,MINUS=YES,FLOAT=$                  
         MVI   0(R1),C'('                                                       
         MVC   P+53(14),WORK+3                                                  
         MVI   P+66,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+66(2),=C'-)'                                                   
         LM    R3,R4,PGROSS                                                     
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+68(14),WORK+3                                                  
         MVI   P+81,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+81(2),=C'-)'                                                   
         EDIT  PCSHDSC,(11,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$                 
         MVI   0(R1),C'('                                                       
         MVC   P+83(11),WORK+6                                                  
         MVI   P+93,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+93(2),=C'-)'                                                   
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND21                                                         
         EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES,FLOAT=$      X           
         MVI   0(R1),C'('                                                       
         MVC   1(11,R3),WORK+6                                                  
         MVI   11(R3),C')'                                                      
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   11(2,R3),=C'-)'                                                  
         LA    R3,13(R3)                                              X         
         L     RF,THISGST   ORDERED GST                                         
         S     RF,BUYGST                                                        
         ST    RF,BUYGST    GET NET WHEN PAID/UNPAID REQ                        
NTCAND21 DS    0H                                                               
*                                                                               
         EDIT  PAID,(13,(R3)),2,COMMAS=YES,MINUS=YES,FLOAT=$       X            
         MVI   0(R1),C'('                                                       
*        MVC   P+94(14),WORK+3                                                  
*        MVI   P+107,C')'                                                       
         MVC   1(14,R3),WORK+3                                                  
         MVI   14(R3),C')'                                                      
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   14(2,R3),=C'-)'                                                  
         GOTO1 VPRINTIT                                                         
         L     R3,BUYGO                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,BUYGO                                                         
         L     R3,BUYGLAC                                                       
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,BUYGLAC                                                       
         L     R3,BUYCD                                                         
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,BUYCD                                                         
******** MVC   BUYGST,PAIDGST                                     ***1          
         L     R3,BUYNP                                                         
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,BUYNP                                                         
*                                                                               
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   LTSTIO                                                           
*                                                                               
*        NOW PRINT PORTION UNCLEARED                                            
*                                                                               
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
         CLI   QOPT1,C'U'                                                       
         BE    *+10                                                             
         MVC   P+35(2),SPACES     CHANGE TO "CLEARED"                           
*                                                                               
         EDIT  BUYGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         EDIT  BUYGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  BUYCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND3                                                          
         EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES               X          
         LA    R3,14(R3)                                                        
*                                                                               
NTCAND3  DS    0H                                                               
*                                                                               
         EDIT  BUYNP,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
*                                                                               
LTSTIO   DS    0H                                                               
         LA    R6,PBUYREC+33     CHECK FOR REF ELEM                             
         MVI   ELCODE,X'83'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST2                                                            
         MVC   P+28(4),=C'REF='                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'            ELCODE + LEN + 1 FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+32(0),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST2    LA    R6,PBUYREC+33     CHECK FOR SPECIAL REP ELEM                     
         MVI   ELCODE,X'80'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST4                                                            
         MVC   P+28(5),=C'SREP='                                                
         MVC   P+34(4),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST4    CLI   PROGPROF+1,C'Y'                                                  
         BNE   RLMTH                                                            
         XC    SV70ELM,SV70ELM                                                  
         LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
LTST5    BAS   RE,LNEXTEL                                                       
         BNE   LTST15                                                           
         OC    2(3,R6),2(R6)                                                    
         BZ    LTST5                                                            
         CLC   SV70ELM+2(3),2(R6)                                               
         BNL   LTST5                                                            
         MVC   SV70ELM(11),0(R6)                                                
         B     LTST5                                                            
*                                                                               
*                                                                               
*              SAVE FIRST 11 BYTES OF 70 ELEM                                   
SV70ELM  DS    CL11                                                             
ELCODE   DS    CL1                                                              
*                                                                               
LNEXTEL  DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R6),0                                                          
         BE    LNEXTELX                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                                                               
         B     LNEXTEL                                                          
LNEXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
LTST15   OC    SV70ELM,SV70ELM                                                  
         BZ    RLMTH                                                            
*        GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(0,P+38)                                
         GOTO1 DATCON,DMCB,(3,SV70ELM+2),(0,P+38)                               
         MVC   P+28(9),=C'LAST I/O='                                            
         MVC   P+37(1),PBUYREC+2                                                
         MVI   P+38,C'-'                                                        
         MVI   P+44,C'-'                                                        
         MVC   HALF,SV70ELM+5                                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+45(4),DUB                                                      
*                                                                               
         CLI   SV70ELM+10,C'N'                                                  
         BNE   LTST20                                                           
         MVC   P+50(3),=C'NEW'                                                  
         B     LTST30                                                           
*                                                                               
LTST20   CLI   SV70ELM+10,C'C'                                                  
         BNE   LTST25                                                           
         MVC   P+50(3),=C'CHA'                                                  
         B     LTST30                                                           
*                                                                               
LTST25   CLI   SV70ELM+10,C'D'                                                  
         BNE   LTST30                                                           
         MVC   P+50(3),=C'CAN'                                                  
*                                                                               
*LTST30   GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(3,P+54)                               
LTST30   GOTO1 DATCON,DMCB,(3,SV70ELM+2),(5,P+54)                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLMTH    DS    0H          PRINT UP TO 5 COMMENTS                               
         CLC   PBYOBFD(L'PBYOBFD),SPACES                                        
         BE    RL0                                                              
         MVC   P+28(L'PBYOBFD),PBYOBFD                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
RL0      LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RL1      CLC   0(47,R3),SPACES                                                  
         BE    RL2X                                                             
         MVC   P+28(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
         LA    R3,47(R3)                                                        
         BCT   R4,RL1                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
RL2X     DS    0H                                                               
         LA    R2,PPFILED+4095                                                  
         LA    R2,1(R2)                                                         
         USING PPFILED+4096,R2                                                  
         OC    PBDJOB,PBDJOB                                                    
         BZ    RLM2                                                             
         CLI   QOPT4,C' '                                                       
         BE    RLM2                                                             
         CLI   QOPT4,C'A'         AD NO ONLY                                    
         BE    RLM1A          NO NEED TO READ JOB REC                           
*                                                                               
*                                                                               
RLM1     DS    0H                                                               
         TM    QOPT4,X'30'                                                      
         BZ    RLM1A                                                            
         MVC   P+28(6),=C'COPY ='                                               
         MVC   P+28+7(17),PJOBCPY                                               
         B     RLM1B                                                            
*                                                                               
RLM1A    DS    0H                                                               
         MVC   P+28(8),=C'AD NO. ='                                             
         MVC   P+28+9(6),PBDJOB                                                 
*                                                                               
RLM1B    GOTO1 VPRINTIT                                                         
         TM    QOPT4,X'02'                                                      
         BZ    RLM2                                                             
         MVC   P+28(25),PJOBCAP1                                                
         MVC   PSECOND+28(25),PJOBCAP2                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLM2     CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    RLMTH1              NO - BYPASS INS TOTALS                       
         AP    MTHINS,=P'1'                                                     
RLMTH1   CLI   QMEDIA,C'N'         IF MAG                                       
         BNE   RLMTH2              BYPASS LINES                                 
RLM2A    CLI   PROGPROF,C'I'                                                    
         BNE   RLM2C                                                            
         CLI   PBDUIND,X'89'                                                    
         BE    RLMTH1B                                                          
         CLI   PBDUIND,C'I'                                                     
         BE    RLM2B                                                            
         L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'1000'                                                     
         DP    DUB,=P'14'                                                       
         SRP   DUB(6),63,5                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2B    L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'100'                                                      
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2C    CLI   PBDUIND,C'I'        CONVERT INCHES TO LINES FOR ACCUMS           
         BE    RLMTH1A                                                          
         CLI   PBDUIND,X'89'       CONVERT INCHES TO LINES FOR ACCUMS           
         BNE   RLMTH1B             LOWER CASE I INCHES TO 2 DECIMALS            
RLMTH1A  SR    R6,R6                                                            
         L     R7,UNITS                                                         
         M     R6,=F'14'                                                        
         ST    R7,UNITS                                                         
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   RLMTH1B             NO                                           
         CVD   R7,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R7,DUB                                                           
         ST    R7,UNITS                                                         
*                                                                               
RLMTH1B  EQU   *                                                                
         L     R3,UNITS                                                         
         CVD   R3,DUB                                                           
         AP    MTHLINES,DUB                                                     
RLMTH2   L     R3,BUYGO                                                         
         CVD   R3,DUB                                                           
         AP    MTHGO,DUB                                                        
         L     R3,BUYGLAC                                                       
         CVD   R3,DUB                                                           
         AP    MTHGLAC,DUB                                                      
         L     R3,BUYCD                                                         
         CVD   R3,DUB                                                           
         AP    MTHCD,DUB                                                        
         L     R3,GSTCOMM                                                       
         CVD   R3,DUB                                                           
         AP    MTHGST,DUB                                                       
         L     R3,BUYNP                                                         
         CVD   R3,DUB                                                           
         AP    MTHNP,DUB                                                        
*                                                                               
         MVC   SAVEYMD,BLBLDT                                                   
         CLI   QBPDATE,C'B'                                                     
         BE    NEXTBUY                                                          
         MVC   SAVEYMD,PBUYKDAT                                                 
         B     NEXTBUY                                                          
*                                                                               
*                                                                               
*                                                                               
NEXTBUY  DS    0H                                                               
         B     EXT                                                              
         EJECT                                                                  
INITIAL  GOTO1 =V(IINITIAL)                                                     
         BR    R9                                                               
         EJECT                                                                  
*              ROUTINE TO FLAG BILLED/TRAFFICED ITEMS                           
TSTBLTR  NTR                                                                    
         LA    R3,PBDELEM                                                       
TSTB1    CLI   0(R3),X'26'                                                      
         BNE   TSTB4                                                            
         OC    5(3,R3),5(R3)       CK FOR ANY DATE                              
         BZ    TSTB4                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB2                                                            
         MVI   P+1,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB2    CLC   5(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    NEXTBEL                                                          
         MVI   P+1,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB4    CLI   0(R3),X'70'                                                      
         BNE   NEXTBEL                                                          
         OC    2(3,R3),2(R3)                                                    
         BZ    NEXTBEL                                                          
         CLI   ASOFDTE,0                                                        
         BNE   TSTB6                                                            
         MVI   P+2,C'T'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB6    CLC   2(3,R3),ASOFDTE                                                  
         BH    NEXTBEL                                                          
         MVI   P+2,C'T'                                                         
*                                                                               
NEXTBEL  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0         END OF REC                                       
         BNE   TSTB1                                                            
*                                                                               
TSTBX    XIT                                                                    
EXT      XMOD1 1                                                                
THISGST  DS    F ORDERED GST          X                                         
PAIDGST  DS    F PAID GST             X                                         
UPAIDGST DS    F UNPAID GST           X                                         
GSTCOMM  DS    F DEPENDING ON OPTION COULD BE UNPAID /PAID            X         
         LTORG                                                                  
FWORK    DS    20F                                                              
         EJECT                                                                  
*                                                                               
PROFRST  NMOD1 0,PROFRST                                                        
*                                                                               
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PRDFX               SKIP PRD PRINT                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'*'        OFFICE REQS                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'&&'       GROUP REQS                                   
         BE    PRDF                                                             
         B     PRDF0               ALL PRDS - PRD WILL BE IN TABLE              
*                                                                               
PRDF     DS    0H                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   PRDF3               YES- MUST READ PRODUCT                       
PRDF0    L     R6,APRDTAB                                                       
PRDF1    CLC   0(3,R6),=X'FFFFFF'                                               
         BNE   *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         CLC   0(3,R6),PBUYKPRD                                                 
         BE    PRDF4                                                            
         LA    R6,23(R6)                                                        
         B     PRDF1                                                            
*                                                                               
*                                                                               
PRDF3    DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,PDIRRD                                                        
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         TM    KEY+25,X'80'                                                     
         BZ    *+6                                                              
         DC    H'0'                FATAL ERROR PRD IS DELETED                   
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,PFILERD                                                       
PRDF3C   DS    0H                                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,PDIRRD                                                        
         B     PRDF5                                                            
*                                                                               
PRDF4    MVC   PPRDNAME(20),3(R6)                                               
         MVC   PPRDKPRD(3),PBUYKPRD                                             
*                                                                               
PRDF5    DS    0H                                                               
         MVC   P+1(7),=C'PRODUCT'                                               
         MVC   P+9(3),PBUYKPRD                                                  
         MVC   P+14(20),PPRDNAME                                                
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
PRDF8    MVC   SAVEPRD,PBUYKPRD                                                 
*                                                                               
PRDFX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
PDIRRD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRX                                                            
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    PDIRX                                                            
         DC    H'0'                                                             
PDIRX    XIT                                                                    
*                                                                               
*                                                                               
PFILERD  NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    PFILX                                                            
         DC    H'0'                                                             
PFILX    XIT                                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PRNTPUB  CSECT                                                                  
         NMOD1 0,PRNTPUB                                                        
         USING PPWORKD,RA                                                       
         USING ASWORKD,R5,R9                                                    
         USING PPFILED,RC                                                       
         L     RC,PPFILEC                                                       
         L     R5,VASWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         CLI   QOPT2,C'Y'     YES= SKIP ON NEW PUB                              
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'6'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         MVC   P+1(17),SPACES                                                   
         MVI   PUBPSW,0                                                         
         MVC   SAVPLIN1,SPACES                                                  
         MVC   SAVPLIN2,SPACES                                                  
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(0,SAVPLIN1+1)                      
         CLI   FORCEHED,C'Y'                                                    
         BE    SETR3               DONT SKIP A LINE                             
         GOTO1 VPRINTIT                                                         
SETR3    LA    R3,SAVPLIN1+1                                                    
         LA    R4,SAVPLIN2+1                                                    
         SR    R9,R9                                                            
SETR3A   CLI   0(R3),C' '                                                       
         BE    FLOAT                                                            
         MVI   0(R4),C'-'                                                       
         LA    R9,1(R9)                                                         
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     SETR3A                                                           
*                                                                               
FLOAT    LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R9,1(R9)            SAVE DISPLACEMENT                            
         GOTO1 VPUBFLOT,DMCB,PUBREC,(R3)                                        
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         MVC   0(11,R4),=C'(CONTINUED)'                                         
*                                                                               
         LA    R3,P+1                                                           
         AR    R3,R9                                                            
         LA    R4,PSECOND+1                                                     
         AR    R4,R9                                                            
*                                                                               
         MVC   SAVEPUB,PBUYKPUB                                                 
         CLI   QOPT6,C'Y'          SEE IF PRINTING PAYING ADDR                  
         BE    PRNTP1                                                           
         CLC   QSORT,=C'08'         OR REP SORT                                 
         BE    PRNTP1                                                           
         CLC   QSORT,=C'09'         OR PAY ADDR NAME                            
         BE    PRNTP1                                                           
         GOTO1 VPRINTIT                                                         
         B     PRNTPX                                                           
*                                                                               
PRNTP1   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNTP2                                                           
         IC    R0,SAVELINE                                                      
         CLC   QSORT,=C'08'        SEE IF SORTING BY REP                        
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDRS                         
         BNE   PRNTP1C                                                          
         AH    R0,=H'4'                                                         
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    *+8                                                              
         AH    R0,=H'1'                                                         
PRNTP1C  STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
PRNTP2   DS    0H                                                               
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    PRNTP2C             YES                                          
         MVC   0(20,R4),=C'** PAY PUB DIRECT **'                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7                                                           
         MVC   0(30,R3),PUBLINE1   P                                            
         MVC   0(30,R4),PUBLINE2   PSECOND                                      
         GOTO1 VPRINTIT                                                         
         LA    R6,PUBREC+33                                                     
PRNTP2A  CLI   0(R6),X'11'                                                      
         BE    PRNTP2B                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    PRNTP7              NO ATTN OR TELE                              
         B     PRNTP2A                                                          
*                                                                               
PRNTP2B  DS    0H                                                               
         USING PUBSADEL,R6                                                      
         LR    R4,R3                                                            
         CLI   PUBATTN,C' '                                                     
         BNH   PRNTP2B2                                                         
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(24,R4),PUBATTN                                                 
         LA    R4,31(R4)                                                        
*                                                                               
PRNTP2B2 CLI   PUBTEL,C' '                                                      
         BNH   PRNTP6              NO TELE                                      
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PUBTEL                                                  
         B     PRNTP6                                                           
*                                                                               
         DROP  R6                                                               
         GOTO1 VPRINTIT                                                         
         B     PRNTP7                                                           
*                                                                               
PRNTP2C  DS    0H                                                               
         CLC   QSORT,=C'08'        SEE IF DOING REP SORT                        
         BE    PRNTP2C5                                                         
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BE    PRNTP2C5                                                         
         B     PRNTP2F                                                          
*                                                                               
PRNTP2C5 CLI   PREPKEY+3,X'11'     SEE IF I FOUND A REP                         
         BNE   PRNTP2F             NO WAS PAY ADDR                              
         MVC   0(4,R4),=C'REP='                                                 
         MVC   5(4,R4),PREPKREP                                                 
         CLI   PREPKREP+4,0                                                     
         BE    PRNTP2D                                                          
         MVI   9(R4),C'.'                                                       
         MVC   10(1,R4),PREPKREP+4    SUFFIX                                    
PRNTP2D  GOTO1 VPRINTIT                                                         
         LR    R4,R3                                                            
*                                                                               
PRNTP2F  MVC   0(30,R4),PREPNAME                                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7              NO THEN DONE                                 
         CLI   PREPLIN1,C' '                                                    
         BNH   PRNTP3                                                           
         MVC   0(30,R3),PREPLIN1                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP3   CLI   PREPLIN2,C' '                                                    
         BNH   PRNTP4                                                           
         MVC   0(30,R3),PREPLIN2                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP4   LR    R4,R3                                                            
         CLI   PREPATTN,C' '                                                    
         BNH   PRNTP5                                                           
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(20,R4),PREPATTN                                                
         LA    R4,27(R4)                                                        
*                                                                               
PRNTP5   CLI   PREPTEL,C' '                                                     
         BNH   PRNTP6              NO TELEPHONE                                 
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PREPTEL                                                 
*                                                                               
PRNTP6   CLC   P,SPACES                                                         
         BE    PRNTP7                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP7   DS    0H                                                               
         GOTO1 VPRINTIT                                                         
PRNTPX   DS    0H                                                               
         MVI   PUBPSW,1            SET PUB PRINTED                              
         XIT1                                                                   
*                                                                               
         DROP  R7,R5,R9                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
IINITIAL CSECT                                                                  
         NMOD1 0,INITL                                                          
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         USING ASWORKD,R5,R6                                                    
         XC    MTHTAB(250),MTHTAB       LINKED VIA R9.                          
         XC    MTHTAB+250(L'MTHTAB-250),MTHTAB+250                              
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         L     RF,=V(MTHEND)           RELOCATE ROUTINES                        
         A     RF,RELO                                                          
         ST    RF,VMTHEND                                                       
         L     RF,=V(PRDEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPRDEND                                                       
         L     RF,=V(PUBEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBEND                                                       
         L     RF,=V(CLTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTEND                                                       
         L     RF,=V(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(BLDMLST)                                                   
         A     RF,RELO                                                          
         ST    RF,VBLDMLST                                                      
         L     RF,=V(ASWORK)                                                    
         A     RF,RELO                                                          
         ST    RF,VASWORK                                                       
         L     RF,=V(CLIF)                                                      
         A     RF,RELO                                                          
         ST    RF,VCLIFRST                                                      
         L     RF,=V(PUBF)                                                      
         A     RF,RELO                                                          
         ST    RF,VPUBFRST                                                      
         L     RF,=V(PUBFLOAT)                                                  
         A     RF,RELO                                                          
         ST    RF,VPUBFLOT                                                      
         L     RF,=V(OFFOUT)                                                    
         A     RF,RELO                                                          
         ST    RF,VOFFOUT                                                       
         L     RF,=V(REPEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VREPEND                                                       
         L     RF,=V(REPCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VREPCSCT                                                      
         L     RF,=V(CLTCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VCLTCSCT                                                      
         L     RF,=V(PROCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPROCSCT                                                      
         L     RF,=V(PRNTPUB)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTPUB                                                      
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R6,PBUYREC                                                       
         L     R7,DATCON                                                        
         LA    R8,GROSS                                                         
         STM   R6,R8,0(R2)                                                      
         MVI   PBYOCTL,X'28'                                                    
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   FCRDBUY,C'Y'        RESET TO READ 20 POINTERS                    
         MVI   FCRDACTV,C'Y'       SET TO READ ACTIVE ONLY                      
         MVI   FCGTJOB,C'N'        RESET JOB RECORD READ                        
         MVI   PRDSW,1                                                          
         CLC   QPRODUCT(3),=C'   '                                              
         BNE   *+12                                                             
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
         MVI   PRDSW,0                                                          
         JIF   QOPT4,EQ,C' ',OR,QOPT4,EQ,C'A',INIT1,JUMP=N                      
         MVI   FCGTJOB,C'Y'        SO PPG WILL GET JOBRECS                      
*                                                                               
INIT1    DS    0H                                                               
         MVI   FCGTREP,C'N'                                                     
         CLI   QOPT6,C'Y'          SEE IF SHOWING PAYING ADDRESS                
         BE    INIT2                                                            
         CLC   QSORT,=C'08'        OR SORTING BY REP                            
         BE    INIT2                                                            
         CLC   QSORT,=C'09'        OR SORTING BY PAY ADDR NAME                  
         BE    INIT2                                                            
         B     INIT2B                                                           
INIT2    MVI   FCGTREP,C'Y'                                                     
INIT2B   CLI   QPAY,C' '           SEE IF USING AS OF DATE                      
         BE    INIT3                                                            
         MVC   DUB(2),RCDATE+6                                                  
         MVC   DUB+2(2),RCDATE+0                                                
         MVC   DUB+4(2),RCDATE+3                                                
         CLC   QPAY(6),DUB         SEE IF AS OF DATE IS TODAY                   
         BNL   INIT3               OR FUTURE - YES CAN USE FCPDFILT             
         B     INIT4                                                            
*                                                                               
INIT3    MVC   FCPDFILT,QOPT1      P=PAID,U=UNPAID                              
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
INIT4    MVI   FCPDFILT,C'N'       RESET TO N                                   
         MVC   PAGE,=H'1'                                                       
         MVI   REQERR,0                                                         
         CLC   SVMEDCLI,QMEDIA     CHK FOR SAME MEDIA/CLIENT                    
         BNE   FRCLI                                                            
INOUTX   XIT                                                                    
FRCLI    DS    0H                  FIRST REQUEST FOR CLIENT                     
         L     R6,APRDTAB                                                       
         CLC   QCLIENT,=C'ALL'                                                  
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'*'                                                     
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'$'            OFFICE LIST                              
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'&&'           GROUP                                    
         BE    FRCLI1                                                           
         CLC   QPRODUCT,=C'ALL'        ONLY BUILD TABLE FOR ALL PRDS            
         BE    FRCLI1C                                                          
         CLC   QPRODUCT,=C'   '                                                 
         BE    FRCLI1C                                                          
FRCLI1   MVC   0(3,R6),=X'FFFFFF'                                               
         XC    SVMEDCLI,SVMEDCLI                                                
         B     FRCLIX              PRD TABLE WILL BE BUILT AT CLTFRST           
*                                  IF NECESSARY                                 
*                                                                               
FRCLI1C  DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG'S KEY                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     FRCLI5                                                           
*                                                                               
FRCLI2   LA    R4,DMRSEQ                                                        
FRCLI5   DS    0H                                                               
******   BAS   RE,PDIRRD                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRXX1                                                          
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    PDIRXX1                                                          
         DC    H'0'                                                             
PDIRXX1  CLC   KEY(7),KEYSAVE                                                   
         BNE   FRCLI20                                                          
         TM    KEY+25,X'80'                                                     
         BO    FRCLI2              BYPASS DELETED PRDS                          
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
*****    BAS   RE,PFILERD                                                       
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     FRCLI2                                                           
*                                                                               
FRCLI20  MVC   0(3,R6),=X'FFFFFF'                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
*******  BAS   RE,PDIRRD                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRXX                                                           
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    PDIRXX                                                           
         DC    H'0'                                                             
PDIRXX   MVC   SVMEDCLI,QMEDIA     SAVE MEDIA/CLIENT                            
FRCLIX   B     INOUTX                                                           
         LTORG                                                                  
         EJECT                                                                  
BLDMLST  CSECT                                                                  
         NMOD1 0,BLDMLST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
SPRDOK   MVC   WORK(12),QSTART       SAVE STRT AND END                          
         CLI   QBPDATE,C'B'                                                     
         BE    BILLPAY                                                          
         CLI   QBPDATE,C'P'                                                     
         BE    BILLPAY                                                          
BLDLIST  LA    R6,ACCNUM                SET FOR BCT                             
         LA    R4,MTHTAB                                                        
         MVC   0(4,R4),QSTART                                                   
PUTMTH   PACK  DUB,2(2,R4)                                                      
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'13'                                                       
         BL    SAMEYR                                                           
         SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         PACK  DUB,0(2,R4)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'           ADD 1 TO YR                                  
*        OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R4),DUB+6(2)                                                 
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE             EXIT                                         
         XC    8(8,R4),8(R4)       CLEAR LAST MTH                               
         B     MTHDONE             EXIT                                         
*                                                                               
SAMEYR   OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         MVC   8(2,R4),0(R4)                                                    
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE                                                          
         XC    8(8,R4),8(R4)                                                    
         B     MTHDONE             EXIT                                         
*                                                                               
NEXTMTH  LA    R4,8(R4)                                                         
         BCT   R6,PUTMTH           ACCNUM MTHS MAX                              
         B     MTHDONE             EXIT                                         
*                                                                               
*                                                                               
*                                                                               
BILLPAY  PACK  DUB,QSTART+2(2)     SET START DATE BACK 3 MONTHS                 
         SP    DUB,=P'3'                                                        
         CP    DUB,=P'0'                                                        
         BNH   CHGSYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         B     CHGEND                                                           
*                                                                               
CHGSYR   AP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         PACK  DUB,QSTART(2)                                                    
         BAS   RE,BLDMDNYR         SUBTRACT 1 FROM YR                           
*        SP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QSTART(2),DUB+6(2)                                               
*                                                                               
CHGEND   PACK  DUB,QEND+2(2)                                                    
         AP    DUB,=P'6'      ADVANCE END DATE 6 MONTHS                         
         CP    DUB,=P'12'                                                       
         BH    CHGEYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         B     BLDLIST                                                          
*                                                                               
CHGEYR   SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         PACK  DUB,QEND(2)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QEND(2),DUB+6(2)                                                 
         B     BLDLIST                                                          
*                                                                               
MTHDONE  MVC   QSTART(12),WORK    RESTORE DATES                                 
*                                                                               
         XC    ASOFDTE,ASOFDTE                                                  
         CLI   QPAY,C' '           AS OF DATE IN QPAY YYMMDD                    
         BE    MTHD1                                                            
*        GOTO1 DTCNV,DMCB,(0,QPAY),(1,ASOFDTE)                                  
         GOTO1 DATCON,DMCB,(0,QPAY),(3,ASOFDTE)                                 
*        GOTO1 DTCNV,DMCB,(1,ASOFDTE),(3,ASDATE)                                
         GOTO1 DATCON,DMCB,(3,ASOFDTE),(5,ASDATE)                               
MTHD1    DS    0H                  NOW CLEAR ALL ACCUMS                         
         LA    R6,7                WAS 6                                        
         LA    R4,MTHTOTS                                                       
CLRMTH   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRMTH                                                        
         LA    R6,7                WAS 6                                        
         LA    R4,PRDTOTS                                                       
CLRPRDA  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRDA                                                       
         LA    R6,ACCNUM*7         WAS 6                                        
         LA    R4,PUBTOTS                                                       
CLRPUB   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPUB                                                        
*                                                                               
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R6,ACCNUM*8         WAS 7                                        
         LA    R4,REPTOTS                                                       
CLRREP   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRREP                                                        
         XC    RTOTPUBS,RTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
         LA    R6,ACCNUM*8         WAS 7                                        
         LA    R4,PROTOTS                                                       
CLRPRO   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRO                                                        
         XC    PTOTPUBS,PTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         LA    R6,ACCNUM*8         WAS 7                                        
         LA    R4,CLTTOTS                                                       
CLRCLT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRCLT                                                        
         XC    CTOTPUBS,CTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
         MVI   CLTACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
*        GOTO1 DTCNV,DMCB,(0,QSTART),(1,REQST)                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQST)                                 
*        GOTO1 DTCNV,DMCB,(0,QEND),(1,REQEND)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQEND)                                  
*                                                                               
*                                                                               
BLMEXT   XIT1                                                                   
****  INCREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." FORMATS               
*                                                                               
BLDMUPYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'90'         YR ENDING IN 9 ??                            
         BO    BMCYUP              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         AP    DUB,=P'1'           INCREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMUPX             DONE WITH YR NOT ENDING IN 9                 
*                                                                               
BMCYUP   DS    0H                                                               
         MVI   DUB+7,X'0F'         LO-ORDER OF YR MUST BE 0 HERE                
         ZIC   R0,STRHIYR                                                       
         AH    R0,=H'1'            INCREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMUPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
****  DECREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." FORMATS               
*                                                                               
BLDMDNYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'F0'         YR ENDING IN 0 ??                            
         BZ    BMCYDN              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         SP    DUB,=P'1'           DECREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMDNX             DONE WITH YR NOT ENDING IN 0                 
*                                                                               
BMCYDN   DS    0H                                                               
         MVI   DUB+7,X'9F'         LO-ORDER OF YR MUST BE 9 HERE                
         ZIC   R0,STRHIYR                                                       
         SH    R0,=H'1'            DECREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMDNX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         MVI   RCSUBPRG,4                                                       
         CLI   PROGPROF,C'I'                                                    
         BNE   PRINT1                                                           
         MVI   RCSUBPRG,6                                                       
*                                                                               
PRINT1   DS    0H                                                               
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BNE   PRINT1B                                                          
         MVC   HEAD3(11),=C'OFFICE LIST'                                        
         MVC   HEAD3+12(1),QCLIENT+1                                            
*****    GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,(C'L',HEAD3+12)                    
         B     PRINT1C                                                          
*                                                                               
PRINT1B  CLI   QCLIENT,C'*'        SEE IF OFFICE REQUEST                        
         BNE   PRINT1D                                                          
PRINT1C  MVC   HEAD4(6),=C'OFFICE'                                              
*****    MVC   HEAD4+7(1),PCLTOFF                                               
         MVC   HEAD4+9(2),SVPTOFC   SVPTOFC IS OUTPUT FROM OFFOUT CALL          
*****    GOTO1 VOFFOUT,DMCB,PCLTOFF,HEXOUT,(C'L',HEAD4+7)                       
         B     PRINT1X                                                          
*                                                                               
PRINT1D  CLI   QCLIENT,C'&&'       GROUP                                        
         BNE   PRINT1X                                                          
         MVC   HEAD4(5),=C'GROUP'                                               
         MVC   HEAD4+6(1),PCLTBLGP                                              
         B     PRINT1X                                                          
*                                                                               
PRINT1X  CLI   PRDSW,0             SEE IF DOING PRDS SEPARATELY                 
         BE    CKMED                                                            
         CLC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         BE    CKMED                                                            
         MVC   HEAD6(7),=C'PRODUCT'                                             
         MVC   HEAD6+9(3),PPRDKPRD                                              
         MVC   HEAD6+13(20),PPRDNAME                                            
CKMED    CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
CKEST    CLC   QEST,=C'ALL'                                                     
         BE    CKESTX                                                           
         CLC   QEST,=C'   '                                                     
         BE    CKEST2                                                           
*                                                                               
CKEST1   ZIC   R2,RCSUBPRG                                                      
         LA    R2,1(R2)                                                         
         STC   R2,RCSUBPRG         SET SPROG - TO PRINT EST FROM PPG            
         B     CKESTX                                                           
*                                                                               
CKEST2   CLC   QESTEND,=C'   '                                                  
         BE    CKESTX                                                           
         B     CKEST1              PRINT FILTERS                                
*                                                                               
CKESTX   DS    0H                                                               
         CLC   P+2(9),=C'** CLIENT'        SEE IF DOING CLT TOTS                
         BE    PRINT2                                                           
         CLC   P+1(10),=C'** PRODUCT'        DOING PRD TOTAL LINE               
         BNE   PRINT4                        FOR PRODUCTS SEPARATELY            
PRINT2   MVI   RCSUBPRG,0                                                       
         MVC   HEAD10+5(7),=C'BILLING'                                          
         CLI   QBPDATE,C'B'                                                     
         BE    PRINT4                                                           
         MVC   HEAD10+4(9),=C'INSERTION'                                        
*                                                                               
PRINT4   MVC   HEAD8(26),=C'** CASH DISC. PUBS ONLY **'                         
         CLI   QOPT3,C'C'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(30),=C'** NON CASH DISC. PUBS ONLY **'                     
         CLI   QOPT3,C'N'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(33),SPACES                                                 
CKPUP    MVC   HEAD8+75(26),=C'** UNCLEARED ITEMS ONLY **'                      
         CLI   QOPT1,C'U'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),=C' ** CLEARED ITEMS ONLY ** '                      
         CLI   QOPT1,C'P'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),SPACES                                              
CKBP     CLI   QBPDATE,C'B'                                                     
         BE    BILMSG                                                           
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+43(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+43(20),=C'** BILLING PERIOD **'                            
CKASOF   CLI   ASOFDTE,0                                                        
         BE    PRINTX         AS OF DATE NOT USED                               
         MVC   HEAD5+45(5),=C'AS OF'                                            
         MVC   HEAD5+51(8),ASDATE                                               
PRINTX   CLI   PAGYNAT,C'C'                                                     
         BNE   PRINTX1                                                          
         ZIC   RF,RCSUBPRG                                                      
         LA    RF,8(RF)                                                         
         STC   RF,RCSUBPRG                                                      
PRINTX1  CLC   LINE,MAXLINES                                                    
         BL    PRINTX5                                                          
         CLI   PUBPSW,1            SEE IF PUB NAME ALREADY PRINTED              
         BNE   PRINTX5             NO - DONT PRINT CONTINUED MESSAGE            
         MVC   SAVEP,P                                                          
         MVC   SAVPSEC,PSECOND                                                  
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         GOTO1 REPORT                                                           
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVPSEC                                                  
PRINTX5  GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
SAVEP    DS    CL132                                                            
SAVPSEC  DS    CL132                                                            
         EJECT                                                                  
MTHEND   CSECT                                                                  
         NMOD1 0,MTHEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         CLI   MTHACT,C'Y'                                                      
         BNE   MTHENDX             NO ACTIVITY                                  
         CLI   QBPDATE,C'B'            THEN NO MTH BREAKS                       
         BE    MTHEND2C                                                         
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(5,P+5)                                   
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(9,P+5)                                  
         MVC   P+11(6),=C'-TOTAL'                                               
*                                                                               
         CP    MTHINS,=P'0'                                                     
         BNH   MTHEND1                                                          
         EDIT  MTHINS,(3,P+28),0                                                
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    MTHINS,=P'1'                                                     
         BNE   MTHEND1                                                          
         MVI   P+41,C' '                                                        
MTHEND1  CLI   QMEDIA,C'N'                                                      
         BNE   MTHEND2             OMIT LINES FOR MAGS + SUPS                   
         CP    MTHLINES,=P'0'                                                   
         BZ    MTHEND2             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   MTHEND1A                                                         
         EDIT  MTHLINES,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     MTHEND2                                                          
MTHEND1A EDIT  MTHLINES,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
MTHEND2  DS    0H                                                               
         EDIT  MTHGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    MTHGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  MTHGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    MTHGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  MTHCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    MTHCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND4                                                          
         EDIT  MTHGST,(12,(R3)),2,COMMAS=YES,MINUS=YES                          
         MVI   12(R3),C'*'                                                      
         CP    MTHGST,=P'0'                                                     
         BL    *+10                                                             
         MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                                        
NTCAND4  DS    0H                                                               
*                                                                               
         EDIT  MTHNP,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI  14(R3),C'*'                                                       
         CP    MTHNP,=P'0'                                                      
         BL    *+10                                                             
*        MVI   P+107,C'*'                                                       
         MVC   13(2,R3),=C'* '                                                  
         ZIC   R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   MTHEND2B                                                         
         GOTO1 VPRINTIT                                                         
*                                                                               
MTHEND2B CLI   PRDSW,1                SEE IF DOING PRDS SEPERATELY              
         BE    MTHEND3C                                                         
*                                                                               
*    ROLL TO PRD TOTALS                                                         
*                                                                               
MTHEND2C L     R3,PRDMTHS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PRDMTHS                                                       
         LA    R3,PRDINS                                                        
         LA    R4,MTHINS                                                        
         LA    R6,7                WAS 6                                        
MTHEND3  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,MTHEND3                                                       
*                                                                               
*     ROLL TO PUB ACCUMS                                                        
MTHEND3C L     R3,PUBMTHS                                                       
         AH    R3,=H'1'                                                         
         ST    R3,PUBMTHS                                                       
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(0,WORK)                                 
         LA    R3,MTHTAB                                                        
         LA    R4,0                                                             
COMPDAT  CLC   WORK(4),0(R3)                                                    
         BE    MTHEND4                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLC   0(4,R3),=4X'00'                                                  
         BNE   COMPDAT                                                          
         DC    H'0'                MTH NOT FOUND IN LIST                        
*                                                                               
MTHEND4  LA    R3,7                WAS 6                                        
         LA    R6,PUBINS                                                        
         LA    R7,MTHINS                                                        
         LA    R6,0(R4,R6)                                                      
MTHEND5  AP    0(8,R6),0(8,R7)                                                  
         LA    R6,ACCNUM*8(R6)                                                  
         LA    R7,8(R7)                                                         
         BCT   R3,MTHEND5                                                       
         LA    R3,7                WAS 6                                        
         LA    R6,MTHTOTS               CLEAR MTH ACCUMS                        
MTHEND8  ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R3,MTHEND8                                                       
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
MTHENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRDEND   CSECT                                                                  
         NMOD1 0,PRDEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,1              SEE  IF DOING PRDS SEPERATELY               
         BE    PRDEND10                                                         
*                                                                               
         L     R3,PUBPRDS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PUBPRDS                                                       
         CLI   QBPDATE,C'B'        SEE IF DOING BILLABLE DATES                  
         BE    PRDEND0                                                          
         CLC   PRDMTHS,=F'1'                                                    
         BNH   CLRPRD                                                           
*                                                                               
PRDEND0  MVC   P+2(20),=C'** PRODUCT TOTALS **'                                 
         CP    PRDINS,=P'0'                                                     
         BE    NOINS               NO INSERTIONS                                
         EDIT  PRDINS,(3,P+28),0                                                
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    PRDINS,=P'1'                                                     
         BNE   *+8                                                              
         MVI   P+41,C' '                                                        
NOINS    CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND1                  SKIP LINES                              
         CP    PRDLINES,=P'0'                                                   
         BE    PRDEND1                                                          
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDENDA                                                          
         EDIT  PRDLINES,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND1                                                          
PRDENDA  EDIT  PRDLINES,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
PRDEND1  DS    0H                                                               
         EDIT  PRDGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    PRDGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  PRDGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    PRDGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  PRDCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    PRDCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND5                                                          
         EDIT  PRDGST,(12,(R3)),2,COMMAS=YES,MINUS=YES                          
         MVI   P+106,C'*'                                                       
         CP    PRDGST,=P'0'                                                     
         BL    *+10                                                             
         MVC   P+105(2),=C'* '                                                  
         LA    R3,14(R3)                                                        
NTCAND5  DS    0H                                                               
*                                                                               
         EDIT  PRDNP,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    PRDNP,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
*        MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   PRDEND8                                                          
         GOTO1 VPRINTIT                                                         
PRDEND8  B     CLRPRD                                                           
*                                                                               
PRDEND10 DS    0H                  PRODUCTS SEPARATELY                          
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
         GOTO1 VPUBEND                                                          
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(20),=C'** PRODUCT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
PRDEND15 LA    R2,PROINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                WAS 6                                        
PRDEND17 CP    0(8,R2),=P'0'                                                    
         BNE   PACTV                                                            
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,PRDEND17                                                      
PRDEND18 LA    R4,8(R4)                                                         
         BCT   R8,PRDEND15                                                      
         B     PRDEND20                                                         
*                                                                               
PACTV    LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R1,PROPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         EDIT  (P8,(R1)),(4,P+15),0                                             
         MVC   P+20(4),=C'PUBS'                                                 
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         LA    R1,PROINS                                                        
         LA    R1,0(R4,R1)                                                      
         CP    0(8,R1),=P'0'                                                    
         BE    NOPINS                                                           
         EDIT  (P8,(R1)),(5,P+24),0                                             
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PACTV5                                                           
         CP    0(8,R1),=P'0'                                                    
         BE    PACTV5                                                           
         CLI   PROGPROF,C'I'                                                    
         BNE   PACTV3                                                           
         EDIT  (P8,(R1)),(9,P+42),2                                             
         MVI   P+51,C'I'                                                        
         B     PACTV5                                                           
PACTV3   EDIT  (P8,(R1)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PACTV5   LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND6                                                          
         EDIT  (P8,(R1)),(12,(R3)),2,COMMAS=YES,MINUS=YES                       
         LA    R3,14(R3)                                                        
NTCAND6  DS    0H                                                               
         LA    R1,ACCNUM*8(R1)    BUMP PAST GST                                 
*                                                                               
         EDIT  (P8,(R1)),(14,(R3)),2,COMMAS=YES,MINUS=YES                       
         GOTO1 VPRINTIT                                                         
         B     PRDEND18                                                         
*                                                                               
*                                                                               
PRDEND20 MVC   P+5(5),=C'TOTAL'                                                 
         L     R2,PTOTPUBS                                                      
         EDIT  (R2),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         ZAP   WKDUB,PROINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,PROINS+8                                                      
PRDEND22 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,PRDEND22                                                      
         CP    WKDUB,=P'0'                                                      
         BE    NOPTINS                                                          
         EDIT  WKDUB,(5,P+24),0                                                 
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPTINS  DS    0H                                                               
         LA    R6,PROLINES                                                      
         LA    R7,6                WAS 5                                        
         LA    R8,TOTALS                                                        
PRDEND23 ZAP   DUB,0(8,R6)                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,8(R6)                                                         
PRDEND24 ZAP   DOUBLE,0(8,R4)                                                   
         AP    DUB,DOUBLE                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PRDEND24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PRDEND23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND26                                                         
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDEND25                                                         
         EDIT  (P8,0(R2)),(10,P+41),2                                           
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND26                                                         
PRDEND25 EDIT  (P8,0(R2)),(7,P+43),0                                            
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
*                                                                               
PRDEND26 LA    R2,8(R2)                                                         
         EDIT  (P8,00(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  (P8,08(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                     
         CP    08(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  (P8,16(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                     
         CP    16(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND7                                                          
         EDIT  (P8,24(R2)),(12,(R3)),2,COMMAS=YES,MINUS=YES                     
         LA    R3,14(R3)                                                        
NTCAND7  DS    0H                                                               
*                                                                               
         EDIT  (P8,32(R2)),(14,(R3)),2,COMMAS=YES,MINUS=YES                     
         MVI   14(R3),C'*'                                                      
         CP    32(8,R1),=P'0'                                                   
         BL    *+10                                                             
*        MVI   P+107,C'*'                                                       
         MVC   13(2,R3),=C'* '                                                  
         GOTO1 VPRINTIT                                                         
         LA    R3,PROTOTS                                                       
         LA    R4,ACCNUM*8         WAS 7                                        
PRDEND30 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,PRDEND30                                                      
         XC    PTOTPUBS,PTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
CLRPRD   DS    0H                                                               
         LA    R3,7               WAS 6                                         
         LA    R4,PRDTOTS                                                       
CLRPRD5  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLRPRD5                                                       
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
PRDENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBEND   CSECT                                                                  
         NMOD1 0,PUBEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         MVI   PUBSW,0                                                          
         CLI   PUBACT,C'Y'                                                      
         BNE   PUBEND30                 NO ACTIVITY                             
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,1                  SEE IF DOING PRDS SEPERATELY            
         BE    PUBEND3                                                          
*                                                                               
         GOTO1 VPRDEND                                                          
         CLC   PUBPRDS,=F'1'                                                    
         BNH   PUBROLL                                                          
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         MVC   P+2(16),=C'** PUB TOTALS **'                                     
         GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBEND1  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                                        
PUBEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,PRTMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBEND1                                                       
         B     PUBEND3                                                          
*                                                                               
PRTMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LA    R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,PUBINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    NOPPINS             NO INSERTIONS                                
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    PRTMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRTMTHA                                                          
         EDIT  (P8,(R2)),(9,P+41),2                                             
         MVI   P+50,C'I'                                                        
         B     PRTMTH1                                                          
PRTMTHA  EDIT  (P8,(R2)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PRTMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         LA    RF,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND8                                                          
         EDIT  (P8,(R2)),(12,(RF)),2,COMMAS=YES,MINUS=YES                       
         LA    RF,14(RF)                                                        
NTCAND8  DS    0H                                                               
         LA    R2,ACCNUM*8(R2)                                                  
*                                                                               
         PRINT GEN                                                              
         EDIT  (P8,(R2)),(14,(RF)),2,COMMAS=YES,MINUS=YES                       
         PRINT NOGEN                                                            
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
PUBEND3  CLI   PRDSW,0              SEE IF DOING PRDS SEPERATELY                
         BE    PUBEND3C                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    PUBEND3D                                                         
*                                                                               
PUBEND3C CLC   PUBMTHS,=F'1'                                                    
         BNH   PUBROLL                                                          
PUBEND3D MVC   P+5(5),=C'TOTAL'                                                 
         CLI   PRDSW,0                                                          
         BE    *+10                                                             
         MVC   P+2(15),=C'** PUB TOTAL **'                                      
         ZAP   WKDUB,PUBINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,PUBINS                                                        
PUBEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BZ    NOPPTINS            NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPTINS CLI   QMEDIA,C'N'                                                      
         BNE   PUBEND6                                                          
         ZAP   WKDUB,PUBLINES                                                   
         LA    R2,PUBLINES                                                      
         LA    R6,ACCNUM-1                                                      
PUBEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    PUBEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PUBEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PUBEND6                                                          
PUBEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PUBEND6  ZAP   WKDUB,PUBGO                                                      
         LA    R2,PUBGO                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,PUBGLAC                                                    
         LA    R2,PUBGLAC                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,PUBCHCD                                                    
         LA    R2,PUBCHCD                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND9                                                          
         ZAP   WKDUB,PUBGSTX                                                    
         LA    R2,PUBGSTX                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND1A AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND1A                                                      
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   12(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                                        
NTCAND9  DS    0H                                                               
*                                                                               
         ZAP   WKDUB,PUBNP                                                      
         LA    R2,PUBNP                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND10                                                      
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   PUBROLL                                                          
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
*     ROLL TO CLT TOTALS                                                        
PUBROLL  L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
*                                                                               
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBENDA  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                                        
PUBENDB  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,BUMPPUB                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBENDB                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBENDA                                                       
         B     PUBENDC                                                          
*                                                                               
BUMPPUB  LA    R2,CLTPUBS    CLT/MTH PUB  TOTALS                                
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         CLI   PRDSW,0       SEE IF COMBINING PRDS                              
         BER   R8                                                               
         L     R1,VPROCSCT                                                      
         USING PRODSCT,R1                                                       
         LA    R2,PROPUBS                                                       
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         DROP  R1                                                               
         BR    R8                                                               
*                                                                               
PUBENDC  LA    R2,ACCNUM*7         WAS 6                                        
         LA    R3,CLTINS                                                        
         LA    R4,PUBINS                                                        
PUBEND11 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBEND11                                                      
         L     R8,CTOTPUBS         ADD 1 TO PUB ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,CTOTPUBS                                                      
         DROP  R9                                                               
****                                                                            
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R2,ACCNUM*7         WAS 6                                        
         LA    R3,REPINS                                                        
         LA    R4,PUBINS                                                        
PUBENDD  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBENDD                                                       
         DROP  R9                                                               
*                                                                               
         CLI   PRDSW,0             SEE IF COMBINING PRODUCTS                    
         BE    PUBEND15                                                         
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
         L     R1,PTOTPUBS                                                      
         AH    R1,=H'1'                                                         
         ST    R1,PTOTPUBS                                                      
         LA    R2,ACCNUM*7         WAS 6                                        
         LA    R3,PROINS                                                        
         LA    R4,PUBINS                                                        
PUBEND13 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBEND13                                                      
         DROP  R9                                                               
*                                                                               
****                                                                            
*                                  CLEAR  PUB ACCUMS                            
PUBEND15 LA    R6,ACCNUM*7         WAS6                                         
         LA    R7,PUBINS                                                        
PUBEND17 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,PUBEND17                                                      
PUBEND30 XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PUBACT,0                                                         
         CLI   PRDSW,1                SEE IF DOING PRDS SEPERATELY              
         BE    PUBENDX                                                          
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         MVI   PRDACT,0                                                         
PUBENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
REPEND   CSECT                                                                  
         NMOD1 0,REPEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   REPACT,C'Y'                                                      
         BNE   REPEND30                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
*                                                                               
         MVI   PUBPSW,0              SO PUB CONTINUED MESSAGE WON'T             
*                                    PRINT IN REP TOTALS                        
         CLI   PREPKEY+3,X'11'                                                  
         BNE   REPE15                                                           
         MVC   P+2(16),=C'** REP TOTALS **'                                     
         B     REPE20                                                           
REPE15   CLC   PREPNAME,SPACES                                                  
         BE    REPEND15                                                         
         MVC   P+2(16),=C'** ADR TOTALS **'                                     
REPE20   GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
REPEND1  LA    R6,REPINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                                        
REPEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,REPMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,REPEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,REPEND1                                                       
         B     REPEND3                                                          
*                                                                               
REPMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LA    R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,REPINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    RNOPINS              NO INSERTIONS                               
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   REPMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    REPMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPMTHA                                                          
         EDIT (P8,(R2)),(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         B     REPMTH1                                                          
REPMTHA  EDIT (P8,(R2)),(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
REPMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         LA    RF,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCANDC                                                          
         EDIT  (P8,(R2)),(12,(RF)),2,COMMAS=YES,MINUS=YES                       
         LA    RF,14(RF)                                                        
NTCANDC  DS    0H                                                               
         LA    R2,ACCNUM*8(R2)    BUMP PAST GST                                 
*                                                                               
         EDIT (P8,(R2)),(14,(RF)),2,COMMAS=YES,MINUS=YES                        
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
REPEND3  MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,REPINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,REPINS                                                        
REPEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BE    RNOPTINS             NO INSERTIONS                               
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPTINS CLI   QMEDIA,C'N'                                                      
         BNE   REPEND6                                                          
         ZAP   WKDUB,REPLINES                                                   
         LA    R2,REPLINES                                                      
         LA    R6,ACCNUM-1                                                      
REPEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    REPEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     REPEND6                                                          
REPEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
REPEND6  ZAP   WKDUB,REPGO                                                      
         LA    R2,REPGO                                                         
         LA    R6,ACCNUM-1                                                      
REPEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,REPGLAC                                                    
         LA    R2,REPGLAC                                                       
         LA    R6,ACCNUM-1                                                      
REPEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,REPCD                                                      
         LA    R2,REPCD                                                         
         LA    R6,ACCNUM-1                                                      
REPEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCANDA                                                          
         ZAP   WKDUB,REPGST                                                     
         LA    R2,REPGST                                                        
         LA    R6,ACCNUM-1                                                      
REPEND1A AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND1A                                                      
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   12(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                                        
NTCANDA  DS    0H                                                               
*                                                                               
         ZAP   WKDUB,REPNP                                                      
         LA    R2,REPNP                                                         
         LA    R6,ACCNUM-1                                                      
REPEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND10                                                      
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   REPROLL                                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
*                                                                               
REPROLL  L     R8,RTOTPUBS         ADD 1 TO REP ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,RTOTPUBS                                                      
*                                  CLEAR  REP ACCUMS                            
REPEND15 LA    R6,ACCNUM*8          WAS 7                                       
         LA    R7,REPINS                                                        
REPEND12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,REPEND12                                                      
         MVI   PRDACT,C'Y'                                                      
*                                                                               
REPEND30 XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
REPENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,0                 SEE IF COMBINING PRDS                    
         BE    CLTE5                                                            
         GOTO1 VPUBEND                                                          
         GOTO1 VPRDEND                                                          
         B     CLTE10                                                           
*                                                                               
CLTE5    GOTO1 VPRDEND                                                          
         GOTO1 VPUBEND                                                          
*                                                                               
CLTE10   CLI   PRDSW,0                SEE IF COMBINING PRDS                     
         BE    CLTE15                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CLTE15                                                           
         B     CLTEN12X                                                         
*                                                                               
CLTE15   MVI   FORCEHED,C'Y'                                                    
         MVI   PUBPSW,0                                                         
         MVC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
CLTEND1  LA    R2,CLTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                WAS 6                                        
CLTEND2  CP    0(8,R2),=P'0'                                                    
         BNE   ACTIVITY                                                         
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,CLTEND2                                                       
CLTEND3  LA    R4,8(R4)                                                         
         BCT   R8,CLTEND1                                                       
         B     CLTEND5        GO TO TOTALS                                      
*                                                                               
ACTIVITY LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
*                                                                               
         CLI   PRDSW,1                     SEE IF DOING PRDS SEP                
         BE    ACTV5                                                            
*                                                                               
         LA    R1,CLTPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(4,P+12),0                                                 
         MVC   P+17(4),=C'PUBS'                                                 
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
ACTV5    LA    R1,CLTINS                                                        
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    NOCINS              NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV1                                                           
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    ACTIV1              NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   ACTIVA                                                           
         EDIT  WKDUB,(10,P+41),2                                                
         MVI   P+51,C'I'                                                        
         B     ACTIV1                                                           
ACTIVA   EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
ACTIV1   LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCANDD                                                          
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES                           
         LA    R3,14(R3)                                                        
NTCANDD  DS    0H                                                               
         LA    R1,ACCNUM*8(R1)    BUMP PAST GST                                 
*                                                                               
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
         B     CLTEND3                                                          
*                                                                               
*                                                                               
CLTEND5  MVC   P+5(5),=C'TOTAL'                                                 
*                                                                               
         CLI   PRDSW,1              SEE IF DOING PRDS SEP                       
         BE    CLTEND5C                                                         
*                                                                               
         L     R2,CTOTPUBS                                                      
         EDIT  (R2),(4,P+12),0                                                  
         MVC   P+17(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
CLTEND5C ZAP   WKDUB,CLTINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTINS+8                                                      
CLTEND6  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND6                                                       
         CP    WKDUB,=P'0'                                                      
         BE    NOCTINS             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCTINS  CLI   QMEDIA,C'N'                                                      
         BNE   CLTEND8                                                          
         ZAP   WKDUB,CLTLINES                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTLINES+8                                                    
CLTEND7  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND7                                                       
         CP    WKDUB,=P'0'                                                      
         BE    CLTEND8             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   CLTEND7A                                                         
         EDIT  WKDUB,(10,P+41),2                                                
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     CLTEND8                                                          
CLTEND7A EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
CLTEND8  ZAP   WKDUB,CLTGO                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGO+8                                                       
CLTEND9  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND9                                                       
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,CLTGLAC                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGLAC+8                                                     
CLTEND10 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND10                                                      
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,CLTCD                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTCD+8                                                       
CLTEND11 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND11                                                      
         CP    WKDUB,=P'99999999'      IF OVER 99,999.99 NO COMMAS              
         BH    CLTE11B                                                          
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         B     CLTE11C                                                          
CLTE11B  EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
CLTE11C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
*===============                                                                
         LA    R1,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCANDAW                                                         
         ZAP   WKDUB,CLTGST                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGST+8                                                      
CLTEND41 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND41                                                      
         EDIT  WKDUB,(12,(R1)),2,COMMAS=YES,MINUS=YES                           
         MVI   12(R1),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R1),=C'* '                                                  
         LA    R1,14(R1)                                                        
NTCANDAW DS    0H                                                               
*===============                                                                
         ZAP   WKDUB,CLTNP                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTNP+8                                                       
CLTEND12 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND12                                                      
         CP    WKDUB,=P'9999999999'   IF OVER 99,999,999.99                     
         BH    CLTE12B                                                          
         EDIT  WKDUB,(14,(R1)),2,COMMAS=YES,MINUS=YES                           
         B     CLTE12C                                                          
CLTE12B  EDIT  WKDUB,(14,(R1)),2,MINUS=YES                                      
CLTE12C  CP    WKDUB,=P'0'                                                      
         MVI   14(R1),C'*'                                                      
         BL    *+10                                                             
         MVC   13(2,R1),=C'* '                                                  
         GOTO1 VPRINTIT                                                         
CLTEN12X DS    0H                                                               
         XC    MTHACT(5),MTHACT                                                 
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,CLTINS                                                        
         LA    R4,ACCNUM*8                  WAS 7                               
CLTEND13 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,CLTEND13                                                      
         XC    CTOTPUBS,CTOTPUBS                                                
*                                                                               
CLTENDX  XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
CLIF     CSECT                                                                  
         NMOD1 0,CLIF                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEPUB,SAVEPUB                                                  
*              FIRST BUILD LIST OF PRD AND NAMES                                
         MVC   PPGKEY,KEY                                                       
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CLIF1                                                            
         CLI   QCLIENT,C'&&'       GROUP                                        
         BE    CLIF1                                                            
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    CLIF0                                                            
         CLI   QCLIENT,C'*'        OFFICE REQS                                  
         BNE   CLIEXT                                                           
*                                                                               
CLIF0    DS    0H       SAVE OFFICE FOR OFFICE LIST AND REQS HEADERS            
         GOTO1 VOFFOUT,DMCB,PCLTOFF,HEXOUT,(C'L',SVPTOFC)                       
*                                                                               
CLIF1    DS    0H                                                               
         L     R6,APRDTAB                                                       
*                                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   CLIF6               YES - ONLY READ PRDS WHEN NEEDED             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     CLIF3                                                            
*                                                                               
CLIF2    LA    R4,DMRSEQ                                                        
CLIF3    BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CLIF6                                                            
*                                                                               
*                                                                               
CLIF4    LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,FILERD                                                        
CLIF5    MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     CLIF2                                                            
*                                                                               
CLIF6    MVC   0(3,R6),=X'FFFFFF'  SET END OF TABLE                             
         B     PPGEXT                                                           
*                                                                               
DIRRD    NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    DIRX                                                             
         CLI   DMCB+8,X'02'         PASS DELETES                                
         BE    DIRX                                                             
         DC    H'0'                                                             
DIRX     XIT                                                                    
*                                                                               
FILERD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    FILX                                                             
         CLI   DMCB+8,X'02'            PASS DELETES                             
         BE    FILX                                                             
         DC    H'0'                                                             
FILX     XIT                                                                    
*                                                                               
PPGEXT   MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
CLIEXT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBF     CSECT                                                                  
         NMOD1 0,PUBF                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VASWORK                                                       
         USING ASWORKD,R5                                                       
         MVI   PUBSW,0                                                          
         CLI   QOPT3,C' '                                                       
         BE    SETFLT              DOING ALL PUBS                               
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         LA    R3,PUBREC+33                                                     
         USING PUBGENEL,R3                                                      
         CLI   0(R3),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    HAVEL       HAVE PROD ELEMENT                                    
         CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
HAVEL    DS    0H                                                               
         OC    PUBCDDAT,PUBCDDAT   CHK FOR CD EFF DATE                          
         BNZ   HAVEL5              IF PRESENT THIS IS OR WAS A CD PUB           
         CP    4(2,R3),=P'0'                                                    
         BE    NCHDIS              NO CASH DISCOUNT                             
HAVEL5   CLI   QOPT3,C'C'                                                       
         BE    SETFLT                                                           
         B     GETNEXT                                                          
*                                                                               
NCHDIS   CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
GETNEXT  MVI   PUBSW,1             DON'T PROCESS THIS PUB                       
         B     PUBFEXT                                                          
*                                                                               
         DROP  R7                                                               
         DROP  R3                                                               
SETFLT   DS    0H                                                               
PUBFEXT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPCSCT  CSECT                                                                  
         DS   ((48*8*8)+4)C FOR REPTOTS  48 X 8 X 8 (+4)                        
*        DS    2692C        FOR REPTOTS  48 X 8 X 7 (+4)                        
CLTCSCT  CSECT                                                                  
         DS   ((48*8*8)+4)C  FOR CLTTOTS  48 X 8 X 8 (+4)                       
*        DS    2692C        FOR CLTTOTS  48 X 8 X 7 (+4)                        
PROCSCT  CSECT                                                                  
         DS    F                                                                
         DS    ((48*8*8)+4)C  FOR PROTOTS  48 X 8 X 8 (+4)                      
*        DS    2692C        FOR PROTOTS  48 X 8 X 7 (+4)                        
ASWORK   CSECT                                                                  
         DS    16500C                                                           
*                                                                               
REPDSCT  DSECT                                                                  
REPTOTS  DS    0D                                                               
REPINS   DS    48PL8                                                            
REPLINES DS    48PL8                                                            
REPGO    DS    48PL8                                                            
REPGLAC  DS    48PL8                                                            
REPCD    DS    48PL8                                                            
REPGST   DS    48PL8                                                            
REPNP    DS    48PL8                                                            
REPPUBS  DS    48PL8                                                            
RTOTPUBS DS    F                                                                
*                                                                               
*                                                                               
CLTDSCT  DSECT                                                                  
CLTTOTS  DS    0D                                                               
CLTINS   DS    48PL8                                                            
CLTLINES DS    48PL8                                                            
CLTGO    DS    48PL8                                                            
CLTGLAC  DS    48PL8                                                            
CLTCD    DS    48PL8                                                            
CLTGST   DS    48PL8                                                            
CLTNP    DS    48PL8                                                            
CLTPUBS  DS    48PL8                                                            
CTOTPUBS DS    F                                                                
*                                                                               
PRODSCT  DSECT                                                                  
PROTOTS  DS    0D                                                               
PROINS   DS    48PL8                                                            
PROLINES DS    48PL8                                                            
PROGO    DS    48PL8                                                            
PROGLAC  DS    48PL8                                                            
PROCD    DS    48PL8                                                            
PROGST   DS    48PL8                                                            
PRONP    DS    48PL8                                                            
PROPUBS  DS    48PL8                                                            
PTOTPUBS DS    F                                                                
*                                                                               
ASWORKD  DSECT                                                                  
APRDTAB  DS    F                                                                
STRHIYR  DS    XL1      USED IN BLDMUP.. AND BLDMDN.. PROCS                     
SVPTOFC  DS    CL2      SAVED OFFICE FROM OFFOUT CALL                           
MTHACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
REPACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
SAVELINE DS    CL1                                                              
PAIDSW   DS    CL1      X'01' IF DATED PAY ELEM FOUND                           
PUBSW    DS    CL1                                                              
PUBPSW   DS    CL1      X'01' IF PUB NAME PRINTED                               
PRDSW    DS    CL1      X'01' IF DOING PRDS SEPERATELY                          
LASTYM   DS    CL2                                                              
SAVEYMD  DS    CL3                                                              
SAVEPRD  DS    CL3                                                              
SAVEPUB  DS    CL6                                                              
PPGKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
SAVEPKEY DS    CL32                                                             
STKEY    DS    CL32                                                             
REQERR   DS    CL1                                                              
REQST    DS    CL3                                                              
REQEND   DS    CL3                                                              
ASOFDTE  DS    CL3                    AS OF DATE YMD                            
ASDATE   DS    CL8           AS  OF DATE MMDD/YY                                
REQEST   DS    H                                                                
WKDUB    DS    PL8                                                              
REQPUB   DS    CL6                                                              
SVMEDCLI DS    CL4                 SAVED MEDIA/CLIENT                           
*                                                                               
SAVPLIN1 DS    CL132                                                            
SAVPLIN2 DS    CL132                                                            
*                                                                               
ACCNUM   EQU   48                  NUMBER OF MTH ACCUMS                         
*                            3 YEARS + 6 MTHS BACK + 6 MTHS FORWARD +1          
MTHTAB   DS    CL((ACCNUM*8)+8)   48 X 8 +8                                     
*MTHTAB   DS    CL392         48 X 8 +8                                         
*                                                                               
PRDMTHS  DS    F                                                                
PUBPRDS  DS    F                                                                
PUBMTHS  DS    F                                                                
*                                                                               
VCLIFRST DS    V                                                                
VPUBFRST DS    V                                                                
VMTHEND  DS    V                                                                
VPRDEND  DS    V                                                                
VPUBEND  DS    V                                                                
VCLTEND  DS    V                                                                
VPRINTIT DS    V                                                                
VBLDMLST DS    V                                                                
VASWORK  DS    V                                                                
VREPEND  DS    V                                                                
VPUBFLOT DS    V                                                                
VOFFOUT  DS    V                                                                
VPRNTPUB DS    V                                                                
VREPCSCT DS    V                                                                
VCLTCSCT DS    V                                                                
VPROCSCT DS    V                                                                
         DS    V                                                                
         DS    V                                                                
TOTALS   DS    6D                  WAS 5D                                       
         DS    0F                                                               
BUYOUTA  DS    600C                OUTPUT AREA FOR PPBUYOUT                     
         SPACE                                                                  
RELO     DS    F                                                                
*                                                                               
*                                                                               
BUYTOTS  DS    0D             BUY LINE TOTALS                                   
BUYGO    DS    F                                                                
BUYGLAC  DS    F                                                                
BUYCD    DS    F                                                                
BUYGST   DS    F                                                                
BUYNP    DS    F                                                                
MTHTOTS  DS    0D             MONTH TOTALS                                      
MTHINS   DS    PL8                                                              
MTHLINES DS    PL8                                                              
MTHGO    DS    PL8                                                              
MTHGLAC  DS    PL8                                                              
MTHCD    DS    PL8                                                              
MTHGST   DS    PL8                                                              
MTHNP    DS    PL8                                                              
*                                                                               
PRDTOTS  DS    0D                                                               
PRDINS   DS    PL8                                                              
PRDLINES DS    PL8                                                              
PRDGO    DS    PL8                                                              
PRDGLAC  DS    PL8                                                              
PRDCD    DS    PL8                                                              
PRDGST   DS    PL8                                                              
PRDNP    DS    PL8                                                              
PUBTOTS  DS    0D                                                               
PUBINS   DS    48PL8                                                            
PUBLINES DS    48PL8                                                            
PUBGO    DS    48PL8                                                            
PUBGLAC  DS    48PL8                                                            
PUBCHCD  DS    48PL8                                                            
PUBGSTX  DS    48PL8                                                            
PUBNP    DS    48PL8                                                            
*                                                                               
PRDTAB   DS    CL11500             TABLE OF PRD CDS AND NAMES                   
*                                  23 X 500 PRDS                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
GVALUESD DSECT                                                                  
       ++INCLUDE GVALUES                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREPAS02 07/09/14'                                      
         END                                                                    
