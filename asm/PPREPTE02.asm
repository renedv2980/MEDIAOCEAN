*          DATA SET PPREPTE02  AT LEVEL 076 AS OF 05/01/02                      
*PHASE PPTE02A                                                                  
         TITLE 'PPREPTE02 - TEXACO INTERFACE'                                   
***********************************************************************         
*   QOPT1 -  D=DUMP OUTPUT RECORDS                                              
*                                                                               
*   QOPT7 - Y= TRACE BUFFALO PUTS,READS                                         
*                                                                               
***********************************************************************         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*        SMYE  05/17/00 NEW AGENCY TO USE - H7 (MINDSHARE - SEE 06/00)          
*                       VENDOR CODE IS 0000500087994                            
*                                                                               
*        KWAN  08/00    NEW PBILLREC DSECT                                      
*                                                                               
*        BPLA  06/00    NEW AGENCY TO USE - JW                                  
*                       VENDOR CODE IS 0000500087994                            
*                                                                               
*        SMYE  04/13/00 ALTERED 12/99 Y2K FIX BELOW (SEE HDRO2)                 
*                       AND MADE TWO MORE Y2K FIXES AT REP10 AND                
*                       AT DETO7 AND MADE THIS VERSION LIVE                     
*                                                                               
*        BPLA  12/9/99  Y2K FIX AND DATCON CALL FIXED                           
*                       FOR NON-DDS 6 BYTE FORMAT                               
*                                                                               
*        BPLA  3/23/99 VENDOR NUMBER CHANGE FOR BD                              
*                                                                               
*        BPLA  2/99   ADD CHECK FOR OMNY  (OM)                                  
*                                                                               
*        BPLA 11/95   ADD CHECK FOR BSZNY (TH - ZENITH)                         
*                                                                               
*        BPLA 3/10/93 USE PBILLGRS FOR INVGRS INSTEAD OF ADDING                 
*                     CD TO PBILLRCV                                            
*                                                                               
PPTE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPTE02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
         L     R8,PPFILEC                                                       
         LA    R9,4095(R8)                                                      
         LA    R9,1(R9)                                                         
         USING PPFILED,R8,R9                                                    
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCBY                                                           
         CLI   MODE,PROCBIL                                                     
         BE    PROCBL                                                           
*                                                                               
         CLI   MODE,LBUYCLI                                                     
         BE    CLTL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                  RELOCATE ADDRESSES                           
         RELOC RELO                                                             
         LA    R0,(ACONSX-ACONS)/4      NO. OF ADDRS                            
         LA    R2,ACONS                                                         
         LA    R3,RCONS                                                         
RUNF2    DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
*                                                                               
*                                  SET BUFFALO PARAMS                           
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        REQFRST                                                                
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVC   SVQOPT1,QOPT1                                                    
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         MVI   FCRDACTV,C'N'                                                    
         CLI   QPRODUCT,C' '                                                    
         BE    INITA                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
*                                                                               
INITA    MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         MVC   MYSTART,QSTART                                                   
         MVC   MYEND,QEND                                                       
         GOTO1 DATCON,DMCB,QSTART,(3,MYSTRTB)                                   
         GOTO1 DATCON,DMCB,QEND,(3,MYENDB)                                      
*                                                                               
         MVC   QSTART,SPACES                                                    
         MVC   QEND,SPACES                                                      
         MVC   BQSTART,=X'000000'                                               
         MVC   BQEND,=X'FFFFFF'                                                 
*                                                                               
         ZAP   QTNET,=P'0'                                                      
         ZAP   QTGRS,=P'0'                                                      
         ZAP   QTCD,=P'0'                                                       
         ZAP   QTDUE,=P'0'                                                      
*                                                                               
         CLI   FIRST,0             FIRST TIME TEST                              
         BNE   REQF20                                                           
         MVI   FIRST,1                                                          
*                                                                               
         ZAP   GTNET,=P'0'                                                      
         ZAP   GTGRS,=P'0'                                                      
         ZAP   GTCD,=P'0'                                                       
         ZAP   GTDUE,=P'0'                                                      
         ZAP   GTINVS,=P'0'                                                     
         ZAP   GTLINS,=P'0'                                                     
*                                                                               
         LA    R5,OUTFILE                                                       
         OPEN  ((R5),OUTPUT)                                                    
*                                                                               
REQF20   DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
PROCBY   GOTO1 APRBUY                                                           
         B     EXIT                                                             
         SPACE 3                                                                
PROCBL   GOTO1 APRBILL                                                          
         B     EXIT                                                             
*        CLTFRST                                                                
CLTF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDFRST                                                                
PRDF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        ESTFRST                                                                
ESTF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDLAST                                                                
PRDL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLT LAST                                                               
         SPACE 2                                                                
CLTL     DS    0H                                                               
         GOTO1 AREPRT                                                           
         B     EXIT                                                             
         SPACE 3                                                                
*        REQ LAST                                                               
REQL     DS    0H                                                               
         GOTO1 APRNT                                                            
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   P(20),=C'** REQUEST TOTALS **'                                   
         EDIT  (P8,QTNET),(15,LNET),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,QTGRS),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,QTCD),(15,LCD),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   COMMISSION TOTAL '                               
         ZAP   DUB,QTDUE                                                        
         SP    DUB,QTNET                                                        
         EDIT  (P8,DUB),(15,LGRS),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   BILL AMOUNT TOTAL'                               
         EDIT  (P8,QTDUE),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
         B     EXIT                                                             
         SPACE 3                                                                
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
RUNL     DS    0H                                                               
         GOTO1 APRNT                                                            
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   P(16),=C'** RUN TOTALS **'                                       
         EDIT  (P8,GTNET),(15,LNET),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,GTGRS),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,GTCD),(15,LCD),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   COMMISSION TOTAL '                               
         ZAP   DUB,GTDUE                                                        
         SP    DUB,GTNET                                                        
         EDIT  (P8,DUB),(15,LGRS),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   BILL AMOUNT TOTAL'                               
         EDIT  (P8,GTDUE),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
*                                                                               
         MVC   P+2(14),=C'INVOICE COUNT='                                       
         EDIT  (P8,GTINVS),(7,P+21)                                             
         GOTO1 APRNT                                                            
         MVC   P+2(18),=C'DETAIL LINE COUNT='                                   
         EDIT  (P8,GTLINS),(7,P+21)                                             
         GOTO1 APRNT                                                            
*                                                                               
         CLI   ERROR,0                                                          
         BE    RUNL4                                                            
         MVC   P(45),=C'***** ERRORS - NO OUTPUT FILE GENERATED *****'          
         GOTO1 APRNT                                                            
         B     RUNLX                                                            
*                                                                               
RUNL4    DS    0H                                                               
*                                                                               
RUNL8    DS    0H                                                               
         CLOSE (OUTFILE)                                                        
*                                                                               
         CLI   SVQOPT1,C'D'        DUMP OUTPUT RECS?                            
         BNE   RUNL10                                                           
         OPEN  (OUTFILR,INPUT)                                                  
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
RUNL9    DS    0H                                                               
         LA    R1,OUTFILR                                                       
         LA    R0,OUTREC                                                        
         GET   (R1),(R0)                                                        
         MVC   P(100),OUTREC                                                    
         MVC   PSECOND(100),OUTREC+100                                          
         GOTO1 APRNT                                                            
         MVC   P(100),OUTREC+200                                                
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         B     RUNL9                                                            
*                                                                               
RUNL10   DS    0H                                                               
RUNLX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
OUTFEOD  DS    0H                                                               
         CLOSE (OUTFILR)                                                        
         B     RUNLX                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
ACONS    DS    0F                                                               
         DC    A(PRNT)                                                          
         DC    A(PRBUY)                                                         
         DC    A(PRBILL)                                                        
         DC    A(REPRT)                                                         
         DC    A(BUFFALOC)                                                      
ACONSX   EQU   *                                                                
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=PPTEXOUT,DSORG=PS,RECFM=FB,LRECL=503,            X        
               BLKSIZE=5030,MACRF=PM                                            
OUTFILR  DCB   DDNAME=PPTEXOUT,DSORG=PS,RECFM=FB,LRECL=503,            X        
               BLKSIZE=5030,MACRF=GM,EODAD=OUTFEOD                              
*                                                                               
         EJECT                                                                  
PRBUY    CSECT                                                                  
         NMOD1 0,PRBUY                                                          
         LA    RC,SPACEND                                                       
*                                                                               
         LA    R7,BUYOUTA                                                       
         USING PPBYOUTD,R7                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R3,PBUYREC                                                       
         L     R4,DATCON                                                        
         LA    R5,GROSS                                                         
         STM   R3,R5,PBYOINPT                                                   
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
         MVC   MYSPACE,PBYOSPC                                                  
         OC    MYSPACE,SPACES                                                   
*                                                                               
         CLI   MYSPACE,C' '                                                     
         BNE   RNB10                                                            
         CLI   QMEDIA,C'N'                                                      
         BNE   RNB10                                                            
*                                                                               
*                                  NEWS                                         
RNB5     DS    0H                                                               
         LA    R4,MYSPACE                                                       
         CP    PBDUNITS,=P'0'                                                   
         BE    RNB7                                                             
         MVI   0(R4),C'0'                                                       
         LA    R0,1                                                             
         ZAP   DUB,PBDUNITS                                                     
         BZ    *+8                                                              
         BAS   RE,FBEDL                                                         
         AR    R4,R0                                                            
         MVI   0(R4),C'L'          LINES                                        
         CLI   PBDUIND,C'L'                                                     
         BE    RNB7                                                             
         MVC   0(2,R4),=C'IN'      INCHES                                       
         LA    R4,1(R4)                                                         
*                                                                               
RNB7     CLI   PBDCL,C' '          PREM                                         
         BNH   *+14                                                             
         MVC   2(1,R4),PBDCL                                                    
         MVI   3(R4),C'C'                                                       
*                                                                               
*        MYSPACE WILL NOW BE SET                                                
*                                                                               
RNB10    DS    0H                                                               
         MVI   ELCODE,X'26'                                                     
         LA    R2,PBDELEM                                                       
         USING PBILELEM,R2                                                      
*                                                                               
RNB11    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB13                                                            
*                                                                               
RNB12    DS    0H                                                               
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB30                                                            
*                                                                               
RNB13    DS    0H                                                               
*                                  TEST IN REQ PERIOD                           
         CLC   PBLDATE,MYSTRTB                                                  
         BL    RNB12                                                            
         CLC   PBLDATE,MYENDB                                                   
         BH    RNB12                                                            
*                                                                               
RNB13T   DS    0H                                                               
         XC    X,X                                                              
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINV(1),PBLDATE+1                                              
         MVC   INVINV+1(2),PBINVNO                                              
*                                                                               
RNB15    DS    0H                                                               
         MVC   INVEST,PBUYKEST                                                  
         BAS   RE,RNBSETU          PEUSER1 => INVEUSER                          
         MVC   INVSPACE,MYSPACE                                                 
         MVC   INVPNAM,PUBNAME                                                  
         MVC   INVZNAM,PUBZNAME                                                 
         MVC   INVMKT,PUBZNAME                                                  
         CLI   PBUYKMED,C'O'             SEE IF OUTDOOR                         
         BE    RNB25                                                            
         MVC   INVMKT,SPACES                                                    
*                                                                               
RNB25    MVC   INVPER,PBUYKDAT           INSERTION DATE                         
         GOTO1 DATCON,DMCB,(3,PBLDATE),(2,INVRDAT)                              
         MVC   INVPRD,PBPRD               PRD FROM BILL ELEMEMT                 
         MVC   INVPUB,PBUYKPUB                                                  
         MVC   INVGRS,PBGROSS                                                   
         ICM   R0,15,PBGROSS                                                    
         ICM   R1,15,PBAGYCOM                                                   
         SR    R0,R1                                                            
         ST    R0,INVNET                                                        
         MVC   INVCD,PBCSHDSC                                                   
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
*                                                                               
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   RNB12                                                            
         MVC   P(14),=C'**BUFF PUT D**'                                         
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,P+5,60,=C'N'                                       
         GOTO1 HEXOUT,DMCB,X+60,PSECOND+5,52,=C'N'                              
         GOTO1 APRNT                                                            
         B     RNB12                                                            
*                                                                               
RNB30    DS    0H                                                               
*                                                                               
RNB40    DS    0H                                                               
RNBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
RNBNXTEL DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RNBNXTL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     RNBNXTEL                                                         
RNBNXTL2 DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
*        RNBSETU  - PEUSER1 => INVEUSER                                         
*                                                                               
RNBSETU  DS    0H                                                               
         MVC   INVEUSER,SPACES                                                  
         LA    RF,PESTELEM                                                      
*                                                                               
RNBSU2   DS    0H                                                               
         CLI   0(RF),0             EOR                                          
         BER   RE                                                               
         CLI   0(RF),X'08'         USER FIELD ELEM                              
         BE    RNBSU4                                                           
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEM                                    
         AR    RF,R0                                                            
         B     RNBSU2                                                           
*                                                                               
RNBSU4   DS    0H                                                               
         MVC   INVEUSER,PEUSER1-PESTUDEF(RF)                                    
         OC    INVEUSER,SPACES                                                  
         BR    RE                                                               
         SPACE 3                                                                
FBEDL    DS    0H                                                               
         CLI   PBDUIND,X'89'       INCHES - 2 DECIMALS                          
         BNE   FBEDL5                                                           
         EDIT  (P8,DUB),(10,(R4)),2,ALIGN=LEFT                                  
         BR    RE                                                               
*                                                                               
FBEDL5   EDIT  (P8,DUB),(10,(R4)),ALIGN=LEFT                                    
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'PRBILL  - PROCESS  BILL RECORDS'                                
PRBILL   CSECT                                                                  
         NMOD1 0,PRBILL                                                         
         LA    RC,SPACEND                                                       
         TM    KEY+25,X'80'                                                     
         BNZ   ROBX                                                             
         CLC   RCSVPRD,=C'ZZZ'                                                  
         BE    ROBX                                                             
*                                                                               
         CLC   PBILKBMN,MYSTRTB                                                 
         BL    ROBX                                                             
         CLC   PBILKBMN,MYENDB                                                  
         BH    ROBX                                                             
*                                  PASS DATA TO SORT                            
ROB10    DS    0H                                                               
*                                                                               
         CLC   PBILLDAT,MYSTART                                                 
         BL    ROBX                                                             
         CLC   PBILLDAT,MYEND                                                   
         BH    ROBX                                                             
         TM    PBILCMSW,X'20'      SKIP AOR BILLS                               
         BNZ   ROBX                SKIP NET (USED FOR SOMETHING ELSE)           
*                                                                               
ROB10B   DS    0H                                                               
         XC    X,X                                                              
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINV(1),PBILKBMN+1                                             
         MVC   INVINV+1(2),PBILKBNO                                             
*                                                                               
         MVC   INVPRD,PBILKPRD                                                  
         MVC   INVEST,PBILKEST                                                  
         MVC   INVEDESC,PESTNAME                                                
         BAS   RE,ROBSETU          PEUSER1 => INVEUSER                          
******                                                                          
**3/10/93ZAP   DUB,PBILLRCV            WAS PBILLRCV?                            
******   CVB   R0,DUB                                                           
******   ST    R0,INVGRS                                                        
*                                        (WILL CATCH BILL FORMULAS)             
         ZAP   DUB,PBILLNET      GROSS- AC - CD                                 
         CVB   R0,DUB                                                           
         ST    R0,INVNET                                                        
         ZAP   DUB,PBILLGRS                                                     
         SP    DUB,PBILLBIL      GROSS - CD                                     
         CVB   R0,DUB                                                           
         ST    R0,INVCD                                                         
         L     R1,INVNET         MUST ADD CD TO NET/NET TO GET NET              
         AR    R1,R0                                                            
         ST    R1,INVNET                                                        
******                                                                          
***3/10/93                                                                      
******   L     R1,INVGRS         MUST ADD CD TO G-CD TO GET GROSS               
******   AR    R1,R0                                                            
         ZAP   DUB,PBILLGRS        INVGRS SHOULD BE GROSS                       
         CVB   R1,DUB              ****** ADDED 3/10/92                         
         ST    R1,INVGRS                                                        
****                                                                            
         ZAP   DUB,PBILLRCV        AMOUNT DUE                                   
         CVB   R0,DUB                                                           
         ST    R0,INVDUE                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(2,INVRDAT)                             
         GOTO1 (RF),(R1),(3,PBILINVD),(2,INVINVD)                               
         GOTO1 (RF),(R1),(3,PBILDUED),(2,INVDUED)                               
*                                                                               
ROB11    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   ROBX                                                             
         MVC   P(14),=C'**BUFF PUT I**'                                         
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,P+5,60,=C'N'                                       
         GOTO1 HEXOUT,DMCB,X+60,PSECOND+5,52,=C'N'                              
         GOTO1 APRNT                                                            
*                                                                               
         B     ROBX                                                             
*                                                                               
ROB40    DS    0H                                                               
ROBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        ROBSETU  - PEUSER1 => INVEUSER                                         
*                                                                               
ROBSETU  DS    0H                                                               
         MVC   INVEUSER,SPACES                                                  
         LA    RF,PESTELEM                                                      
*                                                                               
ROBSU2   DS    0H                                                               
         CLI   0(RF),0             EOR                                          
         BER   RE                                                               
         CLI   0(RF),X'08'         USER FIELD ELEM                              
         BE    ROBSU4                                                           
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEM                                    
         AR    RF,R0                                                            
         B     ROBSU2                                                           
*                                                                               
ROBSU4   DS    0H                                                               
         MVC   INVEUSER,PEUSER1-PESTUDEF(RF)                                    
         OC    INVEUSER,SPACES                                                  
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    CSECT                                                                  
         NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         XC    SVHDR,SVHDR                                                      
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,P                                                             
         USING LINED,R2                                                         
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
REP2     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFREC,0                           
         B     REP4B                                                            
REP4     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFREC,0                            
REP4B    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   REP6                                                             
         MVC   P(12),=C'**BUFF OUT**'                                           
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC,P+5,60,=C'N'                                 
         GOTO1 HEXOUT,DMCB,BUFFREC+60,PSECOND+5,52,=C'N'                        
         GOTO1 APRNT                                                            
*                                                                               
REP6     DS    0H                                                               
         OC    INVPER,INVPER       TEST FOR HEADER                              
         BNZ   REP10                                                            
*                                  INVOICE HEADER                               
         OC    SVHDR,SVHDR         ANY OLD HDR TO FINISH?                       
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 APRNT                                                            
         BAS   RE,HDROUT           DO HEADER OUTPUT                             
         XC    ITGRS(12),ITGRS     CLEAR INV CHECKING TOTALS                    
         ZIC   RF,INVINV                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINV(2),DUB                                                      
         MVI   LINV+2,C'-'                                                      
         MVC   HALF,INVINV+1                                                    
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINV+3(4),DUB                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVINVD),(5,LIDAT)                                
         GOTO1 (RF),(R1),(2,INVDUED),(5,LDDAT)                                  
         GOTO1 (RF),(R1),(2,INVRDAT),(5,LRDAT)                                  
*                                                                               
         MVC   LPRD,INVPRD         PRD                                          
*                                                                               
         MVC   HALF,INVEST         EST                                          
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
         MVC   SVHDR,INVD          SAVE HEADER                                  
         AP    GTINVS,=P'1'        BUMP INVOICE COUNT                           
         B     REP30                                                            
*                                                                               
REP10    DS    0H                  DETAIL LINE                                  
         BAS   RE,DETOUT           DO DETAIL OUTPUT                             
*NOP*    GOTO1 DATCON,DMCB,(3,INVPER),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,INVPER),(X'20',WORK)                              
         MVC   LPER,WORK                                                        
*                                                                               
         GOTO1 PUBEDIT,DMCB,INVPUB,LPUB                                         
*                                                                               
REP22    DS    0H                                                               
         EDIT  INVGRS,(15,LGRS),2,COMMAS=YES,MINUS=YES                          
         EDIT  INVNET,(15,LNET),2,COMMAS=YES,MINUS=YES                          
         EDIT  INVCD,(15,LCD),2,COMMAS=YES,MINUS=YES                            
*                                                                               
         ICM   R0,15,INVGRS        ADD TO INV, REQ, AND RUN TOTALS              
         CVD   R0,DUB                                                           
         AP    QTGRS,DUB                                                        
         AP    GTGRS,DUB                                                        
         A     R0,ITGRS                                                         
         ST    R0,ITGRS                                                         
         ICM   R0,15,INVNET                                                     
         CVD   R0,DUB                                                           
         AP    QTNET,DUB                                                        
         AP    GTNET,DUB                                                        
         A     R0,ITNET                                                         
         ST    R0,ITNET                                                         
         ICM   R0,15,INVCD                                                      
         CVD   R0,DUB                                                           
         AP    QTCD,DUB                                                         
         AP    GTCD,DUB                                                         
         A     R0,ITCD                                                          
         ST    R0,ITCD                                                          
         AP    GTLINS,=P'1'                                                     
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
REP30    DS    0H                                                               
         B     REP4                                                             
*                                                                               
REP50    DS    0H                                                               
         OC    SVHDR,SVHDR         ANY HEADER TO FINISH UP?                     
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
*                                                                               
REPX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        FINISH UP OLD HEADER                                                   
         SPACE 2                                                                
ENDINV   NTR1                                                                   
         MVC   LPER(15),=C'*INVOICE TOTAL*'                                     
         ICM   R0,15,SVHDR+INVGRS-INVD                                          
         EDIT  (R0),(15,LGRS),2,COMMAS=YES,MINUS=YES                            
         ICM   R0,15,SVHDR+INVNET-INVD                                          
         EDIT  (R0),(15,LNET),2,COMMAS=YES,MINUS=YES                            
         ICM   R0,15,SVHDR+INVCD-INVD                                           
         EDIT  (R0),(15,LCD),2,COMMAS=YES,MINUS=YES                             
*                                                                               
         CLC   ITGRS,SVHDR+INVGRS-INVD                                          
         BNE   EINV4                                                            
         CLC   ITNET,SVHDR+INVNET-INVD                                          
         BNE   EINV4                                                            
         CLC   ITCD,SVHDR+INVCD-INVD                                            
         BNE   EINV4                                                            
         LA    R3,OUTREC                                                        
         USING TXDATA,R3                                                        
*                                                                               
         LA    RE,TXDATA           CLEAR RECORD                                 
         LA    RF,L'TXDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TXTT,=C'80'                                                      
*                                                                               
         L     R0,ITNET                                                         
         CVD   R0,DUB                                                           
         UNPK  TXTNET,DUB                                                       
         TM    TXTNET+L'TXTNET-1,X'10'   IS IT NEGATIVE?                        
         BZ    *+8                                                              
         MVI   TXTNET,C'-'                                                      
         OI    TXTNET+L'TXTNET-1,X'F0'                                          
*                                                                               
         L     R0,SVHDR+INVDUE-INVD                                             
         L     R1,ITNET                                                         
         SR    R0,R1                                                            
         A     R0,ITCD             PLUS CD                                      
         CVD   R0,DUB                                                           
         UNPK  TXTCOM,DUB                                                       
         TM    TXTCOM+L'TXTCOM-1,X'10'   IS IT NEGATIVE?                        
         BZ    *+8                                                              
         MVI   TXTCOM,C'-'                                                      
         OI    TXTCOM+L'TXTCOM-1,X'F0'                                          
*                                                                               
         L     R0,ITCD                                                          
         CVD   R0,DUB                                                           
         UNPK  TXTCD,DUB                                                        
         TM    TXTCD+L'TXTCD-1,X'10'     IS IT NEGATIVE?                        
         BZ    *+8                                                              
         MVI   TXTCD,C'-'                                                       
         OI    TXTCD+L'TXTCD-1,X'F0'                                            
*                                                                               
         L     R0,SVHDR+INVDUE-INVD    AMOUNT DUE                               
         CVD   R0,DUB                                                           
         UNPK  TXTDUE,DUB                                                       
         TM    TXTDUE+L'TXTDUE-1,X'10'   IS IT NEGATIVE?                        
         BZ    *+8                                                              
         MVI   TXTDUE,C'-'                                                      
         OI    TXTDUE+L'TXTDUE-1,X'F0'                                          
*                                                                               
         L     R0,SVHDR+INVDUE-INVD  ADD TO DUE TOTALS                          
         CVD   R0,DUB                                                           
         AP    QTDUE,DUB                                                        
         AP    GTDUE,DUB                                                        
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         B     EINVX                                                            
         DROP  R3                                                               
*                                                                               
EINV4    DS    0H                                                               
         MVC   LCD+132(18),=C'**OUT OF BALANCE**'                               
         MVI   ERROR,C'Y'                                                       
*                                                                               
EINVX    DS    0H                                                               
         GOTO1 APRNT                                                            
         XIT1                                                                   
         EJECT                                                                  
*        HEADER OUTPUT                                                          
         SPACE 2                                                                
HDROUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING TXDATA,R3                                                        
*                                                                               
         LA    RE,TXDATA           CLEAR RECORD                                 
         LA    RF,L'TXDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TXDATA(13),=C'*****TEX01810'                                     
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
         MVC   TXHH,=C'10'                                                      
*                                                                               
         MVC   TXHVENCD(13),=C'0000500087994'   JWT VENDOR CODE                 
         CLC   QAGENCY,=C'JW'                                                   
         BE    HDRO2                                                            
*                                                                               
*                                                                               
         MVC   TXHVENCD(13),=C'0000500087994'   MINDSHARE VENDOR CODE           
         CLC   QAGENCY,=C'H7'                  (SAME AS JWT ABOVE)              
         BE    HDRO2                                                            
*                                                                               
         MVC   TXHVENCD(13),=C'1329938710001'   BATES VENDOR CODE               
         CLC   QAGENCY,=C'BS'                                                   
         BE    HDRO2                                                            
         CLC   QAGENCY,=C'TH'       ZENITH      SAME AS BATES                   
         BE    HDRO2                                                            
         MVC   TXHVENCD(13),=C'1331980750001'   DWCN AND DFCN                   
         CLC   QAGENCY,=C'DW'                                                   
         BE    HDRO2                                                            
         CLC   QAGENCY,=C'DF'                                                   
         BE    HDRO2                                                            
***OLD*  MVC   TXHVENCD(13),=C'1304731200001'  BBDO VENDOR CODE                 
         MVC   TXHVENCD(13),=C'0000500000460'  BBDO VENDOR CODE                 
         CLC   QAGENCY,=C'BD'                                                   
         BE    HDRO2                                                            
         CLC   QAGENCY,=C'BN'                  AND BN?                          
         BE    HDRO2                                                            
*                                                                               
         MVC   TXHVENCD(13),=C'0000500050808'  OMNY VENDOR CODE                 
         CLC   QAGENCY,=C'OM'                                                   
         BE    HDRO2                                                            
*                                    ELSE MUST BE                               
*        OLD VENDOR CODES FOR CME                                               
*                                                                               
         MVC   TXHVENCD(13),=C'7412825880001'   HOUSTON OFFICE                  
         CLI   PCLTOFF,C'Z'                     Z IS HOUSTON                    
         BE    *+10                             ELSE NEW YORK                   
         MVC   TXHVENCD(13),=C'4109856650002'   (SHOULD BE V)                   
                                                                                
HDRO2    DS    0H                                                               
*NOP*    GOTO1 DATCON,DMCB,(2,INVINVD),(X'20',TXHDATE)                          
*NOP*    MVC   TXHDATE+6(2),TXHDATE+0   YEAR                                    
*NOP*    MVC   TXHDATE+0(4),TXHDATE+2   MMDD                                    
*NOP*    MVC   TXHDATE+4(2),=C'19'                                              
*NOP*    CLC   TXHDATE+0(2),=C'70'   IF YEAR LOWER THAN 70                      
*NOP*    BH    *+10                  ASSUME 20                                  
*NOP*    MVC   TXHDATE+4(2),=C'20'                                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVINVD),(20,WORK)     YYYYMMDD FORMAT            
         MVC   TXHDATE+0(4),WORK+4      MMDD                                    
         MVC   TXHDATE+4(4),WORK        YYYY                                    
*                                                                               
*NOP*    GOTO1 DATCON,DMCB,(2,INVDUED),(X'20',TXHDUED)                          
*NOP*    MVC   TXHDUED+6(2),TXHDUED+0   YEAR                                    
*NOP*    MVC   TXHDUED+0(4),TXHDUED+2   MMDD                                    
*NOP*    MVC   TXHDUED+4(2),=C'19'                                              
*NOP*    CLC   TXHDUED+0(2),=C'70'   IF YEAR LOWER THAN 70                      
*NOP*    BH    *+10                  ASSUME 20                                  
*NOP*    MVC   TXHDUED+4(2),=C'20'                                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVDUED),(20,WORK)     YYYYMMDD FORMAT            
         MVC   TXHDUED+0(4),WORK+4      MMDD                                    
         MVC   TXHDUED+4(4),WORK        YYYY                                    
*                                                                               
         ZIC   RF,INVINV           INVOICE NUMBER                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHINUM(2),DUB                                                   
         MVC   HALF,INVINV+1                                                    
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHINUM+2(4),DUB                                                 
*                                  VENDOR CONTROL # = CLTPRDEST                 
         MVC   TXHVENCN(3),PCLTKCLT                                             
         CLI   TXHVENCN+2,C' '     SPACE TO ZERO IN 3RD BYTE                    
         BH    *+8                                                              
         MVI   TXHVENCN+2,C'0'                                                  
         MVC   TXHVENCN+3(3),INVPRD                                             
         CLI   TXHVENCN+5,C' '     SPACE TO ZERO IN 3RD BYTE                    
         BH    *+8                                                              
         MVI   TXHVENCN+5,C'0'                                                  
         SR    R0,R0                                                            
         ICM   R0,3,INVEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHVENCN+6(3),DUB                                                
*                                                                               
         MVC   TXHPRD,INVPRD+1    (LAST 2 POS OF PRD CODE)                      
         MVC   TXHIDESC(20),INVEDESC  EST DESCRIPTION                           
         OC    TXHIDESC,SPACES                                                  
*                                                                               
*                         ***NOTE  STILL NEED TO DO-                            
*                                    ORIGINAL INVOICE NUMBER                    
*                                                                               
*                                  GET EUSER FIELDS                             
HDR06    DS    0H                                                               
         OC    INVEUSER,SPACES                                                  
         MVC   TXHBUDYR,INVEUSER+0                                              
         MVC   TXHMBFL,INVEUSER+4                                               
         MVC   TXHVEH,INVEUSER+8                                                
         MVC   TXHATYP,INVEUSER+11                                              
         MVC   TXHSVC,INVEUSER+13                                               
         MVC   TXHTERMS,INVEUSER+15                                             
         MVC   TXHSUPES(3),INVEUSER+5                                           
         MVC   TXHPAYR,INVEUSER+18                                              
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
HDRO8    DS    0H                                                               
         DROP  R3                                                               
         XIT1                                                                   
         EJECT                                                                  
*        DETAIL OUTPUT                                                          
         SPACE 2                                                                
DETOUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING TXDATA,R3                                                        
*                                                                               
         LA    RE,TXDATA           CLEAR RECORD                                 
         LA    RF,L'TXDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TXDD,=C'30'                                                      
*                                                                               
         L     R0,INVNET                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         UNPK  TXDNET,DUB                                                       
*                                                                               
         TM    DUB+7,X'0F'                                                      
         BO    *+12                                                             
         MVI   TXDNET,C'-'                                                      
         OI    TXDNET+L'TXDNET-1,X'F0'                                          
*                                                                               
         L     R0,INVCD                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         UNPK  TXDCD,DUB                                                        
*                                                                               
         TM    DUB+7,X'0F'                                                      
         BO    *+12                                                             
         MVI   TXDCD,C'-'                                                       
         OI    TXDCD+L'TXDCD-1,X'F0'                                            
*                                                                               
         UNPK  TXDCOM,=X'0F'       COMM = 0 FOR DETAILS                         
*                                                                               
         MVC   TXDDESC(17),INVSPACE   DESC = SPACE??                            
         OC    TXDDESC,SPACES                                                   
         MVC   TXDSUBNM(20),INVPNAM                                             
         LA    RF,TXDSUBNM+19      FLOAT ZONE NAME                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(20,RF),INVZNAM                                                 
         OC    TXDSUBNM,SPACES                                                  
*                                                                               
DETO7    DS    0H                                                               
*                                                                               
*NOP*    GOTO1 DATCON,DMCB,(3,INVPER),TXDINS                                    
*NOP*    MVC   TXDINS+6(2),TXDINS+0     YEAR                                    
*NOP*    MVC   TXDINS+0(4),TXDINS+2     MMDD                                    
*NOP*    MVC   TXDINS+4(2),=C'19'                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,INVPER),(20,WORK)       YYYYMMDD FORMAT           
         MVC   TXDINS+0(4),WORK+4       MMDD                                    
         MVC   TXDINS+4(4),WORK         YYYY                                    
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
DETO8    DS    0H                                                               
         DROP  R3                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         MVC   HEAD3+50(4),=C'FROM'                                             
         GOTO1 DATCON,DMCB,(0,MYSTART),(5,HEAD3+55)                             
         MVC   HEAD3+64(2),=C'TO'                                               
         GOTO1 (RF),(R1),(0,MYEND),(5,HEAD3+67)                                 
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*       BILLING WORK AREA DSECT                                                 
BILWRKD  DSECT                                                                  
BILWRK   DS    0C                                                               
RCONS    DS    0F                                                               
APRNT    DS    A                                                                
APRBUY   DS    A                                                                
APRBILL  DS    A                                                                
AREPRT   DS    A                                                                
ABUFFC   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
*                                                                               
MYSPACE  DS    CL20                                                             
*                                                                               
BUYOUTA  DS    CL600            PPBYOUT AREA                                    
*                                                                               
SAVPRDC  DS    CL3                                                              
SAVPRD   DS    X                                                                
SAVPGR   DS    XL2                                                              
SAVPGRU  DS    CL4                                                              
SAVMKT   DS    XL2                                                              
SAVMGR   DS    XL2                                                              
SAVMGRU  DS    CL4                                                              
         DS    0F                                                               
X        DS    XL200                                                            
W        DS    CL132                                                            
SAVMODE  DS    X                                                                
MANRVNO  DS    H                                                                
MANMOS   DS    H                                                                
         DS    0F                                                               
         DS    0F                                                               
INVTOTS  DS    0XL8                                                             
INVTGRS  DS    F                                                                
INVTNET  DS    F                                                                
*                                                                               
INTC     DS    F                                                                
INTSTAT  DS    X                                                                
BUFFREC  DS    XL60                                                             
OLDKEY   DS    XL60                                                             
KPRD     DS    X                                                                
KMKT     DS    XL2                                                              
KSTA     DS    XL3                                                              
HOLDPRD  DS    XL1                                                              
HOLDPRD2 DS    XL1                                                              
SAVR1    DS    F                                                                
FFS      DS    XL6'FF'                                                          
DASHES   DS    CL35'-'                                                          
ELCODE   DS    X                                                                
RECSW    DS    X                                                                
ERR      DS    X                                                                
TOTSTAR  DS    CL2                                                              
BILDAT   DS    H                                                                
INVNO    DS    H                                                                
PINVNO   DS    CL10                                                             
SINVNO   DS    H                                                                
SPINVNO  DS    CL10                                                             
MYSTART  DS    CL6               ORIGINAL START AND END                         
MYEND    DS    CL6                                                              
MYSTRTB  DS    CL3               ORIGINAL START AND END - BINARY                
MYENDB   DS    CL3                                                              
*                                                                               
PSTART   DS    CL8                                                              
PEND     DS    CL8                                                              
         DS    0D                                                               
SVHDR    DS    XL(INVRL)                                                        
FIRST    DS    X                                                                
ERROR    DS    X                                                                
SVQOPT1  DS    C                                                                
*                                                                               
ITGRS    DS    F                                                                
ITNET    DS    F                                                                
ITCD     DS    F                                                                
*                                                                               
GTGRS    DS    PL8                                                              
GTNET    DS    PL8                                                              
GTCD     DS    PL8                                                              
GTDUE    DS    PL8                                                              
GTLINS   DS    PL8                                                              
GTINVS   DS    PL8                                                              
*                                                                               
QTGRS    DS    PL8                                                              
QTNET    DS    PL8                                                              
QTCD     DS    PL8                                                              
QTDUE    DS    PL8                                                              
*                                                                               
OUTREC   DS    XL(TXLRECL)                                                      
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LINV     DS    CL7                                                              
         DS    CL1                                                              
LRDAT    DS    CL8                                                              
         DS    CL1                                                              
LIDAT    DS    CL8                                                              
         DS    CL1                                                              
LDDAT    DS    CL8                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LPER     DS    CL6                                                              
         DS    CL1                                                              
LPUB     DS    CL17                                                             
         DS    CL1                                                              
LGRS     DS    CL15                                                             
         DS    CL1                                                              
LNET     DS    CL15                                                             
         DS    CL1                                                              
LCD      DS    CL15                                                             
         DS    CL1                                                              
         DS    0C                                                               
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVINV   DS    XL3                                                              
INVRDAT  DS    XL2                                                              
INVPRD   DS    CL3                                                              
INVEST   DS    XL2                                                              
*                                INVPER,INSPACE,INVMTK,INVPUB                   
*                                NULL FOR HEADERS                               
INVPER   DS    XL3                                                              
INVMKT   DS    CL20              ZONE FOR OUTDOOR                               
INVPUB   DS    XL6                                                              
INVSPACE DS    CL17                                                             
         ORG   INVSPACE                                                         
INVINVD  DS    XL2                 HEADERS ONLY                                 
INVDUED  DS    XL2                      ''                                      
         ORG                                                                    
INVKL    EQU   *-INVD                                                           
*                                COMMENT DATA                                   
INVPNAM  DS    CL20              PUBNAME                                        
INVZNAM  DS    CL20              ZONE NAME                                      
         ORG   INVPNAM                                                          
INVEUSER DS    CL32              EUSER1 (HEADERS)                               
INVEDESC DS    CL20              EST DESC                                       
         ORG                                                                    
*                                                                               
INVGRS   DS    XL4                                                              
INVNET   DS    XL4                                                              
INVCD    DS    XL4                                                              
INVDUE   DS    XL4                                                              
INVRL    EQU   *-INVD                                                           
         SPACE 2                                                                
         EJECT                                                                  
*                                  BUFFALO CSECT                                
         BUFF  LINES=6000,ROWS=1,COLUMNS=4,FLAVOR=BINARY,COMMENT=52,   X        
               KEYLIST=(56,A)                                                   
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
       ++INCLUDE DDTEXACOD                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076PPREPTE02 05/01/02'                                      
         END                                                                    
