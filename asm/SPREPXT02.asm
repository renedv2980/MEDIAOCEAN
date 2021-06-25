*          DATA SET SPREPXT02  AT LEVEL 026 AS OF 04/01/03                      
*PHASE SPXT02A                                                                  
*INCLUDE DTCNV                                                                  
*INCLUDE NETNET                                                                 
*INCLUDE DAYUNPK                                                                
*INCLUDE NETNET                                                                 
         TITLE 'SPXT02 - SPOTPAK BILLING INTERFACE'                             
***********************************************************************         
*                                                                               
*          ****************   NOTE   *******************                        
*                                                                               
*  THIS PROGRAM DOES NOT ASSEMBLE CLEANLY. THE SOURCE WAS MODIFIED IN           
*  AUG/02 TO USE THE PL6 DOLLAR VALUES IN THE BILL HEADER RECORDS, BUT          
*  COULD NOT BE ASSEMBLED BECAUSE IT REFERS TO FIELDS NO LONGER IN              
*  EXISTENCE. THE LIVE LOAD MODULE WAS *ZAPPED* TO REFLECT THE                  
*  MODIFICATIONS AT LEVEL 25.                                                   
*                                                                               
*  PRESUMABLY, NONE OF THIS REALLY MATTER, SINCE WE DON'T BELIEVE THAT          
*  THIS PROGRAM IS IN USE ANY LONGER.                                           
*                                                                               
***********************************************************************         
                                                                                
***********************************************************************         
*                                                                               
*QOPT1   E=SPECIAL ESTIMATE EXTRACT                                             
*        B=FILTER ON BILLED DATES                                               
*QOPT2   P=PRINT OUTPUT RECORDS                                                 
*QOPT3   IF QOPT1 = E                                                           
*           B = FILTER OF ACTUAL BUCKET MONTH                                   
*                                                                               
*                                                                               
*QOPT4   N=SUPPRESS BUY DETAILS                                                 
*                                                                               
*        NOTE - NETPAK CODE ADDED SEP/92. PIGGYBACKS NOT YET SUPPORTED          
*               BUT THE ROUTINES NETCOST AND SETSHR HAVE SOME USEFUL            
*               CODE.                                                           
*                                                                               
*        NOTE - AT PRESENT ALL THE UNITS ARE READ TOGETHER AT CLIENT            
*               LAST. PRD=ALL AND EST=ALL ARE ASSUMED.                          
*                                                                               
***********************************************************************         
*                                                                               
SPXT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPXT02,RR=R2                                                   
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R8,MEDBUFF                                                       
         USING MEDBLOCK,R8                                                      
         LA    R9,SPACEND                                                       
         USING SPXTWRKD,R9                                                      
         ST    R2,RELO                                                          
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,ESTFRST                                                     
         BE    FEST                                                             
         CLI   MODE,MGR1FRST       SPECIAL FOR B&J CLIENT DDS                   
         BE    FMGR1                                                            
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,OUTCNT           ZAP ACCUMS                                   
         LA    R3,5                                                             
INIT2    ZAP   0(4,R2),=P'0'                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,INIT2                                                         
         XC    LASTDATE,LASTDATE                                                
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         L     R0,=A(SPTHK)                                                     
         A     R0,RELO                                                          
         ST    R0,SPOTHOOK                                                      
         L     R0,=A(STABUCKC)                                                  
         A     R0,RELO                                                          
         ST    R0,ADSTABUC                                                      
         L     R0,=A(OUTFILE)                                                   
         A     R0,RELO                                                          
         ST    R0,AOUTFILE                                                      
         L     R0,=V(DTCNV)                                                     
         A     R0,RELO                                                          
         ST    R0,ADTCNV                                                        
         L     R0,=V(DAYUNPK)                                                   
         A     R0,RELO                                                          
         ST    R0,ADAYUNPK                                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'128'                                                 
         MVI   MEDEXTDM,0          NO DEMOS                                     
         B     EXIT                                                             
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVI   FCRDBUYS,C'Y'                                                    
         MVI   FCRDGOAL,C'B'       SO SPONSOR WILL READ BILLS                   
*                                  FOR MKTLIST                                  
         CLI   QOPT4,C'N'          TEST TO SUPPRESS BUYS                        
         BE    REQF1                                                            
         CLI   QOPT1,C'E'          FOR ESTIMATE BUCKET RUN                      
         BE    REQF1               NO BUYS                                      
         CLI   QOPT1,C'B'          FOR BILLING DATE REQ                         
         BNE   REQF2                                                            
         CLC   QAGY,=C'BJ'         SPECIAL FOR B&J ??????                       
         BNE   REQF1                                                            
         CLC   QCLT,=C'DDS'        SPECIAL CLIENT                               
         BNE   REQF1                                                            
         MVI   QMGR,C'A'           SET FOR SCHEME A                             
         MVC   QMKT(3),=C'ALL'                                                  
         MVC   QSTA(3),=C'ALL'                                                  
         B     REQF1B              MUST LEAVE FCRDBUYS TO YES                   
*                                  SO SPONSOR WILL READ MKT GROUPS              
*                                                                               
REQF1    MVI   FCRDBUYS,C'N'       NO BUYS                                      
         MVI   FCRDGOAL,C'N'                                                    
REQF1B   CLC   =C'NO',QEST                                                      
         BNE   *+10                                                             
         MVC   QEST,=C'ALL'                                                     
REQF2    DS    0H                                                               
         MVC   SVQSTART(12),QSTART                                              
         GOTO1 ADTCNV,DMCB,(0,QSTART),(1,BSSTART)                               
         GOTO1 ADTCNV,DMCB,(0,QEND),(1,BSEND)                                   
         GOTO1 DATCON,DMCB,QSTART,(2,BSSTARTP)                                  
         GOTO1 (RF),(R1),QEND,(2,BSENDP)                                        
         MVC   BQSTART(10),BSSTART                                              
         MVC   QSTAUTO,=C'ES'      CAUSE TO IGNORE DATES                        
*                                                                               
         CLI   NETPAKSW,0          UNLESS HAVE ALREADY DONE                     
         BNE   REQF6                                                            
         L     RF,VMASTC           SET NETPAKSW                                 
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'N'                                                    
         LA    R3,SPDYNDSN         SPOT DYNALLOC                                
         CLI   MCNETPAK,C'Y'                                                    
         BNE   REQF4                                                            
         MVI   NETPAKSW,C'Y'                                                    
         LA    R3,NEDYNDSN         NET DYNALLOC                                 
         DROP  RF                                                               
*                                                                               
REQF4    DS    0H                                                               
         MVC   13(2,R3),QAGY       SET AGENCY                                   
         GOTO1 DYNALLOC,DMCB,(0,=C'SXTTAPE '),(0,0(R3))                         
*                                                                               
         L     R2,AOUTFILE                                                      
         OPEN  ((2),(OUTPUT))                                                   
*                                                                               
REQF6    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
SPDYNDSN DC    CL20'SPTTAPE.SP0XTAG1'                                           
NEDYNDSN DC    CL20'NETTAPE.NE0XTAG1'                                           
         SPACE 2                                                                
*        CLIENT LAST                                                            
*                                                                               
CLTL     DS    0H                                                               
         CLI   NETPAKSW,C'Y'       IF NETPAK                                    
         BNE   EXIT                                                             
         GOTO1 =A(NTUPROC),RR=RELO   DO NETPAK UNITS                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
FEST     DS    0H                  OUTPUT EST HEADER                            
         LA    R7,OUTREC                                                        
         USING DDEST,R7                                                         
         XC    OUTREC(250),OUTREC                                               
*                                                                               
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
         CLC   QEST,=C'NO '                                                     
         BE    ROBILL                                                           
         MVC   DDESYS(3),=C'SEH'                                                
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         MVI   DDESYS,C'N'                                                      
         MVC   DDEAGY,QAGY                                                      
         MVC   DDEMED,QMED                                                      
         MVC   DDECLT,CLT                                                       
         MVC   DDEPRD,PRD                                                       
         MVC   DDEEST,EST                                                       
         MVC   DDECNAME,CLTNM                                                   
         MVC   DDEPNAME,PRDNM                                                   
         MVC   DDEENAME,ESTNM                                                   
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   DDECNUM,=8C'0'                                                   
         CLI   CCLTIFC,C' '        TEST CLT NUMBER PRESENT                      
         BNH   *+10                                                             
         MVC   DDECNUM,CCLTIFC     INTERFACE NUMBER (4)                         
*                                                                               
         USING PRDHDR,R6                                                        
         MVC   DDEPCOD,PACCT                                                    
         OC    DDEPCOD,SPACES                                                   
         MVC   DDESDATE(12),ESTDATES                                            
         OC    DDEPCOD,SPACES                                                   
*                                                                               
         CLI   QOPT1,C'E'          UNLESS EST BUCKET RUN                        
         BNE   FEST40              DONE- WRITE RECORD                           
         CLI   QOPT3,C'B'          TEST FILTER ON BUCKET DATES                  
         BE    FEST5               YES                                          
         CLC   ESTDATES(6),SVQSTART  NO- FILTER ON EST START                    
         BL    FEST40                                                           
         CLC   ESTDATES+6(6),SVQEND                                             
         BH    FEST40                                                           
*                                                                               
FEST5    DS    0H                                                               
         MVC   WORK(12),ESTDATES                                                
         GOTO1 MOBILE,DMCB,(20,WORK),(0,MONLST)                                 
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
         SR    R5,R5               BUCKET DISPLACEMENT                          
         LA    R4,MONLST                                                        
*                                                                               
FEST6    DS    0H                                                               
         CLI   0(R4),X'FF'         EOL                                          
         BE    FEST10                                                           
         CLI   QOPT3,C'B'          TEST FILTER ON BUCKET DATE                   
         BNE   FEST7                                                            
         CLC   2(2,R4),BSSTARTP    IF MONTH ENDS BEFORE                         
         BL    FEST9               REQ START- NEXT MONTH                        
         CLC   0(2,R4),BSENDP      IF MONTH STARTS AFTER                        
         BH    FEST10              REQ END- DONE                                
*                                                                               
FEST7    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),DUB                                        
         LA    R3,DDESDATE+12      START AT SPARE                               
         MVC   0(4,R3),DUB                                                      
         MVI   BYTE,0              ZERO SWITCH                                  
*                                                                               
         LA    RF,EORDN            ORDERED GROSS                                
         L     RF,0(RF,R5)                                                      
         CVD   RF,DUB                                                           
         ZAP   4(6,R3),DUB                                                      
         BZ    *+8                                                              
         MVI   BYTE,1              SET HAVE NON-ZERO AMT                        
*                                                                               
*NOP     LA    RF,ENET             ORDERED NET                                  
*NOP     L     RF,0(RF,R5)                                                      
*NOP     CVD   RF,DUB                                                           
*NOP     ZAP   10(6,R3),DUB                                                     
*NOP     BZ    *+8                                                              
*NOP     MVI   BYTE,1              SET HAVE NON-ZERO AMT                        
*                                                                               
         LA    RF,EPAIDN           PAID GROSS                                   
         L     RF,0(RF,R5)                                                      
         CVD   RF,DUB                                                           
         ZAP   22(6,R3),DUB                                                     
         BZ    *+8                                                              
         MVI   BYTE,1              SET HAVE NON-ZERO AMT                        
*                                                                               
         CLI   BYTE,0              IF ALL AMTS ZERO                             
         BE    FEST9               SKIP WRITE                                   
*                                                                               
         ZAP   16(6,R3),=P'0'      ORD C.D.= ZERO                               
         ZAP   28(6,R3),=P'0'      PAID NET (NOT AVAILABLE)                     
         ZAP   34(6,R3),=P'0'      PAID C.D. = ZERO                             
*                                                                               
         BAS   RE,WRITER                                                        
         AP    HDRCNT,=P'1'                                                     
*                                                                               
FEST9    DS    0H                                                               
         LA    R4,4(R4)            NEXT MONTH                                   
         LA    R5,4(R5)            BUMP DISPLACEMENT                            
         B     FEST6                                                            
*                                                                               
FEST10   DS    0H                                                               
         B     FEST42                                                           
*                                                                               
FEST40   DS    0H                                                               
         BAS   RE,WRITER                                                        
         AP    HDRCNT,=P'1'                                                     
*                                                                               
FEST42   DS    0H                                                               
         B     ROBILL                                                           
         EJECT                                                                  
PROCESS  DS    0H                  PROCESS BUYRECS                              
         CLI   NETPAKSW,C'Y'       SKIP IF NETPAK                               
         BE    EXIT                                                             
         CLI   QOPT1,C'B'          SKIP BUYS IF BILLING DATE RUN                
         BE    EXIT                                                             
         CLI   QOPT1,C'E'          FOR EST BUCKET RUN                           
         BE    EXIT                NO BUYS                                      
         USING DDSPOT,R7                                                        
         LA    R7,OUTREC                                                        
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         XC    OUTREC(250),OUTREC                                               
         XC    LASTDATE,LASTDATE                                                
         XC    NSPTS,NSPTS                                                      
         MVI   SPTNUM,0                                                         
*                                  INITIALIZE SPOT DETAIL REC FOR               
*                                  SPOTHOOK                                     
         MVC   DDSSYS(3),=C'SSP'                                                
         MVC   DDSAGY,QAGY                                                      
         MVC   DDSMED,QMED                                                      
         MVC   DDSCLT,CLT                                                       
         MVC   DDSPRD,PRD                                                       
         MVC   DDSEST,EST                                                       
         GOTO1 MSUNPK,DMCB,BUYMSTA,DDSMKT,DDSSTA                                
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDSBUY,DUB                                                       
         MVC   HALF,BDTIMST        START TIME                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDSSTIM,DUB                                                      
         MVC   HALF,BDTIMEND                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDSETIM,DUB                                                      
         ZIC   R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDSLEN,DUB                                                       
         MVC   DDSDPT,BDDAYPT                                                   
         MVC   DDSPGM,BDPROGRM                                                  
         OC    DDSPGM,SPACES                                                    
*                                                                               
         MVI   MEDBRAND,X'FF'                                                   
         MVI   MEDSPTLN,0                                                       
         GOTO1 MEDGETBY,DMCB,SPWORKD,0                                          
         B     EXIT                                                             
         EJECT                                                                  
ROBILL   DS    0H                  READ OLD STYLE BILL RECS                     
         CLI   QOPT1,C'E'          FOR EST BUCKET RUN                           
         BE    EXIT                NO BILLS                                     
         LA    R6,OUTREC                                                        
         USING DDBILL,R6                                                        
         XC    OUTREC,OUTREC                                                    
         MVC   DDBSYS(3),=C'SIH'                                                
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         MVI   DDBSYS,C'N'                                                      
         MVC   DDBAGY,QAGY                                                      
         MVC   DDBMED,QMED                                                      
         MVC   DDBCLT,CLT                                                       
         MVC   DDBPRD,PRD                                                       
         MVC   KPRD,BPRD                                                        
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
ROB2     DS    0H                                                               
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         CLI   KPRD,0                                                           
         BE    ROB3                SKIP REST OF KEY IF MULTI-PRD                
         MVC   KEY+4(3),PRD                                                     
ROB2EST  DS    0H                                                               
         MVC   KEY+7(1),BEST                                                    
         XC    KEY+8(5),KEY+8                                                   
         CLI   BEST,0                                                           
         BE    ROB3                SKIP REST OF KEY IF MULTI-EST                
ROB2PER  DS    0H                                                               
         XC    KEY+8(5),KEY+8                                                   
*                                                                               
ROB3     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     ROB4B                                                            
ROB4     DS    0H                                                               
         GOTO1 SEQ                                                              
ROB4B    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      AGM/CLT                                      
         BNE   ROB40                                                            
         CLI   KPRD,0                                                           
         BE    ROB5                                                             
         CLC   PRD,KEY+4           ONE PRD MUST BE EQUAL                        
         BNE   ROB40                                                            
ROB5     DS    0H                  MULTI-PROD SITUATION                         
         OC    KEY+8(5),KEY+8                                                   
         BZ    ROB4                BYPASS IF NOT A BILL                         
ROB5A    DS    0H                                                               
         CLI   QPGR,C' '           TEST PGR'S                                   
         BE    ROB6                NO                                           
*                                  YES - GET PGR                                
         CLC   KEY+4(3),SAVPRDC                                                 
         BE    ROB5D                                                            
         MVC   SAVPRDC,KEY+4                                                    
         L     R2,PRDLIST                                                       
ROB5B    DS    0H                                                               
ROB5B2   DS    0H                                                               
         CLC   KEY+4(3),2(R2)                                                   
         BE    ROB5C                                                            
         LA    R2,6(R2)                                                         
         B     ROB5B                                                            
ROB5C    DS    0H                                                               
         MVC   SAVPRD,5(R2)                                                     
         MVC   SAVPGR,0(R2)                                                     
         UNPK  WORK(5),SAVPGR(3)                                                
         MVC   SAVPGRU,WORK                                                     
ROB5D    DS    0H                                                               
         CLC   SAVPGR,=X'9999'                                                  
         BE    ROB5F                                                            
         CLC   SAVPGR,BPGR+1       THEN CHECK RIGHT PGR                         
         BE    ROB6                YES                                          
ROB5F    DS    0H                                                               
         IC    RF,KEY+6            BUMP TO NEXT PROD                            
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         B     ROB2EST             SET KEY FROM EST DOWN                        
*                                                                               
ROB6     DS    0H                                                               
         CLI   BEST,0                                                           
         BE    ROB7                                                             
*                                  ONE EST OR SERIES                            
         CLC   KEY+7(1),BEST                                                    
         BL    ROB2EST                                                          
         BE    ROB7                                                             
*                                                                               
         CLI   BESTEND,0                                                        
         BE    ROB6D                                                            
         CLC   KEY+7(1),BESTEND                                                 
         BNH   ROB7                                                             
*                                                                               
ROB6D    DS    0H                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BE    ROB5F               NEXT PROD IF MULTI-PRD                       
         B     ROB40               ELSE DONE                                    
*                                                                               
ROB7     DS    0H                  BILL DATE                                    
         CLI   QOPT1,C'B'                                                       
         BE    ROB8                                                             
         CLC   KEY+8(2),BSSTART                                                 
         BL    ROB4                                                             
         CLC   KEY+8(2),BSEND                                                   
         BH    ROB4                                                             
*                                                                               
ROB8     DS    0H                                                               
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
*                                  PASS DATA TO SORT                            
ROB10    DS    0H                                                               
         GOTO1 GETBILL                                                          
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         CLI   BTYPE,C'B'          MUST BE A NEW BILLING BILL                   
         BNE   ROB4                                                             
         CLI   QOPT1,C'B'          IF BILLING DATE REQ                          
         BNE   ROB11                                                            
         CLC   BDATE,SVQSTART                                                   
         BL    ROB4                                                             
         CLC   BDATE,SVQEND                                                     
         BH    ROB4                                                             
*                                                                               
ROB11    DS    0H                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDBEST,DUB                                                       
         MVC   DDBMKT,SPACES                                                    
         MVC   DDBSTA,SPACES                                                    
         MVC   DDBMOS,BMONSERV                                                  
         MVC   DDBINV,BINVNO                                                    
         MVC   DDBCD,=12C'0'                                                    
         UNPK  DDBGRS,BGRSP                                                     
         CP    BGRSP,=P'0'         SEE IF NEGATIVE                              
         BL    *+8                                                              
         OI    DDBGRS+11,X'F0'                                                  
         UNPK  DDBNET,BNETP                                                     
         CP    BNETP,=P'0'                                                      
         BL    *+8                                                              
         OI    DDBNET+11,X'F0'                                                  
         UNPK  DDBACT,BACTP                                                     
         CP    BACTP,=P'0'                                                      
         BL    *+8                                                              
         OI    DDBACT+11,X'F0'                                                  
         ZAP   DUB,BACTP                                                        
         SP    DUB,BNETP                                                        
         UNPK  DDBINC,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBINC+11,X'F0'                                                  
         MVC   DDBINVD,BQDATE      REQUEST DATE                                 
         CLC   BQDATE(2),=C'00'                                                 
         BH    *+10                                                             
         MVC   DDBINVD,BDATE       ELSE USE BILL DATE                           
         MVC   DDBDUED,=6C'0'                                                   
         OC    BDUEDATE,BDUEDATE   REQUESTED DUE DATE                           
         BZ    ROB20                                                            
         GOTO1 ADTCNV,DMCB,(1,BDUEDATE),(0,DDBDUED)                             
*                                                                               
ROB20    DS    0H                                                               
         MVC   DDBTYP,BTYPE+1                                                   
         BAS   RE,WRITER                                                        
         AP    BHDRCNT,=P'1'                                                    
         B     ROB4                GO DO NEXT RECORD                            
*                                                                               
ROB40    DS    0H                                                               
FESTX    CLC   QAGY,=C'BJ'         ?????                                        
         BNE   FESTXX                                                           
         CLC   QCLT,=C'DDS'                                                     
         BNE   FESTXX                                                           
         B     EXIT                BILLING DETAILS WILL BE DONE AT              
*                                  MGR1FRST FOR THIS SPECIAL                    
FESTXX   B     BILL                                                             
         EJECT                                                                  
FMGR1    DS    0H                                                               
         CLC   QAGY,=C'BJ'         SPECIAL FOR B&J CLT DDS                      
         BNE   EXIT                DO INVOICE DETAILS AT MGR1FRST               
         CLC   QCLT,=C'DDS'                                                     
         BNE   EXIT                                                             
*                                                                               
BILL     DS    0H                  INVOICE DETAIL RECORDS                       
         CLI   QOPT1,C'E'          FOR EST BUCKET RUN                           
         BE    EXIT                NO BILL DETAILS                              
         LA    R6,OUTREC                                                        
         USING DDBILL,R6                                                        
         XC    OUTREC(250),OUTREC                                               
         MVC   DDBSYS(3),=C'SID'                                                
         MVC   DDBAGY,QAGY                                                      
         MVC   DDBMED,QMED                                                      
         MVC   DDBCLT,CLT                                                       
         MVC   DDBPRD,PRD                                                       
         MVC   KPRD,BPRD                                                        
         XC    KMKT,KMKT                                                        
         XC    KSTA,KSTA                                                        
         XC    SAVPRDC(18),SAVPRDC    PRD AND MKT SAVES                         
         MVC   KEY1,KEY                 SAVE KEY                                
         XC    KEY,KEY                                                          
RNB2     DS    0H                                                               
         MVC   KEY(2),=X'0E01'          RECORD TYPE                             
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         CLI   KPRD,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   KEY+5(1),KPRD                                                    
RNB2EST  DS    0H                                                               
         MVC   KEY+6(1),BEST                                                    
         XC    KEY+7(6),KEY+7                                                   
         CLI   BEST,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-EST            
RNB2MKT  DS    0H                                                               
         MVC   KEY+7(2),KMKT                                                    
         XC    KEY+9(4),KEY+9                                                   
         OC    KMKT,KMKT                                                        
         BZ    RNB3                SKIP STA NOW IF MULTI-MKT                    
RNB2STA  DS    0H                                                               
         MVC   KEY+9(3),KSTA                                                    
RNB3     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB4B                                                            
RNB4     DS    0H                                                               
         GOTO1 SEQ                                                              
RNB4B    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      AGM/CLT                                      
         BNE   RNB40                                                            
         CLI   KPRD,0                                                           
         BE    RNB5                                                             
         CLC   KPRD,KEY+5          ONE PROD MUST BE EQUAL                       
         BNE   RNB40                                                            
RNB5     DS    0H                                                               
*                                  MULTI-PRD STIUATION                          
         CLI   QPGR,C' '           TEST PGR'S                                   
         BE    RNB6                NO                                           
*                                  YES                                          
         CLC   KEY+5(1),SAVPRD                                                  
         BE    RNB5D                                                            
         MVC   SAVPRD,KEY+5                                                     
         L     R2,PRDLIST                                                       
RNB5B    DS    0H                                                               
         CLC   KEY+5(1),5(R2)                                                   
         BE    RNB5C                                                            
         LA    R2,6(R2)                                                         
         B     RNB5B                                                            
RNB5C    DS    0H                                                               
         MVC   SAVPRDC,2(R2)                                                    
         MVC   SAVPGR,0(R2)                                                     
         UNPK  WORK(5),SAVPGR(3)                                                
         MVC   SAVPGRU,WORK                                                     
RNB5D    DS    0H                                                               
         CLC   SAVPGR,=X'9999'     NOT PGR OK                                   
         BE    RNB5F                                                            
         CLC   SAVPGR,BPGR+1       THEN CHECK RIGHT PGR                         
         BE    RNB6                YES                                          
RNB5F    DS    0H                                                               
         IC    RF,KEY+5            BUMP TO NEXT PRD                             
         LA    RF,1(RF)                                                         
         STC   RF,KEY+5                                                         
         B     RNB2EST             SET KEY FROM EST DOWN                        
*                                                                               
RNB6     DS    0H                  EST                                          
         CLI   BEST,0                                                           
         BE    RNB7                                                             
*                                  ONE EST OR SERIES                            
         CLC   KEY+6(1),BEST                                                    
         BL    RNB2EST                                                          
         BE    RNB7                                                             
         CLI   BESTEND,0                                                        
         BE    RNB6D                                                            
         CLC   KEY+6(1),BESTEND                                                 
         BNH   RNB7                                                             
*                                                                               
RNB6D    DS    0H                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BE    RNB5F               NEXT PRD IF MULTI-PRD                        
         B     RNB40               ELSE DONE                                    
*                                                                               
RNB6H    DS    0H                  BUMP TO NEXT EST                             
         ZIC   RF,KEY+6                                                         
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         B     RNB2MKT                                                          
*                                                                               
RNB7     DS    0H                  MARKET                                       
         OC    KMKT,KMKT                                                        
         BZ    RNB8                                                             
         CLC   KEY+7(2),KMKT       ONE MKT                                      
         BE    RNB9                                                             
         BL    RNB2MKT                                                          
*                                  MKT HIGH                                     
*                                  IF MULT-EST BUMP TO NEXT EST                 
RNB7B    DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB6H                                                            
         CLI   BESTEND,0                                                        
         BNE   RNB6H                                                            
         B     RNB6D               NEXT PRD (IF MULTI-PRD)                      
*                                                                               
RNB8     DS    0H                  MULTI-MKT                                    
         CLI   QMGR,C' '           TEST MGR'S                                   
         BE    RNB10               NO                                           
         CLC   SAVMKT,KEY+7                                                     
         BE    RNB8F                                                            
*                                                                               
         L     R2,MKTLIST          ACTIVE MKT LIST                              
RNB8B    DS    0H                                                               
         OC    0(3,R2),0(R2)       CHK FOR END OF LIST                          
         BNE   *+6                                                              
         DC    H'0'                MUST FIND IN MKTLIST                         
*                                                                               
         CLC   KEY+7(2),2(R2)                                                   
         BE    RNB8D                                                            
         LA    R2,5(R2)                                                         
         B     RNB8B                                                            
RNB8D    DS    0H                                                               
         MVC   SAVMGR,0(R2)                                                     
         UNPK  WORK(5),SAVMGR(3)                                                
         MVC   SAVMGRU,WORK                                                     
RNB8F    DS    0H                                                               
         CLC   SAVMGR,=X'9999'     TEST MGR OK                                  
         BE    RNB8H               NO                                           
         CLC   SAVMGR,BMGR+1       THEN TEST MGR                                
         BE    RNB10                                                            
RNB8H    DS    0H                                                               
         MVC   HALF,KEY+7          BUMP TO NEXT MKT                             
         LH    RF,HALF                                                          
         LA    RF,1(RF)                                                         
         STH   RF,HALF                                                          
         MVC   KEY+7(2),HALF                                                    
         B     RNB2STA                                                          
*                                                                               
RNB9     DS    0H                  STATION                                      
         OC    KSTA,KSTA                                                        
         BZ    RNB10                                                            
         CLC   KSTA,KEY+9          ONE STA                                      
         BL    RNB2STA                                                          
         BH    RNB7B               NEXT EST -PRD (IF MULTI)                     
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
RNB10    DS    0H                                                               
         L     R7,ADSTABUC                                                      
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
         GOTO1 GET                                                              
*                                                                               
*                                                                               
         ZIC   R0,STABKEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDBEST,DUB                                                       
         GOTO1 MSUNPK,DMCB,STABKMKT,DDBMKT,DDBSTA                               
         MVI   ELCODE,X'0E'                                                     
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
RNB11    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB13                                                            
RNB12    DS    0H                                                               
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB30                                                            
RNB13    DS    0H                                                               
         CLI   QOPT1,C'B'          IF BILLING DATE REQ                          
         BNE   RNB14                                                            
         CLC   STABBDT,BSSTARTP    FILTER OF BILLING DATE                       
         BL    RNB12                                                            
         CLC   STABBDT,BSENDP                                                   
         BH    RNB12                                                            
         B     RNB15                                                            
*                                                                               
RNB14    DS    0H                                                               
         CLC   STABPER,BSSTART                                                  
         BL    RNB12                                                            
         CLC   STABPER,BSEND                                                    
         BH    RNB12                                                            
*                                                                               
RNB15    DS    0H                                                               
         MVC   WORK(2),STABPER                                                  
         MVI   WORK+2,1            SET DAY TO 1                                 
         GOTO1 ADTCNV,DMCB,(1,WORK),(0,WORK+6)                                  
         MVC   DDBMOS,WORK+6       YYMM  MOS                                    
         GOTO1 ADTCNV,DMCB,(2,STABBDT),(0,WORK)                                 
         MVC   DDBINV(2),WORK+2    BILLING MONTH                                
         MVC   HALF,STABINV        INVOICE NUMBER                               
         NI    HALF,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDBINV+2(4),DUB                                                  
         MVC   FULL,STABGRS                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  DDBGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBGRS+11,X'F0'                                                  
         MVC   FULL,STABNET                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  DDBNET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBNET+11,X'F0'                                                  
         MVC   DDBCD(36),ZEROS                                                  
         MVC   DDBINVD(12),ZEROS                                                
         CLC   QAGY,=C'BJ'          SPECIAL FOR B&J                             
         BNE   RNB25                                                            
         CLC   QCLT,=C'DDS'         ONLY THIS CLIENT                            
         BNE   RNB25                                                            
         MVC   DDBTYP+1(24),ESTNM      ESTIMATE NAME                            
         MVC   DDBTYP+25(5),MGR1       MGROUP CODE                              
         MVC   DDBTYP+30(24),MGR1NM    MGROUP NAME                              
*                                                                               
RNB25    BAS   RE,WRITER                                                        
         AP    DETLCNT,=P'1'                                                    
         B     RNB12               GO DO NEXT ELEM                              
*                                                                               
RNB30    B     RNB4                GO DO NEXT NEW BILL                          
*                                                                               
RNB40    DS    0H                                                               
RNBX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
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
         SPACE 3                                                                
TOTALS   DS    0H                                                               
         LA    R4,TITLES                                                        
         LA    R5,HDRCNT                                                        
         LA    R6,4                FOR BCT                                      
TOT2     MVC   P+7(17),0(R4)                                                    
         EDIT  (P4,0(R5)),(9,P+26),0,COMMAS=YES                                 
         GOTO1 REPORT                                                           
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R6,TOT2                                                          
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P+7(13),=C'TOTAL RECORDS'                                        
         EDIT  OUTCNT,(9,P+26),0,COMMAS=YES                                     
         MVI   P+35,C'*'                                                        
         GOTO1 REPORT                                                           
         L     R2,AOUTFILE                                                      
         CLOSE ((2),)                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
         DC    F'0'                                                             
WRITER    ST    RE,WRITER-4                                                     
         CLI   QOPT2,C'P'                                                       
         BNE   WRIT2                                                            
         MVC   P(125),OUTREC                                                    
         MVC   P2(125),OUTREC+125                                               
         GOTO1 REPORT                                                           
WRIT2    DS    0H                                                               
         L     R1,AOUTFILE                                                      
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         L     RE,WRITER-4                                                      
         BR    RE                                                               
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'ESTIMATE HEADER'                                            
         DC    CL17'SPOT/UNIT DETAIL'                                           
         DC    CL17'INVOICE DETAIL'                                             
         DC    CL17'INVOICE HEADER'                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
SPTHK    CSECT                                                                  
         NMOD1 0,SPTHK                                                          
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R8,MEDBUFF                                                       
         USING MEDBLOCK,R8                                                      
         LA    R9,SPACEND                                                       
         USING SPXTWRKD,R9                                                      
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
*                                                                               
SPTH2    L     R6,SPOTADDR         ADDR OF ELEM                                 
         USING REGELEM,R6                                                       
         CLC   RDATE,BSSTARTP      IGNORE SPOTS OUT OF PERIOD                   
         BL    SPHX                                                             
         CLC   RDATE,BSENDP                                                     
         BH    SPHX                                                             
         LA    R7,OUTREC                                                        
         USING DDSPOT,R7                                                        
         GOTO1 ADTCNV,DMCB,(2,RDATE),(0,DDSDATE)                                
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BE    SPTH2P                                                           
*                                                                               
         ZIC   R0,RNUM             NUMBER OF SPOTS                              
         TM    RSTATUS,X'80'       TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         A     R0,NSPTS                                                         
         ST    R0,NSPTS                                                         
*                                                                               
         LR    RF,R6               GET NEXT SPOT ELEM                           
SPTH2D   DS    0H                                                               
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0             EOR                                          
         BE    SPTH2F                                                           
         CLI   0(RF),6                                                          
         BL    SPTH2D                                                           
         CLI   0(RF),7                                                          
         BH    SPTH2D                                                           
         CLC   RDATE,2(RF)         TEST SAME DATE                               
         BE    SPHX                YES- WAIT TILL DATE BREAK                    
*                                                                               
SPTH2F   DS    0H                                                               
         OC    NSPTS,NSPTS                                                      
         BZ    SPHX                                                             
         MVI   SPTNUM,0                                                         
         B     SPH3                                                             
*                                                                               
SPTH2P   DS    0H                                                               
         TM    RSTATUS,X'C4'       SKIP HIATUS,MINUS,MINUSED                    
         BNZ   SPHX                                                             
         MVC   NSPTS,=F'1'                                                      
         CLC   RDATE,LASTDATE                                                   
         BE    SPH3                                                             
         MVI   SPTNUM,0                                                         
         MVC   LASTDATE,RDATE                                                   
*                                  SPOT NUMBER WITHIN DATE                      
SPH3     DS    0H                                                               
         GOTO1 ADAYUNPK,DMCB,BDDAY,(7,DUB)                                      
         MVC   DDSDAY,DUB                                                       
         GOTO1 GETRATE,DMCB,(X'FF',GRSPOTS),(X'00',(R5)),(R6)                   
         L     R1,GRDOL                                                         
         L     RF,GRSPOTS                                                       
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         M     R0,=F'1'                                                         
         BAS   RE,DIV                                                           
         CVD   R1,DUB                                                           
         UNPK  DDSGROSS,DUB                                                     
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDSGROSS+11,X'F0'                                                
         L     R1,GRDOLN                                                        
         L     RF,GRSPOTS                                                       
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         M     R0,=F'1'                                                         
         BAS   RE,DIV                                                           
         CVD   R1,DUB                                                           
         UNPK  DDSNET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDSNET+11,X'F0'                                                  
         MVC   DDSPAYDT,ZEROS                                                   
         MVC   DDSBLLDT,ZEROS                                                   
*                                                                               
SPH10    DS    0H                  SPECIAL FIELDS                               
         CLC   QAGY,=C'RH'         RUMRILL-HOYT (BACARDI)                       
         BNE   SPH11                                                            
*                                                                               
SPH10A   DS    0H                                                               
         LA    R2,BDELEM                                                        
         MVI   ELCODE,X'66'                                                     
*                                                                               
SPH10B   DS    0H                                                               
         BAS   RE,SPHNXTL                                                       
         BNE   SPH10V                                                           
         LA    R4,9(R2)                                                         
         CLC   =C'PROMO=',3(R2)                                                 
         BE    SPH10F                                                           
         LA    R4,17(R2)                                                        
         CLC   =C'COMMENT-PROMO=',3(R2)                                         
         BNE   SPH10B                                                           
*                                                                               
SPH10F   DS    0H                                                               
         MVC   DDSSPEC(4),0(R4)    PROMO CODE AND STATE                         
         CLI   2(R4),C'/'                                                       
         BE    SPH10H                                                           
         CLI   2(R4),C'-'                                                       
         BNE   SPH10V                                                           
*                                                                               
SPH10H   DS    0H                                                               
         MVC   DDSSPEC+2(2),3(R4)  SKIP SEPARATOR                               
*                                                                               
SPH10V   DS    0H                                                               
         B     SPH20                                                            
*                                                                               
SPH11    DS    0H                                                               
         CLC   QAGY,=C'MC'         MC-CANN      (BACARDI)                       
         BE    SPH10A              SAME AS RH                                   
*                                                                               
SPH20    DS    0H                                                               
         ZIC   R3,SPTNUM                                                        
         LA    R3,1(R3)                                                         
         STC   R3,SPTNUM                                                        
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDSSPOT,DUB                                                      
         CLI   QOPT2,C'P'                                                       
         BNE   SPH22                                                            
         MVC   P(125),OUTREC                                                    
         MVC   P2(125),OUTREC+125                                               
         GOTO1 REPORT                                                           
SPH22    DS    0H                                                               
         L     R1,AOUTFILE                                                      
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         AP    SPOTCNT,=P'1'                                                    
*                                                                               
         L     RF,NSPTS            REPEAT FOR NUMBER OF SPOTS                   
         BCTR  RF,R0                                                            
         ST    RF,NSPTS                                                         
         LTR   RF,RF                                                            
         BP    SPH20                                                            
*                                                                               
SPHX     XIT1                                                                   
         SPACE 3                                                                
SPHNXTL  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SPHNXTL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     SPHNXTL                                                          
*                                                                               
SPHNXTL2 DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 3                                                                
DIV      DRNDR (R0),(RF)                                                        
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=SXTTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00250,                                            X        
               BLKSIZE=02000,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        NTUPROC - GET NETPAK UNITS                                             
*                                                                               
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
NTUPROC  NMOD1 0,**NTUP                                                         
         LA    RC,2048(RA)         RESTORE RC                                   
         LA    RC,2048(RC)                                                      
*                                                                               
         CLI   QOPT4,C'N'          TEST TO SKIP BUYS                            
         BE    NTUX                                                             
*                                  GET BN PROFILE                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0BN'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         CLI   COFFICE-CLTHDR(RF),C' '                                          
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE-CLTHDR(RF)                                    
         GOTO1 GETPROF,DMCB,WORK,PROFBN,DATAMGR                                 
*                                  GET NETIO                                    
         OC    ANETIO,ANETIO       ONCE ONLY                                    
         BNZ   NTU07                                                            
*                                                                               
         GOTO1 LOADER,DMCB,=C'T00A27  '    LOAD IN NETIO                        
         MVC   ANETIO,DMCB+4                                                    
*                                                                               
         L     R4,ADBUY            USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
*                                                                               
NTU07    DS    0H                                                               
         L     R8,=A(NETBLK)                                                    
         A     R8,RELO                                                          
         ST    R8,ANETBLK                                                       
         USING NETBLOCK,R8                                                      
*                                                                               
         LA    RE,NETBLOCK                                                      
         LH    RF,=Y(NBBLKEND-NETBLOCK)                                         
         XCEF                                                                   
         B     NTU08                                                            
*                                                                               
NUFLIST  DC    CL8'UUNTFIL'        U= OPEN FOR UPDATE                           
         DC    CL8'UUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
*                                                                               
NTU08    DS    0H                                                               
         OC    NTPATCH,NTPATCH     TEST ANY NETIO PATCH NEEDED                  
         BZ    NTU08H                                                           
         SR    RF,RF                                                            
         ICM   RF,3,NTPATCH                                                     
         A     RF,ANETIO                                                        
         ZIC   RE,NTPATCH+2                                                     
         BCTR  RE,0                                                             
         EX    RE,NTPMVC                                                        
         B     NTU08H                                                           
*                                                                               
NTPMVC   MVC   0(0,RF),NTPATCH+3                                                
NTPATCH  DC    48X'00'                                                          
*                                                                               
NTU08H   DS    0H                                                               
         L     R8,ANETBLK                                                       
*                                  SET SELECT OPTIONS                           
         MVC   NBSELAGY(3),QAGY    AGY/MED                                      
         MVC   NBSELCLI,CLT        CLIENT                                       
*                                  (ASSUME ALL PRD/EST)                         
         CLI   QOPT1,C'B'          IF DOING BILL DETAILS                        
         BNE   *+14                                                             
         MVC   NBSELSTR(12),=C'800101991231'   READ ALL DATES                   
         B     NTU08J                                                           
*                                                                               
         MVC   NBSELSTR(12),SVQSTART                                            
         CLI   NBSELSTR+4,C' '       IF NO START DAY                            
         BH    *+10                                                             
         MVC   NBSELSTR+4(2),=C'01'  SET TO START OF MONTH                      
         CLI   NBSELEND+4,C' '       IF NO END DAY                              
         BH    *+10                                                             
         MVC   NBSELEND+4(2),=C'31'  SET TO END OF MONTH                        
*                                  SET DATA OPTIONS                             
NTU08J   DS    0H                                                               
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'D'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
*                                                                               
         MVC   NBAIO,ADBUY         USE BUY RECORD AREA                          
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
NTU10    DS    0H                                                               
         GOTO1 ANETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBINVPRD    NO PRODUCT SET UP?                           
         BE    NTUX                                                             
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU12                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTU30                                                            
         B     NTU10                                                            
*                                                                               
         SPACE 3                                                                
*        PROCESS UNIT                                                           
         SPACE 2                                                                
NTU12    DS    0H                                                               
         L     R5,NBAIO                                                         
         USING NURECD,R5                                                        
         CLI   QOPT1,C'B'          UNLESS DOING BILL DETAILS                    
         BE    *+12                                                             
         TM    NURSTAT,X'80'       SKIP DELETES                                 
         BNZ   NTU10                                                            
*                                                                               
         GOTO1 NETCOST,DMCB,ANETBLK,X                                           
         LA    R6,X                                                             
         USING NCOSTD,R6                                                        
*                                                                               
         LA    R7,OUTREC                                                        
         USING DDUNIT,R7                                                        
         XC    OUTREC,OUTREC                                                    
         MVI   DDUSYS,C'N'                                                      
         MVC   DDUCOD,=C'UN'                                                    
         MVC   DDUAGY,QAGY                                                      
         MVC   DDUMED,QMED                                                      
         MVC   DDUCLT,CLT                                                       
         ZIC   R0,NBACTEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDUEST,DUB                                                       
         MVC   DDUPRD,NCP1PC3      1ST PRD (NOTE- PIGGYS NOT SUPPORTED)         
         MVC   DDUNTWK,NBACTNET    NETWORK                                      
         MVC   DDUPGMC,NBACTPRG    PROGRAM CODE                                 
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,DDUAIRDT)                            
         EDIT  (B1,NBACTSUB),(3,DDUSUBL),FILL=0                                 
         EDIT  (B1,NBLEN),(3,DDULEN),FILL=0                                     
         EDIT  (B2,NBTIME),(4,DDUSTIM),FILL=0                                   
         EDIT  (B2,NBTIME+2),(4,DDUETIM),FILL=0                                 
*                                                                               
         MVC   DDUPRD2,SPACES      SET FOR NO PIGGYBACK                         
         MVC   DDUSHR1,=C'10000'                                                
         CLI   NBPRD2,0            TEST ANY PIGGYBACK                           
         BE    NTU12C                                                           
         MVC   DDUPRD2,NCP2PC3     2ND PRD IF PIGGYBACK                         
         EDIT  (B2,NBP1SHR),(5,DDUSHR1),FILL=0                                  
*                                                                               
NTU12C   DS    0H                                                               
         ICM   R0,15,NCP1GRS                                                    
         CVD   R0,DUB                                                           
         UNPK  DDUGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDUGRS+11,X'F0'                                                  
*                                                                               
         ICM   R0,15,NCP1NET                                                    
         CVD   R0,DUB                                                           
         UNPK  DDUNET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDUNET+11,X'F0'                                                  
*                                                                               
         MVC   DDUPGMN,NBPROGNM                                                 
         GOTO1 ADAYUNPK,DMCB,NBDAY,(7,DUB)                                      
         MVC   DDUDAY,DUB                                                       
         MVC   DDUDPT,NBACTDP      DAYPART                                      
*                                  STATUS                                       
         MVC   DDUSTAT,=C'DL'      DELETED (MAY HAVE BEEN BILLED?)              
         TM    NURSTAT,X'80'                                                    
         BNZ   NTU13                                                            
         MVC   DDUSTAT,=C'PR'      PREEMPT                                      
         TM    NUUNITST,X'40'                                                   
         BNZ   NTU13                                                            
         MVC   DDUSTAT,=C'MI'      MINUS UNIT                                   
         TM    NUUNITST,X'40'                                                   
         BNZ   NTU13                                                            
         MVC   DDUSTAT,=C'MS'      MISSED                                       
         TM    NUUNITST,X'02'                                                   
         BNZ   NTU13                                                            
         MVC   DDUSTAT,=C'MG'      MAKE-GOOD                                    
         TM    NUUNITST,X'01'                                                   
         BNZ   NTU13                                                            
         MVC   DDUSTAT,SPACES                                                   
*                                                                               
NTU13    DS    0H                                                               
         CLI   QOPT1,C'B'          TEST DOING BILLING DETAILS                   
         BNE   NTU18               NO, WRITE AS IS AND GET NEXT UNIT            
*                                                                               
         LA    R5,NUDATA           LOOK FOR BILLING ELEMS                       
*                                                                               
NTU14    DS    0H                                                               
         CLI   0(R5),0             EOR                                          
         BE    NTU16                                                            
         CLI   0(R5),X'10'         BILLING ELEM                                 
         BE    NTU15                                                            
*                                                                               
NTU14B   DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     NTU14                                                            
*                                                                               
NTU15    DS    0H                                                               
         USING NUBILD,R5                                                        
         TM    NUBILST,NUBILUBQ    IF UNBILLED                                  
         BNZ   NTU14B              SKIP                                         
         CLC   NUBILDAT,BSSTARTP   TEST BILLDATE VS START-END                   
         BL    NTU14B                                                           
         CLC   NUBILDAT,BSENDP                                                  
         BH    NTU14B                                                           
*                                                                               
         ICM   R0,15,NUBILGRS                                                   
         CVD   R0,DUB                                                           
         UNPK  DDUGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDUGRS+11,X'F0'                                                  
*                                                                               
         ICM   R0,15,NUBILNET                                                   
         CVD   R0,DUB                                                           
         UNPK  DDUNET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDUNET+11,X'F0'                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,DUB)                                 
         MVC   DDUINV(2),DUB+2     MONTH OF BILL                                
         MVC   DDUINV+2(4),NUBILNUM                                             
*                                                                               
         MVC   DDUCTYP,NUBILTYP    COST TYPE                                    
*                                                                               
         MVC   DDUPRD2,SPACES      NO PIGGY FOR UD RECS                         
         MVC   DDUSHR1,=C'10000'                                                
*                                                                               
         GOTO1 GETPRC,DMCB,NUBILPRD,DDUPRD                                      
         MVC   DDUCOD,=C'UD'                                                    
         BAS   RE,UNOUT                                                         
         B     NTU14B              NEXT ELEM                                    
*                                                                               
NTU16    DS    0H                                                               
         B     NTU10               NEXT UNIT                                    
*                                                                               
NTU18    DS    0H                                                               
         BAS   RE,UNOUT                                                         
         B     NTU10               NEXT UNIT                                    
*                                                                               
NTU30    DS    0H                                                               
NTUX     DS    0H                                                               
         DROP  R5                                                               
         XIT1                                                                   
         SPACE 2                                                                
***********************************************************************         
*        UNOUT - WRITE UNIT RECORD                                              
***********************************************************************         
         SPACE 1                                                                
UNOUT    NTR1                                                                   
         CLI   QOPT2,C'P'                                                       
         BNE   UNO2                                                             
         MVC   P(125),OUTREC                                                    
         MVC   P2(125),OUTREC+125                                               
         GOTO1 REPORT                                                           
UNO2     DS    0H                                                               
         L     R1,AOUTFILE                                                      
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         AP    SPOTCNT,=P'1'                                                    
         XIT1                                                                   
         SPACE 2                                                                
***********************************************************************         
*        GETPRC - GET PRODUCT CODE FROM PRODUCT BYTE                            
*                                                                               
*      PARM1        A(1-BYTE PRODUCT CODE)                                      
*      PARM2        A(3-BYTE PRODUCT CODE)                                      
*                                                                               
***********************************************************************         
         SPACE 1                                                                
GETPRC   NTR1                                                                   
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDR(RF)                                              
         L     R2,0(R1)            A(1-BYTE)                                    
         L     R3,4(R1)            A(3-BYTE)                                    
         MVC   0(3,R3),=C'***'     UNALLOCATED                                  
         CLI   0(R2),0                                                          
         BE    GPRCX                                                            
*                                                                               
GPRC2    DS    0H                                                               
         CLC   0(1,R2),3(RF)                                                    
         BE    GPRC4                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),0                                                          
         BNE   GPRC2                                                            
         B     GPRCX                                                            
*                                                                               
GPRC4    DS    0H                                                               
         MVC   0(3,R3),0(RF)                                                    
*                                                                               
GPRCX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        NET COST - GET TOTAL GROSS/NET FOR UNIT                                
*                                                                               
*      PARM1        A(NETBLOCK)                                                 
*      PARM2        A(OUTPUT) - SEE NCOSTD                                      
*                                                                               
***********************************************************************         
         SPACE 1                                                                
NETCOST  NTR1                                                                   
         L     R8,0(R1)            A(NETBLOCK)                                  
         USING NETBLOCK,R8                                                      
         L     R5,NBAIO                                                         
         USING NURECD,R5                                                        
         L     R6,4(R1)            A(OUTPUT) - NCOSTD                           
         USING NCOSTD,R6                                                        
         XC    NCOSTD(NCOSTDL),NCOSTD                                           
*                                  TIME COST                                    
         BAS   RE,SETTIM           ELSE GET ORDERED TIME CHARGES                
*                                                                               
         MVC   NCP1PC1,NUPRD       SET PRODUCT CODES                            
         MVC   NCP2PC1,NUPRD2                                                   
         GOTO1 GETPRC,DMCB,NCP1PC1,NCP1PC3                                      
         GOTO1 (RF),(R1),NCP2PC1,NCP2PC3                                        
*                                                                               
         MVC   FULL,NBINTEG        INTEGRATION                                  
         TM    NBUNITST,X'80'      TEST MINUS UNIT                              
         BZ    NC4B                                                             
         L     RF,FULL             MAKE AMOUNT NEGATIVE                         
         LNR   RF,RF                                                            
         ST    RF,FULL                                                          
*                                                                               
NC4B     DS    0H                  GET GROSS AND NET                            
         IC    R0,NBRTTYPE         RATE TYPE                                    
         TM    NBPACKST,X'04'      TEST NON-COM INTEGRATION                     
         BZ    *+12                                                             
         LA    R0,C'F'             IF SO, NET=GROSS                             
         B     NC4D                                                             
         CLI   NBSDRTCV,C'T'       IF RATE TYPE COVERAGE                        
         BNE   *+6                 IS FOR TIME ONLY                             
         SR    R0,R0               THEN INTEGRATION NET = 85%                   
*                                                                               
NC4D     DS    0H                                                               
         GOTO1 =V(NETNET),DMCB,((R0),FULL),DUB,RR=RELO                          
         L     R0,NCP1GRS                                                       
         A     R0,DUB                                                           
         ST    R0,NCP1GRS                                                       
         L     R0,NCP1NET                                                       
         A     R0,DUB+4                                                         
         ST    R0,NCP1NET                                                       
         DROP  R5                                                               
*                                                                               
*                                  OTHER CHARGES                                
NC6      DS    0H                                                               
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         LA    R2,NUDATA                                                        
*                                                                               
NC6D     DS    0H                                                               
         CLI   0(R2),X'03'         SPECIAL COST ELEM                            
         BE    NC7B                                                             
         CLI   0(R2),0                                                          
         BE    NC20                                                             
*                                                                               
NC7      DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     NC6D                                                             
*                                                                               
NC7B     DS    0H                                                               
         USING NUSPRD,R2                                                        
*                                                                               
         ICM   R0,15,NUSPRAMT      GROSS                                        
         TM    NBUNITST,X'80'      IF MINUS UNIT                                
         BZ    *+6                                                              
         LNR   R0,R0               MAKE AMOUNT NEGATIVE                         
         ST    R0,FULL                                                          
*                                                                               
         CLI   NUSPRCOM,C'C'       IF NOT COMMISSIONABLE                        
         BE    *+12                                                             
         LA    R0,C'F'             GROSS=NET                                    
         B     NC7F                                                             
         IC    R0,NBRTTYPE         ELSE USE RATE TYPE                           
         CLI   NBSDRTCV,C'A'       IF SPECIALS COVERED                          
         BE    NC7F                                                             
         CLI   NBSDRTCV,C' '                                                    
         BNH   NC7F                                                             
         SR    R0,R0               ELSE NET=85%                                 
*                                                                               
NC7F     DS    0H                                                               
         GOTO1 =V(NETNET),DMCB,((R0),FULL),DUB,RR=RELO                          
*                                                                               
         L     R0,NCP1GRS          ADD IN GROSS                                 
         A     R0,DUB                                                           
         ST    R0,NCP1GRS                                                       
         L     R0,NCP1NET          AND NET                                      
         A     R0,DUB+4                                                         
         ST    R0,NCP1NET                                                       
         B     NC7                 NEXT ELEMENT                                 
*                                                                               
NC20     DS    0H                                                               
         BAS   RE,SETSHR                                                        
*                                                                               
NCX      DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        GET ORDERED TIME CHARGES                                               
         SPACE 2                                                                
SETTIM   NTR1                                                                   
         XC    FULL,FULL                                                        
         TM    NBUNITST,X'42'      PRE-EMPT OR MISSED                           
         BNZ   SETTIMX                                                          
         TM    NURSTAT,X'80'       DELETED                                      
         BNZ   SETTIMX             ORDERED = ZERO                               
*                                                                               
         CLI   PROFBN+3,C'Y'       TEST TO BILL ACTUAL                          
         BE    SETTIM4                                                          
         OC    FULL,NBASSIGN       ELSE USE ASSIGNED                            
         BNZ   SETTIM6             IF PRESENT                                   
         TM    NBUNITST,X'08'      OR IF ENTERED AS ZERO                        
         BNZ   SETTIM6                                                          
*                                                                               
SETTIM4  DS    0H                  BILLING ACTUAL                               
         TM    NBUNITST,X'20'      MUST HAVE BEEN ENTERED (7/15/85)             
         BZ    SETTIMX             ELSE ZERO TIME ORDERED                       
         MVC   FULL,NBACTUAL       USE ACTUAL COST                              
*                                                                               
SETTIM6  DS    0H                                                               
         TM    NBUNITST,X'80'      MINUS UNIT                                   
         BZ    *+14                                                             
         L     RF,FULL             MAKE AMOUNT NEGATIVE                         
         LNR   RF,RF                                                            
         ST    RF,FULL                                                          
*                                                                               
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,FULL),DUB,RR=RELO                      
         MVC   NCP1GRS,DUB                                                      
         MVC   NCP1NET,DUB+4                                                    
*                                                                               
SETTIMX  DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
SETSHR   NTR1                      GET PIGGY SHARE                              
         CLI   NUPRD2,0            ANY 2ND PRD                                  
         BE    SETSX                                                            
         MVC   DUB(8),NCP1GRS      SAVE TOTALS                                  
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR        PRD 1 SHARE                                  
         BNZ   SETS1                                                            
         TM    NUUNST2,X'04'       IF ZERO, TEST TRULY ZERO                     
         BNZ   SETS1                                                            
         LH    RF,=H'5000'         DEFAULT IS 50 PCT                            
SETS1    DS    0H                                                               
         ST    RF,FULL                                                          
         L     RF,=F'10000'        SHARE IS N.NN PCT                            
*                                                                               
         L     R1,NCP1GRS                                                       
         M     R0,FULL             SHARE IS IN FULL                             
         BAS   RE,NCDIV                                                         
         ST    R1,NCP1GRS                                                       
*                                                                               
         L     R1,NCP1NET                                                       
         M     R0,FULL             SHARE IS IN FULL                             
         BAS   RE,NCDIV                                                         
         ST    R1,NCP1NET                                                       
*                                                                               
         L     RF,DUB              SET DIFFERENCE IN PRD 2 SLOTS                
         S     RF,NCP1GRS                                                       
         ST    RF,NCP2GRS                                                       
*                                                                               
         L     RF,DUB+4                                                         
         S     RF,NCP1NET                                                       
         ST    RF,NCP2NET                                                       
*                                                                               
SETSX    DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
NCDIV    DRNDR (R0),(RF)                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
STABUCKC DS    CL2000                                                           
         DC    X'00'                                                            
*                                                                               
         DS    0D                                                               
NETBLK   DS    CL4096                                                           
         DC    X'00'                                                            
         EJECT                                                                  
SPXTWRKD DSECT                                                                  
RELO     DC    F'0'                                                             
OUTCNT   DS    PL4'0'                                                           
HDRCNT   DS    PL4'0'                                                           
SPOTCNT  DS    PL4'0'                                                           
DETLCNT  DS    PL4'0'                                                           
BHDRCNT  DS    PL4'0'                                                           
         SPACE 2                                                                
KPRD     DS    CL1                                                              
KMKT     DS    CL2                                                              
KSTA     DS    CL3                                                              
*                                                                               
SPTNUM   DS    CL1                                                              
LASTDATE DS    CL2                                                              
*                                                                               
ELCODE   DS    CL1                                                              
ZEROS    DS    CL50                                                             
*                                                                               
MYKEY    DS    CL32                                                             
SAVPRDC  DS    CL3                                                              
SAVPRD   DS    X                                                                
SAVPGR   DS    XL2                                                              
SAVPGRU  DS    CL4                                                              
SAVMKT   DS    XL2                                                              
SAVMGR   DS    XL2                                                              
SAVMGRU  DS    CL4                                                              
NETPAKSW DS    C                                                                
*                                                                               
OUTREC   DS    CL250                                                            
         DS    F                                                                
ADSTABUC DS    A                                                                
AOUTFILE DS    A                                                                
ADTCNV   DS    A                                                                
ADAYUNPK DS    A                                                                
GRSPOTS  DS    F                   FOR GETRATE                                  
GRDOL    DS    F                                                                
GRDOLN   DS    F                                                                
GRDOLF   DS    F                                                                
NSPTS    DS    F                                                                
SVQSTART DS    CL6                                                              
SVQEND   DS    CL6                                                              
BSSTART  DS    XL3                                                              
BSEND    DS    XL3                                                              
BSSTARTP DS    XL2                                                              
BSENDP   DS    XL2                                                              
MONLST   DS    XL80                                                             
*                                                                               
         DS    0D                                                               
PROFBN   DS    XL16                                                             
ANETIO   DS    A                                                                
ANETBLK  DS    A                                                                
X        DS    XL64                                                             
         SPACE 2                                                                
NCOSTD   DSECT                     DSECT FOR NETCOST RETURN                     
NCP1GRS  DS    F                   PRD 1 GROSS                                  
NCP1NET  DS    F                         NET                                    
NCP1PC1  DS    XL1                       1-BYTE CODE                            
NCP1PC3  DS    CL3                       3-BYTE CODE                            
NCP2GRS  DS    F                   PRD 2 GROSS                                  
NCP2NET  DS    F                         NET                                    
NCP2PC1  DS    XL1                       1-BYTE CODE                            
NCP2PC3  DS    CL3                       3-BYTE CODE                            
NCOSTDL  EQU   *-NCOSTD                                                         
*                                                                               
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENINTFC                                                     
*                                                                               
       ++INCLUDE SPGENBILL                                                      
*                                                                               
       ++INCLUDE SPGENSTAB                                                      
*                                                                               
       ++INCLUDE SPGENBUY                                                       
*                                                                               
       ++INCLUDE SPGENCLT                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENMKA                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPXT02 04/01/03'                                      
         END                                                                    
