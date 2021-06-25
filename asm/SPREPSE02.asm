*          DATA SET SPREPSE02  AT LEVEL 103 AS OF 02/09/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045021.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE SPSE02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE SPBVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'SPSE02 - SPRINT INTERFACE'                                      
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*   BPLA  06/10   REPORT EST USER 2 DATA IN CustomerOrderNumber                 
*                                                                               
*   BPLA  06/10   CHANGE TO MINDSHARE'S REMITTANCE ADDRESS                      
*                                                                               
*   BPLA  03/10   REPORT NETPAK SUBMEDIA O INVS. HONORING THE PROFILES          
*                                                                               
*   BPLA  08/07   CHANGES FOR SPRINT'S CHANGE TO SPREINT NEXTEL                 
*                                                                               
*   BPLA  11/06   FIX NET INV # IN FILE, SORT NET BY INV #                      
*                 REPORT NET INVOICES #'S USING SEB-MEDIA B1 PROFILES           
*                                                                               
*   BPLA  06/06   REWORK FOR MINDSHARE                                          
*                                                                               
*   BPLA  04/06   VCLIST CHECKING CHANGED                                       
*                                                                               
*   BPLA  09/05   OFFICE/GETPROF CHANGE                                         
*                                                                               
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 P=PDUMP RECORDS                                                  
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*                                                                               
SPSE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSE02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R8,SPACEND                                                       
         USING SPSEWRKD,R8                                                      
         LA    R7,SPSE02+4095                                                   
         LA    R7,1(R7)                                                         
         USING SPSE02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,CLTFRST                                                     
         BE    FCLI                                                             
         CLI   MODE,ESTFRST                                                     
         BE    FEST                                                             
         CLI   MODE,PRDFRST                                                     
         BE    FPRD                                                             
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
         CLI   MODE,PROCBILL                                                    
         BE    PROCESS                                                          
*                                                                               
         CLI   MODE,CLTLAST       END OF CLIENT                                 
         BE    LCLI                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                         RUN FIRST                                             
INITIAL  DS    0H                                                               
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         MVC   BRDMON,VBRDMON                                                   
         DROP  RE                                                               
*                                                                               
         L     RF,=A(PROCNET)                                                   
         A     RF,RELO                                                          
         ST    RF,VPROCNET                                                      
*                                                                               
         L     RF,=A(STABUCKC)                                                  
         A     RF,RELO                                                          
         ST    RF,ADSTABUC                                                      
*                                                                               
         L     RF,=A(NETBLK)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETBLK                                                       
*                                                                               
         L     R0,=V(GETUSER)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETUSER                                                      
*                                                                               
         L     R0,=V(DDUCOM)                                                    
         A     R0,RELO                                                          
         ST    R0,VDDUCOM                                                       
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   AFMTINO,VSPFMINO   A(SPFMTINO)                                   
         DROP  RF                                                               
*                                                                               
         L     R0,=V(PERVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,APERVAL                                                       
         L     R0,=V(SPBVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,ASPBVAL                                                       
*                                                                               
         XC    MYDUMP,MYDUMP                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)       YYMMDD                        
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   TOTDOL,=P'0'                                                     
         MVI   COLSW,C'N'                                                       
         MVI   IHDRSW,C'N'                                                      
         MVI   FRSTBILL,C'N'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'                                                    
         ZAP   INVRCNT,=P'0'                                                    
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
         MVI   ALLOWSW,0     DYNAMIC ALLOCATION INV. FILE NOT DONE              
*                                                                               
INIT5    DS    0H                                                               
         L     R0,=A(TITLES)                                                    
         A     R0,RELO                                                          
         ST    R0,ATITLES                                                       
*                                                                               
         L     R0,=A(LENTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,ALENTAB                                                       
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,2                                                             
INIT7L   ZAP   4(4,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,INIT7L                                                        
*                                                                               
*                                                                               
INIT8    MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
*                                                                               
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         CLI   MCNETPAK,C'Y'       SEE IF NETPAK                                
         BE    INIT8X                                                           
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   NETOPT,0                                                         
**                                                                              
INIT8X   DS    0H                                                               
*                                                                               
INIT10   MVI   FCRDBUYS,C'Y'       RESET TO READ BUYS                           
         CLI   NETOPT,C'N'         CAN ONLY BE FOR NEW NETPAK                   
         BNE   INIT30                                                           
         MVI   FCRDBUYS,C'N'       SET FOR NO BUYS                              
*                                 (I'LL BE READING NETPAK UNITS)                
INIT30   CLI   NETOPT,C'N'         SEE IF DOING NEW NETWORK                     
         BNE   INIT70                                                           
         BC    0,INIT70                                                         
         OI    *-3,X'F0'           ONLY DO ONCE                                 
         L     R4,ADBUY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
         B     INIT70                                                           
*                                                                               
NUFLIST  DC    CL8'NUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                  REQS FOR ONE PRD                             
INIT70   DS    0H                                                               
**                                                                              
INITX    B     EXIT                                                             
         EJECT                                                                  
*                       REQUEST FIRST                                           
FIRSTB   DS    0H                                                               
*                                                                               
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEND,QSTART    IF END MISSING SET TO START                       
*                                                                               
         MVC   DYNDDN,=CL8'SSETAPE'                                             
         MVC   DYNDSN,=CL20'NETTAPE.NE0SEXX1'                                   
         MVC   DYNDSN+13(2),QAGY                                                
*                                                                               
         CLI   NETOPT,C'N'      NETPAK?                                         
         BE    REQF10                                                           
         MVC   DYNDSN(3),=C'SPT'    ALTER FOR SPOT                              
         MVC   DYNDSN+8(2),=C'SP'                                               
*                                                                               
REQF10   DS    0H                                                               
         XC    LBILLKEY,LBILLKEY                                                
         XC    LESTOUT,LESTOUT                                                  
         MVI   NACTIVE,C'N'                                                     
         MVC   SVQOPT1,QOPT1      SAVE FILE TYPE                                
         MVC   SVQOPT6,QOPT6      SAVE DO TAPE OPTION                           
         MVC   SVQOPT7,QOPT7      SAVE PDUMPING OPTION                          
*                                                                               
         CLC   QEST(2),=C'NO'                                                   
         BE    REQF65                                                           
REQF50   CLC   QEST(6),SPACES                                                   
         BNE   REQF55                                                           
         MVC   QEST(6),=C'001255'                                               
         B     REQF60                                                           
REQF55   CLC   QESTEND,SPACES                                                   
         BNE   REQF60                                                           
         MVC   QESTEND,QEST      IF ONLY ONE EST SET QESTEND TO IT              
REQF60   PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,MYBEST                                                        
         PACK  DUB,QESTEND                                                      
         CVB   R0,DUB                                                           
         STC   R0,MYBESTE          NEED TO SET NOW FOR TBCLTF                   
         B     REQF70                                                           
*                                                                               
REQF65   MVI   MYBEST,1                                                         
         MVI   MYBESTE,255                                                      
REQF70   DS    0H                                                               
*                                  SPONSOR DOES IT LATER FOR                    
         MVI   RQPRDAAA,C'Y'                                                    
         MVI   RQRDPOL,C'N'                                                     
*                                                                               
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
*                                                                               
         TM    ALLOWSW,X'01'     DYNAMIC ALLOCATION DONE ALREADY?               
         BO    REQF75                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DYNDDN),(0,DYNDSN)                              
         OI    ALLOWSW,X'01'    SO I WON'T DO AGAIN                             
         OPEN  (SSETAPE,OUTPUT)                                                 
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
MIXERR   MVC   P1(37),=C'*** MIX OF TEST AND LIVE REQUESTS ***'                 
         MVC   P2(37),=C'*** THIS REQUEST HAS BEEN SKIPPED ***'                 
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST    SKIP TO NEXT REQUEST                             
         B     EXIT                                                             
*                                                                               
FIRSTB0X DS    0H                                                               
*                             SET MYUSER FROM AGYTAB                            
         LA    R1,AGYTAB                                                        
FIRSTB1  CLI   0(R1),X'FF'            END OF TABLE                              
         BNE   *+6                                                              
         DC    H'0'                   INVALID AGENCY                            
*                                                                               
         CLC   0(2,R1),QAGY                                                     
         BE    FIRSTB1D                                                         
         LA    R1,AGYTABL(R1)                                                   
         B     FIRSTB1                                                          
*                                                                               
FIRSTB1D MVC   MYUSER,2(R1)         USER FIELDS IN USE                          
*                                                                               
FIRSTB2  DS    0H                                                               
         XC    ESTU1,ESTU1          CLEAR USER FIELDS                           
         XC    ESTU2,ESTU2                                                      
         XC    PRDU1,PRDU1                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVBIL,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVRCV,=P'0'                                                    
         MVI   CINVSW,0                                                         
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SVQSTART)                              
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
         MVC   SVQEND,=3X'FF'                                                   
*                                                                               
         CLI   NETOPT,C'N'             IS THIS NETPAK?                          
         BE    FIRSTB2X                YES - SKIP MEDBLOCK STUFF                
*                                                                               
*        ESTABLISH MEDBLOCK                                                     
*                                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         XC    MEDNUMWK(8),MEDNUMWK                                             
         MVI   MEDNUMMO+3,13       13 MONTHS?                                   
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'320'    **ENOUGH FOR 2ND CURRENCY???                 
         MVI   MEDEXTDM,0                                                       
         MVI   MEDEXTAC,C'Y'                                                    
*                                                                               
         GOTO1 MEDPRDRD,DMCB,SPWORKD                                            
*                                                                               
         DROP  RF                                                               
*                                                                               
FIRSTB2X CLC   QEND,SPACES                                                      
         BE    FIRSTB3                                                          
         GOTO1 DATCON,DMCB,(0,QEND),(3,SVQEND)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
FIRSTB3  MVC   MSVSTART(12),QSTART   SAVE EBCDIC DATES FOR BUY CHK              
*                                                                               
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
         MVC   MSVEND(6),=6X'FF'   FOR 21ST CENTURY                             
*                                                                               
FIRSTB3C DS    0H                                                               
*                            SO ALL BUCKETS WILL BE READ                        
*                            I MUST CLEAR THESE START AND END DATES             
         MVC   QSTART,SPACES                                                    
         MVC   QEND,SPACES                                                      
*                                                                               
         MVC   BQSTART,=X'000000'                                               
         MVC   BQEND,=X'FFFFFF'                                                 
*                                                                               
FIRSTB3X LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
*                                                                               
FIRSTB4  DS    0H                                                               
FIRSTBX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                  LAST FOR CLIENT                              
LCLI     DS    0H                                                               
         CLI   NETOPT,C'N'        NETPAK?                                       
         BNE   LCLI80             NO - DO NOTHING                               
*                      UNIT PROCESSING NOW IN IT'S OWN CSECT                    
         GOTO1 VPROCNET,DMCB                                                    
*                                                                               
*        NOW READ BUFFALO RECORDS AND PROCESS USING BLDREC                      
*                                                                               
         CLI   NACTIVE,C'Y'         ANY DATA?                                   
         BNE   LCLI80                                                           
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'I'                                                     
         MVI   TOTTYP,C'I'                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     LCLI10                                                           
LCLI5    GOTO1 BUFFALO,DMCB,=C'SEQ',(TOTTYP,BUFFBUFF),BUFREC,0                  
LCLI10   CLI   DMCB+8,X'80'      END OF FILE                                    
         BE    LCLI80                                                           
*                                                                               
*                                                                               
*        JIM CLONKE SAID TO SKIP ZERO RECORDS JUN10/05                          
*                                                                               
         CP    BUFNET,=P'0'        ZERO $                                       
         BE    LCLI5                SKIP                                        
*                                                                               
         MVI   CKESTREC,C'F'         SET FROM BUFFALO REC                       
         MVC   CKUPRD,BUFPRD                                                    
         GOTO1 =A(CKEST)         GET ESTIMATE AND SET UDEFS                     
*                                                                               
         BAS   RE,FNDPRD        MUST FIND AND READ PRODUCT                      
*                                RETURN IN ADPRD                                
         GOTO1 =A(BLDREC)                                                       
         B     LCLI5                                                            
*                                                                               
LCLI80   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
FNDPRD   NTR1                                                                   
         L     R5,VCLIST       EXPANDED PRODUCT LIST                            
*                                                                               
FNDP5    CLC   3(1,R5),BUFPRD                                                   
         BE    FNDP5X                                                           
         LA    R5,4(R5)                                                         
         CLI   0(R5),0      END OF LIST?                                        
         BNE   FNDP5                                                            
         DC    H'0'         CAN'T FIND PRODUCT                                  
*                                                                               
FNDP5X   DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BUFAM   AGY/MED                                         
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),0(R5)                                                   
         L     RF,ADPRD                                                         
         CLC   0(8,RF),KEY                                                      
         BE    FNDP10                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'       PRODUCT NOT ON FILE                                   
*                                                                               
         GOTO1 GETPRD                                                           
*                                                                               
FNDP10   MVC   PRD,0(R5)       ALSO SET IN PRD                                  
*                                                                               
FNDPX    MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
FCLI     DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1PROFC,B1PROFC      B1 FOR SUBMEDIA C- CABLE                    
         XC    B1PROFO,B1PROFO      B1 FOR SUBMEDIA O- CINEMA (SPRINT)          
         XC    B1PROFS,B1PROFS      B1 FOR SUBMEDIA S- SYNDICATION              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*      NOW ALWAYS PASS OFFICE DATA                                              
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVI   WORK+6,C'C'                                                      
         GOTO1 GETPROF,DMCB,WORK,B1PROFC,DATAMGR                                
         MVI   WORK+6,C'O'                                                      
         GOTO1 GETPROF,DMCB,WORK,B1PROFO,DATAMGR                                
         MVI   WORK+6,C'S'                                                      
         GOTO1 GETPROF,DMCB,WORK,B1PROFS,DATAMGR                                
         MVC   WORK+6(1),MED      RESTORE                                       
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         XC    LINVFULL,LINVFULL      CLEAR FOR NEW REQ                         
         XC    LBQDATE,LBQDATE                                                  
         MVI   FRSTBILL,C'Y'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'     CLEAR TOTAL AMOUNT DUE                         
         XC    INVTAB,INVTAB     CLEAR INVOICE DETAIL TABLE                     
*                                                                               
TBCF60   DS    0H                                                               
         BAS   RE,FNDAAA        MUST FIND AND READ PRODUCT AAA                  
*                                RETURN IN ADPRD                                
         B     EXIT                                                             
*                                                                               
FNDAAA   NTR1                                                                   
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD  AGY/MED                                         
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=C'AAA'                                                 
         L     RF,ADPRD                                                         
         CLC   0(8,RF),KEY                                                      
         BE    FNDP10                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'       PRODUCT NOT ON FILE                                   
*                                                                               
         GOTO1 GETPRD                                                           
*                                                                               
         L     RE,ADPRD                                                         
         USING PRDHDR,RE                                                        
         MVC   APADDR1(120),PADDR1     SAVE AAA 4 ADDRESS LINES                 
         DROP  RE                                                               
*                                                                               
FNDAX    MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY FOR PRODUCT                        
*                                                                               
TBPRD5   DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD                                                    
TBCF3    GOTO1 HIGH                                                             
         B     TBCF6                                                            
TBCF5    GOTO1 SEQ                                                              
TBCF6    CLC   KEY(6),KEYSAVE                                                   
         BNE   TBCF40                   END OF PRD                              
*                                                                               
TBCF8    DS    0H                                                               
         CLI   KEY+12,0          SKIP BUCKETS FOR NON-NATIVE CURRENCY           
         BNE   TBCF5                                                            
*                                SKIP ESTS OUT OF RANGE                         
         CLC   KEY+6(1),MYBEST                                                  
         BL    TBCF5                                                            
         CLC   KEY+6(1),MYBESTE                                                 
         BH    TBCF5                                                            
*                                                                               
TBCF12   DS    0H                                                               
*                                                                               
         CLI   NETOPT,C'N'          NETPAK?                                     
         BNE   TBCF14                                                           
         CLI   BPRD,0           EXPANDED BRAND?                                 
         BNE   TBCF14                                                           
*                                                                               
*        CODE BELOW IS NEEDED SO THAT I WON'T PROCESS                           
*        ALL BUCKETS WITH 0 PRODUCT FOR EACH OF EXPANDED PRD                    
*                                                                               
         LA    R6,KEY                                                           
         USING STABUCKD,R6                                                      
         CLC   STABKSTA(3),PRD     MUST MATCH PRODUCT CODE                      
         BNE   TBCF5               PRODUCT NOW IN STATION                       
*                                                                               
         DROP  R6                                                               
*                                                                               
TBCF14   L     R6,ADSTABUC                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVI   CKESTREC,C'K'          FROM EST BUCKET RECORD                    
         GOTO1 =A(CKEST)                                                        
         USING STABUCKD,R6                                                      
*                                                                               
TBCF15   DS    0H                                                               
*                                                                               
         CLI   NETOPT,C'N'          NETPAK?                                     
         BE    TBCF15A          YES-SHOULD ONLY GET MANUAL BILLS                
*                               ALL 0E01 NET RECORDS ARE MANUALS                
*                               3 CHARACTER PRD MAY NOW BE IN STATION           
*                                                                               
         CLC   STABKSTA(3),=X'D6B5A4'   SPECIAL FOR MANUAL BILLING              
         BNE   TBCF15X                                                          
TBCF15A  MVC   SVTBSTA(8),=CL8'MANUAL'                                          
***                                                                             
         JIF   NETOPT,NE,C'N',TBCF20,JUMP=N    SEE IF NETPAK                    
         MVI   SVTBSTA+7,C'N'      DEFAULT TO NETWORK                           
         TM    STABSTYP,X'03'      OTHER?                                       
         BNO   TBCF15B                                                          
         MVI   SVTBSTA+7,C'O'                                                   
         B     TBCF20                                                           
*                                                                               
TBCF15B  TM    STABSTYP,X'02'      SYNDICATION                                  
         BNO   TBCF15D                                                          
         MVI   SVTBSTA+7,C'S'                                                   
         B     TBCF20                                                           
*                                                                               
TBCF15D  TM    STABSTYP,X'01'      CABLE                                        
         BNO   TBCF15X             OTHERWISE LEAVE AS NETWORK                   
         MVI   SVTBSTA+7,C'C'                                                   
         B     TBCF20                                                           
*                                                                               
TBCF15X  GOTO1 MSUNPK,DMCB,(X'80',STABKMKT),WORK,SVTBSTA                        
*                                                                               
         CLC   SVTBSTA+5(3),SPACES    SEE IF CABLE                              
         BE    TBCF17                                                           
         MVI   SVTBSTA+4,C'/'                                                   
TBCF17   DS    0H                                                               
*                                                                               
TBCF20   DS    0H                                                               
*                                                                               
TBCF22   LA    R2,STABELEM                                                      
         MVI   ELCODE1,X'0E'                                                    
         CLI   0(R2),X'0E'                                                      
         BE    TBCF30                                                           
TBCF25   BAS   RE,NEXTEL                                                        
         BNE   TBCF5               DONE - GO DO NEXT RECORD                     
*                                                                               
TBCF30   DS    0H                                                               
         USING STABELEM,R2                                                      
         CLC   STABBDT,BQENDP      SEE IF BILLED MY PERIOD                      
         BH    TBCF25              NO - BYPASS                                  
         CLC   STABBDT,BQSTARTP    SEE IF BILLED MY PERIOD                      
         BL    TBCF25              NO - BYPASS                                  
         ST    R2,ELADDR                                                        
         GOTO1 =A(BLDREC)          PROCESS ELEMENT DATA                         
         B     TBCF25              GO CHECK FOR MORE ELEMENTS                   
*                                                                               
TBCF40   DS    0H                                                               
         B     FPRDX               DON'T NEED TO DO ANYTHING ELSE?              
*                                                                               
         DROP  R2                                                               
         DROP  R6                                                               
*                                                                               
FPRDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FEST     DS    0H                  ESTIMATE FIRST                               
FESTX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PUTBUFF  DS    0H      FIRST PRINT TOTAL LINE FOR CURRENT INVS                  
         TM    CINVSW,1                                                         
         BZ    PUTB2                                                            
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+28(7),=C'*TOTAL*'                                             
         EDIT  (P8,CINVGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVBIL),(14,P1+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                     
         MVI   P1+51,C'*'                                                       
         MVI   P1+67,C'*'                                                       
         MVI   P1+83,C'*'                                                       
         MVI   P1+99,C'*'                                                       
         BAS   RE,MYRPT                                                         
PUTB2    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
*                                  PUT BUFFALO RECS TO TAPE                     
*                                  AT LBUYREQ                                   
         ZAP   GTTOTCD,=P'0'                                                    
         ZAP   GTTOTAMT,=P'0'                                                   
         ZAP   GTTOTCOM,=P'0'                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
*                                                                               
TOT0     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,2                FOR BCT                                      
TOT2     MVC   P1+7(30),0(R4)                                                   
         EDIT  (P4,4(R3)),(9,P1+40),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,30(R4)                                                        
         LA    R3,8(R3)                                                         
         BCT   R6,TOT2                                                          
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+17(13),=C'TOTAL RECORDS'                                      
         EDIT  TOTCNT,(9,P1+40),0,COMMAS=YES                                    
         MVI   P1+49,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         MVC   P1+18(12),=C'TOTAL AMOUNT'                                       
         EDIT  TOTDOL,(14,P1+35),2,COMMAS=YES,FLOAT=-                           
         MVI   P1+49,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   TAPESW,C'Y'          SEE IF PRODUCING INV. TAPE                  
         BNE   TOT5                                                             
         CLOSE (SSETAPE,)                                                       
*                                                                               
TOT5     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H       PROCESS BUYREC                                          
         B     EXIT              DO NOTHING                                     
*                                                                               
         DS    XL2000   FOR NOW - JUST SO ROUTINES BELOW CAN BE                 
*                       ACCESSED BY OTHER MODULES                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE1,0(R2)                                                    
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
MWRITE   NTR1                      FIND RECORD LENGHT IN LENTAB                 
*                                                                               
         L     R1,ALENTAB                                                       
MWRITE4  CLC   0(2,R1),RECTYPE                                                  
         BE    MWRITE5                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   MWRITE4                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
MWRITE5  MVC   HALF,2(R1)                                                       
         AP    4(4,R1),=P'1'                                                    
**FB**   LH    R3,HALF                                                          
**FB**   LA    R3,4(R3)                                                         
**FB**   STH   R3,OUTREC-4                                                      
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
WRIT1    MVI   RCSUBPRG,10                                                      
         MVC   P1+1(125),OUTREC                                                 
         MVC   P2+1(125),OUTREC+125                                             
         MVC   P3+1(125),OUTREC+250                                             
         MVC   P4+1(125),OUTREC+375                                             
         MVC   P5+1(125),OUTREC+500                                             
         MVC   P6+1(125),OUTREC+625                                             
         MVC   P7+1(125),OUTREC+750                                             
         MVC   P8+1(125),OUTREC+875                                             
         MVC   P9+1(125),OUTREC+1000                                            
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P5,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P6,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P7,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P8,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P9,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVI   RCSUBPRG,10                                                      
         GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P2+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P3+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P4+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P5+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+350,P8+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+400,P9+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+450,P10+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+500,P11+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+550,P12+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+600,P13+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+650,P14+18,50,=C'N'                             
WRIT1C   MVC   P1+1(7),=C'001-050'                                              
         MVC   P2+1(7),=C'051-100'                                              
         MVC   P3+1(7),=C'101-150'                                              
         MVC   P4+1(7),=C'151-200'                                              
         MVC   P5+1(7),=C'201-250'                                              
         MVC   P6+1(7),=C'251-300'                                              
         MVC   P7+1(7),=C'301-350'                                              
         MVC   P8+1(7),=C'351-400'                                              
         MVC   P9+1(7),=C'401-450'                                              
         MVC   P10+1(7),=C'451-500'                                             
         MVC   P11+1(7),=C'501-550'                                             
         MVC   P12+1(7),=C'551-600'                                             
         MVC   P13+1(7),=C'600-650'                                             
         MVC   P14+1(7),=C'651-700'                                             
WRIT1E   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,SSETAPE                                                       
*                                                                               
WRIT2B   LA    R0,OUTREC-4                                                      
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
*                                                                               
MYRPT    NTR1                                                                   
         CLI   TAPESW,C'Y'     SEE IF TAPE PRODUCED                             
         BE    *+10                                                             
         MVC   HEAD1+14(16),=C'*** TEST RUN ***'                                
*                                                                               
         MVC   QSTART(12),MSVSTART     RESTORE FOR HEADLINES                    
*NOP*    CLC   MSVEND,=C'999999'                                                
         CLC   MSVEND,=6X'FF'      FOR 21ST CENTURY                             
         BNE   MYRPT5                                                           
         MVC   QEND,SPACES                                                      
MYRPT5   GOTO1 REPORT                                                           
         MVC   QSTART(12),SPACES                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                   E=ESTIMATE USER FIELDS                      
*                                                                               
AGYTAB   DC    C'H9',C'E'      STARCOM - OLD                                    
         DC    C'H7',C'E'      MINDSHARE                                        
         DC    C'SJ',C'E'      FOR TESTING                                      
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3                                                                
         EJECT                                                                  
*                                                                               
SSETAPE  DCB   DDNAME=SSETAPE,DSORG=PS,RECFM=VB,LRECL=01100,           X        
               BLKSIZE=05504,MACRF=PM                                           
         EJECT                                                                  
*                           NETWORK UNIT PROCESSING                             
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
*                                                                               
*        RA AND R9 FOR SPWORKD                                                  
*        R8 FOR SPSEWRKD                                                        
*                                                                               
*        NOTE THAT R7 SHOULD NOT BE USED IN THIS MODULE                         
*        AS IT'S NEEDED TO REFERENCE ROUTINES IN THE MAIN CODE                  
*                                                                               
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
*                                                                               
*                                                                               
         LA    RE,NETBLOCK                                                      
         LH    RF,=Y(NBBLKEND-NETBLOCK)                                         
         XCEF                                                                   
*                                                                               
*                                  SET SELECT OPTIONS                           
         MVI   NBUSER+13,C'N'     ALWAYS PASS PRE-EMPTED UNITS TO A8            
         MVC   NBSELAGY(3),QAGY    AGY/MED                                      
         MVC   NBSELCLI,CLT        CLT                                          
         MVC   NBSELPRD,=C'POL'    ALWAYS DO ALL PRDS                           
         MVC   NBSELEST(2),BEST    END ST/END                                   
         CLC   =C'NO',QEST                                                      
         BNE   *+10                                                             
         MVC   NBSELEFL,QESTEND    EST FILTER                                   
         MVC   NBSELSTR(12),=C'750101991231'                                    
         MVI   NBSELSTR+6,X'FB'                                                 
*                          THESE DATES ARE REALLY 1975 - 2019                   
*                          X'FC'  CAUSED NETIO TO RETURN                        
*                          END BEFORE START ERROR                               
*                          GOOD LUCK TO FUTURE GENERATIONS OF                   
*                          PROGRAMMERS                                          
*                                  SET DATA OPTIONS                             
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'D'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
         MVI   NBSELPST,C'B'       PASS LOCKED PACKAGES                         
*                                                                               
         MVC   NBAIO,=A(VIRTLREC)  USE VIRTUAL REC AREA                         
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
         OI    NBINDS2,NBBILLRD    RETURN UBILL RECS IF POSSIBLE                
         XC    WK2,WK2             SET UP DSECT FOR BILLREADER                  
         LA    R1,WK2                                                           
         ST    R1,NBABILRD                                                      
K@       USING NBLBILLD,WK2                                                     
         MVC   K@.NBLUNAIO,=A(VIRTLREC)                                         
         OI    K@.NBLFUNC,NBLSEED                                               
         DROP  K@                                                               
*                                                                               
NTU10    DS    0H                                                               
         GOTO1 NETIO,DMCB,NETBLOCK                                              
         CLI   NBERROR,NBINVEST                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,NBINVPRD                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU12                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTUXX                                                            
         CLI   NBMODE,NBVALCLI     SEE IF I JUST VALIDATED CLIENT               
         BNE   NTU10                                                            
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         B     NTU10                                                            
*                                                                               
NTUXX    XIT1               DONE WITH ALL UNITS  (OR ERROR)                     
         EJECT                                                                  
* PROCESS UNIT                                                                  
         SPACE 2                                                                
NTU12    DS    0H                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         CLI   NUPRD,0              SEE IF UNALLOCATED                          
         BE    NTU10                IF SO SKIP                                  
         MVC   SVNUPRD2,NUPRD2       SAVE SECOND PRODUCT                        
*                                                                               
PROCN1   DS    0H                                                               
*                                                                               
         MVI   USTATYPE,C'N'       DEFALUT TO STATION TYPE N                    
*                                                                               
         LA    R2,NUDATA                                                        
         MVI   ELCODE,X'02'        FIND STATION TYPE                            
*                                                                               
NTU12C   DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   NTU12X             DONE WITH RECORD                              
         USING NUSDRD,R2                                                        
         MVC   USTATYPE,NUSTATYP  SAVE STATION TYPE                             
         CLI   USTATYPE,C'S'      SYNDICATION?                                  
         BE    NTU12X                                                           
         CLI   USTATYPE,C'O'      CINEMA FOR SPRINT                             
         BE    NTU12X                                                           
         CLI   USTATYPE,C'C'      CABLE?                                        
         BE    NTU12X                                                           
         MVI   USTATYPE,C'N'      DEFAULT BACK TO N FOR OTHERS                  
*                                                                               
         DROP  R2                                                               
*                                                                               
NTU12X   LA    R2,NUDATA                                                        
         MVI   ELCODE,X'10'        BILL ELEM                                    
*                                                                               
NTU13    DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   NTU10              DONE WITH RECORD                              
*                                                                               
         USING NUBILD,R2                                                        
         TM    NUBILST,X'20'       SEE IF UNBILLED                              
         BO    NTU13               YES SKIP THIS ELEM                           
         OC    NUBILDAT,NUBILDAT   SEE IF BILLED                                
         BZ    NTU13               NO SKIP                                      
         CLC   NUBILDAT,BQENDP     SEE IF BILLED AFTER PERIOD                   
         BH    NTU13               YES BYPASS                                   
         CLC   NUBILDAT,BQSTARTP   SEE IF BILLED BEFORE PERIOD                  
         BL    NTU13               YES BYPASS                                   
         MVC   WPRD,NUBILPRD                                                    
         BAS   RE,CKPRD            TEST NEED THIS PRODUCT                       
         BNE   NTU13                                                            
*                                                                               
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'U',NUBILEL),SPBVALD,0                             
*                                                                               
*        SET EFFECTIVE VALUES INTO ELEM                                         
*        CAN DO SINCE RECORD IS NOT WRITTEN BACK                                
*                                                                               
*                                                                               
         MVC   NUBILGRS,SPBVEGRS                                                
         MVC   NUBILNET,SPBVENET                                                
*                                                                               
*        JIM CLONKE SAID TO INCLUDE FREE UNITS JUN10/05                         
*                                                                               
*******  OC    NUBILNET,NUBILNET    SKIP ZERO ELEMENTS                          
*******  BZ    NTU13                                                            
*                                                                               
         MVI   CKESTREC,C'U'         SET FROM UNIT                              
         MVC   CKUPRD,NUBILPRD                                                  
         GOTO1 =A(CKEST)         GET ESTIMATE AND SET UDEFS                     
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'I'                                                     
         MVC   BUFAM,NUKAM                                                      
         MVC   BUFSTYPE,USTATYPE   SUB-MEDIA                                    
         CLI   USTATYPE,C'C'     SEE IF CABLE STATION TYPE                      
         BE    NTU13A                                                           
         CLI   USTATYPE,C'O'     SEE IF OTHER (CINEMA FOR SPRINT)               
         BE    NTU13A                                                           
         CLI   USTATYPE,C'S'     SEE IF SYNDICATION STATION TYPE                
         BE    NTU13A            IF NOT USE CALENDAR                            
         MVI   BUFSTYPE,C'N'     OTHERWISE NET                                  
NTU13A   DS    0H                                                               
         MVC   BUFNETW,NUKNET                                                   
         MVC   BUFPRD,NUBILPRD                                                  
         MVC   BUFEST,NUKEST                                                    
         MVC   BUFINV,NUBILNUM                                                  
         MVI   BUFITYP,C'T'                                                     
         CLI   NUBILTYP,C'T'    TIME?                                           
         BE    *+8                                                              
         MVI   BUFITYP,X'FF'    FOR SPECIAL CHARGES                             
*                                                                               
         MVC   BUFDATE,NUBILDAT                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(9,WORK+6)                               
         CLI   USTATYPE,C'C'     SEE IF CABLE STATION TYPE                      
         BE    NTU13D                                                           
         CLI   USTATYPE,C'S'     SEE IF SYNDICATION STATION TYPE                
         BNE   NTU13F            IF NOT USE CALENDAR                            
*                                                                               
*        USE BROADCAST FOR CABLE AND SYNDICATION                                
*                                                                               
NTU13D   GOTO1 BRDMON,DMCB,(X'FF',NUKDATE),WORK                                 
         GOTO1 DATCON,DMCB,(2,WORK),(9,WORK+6)                                  
*                                                                               
NTU13F   DS    0H                                                               
         MVC   BUFMOS,WORK+6      MONTH OF SERVICE                              
*                                                                               
         ZAP   BUFCNT,=P'1'       UNIT COUNTER                                  
         L     R0,NUBILNET                                                      
         CVD   R0,DUB                                                           
         ZAP   BUFNET,DUB                                                       
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   NACTIVE,C'Y'      SET BILLING FOUND                              
         B     NTU13             DO NEXT ELEMENT                                
                                                                                
*                                                                               
         DROP  R4                COVERED NETBLOCK                               
         DROP  R2                COVERED NET BILL ELEMENT                       
*                                                                               
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
*                                                                               
CKPRD    CLC   QPRD,=C'ALL'                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'   '                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    CKPYES                                                           
         CLC   WPRD,BPRD                                                        
         BE    CKPYES              PRD NOT OK - RETURN WITH CC NE               
         BR    RE                                                               
*                                                                               
CKPYES   CR    RE,RE               PRD OK                                       
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                         CREATE COLUMN HEADINGS RECORD                         
COLREC   CSECT                                                                  
         NMOD1 0,COLREC                                                         
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'1100'                                                      
         XCEF                                                                   
*                                                                               
         L     RF,=A(COLHDS)                                                    
         MVC   OUTREC(250),0(RF)                                                
         MVC   OUTREC+250(250),250(RF)                                          
         MVC   OUTREC+500(250),500(RF)                                          
         MVC   OUTREC+750(250),750(RF)                                          
         MVC   OUTREC+1000(COLHDLEN-1000),1000(RF)                              
         LH    RF,OUTREC-4                                                      
         LA    RF,COLHDLEN(RF)                                                  
         STH   RF,OUTREC-4                                                      
         MVC   RECTYPE,=C'CH'      COLUMN HEADINGS                              
         BAS   RE,MWRITE                                                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                         CREATE FILE RECORD                                    
BLDREC   CSECT                                                                  
         NMOD1 0,BLDREC                                                         
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
         CLI   COLSW,C'Y'       COLUMN HEADINGS SENT?                           
         BE    BLDR1                                                            
         GOTO1 =A(COLREC)                                                       
         MVI   COLSW,C'Y'        SO I WON'T REDO                                
*                                                                               
BLDR1    DS    0H                                                               
         MVC   RECTYPE,=C'IR'       INSERTION BILLING ELEMENT REC               
*                                                                               
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'1100'                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,OUTREC                                                        
         MVC   0(8,R2),=C'"SPRNT",'    WAS SPRINT BEFORE NEXTEL CHG             
         LA    R2,8(R2)                                                         
*                                                                               
         L     R6,ADPRD                                                         
         USING PRDHDR,R6                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APADDR1     PRD BILL NAME                               
         LA    R2,30(R2)                                                        
BLDR2    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR4                                                            
         BCT   R2,BLDR2                                                         
BLDR4    DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APADDR2     ADDRESS LINE 1                              
         LA    R2,30(R2)                                                        
BLDR6    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR6A                                                           
         BCT   R2,BLDR6                                                         
BLDR6A   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APADDR3     ADDRESS LINE 1                              
         LA    R2,30(R2)                                                        
BLDR7    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7A                                                           
         BCT   R2,BLDR7                                                         
BLDR7A   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,GETCITY           RETURNED IN PRDCITY (CITY)                  
         MVC   0(30,R2),PRDCITY                                                 
         LA    R2,30(R2)                                                        
BLDR7B   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7C                                                           
         BCT   R2,BLDR7B                                                        
BLDR7C   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,GETST             RETURNED IN PRDST (STATE)                   
         MVC   0(2,R2),PRDST                                                    
         LA    R2,2(R2)                                                         
BLDR7D   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7E                                                           
         BCT   R2,BLDR7D                                                        
BLDR7E   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,GETZIP            RETURNED IN PRDZIP (ZIP)                    
         MVC   0(30,R2),PRDZIP                                                  
         LA    R2,30(R2)                                                        
BLDR7F   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7G                                                           
         BCT   R2,BLDR7F                                                        
BLDR7G   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVC   0(21,R2),=C'"Mindshare USA, LLC",'                               
         LA    R2,21(R2)                                                        
         MVC   0(33,R2),=C'"16368 Collections Center Drive",'                   
         LA    R2,33(R2)                                                        
         MVI   0(R2),C','     EMPTY FIELD                                       
         LA    R2,1(R2)                                                         
         MVC   0(10,R2),=C'"Chicago",'                                          
         LA    R2,10(R2)                                                        
         MVC   0(5,R2),=C'"IL",'                                                
         LA    R2,5(R2)                                                         
         MVC   0(08,R2),=C'"60693",'                                            
         LA    R2,08(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APADDR1     PRD BILL NAME                               
         LA    R2,30(R2)                                                        
BLDR12   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR14                                                           
         BCT   R2,BLDR12                                                        
BLDR14   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APADDR2     ADDRESS LINE 1                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R2,30(R2)                                                        
BLDR16   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18                                                           
         BCT   R2,BLDR16                                                        
BLDR18   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C'"",'     EMPTY ADDRLINE 2                             
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(30,R2),PRDCITY                                                 
         LA    R2,30(R2)                                                        
BLDR16A  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18A                                                          
         BCT   R2,BLDR16A                                                       
BLDR18A  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),PRDST                                                    
         LA    R2,2(R2)                                                         
BLDR16B  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18B                                                          
         BCT   R2,BLDR16B                                                       
BLDR18B  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(30,R2),PRDZIP                                                  
         LA    R2,30(R2)                                                        
BLDR16C  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18C                                                          
         BCT   R2,BLDR16C                                                       
BLDR18C  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
***TST   MVC   0(7,R2),=C'"CITY",'                                              
***TST   MVC   7(5,R2),=C'"ST",'                                                
***TST   MVC   12(6,R2),=C'"ZIP",'                                              
***TST   LA    R2,18(R2)      PAST CITY/STATE/ZIP                               
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(24,R2),APADDR4     ATTENTION OF                                
         LA    R2,24(R2)                                                        
BLDR26   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR28                                                           
         BCT   R2,BLDR26                                                        
*                                                                               
BLDR28   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(3,R2),=C'"",'  EMPTY FIELD                                     
         LA    R2,3(R2)                                                         
*                                                                               
         CLI   NETOPT,C'N'      SEE IF NETPAK                                   
         BE    NBLDR30          BRANCH OUT TO ANOTHER ROUTINE                   
*                                                                               
         L     R6,ELADDR        ADDRESS OF BILLING ELEMENT                      
         USING STABELEM,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,WORK)                                 
         GOTO1 AFMTINO,DMCB,WORK,(2,STABINV),                          X        
               (QMED,B1PROF),B1XPROF                                            
         MVC   DINVFULL,SPACES                                                  
         L     RF,DMCB                                                          
         LA    R1,DINVFULL                                                      
         LA    R3,10                                                            
BLDI2    CLI   0(RF),C'-'       SKIP -                                          
         BE    BLDI8                                                            
         MVC   0(1,R1),0(RF)                                                    
         B     BLDI5                                                            
*                                                                               
BLDI5    LA    R1,1(R1)                                                         
BLDI8    LA    RF,1(RF)                                                         
         BCT   R3,BLDI2                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
*                                                                               
*        NOTHING BEFORE THE INVOICE NUMBER FOR MINDSHARE                        
*                                                                               
**       MVC   1(2,R2),=C'RA'                                                   
**       CLI   QMED,C'R'          SPOT RADIO?                                   
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'TV'                                                   
**       CLI   QMED,C'T'          SPOT TV?                                      
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'NX'                                                   
**       CLI   QMED,C'X'          SPOT NETWORK RADIO?                           
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'NT'                                                   
**       CLI   QMED,C'N'          MUST BE NETWORK                               
**       BE    BLDI9                                                            
**       DC    H'0'         UNKNOWN MEDIA                                       
**                                                                              
**DI9    MVC   3(2,R2),WORK+2      SHOULD STILL BE BILLING MONTH                
*                                                                               
         MVC   1(10,R2),DINVFULL                                                
         LA    R2,10(R2)                                                        
BLDI10   CLI   0(R2),C' '                                                       
         BH    BLDI15                                                           
         BCT   R2,BLDI10                                                        
*                                                                               
BLDI15   DS    0H                                                               
*                                                                               
*        NOTHING AFTER THE # FOR MINDSHARE                                      
*                          NEED TO ADD "CR"  IF NEGATIVE INVOICE                
**       L     R0,STABNET                                                       
**       C     R0,=F'0'                                                         
**       BNL   BLDI17                                                           
**       MVC   1(2,R2),=C'CR'                                                   
**       LA    R2,2(R2)                                                         
**                                                                              
BLDI17   MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(2,STABBDT),(X'20',WORK)                             
         MVC   1(2,R2),WORK+2    MM                                             
         MVC   3(2,R2),WORK+4    DD                                             
         MVC   5(2,R2),WORK+0    YY                                             
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
         DROP  R6                                                               
*                                                                               
         ST    R2,FULL                                                          
*                                                                               
BLDR33   DS    0H                                                               
*                                                                               
*  NOTE: NETWORK ROUTINE WILL RETURN HERE                                       
*                                                                               
         L     R2,FULL                                                          
         CLC   ESTU1+21(10),SPACES   ANY DATA?                                  
         BE    BLDR40                                                           
         MVI   0(R2),C'"'                                                       
         MVC   1(32,R2),ESTU1+21   FIRST ESTIMATE UDEF                          
         LA    R2,32(R2)                                                        
BLDR36   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR38                                                           
         BCT   R2,BLDR36                                                        
BLDR38   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
BLDR39   MVC   0(4,R2),=C'"1",'                                                 
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=C'"1",'                                                 
         LA    R2,4(R2)                                                         
         B     BLDR45                                                           
*                                                                               
BLDR40   L     R2,FULL          RESTORE R2                                      
         MVI   0(R2),C','       NO EST UDEF1                                    
         LA    R2,1(R2)                                                         
         B     BLDR39                                                           
*                                                                               
BLDR45   DS    0H                                                               
*                                                                               
*        NETWORK MUST GO TO ANOTHER ROUTINE HERE AS WELL                        
*        RETURNS TO BLDR52                                                      
*                                                                               
         CLI   NETOPT,C'N'                                                      
         BE    NBLDR45                                                          
*                                                                               
         MVC   0(3,R2),=C'"",'     ITEM ID  - EMPTY FOR SPOT                    
         LA    R2,3(R2)                                                         
*                                                                               
         L     R6,ELADDR                                                        
         USING STABELEM,R6                                                      
         MVC   WORK(10),SPACES                                                  
         EDIT  (B2,STABSPTS),(10,WORK),0,ALIGN=LEFT                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(10,R2),WORK                                                    
         LA    R2,10(R2)                                                        
BLDR46   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR48                                                           
         BCT   R2,BLDR46                                                        
*                                                                               
BLDR48   DS    0H                                                               
         MVC   1(7,R2),=C'units",'                                              
         LA    R2,8(R2)                                                         
*                                                                               
         MVC   MYFULL,STABNET    USE NET                                        
         L     RF,MYFULL                                                        
*                                                                               
         MVC   0(4,R2),=C'"1",'                                                 
         C     RF,=F'0'                                                         
         BNL   BLDR50                                                           
         MVC   0(5,R2),=C'"-1",'                                                
         LA    R2,5(R2)                                                         
         B     BLDR52                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
BLDR50   LA    R2,4(R2)                                                         
*                                                                               
BLDR52   DS    0H                                                               
         MVC   0(5,R2),=C'"EA",'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         ST    RF,MYFULL                                                        
         EDIT  (B4,MYFULL),(10,0(R2)),2,ALIGN=LEFT                              
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),=C',,'     2 EMPTY FIELDS                                
         LA    R2,2(R2)                                                         
         BAS   RE,MYGETCOM    RETURNED IN WORK                                  
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(29,R2),WORK                                                    
         LA    R2,29(R2)                                                        
BLDC2    CLI   0(R2),C' '                                                       
         BH    BLDC5                                                            
         BCT   R2,BLDC2                                                         
*                                                                               
BLDC5    MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(17,R2),=C',,,,,,,,,,,,,,,,,,' 17 EMPTY FIELDS                  
         LA    R2,17(R2)                                                        
*                                                                               
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(8,R2),SVTBSTA                                                  
         LA    R2,8(R2)                                                         
BLDR66   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR68                                                           
         BCT   R2,BLDR66                                                        
*                                                                               
BLDR68   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C',,,'    3 MORE EMPTY FIELDS                           
         LA    R2,3(R2)           NO INSTALLATION CITY FOR MINDSHARE            
         B     BLDR75                                                           
*                                                                               
***      CLC   PRD,=C'CON'                                                      
***      BE    BLDR69                                                           
***      CLC   PRD,=C'SAM'                                                      
***      BE    BLDR69                                                           
***      CLC   PRD,=C'SYO'                                                      
***      BNE   BLDR70                                                           
***R69   MVC   0(12,R2),=C'"CORPORATE",'                                        
***      LA    R2,12(R2)                                                        
***      B     BLDR75                                                           
***                                                                             
***R70   DS    0H              IF PRD IS NOT COR,                               
***                            MARKET - FROM STATION RECORD                     
***      BAS   RE,GETMARK                                                       
***      MVI   0(R2),C'"'                                                       
***      MVC   1(20,R2),WORK                                                    
***      LA    R2,19(R2)                                                        
***R70C  CLI   0(R2),C' '     SCAN BACKWARD FOR NON-SPACE                       
***      BH    BLDR70E                                                          
***      BCT   R2,BLDR70C                                                       
***                                                                             
***R70E  MVC   1(2,R2),=C'",'                                                   
***      LA    R2,3(R2)                                                         
***                                                                             
BLDR75   DS    0H                                                               
         CLC   ESTU2+21(6),SPACES     SEE IF DATA PRESENT                       
         BNH   BLDR75E                                                          
         MVC   0(2,R2),=C',,'         2 MORE EMPTY FIELDS                       
         LA    R2,2(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(16,R2),ESTU2+21      CustomerOrderNumber                       
         LA    R2,15(R2)              SCAN BACKWARDS FOR NON-SPACE              
BLDR75A  CLI   0(R2),C' '                                                       
         BH    BLDR75C                                                          
         BCT   R2,BLDR75A                                                       
BLDR75C  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(4,R2),=C',,,,'       4 MORE EMPTY FIELDS                       
         LA    R2,4(R2)                                                         
         B     BLDR76                                                           
*                                                                               
BLDR75E  DS    0H                     HERE IF NO ESTU2 DATA                     
         MVC   0(7,R2),=C',,,,,,,'    7 EMPTY FIELDS                            
         LA    R2,7(R2)                                                         
*                                                                               
BLDR76   MVI   0(R2),C'"'                                                       
         MVC   1(3,R2),PRD                                                      
         MVC   4(2,R2),=C'",'                                                   
         LA    R2,6(R2)                                                         
*                                                                               
         L     RF,ADSTABUC                                                      
         USING STABUCKD,RF                                                      
         ZIC   R0,STABKEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R2),C'"'                                                       
         UNPK  1(3,R2),DUB+6(2)                                                 
         MVC   4(2,R2),=C'",'                                                   
         LA    R2,6(R2)                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R6,ELADDR                                                        
         USING STABELEM,R6                                                      
         MVC   WORK(2),STABPER                                                  
         MVI   WORK+3,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(9,WORK+6)                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(6,R2),WORK+6   MMM/YY                                          
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
*                                                                               
*  NOTE:  SECOND NETWORK ROUTINE WILL RETURN TO BLDR80                          
*         (FROM NBLDR45)                                                        
*                                                                               
BLDR80   MVC   0(5,R2),=C',,,,,'    5 MORE EMPTY FIELDS                         
         LA    R2,5(R2)                                                         
*                                                                               
         CLI   NETOPT,C'N'      NET OPTION?                                     
         BNE   BLDR85                                                           
         AP    TOTDOL,BUFNET    FROM BUFFALO RECORD                             
         B     BLDREND                                                          
*                                                                               
BLDR85   L     R0,MYFULL                                                        
         CVD   R0,DUB           MYFULL SHOULD HAVE ELEMENT'S $                  
         AP    TOTDOL,DUB                                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
BLDREND  DS    0H                                                               
         LA    RE,OUTREC-4                                                      
         LR    RF,R2                                                            
         SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
         STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
         B     BLDRX                                                            
         EJECT                                                                  
*                                                                               
*        FIRST NETWORK BLDREC ROUTINE                                           
*        RETURNS TO MAIN CODE AT BLDR33                                         
*                                                                               
NBLDR30  DS    0H                                                               
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,BUFDATE),(0,WORK)                                 
*                                                                               
*        SET TO PROCESS B1 PROFILE FOR SUB-MEDIA                                
*                                                                               
         LA    R3,B1PROFC                                                       
         CLI   BUFSTYPE,C'C'                                                    
         BE    NBLDR30X                                                         
         LA    R3,B1PROFO                                                       
         CLI   BUFSTYPE,C'O'     CINEMA FOR SPRINT                              
         BE    NBLDR30X                                                         
         LA    R3,B1PROFS                                                       
         CLI   BUFSTYPE,C'S'                                                    
         BE    NBLDR30X                                                         
         LA    R3,B1PROF                                                        
*                                                                               
NBLDR30X ST    R3,DMCB+8                                                        
         MVC   DMCB+8(1),BUFSTYPE                                               
*                                                                               
         GOTO1 AFMTINO,DMCB,WORK,(0,BUFINV),,B1XPROF                            
*                                                                               
         MVC   DINVFULL,SPACES                                                  
         L     RF,DMCB                                                          
         LA    R1,DINVFULL                                                      
         LA    R3,10                                                            
NBLDI2   CLI   0(RF),C'-'       SKIP -                                          
         BE    NBLDI8                                                           
         MVC   0(1,R1),0(RF)                                                    
         B     NBLDI5                                                           
*                                                                               
NBLDI5   LA    R1,1(R1)                                                         
NBLDI8   LA    RF,1(RF)                                                         
         BCT   R3,NBLDI2                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
*                                 NOTHING BEFORE # FOR MINDSHARE                
**       MVC   1(2,R2),=C'NT'                                                   
**       CLI   QMED,C'N'          MUST BE NETWORK                               
**       BE    NBLDI9                                                           
**       DC    H'0'         UNKNOWN MEDIA                                       
**                                                                              
**LDI9   MVC   3(2,R2),WORK+2      SHOULD STILL BE BILLING MONTH                
**       MVC   5(10,R2),DINVFULL                                                
         MVC   1(10,R2),DINVFULL                                                
         LA    R2,14(R2)                                                        
NBLDI10  CLI   0(R2),C' '                                                       
         BH    NBLDI15                                                          
         BCT   R2,NBLDI10                                                       
*                                                                               
NBLDI15  DS    0H                                                               
*                          NEED TO ADD "CR"  IF NEGATIVE INVOICE                
*                          NOT FOR MINDSHARE                                    
**       CP    BUFNET,=P'0'                                                     
**       BNL   NBLDI17                                                          
**       MVC   1(2,R2),=C'CR'                                                   
**       LA    R2,2(R2)                                                         
*                                                                               
NBLDI17  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(2,BUFDATE),(X'20',WORK)                             
         MVC   1(2,R2),WORK+2    MM                                             
         MVC   3(2,R2),WORK+4    DD                                             
         MVC   5(2,R2),WORK+0    YY                                             
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
*                                                                               
         ST    R2,FULL                                                          
         B     BLDR33           RETURN TO MAIN CODE                             
         EJECT                                                                  
*     SECOND NETWORK BLDREC ROUTINE                                             
*     RETURNS TO MAIN CODE AT BLDR80                                            
*                                                                               
NBLDR45  DS    0H                                                               
*                                                                               
         MVC   0(3,R2),=C'"",'     ITEM ID  - EMPTY FOR NET ALSO                
         LA    R2,3(R2)                                                         
*                                                                               
         CLI   BUFITYP,C'T'                                                     
         BNE   NBLDR47                                                          
*                                                                               
         MVC   WORK(10),SPACES                                                  
         EDIT  (P8,BUFCNT),(10,WORK),0,ALIGN=LEFT                               
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(10,R2),WORK                                                    
         LA    R2,10(R2)                                                        
NBLDR45C CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    NBLDR45E                                                         
         BCT   R2,NBLDR45C                                                      
*                                                                               
NBLDR45E DS    0H                                                               
         MVC   1(7,R2),=C'units",'                                              
         LA    R2,8(R2)                                                         
*                                                                               
         B     NBLDR48                                                          
*                                                                               
NBLDR47  MVC   0(18,R2),=C'"special charges",'                                  
         LA    R2,18(R2)                                                        
*                                                                               
NBLDR48  DS    0H                                                               
*                                                                               
         MVC   0(4,R2),=C'"1",'                                                 
         CP    BUFNET,=P'0'                                                     
         BNL   NBLDR50                                                          
         MVC   0(5,R2),=C'"-1",'                                                
         LA    R2,5(R2)                                                         
         B     NBLDR52                                                          
*                                                                               
NBLDR50  LA    R2,4(R2)                                                         
*                                                                               
NBLDR52  DS    0H                                                               
         MVC   0(5,R2),=C'"EA",'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (P8,BUFNET),(10,0(R2)),2,ALIGN=LEFT                              
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),=C',,'     2 EMPTY FIELDS                                
         LA    R2,2(R2)                                                         
         BAS   RE,MYGETCOM    RETURNED IN WORK                                  
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(29,R2),WORK                                                    
         LA    R2,29(R2)                                                        
NBLDC2   CLI   0(R2),C' '                                                       
         BH    NBLDC5                                                           
         BCT   R2,NBLDC2                                                        
*                                                                               
NBLDC5   MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(17,R2),=C',,,,,,,,,,,,,,,,,,' 17 EMPTY FIELDS                  
         LA    R2,17(R2)                                                        
*                                                                               
*                                                                               
         MVI   0(R2),C'"'                                                       
*                                                                               
         MVC   1(4,R2),BUFNETW     NETWORK                                      
*                                                                               
         LA    R2,4(R2)                                                         
NBLDR66  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    NBLDR68                                                          
         BCT   R2,NBLDR66                                                       
*                                                                               
NBLDR68  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C',,,'    3 MORE EMPTY FIELDS                           
         LA    R2,3(R2)        NO INSTALLATIONCITY FOR MINDSHARE                
         B     NBLDR75                                                          
*                                                                               
***      CLC   PRD,=C'CON'                                                      
***      BE    NBLDR69                                                          
***      CLC   PRD,=C'SAM'                                                      
***      BE    NBLDR69                                                          
***      CLC   PRD,=C'SYO'                                                      
***      BNE   NBLDR70                                                          
***DR69  MVC   0(12,R2),=C'"CORPORATE",'                                        
***      LA    R2,12(R2)                                                        
***      B     NBLDR75                                                          
***                                                                             
***DR70  DS    0H              IF PRD IS NOT COR,                               
***      CLC   PRD,=C'BIZ'                                                      
***      BE    NBLDR72                                                          
***      CLC   PRD,=C'PCS'                                                      
***      BNE   NBLDR73                                                          
***DR72  MVC   0(11,R2),=C'"NATIONAL",'                                         
***      LA    R2,11(R2)                                                        
***      B     NBLDR75                                                          
***                      OTHER PRODUCTS  - UNKNOWN FOR NOW                      
***DR73  MVI   0(R2),C'"'                                                       
***      MVC   1(20,R2),=CL20'MARKET'                                           
***      LA    R2,19(R2)                                                        
***DR70C CLI   0(R2),C' '     SCAN BACKWARD FOR NON-SPACE                       
***      BH    NBLDR70E                                                         
***      BCT   R2,NBLDR70C                                                      
***                                                                             
***DR70E MVC   1(2,R2),=C'",'                                                   
***      LA    R2,3(R2)                                                         
***                                                                             
NBLDR75  DS    0H                                                               
         CLC   ESTU2+21(6),SPACES     SEE IF DATA PRESENT                       
         BNH   NBLDR75E                                                         
         MVC   0(2,R2),=C',,'         2 MORE EMPTY FIELDS                       
         LA    R2,2(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(16,R2),ESTU2+21      CustomerOrderNumber                       
         LA    R2,15(R2)              SCAN BACKWARDS FOR NON-SPACE              
NBLDR75A CLI   0(R2),C' '                                                       
         BH    NBLDR75C                                                         
         BCT   R2,NBLDR75A                                                      
NBLDR75C MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(4,R2),=C',,,,'       4 MORE EMPTY FIELDS                       
         LA    R2,4(R2)                                                         
         B     NBLDR76                                                          
*                                                                               
NBLDR75E DS    0H                     HERE IF NO ESTU2 DATA                     
         MVC   0(7,R2),=C',,,,,,,'    7 EMPTY FIELDS                            
         LA    R2,7(R2)                                                         
*                                                                               
NBLDR76  MVI   0(R2),C'"'                                                       
         MVC   1(3,R2),PRD                                                      
         MVC   4(2,R2),=C'",'                                                   
         LA    R2,6(R2)                                                         
*                                                                               
         ZIC   R0,BUFEST     ESTIMATE                                           
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R2),C'"'                                                       
         UNPK  1(3,R2),DUB+6(2)                                                 
         MVC   4(2,R2),=C'",'                                                   
         LA    R2,6(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(6,R2),BUFMOS   MMM/YY                                          
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
         B     BLDR80           RETURN TO MAIN CODE                             
*                                                                               
         EJECT                                                                  
*        GETCITY EXTRACTS CITY FROM APADDR4                                     
*        THE FORMAT SHOULD BE CITY/ST/ZIP                                       
*                                                                               
GETCITY  DS    0H                                                               
         XC    PRDCITY,PRDCITY                                                  
         LA    R1,PRDCITY                                                       
         LA    R3,APADDR4                                                       
         LA    R4,L'APADDR4                                                     
GETC5    CLI   0(R3),C'/'     FIND FIRST /                                      
         BE    GETCX                                                            
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETC5                                                         
*                                                                               
GETCX    OC    PRDCITY,SPACES                                                   
         BR    RE              RETURN                                           
*                                                                               
                                                                                
         EJECT                                                                  
*                                                                               
*        GETST EXTRACTS STATE CODE FROM APADDR4                                 
*        THE FORMAT SHOULD BE CITY/ST/ZIP                                       
*                                                                               
GETST    DS    0H                                                               
         XC    PRDST,PRDST                                                      
         LA    R1,PRDST                                                         
         LA    R3,APADDR4                                                       
         LA    R4,L'APADDR4                                                     
GETS5    CLI   0(R3),C'/'     FIND FIRST /                                      
         BE    GETS10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETS5                                                         
         B     GETSX           MEANS NO / FOUND                                 
*                                                                               
GETS10   LA    R3,1(R3)        BUMP PAST IT                                     
GETS15   CLI   0(R3),C'/'     FIND NEXT /                                       
         BE    GETS20                                                           
         LA    R4,2            STATE CODE MUST BE 2 CHARS                       
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETS15                                                        
         B     GETSX                                                            
*                                                                               
GETS20   DS    0H                                                               
*                                                                               
GETSX    OC    PRDST,SPACES                                                     
         BR    RE              RETURN                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*        GETZIP EXTRACTS ZIP CODE FROM APADDR4                                  
*        THE FORMAT SHOULD BE CITY/ST/ZIP                                       
*                                                                               
GETZIP   DS    0H                                                               
         L     R6,ADPRD                                                         
         USING PRDHDR,R6                                                        
         XC    PRDZIP,PRDZIP                                                    
         LA    R1,PRDZIP                                                        
         LA    R3,APADDR4                                                       
         LA    R4,L'APADDR4                                                     
GETZ5    CLI   0(R3),C'/'     FIND FIRST /                                      
         BE    GETZ10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ5                                                         
         B     GETZX           MEANS NO / FOUND                                 
*                                                                               
GETZ10   LA    R3,1(R3)        BUMP PAST IT                                     
         SH    R4,=H'1'       DECREMENT R4                                      
         CH    R4,=H'0'       DON'T GO NEGATIVE                                 
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ15   CLI   0(R3),C'/'     FIND NEXT /                                       
         BE    GETZ20                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ15                                                        
         B     GETZX                                                            
*                                                                               
GETZ20   DS    0H                                                               
         LA    R3,1(R3)        BUMP PAST IT                                     
         SH    R4,=H'1'       DECREMENT R4                                      
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ25   MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ25                                                        
         B     GETZX                                                            
*                                                                               
*                                                                               
GETZX    OC    PRDZIP,SPACES                                                    
         BR    RE              RETURN                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
MYGETCOM DS    0H              BUILD COMMENT LINE                               
         MVC   WORK(29),SPACES                                                  
         MVC   WORK(3),CLT                                                      
         MVC   WORK+3(3),PRD                                                    
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(3),DUB+6(2)                                               
         MVC   WORK+9(20),EDESC                                                 
         BR    RE             RETURN                                            
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO INCREMENT RECORD LENGTH                                     
*        AND SET R2 TO BEGINNING OF NEXT FIELD                                  
*                                                                               
ADDLEN   LH    RF,OUTREC-4                                                      
         AR    RF,R1                                                            
         AH    RF,=H'1'      FOR THE NEXT COMMA                                 
         STH   RF,OUTREC-4                                                      
         LA    R2,OUTREC-4                                                      
         AR    R2,RF         POINT R2 TO NEXT FIELD                             
         MVI   0(R2),C','    FIELD DELIMITER                                    
         LA    R2,1(R2)      PAST COMMA                                         
         BR    RE            RETURN                                             
*                                                                               
BLDRX    XMOD1                                                                  
         EJECT                                                                  
GETMARK  NTR1                                                                   
*                                 READ STATION REC FOR MKT                      
         CLI   QMED,C'X'          NETWORK RADIO?                                
         BNE   GETMK3                                                           
         MVC   WORK(L'MKTNAME),SPACES                                           
         MVC   WORK(8),=C'NATIONAL'                                             
         B     GETMKX                                                           
*                                                                               
GETMK3   L     R6,ADSTABUC                                                      
         USING STABUCKD,R6                                                      
*                                                                               
         MVC   PPGKEY(64),KEY      SAVE STATION BUCKET KEY                      
*                                                                               
         CLC   SAVSTA,STABKSTA                                                  
         BE    GETMK30                                                          
         MVC   SAVSTA,STABKSTA                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
KEY@     USING STAKEY,KEY                                                       
         MVI   KEY@.STAKTYPE,STAKTYPQ                                           
         MVC   KEY@.STAKMED,QMED                                                
         GOTO1 MSUNPK,DMCB,PPGKEY+7,FULL,DUB                                    
         MVC   KEY@.STAKCALL,DUB                                                
         CLI   QMED,C'R'                                                        
         BE    *+10                                                             
         MVC   KEY@.STAKCALL+4(1),QMED                                          
         MVC   KEY@.STAKAGY,AGY                                                 
         MVC   KEY@.STAKCLT,CLIENT                                              
         MVC   KEY@.STAKFILL,=C'000'                                            
         DROP  KEY@                                                             
         GOTO1 READSTA                                                          
*                                                                               
GETMK30  DS    0H           READ MARKET RECORD                                  
*                                                                               
KEY@     USING MKTKEY,KEY                                                       
         L     RF,ADSTAT                                                        
         MVC   KEY@.MKTKMKT,SMKT-STAKEY(RF)                                     
         MVI   KEY@.MKTKTYPE,MKTKTYPQ                                           
         MVC   KEY@.MKTKMED,QMED                                                
         MVC   KEY@.MKTKAGY,AGY                                                 
         MVC   KEY@.STAKCLT,CLIENT                                              
         MVC   KEY@.MKTKFILL,=C'0000000'                                        
         DROP  KEY@                                                             
         GOTO1 READMKT                                                          
         L     RF,ADMARKET                                                      
         MVC   WORK(L'MKTNAME),MKTNAME-MKTREC(RF)                               
         MVC   KEY(64),PPGKEY                                                   
         GOTO1 HIGH                                                             
GETMKX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8, R9                                 
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         MVI   BILLONLY,C'N'                                                    
*                                                                               
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         L     RF,ADBUY                                                         
         USING BUYREC,RF                                                        
         MVC   KEY+1(1),BUYKAM    A/M                                           
         MVC   KEY+2(2),BUYKCLT   CLT                                           
         MVC   KEY+4(3),PRD       PRD                                           
         MVC   KEY+7(1),BUYKEST   EST                                           
         MVC   CKEPRD,PRD                                                       
         CLI   CKESTREC,C'B'        FROM SPOT BUY                               
         BE    CKEST3                                                           
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,ADSTABUC                                                      
         USING STABUCK,RF                                                       
         MVC   KEY+1(1),STABKAM   A/M                                           
         MVC   KEY+2(2),STABKCLT  CLT                                           
         MVC   KEY+4(3),PRD                                                     
         MVC   KEY+7(1),STABKEST  EST                                           
         CLI   CKESTREC,C'K'        FROM STATION BUCKET REC                     
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   QOPT5,C'Y'      LISTING CURRENT INVOICES?                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,10         WHEN DOING BILLS                             
*                                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(8),BILLREC      00/A/M/CLT/PRD/EST                           
         MVC   CKEPRD,BKEYPRD                                                   
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
*              FOR OTHER SOURCES                                                
*                                                                               
CKEST2   L     R5,VCLIST         EXPANDED PRODUCT LIST                          
CKEST2B  CLC   3(1,R5),CKUPRD    PRODUCT I'M PROCESSING                         
         BE    CKEST2C                                                          
         LA    R5,4(R5)                                                         
         CLI   0(R5),0             END OF LIST?                                 
         BNE   CKEST2B                                                          
         DC    H'0'                MUST FIND PRD                                
*                                                                               
CKEST2C  DS    0H                                                               
         MVC   CKEPRD,0(R5)                                                     
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),CKEPRD                                                  
         MVC   KEY+7(1),NUKEST                                                  
         CLI   CKESTREC,C'U'        FROM A UNIT                                 
         BE    CKEST3                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BUFAM                                                   
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),CKEPRD                                                  
         MVC   KEY+7(1),BUFEST                                                  
         CLI   CKESTREC,C'F'        FROM BUFFALO RECORD                         
         BE    CKEST3                                                           
*                                                                               
         DC    H'0'                 ERROR - UNKNOWN RECORD TYPE                 
*                                                                               
         DROP  R4                                                               
         DROP  RF                                                               
*                                                                               
CKEST3   L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLC   ESTHDR(8),KEY        SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         GOTO1 GETEST                                                           
*                                                                               
CKEST5   DS    0H                                                               
*                                                                               
CKEST5C  DS    0H                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
*                                                                               
         CLI   MYUSER,C'E'         SEE IF USER FIELDS IN USE                    
         BNE   CKEST50                                                          
*                                                                               
         CLC   EKEYPRD,=C'AAA'     NOT FOR PRD AAA                              
         BE    CKEST50                                                          
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'E',ADEST),ESTU1,ESTU2              
         L     RF,ADEST       RESET RF                                          
         CLI   DMCB,X'FF'                                                       
         BE    UESTERR                                                          
         CLI   ESTU1+21,C' '    MUST FIND DATA                                  
         BNH   UESTERR                                                          
         B     CKEST50                                                          
*                                                                               
UESTERR  DS    0H                                                               
         LA    R1,BADESTS                                                       
         MVC   WORK(3),EKEYPRD                                                  
         MVC   WORK+3(1),EKEYEST                                                
UESTERR2 CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    UESTERR3                                                         
         CLC   WORK(4),0(R1)                                                    
         BE    CKEST50                                                          
         LA    R1,5(R1)                                                         
         B     UESTERR2                                                         
*                                                                               
UESTERR3 MVC   0(4,R1),WORK                                                     
         MVI   4(R1),0                                                          
         MVI   5(R1),X'FF'        SET NEW END OF TABLE                          
*                                                                               
         MVC   P1(35),=C'*** MISSING ESTIMATE USER FIELD ***'                   
         MVC   P1+40(3),EKEYPRD                                                 
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+45(3),DUB                                                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'           SEE IF TEST RUN                             
         BE    CKEST50                                                          
         MVC   P2(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
CKEST50  DS    0H                                                               
***                   I DON'T NEED UCOMM DATA?                                  
         B     CKEST80                                                          
***                                                                             
*                                                                               
*        FIRST TRY TO FIND UNDER BRAND EST                                      
*                                                                               
*        GET ESTIMATE UCOMS FOR PRODUCT POL                                     
*        POL ESTIMATE READ INTO ADCOMREC (BIG ENOUGH FOR EST?)                  
*                                                                               
         XC    UCOMDATA,UCOMDATA   CLEAR UCOMM DATA                             
         L     RF,ADEST                                                         
         MVC   KEY,0(RF)                                                        
         L     RE,ADCOMREC                                                      
         ST    RE,AREC                                                          
         MVC   KEY+EKEYPRD-ESTHDR(3),=C'POL'                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CKESMISS                                                         
         GOTO1 GET                                                              
         CLI   DMCB+8,0                                                         
         BNE   CKESMISS                                                         
*                                                                               
*        CALL DDUCOM TO GET ESTIMATE'S FIRST 4 UCOMS                            
*                                                                               
         MVC   USAVKEY,KEY   SAVE MY KEY                                        
         LA    R5,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R5                                                       
         MVC   UCPRD,CKEPRD       FIRST TRY FOR BRAND                           
*                                                                               
CKES1    MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'N'        SYSTEM TO NETPAK                               
         CLI   NETOPT,C'N'       NETPAK?                                        
         BE    *+8                                                              
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         MVC   UCSAM,BAGYMD      AGENCY/MEDIA                                   
         MVC   UCSCLT,BCLT       PACKED CLIENT                                  
*                                DO UCOMMS FOR PRD POL                          
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         L     R2,ADEST          BRAND ESTIMATE                                 
         USING ESTHDR,R2                                                        
         MVC   UCSEST,EKEYEST                                                   
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   CKES3X       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BNO   CKES2                                                            
         CLC   UCPRD,=C'POL'     DID I JUST TRY FOR POL?                        
         BE    CKES2X                                                           
         MVC   UCPRD,=C'POL'     TRY FOR POL UCOMMS                             
         B     CKES1                                                            
*                                                                               
CKES2    XC    UCTTLS(UCALL),UCTTLS                                             
         L     R4,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(R4)   SAVE INFO IN MY STORAGE                           
         LA    R4,UCTTLS      AS OPPOSED TO RD CHANE                            
         L     RE,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(RE)                                                   
*                                                                               
CKES2X   CLI   QOPT1,C'E'     ESTIMATE FILE ONLY                                
         BE    CKES3X         ANY ERRORS WILL BE CAUGHT LATER                   
*                                                                               
         OC    UCOMDATA(4),UCOMDATA   1ST IS VALIDITY YEAR                      
         BNZ   CKES3X                                                           
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),PRD                                                     
         ZIC   R0,EKEYEST      (FROM BRAND ESTIMATE)                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+25(3),DUB+6(2)                                                
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    CKES3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
CKES3X   DS    0H                                                               
         MVI   CKESTSW,1        READ SOMETHING                                  
         DROP  R5                                                               
*                                                                               
CKEST80  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1           SEE IF I READ SOMETHING                      
         BNE   CKESTX              NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKESTX   XIT1                                                                   
*                                                                               
CKESMISS DS    0H                  POL ESTIMATE MISSING                         
         MVC   P1+2(29),=C'** ERROR - POL EST NNN - MISSING **'                 
         L     R2,ADEST      TO BRAND ESTIMATE                                  
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+21(3),DUB+6(2)                                                
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    CKES3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
CKESTSW  DC    X'00'                                                            
*****                                                                           
         EJECT                                                                  
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL30'         COLUMN HEADERS '                                   
         DC    CL30'        INVOICE RECORDS'                                    
*                                                                               
         EJECT                                                                  
*             TABLE OF RECORD TYPES,LENGTHS AND COUNTS                          
LENTAB   CSECT                                                                  
         DC    CL2'CH',AL2(0),PL4'0'                                            
         DC    CL2'IR',AL2(0),PL4'0'                                            
         DC    XL4'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
TMKTTAB  DS    0C      TV- TABLE OF MKTS AND SPRINT NAMES                       
         DC    CL4'0151',CL20'Atlanta'                                          
         DC    CL4'0152',CL20'Atlanta'                                          
         DC    CL4'0171',CL20'Austin'                                           
         DC    CL4'0172',CL20'Austin'                                           
         DC    CL4'0191',CL20'Baltimore'                                        
         DC    CL4'0231',CL20'Beckley-Bluefield'                                
         DC    CL4'0281',CL20'Birmingham'                                       
         DC    CL4'0282',CL20'Birmingham'                                       
         DC    CL4'0321',CL20'Boston'                                           
         DC    CL4'0351',CL20'Buffalo'                                          
         DC    CL4'0352',CL20'Buffalo'                                          
         DC    CL4'0421',CL20'Charleston'                                       
         DC    CL4'0422',CL20'Charleston'                                       
         DC    CL4'0431',CL20'Charlotte'                                        
         DC    CL4'0432',CL20'Charlotte'                                        
         DC    CL4'0441',CL20'Chattanooga'                                      
         DC    CL4'0461',CL20'Chicago'                                          
         DC    CL4'0462',CL20'Chicago'                                          
         DC    CL4'0491',CL20'Cincinnati'                                       
         DC    CL4'0515',CL20'Cleveland'     A/M                                
         DC    CL4'0521',CL20'Colorado Springs'                                 
         DC    CL4'0583',CL20'Corpus Christi'                                   
         DC    CL4'0593',CL20'Dallas'       HISP                                
         DC    CL4'0594',CL20'Dallas'       A/M                                 
         DC    CL4'0611',CL20'Dayton'                                           
         DC    CL4'0631',CL20'Denver'                                           
         DC    CL4'0641',CL20'Des Moines'                                       
         DC    CL4'0651',CL20'Detriot'                                          
         DC    CL4'0652',CL20'Detriot'      A/M                                 
         DC    CL4'0721',CL20'Eugene'                                           
         DC    CL4'0761',CL20'Flint'      Flint/Saginaw                         
         DC    CL4'0762',CL20'Flint'      Flint/Saginaw                         
         DC    CL4'0763',CL20'Flint'                                            
         DC    CL4'0791',CL20'Ft. Myers'                                        
         DC    CL4'0845',CL20'Gainesville'   FLA                                
         DC    CL4'0891',CL20'Greensboro'                                       
         DC    CL4'0892',CL20'Greensboro'                                       
         DC    CL4'1011',CL20'Hartford'                                         
         DC    CL4'0991',CL20'Honolulu'                                         
         DC    CL4'1001',CL20'Houston'                                          
         DC    CL4'1002',CL20'Houston'     HISP                                 
         DC    CL4'1003',CL20'Houston'     A/M                                  
         DC    CL4'1041',CL20'Indianapolis'                                     
         DC    CL4'1061',CL20'Jacksonville'                                     
         DC    CL4'1062',CL20'Jacksonville'                                     
         DC    CL4'1121',CL20'Kansas City'                                      
         DC    CL4'1141',CL20'Knoxville'                                        
         DC    CL4'1201',CL20'Lansing'                                          
         DC    CL4'1221',CL20'Las Vagas'                                        
         DC    CL4'1231',CL20'Lexington'                                        
         DC    CL4'1251',CL20'Lincoln'                                          
         DC    CL4'1261',CL20'Little Rock'                                      
         DC    CL4'1271',CL20'Los Angeles'                                      
         DC    CL4'1281',CL20'Louisville'                                       
         DC    CL4'1381',CL20'Memphis'                                          
         DC    CL4'1382',CL20'Memphis'                                          
         DC    CL4'1401',CL20'Miami'                                            
         DC    CL4'1402',CL20'Miami'    A/M                                     
         DC    CL4'1403',CL20'Miami'    Miami Springs                           
         DC    CL4'1404',CL20'Miami'    HISP                                    
         DC    CL4'1411',CL20'Milwaukee'                                        
         DC    CL4'1421',CL20'Minneapolis'                                      
         DC    CL4'1501',CL20'Nashville'                                        
         DC    CL4'1511',CL20'New Orleans'                                      
         DC    CL4'1512',CL20'New Orleans'   A/M                                
         DC    CL4'1521',CL20'New York'                                         
         DC    CL4'1541',CL20'Norfolk'                                          
         DC    CL4'1542',CL20'Norfolk'       A/M                                
         DC    CL4'1561',CL20'Oklahoma City'                                    
         DC    CL4'1571',CL20'Omaha'                                            
         DC    CL4'1581',CL20'Orlando'                                          
         DC    CL4'1582',CL20'Orlando'                                          
         DC    CL4'1651',CL20'Philadelphia'                                     
         DC    CL4'1652',CL20'Philadelphia'    A/M                              
         DC    CL4'1661',CL20'Phoenix'                                          
         DC    CL4'1662',CL20'Phoenix'      HISP                                
         DC    CL4'1671',CL20'Pittsburg'                                        
         DC    CL4'1681',CL20'Portland, ME'                                     
         DC    CL4'1691',CL20'Portland, OR'                                     
         DC    CL4'1711',CL20'Providence'                                       
         DC    CL4'1715',CL20'Puerto Rico'                                      
         DC    CL4'1731',CL20'Raleigh'                                          
         DC    CL4'1732',CL20'Raleigh'     A/M ?                                
         DC    CL4'1771',CL20'Richmond'                                         
         DC    CL4'1772',CL20'Richmond'    A/M ?                                
         DC    CL4'1811',CL20'Rockford'                                         
         DC    CL4'1831',CL20'Sacramento'                                       
         DC    CL4'1851',CL20'Salt Lake City'                                   
         DC    CL4'1871',CL20'San Antonio'                                      
         DC    CL4'1872',CL20'San Antonio'   HISP                               
         DC    CL4'1881',CL20'San Diego'                                        
         DC    CL4'1891',CL20'San Francisco'                                    
         DC    CL4'1941',CL20'Seattle'                                          
         DC    CL4'2051',CL20'St. Louis'                                        
         DC    CL4'2081',CL20'Tallahassee'                                      
         DC    CL4'2091',CL20'Tampa'                                            
         DC    CL4'2092',CL20'Tampa'                                            
         DC    CL4'2111',CL20'Toledo'                                           
         DC    CL4'2121',CL20'Topeka'                                           
         DC    CL4'2131',CL20'Tucson'                                           
         DC    CL4'2132',CL20'Tucson'    HISP                                   
         DC    CL4'2141',CL20'Tulsa'                                            
         DC    CL4'2184',CL20'Victoria'                                         
         DC    CL4'2185',CL20'Victoria'   HISP                                  
         DC    CL4'2201',CL20'Waco'                                             
         DC    CL4'2202',CL20'Waco'       HISP                                  
         DC    CL4'2211',CL20'Washington DC'                                    
         DC    CL4'2212',CL20'Washington DC'    A/M  ?                          
         DC    CL4'2241',CL20'West Palm Beach'                                  
         DC    CL4'2271',CL20'Wichita'                                          
         DC    CL4'2311',CL20'Youngstown'                                       
         EJECT                                                                  
SPSEWRKD DSECT                                                                  
NETOPT   DS    CL1                                                              
NACTIVE  DS    CL1          SET TO Y IF NET BILLING FOUND                       
SVQOPT1  DS    CL1          SO I'LL KNOW AT RUNLAST WHICH FILE TYPE             
SVQOPT6  DS    CL1          SO I'LL KNOW AT RUNLAST IF TEST RUN                 
SVQOPT7  DS    CL1          SO I'LL KNOW AT RUNLAST IF PDUMPING                 
*                                                                               
ELCODE   DS    CL1                                                              
TOTTYP   DS    CL1                                                              
SAVSTA   DS    CL3                                                              
*                                                                               
MYCD     DS    PL6                                                              
SVTBSTA  DS    CL10         STATION                                             
USTATYPE DS    CL1          UNIT'S STATION TYPE                                 
*                                                                               
CKUPRD   DS    XL1          USED IN CKEST - FROM UNIT                           
CKEPRD   DS    CL3          SET IN CKEST                                        
*                                                                               
EHDRSW   DS    CL1        GETS SET TO Y WHEN EST FILE HEADER WRITTEN            
IHDRSW   DS    CL1        GETS SET TO Y WHEN INVOICE FILE HDR WRITTEN           
ERRSW    DS    CL1                                                              
COLSW    DS    CL1          Y IF COLUMN HEADERS SENT                            
ELADDR   DS    A            ADDRESS OF BILLING ELEMENT                          
RECTYPE  DS    CL2          USED BY MWRITE                                      
*                                                                               
REQUSER  DS    CL10         SET FROM VENTAB                                     
*                                                                               
SVPRDIFC DS    CL4          SAVED FROM PRD INTERFACE CODE ELEMENT               
TOTCNT   DS    PL4'0'                                                           
TOTDOL   DS    PL6'0'                                                           
*                                                                               
MYDUB    DS    PL8                                                              
MYDUB2   DS    PL8                                                              
NETDUB   DS    PL8                                                              
CDDUB    DS    PL8                                                              
SDUB     DS    PL8          USED FOR SHARES IN PROCNET                          
SNETDUB  DS    PL8          USED FOR SHARES IN PROCNET                          
SVNUPRD2 DS    XL1          SAVED NUPRD2                                        
*                                                                               
WRKBDATE DS    CL10                                                             
MYDUMP   DS    XL2                                                              
         DS    0F           ALIGNMENT                                           
WK       DS    XL100                                                            
WK2      DS    XL255                                                            
*                                                                               
ALLOWSW  DS    XL1                                                              
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
*                                                                               
MYFULL   DS    F                                                                
MYAMTD   DS    F           CALCULATED AMT DUE FOR BUCKET                        
WPRD     DS    XL1                                                              
*                                                                               
WRKDATE  DS    XL8                                                              
*                                                                               
PRDCITY  DS    CL30        EXTRACTED FROM APADDR3                               
PRDST    DS    CL30                                                             
PRDZIP   DS    CL30                                                             
*                                                                               
APADDR1  DS    CL30        SAVED ADDRESSES FROM PRODUCT AAA                     
APADDR2  DS    CL30                                                             
APADDR3  DS    CL30                                                             
APADDR4  DS    CL30                                                             
*                                                                               
NEACCNET DS    F                                                                
NEACCGRS DS    F                                                                
*                                                                               
MYBEST   DS    H           FROM QEST                                            
MYBESTE  DS    H           FROM QESTEND                                         
*                                                                               
AAAPRD   DS    CL1         =Y IF WE HAVE PRD=AAA                                
PRDFORMU DS    CL5         BILLING FORMULA FOR REGULAR PROD                     
ESTFORMU DS    CL5         BILLING FORMULA FOR REGULAR EST                      
AAAFORMU DS    CL5         BILLING FORMULA FOR AAA     PROD                     
AAAESTFR DS    CL5         BILLING FORMULA FOR AAAEST                           
BILLFORM DS    CL5         DEFAULT VALUE FOR FORMULA                            
*                                                                               
MSVSTART DS    CL6                                                              
MSVEND   DS    CL6                                                              
SVQSTART DS    XL3                                                              
SVQEND   DS    XL3                                                              
*                                                                               
B1PROF   DS    CL16                                                             
B1PROFC  DS    CL16                                                             
B1PROFS  DS    CL16                                                             
B1PROFO  DS    CL16     CINEMA FOR SPRINT                                       
B1XPROF  DS    CL16                                                             
*                                                                               
DINVFULL DS    CL10                                                             
DINVNO   DS    CL6      SHORT FORMAT                                            
*                                                                               
LINVFULL DS    CL10     SAVED                                                   
LBQDATE  DS    CL6      SAVED                                                   
*                                                                               
INVTOTD  DS    PL5      TOTAL $ FOR INV NUMBER                                  
LASTBILL DS    CL1      Y= LAST BILL                                            
FRSTBILL DS    CL1      Y= FIRST BILL                                           
INVRCNT  DS    PL5      COUNT OF INVOICE FILE RECORDS                           
*                                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
WORK2    DS    CL64                                                             
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
VGETCOST DS    A                                                                
VDDUCOM  DS    A                                                                
AFMTINO  DS    A                                                                
APERVAL  DS    A                                                                
ASPBVAL  DS    A                                                                
ANETACC  DS    A                                                                
ANETNET  DS    A                                                                
ANETBLK  DS    A                                                                
VPROCNET DS    A                                                                
ADSTABUC DS    A                                                                
ANXTINV  DS    A                                                                
AREGTAB  DS    A                                                                
ANXTREG  DS    A                                                                
AREGTABX DS    A                                                                
BRDMON   DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
*                                                                               
MYBILLCD DS    PL8                                                              
MYBILLGR DS    PL8                                                              
*                                                                               
CINVGRS  DS    PL8        CURRENT INVOICE TOTALS                                
CINVBIL  DS    PL8                                                              
CINVCD   DS    PL8                                                              
CINVRCV  DS    PL8                                                              
CINVSW   DS    CL1                                                              
*                                                                               
ADDDEL   DS    CL1                 X'01' IF ADDED + DELETED IN PERIOD           
BILLONLY DS    CL1                 SET IN CKEST Y= BILL ONLY ESTIMATE           
*                                  (NO CHANGES)                                 
SAVMED   DS    CL2               USED IN MWRITE TO SAVE 'REAL' MEDIA            
CKESTREC DS    CL1                                                              
LBILLKEY DS    CL12           KEY OF LAST BILL READ                             
LESTOUT  DS    CL12           KEY OF LAST ESTIMATE OUTPUT                       
*                                                                               
ETOTSW   DS    CL1                                                              
ESTCD    DS    PL8                 ESTIMATE TOTALS FOR PRINTING                 
ESTAMTD  DS    PL8                                                              
ESTCOMM  DS    PL8                                                              
*                                                                               
GTTOTCD  DS    PL8                 REPORT TOTALS                                
GTTOTAMT DS    PL8                                                              
GTTOTCOM DS    PL8                                                              
*                                                                               
TODAY1   DS    CL6                                                              
TODAYY   DS    CL8         YYYYMMDD                                             
NOW      DS    CL6         TIME HHMMSS                                          
ELCODE1  DS    CL1                                                              
SAVCGR   DS    PL8                                                              
*                                                                               
TAPESW   DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
*                          MIX NOT ALLOWED                                      
*                                                                               
CIRCDAT  DS    CL3                                                              
TRCODE   DS    CL1                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    CL4                                                              
ZEROS    DS    CL30                                                             
LASTEST  DS    CL9          USED TO CHECK CHANGE OF EST                         
LASTEVY  DS    CL4          VALIDITY YEAR                                       
LASTEGL  DS    CL10         GL ACCOUNT                                          
LASTEIO  DS    CL12         IO NUMBER                                           
LASTECC  DS    CL10         COST CENTER                                         
*                           WHEN READING BUFFALO RECS                           
APPBYOWK DS    A                                                                
*                                                                               
BADESTS  DS    CL240            ROOM FOR 40 BAD PRD/ESTS                        
*                                PRD(3)/EST(2)/+ ONE BYTE                       
*                                TO BE USED FOR ERRORS                          
         DS    F                                                                
INVTAB   DS    CL252        ROOM FOR 18 MOS X14                                 
*                           ENTRIES ARE MOS (MY)                                
*                                       BACTP  (ACTUAL AMT DUE)                 
*                                       COMMISSION (BACTP-BNETP)                
INVLEN   EQU   14                                                               
*                                                                               
BUFREC   DS    0CL38                                                            
BUFKEY   DS    0CL22                                                            
BUFTYPE  DS    CL1                 I=INVOICE                                    
*                                                                               
BUFAM    DS    CL1                 AGY/MED                                      
BUFINV   DS    CL4                 INVOICE NUM                                  
BUFITYP  DS    CL1                 T=TIME,OTHERWISE SPECIAL CHG                 
BUFSTYPE DS    CL1                 STATION TYPE N,C,S,O                         
BUFNETW  DS    CL4                 NETWORK                                      
BUFPRD   DS    CL1                 PRODUCT                                      
BUFEST   DS    CL1                 ESTIMATE                                     
BUFDATE  DS    XL2                 BILLED DATE                                  
BUFMOS   DS    CL6                 MONTH OF SERVICE  - MMM/YY                   
*                                                                               
BUFCNT   DS    PL8                                                              
BUFNET   DS    PL8                 NET                                          
         ORG                                                                    
*                                                                               
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
*                                                                               
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL32             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
*                                                                               
         DS    F                                                                
OUTREC   DS    CL1100                                                           
*                                                                               
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=2,FLAVOR=PACKED,KEYLIST=(22,A)         
*                                                                               
COLHDS   CSECT                                                                  
         DC    C'"SprintID",'                                                   
         DC    C'"BillToName",'                                                 
         DC    C'"BillToAddr1",'                                                
         DC    C'"BillToAddr2",'                                                
         DC    C'"BillToCity",'                                                 
         DC    C'"BillToState",'                                                
         DC    C'"BillToPostalCode",'                                           
*                                                                               
         DC    C'"RemitToName",'                                                
         DC    C'"RemitToAddr1",'                                               
         DC    C'"RemitToAddr2",'                                               
         DC    C'"RemitToCity",'                                                
         DC    C'"RemitToState",'                                               
         DC    C'"RemitToPostalCode",'                                          
*                                                                               
         DC    C'"ShipToServiceLocation",'                                      
         DC    C'"ShippingAddr1",'                                              
         DC    C'"ShippingAddr2",'                                              
         DC    C'"ShippingCity",'                                               
         DC    C'"ShippingState",'                                              
         DC    C'"ShippingPostalCode",'                                         
*                                                                               
         DC    C'"SprintContactName",'                                          
         DC    C'"SprintContactPhone",'                                         
*                                                                               
         DC    C'"InvNumber",'                                                  
         DC    C'"Invdate",'                                                    
         DC    C'"PONumber",'                                                   
         DC    C'"POLineNumber",'                                               
         DC    C'"POScheduleNumber",'                                           
*                                                                               
         DC    C'"ItemID",'                                                     
         DC    C'"PartDescription",'                                            
         DC    C'"LineItemQty",'                                                
         DC    C'"UOM",'                                                        
         DC    C'"UnitPrice",'                                                  
         DC    C'"TotalTaxesonInv",'                                            
         DC    C'"Freight",'                                                    
         DC    C'"Comments",'                                                   
*                                                                               
         DC    C'"GeneralLedgerBU",'                                            
         DC    C'"CostCenter",'                                                 
         DC    C'"GLAccount",'                                                  
         DC    C'"FunctionbCode",'                                              
         DC    C'"RegularityID",'                                               
         DC    C'"Workorder",'                                                  
         DC    C'"ProjectID",'                                                  
         DC    C'"Location",'                                                   
         DC    C'"GeoCode",'                                                    
         DC    C'"ProductCode",'                                                
         DC    C'"MarketCode",'                                                 
         DC    C'"CommodityCode",'                                              
*                                                                               
         DC    C'"TradeDiscount",'                                              
         DC    C'"DiscountPercent",'                                            
         DC    C'"NetDays",'                                                    
         DC    C'"DiscountDueDays",'                                            
*                                                                               
         DC    C'"ShippingContact",'                                            
         DC    C'"InstallLocation",'                                            
         DC    C'"InstallationAddr1",'                                          
         DC    C'"InstallationAddr2",'                                          
         DC    C'"InstallationCity",'                                           
         DC    C'"InstallationState",'                                          
*                                                                               
         DC    C'"OrigInvNumber",'                                              
         DC    C'"CustomerOrderNumber",'                                        
         DC    C'"SupplierContact",'                                            
         DC    C'"SupplierEmail",'                                              
         DC    C'"SupplierPhone",'                                              
         DC    C'"SupplierAcctNumber",'                                         
*                                                                               
         DC    C'"CustomerUse1",'                                               
         DC    C'"CustomerUse2",'                                               
         DC    C'"CustomerUse3",'                                               
*                                                                               
         DC    C'"ProNumber",'                                                  
         DC    C'"AirwayBill",'                                                 
         DC    C'"ShipVia",'                                                    
         DC    C'"ServiceStartDate",'                                           
         DC    C'"ServiceEndDate",'                                             
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDS                                                         
         EJECT                                                                  
*                                                                               
STABUCKC CSECT                                                                  
         DS    CL3000                                                           
         DC    X'00'                                                            
*                                                                               
VIRTLREC DS    0D                                                               
         DS    20000C                                                           
*                                                                               
NETBLK   CSECT                                                                  
         DS    1200C                                                            
*                                                                               
NETBLKD  DSECT                                                                  
*                                                                               
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NENETRATED                                                     
       ++INCLUDE SPGENSTAB                                                      
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDREPMASTD                                                     
*                                                                               
         PRINT ON                                                               
       ++INCLUDE DDUCOMD                                                        
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103SPREPSE02 02/09/15'                                      
         END                                                                    
