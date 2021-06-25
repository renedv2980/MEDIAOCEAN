*          DATA SET SPREPIV02  AT LEVEL 029 AS OF 11/15/16                      
*PHASE SPIV02A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPIV02 - PAID LISTING BY MKT GROUP'                             
***********************************************************************         
* USER    JIRA      DATE                  CHANGE LOG                  *         
* ---- ---------- -------- ------------------------------------------ *         
* AKAT SPEC-7767  11/15/16 SUPPORT NEW MEDIA OFFICE LIST              *         
* AKAT SPEC-7091  11/01/16 HANDLE SINGLE OFFICE REQUEST FOR XAUTOPAY  *         
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*-DATE-----LVL-BY------------------CHANGE-----------------------------*         
*                                                                     *         
* 07/28/98  01 NRK                 INITIAL RELEASE                    *         
* 08/25/98  02 NRK                 HANDLE AUTOPAY RECS NOT IN MKT GRPS*         
* 03/24/11  27 AKAT                NEW XSPOTFILE AUTOPAY SUPPORT      *         
*                                                                     *         
***********************************************************************         
SPIV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIV02,R8                                                      
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         MVI   PROCFLAG,C'S'        DEFAULT TO SPOT                             
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         CLI   MCNETPAK,C'Y'        NET AUTOPAY?                                
         BNE   *+8                                                              
         MVI   PROCFLAG,C'N'        NET                                         
         DROP  RF                                                               
*                                                                               
         CLI   MODE,REQFRST        REQFRST?                                     
         BE    MAIN                YES - SO CONTINUE                            
*                                                                               
EXIT     XIT1                      ELSE - ALL DONE                              
*                                                                               
* REPORT MAINLINE                                                               
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INITIAL          INITIALIZATION STUFF                         
         CLI   PROCFLAG,C'N'       NET AUTOPAY?                                 
         BE    MAIN10                                                           
*                                                                               
         BAS   RE,APAYREAD         READ THE AUTOPAY RECORDS                     
         BAS   RE,PRINTIT          PRINT THE REPORT                             
         BAS   RE,XAPYREAD         AUTOPAY ON THE XSPTFILE                      
         BAS   RE,PRINTIT          PRINT THE REPORT                             
         B     MAINEXIT                                                         
*                                                                               
MAIN10   DS    0H                                                               
         BAS   RE,NAPYREAD         READ THE NET AUTOPAY RECORDS                 
         BAS   RE,NPRINTIT         ELSE - PRINT THE REPORT                      
*                                                                               
MAINEXIT GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* REPORT INITIALIZATION STUFF                                                   
*                                                                               
INITIAL  NTR1                                                                   
         XC    LASTCLT,LASTCLT                                                  
         XC    LASTMED,LASTMED                                                  
         MVI   CLTSEC,C'N'         DEFAULT TO NOT SEEING FOR                    
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BE    *+8                                                              
         MVI   CLTSEC,C'Y'         EVERYONE ELSE GETS ALL CLTS                  
*                                                                               
         MVC   MYAGY,BAGYMD        AGENCY/MEDIA                                 
         NI    MYAGY,X'F0'         AGENCY ONLY IN HIGH ORDER                    
         MVI   FIRSTREC,1          SET FOR 'FIRST RECORD'                       
         LA    R0,HEADHK           A(HEADLINE ROUTINE)                          
         ST    R0,HEADHOOK         AND STORE IT                                 
*                                                                               
         CLI   PROCFLAG,C'N'                                                    
         BE    INIT0025                                                         
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD INITIALIZE SORTER               
         B     INIT0050                                                         
*                                                                               
INIT0025 GOTO1 =V(SORTER),DMCB,SORTCAR2,RECCARD2 INITIALIZE SORTER              
*                                                                               
* CHECK FOR OPTIONAL 'RUN DATE' IN QSTART                                       
*                                                                               
INIT0050 CLI   QSTART,C' '         ANY START DATE INPUT?                        
         BNH   INIT0100            NO - SO CONTINUE                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,MYDATE) CNVT TO 2-BYTE FMT             
         XC    MYDATE(2),=X'FFFF'  COMPLIMENT OF DATE                           
*                                                                               
* GET THE AGENCY LEVEL AOA PROFILE FOR THE MARKET GROUP SCHEME CODE             
*                                                                               
INIT0100 XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SA0A'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),QAGY                                                   
         GOTO1 GETPROF,DMCB,WORK,PROFILE,DATAMGR                                
*                                                                               
         MVI   MKTGRPS,C'Y'                                                     
         CLI   PROFILE+11,C' '     MARKET GROUP SCHEME CODE THERE?              
         BH    INITEXIT            NO - SET FLAG                                
         MVI   MKTGRPS,C'N'                                                     
*                                                                               
INITEXIT B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
* READ THE NETAUTOPAY CLEARANCE RECORDS AND SORT THE DATA                       
********************************************************************            
NAPYREAD NTR1                                                                   
         MVC   DATADISP,=H'42'                                                  
*                                                                               
         LA    R6,XKEY                                                          
         USING NAPRECD,R6                                                       
         XC    XKEY,XKEY                                                        
         MVI   NAPKTYP,NAPKTYPQ    X'0D'                                        
         MVI   NAPKSUB,NAPKSUBQ    X'3F'                                        
         MVC   NAPKDATE,MYDATE     TODAY                                        
         MVC   NAPKAGMD,MYAGY      AGY CODE IN HIGH ORDER                       
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,DMWORK              
         B     NPRD0200            JUMP OVER SEQ                                
*                                                                               
NPRD0100 LA    R6,XKEY              RESTORE A(KEY) J.I.C                        
         CLI   XKEY,NAPKTYPQ        LAST READ WAS AN AUTOPAY REC                
         BE    NPRD0110            THEN CONTINUE WITH SEQS                      
         MVC   XKEY,SVXKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,DMWORK              
*                                                                               
NPRD0110 DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,DMWORK              
         USING NAPRECD,R6                                                       
*                                                                               
NPRD0200 CLC   NAPKTYP(2),XKEYSAVE  SAME RECORD/SUB TYPE?                       
         BNE   NPRDEXIT            NO - SO ALL DONE                             
*                                                                               
         TM    NAPKCNTL,NAPRCPRC   HAS THIS BEEN PROCESSED?                     
         BZ    NPRD0100            NO - GET NEXT RECORD                         
*                                                                               
         OC    MYDATE,MYDATE       FIRST TIME THRU?                             
         BNZ   NPRD0300            NO - SO CHECK DATE                           
         MVC   MYDATE,NAPKDATE     ELSE - SAVE DATE FOR NEXT PASS               
         B     NPRD0400            AND CONTINUE                                 
*                                                                               
NPRD0300 CLC   MYDATE,NAPKDATE     SAME DATE?                                   
         BNE   NPRDEXIT            NO - SO ALL DONE                             
*                                                                               
NPRD0400 MVC   BYTE,NAPKAGMD       GET THE AGY/MEDIA                            
         NI    BYTE,X'F0'          TURN OFF THE MEDIA                           
         CLC   MYAGY,BYTE          SAME AGY CODE?                               
         BNE   NPRD0100            NO - SO GET NEXT KEY                         
*                                                                               
         MVC   BYTE,NAPKAGMD       ELSE - CHECK FOR MEDIA BREAK                 
         NI    BYTE,X'0F'          TURN OFF THE AGENCY                          
         CLC   BYTE,MYMED          SAME MEDIA?                                  
         BE    NPRD0600            YES - SO CONTINUE                            
*                                                                               
NPRD0600 DS    0H                                                               
         MVC   SVXKEY,XKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',XKEY+36,ADBUY,DMWORK          
         L     R6,ADBUY            R6 = A(BUY) - STILL HAS SAME USING           
         MVC   SVAGMD,NAPKAGMD     SAVE A/M FOR SORT                            
*                                                                               
         USING NAPEL,R6                                                         
         MVI   ELCODE,NAPELQ       X'01' ELEMS                                  
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
NPRD0605 BRAS  RE,NEXTEL                                                        
         BNE   NPRD0100            NEXT RECORD                                  
*                                                                               
         CLC   NAPPAID,=H'0'       ANY PAID DATE?                               
         BE    NPRD0605            NO - SO GET NEXT ELEM                        
*                                                                               
* CHECK FOR OFFICE LIST SECURITY                                                
*                                                                               
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BNE   NPRD0645                                                         
*                                                                               
         CLC   NAPCLT,LASTCLT                                                   
         BNE   NPRD0620                                                         
         CLC   NAPMED,LASTMED                                                   
         BE    NPRD0640            SAME MED/CLT THEN SEC SET                    
NPRD0620 MVC   LASTCLT,NAPCLT                                                   
         MVC   LASTMED,NAPMED                                                   
*                                                                               
*        READ CLIENT FOR COFFICE                                                
*                                                                               
         MVC   SVXKEY,XKEY           SAVE AUTOPAY KEY TO RESTORE SEQ            
         BAS   RE,AMBIT            SET RIGHT MEDIA BIT IN BAGYMD                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,NAPCLT,KEY+2                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
         L     R2,ADCLT                                                         
         USING CLTRECD,R2                                                       
*                                                                               
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(OFCLENQ),DUB                                                 
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCLMT(3),QCLT                                                   
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1,R2                                                            
         MVI   CLTSEC,C'N'         DEFAULT TO NOT PRINTING CLT                  
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   NPRD0640            NO                                           
         MVI   CLTSEC,C'Y'         SET TO PRINT THIS CLIENT                     
         DROP  R3                                                               
*                                                                               
NPRD0640 CLI   CLTSEC,C'Y'         PRINT CLIENT                                 
         BNE   NPRD0100            THEN CONTINUE WITH SEQS                      
*                                                                               
* NOW BUILD THE SORT KEY                                                        
*                                                                               
NPRD0645 XC    SORTREC,SORTREC     INITIALIZE THE SORT RECORD                   
         LA    R7,SORTREC                                                       
         USING NSRTRECD,R7                                                      
*                                                                               
NPRD0650 DS    0H                                                               
         MVC   NSRMED,NAPMED        MEDIA                                       
         CLI   NAPSMED,C' '                                                     
         BE    *+10                                                             
         MVC   NSRMED,NAPSMED                                                   
         MVC   NSRCLT,NAPCLT        CLIENT                                      
         MVC   NSRPRD,NAPPRD        PRODUCT                                     
         MVC   NSRPRD2,NAPPRD2      PIGGY-WIGGY                                 
         MVC   NSRPAK,NAPPAK        PACKAGE                                     
         MVC   NSREST,NAPEST        ESTIMATE                                    
         MVC   NSRSTA,NAPSTA        STATION CALL LETTERS                        
*                                                                               
         MVC   NSRSREP,NAPSREP      SPECIAL REP                                 
         MVC   NSRMONTH(6),NAPMONTH      MONTH OF SERVICE                       
*                                                                               
         CLI   NAPMNTYP,C'B'             BROADCAST?                             
         BNE   *+12                                                             
         MVI   NSRMONTH+6,C'-'                                                  
         MVI   NSRMONTH+7,C'B'                                                  
*                                                                               
         MVC   NSRPAID,NAPPAID      PAID DATE                                   
         DROP  R6                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING NAPIEL,R6                                                        
         MVI   ELCODE,NAPIELQ      X'01' ELEMS                                  
         BRAS  RE,GETEL                                                         
         BNE   NPRD0100                                                         
*                                                                               
         MVC   NSRINV,NAPIINV      INVOICE NUMBER                               
         DROP  R6                                                               
*                                                                               
NPRD0800 GOTO1 =V(SORTER),DMCB,=C'PUT',(R7) WRITE THE REC TO SORT               
         MVI   DATA,1              SET 'DATA FOUND' FLAG                        
         B     NPRD0100            AND GO GET NEXT AUTOPAY KEY                  
*                                                                               
NPRDEXIT B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
********************************************************************            
* READ THE AUTOPAY CLEARANCE RECORDS AND SORT THE DATA                          
********************************************************************            
APAYREAD NTR1                                                                   
         XC    KEY,KEY             INITIALIZE THE KEY                           
         LA    R6,KEY                                                           
         USING APYRECD,R6                                                       
         MVI   APYKTYP,APYKTYPQ    RECORD TYPE '0D'                             
         MVI   APYKSUB,APYKSUBQ    SUB TYPE '3A'                                
         MVC   APYKDATE,MYDATE     0 FIRST TIME THRU                            
         MVC   APYKAGMD,MYAGY      AGY CODE IN HIGH ORDER                       
*                                                                               
         GOTO1 HIGH                                                             
         B     APRD0200            JUMP OVER SEQ                                
*                                                                               
APRD0100 LA    R6,KEY              RESTORE A(KEY) J.I.C                         
         CLI   KEY,APYKTYPQ        LAST READ WAS AN AUTOPAY REC                 
         BE    APRD0110            THEN CONTINUE WITH SEQS                      
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                ELSE RESTORE SEQUENCE FIRST                  
APRD0110 GOTO1 SEQ                                                              
*                                                                               
APRD0200 CLC   APYKTYP(2),KEYSAVE  SAME RECORD/SUB TYPE?                        
         BNE   APRDEXIT            NO - SO ALL DONE                             
*                                                                               
         OC    MYDATE,MYDATE       FIRST TIME THRU?                             
         BNZ   APRD0300            NO - SO CHECK DATE                           
         MVC   MYDATE,APYKDATE     ELSE - SAVE DATE FOR NEXT PASS               
         B     APRD0400            AND CONTINUE                                 
*                                                                               
APRD0300 CLC   MYDATE,APYKDATE     SAME DATE?                                   
         BNE   APRDEXIT            NO - SO ALL DONE                             
*                                                                               
APRD0400 MVC   BYTE,APYKAGMD       GET THE AGY/MEDIA                            
         NI    BYTE,X'F0'          TURN OFF THE MEDIA                           
         CLC   MYAGY,BYTE          SAME AGY CODE?                               
         BNE   APRD0100            NO - SO GET NEXT KEY                         
*                                                                               
         MVC   BYTE,APYKAGMD       ELSE - CHECK FOR MEDIA BREAK                 
         NI    BYTE,X'0F'          TURN OFF THE AGENCY                          
         CLC   BYTE,MYMED          SAME MEDIA?                                  
         BE    APRD0600            YES - SO CONTINUE                            
*                                                                               
         CLI   MKTGRPS,C'N'        AGENCY DOESN'T USE MARKET GROUPS             
         BE    APRD0600                                                         
         MVC   MYMED,BYTE          ELSE - SAVE NEW MEDIA                        
         MVC   MYKEY,APYKEY        SAVE THE KEY                                 
         BAS   RE,MGRREAD          READ THE MARKET GROUP RECORDS                
         MVC   APYKEY,MYKEY        RESTORE THE KEY                              
         GOTO1 HIGH                RESET THE KEY                                
*                                                                               
APRD0600 GOTO1 GETBUY              READ AUTOPAY REC INTO BUY IO AREA            
         L     R6,ADBUY            R6 = A(BUY) - STILL HAS SAME USING           
         MVC   SVAGMD,APYKAGMD     SAVE A/M FOR SORT                            
*                                                                               
         MVI   MULTMONS,C'N'       MULTIPLE MONTHS IN RECORD                    
         TM    APYRCNTL,APYRCMON   MULTIPLE MONTHS                              
         BNO   *+8                                                              
         MVI   MULTMONS,C'Y'                                                    
*                                                                               
         USING APYEL,R6                                                         
         MVI   ELCODE,APYELQ       X'01' ELEMS                                  
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
APRD0605 BRAS  RE,NEXTEL                                                        
         BNE   APRD0100            NEXT RECORD                                  
*                                                                               
         CLC   APYPAID,=H'0'       ANY PAID DATE?                               
         BE    APRD0605            NO - SO GET NEXT ELEM                        
*                                                                               
* CHECK FOR OFFICE LIST SECURITY                                                
*                                                                               
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BNE   APRD0645                                                         
*                                                                               
         CLC   APYCLT,LASTCLT                                                   
         BNE   APRD0620                                                         
         CLC   APYMED,LASTMED                                                   
         BE    APRD0640            SAME MED/CLT THEN SEC SET                    
APRD0620 MVC   LASTCLT,APYCLT                                                   
         MVC   LASTMED,APYMED                                                   
*                                                                               
*        READ CLIENT FOR COFFICE                                                
*                                                                               
         MVC   SVKEY,KEY           SAVE AUTOPAY KEY TO RESTORE SEQ              
         BAS   RE,AMBIT            SET RIGHT MEDIA BIT IN BAGYMD                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,APYCLT,KEY+2                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
         L     R2,ADCLT                                                         
         USING CLTRECD,R2                                                       
*                                                                               
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(OFCLENQ),DUB                                                 
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCLMT(3),QCLT                                                   
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1,R2                                                            
         MVI   CLTSEC,C'N'         DEFAULT TO NOT PRINTING CLT                  
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   APRD0640            NO                                           
         MVI   CLTSEC,C'Y'         SET TO PRINT THIS CLIENT                     
         DROP  R3                                                               
*                                                                               
APRD0640 CLI   CLTSEC,C'Y'         PRINT CLIENT                                 
         BNE   APRD0100            THEN CONTINUE WITH SEQS                      
*                                                                               
* NOW BUILD THE SORT KEY                                                        
*                                                                               
APRD0645 XC    SORTREC,SORTREC     INITIALIZE THE SORT RECORD                   
         LA    R7,SORTREC                                                       
         USING SORTRECD,R7                                                      
*                                                                               
         XC    SRMKTGR,SRMKTGR                                                  
         CLC   APYMKT,=C'ALL '                                                  
         BE    APRD0650                                                         
         CLI   MKTGRPS,C'N'        AGENCY DOESN'T USE MARKET GROUPS             
         BE    APRD0650                                                         
         PACK  DUB,APYMKT(L'APYMKT) SET UP MARKET CONVERSION                    
         CVB   R1,DUB              TO HEX                                       
         SLL   R1,1                DOUBLE MKT # TO GET INDEX                    
         LA    R5,MKGLIST          A(INDEXED MKT LIST)                          
         AR    R5,R1               A(ACTUAL CELL IN LIST)                       
         MVC   SRMKTGR,0(R5)       MARKET GROUP NUMBER                          
*                                                                               
APRD0650 MVC   SRMKT,APYMKT        MARKET NUMBER                                
         MVC   SRSTA,APYSTA        STATION CALL LETTERS                         
         MVC   SRMED,APYMED        MEDIA                                        
         MVC   SRCLT,APYCLT        CLIENT                                       
         MVC   SRPRD,APYPRD        PRODUCT                                      
         MVC   SRPRD2,APYPRD2      PIGGY-WIGGY                                  
         MVC   SREST,APYEST        ESTIMATE                                     
*                                                                               
* AND THE SORT RECORD                                                           
*                                                                               
         CLC   =C'CK',QAGY         SKIP SREP FOR COKE                           
         BE    *+10                                                             
         MVC   SRSREP,APYSREP      SPECIAL REP                                  
         MVC   SRMOS,APYMONTH      MONTH OF SERVICE                             
         MVC   SRPAID,APYPAID      PAID DATE                                    
         MVC   SRBAGYMD,SVAGMD     COMPRESSED AGENCY/MEDIA                      
         CLC   =C'CK',QAGY         SKIP SREP FOR COKE                           
         BNE   APRD0655                                                         
         CLI   APYLEN,APYLN2Q      NEW ELEM WITH ACN?                           
         BL    APRD0655                                                         
         MVC   SRACN,APYACN                                                     
*                                                                               
APRD0655 CLI   MULTMONS,C'Y'       ARE WE PROCESSING MULTIPLE MONTHS            
         BNE   APRD0660                                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R7) WRITE THE REC TO SORT               
         MVI   DATA,1              SET 'DATA FOUND' FLAG                        
         B     APRD0605            AND GO GET NEXT X'01' ELEM                   
*                                                                               
         USING APYIEL,R6                                                        
APRD0660 L     R6,ADBUY                                                         
         MVI   ELCODE,APYIELQ      INVOICE (X'02') ELEMENT                      
         BRAS  RE,GETEL            GET THE ELEMENT                              
         BNE   APRD0800            NOT FOUND - SO JUST PUT RECORD               
*                                                                               
         MVC   SRINVNUM,APYINUM    NUMBER OF INVOICES                           
         ZIC   R1,APYINUM          LOOP COUNTER                                 
         CH    R1,=H'0'                                                         
         BNH   APRD0800                                                         
         LA    R6,APYIINV          A(INVOICE NUMBER)                            
         LA    R5,SRINVLST         A(LIST IN SORT REC)                          
APRD0700 MVC   0(10,R5),0(R6)      MOVE IN THE INVOICE NUMBER                   
         LA    R5,L'SRINVLST(R5)   INC TO NEXT INV NUMBER                       
         LA    R6,APYILENQ(R6)        "    "      "                             
         BCT   R1,APRD0700         GO GET THE NEXT ONE                          
*                                                                               
APRD0800 GOTO1 =V(SORTER),DMCB,=C'PUT',(R7) WRITE THE REC TO SORT               
         MVI   DATA,1              SET 'DATA FOUND' FLAG                        
         B     APRD0100            AND GO GET NEXT AUTOPAY KEY                  
*                                                                               
APRDEXIT B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
*                                                                               
* READ THE MARKET GROUP PASSIVE KEYS FOR THIS SCHEME CODE AND                   
* STORE THE GROUP NUMBER IN THE INDEXED LIST.                                   
*                                                                               
MGRREAD  NTR1                                                                   
         XCEF  MKGLIST,20000                                                    
         XC    KEY,KEY             INITIALIZE THE KEY                           
         LA    R6,KEY                                                           
         USING MKGRECD,R6                                                       
         MVC   MKGPTYP,=X'0D82'    PASSIVE KEY RECORD TYPE                      
         MVC   MKGPAGMD,MYAGY      AGENCY CODE IN HIGH ORDER NIBBLE             
         OC    MKGPAGMD,MYMED      MEDIA CODE IN LOW ORDER                      
         GOTO1 HIGH                READ THE KEY                                 
         B     MGRD0200            JUMP OVER SEQ                                
MGRD0100 GOTO1 SEQ                 GET THE NEXT KEY                             
*                                                                               
MGRD0200 CLC   MKGPTYP(3),KEYSAVE  SAME RECORD TYPE/AGY/MD?                     
         BNE   MGRDEXIT            NO - SO ALL DONE                             
*                                                                               
         CLC   MKGPMID,PROFILE+11  ELSE - SAME GROUP SCHEME CODE                
         BNE   MGRD0100            NO - SO GET NEXT KEY                         
*                                                                               
         LA    R5,MKGLIST          ELSE - GET A(MARKET LIST)                    
         ZICM  R4,MKGPMKT,2        GET THE MARKET NUMBER                        
         SLL   R4,1                DOUBLE IT TO GET INDEX INTO LIST             
         AR    R5,R4               A(ACTUAL CELL IN LIST)                       
         MVC   0(2,R5),MKGPMGRP    MOVE MARKET GROUP NUMBER TO LIST             
         B     MGRD0100            AND GET NEXT KEY                             
*                                                                               
MGRDEXIT B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* READ THE AUTOPAY CLEARANCE RECORDS ON THE XSPFILE AND SORT THE DATA *         
***********************************************************************         
XAPYREAD NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         MVI   DATA,0              CLEAR SORT DATA                              
         XC    MYDATE,MYDATE                                                    
         CLI   QSTART,C' '         ANY START DATE INPUT?                        
         BNH   XAPY00              NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,MYDATE) CNVT TO 2-BYTE FMT             
         XC    MYDATE(2),=X'FFFF'  COMPLIMENT OF DATE                           
*                                                                               
XAPY00   XC    TOTRECS,TOTRECS     CLEAR TOTAL RECS                             
         MVC   DATADISP,=H'42'     DATADISP = 42                                
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R6,XKEY             R6 = KEY                                     
         USING APXRECD,R6          AUTOPAY RECORD DSECT                         
         MVI   APXKTYP,APXKTYPQ    RECORD TYPE '0D'                             
         MVI   APXKSUB,APXKSUBQ    SUB TYPE '3A'                                
         MVC   APXKDATE,MYDATE     0 FIRST TIME THRU                            
         MVC   APXKAGMD,MYAGY      AGY CODE IN HIGH ORDER                       
*                                                                               
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,DMWORK              
         B     XAPY20              JUMP OVER SEQ                                
*                                                                               
XAPY10   LA    R6,XKEY             R6 = KEY                                     
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,DMWORK              
*                                                                               
XAPY20   CLC   APXKEY(2),XKEYSAVE  HAVE AUTOPAY RECORD?                         
         BNE   XAPYXIT             NO - DONE                                    
         OC    MYDATE,MYDATE       FIRST TIME THRU?                             
         BNZ   XAPY30              NO - SO CHECK DATE                           
         MVC   MYDATE,APXKDATE     ELSE - SAVE DATE FOR NEXT PASS               
         B     XAPY40              AND CONTINUE                                 
*                                                                               
XAPY30   CLC   MYDATE,APXKDATE     SAME DATE?                                   
         BNE   XAPYXIT             NO - SO ALL DONE                             
*                                                                               
XAPY40   MVC   BYTE,APXKAGMD       GET THE AGY/MEDIA                            
         NI    BYTE,X'F0'          TURN OFF THE MEDIA                           
         CLC   MYAGY,BYTE          SAME AGY CODE?                               
         BNE   XAPY10              NO - SO GET NEXT KEY                         
*                                                                               
         MVC   BYTE,APXKAGMD       ELSE - CHECK FOR MEDIA BREAK                 
         NI    BYTE,X'0F'          TURN OFF THE AGENCY                          
         CLC   BYTE,MYMED          SAME MEDIA?                                  
         BE    XAPY50              YES - SO CONTINUE                            
*                                                                               
         CLI   MKTGRPS,C'N'        AGENCY USES MARKET GROUPS?                   
         BE    XAPY50              NO                                           
         MVC   MYMED,BYTE          ELSE - SAVE NEW MEDIA                        
         BAS   RE,MGRREAD          READ THE MARKET GROUP RECORDS                
*                                                                               
XAPY50   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',XKEY+36,ADBUY,DMWORK          
         L     R6,ADBUY            R6 = A(AUTOPAY RECORD)                       
         MVC   SVAGMD,APXKAGMD     SAVE A/M FOR SORT                            
*                                                                               
         USING APXEL,R6            X'01' ELEMENT DSECT                          
         MVI   ELCODE,APXELQ       X'01' ELEM                                   
         BRAS  RE,GETEL            HAVE A X'01' ELEMENT?                        
         BNE   XAPY10              NO - READ SEQ                                
*                                                                               
         OC    APXPAID,APXPAID     ANY PAID DATE?                               
         BZ    XAPY10              NO - READ SEQ                                
*                                                                               
         CLI   QCLT,C'$'           OFFICE LIST SECURITY?                        
         BE    *+12                YES                                          
         CLI   QCLT,C'*'           SINGLE OFFICE REQUEST?                       
         BNE   XAPY70              NO                                           
*                                                                               
         CLC   APXCLT,LASTCLT      PROCESSED THIS CLIENT PREVIOUSLY?            
         BNE   *+14                NO - SAVE CLT AND READ CLT REC               
         CLC   APXMED,LASTMED      PROCESSED THIS MEDIA PREVIOUSLY?             
         BE    XAPY60              YES - SAME MED/CLT                           
         MVC   LASTCLT,APXCLT      SET LAST CLIENT                              
         MVC   LASTMED,APXMED      SET LAST MEDIA                               
*                                                                               
         BAS   RE,AMBIT            SET RIGHT MEDIA BIT IN BAGYMD                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+1(1),BAGYMD     A/M                                          
         GOTO1 CLPACK,DMCB,APXCLT,KEY+2                                         
         GOTO1 HIGH                READ THE CLIENT RECORD                       
         CLC   KEY(13),KEYSAVE     FOUND THE CLIENT RECORD?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         GOTO1 GETCLT              GET THE CLIENT RECORD                        
         L     R2,ADCLT            A(CLIENT RECORD)                             
         USING CLTRECD,R2          CLIENT RECORD DSECT                          
         CLI   QCLT,C'*'           SINGLE OFFICE REQUEST?                       
         BNE   XAPY55              NO                                           
         CLC   COFFICE,QCLT+1      MATCH ON CLIENT OFFICE?                      
         BE    XAPY70              YES                                          
         XC    LASTCLT,LASTCLT     FORCE OPTIMIZATION FAIL                      
         B     XAPY10              READ SEQ                                     
*                                                                               
XAPY55   L     R3,ADCONLST         ADCON LIST                                   
         USING SPADCONS,R3         ADCON LIST DSECT                             
         LA    R1,DUB              R1 = DUB                                     
         USING OFFICED,R1          OFFICE DSECT                                 
         XC    DUB(OFCLENQ),DUB    CLEAR DUB FOR OFCLENQ                        
         MVI   OFCSYS,C'S'         SYSTEM = SPOT                                
         MVC   OFCAUTH,QCLT        CLIENT                                       
         MVC   OFCLMT(3),QCLT      1 OR 2 CHARACTER OFFICE LIST                 
         MVC   OFCAGY,QAGY         AGENCY                                       
         MVC   OFCOFC,COFFICE      OFFICE                                       
         DROP  R1,R2               DROP CLIENT RECORD AND OFFICE USINGS         
*                                                                               
         MVI   CLTSEC,C'N'         DEFAULT TO NOT PRINTING CLT                  
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   XAPY10              NO - READ SEQ                                
         MVI   CLTSEC,C'Y'         SET TO PRINT THIS CLIENT                     
         DROP  R3                  DROP ADCONS USING                            
*                                                                               
XAPY60   CLI   CLTSEC,C'Y'         PRINT CLIENT?                                
         BNE   XAPY10              NO - READ SEQ                                
*                                                                               
XAPY70   XC    SORTREC,SORTREC     INITIALIZE THE SORT RECORD                   
         LA    R7,SORTREC          R7 = SORTREC                                 
         USING SORTRECD,R7         SORT RECORD DSECT                            
*                                                                               
         XC    SRMKTGR,SRMKTGR     MARKET GROUP NUMBER                          
         CLC   APXMKT,=C'ALL '     ALL MARKETS?                                 
         BE    XAPY80              YES                                          
         CLI   MKTGRPS,C'N'        AGENCY USES MARKET GROUPS?                   
         BE    XAPY80              NO                                           
         PACK  DUB,APXMKT(4)       SET UP MARKET CONVERSION                     
         CVB   R1,DUB              TO HEX                                       
         SLL   R1,1                DOUBLE MKT # TO GET INDEX                    
         LA    R5,MKGLIST          A(INDEXED MKT LIST)                          
         AR    R5,R1               A(ACTUAL CELL IN LIST)                       
         MVC   SRMKTGR,0(R5)       MARKET GROUP NUMBER                          
*                                                                               
XAPY80   MVC   SRMKT,APXMKT        MARKET NUMBER                                
         MVC   SRSTA,APXSTA        STATION CALL LETTERS                         
         MVC   SRMED,APXMED        MEDIA                                        
         MVC   SRCLT,APXCLT        CLIENT                                       
         MVC   SRPRD,APXPRD        PRODUCT                                      
         MVC   SRPRD2,APXPTN       PARTNER                                      
         MVC   SREST,APXEST        ESTIMATE                                     
*                                                                               
         MVC   SRSREP,APXSREP      SPECIAL REP                                  
         MVC   SRMOS,APXMONTH      MONTH OF SERVICE                             
         MVC   SRPAID,APXPAID      PAID DATE                                    
         MVC   SRBAGYMD,SVAGMD     COMPRESSED AGENCY/MEDIA                      
*                                                                               
         LA    R5,SRINVLST         A(LIST IN SORT REC)                          
         LA    R1,10               UP TO 10 INVOICES                            
         L     R6,ADBUY            A(AUTOPAY RECORD)                            
         MVI   ELCODE,APXIELQ      INVOICE (X'10') ELEMENT                      
         BRAS  RE,GETEL            GET THE ELEMENT                              
         B     *+8                 GO TEST IF ELEMENT FOUND                     
*                                                                               
XAPY90   BRAS  RE,NEXTEL           HAVE ANOTHER X'10' ELEMENT?                  
         BNE   XAPY100             NO - DONE                                    
*                                                                               
         USING APXIEL,R6           INVOICE ELEMENT DSECT                        
         MVC   0(10,R5),APXIINV    MOVE IN THE INVOICE NUMBER                   
         LA    R5,L'SRINVLST(R5)   NEXT INV NUMBER POSITION                     
         BCT   R1,XAPY90           GET THE NEXT INVOICE ELEMENT                 
         DROP  R6                  DROP INVOICE ELEMENT USING                   
*                                                                               
XAPY100  SHI   R1,10               GET NUMBER OF INVOICES                       
         LCR   R1,R1               MAKE SURE IT'S POSITIVE                      
         STC   R1,SRINVNUM         NUMBER OF INVOICES                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R7) WRITE THE REC TO SORT               
         MVI   DATA,1              SET 'DATA FOUND' FLAG                        
         B     XAPY10              GO READ SEQ                                  
*                                                                               
XAPYXIT  B     EXIT                EXIT                                         
         DROP  R7                  DROP SORT RECORD USING                       
         EJECT                                                                  
*                                                                               
* READ THE SORTED RECORDS BACK AND PRINT 'EM OUT                                
*                                                                               
PRINTIT  NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         LA    R4,P                A(PRINT LINE)                                
         USING PRINTD,R4                                                        
*                                                                               
PRNT0100 OC    DATA,DATA           ANY SORT RECORDS WRITTEN?                    
         BZ    PRNTDONE            NO - SO ALL DONE                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)         E.O.F?                                       
         BZ    PRNTDONE            YES - SO ALL DONE                            
*                                                                               
         LA    RE,SORTREC          MOVE REC TO SORTREC TO SEE IN DUMPS          
         LA    RF,SRRECLEN         LENGTH                                       
         LR    R0,R5                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,SORTREC                                                       
         USING SORTRECD,R5                                                      
         OC    FIRSTREC,FIRSTREC   FIRST RECORD?                                
         BNZ   PRNT0200            YES - SO CONTINUE                            
*                                                                               
         CLC   SRMKTGR,PRIORMKG    ELSE - SAME MARKET GROUP?                    
         BNE   PRNT0200            NO - NEW PAGE                                
         CLC   SRMED,PRIORMED      ELSE - SAME MEDIA?                           
         BE    PRNT0300            YES - SO CONTINUE                            
*                                                                               
PRNT0200 MVI   FORCEHED,C'Y'       FORCE A PAGE BREAK                           
         MVC   PRIORMKG,SRMKTGR    SAVE THE NEW MARKET GROUP                    
         MVC   PRIORMED,SRMED      SAVE THE NEW MEDIA                           
*                                                                               
PRNT0300 MVC   PRMKT,SRMKT         MARKET NUMBER                                
         MVC   PRSTA,SRSTA         STATION CALL LETTERS                         
         MVC   PRCLT,SRCLT         CLIENT                                       
         MVC   PRPRD,SRPRD         PRODUCT                                      
         MVC   PRPRD2,SRPRD2       PIGGY PRODUCT                                
         MVC   PREST,SREST         ESTIMATE                                     
         MVC   PRSREP,SRSREP       SPECIAL REP                                  
         CLC   =C'CK',QAGY         COKE                                         
         BNE   *+10                                                             
         MVC   PRACN,SRACN         ACN NUMBER FOR COKE                          
         MVC   PRMOS,SRMOS         MONTH OF SERVICE                             
         GOTO1 DATCON,DMCB,(2,SRPAID),(5,PRPAID)                                
*                                                                               
         LA    R7,SRINVLST         A(INVOICE NUMBERS)                           
         SR    R3,R3               SET R3 TO 0                                  
         ZICM  R2,SRINVNUM,1       # OF INVOICES TO PRINT                       
         BZ    PRNT0550                                                         
         CLI   SRINVNUM,5          MORE THAN 5 INVOICES?                        
         BNH   PRNT0400            NO - SO CONTINUE                             
*                                                                               
         LA    R2,5                ELSE - FORCE TO 5 FOR 1ST PASS               
         IC    R3,SRINVNUM         # OF INVOICES                                
         SR    R3,R2               -5 FOR 2ND PASS                              
*                                                                               
PRNT0400 LA    R6,PRINV            A(INVOICE PRINT AREA)                        
*                                                                               
PRNT0500 MVC   0(10,R6),0(R7)      MOVE OUT THE INVOICE NUMBER                  
         LA    R6,L'PRINV(R6)      INC TO NEXT PRINT POSITION                   
         LA    R7,L'SRINVLST(R7)   INC TO NEXT INV NUMBER                       
         BCT   R2,PRNT0500         AND LOOP BACK                                
*                                                                               
PRNT0550 GOTO1 REPORT              PRINT THE LINE                               
         CH    R3,=H'0'            NEED A 2ND PASS?                             
         BE    PRNT0600            NO - SO CONTINUE                             
*                                                                               
         LR    R2,R3               SET R2 FOR SECOND PASS                       
         SR    R3,R3                                                            
         B     PRNT0400            AND DO 2ND PASS                              
*                                                                               
PRNT0600 XC    FIRSTREC,FIRSTREC   CLEAR THE 'FIRST RECORD' FLAG                
         ZICM  R1,TOTRECS,2        GET CURRENT TOTAL # RECORDS                  
         LA    R1,1(R1)            ADD 1                                        
         STCM  R1,3,TOTRECS        AND STORE IT                                 
         B     PRNT0100            GO GET NEXT SORT RECORD                      
*                                                                               
PRNTDONE GOTO1 REPORT              SKIP A LINE                                  
         MVC   P(6),=C'TOTAL:'                                                  
         EDIT  TOTRECS,(6,P+7),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
PRNTEXIT B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*********************************************************************           
* PRINT NET REPORT VIA SORTED RECORDS                                           
*********************************************************************           
NPRINTIT NTR1                                                                   
         MVI   RCSUBPRG,2                                                       
         LA    R4,P                A(PRINT LINE)                                
         USING NPRINTD,R4                                                       
*                                                                               
NPRNT100 OC    DATA,DATA           ANY SORT RECORDS WRITTEN?                    
         BZ    NPRNTX              NO - SO ALL DONE                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)         E.O.F?                                       
         BZ    NPRNTX              YES - SO ALL DONE                            
*                                                                               
         LA    RE,SORTREC          MOVE REC TO SORTREC TO SEE IN DUMPS          
         LA    RF,NSRRECLN         LENGTH                                       
         LR    R0,R5                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,SORTREC                                                       
         USING NSRTRECD,R5                                                      
         OC    FIRSTREC,FIRSTREC   FIRST RECORD?                                
         BNZ   NPRNT200            YES - SO CONTINUE                            
*                                                                               
         CLC   NSRMED,PRIORMED     ELSE - SAME MEDIA?                           
         BE    NPRNT300            YES - SO CONTINUE                            
*                                                                               
NPRNT200 MVI   FORCEHED,C'Y'       FORCE A PAGE BREAK                           
         MVC   PRIORMED,NSRMED     SAVE THE NEW MEDIA                           
*                                                                               
NPRNT300 DS    0H                                                               
         MVC   NPRMED,NSRMED         MEDIA                                      
         MVC   NPRCLT,NSRCLT         CLIENT                                     
         MVC   NPRPRD,NSRPRD         PRODUCT                                    
         MVC   NPRPRD2,NSRPRD2       PIGGY PRODUCT                              
*                                                                               
         CLC   NSRPAK,=C'000'                                                   
         BE    *+10                                                             
         MVC   NPRPAK,NSRPAK         PACKAGE                                    
*                                                                               
         CLC   NSREST,=C'000'                                                   
         BE    *+10                                                             
         MVC   NPREST,NSREST         ESTIMATE                                   
*                                                                               
         MVC   NPRSTA,NSRSTA         STATION CALL LETTERS                       
         MVC   NPRSREP,NSRSREP       SPECIAL REP                                
         MVC   NPRMONTH,NSRMONTH     MONTH OF SERVICE                           
         GOTO1 DATCON,DMCB,(2,NSRPAID),(5,NPRPAID)                              
         MVC   NPRINV,NSRINV         INVOICE                                    
*                                                                               
NPRNT550 GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
NPRNT600 XC    FIRSTREC,FIRSTREC   CLEAR THE 'FIRST RECORD' FLAG                
         ZICM  R1,TOTRECS,2        GET CURRENT TOTAL # RECORDS                  
         LA    R1,1(R1)            ADD 1                                        
         STCM  R1,3,TOTRECS        AND STORE IT                                 
         B     NPRNT100            GO GET NEXT SORT RECORD                      
*                                                                               
NPRNTX   GOTO1 REPORT              SKIP A LINE                                  
         MVC   P(6),=C'TOTAL:'                                                  
         EDIT  TOTRECS,(6,P+7),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
*        TURN ON RIGHT MEDIA BIT IN BAGYMD                                      
**********************************************************************          
*                                                                               
AMBIT    NTR1                                                                   
         USING APYEL,R6                                                         
         NI    BAGYMD,X'F0'        KEEP AGY                                     
         LA    R5,MEDTAB                                                        
AMB10    CLC   APYMED,0(R5)                                                     
         BE    AMB20                                                            
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   1(R5),X'FF'         END OF TABLE?                                
         BNE   AMB10                                                            
         OI    BAGYMD,X'01'        DEFAULT TO T                                 
         B     AMBX                                                             
AMB20    MVC   BYTE,1(R5)                                                       
         OC    BAGYMD,BYTE         TURN ON CORRECT MEDIA BIT                    
AMBX     B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* THIS ROUTINE DOES THE HEADHOOK STUFF                                          
*                                                                               
HEADHK   NTR1                                                                   
         CLI   PROCFLAG,C'N'       NET AUTOPAY?                                 
         JE    HDHKEXIT                                                         
         OC    DATA,DATA           ANY DATA TO PRINT?                           
         JZ    HDHKEXIT            NO - SO JUST EXIT                            
*                                                                               
         CLC   =C'CK',QAGY         COKE                                         
         BNE   HDHK0010                                                         
         MVC   H7+29(4),=C'ACN '                                                
         MVC   H8+28(5),=C'-----'                                               
*                                                                               
HDHK0010 LA    R5,SORTREC          A(CURRENT SORT RECORD)                       
         USING SORTRECD,R5                                                      
         MVC   H1+14(1),SRMED                                                   
         MVC   H2+14(1),PROFILE+11 MARKET GROUP ID                              
         GOTO1 HEXOUT,DMCB,SRMKTGR,H2+15,2,=C'TOG'                              
*                                                                               
* GET THE NAME OF THE MARKET GROUP                                              
*                                                                               
         XC    KEY,KEY             INITIALIZE THE KEY                           
         LA    R6,KEY                                                           
         USING MKGRECD,R6                                                       
         MVC   MKGKTYP,=X'0D02'    MARKET GROUP RECORD TYPE                     
         MVC   MKGKAGMD,SRBAGYMD   AGENCY/MEDIA                                 
         MVC   MKGKMID,PROFILE+11  MARKET GROUP ID CODE                         
         MVC   MKGKMGRP,SRMKTGR    MARKET GROUP                                 
         GOTO1 HIGH                READ THE KEY                                 
*                                                                               
         CLC   MKGKEY,KEYSAVE      SAME KEY?                                    
         BNE   HDHKEXIT            NO - SO JUST LEAVE                           
*                                                                               
         GOTO1 GETBUY              GET THE RECORD INTO BUY IO AREA              
         L     R6,ADBUY            A(MKGREC)                                    
*                                                                               
         MVI   ELCODE,X'10'        BREAK NAME ELEMENT CODE                      
         BRAS  RE,GETEL                                                         
         BNE   HDHKEXIT            NOT FOUND - JUST LEAVE                       
*                                                                               
         USING MKGEL10,R6                                                       
         CLC   MKGNAM3(4),=C'    ' ANY BREAK 3 NAME?                            
         JNH   HDHK0100            NO - SO CONTIUE                              
*                                                                               
         LA    R1,MKGNAM3          ELSE - SET A(3RD BREAK NAME)                 
         B     HDHK0300            AND CONTINUE                                 
*                                                                               
HDHK0100 CLC   MKGNAM2(4),=C'    ' ANY BREAK 2 NAME?                            
         JNH   HDHK0200            NO - SO CONTINUE                             
         LA    R1,MKGNAM2          ELSE - SET A(2ND BREAK NAME)                 
         B     HDHK0300            AND CONTINUE                                 
*                                                                               
HDHK0200 LA    R1,MKGNAM1          TAKE THE FIRST NAME                          
HDHK0300 MVC   H2+22(24),0(R1)      MOVE OUT THE NAME                           
*                                                                               
HDHKEXIT B     EXIT                                                             
         DROP  R5,R6                                                            
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* SORT FIELDS=(KEY STARTING COL,KEY LENGTH,ASCENDING/DESCENDING)                
* FORMAT=BINARY, I DON'T KNOW WHAT THE 'WORK=' PARAMETER DOES                   
*                                                                               
* SORT CARD FOR DESCENDING COST SORT                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,24,A),FORMAT=BI,WORK=1'                      
*                                                                               
* RECORD TYPE=FIXED/VARIABLE, LENGTH=SRRECLEN                                   
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=138'                                   
*                                                                               
SORTCAR2 DC    CL80'SORT FIELDS=(1,21,A),FORMAT=BI,WORK=1'                      
RECCARD2 DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
*                                                                               
         DS    0D                  RE-ALIGN (J.I.C)                             
*                                                                               
ELCODE   DS    X                                                                
FIRSTREC DS    X                   'FIRST RECORD' FLAG                          
MKTGRPS  DS    X                                                                
MYAGY    DS    X                   AGY CODE IN HIGH ORDER NIBBLE                
MYMED    DS    X                   CURRENT MEDIA IN LOW ORDER                   
MYDATE   DS    XL2                 CURRENT DATE                                 
DATA     DS    X                   'DATA FOUND' FLAG                            
MULTMONS DS    X                   MULTIPLE MONTHS IN REC                       
SVAGMD   DS    X                   SAVED AGY/MED                                
MYKEY    DS    XL13                                                             
TOTRECS  DS    XL2                 TOTAL AUTOPAY RECORDS PROCESSED              
PROCFLAG DS    CL1                 N=NET, S=SPOT                                
CLTSEC   DS    CL1                 Y=OK CLT                                     
LASTCLT  DS    CL3                                                              
LASTMED  DS    CL1                                                              
SVKEY    DS    CL50                                                             
XKEY     DS    XL64                                                             
SVXKEY   DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
PRIORMKG DS    XL2                 PRIOR MARKET GROUP READ                      
PRIORMED DS    CL1                 PRIOR MEDIA                                  
PROFILE  DS    CL16                PROFILE AREA                                 
SORTREC  DS    CL(SRRECLEN)        SORT RECORD                                  
MKGLIST  DS    XL20000             INDEXED LIST OF MARKET GROUPS                
*                                                                               
* SORT RECORD DSECT                                                             
*                                                                               
SORTRECD DSECT                                                                  
SRMED    DS    CL1                 MEDIA                                        
SRMKTGR  DS    XL2                 MARKET GROUP NUMBER (PWOS)                   
SRSTA    DS    CL5                 STATION CALL LETTERS                         
SRMKT    DS    CL4                 MARKET NUMBER                                
SRCLT    DS    CL3                 CLIENT CODE                                  
SRPRD    DS    CL3                 PRODUCT CODE                                 
SRPRD2   DS    CL3                 PASSIVE PRODUCT CODE                         
SREST    DS    CL3                 ESTIMATE CODE                                
*                                                                               
SRKEYLEN EQU   *-SORTRECD          SORT KEY LENGTH                              
*                                                                               
SRSREP   DS    CL4                 SPECIAL REP CODE                             
         DS    CL1                                                              
         ORG   SRSREP                                                           
SRACN    DS    CL5                 ACN NUMBER FOR COKE - REPLACES SREP          
SRMOS    DS    CL6                 MONTH OF SERVICE                             
SRPAID   DS    XL2                 PAID DATE                                    
SRINVNUM DS    XL1                 NUMBER OF INVOICES                           
SRINVLST DS    10CL10              LIST OF UP TO 10 INVOICES                    
SRBAGYMD DS    XL1                 COMPRESSED AGY/MEDIA                         
*                                                                               
SRRECLEN EQU   *-SORTRECD          SORT RECORD LENGTH                           
*                                                                               
***********************************************************************         
*        SORT RECORD FOR NET                                                    
***********************************************************************         
NSRTRECD DSECT                                                                  
NSRMED   DS    CL1                 MEDIA                                        
NSRCLT   DS    CL3                 CLIENT CODE                                  
NSRPRD   DS    CL3                 PRODUCT CODE                                 
NSRPRD2  DS    CL3                 PASSIVE PRODUCT CODE                         
NSRPAK   DS    CL3                 PACKAGE CODE                                 
NSREST   DS    CL3                 ESTIMATE CODE                                
NSRSTA   DS    CL4                 STATION CALL LETTERS                         
NSRKEYLN EQU   *-NSRTRECD          SORT KEY LENGTH                              
NSRSREP  DS    CL4                 SPECIAL REP CODE                             
NSRMONTH DS    CL8                 MONTH OF SERVICE                             
NSRPAID  DS    XL2                 PAID DATE                                    
NSRINV   DS    CL10                INVOICE                                      
NSRRECLN EQU   *-NSRTRECD          SORT RECORD LENGTH                           
*                                                                               
***********************************************************************         
*        PRINT LINE DSECT                                                       
***********************************************************************         
NPRINTD  DSECT                                                                  
         DS    CL2                                                              
NPRMED   DS    CL1                                                              
         DS    CL2                                                              
NPRCLT   DS    CL3                                                              
         DS    CL3                                                              
NPRPRD   DS    CL3                                                              
         DS    CL3                                                              
NPRPRD2  DS    CL3                                                              
         DS    CL3                                                              
NPRPAK   DS    CL3                                                              
         DS    CL3                                                              
NPREST   DS    CL3                                                              
         DS    CL3                                                              
NPRSTA   DS    CL4                                                              
         DS    CL2                                                              
NPRSREP  DS    CL4                 SPECIAL REP                                  
         DS    CL2                                                              
NPRMONTH DS    CL8                 MONTH (MMM/YY)                               
         DS    CL2                                                              
NPRPAID  DS    CL8                 PAID DATE                                    
         DS    CL2                                                              
NPRINV   DS    CL10                LIST OF INVOICES                             
***********************************************************************         
*        PRINT LINE  DSECT                                                      
***********************************************************************         
PRINTD   DSECT                                                                  
PRSTA    DS    CL5                 STATION CALL LETTERS                         
         DS    CL1                                                              
PRMKT    DS    CL4                 MARKET NUMBER                                
         DS    CL1                                                              
PRCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PRPRD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PRPRD2   DS    CL3                 PIGGY PRODUCT                                
         DS    CL1                                                              
PREST    DS    CL3                 ESTIMATE                                     
         DS    CL2                                                              
PRACN    DS    CL5                 ACN NUMBER FOR COKE - REPLACE SREP           
         ORG   PRACN                                                            
         DS    CL1                                                              
PRSREP   DS    CL4                 SPECIAL REP CODE                             
         DS    CL1                                                              
PRMOS    DS    CL6                 MONTH OF SERVICE                             
         DS    CL1                                                              
PRPAID   DS    CL8                 PAID DATE                                    
         DS    CL1                                                              
PRINV    DS    5CL11               1ST 5 INVS (10 BYTES/1 SPACE EACH)           
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENAPY                                                       
       ++INCLUDE SPGENXAPY                                                      
       ++INCLUDE NEGENAPY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPREPIV02 11/15/16'                                      
         END                                                                    
