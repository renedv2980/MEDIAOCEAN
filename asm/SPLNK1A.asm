*          DATA SET SPLNK1A    AT LEVEL 029 AS OF 11/03/20                      
*PHASE T21E1AC                                                                  
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* HWON  04NOV20 028 DS 20.3                                                     
*               028 SPEC-49730|SUPPORT DELETE PURPOSE CODE                      
* HWON  26FEB20 028 DS 20.1                                                     
*               028 SPEC-36542|INCREASE NUMBER OF BOOKS IN SHEET                
* HWON  20NOV19 027 DS 19.3                                                     
*               027 -SPEC-25154|MEDIA R ONLY SUPPORTS 1 DEC RATINGS             
*               027 -SPEC-35129|SUPPORT 2-DECIMAL IMPS (00A) PROFILE            
* HWON  06JUN18 026 SPEC-19318|SPLIT LINE EFF-RATES INTO SEPARATE ELEMS         
* HWON  18APR18 025 SPEC-19994|PRODUCT CODE AAA NOT ALLOWED                     
* HWON  12SEP17 024 SPEC-13049-RELINK FOR LARGER IOAREAS                        
*               024 SEND ERROR IF ADDING DUPLICATE DEV RECORD                   
* HWON  28FEB17 023 2017.1 - AUTOMATED AVAIL UUID SUPPORT                       
* HWON  01NOV16 022 COMSCORE SUPPORT                                            
* HWON  19AUG16 021 TRUNCATE AVAIL/REVLINE NOTE IF TOO LONG                     
*               021 FIX ISSUE WE WERE ADDING CUME WITH NO LENGTH                
*               021 FIX ISSUE WHERE QLADJ                                       
* HWON  20MAY16 020 SEND ERROR IF RECORD OVERFLOWS                              
* HWON  31MAR16 019 DON'T ALLOW INVALID SCHEDULE GUIDELINES                     
* HWON  27JAN16 018 CHANGE QEST TO QBEST                                        
* HWON  13NOV08 012 FIX MORE UPGRADES                                           
* HWON  03NOV08 011 FIX SAVE DEMO/UPGRADE TO THE SHEET                          
* HWON  07AUG08 010 X-SCHED FEATURE                                             
* HWON  09MAR08 009 DEVIATED WEEKS / SCHEDULE GUIDELINES                        
* HWON  08MAY08 008 FIX SAVING SPILL DEMOS                                      
*               008 SAVE ALL TRANSFER LINES                                     
*               008 FIX AVAIL NOTES SAVE                                        
* HWON  18DEC07 007 DELETE ALL SPOTS                                            
* HWON  07NOV07 006 SUPPORT 2 CHARACTER ADJACENCY CODES ON THE LINE             
*               006 CHANGED ERROR RESPONSE MAP CODE FOR VER > 3.0.0.88          
* HWON  01NOV07 005 CHANGE LAST FIX TO NO LONGER TAKE A DEATH, INSTEAD          
*               005 JUST SAVE THE FIRST 8 BOOKS                                 
* HWON  31OCT07 004 MAKE SURE ONLY A MAX OF 8 BOOKS ARE SENT, ELSE DIE          
* HWON  17OCT07 003 SUPPORT ORBITS VALUES OF 99999                              
* HWON  17SEP07 002 2 CHARACTER BOOK TYPES FOR DEMOS/UPGRADES                   
*               002 2 CHARACTER ADJACENCY CODES                                 
*                                                                               
*=====================================================================*         
SPLNK23  TITLE 'SPOT REVISION SAVE- UPLOAD'                                     
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=CODE,REQUEST=*,SYSTEM=SPTSYSQ,LINKIO=Y,     X        
               WORKERKEY=SPCB,RLEN=512,                                X        
               BLOCKS=(B#SAVED,SAVED,B#WORKD,WORKD)                             
*                                                                               
SE#MAXLN EQU   1276                                                             
SE#CKSUM EQU   1296                                                             
SE#CKSML EQU   1297                                                             
SE#RECEX EQU   0049                                                             
SE#SGCKS EQU   1329                                                             
*                                                                               
CODE     NMOD1 0,**SL1A**,RR=RE                                                 
         LR    R6,R1                                                            
         USING LP_D,R6             R6=A(DDLINK PARAMETER BLOCK)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         MVC   MAPNUM,LP_QMAPN     EXTRACT MAP NUMBER                           
         MVC   AGY,LP_AGY          EXTRACT AGENCY                               
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         CLI   RUNPMODE,RRUNSTRQ   TEST 'FIRST' MODE                            
         JE    FIRST                                                            
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         B     EXITY                                                            
         DROP  R7                                                               
*==========================================================                     
* RUN-START - 'FIRST' MODE                                                      
*==========================================================                     
*                                                                               
FIRST    DS    0H                                                               
         CLC   STAMP,STAMPLIT      TEST CORRECT SAVE STORAGE STAMP              
         BE    FIRST10                                                          
         MVC   DUB(4),SVMED        SAVE MEDIA/CLIENT ALPHA                      
*                                                                               
         LA    R0,SAVED            NO - CLEAR SAVED STORAGE                     
         LHI   R1,SAVEDX-SAVED                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVMED(4),DUB        RESTORE MEDIA/CLIENT ALPHA                   
         MVC   STAMP,STAMPLIT                                                   
*                                                                               
FIRST10  MVC   LP_BLKS+((B#CLTREC-1)*L'LP_BLKS)(AIOLAST-AIO2),AIO2              
*                                                                               
         J     EXITY                                                            
*                                                                               
*==========================================================                     
* INIT - NOTE FIRST REC PROCESSED *BEFORE* INIT CALL!                           
*==========================================================                     
INIT     DS    0H                                                               
*&&DO                                                                           
         CLC   STAMP,STAMPLIT      TEST CORRECT SAVE STORAGE STAMP              
         BE    INIT02                                                           
         MVC   DUB(4),SVMED        SAVE MEDIA/CLIENT ALPHA                      
         LA    R0,SAVED            NO - CLEAR SAVED STORAGE                     
         LHI   R1,SAVEDX-SAVED                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SVMED(4),DUB        RESTORE MEDIA/CLIENT ALPHA                   
         MVC   STAMP,STAMPLIT                                                   
*&&                                                                             
*                                                                               
INIT02   MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   DEMTABS,CDEMTABS-COMFACSD(RF)                                    
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
*==========================================================                     
* RUN-REQUEST MODE - PROCESS AN UPLOAD RECORD                                   
*==========================================================                     
*                                                                               
INPUT    LA    RE,RECTAB                                                        
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   0(2,RE),MAPNUM     LOOK UP RECORD MAP CODE IN TABLE              
         BE    INPUT20                                                          
         AHI   RE,L'RECTAB                                                      
         BCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  DS    0H                                                               
         CLI   5(RE),X'FF'          CLEAR ALL ERRORS?                           
         BE    INPUT30               YES                                        
*                                                                               
         TM    5(RE),X'80'          CLEAR LOW LEVEL ERRORS?                     
         BZ    INPUT40                                                          
         CLI   ERRORFLG,FATALERR    FATAL ERROR, DON'T CLEAR                    
         BE    INPUT40                                                          
         CLI   ERRORFLG,SHTCHKSM    SHEET ERROR, DON'T CLEAR                    
         BE    INPUT40                                                          
INPUT30  MVI   ERRORFLG,0                                                       
*                                                                               
INPUT40  CLI   ERRORFLG,0          HAVE ANY ERRORS?                             
         BNE   EXITY               YES                                          
         SR    R0,R0                                                            
         ICM   R0,7,2(RE)                                                       
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR          SET A(PROCESSING ROUTINE)                    
*                                                                               
         MVC   XTRATEXT,SPACES     INITIALIZE EXTRA MESSAGE TEXT                
         GOTOR RECADDR                                                          
         B     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
*================================================================               
* ROUTINE TO VALIDATE AND BUILD REVISION RECORD                                 
*================================================================               
                                                                                
VALREV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVNTELEM,SVNTELEM                                                
         XC    QACOMM1(20),QACOMM1 CLEAR COMMENT ADDRESSES                      
         XC    QSHEET(QSHEETX-QSHEET),QSHEET   CLEAR SHEET VALUES               
         MVI   QENDREV,C'N'        SET REVISION NOT ENDED                       
*                                                                               
         CLI   QACTION,0           TEST ACTION GIVEN                            
         BE    *+10                                                             
         MVC   SVACTION,QACTION                                                 
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
         CLI   QMEDX,0                                                          
         BE    VALRV010                                                         
         MVC   SVBAGYMD,QMEDX                                                   
         MVC   SVMED,QMEDA                                                      
*                                                                               
VALRV010 OC    QCLTX,QCLTX                                                      
         BZ    *+10                                                             
         MVC   SVBCLT,QCLTX                                                     
*                                                                               
         CLI   SVBAGYMD,0          TEST MEDIA KNOWN                             
         BE    VALERR02                                                         
         OC    SVBCLT,SVBCLT       TEST CLIENT KNOWN                            
         BZ    VALERR04                                                         
*                                                                               
         L     R1,ACLTREC          TEST CLIENT RECORD IS AROUND                 
         CLC   SVBAGYMD(3),CKEYAM-CLTHDR(R1)                                    
         BE    VALRV015                                                         
*                                                                               
         MVC   QMEDX,SVBAGYMD      NO - GET CLIENT RECORD                       
         MVC   QCLTX,SVBCLT                                                     
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   VALERR03                                                         
*                                                                               
VALRV015 CLC   LP_VRSN,=AL1(4,6,0,50)  VERSION# >= V4.6.0.50?                   
         JNL   VALRV020                 YES                                     
*                                                                               
         MVC   FULL,=C'POL'        READ POL PRODUCT                             
         BRAS  RE,GTESTREC                                                      
         USING ESTHDR,RE                                                        
         L     RE,AESTREC                                                       
         OC    ENONTDMS(L'ENONTDMS*20),ENONTDMS HAVE CS DEMOS?                  
         JNZ   VALERR05                          YES, SEND AN ERROR             
         DROP  RE                                                               
*                                                                               
NEW      USING DRVKEY,SVKEY        BUILD NEW BUY KEY FROM INPUT DATA            
*                                                                               
VALRV020 XC    NEW.DRVKEY,NEW.DRVKEY                                            
*                                                                               
         MVI   NEW.DRVKTYP,DRVKTYPQ                                             
         MVI   NEW.DRVKSUB,DRVKSUBQ   X'10' - REVISION SUBTYPE                  
         CLC   MAPNUM,=AL2(I#SDRSKU)  REV SHEET UPLOAD?                         
         BE    VALRV030                                                         
         MVI   NEW.DRVKSUB,DWKKSUBQ   X'11' - WORK SUBTYPE                      
         CLC   MAPNUM,=AL2(I#SDWSKU)  WORK SHEET UPLOAD?                        
         JNE   *+2                     NO, BAD MAPNUM                           
*                                                                               
VALRV030 MVC   NEW.DRVKAM,SVBAGYMD                                              
         MVC   NEW.DRVKCLT,SVBCLT                                               
         CLC   QPRD,=C'AAA'        DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         MVC   NEW.DRVKPRD,QPRD                                                 
         MVC   NEW.DRVKEST,QBEST                                                
         MVC   NEW.DRVKMKT,QMKT                                                 
         MVC   NEW.DRVKREVS,QRSEQ                                               
         XC    NEW.DRVKREVS,EFFS                                                
*                                                                               
         OC    QRSEQ,QRSEQ                                                      
         BNZ   VALRV060                                                         
         CLI   QACTION,QACTADD                                                  
         JNE   *+2                 MUST HAVE SEQ NUMBER IF NOT ADD              
*                                                                               
VALRV060 CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         BNE   VALRV090                                                         
*                                                                               
         OC    QRSEQ,QRSEQ                                                      
         JNZ   *+2                 DON'T WANT SEQ NUMBER ON ADD                 
*                                                                               
         MVC   NEW.DRVKREVS,=X'000001'   FORCE TO READ LINES!                   
*                                                                               
         MVC   IOKEY(L'DRVKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOXSPDIR+B#REVREC'                   
*                                                                               
         CLC   IOKEY(DRVKREVS-DRVKEY),IOKEYSAV                                  
         BE    VALRV070                                                         
         MVC   QRSEQ,=X'FFFFFE'    1 COMPLEMENTED                               
         B     VALRV080                                                         
*                                                                               
VALRV070 MVC   QRSEQ,IOKEY+(DRVKREVS-DRVKEY)                                    
         SR    R1,R1                                                            
         ICM   R1,7,QRSEQ                                                       
         AHI   R1,-1               SUB 1 FOR NEXT SEQ# - SINCE COMP             
         C     R1,=F'1'                                                         
         JNH   *+2                 OUT OF SEQ NUMS!  IMPOSSIBLE!                
         STCM  R1,7,QRSEQ                                                       
*                                                                               
VALRV080 MVC   NEW.DRVKREVS,QRSEQ  SET IN KEY FOR ADD                           
*                                                                               
         L     R0,AREVREC          INITIALIZE NEW RECORD                        
         LHI   R1,IO5LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AREVREC          AND SET NEW KEY                              
         MVC   0(L'DRVKEY,R1),SVKEY                                             
         B     EXITY                                                            
*                                                                               
VALRV090 MVC   IOKEY(L'DRVKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUP+IOXSPDIR+B#REVREC'                    
         BNE   VALERR10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREC'                   
         BNE   VALERR11                                                         
* COMPUTE CHECKSUM FOR RECORD                                                   
         CLC   QCHKSUM,=F'1'       SO YOU CAN GET AROUND IN =LINK               
         BE    VALRV100                                                         
         L     RE,AREVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         BO    *-4                                                              
         STCM  R0,15,FULL                                                       
         CLC   FULL,QCHKSUM                                                     
         BNE   VALERR30                                                         
*                                                                               
*   ??   DON'T REALLY DELETE - CLEAR REC AND MARK - NEED KEEP SEQ NUMS          
*                                                                               
VALRV100 CLI   QACTION,QACTDEL     TEST DELETE                                  
         JNE   EXITY                                                            
         XR    R5,R5                                                            
*                                                                               
* DELETE EVERYTHING ASSOCIATED WITH THIS SHEET                                  
*                                                                               
VALRV110 GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOXSPDIR+B#REVREC'                      
         JNE   *+2                                                              
         CLC   IOKEY(DRVKREVS-DRVKEY+L'DRVKREVS),IOKEYSAV                       
         BNE   VALRV120                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUP+IOXSPDIR+B#REVREC'                    
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREC'                   
         JNE   *+2                                                              
*                                                                               
         LA    R2,IOKEY                                                         
         USING DRVRECD,R2                                                       
         OI    DRVKST0,DRVKSDEL    X'80' SET IN KEY+32                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOXSPDIR+B#REVREC'                     
         JNE   *+2                                                              
*                                                                               
         L     R2,AREVREC                                                       
         OI    DRVRST0,DRVRSDEL    X'80' SET IN REC+34                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREC'                     
         JNE   *+2                                                              
         DROP  R2                                                               
*                                                                               
         L     RF,LP_ALPXD                                                      
***                                                                             
* CHECK IF TXPNUM WAS SET                                                       
***                                                                             
         CLC   (LP_XPINF-LP_XD+TXPNUM-TXPINFO)(L'TXPNUM,RF),=X'0000'            
         JNE   VALRV110            YES, PC APP                                  
***                                                                             
* CHECK IF DDS TERMINAL                                                         
***                                                                             
         TM    LP_XTRAV-LP_XD+(XIFLAG1-XTRAINFD)(RF),XIDDSPER+XIDDSTRM          
         JZ    VALRV110            NO, NOT A DDS TERMINAL                       
*                                                                               
         CLC   QCHKSUM,=F'1'       USER ENTERED?                                
         JNE   VALRV110                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         CHI   R5,100              DID WE PUT 100 RECORD TO FILE?               
         JL    VALRV110                                                         
         GOTO1 VDATAMGR,DMCB,(0,=C'COMMIT'),0                                   
         XR    R5,R5                                                            
         J     VALRV110                                                         
*                                                                               
VALRV120 MVC   IOKEY(L'DRVKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOXSPDIR+B#REVREC'                   
         JE    *+12                                                             
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         JZ    *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREC'                   
         JNE   *+2                                                              
*                                                                               
         LA    R2,IOKEY                                                         
         USING DRVRECD,R2                                                       
         OI    DRVKST0,DRVKSDEL    X'80' SET IN KEY+32                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOXSPDIR+B#REVREC'                     
         JNE   *+2                                                              
*                                                                               
         L     R2,AREVREC                                                       
         OI    DRVRST0,DRVRSDEL    X'80' SET IN REC+34                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREC'                     
         JNE   *+2                                                              
         DROP  R2                                                               
*                                                                               
         B     EXITY                                                            
*================================================                               
* REVISION/WORK NAME                                                            
*================================================                               
                                                                                
VRNAME   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RSNELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSNELQ       X'10'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RSNELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,RSNELQ         X'10'                                        
*                                                                               
         L     R2,QRNAME           POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         JNP   EXIT                                                             
         LA    RF,RSNLENQ(RE)      ADD OVERHEAD                                 
         STC   RF,RSNLEN           SET NEW ELEM LENGTH                          
                                                                                
         BCTR  RE,0                                                             
         MVC   RSNAME(0),LW_DATA1                                               
         EX    RE,*-6                                                           
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*================================================                               
* REVISION/WORK DATA                                                            
*================================================                               
                                                                                
VRDATA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RSDELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSDELQ       X'20'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RSDELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSDEL,RSDELQ        X'20'                                        
         MVI   RSDLEN,RSDLENQ                                                   
*                                                                               
         MVC   RSDFLTST,QSTDT                                                   
         MVC   RSDFLTEN,QENDT                                                   
         MVC   RSDADJ,QADJ                                                      
*                                                                               
         ICM   RF,15,QAPURP        TEST PURPOSE CODE CHANGE?                    
         JZ    *+10                                                             
         MVC   RSDPURP,LW_DATA1-LW_D(RF)  YES, SAVE TO RECORD                   
*                                                                               
         CLI   QREP,0                                                           
         JE    *+10                                                             
         MVC   RSDREP,QREP                                                      
         CLI   QBYR,0                                                           
         JE    *+10                                                             
         MVC   RSDBYR,QBYR                                                      
         CLC   QPRD,=C'AAA'        DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         CLI   QPRD,0                                                           
         JE    *+10                                                             
         MVC   RSDPRD,QPRD                                                      
*                                                                               
         OC    QAUDM,QAUDM         HAVE VALUE?                                  
         BZ    VRDAT10              NO                                          
         GOTOR VALDCD,DMCB,QAUDM,(0,L'QAUDM),RSDAUDEM                           
         JNE   EXITN                                                            
         GOTOR BLDNTEL,DMCB,('RSNDLENQ',RSDAUDEM),('RSNDLEN2',QAUDM),0          
*                                                                               
VRDAT10  MVC   RSDFDALY,QDAILY                                                  
         MVC   RSDFCMBD,QCOMBDLY                                                
         MVC   RSDFAADJ,QAUTADJ                                                 
         MVC   RSDFAAD2,QAARHOME                                                
         MVC   RSDFAAD3,QAATARGT                                                
         MVC   RSDHILN#,QHILINE#                                                
         MVC   RSCSBKTY,QCSBKTY                                                 
         MVC   RSCSSRDT,QCSSRDT                                                 
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*================================================                               
* REVISION/WORK DEMOS                                                           
*================================================                               
                                                                                
VRDEMO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RSCELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSCELQ       X'30'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RSCELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSCEL,RSCELQ        X'30'                                        
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QADEMO                                                     
         JZ    EXIT                                                             
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         JZ    EXIT                NONE, EXIT                                   
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VRDDEMO,R2                                                       
*                                                                               
VRDEM10  GOTOR VALDCD,DMCB,VRDDEMO,(0,L'VRDDEMO),RSCDEMO                        
         JNE   EXITN                                                            
*                                                                               
         GOTOR BLDNTEL,DMCB,('RSNDLENQ',RSCDEMO),('RSNDLEN2',VRDDEMO),0         
*                                                                               
VRDEM30  MVC   RSCDFBCS,VRDDFBCS   SET FLAGS                                    
         MVC   RSCDFCBL,VDRDFCBL   SET FLAGS                                    
         MVC   RSCDFDIS,VDRDFDIS   SET FLAGS                                    
*                                                                               
         AHI   R2,VRDLEN           NEXT ARRAY ENTRY                             
         AHI   R3,RSCLEN2          NEXT DEMO IN ELEMENT                         
         BCT   R0,VRDEM10                                                       
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RSCLENQ                                                       
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
*                                                                               
VRDEMX   J     EXIT                                                             
         DROP  R2,R3                                                            
VRDEMD   DSECT                                                                  
VRDDEMO  DS    CL8                 R1..R255 OR X1824/RX1824                     
VRDDFBCS DS    XL(L'RSCDFBCS)      USE FOR B'CAST STATIONS                      
VDRDFCBL DS    XL(L'RSCDFCBL)      USE FOR CABLE STATIONS                       
VDRDFDIS DS    XL(L'RSCDFDIS)      DISPLAY CATEGORY ON R/W SHEET                
VRDLEN   EQU   *-VRDEMD            = LENGTH OF EACH ENTRY                       
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
**************************************************                              
* BUILD THE NON-TRADITINAL ELEMENT                                              
* ON ENTRY : P1 BYTE 0    OVERHEAD                                              
*               BYTE 1-3  3-BYTE DEMO BINARY                                    
*            P2 BYTE 0    L'ENTRY                                               
*               BYTE 1-3  A(SHORT NAME)                                         
*            P3 BYTE 0    BIT FLAG TO BE CHANGED                                
*               BYTE 1-3  A(SHORT NAME FLAG BYTE)                               
*                                                                               
* ON EXIT  : SVNTELEM UPDATED                                                   
**************************************************                              
BLDNTEL  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         ICM   R1,7,DMCB+1         R1 = A(3-BYTE DEMO BINARY)                   
                                                                                
         CLI   2(R1),0             HAVE NON-TRADITIONAL?                        
         BNE   BRSNO                                                            
         SR    RF,RF                                                            
         ICM   RF,1,1(R1)          GET INDEX                                    
         JZ    *+2                 IF 0, SOMETHING IS VERY WRONG                
         LLC   R2,DMCB+4           R2 = L'ENTRY                                 
         MR    RE,R2               MULT BY L'ENTRY                              
         LTR   RE,RE               TEST NUMBER IS GT 255                        
         JNZ   *+2                  IF RE IS NON-ZERO, IT IS                    
         LLC   R2,DMCB                                                          
         LA    RF,0(R2,RF)         ADD OVERHEAD                                 
         CHI   RF,255              ELEM LENGTH HIGHER THAN 255?                 
         JH    *+2                  YES, SOMETHING WRONG, DIE                   
         CLM   RF,1,SVNTELEM+1     IS THIS LENGTH LARGER?                       
         JL    *+8                 NO                                           
         STC   RF,SVNTELEM+1       YES, SET AS NEW ELEM LENGTH                  
*                                                                               
         LLC   R2,DMCB+4           R2 = L'ENTRY                                 
         SR    RF,R2               SUB L'ENTRY FROM LENGTH                      
         LA    RF,SVNTELEM(RF)     ADD LENGTH, RE = A(INSERTION)                
*                                                                               
         ICM   R1,7,DMCB+5         GET THE SHORT NAME                           
         JZ    *+2                  NO NAME WAS PASSED, NOT GOOD                
         CLC   0(L'ENONTDMS,R1),SPACES   IS IT A VALID NAME?                    
         JNH   *+2                        NO, SOMETHING WRONG                   
         MVC   0(L'ENONTDMS,RF),0(R1)    SAVE THE NAME                          
                                                                                
*                                                                               
         ICM   R1,7,DMCB+9         NEED TO SAVE NEW FLAG BIT?                   
         JZ    BRSYES                                                           
         NC    0(1,R1),DMCB+8      SET INPUT BITS TO 0 EXCEPT CHG BIT           
         XI    DMCB+8,X'FF'       NOW FLIP THE BITS SO WE CAN RESET THE         
         NC    L'ENONTDMS(1,RF),DMCB+8  REC BIT TO 0 & LEAVE REST ALONE         
         OC    L'ENONTDMS(1,RF),0(R1)  LASTLY, WE WILL OR IN THE BIT            
BRSYES   J     EXITY                                                            
BRSNO    J     EXITN                                                            
*================================================                               
* REVISION/WORK BOOKS                                                           
*================================================                               
                                                                                
VRBOOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RSBELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSBELQ       X'32'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RSBELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSBEL,RSBELQ        X'32'                                        
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QABOOK                                                     
         JZ    EXIT                                                             
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         JZ    EXIT                NONE, EXIT                                   
*                                                                               
         CHI   R0,20               MAX ENTRIES?                                 
         BNH   *+8                                                              
         LHI   R0,20               SAVE A MAX OF 20 (PREV 8) BOOKS              
*                                                                               
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
*                                                                               
VRBK10   MVC   RSBBOOK,0(R2)       MOVE BOOK YR/MON                             
*                                                                               
         AHI   R2,RSBLEN2          NEXT ARRAY ENTRY                             
         AHI   R3,RSBLEN2          NEXT DEMO IN ELEMENT                         
         BCT   R0,VRBK10                                                        
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RSBLENQ                                                       
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*================================================                               
* REVISION/WORK UPGRADE FORMULAS                                                
*================================================                               
                                                                                
VRUPG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RSUELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSUELQ       X'34'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RSUELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSUEL,RSUELQ        X'34'                                        
*                                                                               
         L     R2,QAUPS            POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN        # OF ENTRIES                                 
         JZ    EXIT                                                             
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VRUPGD,R2                                                        
*                                                                               
VRUPG10  CLC   VRUSTTDT,SPACES                                                  
         JNH   *+10                                                             
         MVC   RSUSTTDT,VRUSTTDT   UPGRADE START EFF DATE                       
         CLC   VRUENDDT,SPACES                                                  
         JNH   *+10                                                             
         MVC   RSUENDDT,VRUENDDT   UPGRADE END EFF DATE                         
         MVC   RSUTBRD,VRUTBRD                                                  
         MVC   RSUTCBL,VRUTCBL                                                  
         MVC   RSUFORM(L'VRUFORM),VRUFORM                                       
         LA    RF,RSUFORM+L'VRUFORM                                             
*                                                                               
VRUPG20  CLI   0(RF),C' '          CLEAR SPACES                                 
         BH    *+8                                                              
         BCT   RF,VRUPG20                                                       
         LA    RF,1(RF)                                                         
         SR    RF,R3                                                            
         CHI   RF,RSULENQ          LESS THAN THE OVERHEAD?                      
         BNL   *+8                                                              
         LA    RF,RSULENQ          SET MINIMUM OVERHEAD                         
         STC   RF,RSULEN                                                        
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         JNE   EXITN                                                            
*                                                                               
         LA    R2,VRUPGLNQ(R2)     BUMP TO NEXT ENTRY                           
         BCT   R4,VRUPG10                                                       
*                                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
VRUPGD   DSECT                                                                  
VRUSTTDT DS   XL2                                                               
VRUENDDT DS   XL2                                                               
VRUTBRD  DS   CL1                                                               
VRUTCBL  DS   CL1                                                               
VRUFORM  DS   CL150                                                             
VRUPGLNQ EQU   *-VRUPGD                                                         
         EJECT                                                                  
SVRDEF   CSECT                                                                  
*================================================                               
* REVISION/WORK CUMES                                                           
*================================================                               
                                                                                
VRCUME   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,RSCUELQ        X'36'                                        
         MVI   ELEM+RSCULEN-RSCUELD,RSCULNQ                                     
*                                                                               
         L     R3,AREVREC                                                       
         USING DRVRECD,R2                                                       
         LA    R3,DRVEL(R3)                                                     
*                                                                               
         MVI   ELCDLO,RSCUELQ      COPY OLD ELEMENT                             
         MVI   ELCDHI,RSCUELQ      X'36'                                        
         BRAS  RE,NEXTEL                                                        
         BNE   VRCM10                                                           
         MVC   ELEM,0(R3)          SAVE OLD ELEM *NOTE COPIES GARBAGE*          
         BRAS  RE,DELEL            AND DELETE IT                                
*                                                                               
         USING RSCUELD,R3                                                       
VRCM10   LA    R3,ELEM                                                          
*                                                                               
         CLI   SVMED,C'T'          TV?                                          
         BE    VRCM15                                                           
         CLI   QTIMEZON,0          SENT TIMEZONE?                               
         BE    VRCM30                                                           
         DC    H'0'                                                             
*                                                                               
VRCM15   CLI   RSCUTZ,0            HAVE TIME ZONE ALREADY?                      
         BE    VRCM20                                                           
         CLI   QTIMEZON,0          -YES, SENT ANOTHER?                          
         BE    VRCM30                -NOPE, THATS GOOD                          
         CLC   RSCUTZ,QTIMEZON       -YES, MAKE SURE THEY ARE THE SAME          
         JNE   *+2                    DIE! YOU CANNOT CHANGE TIMEZONE           
VRCM20   CLI   QTIMEZON,0          -NO, THEN ITS REQUIRED!                      
         JE    *+2                  -DIE, TIMEZONE IS REQUIRED!!                
         MVC   RSCUTZ,QTIMEZON                                                  
*                                                                               
VRCM30   ICM   R2,15,QACUME                                                     
         JZ    VRCM50                                                           
         USING LW_D,R2                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         JNP   VRCM50                                                           
         LA    RF,RSCULNQ(RE)      ADD OVERHEAD                                 
         STC   RF,RSCULEN          SET NEW ELEM LENGTH                          
*                                                                               
         BCTR  RE,0                                                             
         MVC   RSCUME(0),LW_DATA1                                               
         EX    RE,*-6                                                           
*                                                                               
VRCM50   BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*================================================                               
* REVISION/WORK COPIED FROM                                                     
*================================================                               
                                                                                
VRCOPY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RSRELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSRELQ       X'40'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RSRELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSREL,RSRELQ        X'40'                                        
         MVI   RSRLEN,RSRLENQ                                                   
*                                                                               
         L     R1,LP_ASECD         GET PID                                      
         MVC   RSRPID,SECOPASS-SECD(R1)                                         
         GOTO1 VDATCON,DMCB,(5,0),(2,RSRDATE)  AND TODAYS DATE                  
*                                                                               
         MVC   RSRMED,QCPMED                                                    
         MVC   RSRCLT,QCPCLT                                                    
         CLC   QCPPRD,=C'AAA'      DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         MVC   RSRPRD,QCPPRD                                                    
         MVC   RSREST,QCPEST                                                    
         MVC   RSRMKT,QCPMKT                                                    
         MVC   RSRSEQ,QCPSEQ                                                    
         XC    RSRSEQ,EFFS                                                      
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*===============================================================                
* WORKSHEET DAY/TIME PERIOD OVERRIDES                                           
*===============================================================                
                                                                                
VRDTP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         CLI   QDTPFRST,0                                                       
         BNE   VRDTP10                                                          
         MVI   ELCDLO,RSPNELQ      NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSPHELQ      X'44'/X'45'                                  
         BRAS  RE,DELEL                                                         
         MVI   QDTPFRST,C'N'                                                    
*                                                                               
         USING RSPNELD,R3                                                       
VRDTP10  LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSPNEL,RSPNELQ       X'44'                                       
         MVI   RSPNLEN,RSPNELQ                                                  
*                                                                               
         MVC   RSPNSTA,QSTA                                                     
         MVC   RSPNDAYS,QDAYS                                                   
         MVC   RSPNSTIM,QSTIME                                                  
         MVC   RSPNETIM,QETIME                                                  
*                                                                               
VRDTP15  ICM   R2,15,QDTPDEM                                                    
         JZ    VRDTP90                                                          
         MVI   RSPNTYPE,RSPNDEM                                                 
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LR    R1,R0                                                            
         AHI   R1,-14                                                           
VRDTP20  LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VRDTPD,R2                                                        
*                                                                               
VRDTP30  CR    R0,R1               MORE THAN 14 DEMOS?                          
         BH    VRDTP35             NO                                           
         LA    RF,ELEM             YES, ADD IT                                  
         SR    R3,RF                                                            
         AHI   R3,RSPNLENQ                                                      
         STC   R3,ELEM+1                                                        
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
         LA    R3,ELEM             AND ADD ANOTHER                              
*                                                                               
VRDTP35  GOTOR VALDCD,DMCB,VDTPCODE,(0,L'VDTPCODE),RSPNCAT                      
         JNE   EXITN                                                            
         GOTOR BLDNTEL,DMCB,('RSNDLENQ',RSPNCAT),('RSNDLEN2',VDTPCODE),+        
               0                                                                
*                                                                               
********************                                                            
* PROCESS RAW DEMO VALUE                                                        
********************                                                            
VRDTP40  OC    VDTPVAL,VDTPVAL     DEMO RAW VALUE ZERO?                         
         JZ    VRDTP50              YES, SKIP TESTS                             
*                                                                               
         CLI   VDTPPREC,0          DID PC SEND PRECISION?                       
         JNE   VRDTP47              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VRDTP45              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VRDTP45                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VDTPPREC,1          SET DEFAULT 1-DEC PREC                       
         CLI   VDTPCODE,C'R'       IF RATING OR                                 
         JE    VRDTP45                                                          
         CLI   VDTPCODE,C'E'       EXTENDED THEN                                
         JNE   VRDTP47                                                          
VRDTP45  MVI   VDTPPREC,2          SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT RADIO 2-DEC PREC RATING WITH 100TH VALUE                              
*                                                                               
VRDTP47  CLI   VDTPPREC,2          HAVE 2-DEC PREC VALUE?                       
         JNE   VRDTP50                                                          
*                                                                               
         CLI   SVMED,C'R'          MEDIA R?                                     
         JNE   VRDTP50              NO, SKIP TEST                               
         CLI   VDTPCODE,C'R'       RATING OR                                    
         JE    *+12                 YES                                         
         CLI   VDTPCODE,C'E'       EXTENDED?                                    
         JNE   VRDTP50              NO, SKIP TEST                               
*                                                                               
         GOTOR T2DECP,DMCB,(L'VDTPVAL,VDTPVAL)                                  
         JNE   VALERR12                                                         
*                                                                               
********************                                                            
* PROCESS SHR DEMO VALUE                                                        
********************                                                            
VRDTP50  OC    VDTPSHR,VDTPSHR     SHR VALUE ZERO?                              
         JZ    VRDTP60              YES, SKIP TESTS                             
*                                                                               
         CLI   VDTPSPRC,0          DID PC SEND PRECISION?                       
         JNE   VRDTP55              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VRDTP55              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VRDTP55                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VDTPSPRC,1          SET DEFAULT 1-DEC PREC                       
         CLI   VDTPCODE,C'R'       IF RATING OR                                 
         JE    VRDTP55                                                          
         CLI   VDTPCODE,C'E'       EXTENDED THEN                                
         JNE   VRDTP57                                                          
VRDTP55  MVI   VDTPSPRC,2          SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT SHARE 2-DEC PREC IMPRESSION WITH 100TH VALUE                          
*                                                                               
VRDTP57  DS    0H                                                               
*&&DO                                                                           
         CLI   VDTPSPRC,2          HAVE 2-DEC PREC VALUE?                       
         JNE   VRDTP60              NO, SKIP 2-DEC PREC TEST                    
         GOTOR T2DECP,DMCB,(L'VDTPSHR,VDTPSHR)                                  
         JNE   VALERR13                                                         
*&&                                                                             
********************                                                            
* PROCESS PUT DEMO VALUE                                                        
********************                                                            
VRDTP60  OC    VDTPPUT,VDTPPUT     PUT VALUE ZERO?                              
         JZ    VRDTP70              YES, SKIP TESTS                             
*                                                                               
         CLI   VDTPPPRC,0          DID PC SEND PRECISION?                       
         JNE   VRDTP67              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VRDTP65              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VRDTP65                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VDTPPPRC,1          SET DEFAULT 1-DEC PREC                       
         CLI   VDTPCODE,C'R'       IF RATING OR                                 
         JE    VRDTP65                                                          
         CLI   VDTPCODE,C'E'       EXTENDED THEN                                
         JNE   VRDTP67                                                          
VRDTP65  MVI   VDTPPPRC,2          SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT PUT 2-DEC PREC IMPRESSION WITH 100TH VALUE                            
*                                                                               
VRDTP67  DS    0H                                                               
*&&DO                                                                           
         CLI   VDTPPPRC,2          HAVE 2-DEC PREC VALUE?                       
         JNE   VRDTP70              NO                                          
         GOTOR T2DECP,DMCB,(L'VDTPPUT,VDTPPUT)                                  
         JNE   VALERR13                                                         
*&&                                                                             
VRDTP70  MVI   RSPNSVI,100              ??                                      
         MVC   RSPNRAW,VDTPVAL     VALUE                                        
         MVC   RSPNFP,VDTPPREC     DEMO PRECISION                               
         OC    RSPNFP,VDTPFLG      DEMO FLAGS                                   
         MVC   RSPNSHR,VDTPSHR     SHARE                                        
         MVC   RSPNSFP,VDTPSPRC    SHARE PRECISION                              
         OC    RSPNSFP,VDTPSFLG    SHARE FLAGS                                  
         MVC   RSPNPUT,VDTPPUT     PUT                                          
         MVC   RSPNPFP,VDTPPPRC    PUT PRECISION                                
         OC    RSPNPFP,VDTPPFLG    PUT FLAGS                                    
*                                                                               
* CODE BELOW CHECKS IF AN THE DEMO HAS NO OVERRIDES AND NO DEMO VALUES          
*  IF YES, THEN SKIP ADDING THAT DEMO                                           
*&&DO                                                                           
         CLI   RSPNDFP,C'N'        OVERRIDE?                                    
         JNE   VRDTP75             -NO                                          
         CLI   RSPNSDFP,C'N'       OVERRIDE?                                    
         JNE   VRDTP75             -NO                                          
         CLI   RSPNPDFP,C'N'       OVERRIDE?                                    
         JNE   VRDTP75             -NO                                          
         CP    RSPNRAW,=P'0'       0 DEMO VALUE?                                
         JNE   VRDTP75             -NO                                          
         CP    RSPNSHR,=P'0'       0 SHR VALUE?                                 
         JNE   VRDTP75             -NO                                          
         CP    RSPNPUT,=P'0'       0 PUT VALUE?                                 
         JE    VRDTP77             -YES, SKIP THIS ENTRY                        
*&&                                                                             
VRDTP75  AHI   R3,RSPNLNQ2         NEXT DEMO IN ELEMENT                         
VRDTP77  AHI   R2,VRDTPLNQ         NEXT ARRAY ENTRY                             
         BCT   R0,VRDTP30                                                       
*                                                                               
VRDTP80  LA    RF,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,RF                                                            
         AHI   R3,RSPNLENQ                                                      
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
VRDTP90  ICM   R2,15,QDTPUPG                                                    
         JZ    VRDTPX                                                           
         LA    R3,ELEM                                                          
         MVI   RSPNTYPE,RSPNUPG                                                 
         USING LW_D,R2                                                          
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         JNP   EXIT                                                             
         LA    RF,RSPNLENQ(RE)     ADD OVERHEAD                                 
         STC   RF,RSPNLEN          SET NEW ELEM LENGTH                          
*                                                                               
         BCTR  RE,0                                                             
         MVC   RSPNUFRM(0),LW_DATA1                                             
         EX    RE,*-6                                                           
*                                                                               
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
VRDTPX   J     EXIT                                                             
         DROP  R2,R3                                                            
VRDTPD   DSECT                                                                  
VDTPCODE DS    CL8                                                              
VDTPVAL  DS    XL(L'RSPNRAW)                                                    
VDTPFLG  DS    XL(L'RSPNFP)                                                     
VDTPPREC DS    XL(L'RSPNFP)                                                     
VDTPSHR  DS    XL(L'RSPNSHR)                                                    
VDTPSFLG DS    XL(L'RSPNSFP)                                                    
VDTPSPRC DS    XL(L'RSPNSFP)                                                    
VDTPPUT  DS    XL(L'RSPNPUT)                                                    
VDTPPFLG DS    XL(L'RSPNPFP)                                                    
VDTPPPRC DS    XL(L'RSPNPFP)                                                    
VRDTPLNQ EQU   *-VRDTPD                                                         
*                                                                               
SVRDEF   CSECT                                                                  
*===============================================================                
* WRITE REV RECORD TO FILE                                                      
*===============================================================                
                                                                                
ENDREV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
         MVC   QSEQ#,DRVKREVS      TO SEND BACK TO PC                           
         XC    QSEQ#,EFFS          UNCOMPLEMENT FOR THEM                        
*                                                                               
         MVI   QENDREV,C'Y'        SET FIRST REVISION ENDED                     
*                                                                               
         CLI   SVACTION,0          HAVE SAVE ACTION?                            
         JE    *+2                  NO                                          
*                                                                               
         CLI   SVACTION,QACTVAL    VALIDATION ONLY?                             
         BE    ENDREVX1                                                         
*                                                                               
* CHECK IF NEW NON-TRAD ELEMENT                                                 
*                                                                               
         CLI   SVNTELEM+1,0        DO WE HAVE SOMETHING?                        
         JE    ENDREV03            -NO                                          
*                                                                               
         L     R3,AREVREC          POINT TO END OF REC                          
         MVI   ELCDLO,RSNDELQ      NOW FIND THE OLD ELEMENT                     
         MVI   ELCDHI,RSNDELQ      X'38'                                        
         LA    R3,DRVEL                                                         
         BRAS  RE,NEXTEL           DO WE HAVE OLD NON-TRAD ELEM?                
         JNE   ENDREV02             NO                                          
         XC    ELEM,ELEM                                                        
         LLC   RE,1(R3)             YES, LETS SAVE IT IN ELEM                   
         BCTR  RE,0                                                             
         MVC   ELEM(0),0(R3)                                                    
         EX    RE,*-6                                                           
*                                                                               
         MVI   ELCDLO,RSNDELQ      NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSNDELQ      X'38'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         LA    RF,ELEM+2                                                        
         LA    RE,SVNTELEM+2                                                    
         LLC   R1,SVNTELEM+1                                                    
         LA    R1,SVNTELEM(R1)                                                  
*                                                                               
ENDREV01 CR    RE,R1               END OF SVNTELEM? FINISHED?                   
         JNL   ENDREV02             YES                                         
         OC    0(RSNDLEN2,RE),0(RE) NO, SVNTELEM MISSING ENTRY?                 
         JNZ   *+10                  NO, DON'T COPY FROM OLD                    
         MVC   0(RSNDLEN2,RE),0(RF)  YES, COPY FROM OLD                         
         LA    RF,RSNDLEN2(RF)       BUMP TO NEXT IN ELEM                       
         LA    RE,RSNDLEN2(RE)       BUMP TO NEXT IN SVNTELEM                   
         J     ENDREV01                                                         
*                                                                               
ENDREV02 MVC   ELEM,SVNTELEM       -YES, MOVE TO ELEM                           
         MVI   ELEM,RSNDELQ                                                     
         BRAS  RE,ADDEL              AND ADD IT                                 
*                                                                               
ENDREV03 BRAS  RE,GETDTTM          ITS IN WORK, DON'T BLOW IT AWAY              
*                                                                               
         CLI   SVACTION,QACTADD                                                 
         BNE   ENDREV10                                                         
*                                                                               
* INSERT ACTIVITY ELEMENT                                                       
*                                                                               
         USING RSAELD,R3                                                        
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   RSAEL,RSAELQ        X'90'                                        
         MVI   RSALEN,RSALENQ                                                   
         L     R1,LP_ASECD                                                      
         MVC   RSAAPID,SECOPASS-SECD(R1)                                        
         MVC   RSAADD,WORK                                                      
         MVC   RSAATIME,WORK+2                                                  
         DROP  R3                                                               
*                                                                               
ENDREV05 L     R3,AREVREC          POINT TO END OF REC                          
         SR    R0,R0                                                            
         ICM   R0,3,32(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOXSPFIL+B#REVREC'                     
         BE    ENDREVX             EXIT                                         
         DC    H'0'                                                             
*                                                                               
ENDREV10 DS    0H                                                               
         MVI   ELCDLO,RSAELQ       NOW FIND THE OLD ELEMENT                     
         MVI   ELCDHI,RSAELQ       X'90'                                        
         LA    R3,DRVEL                                                         
         BRAS  RE,NEXTEL           DID WE FIND IT?                              
         JNE   *+2                  NO, SHOULD ALWAYS BE THERE                  
         USING RSAEL,R3                                                         
         MVI   RSALEN,RSALENQ                                                   
         L     R1,LP_ASECD         AND UPDATE IT                                
         MVC   RSACPID,SECOPASS-SECD(R1)                                        
         MVC   RSACHG,WORK                                                      
         MVC   RSACTIME,WORK+2                                                  
         DROP  R3                                                               
*                                                                               
ENDREV16 GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREC'                     
         JNE   *+2                                                              
                                                                                
* RUN REV DOWNLOAD ?                                                            
                                                                                
*        SEND BACK CONFIRMATION = SEQ#, PCKEY AND CHKSUM                        
ENDREVX  DS    0H                                                               
*                                                                               
         L     RE,AREVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,QCHKSUM                                                    
*                                                                               
ENDREVX1 LHI   R0,I#SDSCNF          X'0330' - REVSHEET CONFIRMATION             
         CLI   NEW.DRVKSUB,DRVKSUBQ X'10' - REVISION SUBTYPE?                   
         BE    ENDREVX2                                                         
         LHI   R0,I#SDWCNF          X'0333' - WORKSHEET CONFIRMATION            
         CLI   NEW.DRVKSUB,DWKKSUBQ X'11' - WORK SUBTYPE?                       
         JNE   *+2                   NO, POSSIBL NEW RECORD                     
*                                                                               
ENDREVX2 BRAS  RE,SUCRESP          SEND SUCCESS RESPONSE                        
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* REVISION/WORK NOTES                                                           
*================================================================               
                                                                                
VRNOTE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QENDNOTE,C'N'                                                    
         BNE   VALRN10                                                          
         CLI   SVACTION,QACTDEL    ONLY PROCESS DELETES ONCE                    
         JE    EXITY                                                            
         B     VALRN50                                                          
*                                                                               
VALRN10  MVI   QENDNOTE,C'N'                                                    
*                                                                               
         CLI   QACTION,0           TEST ACTION GIVEN                            
         BE    *+10                                                             
         MVC   SVACTION,QACTION                                                 
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
         OC    QRSEQNOT,QRSEQNOT   DO WE HAVE A COMMENT SEQ#?                   
         BNZ   VALRN20                                                          
         CLI   QACTION,QACTDEL     ARE WE DELETING?                             
         JNE   *+2                 MUST HAVE SEQ NUMBER                         
*                                                                               
NEW      USING DRVKEY,SVKEY        BUILD NEW BUY KEY FROM INPUT DATA            
VALRN20  OI    NEW.DRVKFLG,DRVKCOM                                              
         MVC   IOKEY(L'DRVKEY),SVKEY                                            
*                                                                               
         CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         BNE   VALRN30                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOXSPDIR+B#REVREC'                   
         CLC   IOKEY(DRVKSPR3-DRVKEY),IOKEYSAV                                  
         BNE   VALRN25                                                          
*                                                                               
         TM    IOKEY+32,X'80'      DELETED?                                     
         BZ    VALERR18                                                         
*                                                                               
         NI    IOKEY+32,X'7F'      TURN OFF DELETE                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOXSPDIR+B#REVREC'                     
         JNE   *+2                                                              
         MVI   SVACTION,QACTCHA    FORCE ACTION TO CHANGE                       
         MVC   IOKEY(L'DRVKEY),SVKEY                                            
         B     VALRN30                                                          
*                                                                               
VALRN25  L     R0,AREVREC          INITIALIZE NEW RECORD                        
         LHI   R1,IO5LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AREVREC          AND SET NEW KEY                              
         MVC   0(L'DRVKEY,R1),SVKEY                                             
         B     VALRN50                                                          
*                                                                               
VALRN30  GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOXSPDIR+B#REVREC'                   
         BNE   VALERR10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREC'                   
         BNE   VALERR11                                                         
         CLI   QACTION,QACTADD     WAS HE TRYING TO ADD?                        
         BE    VALRN40                                                          
* COMPUTE CHECKSUM FOR RECORD                                                   
         CLC   QCHKSUM,=F'1'       SO YOU CAN GET AROUND IN =LINK               
         BE    VALRN40                                                          
         L     RE,AREVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         BO    *-4                                                              
         STCM  R0,15,FULL                                                       
         CLC   FULL,QCHKSUM                                                     
         BNE   VALERR31                                                         
*                                                                               
VALRN40  CLI   QACTION,QACTDEL     TEST DELETE                                  
         BE    VALRN60                                                          
*                                                                               
         L     RE,AREVREC                                                       
         NI    34(RE),X'7F'        TURN OF DELETED INCASE OF RESTORE            
*                                                                               
* DELETE THE OLD ELEMENTS                                                       
*                                                                               
         MVI   ELCDLO,RSNTELQ      NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RSNTELQ      X'22'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
VALRN50  XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RSNTELD,R3                                                       
         MVI   RSNTEL,RSNTELQ                                                   
         MVC   RSNTSEQ,QRSEQNOT                                                 
*                                                                               
         L     R2,QRNOTE           POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         JNP   EXIT                                                             
         LA    RF,RSNTLENQ(RE)     ADD OVERHEAD                                 
         STC   RF,RSNTLEN          SET NEW ELEM LENGTH                          
*                                                                               
         BCTR  RE,0                                                             
         MVC   RSNTTXT(0),LW_DATA1                                              
         EX    RE,*-6                                                           
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXITY                                                            
*                                                                               
VALRN60  OI    IOKEY+32,X'80'      SET DELETED FLAG                             
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOXSPDIR+B#REVREC'                     
         JNE   *+2                                                              
*                                                                               
         L     R3,AREVREC                                                       
         OI    34(R3),X'80'        SET RECORD DELETED                           
         B     EXITY                                                            
         EJECT                                                                  
*================================================================               
* REVISION/WORK NOTES                                                           
*================================================================               
                                                                                
ENDNTS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
         MVC   QSEQ#,DRVKREVS      TO SEND BACK TO PC                           
         XC    QSEQ#,EFFS          UNCOMPLEMENT FOR THEM                        
         DROP  R2                                                               
*                                                                               
         MVI   QENDNOTE,C'Y'        SET FIRST REVISION ENDED                    
*                                                                               
         CLI   SVACTION,0          SAVE ACTION KNOWN?                           
         JE    *+2                  NO                                          
*                                                                               
         CLI   SVACTION,QACTADD                                                 
         BNE   ENDNTS10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOXSPFIL+B#REVREC'                     
         BE    ENDNTSX                                                          
         DC    H'0'                                                             
*                                                                               
ENDNTS10 GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREC'                     
         JNE   *+2                                                              
                                                                                
*        SEND BACK CONFIRMATION = SEQ#, PCKEY AND CHKSUM                        
ENDNTSX  DS    0H                                                               
         L     RE,AREVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,QCHKSUM                                                    
*                                                                               
         LHI   R0,I#SDNCNF         X'0332' - REVSHEET NOTES CONFIRM...          
         CLI   IOKEY+1,DRVKSUBQ    X'10' - REVISION SUBTYPE?                    
         BE    ENDNTSX2                                                         
         LHI   R0,I#SDMCNF         X'0335' - WORKSHEET NOTES CONFIRM...         
         CLI   IOKEY+1,DWKKSUBQ    X'10' - REVISION SUBTYPE?                    
         BE    ENDNTSX2                                                         
*                                                                               
ENDNTSX2 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(R0))                   
*                                                                               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              *        
               ('LD_LBINQ',QSEQ#),(L'QSEQ#,0)                                   
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',2),              *        
               ('LD_CHARQ',QPCKEY),(L'QPCKEY,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',3),              *        
               ('LD_HEXDQ',QCHKSUM),(L'QCHKSUM,0)                               
         OC    IODA,IODA                                                        
         JZ    *+2                                                              
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',4),              X        
               ('LD_HEXDQ',IODA),(L'IODA,0)                                     
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
*================================================================               
* ROUTINE TO VALIDATE AND BUILD REVLINE/AVAIL RECORD                            
*================================================================               
                                                                                
VALRLINE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVNTELEM,SVNTELEM                                                
         XC    QLINE(QLINEX-QLINE),QLINE    CLEAR LINE VALUES                   
         MVI   QENDLINE,C'N'        SET LINE NOT ENDED                          
*                                                                               
         CLI   QACTION,0           TEST ACTION GIVEN                            
         BE    *+10                                                             
         MVC   SVACTION,QACTION                                                 
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
         CLI   QMEDX,0                                                          
         BE    VALRL10                                                          
         MVC   SVBAGYMD,QMEDX                                                   
         MVC   SVMED,QMEDA                                                      
*                                                                               
VALRL10  OC    QCLTX,QCLTX                                                      
         BZ    *+10                                                             
         MVC   SVBCLT,QCLTX                                                     
*                                                                               
         CLI   SVBAGYMD,0          TEST MEDIA KNOWN                             
         BE    VALERR02                                                         
         OC    SVBCLT,SVBCLT       TEST CLIENT KNOWN                            
         BZ    VALERR04                                                         
*                                                                               
         L     R1,ACLTREC          TEST CLIENT RECORD IS AROUND                 
         CLC   SVBAGYMD(3),CKEYAM-CLTHDR(R1)                                    
         BE    VALRL12                                                          
*                                                                               
         MVC   QMEDX,SVBAGYMD      NO - GET CLIENT RECORD                       
         MVC   QCLTX,SVBCLT                                                     
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   VALERR03                                                         
                                                                                
VALRL12  CLC   LP_VRSN,=AL1(4,6,0,50)  VERSION# >= V4.6.0.50?                   
         JNL   VALRL15                  YES                                     
*                                                                               
         MVC   FULL,=C'POL'        READ POL PRODUCT                             
         BRAS  RE,GTESTREC                                                      
         USING ESTHDR,RE                                                        
         L     RE,AESTREC                                                       
         OC    ENONTDMS(L'ENONTDMS*20),ENONTDMS HAVE CS DEMOS?                  
         JNZ   VALERR05                          YES, SEND AN ERROR             
         DROP  RE                                                               
*                                                                               
NEW      USING DRVKEY,SVKEY        BUILD NEW BUY KEY FROM INPUT DATA            
*                                                                               
VALRL15  OC    QRSEQ,QRSEQ         DID WE JUST ADD THE SHEET?                   
         BNZ   VALRL20                                                          
         MVC   QRSEQ,NEW.DRVKREVS  SAVE THE SHEET SEQ#                          
         XC    QRSEQ,EFFS            AND COMPLIMENT IT                          
*                                                                               
VALRL20  XC    NEW.DRVKEY,NEW.DRVKEY                                            
*                                                                               
         MVI   NEW.DRVKTYP,DRVKTYPQ                                             
         MVI   NEW.DRVKSUB,DRVKSUBQ   X'10' - REVISION SUBTYPE                  
         CLC   MAPNUM,=AL2(I#SDRLKU)  REVLINE UPLOAD?                           
         BE    VALRL25                                                          
         MVI   NEW.DRVKSUB,DWKKSUBQ   X'11' - AVAIL SUBTYPE                     
         CLC   MAPNUM,=AL2(I#SDAVKU)  AVAIL UPLOAD?                             
         JNE   *+2                                                              
*                                                                               
VALRL25  MVC   NEW.DRVKAM,SVBAGYMD                                              
         MVC   NEW.DRVKCLT,SVBCLT                                               
         CLC   QPRD,=C'AAA'        DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         MVC   NEW.DRVKPRD,QPRD                                                 
         MVC   NEW.DRVKEST,QBEST                                                
         MVC   NEW.DRVKMKT,QMKT                                                 
         MVC   NEW.DRVKREVS,QRSEQ                                               
         XC    NEW.DRVKREVS,EFFS                                                
         MVC   NEW.DRVKREVL,QLSEQ                                               
         XC    NEW.DRVKREVL,EFFS                                                
*                                                                               
         OC    QLSEQ,QLSEQ                                                      
         BNZ   VALRL30                                                          
         CLI   QACTION,QACTADD                                                  
         JNE   *+2                 MUST HAVE SEQ NUMBER IF NOT ADD              
*                                                                               
VALRL30  CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         BNE   VALRL55                                                          
*                                                                               
         OC    QLSEQ,QLSEQ                                                      
         JNZ   *+2                 DON'T WANT SEQ NUMBER ON ADD                 
*                                                                               
         L     R0,AREVREC          INITIALIZE NEW RECORD                        
         LHI   R1,IO5LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VALRL35  MVC   NEW.DRVKREVL,=X'000001'   READ FOR ANY LINES                     
*                                                                               
         MVC   IOKEY(L'DRVKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOXSPDIR+B#REVREC'                   
*                                                                               
         CLC   IOKEY(DRVKREVL-DRVKEY),IOKEYSAV                                  
         BE    VALRL40                                                          
         MVC   QLSEQ,=X'FFFFFE'    1 COMPLEMENTED                               
         B     VALRL50                                                          
*                                                                               
VALRL40  MVC   QLSEQ,IOKEY+(DRVKREVL-DRVKEY)                                    
         SR    R1,R1                                                            
         ICM   R1,7,QLSEQ                                                       
         AHI   R1,-1               SUB 1 FOR NEXT SEQ# - SINCE COMP             
         C     R1,=F'1'                                                         
         JNH   *+2                 OUT OF SEQ NUMS!  IMPOSSIBLE!                
         STCM  R1,7,QLSEQ                                                       
*                                                                               
VALRL50  MVC   NEW.DRVKREVL,QLSEQ  SET IN KEY FOR ADD                           
*                                                                               
         L     R1,AREVREC          AND SET NEW KEY                              
         XC    0(DRVEL-DRVRECD,R1),0(R1)                                        
         MVC   0(L'DRVKEY,R1),SVKEY                                             
         J     EXITY                                                            
*                                                                               
*        NOT AN ADD - FOR CHANGE OR DELETE                                      
*                                                                               
VALRL55  MVC   IOKEY(L'DRVKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUP+IOXSPDIR+B#REVREC'                    
         BNE   VALERR10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREC'                   
         BNE   VALERR11                                                         
*                                                                               
* COMPUTE CHECKSUM FOR RECORD                                                   
         CLC   QCHKSUM,=F'1'       SO YOU CAN GET AROUND IN =LINK               
         BE    VALRL60                                                          
         L     RE,AREVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         BO    *-4                                                              
         STCM  R0,15,FULL                                                       
         CLC   FULL,QCHKSUM                                                     
         BNE   VALERR35                                                         
*                                                                               
VALRL60  CLI   QACTION,QACTCHA     TEST CHANGE                                  
         BNE   VALRL70                                                          
*                                                                               
* CHECK IF RECORD ALREADY TRANSFERRED?                                          
         USING DRVRECD,R2                                                       
         L     R2,AREVREC                                                       
         TM    DRVRST1,DRVRSXFR    TRASNFERRED? X'40' IN REC+36                 
         BZ    VALRL70             NO                                           
         MVC   SVKEYREP,0(R2)      YES, SAVE ORIG KEY TO UPDATE LATER           
         MVI   SVACTION,QACTREP    CHANGE ACTION TO REPLACE                     
*                                                                               
         MVI   ELCDLO,RLDELQ       DELETE EVERYTHING BTWN X"11"                 
         MVI   ELCDHI,RLAELQ-1     AND "X'90"                                   
         BRAS  RE,DELEL            LETS DELETE IT                               
         B     VALRL35                                                          
         DROP  R2                                                               
*                                                                               
VALRL70  CLI   QACTION,QACTDEL     TEST DELETE                                  
         BE    VALRL80                                                          
*                                                                               
*        CLEAR OUT ALL SPOT ELEMS ON CHANGE                                     
*                                                                               
         MVI   ELCDLO,RLSELQ       NOW DELETE THE OLD LINE SPOTS ELEM           
         MVI   ELCDHI,RLSELQ       X'31'                                        
         BRAS  RE,DELEL                                                         
         J     EXIT                                                             
*                                                                               
         USING DRVRECD,R2                                                       
VALRL80  LA    R2,IOKEY                                                         
         OI    DRVKST0,DRVKSDEL    X'80' SET DELETED FLAG                       
         L     R2,AREVREC                                                       
         OI    DRVRST0,DRVRSDEL    X'80' SET RECORD DELETED                     
         BRAS  RE,VALRLWRT                                                      
         J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
VALRLWRT LR    R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOXSPDIR+B#REVREC'                     
         JNE   *+2                                                              
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREC'                     
         JNE   *+2                                                              
         LR    RE,R0                                                            
         BR    RE                                                               
*================================================                               
* LINE  DATA                                                                    
*================================================                               
                                                                                
VLDATA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLDELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLDELQ       X'11'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RLDELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLDEL,RLDELQ        X'11'                                        
         MVI   RLDLEN,RLDLENQ                                                   
*                                                                               
         MVC   RLDBLINE,QLBLINE                                                 
         CLI   QLSTA,0                                                          
         JE    *+10                                                             
         MVC   RLDSTA,QLSTA                                                     
         CLI   QLBYR,0                                                          
         JE    *+10                                                             
         MVC   RLDBYR,QLBYR                                                     
         MVC   RLDSTDT,QLSTDT                                                   
         MVC   RLDENDT,QLENDT                                                   
         MVC   RLDDAYS,QLDAYS                                                   
         MVC   RLDSTDAY,QLSTDAY                                                 
         MVC   RLDSTIME,QLSTIME                                                 
         MVC   RLDETIME,QLETIME                                                 
         MVC   RLDSLN,QLSLN                                                     
         MVC   RLDNPW,QLNPW                                                     
         MVC   RLDDAYPT,QLDAYPT                                                 
         MVC   RLDSDYPT,QLSDYPT                                                 
         CLI   QLPROG,0                                                         
         JE    *+10                                                             
         MVC   RLDPROG,QLPROG                                                   
*                                                                               
         MVC   RLDADJ,QLADJ                                                     
*&&DO                                                                           
         CLI   QLADJ+1,C' '        THIS CODE COMMENTED OUT AS                   
         BL    VLDATA05             TRNSADJ DID THIS FOR YOU                    
         PACK  RLDADJ,QLADJ                                                     
         NI    QLADJ,X'F0'                                                      
         MVC   BYTE,QLADJ+1                                                     
         NI    BYTE,X'0F'                                                       
         OC    RLDADJ,BYTE                                                      
*&&                                                                             
VLDATA05 MVC   RLDCOST,QLCOST                                                   
         CLI   QLREP,0                                                          
         JE    *+10                                                             
         MVC   RLDREP,QLREP                                                     
         CLC   QLPRD,=C'AAA'       DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         CLI   QLPRD,0                                                          
         JE    *+10                                                             
         MVC   RLDPRD,QLPRD                                                     
         CLC   QLPRD2,=C'AAA'      DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         CLI   QLPRD2,0                                                         
         JE    *+10                                                             
         MVC   RLDPRD2,QLPRD2                                                   
         MVC   RLDPRDLN,QLPRD2L                                                 
         CLI   QLMGCD,0                                                         
         JE    *+10                                                             
         MVC   RLDMGCD,QLMGCD                                                   
         CLI   QLBYID,0                                                         
         JE    *+10                                                             
         MVC   RLDBYID,QLBYID                                                   
         CLI   QLREASN,0                                                        
         JE    *+10                                                             
         MVC   RLDREASN,QLREASN                                                 
         MVC   RLDTAX,QLTAX                                                     
         ICM   RF,15,QALPURP       TEST PURPOSE CODE CHANGE?                    
         JZ    *+10                                                             
         MVC   RLDPURP,LW_DATA1-LW_D(RF)  YES, SAVE TO RECORD                   
         MVC   RLDHISQ#,QLHISQ#                                                 
*                                                                               
         ICM   R2,15,QALCOST2                                                   
         BZ    VLDATA10                                                         
         USING LW_D,R2                                                          
         XC    RLCOST2,RLCOST2                                                  
         MVI   RLCOST2+5,X'FF'                                                  
         SR    RF,RF                                                            
         ICM   RF,3,LW_LN                                                       
         AHI   RF,-(LW_LN1Q)                                                    
         CLC   =C'NOCOS2',LW_DATA1   NO COS2?                                   
         BE    VLDATA10                                                         
         GOTOR VCASHVAL,DMCB,(C'0',LW_DATA1),(RF)                               
         CLI   0(R1),FF                                                         
         JE    *+2                 INVALID                                      
         ZAP   RLCOST2,4(8,R1)                                                  
         CP    RLCOST2,4(8,R1)                                                  
         JNE   *+2                 MAKE SURE IT STILL MATCHES                   
         DROP  R2                                                               
*                                                                               
VLDATA10 ICM   R2,15,QALCIND                                                    
         BZ    VLDATA20                                                         
         USING LW_D,R2                                                          
         MVC   RLDCIND,LW_DATA1                                                 
         CLC   =C'NORATETYPE',LW_DATA1   NO RATE TYPE?                          
         BNE   *+8                                                              
         MVI   RLDCIND,RLDNORTP                                                 
         DROP  R2                                                               
*                                                                               
VLDATA20 MVC   RLDFDALY,QLDAILY                                                 
         MVC   RLDFPSCD,QLPRVSHD                                                
         MVC   RLDFDELX,QLDELBUY                                                
         MVC   RLCOS2TP,QLC2IND                                                 
         MVC   RLLINEID,QLLINEID                                                
         MVC   RLDDLSPT,QLDELSPT                                                
         MVC   RLC2RTYP,QLC2RTYP                                                
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL  COMMENTS                                                       
*================================================                               
*                                                                               
VLCOM    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLCMELQ      REMOVE ALL COMMENTS X'33'                    
         MVI   ELCDHI,RLCMELQ                                                   
         BRAS  RE,DELEL                                                         
*                                                                               
VALCOM4  LA    R4,QACOMM1                                                       
                                                                                
         USING RLCMELD,R3                                                       
VALCOM10 LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,RLCMELQ        X'33'                                        
*                                                                               
         L     R2,0(R4)            POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         BNP   VALCOM12                                                         
         LA    RF,RLCMLENQ(RE)     ADD OVERHEAD                                 
         STC   RF,RLCMLEN          SET NEW ELEM LENGTH                          
*                                                                               
         MVC   RLCMNUM,LW_CODE+1   SET COMMENT NUMBER                           
*                                                                               
         BCTR  RE,0                                                             
         MVC   RLCMTXT(0),LW_DATA1                                              
         EX    RE,*-6                                                           
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
VALCOM12 LA    R4,4(R4)            NEXT COMMENT AREA                            
         LA    R0,QACOMM5                                                       
         CR    R4,R0                                                            
         BNH   VALCOM10                                                         
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL ESTIMATE DEMOS                                                  
*================================================                               
                                                                                
VLEDEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLNEDELQ     NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLEDELQ      X'22' & X'23'                                
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RLNEDELD,R3                                                      
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLNEDEL,RLNEDELQ    X'22'                                        
         MVC   RLNEDBK,QLBOOK      BOOK                                         
         MVC   RLNEDBT,QLBKTYP     BOOKTYPE                                     
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QALEDEM       DO WE HAVE ANY DEMOS TO SAVE?                
         JZ    VLEDM60             NO                                           
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VEDD,R2                                                          
*                                                                               
VLEDM10  LA    RF,ELEM                                                          
         LR    RE,R3                                                            
         SR    RE,RF                                                            
         CHI   RE,RLNEDLN2Q*10     SAVING MORE THAN 10 DEMOS?                   
         BNH   VLEDM20             NO, NO NEED TO SPLIT                         
         AHI   RE,RLNEDLNQ         ADD OVERHEAD                                 
         STC   RE,ELEM+1           SAVE LENGTH                                  
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
         LA    R3,ELEM             RESET R3                                     
         XC    ELEM,ELEM           CLEAR ELEM                                   
         MVI   RLNEDEL,RLNEDELQ    X'22'                                        
         MVC   RLNEDBK,QLBOOK      BOOK                                         
         MVC   RLNEDBT,QLBKTYP     BOOKTYPE                                     
*                                                                               
VLEDM20  GOTOR VALDCD,DMCB,VEDCAT,(0,L'VEDCAT),RLNEDCAT                         
         JNE   EXITN                                                            
*                                                                               
         GOTOR BLDNTEL,DMCB,('RLNDLENQ',RLNEDCAT),('RLNDLEN2',VEDCAT), +        
               ('RLNDFDLK',VEDNTFLG)                                            
*                                                                               
********************                                                            
* PROCESS RAW DEMO VALUE                                                        
********************                                                            
         OC    VEDRAW,VEDRAW       DEMO RAW VALUE ZERO?                         
         JZ    VLEDM30              YES, SKIP TESTS                             
*                                                                               
         CLI   VEDPREC,0           DID PC SEND PRECISION?                       
         JNE   VLEDM27              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLEDM25              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VLEDM25                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VEDPREC,1           SET DEFAULT 1-DEC PREC                       
         CLI   VEDCAT,C'R'         IF RATING OR                                 
         JE    *+12                                                             
         CLI   VEDCAT,C'E'         EXTENDED THEN                                
         JNE   VLEDM27                                                          
VLEDM25  MVI   VEDPREC,2           SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT RADIO 2-DEC PREC RATING WITH 100TH VALUE                              
*                                                                               
VLEDM27  CLI   VEDPREC,2           HAVE 2-DEC PREC VALUE?                       
         JNE   VLEDM30                                                          
*                                                                               
         CLI   SVMED,C'R'          MEDIA R?                                     
         JNE   VLEDM30              NO, SKIP TEST                               
         CLI   VEDCAT,C'R'         RATING OR                                    
         JE    *+12                 YES                                         
         CLI   VEDCAT,C'E'         EXTENDED?                                    
         JNE   VLEDM30              NO, SKIP TEST                               
*                                                                               
*** DUE TO SBTK AUTO-ADJUST, RADIO DEMO VALUES SAVED WITH 2-DEC PREC            
*** DON'T PERFORM THIS CHECK UNTIL SBTK V4.7.0.37                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLEDM30              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# < V4.7.0.37?                     
         JL    VLEDM30                 YES, ALLOW RADIO 2-DEC PREC              
*                                                                               
         GOTOR T2DECP,DMCB,(L'VEDRAW,VEDRAW)                                    
         JNE   VALERR12                                                         
*                                                                               
********************                                                            
* PROCESS SHR DEMO VALUE                                                        
********************                                                            
VLEDM30  OC    VEDSHR,VEDSHR       SHR VALUE ZERO?                              
         JZ    VLEDM40              YES, SKIP TESTS                             
*                                                                               
         CLI   VEDSPREC,0          DID PC SEND PRECISION?                       
         JNE   VLEDM37              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLEDM35              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VLEDM35                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VEDSPREC,1          SET DEFAULT 1-DEC PREC                       
         CLI   VEDCAT,C'R'         IF RATING OR                                 
         JE    VLEDM35                                                          
         CLI   VEDCAT,C'E'         EXTENDED THEN                                
         JNE   VLEDM37                                                          
VLEDM35  MVI   VEDSPREC,2          SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT SHARE 2-DEC PREC IMPRESSION WITH 100TH VALUE                          
*                                                                               
VLEDM37  DS    0H                                                               
*&&DO                                                                           
         CLI   VEDSPREC,2          HAVE 2-DEC PREC VALUE?                       
         JNE   VLEDM40              NO, SKIP 2-DEC PREC TEST                    
         GOTOR T2DECP,DMCB,(L'VEDSHR,VEDSHR)                                    
         JNE   VALERR13                                                         
*&&                                                                             
********************                                                            
* PROCESS PUT DEMO VALUE                                                        
********************                                                            
VLEDM40  OC    VEDPUT,VEDPUT       PUT VALUE ZERO?                              
         JZ    VLEDM50              YES, SKIP TESTS                             
*                                                                               
         CLI   VEDPPREC,0          DID PC SEND PRECISION?                       
         JNE   VLEDM47              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLEDM45              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VLEDM45                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VEDPPREC,1          SET DEFAULT 1-DEC PREC                       
         CLI   VEDCAT,C'R'         IF RATING OR                                 
         JE    VLEDM45                                                          
         CLI   VEDCAT,C'E'         EXTENDED THEN                                
         JNE   VLEDM47                                                          
VLEDM45  MVI   VEDPPREC,2          SET DEFAULT 1-DEC PREC                       
*                                                                               
* PREVENT PUT 2-DEC PREC IMPRESSION WITH 100TH VALUE                            
*                                                                               
VLEDM47  DS    0H                                                               
*&&DO                                                                           
         CLI   VEDPPREC,2          HAVE 2-DEC PREC VALUE?                       
         JNE   VLEDM50              NO                                          
         GOTOR T2DECP,DMCB,(L'VEDPUT,VEDPUT)                                    
         JNE   VALERR13                                                         
*&&                                                                             
VLEDM50  MVI   RLNEDSVI,100             ??                                      
         MVC   RLNEDFP,VEDPREC     DEMO PRECISION                               
         OC    RLNEDFP,VEDFLG      DEMO FLAG                                    
         MVC   RLNEDRAW,VEDRAW     DEMO VALUE                                   
         MVC   RLNEDSFP,VEDSPREC   SHR PRECISIO                                 
         OC    RLNEDSFP,VEDSFLG    SHR FLAG                                     
         MVC   RLNEDSHR,VEDSHR     SHR VALUE                                    
         MVC   RLNEDPFP,VEDPPREC   PUT PRECISION                                
         OC    RLNEDPFP,VEDPFLG    PUT FLAGS                                    
         MVC   RLNEDPUT,VEDPUT     PUT VALUE                                    
*                                                                               
         AHI   R2,VEDLNQ           NEXT ARRAY ENTRY                             
         AHI   R3,RLNEDLN2Q        NEXT DEMO IN ELEMENT                         
         BCT   R0,VLEDM10                                                       
*                                                                               
VLEDM60  LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RLNEDLNQ         ADD THE OVERHEAD                             
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
*                                                                               
VLEDMX   J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
VEDD     DSECT                                                                  
VEDCAT   DS    CL8                                                              
VEDRAW   DS    XL(L'RLNEDRAW)                                                   
VEDFLG   DS    XL(L'RLNEDFP)                                                    
VEDPREC  DS    XL(L'RLNEDFP)                                                    
VEDSHR   DS    XL(L'RLNEDSHR)                                                   
VEDSFLG  DS    XL(L'RLNEDSFP)                                                   
VEDSPREC DS    XL(L'RLNEDSFP)                                                   
VEDPUT   DS    XL(L'RLNEDPUT)                                                   
VEDPFLG  DS    XL(L'RLNEDPFP)                                                   
VEDPPREC DS    XL(L'RLNEDPFP)                                                   
VEDNTFLG DS    XL(L'RLNDDFLG)                                                   
VEDLNQ   EQU   *-VEDD                                                           
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL ESTIMATE SPILL DEMOS                                            
*================================================                               
                                                                                
VLSDEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         CLI   QSDMFRST,0                                                       
         BNE   VLSDM10                                                          
         MVI   ELCDLO,RLNLDELQ     NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLLDELQ      X'26' & X'27'                                
         BRAS  RE,DELEL                                                         
         MVI   QSDMFRST,C'N'                                                    
*                                                                               
         USING RLNLDELD,R3                                                      
VLSDM10  LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLNLDEL,RLNLDELQ    X'26'                                        
*                                                                               
         MVC   RLNLDMKT,QLDMKT                                                  
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QALSDEM                                                    
         JZ    EXIT                                                             
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VSDD,R2                                                          
*                                                                               
VLSDM20  GOTOR VALDCD,DMCB,VSDCAT,(0,L'VSDCAT),RLNLDCAT                         
         JNE   EXITN                                                            
*                                                                               
         GOTOR BLDNTEL,DMCB,('RLNDLENQ',RLNLDCAT),('RLNDLEN2',VSDCAT), +        
               ('RLNDFSDL',VSDNTFLG)                                            
*                                                                               
         OC    VSDRAW,VSDRAW       DEMO VALUE ZERO?                             
         JZ    VLSDM30              YES, SKIP TEST                              
*                                                                               
         CLI   VSDPREC,0           DID PC SEND PRECISION?                       
         JNE   VLSDM27              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLSDM25              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VLSDM25                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VSDPREC,1           SET DEFAULT 1-DEC PREC                       
         CLI   VSDCAT,C'R'         IF RATING OR                                 
         JE    VLSDM25                                                          
         CLI   VSDCAT,C'E'         EXTENDED THEN                                
         JNE   VLSDM27                                                          
VLSDM25  MVI   VSDPREC,2           SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT RADIO 2-DEC PREC RATING WITH 100TH VALUE                              
*                                                                               
VLSDM27  CLI   VSDPREC,2           HAVE 2-DEC PREC VALUE?                       
         JNE   VLSDM30              NO                                          
*                                                                               
         CLI   SVMED,C'R'          MEDIA R?                                     
         JNE   VLSDM30              NO, SKIP TEST                               
         CLI   VSDCAT,C'R'         RATING OR                                    
         JE    *+12                 YES                                         
         CLI   VSDCAT,C'E'         EXTENDED?                                    
         JNE   VLSDM30              NO, SKIP TEST                               
*                                                                               
*** DUE TO SBTK AUTO-ADJUST, RADIO DEMO VALUES SAVED WITH 2-DEC PREC            
*** DON'T PERFORM THIS CHECK UNTIL SBTK V4.7.0.37                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLSDM30              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# < V4.7.0.37?                     
         JL    VLSDM30                 YES, ALLOW RADIO 2-DEC PREC              
*                                                                               
         GOTOR T2DECP,DMCB,(L'VSDRAW,VSDRAW)                                    
         JNE   VALERR12                                                         
*                                                                               
VLSDM30  MVI   RLNLDSVI,100                                                     
         MVC   RLNLDRAW,VSDRAW     VALUE                                        
         MVC   RLNLDFP,VSDPREC     SET PRECISION                                
         OC    RLNLDFP,VSDFLG      SET FLAGS                                    
*                                                                               
         AHI   R2,VSDLNQ           NEXT ARRAY ENTRY                             
         AHI   R3,RLNLDLN2Q        NEXT DEMO IN ELEMENT                         
         BCT   R0,VLSDM20                                                       
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RLNLDLNQ                                                      
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
VSDD     DSECT                                                                  
VSDCAT   DS    CL8                                                              
VSDRAW   DS    XL(L'RLNLDRAW)                                                   
VSDFLG   DS    XL(L'RLNLDFP)                                                    
VSDPREC  DS    XL(L'RLNLDFP)                                                    
VSDNTFLG DS    XL(L'RLNDDFLG)                                                   
VSDLNQ   EQU   *-VSDD                                                           
*                                                                               
SVRDEF   CSECT                                                                  
*========                                                                       
* TEST IF DEMO HAS 2 DECIMAL PRECISION VALUE (NON ZERO 100TH)                   
* ON ENTRY : P1(0)     L'(PACKED DEMO VALUE)                                    
*          : P1(1-3)   A(PACKED DEMO VALUE)                                     
*          : P2(0)     DEMO TYPE - (R)ATING, (I)IMP, (E)XT, ETC                 
*                                                                               
* ON EXIT  : CC        EQ = NOT 2-DECIMAL PRECISION                             
*          :           NEQ = HAVE 2-DECIMAL PRECISION                           
*========                                                                       
T2DECP   DS    0H                                                               
         LLC   RF,0(R1)            TEST 2-DEC PRECISION VALUE                   
         L     R1,0(R1)                                                         
         LAY   R1,-1(RF,R1)                                                     
         CLI   0(R1),X'0C'         VALUE HAS 2 DECIMAL PRECISION?               
         BR    RE                                                               
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL UPGRADE FORMULA                                                 
*================================================                               
                                                                                
VLUPG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLUELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLUELQ       X'25'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RLUELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLUEL,RLUELQ        X'25'                                        
*                                                                               
         L     R2,QALUPG           POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN        # OF ENTRIES                                 
         JZ    EXIT                                                             
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VLUPGD,R2                                                        
*                                                                               
VLUPG10  MVC   RLUSTTDT,VLUPSTDT   UPGRADE EFF START DATE                       
         MVC   RLUENDDT,VLUPENDT   UPGRADE EFF END DATE                         
         MVC   RLUFORM(L'VLUPUFRM),VLUPUFRM SET UPGRADE FORMULA                 
         LA    RF,RLUFORM+L'VLUPUFRM                                            
*                                                                               
VLUPG20  CLI   0(RF),C' '          CLEAR SPACES                                 
         BH    *+8                                                              
         BCT   RF,VLUPG20                                                       
         LA    RF,1(RF)                                                         
         SR    RF,R3                                                            
         CHI   RF,RLULENQ          LESS THAN THE OVERHEAD?                      
         BNL   *+8                                                              
         LA    RF,RLULENQ          SET MINIMUM OVERHEAD                         
         STC   RF,RLULEN                                                        
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         JNE   EXITN                                                            
*                                                                               
         LA    R2,VLUPLENQ(R2)     BUMP TO NEXT ENTRY                           
         BCT   R4,VLUPG10                                                       
*                                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
VLUPGD   DSECT                                                                  
VLUPSTDT DS   XL(L'RLUSTTDT)                                                    
VLUPENDT DS   XL(L'RLUENDDT)                                                    
VLUPUFRM DS   CL150                                                             
VLUPLENQ EQU   *-VLUPGD                                                         
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL SPOTS                                                           
*================================================                               
                                                                                
VLSPOT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,QALSPOT          POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN        # OF ENTRIES                                 
         JZ    EXIT                                                             
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VSPD,R2                                                          
         LA    R3,ELEM                                                          
         USING RLSELD,R3                                                        
*                                                                               
VLS010   XC    ELEM,ELEM                                                        
         MVI   RLSEL,RLSELQ        X'31'                                        
         MVI   RLSLEN,RLSLENQ                                                   
         MVC   RLSDATE,QLSDATE                                                  
         MVC   RLSREF#,VSPREF#                                                  
         MVC   RLSNOWK,VSPNOWK                                                  
*                                                                               
         L     R1,AREVREC                                                       
         CLI   DRVKSUB-DRVKEY(R1),DWKKSUBQ  X'11' - WORKSHEET?                  
         BE    VLS020                       YES, PRD SHOULD NOT BE SET          
         CLC   VSPPRD,=C'AAA'      DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         CLI   VSPPRD,0                                                         
         JE    *+10                                                             
         MVC   RLSPRD,VSPPRD                                                    
         CLC   VSPPRD2,=C'AAA'     DO WE HAVE PRODUCT AAA?                      
         JE    VALERR19                                                         
         CLI   VSPPRD2,0                                                        
         JE    *+10                                                             
         MVC   RLSPRD2,VSPPRD2                                                  
*                                                                               
VLS020   MVC   RLSPRDLN,VSPPRDLN                                                
         MVC   RLSCOST,VSPCOST                                                  
         MVC   RLSXSCHD,VSPXSCHD                                                
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         JNE   EXITN                                                            
*                                                                               
         LA    R2,VSPLNQ(R2)       BUMP TO NEXT ENTRY                           
         BCT   R4,VLS010                                                        
*                                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
VSPD     DSECT                                                                  
VSPREF#  DS    XL(L'RLSREF#)                                                    
VSPNOWK  DS    XL(L'RLSNOWK)                                                    
VSPPRD   DS    XL(L'RLSPRD)                                                     
VSPPRD2  DS    XL(L'RLSPRD2)                                                    
VSPPRDLN DS    XL(L'RLSPRDLN)                                                   
VSPCOST  DS    XL(L'RLSCOST)                                                    
VSPXSCHD DS    XL(L'RLSXSCHD)                                                   
VSPLNQ   EQU   *-VSPD                                                           
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL EFFECTIVE RATES                                                 
*================================================                               
                                                                                
VLRATES  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLMELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLMELQ       X'15'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QALRATE                                                    
         JZ    EXIT                                                             
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         JZ    EXIT                                                             
*                                                                               
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VLRATED,R2                                                       
*                                                                               
         USING RLMELD,R3                                                        
VLRAT10  LA    R3,ELEM             START A NEW ELEM                             
         XC    ELEM,ELEM                                                        
         MVI   RLMELD,RLMELQ                                                    
*                                                                               
* SBTK WAS SUPPOSED TO LIMIT TO 14 ENTRIES, BUT DEFECT ALLOWS FOR MORE          
* NOW WE HAVE TO BREAK UP ELEMENT AFTER 18 ENTRIES                              
*                                                                               
         LA    RF,ELEM+18*RLMLENQ2 A(18 ENTRIES BEYOND START)                   
VLRAT20  CR    R3,RF               BEYOND MAX 18 ENTRIES                        
         BNH   VLRAT30              NO                                          
         MVI   ELEM+1,RLMLENQ+18*RLMLENQ2                                       
         BRAS  RE,ADDEL                                                         
         B     VLRAT10             GO BACK AND START A NEW ELEM                 
*                                                                               
VLRAT30  MVC   RLMSTTDT,VRTSTDT    START DATE                                   
         MVC   RLMENDDT,VRTENDT    END DATE                                     
         MVC   RLMRATE,VRTRATE     EFFECTIVE RATE                               
*                                                                               
         AHI   R2,VRTLENQ          NEXT ARRAY ENTRY                             
         AHI   R3,RLMLENQ2         NEXT EFF RATE IN ELEMENT                     
         BCT   R0,VLRAT20                                                       
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RLMLENQ          ADD OVERHEAD                                 
         STC   R3,ELEM+1           AND SET ELEM LENGTH                          
         BRAS  RE,ADDEL                                                         
*                                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
*                                                                               
VLRATED  DSECT                                                                  
VRTSTDT  DS    XL(L'RLMSTTDT)                                                   
VRTENDT  DS    XL(L'RLMENDDT)                                                   
VRTRATE  DS    XL(L'RLMRATE)                                                    
VRTLENQ  EQU   *-VLRATED                                                        
                                                                                
SVRDEF   CSECT                                                                  
*================================================                               
* REVLINE/AVAIL HIATUS PERIODS                                                  
*================================================                               
                                                                                
VLHIAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLHELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLHELQ       X'43'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RLHELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLHEL,RLHELQ        X'43'                                        
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QALHIAT                                                    
         JZ    EXIT                                                             
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VHIATD,R2                                                        
*                                                                               
VLHI10   MVC   RLHIAST,VLHIAST     START DATE                                   
         MVC   RLHIAEND,VLHIAEND   END DATE                                     
*                                                                               
         AHI   R2,VLHLNQ           NEXT ARRAY ENTRY                             
         AHI   R3,RLHLEN2          NEXT DEMO IN ELEMENT                         
         BCT   R0,VLHI10                                                        
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RLHLEN1                                                       
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
VHIATD   DSECT                                                                  
VLHIAST  DS    XL(L'RLHIAST)                                                    
VLHIAEND DS    XL(L'RLHIAEND)                                                   
VLHLNQ   EQU   *-VHIATD                                                         
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL ORBITS                                                          
*================================================                               
                                                                                
VLORB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* GET PRIMARY DEMO TYPE ON POL ESTIMATE                                         
*                                                                               
         MVI   SVESTPDM,0                                                       
         MVC   FULL,=C'POL'             READ POL PRODUCT                        
         BRAS  RE,GTESTREC                                                      
         L     RE,AESTREC                                                       
         MVC   SVESTPDM,EDEMLTYP-ESTHDR(RE) SAVE NSI DEMO TYPE                  
         CLI   EDEMLNUM-ESTHDR(RE),0    HAVE NON-TRAD DEMO?                     
         JNE   VLORB05                   NO                                     
         LLC   RF,EDEMLTYP-ESTHDR(RE)   GET INDEX                               
         LA    RE,ENONTDMS-ESTHDR(RE)   RE=A(START OF NT DEMO LIST)             
         SHI   RF,1                     SUB 1 FROM INDEX                        
         MHI   RF,L'ENONTDMS            MULTIPLE * LENGTH OF ENTRY              
         AR    RF,RE                    ADD OFFSET TO START OF LIST             
         LA    RE,20*L'ENONTDMS(RE)                                             
         CR    RF,RE                    BEYOND LIST?                            
         JNL   *+2                       SOMETHING WRONG                        
         CLC   0(L'ENONTDMS,RF),SPACES  HAVE VALID NT DEMO NAME?                
         JNH   *+2                        NO, SOMETHING WRONG                   
         MVC   SVESTPDM,0(RF)           SAVE COMSCORE DEMO TYPE                 
*                                                                               
VLORB05  L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLNOELQ      NOW DELETE THE OLD ELEMENTS                  
         MVI   ELCDHI,RLOELQ       X'52'/X'53'                                  
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RLNOELD,R3                                                       
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLNOEL,RLNOELQ      X'52'                                        
         MVI   RLNOLEN,RLNOLENQ                                                 
         MVC   RLNODEL,QLODEL                                                   
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QALORB                                                     
         JNZ   VLORB10                                                          
         CLI   RLNODEL,0                                                        
         JE    EXIT                                                             
         J     VLORB40                                                          
*                                                                               
         USING LW_D,R2                                                          
VLORB10  SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VORBD,R2                                                         
*                                                                               
VLORB20  DS    0H                                                               
         OC    VORDEM,VORDEM       DEMO VALUE ZERO?                             
         JZ    VLORB30              YES, SKIP TEST                              
*                                                                               
         CLI   VORDPREC,0          DID PC SEND PRECISION?                       
         JNE   VLORB27              YES                                         
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLORB25              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# >= V4.7.0.37?                    
         JNL   VLORB25                 YES, DEFAULT 2-DEC PREC                  
*                                                                               
         MVI   VORDPREC,1          SET DEFAULT 1-DEC PREC                       
         CLI   SVESTPDM,C'R'       IF RATING OR                                 
         JE    VLORB25                                                          
         CLI   SVESTPDM,C'E'       EXTENDED THEN                                
         JNE   VLORB27                                                          
VLORB25  MVI   VORDPREC,2          SET DEFAULT 2-DEC PREC                       
*                                                                               
* PREVENT RADIO 2-DEC PREC RATING WITH 100TH VALUE                              
*                                                                               
VLORB27  CLI   VORDPREC,2          HAVE 2-DEC PREC VALUE?                       
         JNE   VLORB30              NO                                          
*                                                                               
         CLI   SVMED,C'R'          MEDIA R?                                     
         JNE   VLORB30              NO, SKIP TEST                               
         CLI   SVESTPDM,C'R'       RATING OR                                    
         JE    *+12                 YES                                         
         CLI   SVESTPDM,C'E'       EXTENDED?                                    
         JNE   VLORB30              NO, SKIP TEST                               
*                                                                               
*** DUE TO SBTK AUTO-ADJUST, RADIO DEMO VALUES SAVED WITH 2-DEC PREC            
*** DON'T PERFORM THIS CHECK UNTIL SBTK V4.7.0.37                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION CONTROL?                        
         JZ    VLORB30              NO, DEFAULT 2-DEC PREC                      
         CLC   LP_VRSN,=AL1(4,7,0,37) VERSION# < V4.7.0.37?                     
         JL    VLORB30                 YES, ALLOW RADIO 2-DEC PREC              
*                                                                               
         GOTOR T2DECP,DMCB,(L'VORDEM,VORDEM)                                    
         JNE   VALERR12                                                         
*                                                                               
VLORB30  MVC   RLNODAY,VORDAY      DAY                                          
         MVC   RLNOSTIM,VORSTIM    START TIME                                   
         MVC   RLNOETIM,VORETIM    END TIME                                     
         MVC   RLNODESC,VORDESC    DESC/PROGRAM                                 
         MVC   RLNODEM,VORDEM      DEMO - ALWAYS ESTIMATED                      
         MVC   RLNOFP,VORDPREC     DEMO PRECISION                               
*                                                                               
         AHI   R2,VODLNQ           NEXT ARRAY ENTRY                             
         AHI   R3,RLNOLN2Q         NEXT DEMO IN ELEMENT                         
         BCT   R0,VLORB20                                                       
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RLOLENQ                                                       
         STC   R3,ELEM+1                                                        
*                                                                               
VLORB40  BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
VORBD    DSECT                                                                  
VORDAY   DS    XL(L'RLNODAY)                                                    
VORSTIM  DS    XL(L'RLNOSTIM)                                                   
VORETIM  DS    XL(L'RLNOETIM)                                                   
VORDESC  DS    XL(L'RLNODESC)                                                   
VORDEM   DS    XL(L'RLNODEM)                                                    
VORDPREC DS    XL(L'RLNOFP)                                                     
VODLNQ   EQU   *-VORBD                                                          
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL PACKAGE INFO                                                    
*================================================                               
                                                                                
VLPKG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLPELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLPELQ       X'61'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         USING RLPELD,R3                                                        
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLPEL,RLPELQ        X'61'                                        
*                                                                               
         MVC   RLPIND,QLPIND                                                    
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QALPKG                                                     
         JZ    VLPKG20                                                          
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
*                                                                               
VLPKG10  MVC   RLPLINES,0(R2)      MOVE PACKAGE LINE                            
*                                                                               
         AHI   R2,RLPLN2Q          NEXT ARRAY ENTRY                             
         AHI   R3,RLPLN2Q          NEXT DEMO IN ELEMENT                         
         BCT   R0,VLPKG10                                                       
*                                                                               
VLPKG20  LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,RLPLENQ                                                       
         STC   R3,ELEM+1                                                        
*                                                                               
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL AUTOMATED-AVAILS UUID                                           
*================================================                               
*                                                                               
VLAAUUID NTR1  BASE=*,LABEL=*                                                   
****  COMMENT OUT, AS THEY MAY WANT TO SET ON PRE-EXISTING AVAIL                
****     CLI   SVACTION,QACTADD                                                 
****     JNE   EXITN                                                            
****  COMMENT OUT, AS THEY MAY WANT TO SET ON PRE-EXISTING AVAIL                
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLAAELQ      CHECK IF X'19' ALREADY PRESENT               
         MVI   ELCDHI,RLAAELQ                                                   
         LA    R3,DRVEL                                                         
         BRAS  RE,NEXTEL           ALREADY HAVE UUID?                           
         BE    EXITN                YES, SOMETHING WRONG                        
*                                                                               
                                                                                
         USING RLAUTAVD,R3                                                      
VLAAU010 DS    0H                                                               
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RLAAEL,RLAAELQ      X'19'                                        
*                                                                               
         L     R2,QALAAUID                                                      
         USING LW_D,R2                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         JNP   EXITN                                                            
         LA    RF,RLAALENQ(RE)     ADD OVERHEAD                                 
         STC   RF,RLAALEN          SET NEW ELEM LENGTH                          
*                                                                               
         BCTR  RE,0                                                             
         MVC   RLAAUUID(0),LW_DATA1                                             
         EX    RE,*-6                                                           
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
*&&DO                                                                           
VLAAU020 LA    R4,4(R4)            NEXT UUID AREA                               
         LA    R0,QACOMM5                                                       
         CR    R4,R0                                                            
         BNH   VLAAU010                                                         
*&&                                                                             
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================================================                               
* REVLINE/AVAIL NOTES                                                           
*================================================                               
*                                                                               
VLNOTE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RLNTELQ      REMOVE ALL COMMENTS X'35'                    
         MVI   ELCDHI,RLNTELQ                                                   
         BRAS  RE,DELEL                                                         
*                                                                               
         LA    R3,ELEM                                                          
         USING RLNTELD,R3                                                       
         XC    ELEM,ELEM                                                        
         MVI   ELEM,RLNTELQ        X'33'                                        
*                                                                               
         L     R2,QANOTE           POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-(LW_LN1Q)       DATA LENGTH                                  
         BNP   VALNOTX                                                          
         LA    RF,RLNTLENQ(RE)     ADD OVERHEAD                                 
         STC   RF,RLNTLEN          SET NEW ELEM LENGTH                          
         CHI   RF,255              DOES TOTAL LENGTH EXCEED 255                 
         BNH   *+8                 -NO                                          
         MVI   RLNTLEN,255         -YES, TRUNCATE IT                            
         BCTR  RE,0                                                             
         MVC   RLNTTXT(0),LW_DATA1                                              
         EX    RE,*-6                                                           
         BRAS  RE,ADDEL                                                         
*                                                                               
VALNOTX  J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*===============================================================                
* WRITE REVLINE/AVAIL RECORD TO FILE                                            
*===============================================================                
                                                                                
ENDLINE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
         MVC   QSEQ#,DRVKREVL      TO SEND BACK TO PC                           
         XC    QSEQ#,EFFS          UNCOMPLEMENT FOR THEM                        
*                                                                               
         MVI   QENDLINE,C'Y'        SET FIRST REVISION ENDED                    
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
* CHECK IF NEW NON-TRAD ELEMENT                                                 
*                                                                               
         CLI   SVNTELEM+1,0        DO WE HAVE SOMETHING?                        
         JE    ENDLIN05            -NO                                          
*                                                                               
         L     R3,AREVREC          POINT TO END OF REC                          
         MVI   ELCDLO,RLNDELQ      NOW FIND THE OLD ELEMENT                     
         MVI   ELCDHI,RLNDELQ      X'38'                                        
         LA    R3,DRVEL                                                         
         BRAS  RE,NEXTEL           DO WE HAVE OLD NON-TRAD ELEM?                
         JNE   ENDLIN04             NO, THEN JUST ADD NEW ONE                   
         XC    ELEM,ELEM                                                        
         LLC   RE,1(R3)             YES, LETS SAVE IT IN ELEM                   
         BCTR  RE,0                                                             
         MVC   ELEM(0),0(R3)                                                    
         EX    RE,*-6                                                           
*                                                                               
         MVI   ELCDLO,RLNDELQ      NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLNDELQ      X'38'                                        
         BRAS  RE,DELEL                                                         
*                                                                               
         LA    RF,ELEM+2                                                        
         LA    RE,SVNTELEM+2                                                    
         LLC   R1,SVNTELEM+1                                                    
         LA    R1,SVNTELEM(R1)                                                  
*                                                                               
ENDLIN02 CR    RE,R1               END OF SVNTELEM? FINISHED?                   
         JNL   ENDLIN04             YES                                         
         OC    0(RLNDLEN2,RE),0(RE) NO, SVNTELEM MISSING ENTRY?                 
         JNZ   *+16                  NO, DON'T COPY FROM OLD                    
         MVC   0(RLNDLEN2,RE),0(RF)  YES, COPY FROM OLD                         
         MVC   RLNDLEN2(1,RE),RLNDLEN2(RF)                                      
         LA    RF,RLNDLEN2(RF)       BUMP TO NEXT IN ELEM                       
         LA    RE,RLNDLEN2(RE)       BUMP TO NEXT IN SVNTELEM                   
         J     ENDLIN02                                                         
*                                                                               
ENDLIN04 MVC   ELEM,SVNTELEM       -YES, MOVE TO ELEM                           
         MVI   ELEM,RLNDELQ                                                     
         BRAS  RE,ADDEL              AND ADD IT                                 
*                                                                               
ENDLIN05 BRAS  RE,GETDTTM          ITS IN WORK, DON'T BLOW IT AWAY              
*                                                                               
         CLI   SVACTION,QACTADD                                                 
         BNE   ENDLIN10                                                         
*                                                                               
* INSERT ACTIVITY ELEMENT                                                       
*                                                                               
         USING RLAELD,R3                                                        
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   RLAEL,RLAELQ        X'91'                                        
         MVI   RLALEN,RLALENQ                                                   
         L     R1,LP_ASECD                                                      
         MVC   RLAAPID,SECOPASS-SECD(R1)                                        
         MVC   RLAADD,WORK                                                      
         MVC   RLAATIME,WORK+2                                                  
         DROP  R3                                                               
*                                                                               
         L     R3,AREVREC          POINT TO END OF REC                          
         SR    R0,R0                                                            
         ICM   R0,3,32(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOXSPFIL+B#REVREC'                     
         BE    ENDLINX                                                          
         DC    H'0'                                                             
*                                                                               
ENDLIN10 DS    0H                                                               
         MVI   ELCDLO,RLAELQ       NOW FIND THE OLD ELEMENT                     
         MVI   ELCDHI,RLAELQ       X'91'                                        
         LA    R3,DRVEL                                                         
         BRAS  RE,NEXTEL           DID WE FIND IT?                              
         JNE   *+2                  NO                                          
         USING RLAEL,R3                                                         
         L     R1,LP_ASECD         AND UPDATE IT                                
         MVC   RLACPID,SECOPASS-SECD(R1)                                        
         MVC   RLACHG,WORK                                                      
         MVC   RLACTIME,WORK+2                                                  
         DROP  R3,R2                                                            
*                                                                               
         LHI   R1,IOBPUT+IOXSPFIL+B#REVREC                                      
         CLI   SVACTION,QACTREP            RECORD REPLACED?                     
         BNE   ENDLIN15                                                         
         BRAS  RE,UPDREP           UPDATE ORIGINAL RECORD REPLACED              
*                                                                               
         LHI   R1,IOBADD+IOXSPFIL+B#REVREC YES, THEN ADD IT                     
ENDLIN15 GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
* RUN REV DOWNLOAD ?                                                            
*                                                                               
*        SEND BACK CONFIRMATION = SEQ#, PCKEY AND CHKSUM                        
ENDLINX  DS    0H                                                               
         L     RE,AREVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,QCHKSUM                                                    
*                                                                               
         LHI   R0,I#SDLCNF          X'0331' - REVLINE CONFIRMATION              
         CLI   NEW.DRVKSUB,DRVKSUBQ X'10' - REVISION SUBTYPE?                   
         BE    ENDLINX2                                                         
         LHI   R0,I#SDACNF          X'0334' - AVAIL CONFIRMATION                
         CLI   NEW.DRVKSUB,DWKKSUBQ X'11' - WORK SUBTYPE?                       
         JNE   *+2                   NO, NEW RECORD TYPE??                      
*                                                                               
ENDLINX2 BRAS  RE,SUCRESP          SEND SUCCESS RESPONSE                        
         J     EXIT                                                             
************                                                                    
*** MARK ORIGINAL RECORD REPLACED                                               
* THIS CODE WAS MOVED HERE FROM VALRL55 BECAUSE WE DON'T WANT TO UPDATE         
* OLD LINE AS REPLACED TO EARLY IN CASE THERE'S AN ERROR ON NEW LINE            
************                                                                    
UPDREP   NTR1                                                                   
         MVC   IOKEY,SVKEYREP      READ KEY TO BE MARKED REPLACED               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUP+IOXSPDIR+B#REVREP'                    
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREP'                   
         JNE   *+2                                                              
*                                                                               
         LA    R2,IOKEY                                                         
         USING DRVRECD,R2                                                       
         OI    DRVKST1,DRVKSRPL    X'80' IN KEY+33                              
         L     R2,AREVREP                                                       
         OI    DRVRST1,DRVRSRPL    X'80' IN REC+35                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOXSPDIR+B#REVREP'                     
         JNE   *+2                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREP'                     
         JNE   *+2                                                              
         MVC   IOKEY,SVKEY         RESTORE IOKEY                                
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
*===============================================================                
* ROUTINE TO SEND SUCCESS RESPONSE                                              
* ON ENTRY  WORK(2) - ADD/CHANGE DATE                                           
*           WORK+2(3) - ADD/CHANGE TIME                                         
*===============================================================                
SUCRESP  NTR1  BASE=*,LABEL=*                                                   
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(R0))                   
*                                                                               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_LBINQ',QSEQ#),(L'QSEQ#,0)                                   
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',2),              X        
               ('LD_CHARQ',QPCKEY),(L'QPCKEY,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',3),              X        
               ('LD_HEXDQ',QCHKSUM),(L'QCHKSUM,0)                               
         OC    IODA,IODA           HAVE D/A?                                    
         JZ    *+2                  NO                                          
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',4),              X        
               ('LD_HEXDQ',IODA),(L'IODA,0)                                     
         L     R2,AREVREC                                                       
         USING DRVRECD,R2                                                       
         LA    R3,DRVEL                                                         
         USING RLDELD,R3                                                        
         CLI   RLDEL,RLDELQ                                                     
         BE    SR05                                                             
         MVI   ELCDLO,RLDELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,RLDELQ       X'11'                                        
         BRAS  RE,NEXTEL                                                        
         JNE   SR10                                                             
         OC    RLLINEID,RLLINEID                                                
         JZ    SR10                                                             
SR05     GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',5),            X        
               ('LD_LBINQ',RLLINEID),(L'RLLINEID,0)                             
*                                                                               
SR10     GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',6),            X        
               ('LD_CDATQ',WORK),(2,0)                                          
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',7),              X        
               ('LD_HEXDQ',WORK+2),(3,0)                                        
*                                                                               
         LA    R4,IOKEY                                                         
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY       READ PERSON PASSWORD RECORD                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
*                                                                               
         L     R2,LP_ASECD         AND UPDATE IT                                
         USING SECD,R2                                                          
         MVC   SA0KNUM,SECOPASS                                                 
         MVC   SA0KAGY,SECAGY                                                   
         DROP  R2                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOCTFILE+B#SA0REC'                      
         JNE   SRX                                                              
*                                                                               
         L     R4,ASA0REC                                                       
         LA    R2,SA0DATA                                                       
         USING SAPALD,R2                                                        
         SR    R0,R0                                                            
SR20     CLI   SAPALEL,0           TEST END OF RECORD                           
         JE    SRX                                                              
         CLI   SAPALEL,SAPALELQ    TEST NEW SECURITY PERSON ELEMENT             
         JE    SR30                                                             
         IC    R0,SAPALLN          BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         J     SR20                                                             
*                                                                               
SR30     GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',8),            X        
               ('LD_CHARQ',SAPALPID),(L'SAPALPID,0)                             
SRX      J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*===============================================================                
* GET THE DATE AND TIME FOR ACTIVITY                                            
*  OUPUT : WORK(2)   - BINARY DATE                                              
*          WORK+2(3) - TIME                                                     
*===============================================================                
                                                                                
GETDTTM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,WORK)                                      
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         JL    GDT10                                                            
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTO1 VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTO1 VDATCON,DMCB,(0,DUB),(2,WORK)                                    
*                                                                               
GDT10    ICM   R1,15,PACKOF4B                                                   
         SRL   R1,4                GET RID OF SECONDS AND SIGN                  
         STCM  R1,7,WORK+2                                                      
         J     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* ROUTINE TO VALIDATE AND BUILD DEVIATED WEEKS RECORD                           
*  AKA - SCHEDULE GUIDELINES                                                    
*===============================================================                
VALDEV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   QENDDEV,C'N'        SET DEVIATED WEEKS NOT ENDED                 
*                                                                               
         CLI   QACTION,0           TEST ACTION GIVEN                            
         BE    *+10                                                             
         MVC   SVACTION,QACTION                                                 
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
         CLI   QMEDX,0                                                          
         BE    VALDV010                                                         
         MVC   SVBAGYMD,QMEDX                                                   
         MVC   SVMED,QMEDA                                                      
*                                                                               
VALDV010 OC    QCLTX,QCLTX                                                      
         BZ    *+10                                                             
         MVC   SVBCLT,QCLTX                                                     
*                                                                               
         CLI   SVBAGYMD,0          TEST MEDIA KNOWN                             
         BE    VALERR02                                                         
         OC    SVBCLT,SVBCLT       TEST CLIENT KNOWN                            
         BZ    VALERR04                                                         
*                                                                               
         L     R1,ACLTREC          TEST CLIENT RECORD IS AROUND                 
         CLC   SVBAGYMD(3),CKEYAM-CLTHDR(R1)                                    
         BE    VALDV020                                                         
*                                                                               
         MVC   QMEDX,SVBAGYMD      NO - GET CLIENT RECORD                       
         MVC   QCLTX,SVBCLT                                                     
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   VALERR03                                                         
*                                                                               
NEW      USING DDVKEY,SVKEY        BUILD NEW BUY KEY FROM INPUT DATA            
*                                                                               
VALDV020 XC    NEW.DDVKEY,NEW.DDVKEY                                            
*                                                                               
         MVI   NEW.DDVKTYP,DDVKTYPQ                                             
         MVI   NEW.DDVKSUB,DDVKSUBQ   X'13' - DEVIATED WEEKS SUBTYPE            
         MVC   NEW.DDVKAM,SVBAGYMD                                              
         MVC   NEW.DDVKCLT,SVBCLT                                               
         MVC   NEW.DDVKPRD,QPRD                                                 
         MVC   NEW.DDVKEST,QBEST                                                
         MVC   NEW.DDVKMKT,QMKT                                                 
*                                                                               
VALDV030 CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         BNE   VALDV040                                                         
*                                                                               
         L     R0,ADEVREC          INITIALIZE NEW RECORD                        
         LHI   R1,IO5LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,ADEVREC          AND SET NEW KEY                              
         MVC   0(L'DRVKEY,R1),SVKEY                                             
         B     EXITY                                                            
*                                                                               
VALDV040 MVC   IOKEY(L'DRVKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUP+IOXSPDIR+B#DEVREC'                    
         BNE   VALERR10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#DEVREC'                   
         BNE   VALERR11                                                         
* COMPUTE CHECKSUM FOR RECORD                                                   
         CLC   QCHKSUM,=F'1'       SO YOU CAN GET AROUND IN =LINK               
         BE    VALDV090                                                         
         L     RE,ADEVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         BO    *-4                                                              
         STCM  R0,15,FULL                                                       
         CLC   FULL,QCHKSUM                                                     
         BNE   VALERR20                                                         
*                                                                               
* DELETE ALL DATA FROM THE RECORD                                               
*                                                                               
VALDV090 MVI   ELCDLO,DVDELQ       NOW DELETE THE OLD ELEMENT                   
         MVI   ELCDHI,DVDELQ       X'05'                                        
         BRAS  RE,DELEL                                                         
         J     EXITY                                                            
*===============================================================                
* DEVIATED WEEKS DATA  (AKA SCHEDULE GUIDELINES)                                
*===============================================================                
VDDATA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ADEVREC                                                       
         USING DDVRECD,R2                                                       
*                                                                               
         USING DVDATAD,R3                                                       
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   DVDEL,DVDELQ        X'05'                                        
         MVI   DVDLEN,DVDLENQ                                                   
         MVC   DVDSTDAT,QDEVDATE                                                
         MVC   DVDINEX,QDEVINEX                                                 
*                                                                               
         OC    DVDSTDAT,DVDSTDAT   MUST HAVE DATE                               
         JZ    *+2                                                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QADEVWK                                                    
         JZ    VDATAX                                                           
*                                                                               
         USING LW_D,R2                                                          
VDDAT10  SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         USING VDATAD,R2                                                        
*                                                                               
VDDAT20  MVC   DVDAYS,VDADAY       DAY                                          
         OC    DVDAYS,DVDAYS       MUST HAVE DAYS                               
         JZ    *+2                                                              
         MVC   DVDSTIME,VDASTIM    START TIME                                   
         MVC   DVDETIME,VDAETIM    END TIME                                     
*                                                                               
         AHI   R2,VDDLNQ           NEXT ARRAY ENTRY                             
         AHI   R3,DVDLEN2Q         NEXT DEMO IN ELEMENT                         
         BCT   R0,VDDAT20                                                       
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,DVDLENQ                                                       
         STC   R3,ELEM+1                                                        
*                                                                               
VDDAT30  BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
VDATAX   J     EXITY                                                            
*                                                                               
VDATAD   DSECT                                                                  
VDADAY   DS    XL(L'DVDAYS)                                                     
VDASTIM  DS    XL(L'DVDSTIME)                                                   
VDAETIM  DS    XL(L'DVDETIME)                                                   
VDDLNQ   EQU   *-VDATAD                                                         
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*===============================================================                
* WRITE DEVIATED WEEKS RECORD TO FILE                                           
*===============================================================                
ENDDEV   NTR1  BASE=*,LABEL=*                                                   
         XC    QSEQ#,QSEQ#                                                      
*                                                                               
         L     R2,ADEVREC                                                       
         USING DDVRECD,R2                                                       
*                                                                               
         MVI   QENDLINE,C'Y'        SET FIRST REVISION ENDED                    
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
         BRAS  RE,GETDTTM          ITS IN WORK, DON'T BLOW IT AWAY              
*                                                                               
         CLI   SVACTION,QACTADD                                                 
         BNE   ENDDEV10                                                         
*                                                                               
* INSERT ACTIVITY ELEMENT                                                       
*                                                                               
         USING DVAELD,R3                                                        
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   DVAEL,DVAELQ        X'91'                                        
         MVI   DVALEN,DVALENQ                                                   
         L     R1,LP_ASECD                                                      
         MVC   DVAAPID,SECOPASS-SECD(R1)                                        
         MVC   DVAADD,WORK                                                      
         MVC   DVAATIME,WORK+2                                                  
         DROP  R3                                                               
*                                                                               
         L     R3,ADEVREC          POINT TO END OF REC                          
         SR    R0,R0                                                            
         ICM   R0,3,32(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         JNE   EXITN                                                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOXSPFIL+B#DEVREC'                     
         BE    ENDDEVX                                                          
         TM    IOERR,IOEDUP        DUPE RECORD?                                 
         BO    VALERR18             YES, SEND ERROR                             
         DC    H'0'                                                             
*                                                                               
ENDDEV10 DS    0H                                                               
         MVI   ELCDLO,DVAELQ       NOW FIND THE OLD ELEMENT                     
         MVI   ELCDHI,DVAELQ       X'91'                                        
         LA    R3,DDVEL                                                         
         BRAS  RE,NEXTEL2          FIND IT?                                     
         JNE   *+2                  NO                                          
         USING DVAEL,R3                                                         
         L     R1,LP_ASECD         AND UPDATE IT                                
         MVC   DVACPID,SECOPASS-SECD(R1)                                        
         MVC   DVACHG,WORK                                                      
         MVC   DVACTIME,WORK+2                                                  
         DROP  R3                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#DEVREC'                     
         JNE   *+2                                                              
                                                                                
*        SEND BACK CONFIRMATION = PCKEY, CHKSUM, D/A                            
ENDDEVX  DS    0H                                                               
         L     RE,ADEVREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,QCHKSUM                                                    
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#SDDCNF)               
*                                                                               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_CHARQ',QPCKEY),(L'QPCKEY,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',2),              X        
               ('LD_HEXDQ',QCHKSUM),(L'QCHKSUM,0)                               
         OC    IODA,IODA           HAVE D/A?                                    
         JZ    *+2                  NO                                          
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',3),              X        
               ('LD_HEXDQ',IODA),(L'IODA,0)                                     
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* VALIDATE MEDIA CODE                                                           
*===============================================================                
                                                                                
VALMED   XC    SVBVALS(SVBVALSX-SVBVALS),SVBVALS                                
         GOTOR (#VALMED,AVALMED),LP_AINP                                        
         MVC   SVMED,QMEDA                                                      
         MVC   SVBAGYMD,QMEDX                                                   
         B     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE CLIENT CODE                                                          
*===============================================================                
                                                                                
VALCLT   XC    SVBCLT(SVBVALSX-SVBCLT),SVBCLT                                   
         MVC   QMEDA,SVMED                                                      
         MVC   QMEDX,SVBAGYMD                                                   
         GOTOR (#VALCLT,AVALCLT),LP_AINP                                        
         MVC   SVCLT,QCLTA                                                      
         MVC   SVBCLT,QCLTX                                                     
         B     EXITY                                                            
                                                                                
*=================================================================              
* LOOK UP PRODUCT CODE IN CLIENT RECORD                                         
* NTRY:- R1=A(PRODUCT CODE)                                                     
* EXIT:- CC=LOW IF PRODUCT CODE IS NOT PRESENT                                  
*        CC=EQUAL IF PRODUCT CODE IS PRESENT AND CORRECT AND                    
*           R1=A(PRODUCT NUMBER)                                                
*        CC=HIGH IF PRODUCT CODE IS INVALID                                     
*=================================================================              
                                                                                
FINDPRD  CLI   0(R1),C'A'          TEST THERE IS A PRODUCT                      
         BLR   RE                  NO - EXIT WITH CC LOW                        
*                                                                               
         L     RF,ACLTREC          POINT TO CLIENT RECORD                       
         AHI   RF,CLIST-CLTHDR                                                  
*                                                                               
FINDPRD2 CLC   0(3,R1),0(RF)       MATCH PRODUCT CODE                           
         JNE   *+10                                                             
         LA    R1,3(RF)            FOUND - EXIT WITH CC EQUAL                   
         BR    RE                                                               
*                                                                               
         AHI   RF,4                                                             
         CLI   0(RF),C' '                                                       
         JH    FINDPRD2                                                         
         CLI   0(R1),0             NOT FOUND - EXIT WITH CC HIGH                
         BR    RE                                                               
         EJECT                                                                  
*===============================================================                
* CALL LINKIO TO BUILD ERROR RETURN                                             
*===============================================================                
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         MVC   WORK(2),2(R1)                                                    
*                                                                               
         L     RF,ALIOB                 SET ERROR MESSAGE FROM SYSTEM           
         MVC   LIOBMSYS-LIOBD(L'LIOBMSYS,RF),0(R1)                              
*                                                                               
         CLC   LP_VRSN,=AL1(3,0,0,88)  VERSION# >= V3.0.0.88?                   
         JNL   PUTERR10                 NO                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#UPLRSP)               
         J     PUTERR20                                                         
*                                                                               
PUTERR10 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#SDSERP)               
*                                                                               
PUTERR20 GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              *        
               ('LD_CHARQ',QPCKEY),(L'QPCKEY,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERR',40),             X        
               WORK,(L'XTRATEXT,XTRATEXT)                                       
         MVC   XTRATEXT,SPACES     RESET EXTRA MESSAGE TEXT TO SPACES           
PUTERRX  B     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* ROUTINE TO VALIDATE AND TRANSLATE ADJACENCY CODES                             
*===============================================================                
                                                                                
TRNSADJ  LM    R2,R4,LP_AINP                                                    
*                                                                               
         CHI   R3,2                ERROR IF LONGER THAN 2 CHARACTER             
         JH    EXITN                                                            
         CLI   0(R2),C' '          FIRST CHAR MUST BE ALPHANUMERIC              
         JNH   EXITN                                                            
*                                                                               
         MVC   0(L'QADJ,R4),0(R2)  MOVE 1 BYTE FIRST                            
         CHI   R3,1                IS IT ONE CHARACTER?                         
         JE    EXITY               -YES, EXIT                                   
         CLI   1(R2),C' '          -NO, IS IT STILL 1 CHARACTER?                
         JNH   EXITY               - YES, EXIT                                  
*                                                                               
         CLI   0(R2),C'0'          2 CHARACTER ADJ MUST BE NUMERIC?             
         JL    EXITN                                                            
         CLI   1(R2),C'0'                                                       
         JL    EXITN                                                            
*                                                                               
         PACK  0(L'QADJ,R4),0(1,R2)                                             
         NI    0(R4),X'F0'         SAVE 1ST CHAR/# IN HIGH NIBBLE               
         MVC   BYTE,1(R2)          SAVE 2ND CHAR INTO BYTE                      
         NI    BYTE,X'0F'          CLEAR BITS EXCEPT LOW NIBBLE                 
         OC    0(L'QADJ,R4),BYTE   SAVE 2ND CHAR/# IN LOW NIBBLE                
         J     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* ROUTINE TO VALIDATE BOOK TYPES                                                
*===============================================================                
                                                                                
TRNSBKT  LM    R2,R4,LP_AINP                                                    
*                                                                               
         MVI   0(R4),X'FF'                                                      
         CLC   =C'~#CLEAR',0(R2)           SET TO CLEAR FIELD                   
         JE    EXITY                                                            
*                                                                               
         BCTR  R3,0                                                             
         XC    HALF,HALF                                                        
         BASR  RE,0                                                             
         MVC   HALF(0),0(R2)                                                    
         EX    R3,0(RE)                                                         
*                                                                               
         CLI   HALF,C' '                                                        
         JNH   EXITN                                                            
         OC    HALF,SPACES                                                      
*                                                                               
         GOTOR (#TRNSBT,ATRNSBT),DMCB,(R2),2,(R4),0                             
         CLI   0(R4),X'FF'                                                      
         JE    EXITN                                                            
         J     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* VALIDATE THE DEMO CODES                                                       
* ON ENTRY : PARM1 = A(INPUT FIELD)                                             
*            PARM2 = L'INPUT FIELD                                              
*            PARM3 = A(OUTPUT FIELD)                                            
*                                                                               
* ON EXIT  : PARM3 = CC IS EQ, OUTPUT IS SET                                    
*                    CC IS NEQ, ERROR HAS BEEN SENT                             
*===============================================================                
VALDCD   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         CLI   0(R2),C'X'          COMSCORE?                                    
         JE    VLDC020              YES                                         
         CLI   1(R2),C'X'          COMSCORE?                                    
         JE    VLDC020              YES                                         
*                                                                               
         LAY   RF,-1(R2,R3)        TREAT INPUT AS TRAD DEMO CATEGORY            
VLDC010  CLI   0(RF),C' '          GET THE LENGTH OF INPUT                      
         JH    *+8                                                              
         JCT   RF,VLDC010                                                       
         LAY   RE,-1(R2)                                                        
         SR    RF,RE                IF LESS THAN 1                              
         JNP   VALERR07              SEND ERROR                                 
         CHI   RF,4                 OR GREATER THAN 4                           
         JH    VALERR07              SEND ERROR                                 
         GOTOR (#VALDCD,AVALDCD),DMCB,(R2),(RF),(R4),0                          
         JNE   VALERR07                                                         
         J     VLDCYES                                                          
*                                                                               
VLDC020  GOTOR GTNTDEMC,DMCB,(R2),(R4)                                          
         JNE   VALERR07                                                         
VLDCYES  J     EXITY                                                            
*============================================                                   
* GET NON-TRADITIONAL INTERNAL DEMO CODE                                        
* ON ENTRY : PARM1 = A(NON-TRADITION DEMO NAME)                                 
*            PARM2 = A(OUTPUT)                                                  
*                                                                               
* ON EXIT  : PARM2 = CC IS EQ, OUTPUT IS SET                                    
*                    CC IS NEQ, ERROR HAS BEEN SENT                             
*============================================                                   
GTNTDEMC NTR1  LABEL=*                                                          
         L     R2,DMCB                                                          
         L     R4,DMCB+4                                                        
*                                                                               
         MVC   FULL,=C'POL'     POL PRODUCT                                     
         BRAS  RE,GTESTREC                                                      
         L     R5,AESTREC                                                       
         USING ESTHDR,R5                                                        
*                                                                               
GTNTD010 LA    RE,20              SPOT ONLY HAS 20 MAX ENTRIES                  
         LA    RF,ENONTDMS                                                      
GTNTD020 CLC   0(L'ENONTDMS,RF),0(R2)  FOUND MATCH?                             
         BE    GTNTD030                -YES                                     
         LA    RF,L'ENONTDMS(RF)       -NO, BUMP TO NEXT                        
         BCT   RE,GTNTD020                                                      
         J     EXITN                   IF EOL, EXIT NO                          
GTNTD030 LA    RE,ENONTDMS-L'ENONTDMS                                           
         SR    RF,RE               GET DISPLACEMENT                             
         SRL   RF,3                GET INDEX                                    
         STC   RF,1(R4)                                                         
         J     EXITY                                                            
         DROP  R5                                                               
         EJECT                                                                  
*===========================================                                    
* READ THE POL ESTIMATE IN AESTREC                                              
*  ON ENTRY :  FULL    SET TO BRAND OR POL                                      
*                                                                               
*  ON EXIT  :  AESTREC HAS ESTIMATE RECORD                                      
*===========================================                                    
         USING GERWORKD,RC                                                      
GTESTREC NTR1  LABEL=*,WORK=(RC,GERWORKL)                                       
         MVC   GERIOVAL,IOVALS                                                  
         LA    R2,IOKEY                                                         
         USING ESTHDR,R2           READ ESTIMATE TO ESTABLISH                   
         XC    EKEY,EKEY            NON-TRADITIONAL DEMO NAMES                  
         MVC   EKEYAM,SVBAGYMD     A/M                                          
         MVC   EKEYCLT,SVBCLT      CLT                                          
         MVC   EKEYPRD,FULL        POL PRODUCT                                  
         MVC   EKEYEST,QBEST       EST                                          
         L     R2,AESTREC                                                       
         CLC   EKEY,IOKEY          TEST ESTMIATE RECORD AROUND                  
         JE    GTESTRCX                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#ESTREC'                         
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ESTREC'                        
         JNE   *+2                                                              
GTESTRCX MVC   IOVALS(IOVALL),GERIOVAL                                          
         J     EXITY                                                            
         DROP  R2                                                               
GERWORKD DSECT                                                                  
GERIOVAL DS    XL(IOVALL)                                                       
GERWORKL EQU   *-GERWORKD                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*=================================================================              
* ELEMENT MAINTENANCE ROUTINES                                                  
*=================================================================              
                                                                                
BUMPADD  CLI   0(R3),0             TEST AT EOR ALREADY                          
         JE    ADDEL                                                            
         SR    R0,R0               BUMP TO NEXT ELEMENT AND ADD                 
         IC    R0,1(R3)            ELEMENT                                      
         AR    R3,R0                                                            
                                                                                
ADDEL    NTR1  ,                   ADD AN ELEMENT                               
         LLC   R1,ELEM+1                                                        
         AHI   R1,-2                                                            
         BASR  RE,0                                                             
         OC    ELEM+2(0),ELEM+2    NO CHANGE TO ELEMENT?                        
         EX    R1,0(RE)                                                         
         JZ    EXIT                                                             
*                                                                               
         GOTOR VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO5,ELEM,0                        
         CLI   DMCB+12,0                                                        
         JE    EXIT                                                             
         TM    DMCB+12,X'05'       RECORD OVERFLOW?                             
         JO    VALERR06                                                         
         DC    H'0'                                                             
                                                                                
DELEL    NTR1  ,                   DELETE AN ELEMENT                            
         CLC   ELCDLO,ELCDHI       VALID RANGE?                                 
         JH    *+2                  PREVENT INFINITE LOOP                       
         MVC   BYTE,ELCDLO                                                      
DELEL10  GOTOR VHELLO,DMCB,(C'D',=C'XSPFIL'),(BYTE,AIO5),0,0                    
         CLI   DMCB+12,0           ANY ERRORS?                                  
         JNE   *+2                  YES, DIE                                    
         CLC   BYTE,ELCDHI         FINISH DELETING ALL ELEMS                    
         JNL   EXIT                                                             
         LLC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         J     DELEL10                                                          
                                                                                
NEXTEL   CLI   0(R3),0             LOCATE AN ELEMENT                            
         JE    NEXTELN                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R3)          VALID ELEM LENGTH?                           
         JZ    *+2                  NO                                          
         AR    R3,RF                                                            
NEXTEL2  CLC   ELCDLO,0(R3)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R3)                                                     
         JL    NEXTEL                                                           
NEXTELY  CR    RE,RE                                                            
         BR    RE                                                               
NEXTELN  LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBALLY ADDRESSABLE THINGS **            
                                                                                
VALERR02 LHI   RE,SE#MSGMD         MISSING MEDIA                                
         B     VALERXS2                                                         
                                                                                
VALERR03 MVC   XTRATEXT+5(L'QCLTA),QCLTA                                        
         LHI   RE,SE#INCLT         INVALID CLIENT                               
         B     VALERXS2                                                         
                                                                                
VALERR04 LHI   RE,SE#MSGCL         MISSING CLIENT                               
         B     VALERXS2                                                         
                                                                                
VALERR05 LHI   RE,SE#CSUPD         CAN'T UPDATE, COMSCORE DEMO ON EST,          
         B     VALERXS3            PLEASE UPGRADE                               
                                                                                
VALERR06 LHI   RE,SE#RCOVR         RECORD OVERFLOW, RECORD NOT SAVED            
         B     VALERXS2                                                         
                                                                                
VALERR07 LHI   RE,SE#INVDM         INVALID DEMO                                 
         B     VALERXS2                                                         
                                                                                
VALERR10 LHI   RE,SE#KEYNF         KEY NOT FOUND                                
         B     VALERXS2                                                         
                                                                                
VALERR11 LHI   RE,SE#RECNF         RECORD NOT FOUND                             
         B     VALERXS2                                                         
                                                                                
VALERR12 LHI   RE,SE#NO2DR         MEDIA CANNOT USE 2-DECIMAL RATING            
         B     VALERXS2                                                         
                                                                                
VALERR13 LHI   RE,SE#NO2PS         NO 2-DEC PREC SUPPORT FOR PUT&SHR            
         B     VALERXS2                                                         
                                                                                
VALERR18 LHI   RE,SE#RECEX         RECORD ALREADY EXISTS ON ADD                 
         B     VALERXS2                                                         
*                                                                               
VALERR19 LHI   RE,SE#NOAAA         PRODUCT AAA NOT ALLOWED!                     
         B     VALERXS2                                                         
                                                                                
VALERR20 MVI   ERRORFLG,SGLCHKSM                                                
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRUN',I#SDDEVD)               
         LHI   RE,SE#SGCKS                                                      
         B     VALERR39                                                         
                                                                                
VALERR30 MVI   ERRORFLG,SHTCHKSM                                                
         B     *+8                                                              
VALERR31 MVI   ERRORFLG,SNTCHKSM                                                
         LHI   R0,I#SDRSDR                                                      
         CLI   IOKEY+1,DRVKSUBQ    X'10' - REVISION SUBTYPE?                    
         BE    VALERR32                                                         
         LHI   R0,I#SDWSDR                                                      
         CLI   IOKEY+1,DWKKSUBQ    X'11' - WORKSHEET SUBTYPE?                   
         JNE   *+2                                                              
VALERR32 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRUN',(R0))                   
         LHI   RE,SE#CKSUM         CHKSUM MISMATCH - ON SHEET                   
         B     VALERR39                                                         
                                                                                
VALERR35 MVI   ERRORFLG,LINCHKSM                                                
         LHI   R0,I#SDRLDR                                                      
         CLI   IOKEY+1,DRVKSUBQ    X'10' - REVISION SUBTYPE?                    
         BE    VALERR36                                                         
         LHI   R0,I#SDALDR                                                      
         CLI   IOKEY+1,DWKKSUBQ    X'11' - WORKSHEET SUBTYPE?                   
         JNE   *+2                                                              
VALERR36 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRUN',(R0))                   
         LHI   RE,SE#CKSML         CHKSUM MISMATCH - ON REVLINE/AVAIL           
                                                                                
VALERR39 CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XTRATEXT(4),DUB                                                  
         STCM  RE,3,WORK                                                        
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLERR),     *        
               WORK,(L'XTRATEXT,XTRATEXT)                                       
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#UPLERN),     *        
               ('LD_HEXDQ',WORK),(2,0)                                          
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#DA),         *        
               ('LD_HEXDQ',IOKEY+36),(L'DRVKDA,0)                               
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',D#PCKEY),        *        
               ('LD_CHARQ',QPCKEY),(L'QPCKEY,0)                                 
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERU',0),0,0                    
         J     EXITN                                                            
                                                                                
VALERXS2 LA    RF,SE#SY002         SET SYSTEM TO SPOT                           
         J     *+8                                                              
VALERXS3 LA    RF,SE#SY003         SET SYSTEM TO NET                            
                                                                                
VALERRX  MVI   ERRORFLG,FATALERR   SET TO SKIP ALL DATA                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XTRATEXT(4),DUB                                                  
         GOTOR PUTERR,DMCB,((RF),(RE))                                          
                                                                                
EXITN    SR    RE,RE                                                            
         B     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
STAMPLIT DC    C'**SL1A**'                                                      
                                                                                
RECTAB   DS    0XL6                        ** RECORD TABLE **                   
*                                   ** DEVIATED WEEKS UPLOAD MAPS **            
         DC    AL2(I#SDDWKH),AL3(VALDEV),X'FF'   03D6 DEV WEEKS HEADER          
         DC    AL2(I#SDDWDT),AL3(VDDATA),X'00'   03D7 DEV WEEKS DATA            
         DC    AL2(I#SDDWND),AL3(ENDDEV),X'00'   03D8 DEV WEEKS END             
*                                                                               
*                                   ** SHEET UPLOAD MAPS **                     
         DC    AL2(I#SDRSKU),AL3(VALREV),X'FF'   035F REVSHEET HEADER           
         DC    AL2(I#SDWSKU),AL3(VALREV),X'FF'   0340 WORKSHEET HEADER          
         DC    AL2(I#SDRSNM),AL3(VRNAME),X'0'    0368 NAME                      
         DC    AL2(I#SDRSDT),AL3(VRDATA),X'0'    0361 DATA                      
         DC    AL2(I#SDRSDM),AL3(VRDEMO),X'0'    0362 DEMOS                     
         DC    AL2(I#SDRSBK),AL3(VRBOOK),X'0'    0363 BOOKS                     
         DC    AL2(I#SDRSUP),AL3(VRUPG),X'0'     0364 UPGRADE FORM              
         DC    AL2(I#SDRSCP),AL3(VRCOPY),X'0'    0367 COPY FROM                 
         DC    AL2(I#SDRLPM),AL3(VRDTP),X'0'     0366 DAY/TIME PER. OVR         
         DC    AL2(I#SDRCME),AL3(VRCUME),X'0'    036B CUME                      
         DC    AL2(I#SDRSND),AL3(ENDREV),X'0'    0369 END                       
*                                                                               
         DC    AL2(I#SDRSNT),AL3(VRNOTE),X'80'   0365 NOTES                     
         DC    AL2(I#SDRSEN),AL3(ENDNTS),X'0'    036A NOTES END                 
*                                                                               
*                                   ** LINE UPLOAD MAPS **                      
         DC    AL2(I#SDRLKU),AL3(VALRLINE),X'80' 0360 REVLINE HEADER            
         DC    AL2(I#SDAVKU),AL3(VALRLINE),X'80' 0341 AVAIL HEADER              
         DC    AL2(I#SDRLDT),AL3(VLDATA),X'0'    0370 DATA                      
         DC    AL2(I#SDRLCM),AL3(VLCOM),X'0'     0371 COMMENTS                  
         DC    AL2(I#SDRLDM),AL3(VLEDEM),X'0'    0372 ESTIMATE DEMOS            
         DC    AL2(I#SDRLSD),AL3(VLSDEM),X'0'    037B EST SPILLDEMOS            
         DC    AL2(I#SDRLUP),AL3(VLUPG),X'0'     0373 UPGRADE FORMULA           
         DC    AL2(I#SDRLSP),AL3(VLSPOT),X'0'    0374 SPOTS                     
         DC    AL2(I#SDRLRT),AL3(VLRATES),X'0'   0375 EFFECTIVE RATES           
****     DC    AL2(I#SDRLHI),AL3(VLHIAT),X'0'    0376 HIATUS                    
         DC    AL2(I#SDRLOR),AL3(VLORB),X'0'     0377 ORBITS                    
         DC    AL2(I#SDRLPK),AL3(VLPKG),X'0'     0378 PACKAGES                  
         DC    AL2(I#SDRLNT),AL3(VLNOTE),X'0'    037A NOTES                     
         DC    AL2(I#SDRLAA),AL3(VLAAUUID),X'0'  037C VIDEA UUID                
         DC    AL2(I#SDRLND),AL3(ENDLINE),X'0'   0380 END                       
*                                                                               
RECTABX  EQU   *                                                                
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
*                                                                               
         EJECT                                                                  
TADJ     DC    C'Adj Code'                                                      
TRAUDM   DC    C'Aud Demo'                                                      
TRAUTADJ DC    C'Auto Adjust'                                                   
TRAARHOM DC    C'Auto Adjust if RHomes'                                         
TRAATARG DC    C'Auto Adjust if Target Demo'                                    
TLAAUID  DC    C'Automated Avail UUID'                                          
TRBOOK   DC    C'Book'                                                          
TRBKTYP  DC    C'Book Type'                                                     
TLBYID   DC    C'Buy Id/Contrct'                                                
TLBLINE  DC    C'Buy Line#'                                                     
TRBYR    DC    C'Buyer'                                                         
TCMDATA1 DC    C'Com Line 1'                                                    
TCMDATA2 DC    C'Com Line 2'                                                    
TCMDATA3 DC    C'Com Line 3'                                                    
TCMDATA4 DC    C'Com Line 4'                                                    
TCMDATA5 DC    C'Com Line 5'                                                    
TRCMBDLY DC    C'Combine Daily Buys'                                            
TRCSBKTY DC    C'comScore Booktype'                                             
TRCSSRDT DC    C'comScore Survey Dates'                                         
TRCPMED  DC    C'Copied Med'                                                    
TRCPCLT  DC    C'Copied Clt'                                                    
TRCPPRD  DC    C'Copied Prd'                                                    
TRCPEST  DC    C'Copied Est'                                                    
TRCPMKT  DC    C'Copied Mkt'                                                    
TRCPSEQ  DC    C'Copied Seq'                                                    
TLSCOST  DC    C'Cost Override'                                                 
TLCOST2  DC    C'Cost2/-Factor'                                                 
TLC2IND  DC    C'Cost2/-Factor Ind'                                             
TRCUME   DC    C'CUME Book'                                                     
TRDLY    DC    C'Daily flag'                                                    
TLDAYPT  DC    C'Daypart'                                                       
TDAYS    DC    C'Days'                                                          
TDECPREC DC    C'Decimal Precision'                                             
TLODEL   DC    C'Delete orbits on transfer'                                     
TDELBUY  DC    C'Delete Buy on Transfer'                                        
TDELSPT  DC    C'Delete Spots on Transfer'                                      
TDEMOVAL DC    C'Demoval'                                                       
TDSP     DC    C'Display on sheet'                                              
TDSKADDR DC    C'DskAddr'                                                       
TLRTSDAT DC    C'Eff Rate St Date'                                              
TLRTEDAT DC    C'Eff Rate End Date'                                             
TLRATE   DC    C'Eff Rate'                                                      
TENDDT   DC    C'End Date'                                                      
TETIME   DC    C'End Time'                                                      
TRSTDT   DC    C'Flt Start Date'                                                
TRENDT   DC    C'Flt End Date'                                                  
TLHIST   DC    C'Hiatus Start'                                                  
TLHIND   DC    C'Hiatus End'                                                    
THILINE# DC    C'High Line ID'                                                  
TLHISQ#  DC    C'High Spot Seq#'                                                
TINCEXC  DC    C'Include/Exclude?'                                              
TLINSEQ  DC    C'Line Seq#'                                                     
TLLINEID DC    C'Line ID'                                                       
TLMGCD   DC    C'Makegood Code'                                                 
TNTLKUP  DC    C'non-Trad Demo Looked Up?'                                      
TNTSLKUP DC    C'non-Trad Spill Looked Up?'                                     
TNOTE    DC    C'Note'                                                          
TNOTSEQ  DC    C'Note Seq#'                                                     
TLSNOWK  DC    C'Number of spots'                                               
TPRVSCHD DC    C'Prev. Schd. Outside Dates'                                     
TLODAY   DC    C'Orbit Day'                                                     
TLOSTIM  DC    C'Orb St Time'                                                   
TLOETIM  DC    C'Orb End Time'                                                  
TLODESC  DC    C'Orbit Desc'                                                    
TLODEM   DC    C'Orbit Demo'                                                    
TLOFLAG  DC    C'Orbit Demo Flag'                                               
TOVRD    DC    C'Override'                                                      
TPCKEY   DC    C'PC Key'                                                        
TLPIND   DC    C'Pkg Ind'                                                       
TLPLINE  DC    C'Pkg Lines'                                                     
TPRD     DC    C'Prd Code'                                                      
TPRD2    DC    C'Prd Code 2'                                                    
TPRD2L   DC    C'Prd Code 2 Len'                                                
TLPROG   DC    C'Program'                                                       
TPURP    DC    C'Purpose Code'                                                  
TPUT     DC    C'Put'                                                           
TLCOST   DC    C'Rate'                                                          
TLCIND   DC    C'Rate Type'                                                     
TLCIND2  DC    C'Rate Type 2'                                                   
TLREASN  DC    C'Reason Code'                                                   
TREP     DC    C'Rep Code'                                                      
TLSSCHD  DC    C'Schedule Date'                                                 
TSHR     DC    C'Share'                                                         
TRNAME   DC    C'Sheet Name'                                                    
TSHTSEQ  DC    C'Sheet Seq#'                                                    
TLSDATE  DC    C'Spot Date'                                                     
TLSLN    DC    C'Spot Len'                                                      
TLSREF   DC    C'Spot Ref#'                                                     
TSTDT    DC    C'Start Date'                                                    
TSTDAY   DC    C'Start Day'                                                     
TSTIME   DC    C'Start Time'                                                    
TSTA     DC    C'Station'                                                       
TLSDYPT  DC    C'Sub Dpart'                                                     
TLTAX    DC    C'Tax'                                                           
TTIMEZON DC    C'Time Zone'                                                     
TTOKEN   DC    C'Token'                                                         
TRUPSEQ# DC    C'Upgrade Seq#'                                                  
TRUPSDAT DC    C'Upgrade St Date'                                               
TRUPEDAT DC    C'Upgrade End Date'                                              
TRUPFLG  DC    C'Upgrade Flag'                                                  
TRUPGRD  DC    C'Upgrade Form'                                                  
TBCAST   DC    C'Use for Broadcast Station'                                     
TCABLE   DC    C'Use for Cable Station'                                         
TLXSCHD  DC    C'X - DO NOT SCHEDULE'                                           
TLNPW    DC    C'#/Week'                                                        
*                                                                               
         EJECT                                                                  
*============================================================                   
* KEY - REVISION, WORKSHEET, DEVIATED WEEKS                                     
*============================================================                   
                                                                                
WRKSHT   LKREQ H,I#SDWSKU,POINTTO=HDRSHT     0340                               
REVSHT   LKREQ H,I#SDRSKU,POINTTO=HDRSHT     035F                               
DEVWEEK  LKREQ H,I#SDDWKH,POINTTO=HDRSHT     03D6                               
                                                                                
HDRSHT   LKREQ H,0,NEWREC=Y                                                     
                                                                                
SMED     LKREQ F,1,(D,B#WORKD,QMEDX),(R,VALMED),TEXT=SP#MED                     
SCLT     LKREQ F,2,(D,B#WORKD,QCLTX),(R,VALCLT),TEXT=SP#CLI                     
SPRD     LKREQ F,3,(D,B#SAVED,QPRD),CHAR,TEXT=SP#PRD                            
SEST     LKREQ F,4,(D,B#SAVED,QBEST),LBIN,TEXT=SP#EST                           
SMKT     LKREQ F,5,(D,B#SAVED,QMKT),LBIN,TEXT=SP#MKT                            
SSEQ     LKREQ F,6,(D,B#SAVED,QRSEQ),LBIN,TEXT=(*,TSHTSEQ)                      
SCHKSUM  LKREQ F,7,(D,B#SAVED,QCHKSUM),HEXD,TEXT=SP#CKSUM                       
SDSKADDR LKREQ F,8,(D,B#SAVED,QDSKADDR),HEXD,TEXT=(*,TDSKADDR)                  
SACTION  LKREQ F,9,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
SPCKEY   LKREQ F,10,(D,B#SAVED,QPCKEY),CHAR,TEXT=(*,TPCKEY)                     
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET NAME                                                       
*========================================================                       
                                                                                
SHNAME   LKREQ H,I#SDRSNM,NEWREC=Y           0368                               
                                                                                
SEQ      LKREQ F,1,(D,B#SAVED,QRSEQ2),LBIN,TEXT=(*,TSHTSEQ)                     
SNAME    LKREQ F,2,(I,B#SAVED,QRNAME),VSTR,TEXT=(*,TRNAME),            X        
               MAXLEN=255-RSNLENQ                                               
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET DATA                                                       
*========================================================                       
                                                                                
SHDATA   LKREQ H,I#SDRSDT,NEWREC=Y           0361                               
                                                                                
SSTDATE  LKREQ F,2,(D,B#SAVED,QSTDT),CDAT,TEXT=(*,TRSTDT)                       
SENDATE  LKREQ F,3,(D,B#SAVED,QENDT),CDAT,TEXT=(*,TRENDT)                       
SADJ     LKREQ F,4,(D,B#SAVED,QADJ),(R,TRNSADJ),TEXT=(*,TADJ),         X        
               OLEN=L'QADJ                                                      
SREP     LKREQ F,5,(D,B#SAVED,QREP),CHAR,TEXT=(*,TREP),OLEN=L'QREP              
SPURP    LKREQ F,6,(I,B#SAVED,QAPURP),VSTR,MAXLEN=L'RSDPURP,           X        
               TEXT=(*,TPURP)                                                   
SBYR     LKREQ F,7,(D,B#SAVED,QBYR),CHAR,TEXT=(*,TRBYR),OLEN=L'QBYR             
SPRD     LKREQ F,8,(D,B#SAVED,QPRD),CHAR,TEXT=(*,TPRD),OLEN=L'QPRD              
SAUDEM   LKREQ F,9,(D,B#SAVED,QAUDM),CHAR,,OLEN=L'QAUDM,TEXT=(*,TRAUDM)         
SDAILY   LKREQ F,20,(D,B#SAVED,QDAILY),CHAR,TEXT=(*,TRDLY),            X        
               OLEN=L'QDAILY                                                    
SCMBDLY  LKREQ F,21,(D,B#SAVED,QCOMBDLY),CHAR,TEXT=(*,TRCMBDLY),       X        
               OLEN=L'QCOMBDLY                                                  
SAUTADJ  LKREQ F,22,(D,B#SAVED,QAUTADJ),CHAR,TEXT=(*,TRAUTADJ),        X        
               OLEN=L'QAUTADJ                                                   
SAARHOME LKREQ F,23,(D,B#SAVED,QAARHOME),CHAR,TEXT=(*,TRAARHOM),       X        
               OLEN=L'QAARHOME                                                  
SAATARGT LKREQ F,24,(D,B#SAVED,QAATARGT),CHAR,TEXT=(*,TRAATARG),       X        
               OLEN=L'QAATARGT                                                  
SHILINE# LKREQ F,25,(D,B#SAVED,QHILINE#),LBIN,TEXT=(*,THILINE#)                 
SCMSCBTY LKREQ F,26,(D,B#SAVED,QCSBKTY),CHAR,TEXT=(*,TRCSBKTY),        X        
               OLEN=L'QCSBKTY                                                   
SCMSCBTY LKREQ F,27,(D,B#SAVED,QCSSRDT),CHAR,TEXT=(*,TRCSSRDT),        X        
               OLEN=L'QCSSRDT                                                   
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET DEMOS                                                      
*========================================================                       
                                                                                
SHDEMO   LKREQ H,I#SDRSDM,NEWREC=Y           0362                               
                                                                                
SDEMCD   LKREQ F,1,(I,B#SAVED,QADEMO),CHAR,ARRAY=S,SORT=NO,            X        
               TEXT=SP#DEMO,OLEN=L'VRDDEMO                                      
SDMBCAST LKREQ F,2,,CHAR,OLEN=L'VRDDFBCS,TEXT=(*,TBCAST)                        
SDMCABLE LKREQ F,3,,CHAR,OLEN=L'VDRDFCBL,TEXT=(*,TCABLE)                        
SDMDISPL LKREQ F,4,,CHAR,OLEN=L'VDRDFDIS,TEXT=(*,TDSP),ARRAY=E                  
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET BOOKS                                                      
*========================================================                       
                                                                                
SHBOOK   LKREQ H,I#SDRSBK,NEWREC=Y           0363                               
                                                                                
SHBOOK   LKREQ F,1,(I,B#SAVED,QABOOK),CHAR,LIST=F,SORT=N,              X        
               OLEN=L'RSBBOOK,TEXT=(*,TRBOOK)                                   
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET UPGRADES                                                   
*========================================================                       
                                                                                
SHUPS    LKREQ H,I#SDRSUP,NEWREC=Y           0364                               
                                                                                
SHUPSDAT LKREQ F,1,(I,B#SAVED,QAUPS),CDAT,OLEN=L'VRUSTTDT,ARRAY=S,     X        
               SORT=NO,TEXT=(*,TRUPSDAT)                                        
SHUPEDAT LKREQ F,2,,CDAT,OLEN=L'VRUENDDT,TEXT=(*,TRUPEDAT)                      
SHBCAST  LKREQ F,3,,CHAR,OLEN=L'VRUTBRD,TEXT=(*,TBCAST)                         
SHCABLE  LKREQ F,4,,CHAR,OLEN=L'VRUTCBL,TEXT=(*,TCABLE)                         
SHUPGRD  LKREQ F,5,,CHAR,OLEN=L'VRUFORM,TEXT=(*,TRUPGRD),              X        
               DELIM=X'00',ARRAY=E                                              
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET UPGRADES                                                   
*========================================================                       
                                                                                
SHCUMES  LKREQ H,I#SDRCME,NEWREC=Y           036B                               
                                                                                
SHTIMZON LKREQ F,1,(D,B#SAVED,QTIMEZON),CHAR,TEXT=(*,TTIMEZON)                  
CUMEBOOK LKREQ F,2,(I,B#SAVED,QACUME),VSTR,TEXT=(*,TRCUME),            X        
               MAXLEN=255-RSCULNQ                                               
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVISION/WORKSHEET DAY/TIME PERIOD OVERRIDES                                  
*========================================================                       
                                                                                
SHDTPOR  LKREQ H,I#SDRLPM,NEWREC=Y           0366                               
                                                                                
SDTPSTA  LKREQ F,1,(D,B#SAVED,QSTA),CHAR,TEXT=(*,TSTA)                          
SDTPDAYS LKREQ F,2,(D,B#SAVED,QDAYS),LBIN,TEXT=(*,TDAYS)                        
SDTPSTIM LKREQ F,3,(D,B#SAVED,QSTIME),LBIN,TEXT=(*,TSTIME)                      
SDTPETIM LKREQ F,4,(D,B#SAVED,QETIME),LBIN,TEXT=(*,TETIME)                      
SDTPUPG  LKREQ F,5,(I,B#SAVED,QDTPUPG),VSTR,TEXT=(*,TRUPGRD),          X        
               MAXLEN=255-RSPNLENQ                                              
SLDCODE  LKREQ F,6,(I,B#SAVED,QDTPDEM),CHAR,ARRAY=S,SORT=NO,           X        
               OLEN=L'VDTPCODE,TEXT=SP#DEMO                                     
SDTPVAL  LKREQ F,7,,SPAK,OLEN=L'VDTPVAL,TEXT=(*,TDEMOVAL)                       
SDTPDOVR LKREQ F,8,,MB80,OLEN=L'VDTPFLG,TEXT=(*,TOVRD)                          
SDTPPREC LKREQ F,13,,LBIN,OLEN=L'VDTPPREC,TEXT=(*,TDECPREC)                     
SDTPSHR  LKREQ F,9,,SPAK,OLEN=L'VDTPSHR,TEXT=(*,TSHR)                           
SDTPSOVR LKREQ F,10,,MB80,OLEN=L'VDTPSFLG,TEXT=(*,TOVRD)                        
SDTPSPRC LKREQ F,14,,LBIN,OLEN=L'VDTPSPRC,TEXT=(*,TDECPREC)                     
SDTPPUT  LKREQ F,11,,SPAK,OLEN=L'VDTPPUT,TEXT=(*,TPUT)                          
SDTPPOVR LKREQ F,12,,MB80,OLEN=L'VDTPPFLG,TEXT=(*,TOVRD)                        
SDTPPPRC LKREQ F,15,,LBIN,OLEN=L'VDTPPPRC,TEXT=(*,TDECPREC),ARRAY=E             
         LKREQ E                                                                
*========================================================                       
* REVISION/WORKSHEET COPY FROM                                                  
*========================================================                       
                                                                                
SHCOPY   LKREQ H,I#SDRSCP,NEWREC=Y           0367                               
                                                                                
SCPMED   LKREQ F,3,(D,B#SAVED,QCPMED),CHAR,TEXT=(*,TRCPMED)                     
SCPCLT   LKREQ F,4,(D,B#SAVED,QCPCLT),(R,VALCLT),TEXT=(*,TRCPCLT)               
SCPPRD   LKREQ F,5,(D,B#SAVED,QCPPRD),CHAR,TEXT=(*,TRCPPRD)                     
SCPEST   LKREQ F,6,(D,B#SAVED,QCPEST),LBIN,TEXT=(*,TRCPEST)                     
SCPMKT   LKREQ F,7,(D,B#SAVED,QCPMKT),LBIN,TEXT=(*,TRCPMKT)                     
SCPSEQ   LKREQ F,8,(D,B#SAVED,QCPSEQ),LBIN,TEXT=(*,TRCPSEQ)                     
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* REVISION/WORK NOTES                                                           
*============================================================                   
RSNOTES  LKREQ H,I#SDRSNT,NEWREC=Y           0365                               
                                                                                
SEQ      LKREQ F,1,(D,B#SAVED,QRSEQNOT),LBIN,TEXT=(*,TNOTSEQ)                   
NOTES    LKREQ F,2,(I,B#SAVED,QRNOTE),VSTR,TEXT=(*,TNOTE),             X        
               MAXLEN=255-RSNTLENQ                                              
ACTION   LKREQ F,3,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
CHKSUM   LKREQ F,4,(D,B#SAVED,QCHKSUM),HEXD,TEXT=SP#CKSUM                       
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* DEVIATED WEEKS DATA                                                           
*============================================================                   
                                                                                
DVDATA   LKREQ H,I#SDDWDT,NEWREC=Y           03D7                               
                                                                                
DVDATE   LKREQ F,1,(D,B#SAVED,QDEVDATE),CDAT,TEXT=(*,TSTDT)                     
DVIN/EX  LKREQ F,2,(D,B#SAVED,QDEVINEX),CHAR,TEXT=(*,TINCEXC)                   
DVDAYS   LKREQ F,3,(I,B#SAVED,QADEVWK),LBIN,                           *        
               OLEN=L'VDADAY,ARRAY=S,SORT=NO,TEXT=(*,TDAYS)                     
DVSDTIM  LKREQ F,4,,LBIN,OLEN=L'VDASTIM,TEXT=(*,TSTIME)                         
DVEDTIM  LKREQ F,5,,LBIN,OLEN=L'VDAETIM,TEXT=(*,TETIME),ARRAY=E                 
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* END OF REVISION, WORKSHEET, NOTES, DEVIATED WEEKS                             
*============================================================                   
                                                                                
NDREV    LKREQ *,I#SDRSND,NEWREC=Y           0369                               
NDRVNOT  LKREQ *,I#SDRSEN,NEWREC=Y           036A                               
NDDEVWK  LKREQ *,I#SDDWND,NEWREC=Y           03D8                               
                                                                                
*============================================================                   
* REVLINE/AVAIL KEY - SINGLE REVLINE/AVAIL                                      
*============================================================                   
                                                                                
AVAIL    LKREQ H,I#SDAVKU,POINTTO=HDRLIN     0341                               
RLINE    LKREQ H,I#SDRLKU,POINTTO=HDRLIN     0360                               
*                                                                               
HDRLIN   LKREQ H,0,NEWREC=Y                                                     
                                                                                
MED      LKREQ F,1,(D,B#WORKD,QMEDX),(R,VALMED),TEXT=SP#MED                     
CLT      LKREQ F,2,(D,B#WORKD,QCLTX),(R,VALCLT),TEXT=SP#CLI                     
PRD      LKREQ F,3,(D,B#SAVED,QPRD),CHAR,TEXT=SP#PRD                            
EST      LKREQ F,4,(D,B#SAVED,QBEST),LBIN,TEXT=SP#EST                           
MKT      LKREQ F,5,(D,B#SAVED,QMKT),LBIN,TEXT=SP#MKT                            
SEQ      LKREQ F,6,(D,B#SAVED,QRSEQ),LBIN,TEXT=(*,TSHTSEQ)                      
LSEQ     LKREQ F,7,(D,B#SAVED,QLSEQ),LBIN,TEXT=(*,TLINSEQ)                      
CHKSUM   LKREQ F,8,(D,B#SAVED,QCHKSUM),HEXD,TEXT=SP#CKSUM                       
DSKADDR  LKREQ F,9,(D,B#SAVED,QDSKADDR),HEXD,TEXT=(*,TDSKADDR)                  
ACTION   LKREQ F,10,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                       
PCKEY    LKREQ F,11,(D,B#SAVED,QPCKEY),CHAR,TEXT=(*,TPCKEY)                     
                                                                                
         LKREQ E                                                                
*========================================================                       
* REVLINE/AVAIL  DATA                                                           
*========================================================                       
                                                                                
LPHDT    LKREQ H,I#SDRLDT,NEWREC=Y           0370                               
                                                                                
LSEQ     LKREQ F,1,(D,B#SAVED,QLSEQ2),LBIN,TEXT=(*,TLINSEQ)                     
LBLINE   LKREQ F,2,(D,B#SAVED,QLBLINE),LBIN,TEXT=(*,TLBLINE)                    
LSTA     LKREQ F,3,(D,B#SAVED,QLSTA),CHAR,OLEN=L'QLSTA,TEXT=(*,TSTA)            
LBYR     LKREQ F,4,(D,B#SAVED,QLBYR),CHAR,OLEN=L'QLBYR,TEXT=(*,TRBYR)           
LSTDT    LKREQ F,5,(D,B#SAVED,QLSTDT),CDAT,TEXT=(*,TSTDT)                       
LENDT    LKREQ F,6,(D,B#SAVED,QLENDT),CDAT,TEXT=(*,TENDDT)                      
LDAYS    LKREQ F,7,(D,B#SAVED,QLDAYS),LBIN,TEXT=(*,TDAYS)                       
LSTDAY   LKREQ F,8,(D,B#SAVED,QLSTDAY),HEXD,TEXT=(*,TSTDAY)                     
LSTIME   LKREQ F,9,(D,B#SAVED,QLSTIME),SPAK,TEXT=(*,TSTIME)                     
LETIME   LKREQ F,10,(D,B#SAVED,QLETIME),SPAK,TEXT=(*,TETIME)                    
LSLN     LKREQ F,11,(D,B#SAVED,QLSLN),LBIN,TEXT=(*,TLSLN)                       
LNPW     LKREQ F,12,(D,B#SAVED,QLNPW),SPAK,TEXT=(*,TLNPW)                       
LDAYPT   LKREQ F,13,(D,B#SAVED,QLDAYPT),CHAR,OLEN=L'QLDAYPT,           X        
               TEXT=(*,TLDAYPT)                                                 
LSDYPT   LKREQ F,14,(D,B#SAVED,QLSDYPT),CHAR,OLEN=L'QLSDYPT,           X        
               TEXT=(*,TLSDYPT)                                                 
LPROG    LKREQ F,15,(D,B#SAVED,QLPROG),CHAR,OLEN=L'QLPROG,             X        
               TEXT=(*,TLPROG)                                                  
*ADJ     LKREQ F,16,(D,B#SAVED,QLADJ),CHAR,OLEN=L'QLADJ,TEXT=(*,TADJ)           
LADJ     LKREQ F,16,(D,B#SAVED,QLADJ),(R,TRNSADJ),TEXT=(*,TADJ),       X        
               OLEN=L'QADJ                                                      
LCOST    LKREQ F,17,(D,B#SAVED,QLCOST),SPAK,TEXT=(*,TLCOST)                     
LCIND    LKREQ F,18,(I,B#SAVED,QALCIND),VSTR,TEXT=(*,TLCIND)                    
LREP     LKREQ F,19,(D,B#SAVED,QLREP),CHAR,OLEN=L'QLREP,TEXT=(*,TREP)           
LPRD     LKREQ F,20,(D,B#SAVED,QLPRD),CHAR,OLEN=L'QLPRD,TEXT=(*,TPRD)           
LPRD2    LKREQ F,21,(D,B#SAVED,QLPRD2),CHAR,OLEN=L'QLPRD2,             X        
               TEXT=(*,TPRD2)                                                   
LPRD2L   LKREQ F,22,(D,B#SAVED,QLPRD2L),SPAK,TEXT=(*,TPRD2L)                    
LMGCD    LKREQ F,23,(D,B#SAVED,QLMGCD),CHAR,OLEN=L'QLMGCD,             X        
               TEXT=(*,TLMGCD)                                                  
LBYID    LKREQ F,24,(D,B#SAVED,QLBYID),CHAR,OLEN=L'QLBYID,             X        
               TEXT=(*,TLBYID)                                                  
LREASN   LKREQ F,25,(D,B#SAVED,QLREASN),CHAR,OLEN=L'QLREASN,           X        
               TEXT=(*,TLREASN)                                                 
LTAX     LKREQ F,26,(D,B#SAVED,QLTAX),SPAK,TEXT=(*,TLTAX)                       
LPURP    LKREQ F,27,(I,B#SAVED,QALPURP),VSTR,MAXLEN=L'RLDPURP,         X        
               TEXT=(*,TPURP)                                                   
LHISQ#   LKREQ F,28,(D,B#SAVED,QLHISQ#),LBIN,TEXT=(*,TLHISQ#)                   
LCOST2   LKREQ F,29,(I,B#SAVED,QALCOST2),VSTR,TEXT=(*,TLCOST2),        X        
               MAXLEN=L'RLCOST2*2-1                                             
LDAILY   LKREQ F,30,(D,B#SAVED,QLDAILY),CHAR,OLEN=L'QLDAILY,           X        
               TEXT=(*,TLCIND2)                                                 
LPRVSCHD LKREQ F,31,(D,B#SAVED,QLPRVSHD),CHAR,OLEN=L'QLPRVSHD,         X        
               TEXT=(*,TPRVSCHD)                                                
LDELBUY  LKREQ F,32,(D,B#SAVED,QLDELBUY),CHAR,OLEN=L'QLDELBUY,         X        
               TEXT=(*,TDELBUY)                                                 
LCOS2FAC LKREQ F,33,(D,B#SAVED,QLC2IND),CHAR,OLEN=L'QLC2IND,           X        
               TEXT=(*,TLC2IND)                                                 
LLINEID  LKREQ F,36,(D,B#SAVED,QLLINEID),LBIN,TEXT=(*,TLLINEID)                 
LDELSPT  LKREQ F,37,(D,B#SAVED,QLDELSPT),CHAR,OLEN=L'QLDELSPT,         X        
               TEXT=(*,TDELSPT)                                                 
LC2RTYP  LKREQ F,38,(D,B#SAVED,QLC2RTYP),CHAR,OLEN=L'QLC2RTYP,         X        
               TEXT=(*,TLCIND)                                                  
                                                                                
         LKREQ E                                                                
*============================================================                   
* REVLINE/AVAIL COMMENT ELEMENTS (MAP CODE IS COMMENT NUMBER)                   
*============================================================                   
                                                                                
COMDEFN  LKREQ H,I#SDRLCM,NEWREC=Y           0371                               
                                                                                
CMDATA1  LKREQ F,1,(I,B#SAVED,QACOMM1),VSTR,TEXT=(*,TCMDATA1),         X        
               MAXLEN=255-RLCMLENQ                                              
CMDATA2  LKREQ F,2,(I,B#SAVED,QACOMM2),VSTR,TEXT=(*,TCMDATA2),         X        
               MAXLEN=255-RLCMLENQ                                              
CMDATA3  LKREQ F,3,(I,B#SAVED,QACOMM3),VSTR,TEXT=(*,TCMDATA3),         X        
               MAXLEN=255-RLCMLENQ                                              
CMDATA4  LKREQ F,4,(I,B#SAVED,QACOMM4),VSTR,TEXT=(*,TCMDATA4),         X        
               MAXLEN=255-RLCMLENQ                                              
CMDATA5  LKREQ F,5,(I,B#SAVED,QACOMM5),VSTR,TEXT=(*,TCMDATA5),         X        
               MAXLEN=255-RLCMLENQ                                              
                                                                                
         LKREQ E                                                                
*========================================================                       
* REVLINE/AVAIL ESTIMATE DEMOS                                                  
*========================================================                       
                                                                                
RLEDEM   LKREQ H,I#SDRLDM,NEWREC=Y           0372                               
                                                                                
RLEDBK   LKREQ F,1,(D,B#SAVED,QLBOOK),BMON,TEXT=(*,TRBOOK)                      
*LEDBT   LKREQ F,2,(D,B#SAVED,QLBKTYP),CHAR,TEXT=(*,TRBKTYP)                    
RLEDBT   LKREQ F,2,(D,B#SAVED,QLBKTYP),(R,TRNSBKT),TEXT=(*,TRBKTYP)             
RLDCODE  LKREQ F,3,(I,B#SAVED,QALEDEM),CHAR,ARRAY=S,SORT=NO,           X        
               OLEN=L'VEDCAT,TEXT=SP#DEMO                                       
RLDVAL   LKREQ F,4,,SPAK,OLEN=L'VEDRAW,TEXT=(*,TDEMOVAL)                        
RLDOVR   LKREQ F,5,,MB80,OLEN=L'VEDFLG,TEXT=(*,TOVRD)                           
RLDPREC  LKREQ F,12,,LBIN,OLEN=L'VEDPREC,TEXT=(*,TDECPREC)                      
RLDSHR   LKREQ F,6,,SPAK,OLEN=L'VEDSHR,TEXT=(*,TSHR)                            
RLDSOVR  LKREQ F,7,,MB80,OLEN=L'VEDSFLG,TEXT=(*,TOVRD)                          
RLDSPREC LKREQ F,13,,LBIN,OLEN=L'VEDSPREC,TEXT=(*,TDECPREC)                     
RLDPUT   LKREQ F,8,,SPAK,OLEN=L'VEDPUT,TEXT=(*,TPUT)                            
RLDPOVR  LKREQ F,9,,MB80,OLEN=L'VEDPFLG,TEXT=(*,TOVRD)                          
RLDPPREC LKREQ F,14,,LBIN,OLEN=L'VEDPPREC,TEXT=(*,TDECPREC)                     
RLDNTLKU LKREQ F,10,,MB80,OLEN=0,TEXT=(*,TNTLKUP)                               
RLDNTSLK LKREQ F,11,,MB40,OLEN=L'VEDNTFLG,TEXT=(*,TNTSLKUP),ARRAY=E             
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVLINE/AVAIL ESTIMATE SPILL DEMOS                                            
*========================================================                       
                                                                                
RLSDEM   LKREQ H,I#SDRLSD,NEWREC=Y           037B                               
                                                                                
RLSDMKT  LKREQ F,1,(D,B#SAVED,QLDMKT),LBIN,TEXT=SP#MKT                          
                                                                                
RLSCODE  LKREQ F,2,(I,B#SAVED,QALSDEM),CHAR,ARRAY=S,SORT=NO,           X        
               OLEN=L'VSDCAT,TEXT=SP#DEMO                                       
RLSVAL   LKREQ F,3,,SPAK,OLEN=L'VSDRAW,TEXT=(*,TDEMOVAL)                        
RLSOVER  LKREQ F,4,,MB80,OLEN=L'VSDFLG,TEXT=(*,TOVRD)                           
RLSPREC  LKREQ F,5,,LBIN,OLEN=L'VSDPREC,TEXT=(*,TDECPREC)                       
RLSNTLKU LKREQ F,10,,MB80,OLEN=0,TEXT=(*,TNTLKUP)                               
RLSNTSLK LKREQ F,11,,MB40,OLEN=L'VSDNTFLG,TEXT=(*,TNTSLKUP),ARRAY=E             
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVLINE/AVAIL UPGRADE FORMULA                                                 
*========================================================                       
                                                                                
RLUPG    LKREQ H,I#SDRLUP,NEWREC=Y           0373                               
                                                                                
RLUPSDAT LKREQ F,1,(I,B#SAVED,QALUPG),CDAT,OLEN=L'VLUPSTDT,ARRAY=S,    X        
               SORT=NO,TEXT=(*,TRUPSDAT)                                        
RLUPEDAT LKREQ F,2,,CDAT,OLEN=L'VLUPENDT,TEXT=(*,TRUPEDAT)                      
RLUPGRD  LKREQ F,3,,CHAR,OLEN=L'VLUPUFRM,TEXT=(*,TRUPGRD),DELIM=X'00', X        
               ARRAY=E                                                          
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVLINE/AVAIL SPOTS                                                           
*========================================================                       
                                                                                
SHSPOTS  LKREQ H,I#SDRLSP,NEWREC=Y           0374                               
                                                                                
RLDATE   LKREQ F,1,(D,B#SAVED,QLSDATE),CDAT,TEXT=(*,TLSDATE)                    
RLREF    LKREQ F,2,(I,B#SAVED,QALSPOT),LBIN,OLEN=L'VSPREF#,ARRAY=S,    X        
               SORT=NO,TEXT=(*,TLSREF)                                          
RLNOWK   LKREQ F,3,,SPAK,OLEN=L'VSPNOWK,TEXT=(*,TLSNOWK)                        
RLPRD    LKREQ F,4,,CHAR,OLEN=L'VSPPRD,TEXT=(*,TPRD)                            
RLPRD2   LKREQ F,5,,CHAR,OLEN=L'VSPPRD2,TEXT=(*,TPRD2)                          
RLPRD2L  LKREQ F,6,,SPAK,OLEN=L'VSPPRDLN,TEXT=(*,TPRD2L)                        
RLCOST   LKREQ F,7,,SPAK,OLEN=L'VSPCOST,TEXT=(*,TLSCOST)                        
RLXSCHED LKREQ F,8,,CHAR,OLEN=L'VSPXSCHD,TEXT=(*,TLXSCHD),ARRAY=E               
                                                                                
         LKREQ E                                                                
*========================================================                       
* REVLINE/AVAIL EFFECTIVE RATES                                                 
*========================================================                       
                                                                                
RLRATES  LKREQ H,I#SDRLRT,NEWREC=Y           0375                               
                                                                                
RLRTSDAT LKREQ F,1,(I,B#SAVED,QALRATE),CDAT,OLEN=L'RLMSTTDT,ARRAY=S,   X        
               SORT=NO,TEXT=(*,TLRTSDAT)                                        
RLRTEDAT LKREQ F,2,,CDAT,OLEN=L'RLMENDDT,TEXT=(*,TLRTEDAT)                      
RLRATE   LKREQ F,3,,SPAK,OLEN=L'RLMRATE,TEXT=(*,TLRATE),ARRAY=E                 
                                                                                
         LKREQ E                                                                
*========================================================                       
* REVLINE/AVAIL HIATUS PERIODS                                                  
*========================================================                       
                                                                                
RLHIAT   LKREQ H,I#SDRLHI,NEWREC=Y           0376                               
                                                                                
RLHSTDT  LKREQ F,1,(I,B#SAVED,QALHIAT),CDAT,OLEN=L'VLHIAST,            X        
               ARRAY=S,SORT=NO,TEXT=(*,TLHIST)                                  
RLHENDT  LKREQ F,2,,CDAT,OLEN=L'VLHIAEND,TEXT=(*,TLHIND),ARRAY=E                
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVLINE/AVAIL ORBIT INFO                                                      
*========================================================                       
                                                                                
RLORB    LKREQ H,I#SDRLOR,NEWREC=Y           0377                               
                                                                                
RLODAY   LKREQ F,1,(I,B#SAVED,QALORB),LBIN,OLEN=L'VORDAY,              X        
               ARRAY=S,SORT=NO,TEXT=(*,TLODAY)                                  
RLOSTIM  LKREQ F,2,,SPAK,OLEN=L'VORSTIM,TEXT=(*,TLOSTIM)                        
RLOETIM  LKREQ F,3,,SPAK,OLEN=L'VORETIM,TEXT=(*,TLOETIM)                        
RLODESC  LKREQ F,4,,CHAR,OLEN=L'VORDESC,TEXT=(*,TLODESC)                        
RLODEM   LKREQ F,5,,SPAK,OLEN=L'VORDEM,TEXT=(*,TLODEM)                          
RLOPREC  LKREQ F,7,,LBIN,OLEN=L'VORDPREC,TEXT=(*,TDECPREC),ARRAY=E              
RLODEL   LKREQ F,6,(D,B#SAVED,QLODEL),CHAR,TEXT=(*,TLODEL)                      
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* REVLINE/AVAIL PACKAGE INFO                                                    
*========================================================                       
                                                                                
RLPKG    LKREQ H,I#SDRLPK,NEWREC=Y           0378                               
                                                                                
RLPKGIND LKREQ F,1,(D,B#SAVED,QLPIND),LBIN,TEXT=(*,TLPIND)                      
                                                                                
RLPKGLIN LKREQ F,2,(I,B#SAVED,QALPKG),LBIN,OLEN=1,                     X        
               LIST=F,SORT=NO,TEXT=(*,TLPLINE)                                  
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* REVLINE/AVAIL NOTES ELEMENT                                                   
*============================================================                   
                                                                                
NOTDEFN  LKREQ H,I#SDRLNT,NEWREC=Y           037A                               
                                                                                
NTDATA1  LKREQ F,1,(I,B#SAVED,QANOTE),VSTR,TEXT=(*,TNOTE),MAXLEN=250            
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* REVLINE/AVAIL AUTOMATED AVAIL UUID                                            
*============================================================                   
                                                                                
LNAUTAV  LKREQ H,I#SDRLAA,NEWREC=Y           037C                               
                                                                                
LNAAUUID LKREQ F,1,(I,B#SAVED,QALAAUID),VSTR,TEXT=(*,TLAAUID)                   
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* END OF REVLINE/AVAIL                                                          
*============================================================                   
                                                                                
LINEND   LKREQ *,I#SDRLND,NEWREC=Y           0380                               
                                                                                
         LKREQ X                                                                
*============================================================                   
         EJECT                                                                  
                                                                                
       ++INCLUDE SPLNKRECS                                                      
                                                                                
MAXRECLN EQU   3972                MAXIMUM RECORD LENGTH                        
                                                                                
SAVED    DSECT                                                                  
                                                                                
STAMP    DS    CL(L'STAMPLIT)      OVERLAY STAMP                                
                                                                                
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
DEMTABS  DS    A                   A(DEMO BOOK TYPES TABLE)                     
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
*                                                                               
AGY      DS    CL(L'LP_AGY)        AGENCY CODE                                  
MAPNUM   DS    XL(L'LP_QMAPN)      RECORD MAP NUMBER                            
DATALEN  DS    H                   LENGTH OF INPUT DATA                         
*                                                                               
ERRORFLG DS    X                                                                
FATALERR EQU   C'F'                FATAL ERROR, EXIT                            
SHTCHKSM EQU   C'S'                SHEET CHECKSUM ERROR                         
SNTCHKSM EQU   C'N'                SHEET NOTES CHECKSUM ERROR                   
LINCHKSM EQU   C'L'                LINE CHECKSUM ERROR                          
SLNCHKSM EQU   C'T'                LINE NOTES CHECKSUM ERROR                    
SGLCHKSM EQU   C'G'                SCHEDULE GUIDELINES CHECKSUM ERROR           
*                                                                               
SVESTPDM DS    C                   POL ESTIMATE PRIMARY DEMO TYPE               
*                                                                               
SVNTELEM DS    XL(L'ELEM)                                                       
*                                                                               
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVACTION DS    C                   SAVED ACTION CODE                            
*                                                                               
SVMED    DS    CL1                 MEDIA LETTER                                 
SVCLT    DS    CL3                 CLIENT CODE                                  
*                                                                               
SVBVALS  EQU   *                                                                
SVBAGYMD DS    XL1                 MEDIA CODE                                   
SVBCLT   DS    XL2                 CLIENT PACKED                                
SVBEST   DS    X                   ESTIMATE NUMBER                              
SVBLIN   DS    X                   BUY LINE NUMBER                              
SVBMKT   DS    XL2                 MARKET NUMBER                                
SVBSTA   DS    XL3                 STATION PACKED                               
SVSTA    DS    CL8                 STATION CALL LETTERS                         
SVKEY    DS    XL50                REV KEY                                      
SVKEYREP DS    XL50                REV KEY TO BE UPDATED AS REPLACED            
SVSPDATE DS    XL2                 SPOT DATE                                    
SVMKTSTA DS    XL5                                                              
*                                                                               
SVBVALSX EQU   *                                                                
                                                                                
XTRATEXT DS    CL20                EXTRA ERROR MESSAGE TEXT                     
                                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
QACTION  DS    C                   ** ACTION CODE **                            
QACTADD  EQU   C'A'                ADD A NEW BUY                                
QACTCHA  EQU   C'C'                CHANGE AN EXISTING BUY                       
QACTDEL  EQU   C'D'                DELETE AN EXISTING BUY                       
QACTREP  EQU   C'R'                REPLACE AN EXISTING BUY                      
QACTVAL  EQU   C'V'                VALIDATE THE RECORD ONLY                     
*                                                                               
QPRD     DS    CL3                                                              
QMKT     DS    XL2                                                              
QBEST    DS    X                   ESTIMATE NUMBER                              
QRSEQ    DS    XL3                                                              
QRSEQ2   DS    XL3                                                              
QRSEQNOT DS    X                                                                
QLSEQ    DS    XL3                                                              
QLSEQ2   DS    XL3                                                              
QSEQ#    DS    XL3                                                              
QCHKSUM  DS    F                   CHECK SUM                                    
QDSKADDR DS    F                                                                
QTOKEN   DS    CL2                                                              
QENDREV  DS    C                                                                
QENDLINE DS    C                                                                
QENDNOTE DS    C                                                                
QENDDEV  DS    C                                                                
QPCKEY   DS    CL20                                                             
*                                                                               
QDEVDATE DS    XL3                                                              
QDEVINEX DS    CL1                                                              
QADEVWK  DS    A                                                                
*                                                                               
QSHEET   EQU   *                   REVISION/WORK DATA                           
*                                                                               
QRNAME   DS    A                   A(NAME)                                      
QADEMO   DS    A                   A(DEMO ARRAY)                                
QABOOK   DS    A                   A(BOOK ARRAY)                                
QAUPS    DS    A                   A(UPGRADE ARRAY)                             
QDTPUPG  DS    A                   A(DAY/TIME PERIOD UPGRADE FORM)              
QDTPDEM  DS    A                   A(DAY/TIME PERIOD DEMO ARRAY)                
QRNOTE   DS    A                   A(REVISION/WORK NOTE ARRAY)                  
QACUME   DS    A                   A(CUME)                                      
QAPURP   DS    A                                                                
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
QSTDT    DS    XL2                 DATA FIELDS                                  
QENDT    DS    XL2                                                              
QSTIME   DS    XL2                                                              
QETIME   DS    XL2                                                              
QAUDM    DS    CL8                                                              
QDAILY   DS    XL1                 DAILY FLAG                                   
QCOMBDLY DS    XL1                 COMBINE DAILY BUYS                           
QAUTADJ  DS    XL1                 AUTO ADJUST                                  
QAARHOME DS    XL1                 AUTO ADJUST IF RHOMES                        
QAATARGT DS    XL1                 AUTO ADJUST IF TARGET                        
QHILINE# DS    XL2                                                              
QCSBKTY  DS    XL1                                                              
QCSSRDT  DS    XL1                                                              
QTIMEZON DS    XL1                                                              
QFLAG    DS    XL1                                                              
QADJ     DS    XL1                                                              
QREP     DS    CL3                                                              
QBYR     DS    CL12                                                             
*                                                                               
QCPMED   DS    CL1                 COPIED FROM DATA                             
QCPCLT   DS    XL2                                                              
QCPPRD   DS    CL3                                                              
QCPEST   DS    XL1                                                              
QCPMKT   DS    XL2                                                              
QCPSEQ   DS    XL3                                                              
*                                                                               
QSTA     DS    CL10                                                             
QDAYS    DS    XL1                                                              
QDTPFRST DS    X                                                                
QSHEETX  EQU   *                                                                
*                                                                               
QLINE    EQU   *                   REVLINE/AVAIL  DATA                          
*                                                                               
QLSTA    DS    CL10                                                             
QLBLINE  DS    XL2                                                              
QLSTDT   DS    XL2                                                              
QLENDT   DS    XL2                                                              
QLDAYS   DS    XL1                                                              
QLSTDAY  DS    XL1                                                              
QLSTIME  DS    XL3                                                              
QLETIME  DS    XL3                                                              
QLSLN    DS    XL1                                                              
QLNPW    DS    XL2                                                              
QLDAYPT  DS    CL1                                                              
QLSDYPT  DS    CL1                                                              
QLPROG   DS    CL18                                                             
QLADJ    DS    XL1                                                              
QLCOST   DS    XL6                                                              
QLREP    DS    CL3                                                              
QLPRD    DS    CL3                                                              
QLPRD2   DS    CL3                                                              
QLPRD2L  DS    XL2                                                              
QLMGCD   DS    CL2                                                              
QLBYID   DS    CL12                                                             
QLREASN  DS    CL6                                                              
QLTAX    DS    XL4                                                              
QLHISQ#  DS    XL2                                                              
QLDAILY  DS    CL1                                                              
QLPRVSHD DS    CL1                                                              
QLDELBUY DS    CL1                                                              
QLDELSPT DS    CL1                                                              
QLBYR    DS    CL12                                                             
QLC2IND  DS    X                                                                
QLLINEID DS    XL2                                                              
QLODEL   DS    X                                                                
QLC2RTYP DS    X                                                                
*                                                                               
QACOMM1  DS    A                   A(COMMENT LINE 1)                            
QACOMM2  DS    A                   A(COMMENT LINE 2)                            
QACOMM3  DS    A                   A(COMMENT LINE 3)                            
QACOMM4  DS    A                   A(COMMENT LINE 4)                            
QACOMM5  DS    A                   A(COMMENT LINE 5)                            
QANOTE   DS    A                   A(NOTES LINE)                                
*                                                                               
QALPURP  DS    A                                                                
QALCOST2 DS    A                                                                
QALCIND  DS    A                                                                
QALUPG   DS    A                   A(UPGRADE FORMULA)                           
QALRATE  DS    A                   A(EFFECTIVE RATES)                           
QALEDEM  DS    A                   A(ESTIMATE DEMOS)                            
*                                                                               
QALAAUID DS    A                   A(AUTO-AVAIL UUID)                           
QALHIAT  DS    A                   A(HIATUS PERIODS)                            
QALORB   DS    A                   A(ORBIT INFO)                                
*                                                                               
QALXDEM  DS    A                   A(EXTRA DEMOS)                               
QALSDEM  DS    A                   A(ESTIMATE SPILL DEMOS)                      
QALSPOT  DS    A                   A(SPOT CHANGE)                               
*                                                                               
QLDMKT   DS    XL2                 SPILL MKT                                    
QLBOOK   DS    XL2                                                              
QLBKTYP  DS    CL1                                                              
QLSDATE  DS    XL2                                                              
*                                                                               
QLSWDATE DS    XL2                                                              
QLSWNOWK DS    PL2                                                              
*                                                                               
QLPIND   DS    XL1                 PACKAGE TYPE INDICATOR                       
QALPKG   DS    A                   A(PACKAGE INFO)                              
*                                                                               
QSDMFRST DS    X                                                                
QLINEX   EQU   *                                                                
                                                                                
SAVEDX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* INCLUDED DSECTS FOLLOW                                                        
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
*                                                                               
B#B3IO1  EQU   3                   IO1                                          
B#AGYREC EQU   3                   - AGENCY RECORD                              
B#B4IO2  EQU   4                   IO2 -                                        
B#CLTREC EQU   4                   - CLIENT RECORD                              
B#B5IO3  EQU   5                   IO3 -                                        
B#ESTREC EQU   5                   - ESTIMATE RECORD                            
AESTREC  EQU   LP_BLKS+((B#ESTREC-1)*L'LP_BLKS)                                 
B#B6IO4  EQU   6                   IO4 -                                        
B#B7IO5  EQU   7                   IO5 -                                        
B#REVREC EQU   7                   - REVISION RECORDS                           
AREVREC  EQU   LP_BLKS+((B#REVREC-1)*L'LP_BLKS)                                 
B#DEVREC EQU   7                   - DEVIATED RECORDS                           
ADEVREC  EQU   LP_BLKS+((B#DEVREC-1)*L'LP_BLKS)                                 
B#B8IO6  EQU   8                   IO6 -                                        
B#REVREP EQU   8                                                                
AREVREP  EQU   LP_BLKS+((B#REVREP-1)*L'LP_BLKS)                                 
B#B9IO7  EQU   9                   IO7 -                                        
B#SA0REC EQU   9                   - CONTROL PERSON PASSWORD RECORD             
ASA0REC  EQU   LP_BLKS+((B#SA0REC-1)*L'LP_BLKS)                                 
B#B10IO8 EQU   10                  IO8 -                                        
B#SVRDEF EQU   12                  SERVER SPLNK22                               
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   14                  LP_D                                         
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPGENDREV                                                      
       ++INCLUDE SPGENDDEV                                                      
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPSTAPACKD                                                     
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
                                                                                
WORKD    DSECT                                                                  
         ORG   OVERWORK            REDEFINE 1K OVERLAY WORKING STORAGE          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPLNK1A   11/03/20'                                      
         END                                                                    
