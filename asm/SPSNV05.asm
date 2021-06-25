*          DATA SET SPSNV05    AT LEVEL 097 AS OF 10/05/16                      
*PHASE T21005A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21005 - MOVE/COPY INVOICE                                            
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T21000), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     SPSNVFA (T210FA) -- MOVE/COPY SCREEN                            
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T21005   TITLE 'SPSNV05 - SPOT INVOICE MOVE/COPY OVERLAY'                       
T21005   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*T21005*,R7,RR=R3                                    
         LR    R6,RC                                                            
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         ST    R6,ATEMPIO                                                       
         AHI   R6,SPCLTABL-WORKD                                                
         ST    R6,ASPCLTBL                                                      
         AHI   R6,BUFFER-SPCLTABL                                               
         ST    R6,ABUFFER                                                       
*                                                                               
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         BRAS  RE,SETPFTBL                                                      
*                                                                               
         L     R1,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(R1)                                   
         MVC   VPARSNIP,CPARSNIP-COMFACSD(R1)                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'INV',3,GLVPGM                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VREC                                                             
*                                                                               
MAINX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         MVI   MISCFLG1,0                                                       
*                                                                               
         CLI   CALLSP,1                                                         
         BNE   *+14                                                             
         MVC   MOVPFLN(11),=CL11'PF12=Return'                                   
         B     *+10                                                             
         MVC   MOVPFLN(11),=CL11'PF12=List'                                     
         OI    MOVPFLNH+6,X'80'                                                 
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VKMED00  DS    0H                                                               
         LA    R2,MOVMEDH                                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   VKMED10                                                          
         OI    MISCFLG1,MF1MVCHG                                                
         NI    MOVFCLTH+4,X'FF'-X'20'                                           
         NI    MOVFSTAH+4,X'FF'-X'20'                                           
         NI    MOVFPRDH+4,X'FF'-X'20'                                           
         NI    MOVFESTH+4,X'FF'-X'20'                                           
         NI    MOVFPERH+4,X'FF'-X'20'                                           
         NI    MOVFINVH+4,X'FF'-X'20'                                           
         NI    MOVTCLTH+4,X'FF'-X'20'                                           
         NI    MOVTPRDH+4,X'FF'-X'20'                                           
         NI    MOVTESTH+4,X'FF'-X'20'                                           
         NI    MOVTSTAH+4,X'FF'-X'20'                                           
         NI    MOVTINVH+4,X'FF'-X'20'                                           
*                                                                               
VKMED10  CLI   5(R2),0                                                          
         BNE   VKMED20                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
         CLI   5(R2),0                                                          
         BE    NEEDMDIA                                                         
*                                                                               
VKMED20  GOTO1 VALIMED                                                          
         MVC   MOVMDNM(L'MEDNM),MEDNM       SHOW MEDIA NAME                     
         OI    MOVMDNMH+6,X'80'                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
*                                                                               
VKMEDX   OI    4(R2),X'20'                                                      
***********************************************************************         
* 'FROM' SECTION OF THE SCREEN                                                  
***********************************************************************         
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VKFCL00  DS    0H                                                               
         LA    R2,MOVFCLTH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKFCL10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPCLT                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKFCL10  GOTO1 VALICLT                                                          
         MVC   MOVFCLN,CLTNM       SHOW CLIENT NAME                             
         OI    MOVFCLNH+6,X'80'                                                 
*                                                                               
         L     R0,AIO1             SAVE 'FROM' CLIENT RECORD                    
         LA    R1,LIOS                                                          
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BRAS  RE,GETPROFS                                                      
*                                                                               
VKFCLX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
         MVC   SVFBCLT,BCLT                                                     
         MVC   SVFQCLT,QCLT                                                     
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE PRODUCT                                                          
*****                                                                           
VKFPRD00 DS    0H                                                               
         LA    R2,MOVFPRDH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKFEST00                                                         
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPRD                            
         CLI   5(R2),0                                                          
         BE    VKFEST00                                                         
         OI    MOVFPRDH+4,X'20'         VALIDATED                               
*****                                                                           
* VALIDATE THE ESTIMATE                                                         
*****                                                                           
VKFEST00 DS    0H                                                               
         LA    R2,MOVFESTH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKFST00                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPEST                            
         CLI   5(R2),0                                                          
         BE    VKFST00                                                          
         OI    MOVFESTH+4,X'20'         VALIDATED                               
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKFST00  DS    0H                                                               
         LA    R2,MOVFSTAH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKFST10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPSTA                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKFST10  GOTO1 VALISTA                                                          
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   *+12                                                             
         CLI   QSTA+4,C'C'                                                      
         BE    VKFST15                                                          
*                                                                               
         CLI   QSTA+4,C'S'                                                      
         BE    *+12                                                             
         CLI   QSTA+4,C'D'                                                      
         BNE   *+8                                                              
*                                                                               
VKFST15  DS    0H                                                               
         OI    MISCFLG1,MF1FSTDQ                                                
*                                                                               
         MVC   SVFQMKT,QMKT                                                     
         MVC   SVFQSTA,QSTA                                                     
         MVC   SVFQSUBM,QSUBMED                                                 
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   VKFST20                                                          
         DROP  R1                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2Z',PROFI2Z    !!! lowercase 's' !!!          
         CLI   PROFI2Z+6,C'Y'     READ PROFILES BY SUBMEDIA?                    
         BNE   VKFST20                                                          
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         CLI   STYPE,C' '                                                       
         BNH   VKFST20                                                          
         CLC   QMED,STYPE                                                       
         BE    VKFST20                                                          
         MVC   SVMEDIA,QMED                                                     
         MVC   QMED,STYPE          GET SUBMEDIA                                 
         GOTO1 GETPRFIL,DMCB,=C'S0I2',FPROF0I2                                  
         GOTO1 (RF),(R1),=C'sI2X',FPROFI2X   <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'S0TI',FPROF0TI                                      
         MVC   QMED,SVMEDIA                                                     
         DROP  R1                                                               
*                                                                               
VKFST20  DS    0H                                                               
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,SPACES                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   MOVFSTN,MKTNM       SHOW STATION NAME                            
         OI    MOVFSTNH+6,X'80'                                                 
*                                                                               
VKFSTX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                            
         MVC   SVFBMSTA,BMKTSTA                                                 
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE PERIOD                                                           
*****                                                                           
VKFPR00  DS    0H                                                               
         LA    R2,MOVFPERH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKFPR10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPER                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKFPR10  GOTO1 PERVAL,DMCB,(5(R2),8(R2)),PERVALST                               
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
*                                                                               
VKFPVALD USING PERVALD,PERVALST                                                 
* WAS THE START DAY OR ANY PART OF THE END DAY ENTERED?                         
         TM    VKFPVALD.PVALASSM,PVALASD+PVALAED+PVALAEM+PVALAEY                
         BNO   BADDTFMT                WITHOUT THE DAY                          
*                                                                               
         MVC   BMOSS,VKFPVALD.PVALCSTA     COMPRESSED DATES                     
         MVC   BMOSE,VKFPVALD.PVALCEND                                          
*                                                                               
         MVC   VKFPVALD.PVALESTA+4(2),=C'15'                                    
         GOTO1 GETBROAD,DMCB,(1,VKFPVALD.PVALESTA),BRDDATES,           X        
               GETDAY,ADDAY                                                     
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                DATE SHOULDN'T BE INVALID                    
         DROP  VKFPVALD                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(X'10',BRDDATES),(2,WORK)                            
         MVC   BRDCSDAT,WORK                                                    
         MVC   BRDCEDAT,WORK+3                                                  
*                                                                               
VKFPRX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPPER                            
         OI    4(R2),X'20'                                                      
*                                                                               
         XC    WORK,WORK           SHOW PERIOD DATES                            
         MVC   WORK(L'BMOSS+L'BMOSE),BMOSS                                      
         CLI   FP0I2BC,C'C'        CALENDAR MONTHS?                             
         BE    *+10                                                             
         MVC   WORK(L'BRDCSDAT+L'BRDCEDAT),BRDCSDAT                             
*                                                                               
         OI    MOVFPDNH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(X'12',WORK),(8,MOVFPDN)                             
         GOTO1 (RF),(R1),(2,WORK),(0,PERDSYMD)                                  
         GOTO1 (RF),(R1),(2,WORK+L'BMOSS),(0,PERDEYMD)                          
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKFIN00  DS    0H                                                               
         LA    R2,MOVFINVH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         MVC   QINVOICE,8(R2)                                                   
         OC    QINVOICE,SPACES                                                  
         MVC   SVFINVCE,QINVOICE                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING SNVKEY,R3                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         MVC   SNVKMOS,BMOSS                                                    
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,QINVOICE                                                 
         MVC   SNVKMINK,=6X'FF'                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),INVDIR,KEY,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVKMAST,KEY        RECORD EXISTS?                               
         BNE   RECNTFND                                                         
         MVC   SVMASTKY(L'SNVKMAST),SNVKMAST                                    
         DROP  R6                                                               
*                                                                               
         BRAS  RE,GETEST                                                        
         BE    *+12                                                             
         LA    R2,MOVFESTH                                                      
         B     BADESTM                                                          
*                                                                               
         BRAS  RE,CHKEST                                                        
         BE    VKFIN50                                                          
         LA    R2,MOVFINVH                                                      
         MVC   GERROR,=AL2(1225)   INVOICE PAID                                 
         CLI   BYTE,X'01'                                                       
         BE    *+10                                                             
         MVC   GERROR,=AL2(1224)   ESTIMATE LOCKED                              
         B     ERREXIT                                                          
*                                                                               
VKFIN50  DS    0H                                                               
         BRAS  RE,TSTLOCK                                                       
         BE    VKFINX                                                           
         MVC   GERROR,=AL2(808)                                                 
         LA    R2,MOVOKAYH                                                      
         B     ERREXIT                                                          
*                                                                               
VKFINX   OI    4(R2),X'20'                                                      
*                                                                               
VKX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORDS                                                          
***********************************************************************         
VREC     DS    0H                                                               
***********************************************************************         
* 'TO' SECTION OF THE SCREEN                                                    
***********************************************************************         
*****                                                                           
* VALIDATE THE CLIENT & PRODUCTS                                                
*****                                                                           
VRTCL00  DS    0H                                                               
         LA    R2,MOVTCLTH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    NEEDMDIA            PLEASE ENTER FIELDS AS REQUIRED              
         LA    R2,MOVTCLTH                                                      
         GOTO1 VALICLT                                                          
         MVC   SVTBCLT,BCLT                                                     
         MVC   SVTQCLT,QCLT                                                     
         MVC   SVTCOPT4,SVCOPT4                                                 
*                                                                               
         L     R0,AIO1             SAVE 'TO' CLIENT RECORD                      
         LA    R1,LIOS                                                          
         L     RE,ATEMPIO                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'S0I2',TPROF0I2                                  
* MOVED DOWN, AFTER STAVAL CODE                                                 
*        CLC   FP0I2BC(1),TP0I2BC  INCOMPATIBLE CLIENTS IF NOT THE              
*        BNE   INCMPMON            SAME BROADCAST/CALENDAR PROFILE OPT          
*                                                                               
         GOTO1 (RF),(R1),=C'sI2R',TPROFI2R   <== NOTE LOWER CASE 's'            
         GOTO1 (RF),(R1),=C'sI2X',TPROFI2X   <== NOTE LOWER CASE 's'            
         GOTO1 (RF),(R1),=C'sI2Y',TPROFI2Y   <== NOTE LOWER CASE 's'            
         GOTO1 (RF),(R1),=C'S0TI',TPROF0TI                                      
*                                                                               
         OI    MOVTCLTH+4,X'20'                                                 
*                                                                               
         MVI   SVTBPRD,0           CLEAR THE 'SENT TO' PRODUCTS                 
         MVI   SVTBPRD2,0                                                       
         MVI   SVTBEST,0           CLEAR ESTIMATE                               
         XC    SVTQPRD,SVTQPRD                                                  
         XC    SVTQPRD2,SVTQPRD2                                                
*                                                                               
VRTPRD00 DS    0H                                                               
         LA    R2,MOVTPRDH                                                      
         CLI   5(R2),0                                                          
         BE    VRTES00                                                          
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         XC    BLOCK(50),BLOCK                                                  
         MVI   BYTE,PSNMVOKQ+PSNNPARQ                                           
         GOTO1 VPARSNIP,DMCB,(R2),(2,BLOCK),(BYTE,PARSEPTR)                     
         CLI   8(R1),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
         MVI   BYTE,1              1ST PRODUCT                                  
         LA    R3,BLOCK                                                         
         USING PSND,R3                                                          
*                                                                               
VRTPRD10 DS    0H                                                               
         L     R1,ATIOB            SET CURSOR FOR ERRORS TO POINT TO            
         USING TIOBD,R1              THE 'TO' PRODUCT                           
         MVI   TIOBCURI,0                                                       
         OI    TIOBINDS,TIOBSETC                                                
         LA    R0,MOVTPRDH         FIELD HEADER                                 
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         L     R0,PSNCOMP          ADDRESS OF FIELD                             
         LA    RE,MOVTPRD                                                       
         SR    R0,RE               DISPLCMENT INTO FLD OF PRD                   
         STC   R0,TIOBCURI                                                      
         DROP  R1                                                               
*                                                                               
         LA    R2,FAKEFLDH         VALIDATE THE PRODUCT                         
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         L     R1,PSNCOMP          ADDRESS OF COMPONENT                         
         ZIC   RE,PSNLEN           LENGTH OF COMPONENT                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),0(R1)                                                 
         MVC   5(1,R2),PSNLEN      STORE INPUT LENGTH IN FIELD HEADER           
*                                                                               
         CLI   BYTE,1              1ST PRODUCT?                                 
         BNE   VRTPRD20            NO - 2ND                                     
*                                                                               
         MVC   SVTQPRD,SPACES                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVTQPRD(0),0(R1)    SAVE EBCDIC PRODUCT                          
         B     VRTPRD30                                                         
*                                                                               
VRTPRD20 MVC   SVTQPRD2,SPACES                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVTQPRD2(0),0(R1)                                                
*                                                                               
VRTPRD30 GOTO1 VALIPRD                                                          
*                                                                               
         CLI   MOVTPRN,0           IS THERE ALREADY A NAME SHOWN?               
         BNE   *+14                                                             
         MVC   MOVTPRN,PRDNM       NO-SHOW 1ST PRODUCT NAME                     
         OI    MOVTPRNH+6,X'80'                                                 
*                                                                               
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   SVTBPRD,BPRD                                                     
         B     *+10                                                             
         MVC   SVTBPRD2,BPRD                                                    
*                                                                               
         MVI   BYTE,2              INDICATE 2ND PRODUCT                         
         LA    R3,PSNL(R3)         GO TO NEXT ENTRY                             
         CLI   PSNTAG,0            ANY MORE COMPONENTS                          
         BNE   VRTPRD10                                                         
         DROP  R3                                                               
*                                                                               
VRTPRD40 OI    MOVTPRDH+4,X'20'                                                 
*****                                                                           
* VALIDATE THE ESTIMATE                                                         
*****                                                                           
VRTES00  DS    0H                                                               
         L     RF,ATIOB            POSITION CURSOR REGULAR WAY                  
         USING TIOBD,RF                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  RF                                                               
*                                                                               
         OC    MOVTEST,MOVTEST                                                  
         BZ    VRTESX                                                           
         LA    R2,MOVTESTH                                                      
*                                                                               
         TM    4(R2),X'20'         VALIDATED?                                   
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         MVC   BCLT,SVTBCLT        VALIDATE ESTIMATE FOR THIS CLT/PRD           
         MVC   QCLT,SVTQCLT        VALIDATE ESTIMATE FOR THIS CLT/PRD           
         MVC   QPRD,SVTQPRD                                                     
         GOTO1 VALIEST                                                          
*                                                                               
VRTES10  MVC   SVTBEST,BEST                                                     
         CLI   SVTQPRD2,C' '                                                    
         BNH   VRTESX                                                           
         CLI   SVTBPRD2,0          IS THERE A PIGGY BACK                        
         BE    VRTESX              NO, SO DONE                                  
         MVC   QPRD,SVTQPRD2                                                    
         LA    R2,MOVTESTH                                                      
         GOTO1 VALIEST                                                          
*                                                                               
VRTESX   DS    0H                                                               
         MVI   ERROPT,0            RESET                                        
         L     R1,ATIOB            DON'T SET CURSOR                             
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         OI    MOVTESTH+4,X'20'    VALIDATED                                    
         DROP  R1                                                               
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VRTST00  DS    0H                                                               
         LA    R2,MOVTSTAH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VRTST10  GOTO1 VALISTA                                                          
         MVC   SVTSFLG1,SVSFLAG1                                                
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   *+12                                                             
         CLI   QSTA+4,C'C'                                                      
         BE    VRTST12                                                          
*                                                                               
         CLI   QSTA+4,C'S'                                                      
         BE    *+12                                                             
         CLI   QSTA+4,C'D'                                                      
         BNE   *+8                                                              
VRTST12  OI    MISCFLG1,MF1TSTDQ                                                
*                                                                               
         TM    MISCFLG1,MF1FSTDQ+MF1TSTDQ                                       
         BM    INVLFLD                                                          
*                                                                               
         TM    SVTSFLG1,STPG                                                    
         BZ    *+12                                                             
         TM    SVTCOPT4,COP4PG                                                  
         BZ    CLTPGERR                                                         
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   VRTST15                                                          
         DROP  R1                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2Z',PROFI2Z    !!! lowercase 's' !!!          
         CLI   PROFI2Z+6,C'Y'     READ PROFILES BY SUBMEDIA?                    
         BNE   VRTST15                                                          
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         CLI   STYPE,C' '                                                       
         BNH   VRTST15                                                          
         CLC   QMED,STYPE                                                       
         BE    VRTST15                                                          
         MVC   SVMEDIA,QMED                                                     
         MVC   QMED,STYPE          GET SUBMEDIA                                 
         GOTO1 GETPRFIL,DMCB,=C'S0I2',TPROF0I2                                  
         GOTO1 (RF),(R1),=C'sI2X',FPROFI2X   <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'S0TI',FPROF0TI                                      
         MVC   QMED,SVMEDIA                                                     
         DROP  R1                                                               
*                                                                               
         CLC   FP0I2BC(1),TP0I2BC  INCOMPATIBLE CLIENTS IF NOT THE              
         BNE   INCMPMON            SAME BROADCAST/CALENDAR PROFILE OPT          
*                                                                               
*                                                                               
VRTST15  DS    0H                                                               
         XC    FRMTONWK(FRM2NWKX-FRMTONWK),FRMTONWK                             
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,SPACES                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   MOVTSTN,MKTNM       SHOW STATION NAME                            
         OI    MOVTSTNH+6,X'80'                                                 
*                                                                               
VRTST20  MVC   SVTBMSTA,BMKTSTA                                                 
*                                                                               
         CLI   SVFBMSTA+2,X'E8'    'FROM' CABLE STATION?                        
         BNL   VRTST30                                                          
*********                                                                       
* 'FROM' REGULAR STATION                                                        
*********                                                                       
         CLI   SVTBMSTA+2,X'E8'    'TO' CABLE STATION?                          
         BL    VRTSTX              NO, 'TO' REGULAR STATION                     
         B     CNTMIXST                 CAN'T MIX STATION TYPES                 
*********                                                                       
* 'FROM' CABLE STATION                                                          
*********                                                                       
VRTST30  CLI   SVTBMSTA+2,X'E8'    'TO' CABLE STATION?                          
         BL    CNTMIXST            NO, CAN'T MIX STATION TYPES                  
         MVC   MOVTSTA(4),QSTA                                                  
         OI    MISCFLG1,MF1CBLNW   NEED TO CHECK NETWORK AGAINST 'TO'           
*                                                                               
VRTSTX   OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VRTIN00  DS    0H                                                               
         LA    R2,MOVTINVH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1MVCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
* NO PERIODS OR COMMAS IN INVOICE NAME                                          
*                                                                               
         LLC   RF,5(R2)                                                         
         LA    RE,8(R2)                                                         
*                                                                               
VRTIN05  CLI   0(RE),C'.'                                                       
         BE    INVLFLD                                                          
         CLI   0(RE),C','                                                       
         BE    INVLFLD                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VRTIN05                                                       
*                                                                               
         MVC   QINVOICE,8(R2)                                                   
         OC    QINVOICE,SPACES                                                  
         MVC   SVTINVCE,QINVOICE                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING SNVKEY,R3                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         MVC   SNVKMOS,BMOSS                                                    
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,QINVOICE                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=CL8'DMRDHI'),INVDIR,KEY,AIO                 
         CLI   DMCB+8,2                                                         
         BNE   VRTIN10                                                          
         L     R6,AIO                                                           
         CLC   0(L'SNVKMAST,R6),KEY                                             
         BE    RECDLTED            RECORD IS DELETED                            
         B     VRTIN20                                                          
*                                                                               
VRTIN10  CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVKMAST,KEY        RECORD ALREADY EXISTS?                       
         BE    RECXISTS                                                         
         DROP  R6                                                               
*                                                                               
VRTIN20  BRAS  RE,TSTLOCK                                                       
         BE    VRTINX                                                           
         MVC   GERROR,=AL2(808)                                                 
         LA    R2,MOVOKAYH                                                      
         B     ERREXIT                                                          
*                                                                               
VRTINX   OI    4(R2),X'20'                                                      
***********************************************************************         
* VALIDATE OKAY FIELD                                                           
***********************************************************************         
VRMOV00  LA    R2,MOVOKAYH                                                      
*                                                                               
         TM    MISCFLG1,MF1MVCHG                                                
         BZ    VRMOV10                                                          
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0             CLEAR IT                                     
         MVI   8(R2),0                                                          
         B     MISSFLD                                                          
*                                                                               
VRMOV10  CLI   5(R2),0             NOTHING HERE?                                
         BE    MISSFLD                                                          
         CLI   8(R2),C'N'                                                       
         BE    VRX                                                              
         CLI   8(R2),C'Y'          OKAY?                                        
         BNE   INVLFLD                                                          
*                                                                               
***********************************************************************         
* ERROR CHECKING TO MAKE SURE WE CAN COPY THE INVOICE OVER TO THE               
* OTHER CLIENT FROM THE FIRST CLIENT                                            
***********************************************************************         
VRCHK010 L     R0,AIO3             RESTORE 'FROM' CLIENT RECORD TO AIO1         
         LA    R1,LIOS                                                          
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   BCLT,SVFBCLT                                                     
         MVC   QCLT,SVFQCLT                                                     
         MVC   BMKTSTA,SVFBMSTA                                                 
         MVC   QINVOICE,SVFINVCE                                                
         GOTO1 INITMNIO                                                         
*                                                                               
***********************************                                             
* PHASE 0 - NEED TO CHECK THE FILM CODES ARE VALID FOR THE NEW CLIENT           
***********************************                                             
         BAS   RE,READFLMS                                                      
*                                                                               
         CLC   SVFBCLT,SVTBCLT     CHANGE IN THE CLIENT CODE?                   
         BE    VRCHK099            NO, FILM CODES SHOULD BE VALID STILL         
*                                                                               
         L     R3,ABUFFER                                                       
         USING BUFFDSCT,R3                                                      
VRCHK020 CLI   BUFFCODE,0                                                       
         BE    VRCHK099            WENT THROUGH ALL OF THEM                     
*                                                                               
         LA    R1,KEY                                                           
         XC    KEY,KEY                                                          
         USING CMLRECD,R1                                                       
*                                                                               
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVTBCLT                                                  
*                                                                               
         CLI   BUFFCMCD+8,C' '     CMML = AD-ID?                                
         BH    VRCHK022            YES                                          
*                                                                               
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKCML,BUFFCMCD                                                 
         B     VRCHK023                                                         
*                                                                               
VRCHK022 DS    0H                                                               
         MVC   CMLKID,=X'0AC1'     AD-ID                                        
         TM    BUFFFLAG,SNVCMHDQ   HI-DEF COMMERCIAL?                           
         BZ    *+10                NO                                           
         MVC   CMLKID,=X'0AC2'     YES, READ HD PP                              
         DROP  R1                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE' TRPACK                                    
         GOTO1 CALLOV,DMCB                                                      
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            ADDRESS OF TRPACK                            
*                                                                               
         GOTO1 (RF),DMCB,(C'P',BUFFCMCD),WORK                                   
         BNE   VRCHK024                                                         
         MVC   KEY+CMLKCML-CMLKEY(L'CMLKCML),WORK                               
*                                                                               
VRCHK023 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'TRFDIR',KEY,KEY,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'CMLKEY),KEYSAVE                                            
         BNE   VRCHK024                                                         
*                                                                               
         CLC   =X'0A21',KEY                                                     
         BNE   VRCHK025                                                         
         TM    KEY+CMLKSTAT-CMLKEY,CMLKSTA_PCKD                                 
         BZ    VRCHK025                                                         
*                                                                               
VRCHK024 DS    0H                                                               
         CLI   TP0TIIFC,C'Y'       ACCEPT INVALID FILM CODES?                   
         BNE   *+14                                                             
         XC    BUFFSEQN,BUFFSEQN   YES, NO COMML SEQUENCE NUMBER THEN           
         B     VRCHK080                                                         
*                                                                               
         LA    R1,BUFFCMCD                                                      
         ICM   R1,8,=AL1(L'BUFFCMCD)                                            
         BRAS  RE,FINDLEN                                                       
         CHI   RF,0                                                             
         BH    *+8                                                              
         LHI   RF,8                SOMETHING IS WRONG - DEFAULT TO 8            
*                                                                               
         AHI   RF,1                L'COMML+1                                    
         STC   RF,BLOCK                                                         
         LA    RE,BLOCK(RF)                                                     
         MVI   0(RE),4                                                          
         MVC   1(3,RE),MOVTCLT                                                  
         MVI   4(RE),0             TERMINATING 0                                
*                                                                               
         BCTR  RF,0                RF = L'COMML CODE                            
         BCTR  RF,0                RF = L'COMML CODE - 1                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+1(0),BUFFCMCD                                              
*                                                                               
         LA    R2,MOVTCLTH                                                      
         B     FILMCLT2            FILM &1 DOES NOT EXIST FOR CLIENT &2         
*                                                                               
VRCHK025 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'TRFFILE',                X        
               KEY+14,AIO1,(0(RA),DMWORK)                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO1             FIND THE COMMERCIAL DATA ELEMENT             
         AHI   RE,CMLDTAEL-CMLKEY                                               
VRCHK030 CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF THERE ISN'T ONE                       
         CLI   0(RE),X'10'                                                      
         BE    VRCHK040                                                         
         XR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     VRCHK030                                                         
*                                                                               
         USING CMLDTAEL,RE                                                      
VRCHK040 MVC   BUFFSEQN,CMLSEQ+1   SET COMMERCIAL SEQ NO.                       
         DROP  RE                                                               
*                                                                               
VRCHK080 LA    R3,BUFFNEXT         CHECK ALL THE FILM CODES                     
         B     VRCHK020                                                         
*                                                                               
VRCHK099 L     R0,AIO3             RESTORE 'FROM' CLIENT RECORD TO AIO1         
         LA    R1,LIOS               IN CASE AIO1 GOT OVERWRITTEN               
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         DROP  R3                                                               
*                                                                               
***********************************                                             
* PHASE 1 - NEED TO CHECK NETWORK ALSO EXISTS FOR THE 'TO' CABLE/               
*           STATION                                                             
***********************************                                             
VRCHK100 TM    MISCFLG1,MF1CBLNW   NEED TO CHECK NETWORKS?                      
         BZ    VRCHK200            NO                                           
*                                                                               
         CLC   SVFBMSTA+2(3),SVTBMSTA+2   SAME CABLE STATION?                   
         BE    VRCHK200            YES                                          
*                                                                               
         XC    MINEKEY,MINEKEY     GET AN INVOICE DETAIL                        
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0            GOT ONE?                                     
         BE    VRCHK110                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    VRCHK200                                                         
         DC    H'0'                                                             
*                                                                               
VRCHK110 L     R6,MINELEM                                                       
         CLI   0(R6),SNVIDELQ      MAKE SURE WE HAVE A DETAIL                   
         BNE   VRCHK200                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
* LET'S SEE IF THIS FROM NETWORK IS IN OUR FRMTONWK TABLE                       
*                                                                               
         LA    R1,FRMTONWK         NOT SURE IF TOP 24 NWK IN 'TO' STA           
         LA    RE,FRM2NWKX         DON'T WANT TO BLOW THE STUFF AFTER           
VRCHK113 CLI   0(R1),0             NO MORE ENTRIES IN FRMTONWK TABLE?           
         BNE   VRCHK116                                                         
         MVC   0(1,R1),SNVIDNWK    PUT 'FROM' ENTRY IN                          
         LR    R0,R1               SAVE OFF WHERE WE'RE GOING TO STORE          
         B     VRCHK120            AND CALL MSPACK TO GET THE 'TO' CODE         
*                                                                               
VRCHK116 CLC   SNVIDNWK,0(R1)                                                   
         BE    VRCHK130            FOUND ENTRY, GO TO NEXT DETAIL               
         AHI   R1,L'FRMTONWK                                                    
         CR    R1,RE                                                            
         BL    VRCHK113                                                         
         DC    H'0'                                                             
*                                                                               
VRCHK120 MVC   DUB(L'SVFBMSTA),SVFBMSTA                                         
         OC    DUB+L'SVFBMSTA-1(1),SNVIDNWK                                     
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4   GET ALPHA NETWORK          
         MVC   WORK+4(4),MOVTSTA        MOVE IN THE 'TO' STATION                
         MVI   WORK+4+4,C'/'                                                    
         GOTO1 MSPACK,DMCB,WORK,WORK+4,DUB     VALID FOR 'TO' STATION?          
         BNE   VRCHK125                        YES                              
         LR    RE,R0                                                            
         MVC   1(1,RE),DUB+4       COPY THE NETWORK BYTE                        
         NI    1(RE),X'FF'-X'80'    DON'T HAVE TO CALL MSPACK AGAIN             
         B     VRCHK130                        YES                              
*                                                                               
VRCHK125 MVI   BLOCK,4                ERROR, 'TO' STATION DOESN'T HAVE          
         MVC   BLOCK+1(3),WORK+4+5       THIS NETWORK CODE                      
         MVI   BLOCK+4,5                                                        
         MVC   BLOCK+5(4),MOVTSTA                                               
         MVI   BLOCK+9,0                                                        
         LA    R2,MOVTSTAH                                                      
         B     NWKNSTMS                                                         
         DROP  R6                                                               
*                                                                               
VRCHK130 BRAS  RE,MINIOSEQ         CHECK NEXT DETAIL                            
         BE    VRCHK110                                                         
*                                                                               
***********************************                                             
* PHASE 2 - BUILD A TABLE OF PRODUCT-ESTIMATE COMBINATIONS USED IN              
*           'FROM' INVOICE                                                      
***********************************                                             
VRCHK200 L     R0,ASPCLTBL         CLEAR THIS TABLE                             
         L     R1,=A(MAXSPCLS*LENSPCL)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                  SETUP BINSRCH PARAMETERS                     
         GOTO1 ,BINPARMS,,ASPCLTBL,0,LENSPCL,SPCLLKEY,MAXSPCLS                  
*                                                                               
         XC    MINEKEY,MINEKEY     GET THE INVOICE HEADER                       
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         MVI   SVFBPRD,0                                                        
         MVI   SVFBPRD2,0                                                       
         XC    SVFQPRD,SVFQPRD                                                  
         XC    SVFQPRD2,SVFQPRD2                                                
*                                                                               
         CLI   SNVHDLEN,SNVHDLN3   IS IT THE NEW LENGTH WITH ALPHA PRD?         
         BL    VRCHK201             - NOPE, IT'S NOT                            
*                                                                               
* ALPHA PRODUCTS HERE                                                           
         CLI   SNVHDAP1,C' '       DO WE HAVE AN ALPHA PRODUCT?                 
         BNH   VRCHK201             - NOPE!                                     
         MVC   SVFQPRD,SNVHDAP1     - YUP, WE HAVE IT                           
         MVC   SVFQPRD2,SNVHDAP2                                                
         B     VRCHK203                                                         
*                                                                               
* BINARY PRODUCTS HERE                                                          
VRCHK201 CLI   SNVHDPRD,0          PRODUCT IN HEADER?                           
         BNE   VRCHK203                                                         
*                                                                               
         LA    R2,MOVTCLTH         NO, PRODUCTS & ESTIMATES IN DETAILS          
         CLI   SVTQPRD,C' '        ANY PRODUCT HERE?                            
         BH    NOPRDHDR             - YUP THERE IS                              
         CLI   SVTBPRD,0           IF A PRD IS SPECIFIED IN 'SEND TO'           
         BNE   NOPRDHDR              THEN A PRD NEEDS TO BE IN HEADER           
         B     VRCHK205              OF THE 'FROM' INVOICE                      
*                                                                               
VRCHK203 MVC   SVFBPRD,SNVHDPRD    YES, PRODUCT(S) IS FROM HEADER               
         MVC   SVFBPRD2,SNVHDPR2                                                
*                                                                               
VRCHK204 CLI   SNVHDEST,0          ESTIMATE IN HEADER ALSO?                     
         BE    VRCHK205            NO, ESTIMATES ARE IN DETAILS                 
*                                                                               
* PRODUCTS, ESTIMATE IN INVOICE HEADER                                          
*                                                                               
         XC    BINSREC,BINSREC                                                  
         LA    R2,BINSREC                                                       
         USING SPCLNTRY,R2                                                      
*                                                                               
* "FROM" BPRD, QPRD, EST                                                        
         MVC   SPCLBPRD,SVFBPRD                                                 
         MVC   SPCLQPRD,SVFQPRD                                                 
         MVC   SPCLBEST,SNVHDEST                                                
*                                                                               
* "TO" BPRD, QPRD, EST                                                          
         MVC   SPCLBTPR,SVTBPRD                                                 
         MVC   SPCLQTPR,SVTQPRD                                                 
         MVC   SPCLBTES,SVTBEST                                                 
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         CLI   SVTQPRD2,C' '       GOT A PIGGYBACK PRODUCT?                     
         BH    *+12                YES                                          
         CLI   SVTBPRD2,0          GOT A PIGGYBACK PRODUCT?                     
         BE    VRCHK300            NO, GO TO PHASE 3                            
*                                                                               
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLQPRD,SVFQPRD2                                                
         MVC   SPCLBEST,SNVHDEST                                                
*                                                                               
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
         MVC   SPCLBTES,SVTBEST                                                 
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         B     VRCHK300            GO TO PHASE 3                                
         DROP  R2,R6                                                            
*                                                                               
***************                                                                 
* ESTIMATES ARE IN THE DETAILS (IF ANY DETAILS)                                 
* PRODUCTS ARE IN THE DETAILS (ONLY IF SVFBPRD=0), OR IN THE HEADER             
***************                                                                 
VRCHK205 LA    R2,MOVTCLTH                                                      
         CLI   SVTBEST,0                                                        
         BNE   NOESTHDR                                                         
*                                                                               
         XC    MINEKEY,MINEKEY     ANY DETAILS FOR THIS INVOICE?                
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
*                                                                               
         CLI   MINERR,0            GOT SOMETHING BACK FROM MINIOHI?             
         BE    VRCHK210                                                         
         CLI   MINERR,MINEEOF      NO, HIT END OF RECORD?                       
         BE    VRCHK220                YES, NO DETAILS FOR INVOICE              
*                                                                               
         DC    H'0'                    NO, SOME TERRIBLE ERROR                  
*                                                                               
VRCHK210 L     R6,MINELEM          WE GOT COMETHING BACK FROM MINIOHI           
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ    IS IT A DETAIL ELEMENT?                      
         BE    VRCHK230            YES                                          
*                                                                               
*********                                                                       
* NO DETAILS FOR THIS INVOICE                                                   
*********                                                                       
VRCHK220 DS    0H                                                               
         CLI   SVFQPRD,C' '        DO WE HAVE A CHARACTER HERE?                 
         BH    VRCHK223             - YUP, CONTINUE NORMALLY                    
         CLI   SVFBPRD,0           WAS THERE A PRODUCT CODE IN HEADER?          
         BE    VRCHKX              NO, NO PRODUCT OR ESTIMATES TO CHECK         
*                                                                               
VRCHK223 XC    BINSREC,BINSREC     YES, STILL NEED TO CHECK IF THE PRD          
         LA    R2,BINSREC             IS IN THE 'TO' CLIENT                     
         USING SPCLNTRY,R2                                                      
*                                                                               
         MVC   SPCLBPRD,SVFBPRD                                                 
         MVC   SPCLQPRD,SVFQPRD                                                 
         MVC   SPCLBTPR,SVTBPRD                                                 
         MVC   SPCLQTPR,SVTQPRD                                                 
* ESTIMATES ARE NOT FILLED IN - THEY'RE IN DETAILS                              
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         CLI   SVTBPRD2,0          GOT A PIGGYBACK PRODUCT?                     
         BE    VRCHK300            NO, GO TO PHASE 3                            
*                                                                               
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLQPRD,SVFQPRD2                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   VRCHK300            GO TO PHASE 3                                
         DC    H'0'                DIE IF TABLE IS FULL                         
         DROP  R2                                                               
*                                                                               
*********                                                                       
* WE HAVE AT LEAST ONE DETAIL FOR THIS INVOICE                                  
*********                                                                       
VRCHK230 XC    BINSREC,BINSREC     SET UP BINSRCH RECORD                        
         LA    R2,BINSREC                                                       
         USING SPCLNTRY,R2                                                      
*                                                                               
         CLI   SVFQPRD,C' '        DO WE HAVE A CHARACTER HERE?                 
         BH    VRCHK233             - YUP, CONTINUE NORMALLY                    
*                                                                               
         CLI   SNVIDLEN,SNVIDL3Q   IS IT NEW LENGTH WITH ALPHA PRD?             
         BL    VRCHK231                                                         
         CLI   SNVIDAP1,C' '       DO WE HAVE AN ALPHA PRODUCT?                 
         BNH   VRCHK231             - NOPE!                                     
         MVC   SPCLQPRD,SNVIDAP1    - YUP, WE HAVE IT                           
**                                                                              
         MVC   SPCLBPRD,SVFBPRD    SPECIAL CASE                                 
         CLI   SVFBPRD,0                                                        
         BNE   *+10                                                             
         MVC   SPCLBPRD,SNVIDPRD                                                
         B     VRCHK234                                                         
*                                                                               
VRCHK231 CLI   SVFBPRD,0           DID HEADER HAVE THE PRODUCT?                 
         BE    VRCHK235                                                         
*                                                                               
VRCHK233 DS    0H                                                               
         MVC   SPCLBPRD,SVFBPRD                                                 
****                                                                            
         MVC   SPCLQPRD,SVFQPRD                                                 
****                                                                            
VRCHK234 MVC   SPCLBTPR,SVTBPRD                                                 
         MVC   SPCLQTPR,SVTQPRD                                                 
         B     VRCHK236                                                         
*                                                                               
VRCHK235 MVC   SPCLBPRD,SNVIDPRD                                                
***  WE NEED TO SUPPLY THE QPRD (OLD DETAIL LENGTH)                             
         GOTO1 =A(CHKBPRD),DMCB,SPCLBPRD,SPCLQPRD,RR=RELO                       
*                                                                               
VRCHK236 CLI   SNVIDEST,0          ANY ESTIMATE?                                
         BNE   VRCHK240            YES                                          
*                                                                               
*********                                                                       
* ESTIMATE IS NOT IN THE DETAIL                                                 
*********                                                                       
         CLI   TPI2XERQ,C'Y'       IS ESTIMATE REQUIRED FOR 'TO' CLT?           
         BE    ESTREQRD            YES, ESTIMATE NOT FOUND IN INVOICE           
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         XC    BINSREC,BINSREC     CLEAR THIS OUT                               
*                                                                               
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   SVFQPRD,C' '        DO WE HAVE A CHARACTER HERE?                 
         BH    VRCHK237             - YUP, CONTINUE NORMALLY                    
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   SVFBPRD,0           DID HEADER HAVE THE PRODUCT?                 
         BE    VRCHK238                                                         
VRCHK237 DS    0H                                                               
****                                                                            
         MVC   SPCLQPRD,SVFQPRD2                                                
****                                                                            
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
         B     *+10                                                             
*                                                                               
VRCHK238 MVC   SPCLBPRD,SNVIDPR2                                                
*                                                                               
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   SPCLQPRD,C' '       DO WE HAVE A CHARACTER HERE?                 
         BH    VRCHK239             - YUP, CONTINUE NORMALLY                    
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   SPCLBPRD,0                                                       
         BE    VRCHK250                                                         
*                                                                               
VRCHK239 GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   VRCHK250                                                         
         DC    H'0'                DIE IF TABLE IS FULL                         
*********                                                                       
* ESTIMATE IS IN THE DETAIL                                                     
*********                                                                       
VRCHK240 MVC   SPCLBEST,SNVIDEST                                                
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         XC    BINSREC,BINSREC     LET'S CLEAR THIS                             
*                                                                               
         CLI   SVFQPRD,C' '        DO WE HAVE A CHARACTER HERE?                 
         BH    VRCHK243             - YUP, CONTINUE NORMALLY                    
*                                                                               
         CLI   SVFBPRD,0           DID HEADER HAVE THE PRODUCT?                 
         BE    VRCHK245                                                         
*                                                                               
VRCHK243 DS    0H                                                               
         MVC   SPCLQPRD,SVFQPRD2                                                
*                                                                               
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
         B     *+10                                                             
*                                                                               
VRCHK245 MVC   SPCLBPRD,SNVIDPR2                                                
*                                                                               
         CLI   SPCLQPRD,C' '       DO WE HAVE A CHARACTER HERE?                 
         BH    VRCHK247             - YUP, CONTINUE NORMALLY                    
*                                                                               
         CLI   SPCLBPRD,0                                                       
         BE    VRCHK250                                                         
*                                                                               
VRCHK247 GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   VRCHK250                                                         
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
*********                                                                       
* CHECK NEXT DETAIL IF ANY                                                      
*********                                                                       
VRCHK250 BRAS  RE,MINIOSEQ                                                      
         BNE   VRCHK300                                                         
         CLI   SNVIDEL,SNVIDELQ                                                 
         BE    VRCHK230                                                         
         DROP  R2,R6                                                            
*                                                                               
***********************************                                             
* PHASE 3A - CONVERT THE BINARY PRODUCTS IN OUR TABLE TO EBCDIC                 
*            PRODUCTS FOR THE 'FROM' CLIENT                                     
***********************************                                             
VRCHK300 L     R2,ASPCLTBL                                                      
         USING SPCLNTRY,R2                                                      
*                                                                               
VRCHK310 DS    0H                                                               
         CLI   SPCLQPRD,C' '       DO WE HAVE ANYTHING HERE?                    
         BH    VRCHK320             - YUP, NO NEED TO CONVERT                   
         CLI   0(R2),0             (SPCLBPRD)                                   
         BE    VRCHK350                                                         
*                                                                               
VRCHK315 GOTO1 =A(CHKBPRD),DMCB,SPCLBPRD,SPCLQPRD,RR=RELO                       
         BNE   INVDTLPR                                                         
*                                                                               
VRCHK320 LA    R2,LENSPCL(R2)                                                   
         B     VRCHK310                                                         
*                                                                               
***********************************                                             
* PHASE 3B - CONVERT THE EBCDIC PRODUCTS IN OUR TABLE TO BINARY                 
*            PRODUCTS FOR THE 'TO' CLIENT                                       
***********************************                                             
VRCHK350 L     R0,AIO1                                                          
         LA    R1,LIOS                                                          
         L     RE,ATEMPIO                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY THE 'TO' CLT RECORD TO AIO1             
*                                                                               
         L     R2,ASPCLTBL         START FROM BEGINNING OF OUR TABLE            
*                                                                               
VRCHK360 DS    0H                                                               
         CLI   SPCLQPRD,C' '                                                    
         BNH   VRCHK400            END OF TABLE                                 
         CLI   SPCLQTPR,C' '       HAVE ANYTHING HERE?                          
         BH    VRCHK363            YES - CONVERT IT TO BINARY                   
*                                                                               
VRCHK363 CLI   SPCLBTPR,0          BINARY PRODUCT ALREADY CALCULATED            
         BNE   VRCHK370                                                         
*                                                                               
         CLI   SPCLQTPR,C' '       ANYTHING IN THE "TO" PRD?                    
         BH    *+10                YES                                          
         MVC   SPCLQTPR,SPCLQPRD   NO - WE NEED SOMETHING HERE                  
*                                                                               
         GOTO1 CHKQPRD,DMCB,SPCLQTPR,SPCLBTPR,SVTBCLT                           
         BE    VRCHK370                                                         
*                                                                               
VRCHK367 MVI   BLOCK,4             L'TEXT+1,TEXT                                
         MVC   BLOCK+1(3),SPCLQPRD                                              
         MVI   BLOCK+4,4           L'TEXT+1,TEXT                                
         MVC   BLOCK+5(3),MOVTCLT                                               
         MVI   BLOCK+8,0           TERMINATING 0                                
         LA    R2,MOVTCLTH                                                      
         B     PRODCLT2            PRODUCT &1 DOES NOT EXIST FOR CLIENT         
*                                                                               
VRCHK370 LA    R2,LENSPCL(R2)                                                   
         B     VRCHK360                                                         
         DROP  R2                                                               
*                                                                               
***********************************                                             
* PHASE 4A - GET ESTIMATE START & END DATES FOR THE 'FROM' ESTIMATES            
***********************************                                             
VRCHK400 L     R6,ASPCLTBL                                                      
         USING SPCLNTRY,R6                                                      
*                                                                               
VRCHK410 DS    0H                                                               
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   SPCLQPRD,C' '       ANYTHING HERE?                               
         BH    VRCHK413             - YUP, CERTAINLY                            
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   0(R6),0             NO MORE ENTRIES FOR 'FROM' ESTIMATE          
         BE    VRCHK450                                                         
*                                                                               
VRCHK413 CLI   SPCLBEST,0          NO ESTIMATE FOR THIS ENTRY?                  
         BE    VRCHK420            NONE, SKIP TO NEXT ENTRY                     
*                                                                               
         MVC   BCLT,SVFBCLT        VALIDATE ESTIMATE FOR THIS CLIENT            
         MVC   QCLT,SVFQCLT        VALIDATE ESTIMATE FOR THIS CLIENT            
         MVC   QPRD,SPCLQPRD           AND PRODUCT                              
         LA    R2,FAKEFLDH                                                      
         OI    4(R2),X'08'                                                      
         MVI   5(R2),3                                                          
         XC    8(L'FAKEFLD,R2),8(R2)                                            
         EDIT  (B1,SPCLBEST),(3,8(R2)),FILL=0                                   
         GOTO1 VALIEST                                                          
*                                                                               
         MVC   SPCLESTD,ESTSTRT    COPY THE START AND END DATES FOR             
         MVC   SPCLENDD,ESTEND        THE ESTIMATE                              
*                                                                               
VRCHK420 LA    R6,LENSPCL(R6)                                                   
         B     VRCHK410                                                         
*                                                                               
***********************************                                             
* PHASE 4B - GET ESTIMATE START & END DATES FOR THE 'TO' ESTIMATES              
***********************************                                             
VRCHK450 L     R1,ATIOB            SET CURSOR FOR ERRORS TO POINT TO            
         USING TIOBD,R1              THE 'TO' CLIENT                            
         MVI   TIOBCURI,0                                                       
         OI    TIOBINDS,TIOBSETC                                                
         LA    R0,MOVTCLTH                                                      
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         DROP  R1                                                               
*                                                                               
         L     R6,ASPCLTBL                                                      
VRCHK460 DS    0H                                                               
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   SPCLQPRD,C' '       ANYTHING HERE?                               
         BH    VRCHK461             - YUP, CERTAINLY                            
****  MIGHT BE AN OVERFLOW PRODUCT                                              
         CLI   0(R6),0             NO MORE ENTRIES FOR 'FROM' ESTIMATE          
         BE    VRCHK500                                                         
*                                                                               
VRCHK461 CLI   SPCLBTES,0          'TO' ESTIMATE IN THIS ENTRY?                 
         BNE   VRCHK463            YES, SO USE IT                               
         CLI   SPCLBEST,0          NO ESTIMATE FOR THIS ENTRY?                  
         BE    VRCHK470            NONE, SKIP TO NEXT ENTRY                     
         MVC   SPCLBTES,SPCLBEST   USE THE 'FROM' ESTIMATE                      
*                                                                               
VRCHK463 MVC   BCLT,SVTBCLT        VALIDATE ESTIMATE FOR THIS CLT/PRD           
         MVC   QCLT,SVTQCLT        VALIDATE ESTIMATE FOR THIS CLT/PRD           
         OC    SPCLQTPR,SPCLQTPR   USER ENTERED A 'SEND TO' PRODUCT             
         BNZ   *+10                                                             
         MVC   SPCLQTPR,SPCLQPRD   NO, SO USE SEND FROM ONE                     
         MVC   QPRD,SPCLQTPR                                                    
*                                                                               
         CLI   SPCLBTES,0          USER ENTER A 'TO' ESTIMATE                   
         BNE   *+10                                                             
         MVC   SPCLBTES,SPCLBEST   NO, SO USE 'FROM' ESTIMATE                   
*                                                                               
         LA    R2,FAKEFLDH                                                      
         OI    4(R2),X'08'                                                      
         MVI   5(R2),3                                                          
         XC    8(L'FAKEFLD,R2),8(R2)                                            
         EDIT  (B1,SPCLBTES),(3,8(R2)),FILL=0                                   
         MVI   ERROPT,C'Y'         OPTION TO COME BACK IF ERROR                 
         GOTO1 VALIEST                                                          
         MVI   ERROPT,0            RESET                                        
*                                                                               
         CLC   KEY(L'EKEY),KEYSAVE   DOES THE ESTIMATE EXIST?                   
         BE    VRCHK465              YES                                        
*                                                                               
         MVI   BLOCK,8             L'TEXT+1,TEXT                                
         MVC   BLOCK+1(3),SPCLQTPR                                              
         MVI   BLOCK+4,C'/'                                                     
         EDIT  (B1,SPCLBTES),(3,BLOCK+5),FILL=0                                 
         MVI   BLOCK+8,4           L'TEXT+1,TEXT                                
         MVC   BLOCK+9(3),MOVTCLT                                               
         MVI   BLOCK+12,0                                                       
         B     ESTMCLT2                                                         
*                                                                               
VRCHK465 CLC   SPCLESTD,ESTSTRT    IS 'FROM' ESTIMATE PERIOD A SUBSET           
         BL    *+14                   'TO' ESTIMATE PERIOD?                     
         CLC   SPCLENDD,ESTEND                                                  
         BNH   VRCHK470            YES                                          
*                                                                               
         CLI   TPI2YESQ,C'Y'       OKAY TO HAVE EST DATES OUTSIDE?              
         BE    VRCHK470            YES, SKIP THE RULE                           
*                                                                               
         MVI   BLOCK,8             L'TEXT+1,TEXT                                
         MVC   BLOCK+1(3),SPCLQTPR                                              
         MVI   BLOCK+4,C'/'                                                     
         EDIT  (B1,SVTBEST),(3,BLOCK+5),FILL=0                                  
         MVI   BLOCK+8,0                                                        
         B     CHKESTMS                                                         
*                                                                               
VRCHK470 LA    R6,LENSPCL(R6)                                                   
         B     VRCHK460                                                         
         DROP  R6                                                               
*                                                                               
VRCHK480 L     R1,ATIOB            DON'T SET CURSOR                             
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
VRCHK500 DS    0H                                                               
*                                                                               
VRCHKX   DS    0H                                                               
*                                                                               
***********************************************************************         
* ACTUAL COPY OF 'FROM' INVOICE                                                 
***********************************************************************         
VRCPY00  MVC   BCLT,SVFBCLT                                                     
         MVC   QCLT,SVFQCLT                                                     
         MVC   BMKTSTA,SVFBMSTA                                                 
         MVC   QINVOICE,SVFINVCE                                                
         GOTO1 INITMNIO                                                         
*                                                                               
         XC    MINEKEY,MINEKEY     CLEAR MASTER KEY FOR MINIO                   
         LA    R6,MINEKEY                                                       
         USING SNVKEY,R6                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,SVTBCLT                                                  
         MVC   SNVKSTA,SVTBMSTA+2                                               
         MVC   SNVKMOS,BMOSS                                                    
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,SVTINVCE                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 MINIO,DMCB,('MINCPY',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRCPY15  MVC   BCLT,SVTBCLT                                                     
         MVC   QCLT,SVTQCLT                                                     
         MVC   BMKTSTA,SVTBMSTA                                                 
         MVC   QINVOICE,SVTINVCE                                                
         GOTO1 INITMNIO                                                         
*                                                                               
         CLI   ACTNUM,ACTCOPY      ARE WE COPYING AN INVOICE?                   
         BNE   VRCPY17                                                          
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'F1'       DON'T NEED THE ACTIVITY ELEM AS              
         BRAS  RE,MINIOHI             SNV00 WILL ADD ANOTHER                    
         CLI   MINERR,0                                                         
         BNE   VRCPY17                                                          
         BRAS  RE,MINIODEL                                                      
*                                                                               
VRCPY17  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO HEADER                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   ACTNUM,ACTCOPY      ARE WE COPYING AN INVOICE?                   
         BNE   VRCPY18                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,SNVHDCDT)   COPY CREATED TODAY              
         XC    SNVHDEZS,SNVHDEZS   CLEAR EASI SOURCE                            
         XC    SNVHDEZB,SNVHDEZB   CLEAR EASI BATCH DATE                        
         BRAS  RE,MINIOWRT         WRITE OUT THE NEW HEADER ELEMENT             
*                                                                               
* RE-SET ELEMENT POINTER                                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO HEADER                             
*                                                                               
VRCPY18  DS    0H                                                               
         CLI   SNVHDPRD,0          PRODUCT ENTERED IN THE HEADER?               
         BNE   *+12                 - YUP IT IS                                 
         CLI   SNVHDAP1,C' '       DO WE HAVE THE ALPHA PRODUCT?                
         BNH   VRCPY50              - NO, WE DON'T                              
*                                                                               
***************                                                                 
* PRODUCT IN THE INVOICE HEADER                                                 
***************                                                                 
         XC    BINSREC,BINSREC     YES, CHANGE PRODUCT CODE TO NEW CODE         
         MVC   BINSREC(1),SNVHDPRD                                              
         CLI   SNVHDLEN,SNVHDLN3                                                
         BL    *+10                                                             
         MVC   BINSREC+1(3),SNVHDAP1                                            
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVHDPRD,SPCLBTPR   NEW PRODUCT CODE FOR 'TO' CLIENT             
         MVC   SNVHDAP1,SPCLQTPR   THE EBCDIC PRODUCT CODE                      
         MVI   SNVHDLEN,SNVHDLN3   UPDATE ELEMENT LENGTH JUST IN CASE           
*                                                                               
         CLI   SVTBPRD2,0          ANY PIGGYBACK PRODUCT CODE?                  
         BNE   *+12                NONE, WRITE OUT THE HEADER ELEMENT           
         CLI   SVTQPRD2,C' '       ANY PIGGYBACK PRODUCT CODE?                  
         BNH   VRCPY20             NONE, WRITE OUT THE HEADER ELEMENT           
*                                                                               
         LA    R2,BINSREC                                                       
         XC    BINSREC,BINSREC     YES, CHANGE PIGGY CODE TO NEW CODE           
         MVC   SPCLBPRD,SNVHDPR2   SEARCH FOR NEW PIGGYBACK                     
         CLI   SNVHDLEN,SNVHDLN3                                                
         BL    *+10                                                             
         MVC   SPCLQPRD,SNVHDAP2                                                
         DROP  R2                                                               
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVHDPR2,SPCLBTPR   NEW PIGGY CODE FOR 'TO' CLIENT               
         MVC   SNVHDAP2,SPCLQTPR   PIGGY EBCDIC CODE                            
*                                                                               
VRCPY20  DS    0H                                                               
         CLI   SNVHDEST,0                                                       
         BE    *+10                                                             
         MVC   SNVHDEST,SPCLBTES                                                
*                                                                               
         BRAS  RE,MINIOWRT         WRITE OUT THE NEW HEADER ELEMENT             
*                                                                               
         B     VRCPY75A            PROCEED TO NETWORKS                          
         DROP  R6                                                               
*                                                                               
***************                                                                 
* PRODUCT CODES ARE IN THE DETAILS                                              
***************                                                                 
VRCPY50  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
*                                                                               
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    VRCPY60                                                          
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY75A                                                         
         DC    H'0'                                                             
*                                                                               
VRCPY60  L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ    DETAIL ELEMENT?                              
         BNE   VRCPY75             NO, CHECK THE NEXT ONE                       
*                                                                               
         XC    BINSREC,BINSREC     CHANGE PRODUCT CODE TO NEW CODE              
         MVC   BINSREC(1),SNVIDPRD                                              
         OC    SNVIDAP1,SNVIDAP1                                                
         BNZ   VRCPY65G                                                         
         CLI   SNVIDLEN,SNVIDL2Q   OLD LENGTH?                                  
         BH    VRCPY65G             - NOPE                                      
*                                                                               
         GOTO1 =A(CHKBPRD),DMCB,SNVIDPRD,SNVIDAP1,RR=RELO                       
*                                                                               
VRCPY65G MVC   BINSREC+1(3),SNVIDAP1                                            
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVIDPRD,SPCLBTPR   NEW PRODUCT CODE FOR 'TO' CLIENT             
         MVC   SNVIDAP1,SPCLQTPR   EBCDIC PRODUCT CODE                          
*        MVI   SNVIDLEN,SNVIDL3Q   MOVE IN NEW LENGTH JUST IN CASE              
         DROP  R2                                                               
*                                                                               
         CLI   SNVIDPR2,0          ANY PIGGYBACK PRODUCT CODE?                  
         BE    VRCPY70             NONE, WRITE OUT THE DETAIL ELEMENT           
*                                                                               
         XC    BINSREC,BINSREC     YES, CHANGE PIGGY CODE TO NEW CODE           
         MVC   BINSREC(1),SNVIDPR2                                              
         MVC   BINSREC+1(3),SNVIDAP2                                            
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVIDPR2,SPCLBTPR   NEW PIGGY CODE FOR 'TO' CLIENT               
         MVC   SNVIDAP2,SPCLQTPR   PIGGY EBCDIC PRODUCT CODE                    
         DROP  R2                                                               
*                                                                               
VRCPY70  BRAS  RE,MINIOWRT         WRITE OUT THE DETAIL ELEMENT                 
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),SNVIDDAY                                 
*                                                                               
         BRAS  RE,MINIOHI          RESET ELEMENT POINTER                        
*                                                                               
VRCPY75  BRAS  RE,MINIOSEQ         CHECK NEXT DETAIL                            
         BE    VRCPY60                                                          
*                                                                               
* PROCESS DETAIL ELEMENTS - NETWORKS                                            
*                                                                               
VRCPY75A DS    0H                                                               
         TM    MISCFLG1,MF1CBLNW   CABLE STATION?                               
         BZ    VRCPY80             NO - SKIP NETWORKS                           
         CLC   SVFBMSTA+2(3),SVTBMSTA+2   SAME CABLE STATION?                   
         BE    VRCPY80             YES                                          
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    VRCPY75C                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY80                                                          
         DC    H'0'                                                             
*                                                                               
VRCPY75C L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ    DETAIL ELEMENT?                              
         BNE   VRCPY75H            NO, CHECK THE NEXT ONE                       
*                                                                               
         CLI   SNVIDNWK,24         IS THIS A TOP 24 NETWORK?                    
         BNH   VRCPY75H            YES, SAME NETWORK CODE FOR TOP 24            
*                                                                               
         LA    R1,FRMTONWK         DON'T HAVE TO CALL MSPACK ANYMORE            
VRCPY75F CLC   SNVIDNWK,0(R1)                                                   
         BE    *+12                                                             
         AHI   R1,L'FRMTONWK                                                    
         B     VRCPY75F                                                         
         MVC   SNVIDNWK,1(R1)                                                   
*                                                                               
         BRAS  RE,MINIOWRT         WRITE OUT THE DETAIL ELEMENT                 
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),SNVIDDAY                                 
*                                                                               
         BRAS  RE,MINIOHI          RESET ELEMENT POINTER                        
*                                                                               
VRCPY75H BRAS  RE,MINIOSEQ         CHECK NEXT DETAIL                            
         BE    VRCPY75C                                                         
*                                                                               
* PROCESS COMMERCIAL CODES                                                      
*                                                                               
VRCPY80  XC    MINEKEY,MINEKEY     LOOK FOR THE COMMERCIAL CODES                
         MVI   MINEKEY,SNVCMELQ                                                 
*                                                                               
         BRAS  RE,MINIOHI          RESET ELEMENT POINTER                        
         BNE   VRCPY85                                                          
*                                                                               
VRCPY82  CLI   0(R6),SNVCMELQ                                                   
         BNE   VRCPY82N                                                         
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVCMELD,R6                                                      
         L     RE,ABUFFER                                                       
         USING BUFFDSCT,RE                                                      
VRCPY82L CLI   0(RE),0             SHOULD HAVE THIS FILM IN THE BUFFER          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SNVCMICD,BUFFCODE   GOT OUR FILM?                                
         BE    *+12                                                             
         LA    RE,BUFFNEXT                                                      
         B     VRCPY82L                                                         
*                                                                               
         MVC   SNVCMSEQ,BUFFSEQN                                                
         BRAS  RE,MINIOWRT                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     RE-ESTABLISH THE KEY                         
         MVI   MINEKEY,SNVCMELQ                                                 
         MVC   MINEKEY+1(L'SNVCMICD),SNVCMICD                                   
*                                                                               
         BRAS  RE,MINIOHI          RESET ELEMENT POINTER                        
*                                                                               
VRCPY82N BRAS  RE,MINIOSEQ                                                      
         BE    VRCPY82                                                          
         DROP  RE                                                               
*                                                                               
VRCPY85  LA    R1,MINMKEY                                                       
         USING SNVKEY,R1                                                        
         NI    SNVDSTAT+1,X'FF'-X'80'                                           
         DROP  R1                                                               
*                                                                               
         XC    MINEKEY,MINEKEY     NEED TO GET RID OF THE MM INFO               
         MVI   MINEKEY,SNVIDELQ    LOOK FOR DETAILS FIRST                       
         BRAS  RE,MINIOHI                                                       
VRCPY85A CLI   MINERR,0            GOT ONE?                                     
         BE    *+12                YES                                          
         CLI   MINERR,MINEEOF      END OF FILE?                                 
         BE    VRCPY90             YES, DONE WITH NEW INVOICE                   
         L     R6,MINELEM          A(ELEMENT)                                   
*                                                                               
         CLI   0(R6),SNVIDELQ      POINTING TO A DETAIL?                        
         BNE   VRCPY86             NOPE                                         
***                                                                             
* CLEAR THE MM INFO FROM THE DETAIL ELEMENT                                     
***                                                                             
         USING SNVIDELD,R6                                                      
         XC    SNVIDBES(6),SNVIDBES CLEAR MM INFO                               
         BRAS  RE,MINIOWRT          WRITE OUT THE DETAIL ELEMENT                
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),SNVIDDAY                                 
         BRAS  RE,MINIOHI          RESET ELEMENT POINTER                        
         BRAS  RE,MINIOSEQ         CHECK NEXT DETAIL                            
         B     VRCPY85A                                                         
***                                                                             
* IF POINTING TO AN E8/E9 ELEMENT, DELETE IT                                    
***                                                                             
VRCPY86  CLI   0(R6),SNVMMELQ      POINTING TO AN E8 ELEMENT?                   
         BE    *+12                YES, DELETE IT                               
         CLI   0(R6),SNVMTELQ      POINTING TO AN E9 ELEMENT?                   
         BNE   VRCPY90             NO, DONE                                     
         BRAS  RE,MINIODEL                                                      
***                                                                             
* NOW ADD FE ELEMENT IF NOT THERE ALREADY                                       
***                                                                             
         XC    MINEKEY,MINEKEY     ADD X'FE' DUMMY ELEM                         
         MVI   MINEKEY,X'FE'                                                    
         XC    MELEM,MELEM                                                      
         BRAS  RE,MINIOHI                                                       
         BE    VRCPY85             ALREADY THERE                                
*                                                                               
         XC    MELEM,MELEM         ADD THIS SO UPDATIVE SOON WON'T DIE          
         MVI   MELEM,X'FE'            WHEN THE RECORD NEEDS TO SPLIT            
         MVI   MELEM+1,X'4B'                                                    
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    VRCPY85                                                          
         DC    H'0'                                                             
*                                                                               
VRCPY90  DS    0H                                                               
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
*                                                                               
         GOTO1 CKSPCLI2                                                         
         BNE   VRCPY92                                                          
         GOTO1 BLDPRTAB                                                         
         GOTO1 GENI2                                                            
*                                                                               
VRCPY92  DS    0H                                                               
         CLI   ACTNUM,ACTCOPY      IF COPYING, DON'T DELETE THE SOURCE          
         BE    VRX                 LET SNV00 DO PERFORM THE MINCLS              
*                                                                               
         BRAS  RE,WRTIMGR                                                       
         BRAS  RE,MINIOCLS                                                      
*                                                                               
         MVC   BCLT,SVFBCLT        DELETE 'FROM' INVOICE                        
         MVC   QCLT,SVFQCLT        DELETE 'FROM' INVOICE                        
         MVC   BMKTSTA,SVFBMSTA                                                 
         MVC   QINVOICE,SVFINVCE                                                
         GOTO1 INITMNIO                                                         
***                                                                             
* IF ORIGINAL INVOICE HAS AN E8 ELEMENT, DELETE THE PASSIVE                     
***                                                                             
         LA    R6,MINMKEY                                                       
         USING SNVKEY,R6                                                        
********************************************************************            
*        PASSIVE KEY:   BUILD PASSIVE KEY.                                      
********************************************************************            
         XC    XPKEY,XPKEY                                                      
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3              NET SYSTEM?                               
         BE    VRCPY95                YES, BUILD NETPACK PASSIVE KEY            
         DROP  R1                                                               
*                                                                               
         MVC   XPKEY(2),=X'0E83'                                                
         MVC   XPKEY+2(1),SNVKAM      AGY/MD                                    
         MVC   XPKEY+5(3),SNVKSTA     STATION                                   
         MVC   XPKEY+8(2),SNVKMOS     YYMM01                                    
         MVC   XPKEY+10(2),SNVKCLT    CLIENT                                    
         MVC   XPKEY+17(10),SNVKINV   INVOICE NUMBER                            
         B     VRCPY96                                                          
***                                                                             
* START BUILDING NETPACK PASSIVE KEY FROM ACTIVE KEY                            
***                                                                             
VRCPY95  DS    0H                                                               
N        USING SNVNKEY,XPKEY                                                    
         MVI   N.SNVNTYP,SNVNTYPQ     X'0E'                                     
         MVI   N.SNVNSUB,SNVNSUBQ     X'93'                                     
         MVC   N.SNVNAM,SNVKAM        A/M                                       
         MVC   N.SNVNCLT,SNVKCLT      CLIENT                                    
         MVC   N.SNVNMOS,SNVKMOS      MONTH OF SERVICE                          
         MVC   N.SNVNINV,SNVKINV      INVOICE NUMBER                            
*                                                                               
VRCPY96  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'E8'          LOOK FOR E8 ELEMENT                       
         MVI   MINFILTL,1                                                       
         XC    MELEM,MELEM                                                      
         LA    R6,MELEM                                                         
         USING SNVMMELD,R6                                                      
         BRAS  RE,MINIOHI             FOUND E8 ELEMENT?                         
         BNE   VRCPY99                NO, NO PASSIVE TO DELETE                  
         CLI   SNVMMEL,SNVMMELQ       FOUND E8 ELEMENT?                         
         BNE   VRCPY99                NO, NO PASSIVE TO DELETE                  
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3              NET SYSTEM?                               
         BE    VRCPY97                YES, BUILD NETPACK PASSIVE KEY            
         DROP  R1                                                               
*                                                                               
         MVC   XPKEY+3(2),SNVMMMKT    MARKET                                    
         MVC   XPKEY+12(3),SNVMMRP1   PRODUCT                                   
         MVC   XPKEY+15(1),SNVMMRE1   ESTIMATE                                  
         MVC   XPKEY+16(1),SNVMMRE2   ESTIMATE 2                                
         MVC   XPKEY+27(3),SNVMMRP2   PRODUCT 2                                 
         B     VRCPY98                                                          
***                                                                             
* CONTUNUE BUILDING NETPACK PASSIVE KEY FROM E8 ELEM                            
***                                                                             
VRCPY97  MVC   N.SNVNNETW,SNVMMNWK    NETWORK                                   
         MVC   N.SNVNPRD,SNVMMRP1     PRODUCTS                                  
         MVC   N.SNVNPRD2,SNVMMRP2    PIGGY                                     
         MVC   N.SNVNEST,SNVMMRE1     ESTIMATE                                  
         MVC   N.SNVNEST2,SNVMMRE2    ESTIMATE                                  
         MVC   N.SNVNPGR,SNVMMRPG     PRODUCT GROUP                             
         DROP  N                                                                
***********************************************************************         
*        PASSIVE KEY: DELETE                                                    
***********************************************************************         
VRCPY98  MVC   XSVPKEY,XPKEY       SAVE THE PASSIVE KEY                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),INVDIR,XPKEY,XPKEY,0                 
         CLC   XSVPKEY(32),XPKEY                                                
         BNE   VRCPY99                                                          
         OI    XPKEY+32,X'80'                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),INVDIR,XPKEY,XPKEY,0                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRCPY99  DS    0H                                                               
         GOTO1 CKSPCLI2                                                         
         BNE   VRCPY99A                                                         
         MVC   QMKT,SVFQMKT                                                     
         MVC   QSTA,SVFQSTA                                                     
         MVC   QSUBMED,SVFQSUBM                                                 
         GOTO1 CLRPRTAB                                                         
         GOTO1 BLDPRTAB                                                         
         GOTO1 GENI2                                                            
*                                                                               
VRCPY99A GOTO1 MINIO,DMCB,('MINDLF',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,MINIOCLS                                                      
*                                                                               
VRX      MVI   MOVOKAY,0                                                        
         OI    MOVOKAYH+6,X'80'                                                 
         J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS A TABLE OF FILM CODES BASED ON CURRENT INVOICE            
***********************************************************************         
READFLMS NTR1                                                                   
         L     RE,ABUFFER          CLEAR THE TABLE                              
         LHI   RF,L'BUFFER                                                      
         XCEFL                                                                  
*                                                                               
*        XC    FLMTAB,FLMTAB                                                    
*                                                                               
         L     R3,ABUFFER          R3 = A(FIRST ENTRY IN TABLE)                 
         SR    R2,R2               ENTRY COUNTER                                
*                                                                               
         XC    MINEKEY,MINEKEY     FIND ALL THE COMMERCIAL ELEMENTS             
         MVI   MINEKEY,SNVCMELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+12                                                             
RDFLM10  CLI   MINERR,MINEEOF                                                   
         BE    RDFLMX                                                           
*                                                                               
         CHI   R2,MXBUFNUM                                                      
         BL    *+6                                                              
         DC    H'0'                MAX NUMBER OF ENTRIES EXCEEDED               
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVCMELD,R6                                                      
         CLI   SNVCMEL,SNVCMELQ                                                 
         BNE   RDFLMX                                                           
         USING BUFFDSCT,R3                                                      
         MVC   BUFFCODE,SNVCMICD     PUT COMMERCIAL IN TABLE                    
         MVC   BUFFSEQN,SNVCMSEQ                                                
         MVI   BUFFSTAT,0                                                       
         MVC   BUFFCMCD,SNVCMCD                                                 
         MVC   BUFFFLAG,SNVCMFLG                                                
         LA    R3,BUFFNEXT                                                      
         AHI   R2,1                                                             
*                                                                               
         BRAS  RE,MINIOSEQ                                                      
         B     RDFLM10                                                          
*                                                                               
RDFLMX   J     XIT                                                              
         DROP  R3,R6                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CHECK THE EBCDIC PRODUCT TO SEE IF IN CLIENT PRODUCT LIST                     
*                                                                               
* ON ENTRY:    PARAM 1             A(EBCDIC PRODUCT CODE)                       
*              PARAM 2             A(BINARY PRODUCT CODE)                       
***********************************************************************         
CHKQPRD  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
         LHI   R0,220                                                           
*                                                                               
CKQP10   CLI   0(RE),0                                                          
         BE    CKQP20                                                           
*                                                                               
         CLC   0(L'QPRD,RE),0(R2)                                               
         BNE   *+14                                                             
         MVC   0(L'BPRD,R3),3(RE)                                               
         B     CKQPYES                                                          
*                                                                               
         LA    RE,4(RE)                                                         
         BCT   R0,CKQP10                                                        
*                                                                               
CKQP20   DS    0H                                                               
         LA    RE,CLIST2                                                        
         LHI   R0,35                                                            
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BNE   *+8                  - NOPE                                      
         LHI   R0,30                - YES, TOTAL OF 250 INSTEAD OF 255          
*                                                                               
CKQP30   CLI   0(RE),0                                                          
         BE    CKQP40                                                           
*                                                                               
         CLC   0(L'QPRD,RE),0(R2)                                               
         BNE   *+14                                                             
         MVC   0(L'BPRD,R3),3(RE)                                               
         B     CKQPYES                                                          
*                                                                               
         LA    RE,4(RE)                                                         
         BCT   R0,CKQP30                                                        
*                                                                               
****  IS IT AN OVERFLOW PRODUCT?  LOOK FOR PRODUCT RECORD                       
*                                                                               
CKQP40   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PRDHDRD,RE                                                       
*                                                                               
         MVI   PLSTTYPE,X'0D'      RECORD TYPE X'0D'                            
         MVI   PLSTSUB,X'F1'       RECORD SUBTYPE X'F1'                         
         MVC   PLSTAM,BAGYMD                                                    
         MVC   PLSTCLT,SVTBCLT     BINARY CLIENT                                
         MVC   PLSTPRD,0(R2)       EBCDIC PRODUCT                               
*                                                                               
         MVI   BYTE,X'00'                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         CLC   KEY(9),KEYSAVE                                                   
         BNE   CKQPNO                                                           
*                                                                               
         LA    RE,KEY                                                           
         OC    PLSTBPRD,PLSTBPRD   ANY BINARY PRODUCT?                          
         BZ    CKQPYES              - NOPE, IT'S AN OVERFLOW                    
*                                                                               
* IF PASSIVE KEY HAS A BINARY VALUE FOR THIS PRODUCT,                           
* THEN THE PRODUCT IS *NOT* EXTENDED BRANDS, AND IT SHOULD HAVE                 
* BEEN FOUND IN CLIST.  SOMETHING WENT WRONG SOMEWHERE, HENCE DC H'0'           
*                                                                               
         DC    H'0'                                                             
*                                                                               
CKQPNO   J     NO                                                               
*                                                                               
CKQPYES  J     YES                                                              
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GENERAL INFORMATIONAL MESSAGES                                                
***********************************************************************         
NEEDMDIA MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
***********************************************************************         
* REGULAR ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECXISTS MVI   GERROR1,RECEXIST    RECORD ALREADY EXISTS                        
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
RECDLTED MVI   GERROR1,RECISDEL    RECORD IS DELETED                            
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM 23 ERROR MESSAGES                                                      
***********************************************************************         
INCMPMON MVI   GERROR1,BRDCALOP    NOT THE SAME BROADCAST/CALENDAR OPT          
         B     SYS23ERR                                                         
*                                                                               
CNTMIXST MVI   GERROR1,STATYPES    CAN NOT COPY FROM-TO DIFFERENT ST...         
         B     SYS23ERR                                                         
*                                                                               
NOESTHDR MVI   GERROR1,ESTNTHDR    NO EST IN HEADER OF FROM INVOICE             
         B     SYS23ERR                                                         
*                                                                               
NOPRDHDR MVI   GERROR1,PRDNTHDR    NO PRDTS IN HEADER OF FROM INVOICE           
         B     SYS23ERR                                                         
*                                                                               
SYS23ERR MVI   GETMSYS,23                                                       
         B     ERREXIT                                                          
*                                                                               
ESTMCLT  MVI   GERROR1,ESTNOTIN    ESTIMATE &1 DOES NOT EXIST FOR CL...         
         B     ERREXIT                                                          
*                                                                               
***********************************************************************         
* SYSTEM 23 ERRORS WITH REPLACEMENT TEXT (&1)                                   
***********************************************************************         
PRODCLT2 MVI   GERROR1,PRDNOTIN    PRODUCT &1 DOES NOT EXIST FOR CLIENT         
         B     ERR23TXT                                                         
*                                                                               
ESTMCLT2 MVI   GERROR1,ESTNOTIN    ESTIMATE &1 DOES NOT EXIST FOR CL...         
         B     ERR23TXT                                                         
*                                                                               
CHKESTMS MVI   GERROR1,CKESTMAT    PLEASE CHECK ESTIMATE DATES FOR E...         
         B     ERR23TXT                                                         
*                                                                               
NWKNSTMS MVI   GERROR1,NETNOTIN    NETWORK &1 DOES NOT EXIST FOR STA...         
         B     ERR23TXT                                                         
*                                                                               
FILMCLT2 MVI   GERROR1,FLMNOTIN    FILM &1 DOES NOT EXIST FOR CLIENT &2         
         B     ERR23TXT                                                         
*                                                                               
ERR23TXT MVI   GETMSYS,23                                                       
         B     ERRRTEXT                                                         
***********************************************************************         
* SYSTEM SPOT ERROR MESSAGES                                                    
***********************************************************************         
BADRQST  MVI   GERROR1,INVREQ      INVALID REQUEST                              
         B     ERREXIT2                                                         
*                                                                               
BADDTFMT MVI   GERROR1,INVDTFMT    INVALID DATE FORMAT                          
         B     ERREXIT2                                                         
*                                                                               
BADPROD  MVI   GERROR1,INVPROD     INVALID PRODUCT                              
         B     ERREXIT2                                                         
*                                                                               
BADESTM  MVI   GERROR1,INVESTMT    INVALID ESTIMATE                             
         B     ERREXIT2                                                         
*                                                                               
BADPROD2 MVI   GERROR1,INVPROD2    INVALID PRODUCT #2                           
         B     ERREXIT2                                                         
*                                                                               
BADDTEST MVI   GERROR1,INVDTEST    DATES NOT WITHIN ESTIMATE PERIOD             
         B     ERREXIT2                                                         
*                                                                               
BADIDERR MVI   GERROR1,INVIDERR    INPUT NOT COMPATIBLE WITH ID NUMBER          
         B     ERREXIT2                                                         
*                                                                               
NORATBOK MVI   GERROR1,INVRATBK    RATING BOOK NOT ON FILE                      
         B     ERREXIT2                                                         
*                                                                               
BKHUTINV MVI   GERROR1,INVBKHUT    BOOK-HUT INVALID                             
         B     ERREXIT2                                                         
*                                                                               
ESTREQRD MVC   GERROR,=Y(ESTNTFND)   ESTIMATE NOT FOUND                         
         B     ERREXIT2                                                         
*                                                                               
INVDTLPR MVC   GERROR,=Y(INVDTLPQ)  INVALID PRODUCT IN 'FROM' INVOICE           
         B     ERREXIT2                                                         
INVDTLPQ EQU   1199                INVALID PRODUCT IN "FROM" INVOICE            
*                                                                               
CLTPGERR MVC   GERROR,=Y(CLTPGERQ) CLT NOT AUTHORIZED FOR STA (P&G)             
         B     ERREXIT2                                                         
CLTPGERQ EQU   1383                                                             
*                                                                               
ERREXIT2 OI    GENSTAT2,USMYERSY                                                
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         NI    MNIOFLAG,X'FF'-X'80'  DON'T SAVE CHANGES ON ERRORS               
         B     MYERRXIT                                                         
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
INFRTEXT LA    R1,INFEXIT                                                       
         B     *+8                                                              
ERRRTEXT LA    R1,ERREXIT                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSYS,GETMSYS                                                   
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         BR    R1                                                               
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
PARSEPTR DC    AL1(1),C'/'         PARSNIP SEPARATOR OVERRIDE LIST              
         DC    AL1(2),C'/-'                                                     
         DC    AL1(1),C','                                                      
         DC    AL1(1),C','                                                      
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE LIST                                      
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
***********************************************************************         
* SPECIAL PFKEY TABLE DEFINITIONS                                               
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* GOTO TO LIST IF CALLSP=0 FOR PF12                                             
         DC    AL1(SPF12X-*,12,0,(SPF12X-SPF12)/KEYLNQ,0)                       
         DC    CL3' ',CL8'INVOICE',CL8'LIST'                                    
SPF12    DC    AL1(KEYTYTWA,L'MOVMED-1),AL2(MOVMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'MOVFCLT-1),AL2(MOVFCLT-T210FFD)                   
         DC    AL1(KEYTYTWA,L'MOVFSTA-1),AL2(MOVFSTA-T210FFD)                   
SPF12X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF24X-*,24,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
SPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         JE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         JE    YES                                                              
         J     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE DELETES THE MINIO ELEMENT READ.                                  
***********************************************************************         
MINIODEL NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         JE    YES                                                              
         J     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         JE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         JE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1  BASE=*,LABEL=*                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         JE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1  BASE=*,LABEL=*                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         JE    YES                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         JE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
***********************************************************************         
* THIS ROUTINE CLOSES THE MINIO SET.                                            
***********************************************************************         
MINIOCLS NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    MNIOFLAG,X'FF'-X'80'  NO NEED TO CLOSE MINIO FILE AGAIN          
         B     YES                                                              
         EJECT                                                                  
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
***********************************************************************         
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGENCY                                                 
         MVC   L.LOCKRTY,=C'NV'    LOCKED INVOICES                              
         MVC   L.LOCKMED,QMED                                                   
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         CLI   L.LOCKMED,C'X'                                                   
         BNE   *+8                                                              
         MVI   L.LOCKSTA+4,C'X'                                                 
         CLI   L.LOCKSTA+4,C' '                                                 
         BH    *+8                                                              
         MVI   L.LOCKSTA+4,C'T'                                                 
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         DROP  L                                                                
*                                                                               
TSTLOCK2 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOCK2                                                         
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ON RETURN IF BYTE CONTAINS X'01' - PAID INVOICE.  EST LOCK OTHERWISE          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHKEST   NTR1  BASE=*,LABEL=*                                                   
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   PA0APAY,C'Y'        ARE WE LOCKING PAID INVOICES?                
         BNE   CHKE10              NO. SEE IF ANY ESTIMATES ARE LOCKED          
*                                                                               
         MVI   BYTE,X'01'          ERROR CODE                                   
         TM    SVCTRLB,SNVHDPDQ    PAID?                                        
         JO    NO                  YES - ISSUE ERROR MESSAGE                    
*                                                                               
CHKE10   DS    0H                                                               
         CLI   PI2PLKE,C'Y'        PREVENTING CHANGES FOR LOCKED ESTS?          
         JNE   YES                 NO - OK TO CHANGE THIS INVOICE               
*                                                                               
         MVI   BYTE,X'00'                                                       
         TM    SVECNTRP,X'08'      POL ESTIMATE LOCKED?                         
         JO    NO                                                               
         TM    SVECNTR1,X'08'      EST FOR PRODUCT1 LOCKED?                     
         JO    NO                                                               
         TM    SVECNTR2,X'08'      PRODUCT 2?                                   
         JO    NO                                                               
*                                                                               
* NOW CHECK IF INVOICE MOS FALLS INTO ESTIMATE LOCK DATE RANGE                  
*                                                                               
         OC    SVELKYMP,SVELKYMP   LOCK DATES FOR POL EST?                      
         BZ    *+12                                                             
         LA    R1,SVELKYMP                                                      
         BRAS  RE,SETLKDT          SET LOCK DATE RANGE                          
*                                                                               
         OC    SVELKYM1,SVELKYM1   PROD 1?                                      
         BZ    *+12                                                             
         LA    R1,SVELKYM1                                                      
         BRAS  RE,SETLKDT                                                       
*                                                                               
         OC    SVELKYM2,SVELKYM2   PROD 2?                                      
         BZ    *+12                                                             
         LA    R1,SVELKYM2                                                      
         BRAS  RE,SETLKDT                                                       
*                                                                               
         MVI   BYTE,X'00'          ERROR CODE                                   
         LA    R1,MINMKEY+(SNVKMOS-SNVKEY)                                      
         MVC   WORK(2),0(R1)                                                    
         XC    WORK(2),=X'FFFF'    XOR INVOICE MOS TO GET READABLE DATE         
*                                                                               
         OC    SVELKYMP,SVELKYMP   DATE RANGE FOR POL ESTIMATE?                 
         BZ    CHKE20              NONE.  SKIP COMPARISON                       
*                                                                               
         CLC   SVELKDTP(2),WORK                                                 
         BH    CHKE20                                                           
         CLC   SVELKDTP+2(2),WORK                                               
         JNL   NO                                                               
*                                                                               
CHKE20   DS    0H                                                               
         OC    SVELKYM1,SVELKYM1                                                
         BZ    CHKE30                                                           
*                                                                               
         CLC   SVELKDT1(2),WORK                                                 
         BH    *+14                                                             
         CLC   SVELKDT1+2(2),WORK                                               
         JNL   NO                                                               
*                                                                               
CHKE30   DS    0H                                                               
         OC    SVELKYM2,SVELKYM2                                                
         JZ    YES                                                              
*                                                                               
         CLC   SVELKDT2(2),WORK                                                 
         JH    YES                                                              
         CLC   SVELKDT2+2(2),WORK                                               
         JNL   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*===============================================================*               
* R1 POINTS TO LOCK DATE Y/M  (2)                               *               
* OUTPUT LOCK START DATE AT 2(2,R1)                             *               
*    AND LOCK   END DATE AT 4(2,R1)                             *               
*===============================================================*               
SETLKDT  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R1                                                            
         MVC   DUB(2),0(R4)                                                     
         NI    DUB+1,X'3F'         DROP PRIOR/SUBSEQ FLAGS                      
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),WORK                                         
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'1D'                                                     
         GOTO1 CALLOV,DMCB                                                      
*                                                                               
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),(R1),(1,WORK),WORK+6,GETDAY,ADDAY                           
         GOTO1 DATCON,DMCB,WORK+6,(2,2(R4))                                     
         GOTO1 (RF),(R1),WORK+12,(2,4(R4))                                      
*                                                                               
         TM    1(R4),X'80'         TEST MONTH AND PRIOR                         
         BZ    *+10                                                             
         XC    2(2,R4),2(R4)       CLEAR START DATE                             
*                                                                               
         TM    1(R4),X'40'         TEST MONTH AND SUBSEQUENT                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         STCM  R0,3,4(R4)          SET HIGH END DATE                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* R5 ADDRESSES MINIO BLOCK                                                      
* IF ESTIMATE IS PRESENT IN HEADER, CONTROL BYTE AND LOCK DATES                 
* ARE SAVED                                                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
GETEST   NTR1  BASE=*,LABEL=*                                                   
         XC    SVLKD(SVLKDLQ),SVLKD                                             
*                                                                               
         CLI   PI2PLKE,C'Y'        PREVENTING CHANGES FOR LOCKED ESTS?          
         JNE   YES                 NO - OK TO CHANGE THIS INVOICE               
*                                                                               
         GOTO1 INITMNIO                                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         JNE   YES                                                              
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         JNE   YES                                                              
*                                                                               
         MVC   SVCTRLB,SNVHDCTL    SAVE CONTROL BYTE                            
*                                                                               
         CLI   SNVHDPRD,0          PRODUCT IN HEADER?                           
         JE    YES                                                              
         CLI   SNVHDEST,0                                                       
         JE    YES                                                              
*                                                                               
* CHECK ESTIMATE FOR POL FIRST                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MINMKEY+2            A/M,CLT                            
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),SNVHDEST                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'SPTDIR',KEY,KEY                     
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NO                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO1                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
*                                                                               
         L     R2,AIO1                                                          
         USING ESTHDR,R2                                                        
         MVC   SVECNTRP,ECNTRL-ESTHDR(R2)                                       
         MVC   SVELKYMP,ELOCKYR-ESTHDR(R2)                                      
         DROP  R2                                                               
*                                                                               
* CHECK INDIVIDUAL PRODUCTS NOW                                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MINMKEY+2            A/M,CLT                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'SPTDIR',KEY,KEY                     
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NO                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO1                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
*                                                                               
         L     R2,AIO1                                                          
         USING CLTHDR,R2                                                        
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         LA    R0,SVCLIST                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SVCLIST+880(140),CLIST2                                          
         DROP  R2                                                               
*                                                                               
* FIND PRODUCT 3-CHAR CODE IN CLIST                                             
         LHI   R0,255                                                           
         LA    R2,SVCLIST                                                       
GETE10   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                PRODUCT CODE NOT IN CLIST                    
         CLC   SNVHDPRD,3(R2)                                                   
         BE    GETE15                                                           
         LA    R2,4(R2)                                                         
         BCT   R0,GETE10                                                        
         DC    H'0'                PRODUCT CODE NOT IN CLIST                    
*                                                                               
GETE15   DS    0H                                                               
         MVC   KEY+4(3),0(R2)      3-CHARACTER PRODUCT CODE FROM CLIST          
         MVC   KEY+7(1),SNVHDEST                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'SPTDIR',KEY,KEY                     
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NO                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO1                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         L     R2,AIO1                                                          
         USING ESTHDR,R2                                                        
         MVC   SVECNTR1,ECNTRL-ESTHDR(R2)                                       
         MVC   SVELKYM1,ELOCKYR-ESTHDR(R2)                                      
         DROP  R2                                                               
*                                                                               
         CLI   SNVHDPR2,0                                                       
         JE    YES                                                              
*                                                                               
         LA    R2,SVCLIST                                                       
         LHI   R0,255                                                           
GETE20   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SNVHDPR2,3(R2)                                                   
         BE    GETE25                                                           
         LA    R2,4(R2)                                                         
         BCT   R0,GETE20                                                        
         DC    H'0'                                                             
*                                                                               
GETE25   DS    0H                                                               
         MVC   KEY+4(3),0(R2)                                                   
         MVC   KEY+7(1),SNVHDEST                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'SPTDIR',KEY,KEY                     
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NO                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO1                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         L     R2,AIO1                                                          
         USING ESTHDR,R2                                                        
         MVC   SVECNTR2,ECNTRL-ESTHDR(R2)                                       
         MVC   SVELKYM2,ELOCKYR-ESTHDR(R2)                                      
         DROP  R2                                                               
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
WRTIMGR  NTR1  BASE=*,LABEL=*                                                   
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIMELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BNE   WRTX                                                             
         L     R6,MINELEM                                                       
         USING SNVIMIDD,R6                                                      
*                                                                               
         CLI   0(R6),SNVIMELQ                                                   
         BNE   WRTX                                                             
*                                                                               
         LA    R2,BLOCK                                                         
         MVC   0(L'WRTIMQNM,R2),WRTIMQNM                                        
         LA    R2,L'WRTIMQNM(R2)                                                
         LHI   R3,L'WRTIMQNM+4                                                  
         EDIT  (R3),(4,(R2))                                                    
         OC    0(4,R2),=C'0000'                                                 
         LA    R2,4(R2)                                                         
         LHI   R3,WMGRDLQ+4                                                     
         EDIT  (R3),(4,(R2))                                                    
         OC    0(4,R2),=C'0000'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
         LR    RE,R2                                                            
         LHI   RF,WMGRDLQ                                                       
         XCEFL                                                                  
*                                                                               
         USING WMGRD,R2                                                         
*                                                                               
* FROM THE IM ID ELEMENT *                                                      
*                                                                               
         MVC   WMGRSTA,SNVIMSTA                                                 
         MVC   WMGRMED,SNVIMMED                                                 
         MVC   WMGRBND,SNVIMBND                                                 
         MVC   WMGRMOS,SNVIMMOS                                                 
         MVC   WMGRVER,SNVIMVER                                                 
*                                                                               
         MVC   WMGRINV,SNVIMINV                                                 
*                                                                               
* LINE BREAK                                                                    
         MVC   WMGRSEP,=C'<DDSSEPERATOR>'                                       
*                                                                               
* ACTION                                                                        
         MVI   WMGRACT,C'U'                                                     
*                                                                               
* GET THE USER ID                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+CTIKNUM-CTIKEY(2),TWAORIG                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO3                      
         L     R6,AIO3                                                          
         CLC   0(L'CTIKEY,R6),KEY                                               
         BNE   WRT05                                                            
*                                                                               
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         BNE   WRT05                                                            
         CLC   =X'02',0(R6)                                                     
         BNE   WRT05                                                            
*                                                                               
         ZIC   R1,1(R6)                                                         
         SHI   R1,3                                                             
         CHI   R1,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WMGRUID(0),2(R6)                                                 
*                                                                               
         USING MINBLKD,R5                                                       
         LA    R6,MINMKEY                                                       
         USING SNVKEYD,R6                                                       
*                                                                               
* AGENCY ALPHA                                                                  
WRT05    DS    0H                                                               
         MVC   WMGRAGY,AGENCY                                                   
*                                                                               
* FROM THE INVOICE KEY *                                                        
*                                                                               
* MEDIA                                                                         
         LA    R1,SNVKAM                                                        
         ICM   R1,8,=C'A'                                                       
         BRAS  RE,GETMED                                                        
         MVC   WMGRNMED(1),BYTE                                                 
*                                                                               
* CLIENT                                                                        
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CLTHDR,R1                                                        
         MVC   CKEYAM,SNVKAM                                                    
         MVC   CKEYCLT,SNVKCLT                                                  
         MVC   KEYSAVE(L'CKEY),KEY                                              
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'SPTDIR',KEY,KEY                     
         CLC   KEYSAVE(L'CKEY),KEY                                              
         BNE   WRT10                                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO3                 
         CLI   DMCB+8,0                                                         
         BNE   WRT10                                                            
         L     R1,AIO3                                                          
         MVC   BYTE,CPROF+6                                                     
         GOTO1 CLUNPK,DMCB,(BYTE,SNVKCLT),WMGRCLT                               
         DROP  R1                                                               
*                                                                               
* STATION                                                                       
WRT10    DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SNVKSTA                                                 
         GOTO1 MSUNPK,DMCB,DUB,WORK,WORK+4                                      
         MVC   WMGRNSTA,WORK+4                                                  
*                                                                               
* BAND                                                                          
         MVC   WMGRNBND(1),WORK+8                                               
*                                                                               
* INVOICE NUMBER                                                                
         MVC   WMGRNINV,SNVKINV                                                 
         DROP  R6                                                               
*                                                                               
* FROM THE INVOICE HEADER ELEMENT *                                             
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BNE   WRT100                                                           
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
* INVOICE DATE                                                                  
         GOTO1 DATCON,DMCB,(2,SNVHDIDT),(8,WMGRDATE)                            
*                                                                               
* PRODUCT1                                                                      
         MVC   WMGRPRD1,SNVHDAP1                                                
*                                                                               
* PRODUCT2                                                                      
         MVC   WMGRPRD2,SNVHDAP2                                                
*                                                                               
* ESTIMATE                                                                      
         EDIT  SNVHDEST,WMGREST                                                 
*                                                                               
* PAYING REP                                                                    
         MVC   WMGRPREP,SNVHDREP                                                
*                                                                               
* PACKAGE CODE                                                                  
         EDIT  SNVHDPKG,WMGRPKG                                                 
         DROP  R6                                                               
*                                                                               
* INSERT DELIMITERS AND OR WITH SPACES                                          
*                                                                               
WRT100   DS    0H                                                               
         MVI   WMGRACT+L'WMGRACT,X'4F'                                          
         MVI   WMGRINV+L'WMGRINV,X'4F'                                          
         MVI   WMGRDATE+L'WMGRDATE,X'4F'                                        
         MVI   WMGRSTA+L'WMGRSTA,X'4F'                                          
         MVI   WMGRMED+L'WMGRMED,X'4F'                                          
         MVI   WMGRBND+L'WMGRBND,X'4F'                                          
         MVI   WMGRUID+L'WMGRUID,X'4F'                                          
         MVI   WMGRAGY+L'WMGRAGY,X'4F'                                          
         MVI   WMGRNSTA+L'WMGRNSTA,X'4F'                                        
         MVI   WMGRNMED+L'WMGRNMED,X'4F'                                        
         MVI   WMGRNBND+L'WMGRNBND,X'4F'                                        
         MVI   WMGRCLT+L'WMGRCLT,X'4F'                                          
         MVI   WMGRPRD1+L'WMGRPRD1,X'4F'                                        
         MVI   WMGRPRD2+L'WMGRPRD2,X'4F'                                        
         MVI   WMGREST+L'WMGREST,X'4F'                                          
         MVI   WMGRPREP+L'WMGRPREP,X'4F'                                        
         MVI   WMGRPKG+L'WMGRPKG,X'4F'                                          
         MVI   WMGRVER+L'WMGRVER,X'4F'                                          
         MVI   WMGRMOS+L'WMGRMOS,X'4F'                                          
         MVI   WMGRNINV+L'WMGRNINV,X'4F'                                        
         MVI   WMGRRVER+L'WMGRRVER,X'4F'                                        
         MVI   WMGRBDAT+L'WMGRBDAT,X'4F'                                        
*                                                                               
         OC    WMGRACT,SPACES                                                   
         OC    WMGRINV,SPACES                                                   
         OC    WMGRDATE,SPACES                                                  
         OC    WMGRSTA,SPACES                                                   
         OC    WMGRMED,SPACES                                                   
         OC    WMGRBND,SPACES                                                   
         OC    WMGRUID,SPACES                                                   
         OC    WMGRAGY,SPACES                                                   
         OC    WMGRNSTA,SPACES                                                  
         OC    WMGRNMED,SPACES                                                  
         OC    WMGRNBND,SPACES                                                  
         OC    WMGRCLT,SPACES                                                   
         OC    WMGRPRD1,SPACES                                                  
         OC    WMGRPRD2,SPACES                                                  
         OC    WMGREST,SPACES                                                   
         OC    WMGRPREP,SPACES                                                  
         OC    WMGRPKG,SPACES                                                   
         OC    WMGRVER,SPACES                                                   
         OC    WMGRMOS,SPACES                                                   
         OC    WMGRNINV,SPACES                                                  
         OC    WMGRRVER,SPACES                                                  
         OC    WMGRBDAT,SPACES                                                  
*                                                                               
         L     R1,=A(TRTAB)                                                     
         A     R1,RELO                                                          
         TR    WMGRSTA,0(R1)                                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         LHI   R3,WMGRDLQ+L'WRTIMQNM+8                                          
         ICM   RF,15,CMQIO-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=CL8'PUT',BLOCK,(R3)                                   
*                                                                               
WRTX     DS    0H                                                               
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
WRTIMQNM DC    C'IMSTATUS********'                                              
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =         
* R1 EXPECTED TO ADDRESS EITHER A/M BYTE OR MEDIA CODE                          
* R1 HIGH-ORDER BYTE MUST HAVE INPUT TYPE C=CODE A=A/M                          
* ON EXIT, BYTE CONTAINS MEDIA CODE OR MEDIA HALFWORD FOR A/M                   
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =         
GETMED   NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,0(R1)          INPUT                                        
*                                                                               
         CLM   R1,8,=C'A'          IS INPUT A/M BYTE?                           
         BNE   *+8                 NO                                           
         NI    BYTE,X'0F'          YES - TURN OFF AGENCY BITS                   
*                                                                               
         LA    R5,GMTAB                                                         
*                                                                               
GM10     DS    0H                                                               
         CLM   R1,8,=C'A'          IS INPUT A/M BYTE?                           
         BNE   GM15                NO                                           
*                                  YES                                          
         CLC   BYTE,1(R5)          COMPARE ON A/M BYTE COLUMN                   
         BE    GM20                                                             
         B     GM18                                                             
*                                                                               
GM15     CLC   BYTE,0(R5)          COMPARE ON MEDIA LETTER COLUMN               
         BE    GM20                                                             
*                                                                               
GM18     LA    R5,GMTABLQ(R5)      ADVANCE TO NEXT LINE IN THE TABLE            
         CLI   0(R5),X'FF'                                                      
         BNE   GM10                                                             
         J     NO                                                               
*                                                                               
GM20     DS    0H                                                               
         MVC   BYTE,0(R5)          COPY MEDIA CODE                              
         CLM   R1,8,=C'C'          INPUT = A/M BYTE?                            
         BNE   *+10                NO                                           
         MVC   BYTE,1(R5)          COPY A/M HALFWORD                            
*                                                                               
         J     YES                                                              
*                                                                               
GMTAB    DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
GMTABLQ  EQU   *-GMTAB                                                          
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CHECK THE BINARY PRODUCT TO SEE IF IN CLIENT PRODUCT LIST                     
*                                                                               
* ON ENTRY:    PARAM 1             A(BINARY PRODUCT CODE)                       
*              PARAM 2             A(EBCDIC PRODUCT CODE)                       
***********************************************************************         
CHKBPRD  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         L     R6,AIO1             SAVED 'FROM' CLIENT RECORD                   
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
         LHI   R0,220                                                           
*                                                                               
CKBPLP   CLI   0(RE),0                                                          
         BE    CKBPNO                                                           
*                                                                               
         CLC   3(L'BPRD,RE),0(R2)                                               
         BNE   *+14                                                             
         MVC   0(L'QPRD,R3),0(RE)                                               
         B     CKBPYES                                                          
*                                                                               
         LA    RE,4(RE)                                                         
         BCT   R0,CKBPLP                                                        
*                                                                               
         LA    RE,CLIST2                                                        
         LHI   R0,35                                                            
*                                                                               
CKBPLP2  CLI   0(RE),0                                                          
         BE    CKBPNO                                                           
*                                                                               
         CLC   3(L'BPRD,RE),0(R2)                                               
         BNE   *+14                                                             
         MVC   0(L'QPRD,R3),0(RE)                                               
         B     CKBPYES                                                          
*                                                                               
         LA    RE,4(RE)                                                         
         BCT   R0,CKBPLP2                                                       
*                                                                               
CKBPNO   J     NO                                                               
*                                                                               
CKBPYES  J     YES                                                              
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FINDLEN - FINDS LENGTH OF INPUT IN A FIELD                                    
*                                                                               
* ON ENTRY: R1 = A(FIELD), INPUT ASSUMED TO BE LEFT-ALIGNED                     
*           R1 HOB = LENGTH OF THE FIELD                                        
*                                                                               
* ON EXIT:  RF = LENGTH OF INPUT, EQUAL CONDITION CODE                          
*           UNEQUAL CC IF LENGTH COULD NOT BE FOUND                             
*                                                                               
* REGISTER USAGE: RE,RF,R1                                                      
***********************************************************************         
FINDLEN  DS    0H                                                               
         LR    RF,R1                                                            
         SRL   RF,24               ISOLATE FIELD LENGTH IN RF LOB               
         AR    R1,RF               R1 -> END OF THE FIELD                       
         BCTR  R1,0                R1 -> LAST CHAR OF THE FIELD                 
*                                                                               
FINDL10  CHI   RF,0                                                             
         BNHR  RE                                                               
         CLI   0(R1),C' '                                                       
         BHR   RE                                                               
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         J     FINDL10                                                          
*                                                                               
*                                                                               
*                                                                               
TRTAB    DC    XL16'000102030405060708090A0B0C0D0E0F'                           
         DC    XL16'101112131415161718191A1B1C1D1E1F'                           
         DC    XL16'202122232425262728292A2B2C2D2E2F'                           
         DC    XL16'303132333435363738393A3B3C3D3E3F'                           
         DC    XL16'40414040404040404040404B4C4D4E4F'                           
         DC    XL16'504040404040404040405A5B5C5D5E5F'                           
         DC    XL16'606140404040404040406A6B6C6D6E6F'                           
         DC    XL16'704040404040404040797A7B7C7D7E7F'                           
         DC    XL16'808182838485868788898A8B8C8D8E8F'                           
         DC    XL16'909192939495969798999A9B9C9D9E9F'                           
         DC    XL16'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'                           
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                           
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CA4040404040'                           
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DA4040404040'                           
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                           
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFB404040FF'                           
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF10                                                           
         CLI   PFKEY,12                                                         
         BL    STPF20                                                           
*                                                                               
STPF10   LAY   R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR   DON'T CLEAR APPLICATION STORAGE              
*                                                                               
STPF20   GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
         CLI   PFKEY,12                                                         
         BNE   STPFX                                                            
*                                                                               
         LAY   R2,SPFTABLE                                                      
         CLI   CALLSP,1            CAN WE GO BACK FROM WHENCE WE CAME?          
         BNE   *+8                 NO                                           
         MVI   PFKEY,24            YES                                          
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPFX    J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
GETPROFS NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETPRFIL,DMCB,=C'S0I2',FPROF0I2                                  
         GOTO1 (RF),(R1),=C'sI2R',FPROFI2R   <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'sI2X',FPROFI2X   <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'sI2P',PROFI2P    <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'sA0A',PROFA0A    <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'S0TI',FPROF0TI                                      
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE DDPARSNIPD        PARSNIP DSECT                                
         EJECT                                                                  
       ++INCLUDE SPSNVFEQTB        FIELD EQUATE TABLE                           
         EJECT                                                                  
       ++INCLUDE SPSNVWORKD        SYSTEM AREAS                                 
         EJECT                                                                  
       ++INCLUDE SPSNVRCRD         REQUEST CARD DSECT                           
         EJECT                                                                  
       ++INCLUDE SPGENSNV          INVOICE RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPSNVFFD          BASE SCREEN FOR SYSTEM                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSNVFAD          OUR MOVE SCREEN                              
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* SPGENCLT                                                                      
* SPGENEST                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPGENSTA                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPWMGRD                                                        
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* MY STORAGE AREA                                                               
*                                                                               
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
ATEMPIO  DS    A                   A(TEMPORARY I/O AREA)                        
ASPCLTBL DS    A                   A(SPECIAL TABLE)                             
ABUFFER  DS    A                   A(FILM CODE BUFFER)                          
*                                                                               
BINPARMS DS    6F                  BINSRCH'S PARAMETER LIST                     
*                                                                               
MISCFLG1 DS    X                                                                
MF1MVCHG EQU   X'80'               MOVE KEYS CHANGED                            
MF1CBLNW EQU   X'40'               NEED TO CHECK NETWORK AGAINST 'TO'           
MF1FSTDQ EQU   X'20'               'FROM' STATION IS DIGITAL                    
MF1TSTDQ EQU   X'10'               'TO' STATION IS DIGITAL                      
*                                                                               
SVFSEP   DS    X                   SAVED PARSNIP FIELD SEPARATOR                
*                                                                               
SVAREA   DS    0C                                                               
SVFBCLT  DS    XL2                 SAVED 'FROM' BINARY CLIENT CODE              
SVFQCLT  DS    CL3                 SAVED 'FROM' ALPHA CLIENT CODE               
SVFBMSTA DS    XL5                              MARKET STATION PACK             
SVFQMKT  DS    CL4                              MARKET, ALPHA                   
SVFQSTA  DS    CL5                              STATION, ALPHA                  
SVFQSUBM DS    C                                SUBMEDIA                        
SVFINVCE DS    CL10                             INVOICE NUMBER                  
SVFBPRD  DS    XL1                              BINARY PRODUCT CODE             
SVFBPRD2 DS    XL1                              BINARY PIGGYBACK CODE           
*                                                                               
SVTBCLT  DS    XL2                 SAVED  'TO'  BINARY CLIENT CODE              
SVTQCLT  DS    CL3                 SAVED  'TO'  ALPHA CLIENT CODE               
SVTCOPT4 DS    X                                CLT COPT4                       
SVTBMSTA DS    XL5                              MARKET STATION PACK             
SVTSFLG1 DS    X                                STATION SFLAG1                  
SVTINVCE DS    CL10                             INVOICE NUMBER                  
SVTBPRD  DS    XL1                              BINARY PRODUCT CODE             
SVTBPRD2 DS    XL1                              BINARY PIGGYBACK CODE           
SVTQPRD  DS    CL3                              EBCDIC PRODUCT CODE             
SVTQPRD2 DS    CL3                              EBCDIC PIGGYBACK CODE           
SVTBEST  DS    XL2                              BINARY ESTIMATE                 
SVAREAX  DS    0C                                                               
*                                                                               
VPARSNIP DS    V                   ADDRESS OF PARSNIP                           
*                                                                               
INTESTSD DS    CL6                 INTERSECTED ESTIMATE START DATE              
INTESTED DS    CL6                                      END DATE                
*                                                                               
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                 BROADCAST COMPRESSED START DATE              
BRDCEDAT DS    XL2                 BROADCAST COMPRESSED END DATE                
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
PERDSYMD DS    CL6                 PERIOD'S START YYMMDD                        
PERDEYMD DS    CL6                          END   YYMMDD                        
*                                                                               
TEMPDATE DS    CL6                 TEMPORARY DATE FIELD                         
*********                                                                       
* 'FROM' PROFILES                                                               
*********                                                                       
FPROF0I2 DS    CL16                I2  PROFILE                                  
FP0I2BC  EQU   FPROF0I2+9          BROADCAST/CALENDAR MONTHS                    
*                                                                               
FPROFI2R DS    CL16                I2R PROFILE                                  
FPI2RAUT EQU   FPROFI2R+0          AUTO U2 FOR $INV                             
FPI2RPOL EQU   FPROFI2R+2          POL/ALL IF PRD NOT STATED                    
FPI2RHUT EQU   FPROFI2R+3          HUT ADJUSTMENT FOR AUTO BOOK LOOKUP          
*                                                                               
FPROFI2X DS    CL16                I2X PROFILE                                  
FPI2XERQ EQU   FPROFI2X+14         ESTIMATE REQUIRED                            
*                                                                               
FPROF0TI DS    CL16                TI  PROFILE                                  
FP0TIFCA EQU   FPROF0TI+0          FILM CODES ACCEPTED                          
*                                                                               
*                                                                               
PROFI2P  DS    CL16                I2P PROFILE                                  
PI2PLKE  EQU   PROFI2P+2           NO LOCKED EST ADD/CHA/DEL/MOVE               
*                                                                               
PROFA0A  DS    CL16                A0A PROFILE                                  
PA0APAY  EQU   PROFA0A+14          NO PAID INVOICE CHA/DEL/MOVE                 
*********                                                                       
* 'TO' PROFILES                                                                 
*********                                                                       
TPROF0I2 DS    CL16                I2  PROFILE                                  
TP0I2BC  EQU   TPROF0I2+9          BROADCAST/CALENDAR MONTHS                    
*                                                                               
TPROFI2R DS    CL16                I2R PROFILE                                  
TPI2RAUT EQU   TPROFI2R+0          AUTO U2 FOR $INV                             
TPI2RPOL EQU   TPROFI2R+2          POL/ALL IF PRD NOT STATED                    
TPI2RHUT EQU   TPROFI2R+3          HUT ADJUSTMENT FOR AUTO BOOK LOOKUP          
*                                                                               
TPROFI2X DS    CL16                I2X PROFILE                                  
TPI2XERQ EQU   TPROFI2X+14         ESTIMATE REQUIRED                            
*                                                                               
TPROFI2Y DS    CL16                I2Y PROFILE                                  
TPI2YESQ EQU   TPROFI2Y+15         DATES OUTSIDE ESTIMATE PERIOD?               
*                                                                               
TPROF0TI DS    CL16                TI  PROFILE                                  
TP0TIFCA EQU   TPROF0TI+0          FILM CODES ACCEPTED                          
TP0TIIFC EQU   TPROF0TI+15         ACCEPT INVALID FILM CODES?                   
*                                                                               
BINSREC  DS    XL(LENSPCL)                                                      
PERVALST DS    XL56                PERVAL STORAGE                               
*                                                                               
*FLMTAB   DS    XL170                                                           
*FLMTABX  EQU   *                                                               
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
PROFI2Z  DS    CL16                I2Z PROFILE                                  
*                                                                               
SVMEDIA  DS    C                                                                
XPKEY    DS    XL64                                                             
XSVPKEY  DS    XL64                                                             
*                                                                               
SVLKD    DS    0H                                                               
SVCTRLB  DS    X                                                                
*                                                                               
SVELOCK1 DS    0XL7                                                             
SVECNTR1 DS    X                                                                
SVELKYM1 DS    XL2                                                              
SVELKDT1 DS    XL4                                                              
*                                                                               
SVELOCK2 DS    0XL7                                                             
SVECNTR2 DS    X                                                                
SVELKYM2 DS    XL2                                                              
SVELKDT2 DS    XL4                                                              
*                                                                               
SVELOCKP DS    0XL7                                                             
SVECNTRP DS    X                                                                
SVELKYMP DS    XL2                                                              
SVELKDTP DS    XL4                                                              
*                                                                               
SVLKDLQ  EQU   *-SVLKD                                                          
*                                                                               
FRMTONWK DS    127XL2              'FROM' 'TO' CBL NTWK CONVERSION TBL          
FRM2NWKX EQU   *                                                                
*                                                                               
SVFQPRD  DS    CL3                              EBCDIC PRODUCT CODE             
SVFQPRD2 DS    CL3                              EBCDIC PIGGYBACK CODE           
***********************************************************************         
* DSECT FOR THE BUFFER ENTRIES                                                  
***********************************************************************         
BUFFDSCT DSECT                                                                  
BUFFCODE DS    XL1                 FILM INTERNAL CODE                           
BUFFSEQN DS    XL2                 FILM SEQUENCE ID NUMBER                      
BUFFSTAT DS    XL1                 STATUS                                       
BUFFSINV EQU   X'80'                - INVALID FILM CODE                         
*BUFFCMCD DS    CL8                 FILM CODE                                   
BUFFCMCD DS    CL12                FILM CODE                                    
BUFFFLAG DS    X                   FILM CODE FLAG                               
BUFFNEXT DS    0C                  NEXT FILM ENTRY                              
***********************************************************************         
* DSECT FOR THE FILMTAB ENTRIES                                                 
***********************************************************************         
FILMTABD DSECT                                                                  
FTABCODE DS    XL1                 FILM INTERNAL CODE                           
FTABELEN DS    XL1                 ENTRY LENGTH                                 
FTABSLEN DS    XL1                 SECONDS LENGTH                               
FTABPRDS DS    0X                  VARIABLE LENGTH PRODUCT LIST                 
         EJECT                                                                  
***********************************************************************         
* EXTRA TEMPORARY WORKING STORAGE                                               
***********************************************************************         
WORKD    DSECT                                                                  
TEMPIO   DS    CL(LIOS)            TEMPORARY IO AREA                            
MAXSPCLS EQU   500                 MAX NUMBER OF ENTRIES IN TABLE               
SPCLTABL DS    (MAXSPCLS)XL(LENSPCL)                                            
MXBUFNUM EQU   200                 MAXIMUM NUMBER OF BUFFER ENTRIES             
BUFFER   DS    XL(MXBUFNUM*(BUFFNEXT-BUFFCODE))                                 
BUFFERX  EQU   *                                                                
WORKX    EQU   *                                                                
*                                                                               
SPCLNTRY DSECT                                                                  
SPCLBPRD DS    XL1                 'FROM' BINARY PRODUCT CODE                   
SPCLQPRD DS    CL3                        EBCDIC PRODUCT CODE                   
SPCLBEST DS    XL1                        BINARY ESTIMATE CODE                  
SPCLLKEY EQU   *-SPCLNTRY                                                       
SPCLBTPR DS    XL1                 'TO'   BINARY PRODUCT CODE                   
SPCLQTPR DS    CL3                        EBCDIC PRODUCT CODE                   
SPCLBTES DS    XL1                        BINARY ESTIMATE CODE                  
*                                                                               
SPCLESTD DS    XL6                 'FROM' ESTIMATE START DATE  (YYMMDD)         
SPCLENDD DS    XL6                                 END   DATE  (YYMMDD)         
LENSPCL  EQU   *-SPCLNTRY          LENGTH OF EACH ENTRY                         
*                                                                               
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKSTA  DS    XL5                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097SPSNV05   10/05/16'                                      
         END                                                                    
