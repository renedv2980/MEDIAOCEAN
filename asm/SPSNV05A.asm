*          DATA SET SPSNV05A   AT LEVEL 069 AS OF 06/18/01                      
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
         BAS   RE,SETPFTBL                                                      
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
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1                                                                   
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF10                                                           
         CLI   PFKEY,12                                                         
         BL    STPF20                                                           
*                                                                               
STPF10   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR   DON'T CLEAR APPLICATION STORAGE              
*                                                                               
STPF20   GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
         CLI   PFKEY,12                                                         
         BNE   STPFX                                                            
*                                                                               
         LA    R2,SPFTABLE                                                      
         CLI   CALLSP,1            CAN WE GO BACK FROM WHENCE WE CAME?          
         BNE   *+8                 NO                                           
         MVI   PFKEY,24            YES                                          
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPFX    B     XIT                                                              
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
         GOTO1 GETPRFIL,DMCB,=C'S0I2',FPROF0I2                                  
         GOTO1 (RF),(R1),=C'sI2R',FPROFI2R   <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'sI2X',FPROFI2X   <== NOTE LOWER CASE 'S'            
         GOTO1 (RF),(R1),=C'S0TI',FPROF0TI                                      
*                                                                               
VKFCLX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
         MVC   SVFBCLT,BCLT                                                     
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
VKFINX   OI    4(R2),X'20'                                                      
*                                                                               
VKX      B     XIT                                                              
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
*                                                                               
         L     R0,AIO1             SAVE 'TO' CLIENT RECORD                      
         LA    R1,LIOS                                                          
         L     RE,ATEMPIO                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'S0I2',TPROF0I2                                  
*                                                                               
         CLC   FP0I2BC(1),TP0I2BC  INCOMPATIBLE CLIENTS IF NOT THE              
         BNE   INCMPMON            SAME BROADCAST/CALENDAR PROFILE OPT          
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
         GOTO1 VPARSNIP,DMCB,(R2),(2,BLOCK),                           +        
               (PSNMVOKQ+PSNNPARQ,PARSEPTR)                                     
         CLI   8(R1),0                                                          
         BNE   INVLFLD                                                          
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
         EX    RE,*+4                                                           
         MVC   FAKEFLD(0),0(R1)                                                 
         MVC   5(1,R2),PSNLEN      STORE INPUT LENGTH IN FIELD HEADER           
         CLI   SVTQPRD,0                                                        
         BNE   VRTPRD20                                                         
*                                                                               
         MVC   SVTQPRD,SPACES                                                   
         EX    RE,*+4                                                           
         MVC   SVTQPRD(0),0(R1)    SAVE EBCDIC PRODUCT                          
         B     VRTPRD30                                                         
*                                                                               
VRTPRD20 MVC   SVTQPRD2,SPACES                                                  
         EX    RE,*+4                                                           
         MVC   SVTQPRD2(0),0(R1)                                                
*                                                                               
VRTPRD30 GOTO1 VALIPRD                                                          
*                                                                               
         CLI   MOVTPRN,0           IS THERE ALREADY A NAME SHOWN?               
         BNE   *+14                                                             
         MVC   MOVTPRN,PRDNM       NO-SHOW 1ST PRODUCT NAME                     
         OI    MOVTPRNH+6,X'80'                                                 
*                                                                               
         CLI   SVTBPRD,0                                                        
         BNE   *+14                                                             
         MVC   SVTBPRD,BPRD                                                     
         B     *+10                                                             
         MVC   SVTBPRD2,BPRD                                                    
*                                                                               
         LA    R3,PSNL(R3)         GO TO NEXT ENTRY                             
         CLI   PSNTAG,0            ANY MORE COMPONENTS                          
*                                                                               
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
         MVC   QPRD,SVTQPRD                                                     
         GOTO1 VALIEST                                                          
*                                                                               
VRTES10  MVC   SVTBEST,BEST                                                     
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
         TM    SVFBMSTA+2,X'F0'    'FROM' CABLE STATION?                        
         BO    VRTST30                                                          
*********                                                                       
* 'FROM' REGULAR STATION                                                        
*********                                                                       
         TM    SVTBMSTA+2,X'F0'    'TO' CABLE STATION?                          
         BNO   VRTSTX              NO, 'TO' REGULAR STATION                     
         B     CNTMIXST                 CAN'T MIX STATION TYPES                 
*********                                                                       
* 'FROM' CABLE STATION                                                          
*********                                                                       
VRTST30  TM    SVTBMSTA+2,X'F0'    'TO' CABLE STATION?                          
         BNO   CNTMIXST            NO, CAN'T MIX STATION TYPES                  
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
***********************************************************************         
* ERROR CHECKING TO MAKE SURE WE CAN COPY THE INVOICE OVER TO THE               
* OTHER CLIENT FROM THE FIRST CLIENT                                            
***********************************************************************         
*VRCHK000 CLC   MOVFCLT,MOVTCLT     SAME CLIENT?                                
*         BNE   VRCHK010            NO                                          
*         TM    MISCFLG1,MF1CBLNW   NEED TO CHECK THE NETWORKS?                 
*         BZ    VRCHKX              NO, DON'T HAVE TO GO THROUGH MESS           
*         CLC   MOVFSTA,MOVTSTA     YES, SAME CABLE STATION?                    
*         BE    VRCHKX                   YES, THEN SAME NETWORKS                
*                                                                               
VRCHK010 L     R0,AIO3             RESTORE 'FROM' CLIENT RECORD TO AIO1         
         LA    R1,LIOS                                                          
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   BCLT,SVFBCLT                                                     
         MVC   BMKTSTA,SVFBMSTA                                                 
         MVC   QINVOICE,SVFINVCE                                                
         GOTO1 INITMNIO                                                         
         EJECT                                                                  
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
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
*****    OC    CMLKCLT,BTRACLT     USE TRAFFIC CLIENT                           
*****    BNZ   *+10                                                             
         MVC   CMLKCLT,SVTBCLT     OTHERWISE USE NORMAL CLIENT                  
         MVC   CMLKCML,BUFFCMCD                                                 
         DROP  R1                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'TRFDIR',KEY,KEY,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'CMLKEY),KEYSAVE                                            
         BE    VRCHK025                                                         
         CLI   TP0TIIFC,C'Y'       ACCEPT INVALID FILM CODES?                   
         BNE   *+14                                                             
         XC    BUFFSEQN,BUFFSEQN   YES, NO COMML SEQUENCE NUMBER THEN           
         B     VRCHK080                                                         
*                                                                               
         MVI   BLOCK,9             L'TEXT+1,TEXT                                
         MVC   BLOCK+1(8),BUFFCMCD                                              
         MVI   BLOCK+9,4           L'TEXT+1,TEXT                                
         MVC   BLOCK+10(3),MOVTCLT                                              
         MVI   BLOCK+13,0           TERMINATING 0                               
         LA    R2,MOVTCLTH                                                      
         B     FILMCLT2            FILM &1 DOES NOT EXIST FOR CLIENT &2         
*                                                                               
VRCHK025 GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'TRFFILE',                X        
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
         EJECT                                                                  
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
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0            GOT ONE?                                     
         BE    VRCHK110                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    VRCHK200                                                         
         DC    H'0'                                                             
*                                                                               
VRCHK110 L     R6,MINELEM                                                       
         CLI   0(R6),SNVIDELQ      MAKE SURE WE HAVE A DETAIL                   
         BNE   VRCHK200                                                         
*                                                                               
         USING SNVIDELD,R6                                                      
         MVC   DUB(L'SVFBMSTA),SVFBMSTA                                         
         OC    DUB+L'SVFBMSTA-1(1),SNVIDNWK                                     
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4   GET ALPHA NETWORK          
         MVC   WORK+4(4),MOVTSTA        MOVE IN THE 'TO' STATION                
         MVI   WORK+4+4,C'/'                                                    
         GOTO1 MSPACK,DMCB,WORK,WORK+4,DUB     VALID FOR 'TO' STATION?          
         BE    VRCHK130                        YES                              
*                                                                               
         MVI   BLOCK,4                ERROR, 'TO' STATION DOESN'T HAVE          
         MVC   BLOCK+1(3),WORK+4+5       THIS NETWORK CODE                      
         MVI   BLOCK+4,5                                                        
         MVC   BLOCK+5(4),MOVTSTA                                               
         MVI   BLOCK+9,0                                                        
         LA    R2,MOVTSTAH                                                      
         B     NWKNSTMS                                                         
         DROP  R6                                                               
*                                                                               
VRCHK130 BAS   RE,MINIOSEQ         CHECK NEXT DETAIL                            
         BE    VRCHK110                                                         
         EJECT                                                                  
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
*        CLC   MOVFCLT,MOVTCLT     SAME CLIENT?                                 
*        BE    VRCHKX              YES                                          
*                                                                               
         XC    MINEKEY,MINEKEY     GET THE INVOICE HEADER                       
         MVI   MINEKEY,SNVHDELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         MVI   SVFBPRD,0                                                        
         MVI   SVFBPRD2,0                                                       
         CLI   SNVHDPRD,0          PRODUCT IN HEADER?                           
         BNE   VRCHK203                                                         
         LA    R2,MOVTCLTH         NO, PRODUCTS & ESTIMATES IN DETAILS          
         CLI   SVTBPRD,0           IF A PRD IS SPECIFIED IN 'SEND TO'           
         BNE   NOPRDHDR              THEN A PRD NEEDS TO BE IN HEADER           
         B     VRCHK205              OF THE 'FROM' INVOICE                      
*                                                                               
VRCHK203 MVC   SVFBPRD,SNVHDPRD    YES, PRODUCT(S) IS FROM HEADER               
         MVC   SVFBPRD2,SNVHDPR2                                                
*                                                                               
         CLI   SNVHDEST,0          ESTIMATE IN HEADER ALSO?                     
         BE    VRCHK205            NO, ESTIMATES ARE IN DETAILS                 
***************                                                                 
* AT MOST 2 PRODUCTS (PRIMARY & PIGGYBACK) AND 1 ESTIMATE                       
***************                                                                 
         XC    BINSREC,BINSREC                                                  
         LA    R2,BINSREC                                                       
         USING SPCLNTRY,R2                                                      
         MVC   SPCLBPRD,SVFBPRD                                                 
         MVC   SPCLBEST,SNVHDEST                                                
         MVC   SPCLBTPR,SVTBPRD                                                 
         MVC   SPCLQTPR,SVTQPRD                                                 
         MVC   SPCLBTES,SVTBEST                                                 
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         CLI   SVTBPRD2,0          GOT A PIGGYBACK PRODUCT?                     
         BE    VRCHK300            NO, GO TO PHASE 3                            
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLBEST,SNVHDEST                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
         MVC   SPCLBTES,SVTBEST                                                 
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
         B     VRCHK300            GO TO PHASE 3                                
         DROP  R2,R6                                                            
***************                                                                 
* ESTIMATES ARE IN THE DETAILS (IF ANY DETAILS)                                 
* PRODUCTS ARE IN THE DETAILS ONLY IF SVFBPRD=0                                 
***************                                                                 
VRCHK205 LA    R2,MOVTCLTH                                                      
         CLI   SVTBEST,0                                                        
         BNE   NOESTHDR                                                         
         XC    MINEKEY,MINEKEY     ANY DETAILS FOR THIS INVOICE?                
         MVI   MINEKEY,SNVIDELQ                                                 
         BAS   RE,MINIOHI                                                       
*                                                                               
         CLI   MINERR,0            GOT SOMETHING BACK FROM MINIOHI?             
         BE    VRCHK210                                                         
         CLI   MINERR,MINEEOF      NO, HIT END OF RECORD?                       
         BE    VRCHK220                YES, NO DETAILS FOR INVOICE              
         DC    H'0'                    NO, SOME TERRIBLE ERROR                  
*                                                                               
VRCHK210 L     R6,MINELEM          WE GOT COMETHING BACK FROM MINIOHI           
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ    IS IT A DETAIL ELEMENT?                      
         BE    VRCHK230            YES                                          
*********                                                                       
* NO DETAILS FOR THIS INVOICE                                                   
*********                                                                       
VRCHK220 CLI   SVFBPRD,0           WAS THERE A PRODUCT CODE IN HEADER?          
         BE    VRCHKX              NO, NO PRODUCT OR ESTIMATES TO CHECK         
*                                                                               
         XC    BINSREC,BINSREC     YES, STILL NEED TO CHECK IF THE PRD          
         LA    R2,BINSREC             IS IN THE 'TO' CLIENT                     
         USING SPCLNTRY,R2                                                      
         MVC   SPCLBPRD,SVFBPRD                                                 
         MVC   SPCLBTPR,SVTBPRD                                                 
         MVC   SPCLQTPR,SVTQPRD                                                 
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
*                                                                               
         CLI   SVTBPRD2,0          GOT A PIGGYBACK PRODUCT?                     
         BE    VRCHK300            NO, GO TO PHASE 3                            
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   VRCHK300            GO TO PHASE 3                                
         DC    H'0'                DIE IF TABLE IS FULL                         
         DROP  R2                                                               
         EJECT                                                                  
*********                                                                       
* WE HAVE AT LEAST ONE DETAIL FOR THIS INVOICE                                  
*********                                                                       
VRCHK230 XC    BINSREC,BINSREC     SET UP BINSRCH RECORD                        
         LA    R2,BINSREC                                                       
         USING SPCLNTRY,R2                                                      
*                                                                               
         CLI   SVFBPRD,0           DID HEADER HAVE THE PRODUCT?                 
         BE    VRCHK235                                                         
         MVC   SPCLBPRD,SVFBPRD                                                 
         MVC   SPCLBTPR,SVTBPRD                                                 
         MVC   SPCLQTPR,SVTQPRD                                                 
         B     *+10                                                             
*                                                                               
VRCHK235 MVC   SPCLBPRD,SNVIDPRD                                                
*                                                                               
         CLI   SNVIDEST,0          ANY ESTIMATE?                                
         BNE   VRCHK240            YES                                          
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
         CLI   SVFBPRD,0           DID HEADER HAVE THE PRODUCT?                 
         BE    VRCHK237                                                         
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
         B     *+10                                                             
*                                                                               
VRCHK237 MVC   SPCLBPRD,SNVIDPR2                                                
*                                                                               
         CLI   SPCLBPRD,0                                                       
         BE    VRCHK250                                                         
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
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
         CLI   SVFBPRD,0           DID HEADER HAVE THE PRODUCT?                 
         BE    VRCHK245                                                         
         MVC   SPCLBPRD,SVFBPRD2                                                
         MVC   SPCLBTPR,SVTBPRD2                                                
         MVC   SPCLQTPR,SVTQPRD2                                                
         B     *+10                                                             
*                                                                               
VRCHK245 MVC   SPCLBPRD,SNVIDPR2                                                
*                                                                               
         CLI   SPCLBPRD,0                                                       
         BE    VRCHK250                                                         
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'01',BINSREC)                                 
         OC    BINPARMS,BINPARMS                                                
         BNZ   VRCHK250                                                         
         DC    H'0'                DIE IF TABLE IS FULL                         
*********                                                                       
* CHECK NEXT DETAIL IF ANY                                                      
*********                                                                       
VRCHK250 BAS   RE,MINIOSEQ                                                      
         BNE   VRCHK300                                                         
         CLI   SNVIDEL,SNVIDELQ                                                 
         BE    VRCHK230                                                         
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************                                             
* PHASE 3A - CONVERT THE BINARY PRODUCTS IN OUR TABLE TO EBCDIC                 
*            PRODUCTS                                                           
***********************************                                             
VRCHK300 L     R2,ASPCLTBL                                                      
         USING SPCLNTRY,R2                                                      
*                                                                               
VRCHK310 CLI   0(R2),0                                                          
         BE    VRCHK350                                                         
         GOTO1 CHKBPRD,DMCB,SPCLBPRD,SPCLQPRD                                   
         BE    *+6                                                              
         DC    H'0'                EBCDIC PRODUCT HAD BETTER EXIST              
*                                                                               
         LA    R2,LENSPCL(R2)                                                   
         B     VRCHK310                                                         
***********************************                                             
* PHASE 3B - CONVERT THE EBCDIC PRODUCTS IN OUR TABLE TO BINARY                 
*            PRODUCTS FOR THE 'TO' CLIENT                                       
***********************************                                             
VRCHK350 L     R0,AIO1                                                          
         LA    R1,LIOS                                                          
         L     RE,ATEMPIO                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ASPCLTBL         START FROM BEGINNING OF OUR TABLE            
VRCHK360 CLI   0(R2),0                                                          
         BE    VRCHK400                                                         
         CLI   SPCLBTPR,0          BINARY PRODUCT ALREADY CALCULATED            
         BNE   VRCHK370                                                         
         GOTO1 CHKQPRD,DMCB,SPCLQPRD,SPCLBTPR                                   
         BE    VRCHK370                                                         
         MVI   BLOCK,4             L'TEXT+1,TEXT                                
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
         EJECT                                                                  
***********************************                                             
* PHASE 4A - GET ESTIMATE START & END DATES FOR THE 'FROM' ESTIMATES            
***********************************                                             
VRCHK400 L     R6,ASPCLTBL                                                      
         USING SPCLNTRY,R6                                                      
*                                                                               
VRCHK410 CLI   0(R6),0             NO MORE ENTRIES FOR 'FROM' ESTIMATE          
         BE    VRCHK450                                                         
*                                                                               
         CLI   SPCLBEST,0          NO ESTIMATE FOR THIS ENTRY?                  
         BE    VRCHK420            NONE, SKIP TO NEXT ENTRY                     
*                                                                               
         MVC   BCLT,SVFBCLT        VALIDATE ESTIMATE FOR THIS CLIENT            
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
VRCHK460 CLI   0(R6),0             NO MORE ENTRIES FOR 'FROM' ESTIMATE          
         BE    VRCHK500                                                         
*                                                                               
         CLI   SPCLBTES,0          'TO' ESTIMATE IN THIS ENTRY?                 
         BNE   VRCHK463            YES, SO USE IT                               
         CLI   SPCLBEST,0          NO ESTIMATE FOR THIS ENTRY?                  
         BE    VRCHK470            NONE, SKIP TO NEXT ENTRY                     
         MVC   SPCLBTES,SPCLBEST   USE THE 'FROM' ESTIMATE                      
*                                                                               
VRCHK463 MVC   BCLT,SVTBCLT        VALIDATE ESTIMATE FOR THIS CLT/PRD           
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
***********************************************************************         
* ACTUAL COPY OF 'FROM' INVOICE                                                 
***********************************************************************         
VRCPY00  MVC   BCLT,SVFBCLT                                                     
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
*&&DO                                                                           
VRCPY10  CLC   SVTBCLT,SVFBCLT     IF CLIENTS AND PRODUCTS ARE THE SAME         
         BNE   VRCPY15             DON'T NEED TO DO PRODUCT CONVERSION          
         CLC   SVFBPRD,SVTBPRD                                                  
         BNE   VRCPY15                                                          
         CLC   SVFBPRD2,SVTBPRD2                                                
         BE    VRCPY80                                                          
*&&                                                                             
VRCPY15  MVC   BCLT,SVTBCLT                                                     
         MVC   BMKTSTA,SVTBMSTA                                                 
         MVC   QINVOICE,SVTINVCE                                                
         GOTO1 INITMNIO                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO HEADER                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   0(R6),SNVHDELQ                                                   
         BE    *+6                                                              
         DC    H'0'                DIE IF NO HEADER                             
         GOTO1 DATCON,DMCB,(5,0),(2,SNVHDCDT)  CHANGE DATE FOR TODAY            
         CLI   SNVHDPRD,0          PRODUCT ENTERED IN THE HEADER?               
         BNE   *+12                                                             
         BAS   RE,MINIOWRT                                                      
         B     VRCPY50             NO                                           
***************                                                                 
* PRODUCT IN THE INVOICE HEADER                                                 
***************                                                                 
         XC    BINSREC,BINSREC     YES, CHANGE PRODUCT CODE TO NEW CODE         
         MVC   BINSREC(1),SNVHDPRD                                              
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVHDPRD,SPCLBTPR   NEW PRODUCT CODE FOR 'TO' CLIENT             
*                                                                               
         MVI   SNVHDPR2,0                                                       
         CLI   SVTBPRD2,0          ANY PIGGYBACK PRODUCT CODE?                  
         BE    VRCPY20             NONE, WRITE OUT THE HEADER ELEMENT           
*                                                                               
         LA    R2,BINSREC                                                       
         XC    BINSREC,BINSREC     YES, CHANGE PIGGY CODE TO NEW CODE           
         MVC   SPCLBTPR(1),SVTBPRD2    SEARCH FOR NEW PIGGYBACK                 
         DROP  R2                                                               
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC),,,,                    +        
               (1,SPCLBTPR-SPCLBPRD)                                            
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVHDPR2,SPCLBTPR   NEW PIGGY CODE FOR 'TO' CLIENT               
*                                                                               
VRCPY20  DS    0H                                                               
         CLI   SPCLBTES,0                                                       
         BE    *+10                                                             
         MVC   SNVHDEST,SPCLBTES                                                
         DROP  R2                                                               
         BAS   RE,MINIOWRT         WRITE OUT THE NEW CODE                       
*                                                                               
         CLC   SVFBMSTA+2(3),SVTBMSTA+2   SAME CABLE STATION?                   
         BE    VRCPY80             YES                                          
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    VRCPY30                                                          
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY80                                                          
         DC    H'0'                                                             
*                                                                               
VRCPY30  L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ    DETAIL ELEMENT?                              
         BNE   VRCPY45             NO, CHECK THE NEXT ONE                       
*                                                                               
         CLI   SNVIDNWK,24         IS THIS A TOP 24 NETWORK?                    
         BNH   VRCPY45             YES, SAME NETWORK CODE FOR TOP 24            
*                                                                               
         MVC   DUB(L'SVFBMSTA),SVFBMSTA                                         
         OC    DUB+L'SVFBMSTA-1(1),SNVIDNWK                                     
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4   GET ALPHA NETWORK          
         MVC   WORK+4(4),MOVTSTA        MOVE IN THE 'TO' STATION                
         MVI   WORK+4+4,C'/'                                                    
         GOTO1 MSPACK,DMCB,WORK,WORK+4,DUB                                      
         NI    DUB+L'SVFBMSTA-1,X'7F'                                           
         MVC   SNVIDNWK,DUB+L'SVFBMSTA-1                                        
*                                                                               
VRCPY40  BAS   RE,MINIOWRT         WRITE OUT THE DETAIL ELEMENT                 
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),SNVIDDAY                                 
*                                                                               
         BAS   RE,MINIOHI          RESET ELEMENT POINTER                        
*                                                                               
VRCPY45  BAS   RE,MINIOSEQ         CHECK NEXT DETAIL                            
         BE    VRCPY30                                                          
         B     VRCPY80                                                          
         DROP  R6                                                               
***************                                                                 
* PRODUCT AND NETWORK CODES ARE IN THE DETAILS                                  
***************                                                                 
VRCPY50  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    VRCPY60                                                          
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY80                                                          
         DC    H'0'                                                             
*                                                                               
VRCPY60  L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ    DETAIL ELEMENT?                              
         BNE   VRCPY75             NO, CHECK THE NEXT ONE                       
*                                                                               
         CLC   SVFBMSTA+2(3),SVTBMSTA+2   SAME CABLE STATION?                   
         BE    VRCPY65             YES, SAME NETWORK CODES                      
*                                                                               
         CLI   SNVIDNWK,24         IS THIS A TOP 24 NETWORK?                    
         BNH   VRCPY65             YES, SAME NETWORK CODE FOR TOP 24            
*                                                                               
         MVC   DUB(L'SVFBMSTA),SVFBMSTA                                         
         OC    DUB+L'SVFBMSTA-1(1),SNVIDNWK                                     
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4   GET ALPHA NETWORK          
         MVC   WORK+4(4),MOVTSTA        MOVE IN THE 'TO' STATION                
         MVI   WORK+4+4,C'/'                                                    
         GOTO1 MSPACK,DMCB,WORK,WORK+4,DUB                                      
         NI    DUB+L'SVFBMSTA-1,X'7F'                                           
         MVC   SNVIDNWK,DUB+L'SVFBMSTA-1                                        
*                                                                               
VRCPY65  XC    BINSREC,BINSREC     CHANGE PRODUCT CODE TO NEW CODE              
         MVC   BINSREC(1),SNVIDPRD                                              
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVIDPRD,SPCLBTPR   NEW PRODUCT CODE FOR 'TO' CLIENT             
         DROP  R2                                                               
*                                                                               
         CLI   SNVIDPR2,0          ANY PIGGYBACK PRODUCT CODE?                  
         BE    VRCPY70             NONE, WRITE OUT THE DETAIL ELEMENT           
*                                                                               
         XC    BINSREC,BINSREC     YES, CHANGE PIGGY CODE TO NEW CODE           
         MVC   BINSREC(1),SNVIDPR2                                              
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(X'02',BINSREC)                                 
         CLI   BINPARMS,X'01'          RECORD NOT FOUND?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,0(R1)                                                         
         USING SPCLNTRY,R2                                                      
         MVC   SNVIDPR2,SPCLBTPR   NEW PIGGY CODE FOR 'TO' CLIENT               
         DROP  R2                                                               
*                                                                               
VRCPY70  BAS   RE,MINIOWRT         WRITE OUT THE DETAIL ELEMENT                 
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),SNVIDDAY                                 
*                                                                               
         BAS   RE,MINIOHI          RESET ELEMENT POINTER                        
*                                                                               
VRCPY75  BAS   RE,MINIOSEQ         CHECK NEXT DETAIL                            
         BE    VRCPY60                                                          
*                                                                               
VRCPY80  XC    MINEKEY,MINEKEY     LOOK FOR THE COMMERCIAL CODES                
         MVI   MINEKEY,SNVCMELQ                                                 
*                                                                               
         BAS   RE,MINIOHI          RESET ELEMENT POINTER                        
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
         BAS   RE,MINIOWRT                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     RE-ESTABLISH THE KEY                         
         MVI   MINEKEY,SNVCMELQ                                                 
         MVC   MINEKEY+1(L'SNVCMICD),SNVCMICD                                   
*                                                                               
         BAS   RE,MINIOHI          RESET ELEMENT POINTER                        
*                                                                               
VRCPY82N BAS   RE,MINIOSEQ                                                      
         BE    VRCPY82                                                          
         DROP  RE                                                               
*                                                                               
VRCPY85  LA    R1,MINMKEY                                                       
         USING SNVKEY,R1                                                        
         NI    SNVDSTAT+1,X'FF'-X'80'                                           
         DROP  R1                                                               
*                                                                               
         XC    MINEKEY,MINEKEY     NEED TO GET RID OF THE MATCHMAKER            
         MVI   MINEKEY,SNVMMELQ        INFO                                     
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0            GOT ONE?                                     
         BE    *+12                                                             
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY86                                                          
         L     R6,MINELEM                                                       
         CLI   0(R6),SNVMMELQ                                                   
         BE    *+12                                                             
         CLI   0(R6),SNVMTELQ                                                   
         BNE   VRCPY86            DIDN'T FIND E8 OR E9, LOOK FOR F1             
         BAS   RE,MINIODEL                                                      
         B     VRCPY85                                                          
*                                                                               
VRCPY86  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'F1'       GET RID OF F1 ELEM                           
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+12                                                             
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY86A                                                         
         L     R6,MINELEM                                                       
         CLI   0(R6),X'F1'         GOT ONE?                                     
         BNE   *+8                                                              
         BAS   RE,MINIODEL         YES, DELETE                                  
*                                                                               
VRCPY86A XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'FE'       CHECK IF THERE'S FE ELEM                     
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+12                                                             
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY86B                                                         
         L     R6,MINELEM                                                       
         CLI   0(R6),X'FE'         GOT ONE?                                     
         BE    VRCPY90                                                          
VRCPY86B XC    0(L'MELEM,R6),0(R6)   CLEAR MINELEM AREA                         
         MVI   0(R6),X'FE'           ELEMENT CODE                               
         MVI   1(R6),100             ELEMENT LENGTH                             
         BAS   RE,MINIOADD           ADD ELEM                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&DO                                                                           
VRCPY87  XC    MINEKEY,MINEKEY     LOOK FOR X'10' ELEM                          
         MVI   MINEKEY,SNVHDELQ    CHECK IF THERE'S ONE                         
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+12                                                             
         CLI   MINERR,MINEEOF                                                   
         BE    VRCPY90                                                          
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   0(R6),SNVHDELQ      GOT ONE?                                     
         BE    *+6                 HAS TO BE THERE                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(5,0),(2,SNVHDCDT)  CHANGE DATE FOR TODAY            
         BAS   RE,MINIOWRT                                                      
*&&                                                                             
*                                                                               
VRCPY90  BAS   RE,MINIOCLS                                                      
*                                                                               
         CLI   ACTNUM,ACTCOPY      IF WE'RE COPYING                             
         BE    VRX                 DON'T DELETE ORIGINAL                        
*                                                                               
         MVC   BCLT,SVFBCLT        DELETE 'FROM' INVOICE                        
         MVC   BMKTSTA,SVFBMSTA                                                 
         MVC   QINVOICE,SVFINVCE                                                
         GOTO1 INITMNIO                                                         
*                                                                               
         GOTO1 MINIO,DMCB,('MINDLF',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MINIOCLS                                                      
*                                                                               
VRX      MVI   MOVOKAY,0                                                        
         OI    MOVOKAYH+6,X'80'                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS A TABLE OF FILM CODES BASED ON CURRENT INVOICE            
***********************************************************************         
READFLMS NTR1                                                                   
         L     R0,ABUFFER          CLEAR THE TABLE                              
         LA    R1,L'BUFFER                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    FLMTAB,FLMTAB                                                    
*                                                                               
         L     R3,ABUFFER          R3 = A(FIRST ENTRY IN TABLE)                 
*                                                                               
         XC    MINEKEY,MINEKEY     FIND ALL THE COMMERCIAL ELEMENTS             
         MVI   MINEKEY,SNVCMELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+12                                                             
RDFLM10  CLI   MINERR,MINEEOF                                                   
         BE    RDFLMX                                                           
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
         LA    R3,BUFFNEXT                                                      
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         B     RDFLM10                                                          
*                                                                               
RDFLMX   B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
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
CKQPLP   CLI   0(RE),0                                                          
         BE    CKQPNO                                                           
*                                                                               
         CLC   0(L'QPRD,RE),0(R2)                                               
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     CKQPLP                                                           
*                                                                               
         MVC   0(L'BPRD,R3),3(RE)                                               
*                                                                               
CKQPYES  B     YES                                                              
*                                                                               
CKQPNO   B     NO                                                               
         DROP  R6                                                               
***********************************************************************         
* CHECK THE BINARY PRODUCT TO SEE IF IN CLIENT PRODUCT LIST                     
*                                                                               
* ON ENTRY:    PARAM 1             A(BINARY PRODUCT CODE)                       
*              PARAM 2             A(EBCDIC PRODUCT CODE)                       
***********************************************************************         
CHKBPRD  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         L     R6,AIO1             SAVED 'FROM' CLIENT RECORD                   
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
CKBPLP   CLI   0(RE),0                                                          
         BE    CKBPNO                                                           
*                                                                               
         CLC   3(L'BPRD,RE),0(R2)                                               
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     CKBPLP                                                           
*                                                                               
         MVC   0(L'QPRD,R3),0(RE)                                               
*                                                                               
CKBPYES  B     YES                                                              
*                                                                               
CKBPNO   B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE DELETES THE MINIO ELEMENT READ.                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    YES                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
***********************************************************************         
* THIS ROUTINE CLOSES THE MINIO SET.                                            
***********************************************************************         
MINIOCLS NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    MNIOFLAG,X'FF'-X'80'  NO NEED TO CLOSE MINIO FILE AGAIN          
         B     YES                                                              
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
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
         CLI   QSTA,C'0'                                                        
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
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
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
*                                                                               
SVFSEP   DS    X                   SAVED PARSNIP FIELD SEPARATOR                
*                                                                               
SVAREA   DS    0C                                                               
SVFBCLT  DS    XL2                 SAVED 'FROM' BINARY CLIENT CODE              
SVFBMSTA DS    XL5                              MARKET STATION PACK             
SVFINVCE DS    CL10                             INVOICE NUMBER                  
SVFBPRD  DS    XL1                              BINARY PRODUCT CODE             
SVFBPRD2 DS    XL1                              BINARY PIGGYBACK CODE           
*                                                                               
SVTBCLT  DS    XL2                 SAVED  'TO'  BINARY CLIENT CODE              
SVTBMSTA DS    XL5                              MARKET STATION PACK             
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
FLMTAB   DS    XL170                                                            
FLMTABX  EQU   *                                                                
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
***********************************************************************         
* DSECT FOR THE BUFFER ENTRIES                                                  
***********************************************************************         
BUFFDSCT DSECT                                                                  
BUFFCODE DS    XL1                 FILM INTERNAL CODE                           
BUFFSEQN DS    XL2                 FILM SEQUENCE ID NUMBER                      
BUFFSTAT DS    XL1                 STATUS                                       
BUFFSINV EQU   X'80'                - INVALID FILM CODE                         
BUFFCMCD DS    CL8                 FILM CODE                                    
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
MXBUFNUM EQU   100                 MAXIMUM NUMBER OF BUFFER ENTRIES             
BUFFER   DS    XL(MXBUFNUM*(BUFFNEXT-BUFFCODE))                                 
BUFFERX  EQU   *                                                                
WORKX    EQU   *                                                                
*                                                                               
SPCLNTRY DSECT                                                                  
SPCLBPRD DS    XL1                 'FROM' BINARY PRODUCT CODE                   
SPCLBEST DS    XL1                        BINARY ESTIMATE CODE                  
SPCLLKEY EQU   *-SPCLNTRY                                                       
SPCLQPRD DS    CL3                        EBCDIC PRODUCT CODE                   
SPCLBTPR DS    XL1                 'TO'   BINARY PRODUCT CODE                   
SPCLBTES DS    XL1                        BINARY ESTIMATE CODE                  
SPCLQTPR DS    CL3                        EBCDIC PRODUCT CODE                   
*                                                                               
SPCLESTD DS    XL6                 ESTIMATE START DATE  (YYMMDD)                
SPCLENDD DS    XL6                          END   DATE  (YYMMDD)                
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
**PAN#1  DC    CL21'069SPSNV05A  06/18/01'                                      
         END                                                                    
