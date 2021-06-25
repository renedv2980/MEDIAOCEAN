*          DATA SET SPSNV03    AT LEVEL 077 AS OF 06/08/20                      
*PHASE T21003A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21003 - MAINTENANCE OF DETAILS                                       
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T21000), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     SPSNVFC (T210FC) -- MAINTENANCE                                 
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
*          R8 - THIRD BASE                                                      
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-14829  07/28/17 MAKE SURE WE ADD X'30' ELEM FOR 2ND FILM  *         
***********************************************************************         
T21003   TITLE 'SPSNV03 - SPOT INVOICE DETAIL MAINT OVERLAY'                    
T21003   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21003*,R7,R8,RR=R3                                           
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
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
         BRAS  RE,SETUP                                                         
         GOTO1 =A(SETPFTBL),DMCB,(RC),RR=RELO                                   
*                                                                               
         TM    CTLRFLG1,CF1CKOFF   SAVE OFFSET OF SELECTED LINE?                
         BZ    *+14                                                             
         MVC   SVDOFFST,SELOFFST   YES                                          
         NI    CTLRFLG1,X'FF'-CF1CKOFF                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'INV',3,GLVPGM                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VREC                                                             
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         XC    LKDATA(LKDATALQ),LKDATA                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
*                                                                               
         LA    R2,DTLKEYLH                                                      
         OI    6(R2),X'20'         PROTECT THIS FIELD                           
*                                                                               
*        TM    4(R2),X'80'         FIELD WAS CHANGED?                           
*        BZ    VK05                                                             
         TM    4(R2),X'20'         FIELD ALREADY VALIDATED?                     
         BO    VK05                YES                                          
*                                                                               
         OI    4(R2),X'20'         SET FIELD VALIDATED                          
         OI    MISCFLG1,MF1KYCHG   YES                                          
         ZAP   TOTAMNTS,=P'0'      TOTAL/NUMBER FOR ALL THE DETAILS             
         ZAP   TOTSPOTS,=P'0'                                                   
         ZAP   FLTAMNTS,=P'0'      TOTAL/NUMBER FOR THOSE FILTERED              
         ZAP   FLTSPOTS,=P'0'                                                   
*                                                                               
VK05     CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         ZIC   R1,5(R2)            CONVERT THE PERIODS TO COMMAS                
         LA    R3,8(R2)                                                         
VK10     CLI   0(R3),C'.'                                                       
         BNE   *+8                                                              
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         BCT   R1,VK10                                                          
         OI    6(R2),X'80'         RETRANSMIT TO DISPLAY COMMAS                 
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'87',BLOCK)                                  
         CLI   4(R1),5                                                          
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,FAKEFLDH                                                      
         L     R1,ATIOB            SET UP TIOB SO WE CAN POINT TO ERROR         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LA    R0,DTLKEYLH                                                      
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         DROP  R1                                                               
*                                                                               
         LA    R3,BLOCK            R3=A(SCANNED FIELDS)                         
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)      SO ANY ERRORS WILL BE POINTED OUT            
         DROP  R1                                                               
         CLI   0(R3),1             MAKE SURE MEDIA IN CORRECT FORMAT            
         BL    MISSFLD                                                          
         BNE   BADMEDIA               MEDIA HAS TO BE 1 BYTE                    
         CLI   1(R3),0                                                          
         BNE   BADMEDIA                                                         
         MVC   FAKEFLD(1),12(R3)                                                
         MVI   FAKEFLDH+5,1                                                     
         GOTO1 VALIMED                                                          
         MVC   SVQMED,QMED                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPMD                                 
*                                                                               
         LA    R3,32(R3)                                                        
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)      SO ANY ERRORS WILL BE POINTED OUT            
         DROP  R1                                                               
         CLI   0(R3),0             MAKE SURE CLIENT IN CORRECT FORMAT           
         BE    MISSFLD                                                          
         CLI   1(R3),0                                                          
         BNE   BADCLNT                                                          
         ZIC   R1,0(R3)                                                         
         STC   R1,FAKEFLDH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
         GOTO1 VALICLT                                                          
         MVC   SVQCLT,QCLT                                                      
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2P',PROFI2P                                   
         GOTO1 (RF),(R1),=C'sA0A',PROFA0A                                       
         GOTO1 (RF),(R1),(X'90',=C'sI2N'),PROFI2N                               
         GOTO1 (RF),(R1),(X'90',=C'sI2X'),PROFI2X                               
         GOTO1 (RF),(R1),(X'90',=C'sI2R'),PROFI2R                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPCLT                                
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    VK12                NO, NO NEED TO GET PROFILES AGAIN            
         GOTO1 GETPRFIL,DMCB,=C'S0T1',PROF0T1   GET PROFILES                    
         GOTO1 (RF),(R1),=C'sI2Y',PROFI2Y                                       
*                                                                               
VK12     LA    R3,32(R3)                                                        
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)      SO ANY ERRORS WILL BE POINTED OUT            
         DROP  R1                                                               
         CLI   0(R3),0             MAKE SURE STATION IN CORRECT FORMAT          
         BE    MISSFLD                                                          
         CLI   1(R3),0                                                          
         BNE   BADSTATN                                                         
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         STC   R1,FAKEFLDH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
         GOTO1 VALISTA                                                          
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,BLANKS                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   SVQSTA,QSTA                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPSTA                                
         LA    R3,32(R3)                                                        
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)      SO ANY ERRORS WILL BE POINTED OUT            
         DROP  R1                                                               
         CLI   0(R3),0             MAKE SURE PERIOD IN CORRECT FORMAT           
         BE    MISSFLD                                                          
         CLI   1(R3),0                                                          
         BNE   BADDTFMT                                                         
         GOTO1 PERVAL,DMCB,(0(R3),12(R3)),PERVALST                              
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         TM    PVALASSM,PVALASD                                                 
         BZ    BADDTFMT                                                         
         MVC   BMOSS,PVALCSTA                                                   
         MVC   BMOSE,PVALCEND                                                   
         DROP  R1                                                               
         ZIC   R0,0(R3)                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',12(R3),(R0),GLVSPPER                          
         LA    R3,32(R3)                                                        
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)      SO ANY ERRORS WILL BE POINTED OUT            
         DROP  R1                                                               
         CLI   0(R3),0             MAKE SURE INVOICE EXISTS                     
         BE    MISSFLD                                                          
         CLI   1(R3),0                                                          
         BNE   INVLFLD                                                          
         XC    QINVOICE,QINVOICE                                                
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QINVOICE(0),12(R3)                                               
         OC    QINVOICE,=CL132' '                                               
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
*                                                                               
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VK13                THEN NO D/A                                  
         MVC   CONHED2(4),=C'D/A='                                              
         GOTO1 HEXOUT,DMCB,SNVDDA,CONHED2+4,L'SNVDDA                            
         OI    CONHED2H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
VK13     GOTO1 INITMNIO                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    LOOK FOR THE HEADER ELEMENT                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING HEADER ELEMENT          
*                                                                               
         MVC   SVCTRLB,SNVHDCTL    HEADER ELEMENT CONTROL BYTE                  
         NI    MISCFLG2,X'FF'-MF2MTIME                                          
         TM    SNVHDCTL,SNVHDMTQ   INVOICE USES MILITARY TIME?                  
         BZ    *+8                                                              
         OI    MISCFLG2,MF2MTIME   YES                                          
*                                                                               
         OI    DTLKYL2H+6,X'80'    SHOW THE SECOND KEY LINE                     
         NI    MISCFLG1,X'FF'-MF1GBLPR     NO GLOBAL PRODUCT                    
*                                                                               
         XC    CURRBPRD(L'CURRBPRD*2+L'CURRQPRD*2),CURRBPRD                     
*                                                                               
         CLI   SNVHDPRD,0          DO WE HAVE A GLOBAL PRODUCT?                 
         BNE   VK14                                                             
         CLI   SNVHDLEN,SNVHDLN2   MAYBE, LET'S SEE IF JUST ALPHA PRD           
         BNH   *+14                NOPE                                         
         CLC   SNVHDAP1,BLANKS                                                  
         BH    VK14                                                             
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPRD                                    
         B     VK20                NO                                           
*                                                                               
VK14     MVC   CURRBPRD,SNVHDPRD   COPY GLOBAL PRODUCT                          
         OI    MISCFLG1,MF1GBLPR   YES                                          
         MVC   DTLKYL2(4),=CL4'PRD='                                            
*                                                                               
         CLI   SNVHDLEN,SNVHDLN2                                                
         BNH   VK14G                                                            
         CLC   SNVHDAP1,BLANKS                                                  
         BNH   VK14G                                                            
         MVC   DMCB+1(3),SNVHDAP1                                               
         B     VK15                                                             
*                                                                               
VK14G    GOTO1 FINDQPRD,DMCB,(SNVHDPRD,0)  SHOW THE PRODUCT ON TOP PART         
         BE    VK15                                                             
         MVC   DTLKYL2+4(3),=3C'?'                                              
         MVC   CURRQPRD,=3C'?'                                                  
         B     VK16                                                             
*                                                                               
VK15     MVC   DTLKYL2+4(3),DMCB+1                                              
         MVC   FAKEFLD(3),DMCB+1                                                
         MVI   FAKEFLDH+5,3                                                     
         MVC   CURRQPRD,DMCB+1                                                  
*                                                                               
         CLI   SNVHDPR2,0          ANY PIGGYBACK?                               
         BNE   VK15A                                                            
         CLI   SNVHDLEN,SNVHDLN2                                                
         BNH   VK16                                                             
         CLC   SNVHDAP2,BLANKS                                                  
         BNH   VK16                NO                                           
*                                                                               
VK15A    MVC   CURRBPR2,SNVHDPR2   COPY GLOBAL PIGGYBACK PRODUCT                
         MVC   DTLKYL2+7(1),P0TIPBS                                             
         MVC   FAKEFLD+3(1),P0TIPBS                                             
         CLI   P0TIPBS,0                                                        
         BE    *+12                                                             
         CLI   P0TIPBS,C'N'                                                     
         BNE   *+12                                                             
         MVI   DTLKYL2+7,C'-'      YES, SHOW PIGGY NEXT TO 1ST PRODUCT          
         MVI   FAKEFLD+3,C'-'                                                   
*                                                                               
         CLI   SNVHDLEN,SNVHDLN2                                                
         BNH   VK15C                                                            
         MVC   DMCB+1(3),SNVHDAP2                                               
         B     VK15G                                                            
*                                                                               
VK15C    GOTO1 FINDQPRD,DMCB,(SNVHDPR2,0)                                       
         BE    VK15G                                                            
         MVC   DTLKYL2+8(3),=3C'?'                                              
         MVC   CURRQPR2,=3C'?'                                                  
         B     VK16                                                             
*                                                                               
VK15G    MVC   DTLKYL2+8(3),DMCB+1                                              
         MVC   CURRQPR2,DMCB+1                                                  
         MVC   FAKEFLD+4(3),DMCB+1                                              
         MVI   FAKEFLDH+5,7                                                     
*                                                                               
VK16     DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',FAKEFLDH,,GLVSPPRD                            
*                                                                               
VK20     MVC   GLOBLEST,SNVHDEST   COPY GLOBAL ESTIMATE                         
         XC    INTESTSD(L'INTESTSD*2),INTESTSD  CLEAR ESTIMATE DATES            
*                                                                               
         CLI   SNVHDEST,0          DO WE HAVE A GLOBAL ESTIMATE?                
         BNE   VK25                YES                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPEST                                    
         XC    SVELOCK1,SVELOCK1                                                
         XC    SVELOCK2,SVELOCK2                                                
         XC    SVELOCKP,SVELOCKP                                                
         B     VK30                NO                                           
*                                                                               
VK25     MVC   DTLKYL2+15(4),=CL4'EST='   YES                                   
         LA    R2,DTLKYL2+19                                                    
         EDIT  (B1,SNVHDEST),(3,(R2)),FILL=0                                    
*                                                                               
         L     R1,ATIOB            POINT TO ESTIMATE IN CASE OF ERROR           
         USING TIOBD,R1                                                         
         LA    R0,DTLKYL2H                                                      
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,19                                                      
         DROP  R1                                                               
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         MVC   FAKEFLDH,DTLKYL2H                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(3),0(R2)                                                 
         OI    FAKEFLDH+4,X'08'                                                 
         MVI   FAKEFLDH+5,3                                                     
*                                                                               
         LA    R2,FAKEFLDH                                                      
         MVC   QPRD,CURRQPRD                                                    
         CLC   CURRQPRD,=3C'?'                                                  
         BE    *+14                                                             
         CLC   CURRQPR2,=3C'?'                                                  
         BNE   VK26                                                             
         XC    SVELOCK1,SVELOCK1                                                
         XC    SVELOCK2,SVELOCK2                                                
         XC    SVELOCKP,SVELOCKP                                                
         B     VK27                                                             
*                                                                               
VK26     DS    0H                                                               
         GOTO1 VALIEST                                                          
*                                                                               
         L     R1,AIO                                                           
         MVC   SVECNTR1,ECNTRL-ESTHDR(R1)                                       
         MVC   SVELKYM1,ELOCKYR-ESTHDR(R1)                                      
*                                                                               
         MVC   INTESTSD,ESTSTRT    FIND THE START AND END OF EST FOR            
         MVC   INTESTED,ESTEND        BOTH PRODUCTS                             
*                                                                               
         CLC   CURRQPR2,BLANKS     NO PIGGYBACK                                 
         BNH   VK27                                                             
*                                                                               
         MVC   QPRD,CURRQPR2                                                    
         GOTO1 VALIEST                                                          
*                                                                               
         L     R1,AIO                                                           
         MVC   SVECNTR2,ECNTRL-ESTHDR(R1)                                       
         MVC   SVELKYM2,ELOCKYR-ESTHDR(R1)                                      
*                                                                               
         CLC   INTESTSD,ESTSTRT    FIND INTERSECTION OF EST DATES               
         BH    *+10                                                             
         MVC   INTESTSD,ESTSTRT                                                 
         CLC   INTESTED,ESTEND                                                  
         BL    *+10                                                             
         MVC   INTESTED,ESTEND                                                  
*                                                                               
         CLC   INTESTSD,INTESTED   MAKE SURE START DATE IS LESS THAN            
         BH    INVLFLD                END DATE                                  
*                                                                               
VK27     DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',FAKEFLDH,,GLVSPEST                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',FAKEFLDH,,GLVSPPRD                            
         LA    RE,FAKEFLD                                                       
         ZIC   R0,FAKEFLDH+5                                                    
         AR    RE,R0                                                            
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         EDIT  (B1,GLOBLEST),(3,0(RE)),FILL=0                                   
         ZIC   R1,FAKEFLDH+5                                                    
         AH    R1,=H'4'                                                         
         STC   R1,FAKEFLDH+5                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',FAKEFLDH,,GLVSPPRD                            
*                                                                               
VK30     L     R1,ATIOB            DON'T NEED TO POINT SPECIFICALLY             
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(X'12',SNVHDSDT),(0,WORK)   PERIOD DATES             
         MVC   PERSDATE,WORK                                                    
         MVC   PEREDATE,WORK+L'PERSDATE+1                                       
*********                                                                       
* DISPLAY HEADER TOTALS                                                         
*********                                                                       
         OC    SNVHDTCS,SNVHDTCS   JUST IN CASE NO VALUE                        
         BNZ   *+10                                                             
         ZAP   SNVHDTCS,=P'0'                                                   
*                                                                               
         NI    MISCFLG2,X'FF'-MF2RPNSE                                          
         TM    SNVHDCTL,SNVHDRSQ   RESPONSE INVOICE?                            
         BZ    VK40                                                             
         OI    MISCFLG2,MF2RPNSE   YES                                          
         SRP   SNVHDTCS,64-2,0     DIVIDE BY 100                                
         EDIT  (P8,SNVHDTCS),(15,DTLHNUM),COMMAS=YES,ZERO=NOBLANK               
         B     VK45                                                             
*                                                                               
VK40     EDIT  (P8,SNVHDTCS),(15,DTLHNUM),2,ZERO=NOBLANK,FLOAT=-                
*                                                                               
VK45     MVI   DTLHNUM+15,C'/'                                                  
         LA    RE,DTLHNUM+16                                                    
         EDIT  (B2,SNVHDTSP),(4,0(RE)),ZERO=NOBLANK                             
         OI    DTLHNUMH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
*****                                                                           
* VALIDATE THE FILTERS/OPTIONS                                                  
*****                                                                           
VKFLT00  LA    R2,DTLOPTNH                                                      
         MVI   FLTRFLG1,0                                                       
         MVI   FLTRFLG2,0                                                       
         MVC   BOOKOVR,BLANKS                                                   
         MVI   INTOPT,C' '                                                      
*                                                                               
         TM    4(R2),X'80'         FIELD WAS CHANGED?                           
         BZ    *+8                                                              
         OI    MISCFLG1,MF1KYCHG   YES                                          
         MVI   MATCHFL,0                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKFLTX                                                           
*                                                                               
         XCEFL BLOCK,480                                                        
         L     R1,ACOMFACS                                                      
         L     RF,CPARSNIP-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,(R2),(15,BLOCK),(X'08',0)                              
         CLI   4(R1),0                                                          
         BE    INVLFLD                                                          
         CLI   4(R1),15            TOO MANY FILTERS                             
         BE    INVLFLD                                                          
*                                                                               
         L     R1,ATIOB            SET CURSOR IN CASE OF ERROR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         LA    R3,BLOCK                                                         
         USING PSND,R3                                                          
VKFLT10  CLI   PSNTAG,0            NO INPUT ANYMORE?                            
         BE    VKFLT100            NONE                                         
*                                                                               
         LA    R1,FLDEQTBL                                                      
         ZIC   RF,PSNLEN           NO, MATCHED ON THIS ENTRY IN TABLE?          
         BCTR  RF,0                                                             
         L     RE,PSNCOMP                                                       
VKFLT15  CLI   0(R1),0             REACHED END OF TABLE?                        
         BNE   VKFLT16                                                          
*                                                                               
         CLI   PFKEY,11            REQUESTING I2?                               
         BNE   VKFLT15A            NO - OUT-OF-TABLE INPUT IS INVALID           
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=C'BOOK'                                                 
         BE    VKFLT60                                                          
*                                                                               
* INT/INTONLY OPTIONS DISABLED FOR NOW 11/04/2007                               
         B     VKFLT15A                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=C'INT'                                                  
         BNE   *+12                                                             
         MVI   INTOPT,C'I'                                                      
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=C'INTONLY'                                              
         BNE   *+12                                                             
         MVI   INTOPT,C'O'                                                      
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
VKFLT15A DS    0H                                                               
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VKFLTINV            DON'T CHECK FOR MATCH OPTION                 
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=C'MATCH'                                                
         BNE   VKFLT15B                                                         
         OI    MATCHFL,MFMATCHQ                                                 
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
VKFLT15B DS    0H                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=C'OCOST'                                                
         BNE   VKFLTINV                                                         
         OI    MATCHFL,MFOCOSTQ                                                 
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
VKFLT16  DS    0H                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),2(R1)                                                    
         BE    VKFLT20                                                          
         LA    R1,L'FLDEQTBL(R1)       NO, CHECK NEXT ENTRY                     
         B     VKFLT15                                                          
*                                                                               
VKFLTINV L     R1,ATIOB            SET CURSOR IN CASE OF ERROR                  
         USING TIOBD,R1                                                         
         L     RE,PSNCOMP                                                       
         LA    RF,DTLOPTN                                                       
         SR    RE,RF                                                            
         STC   RE,TIOBCURI                                                      
         DROP  R1                                                               
         B     INVLFLD                                                          
*********                                                                       
* DATE FILTER                                                                   
*********                                                                       
VKFLT20  CLI   0(R1),FLDNDATE      FILTER ON DATE?                              
         BNE   VKFLT22                                                          
         TM    FLTRFLG1,FF1DDATE   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL       ANY VALUE?                                   
         BZ    VKFLTINV            NO, NO DATE TO FILTER ON                     
*                                                                               
         L     R3,PSNVAL                                                        
         CLI   PSNLEN,0                                                         
         BE    VKFLTINV                                                         
         L     R0,PSNCOMP                                                       
         ST    R0,DMCB                                                          
         MVC   DMCB(1),PSNLEN                                                   
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(X'20',PERVALST)                                    
*                                                                               
         TM    DMCB+4,X'03'                                                     
         BNZ   VKFLTINV                                                         
*                                                                               
         XC    FLTRDTE1(2),FLTRDTE1                                             
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT?                         
         BE    *+12                                                             
         MVI   FLTRDTE2,C'Y'       WE HAVE A SECOND DATE                        
         B     VKFLT20G                                                         
*                                                                               
         ZIC   R1,PSNLEN                                                        
         L     RE,PSNCOMP                                                       
         LA    RE,0(R1,RE)                                                      
         BCTR  RE,0                                                             
         CLI   0(RE),C'-'          "MMMDD/YY-"?                                 
         BNE   VKFLT20G            NO, JUST A SINGLE DATE                       
         MVI   FLTRDTE2,40         YES, OPEN ENDED, SHOULD HAVE 40 DAYS         
*                                                                               
VKFLT20D USING PERVALD,PERVALST                                                 
*                                                                               
VKFLT20G TM    VKFLT20D.PVALASSM,PVALASY  YEAR ASSUMED?                         
         BZ    VKFLT21                                                          
*                                                                               
         CLC   VKFLT20D.PVALESTA+2(2),PERSDATE+2  MONTH SAME AS START?          
         BNE   *+14                                                             
         MVC   VKFLT20D.PVALESTA(2),PERSDATE      YES, USE START YEAR           
         B     VKFLT21                                                          
         MVC   VKFLT20D.PVALESTA(2),PEREDATE      NO, USE END YEAR              
*                                                                               
VKFLT21  CLC   VKFLT20D.PVALESTA,PERSDATE                                       
         BL    VKFLTINV            FILTER DATE SHOULD BE IN PERIOD              
         CLC   VKFLT20D.PVALESTA,PEREDATE                                       
         BH    VKFLTINV                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(X'20',PERSDATE),(8,FAKEFLD),               X        
               VKFLT20D.PVALESTA                                                
         MVI   FAKEFLDH+5,17                                                    
*******                                                                         
         MVC   WORK(L'PERVALST),PERVALST  <===  SAVE ORIGINAL PERVALST          
         LA    R0,FAKEFLD                                                       
         ST    R0,DMCB                                                          
         MVC   DMCB(1),FAKEFLDH+5                                               
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,PERVALST                                            
*******                                                                         
         ZICM  R1,VKFLT20D.PVALNDYS,2                                           
         BCTR  R1,0                SHOULD BE DISPLACEMENT FROM START            
         STC   R1,FLTRDTE1                                                      
*                                                                               
         CLI   FLTRDTE2,C'Y'       DO WE HAVE A SECOND DATE?                    
         BE    VKFLT21A                                                         
         CLI   FLTRDTE2,0          NO, JUST A SINGLE DATE?                      
         BNE   VKFLT21X                NO, OPEN ENDED                           
         MVC   FLTRDTE2,FLTRDTE1       YES, SAME FOR END DATE                   
         B     VKFLT21X                                                         
*******                                                                         
VKFLT21A MVC   PERVALST,WORK   <=== RESTORE ORIGINAL PERVALST                   
*******                                                                         
         TM    VKFLT20D.PVALASSM,PVALAEY  YEAR ASSUMED?                         
         BZ    VKFLT21G                                                         
*                                                                               
         CLC   VKFLT20D.PVALEEND+2(2),PERSDATE+2  MONTH SAME AS START?          
         BNE   *+14                                                             
         MVC   VKFLT20D.PVALEEND(2),PERSDATE      YES, USE START YEAR           
         B     VKFLT21G                                                         
         MVC   VKFLT20D.PVALEEND(2),PEREDATE      NO, USE END YEAR              
*                                                                               
VKFLT21G CLC   VKFLT20D.PVALEEND,PERSDATE                                       
         BL    VKFLTINV            FILTER DATE SHOULD BE IN PERIOD              
         CLC   VKFLT20D.PVALEEND,PEREDATE                                       
         BH    VKFLTINV                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(X'20',PERSDATE),(8,FAKEFLD),               X        
               VKFLT20D.PVALEEND                                                
         MVI   FAKEFLDH+5,17                                                    
         LA    R0,FAKEFLD                                                       
         ST    R0,DMCB                                                          
         MVC   DMCB(1),FAKEFLDH+5                                               
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,PERVALST                                            
*                                                                               
         ZICM  R1,VKFLT20D.PVALNDYS,2                                           
         BCTR  R1,0                SHOULD BE DISPLACEMENT FROM START            
         STC   R1,FLTRDTE2                                                      
*                                                                               
VKFLT21X OI    FLTRFLG1,FF1DDATE   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
         DROP  VKFLT20D                                                         
*********                                                                       
* SPOT LENGTH FILTER                                                            
*********                                                                       
VKFLT22  CLI   0(R1),FLDNSLEN      FILTER ON SPOT LENGTH?                       
         BNE   VKFLT24                                                          
         TM    FLTRFLG1,FF1DSLEN   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         TM    PSNSTAT,PSNNUMQ     VALID NUMERIC?                               
         BZ    VKFLTINV                                                         
*                                                                               
         L     R1,PSNNUM                                                        
         CH    R1,=H'256'                                                       
         BH    VKFLTINV                                                         
         STC   R1,FLTRSLEN                                                      
         OI    FLTRFLG1,FF1DSLEN   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* FILM CODE FILTER                                                              
*********                                                                       
VKFLT24  CLI   0(R1),FLDNFILM      FILTER ON FILM CODE?                         
         BNE   VKFLT26                                                          
         TM    FLTRFLG1,FF1DFILM   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         L     RE,PSNCOMP                                                       
         CLI   PSNLEN,0                                                         
         BE    VKFLTINV                                                         
         XC    FLTRFILM,FLTRFILM                                                
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTRFILM(0),0(RE)                                                
         OC    FLTRFILM,BLANKS                                                  
         OI    FLTRFLG1,FF1DFILM   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* 2ND FILM CODE FILTER                                                          
*********                                                                       
VKFLT26  CLI   0(R1),FLDNPFLM      FILTER ON PIGGY FILM CODE?                   
         BNE   VKFLT28                                                          
         TM    FLTRFLG1,FF1DFLM2   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         L     RE,PSNCOMP                                                       
         CLI   PSNLEN,0                                                         
         BE    VKFLTINV                                                         
         XC    FLTRFLM2,FLTRFLM2                                                
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTRFLM2(0),0(RE)                                                
         OC    FLTRFLM2,BLANKS                                                  
         OI    FLTRFLG1,FF1DFLM2   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* COST FILTER                                                                   
*********                                                                       
VKFLT28  CLI   0(R1),FLDNCOST      FILTER ON COST?                              
         BNE   VKFLT30                                                          
         TM    FLTRFLG1,FF1DCOST   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         ZIC   R0,PSNLEN                                                        
         GOTO1 CASHVAL,DMCB,PSNCOMP,(R0)                                        
         CLI   DMCB,X'FF'                                                       
         BE    VKFLTINV                                                         
         MVC   FLTRCOST,DMCB+4                                                  
         OI    FLTRFLG1,FF1DCOST   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* PRODUCT FILTER                                                                
*********                                                                       
VKFLT30  CLI   0(R1),FLDNPROD      FILTER ON PRODUCT?                           
         BNE   VKFLT32                                                          
         TM    FLTRFLG1,FF1DPROD   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         L     RE,PSNCOMP                                                       
         CLI   PSNLEN,0                                                         
         BE    VKFLTINV                                                         
         XC    FLTRPROD,FLTRPROD                                                
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTRPROD(0),0(RE)                                                
         OC    FLTRPROD,BLANKS                                                  
         OI    FLTRFLG1,FF1DPROD   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* ESTIMATE FILTER                                                               
*********                                                                       
VKFLT32  CLI   0(R1),FLDNESTM      FILTER ON ESTIMATE?                          
         BNE   VKFLT34                                                          
         TM    FLTRFLG1,FF1DESTM   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         TM    PSNSTAT,PSNNUMQ     VALID NUMERIC?                               
         BZ    VKFLTINV                                                         
         MVC   FLTRESTM,PSNNUM+3                                                
         OI    FLTRFLG1,FF1DESTM   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* RESPONSE COUNT FILTER                                                         
*********                                                                       
VKFLT34  CLI   0(R1),FLDNRCNT      FILTER ON RESPONSE COUNT?                    
         BNE   VKFLT36                                                          
         TM    FLTRFLG1,FF1DRCNT   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         TM    PSNSTAT,PSNNUMQ     VALID NUMERIC?                               
         BZ    VKFLTINV                                                         
         MVC   FLTRRCNT,PSNNUM+1                                                
         OI    FLTRFLG1,FF1DRCNT   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* NETWORK FILTER                                                                
*********                                                                       
VKFLT36  CLI   0(R1),FLDNNTWK      FILTER ON NETWORK?                           
         BNE   VKFLT37                                                          
         TM    FLTRFLG2,FF2DNTWK   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         L     RE,PSNCOMP                                                       
         CLI   PSNLEN,0                                                         
         BE    VKFLTINV                                                         
         XC    FLTRNTWK,FLTRNTWK                                                
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTRNTWK(0),0(RE)                                                
         OC    FLTRNTWK,BLANKS                                                  
         OI    FLTRFLG2,FF2DNTWK   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* BILLBOARD FILTER                                                              
*********                                                                       
VKFLT37  CLI   0(R1),FLDNBILB      ONLY SHOW THE BILLBOARD DETAILS?             
         BNE   VKFLT38                                                          
         OC    PSNVAL,PSNVAL       EITHER YOU PUT IN BILL OR NOT                
         BNZ   VKFLTINV                                                         
         OI    FLTRFLG2,FF2BLLBD   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*********                                                                       
* INTEGRATION FILTER                                                            
*********                                                                       
VKFLT38  CLI   0(R1),FLDNINTG      FILTERING ON INTEGRATION                     
         BNE   VKFLT40                                                          
         TM    FLTRFLG2,FF2INTEG   YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         ZIC   R0,PSNLEN                                                        
         GOTO1 CASHVAL,DMCB,PSNCOMP,(R0)                                        
         CLI   DMCB,X'FF'                                                       
         BE    VKFLTINV                                                         
         MVC   FLTRINTG,DMCB+4                                                  
         OI    FLTRFLG2,FF2INTEG   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
VKFLT40  DS    0H                                                               
         CLI   0(R1),FLDNNPKG      FILTER ON NETWORK PACKAGE?                   
         BNE   VKFLT60                                                          
         TM    FLTRFLG2,FF2NPKG    YES, FILTER ALREADY SET?                     
         BNZ   VKFLTINV                                                         
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
*                                                                               
         L     R3,PSNVAL                                                        
         TM    PSNSTAT,PSNNUMQ     VALID NUMERIC?                               
         BZ    VKFLTINV                                                         
*                                                                               
         L     R1,PSNNUM                                                        
         CH    R1,=H'255'                                                       
         BH    VKFLTINV                                                         
         STC   R1,FLTRNPKG                                                      
         OI    FLTRFLG2,FF2NPKG    SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
VKFLT60  DS    0H                                                               
         CLC   =C'BOOK',0(RE)                                                   
         BNE   VKFLT70                                                          
*                                                                               
         OC    PSNVAL,PSNVAL                                                    
         BZ    VKFLTINV                                                         
         L     R3,PSNVAL                                                        
*                                                                               
         CLI   PSNLEN,0                                                         
         BE    VKFLTINV                                                         
*                                                                               
         BRAS  RE,VALBOOK                                                       
         BNE   VKFLTINV                                                         
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*                                                                               
VKFLT70  B     VKFLTINV            DOESN'T MATCH A VALID FILTER                 
*                                                                               
VKFLTNXT LA    R3,PSNL(R3)         CHECK NEXT SCANNER BLOCK                     
         B     VKFLT10                                                          
*                                                                               
VKFLT100 DS    0H                                                               
         TM    MATCHFL,MFMATCHQ                                                 
         BZ    *+8                                                              
         NI    MATCHFL,X'FF'-MFOCOSTQ                                           
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC    DON'T SET CURSOR ANYMORE              
         DROP  R1                                                               
*                                                                               
VKFLTX   DS    0H                                                               
         DROP  R3                                                               
*****                                                                           
* DONE VALIDATING ALL THE KEY FIELDS                                            
*****                                                                           
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VREC     DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C's00A'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+32,DATAMGR                        
         MVC   TWODECIM,WORK+6+32  2 DECIMAL IMPRESSIONS? (AGY)                 
*                                                                               
         NI    MISCFLG1,X'FF'-MF1RDFLM   HAVEN'T READ FILMS INTO BUFFER         
         NI    MISCFLG2,X'FF'-MF2CHNGD   NOTHING HAPPENED TO DETAILS            
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    VR00                                                             
         GOTO1 =A(SETSCRN),DMCB,(RC),RR=RELO                                    
         B     VRX                                                              
*                                                                               
VR00     DS    0H                                                               
* CHECK IF CLICKS, RATINGS OR IMPRESSIONS ARE PRESENT IN DETAILS                
* IF YES - INDICATE ELEMENT LENGTH NEEDS TO BE SNVIDL4Q                         
         NI    MISCFLG3,X'FF'-MF3Q4Q                                            
*                                                                               
         LA    R1,SVFLDLST                                                      
         ZIC   R0,SVFLDCNT                                                      
*                                                                               
VR01     CLI   0(R1),FLDNCLK                                                    
         BE    VR02                                                             
         CLI   0(R1),FLDNRAT                                                    
         BE    VR02                                                             
         CLI   0(R1),FLDNIMP                                                    
         BE    VR02                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,VR01                                                          
         B     *+8                                                              
VR02     OI    MISCFLG3,MF3Q4Q     INDICATE NEED SNVIDL4Q                       
*                                                                               
*                                                                               
         LH    R2,DFIRSTLN         FIRST FIELD OF FIRST LINE                    
         AR    R2,RA                                                            
         NI    MISCFLG2,X'FF'-MF2DLRST                                          
*                                                                               
VR03     CLI   0(R2),0             WE HAVE TO RETRANSMIT THE WHOLE              
         BE    VR06                    SO THAT ANY ERRORS WILL NOT              
         ZIC   R0,0(R2)                CAUSE THE FIELD FOLLOWING TO BE          
         AR    R2,R0                   PROTECTED                                
         B     VR03                                                             
VR06     MVC   1(2,R2),=X'0101'                                                 
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   *+8                                                              
         BRAS  RE,CHKINT                                                        
*                                                                               
         LH    R2,DFIRSTLN         START VALIDATING HERE                        
         AR    R2,RA                                                            
*                                                                               
VR10     CLI   0(R2),0             END OF THE TWA?                              
         BE    VR30                                                             
         CLC   =X'0730',2(R2)         OR ON PFKEY LINE?                         
         BL    VR30                YES, DISPLAY DETAILS                         
*                                                                               
         TM    1(R2),X'20'         IS THIS FIELD PROTECTED?                     
         BNZ   VR20                                                             
*                                                                               
         TM    MISCFLG2,MF2DLRST   DELETE TILL END OF SCREEN?                   
         BZ    VR11                NO                                           
         NI    4(R2),X'FF'-X'20'   GET THIS LINE VALIDATED                      
         MVC   8(3,R2),=C'D//'     CONTINUE                                     
*                                                                               
VR11     TM    4(R2),X'20'         NO, FIELD VALIDATED BEFORE?                  
         BNZ   VR20                                                             
*                                                                               
         LR    R1,R2               FIND ORDINAL # OF RECORD IN LIST             
         SR    R1,RA                                                            
         SH    R1,DFIRSTLN                                                      
         LH    R0,BTWNR1R2                                                      
         ST    R0,FULL                                                          
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         STC   R1,NUMINLST         NUMBER OF RECORD IN LIST                     
*                                                                               
         XC    SVDMELEM,SVDMELEM   NO PREVIOUS DETAIL ELEMENT YET               
         CLI   NUMINLST,0          1ST LINE HAS NO PREVIOUS DETAIL              
         BE    VR18                                                             
*                                                                               
VR12     BCTR  R1,0                CHECK IF ANY PREVIOUS DETAILS                
         LR    RE,R1                                                            
         MH    RE,=Y(DLSTLENQ)                                                  
         LA    RE,DETLLIST(RE)                                                  
*                                                                               
         OC    0(DLSTLENQ,RE),0(RE)  NOTHING BEFORE CURRENT DETAIL              
         BNZ   VR14                                                             
*                                                                               
         OC    PREFILTD,PREFILTD                                                
         BZ    VR13                                                             
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(DLSTLENQ-1),PREFILTD                                   
         B     VR15                                                             
*                                                                               
VR13     DS    0H                                                               
         LTR   R1,R1               CHECK PREVIOUS, ANY BEFORE THAT ONE?         
         BZ    VR18                NO                                           
         B     VR12                                                             
*                                                                               
VR14     XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(DLSTLENQ-1),0(RE)                                      
*                                                                               
VR15     DS    0H                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ    MAKE SURE WE HAVE PREVIOUS DETAIL            
         BNE   UPDERR              PREVENT DUMPS ON SIMULTANEOUS UPDATE         
         CLC   SNVIDDAY(DLSTLENQ-1),MINEKEY+1                                   
         BNE   UPDERR              PREVENT DUMPS ON SIMULTANEOUS UPDATE         
         DROP  R6                                                               
         MVC   SVDMELEM,MELEM                                                   
*                                                                               
VR18     ZIC   R1,NUMINLST         SAVE DISPLACEMENT OF CHANGED LINE            
         MH    R1,BTWNR1R2                                                      
         AH    R1,DFIRSTLN                                                      
         STH   R1,DCURRENT                                                      
*                                                                               
         TM    MISCFLG1,MF1RDFLM   READ FILMS INTO BUFFER YET?                  
         BNZ   *+12                                                             
         BAS   RE,READFLMS                                                      
         OI    MISCFLG1,MF1RDFLM                                                
*                                                                               
         BRAS  RE,TSTLOCK                                                       
         BE    VR19                                                             
         MVC   GERROR,=AL2(808)                                                 
         LH    R2,DCURRENT                                                      
         AR    R2,RA                                                            
         B     ERREXIT                                                          
*                                                                               
VR19     DS    0H                                                               
         BRAS  RE,GETEST                                                        
         BE    *+12                                                             
         LA    R2,CONRECH                                                       
         B     BADESTMT                                                         
*                                                                               
         BRAS  RE,CHKEST                                                        
         BE    VR19A                                                            
         MVC   GERROR,=AL2(1223)   INVOICE PAID                                 
         CLI   BYTE,X'01'                                                       
         BE    *+10                                                             
         MVC   GERROR,=AL2(1221)   ESTIMATE LOCKED                              
         LH    R2,DCURRENT                                                      
         AR    R2,RA                                                            
         B     ERREXIT                                                          
*                                                                               
VR19A    DS    0H                                                               
         BAS   RE,VALDLINE         VALIDATE THE DETAIL LINE                     
         OI    MISCFLG2,MF2CHNGD                                                
*                                                                               
         LH    R2,DCURRENT         CHECK NEXT DETAIL FOR ANY CHANGES            
         AR    R2,RA                                                            
         AH    R2,BTWNR1R2                                                      
         B     VR10                                                             
*                                                                               
VR20     ZIC   R1,0(R2)            GOTO THE NEXT FIELD                          
         AR    R2,R1                                                            
         B     VR10                                                             
*                                                                               
VR30     TM    MISCFLG2,MF2CHNGD   A LINE WAS CHANGED?                          
         BZ    VRX                 NO                                           
*                                                                               
         CLI   PI2NAI2A,C'Y'                                                    
         BNE   VR35                                                             
         GOTO1 =A(BLDREQ),DMCB,(RC),RR=RELO                                     
         B     VR35                                                             
*                                                                               
VR32     DS    0H                                                               
         GOTO1 CKSPCLI2                                                         
         BNE   VR35                                                             
         GOTO1 BLDPRTAB                                                         
         GOTO1 GENI2                                                            
         B     DREC                NO                                           
*                                                                               
VR35     DS    0H                                                               
         TM    MISCFLG2,MF2MTIME   USES MILITARY TIME?                          
         BZ    VRX                 NO                                           
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BAS   RE,MINIOHI                                                       
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         TM    SNVHDCTL,SNVHDMTQ   USES MILITARY TIME                           
         BNZ   VRX                                                              
         OI    SNVHDCTL,SNVHDMTQ                                                
         BAS   RE,MINIOWRT                                                      
         DROP  R6                                                               
*                                                                               
VRX      TM    MISCFLG1,MF1RDFLM   READ FILMS INTO BUFFER YET?                  
         BNZ   *+8                                                              
         BAS   RE,READFLMS         YES                                          
*                                                                               
         TM    MISCFLG2,MF2CHNGD   DETAIL WAS CHANGED                           
         BO    *+12                                                             
         TM    MISCFLG1,MF1ADDNG   DETAIL WAS ADDED                             
         BNO   *+8                                                              
         BAS   RE,OFFMATCH         TURN OFF MATCH CONFIRMATION BIT              
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   *+8                                                              
         BRAS  RE,GENI2RQ                                                       
*                                                                               
         B     DREC                                                             
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES A DETAIL SCREEN LINE                                   
*                                                                               
* ON ENTRY:    DCURRENT            SET TO D(CHANGED DETAIL LINE)                
*              NUMINLST            ORDINAL NUMBER OF DETAIL IN LIST             
*                                                                               
* ON EXIT:     MELEM2              NEW OR CHANGED DETAIL ELEMENT                
***********************************************************************         
VALDLINE NTR1                                                                   
*                                  DON'T ADD FILM ELEMENTS OR CLEAR THE         
*                                      LINE WHEN REVALIDATING LINE BITS         
         NI    MISCFLG2,X'FF'-MF2ADFLM-MF2CLRLN                                 
         XC    TWOFILMS,TWOFILMS                                                
         MVI   ADDFLAGS,C'N'                                                    
         MVI   ADDFLAGS+1,C'N'                                                  
         XC    TWOLENS,TWOLENS                                                  
         XC    SVR2,SVR2                                                        
*                                                                               
         XC    MELEM2,MELEM2                                                    
         LA    R6,MELEM2                                                        
         USING SNVIDELD,R6                                                      
         MVI   SNVIDEL,SNVIDELQ                                                 
         MVI   SNVIDLEN,SNVIDL3Q   NEW LENGTH FOR ALL (AS OF 20051013)          
*                                                                               
         CLI   SVQMED,C'R'                                                      
         BNE   *+12                                                             
         CLI   SVQSTA+4,C'C'                                                    
         BE    VDLN00A                                                          
*                                                                               
         CLI   SVQSTA+4,C'S'                                                    
         BE    *+12                                                             
         CLI   SVQSTA+4,C'D'                                                    
         BNE   VDLN000                                                          
*                                                                               
VDLN00A  DS    0H                                                               
         TM    MISCFLG3,MF3Q4Q     SEE IF WE NEED SNVIDL4Q                      
         BZ    *+8                                                              
         MVI   SNVIDLEN,SNVIDL4Q                                                
*                                                                               
VDLN000  DS    0H                                                               
         LH    R2,DCURRENT                                                      
         AR    R2,RA                                                            
         LR    R3,R2                                                            
*                                                                               
VDLN00   TM    1(R3),X'20'         PROTECTED FIELD?                             
         BNZ   VDLN02              YES                                          
*                                                                               
         CLI   5(R3),0             IF NOTHING IN THIS LINE                      
         BNE   VDLN05                                                           
*                                                                               
VDLN02   ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),0             HIT END OF SCREEN?                           
         BE    VDLN150             YES, DONE WITH THE DETAIL                    
*                                                                               
         ZICM  R1,2(R3),2          SEE IF WE'RE DONE WITH THIS DETAIL           
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         AH    R0,=H'1'                                                         
         CH    R0,=H'2'            EVERY DETAIL STARTS AT COLUMN 2              
         BNE   VDLN00              STILL THE SAME DETAIL, MORE FIELDS           
         B     VDLN150             NOTHING ON THIS LINE, SKIP IT                
*                                                                               
VDLN05   TM    MISCFLG1,MF1GBLPR   GOT GLOBAL PRODUCT?                          
         BNZ   VDLN07              YES                                          
*                                  CLEAR CURRENT PRODUCT INFO                   
         XC    CURRBPRD(L'CURRBPRD*2+L'CURRQPRD*2),CURRBPRD                     
*                                                                               
VDLN07   NI    MISCFLG1,X'FF'-MF1ADDNG   DON'T KNOW IF WE'RE ADDING YET         
         ZIC   R1,NUMINLST         CHECK IF ANY DETAIL HERE FOR THIS            
         MH    R1,=Y(DLSTLENQ)         CHANGED LINE                             
         LA    R1,DETLLIST(R1)                                                  
         OC    0(DLSTLENQ,R1),0(R1)                                             
         BNZ   *+12                DETAIL HERE, CHANGE IT                       
         OI    MISCFLG1,MF1ADDNG   GOING TO ADD DETAIL ELEMENT                  
         B     VDLN10                                                           
*                                                                               
         XC    MINEKEY,MINEKEY     FETCH THE DETAIL                             
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(DLSTLENQ-1),0(R1)                                      
*                                                                               
         BAS   RE,MINIOHI          DETAIL BETTER EXIST                          
         BNE   UPDERR                                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ    MAKE SURE WE HAVE PREVIOUS DETAIL            
         BNE   UPDERR                                                           
         CLC   SNVIDDAY(DLSTLENQ-1),0(R1)                                       
         BNE   UPDERR                                                           
*                                                                               
         LA    R6,MELEM2           R6 = A(USER CHANGED DETAIL)                  
         MVC   MELEM2,MELEM        COPY DETAIL SO WE CAN MAKE CHANGES           
*                                                                               
         CLI   SVQMED,C'R'                                                      
         BNE   *+12                                                             
         CLI   SVQSTA+4,C'C'                                                    
         BE    VDLN08                                                           
*                                                                               
         CLI   SVQSTA+4,C'S'                                                    
         BE    *+12                                                             
         CLI   SVQSTA+4,C'D'                                                    
         BNE   VDLN10                                                           
*                                                                               
VDLN08   DS    0H                                                               
         TM    MISCFLG3,MF3Q4Q     SEE IF WE NEED SNVIDL4Q                      
         BZ    *+8                                                              
         MVI   SNVIDLEN,SNVIDL4Q                                                
*                                                                               
VDLN10   ZICM  RE,5(R2),1          DELETING LINE?                               
         BZ    VDLN15              NO, NO DATA IN 1ST FIELD OF LINE             
*                                                                               
         CLC   =C'D//',8(R2)       DELETE FROM THIS LINE TILL END?              
         BNE   *+12                                                             
         OI    MISCFLG2,MF2DLRST                                                
         B     VDLN11A             YES                                          
*                                                                               
         ZIC   RE,0(R2)            TEST TO SEE IF THEY CAN TYPE 'DEL'           
         SH    RE,=H'17'                                                        
         CH    RE,=H'1'                                                         
         BH    VDLN10A                                                          
         EX    RE,*+8              NO, CHECK IF 'D' OR 'DE'                     
         B     *+10                                                             
         CLC   8(0,R2),=C'DEL'                                                  
         BNE   VDLN15                                                           
         B     VDLN11A             WE CAN DELETE                                
*                                                                               
VDLN10A  ZIC   RE,5(R2)            SO THEY CAN TYPE 'DEL' FOR DELETE            
         CH    RE,=H'3'                                                         
         BL    VDLN15              CAN'T DELETE IF LESS THAN 3                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'DELETE'                                               
VDLN11   BNE   VDLN15              NO, 1ST FIELD SHOULD READ 'DELETE'           
*                                                                               
VDLN11A  OI    MISCFLG2,MF2CLRLN   YES, CLEAR LINE WHEN REVALIDATING            
         TM    MISCFLG1,MF1ADDNG   DELETING AN NEW LINE?                        
         BNZ   VDLN150             YES, JUST CLEAR AND REVALIDATE LINE          
*                                                                               
         ICM   RE,15,SNVIDCST      ADJUST THE TOTALS FOR THE DELETION           
         TM    MISCFLG2,MF2RPNSE   RESPONSE INVOICE?                            
         BZ    *+10                                                             
         SR    RE,RE                                                            
         ICM   RE,7,SNVIDRSP                                                    
*                                                                               
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   RE,RE               YES                                          
*                                                                               
VDLN12   CVD   RE,TEMPPL8                                                       
         SP    TOTAMNTS,TEMPPL8                                                 
         SP    TOTSPOTS,=P'1'                                                   
*                                                                               
         BAS   RE,CHKFLTRS         DID OLD DETAIL PASS FILTERS?                 
         BNE   VDLN13                                                           
         SP    FLTAMNTS,TEMPPL8    YES, ADJUST FILTER AMOUNTS AND SPOTS         
         SP    FLTSPOTS,=P'1'                                                   
*                                                                               
VDLN13   DS    0H                                                               
         BAS   RE,MINIODEL         DELETE RECORD                                
         XC    0(DLSTLENQ,R1),0(R1)  CLEAR LIST TABLE ENTRY                     
         B     VDLN150             NOW CLEAR AND REVALIDATE LINE                
*                                                                               
VDLN15   TM    1(R2),X'20'         PROTECTED FIELD?                             
         BNZ   VDLN20              YES                                          
*                                                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 GTROUTIN,DMCB,(C'V',(R2))                                        
         L     RF,DMCB                                                          
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC),(R2),(R6)                                         
*                                                                               
VDLN20   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R2),0             HIT END OF SCREEN?                           
         BE    VDLN25              YES, DONE WITH THE DETAIL                    
*                                                                               
         ZICM  R1,2(R2),2          SEE IF WE'RE DONE WITH THIS DETAIL           
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         AH    R0,=H'1'                                                         
         CH    R0,=H'2'            EVERY DETAIL STARTS AT COLUMN 2              
         BNE   VDLN15              STILL THE SAME DETAIL, MORE FIELDS           
*                                                                               
VDLN25   DS    0H                                                               
         BRAS  RE,CKFLEN                                                        
         BE    VDLN25A                                                          
*                                                                               
         CLI   P0TIAIF,C'Y'        TEST OK TO ACCEPT BAD FILMS                  
         BE    VDLN25A                                                          
         ICM   R2,15,SVR2                                                       
         MVI   GERROR1,FSLNERR     FILM & SPOT LENGTH DON'T AGREE               
         B     ERREXIT                                                          
*                                                                               
VDLN25A  DS    0H                                                               
         TM    MISCFLG1,MF1ADDNG   ADDING THE DETAIL ELEMENT?                   
         BNZ   VDLN50              YES                                          
*                                                                               
*                                  DATE AND TIME WASN'T CHANGED?                
         CLC   SNVIDDAY(DLSTLENQ-1),MELEM+SNVIDDAY-SNVIDELD                     
         BNE   VDLN50                                                           
*                                                                               
         CLI   PI2YDUP,C'Y'        ALLOW DUPLICATES?                            
         BE    VDLN27              YES                                          
*                                                                               
         XC    MINEKEY,MINEKEY     LOOK FOR SAME DATE/TIME                      
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(SNVIDSEQ-SNVIDDAY),SNVIDDAY                            
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                OUR PREVIOUS BETTER BE THERE                 
*                                                                               
VDLN25A0 CLI   MELEM,SNVIDELQ      STILL AN INVOICE ITEM?                       
         BNE   VDLN26                                                           
*                                                                               
         CLC   SNVIDDAY(SNVIDSEQ-SNVIDDAY),MELEM+SNVIDDAY-SNVIDELD              
         BNE   VDLN26                                                           
*                                                                               
         CLC   SNVIDSEQ,MELEM+SNVIDSEQ-SNVIDELD   SAME SEQ?                     
         BE    VDLN25A8                           YES, SKIP IT                  
*                                                                               
         CLC   SNVIDSLN(SNVIDCML-SNVIDSLN),MELEM+SNVIDSLN-SNVIDELD              
         BNE   VDLN25A8                                                         
         CLC   SNVIDCTL,MELEM+SNVIDCTL-SNVIDELD                                 
         BNE   VDLN25A8                                                         
         CLC   SNVIDNWK,MELEM+SNVIDNWK-SNVIDELD                                 
         BNE   VDLN25A8                                                         
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   VDLN70ER                                                         
         CLI   MELEM+SNVIDLEN-SNVIDELD,SNVIDL2Q                                 
         BNH   VDLN70ER                                                         
*        CLC   SNVIDAP1(SNVIDL3Q-SNVIDL2Q),MELEM+SNVIDAP1-SNVIDELD              
         CLC   SNVIDAP1(L'SNVIDAP1+L'SNVIDAP2),MELEM+SNVIDAP1-SNVIDELD          
*        BNE   VDLN80                                                           
         BNE   VDLN25A8                                                         
         LH    R2,DCURRENT                                                      
         AR    R2,RA                                                            
         B     DUPEDERR            NO, DUPLICATE INVOICE ITEM                   
*                                                                               
VDLN25A8 BAS   RE,MINIOSEQ                                                      
         BE    VDLN25A0                                                         
*                                                                               
VDLN26   XC    MINEKEY,MINEKEY     LOOK FOR SAME DATE/TIME/SEQ                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(SNVIDSLN-SNVIDDAY),SNVIDDAY                            
         BAS   RE,MINIOHI                                                       
*                                                                               
*                                  ADJUST THE TOTALS FROM OLD DETAIL            
VDLN27   ICM   RE,15,MELEM+SNVIDCST-SNVIDELD                                    
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    *+10                                                             
         SR    RE,RE                                                            
         ICM   RE,7,MELEM+SNVIDRSP-SNVIDELD                                     
*                                                                               
         TM    MELEM+SNVIDCTL-SNVIDELD,SNVIDNGQ   NEGATIVE AMOUNT?              
         BZ    *+6                                                              
         LNR   RE,RE               YES                                          
*                                                                               
VDLN30   CVD   RE,TEMPPL8                                                       
         SP    TOTAMNTS,TEMPPL8                                                 
*                                                                               
         LA    R6,MELEM            POINT TO OLD ONE HERE                        
         BAS   RE,CHKFLTRS         DID OLD DETAIL PASS FILTERS?                 
         LA    R6,MELEM2                                                        
         BNE   VDLN35                                                           
         SP    FLTAMNTS,TEMPPL8    YES, ADJUST FILTER AMOUNTS AND SPOTS         
         SP    FLTSPOTS,=P'1'                                                   
*                                                                               
VDLN35   MVC   MELEM,MELEM2        NO, WRITE THIS DETAIL OUT                    
         XC    SNVIDBES(6),SNVIDBES                                             
         BAS   RE,MINIOWRT                                                      
*                                                                               
         ICM   RE,15,SNVIDCST      ADJUST THE TOTALS FOR NEW DETAIL             
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    *+10                                                             
         SR    RE,RE                                                            
         ICM   RE,7,SNVIDRSP                                                    
*                                                                               
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   RE,RE               YES                                          
*                                                                               
VDLN40   CVD   RE,TEMPPL8                                                       
         AP    TOTAMNTS,TEMPPL8                                                 
*                                                                               
         XC    PREFILTD,PREFILTD   CLEAR PRE-FILTER ELEM                        
         BAS   RE,CHKFLTRS         DOES NEW DETAIL PASS FILTERS?                
*        BNE   VDLN45                                                           
*                                                                               
         BE    VDLN42                                                           
         OI    MISCFLG2,MF2CLRLN                                                
         ZIC   R1,NUMINLST         CHECK IF ANY DETAIL HERE FOR THIS            
         MH    R1,=Y(DLSTLENQ)         CHANGED LINE                             
         LA    R1,DETLLIST(R1)                                                  
*                                                                               
         MVC   PREFILTD,0(R1)                                                   
*                                                                               
         XC    0(DLSTLENQ,R1),0(R1)                                             
         B     VDLN45                                                           
*                                                                               
VDLN42   DS    0H                                                               
         AP    FLTAMNTS,TEMPPL8    YES, ADJUST FILTER AMOUNTS AND SPOTS         
         AP    FLTSPOTS,=P'1'                                                   
*                                                                               
VDLN45   MVC   SVDMELEM,MELEM                                                   
         B     VDLN150                                                          
*                                                                               
VDLN50   XC    MINEKEY,MINEKEY     SEE IF WE NEED A SEQ NUMBER                  
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(SNVIDSEQ-SNVIDDAY),SNVIDDAY                            
*                                                                               
         MVI   BYTE,0                                                           
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    VDLN60                                                           
*                                                                               
         CLI   MINERR,MINEEOF                                                   
         BE    VDLN90                                                           
         CLI   MINERR,MINERNF                                                   
         BE    VDLN90                                                           
         DC    H'0'                                                             
*                                                                               
VDLN60   CLC   SNVIDDAY(SNVIDSEQ-SNVIDDAY),MELEM+SNVIDDAY-SNVIDELD              
         BNE   VDLN90                                                           
*                                                                               
VDLN70   MVC   BYTE,MELEM+SNVIDSEQ-SNVIDELD    COPY CURRENT SEQ NUMBER          
*                                                                               
         CLI   PI2YDUP,C'Y'        ALLOW DUPLICATES?                            
         BE    VDLN80              YES                                          
*                                                                               
         CLC   SNVIDSLN(SNVIDCML-SNVIDSLN),MELEM+SNVIDSLN-SNVIDELD              
         BNE   VDLN80                                                           
         CLC   SNVIDCTL,MELEM+SNVIDCTL-SNVIDELD                                 
         BNE   VDLN80                                                           
         CLC   SNVIDNWK,MELEM+SNVIDNWK-SNVIDELD                                 
         BNE   VDLN80                                                           
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   VDLN70ER                                                         
         CLI   MELEM+SNVIDLEN-SNVIDELD,SNVIDL2Q                                 
         BNH   VDLN70ER                                                         
*        CLC   SNVIDAP1(SNVIDL3Q-SNVIDL2Q),MELEM+SNVIDAP1-SNVIDELD              
         CLC   SNVIDAP1(L'SNVIDAP1+L'SNVIDAP2),MELEM+SNVIDAP1-SNVIDELD          
         BNE   VDLN80                                                           
*                                                                               
VDLN70ER LH    R2,DCURRENT                                                      
         AR    R2,RA                                                            
         B     DUPEDERR            NO, DUPLICATE INVOICE ITEM                   
*                                                                               
VDLN80   BAS   RE,MINIOSEQ                                                      
         BNE   VDLN90                                                           
*                                                                               
         CLI   MELEM,SNVIDELQ      STILL DETAIL ELEMENT?                        
         BNE   VDLN90                                                           
         CLC   SNVIDDAY(SNVIDSEQ-SNVIDDAY),MELEM+SNVIDDAY-SNVIDELD              
         BE    VDLN70                                                           
*                                                                               
VDLN90   ZIC   R1,BYTE                                                          
         AH    R1,=H'1'                                                         
         CH    R1,=H'256'                                                       
         BL    VDLN92                                                           
         LH    R2,DCURRENT                                                      
         AR    R2,RA                                                            
         B     ERR255                                                           
*                                                                               
*        BL    *+6                                                              
*        DC    H'0'                DIE IF SEQUENCE NUMBER OVERFLOW              
*                                                                               
VDLN92   DS    0H                                                               
         STC   R1,SNVIDSEQ         SEQUENCE NUMBER OF DETAIL                    
*                                                                               
VDLN100  XC    SNVIDBES(6),SNVIDBES                                             
         MVC   MELEM,MELEM2                                                     
*                                                                               
         BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDMELEM,MELEM                                                   
*                                                                               
         ICM   RE,15,SNVIDCST      ADJUST THE TOTALS FOR DETAIL                 
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    *+10                                                             
         SR    RE,RE                                                            
         ICM   RE,7,SNVIDRSP                                                    
*                                                                               
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   RE,RE               YES                                          
*                                                                               
VDLN103  CVD   RE,TEMPPL8                                                       
         AP    TOTAMNTS,TEMPPL8                                                 
*                                                                               
         BAS   RE,CHKFLTRS         DOES NEW DETAIL PASS FILTERS?                
         BNE   VDLN104                                                          
         AP    FLTAMNTS,TEMPPL8    YES, ADJUST FILTER AMOUNTS AND SPOTS         
         AP    FLTSPOTS,=P'1'                                                   
*                                                                               
VDLN104  ZIC   R1,NUMINLST         POINT TO DETAIL LIST ENTRY                   
         MH    R1,=Y(DLSTLENQ)                                                  
         LA    R1,DETLLIST(R1)                                                  
*                                                                               
         TM    MISCFLG1,MF1ADDNG   JUST A PLAIN ADD?                            
         BZ    VDLN105                                                          
         AP    TOTSPOTS,=P'1'      YES                                          
         B     VDLN110                                                          
*                                                                               
VDLN105  XC    MINEKEY,MINEKEY     SAVE THE KEY SO WE CAN DELETE LATER          
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(DLSTLENQ-1),0(R1)                                      
*                                                                               
VDLN110  MVC   0(DLSTLENQ,R1),SNVIDDAY                                          
*                                                                               
         TM    MISCFLG1,MF1ADDNG   JUST A PLAIN ADD?                            
         BNZ   VDLN150             YES                                          
*                                                                               
         BAS   RE,MINIOHI          DETAIL BETTER EXIST                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  ADJUST THE TOTALS FROM OLD  DETAIL           
         ICM   RE,15,MELEM+SNVIDCST-SNVIDELD                                    
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    *+10                                                             
         SR    RE,RE                                                            
         ICM   RE,7,MELEM+SNVIDRSP-SNVIDELD                                     
*                                                                               
         TM    MELEM+SNVIDCTL-SNVIDELD,SNVIDNGQ   NEGATIVE AMOUNT?              
         BZ    *+6                                                              
         LNR   RE,RE               YES                                          
*                                                                               
VDLN120  CVD   RE,TEMPPL8                                                       
         SP    TOTAMNTS,TEMPPL8                                                 
*                                                                               
         LA    R6,MELEM                                                         
         BAS   RE,CHKFLTRS         DID OLD DETAIL PASS FILTERS?                 
         LA    R6,MELEM2                                                        
         BNE   VDLN130                                                          
         SP    FLTAMNTS,TEMPPL8    YES, ADJUST FILTER AMOUNTS AND SPOTS         
         SP    FLTSPOTS,=P'1'                                                   
*                                                                               
VDLN130  DS    0H                                                               
         BAS   RE,MINIODEL         NOW WE CAN REMOVE THE DETAIL                 
*                                                                               
VDLN150  LH    R2,DCURRENT         RE-VALIDATE THE LINE FIELDS                  
         AR    R2,RA                                                            
*                                                                               
VDLN160  TM    1(R2),X'20'         PROTECTED FIELD?                             
         BNZ   VDLN170             YES                                          
*                                                                               
         OI    4(R2),X'20'         SET VALIDATED BIT ON                         
*                                                                               
         TM    MISCFLG2,MF2CLRLN   CLEAR FIELD WHILE REVALIDATING?              
         BZ    VDLN170                                                          
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       YES                                          
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VDLN170  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R2),0             HIT END OF SCREEN?                           
         BE    VDLN180             YES, DONE WITH THE DETAIL                    
*                                                                               
         ZICM  R1,2(R2),2          SEE IF WE'RE DONE WITH THIS DETAIL           
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         AH    R0,=H'1'                                                         
         CH    R0,=H'2'            EVERY DETAIL STARTS AT COLUMN 2              
         BNE   VDLN160             STILL THE SAME DETAIL, MORE FIELDS           
*                                                                               
VDLN180  TM    MISCFLG2,MF2ADFLM   DO WE NEED TO ADD FILM ELEMS?                
         BZ    VDLN190             NO                                           
*                                                                               
         LA    R2,TWOFILMS                                                      
VDLN182  LA    R3,BUFFER                                                        
         USING BUFFDSCT,R3                                                      
*                                                                               
VDLN184  CLI   0(R2),0             ANY FILMS HERE?                              
***      BE    VDLN190                                                          
         BE    VDLN188             MAKE SURE WE PROCESSED BOTH FILMS!           
         CLI   2(R2),C'Y'          ADDFLAG?                                     
         BNE   VDLN188                                                          
*                                                                               
         ZIC   R1,0(R2)            YES                                          
*                                                                               
VDLN186  CLI   BUFFCODE,0          FILM BETTER BE IN BUFFER                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLM   R1,1,BUFFCODE       FOUND THE FILM IN BUFFER?                    
         BE    *+12                                                             
         LA    R3,BUFFNEXT                                                      
         B     VDLN186                                                          
*                                                                               
         XC    MELEM,MELEM         BUILD FILM ELEMENT FOR THIS CODE             
         LA    R6,MELEM                                                         
         USING SNVCMELD,R6                                                      
         MVI   SNVCMEL,SNVCMELQ                                                 
         MVI   SNVCMLEN,SNVCMLNQ                                                
         MVC   SNVCMICD,BUFFCODE                                                
         MVC   SNVCMSEQ,BUFFSEQN                                                
         MVC   SNVCMCD(L'SNVCMCD+L'SNVCMSUF),BUFFCMCD                           
         OC    SNVCMCD(L'SNVCMCD+L'SNVCMSUF),BLANKS                             
         TM    BUFFSTAT,BUFFSHD                                                 
         BZ    *+8                                                              
         OI    SNVCMFLG,SNVCMHDQ                                                
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVCMELQ                                                 
         MVC   MINEKEY+1(SNVCMCD-SNVCMICD),SNVCMICD                             
         DROP  R6                                                               
*                                                                               
         BAS   RE,MINIOADD         SHOULDN'T HAVE ANY PROBLEMS ADDING           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VDLN188  DS    0H                                                               
         LA    R2,1(R2)                                                         
         LA    R0,TWOFILMS+L'TWOFILMS                                           
         CR    R2,R0                                                            
         BL    VDLN182                                                          
*                                                                               
VDLN190  DS    0H                                                               
*                                                                               
VDLNX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL SIGNAL TO MATCHMAKER THAT THIS RECORD NO LONGER             
* MATCHS BECAUSE CHANGES TO THE DETAILS HAVE BEEN MADE OR NEW DETAIL(S)         
* HAVE BEEN ADDED.                                                              
*                                                                               
* ON EXIT:     RECORD MATCH FLAG TURNED OFF IN SNVDSTAT+1                       
*              SNVIDBES,SNVIDBLN,SNVIDBDT,SNVIDBNO,DNVIDBOR : CLEARED!          
*              RECORD MATCH FLAG IN PASSIVE KEY AND RECORD ALSO OFFED           
***********************************************************************         
OFFMATCH NTR1                                                                   
         MVI   MISCFLG3,0                                                       
         LA    R6,MINMKEY                                                       
         USING SNVKEY,R6                                                        
***********************************************************************         
*        KEY:  TURN MATCH STATUS BIT OFF.                                       
***********************************************************************         
***                                                                             
* IF WE DELETED ALL THE DETAILS THEN DELETE THE PASSIVE POINTER                 
***                                                                             
         XC    MINEKEY,MINEKEY     IF '40' ELEM NOT FOUND                       
         MVI   MINEKEY,X'40'       PROBABLY MEANS DETAILS WERE DELETED          
         MVI   MINFILTL,1          WHICH MEANS WE SHOULD DELETE THE             
         XC    MELEM,MELEM         PASSIVE KEY                                  
         LA    R3,MELEM                                                         
         USING SNVMMELD,R3                                                      
         BRAS  RE,MINIOHI          FOUND A DETAIL ELEM?                         
         BE    OFF0A               YES, DONE BECAUSE WE DID NOT DELETE          
         CLI   SNVMMEL,X'40'       DETAIL ELEM?                                 
         BE    OFF0A               YES, DONE BECAUSE WE DID NOT DELETE          
         OI    MISCFLG3,MF3DELP    DEL PASSIVE (NO MORE DETAIL ELEMS)           
         DROP  R3                                                               
*                                                                               
OFF0A    TM    SNVDSTAT+1,X'80'    MATCH BIT ON OR NO?                          
         BNZ   OFF0B               ON                                           
         TM    MISCFLG3,MF3DELP    DEL PASSIVE?                                 
         BZ    OFFX                NOPE, NOTHING TO DO HERE                     
*                                                                               
OFF0B    NI    SNVDSTAT+1,X'FF'-X'80' TURN MATCH BIT OFF                        
***********************************************************************         
*        PASSIVE KEY:   BUILD PASSIVE KEY.                                      
***********************************************************************         
         XC    XPKEY,XPKEY                                                      
         CLI   NETPAKSW,C'Y'          NET MEDIA?                                
         BE    OFF00                  YES, BUILD NETPACK PASSIVE KEY            
*                                                                               
         MVC   XPKEY(2),=X'0E83'                                                
         MVC   XPKEY+2(1),SNVKAM      AGY/MD                                    
         MVC   XPKEY+5(3),SNVKSTA     STATION                                   
         MVC   XPKEY+8(2),SNVKMOS     YYMM01                                    
         MVC   XPKEY+10(2),SNVKCLT    CLIENT                                    
         MVC   XPKEY+17(10),SNVKINV   INVOICE NUMBER                            
         B     OFF01                                                            
***                                                                             
* START BUILDING NETPACK PASSIVE KEY FROM ACTIVE KEY                            
***                                                                             
OFF00    DS    0H                                                               
N        USING SNVNKEY,XPKEY                                                    
         MVI   N.SNVNTYP,SNVNTYPQ       X'0E'                                   
         MVI   N.SNVNSUB,SNVNSUBQ       X'93'                                   
         MVC   N.SNVNAM,SNVKAM          A/M                                     
         MVC   N.SNVNCLT,SNVKCLT        CLIENT                                  
         MVC   N.SNVNMOS,SNVKMOS        MONTH OF SERVICE                        
         MVC   N.SNVNINV,SNVKINV        INVOICE NUMBER                          
*                                                                               
OFF01    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'E8'                                                    
         MVI   MINFILTL,1                                                       
         XC    MELEM,MELEM                                                      
         LA    R6,MELEM                                                         
         USING SNVMMELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BNE   OFFX                                                             
         CLI   SNVMMEL,SNVMMELQ       FOUND MATCHMAKER STATUS ELEMENT?          
         BNE   OFFX                   NO, DONE                                  
*                                                                               
         CLI   NETPAKSW,C'Y'          NET MEDIA?                                
         BE    OFF02                  YES, BUILD NETPACK PASSIVE KEY            
*                                                                               
         MVC   XPKEY+3(2),SNVMMMKT    MARKET                                    
         MVC   XPKEY+12(3),SNVMMRP1   PRODUCT                                   
         MVC   XPKEY+15(1),SNVMMRE1   ESTIMATE                                  
         MVC   XPKEY+16(1),SNVMMRE2   ESTIMATE 2                                
         MVC   XPKEY+27(3),SNVMMRP2   PRODUCT 2                                 
         B     OFF03                                                            
***                                                                             
* CONTUNUE BUILDING NETPACK PASSIVE KEY FROM E8 ELEM                            
***                                                                             
OFF02    MVC   N.SNVNNETW,SNVMMNWK      NETWORK                                 
         MVC   N.SNVNPRD,SNVMMRP1       PRODUCTS                                
         MVC   N.SNVNPRD2,SNVMMRP2      PIGGY                                   
         MVC   N.SNVNEST,SNVMMRE1       ESTIMATE                                
         MVC   N.SNVNEST2,SNVMMRE2      ESTIMATE                                
         MVC   N.SNVNPGR,SNVMMRPG       PRODUCT GROUP                           
         DROP  N                                                                
***********************************************************************         
*        PASSIVE KEY: TURN OFF MATCH STATUS BIT                                 
***********************************************************************         
OFF03    MVC   XSVPKEY,XPKEY       SAVE THE PASSIVE KEY                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',INVDIR,XPKEY,XPKEY,0                     
         CLC   XSVPKEY(32),XPKEY                                                
         BNE   OFFX                                                             
         NI    XPKEY+33,X'FF'-X'80'                                             
         TM    MISCFLG3,MF3DELP    DEL PASSIVE?                                 
         BZ    *+8                 NO                                           
         OI    XPKEY+32,X'80'      PASSIVE DELETED                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',INVDIR,XPKEY,XPKEY,0                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OFFX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DREC     DS    0H                                                               
*********                                                                       
* DISPLAY DETAIL TOTALS                                                         
*********                                                                       
DR00     TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BZ    DR00X               NO, DON'T GO THROUGH DETAILS AGAIN           
*                                                                               
         XC    MINEKEY,MINEKEY     CALCULATE THE TOTAL OF THE DETAILS           
         MVI   MINEKEY,SNVIDELQ                                                 
         ZAP   TOTAMNTS,=P'0'      TOTAL/NUMBER FOR ALL THE DETAILS             
         ZAP   TOTSPOTS,=P'0'                                                   
         ZAP   FLTAMNTS,=P'0'      TOTAL/NUMBER FOR THOSE FILTERED              
         ZAP   FLTSPOTS,=P'0'                                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    DR00LOOP                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    DR00X                                                            
         DC    H'0'                                                             
*                                                                               
DR00LOOP L     R6,MINELEM          SUM UP THE DETAIL AMOUNTS FROM THE           
         USING SNVIDELD,R6             DETAILS                                  
         CLI   SNVIDEL,SNVIDELQ                                                 
         BNE   DR00X                                                            
*                                                                               
         BAS   RE,CHKFLTRS         GOT FILTER TOTALS?                           
         BNE   DR04                NOT FOR THIS DETAIL                          
*                                                                               
         ICM   R1,15,SNVIDCST      COST                                         
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    DR02                                                             
         ZICM  R1,SNVIDRSP,3                                                    
*                                                                               
DR02     TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         CVD   R1,DUB                                                           
         AP    FLTAMNTS,DUB                                                     
         AP    FLTSPOTS,=P'1'                                                   
*                                                                               
DR04     ICM   R1,15,SNVIDCST                                                   
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    DR06                                                             
         ZICM  R1,SNVIDRSP,3                                                    
*                                                                               
DR06     TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         CVD   R1,DUB                                                           
         AP    TOTAMNTS,DUB                                                     
         AP    TOTSPOTS,=P'1'                                                   
*                                                                               
DR00NEXT BAS   RE,MINIOSEQ                                                      
         BE    DR00LOOP                                                         
         DROP  R6                                                               
*                                                                               
DR00X    TM    MISCFLG2,MF2RPNSE                                                
         BZ    DR00X1                                                           
         EDIT  (P16,TOTAMNTS),(15,DTLDNUM),COMMAS=YES,ZERO=NOBLANK              
         B     DR00X2                                                           
DR00X1   EDIT  (P16,TOTAMNTS),(15,DTLDNUM),2,ZERO=NOBLANK,FLOAT=-               
*                                                                               
DR00X2   MVI   DTLDNUM+15,C'/'                                                  
         LA    RE,DTLDNUM+16                                                    
         EDIT  (P4,TOTSPOTS),(4,0(RE)),ZERO=NOBLANK                             
*                                                                               
         XC    DTLFNUM,DTLFNUM     CLEAR ANY FILTER NUMBERS FIRST               
         OI    DTLFNUMH+6,X'80'                                                 
*                                                                               
         OC    FLTRFLG1(NUMFLTRS),FLTRFLG1   ANY FILTERS?                       
         BZ    DR00X5                        NONE                               
*                                                                               
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    DR00X3                                                           
         EDIT  (P16,FLTAMNTS),(15,DTLFNUM),COMMAS=YES,ZERO=NOBLANK              
         B     DR00X4                                                           
DR00X3   EDIT  (P16,FLTAMNTS),(15,DTLFNUM),2,ZERO=NOBLANK,FLOAT=-               
*                                                                               
DR00X4   MVI   DTLFNUM+15,C'/'                                                  
         LA    RE,DTLFNUM+16                                                    
         EDIT  (P4,FLTSPOTS),(4,0(RE)),ZERO=NOBLANK                             
*                                                                               
DR00X5   DS    0H                                                               
*                                                                               
DR10     DS    0H                                                               
         LH    R2,DFIRSTLN         CLEAR THE SCREEN                             
         AR    R2,RA                                                            
         OI    6(R2),X'40'         POSITION CURSOR HERE                         
*                                                                               
         XC    QFILM,QFILM         CLEAR THE FILM CODE NOW                      
*                                                                               
         TM    MISCFLG2,MF2CHNGD   DID ANYTHING HAPPEN ON THIS PAGE?            
         BZ    DR11                NO                                           
         CLI   PFKEY,6             YES, FORCE TO A CERTAIN POSITION?            
         BL    *+12                                                             
         CLI   PFKEY,9                                                          
         BNH   DR11                     YES                                     
         MVI   NXTUPKEY,SNVIDELQ        NO, STAY ON THIS SCREEN                 
         MVC   NXTUPKEY+1(DLSTLENQ-1),DETLLIST                                  
*                                                                               
DR11     SR    RE,RE               TWAXC (R2) WITH VALIDATE FIELDS              
         LR    R1,R2                                                            
         LA    RF,4095(R1)                                                      
*                                                                               
DR12     IC    RE,0(R1)                                                         
*                                                                               
         C     R1,LASTLINE         ARE WE AT PF KEY LINE?                       
         BNL   DR13         IF YES - SKIP MATCHFL CHECK. CAN'T CLEAR IT         
*                                                                               
         TM    MATCHFL,MFMATCHQ                                                 
         BO    DR14                SKIP OVER TM                                 
*                                                                               
DR13     DS    0H                                                               
         TM    1(R1),X'20'         PROTECTED FIELD?                             
         BZ    DR14                NO - CLEAR IT                                
*                                                                               
* YES,FIELD IS PROTECTED                                                        
* SEE IF PROTECTED FIELD IS ORIGINAL COST                                       
         TM    MATCHFL,MFOCOSTQ    DISPLAYING ORIG COST?                        
         BZ    DR16                NO - SKIP THIS PROTECTED FIELD               
         TM    1(R1),X'02'         EXTENDED FIELD?                              
         BZ    DR16                NO - IT CAN'T BE ORIG COST                   
         LR    R0,RE               FIELD LENGTH                                 
         SH    R0,=H'8'            YES, TAKE OFF L(EXTEDNED HEADER)             
         LR    R3,R1               A(FIELD)                                     
         AR    R3,R0                                                            
         CLI   0(R3),FLDNOCST      IS THIS FIELD ORIG COST?                     
         BNE   DR16                NO - DON'T CLEAR IT                          
*                                                                               
DR14     DS    0H                                                               
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'         EXTENDED FIELD?                              
         BZ    *+8                                                              
         SH    RE,=H'8'            YES, TAKE OFF L(EXTEDNED HEADER)             
         LTR   RE,RE                                                            
         BM    DR18                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)       CLEAR THE FIELD                              
         MVI   5(R1),0                                                          
         OI    6(R1),X'80'         TRANSMIT THE FIELD                           
         OI    4(R1),X'20'         VALIDATE THE FIELD                           
         IC    RE,0(R1)                                                         
DR16     BXLE  R1,RE,DR12          CHECK ALL THE FIELDS ON THE TWA              
*                                                                               
DR18     MVC   DCURRENT,DFIRSTLN   SAVE A(CURRENT LINE)                         
         LH    R2,DCURRENT         WHERE TO START DISPLAYING                    
         AR    R2,RA                                                            
         NI    MISCFLG2,X'FF'-MF2NDSCR                                          
         ZAP   SCRAMNTS,=P'0'      RESET SCREEN TOTALS/SPOTS                    
         ZAP   SCRSPOTS,=P'0'                                                   
*                                                                               
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BZ    *+10                                                             
         XC    NXTUPKEY,NXTUPKEY   YES, THEN START THE DISPLAY OVER             
*********                                                                       
* PF04                                                                          
*********                                                                       
         CLI   PFKEY,4             ADDING DETAILS?                              
         BNE   DR20                                                             
         XC    DETLLIST,DETLLIST   CLEAR THE DETAIL LIST                        
         B     DRX                                                              
*********                                                                       
* PF06                                                                          
*********                                                                       
DR20     CLI   PFKEY,6             GO TO THE TOP OF THE LIST?                   
         BNE   DR30                                                             
         XC    NXTUPKEY,NXTUPKEY                                                
         B     DR50                                                             
*********                                                                       
* PF07                                                                          
*********                                                                       
DR30     CLI   PFKEY,7             GO TO THE PREVIOUS SCREEN?                   
         BNE   DR35                                                             
         OC    DETLLIST(DLSTLENQ),DETLLIST                                      
         BZ    DR30X                                                            
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(DLSTLENQ-1),DETLLIST                                   
*                                                                               
         ZIC   R0,NDETLLNS         MAXIMUM NUMBER OF DETAILS ON SCREEN          
         BAS   RE,MINIOHI                                                       
         BE    DR30LOOP                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    DR30X                                                            
         DC    H'0'                                                             
*                                                                               
DR30LOOP L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ                                                 
         BNE   DR30X                                                            
         MVI   NXTUPKEY,SNVIDELQ                                                
         MVC   NXTUPKEY+1(DLSTLENQ-1),SNVIDDAY                                  
         BAS   RE,MINIOBSQ                                                      
         BNE   DR30X                                                            
         BCT   R0,DR30LOOP                                                      
DR30X    B     DR50                                                             
*********                                                                       
* PF08 - DOES NOTHING SPECIAL (WAS CHECKED BETWEEN DR10 AND DR11)               
*********                                                                       
DR35     DS    0H                                                               
         CLI   PFKEY,8             PAGE DOWN?                                   
         BE    DR50                                                             
*********                                                                       
* PF09                                                                          
*********                                                                       
DR40     CLI   PFKEY,9             GO TO THE BOTTOM OF THE LIST?                
         BNE   DR45                                                             
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         OC    NXTUPKEY,NXTUPKEY                                                
         BZ    *+10                                                             
         MVC   MINEKEY(L'NXTUPKEY),NXTUPKEY                                     
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    DR40LOOP                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    DR40X                                                            
         DC    H'0'                                                             
*                                                                               
DR40LOOP L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ                                                 
         BNE   DR40X                                                            
         MVI   NXTUPKEY,SNVIDELQ                                                
         MVC   NXTUPKEY+1(DLSTLENQ-1),SNVIDDAY                                  
         BAS   RE,MINIOSEQ                                                      
         BE    DR40LOOP                                                         
DR40X    DS    0H                                                               
*                                                                               
DR45     MVI   NUMINLS2,0                                                       
         TM    MISCFLG2,MF2CHNGD   DID SOMETHING HAPPENED TO A DETAIL?          
         BZ    DR50                                                             
         CLI   PFKEY,9                                                          
         BE    DR50                                                             
         LA    R3,DETLLIST                                                      
*                                                                               
DR47     CLI   NUMINLS2,NLSTLINS   END OF TABLE                                 
         BNL   DR100                                                            
         ZIC   R1,NUMINLS2         INCREASE NUMBER OF DETAILS IN LIST           
         LA    R1,1(R1)                                                         
         STC   R1,NUMINLS2                                                      
         OC    0(DLSTLENQ,R3),0(R3)   ANYTHING IN THIS ENTRY                    
         BNZ   DR48                                                             
         LA    R3,DLSTLENQ(R3)     NO, GO TO NEXT                               
         B     DR47                                                             
*                                                                               
DR48     XC    MINEKEY,MINEKEY     NO, START DISPLAY FROM FIRST DETAIL          
         MVI   MINEKEY,SNVIDELQ                                                 
         MVC   MINEKEY+1(SNVIDSLN-SNVIDDAY),0(R3)                               
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    DR70                                                             
         B     DR65                                                             
*                                                                               
DR50     XC    DETLLIST,DETLLIST   CLEAR THE DETAIL LIST                        
         MVI   NUMINLST,0                                                       
*                                                                               
         OC    NXTUPKEY,NXTUPKEY               DO WE CONTINUE DISPLAY?          
         BZ    DR55                                                             
         MVC   MINEKEY(L'NXTUPKEY),NXTUPKEY    YES                              
         B     DR60                                                             
*                                                                               
DR55     XC    MINEKEY,MINEKEY     NO, START DISPLAY FROM FIRST DETAIL          
         MVI   MINEKEY,SNVIDELQ                                                 
*                                                                               
DR60     BAS   RE,MINIOHI                                                       
         BE    DR70                                                             
DR65     CLI   MINERR,MINEEOF                                                   
         BE    DRRESET                                                          
         CLI   MINERR,MINERNF                                                   
         BE    DRRESET                                                          
         DC    H'0'                                                             
*                                                                               
DR70     LR    R0,R2               SAVE D(CURRENT LINE IN TWA)                  
         SR    R0,RA                                                            
         STH   R0,DCURRENT                                                      
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ    STILL AN INVOICE DETAIL ELEMENT?             
         BE    DR80                YES                                          
*                                                                               
DRRESET  TM    MISCFLG2,MF2NDSCR   HIT END OF SCREEN WHILE DISPLAYING?          
         BNZ   DR100               YES                                          
         XC    NXTUPKEY,NXTUPKEY   NO, RESTART LIST NEXT TIME                   
         B     DR100                                                            
*                                                                               
DR80     BAS   RE,CHKFLTRS         DOES THIS DETAIL PASS FILTERS?               
         BNE   DR90                NO, CHECK NEXT DETAIL                        
*                                                                               
DR81     TM    MISCFLG2,MF2NDSCR   HIT END OF SCREEN WHILE DISPLAYING?          
         BNZ   DR100               YES                                          
*                                                                               
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BZ    DR82                NO, DISPLAY THE FIELD                        
         TM    MATCHFL,MFMATCHQ                                                 
         BO    DR82                                                             
* SEE IF THE FIELD IS ORIGINAL COST                                             
         TM    1(R2),X'02'         EXTENDED FIELD?                              
         BZ    DR83                NO, CAN'T BE ORIG COST                       
         ZIC   RF,0(R2)            FIELD LENGTH                                 
         SH    RF,=H'8'            L(HEADER EXTENTION)                          
         LA    RE,0(R2,RF)         A(HEADER EXTENTION)                          
         CLI   0(RE),FLDNOCST      ORIGINAL COST?                               
         BNE   DR83                NO - DON'T DISPLAY IT                        
*                                                                               
DR82     DS    0H                                                               
         GOTO1 GTROUTIN,DMCB,(C'D',(R2))   GET THE DISPLAY ROUTINE              
         L     RF,DMCB                                                          
         A     RF,RELO                                                          
*                                                                               
         TM    MATCHFL,MFMATCHQ    MATCH OPTION?                                
         BNO   *+8                                                              
         OI    1(R2),X'20'         PROTECT ALL                                  
*                                                                               
         LR    RE,R2               R1 = A(EXTENDED FIELD HEADER)                
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'                                                         
         AR    RE,R0                                                            
         CLI   0(RE),FLDNOCST      ORIG COST FIELD?                             
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT IT                                   
*                                                                               
         GOTO1 (RF),DMCB,(RC),(R2),(R6)   DISPLAY THE FIELD                     
         BE    DR83                                                             
         LH    R2,DCURRENT         RESET A(CURRENT LINE)                        
         AR    R2,RA                                                            
         TWAXC (R2)                                                             
         B     DR90                                                             
*                                                                               
DR83     ZIC   R0,0(R2)            R2 = A(NEXT FIELD)                           
         AR    R2,R0                                                            
*                                                                               
         ZICM  R1,2(R2),2          SEE IF NEXT FIELD IS NEXT DETAIL'S           
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         AH    R0,=H'1'                                                         
         CH    R0,=H'2'            COLUMN OF THIS FIELD IS 2?                   
         BNE   DR80                NO, ANOTHER DETAIL FIELD TO DISPLAY          
*                                                                               
         TM    MISCFLG2,MF2CHNGD   DID SOMETHING HAPPENED TO A DETAIL?          
         BZ    DR84                                                             
         CLI   PFKEY,6             PFKEY 6 - 9 ENTERED?                         
         BL    DR85                                                             
         CLI   PFKEY,9                                                          
         BH    DR85                YES                                          
*                                                                               
DR84     ZIC   R1,NUMINLST         SAVE DETAIL INFO IN DETAIL LIST              
         MH    R1,=Y(DLSTLENQ)                                                  
         LA    R1,DETLLIST(R1)                                                  
         MVC   0(DLSTLENQ,R1),SNVIDDAY                                          
*                                                                               
DR85     ZIC   R1,NUMINLST         INCREASE NUMBER OF DETAILS IN LIST           
         AH    R1,=H'1'                                                         
         STC   R1,NUMINLST                                                      
*                                                                               
         TM    1(R2),X'02'         ARE WE ON THE PFKEY LINE?                    
         BZ    DR86                NO                                           
         LA    R1,8(R2)                                                         
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'16'                                                        
         AR    R1,R0                                                            
         CLI   0(R1),FLDNPFKY                                                   
         BNE   DR86                                                             
         OI    MISCFLG2,MF2NDSCR   YES, GOT END OF SCREEN                       
         MVI   NXTUPKEY,SNVIDELQ   SAVE KEY FOR LATER USE                       
         MVC   NXTUPKEY+1(DLSTLENQ-1),SNVIDDAY                                  
*                                                                               
DR86     AP    SCRSPOTS,=P'1'      INCREMENT NUMBER ON SCREEN                   
*                                                                               
         ICM   R1,15,SNVIDCST      CALCULATE TOTAL FOR SCREEN                   
         TM    MISCFLG2,MF2RPNSE                                                
         BZ    *+10                                                             
         SR    R1,R1                                                            
         ICM   R1,7,SNVIDRSP                                                    
*                                                                               
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         CVD   R1,DUB                                                           
         AP    SCRAMNTS,DUB                                                     
         DROP  R6                                                               
*                                                                               
DR90     TM    MISCFLG2,MF2CHNGD   DID SOMETHING HAPPENED TO A DETAIL?          
         BZ    DR95                                                             
         CLI   PFKEY,6             PFKEY 6 - 9 ENTERED?                         
         BL    *+12                                                             
         CLI   PFKEY,9                                                          
         BNH   DR95                YES                                          
         LA    R3,DLSTLENQ(R3)                                                  
         B     DR47                                                             
*                                                                               
DR95     BAS   RE,MINIOSEQ                                                      
         BE    DR70                                                             
         B     DR65                                                             
*                                                                               
DR100    TM    MISCFLG2,MF2RPNSE                                                
         BZ    DR102                                                            
         EDIT  (P16,SCRAMNTS),(15,DTLSNUM),COMMAS=YES,ZERO=NOBLANK              
         B     DR104                                                            
DR102    EDIT  (P16,SCRAMNTS),(15,DTLSNUM),2,ZERO=NOBLANK,FLOAT=-               
*                                                                               
DR104    MVI   DTLSNUM+15,C'/'                                                  
         LA    RE,DTLSNUM+16                                                    
         EDIT  (P4,SCRSPOTS),(4,0(RE)),ZERO=NOBLANK                             
*                                                                               
         GOTO1 CKSPCLI2            IF RUNNING SPECIAL I2S                       
         BE    DR110               DON'T DO U2                                  
         TM    MISCFLG2,MF2CHNGD   DID SOMETHING HAPPENED TO A DETAIL?          
         BZ    DR110                                                            
         CLI   PI2RAUTO,C'Y'                                                    
         BNE   DR110                                                            
         GOTO1 =A(BLDREQ),DMCB,(RC),RR=RELO                                     
*                                                                               
DR110    CLI   PFKEY,5                                                          
         BNE   DR120                                                            
*                                                                               
         CLC   =CL4'PRD=',DTLKYL2  WE HAVE A GLOBAL PRODUCT?                    
         BE    DR112                                                            
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'POL',3,GLVSPPRD                            
*                                                                               
DR112    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BAS   RE,MINIOHI                                                       
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         TM    SNVHDCTL,SNVHDMCQ   MIDFLIGHT INVOICE?                           
         BZ    DR118                                                            
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'MCT',3,GLVSPOPT                            
         DROP  R6                                                               
*                                                                               
DR118    XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'NIN'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'MAT'                                                 
         OI    GLVXFLG1,GLV1SEPS+GLV1RETG   CALL BASE ON TRANSFER               
         DROP  R1                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,14,GLVXCTL                               
         B     DRX                                                              
*                                                                               
DR120    DS    0H                                                               
         TM    MISCFLG2,MF2CHNGD   DID SOMETHING HAPPENED TO A DETAIL?          
         BZ    DRX                                                              
         CLI   PFKEY,6             PFKEY 6 - 9 ENTERED?                         
         BL    *+12                                                             
         CLI   PFKEY,9                                                          
         BNH   DRX                 YES                                          
*                                                                               
* SQUASH THE TABLE TO GET RID OF NULLS                                          
         LA    R3,DETLLIST                                                      
         LR    R6,R3                                                            
         MVI   NUMINLST,0                                                       
*                                                                               
DR130    ZIC   R1,NUMINLST         INCREMENT DETAIL LINE COUNT                  
         LA    R1,1(R1)                                                         
         STC   R1,NUMINLST                                                      
         LA    RE,NLSTLINS         MAX DETAILS IN LIST                          
         CR    R1,RE                                                            
         BH    DRX                    DONE                                      
         OC    0(DLSTLENQ,R6),0(R6)   ANYTHING IN THIS ENTRY                    
         BZ    DR140                  NO                                        
         CR    R3,R6                  POINTING TO SAME ENTRY?                   
         BE    DR135                                                            
         MVC   0(DLSTLENQ,R3),0(R6)   MOVE THE ENTRY                            
         XC    0(DLSTLENQ,R6),0(R6)   CLEAR IT                                  
*                                                                               
DR135    LA    R3,DLSTLENQ(R3)        GO TO NEXT                                
DR140    LA    R6,DLSTLENQ(R6)        GO TO NEXT                                
         B     DR130                                                            
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE DETAIL PASSES THE FILTERS SET BY            
* THE USER                                                                      
*                                                                               
* ON ENTRY:    (R6)                A(DETAIL ELEMENT)                            
***********************************************************************         
         USING SNVIDELD,R6                                                      
CHKFLTRS NTR1                                                                   
         OC    FLTRFLG1(NUMFLTRS),FLTRFLG1    ANY FILTERS SET?                  
         BZ    CKFLTYES                       NONE, PASS                        
*                                                                               
         XC    FAKEFLDH(L'FAKEFLDH+L'FAKEFLD),FAKEFLDH                          
         LA    R2,FAKEFLDH         MAKE BELIEVE WE HAVE XTND FLDHDR             
         MVI   0(R2),L'FAKEFLDH+L'FAKEFLD                                       
         OI    1(R2),X'02'                                                      
*                                                                               
         LA    R3,FLTREQUS                                                      
CKFLT10  CLI   0(R3),X'FF'         NO MORE FILTER BITS TO CHECK?                
         BE    CKFLTYES            NO MORE, PASS ALL FILTERS                    
*                                                                               
         ZIC   R1,0(R3)            R0 = DISP FROM FLTRFLG1                      
         LA    RE,FLTRFLG1(R1)     FILTER FLAG WE NEED TO TEST AGAINST          
         ZIC   R1,1(R3)            R1 = BIT TO TEST TO SEE IF ON                
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RE),0                                                          
         BZ    CKFLTNXT            IF NOT ON, THEN CHECK NEXT BIT               
*                                                                               
         LA    R2,FAKEFLDH+L'FAKEFLDH+L'FAKEFLD-8   WHERE WE PUT FLD #          
         MVC   0(1,R2),2(R3)       MOVE IN THE FIELD NUMBER                     
*                                                                               
         GOTO1 GTROUTIN,DMCB,(C'D',FAKEFLDH)                                    
         L     RF,DMCB                                                          
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC),FAKEFLDH,(R6)                                     
         BNE   CKFLTNO             PASSED THE FILTERS?                          
*                                                                               
CKFLTNXT LA    R3,L'FLTREQUS(R3)                                                
         B     CKFLT10                                                          
*                                                                               
CKFLTYES NI    FAKEFLDH+1,X'FF'-X'02'                                           
         B     YES                                                              
*                                                                               
CKFLTNO  NI    FAKEFLDH+1,X'FF'-X'02'                                           
         B     NO                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE BUILDS A TABLE OF FILM CODES BASED ON CURRENT INVOICE            
***********************************************************************         
READFLMS NTR1                                                                   
         LA    R0,BUFFER           CLEAR THE TABLE                              
         LA    R1,L'BUFFER                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   FLMTABTY,0          DON'T KNOW IF BINARY OR ALPHA PRDS           
         LA    R0,FLMTAB                                                        
         LA    R1,FLMTABX-FLMTAB                                                
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,BUFFER           R3 = A(FIRST ENTRY IN TABLE)                 
         SR    R2,R2                                                            
*                                                                               
         XC    MINEKEY,MINEKEY     FIND ALL THE COMMERCIAL ELEMENTS             
         MVI   MINEKEY,SNVCMELQ                                                 
         BAS   RE,MINIOHI                                                       
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
*                                                                               
         MVC   BUFFCODE,SNVCMICD     PUT COMMERCIAL IN TABLE                    
         MVC   BUFFSEQN,SNVCMSEQ                                                
         MVI   BUFFSTAT,0                                                       
         MVC   BUFFCMCD,SNVCMCD                                                 
         OC    BUFFCMCD,BLANKS                                                  
         LA    R3,BUFFNEXT                                                      
         AHI   R2,1                                                             
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         B     RDFLM10                                                          
*                                                                               
RDFLMX   B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FINDS THE EBCDIC EQUIVALENT OF A BINARY PRODUCT CODE             
*                                                                               
* ON ENTRY:    PARAM1   BYTE 0     BINARY PRODUCT CODE                          
*              SVCLIST             CLIENT'S PRODUCT LIST                        
*                                                                               
* ON EXIT:     PARAM1   BYTE 1-3   EBCDIC PRODUCT                               
***********************************************************************         
FINDQPRD NTR1                                                                   
         LA    R1,SVCLIST                                                       
*                                                                               
FQPRD10  CLI   0(R1),0             ANY MORE PRODUCTS?                           
         BE    FQPRDNO             NO                                           
*                                                                               
         CLC   3(L'BPRD,R1),DMCB   BINARY PRODUCT CODE MATCH?                   
         BE    *+12                                                             
         LA    R1,4(R1)            NO, CHECK NEXT PRODUCT                       
         B     FQPRD10                                                          
*                                                                               
         MVC   DMCB+1(L'QPRD),0(R1)  YES, COPY THE EBCDIC PRODUCT               
*                                                                               
FQPRDYES B     YES                                                              
*                                                                               
FQPRDNO  B     NO                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
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
* THIS ROUTINE READ SEQUENTIAL BACKWARDS FOR A MINIO ELEMENT.                   
***********************************************************************         
MINIOBSQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE (BOF)             
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
         BE    XIT                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
***********************************************************************         
MINIODEL NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
***********************************************************************         
* NOTE: FROM THIS POINT ON WE WON'T USE R5 AS A GLOBAL MINIO DSECT REG          
***********************************************************************         
         DROP  R5                                                               
***********************************************************************         
* FINDS THE RIGHT ROUTINE FROM THE ROUTINE TABLE FOR A SPECIFIC FIELD           
*                                                                               
* ON ENTRY:    PARAM1   BYTE 0     (V)ALIDATION OR (D)ISPLAY                    
*                       BYTE 1-3   FIELD HEADER                                 
*                                                                               
* ON EXIT:     PARAM1              A(ROUTINE)                                   
***********************************************************************         
GTROUTIN NTR1                                                                   
         L     R2,DMCB                                                          
         TM    1(R2),X'02'         THIS FIELD HAD BETTER HAVE AN XHDR           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R1,R2               R1 = A(EXTENDED FIELD HEADER)                
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'                                                         
         AR    R1,R0                                                            
*                                                                               
         LA    R3,ROUTINES                                                      
         USING ROUTTABD,R3                                                      
GRTN10   CLI   ROUTFLDN,0          END OF ROUTINE TABLE?                        
         BNE   *+6                                                              
         DC    H'0'                YES, SHOULDN'T HAPPEN                        
*                                                                               
         CLC   ROUTFLDN,0(R1)      SAME FIELD NUMBER?                           
         BE    GRTN20                                                           
         LA    R3,ROUTNEXT         NO, CHECK NEXT TABLE ENTRY                   
         B     GRTN10                                                           
*                                                                               
GRTN20   CLI   DMCB,C'V'           CALLER WANTS VALIDATION ROUTINE?             
         BNE   *+14                                                             
         MVC   DMCB,ROUTVALR       YES                                          
         B     GRTNX                                                            
*                                                                               
         MVC   DMCB,ROUTDISR       NO, CALLER WANTS DISPLAY ROUTINE             
*                                                                               
GRTNX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
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
***********************************************************************         
* SYSTEM 23 ERROR MESSAGES                                                      
***********************************************************************         
NETMASTR MVI   GERROR1,CBLNETER    CABLE NETWORK NOT ON MASTER RECORD           
         B     SYS23ERR                                                         
*                                                                               
NOPIGGYB MVC   GERROR,=AL2(MISSPIGY)  PIGGYBACK PRODUCT MISSING                 
*                                                                               
SYS23ERR MVI   GETMSYS,23                                                       
         B     ERREXIT                                                          
*                                                                               
***********************************************************************         
* SYSTEM SPOT ERROR MESSAGES                                                    
***********************************************************************         
*                                                                               
PFERR    MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         MVI   GETMSYS,23                                                       
         LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         B     ERREXIT                                                          
*                                                                               
BADMEDIA MVI   GERROR1,INVMED      INVALID MEDIA                                
         B     ERREXIT2                                                         
*                                                                               
BADCLNT  MVI   GERROR1,INVMED      INVALID CLIENT                               
         B     ERREXIT2                                                         
*                                                                               
BADPROD  MVI   GERROR1,INVPROD     INVALID PRODUCT                              
         B     ERREXIT2                                                         
*                                                                               
BADPROD2 MVI   GERROR1,INVPROD2    INVALID PRODUCT #2                           
         B     ERREXIT2                                                         
*                                                                               
BADESTMT MVI   GERROR1,INVESTMT    INVALID ESTIMATE                             
         B     ERREXIT2                                                         
*                                                                               
BADSTATN MVI   GERROR1,INVSTATN    INVALID STATION                              
         B     ERREXIT2                                                         
*                                                                               
BADDTFMT MVI   GERROR1,INVDTFMT    INVALID DATE FORMAT                          
         B     ERREXIT2                                                         
*                                                                               
BADSLEN  MVI   GERROR1,INVSLEN     INVALID LENGTH                               
         B     ERREXIT2                                                         
*                                                                               
BADTIME  MVI   GERROR1,INVTIMXP    INVALID TIME EXPRESSION                      
         B     ERREXIT2                                                         
*                                                                               
BADFLPRD MVI   GERROR1,INVFLPRD    FILM AND PRODUCT DO NOT AGREE                
         B     ERREXIT2                                                         
*                                                                               
BADFLSLN MVI   GERROR1,INVFLSLN    FILM AND SPOT LENGTH DO NOT AGREE            
         B     ERREXIT2                                                         
*                                                                               
BADDTEST MVI   GERROR1,INVDTEST    DATES NOT WITHIN ESTIMATE PERIOD             
         B     ERREXIT2                                                         
*                                                                               
BADDTOUT MVI   GERROR1,INVDTOUT    DATE IS OUT OF INPUT MONTH                   
         B     ERREXIT2                                                         
*                                                                               
BADFILM1 MVI   GERROR1,INVFILM1    INVALID FILM CODE                            
         B     ERREXIT2                                                         
*                                                                               
BADFILM2 MVI   GERROR1,INVFILM2    INVALID SECOND FILM                          
         B     ERREXIT2                                                         
*                                                                               
DUPEDERR MVI   GERROR1,DUPLINVI    DUPLICATE INVOICE ITEM                       
         B     ERREXIT2                                                         
*                                                                               
ERR255   MVC   GERROR,=AL2(ERR255Q) MORE THAN 255 IDENTICAL DETAILS             
         B     ERREXIT2                                                         
*                                                                               
UPDERR   MVC   GERROR,=AL2(1158)                                                
         LH    R2,DFIRSTLN                                                      
         AR    R2,RA                                                            
         B     ERREXIT                                                          
*                                                                               
MIXERR   MVC   GERROR,=AL2(1256)   CAN'T MIX AD-IDS AND ISCII FILMCODES         
         B     ERREXIT                                                          
*                                                                               
NORATBOK MVI   GERROR1,INVRATBK    RATING BOOK NOT ON FILE                      
*                                                                               
ERREXIT2 OI    GENSTAT2,USMYERSY   STICK WITH THIS MESSAGE SYSTEM               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         B     MYERRXIT                                                         
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT DS    0H                                                               
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS (SEE  PFTABD  IN  SPSNVWORKD)                         
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* INVOICE ADD  (KEY IS STORED GLOBBER)                                          
         DC    AL1(PF01X-*,01,0,0,0)                                            
         DC    CL3' ',CL8'INVOICE',CL8'ADD'                                     
PF01X    EQU   *                                                                
*                                                                               
* INVOICE MATCH DRIVER                                                          
         DC    AL1(PF05X-*,05,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF05X    EQU   *                                                                
*                                                                               
* TOP PFKEY DRIVER                                                              
         DC    AL1(PF06X-*,06,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF06X    EQU   *                                                                
*                                                                               
* PGUP DRIVER                                                                   
         DC    AL1(PF07X-*,07,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF07X    EQU   *                                                                
*                                                                               
* PGDN DRIVER - LIKE THE ENTER KEY (NOT CHECKED)                                
         DC    AL1(PF08X-*,08,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF08X    EQU   *                                                                
*                                                                               
* BOTTOM PFKEY DRIVER                                                           
         DC    AL1(PF09X-*,09,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF09X    EQU   *                                                                
*                                                                               
* INVOICE REQUEST DRIVER                                                        
         DC    AL1(PF10X-*,10,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF10X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* SPECIAL PFKEY TABLE DEFINITIONS                                               
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* GO BACK TO LIST OVERLAY TO INVOKE REQUEST                                     
         DC    AL1(SPF10X-*,10,PFTRPROG,0,PFTCLRKY)                             
         DC    CL3' ',CL8' ',CL8' '                                             
SPF10X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPSNVFLDS                                                      
***********************************************************************         
* ROUTINE TABLE FOR VALIDATING AND DISPLAYING OF THE DETAIL FIELDS              
*                                                                               
*   XL1        - FIELD EQUATE NUMBER                                            
*   AL4        - VALIDATION ROUTINE FOR THIS FIELD                              
*   AL4        - DISPLAY ROUTINE FOR THIS FIELD                                 
***********************************************************************         
ROUTINES DS    0CL9                                                             
         DC    AL1(FLDNDATE),AL4(VALDATE),AL4(DISDATE)                          
         DC    AL1(FLDNTIME),AL4(VALTIME),AL4(DISTIME)                          
         DC    AL1(FLDNSLEN),AL4(VALSLEN),AL4(DISSLEN)                          
         DC    AL1(FLDNFILM),AL4(VALF1),AL4(DISF1)                              
         DC    AL1(FLDNCOST),AL4(VALCOST),AL4(DISCOST)                          
         DC    AL1(FLDNPROD),AL4(VALPROD),AL4(DISPROD)                          
         DC    AL1(FLDNESTM),AL4(VALESTM),AL4(DISESTM)                          
         DC    AL1(FLDNRCNT),AL4(VALRCNT),AL4(DISRCNT)                          
         DC    AL1(FLDNINTG),AL4(VALINTG),AL4(DISINTG)                          
         DC    AL1(FLDNPROG),AL4(VALPROG),AL4(DISPROG)                          
         DC    AL1(FLDNNTWK),AL4(VALNTWK),AL4(DISNTWK)                          
         DC    AL1(FLDNPFLM),AL4(VALF2),AL4(DISF2)                              
         DC    AL1(FLDNMKGD),AL4(VALMKGD),AL4(DISMKGD)                          
         DC    AL1(FLDNBILB),AL4(VALBILB),AL4(DISBILB)                          
*                                                                               
* FOLLOWING 9 SUBROUTINES ARE DDS ONLY, FOR DEBUGGING PURPOSES                  
* SPOT MATCHMAKER                                                               
         DC    AL1(FLDNMEST),AL4(0),AL4(DISMEST)                                
         DC    AL1(FLDNLINE),AL4(0),AL4(DISLINE)                                
         DC    AL1(FLDNSDAT),AL4(0),AL4(DISSDAT)                                
         DC    AL1(FLDNSPOT),AL4(0),AL4(DISSPOT)                                
* NET MATCHMAKER                                                                
         DC    AL1(FLDNNEST),AL4(0),AL4(DISMEST)                                
         DC    AL1(FLDNNDAT),AL4(0),AL4(DISSDAT)                                
         DC    AL1(FLDNSUBL),AL4(0),AL4(DISLINE)                                
         DC    AL1(FLDNNTIM),AL4(0),AL4(DISNTIM)                                
         DC    AL1(FLDNNPGM),AL4(0),AL4(DISNPGM)                                
*                                                                               
         DC    AL1(FLDNNPKG),AL4(VALNPKG),AL4(DISNPKG)                          
* THIS FIELD IS DDS-ONLY                                                        
         DC    AL1(FLDNDEMV),AL4(0),AL4(DISDEM)                                 
* THIS FIELD IS DDS-ONLY                                                        
         DC    AL1(FLDNOCST),AL4(0),AL4(DISOCST)                                
*                                                                               
         DC    AL1(FLDNCLK),AL4(VALCLK),AL4(DISCLK)                             
         DC    AL1(FLDNRAT),AL4(VALRAT),AL4(DISRAT)                             
         DC    AL1(FLDNIMP),AL4(VALIMP),AL4(DISIMP)                             
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER BIT EQUATES TO FIELD NUMBER EQUATES USED FOR CHKFLTRS                  
*                                                                               
*   XL1        - DISPLACEMENT FROM 1ST FILTER FLAG                              
*   XL1        - FILTER BIT EQUATE                                              
*   XL1        - FIELD EQUATE NUMBER                                            
***********************************************************************         
FLTREQUS DS    0CL3                                                             
         DC    AL1(0),AL1(FF1DDATE),AL1(FLDNDATE)                               
         DC    AL1(0),AL1(FF1DSLEN),AL1(FLDNSLEN)                               
         DC    AL1(0),AL1(FF1DFILM),AL1(FLDNFILM)                               
         DC    AL1(0),AL1(FF1DFLM2),AL1(FLDNPFLM)                               
         DC    AL1(0),AL1(FF1DCOST),AL1(FLDNCOST)                               
         DC    AL1(0),AL1(FF1DPROD),AL1(FLDNPROD)                               
         DC    AL1(0),AL1(FF1DESTM),AL1(FLDNESTM)                               
         DC    AL1(0),AL1(FF1DRCNT),AL1(FLDNRCNT)                               
*                                                                               
         DC    AL1(1),AL1(FF2DNTWK),AL1(FLDNNTWK)                               
         DC    AL1(1),AL1(FF2BLLBD),AL1(FLDNBILB)                               
         DC    AL1(1),AL1(FF2INTEG),AL1(FLDNINTG)                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE CHECKS THE FILM CODE ENTRY SCANNED BY VALFILM                    
*                                                                               
* ON ENTRY:    PARAM 1             A(FILM FIELD)                                
*              PARAM 2             A(INVOICE DETAIL ELEMENT)                    
*              BPRD                BINARY PRODUCT CODE                          
*              QPRD                EBCDIC PRODUCT CODE                          
*                                                                               
* ON EXIT:     GERROR1             ERROR CODE (0=NO ERROR, EQ. EXIT)            
*              BFILM               INTERNAL FILM CODE                           
*              BFILMSQ             FILM SEQUENCE NUMBER                         
*              QFILM               FILM CODE                                    
*              QFILMLEN            FILM CODE LENGTH                             
*                                                                               
*              MF3ADID             AD ID OR NOT                                 
*              MF3HD               HD OR NOT                                    
*              MF2NWFLM            NEW FILM FLAG                                
***********************************************************************         
CHKFILM  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(FILM)                                      
         L     R6,4(R1)            A(DETAIL ELEM)                               
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   5(R2),8             FILM LENGTH > 8?                             
         BNH   CFLM05              NO -> IT IS NOT AN AD ID                     
*                                                                               
         LA    R1,8(R2)                                                         
         BRAS  RE,CHKADID                                                       
         BNE   CFLMERR                                                          
         OI    MISCFLG3,MF3ADID                                                 
*                                                                               
CFLM05   DS    0H                                                               
         CLI   QMED,C'R'           RADIO?                                       
         BNE   *+12                NO - DON'T CHECK T1 PROFILE                  
         CLI   P0T1NSTD,C'Y'       YES: NON-STD COMMLS ACCEPTED?                
         BE    *+12                YES, FILM CODE CAN BE < 8 BYTES              
         CLI   5(R2),7             FILM CODE MUST BE 8 BYTES                    
         BNH   CFLMERR                                                          
*                                                                               
* LOOK UP THE COMMERCIAL IN FILM BUFFER, FILM TABLE,                            
* SET BFILM, QFILMLEN                                                           
*                                                                               
CFLM10   DS    0H                                                               
*                                                                               
* LOCATE COMMERCIAL IN FILM BUFFER                                              
*                                                                               
         LA    R1,8(R2)            A(FILM)                                      
         ICM   R1,8,=X'02'                                                      
         BRAS  RE,GETFBUF                                                       
         BE    *+12                                                             
         OI    MISCFLG2,MF2NWFLM   YES, WE GOT A NEW FILM                       
         B     CFLM30                                                           
*                                                                               
         L     R1,FULL                                                          
         MVC   BFILM,BUFFCODE-BUFFDSCT(R1)                                      
         MVC   BFILMSQ,BUFFSEQN-BUFFDSCT(R1)                                    
         MVC   QFILM,BUFFCMCD-BUFFDSCT(R1)                                      
         LA    R1,QFILM                                                         
         LHI   R0,L'QFILM                                                       
         BRAS  RE,FINDLEN                                                       
         STC   RF,QFILMLEN                                                      
*                                                                               
* LOCATE COMMERCIAL IN FILM TABLE                                               
*                                                                               
CFLM30   DS    0H                                                               
         LA    R5,FLMTAB           CHECK FILM IN CURRENT TABLE                  
         LA    R3,FLMTABX                                                       
         USING FILMTABD,R5         FOR SLN AND PRD VALIDATION                   
*                                                                               
CFLM35   ST    R5,AFTBENT          SAVE ENTRY ADDRESS                           
         CR    R5,R3               END OF TABLE?                                
         BNL   CFLM70              YES - FILM NOT FOUND                         
         CLI   FTABCODE,0          END OF TABLE?                                
         BE    CFLM70              YES - FILM NOT FOUND                         
*                                                                               
         CLC   BFILM,FTABCODE      MATCH ON THIS FILM CODE?                     
         BE    CFLM40              YES                                          
*                                                                               
         ZICM  R0,FTABELEN,1       NO, CHECK NEXT ENTRY                         
         BZ    CFLM70              END OF TABLE                                 
         AR    R5,R0                                                            
         B     CFLM35                                                           
*                                                                               
* COMML FOUND IN FILM TABLE                                                     
*                                                                               
CFLM40   DS    0H                                                               
         L     R5,AFTBENT          LOAD A(ENTRY) FOR 2ND PASS                   
         MVC   TRFLEN,FTABSLEN                                                  
*                                                                               
         CLI   FTABELEN,2          FILM NOT IN TRAFFIC?                         
         BNE   CFLM60                                                           
*                                                                               
         MVI   TRFLEN,X'FF'                                                     
         OI    TRFERR,X'40'        WRONG SECONDS LENGTH                         
*                                                                               
CFLM60   DS    0H                                                               
         CLI   FTABELEN,2          FILM NOT IN TRAFFIC?                         
         BE    CFLM69                                                           
*                                                                               
         CLI   FTABPRDS,X'FF'      PRD=ALL                                      
         BE    CFLM69X                                                          
*                                                                               
         CLC   VFQPRD,=C'POL'                                                   
         BE    CFLM69X                                                          
*                                                                               
         LA    RF,FTABPRDS                                                      
         ZIC   R3,FTABELEN         RE = NUMBER OF PRODS                         
         SHI   R3,FTABPRDS-FTABTYPE                                             
         CLI   FTABTYPE,X'29'      NEW ALPHA PRD LIST?                          
         BNE   CFLM66                                                           
         LR    R1,R3                                                            
         XR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R3,R1               DIVIDE BY 3 FOR # OF ALPHA PRODUCTS          
*                                                                               
CFLM63   LA    RE,VFQPRD           NETPAK HAS OVERFLOW PRODUCTS AND             
         CLC   VFQPRD,0(RF)        DOES NOT USE MASTER TRAFFIC PRD              
         BE    CFLM69X             LIKE SPOTPAK DOES IN CFLM66                  
         LA    RF,3(RF)                                                         
         BCT   R3,CFLM63                                                        
         B     CFLM69                                                           
*                                                                               
CFLM66   LA    RE,VFBPRD                                                        
         CLI   BTRAPRD,0           USE TRAFFIC PRODUCT OVERRIDE                 
         BE    *+8                 IF IT EXISTS                                 
         LA    RE,BTRAPRD                                                       
         CLC   0(1,RE),0(RF)                                                    
         BE    CFLM69X                                                          
         LA    RF,1(RF)                                                         
         BCT   R3,CFLM66                                                        
*                                                                               
CFLM69   OI    TRFERR,X'20'        PRD INVALID                                  
         MVI   GERROR1,FPRDERR                                                  
*                                                                               
CFLM69X  B     CFLMX                                                            
*                                                                               
* LOOK UP THE COMMERCIAL RECORD                                                 
*                                                                               
CFLM70   XC    KEYSAVE,KEYSAVE                                                  
CFLM70A  XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
*                                                                               
         TM    MISCFLG3,MF3ADID                                                 
         BO    CFLM75              WE HAVE AN AD-ID                             
*                                  REGULAR COMML RECORD HERE                    
         USING CMLRECD,R3                                                       
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT        OTHERWISE USE NORMAL CLIENT                  
         OC    BTRACLT,BTRACLT     USE TRAFFIC CLIENT                           
         BZ    *+10                                                             
         MVC   CMLKCLT,BTRACLT     OTHERWISE USE NORMAL CLIENT                  
         MVC   CMLKCML,8(R2)       FILM CODE                                    
         B     CFLM78                                                           
*                                                                               
CFLM75   DS    0H                  AD-ID HERE                                   
         USING CMLPIDAD,R3                                                      
*                                                                               
         MVC   CMLPIDAD(2),=X'0AC1' AD-ID PASSIVE KEY                           
         CLC   =X'0AC1',KEYSAVE    LAST CHECKED PKEY = ADID?                    
         BNE   *+10                YES, CHECK HD PKEY                           
         MVC   CMLPIDAD(2),=X'0AC2' CHECK HD PKEY                               
*                                                                               
         MVC   CMLPADAM,BAGYMD                                                  
         MVC   CMLPADCL,BCLT        USE NORMAL CLIENT                           
         OC    BTRACLT,BTRACLT      USE TRAFFIC CLIENT                          
         BZ    *+10                                                             
         MVC   CMLPADCL,BTRACLT                                                 
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            ADDRESS OF TRPACK                            
*                                                                               
         GOTO1 (RF),DMCB,(C'P',8(R2)),CMLPADID                                  
         BNE   CFLMERR                                                          
*                                                                               
CFLM78   MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'TRFDIR',KEY,KEY,(0(RA),0)         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CMLKEY,KEYSAVE                                                   
         BE    CFLM80                                                           
*                                                                               
* COMMERCIAL RECORD NOT IN TRAFFIC                                              
*                                                                               
         TM    MISCFLG3,MF3ADID    WORKING WITH AD-IDS?                         
         BZ    *+14                NO - NO PASSIVE KEYS TO CHECK                
         CLC   =X'0AC1',KEYSAVE    LAST CHECKED PKEY = ADID?                    
         BE    CFLM70A             YES, GO BACK AND DO HD PKEY                  
*                                                                               
         OI    TRFERR,X'80'        SET FILM NOT FOUND                           
         B     CFLM110             UPDATE FILM BUFFER                           
*                                                                               
* COMMERCIAL RECORD FOUND IN TRAFFIC                                            
*                                                                               
CFLM80   DS    0H                                                               
         CLC   =X'0A21',KEY        PRIMARY KEY?                                 
         BNE   CFLM82              NO, DON'T WORRY                              
         TM    KEY+CMLKSTAT-CMLKEY,CMLKSTA_PCKD IS CML PACKED?                  
         BZ    CFLM82              YES - THIS IS AN AD-ID                       
         OI    TRFERR,X'80'        SET FILM NOT FOUND                           
         B     CFLM110             UPDATE FILM BUFFER                           
*                                                                               
CFLM82   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'TRFFILE',                X        
               KEY+14,AIO,(0(RA),DMWORK)                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    MISCFLG3,MF3ADID    WORKING WITH AD-IDS?                         
         BZ    *+18                NO, DON'T BOTHER WITH HD FLAG                
         CLC   =X'0AC2',KEYSAVE    IS THE AD-ID HI-DEF?                         
         BNE   *+8                 NO                                           
         OI    MISCFLG3,MF3HD      YES - SET THE HD FLAG                        
*                                                                               
* LOOK FOR COMMERCIAL DATA ELEMENT                                              
*                                                                               
         L     R3,AIO                                                           
         AH    R3,DATADISP                                                      
*                                                                               
CFLM85   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE ON EOR IF NO X'10' ELEMENT               
*                                                                               
         CLI   0(R3),X'10'         CMML DATA ELEMENT                            
         BE    CFLM90                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFLM85                                                           
*                                                                               
         USING CMLDTAEL,R3                                                      
CFLM90   DS    0H                                                               
         MVC   BFILMSQ,CMLSEQ+1    SET COMMERCIAL SEQ NO.                       
         MVC   TRFLEN,CMLSLN       SET LENGTH                                   
         DROP  R3                                                               
*                                                                               
* LOOK FOR PRODUCT LIST ELEMENT                                                 
*                                                                               
         L     R3,AIO                                                           
         AH    R3,DATADISP                                                      
*                                                                               
CFLM95   CLI   0(R3),0                                                          
         BE    CFLM105                                                          
         CLI   0(R3),X'20'         WE'RE GOING TO USE X'20'                     
         BE    CFLM100                                                          
         CLI   0(R3),X'29'         UNLESS WE GOT A X'29'                        
         BE    CFLM100                                                          
*                                                                               
CFLM97   ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFLM95                                                           
*                                                                               
* FOUND THE PRODUCT LIST ELEMENT                                                
*                                                                               
         USING CMLPRDEL,R3         X'20' & X'29' HAVE SIMILAR DSECTS            
CFLM100  ZIC   RF,CMLPRDLN                                                      
         LA    RE,2(RF)            ENTRY IS 2 BYTE LONGER THAN ELEM             
         A     RE,AFTBENT          ENTRY ADDRESS                                
         LA    R0,FLMTABX                                                       
         CR    RE,R0               WILL ENTRY FIT IN FLMTAB?                    
         BL    *+6                                                              
         DC    H'0'                FILM TABLE OVERFLOW                          
*                                                                               
         SHI   RF,3                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FTABPRDS(0),CMLPRDS SAVE PRD LIST IN TABLE                       
         MVC   FTABTYPE,CMLPRDEL   COPY TYPE  X'20' OR X'29'                    
         MVC   FTABSLEN,TRFLEN     SECONDS LENGTH IN FILM TABLE                 
*                                                                               
* NOTE: AT THIS POINT INTERNAL SEQ#(FTABCODE) IS UNKNOWN                        
*       IT WILL ADDED LATER, AFTER FILM IS ADDED TO THE BUFFER                  
*       AND INTERNAL SEQ NUMBER IS CALCULATED                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         LA    RF,5(RF)                                                         
         STC   RF,FTABELEN         SET TOTAL TABLE ENTRY LENGTH                 
*                                                                               
         CLI   FTABTYPE,X'29'      DID WE GET ALPHA PRODUCT ELEM?               
         BNE   CFLM97              LET'S SEE IF WE CAN FIND IT                  
*                                                                               
* END OF TRAFFIC RECORD                                                         
*                                                                               
CFLM105  CLI   FTABTYPE,0          WE GET A PRODUCT LIST ELEM?                  
         BNE   *+6                                                              
         DC    H'0'                NO, WE SHOULD HAVE                           
*                                                                               
* CALCULATE INTERNAL SEQ#, AND PUT THE NEWLY FOUND FILM IN FILM BUFFER          
*                                                                               
CFLM110  DS    0H                                                               
         CLI   BFILM,X'00'         FILM ALREADY IN BUFFER?                      
         BNE   CFLM130             YES - SKIP BUFFER UPDATE                     
*                                                                               
         LA    R5,BUFFER                                                        
         LR    R3,R5                                                            
         AHI   R3,BUFFERX-BUFFER                                                
         SR    R1,R1               INTERNAL SEQ #                               
         USING BUFFDSCT,R5                                                      
*                                                                               
CFLM115  DS    0H                                                               
         CR    R3,R5               REACHED EOT?                                 
         BH    *+6                 NO                                           
         DC    H'0'                FILM BUFFER OVERFLOW                         
*                                                                               
         CLI   BUFFCODE,0                                                       
         BE    CFLM120                                                          
*                                                                               
         ZIC   R1,BUFFCODE                                                      
         LA    R5,BUFFNEXT                                                      
         B     CFLM115                                                          
*                                                                               
* FOUND AN EMPTY SLOT IN BUFFER                                                 
*                                                                               
CFLM120  LA    R1,1(R1)            FOUND AN EMPTY SLOT IN BUFFER                
         CH    R1,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                DIE WE RUN OUT OF INTERNAL CODES             
*                                                                               
         STC   R1,BFILM            SAVE INTERNAL FILM CODE                      
*                                                                               
         MVC   BUFFCODE,BFILM      PUT INTERNAL FILM CODE IN BUFFER             
         MVC   BUFFSEQN,BFILMSQ    PUT TRAFFIC FILM SEQ IN BUFFER               
         TM    MISCFLG3,MF3HD                                                   
         BZ    *+8                                                              
         OI    BUFFSTAT,BUFFSHD                                                 
         CLI   TRFERR,0                                                         
         BE    *+8                                                              
         OI    BUFFSTAT,BUFFSINV                                                
         MVC   BUFFCMCD,8(R2)                                                   
         OC    BUFFCMCD,BLANKS                                                  
*                                                                               
* PUT THE FILM SEQ#(FTABCODE) IN FILM TABLE                                     
*                                                                               
CFLM130  DS    0H                                                               
         L     RF,AFTBENT          FLMTAB ENTRY                                 
         MVC   (FTABCODE-FILMTABD)(1,RF),BFILM                                  
         CLI   (FTABELEN-FILMTABD)(RF),X'00'                                    
         BNE   *+8                                                              
         MVI   (FTABELEN-FILMTABD)(RF),X'02'                                    
*                                                                               
         B     CFLM40              2ND PASS - CHECK PRD, LENGTH                 
*                                                                               
CFLMX    DS    0H                                                               
         CLI   TRFERR,X'00'                                                     
         BE    YES                                                              
*                                                                               
CFLMERR  MVI   GERROR1,FLMCERR1    1ST FILM ERROR                               
         B     NO                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL DS    0H                                                               
         NMOD1 0,**SPFK**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   PFKEY,11                                                         
         BE    STPFX                                                            
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF10                                                           
*                                                                               
         CLI   PFKEY,1             GO IMMEDIATELY TO THE ADD SCREEN?            
         BNE   STPF05                                                           
         MVI   CALLSP,0            YES                                          
         MVI   CALLSTCK,X'FC'      TELL X'02' THAT IT'S FROM DETAIL SCR         
         LA    R2,PFTABLE                                                       
         B     STPF20                                                           
*                                                                               
STPF05   CLI   PFKEY,5                                                          
         BE    STPF10                                                           
         CLI   PFKEY,7                                                          
         BL    STPF20                                                           
         CLI   PFKEY,10                                                         
         BNH   STPF10                                                           
         CLI   PFKEY,12                                                         
         BNE   STPF20                                                           
*                                                                               
STPF10   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR                                                
*                                                                               
STPF20   DS    0H                                                               
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+12                                                             
         CLI   PFKEY,5                                                          
         BE    PFERR                                                            
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
         CLI   PFKEY,10                                                         
         BE    STPF110                                                          
*                                  PF5-PF9 ARE HANDLED AT  DREC                 
         B     STPFX                                                            
*********                                                                       
* PF10                                                                          
*********                                                                       
*                                                                               
STPF110  CLC   =CL4'PRD=',DTLKYL2  WE HAVE A GLOBAL PRODUCT?                    
         BE    STPF110A                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPRD                                    
*                                  TELL CONTROLLER TO IGNORE SEL CODES          
STPF110A LA    R2,SPFTABLE                                                      
         OI    CTLRFLG1,CF1TSELQ+CF1CKOFF   AND LIST OVERLAY TO CHECK           
         MVC   SELOFFST,SVDOFFST            OFFSET OF SELECT                    
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPF110X B     STPFX                                                            
*                                                                               
STPFX    B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE BUILDS THE TWA ON THE FLY                                        
***********************************************************************         
SETSCRN  DS    0H                                                               
         NMOD1 0,**SSCR**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         XC    SVFLDLST,SVFLDLST                                                
         XC    MINEKEY,MINEKEY     DO WE HAVE A DETAIL ORDER ELEMENT?           
         MVI   MINEKEY,SNVDTELQ                                                 
         BAS   RE,MINIOHI                                                       
         BNE   SSCR00                                                           
         L     R6,MINELEM          R6 = A(DETAIL ORDER ELEMENT)                 
         USING SNVDTELD,R6                                                      
         CLI   SNVDTEL,SNVDTELQ                                                 
         BE    SSCR10                                                           
*****                                                                           
* DEFAULT LIST IS THE REQUIRED FIELDS                                           
*****                                                                           
SSCR00   MVI   SVFLDCNT,4          NO, USE FIRST 4 FIELDS IN TABLE              
         ZIC   R1,SVFLDCNT                                                      
         LA    R2,FLDEQTBL                                                      
         LA    R3,SVFLDLST                                                      
SSCR02   MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
         LA    R2,L'FLDEQTBL(R2)                                                
         BCT   R1,SSCR02                                                        
*                                                                               
         TM    MISCFLG1,MF1GBLPR   IF WE HAVE A GLOBAL PRODUCT                  
         BNZ   SSCR04              THEN WE DON'T NEED PRODUCT FIELD             
         MVI   0(R3),FLDNPROD                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
SSCR04   CLI   GLOBLEST,0          IF WE HAVE A GLOBAL ESTIAMTE                 
         BNE   SSCR06              THEN WE DON'T NEED ESTIMATE FIELD            
         MVI   0(R3),FLDNESTM                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
SSCR06   CLI   P0TIFCA,C'Y'        FILM CODES ACCEPTED?                         
         BNE   SSCR07              NO, THEN WE DON'T NEED A FILM FIELD          
         MVI   0(R3),FLDNFILM                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
SSCR07   CLI   NETPAKSW,C'Y'       DO WE NEED NETWORK FIELD?                    
         BE    SSCR08              NO                                           
         CLI   BSTA,X'E8'          CABLE STATION?                               
         BL    SSCR08              NO                                           
         TM    BSTA+2,X'7F'        YES, GOT NETWORK ALREADY?                    
         BNZ   SSCR08 '                 YES                                     
         MVI   0(R3),FLDNNTWK                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
SSCR08   DS    0H                                                               
         TM    MATCHFL,MFMATCHQ                                                 
         BNO   SSCR20                                                           
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   SSCR08A                                                          
*                                                                               
         MVI   0(R3),FLDNNEST                                                   
         MVI   1(R3),FLDNNDAT                                                   
         MVI   2(R3),FLDNSUBL                                                   
         MVI   3(R3),FLDNNTIM                                                   
         MVI   4(R3),FLDNNPGM                                                   
         ZIC   R1,SVFLDCNT                                                      
         AHI   R1,5                                                             
         STC   R1,SVFLDCNT                                                      
         LA    R3,5(R3)                                                         
         B     SSCR20                                                           
*                                                                               
SSCR08A  DS    0H                                                               
         MVI   0(R3),FLDNMEST                                                   
         MVI   1(R3),FLDNLINE                                                   
         MVI   2(R3),FLDNSDAT                                                   
         MVI   3(R3),FLDNSPOT                                                   
         ZIC   R1,SVFLDCNT                                                      
         AHI   R1,4                                                             
         STC   R1,SVFLDCNT                                                      
         LA    R3,4(R3)                                                         
         B     SSCR20                                                           
*                                                                               
* GET DETAILS FROM DETAIL ORDER ELEMENT                                         
*                                                                               
SSCR10   ZIC   R1,SNVDTLEN         R1 = NUMBER OF FIELD IN LIST                 
         SH    R1,=Y(SNVDTOVQ)                                                  
         STC   R1,SVFLDCNT                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVFLDLST(0),SNVDTFLD  COPY FIELD LIST                            
*                                                                               
         TM    MATCHFL,MFOCOSTQ                                                 
         BNO   SSCR12                                                           
         ZIC   RF,SNVDTLEN         R1 = NUMBER OF FIELD IN LIST                 
         SH    RF,=Y(SNVDTOVQ)                                                  
         LA    R1,SVFLDLST                                                      
         AR    R1,RF                                                            
         MVI   0(R1),FLDNOCST                                                   
         ZIC   R1,SVFLDCNT                                                      
         AHI   R1,1                                                             
         STC   R1,SVFLDCNT                                                      
*                                                                               
SSCR12   DS    0H                                                               
         TM    MATCHFL,MFMATCHQ                                                 
         BNO   SSCR20                                                           
         XC    SVFLDLST+4(L'SVFLDLST-4),SVFLDLST+4                              
*                                                                               
         LA    R1,SVFLDLST+4                                                    
         CLI   NETPAKSW,C'Y'                                                    
         BE    SSCR15                                                           
*                                                                               
         MVI   0(R1),FLDNMEST                                                   
         MVI   1(R1),FLDNLINE                                                   
         MVI   2(R1),FLDNSDAT                                                   
         MVI   3(R1),FLDNSPOT                                                   
         MVI   4(R1),FLDNDEMV                                                   
         MVI   SVFLDCNT,9                                                       
         B     SSCR20                                                           
*                                                                               
SSCR15   DS    0H                                                               
         MVI   0(R1),FLDNNEST                                                   
         MVI   1(R1),FLDNNDAT                                                   
         MVI   2(R1),FLDNSUBL                                                   
         MVI   3(R1),FLDNNTIM                                                   
         MVI   4(R1),FLDNNPGM                                                   
         MVI   5(R1),FLDNDEMV                                                   
         MVI   SVFLDCNT,10                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
SSCR20   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
SSCRPARM USING TWAPARMD,DMCB                                                    
         ST    RA,SSCRPARM.TWAPATWA                                             
         XC    TWAELEM,TWAELEM                                                  
         LA    R3,TWAELEM                                                       
         ST    R3,SSCRPARM.TWAPAFST                                             
         LA    R3,DTLTAGH                                                       
         ST    R3,SSCRPARM.TWAPAOUT                                             
*                                                                               
SSCRELEM USING TWAELEMD,TWAELEM                                                 
         MVI   SSCRELEM.TWAELCD,1                                               
         MVI   SSCRELEM.TWAERLN,0                                               
         MVI   SSCRELEM.TWAECOL,2          ALL LINES START @ COL 2              
*****                                                                           
* COMMON COLUMN HEADINGS                                                        
*****                                                                           
         LA    R6,SVFLDLST         R6 = A(1ST FIELD TO SCREEN)                  
         ZIC   R3,SVFLDCNT                                                      
*        NI    MISCFLG1,X'FF'-MF1USESM    FIRST LINE                            
         NI    MISCFLG1,X'FF'-MF1USESM-MF1NT1ST    FIRST LINE                   
*                                                                               
SSCR30   LA    R2,FLDTABLE         R2 = A(FIELD TABLE)                          
         USING TABLDSCT,R2                                                      
*                                                                               
SSCR33   CLI   TABLELEN,X'FF'      DIE IF WE COULDN'T FIND FIELD                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R6),TABLFNUM    MATCH ON FIELD NUMBER?                       
         BE    SSCR36              YES                                          
         ZIC   R0,TABLELEN         NO, BUMP TO NEXT ENTRY                       
         AR    R2,R0                                                            
         B     SSCR33                                                           
*                                                                               
SSCR36   ZIC   R1,SSCRELEM.TWAECOL    WILL THE FIELD FIT ON THIS LINE?          
         ZIC   R0,TABLFLEN                                                      
         AR    R0,R1                                                            
         CH    R0,=H'81'              NO                                        
         BNH   *+12                                                             
         OI    MISCFLG1,MF1USESM   2 LINES OR MORE PER DETAIL RECORD            
         B     SSCR50                                                           
         BAS   RE,COLTOSCR         SEND THE COLUMN HEADING TO SCREEN            
*                                                                               
         LA    R6,1(R6)            TRY TO PUT NEXT HEADING ON SCREEN            
         BCT   R3,SSCR30                                                        
         DROP  R2                                                               
         EJECT                                                                  
*****                                                                           
* DO THE UNPROTECTED FIELDS NOW                                                 
*****                                                                           
SSCR50   LA    R3,NLSTLINS         NUMBER OF LINES ON SCREEN                    
         MVI   NDETLLNS,0          MAXIMUM NUMBER OF DETAILS ON SCREEN          
         MVI   NLINPREC,1          NUMBER OF LINES PER DETAIL RECORD            
         L     R0,SSCRPARM.TWAPAOUT                                             
         SR    R0,RA                                                            
         STH   R0,DFIRSTLN           SAVE A(FIRST RECORD LINE)                  
         NI    MISCFLG1,X'FF'-MF1GOTAD   NEED A(SECOND RECORD LINE)             
*                                                                               
SSCRLOOP TM    MISCFLG1,MF1USESM   NEED MORE THAN 1 LINE/RECORD?                
         BZ    SSCR55              NO                                           
         CLM   R3,1,NLINPREC       DO WE HAVE TWO LINES TO USE?                 
         BL    SSCR100                                                          
*                                                                               
SSCR55   MVI   SSCRELEM.TWAERLN,1    START ON THE NEXT LINE                     
         MVI   SSCRELEM.TWAECOL,2    ALL LINES START @ COL 2                    
*                                                                               
         LA    R6,SVFLDLST         R6 = A(1ST FIELD TO GO OUT)                  
         ZIC   R0,SVFLDCNT         R0 = NUMBER OF FIELD IN LIST                 
         NI    MISCFLG1,X'FF'-MF1NT1ST    FIRST LINE OF RECORD AGAIN            
*                                                                               
SSCR60   LA    R2,FLDTABLE         R2 = A(FIELD TABLE)                          
         USING TABLDSCT,R2                                                      
*                                                                               
SSCR62   CLI   TABLELEN,X'FF'      DIE IF WE COULDN'T FIND FIELD                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R6),TABLFNUM    MATCH ON FIELD NUMBER?                       
         BE    SSCR64              YES                                          
         ZIC   RE,TABLELEN         NO, BUMP TO NEXT ENTRY                       
         AR    R2,RE                                                            
         B     SSCR62                                                           
*                                                                               
SSCR64   ZIC   RE,SSCRELEM.TWAECOL    WILL THE FIELD FIT ON THIS LINE?          
         ZIC   RF,TABLFLEN                                                      
         AR    RE,RF                                                            
         CH    RE,=H'81'              NO                                        
         BH    SSCR70                                                           
*                                                                               
         TM    MISCFLG1,MF1NT1ST   THIS FIELD GOES ON THE SECOND LINE?          
         BZ    SSCR66              NO                                           
         ZIC   RF,TABLELEN         YES, SEE IF TITLE OF FIELD FITS ALSO         
         SH    RF,=Y(TABLTEXT-TABLDSCT)   FIELD'S TITLE LENGTH                  
         LA    RF,2(RF)            NEED 2 EXTRA COLUMNS FOR '= '                
         AR    RE,RF                                                            
         CH    RE,=H'81'           DOES IT FIT WITH TITLE?                      
         BH    SSCR70              NO                                           
*                                                                               
SSCR66   DS    0H                                                               
         MVC   FULL,SSCRPARM.TWAPANXT                                           
         BAS   RE,FLDTOSCR         SEND THE COLUMN HEADING TO SCREEN            
         CLI   0(R6),FLDNOCST                                                   
         BNE   SSCR80                                                           
         L     RF,FULL                                                          
         TM    MISCFLG1,MF1NT1ST   THIS FIELD GOES ON THE SECOND LINE?          
         BZ    *+12                NO                                           
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         OI    1(RF),X'20'                                                      
*                                                                               
         B     SSCR80                                                           
*                                                                               
SSCR70   OI    MISCFLG1,MF1NT1ST   OFFICIALLY PAST FIRST LINE                   
         BCTR  R3,0                WE HAVE 1 LESS LINE TO WORK WITH             
*                                                                               
         TM    MISCFLG1,MF1GOTAD   ESTABLISHED # OF LINES/REC YET?              
         BNZ   SSCR75              YES                                          
         ZIC   RE,NLINPREC         NO, FIGURE OUT HOW MUCH THEN                 
         LA    RE,1(RE)                                                         
         STC   RE,NLINPREC                                                      
*                                                                               
SSCR75   MVI   SSCRELEM.TWAERLN,1    START ON THE NEXT LINE                     
         MVI   SSCRELEM.TWAECOL,6    SECOND LINES START @ COL 6                 
         B     SSCR64                                                           
*                                                                               
SSCR80   LA    R6,1(R6)            TRY TO PUT NEXT FIELD ON SCREEN              
         BCT   R0,SSCR60                                                        
*                                                                               
         ZIC   R0,NDETLLNS         SAVE MAX NUMBER OF DETAILS ON SCREEN         
         AH    R0,=H'1'                                                         
         STC   R0,NDETLLNS                                                      
*                                                                               
         TM    MISCFLG1,MF1GOTAD   GOT A(SECOND RECORD LINE)?                   
         BNZ   SSCR90                                                           
         OI    MISCFLG1,MF1GOTAD   NO, SAVE IT NOW                              
         L     R1,SSCRPARM.TWAPAOUT                                             
         SR    R1,RA                                                            
         LH    R0,DFIRSTLN                                                      
         SR    R1,R0                                                            
         STH   R1,BTWNR1R2                                                      
*                                                                               
SSCR90   L     R1,SSCRPARM.TWAPAOUT   SEE IF WE HAVE ENOUGH ROOM FOR            
         AH    R1,BTWNR1R2               ANOTHER DETAIL RECORD, PFKEY           
         AH    R1,=Y(8+LENFPFKY+8)       LINE AND 2 1-BYTE UNPROTECTED          
         AH    R1,=Y(2*(8+1))            FIELDS BEFORE WE BEGIN TO              
         LA    R0,AFRSTKEY               OVERWRITE GENCON'S WORK SPACE          
         CR    R1,R0                     WHICH BEGINS @ AFRSTKEY                
         BH    SSCR100                                                          
*                                                                               
         BCT   R3,SSCRLOOP         FILL UP THE SCREEN                           
         DROP  R2                                                               
         EJECT                                                                  
*****                                                                           
* PUT PFKEY LINE OUT                                                            
*****                                                                           
SSCR100  MVI   SSCRELEM.TWAELCD,1          SET ELEMENT CODE                     
         MVI   SSCRELEM.TWAERLN,24         PFKEY LINE IS ON LAST LINE           
         OI    SSCRELEM.TWAERLN,TWAERLAB                                        
         MVI   SSCRELEM.TWAECOL,2                        COL 2                  
         MVI   SSCRELEM.TWAEATB,X'68'      PROT,LOWER,& HIGHLIGHTED             
         MVI   SSCRELEM.TWAEFLD,FLDNPFKY   SET PFKEY LINE'S OWN NUMBER          
         XC    SSCRELEM.TWAEDTA(L'TWAELEM-TWAELLNQ),SSCRELEM.TWAEDTA            
         MVI   SSCRELEM.TWAEFLN,73         SET FIELD LENGTH                     
         MVC   SSCRELEM.TWAEDTA(73),=CL74'PF1=Add Hdr 4=Add Dtls 5=Mat X        
               6=Top 7=Prev 8=Next 9=Bot 10=Req 11=i2 12=Ret'                   
*        MVC   SSCRELEM.TWAEDTA(73),=CL73'PF1=Add Hdr 4=Add Dtls 5=MatcX        
               h 6=Top 7=Prev 8=Next 9=Bottom 10=Req 12=Ret'                    
*                                                                               
         MVC   PARAS(6*4),DMCB                                                  
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   *+10                                                             
         MVC   SSCRELEM.TWAEDTA+23(7),BLANKS                                    
         MVC   DMCB(6*4),PARAS                                                  
         DROP  R1                                                               
*                                                                               
         MVI   SSCRELEM.TWAELLN,TWAELLNQ+73  SET LENGTH OF THIS ELEMENT         
*                                                                               
         GOTO1 TWABLD,DMCB         BUILD THIS FIELD                             
         CLI   SSCRPARM.TWAPERRS,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTLINE,SSCRPARM.TWAPAOUT                                       
*****                                                                           
* PUT OUT 1 BYTE UNPROTECTED FIELD                                              
*****                                                                           
*                                  NEXT AVAILABLE WILL BE OUTPUT AREA           
         MVC   SSCRPARM.TWAPAOUT,SSCRPARM.TWAPANXT                              
         MVI   SSCRELEM.TWAELCD,1          SET ELEMENT CODE                     
         MVI   SSCRELEM.TWAERLN,0          RESET RELATIVE LINE NUMBER           
         MVI   SSCRELEM.TWAECOL,76         COLUMN 76                            
         MVI   SSCRELEM.TWAEATB,0                                               
         MVI   SSCRELEM.TWAEFLD,0                                               
         XC    SSCRELEM.TWAEDTA(L'TWAELEM-TWAELLNQ),SSCRELEM.TWAEDTA            
         MVI   SSCRELEM.TWAEFLN,1          SET FIELD LENGTH                     
         MVI   SSCRELEM.TWAELLN,TWAELLNQ   SET LENGTH OF THIS ELEMENT           
*                                                                               
         GOTO1 TWABLD,DMCB         BUILD THIS FIELD                             
         CLI   SSCRPARM.TWAPERRS,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*****                                                                           
* PUT OUT 1 BYTE UNPROTECTED FIELD                                              
*****                                                                           
*                                  NEXT AVAILABLE WILL BE OUTPUT AREA           
         MVC   SSCRPARM.TWAPAOUT,SSCRPARM.TWAPANXT                              
         MVI   SSCRELEM.TWAELCD,1          SET ELEMENT CODE                     
         MVI   SSCRELEM.TWAERLN,0          RESET RELATIVE LINE NUMBER           
         MVI   SSCRELEM.TWAECOL,80         COLUMN 80                            
         MVI   SSCRELEM.TWAEATB,0                                               
         MVI   SSCRELEM.TWAEFLD,0                                               
         XC    SSCRELEM.TWAEDTA(L'TWAELEM-TWAELLNQ),SSCRELEM.TWAEDTA            
         MVI   SSCRELEM.TWAEFLN,1          SET FIELD LENGTH                     
         MVI   SSCRELEM.TWAELLN,TWAELLNQ   SET LENGTH OF THIS ELEMENT           
*                                                                               
         GOTO1 TWABLD,DMCB         BUILD THIS FIELD                             
         CLI   SSCRPARM.TWAPERRS,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SSCRX    B     XIT                                                              
         DROP  R5,SSCRELEM,SSCRPARM                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SENDS A COLUMN HEADING TO THE SCREEN WITH TWABLD                 
*                                                                               
* ON ENTRY:    DMCB                TWABLD PARAMETER BLOCK                       
*              TWAELEM.TWAERLN     RELATIVE LINE NUMBER OF FIELD                
*              TWAELEM.TWAECOL     COLUMN NUMBER OF THE FIELD                   
*              (R2)                TABLE ENTRY                                  
*                                                                               
* ON EXIT:     DMCB                TWABLD PARAMETER BLOCK GETS UPDATED          
*              TWAELEM.TWAERLN     RESET TO 0 SO WE CAN BUILD COLUMNS           
*              TWAELEM.TWAECOL     NEXT AVAILABLE COLUMN FOR NEXT FIELD         
***********************************************************************         
COLTOSCR NTR1                                                                   
CSCRPARM USING TWAPARMD,DMCB                                                    
CSCRELEM USING TWAELEMD,TWAELEM                                                 
         USING TABLDSCT,R2                                                      
*                                                                               
CSCR10   MVI   CSCRELEM.TWAELCD,1          SET ELEMENT CODE                     
         MVI   CSCRELEM.TWAEATB,X'68'      PROT,LOWER,& HIGHLIGHTED             
         MVI   CSCRELEM.TWAEFLD,0          NO FIELD NUMBER                      
         XC    CSCRELEM.TWAEDTA(L'TWAELEM-TWAELLNQ),CSCRELEM.TWAEDTA            
         ZIC   R1,TABLELEN                 COPY HEADER TITLE                    
         SH    R1,=Y(TABLTEXT-TABLDSCT)                                         
         STC   R1,CSCRELEM.TWAEFLN         TEXT HAS ITS OWN LENGTH              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSCRELEM.TWAEDTA(0),TABLTEXT                                     
         AH    R1,=Y(TWAELLNQ+1)        SET LENGTH OF THIS ELEMENT              
         STC   R1,CSCRELEM.TWAELLN                                              
*                                                                               
         TM    MISCFLG1,MF1NT1ST   ARE WE ON A SECOND LINE?                     
         BNZ   CSCR15                                                           
         MVC   CSCRELEM.TWAEFLN,TABLFLEN   NO, COLUMN IS WIDE AS FIELD          
         B     CSCR20                                                           
*                                                                               
CSCR15   IC    R1,CSCRELEM.TWAEFLN   YES, COLUMN HAS '=' AFTER TEXT             
         LA    R1,CSCRELEM.TWAEDTA(R1)                                          
         MVI   0(R1),C'='                                                       
         IC    R1,CSCRELEM.TWAEFLN   FIELD IS 1 BYTE LONGER WITH '='            
         AH    R1,=H'1'                                                         
         STC   R1,CSCRELEM.TWAEFLN                                              
         IC    R1,CSCRELEM.TWAELLN   TWA BUILD ELEMENT IS 1 BYTE LONGER         
         AH    R1,=H'1'                                                         
         STC   R1,CSCRELEM.TWAELLN                                              
*                                                                               
CSCR20   GOTO1 TWABLD,DMCB         BUILD THIS FIELD                             
         CLI   CSCRPARM.TWAPERRS,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                    NEXT AVAILABLE WILL BE OUTPUT AREA         
         MVC   CSCRPARM.TWAPAOUT,CSCRPARM.TWAPANXT                              
*                                                                               
         MVI   CSCRELEM.TWAERLN,0    RESET RELATIVE LINE NUMBER                 
         ZIC   R1,CSCRELEM.TWAEFLN   BUMP TO NEXT COLUMN                        
         ZIC   R0,CSCRELEM.TWAECOL                                              
         AR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,CSCRELEM.TWAECOL                                              
*                                                                               
CSCRX    B     XIT                                                              
         DROP  R2,CSCRELEM,CSCRPARM                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SENDS A FIELD TO THE SCREEN WITH TWABLD                          
*                                                                               
* ON ENTRY:    DMCB                TWABLD PARAMETER BLOCK                       
*              TWAELEM.TWAERLN     RELATIVE LINE NUMBER OF FIELD                
*              TWAELEM.TWAECOL     COLUMN NUMBER OF THE FIELD                   
*              (R2)                TABLE ENTRY                                  
*                                                                               
* ON EXIT:     DMCB                TWABLD PARAMETER BLOCK GETS UPDATED          
*              TWAELEM.TWAERLN     RESET TO 0 SO WE CAN BUILD COLUMNS           
*              TWAELEM.TWAECOL     NEXT AVAILABLE COLUMN FOR NEXT FIELD         
***********************************************************************         
FLDTOSCR NTR1                                                                   
FSCRPARM USING TWAPARMD,DMCB                                                    
FSCRELEM USING TWAELEMD,TWAELEM                                                 
         USING TABLDSCT,R2                                                      
*                                                                               
         TM    MISCFLG1,MF1NT1ST   ARE WE ON THE SECOND LINE?                   
         BZ    *+8                 NO                                           
         BAS   RE,COLTOSCR         YES, SHOW TITLE OF THE FIELD                 
*                                                                               
         MVI   FSCRELEM.TWAELCD,1          SET ELEMENT CODE                     
         MVC   FSCRELEM.TWAEFLN,TABLFLEN   SET FIELD LENGTH                     
         MVI   FSCRELEM.TWAEATB,0          NO SPECIAL ATTRIBUTES                
         MVC   FSCRELEM.TWAEFLD,TABLFNUM   FIELD NUMBER                         
         XC    FSCRELEM.TWAEDTA(L'TWAELEM-TWAELLNQ),FSCRELEM.TWAEDTA            
         MVI   FSCRELEM.TWAELLN,TWAELLNQ   SET LENGTH OF ELEMENT                
*                                                                               
         GOTO1 TWABLD,DMCB         BUILD THIS FIELD                             
         CLI   FSCRPARM.TWAPERRS,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                    NEXT AVAILABLE WILL BE OUTPUT AREA         
         MVC   FSCRPARM.TWAPAOUT,FSCRPARM.TWAPANXT                              
*                                                                               
         MVI   FSCRELEM.TWAERLN,0    RESET RELATIVE LINE NUMBER                 
         ZIC   R1,TABLFLEN           BUMP TO NEXT COLUMN                        
         ZIC   R0,FSCRELEM.TWAECOL                                              
         AR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,FSCRELEM.TWAECOL                                              
*                                                                               
FSCRX    B     XIT                                                              
         DROP  R2,FSCRELEM,FSCRPARM                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE DATE OF THE DETAIL ELEMENT ON THE FIELD             
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISDATE  DS    0H                                                               
         NMOD1 0,**DDTE**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         ZIC   R3,SNVIDDAY                                                      
         GOTO1 ADDAY,DMCB,PERSDATE,WORK,(R3)                                    
         GOTO1 DATCON,DMCB,(0,WORK),(8,8(R2))                                   
*                                                                               
DDTEFCHK TM    FLTRFLG1,FF1DDATE   CHECK IF FILTER                              
         BZ    DDTEYES                                                          
         CLC   SNVIDDAY,FLTRDTE1                                                
         BL    DDTENO                                                           
         CLC   SNVIDDAY,FLTRDTE2                                                
         BH    DDTENO                                                           
*                                                                               
DDTEYES  B     YES                                                              
*                                                                               
DDTENO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS MATCHMAKER DETAIL DATE                                  
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISSDAT  DS    0H                                                               
         NMOD1 0,**DSDT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R5,SNVIDBDT                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    R5,SNVIDUDT                                                      
*                                                                               
         OC    0(2,R5),0(R5)                                                    
         BNZ   DISS10                                                           
         XC    8(LENFSDAT,R2),8(R2)                                             
         MVI   5(R2),0                                                          
         B     YES                                                              
*                                                                               
DISS10   GOTO1 DATCON,DMCB,(2,0(R5)),(8,8(R2))                                  
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE DATE OF THE DETAIL LINE                            
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALDATE  DS    0H                                                               
         NMOD1 0,**VDTE**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VDTE02                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM   IF NOTHING BEFORE                            
         BZ    MISSFLD                                                          
*                                                                               
VDTEDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDDAY,VDTEDSCT.SNVIDDAY   COPY THE DATE                       
         GOTO1 =A(DISDATE),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VDTEX                                                            
*                                                                               
VDTE02   CLI   5(R2),2             IF NUMERIC AND LESS THAN 2 BYTES             
         BH    VDTE10                                                           
         TM    4(R2),X'08'                                                      
         BZ    VDTE10                                                           
*                                                                               
         ZIC   R1,5(R2)            THEN ASSUME DAY OF PERIOD MONTH              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM   DO WE HAVE A DETAIL IN LINE ABOVE?           
         BNZ   VDTE06                                                           
*                                                                               
         GOTO1 ADDAY,DMCB,PERSDATE,DUB,15   NO, USE PERIOD MMM/YY               
VDTE04   MVC   DUB+4(2),=C'01'                                                  
         BCTR  R3,0                                                             
         GOTO1 ADDAY,DMCB,DUB,WORK,(R3)                                         
         GOTO1 DATCON,DMCB,(0,WORK),(11,8(R2))                                  
         MVI   5(R2),8                                                          
         B     VDTE10                                                           
*                                                                               
VDTE06   ZIC   R1,VDTEDSCT.SNVIDDAY         YES, USE PREVIOUS LINE'S            
         ST    R1,DMCB+8                       MMM/YY                           
         GOTO1 ADDAY,DMCB,PERSDATE,DUB                                          
         B     VDTE04                                                           
         DROP  VDTEDSCT                                                         
*                                                                               
VDTE10   LA    R0,8(R2)                                                         
         ST    R0,DMCB                                                          
         MVC   DMCB(1),5(R2)                                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(X'60',PERVALST)                                    
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
*                                                                               
VDTEPVAL USING PERVALD,PERVALST                                                 
         TM    VDTEPVAL.PVALASSM,PVALASY       YEAR ASSUMED?                    
         BZ    VDTE12                          NO                               
         CLC   VDTEPVAL.PVALESTA(2),PEREDATE   YES, SAME AS PERIOD?             
         BE    VDTE12                                                           
         MVC   VDTEPVAL.PVALESTA(2),PEREDATE   MAKE IT THE SAME IF NOT          
         GOTO1 DATCON,DMCB,(0,VDTEPVAL.PVALESTA),(11,8(R2))                     
         OI    6(R2),X'80'                                                      
         MVI   5(R2),8                                                          
*                                                                               
VDTE12   CLI   GLOBLEST,0          DO WE HAVE A GLOBAL ESTIMATE?                
         BNE   VDTE20              YES, CHECK AGAINST ESTIMATE DATES            
*                                                                               
         LA    R1,SVFLDLST                                                      
         ZIC   R0,SVFLDCNT                                                      
VDTE15   CLI   0(R1),FLDNDATE      DATE BEFORE ESTIMATE FIELD?                  
         BE    VDTE30              YES                                          
         CLI   0(R1),FLDNESTM      ESTIMATE BEFORE DATE?                        
         BE    VDTE20              YES                                          
         BCT   R0,VDTE15                                                        
*                                                                               
VDTE20   OC    INTESTSD,INTESTSD   ANY ESTIMATE?                                
         BZ    VDTE30              NO, CHECK AGAINST PERIOD DATE                
*                                                                               
         CLC   VDTEPVAL.PVALESTA,INTESTSD    MAKE SURE DETAIL DATE IS           
         BL    *+14                            BETWEEN ESTIMATE DATES           
         CLC   VDTEPVAL.PVALESTA,INTESTED                                       
         BNH   VDTE30                                                           
         CLI   PI2YCKEE,C'Y'       CAN THE DATE BE OUTSIDE OF ESTIMATE?         
         BNE   BADDTEST            NO, WE'RE CHECKING BASED ON I2Y PROF         
*                                                                               
VDTE30   CLC   VDTEPVAL.PVALESTA,PERSDATE    MAKE SURE DETAIL DATE IS           
         BL    BADDTOUT                        BETWEEN INVOICE DATES            
         CLC   VDTEPVAL.PVALESTA,PEREDATE                                       
         BH    BADDTOUT                                                         
*                                  GET # OF DAYS FROM START DATE                
         GOTO1 DATCON,DMCB,(X'20',PERSDATE),(8,FAKEFLD),               X        
               VDTEPVAL.PVALESTA                                                
         MVI   FAKEFLDH+5,17                                                    
         LA    R0,FAKEFLD                                                       
         ST    R0,DMCB                                                          
         MVC   DMCB(1),FAKEFLDH+5                                               
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,PERVALST                                            
*                                                                               
         ZICM  R1,VDTEPVAL.PVALNDYS,2                                           
         BCTR  R1,0                SHOULD BE DISPLACEMENT FROM START            
         STC   R1,SNVIDDAY                                                      
*                                                                               
VDTEX    B     XIT                                                              
         DROP  R6,VDTEPVAL                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE TIME OF THE DETAIL ELEMENT ON THE FIELD             
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISTIME  DS    0H                                                               
         NMOD1 0,**DTIM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R3,8(R2)                                                         
         MVI   7(R2),0                                                          
*                                                                               
         TM    SNVIDCTL,SNVIDSIQ   SKIP INTERVAL CHECK?                         
         BZ    DTIM10                                                           
         MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
         ZIC   R1,7(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,7(R2)                                                         
*                                                                               
DTIM10   TM    SNVIDCTL,SNVIDITQ   IGNORE TIME?                                 
         BZ    DTIM20                                                           
         MVI   0(R3),C'T'                                                       
         LA    R3,1(R3)                                                         
         ZIC   R1,7(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,7(R2)                                                         
*                                                                               
DTIM20   SR    R0,R0                                                            
         ZICM  R1,SNVIDTIM,2                                                    
         D     R0,=F'60'           CONVERT THE TIME TO MILITARY                 
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
*                                                                               
         AH    R1,=H'600'          THE TIME IS DISPLACEMENT FROM 6:00A          
         CH    R1,=H'2400'         EARLY MORNING?                               
         BL    *+8                                                              
         SH    R1,=H'2400'         YES                                          
         STH   R1,HALF                                                          
*                                                                               
         GOTO1 TIMUNPK,DMCB,HALF,WORK                                           
         MVC   0(5,R3),WORK        COPY THE TIME                                
         ZIC   R1,7(R2)            5 BYTE TIME                                  
         LA    R1,5(R1)                                                         
         STC   R1,7(R2)                                                         
*                                                                               
         CLI   WORK,C' '                                                        
         BNE   DTIMFCHK                                                         
         MVC   0(4,R3),WORK+1                                                   
         MVI   4(R3),0                                                          
         IC    R1,7(R2)            4 BYTE TIME                                  
         BCTR  R1,0                   LESS ONE BYTE THAN BEFORE                 
         STC   R1,7(R2)                                                         
*                                                                               
DTIMFCHK DS    0H                                                               
*                                                                               
DTIMYES  B     YES                                                              
*                                                                               
DTIMNO   B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE DETAIL UNIT TIME FOR MATCHMAKER (NET)               
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISNTIM  DS    0H                                                               
         NMOD1 0,**DNTM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         OC    SNVIDUDT(20),SNVIDUDT                                            
         BZ    YES                                                              
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,SNVIDUTM         TIME IN QUARTERS OF AN HOUR                  
         MHI   R1,15               CONVERT TO MINUTES                           
         D     R0,=F'60'           CONVERT THE TIME TO MILITARY                 
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
*                                                                               
         AH    R1,=H'600'          THE TIME IS DISPLACEMENT FROM 6:00A          
         CH    R1,=H'2400'         EARLY MORNING?                               
         BL    *+8                                                              
         SH    R1,=H'2400'         YES                                          
         STH   R1,HALF                                                          
*                                                                               
         GOTO1 TIMUNPK,DMCB,HALF,WORK                                           
         MVC   8(5,R2),WORK                                                     
         MVI   5(R2),5                                                          
*                                                                               
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE TIME OF THE DETAIL LINE                            
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALTIME  DS    0H                                                               
         NMOD1 0,**VTIM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VTIM10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
VTIMDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDTIM,VTIMDSCT.SNVIDTIM                                       
*                                                                               
         NI    SNVIDCTL,X'FF'-(SNVIDSIQ+SNVIDITQ)                               
         TM    VTIMDSCT.SNVIDCTL,SNVIDSIQ     SKIP INTERVAL CHECK?              
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDSIQ                                                
         TM    VTIMDSCT.SNVIDCTL,SNVIDITQ     IGNORE TIME?                      
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDITQ                                                
         GOTO1 =A(DISTIME),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VTIMX                                                            
*                                                                               
VTIM10   LA    R3,8(R2)                                                         
         NI    SNVIDCTL,X'FF'-(SNVIDSIQ+SNVIDITQ)                               
*                                                                               
VTIM15   CLI   0(R3),C'T'          IGNORE TIME FOR MATCH?                       
         BNE   VTIM20                                                           
         TM    SNVIDCTL,SNVIDITQ       ALREADY ON?                              
         BNZ   INVLFLD                                                          
         OI    SNVIDCTL,SNVIDITQ   YES                                          
         LA    R3,1(R3)                                                         
         B     VTIM15                                                           
*                                                                               
VTIM20   CLI   0(R3),C'N'          SKIP INTERVAL CHECK?                         
         BNE   VTIM30                                                           
         TM    SNVIDCTL,SNVIDSIQ       ALREADY ON?                              
         BNZ   INVLFLD                                                          
         OI    SNVIDCTL,SNVIDSIQ   YES                                          
         LA    R3,1(R3)                                                         
         B     VTIM15                                                           
*                                                                               
VTIM30   GOTO1 TIMPK,DMCB,0(R3),HALF                                            
         CLI   DMCB,0                                                           
         BE    BADTIME                                                          
*                                                                               
         ZIC   R1,5(R2)            FIND OUT WHAT THE LAST CHARACTER IS          
         LA    RE,8(R1,R2)                                                      
         BCTR  RE,0                                                             
         CLI   0(RE),C'M'          MIDNIGHT OR 'M' OF AM/PM?                    
         BE    VTIM50                                                           
         CLI   0(RE),C'N'          NOON?                                        
         BE    VTIM50                                                           
         CLI   0(RE),C'A'          AM?                                          
         BE    VTIM50                                                           
         CLI   0(RE),C'P'          PM?                                          
         BE    VTIM50                                                           
*                                                                               
         CLC   HALF,=H'1300'       IS TIME MILITARY?                            
         BL    *+12                NO                                           
         OI    MISCFLG2,MF2MTIME   YES, MARK AS MILITARY TIME                   
         B     VTIM50                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM   ANYTHING BEFORE?                             
         BZ    VTIM50              NONE, USE MILITARY ALSO                      
*                                                                               
         TM    MISCFLG2,MF2MTIME   MILITARY TIME USED BEFORE?                   
         BNZ   VTIM50              YES, DON'T CARE WHAT'S ABOVE                 
*                                                                               
         SR    R0,R0                                                            
         ZICM  R1,VTIMDSCT.SNVIDTIM,2                                           
         D     R0,=F'60'           CONVERT FROM MINUTES TO MILITARY             
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         AH    R1,=H'600'          THE TIME IS DISPLACED FROM 6:00A             
         CH    R1,=H'2400'         EARLY MORNING?                               
         BL    *+8                                                              
         SH    R1,=H'2400'                                                      
*                                                                               
         CH    R1,=H'1200'         IS ABOVE DTL'S TIME PAST NOON?               
         BL    VTIM40              NO, AM IS DEFAULT FOR TIMPK                  
         CLC   HALF,=H'1200'       LINE ABOVE IS PM AND TIME ENTERED IS         
         BNL   VTIM50                 BETWEEN 1200P-1259P?                      
         LH    R1,HALF             THEN THIS DTL'S TIME IS PAST NOON            
         AH    R1,=H'1200'                                                      
         STH   R1,HALF                                                          
         B     VTIM50                                                           
*                                                                               
VTIM40   CLC   HALF,=H'1200'       LINE ABOVE IS AM AND TIME ENTERED IS         
         BL    VTIM50                 BETWEEN 1200P-1259P?                      
         LH    R1,HALF                                                          
         SH    R1,=H'1200'         YES                                          
         STH   R1,HALF             THE TIME IS DISPLACEMENT FROM 6:00A          
*                                                                               
VTIM50   LH    R1,HALF                                                          
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         AH    R1,=H'2400'         YES                                          
         SH    R1,=H'600'          THE TIME IS DISPLACEMENT FROM 6:00A          
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         MH    R1,=H'60'           CONVERT THE TIME TO MILITARY                 
         AR    R1,R0                                                            
         STCM  R1,3,SNVIDTIM                                                    
*                                                                               
VTIMX    B     XIT                                                              
         DROP  VTIMDSCT,R6                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE SPOT LENGTH OF THE DETAIL ELEMENT ON THE            
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISSLEN  DS    0H                                                               
         NMOD1 0,**DSLN**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R3,8(R2)                                                         
         TM    SNVIDCT2,SNVIDISQ                                                
         BZ    *+12                                                             
         MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (B1,SNVIDSLN),(3,0(R3)),ALIGN=LEFT                               
*                                                                               
DSLNFCHK TM    FLTRFLG1,FF1DSLEN   CHECK IF FILTER                              
         BZ    DSLNYES                                                          
         CLC   FLTRSLEN,SNVIDSLN                                                
         BNE   DSLNNO                                                           
*                                                                               
DSLNYES  B     YES                                                              
*                                                                               
DSLNNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE CLICKS VALUE                                        
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
***********************************************************************         
DISCLK   DS    0H                                                               
         NMOD1 0,**DCLK**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R3,8(R2)                                                         
*                                                                               
         CLI   SNVIDLEN,SNVIDL4Q                                                
         BL    DISCLKX                                                          
*                                                                               
         CLC   SNVIDCLK,=X'FFFFFFFF'                                            
         BNE   *+14                                                             
         MVC   0(4,R3),=C'NONE'                                                 
         B     YES                                                              
*                                                                               
         EDIT  (B4,SNVIDCLK),(9,0(R3)),ALIGN=LEFT,COMMAS=YES,          X        
               ZERO=NOBLANK                                                     
*                                                                               
DISCLKX  B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE RATINGS VALUE                                       
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
***********************************************************************         
DISRAT   DS    0H                                                               
         NMOD1 0,**DRAT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R3,8(R2)                                                         
*                                                                               
         CLI   SNVIDLEN,SNVIDL4Q                                                
         BL    DISRATX                                                          
*                                                                               
         CLC   SNVIDRAT,=X'FFFFFFFF'                                            
         BNE   *+14                                                             
         MVC   0(4,R3),=C'NONE'                                                 
         B     YES                                                              
*                                                                               
         EDIT  (B4,SNVIDRAT),(6,0(R3)),2,ALIGN=LEFT,ZERO=NOBLANK                
*                                                                               
DISRATX  B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE IMPRESSIONS VALUE                                   
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
***********************************************************************         
DISIMP   DS    0H                                                               
         NMOD1 0,**DIMP**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R3,8(R2)                                                         
*                                                                               
         CLI   SNVIDLEN,SNVIDL4Q                                                
         BL    DISIMPX                                                          
*                                                                               
         CLC   SNVIDIMP,=X'FFFFFFFF'                                            
         BNE   *+14                                                             
         MVC   0(4,R3),=C'NONE'                                                 
         B     YES                                                              
*                                                                               
         TM    SNVIDIMP,X'40'      2-DECIMAL VALUE?                             
         BO    DISIMP10                                                         
*                                                                               
         EDIT  SNVIDIMP,(7,0(R3)),ALIGN=LEFT,1,COMMAS=YES,ZERO=NOBLANK          
         B     DISIMPX                                                          
*                                                                               
DISIMP10 DS    0H                                                               
         MVC   FULL,SNVIDIMP                                                    
         NI    FULL,X'FF'-X'40'    TURN OFF 2-DECIMAL BIT                       
         EDIT  FULL,(8,0(R3)),ALIGN=LEFT,2,COMMAS=YES,ZERO=NOBLANK              
*                                                                               
DISIMPX  B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE PACKAGE CODE                                        
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISNPKG  DS    0H                                                               
         NMOD1 0,**DPKG**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         EDIT  (B1,SNVIDPKG),(3,8(R2)),ALIGN=LEFT                               
*                                                                               
DSNPFCHK TM    FLTRFLG2,FF2NPKG    CHECK IF FILTER                              
         BZ    DSNPYES                                                          
         CLC   FLTRNPKG,SNVIDPKG                                                
         BNE   DSNPNO                                                           
*                                                                               
DSNPYES  B     YES                                                              
*                                                                               
DSNPNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE LINE NUMBER                                         
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISLINE  DS    0H                                                               
         NMOD1 0,**DLIN**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R5,SNVIDBLN                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    R5,SNVIDUSB                                                      
*                                                                               
         EDIT  (B1,0(R5)),(3,8(R2)),ALIGN=LEFT                                  
*                                                                               
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE MATCHMAKER PROGRAM (NET)                            
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISNPGM  DS    0H                                                               
         NMOD1 0,**DNPG**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         MVC   8(6,R2),SNVIDUPG                                                 
*                                                                               
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS SPOT NUMBER WITHIN DATE                                 
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISSPOT  DS    0H                                                               
         NMOD1 0,**DSLN**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         EDIT  (B1,SNVIDBNO),(3,8(R2)),ALIGN=LEFT                               
*                                                                               
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS DEMO VALUE                                              
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISDEM   DS    0H                                                               
         NMOD1 0,**DDEM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         OC    SNVIDDM4,SNVIDDM4                                                
         BZ    DISD15                                                           
*                                                                               
         MVC   FULL,SNVIDDM4                                                    
         TM    FULL,X'40'                                                       
         BZ    DISD10                                                           
         NI    FULL,X'FF'-X'40'                                                 
*                                                                               
         EDIT  FULL,(7,8(R2)),2,ZERO=NOBLANK,ALIGN=RIGHT                        
         B     YES                                                              
*                                                                               
DISD10   DS    0H                                                               
         EDIT  FULL,(7,8(R2)),1,ZERO=NOBLANK,ALIGN=RIGHT                        
         B     YES                                                              
*                                                                               
DISD15   DS    0H                                                               
         OC    SNVIDDEM,SNVIDDEM                                                
         BZ    YES                                                              
*                                                                               
         MVC   HALF,SNVIDDEM                                                    
         TM    HALF,X'40'                                                       
         BZ    DISD20                                                           
         NI    HALF,X'FF'-X'40'                                                 
*                                                                               
         EDIT  HALF,(6,8(R2)),2,ZERO=NOBLANK,ALIGN=RIGHT                        
         B     YES                                                              
*                                                                               
DISD20   DS    0H                                                               
         EDIT  HALF,(6,8(R2)),1,ZERO=NOBLANK,ALIGN=RIGHT                        
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE SPOT LENGTH OF THE DETAIL LINE                     
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALSLEN  DS    0H                                                               
         NMOD1 0,**VSLN**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         NI    SNVIDCT2,X'FF'-SNVIDISQ                                          
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VSLN10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
VSLNDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDSLN,VSLNDSCT.SNVIDSLN                                       
         TM    VSLNDSCT.SNVIDCT2,SNVIDISQ                                       
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDISQ                                                
         DROP  VSLNDSCT                                                         
         GOTO1 =A(DISSLEN),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VSLNX                                                            
*                                                                               
VSLN10   DS    0H                                                               
         TM    4(R2),X'08'         MUST BE VALID NUMERIC                        
*        BZ    BADSLEN                                                          
         BO    VSLN15                                                           
*                                                                               
         CLI   8(R2),C'N'                                                       
         BNE   BADSLEN                                                          
         OI    SNVIDCT2,SNVIDISQ                                                
         ZIC   R0,5(R2)                                                         
         CHI   R0,2                                                             
         BL    BADSLEN                                                          
         CHI   R0,4                                                             
         BH    BADSLEN                                                          
         BCTR  R0,0                                                             
         LA    R1,9(R2)                                                         
*                                                                               
         BRAS  RE,ISNUM                                                         
         BNE   BADSLEN                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VSLN15   DS    0H                                                               
         LA    R3,8(R2)                                                         
         ZIC   R1,5(R2)                                                         
*                                                                               
         TM    SNVIDCT2,SNVIDISQ                                                
         BZ    *+10                                                             
         LA    R3,1(R3)                                                         
         BCTR  R1,0                                                             
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*        PACK  DUB,8(0,R2)                                                      
         PACK  DUB,0(0,R3)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         LTR   R1,R1               CAN'T HAVE A ZERO SPOT LENGTH                
         BZ    BADSLEN                                                          
         CHI   R1,255              MAKE SURE SPOT LENGTH FITS IN 1 BYTE         
         BH    BADSLEN                                                          
         STC   R1,BYTE                                                          
*                                                                               
         CLI   NETPAKSW,C'Y'       ARE WE UNDER NETPAK?                         
         BE    VSLN90              YES, ALL LENGTHS ARE AVAILABLE               
*                                                                               
         L     RF,ASLNTAB                                                       
         LR    R1,RF               POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    VSLN20                                                           
         CLI   QMED,C'N'                                                        
         BE    VSLN20                                                           
         CLI   QMED,C'C'                                                        
         BE    VSLN20                                                           
*                                                                               
         LA    R0,C'R'                                                          
         CLI   QMED,C'R'                                                        
         BE    VSLN20                                                           
         CLI   QMED,C'X'                                                        
         BE    VSLN20                                                           
         DC    H'0'                                                             
*                                                                               
VSLN20   CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    VSLN40                                                           
         CLC   AGENCY,0(R1)        MATCH AGY                                    
         BNE   *+12                                                             
VSLN40   CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    VSLN60                                                           
*                                                                               
         BXLE  R1,RE,VSLN20        NEXT ENTRY                                   
         B     BADSLEN                                                          
*                                                                               
VSLN60   DS    0H                                                               
         AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,BYTE             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BE    BADSLEN             EXIT WITH CC SET                             
*                                                                               
VSLN90   MVC   SNVIDSLN,BYTE                                                    
*                                                                               
VSLNX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE CLICKS FIELD                                       
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALCLK   DS    0H                                                               
         NMOD1 0,**VCLK**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VCLK10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
         MVC   SNVIDCLK,SVDMELEM+SNVIDCLK-SNVIDELD                              
*                                                                               
         GOTO1 =A(DISCLK),DMCB,(RC),(R2),(R6),RR=RELO                           
         B     VCLKX                                                            
*                                                                               
VCLK10   DS    0H                                                               
         CLI   5(R2),4                                                          
         BNE   VCLK20                                                           
         CLC   =C'NONE',8(R2)                                                   
         BNE   VCLK20                                                           
         MVC   SNVIDCLK,=X'FFFFFFFF'                                            
         B     XIT                                                              
*                                                                               
VCLK20   LLC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(X'40',(R0))                                  
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
         CLC   =AL4(1),DMCB+4                                                   
         BH    INVLFLD                                                          
         CLC   =AL4(9999999),DMCB+4                                             
         BL    INVLFLD                                                          
*                                                                               
         MVC   SNVIDCLK,DMCB+4                                                  
*                                                                               
VCLKX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE RATINGS FIELD                                      
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALRAT   DS    0H                                                               
         NMOD1 0,**VRAT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VRAT10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
         MVC   SNVIDRAT,SVDMELEM+SNVIDRAT-SNVIDELD                              
*                                                                               
         GOTO1 =A(DISRAT),DMCB,(RC),(R2),(R6),RR=RELO                           
         B     VRATX                                                            
*                                                                               
VRAT10   DS    0H                                                               
         CLI   5(R2),4                                                          
         BNE   VRAT20                                                           
         CLC   =C'NONE',8(R2)                                                   
         BNE   VRAT20                                                           
         MVC   SNVIDRAT,=X'FFFFFFFF'                                            
         B     XIT                                                              
*                                                                               
VRAT20   LLC   R0,5(R2)                                                         
         LLC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
         CLC   =AL4(1),DMCB+4                                                   
         BH    INVLFLD                                                          
         CLC   =AL4(10000),DMCB+4                                               
         BL    INVLFLD                                                          
*                                                                               
         MVC   SNVIDRAT,DMCB+4                                                  
*                                                                               
VRATX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE IMPRESSIONS FIELD                                  
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALIMP   DS    0H                                                               
         NMOD1 0,**VIMP**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VIMP10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
         MVC   SNVIDIMP,SVDMELEM+SNVIDIMP-SNVIDELD                              
*                                                                               
         GOTO1 =A(DISIMP),DMCB,(RC),(R2),(R6),RR=RELO                           
         B     VIMPX                                                            
*                                                                               
VIMP10   DS    0H                                                               
         CLI   5(R2),4                                                          
         BNE   VIMP20                                                           
         CLC   =C'NONE',8(R2)                                                   
         BNE   VIMP20                                                           
         MVC   SNVIDIMP,=X'FFFFFFFF'                                            
         B     XIT                                                              
*                                                                               
VIMP20   LLC   R0,5(R2)                                                         
         CLI   TWODECIM,C'Y'                                                    
         BE    VIMP30                                                           
*                                                                               
* 1-DECIMAL VALIDATION HERE                                                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(1,8(R2)),(X'40',(R0))                              
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
         CLC   =AL4(1),DMCB+4                                                   
         BH    INVLFLD                                                          
         CLC   =AL4(99999),DMCB+4                                               
         BL    INVLFLD                                                          
*                                                                               
         B     VIMP40                                                           
*                                                                               
* 2-DECIMAL VALIDATION HERE                                                     
*                                                                               
VIMP30   DS    0H                                                               
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
         CLC   =AL4(1),DMCB+4                                                   
         BH    INVLFLD                                                          
         CLC   =AL4(999999),DMCB+4                                              
         BL    INVLFLD                                                          
*                                                                               
         OI    DMCB+4,X'40'        INDICATE 2-DECIMAL VALUE                     
*                                                                               
VIMP40   DS    0H                                                               
         MVC   SNVIDIMP,DMCB+4                                                  
*                                                                               
VIMPX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS THE FILM CODE OF THE DETAIL ELEMENT ON THE              
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISF1    NMOD1 0,**DF1 **                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         XC    DFBLOCK,DFBLOCK                                                  
         MVC   DFBFILM,SNVIDCML                                                 
         GOTO1 =A(DISFILM),DMCB,(RC),(R2),(R6),RR=RELO                          
*                                                                               
* CHECK FOR FILTERS                                                             
*                                                                               
         TM    FLTRFLG1,FF1DFILM   CHECK IF FILTER                              
         BZ    YES                                                              
*                                                                               
         CLC   FLTRFILM,8(R2)                                                   
         BNE   NO                                                               
*                                                                               
         B     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS THE 2ND FILM CODE OF THE DETAIL ELEMENT ON THE          
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISF2    NMOD1 0,**DF2 **                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         XC    DFBLOCK,DFBLOCK                                                  
         MVC   DFBFILM,SNVIDCM2                                                 
         GOTO1 =A(DISFILM),DMCB,(RC),(R2),(R6),RR=RELO                          
*                                                                               
         TM    FLTRFLG1,FF1DFLM2   CHECK IF FILTER                              
         BZ    YES                                                              
*                                                                               
         CLC   FLTRFLM2,8(R2)                                                   
         BNE   NO                                                               
*                                                                               
         B     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS THE FILM CODE OF THE DETAIL ELEMENT ON THE              
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3  BYTE 0     0,1 = FIRST FILM, 2 = PIGGYBACK              
*                       BYTE 1-3   A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISFILM  NMOD1 0,**DFLM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         BRAS  RE,XCFLD            CLEAR THE FIELD                              
*                                                                               
         CLI   DFBFILM,0           ANY COMMERCIAL CODE FOR THIS DETAIL?         
         BNE   DFLM10                                                           
*                                                                               
         MVC   8(4,R2),=CL4'NONE'                                               
         MVI   5(R2),4                                                          
         B     DFLMYES                                                          
*                                                                               
DFLM10   LA    R5,8(R2)            ADDR OF WHERE TO OUTPUT FILM NAME            
         TM    SNVIDCTL,SNVIDIFQ   IGNORE FILM?                                 
         BZ    *+14                NO                                           
         MVC   8(2,R2),=C'N-'      YES                                          
         LA    R5,2(R5)                                                         
*                                                                               
         LA    R1,DFBFILM          FILM CODE                                    
         ICM   R1,8,=X'01'                                                      
         BRAS  RE,GETFBUF                                                       
         BE    *+14                                                             
         MVC   GERROR,=AL2(NOCOMEL)                                             
         B     ERREXIT2                                                         
*                                                                               
         L     R1,FULL             A(FILM IN BUFFER)                            
         MVC   0(L'SNVCMAID,R5),BUFFCMCD-BUFFDSCT(R1) OUTPUT TO FIELD           
         MVC   DFQFILM,BUFFCMCD-BUFFDSCT(R1)          SAVE IN DFBLOCK           
*                                                                               
         LA    R1,DFQFILM                                                       
         LHI   R0,L'DFQFILM                                                     
         BRAS  RE,FINDLEN       FIND ACTUAL LENGTH OF INPUT                     
         STC   RF,DFQFLEN       SAVE FILM LENGTH                                
*                                                                               
         L     R1,FULL                                                          
         TM    (BUFFSTAT-BUFFDSCT)(R1),BUFFSINV  IS OUR FILM INVALID?           
         BZ    DFLMYES             NO                                           
*                                                                               
* YES - ADD "*F" AT THE END                                                     
*                                                                               
         ZIC   RF,DFQFLEN                                                       
         AR    R5,RF                                                            
         MVC   0(2,R5),=C'*F'                                                   
         LA    R5,2(R5)                                                         
*                                                                               
         SR    R5,R2               MINUS START OF INPUT                         
         SHI   R5,8                MINUS FIELD HEADER                           
         STC   R5,5(R2)            SET FIELD INPUT LENGTH                       
*                                                                               
DFLMYES  B     YES                                                              
*                                                                               
DFLMNO   B     NO                                                               
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VALF1    NMOD1 0,**VF1**                                                        
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         STCM  R2,15,SVR2                                                       
*                                                                               
         USING SNVIDELD,R6                                                      
         NI    SNVIDCTL,X'FF'-SNVIDIFQ   TAKE OFF IGNORE FILM FOR MATCH         
*                                                                               
         MVI   SNVIDCML,0          GET RID OF COMMERCIAL CODE                   
*                                                                               
* FILL IN THE VALFILM BLOCK                                                     
*                                                                               
         XC    VFBLOCK(VFBLKLQ),VFBLOCK                                         
         MVC   VFBPRD,CURRBPRD                                                  
         MVC   VFQPRD,CURRQPRD                                                  
         MVC   VFPREVF,SVDMELEM+SNVIDCML-SNVIDELD         PREVIOUS FILM         
         TM    SVDMELEM+SNVIDCTL-SNVIDELD,SNVIDIFQ                              
         BZ    *+8                                                              
         OI    VFPREVST,SNVIDIFQ                                                
*                                                                               
         GOTO1 =A(VALFILM),DMCB,(RC),(R2),(R6),RR=RELO                          
*                                                                               
         MVC   SNVIDCML,BFILM                                                   
         TM    VFSTAT,SNVIDIFQ     IGNORE FILM FOR MATCH?                       
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDIFQ                                                
*                                                                               
         MVC   TWOFILMS(L'BFILM),BFILM                                          
         TM    MISCFLG2,MF2NWFLM                                                
         BZ    *+8                                                              
         MVI   ADDFLAGS,C'Y'                                                    
         MVC   TWOLENS(L'TRFLEN),TRFLEN                                         
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VALF2    NMOD1 0,**VF2**                                                        
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         USING SNVIDELD,R6                                                      
*                                                                               
         MVI   SNVIDCM2,0          GET RID OF COMMERCIAL CODE                   
*                                                                               
* FILL IN THE VALFILM BLOCK                                                     
*                                                                               
         XC    VFBLOCK(VFBLKLQ),VFBLOCK                                         
         MVC   VFBPRD,CURRBPR2                                                  
         MVC   VFQPRD,CURRQPR2                                                  
         MVC   VFPREVF,SVDMELEM+SNVIDCM2-SNVIDELD         PREVIOUS FILM         
         TM    SVDMELEM+SNVIDCTL-SNVIDELD,SNVIDIFQ                              
         BZ    *+8                                                              
         OI    VFPREVST,SNVIDIFQ                                                
*                                                                               
         GOTO1 =A(VALFILM),DMCB,(RC),(R2),(R6),RR=RELO                          
*                                                                               
         MVC   SNVIDCM2,BFILM                                                   
         TM    VFSTAT,SNVIDIFQ     IGNORE FILM FOR MATCH?                       
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDIFQ                                                
*                                                                               
         MVC   TWOFILMS+1(L'BFILM),BFILM                                        
         TM    MISCFLG2,MF2NWFLM                                                
         BZ    *+8                                                              
         MVI   ADDFLAGS+1,C'Y'                                                  
         MVC   TWOLENS+1(L'TRFLEN),TRFLEN                                       
*                                                                               
VALF2X   DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE FILM CODE OF THE DETAIL LINE                       
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALFILM  NMOD1 0,**VFLM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         USING SNVIDELD,R6                                                      
*                                                                               
         MVI   GERROR1,0                                                        
         NI    MISCFLG3,X'FF'-MF3ADID-MF3HD                                     
         NI    MISCFLG2,X'FF'-MF2NWFLM                                          
*                                                                               
* EXTRACT DATA FROM PREVIOUS FILM (FOR COPYDOWN)                                
*                                                                               
         CLI   VFPREVF,X'00'       DO WE EVEN HAVE A PREVIOUS FILM?             
         BE    VFLM03              NO                                           
*                                                                               
         LA    R1,VFPREVF          PREVIOUS FILM CODE                           
         ICM   R1,8,=X'01'         INDICATE LOOKING FOR BINARY CODE             
         BRAS  RE,GETFBUF          LOOK IT UP IN THE BUFFER                     
         BE    *+6                                                              
         DC    H'0'                PREV FILM MUST BE IN BUFFER                  
*                                                                               
         L     R1,FULL             A(FILM IN BUFFER)                            
         MVC   SVQFILM,BUFFCMCD-BUFFDSCT(R1)          PREVIOUS FILMCODE         
*                                                                               
         LA    R1,SVQFILM                                                       
         LHI   R0,L'SVQFILM                                                     
         BRAS  RE,FINDLEN          FIND ACTUAL LENGTH OF INPUT                  
         STC   RF,SVQFLEN          PREVIOUS FILMCODE LENGTH                     
*                                                                               
VFLM03   DS    0H                                                               
         BRAS  RE,UCFLD            FIELD INPUT TO UPPERCASE                     
*                                                                               
         CLI   5(R2),4             CHECK IF IT IS "NONE" BY ITSELF              
         BNE   *+14                                                             
         CLC   =C'NONE',8(R2)                                                   
         BE    VFLMX                                                            
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
*                                                                               
* COPY ENTIRE INPUT FIELD TO FAKEFLD, IN CASE THERE'S NO COPYDOWN               
*                                                                               
         ZIC   RE,0(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLDH(0),0(R2)                                                
*                                                                               
* CHECK IF WE NEED TO COPY DOWN                                                 
*                                                                               
         CLI   P0TIPBS,0                                                        
         BE    *+12                                                             
         CLI   P0TIPBS,C'N'                                                     
         BNE   VFLM10                                                           
*                                                                               
         CLC   =C'N-',8(R2)                                                     
         BE    VFLM10                                                           
         CLI   VFPREVF,0           PREVIOUS FILM?                               
         BE    VFLM10              NOTHING, DO NOT COPY DOWN                    
         CLI   5(R2),8             INPUT LENGTH 8 OR MORE?                      
         BNL   VFLM10              YES - DO NOT COPY DOWN                       
         CLC   SVQFLEN,5(R2)       PREV FILM LENGTH < INPUT?                    
         BNH   VFLM10              YES - DO NOT COPY DOWN                       
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   RF,SVQFLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),SVQFILM  COPY PREV FILM TO FAKEFLD                    
         MVC   FAKEFLDH+5(1),SVQFLEN SET INPUT LENGTH                           
*                                                                               
* PREVIOUS FILM COPIED DOWN INTO FAKEFIELD                                      
* NOW SEE IF WE NEED TO OVERWRITE FILM'S END WITH FIELD INPUT                   
*                                                                               
         LLC   RF,SVQFLEN          PREV FILM'S LENGTH                           
         LA    RF,FAKEFLD(RF)      ADVANCE PAST FILMCODE                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)          LEN OF CURRENT INPUT                         
         BZ    VFLM08              IF NOTHING - DON'T OVERWRITE                 
*                                                                               
         SR    RF,RE               BACK UP BY THAT AMOUNT                       
* COPY CURRENT INPUT OVER THE END OF PREVIOUS FILMCODE                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
*                                                                               
* CHECK IF WE NEED TO COPY THE IGNORE FILM FLAG FROM PREV FILM                  
*                                                                               
VFLM08   DS    0H                                                               
         TM    VFPREVST,SNVIDIFQ   PREV FILM IGNORED FOR MATCH?                 
         BZ    *+8                 NO                                           
         OI    VFSTAT,SNVIDIFQ     YES, IGNORE THIS ONE TOO                     
*                                                                               
* HERE FAKEFLD CONTAINS THE KEYED IN OR COPIED-DOWN FILM,                       
*                                                                               
VFLM10   DS    0H                                                               
         STCM  R2,15,SVREG                                                      
         LA    R2,FAKEFLDH                                                      
         BRAS  RE,UCFLD            FIELD INPUT TO UPPERCASE                     
*                                                                               
         CLC   =C'N-',8(R2)        IGNORE FILM PREFIX ENTERED?                  
         BNE   VFLM20              NO - JUST VALIDATE THE FIELD                 
*                                                                               
* YES - SET IGNORE FLAG, AND REMOVE THE "N-" FOR VALIDATION                     
         OI    VFSTAT,SNVIDIFQ     SET IGNORE FILM FOR MATCH FLAG               
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),10(R2)                                                   
         BCTR  RE,0                                                             
         STC   RE,5(R2)                                                         
*                                                                               
         BRAS  RE,GFLEN                                                         
         LA    RF,8(R2,RF)                                                      
         SHI   RF,2                                                             
         MVC   0(2,RF),=C'  '                                                   
*                                                                               
VFLM20   DS    0H                                                               
         ICM   R2,15,SVREG                                                      
*                                                                               
         CLI   FAKEFLDH+5,X'00'    ANYTHING?                                    
         BE    VFLMX                                                            
*                                                                               
         GOTOR CHKFILM,DMCB,FAKEFLDH,(R6)                                       
         CLI   GERROR1,0                                                        
         BE    VFLM30                                                           
         CLI   TRFERR,0                                                         
         BE    ERREXIT                                                          
         CLI   P0TIAIF,C'Y'        TEST OK TO ACCEPT BAD FILMS                  
         BNE   ERREXIT                                                          
*                                                                               
VFLM30   DS    0H                                                               
         TM    MISCFLG2,MF2NWFLM   GOT A NEW FILM?                              
         BZ    *+8                                                              
         OI    MISCFLG2,MF2ADFLM   YES, GOT TO ADD IT LATER                     
*                                                                               
VFLMX    DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS THE ORIGINAL COST                                       
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISOCST  DS    0H                                                               
         NMOD1 0,**DOCS**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         BRAS  RE,XCFLD                                                         
*&&DO                                                                           
         ZIC   RF,0(R2)                                                         
         AHI   RF,-8               MINUS HEADER                                 
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    *+8                 NO                                           
         AHI   RF,-8               MINUS HEADER EXTENTION                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLANKS                                                   
*&&                                                                             
*                                                                               
         LA    R0,LENFOCST                                                      
         LA    R3,8(R2)                                                         
*                                                                               
         ICM   R1,15,SNVIDOR$                                                   
         BZ    DOCSYES                                                          
         CLC   SNVIDOR$,=X'FFFFFFFF'                                            
         BNE   *+6                                                              
         SR    R1,R1                                                            
         EDIT  (R1),(12,(R3)),2,ALIGN=LEFT,FLOAT=-                              
*                                                                               
DOCSYES  B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE COST OF THE DETAIL ELEMENT ON THE FIELD             
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISCOST  DS    0H                                                               
         NMOD1 0,**DCST**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R0,LENFCOST                                                      
         LA    R3,8(R2)                                                         
*                                                                               
         TM    SNVIDCT2,SNVIDBON   BONUS ?                                      
         BZ    *+12                                                             
         MVI   0(R3),C'#'                                                       
         B     DCSTYES                                                          
*                                                                               
         TM    SNVIDCTL,SNVIDICQ   IGNORE COST?                                 
         BZ    DCST05                                                           
         MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
*                                                                               
DCST05   TM    SNVIDCT2,SNVIDCTQ   CONTRACT TRADE ITEM?                         
         BZ    DCST10                                                           
         MVI   0(R3),C'T'                                                       
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
*                                                                               
DCST10   ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         EDIT  (R1),(12,(R3)),2,ALIGN=LEFT,FLOAT=-                              
*                                                                               
DCSTFCHK DS    0H                                                               
         TM    FLTRFLG1,FF1DCOST   CHECK IF FILTER                              
         BZ    DCSTYES                                                          
         ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   SEE IF NEGATIVE AMOUNT                       
         BZ    *+6                                                              
         LNR   R1,R1               NEGATE                                       
         CLM   R1,15,FLTRCOST                                                   
         BNE   DCSTNO                                                           
*                                                                               
DCSTYES  B     YES                                                              
*                                                                               
DCSTNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE COST OF THE DETAIL LINE                            
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALCOST  DS    0H                                                               
         NMOD1 0,**VCST**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VCST05                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
VCSTDSCT USING SNVIDELD,SVDMELEM                                                
*                                                                               
* SAVE ORIGINAL COST HERE                                                       
*                                                                               
         CLI   SNVIDLEN,SNVIDL3Q   ELEMENT LONG ENOUGH FOR ORIG COST?           
         BL    VCST03              NO - DON'T SAVE ORIG COST                    
         OC    SNVIDOR$,SNVIDOR$   ORIG. COST ALREADY SET?                      
         BNZ   VCST03              YES - DO NOT OVERWRITE IT                    
         TM    MISCFLG1,MF1ADDNG   ADDING DETAIL?                               
         BO    VCST03                                                           
*                                                                               
         ICM   R0,15,SNVIDCST      OLD COST                                     
         BNZ   *+14                                                             
         MVC   SNVIDOR$,=X'FFFFFFFF' ZERO, SAVE AS FF'S                         
         B     VCST02                                                           
*                                                                               
         TM    VCSTDSCT.SNVIDCTL,SNVIDNGQ                                       
         BZ    VCST02                                                           
         LNR   R0,R0                                                            
*                                                                               
VCST02   DS    0H                                                               
         STCM  R0,15,SNVIDOR$                                                   
*                                                                               
VCST03   DS    0H                                                               
         MVC   SNVIDCST,VCSTDSCT.SNVIDCST                                       
*                                                                               
         NI    SNVIDCTL,X'FF'-(SNVIDICQ+SNVIDMGQ+SNVIDNGQ)                      
         TM    VCSTDSCT.SNVIDCTL,SNVIDICQ     IGNORE COST?                      
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDICQ                                                
         TM    VCSTDSCT.SNVIDCTL,SNVIDMGQ     MAKEGOOD?                         
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDMGQ                                                
         TM    VCSTDSCT.SNVIDCTL,SNVIDNGQ     NEGATIVE?                         
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDNGQ                                                
         TM    VCSTDSCT.SNVIDCT2,SNVIDCTQ     CONTRACT TRADE ITEM?              
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDCTQ                                                
         TM    VCSTDSCT.SNVIDCT2,SNVIDBON     BONUS?                            
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDBON                                                
*                                                                               
         DROP  VCSTDSCT                                                         
*                                                                               
         GOTO1 =A(DISCOST),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VCSTX                                                            
*                                                                               
VCST05   DS    0H                                                               
         MVC   SVCTL,SNVIDCTL                                                   
         NI    SNVIDCTL,X'FF'-(SNVIDICQ+SNVIDMGQ+SNVIDNGQ)                      
         LA    R3,8(R2)                                                         
*                                                                               
* MY CODE FOR # SIGN                                                            
         CLI   0(R3),C'#'                                                       
         BE    *+12                                                             
         NI    SNVIDCT2,X'FF'-SNVIDBON                                          
         B     VCST10                                                           
* IF # SIGN THERE LOOK FOR INTEGRATION                                          
         LH    RE,DCURRENT         GET TO BEGIN OF ROW                          
         AR    RE,RA                                                            
*                                  1ST FIELD                                    
         ZICM  R1,2(RE),2          CHECK IF STARTS IN 2ND COL                   
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         CHI   R0,1                                                             
         BE    *+6                                                              
         DC    H'0'                IF NOT INVALID                               
*                                                                               
VCST6    TM    1(RE),X'02'         FIELD HAS EXTENSION ?                        
         BZ    VCST7               IF NO EXTENSION, NXT FIELD                   
         ZIC   R1,0(RE)                                                         
         AR    R1,RE                                                            
         SHI   R1,8                                                             
         CLI   0(R1),FLDNINTG      IS IT INTEGRATION ?                          
         BNE   VCST7                                                            
*                                                                               
* SAVE ORIGINAL COST HERE                                                       
*                                                                               
         CLI   SNVIDLEN,SNVIDL3Q   ELEMENT LONG ENOUGH FOR ORIG COST?           
         BL    VCST6B              NO - DON'T SAVE ORIG COST                    
         OC    SNVIDOR$,SNVIDOR$   ORIG. COST ALREADY SET?                      
         BNZ   VCST6B              YES - DO NOT OVERWRITE IT                    
         TM    MISCFLG1,MF1ADDNG   ADDING DETAIL?                               
         BO    VCST6B                                                           
*                                                                               
         ICM   R0,15,SNVIDCST      PREVIOIUS COST, IS IT ZERO?                  
         BNZ   *+14                NO                                           
         MVC   SNVIDOR$,=X'FFFFFFFF' YES - SAVE FF'S                            
         B     VCST6A                                                           
*                                                                               
         TM    SVCTL,SNVIDNGQ      NEGATIVE COST?                               
         BZ    VCST6A                                                           
         LNR   R0,R0                                                            
*                                                                               
VCST6A   DS    0H                                                               
         STCM  R0,15,SNVIDOR$                                                   
*                                                                               
VCST6B   XC    SNVIDCST,SNVIDCST   YES, CLEAR COST                              
         OI    SNVIDCT2,SNVIDBON   TURN ON BONUS BIT                            
         B     VCSTX                                                            
*                                                                               
VCST7    ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
*                                                                               
         ZICM  R1,2(RE),2          CHECK IF STARTS IN 2ND COL                   
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         CHI   R0,1                                                             
         BE    INVLFLD             LIN FINISHED, NO INTEG FOUND                 
*                                                                               
         B     VCST6                                                            
*                                                                               
VCST10   ZIC   R0,5(R2)                                                         
*                                                                               
VCST20   CLI   0(R3),C'N'          IGNORE COST FOR MATCH?                       
         BNE   VCST25                                                           
         TM    SNVIDCTL,SNVIDICQ   ALREADY ON?                                  
         BNZ   INVLFLD                                                          
         OI    SNVIDCTL,SNVIDICQ   YES                                          
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
         B     VCST20                                                           
*                                                                               
VCST25   CLI   0(R3),C'T'          CONTRACT TRADE ITEM?                         
         BNE   VCST30                                                           
         TM    SNVIDCT2,SNVIDCTQ   ALREADY ON?                                  
         BNZ   INVLFLD                                                          
         OI    SNVIDCT2,SNVIDCTQ   YES                                          
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
         B     VCST20                                                           
*                                                                               
VCST30   XC    WORK,WORK           ANY PERCENTAGE ADJUSTMENT?                   
         MVC   WORK+1(LENFCOST),0(R3)                                           
         XC    SAVPCT,SAVPCT       NO PERCENTAGE YET                            
         MVI   SAVPCTL,0                                                        
         LA    RF,WORK                                                          
         AR    RF,R0                                                            
         CLI   0(RF),C'%'                                                       
         BNE   VCST50              NO                                           
         LR    RE,RF                                                            
*                                                                               
VCST35   CLI   0(RE),C' '          NO PCT IF NO - OR +                          
         BNH   VCST50                                                           
         CLI   0(RE),C'-'                                                       
         BE    VCST40                                                           
         CLI   0(RE),C'+'                                                       
         BE    VCST40                                                           
         BCT   RE,VCST35                                                        
*                                                                               
VCST40   MVC   SAVPCT,0(RE)        SAVE PCT ADJ EXPRESSION                      
         LA    R1,1(RF)                                                         
         SR    R1,RE                                                            
         STC   R1,SAVPCTL          LENGTH OF PCT ADJ EXPRESSION                 
         B     VCST55                                                           
*                                                                               
VCST50   TM    4(R2),X'20'         FIELD PREVIOUSLY VALIDATED?                  
         BNZ   VCST55                                                           
         MVC   1(L'SAVPCT,RF),SAVPCT                                            
         ZIC   R1,SAVPCTL                                                       
         AR    R0,R1                                                            
*                                                                               
VCST55   GOTO1 CASHVAL,DMCB,WORK+1,(R0)                                         
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
* SAVE ORIGINAL COST HERE.                                                      
*                                                                               
         CLI   SNVIDLEN,SNVIDL3Q   ELEMENT LONG ENOUGH FOR ORIG COST?           
         BL    VCST55B             NO - DON'T SAVE ORIG COST                    
         OC    SNVIDOR$,SNVIDOR$   ORIG. COST ALREADY SET?                      
         BNZ   VCST55B             YES - DO NOT OVERWRITE IT                    
         TM    MISCFLG1,MF1ADDNG   ADDING DETAIL?                               
         BO    VCST55B                                                          
*                                                                               
         L     RF,DMCB+4           NEW COST                                     
         LPR   RF,RF                                                            
         ICM   R0,15,SNVIDCST      PREVIOUS COST                                
         CR    RF,R0               SAME AS NEW?                                 
         BE    VCST55B             YES, DON'T SAVE IT                           
         LTR   R0,R0               PREVIOUS COST                                
         BNZ   VCST55A             ZERO?                                        
         MVC   SNVIDOR$,=X'FFFFFFFF' YES, SAVE AS FF'S                          
         B     VCST55B                                                          
*                                                                               
VCST55A  TM    SVCTL,SNVIDNGQ      NEGATIVE COST?                               
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         STCM  R0,15,SNVIDOR$                                                   
*                                                                               
VCST55B  L     R1,DMCB+4           NEGATIVE AMOUNT?                             
         LTR   R1,R1                                                            
         BNM   *+10                                                             
         OI    SNVIDCTL,SNVIDNGQ   YES, SET THE BIT                             
         LPR   R1,R1                                                            
         STCM  R1,15,SNVIDCST                                                   
*                                                                               
VCSTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE PRODUCT(S) OF THE DETAIL ELEMENT ON THE             
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISPROD  DS    0H                                                               
         NMOD1 0,**DPRD**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
*                                                                               
         CLI   SNVIDPRD,0                                                       
         BNE   DPRD10                                                           
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   DPRD10                                                           
         CLC   SNVIDAP1,BLANKS                                                  
         BNH   DPRD10                                                           
         MVC   DMCB+1(3),SNVIDAP1                                               
         B     DPRD20                                                           
*                                                                               
DPRD10   GOTO1 FINDQPRD,DMCB,(SNVIDPRD,0)                                       
         BE    *+14                                                             
         MVC   8(3,R2),=C'???'                                                  
         B     *+10                                                             
DPRD20   MVC   8(L'QPRD,R2),DMCB+1                                              
         MVC   QPRD,DMCB+1                                                      
*                                                                               
         CLI   SNVIDPR2,0          ANY PIGGYBACK?                               
         BNE   DPRD30              NO, DONE DISPLAYING PRODUCT                  
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   DPRDFCHK                                                         
         CLC   SNVIDAP2,BLANKS                                                  
         BNH   DPRDFCHK                                                         
         MVI   8+L'QPRD(R2),C'-'                                                
         MVC   DMCB+1(3),SNVIDAP2                                               
         B     DPRD40                                                           
*                                                                               
DPRD30   MVI   8+L'QPRD(R2),C'-'                                                
         GOTO1 FINDQPRD,DMCB,(SNVIDPR2,0)                                       
         BE    *+14                                                             
         MVC   8+L'QPRD+1(3,R2),=C'???'                                         
         B     *+10                                                             
DPRD40   MVC   8+L'QPRD+1(L'QPRD,R2),DMCB+1                                     
         MVC   QPRD2,DMCB+1                                                     
*                                                                               
DPRDFCHK TM    FLTRFLG1,FF1DPROD   CHECK IF FILTER                              
         BZ    DPRDYES                                                          
         CLC   FLTRPROD,QPRD                                                    
         BNE   DPRDNO                                                           
*                                                                               
DPRDYES  B     YES                                                              
*                                                                               
DPRDNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PRODUCT(S) OF THE DETAIL LINE                      
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALPROD  DS    0H                                                               
         NMOD1 0,**VPRD**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VPRD10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
VPRDDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDPRD(L'SNVIDPRD+L'SNVIDPR2),VPRDDSCT.SNVIDPRD                
****                                                                            
         MVC   CURRBPRD,SNVIDPRD                                                
         MVC   CURRBPR2,SNVIDPR2                                                
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   VPRD05                                                           
         CLC   VPRDDSCT.SNVIDAP1(L'SNVIDAP1+L'SNVIDAP2),BLANKS                  
         BNH   VPRD05                                                           
         MVC   SNVIDAP1(L'SNVIDAP1+L'SNVIDAP2),VPRDDSCT.SNVIDAP1                
         MVC   CURRQPRD,SNVIDAP1                                                
         MVC   CURRQPR2,SNVIDAP2                                                
         B     VPRDX                                                            
****                                                                            
         DROP  VPRDDSCT                                                         
*                                                                               
VPRD05   GOTO1 =A(DISPROD),DMCB,(RC),(R2),(R6),RR=RELO                          
         MVC   CURRQPRD,QPRD                                                    
         MVC   CURRQPR2,QPRD2                                                   
         B     VPRDX                                                            
*                                                                               
VPRD10   XCEFL BLOCK,480           CLEAR THE BLOCK                              
*                                                                               
         ZIC   RE,5(R2)                                                         
         LA    RF,8(R2)                                                         
VPRD15   CLI   0(RF),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         BCT   RE,VPRD15                                                        
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'82',BLOCK),C',=-='                          
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
*                                                                               
* 05/01/09 - CHANGE TO ALLOW 1 FILMCODE AND 2 PRODUCTS                          
*            AT CANADA'S REQUEST                                                
*                                                                               
         B     VPRD15B                                                          
*                                                                               
* SEE IF WE HAVE 2ND FILM COLUMN.  IF YES - FORCE TO ENTER PIGGY PRD            
         LA    R1,SVFLDLST                                                      
         LHI   R0,MAXFLDEQ                                                      
*                                                                               
         CLI   0(R1),FLDNPFLM                                                   
         BE    VPRD15A                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
* ONLY ONE FILM COLUMN HERE                                                     
         CLI   DMCB+4,1            ONE PRODUCT ENTERED?                         
         BE    VPRD15B             YES - WE'RE OK                               
         B     INVLFLD             NO - CAN'T HAVE PIGGYBACKS                   
*                                                                               
* HAVE 2ND FILM COLUMN HERE                                                     
VPRD15A  DS    0H                                                               
         CLI   DMCB+4,2            TWO PRODUCTS ENTERED?                        
         BE    VPRD15B             YES - WE'RE OK                               
         MVC   GERROR,=AL2(MISSPIGQ) NO - 2ND PRODUCT MISSING ERROR             
         B     ERREXIT2                                                         
*                                                                               
VPRD15B  DS    0H                                                               
         LA    R3,BLOCK            R3 = A(1ST PRODUCT)                          
         CLI   0(R3),0                                                          
         BE    BADPROD                                                          
         CLI   1(R3),0             PRODUCT CAN'T HAVE SUB-FIELDS                
         BNE   BADPROD                                                          
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
*                                                                               
         MVC   FAKEFLDH,0(R2)                                                   
         MVC   FAKEFLDH+5(1),0(R3)                                              
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
*                                                                               
         CLC   =CL3'AAA',FAKEFLD                                                
         BNE   VPRD16                                                           
         MVC   GERROR,=AL2(AAANOGO)                                             
         B     ERREXIT2                                                         
*                                                                               
VPRD16   DS    0H          POL CHECK TEMPORARILY DISABLED                       
*        CLC   =CL3'POL',FAKEFLD                                                
*        BNE   VPRD17                                                           
*        MVC   GERROR,=AL2(POLNOGO)                                             
*        B     ERREXIT2                                                         
*                                                                               
VPRD17   LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD                                                          
         MVC   CURRBPRD,BPRD                                                    
         MVC   CURRQPRD,QPRD                                                    
         MVC   SNVIDPRD,BPRD       SAVE PRIMARY PRODUCT CODE HERE FIRST         
*                                                                               
*        CLI   NETPAKSW,C'Y'       NETPAK?                                      
*        BNE   VPRD18                                                           
         MVC   SNVIDAP1,QPRD       YES, SAVE THE EBCDIC PRODUCT                 
         CLI   SNVIDLEN,SNVIDL3Q   ELEMENT BEFORE ALPHA PRODS STORED?           
         BNL   VPRD18                                                           
         MVI   SNVIDLEN,SNVIDL3Q   YES, CONVERT TO NEW LENGTH                   
*                                                                               
VPRD18   LA    R3,32(R3)           R3 = A(PIGGYBACK PRODUCT) IF ANY             
         CLI   0(R3),0                                                          
         BNE   VPRD19                                                           
         MVI   SNVIDPR2,0          NO PIGGYBACK PRODUCT                         
*        CLI   NETPAKSW,C'Y'       NETPAK?                                      
*        BNE   VPRD20                                                           
         MVC   SNVIDAP2,BLANKS                                                  
         B     VPRD20                                                           
*                                                                               
VPRD19   L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
*                                                                               
         CLI   1(R3),0             PRODUCT CAN'T HAVE SUB-FIELDS                
         BNE   BADPROD2                                                         
         ZICM  R0,FAKEFLDH+2,2     CALCULATE COL OF PIGGY                       
         ZIC   R1,4(R3)                                                         
         AR    R0,R1                                                            
         STCM  R0,3,FAKEFLDH+2                                                  
         MVC   FAKEFLDH+5(1),0(R3)    GIVE IT THE PROPER LENGTH                 
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   CURRBPR2,BPRD                                                    
         MVC   CURRQPR2,QPRD                                                    
         MVC   SNVIDPR2,BPRD                                                    
*        CLI   NETPAKSW,C'Y'       NETPAK?                                      
*        BNE   VPRD20                                                           
         MVC   SNVIDAP2,QPRD                                                    
*                                                                               
VPRD20   L     R1,ATIOB            TAKE OFF ERROR MESSAGE CURSOR                
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
VPRDX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE ESTIMATE OF THE DETAIL ELEMENT ON THE FIELD         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISESTM  DS    0H                                                               
         NMOD1 0,**DEST**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   SNVIDEST,0                                                       
         BNE   DEST10                                                           
         XC    8(LENFESTM,R2),8(R2)                                             
         MVI   5(R2),0                                                          
         B     DESTFCHK                                                         
*                                                                               
DEST10   EDIT  (B1,SNVIDEST),(3,8(R2)),FILL=0                                   
         OI    4(R2),X'08'         VALID NUMERIC                                
         MVI   5(R2),3                                                          
*                                                                               
DESTFCHK TM    FLTRFLG1,FF1DESTM   CHECK IF FILTER                              
         BZ    DESTYES                                                          
         CLC   FLTRESTM,SNVIDEST                                                
         BNE   DESTNO                                                           
*                                                                               
DESTYES  B     YES                                                              
*                                                                               
DESTNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE MATCHMAKER ESTIMATE                                 
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISMEST  DS    0H                                                               
         NMOD1 0,**DMST**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R5,SNVIDBES                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    R5,SNVIDUES                                                      
*                                                                               
         CLI   0(R5),0                                                          
         BNE   DMES10                                                           
         XC    8(LENFESTM,R2),8(R2)                                             
         MVI   5(R2),0                                                          
         B     YES                                                              
*                                                                               
DMES10   EDIT  (B1,0(R5)),(3,8(R2)),FILL=0                                      
         OI    4(R2),X'08'         VALID NUMERIC                                
         MVI   5(R2),3                                                          
*                                                                               
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE ESTIMATE OF THE DETAIL LINE                        
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALESTM  DS    0H                                                               
         NMOD1 0,**VEST**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VEST10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM   DO WE HAVE A PREVIOUS LINE?                  
         BNZ   VEST05                                                           
         CLI   PI2XERQ,C'Y'        IS ESTIMATE REQUIRED?                        
         BE    MISSFLD             YES                                          
         MVI   SNVIDEST,0          NO, STORE IT AS X'00'                        
         B     VESTX                                                            
*                                                                               
VEST05   GOTO1 =A(DISESTM),DMCB,(RC),(R2),SVDMELEM,RR=RELO                      
         CLI   5(R2),0             ANYTHING FROM PREVIOUS LINE?                 
         BNE   VEST10                                                           
         CLI   PI2XERQ,C'Y'        IS ESTIMATE REQUIRED?                        
         BE    MISSFLD             YES                                          
         MVI   SNVIDEST,0          NO, STORE IT AS X'00'                        
         B     VESTX                                                            
*                                                                               
VEST10   CLI   5(R2),1             DON'T COPY PREVIOUS LINE?                    
         BNE   VEST15                                                           
         CLI   8(R2),C'.'                                                       
         BNE   VEST15                                                           
         CLI   PI2XERQ,C'Y'        IS ESTIMATE REQUIRED?                        
         BE    MISSFLD             YES                                          
         MVI   SNVIDEST,0          NO, STORE ESTIMATE AS X'00'                  
         B     VESTX                                                            
*                                                                               
VEST15   MVC   QPRD,CURRQPRD       MAKE SURE ESTIMATE EXISTS FOR BOTH           
         GOTO1 VALIEST                 PRODUCTS                                 
*                                                                               
         CLC   ESTSTRT,PEREDATE    MAKE SURE PERIOD IS A SUBSET OF              
         BH    *+14                    ESTIMATE PERIOD                          
         CLC   ESTEND,PERSDATE                                                  
         BNL   *+12                                                             
*                                                                               
         CLI   PI2YCKEE,C'Y'       DO WE NEED TO CHECK ESTIMATE DATES           
         BNE   BADDTEST                                                         
*                                                                               
         MVC   INTESTSD,ESTSTRT                                                 
         MVC   INTESTED,ESTEND                                                  
*                                                                               
         CLC   CURRQPR2,BLANKS     NOTHING IF NOT HIGHER THAN SPACES            
         BNH   VEST20                                                           
         MVC   QPRD,CURRQPR2                                                    
         GOTO1 VALIEST                                                          
*                                                                               
         CLC   ESTSTRT,PEREDATE    MAKE SURE PERIOD IS A SUBSET OF              
         BH    *+14                    ESTIMATE PERIOD                          
         CLC   ESTEND,PERSDATE                                                  
         BNL   *+12                                                             
*                                                                               
         CLI   PI2YCKEE,C'Y'       DO WE NEED TO CHECK ESTIMATE DATES           
         BNE   BADDTEST                                                         
*                                                                               
         CLC   INTESTSD,ESTSTRT    CALCULATE INTERSECTION OF ESTIMATE           
         BH    *+10                  PERIODS                                    
         MVC   INTESTSD,ESTSTRT                                                 
         CLC   INTESTED,ESTEND                                                  
         BL    *+10                                                             
         MVC   INTESTED,ESTEND                                                  
*                                                                               
         CLC   INTESTSD,INTESTED                                                
         BH    BADDTEST                                                         
*                                                                               
VEST20   LA    R1,SVFLDLST                                                      
         ZIC   R0,SVFLDCNT                                                      
VEST25   CLI   0(R1),FLDNDATE      DATE BEFORE ESTIMATE FIELD?                  
         BE    VEST40              YES                                          
         CLI   0(R1),FLDNESTM      ESTIMATE BEFORE DATE?                        
         BE    VEST50              YES                                          
         BCT   R0,VEST25                                                        
*                                                                               
VEST40   ZIC   R1,SNVIDDAY                                                      
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,PERSDATE,WORK                                         
*                                                                               
         CLC   WORK(6),INTESTSD    DETAIL DATE SHOULD BE INSIDE THE             
         BL    *+14                  INTERSECTION OF ESTIMATE PERIODS           
         CLC   WORK(6),INTESTED                                                 
         BNH   VEST50                                                           
         CLI   PI2YCKEE,C'Y'       CAN THE DATE BE AFTER EST END?               
         BNE   BADDTEST            NO, WE'RE CHECKING BASED ON I2Y PROF         
*                                                                               
VEST50   MVC   SNVIDEST,BEST                                                    
*                                                                               
VESTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE RESPONSE COUNT OF THE DETAIL ELEMENT ON THE         
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISRCNT  DS    0H                                                               
         NMOD1 0,**DRCT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         ZICM  R1,SNVIDRSP,3                                                    
         EDIT  (R1),(8,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
*                                                                               
DRCTFCHK TM    FLTRFLG1,FF1DRCNT   CHECK IF FILTER                              
         BZ    DRCTYES                                                          
         CLC   FLTRRCNT,SNVIDRSP                                                
         BNE   DRCTNO                                                           
*                                                                               
DRCTYES  B     YES                                                              
*                                                                               
DRCTNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE RESPONSE COUNT OF THE DETAIL LINE                  
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALRCNT  DS    0H                                                               
         NMOD1 0,**VRCT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VRCT10                                                           
         OC    SVDMELEM,SVDMELEM                                                
         BE    MISSFLD                                                          
*                                                                               
VRCTDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDRSP,VRCTDSCT.SNVIDRSP                                       
         GOTO1 =A(DISRCNT),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VRCTX                                                            
         DROP  VRCTDSCT                                                         
*                                                                               
VRCT10   ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
         SR    R0,R0                                                            
         L     R1,DMCB+4           IN NUMBER OF HUNDREDTHS                      
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         BNZ   INVLFLD             SHOULDN'T HAVE ANY HUNDREDTHS                
*                                                                               
         C     R1,=X'00FFFFFF'     CAN'T HAVE MORE THAN 3 BYTES                 
         BH    INVLFLD                                                          
*                                                                               
         STCM  R1,7,SNVIDRSP                                                    
*                                                                               
VRCTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE INTEGRATION OF THE DETAIL ELEMENT ON THE            
* FIELD                                                                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISINTG  DS    0H                                                               
         NMOD1 0,**DINT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R0,LENFINTG                                                      
         LA    R3,8(R2)                                                         
*                                                                               
         TM    SNVIDCT2,SNVIDIIQ   IGNORE COST?                                 
         BZ    DINT10                                                           
         MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
*                                                                               
DINT10   ICM   R1,15,SNVIDINT                                                   
         TM    SNVIDCT2,SNVIDNIQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         CHI   R0,LENFINTG                                                      
         BL    DINT15                                                           
         EDIT  (R1),(12,(R3)),2,ALIGN=LEFT,FLOAT=-                              
         B     DINTFCHK                                                         
DINT15   EDIT  (R1),(11,(R3)),2,ALIGN=LEFT,FLOAT=-                              
*                                                                               
DINTFCHK DS    0H                                                               
         TM    FLTRFLG2,FF2INTEG   CHECK IF FILTER                              
         BZ    DINTYES                                                          
         ICM   R1,15,SNVIDINT                                                   
         TM    SNVIDCT2,SNVIDNIQ   SEE IF NEGATIVE AMOUNT                       
         BZ    *+6                                                              
         LNR   R1,R1               NEGATE                                       
         CLM   R1,15,FLTRINTG                                                   
         BNE   DINTNO                                                           
*                                                                               
DINTYES  B     YES                                                              
*                                                                               
DINTNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE INTEGRATION OF THE DETAIL LINE                     
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALINTG  DS    0H                                                               
         NMOD1 0,**VINT**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VINT10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
VINTDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDINT,VINTDSCT.SNVIDINT                                       
*                                                                               
         NI    SNVIDCT2,X'FF'-(SNVIDIIQ+SNVIDNIQ)                               
         TM    VINTDSCT.SNVIDCT2,SNVIDIIQ     IGNORE COST?                      
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDIIQ                                                
         TM    VINTDSCT.SNVIDCT2,SNVIDNIQ     NEGATIVE?                         
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDNIQ                                                
         DROP  VINTDSCT                                                         
         GOTO1 =A(DISINTG),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VINTX                                                            
*                                                                               
VINT10   ZIC   R0,5(R2)                                                         
         LA    R3,8(R2)                                                         
         NI    SNVIDCT2,X'FF'-(SNVIDIIQ+SNVIDNIQ)                               
*                                                                               
VINT20   CLI   0(R3),C'N'          IGNORE COST FOR MATCH?                       
         BNE   VINT30                                                           
         TM    SNVIDCT2,SNVIDIIQ   ALREADY ON?                                  
         BNZ   INVLFLD                                                          
         OI    SNVIDCT2,SNVIDIIQ   YES                                          
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
         B     VINT20                                                           
*                                                                               
VINT30   XC    WORK,WORK           ANY PERCENTAGE ADJUSTMENT?                   
         MVC   WORK+1(LENFINTG),0(R3)                                           
         XC    SAVPCT,SAVPCT       NO PERCENTAGE YET                            
         MVI   SAVPCTL,0                                                        
         LA    RF,WORK                                                          
         AR    RF,R0                                                            
         CLI   0(RF),C'%'                                                       
         BNE   VINT50              NO                                           
         LR    RE,RF                                                            
*                                                                               
VINT35   CLI   0(RE),C' '          NO PCT IF NO - OR +                          
         BNH   VINT50                                                           
         CLI   0(RE),C'-'                                                       
         BE    VINT40                                                           
         CLI   0(RE),C'+'                                                       
         BE    VINT40                                                           
         BCT   RE,VINT35                                                        
*                                                                               
VINT40   MVC   SAVPCT,0(RE)        SAVE PCT ADJ EXPRESSION                      
         LA    R1,1(RF)                                                         
         SR    R1,RE                                                            
         STC   R1,SAVPCTL          LENGTH OF PCT ADJ EXPRESSION                 
         B     VINT55                                                           
*                                                                               
VINT50   TM    4(R2),X'20'         FIELD PREVIOUSLY VALIDATED?                  
         BNZ   VINT55                                                           
         MVC   1(L'SAVPCT,RF),SAVPCT                                            
         ZIC   R1,SAVPCTL                                                       
         AR    R0,R1                                                            
*                                                                               
VINT55   GOTO1 CASHVAL,DMCB,WORK+1,(R0)                                         
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
         L     R1,DMCB+4           NEGATIVE AMOUNT?                             
         LTR   R1,R1                                                            
         BNM   *+10                                                             
         OI    SNVIDCT2,SNVIDNIQ   YES, SET THE BIT                             
         LPR   R1,R1                                                            
         STCM  R1,15,SNVIDINT                                                   
*                                                                               
VINTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE PROGRAM OF THE DETAIL ELEMENT ON THE FIELD          
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISPROG  DS    0H                                                               
         NMOD1 0,**DPRG**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
DPRGFCHK DS    0H                                                               
*                                                                               
DPRGYES  B     YES                                                              
*                                                                               
DPRGNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PROGRAM OF THE DETAIL LINE                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALPROG  DS    0H                                                               
         NMOD1 0,**VPRG**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
*                                                                               
VPRGX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE NETWORK OF THE DETAIL ELEMENT ON THE FIELD          
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISNTWK  DS    0H                                                               
         NMOD1 0,**DNWK**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         NI    BSTA+2,X'80'                                                     
         MVC   DUB(L'BMKTSTA),BMKTSTA                                           
         OC    DUB+L'BMKT+2(1),SNVIDNWK   OR IN THE NETWORK                     
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
*                                                                               
         MVC   8(3,R2),WORK+4+5    SHOW THE NETWORK                             
*                                                                               
DNWKFCHK TM    FLTRFLG2,FF2DNTWK   CHECK IF FILTER                              
         BZ    DNWKYES                                                          
         CLC   FLTRNTWK,8(R2)                                                   
         BNE   DNWKNO                                                           
*                                                                               
DNWKYES  B     YES                                                              
*                                                                               
DNWKNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE NETWORK OF THE DETAIL LINE                         
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALNTWK  DS    0H                                                               
         NMOD1 0,**VNWK**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0             NEED A NETWORK                               
         BNE   VNWK10                                                           
         OC    SVDMELEM,SVDMELEM                                                
         BZ    MISSFLD                                                          
*                                                                               
VNWKDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDNWK,VNWKDSCT.SNVIDNWK                                       
         GOTO1 =A(DISNTWK),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VNWKX                                                            
*                                                                               
VNWK10   CLI   5(R2),3             CAN'T BE MORE THAN 3 CHARACTERS              
         BH    INVLFLD                                                          
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLDH(8),0(R2)   COPY THE FIELD HEADER INFO                   
         MVC   FAKEFLD(4),QSTA        CABLE ARE 4 BYTE NUMERIC                  
         MVI   FAKEFLD+4,C'/'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD+4+1(0),8(R2)                                             
         OC    FAKEFLD+4+1(3),BLANKS                                            
         MVI   FAKEFLDH+5,8                                                     
         CLI   5(R2),3                                                          
         BE    *+8                                                              
         MVI   FAKEFLDH+5,7                                                     
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         LR    R0,R2                                                            
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALISTA                                                          
         LR    R2,R0                                                            
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*&&DO                                                                           
         GOTO1 MSPACK,DMCB,=C'0000',FAKEFLD,DUB                                 
         BNE   NETMASTR                                                         
*                                                                               
         NI    DUB+L'BMKT+2,X'7F'  ISOLATE NETWORK BYTE                         
*                                                                               
         LA    RE,BSYSNWKS         MAKE SURE NETWORK IS FOR STATION             
         ZIC   R0,DUB+L'BMKT+2                                                  
         SH    R0,=H'1'            WE START FROM 1 NOT 0 IN SPCBLLST            
         SRL   R0,3                                                             
         AR    RE,R0               RE = A(BYTE FOR THE NETWORK)                 
*                                                                               
         CLI   0(RE),0             ANY BITS ON FOR THIS BYTE?                   
         BE    NETMASTR            NO, NETWORK NOT IN THIS STATION              
*                                                                               
         MVC   BYTE,DUB+L'BMKT+2   BYTE = WHICH BIT TO TEST IF ON               
         NI    BYTE,X'07'                                                       
         CLI   BYTE,0                                                           
         BNE   *+8                                                              
         MVI   BYTE,8                                                           
*                                                                               
         ZIC   RF,BYTE             CREATE TESTER BYTE                           
         BCTR  RF,0                                                             
         LA    R0,X'80'                                                         
         LTR   RF,RF                                                            
         BZ    VNWK60                                                           
VNWK50   SRL   R0,1                                                             
         BCT   RF,VNWK50                                                        
VNWK60   STC   R0,BYTE                                                          
*                                                                               
         NC    BYTE,0(RE)          NETWORK IN THIS STATION?                     
         BZ    NETMASTR            NO                                           
*&&                                                                             
         MVC   SNVIDNWK,BNTWK      SAVE NETWORK BYTE IN ELEMENT                 
*                                                                               
VNWKX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS IF THE DETAIL IS MAKEGOOD                               
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
DISMKGD  DS    0H                                                               
         NMOD1 0,**DMGD**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         MVI   8(R2),C'N'                                                       
         TM    SNVIDCTL,SNVIDMGQ   MAKEGOOD?                                    
         BZ    DMKGDFCK                                                         
         MVI   8(R2),C'Y'          YES                                          
*                                                                               
DMKGDFCK DS    0H                                                               
*                                                                               
DMKGDYES B     YES                                                              
*                                                                               
DMKGDNO  B     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES IF THE DETAIL IS MAKEGOOD                              
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALMKGD  DS    0H                                                               
         NMOD1 0,**VMGD**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         NI    SNVIDCTL,X'FF'-SNVIDMGQ                                          
         CLI   5(R2),0             ANY INPUT IN MAKEGOOD COLUMN?                
         BNE   VMKGD10             YES                                          
*                                                                               
         OC    SVDMELEM,SVDMELEM   NONE, IF NOTHING BEFORE                      
         BZ    VMKGDX                    THEN NO MAKEGOOD                       
*                                                                               
VMKGDSCT USING SNVIDELD,SVDMELEM                                                
         TM    VMKGDSCT.SNVIDCTL,SNVIDMGQ   MAKEGOOD BEFORE?                    
         BZ    VMKGDX                       NO                                  
         OI    SNVIDCTL,SNVIDMGQ            YES                                 
         B     VMKGDX                                                           
*                                                                               
VMKGD10  CLI   8(R2),C'Y'          MAKEGOOD?                                    
         BNE   VMKGD20                                                          
         OI    SNVIDCTL,SNVIDMGQ   YES                                          
         B     VMKGDX                                                           
*                                                                               
VMKGD20  CLI   8(R2),C'N'          FORCE MAKEGOOD OFF?                          
         BNE   INVLFLD             NO, ERROR                                    
*                                                                               
VMKGDX   B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS IF THE DETAIL HAS BILLBOARD                             
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISBILB  DS    0H                                                               
         NMOD1 0,**DBIL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         MVI   8(R2),C'N'                                                       
         TM    SNVIDCTL,SNVIDBLQ   BILLBOARD?                                   
         BZ    DBILBFCK                                                         
         MVI   8(R2),C'Y'          YES                                          
*                                                                               
DBILBFCK TM    FLTRFLG2,FF2BLLBD   CHECK IF FILTER                              
         BZ    DBILBYES                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   DBILBNO                                                          
*                                                                               
DBILBYES B     YES                                                              
*                                                                               
DBILBNO  B     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES IF THE DETAIL HAS BILLBOARD                            
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALBILB  DS    0H                                                               
         NMOD1 0,**VDTE**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         NI    SNVIDCTL,X'FF'-SNVIDBLQ                                          
         CLI   5(R2),0             ANY INPUT IN COLUMN?                         
         BNE   VBILB10             YES                                          
*                                                                               
         OC    SVDMELEM,SVDMELEM   NONE, IF NOTHING BEFORE                      
         BZ    VBILBX                    THEN NO BILLBOARD                      
*                                                                               
VBILBSCT USING SNVIDELD,SVDMELEM                                                
         TM    VBILBSCT.SNVIDCTL,SNVIDBLQ   BILLBOARD BEFORE?                   
         BZ    VBILBX                       NO                                  
         OI    SNVIDCTL,SNVIDBLQ            YES                                 
         B     VBILBX                                                           
*                                                                               
VBILB10  CLI   8(R2),C'Y'          BILLBOARD?                                   
         BNE   VBILB20                                                          
         OI    SNVIDCTL,SNVIDBLQ   YES                                          
         B     VBILBX                                                           
*                                                                               
VBILB20  CLI   8(R2),C'N'          FORCE BILLBOARD OFF?                         
         BNE   INVLFLD             NO, ERROR                                    
*                                                                               
VBILBX   B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE PACKAGE CODE OF THE DETAIL LINE                    
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
***********************************************************************         
VALNPKG  DS    0H                                                               
         NMOD1 0,**VPKG**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VPKG10                                                           
*                                                                               
         OC    SVDMELEM,SVDMELEM    IF NOTHING BEFORE                           
         BZ    MISSFLD                                                          
*                                                                               
VPKGDSCT USING SNVIDELD,SVDMELEM                                                
         MVC   SNVIDPKG,VPKGDSCT.SNVIDPKG                                       
         DROP  VPKGDSCT                                                         
         GOTO1 =A(DISNPKG),DMCB,(RC),(R2),(R6),RR=RELO                          
         B     VPKGX                                                            
*                                                                               
VPKG10   TM    4(R2),X'08'         MUST BE VALID NUMERIC                        
         BZ    VPKGERR                                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         LTR   R1,R1                                                            
         BZ    VPKGERR                                                          
         CHI   R1,255                                                           
         BH    VPKGERR                                                          
*                                                                               
         STC   R1,SNVIDPKG                                                      
*                                                                               
VPKGX    B     XIT                                                              
*                                                                               
VPKGERR  MVC   GERROR,=AL2(INVPKG)                                              
         B     ERREXIT2                                                         
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* BUILDS THE TURNAROUND REQUEST                                                 
***********************************************************************         
BLDREQ   DS    0H                                                               
         NMOD1 0,**BREQ**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
* COPY MOS FROM MINIO MASTER KEY(SNVKEY) IN MINBLOCK                            
         MVC   INVMOS,MINBLOCK+MINMKEY-MINBLKD+SNVKMOS-SNVKEY                   
         XC    INVMOS,=X'FFFF'                                                  
*                                                                               
         XC    RQOPTS,RQOPTS       WE DON'T HAVE ANY REQUEST OPTIONS            
*                                                                               
         CLI   RQMOS,C' '                                                       
         BH    *+10                                                             
         MVC   TEMPDATE(4),PEREDATE   PERIOD END DATE(4) CONTAIN MONTH          
         L     R6,AIO                                                           
         USING RCRDD,R6                                                         
         XC    RCRDCTL,RCRDCTL                                                  
         MVI   RCRDAREA,C' '                                                    
         MVC   RCRDAREA+1(79),RCRDAREA                                          
*                                                                               
         MVC   RCRDCODE,=C'U2'                                                  
*                                                                               
         MVC   RCRDAGY,AGENCY                                                   
         MVC   RCRDMED,QMED                                                     
         MVC   RCRDCLT,QCLT                                                     
         L     RE,ASPOOLD                                                       
         USING SPOOLD,RE                                                        
         OC    RCRDCLT,BLANKS                                                   
         DROP  RE                                                               
*                                                                               
         TM    MISCFLG1,MF1GBLPR                                                
         BZ    *+10                                                             
         MVC   RCRDPRD,CURRQPRD                                                 
*                                                                               
         CLI   RCRDPRD,C' '                                                     
         BH    BTREQ90                                                          
         CLI   PI2RPOL,C'A'        IF NO PRD, TEST PROFILE                      
         BE    BTREQ60             PRD = ALL                                    
         MVC   RCRDPRD,=C'POL'     ELSE POL IF PRESENT                          
*                                                                               
         LA    RF,SVCLIST                                                       
BTREQ30  CLI   3(RF),0                                                          
         BE    BTREQ60                                                          
         CLI   3(RF),X'FF'                                                      
         BE    BTREQ90                                                          
         LA    RF,4(RF)                                                         
         B     BTREQ30                                                          
*                                                                               
BTREQ60  MVC   RCRDPRD,=C'ALL'     ELSE ALL                                     
*                                                                               
BTREQ90  TM    MISCFLG1,MF1GBLPR                                                
         BZ    BTREQ100                                                         
         CLI   CURRQPR2,C' '                                                    
         BNH   *+10                                                             
         MVC   RCRDPRD2,CURRQPR2                                                
*                                                                               
BTREQ95  DS    0H                                                               
         BRAS  RE,SETPRD                                                        
         BE    *+16                                                             
         MVC   RCRDPRD,WORK                                                     
         MVC   RCRDPRD2,WORK+3                                                  
*                                                                               
BTREQ100 CLI   GLOBLEST,0                                                       
         BE    BTREQ110                                                         
         EDIT  (B1,GLOBLEST),(3,RCRDEST),FILL=0                                 
*                                                                               
         BRAS  RE,SETEST                                                        
         BE    *+10                                                             
         MVC   RCRDEST,=C'NO '                                                  
*                                                                               
BTREQ110 MVC   HALF,BMKTSTA                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDMKT,DUB                                                      
*                                                                               
         MVC   RCRDSTA,QSTA                                                     
         CLI   RCRDSTA+3,C'-'                                                   
         BNE   *+12                                                             
         MVI   RCRDSTA+3,C' '                                                   
         B     BTREQ120                                                         
*                                                                               
         CLI   RCRDSTA,C'0'        CABLE STATIONS NEED THE /                    
         BL    *+8                                                              
         MVI   RCRDSTA+4,C'/'                                                   
*                                                                               
         CLI   RCRDSTA+4,C'/'                                                   
         BE    BTREQ120                                                         
         CLI   RCRDSTA+4,C'A'        KEEP X OF WABCX                            
         BNL   BTREQ120                                                         
         MVC   RCRDSTA+4(1),QSTA+5   WABC-X                                     
*                                                                               
BTREQ120 OC    RCRDSTA,BLANKS                                                   
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+10                                                             
         MVC   RCRDAREA+56(1),QSUBMED SUBMEDIA PASSED IN QCOMPARE               
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVMOS),(X'20',WORK)                              
         MVC   RCRDSDAT(4),WORK    ONLY YYMM                                    
*                                                                               
         CLI   RQBOOK,C' '         TEST BOOK SPECIFIED                          
         BNH   BTREQ130                                                         
         CLI   RQBOOK,X'FF'        TEST BOOK=NO                                 
         BE    BTREQ180                                                         
         MVC   RCRDBK1(6),RQBOOK                                                
         B     BTREQ180                                                         
*                                                                               
BTREQ130 DS    0H                  NO BOOK GIVEN                                
         CLI   PI2RHUT,C'N'        TEST HUT OPTION                              
         BNE   *+10                                                             
         MVC   RCRDBK1+4(2),=C'NO'                                              
         MVC   TEMPDATE(6),RCRDSDAT  USE MOS                                    
         CLI   TEMPDATE+4,C' '            IF FULL DATE GIVEN                    
         BNH   BTREQ140                                                         
         GOTO1 ADDAY,DMCB,TEMPDATE,TEMPDATE,6   ADD 6 DAYS TO ENSURE            
*                                  RIGHT CALENDAR MONTH                         
BTREQ140 DS    0H                                                               
         PACK  DUB,TEMPDATE+2(2)   LOOK UP BOOK BASED ON MOS MONTH              
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         IC    R0,PI2RHUT(R1)      BOOKS IN PROFI2R+4 THRU +15                  
         LTR   R0,R0               TEST VALUE PRESENT                           
         BZ    BTREQ180            NO - NO BOOK                                 
         CH    R0,=H'13'           MONTH 13 = ACT BOOK                          
         BNE   BTREQ150                                                         
         MVC   RCRDBK1(4),=C'ACT '                                              
         B     BTREQ180                                                         
*                                                                               
BTREQ150 DS    0H                                                               
         MVI   BYTE,0                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDBK1+2(2),DUB    SET MONTH                                    
         MVC   RCRDBK1(2),TEMPDATE   SET YEAR = MOS YEAR                        
         CR    R0,R1               UNLESS BOOK GT MOS                           
         BNH   BTREQ170                                                         
*                                                                               
BTREQ160 DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                 
         MVC   RCRDBK1(2),WORK+6                                                
*                                               SEE IF BOOK EXISTS              
BTREQ170 DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   KEY+2,C'A'          ARB                                          
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         LA    R3,200(R6)                                                       
*                                  NOTE- USE IOAREA+200 BECAUSE                 
*                                       REQUEST BUILT AT IOAREA                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,(R3),DMWORK               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+12                                                             
BTREQER1 LA    R2,DTLOPTNH                                                      
         B     NORATBOK                                                         
*                                                                               
         CLC   KEY(5),0(R3)                                                     
         BE    BTREQ180            BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   BTREQER1            ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     BTREQ160                                                         
*                                                                               
BTREQ180 DS    0H                                                               
         MVC   RCRDRQTR(10),=CL10'AUTO U2 RQ'                                   
         CLI   RQBUYOPT,C' '                                                    
         BNH   *+10                                                             
         MVC   RCRDOPT1,RQBUYOPT                                                
*                                                                               
         CLI   PSEUDOPT,C'R'         RESPONSE                                   
         BNE   *+8                                                              
         MVI   RCRDAREA+32,C'R'                                                 
*                                                                               
         CLI   PSEUDOPT,C'M'         MCT                                        
         BNE   *+8                                                              
         MVI   RCRDAREA+32,C'M'                                                 
*                                                                               
         PACK  DUB,RCRDCODE                                                     
         CVB   R0,DUB                                                           
         STC   R0,RCRDCTL+10                                                    
         MVI   RCRDCTL+14,106                                                   
*                                                                               
         CLI   PI2NAI2A,C'Y'                                                    
         BNE   BTREQ190                                                         
*                                                                               
* MODIFY REQUEST TO SPEC-33373 REQUIREMENTS                                     
*                                                                               
         MVI   RCRDBYID,C' '     QBYD = RUN BY BUY ID                           
         CLI   PI2NAI2B,C'Y'                                                    
         BNE   *+8                                                              
         MVI   RCRDBYID,C'Y'     QBYD = RUN BY BUY ID                           
*                                                                               
         MVI   RCRDOPT4,C'S'     SUMMARY MODE                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'20',WORK+6),-12                        
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'20',WORK+12),-1                        
         CLC   RCRDSDAT(4),WORK+6                                               
         BL    BTREQX                                                           
         CLC   RCRDSDAT(4),WORK+12                                              
         BH    BTREQX                                                           
*                                                                               
BTREQ190 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',                     X        
               RCRDCTL,RCRDCTL                                                  
         TM    8(R1),X'FF'                                                      
         BNZ   BTREQX                                                           
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'** REQUEST GENERATED **'                          
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
BTREQX   B     XIT                                                              
         DROP  R6                                                               
*                                                                               
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
         EJECT                                                                  
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
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   PI2PLKE,C'Y'        PREVENTING CHANGES FOR LOCKED ESTS?          
         JNE   YES                 NO - OK TO CHANGE THIS INVOICE               
*                                                                               
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
         CLI   SNVHDPRD,0          PRODUCT IN HEADER?                           
         JE    YES                 NO - JUST EXIT                               
         CLI   SNVHDEST,0          ESTIMATE IN HEADER?                          
         JE    YES                 NO - EXIT                                    
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
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO3                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
*                                                                               
         L     R2,AIO3                                                          
         USING ESTHDR,R2                                                        
         MVC   SVECNTRP,ECNTRL-ESTHDR(R2)                                       
         MVC   SVELKYMP,ELOCKYR-ESTHDR(R2)                                      
         DROP  R2                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R5,R6                                                            
*                                                                               
*                                                                               
*                                                                               
CHKADID  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,12                                                            
*                                                                               
CHKAD10  DS    0H                  SEE IF ALPHA                                 
         CLI   0(R1),C' '          SPACE?                                       
         BH    CHKAD15             NO                                           
*                                                                               
* SPACE HERE                                                                    
         CHI   R0,3                                                             
         BH    CHKADNO             ONLY CHARS 9-12 CAN BE SPACES                
         B     CHKADYES                                                         
*                                                                               
CHKAD15  DS    0H                  SEE IF ALPHA                                 
         CLI   0(R1),C'A'                                                       
         BL    CHKADNO                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   CHKAD20                                                          
*                                                                               
* 08/13/2008 - REMOVING 4A/8AN RESTRICTIONS ON AD-IDS                           
* NON-ALPHA HERE                                                                
*        CHI   R0,8                                                             
*        BH    CHKADNO             1-4 HAS TO BE ALPHA                          
*                                                                               
* CHARACTERS 5-12 CAN BE NUMERIC                                                
         CLI   0(R1),C'0'                                                       
         BL    CHKADNO                                                          
         CLI   0(R1),C'9'                                                       
         BH    CHKADNO                                                          
*                                                                               
CHKAD20  DS    0H                                                               
         LA    R1,1(R1)                                                         
         BCT   R0,CHKAD10                                                       
*                                                                               
CHKADYES J     YES                                                              
CHKADNO  J     NO                                                               
*                                                                               
*                                                                               
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
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FINDLEN - FINDS LENGTH OF INPUT IN A FIELD                                    
*           INPUT ASSUMED TO BE LEFT-ALIGNED                                    
* ON ENTRY: R1 = FIELD                                                          
*           R0 = LENGTH OF FIELD                                                
* ON EXIT:  RF = LENGTH OF INPUT                                                
***********************************************************************         
FINDLEN  DS    0H                                                               
         LR    RF,R0               L'FIELD                                      
         AR    R1,R0                                                            
         BCTR  R1,0                R1 -> LAST CHAR                              
*                                                                               
         CLI   0(R1),C' '                                                       
         BHR   RE                                                               
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         JH    *-14                                                             
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ON ENTRY:    R1 CONTAINS ADDRESS OF THE CHARACTER                             
* ON EXIT:     EQUAL CONDITION CODE RETURNED IF                                 
*              THE CHARACTER IS ALPHA/NUMERIC                                   
*              UNEQUAL COND. CODE RETURNED OTHERWISE                            
***********************************************************************         
ISALPHA  CLI   0(R1),C'A'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'Z'                                                       
         JH    ISNEQX                                                           
         J     ISEQX                                                            
*                                                                               
ISNUM    CLI   0(R1),C'0'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'9'                                                       
         JH    ISNEQX                                                           
*                                                                               
ISEQX    CR    RB,RB                                                            
         BR    RE                                                               
ISNEQX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* THIS SUBROUTINE READS I2N PROFILE AND SETS PRODUCT                            
* FOR THE U2 REQUEST DEPENDING ON VALUES                                        
* I2N PROFILE IS EXPECTED TO BE IN PROFI2N                                      
* ON EXIT EQUAL CONDITION IF NO PRODUCT CHANGES REQUIRED FOR U2                 
* UNEQUAL + PRODUCTS SET IN WORK, WORK+3 IF CHANGES ARE NECESSARY               
*                                                                               
* SPOT-POL/ALL UPDATIVE I2S ONLY                                                
* NET -POL/ALL UPDATIVE I2S ONLY                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
SETPRD   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
*                                                                               
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BNE   SETPQX                                                           
*&&DO                                                                           
* SPOT PROFILE HERE                                                             
         CLI   PI2NPOLS,C'P'                                                    
         BNE   *+16                                                             
*                                                                               
         CLI   SVCPROF,X'00'       TPOL CLIENT?                                 
         BE    SETP50              YES                                          
         B     SETPQX              NO - DON'T FORCE PRODUCT TO POL              
*                                                                               
         CLI   PI2NPOLS,C'A'                                                    
         BE    SETP60                                                           
         B     SETPQX              LEAVE PRODUCT AS IS                          
*&&                                                                             
* NET PROFILE HERE                                                              
SETP20   DS    0H                                                               
         CLI   PI2NPOLN,C'A'                                                    
         BE    SETP60                                                           
         CLI   PI2NPOLN,C'P'                                                    
         BNE   SETPQX              LEAVE PRODUCT AS IS                          
*                                                                               
* FORCE PRODUCT TO POL                                                          
SETP50   DS    0H                                                               
         MVC   WORK(3),=C'POL'                                                  
         B     SETPNQX                                                          
*                                                                               
* FORCE PRODUCT TO ALL                                                          
SETP60   DS    0H                                                               
         MVC   WORK(3),=C'ALL'                                                  
*                                                                               
SETPNQX  J     NO                                                               
SETPQX   J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* THIS SUBROUTINE READS I2N PROFILE AND SETS ESTIMATE                           
* FOR THE U2 REQUEST DEPENDING ON VALUES                                        
* I2N PROFILE IS EXPECTED TO BE IN PROFI2N                                      
* ON EXIT EQUAL CONDITION IF NO EST CHANGES REQUIRED FOR U2                     
* UNEQUAL IF ESTIMATE NEEDS TO BE SET TO "NO"                                   
*                                                                               
* SPOT-EST=NO UPDATIVE I2S ONLY                                                 
* NET -EST=NO UPDATIVE I2S ONLY                                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
SETEST   NTR1  BASE=*,LABEL=*                                                   
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BE    SETE10                                                           
*                                                                               
* SPOT PROFILE HERE                                                             
         CLI   PI2NESTS,C'Y'                                                    
         JNE   YES                 LEAVE ESTIMATE AS IS                         
         J     NO                                                               
*                                                                               
SETE10   DS    0H                                                               
* NET PROFILE HERE                                                              
         CLI   PI2NESTN,C'Y'                                                    
         JNE   YES                                                              
         J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
GENI2RQ  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVMED,QMED                                                       
         CLI   NETPAKSW,C'N'       SET NETPAK SWITCH                            
         BNE   GENI2005                                                         
*                                                                               
         LA    R2,=C'SI2Z'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'Z'                               
         GOTO1 GETPRFIL,DMCB,(R2),PROFI2Z                                       
         CLI   PI2ZSUB,C'Y'                                                     
         BNE   GENI2005                                                         
*                                                                               
         CLC   QMED,QSUBMED                                                     
         BE    GENI2005                                                         
         CLI   QSUBMED,C' '                                                     
         BE    GENI2005                                                         
         MVC   QMED,QSUBMED                                                     
*                                                                               
GENI2005 DS    0H                                                               
         LA    R2,=C'S0I2'                                                      
         GOTO1 GETPRFIL,DMCB,(R2),PROFI2                                        
         MVI   UPDFLAG,C'Y'                                                     
         CLI   PI2POST,C'Y'                                                     
         BE    *+8                                                              
         MVI   UPDFLAG,C'N'                                                     
*                                                                               
         MVC   QMED,SVMED                                                       
*                                                                               
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         L     R2,AIO3                                                          
         USING SPOOK,R2                                                         
*                                                                               
         XC    0(SPOOKXL,R2),0(R2)                                              
*                                                                               
         MVC   SPOOKUID,TWAORIG                                                 
         MVC   SPOOKDES,TWAORIG                                                 
         MVC   SPOOKAGY,AGENCY                                                  
*                                                                               
         LA    R0,SECBLK                                                        
         ST    R0,ASECBLK                                                       
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         L     R1,ASECBLK                                                       
         LA    R1,(SECPID-SECD)(R1)                                             
*                                                                               
         CLC   0(L'SECPID,R1),BLANKS                                            
         BNH   *+20                                                             
         CLC   =C'DDS',0(R1)                                                    
         BE    *+10                                                             
         MVC   SAVEPID,0(R1)                                                    
*                                                                               
         MVC   SPOOKDID,=C'SI2'                                                 
         CLC   SAVEPID,BLANKS                                                   
         BNH   *+10                                                             
         MVC   SPOOKDID,SAVEPID                                                 
*                                                                               
         MVC   SPOOKSYS,=C'SP'                                                  
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BNE   *+10                                                             
         MVC   SPOOKSYS,=C'NE'                                                  
*                                                                               
         MVC   SPOOKEOD,=C'I2'                                                  
         MVC   SPOOKJCL,=C'I2'                                                  
*                                                                               
         MVC   SPOOKRLD(3),=C'LD='                                              
         MVC   SPOOKRLD+3(2),=H'48'                                             
         MVC   SPOOKRLD+5(2),=H'24'                                             
         MVI   SPOOKWEN,2          NON-UPDATIVE SOON                            
*                                                                               
         MVC   SPOOKXT,=C'XT='     INDICATE WE HAVE EXTENDED SPOOKD             
         DROP  R2                                                               
*                                                                               
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
* 01/27/2010 - SKIPPING INITMNIO: IT RE-INITIALIZES MINMKEY                     
*              WITH QSTA+QNET LEFT OVER FROM NETWORK FIELD VALIDATION           
*                                                                               
*        GOTO1 INITMNIO                                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         L     RF,MINELEM                                                       
         CLI   0(RF),SNVIDELQ                                                   
         BNE   GENI2NDX                                                         
*                                                                               
         LA    R6,MINMKEY                                                       
         USING SNVKEYD,R6                                                       
*                                                                               
         XC    RCRDCTL,RCRDCTL                                                  
         MVI   RCRDAREA,C' '                                                    
         MVC   RCRDAREA+1(L'RCRDAREA-1),RCRDAREA                                
         MVI   RCRDREC2,C' '                                                    
         MVC   RCRDREC2+1(L'RCRDREC2-1),RCRDREC2                                
*                                                                               
         MVC   RCRDCODE,=C'I2'                                                  
         MVC   RCRDOPT1+4(1),UPDFLAG                                            
         MVI   RCRDCONT,C'*'                                                    
         OI    RCRDCTL+15,X'12'           SET 2-CARD LINKED                     
*                                                                               
         MVC   RCRDINT,INTOPT                                                   
         CLI   INTOPT,C' '                                                      
         BH    GENI2007                                                         
*                                                                               
         TM    INTFLAG,IFINTQ      ANY INTEGRATION CHARGES?                     
         BZ    GENI2007            NO - DON'T BOTHER WITH INT OPTIONS           
         MVI   RCRDINT,C'I'        INDICATE SOME INT CHARGES                    
         TM    INTFLAG,IFCOSTQ     ANY NONZERO COST SPOTS?                      
         BO    GENI2007            NO                                           
         MVI   RCRDINT,C'O'        INDICATE INT-ONLY INVOICE                    
*                                                                               
GENI2007 DS    0H                                                               
         MVC   RCRDAGY,AGENCY                                                   
         MVC   RCRDMED,QMED                                                     
         MVC   RCRDCLT,QCLT                                                     
         OC    RCRDCLT,BLANKS                                                   
*                                                                               
         TM    MISCFLG1,MF1GBLPR   GLOBAL PRODUCT?                              
         BO    GENI2010            YES                                          
*                                                                               
         MVC   RCRDPRD,=C'ALL'                                                  
         CLI   PI2RPOL,C'A'                                                     
         BE    *+10                                                             
         MVC   RCRDPRD,=C'POL'                                                  
         B     GENI2020                                                         
*                                                                               
GENI2010 DS    0H                  PRODUCT IN THE HEADER                        
         MVC   RCRDPRD,CURRQPRD                                                 
         MVC   RCRDPRD2,CURRQPR2                                                
         OC    RCRDPRD2,=C'   '                                                 
*                                                                               
GENI2020 DS    0H                                                               
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BNE   GENI2080            NO - DON'T DO ANYTHING                       
*                                                                               
* NETPAK INVOICE HERE                                                           
         CLI   PI2NPOLN,C'P'                                                    
         BNE   GENI2030                                                         
         CLC   =C'POL',RCRDPRD                                                  
         BE    GENI2080                                                         
         MVI   RCRDOPT1+4,C'N'                                                  
         B     GENI2080                                                         
*                                                                               
GENI2030 DS    0H                                                               
         CLI   PI2NPOLN,C'A'                                                    
         BNE   GENI2080                                                         
         CLC   =C'ALL',RCRDPRD                                                  
         BE    GENI2080                                                         
         MVI   RCRDOPT1+4,C'N'                                                  
*                                                                               
GENI2080 DS    0H                  ESTIMATE HERE                                
         MVC   RCRDEST,=C'NO '                                                  
         CLI   GLOBLEST,0                                                       
         BE    GENI2100                                                         
         EDIT  (B1,GLOBLEST),(3,RCRDEST),FILL=0                                 
*                                                                               
GENI2100 DS    0H                  NO ESTIMATE                                  
         LA    R1,PI2NESTS                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    R1,PI2NESTN                                                      
*                                                                               
         CLC   =C'NO ',RCRDEST                                                  
         BE    GENI2300                                                         
*                                                                               
         CLI   0(R1),C'Y'                                                       
         BNE   *+8                                                              
         MVI   RCRDOPT1+4,C'N'                                                  
*                                                                               
GENI2300 DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SNVKSTA                                                 
         GOTO1 MSUNPK,DMCB,DUB,WORK,WORK+4                                      
         MVC   RCRDSTA,WORK+4                                                   
*                                                                               
*        LA    R0,RCRDSTA+4                                                     
*        LA    R0,QSTA+4                                                        
*        LA    R1,RCRDMED                                                       
*        BRAS  RE,SETMED                                                        
*        GOTO1 SETMED                                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         CLI   RCRDSTA,C'0'                                                     
         BL    *+8                                                              
         MVI   RCRDSTA+4,C'/'                                                   
*                                                                               
         MVC   WORK(2),MINMKEY+SNVKMOS-SNVKEY                                   
         XC    WORK(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,WORK),(0,RCRDSDAT)                                
         MVC   RCRDSDAT+4(2),=C'  '                                             
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   RCRDAREA+56(1),QSUBMED QCOMPARE                                  
         B     GENI2400                                                         
*                                                                               
         CLI   BOOKOVR,C' '                                                     
         BNH   GENI2310                                                         
         CLI   BOOKOVR,X'FF'                                                    
         BE    GENI2400                                                         
         MVC   RCRDBK1(6),BOOKOVR                                               
         B     GENI2400                                                         
*                                                                               
GENI2310 DS    0H                  NO BOOK GIVEN                                
         CLI   PI2RHUT,C'N'        TEST HUT OPTION                              
         BNE   *+10                                                             
         MVC   RCRDBK1+4(2),=C'NO'                                              
         MVC   WORK(6),RCRDSDAT    USE MOS                                      
         CLI   WORK+4,C' '         IF FULL DATE GIVEN                           
         BNH   GENI2320                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK,6   ADD 6 DAYS TO ENSURE                    
*                                                                               
GENI2320 DS    0H                                                               
         PACK  DUB,WORK+2(2)   LOOK UP BOOK BASED ON MOS MONTH                  
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         IC    R0,PI2RHUT(R1)      BOOKS IN PROFI2R+4 THRU +15                  
         LTR   R0,R0               TEST VALUE PRESENT                           
         BZ    GENI2400            NO - NO BOOK                                 
         CH    R0,=H'13'           MONTH 13 = ACT BOOK                          
         BNE   GENI2330                                                         
         MVC   RCRDBK1(4),=C'ACT '                                              
         B     GENI2400                                                         
*                                                                               
GENI2330 DS    0H                                                               
         MVI   BYTE,0                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDBK1+2(2),DUB    SET MONTH                                    
         MVC   RCRDBK1(2),WORK     SET YEAR = MOS YEAR                          
         CR    R0,R1               UNLESS BOOK GT MOS                           
         BNH   GENI2360                                                         
*                                                                               
GENI2350 DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                 
         MVC   RCRDBK1(2),WORK+6                                                
*                                                                               
GENI2360 DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   KEY+2,C'A'          ARB                                          
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         L     R3,AIO2                                                          
*                                  NOTE- USE IOAREA+200 BECAUSE                 
*                                       REQUEST BUILT AT IOAREA                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,(R3),DMWORK               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+14                                                             
*                                                                               
GENI2380 MVC   RCRDBK1(6),=C'      '   INVALID BOOK - SO SKIP                   
         B     GENI2400                                                         
*                                                                               
         CLC   KEY(5),0(R3)                                                     
         BE    GENI2400            BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   GENI2380            ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     GENI2350                                                         
*                                                                               
GENI2400 DS    0H                                                               
         MVC   RCRDRQTR(10),=CL10'AUTO I2 RQ'                                   
         CLC   SAVEPID,BLANKS                                                   
         BNH   *+10                                                             
         MVC   RCRDRQTR(10),SAVEPID                                             
         DROP  R2                                                               
*                                                                               
         L     R2,AIO3             SPOOK+REQUEST AREA                           
         LA    R2,SPOOKXL(R2)      A(RCRDD)                                     
         CLI   RCRDOPT1+4-RCRDD(R2),C'N'   NON-UPDATIVE?                        
         BE    GENI2500            DON'T GENERATE LOCKS                         
*                                                                               
* UPDATIVE REQUEST HERE                                                         
*                                                                               
         CLI   PROFI2N+3,C'Y'      ESTIMATE "NO" INVALID?                       
         BE    GENI2NQX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,(CXTRAINF-COMFACSD)(RF)                                    
         BZ    GENI2450            ALLOW IT                                     
*                                                                               
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BNZ   GENI2NUP                                                         
*                                                                               
GENI2450 DS    0H                                                               
         L     R2,AIO3             A(SPOOK)                                     
         MVI   SPOOKWEN-SPOOK(R2),5 UPDATIVE SOON                               
         BRAS  RE,ADDLOCKS                                                      
         BNE   GENI2LKX                                                         
*                                                                               
GENI2500 DS    0H                                                               
         L     RF,ACOMFACS                                                      
         MVC   DMCB+8(4),CDATAMGR-COMFACSD(RF)                                  
         ST    RF,DMCB+12                                                       
         L     RF,CREQTWA-COMFACSD(RF)                                          
         L     R2,AIO3                                                          
         GOTO1 (RF),DMCB,(5,(RA)),SPOOKXL(R2),,,(R2)                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'REPORT XXX,9999 WILL BE PROCESSED SOON'           
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         MVC   CONHEAD+7(3),2(RE)                                               
         LH    RF,6(RE)                                                         
         LA    R2,CONHEAD+11                                                    
         EDIT  (RF),(4,(R2)),ALIGN=LEFT                                         
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
GENI2QX  J     YES                                                              
*                                                                               
GENI2NQX DS    0H                                                               
         MVC   CONHEAD(40),=C'REQUEST NOT GENERATED. CHECK I2N PROFILE'         
         OI    CONHEADH+6,X'80'                                                 
         J     YES                                                              
*                                                                               
GENI2NDX DS    0H                                                               
         MVC   CONHEAD(40),=CL40'NO I2 GENERATED: THERE ARE NO DETAILS'         
         OI    CONHEADH+6,X'80'                                                 
         J     YES                                                              
*                                                                               
GENI2LKX DS    0H                                                               
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         OI    CONHEADH+6,X'80'                                                 
         J     YES                                                              
*                                                                               
GENI2NUP DS    0H                                                               
         MVC   CONHEAD(36),=CL36'NOT AUTHORIZED TO UPDATE THIS SYSTEM'          
         OI    CONHEADH+6,X'80'                                                 
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ADDLOCKS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* NINV RECORDS                                                                  
*                                                                               
         BRAS  RE,BLDNVLK                                                       
*                                                                               
* TEST NV LOCK - IF NOT AVAILABLE, STOP                                         
*                                                                               
ADDLK2   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('LKTESTQ',IFLD),ACOMFACS                              
         CLI   4(R1),2                                                          
         BE    ADDLK2                                                           
*                                                                               
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
* BUY RECORDS                                                                   
*                                                                               
         BRAS  RE,TSTBALK          TEST FOR BUY ALLOCATION LOCKS                
         BNE   ADDLKERR                                                         
*                                                                               
* ADD BUY LOCK                                                                  
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+12                                                             
         BRAS  RE,BLDUNLK          YES/UNIT LOCK                                
         B     *+8                                                              
*                                                                               
         BRAS  RE,BLDBULK                                                       
*                                                                               
ADDLK4   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('LKLOCKQ',IFLD),ACOMFACS                              
         CLI   4(R1),2                                                          
         BE    ADDLK4                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
         BRAS  RE,BLDNVLK                                                       
*                                                                               
ADDLK6   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('LKLOCKQ',IFLD),ACOMFACS                              
         CLI   4(R1),2                                                          
         BE    ADDLK6                                                           
         CLI   4(R1),0                                                          
         BE    ADDLKOK                                                          
*                                                                               
ADDLKOK  J     YES                                                              
*                                                                               
ADDLKERR J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
BLDNVLK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
         L     R2,AIO3                                                          
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'NV'    INVOICE RECORDS                              
         MVC   L.LKNVMED,RCRDMED                                                
         MVC   L.LKNVCLT,RCRDCLT                                                
         MVC   L.LKNVSTA,RCRDSTA                                                
         CLI   RCRDMED,C'X'        IF MEDIA = X                                 
         BNE   *+8                                                              
         MVI   L.LKNVSTA+4,C'X'    MAKE STATION X                               
*                                                                               
         CLI   COUNTRY,C'C'         IF CANADIAN                                 
         BNE   BLDNV10                                                          
*                                                                               
         CLI   L.LKNVSTA+4,X'40'                                                
         BH    BLDNVX                                                           
         MVC   L.LKNVSTA+4(1),RCRDMED                                           
         B     BLDNVX                                                           
*                                                                               
BLDNV10  CLI   L.LKNVSTA+4,X'40'   ELSE                                         
         BH    *+8                                                              
         MVI   L.LKNVSTA+4,C'T'    USE T                                        
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   BLDNVX                                                           
*                                                                               
         CLI   RCRDSTA+4,X'40'     AND NO NETWORK INDICATOR                     
         BH    BLDNVX                                                           
         MVI   L.LKNVSTA+4,C'N'    MAKE IT N                                    
*                                                                               
BLDNVX   J     YES                                                              
         LTORG                                                                  
         DROP  L                                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
BLDBULK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
         L     R2,AIO3                                                          
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'BU'    BUY RECORDS                                  
         MVC   L.LKBUMED,RCRDMED                                                
         MVC   L.LKBUCLT,RCRDCLT                                                
         MVC   L.LKBUSTA,RCRDSTA                                                
                                                                                
         CLI   RCRDMED,C'X'        IF MEDIA = X                                 
         BNE   *+8                                                              
         MVI   L.LKBUSTA+4,C'X'    MAKE STATION X                               
                                                                                
         CLI   COUNTRY,C'C'        IF CANADIAN                                  
         BNE   BLDBU10                                                          
*                                                                               
         CLI   L.LKBUSTA+4,X'40'                                                
         BH    BLDBX                                                            
         MVC   L.LKBUSTA+4(1),RCRDMED                                           
         B     BLDBX                                                            
*                                                                               
BLDBU10  CLI   L.LKBUSTA+4,X'40'                                                
         BH    *+8                                                              
         MVI   L.LKBUSTA+4,C'T'                                                 
*                                                                               
BLDBX    J     YES                                                              
         LTORG                                                                  
         DROP  L                                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
BLDUNLK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
         L     R2,AIO3                                                          
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'UN'    UNIT RECORDS                                 
         MVC   L.LKUNCLT,RCRDCLT                                                
         MVC   L.LKUNSTA,RCRDSTA                                                
*                                                                               
BLDUNX   J     YES                                                              
         LTORG                                                                  
         DROP  L                                                                
         DROP  R2                                                               
*                                                                               
* TEST FOR BUY ALLOCATION LOCKS                                                 
*                                                                               
TSTBALK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
         L     R3,AIO3                                                          
         LA    R3,SPOOKXL(R3)                                                   
         USING RCRDD,R3                                                         
*                                                                               
         LA    R2,FULL             R2=A(MEDIA LIST FOR LOCK TESTS)              
         MVC   0(L'RCRDMED,R2),RCRDMED ALWAYS TEST FOR REQUESTED MEDIA          
         LA    R0,1                (R0=MEDIA LOOP COUNT)                        
         CLI   COUNTRY,C'C'                                                     
         BNE   TSTBALK4                                                         
*                                                                               
         CLI   RCRDMED,C'C'        TEST CANADIAN COMBINED MEDIA                 
         BNE   TSTBALK2                                                         
         MVI   1(R2),C'N'          YES - TEST FOR NETWORK                       
         MVI   2(R2),C'T'          AND TELEVISION ALSO                          
         LA    R0,3                                                             
         B     TSTBALK4                                                         
*                                                                               
TSTBALK2 CLI   RCRDMED,C'T'        TEST CANADIAN TELEVISION                     
         BE    *+12                                                             
         CLI   RCRDMED,C'N'        OR CANANDIAN NETWORK                         
         BNE   TSTBALK4                                                         
         MVI   1(R2),C'C'          YES - TEST FOR COMBINED MEDIA ALSO           
         LA    R0,2                                                             
*                                                                               
TSTBALK4 XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'BA'    BUY ALLOCATION LOCK TYPE                     
         MVC   L.LOCKKEY,BLANKS                                                 
         MVC   L.LKBAMED,0(R2)                                                  
         MVC   L.LKBACLT,RCRDCLT                                                
*                                                                               
* FOR 'ALL' ESTIMATE REQUEST THERE CAN'T BE ANY ESTIMATES LOCKED                
*                                                                               
         XC    L.LKBAEST,L.LKBAEST NULL TO TEST FOR ANY ESTIMATE                
         MVC   L.LKBAEST,RCRDEST   YES - TEST FOR SPECIFIC ESTIMATE             
*                                                                               
         BRAS  RE,TSTLOK           TEST FOR SPECIFIC/ALL ESTIMATES              
         BNE   TSTBNQX                                                          
         CLC   L.LKBAEST,BLANKS    TEST ALL ESTIMATES CHECKED                   
         BE    TSTBALK8                                                         
         MVC   L.LKBAEST,BLANKS                                                 
         BRAS  RE,TSTLOK           TEST FOR ALL ESTIMATES                       
         BNE   TSTBNQX                                                          
*                                                                               
TSTBALK8 LA    R2,1(R2)            BUMP TO NEXT MEDIA LETTER                    
         BCT   R0,TSTBALK4         DO FOR NUMBER OF MEDIAS                      
*                                                                               
TSTBQX   J     YES                                                              
TSTBNQX  J     NO                                                               
         LTORG                                                                  
         DROP  L                                                                
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
TSTLOK   NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
TSTLOK2  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LKTESTQ',L.LKKEYD),ACOMFACS                          
         CLI   4(R1),2                                                          
         BE    TSTLOK2                                                          
         CLI   4(R1),0                                                          
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         DROP  L                                                                
*                                                                               
*                                                                               
VALBOOK  NTR1  BASE=*,LABEL=*                                                   
         USING PSND,R3                                                          
         L     R2,PSNCOMP                                                       
*                                                                               
         MVC   BOOKOVR,=C'     '                                                
*                                                                               
         CLC   =C'NO',0(R2)                                                     
         BNE   VALB10                                                           
         CLI   PSNLEN,2                                                         
         BNE   VALB10                                                           
         MVI   BOOKOVR,X'FF'                                                    
         B     VALBQX                                                           
*                                                                               
VALB10   DS    0H                                                               
         CLC   =C'ACT',0(R2)                                                    
         BNE   VALB20                                                           
         CLI   PSNLEN,3                                                         
         BNE   VALB20                                                           
         MVC   BOOKOVR(3),=C'ACT'                                               
         B     VALBQX                                                           
*                                                                               
VALB20   DS    0H                                                               
         GOTO1 DATVAL,DMCB,(2,0(R2)),WORK                                       
         CLC   =C'000000',WORK                                                  
         BE    VALBNQX                                                          
*                                                                               
         MVI   WORK,C'0'                                                        
         MVC   BOOKOVR,WORK                                                     
         MVC   BOOKOVR+4(2),=C'  '                                              
         CLC   DMCB+3(1),PSNLEN    ONLY DATE GIVEN?                             
         BE    VALB30              YES - NO HUT                                 
*                                                                               
         LA    RF,0(R2)                                                         
         A     RF,DMCB                                                          
         MVC   BOOKOVR+4(2),1(RF)                                               
         CLC   BOOKOVR+4(2),=C'NO'                                              
         BE    VALB30                                                           
         CLC   BOOKOVR+4(2),=C'00'                                              
         BNH   VALBNQX                                                          
         CLC   BOOKOVR+4(2),=C'12'                                              
         BH    VALBNQX                                                          
*                                                                               
VALB30   DS    0H                                                               
         MVC   WORK(4),BOOKOVR                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVI   KEY+2,C'N'          NIELSEN                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    VALB40                                                           
         CLC   WORK(2),=X'5E01'    JAN94+ ALWAYS NEILSEN                        
         BNL   *+8                                                              
         MVI   KEY+2,C'A'          ELSE ARB                                     
*                                                                               
         CLI   COUNTRY,C'C'        US AGENCY?                                   
         BE    VALB40              NO, CANADA - SKIP                            
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   KEY+2,C'A'          US RADIO STATIONS USE ARB                    
*                                                                               
VALB40   MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,KEYSAVE,DMWORK            
         TM    DMCB+8,X'FF'                                                     
         BNZ   VALBNQX                                                          
         CLC   KEY(5),KEYSAVE                                                   
         BNE   VALBNQX                                                          
*                                                                               
VALBQX   J     YES                                                              
VALBNQX  J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKINT   NTR1  BASE=*,LABEL=*                                                   
         MVI   INTFLAG,X'00'                                                    
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BNE   CHKIQX                                                           
*                                                                               
CHKI10   DS    0H                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ       ANY DETAILS?                              
         BNE   CHKIQX                 NO                                        
*                                                                               
         OC    SNVIDCST,SNVIDCST   ANYTHING IN COST FIELD?                      
         BZ    *+8                                                              
         OI    INTFLAG,IFCOSTQ                                                  
*                                                                               
         OC    SNVIDINT,SNVIDINT   ANYTHING IN INT FIELD?                       
         BZ    *+8                                                              
         OI    INTFLAG,IFINTQ                                                   
         DROP  R6                                                               
*                                                                               
         BRAS  RE,MINIOSEQ                                                      
         BE    CHKI10                                                           
*                                                                               
CHKIQX   J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE FINDS FILM IN THE BUFFER                                         
*                                                                               
* ON ENTRY:    R1(HOB) - 1 AND                                                  
*              R1 - A(INTERNAL FILM CODE)                                       
*              OR                                                               
*              R1(HOB) - 2 AND                                                  
*              R1 - A(FILMCODE)                                                 
*                                                                               
* ON EXIT:     FULL - A(FILM IN THE FILM BUFFER)                                
*              EQUAL CONDITION CODE                                             
*                                                                               
*              UNEQUAL CONDITION CODE IF FILM NOT FOUND IN BUFFER               
***********************************************************************         
GETFBUF  NTR1  BASE=*,LABEL=*                                                   
         XC    FULL,FULL                                                        
         LR    R0,R1                                                            
         SRL   R0,24               HOB IS NOW LOW-ORDER BYTE                    
         LA    R1,0(R1)            CLEAR HOB                                    
         LA    R2,BUFFER           R2 = A(FIRST ENTRY IN TABLE)                 
         LR    R3,R2                                                            
         AHI   R3,BUFFERX-BUFFER                                                
*                                                                               
         USING BUFFDSCT,R2                                                      
*                                                                               
GTFB10   CR    R2,R3               END OF BUFFER?                               
         BNL   GTFBNQX             FILM NOT FOUND                               
         CLI   BUFFCODE,0          END OF TABLE ALREADY?                        
         BE    GTFBNQX             FILM NOT FOUND                               
*                                                                               
         CHI   R0,1                LOOKING UP INTERNAL CODE?                    
         BNE   GTBF30              NO - CHECK 12-CHAR FILM                      
*                                                                               
         CLC   BUFFCODE,0(R1)      CHECK INTERNAL CODE                          
         BE    GTFBQX                                                           
         B     GTBF50                                                           
*                                                                               
GTBF30   DS    0H                                                               
         CLC   BUFFCMCD,0(R1)                                                   
         BE    GTFBQX                                                           
*                                                                               
GTBF50   DS    0H                                                               
         LA    R2,BUFFNEXT                                                      
         J     GTFB10                                                           
         DROP  RF                                                               
*                                                                               
GTFBQX   ST    R2,FULL                                                          
         J     YES                                                              
*                                                                               
GTFBNQX  DS    0H                                                               
         J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS FIELD HEADER                                           
* ON EXIT RF HAS FIELD LENGTH                                                   
GFLEN    ZIC   RF,0(R2)            LENGTH OF THE FIELD                          
         SHI   RF,8                MINUS L(FIELD HEADER)                        
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZR   RE                  NO - EXIT                                    
         SHI   RF,8                YES - SUBTRACT L(EXT HEADER)                 
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS FIELD HEADER                                           
* FIELD FILLED WITH BLANKS                                                      
XCFLD    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GFLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLANKS                                                   
         MVI   5(R2),X'00'         INPUT DATA LENGTH=0                          
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS FIELD HEADER                                           
* FIELD INPUT CONVERTED TO UPPERCASE                                            
UCFLD    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GFLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),BLANKS                                                   
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A57'  GET A(SLNTAB)                            
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ASLNTAB,0(R1)       ADDRESS OF SPOT LENGTH TABLE                 
*                                                                               
         MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(L'BLANKS-1),BLANKS                                      
*                                                                               
SETUPX   DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* ROUTINE THAT CHECKS SPOT LENGTH AGAINS FILM LENGHTS                           
* TWOLENS,SNVIDSLN ARE EXPECTED TO BE FILLED IN                                 
CKFLEN   NTR1  BASE=*,LABEL=*                                                   
         USING SNVIDELD,R6                                                      
*                                                                               
         OC    TWOFILMS,TWOFILMS                                                
         BZ    CKFQX                                                            
*                                                                               
         CLC   TWOFILMS(1),TWOFILMS+1 SAME FILM?                                
         BNE   CKFL20              NO - ADD THE LENGTHS                         
         CLC   SNVIDSLN,TWOLENS                                                 
         BE    CKFQX                                                            
*                                                                               
CKFL20   DS    0H                                                               
         SR    R0,R0               SUM OF TWO LENGTHS                           
*                                                                               
         MVI   BYTE,X'00'                                                       
         ZIC   RF,TWOLENS          FIRST LENGTH                                 
         CLI   TWOLENS,X'FF'       LEN=ALL?                                     
         BNE   *+12                NO - ADD TO LENGTH SUM                       
         MVI   BYTE,X'FF'          INDICATE WE HAVE AN "ALL" LENGTH             
         B     *+6                 DO NOT ADD TO SUM                            
         AR    R0,RF               ADD TO SUM OF SPOT LENGTH                    
*                                                                               
         ZIC   RF,TWOLENS+1        2ND LENGTH                                   
         CLI   TWOLENS+1,X'FF'     LEN=ALL?                                     
         BNE   *+12                NO - ADD TO LENGTH SUM                       
         MVI   BYTE,X'FF'          INDICATE WE HAVE AN "ALL" LENGTH             
         B     *+6                 DO NOT ADD TO SUM                            
         AR    R0,RF               ADD TO SUM OF SPOT LENGTH                    
*                                                                               
         CLI   BYTE,X'FF'          ANY LEN=ALL COMMERCIALS?                     
         BE    CKFL50              YES                                          
*                                                                               
         CLM   R0,1,SNVIDSLN                                                    
         BNE   CKFNQX                                                           
         B     CKFQX                                                            
*                                                                               
CKFL50   DS    0H                  LENGTH=ALL PRESENT                           
         CLM   R0,1,SNVIDSLN                                                    
         BH    CKFNQX                                                           
*                                                                               
CKFQX    DS    0H                                                               
         J     YES                                                              
*                                                                               
CKFNQX   DS    0H                                                               
         J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* REQUEST SPECIAL I2 FOR MINDSHARE                                              
REQI2    NTR1  BASE=*,LABEL=*                                                   
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         CLC   SNVIDAP1,BLANKS                                                  
         BNH   *+14                                                             
         MVC   WORK(3),SNVIDAP1                                                 
         B     REQI210                                                          
*                                                                               
         GOTO1 FINDQPRD,DMCB,(SNVIDPRD,0)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(3),DMCB+1                                                   
*                                                                               
         CLC   SNVIDAP2,BLANKS                                                  
         BNH   REQI210                                                          
         MVC   WORK+3(3),SNVIDAP2                                               
         B     REQI210                                                          
         GOTO1 FINDQPRD,DMCB,(SNVIDPR2,0)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+3(3),DMCB+1                                                 
*                                                                               
REQI210  DS    0H                                                               
         CLI   SNVIDEST,0                                                       
         BE    REQI220                                                          
         EDIT  (B1,SNVIDEST),(3,WORK+6),FILL=0,WRK=WORK+9                       
*                                                                               
REQI220  DS    0H                                                               
         GOTO1 ADPRDEST                                                         
*                                                                               
         J     YES                                                              
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKBUMED  DS    XL1                                                              
LKBUCLT  DS    XL3                                                              
LKBUSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKBAMED  DS    CL1                                                              
LKBACLT  DS    CL3                                                              
LKBAEST  DS    CL3                                                              
         ORG   LOCKKEY                                                          
LKNVMED  DS    XL1                                                              
LKNVCLT  DS    XL3                                                              
LKNVSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKUNCLT  DS    XL3                                                              
LKUNSTA  DS    XL4                                                              
         ORG   LOCKKEY                                                          
LKNBCLT  DS    XL3                                                              
LKNBEST  DS    XL3                                                              
LKNBNET  DS    XL4                                                              
*                                                                               
*                                                                               
       ++INCLUDE SPSNVWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENSNV          (INVOICE DSECT)                              
         EJECT                                                                  
       ++INCLUDE SPSNVRCRD         (REQUEST CARD)                               
         EJECT                                                                  
       ++INCLUDE SPSNVFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSNVFCD           (OUR MAINT SCREEN)                          
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAFACTS                                                                       
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDGLVXCTLD                                                                    
* DDMINBLK                                                                      
* DDTWABLDD                                                                     
* DDPARSNIPD                                                                    
* SPTRCMML                                                                      
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDTWABLDD                                                      
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE DDREQHDR                                                       
         PRINT ON                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
TEMPPL8  DS    PL8                 TEMPORARY PACKED VALUE                       
SVREG    DS    XL4                 REGISTER SAVE AREA                           
*                                                                               
VGLOBBER DS    A                   A(GLOBBER)                                   
VLOCKET  DS    A                   A(LOCKET)                                    
AFTBENT  DS    A                   A(FILM TABLE ENTRY)                          
ASLNTAB  DS    A                   A(SPOT LENGTH TABLE)                         
*                                                                               
SVDDMCB  DS    6F                  SAVED PARAMETER LIST                         
*                                                                               
DFIRSTLN DS    H                   D(FIRST RECORD LINE) INTO TWA                
DCURRENT DS    H                   D(CURRENT LINE) INTO TWA                     
BTWNR1R2 DS    H                   # OF BYTES BTWN DETAIL LINES 1 & 2           
*                                                                               
INTOPT   DS    C                                                                
INTFLAG  DS    X                                                                
IFINTQ   EQU   X'01'               AT LEAST 1 INTEGRATION SPOT PRESENT          
IFCOSTQ  EQU   X'02'               AT LEAST 1 SPOT WITH NONZERO COST            
*                                                                               
SVDOFFST DS    XL1                 SAVED OFFSET OF SELECTED REC                 
*                                                                               
PROFI2Y  DS    CL16                I2Y PROFILE                                  
PI2YDUP  EQU   PROFI2Y+3           ALLOW DUPLICATE INVOICE ITEMS                
PI2YCKEE EQU   PROFI2Y+15          CHECK DETAIL DATE WITH EST END DATE          
*                                                                               
PROF0T1  DS    XL16                TRAFFIC I1 PROFILE                           
P0T1NSTD EQU   PROF0T1+9           NON-STD COMML CODES - RADIO ONLY             
*                                                                               
PROFI2P  DS    CL16                I2P PROFILE                                  
PI2PLKE  EQU   PROFI2P+2           NO LOCKED EST ADD/CHA/DEL/MOVE               
*                                                                               
PROFA0A  DS    CL16                A0A PROFILE                                  
PA0APAY  EQU   PROFA0A+14          NO PAID INVOICE CHA/DEL/MOVE                 
*                                                                               
PROFI2N  DS    CL16                I2N PROFILE                                  
PI2NPOLS EQU   PROFI2N+4           SPOT-PRD=POL UPDATIVE I2S ONLY               
PI2NESTS EQU   PROFI2N+5           SPOT-EST=NO UPDATIVE I2S ONLY                
PI2NPOLN EQU   PROFI2N+6           NET -PRD=POL UPDATIVE I2S ONLY               
PI2NESTN EQU   PROFI2N+7           NET -EST=NO UPDATIVE I2S ONLY                
PI2NAI2B EQU   PROFI2N+8           Auto I2s by buy ID                           
PI2NAI2A EQU   PROFI2N+10          Summary I2's for NINV                        
*                                                                               
PROFI2   DS    CL16                I2  PROFILE                                  
PI2POST  EQU   PROFI2+1            POST AFFIDS TO BUY RECORDS?                  
*                                                                               
PROFI2Z  DS    CL16                I2Z PROFILE                                  
PI2ZSUB  EQU   PROFI2Z+6           RE-READ I2 BY SUBMEDIA                       
*                                                                               
PI2XEAN  EQU   PROFI2X             EST=ALL=NO                                   
*                                                                               
SVMED    DS    C                                                                
UPDFLAG  DS    C                                                                
*                                                                               
*                                                                               
NLINPREC DS    X                   # OF LINES BTWN EACH DETAIL RECORD           
NDETLLNS DS    X                   MAXIMUM NUMBER OF DETAILS ON SCREEN          
*                                                                               
MISCFLG1 DS    X                                                                
MF1KYCHG EQU   X'80'               KEY CHANGED, BUILD THE SCREEN                
MF1USESM EQU   X'40'               USES MORE THAN 1 LINE PER RECORD             
MF1NT1ST EQU   X'20'               NOT ON FIRST LINE (BUILDING SCREEN)          
MF1GOTAD EQU   X'10'               GOT ADDRESS OF 2ND RECORD LINE               
MF1NETAM EQU   X'08'               NET AMOUNTS                                  
MF1ADDNG EQU   X'04'               ADDING THE DETAIL ELEMENT                    
MF1GBLPR EQU   X'02'               GOT GLOBAL PRODUCT FROM HEADER               
MF1RDFLM EQU   X'01'               READ FILMS ALREADY                           
*                                                                               
MISCFLG2 DS    X                                                                
MF2ADFLM EQU   X'80'               NEED TO ADD FILM ELEMENT(S)                  
MF2NWFLM EQU   X'40'               GOT A NEW FILM                               
MF2CLRLN EQU   X'20'               CLEAR LINE WHILE REVALIDATING LINE           
MF2NDSCR EQU   X'10'               END OF SCREEN DISPLAYING DETAILS             
MF2RPNSE EQU   X'08'               RESPONSE INVOICE                             
MF2DLRST EQU   X'04'               DELETE TILL END OF SCREEN                    
MF2CHNGD EQU   X'02'               DETAIL WAS CHANGED/ADDED/ OR DELETED         
MF2MTIME EQU   X'01'               MILITARY TIME USED                           
*                                                                               
MISCFLG3 DS    X                                                                
MF3DELP  EQU   X'80'               DEL PASSIVE (NO MORE DETAIL ELEMS)           
MF3ADID  EQU   X'40'               AD-IDS USED                                  
MF3HD    EQU   X'20'               HI-DEF COMMERCIAL                            
MF3Q4Q   EQU   X'10'               SET DETAIL LENGTH TO SNVIDL4Q                
*                                                                               
FLTRFLG1 DS    X                   1ST SET OF FILTER FLAGS                      
FF1DDATE EQU   X'80'                - DATE                                      
FF1DSLEN EQU   X'40'                - SPOT LENGTH                               
FF1DFILM EQU   X'20'                - FILM CODE                                 
FF1DFLM2 EQU   X'10'                - 2NDFILM CODE                              
FF1DCOST EQU   X'08'                - COST                                      
FF1DPROD EQU   X'04'                - PRODUCT                                   
FF1DESTM EQU   X'02'                - ESTIMATE                                  
FF1DRCNT EQU   X'01'                - RESPONSE COUNT                            
*                                                                               
FLTRFLG2 DS    X                   2ND SET OF FILTER FLAGS                      
FF2DNTWK EQU   X'80'                - NETWORK                                   
FF2BLLBD EQU   X'40'                - BILLBOARD                                 
FF2INTEG EQU   X'20'                - INTEGRATION                               
FF2NPKG  EQU   X'10'                - NETWORK PACKAGE                           
*                                                                               
TWOFILMS DS    XL2                 THE TWO POSSIBLE FILM CODES                  
ADDFLAGS DS    CL2                 ADD FLAGS                                    
*                                                                               
TWOLENS  DS    XL2                 THE TWO POSSIBLE FILMCODE LENGTHS            
SVR2     DS    XL4                 A(FILMCODE FIELD) FOR CKFLEN                 
*                                                                               
SVQMED   DS    CL1                 SAVED VALUES FOR ADD                         
SVQCLT   DS    CL3                                                              
SVQSTA   DS    CL5                                                              
*                                                                               
CURRBPRD DS    X                   CURRENT PRODUCT CODE                         
CURRBPR2 DS    X                   CURRENT PIGGYBACK CODE                       
CURRQPRD DS    CL3                 CURRENT EBCDIC PRODUCT CODE                  
CURRQPR2 DS    CL3                 CURRENT EBCDIC PIGGYBACK CODE                
GLOBLEST DS    X                   GLOBAL ESTIMATE CODE                         
INTESTSD DS    CL6                 EST START INTERSECTION                       
INTESTED DS    CL6                 EST END INTERSECTION                         
*                                                                               
* FIELDS BELOW ARE FILLED IN BY DISFILM                                         
*                                                                               
DFBLOCK  DS    0XL14                                                            
DFBFILM  DS    X                                                                
DFQFILM  DS    CL12                                                             
DFQFLEN  DS    X                                                                
*                                                                               
* FIELDS BELOW FILLED IN BY VALFILM+CHKFILM                                     
*                                                                               
VFBLOCK  DS    0X                                                               
*                                                                               
* FIELDS BELOW NEED TO BE PASSED TO VALFILM                                     
VFQPRD   DS    CL3                                                              
VFBPRD   DS    X                                                                
VFPREVF  DS    X                   PREVIOUS FILM                                
VFPREVST DS    X                   PREVIOUS FILM'S STATUS                       
*                                                                               
* THE FIELDS BELOW WILL BE FILLED IN BY FALFILM                                 
BFILM    DS    X                   BINARY INTERNAL FILM                         
BFILMSQ  DS    XL2                 TRAFFIC SEQ NUMBER                           
QFILM    DS    CL12                FILM CODE                                    
QFILMLEN DS    X                   FILM CODE LENGTH                             
TRFLEN   DS    X                   LENGTH                                       
VFSTAT   DS    X                   STATUS                                       
*        EQU   X'04'               SNVIDIFQ-IGNORE FILM FOR MATCH               
*                                                                               
TRFERR   DS    X                                                                
*                                                                               
SVQFILM  DS    CL12                                                             
SVQFLEN  DS    X                                                                
SVQFSLEN DS    X                                                                
VFBLKLQ  EQU   *-VFBLOCK                                                        
*                                                                               
*                                                                               
*                                                                               
SVFLDLST DS    XL(MAXFLDEQ)        DETAIL ORDER LIST                            
SVFLDCNT DS    X                   NUMBER OF FIELDS IN LIST                     
*                                                                               
PERDATES DS    0CL12               INVOICE PERIOD DATES                         
PERSDATE DS    CL6                     START DATE                               
PEREDATE DS    CL6                     END DATE                                 
*                                                                               
NXTUPKEY DS    XL(L'SNVKMINK)      LAST DETAIL/UPDATE KEY                       
PERVALST DS    XL56                PERVAL STORAGE                               
TWAELEM  DS    XL85                TWA BUILD ELEMENT                            
*                                                                               
SAVPCT   DS    CL9                 %-AGE EXPRESSION                             
SAVPCTL  DS    XL1                 LENGTH OF %-AGE EXPRESSION                   
*                                                                               
TOTAMNTS DS    PL16                TOTAL AMOUNT OF DETAILS                      
TOTSPOTS DS    PL4                 TOTAL NUMBER OF DETAILS                      
*                                                                               
SCRAMNTS DS    PL16                SCREEN AMOUNT OF DETAILS                     
SCRSPOTS DS    PL4                 SCREEN NUMBER OF DETAILS                     
*                                                                               
FLTAMNTS DS    PL16                FILTER AMOUNT OF DETAILS                     
FLTSPOTS DS    PL4                 FILTER NUMBER OF DETAILS                     
*                                                                               
NUMINLST DS    XL1                 ORDINAL NUMBER OF THE DETAIL CHANGED         
NUMINLS2 DS    XL1                 ORDINAL NUMBER OF THE DETAIL CHANGED         
*                                                                               
PSEUDOPT DS    CL1                                                              
TEMPDATE DS    CL6                 TEMPORARY DATE FIELD                         
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                  FILTER FIELDS                                
FLTRDTE1 DS    XL1                 DATE 1 FROM START OF PERIOD                  
FLTRDTE2 DS    XL1                 DATE 2 FROM START OF PERIOD                  
FLTRSLEN DS    XL1                 SECONDS                                      
FLTRFILM DS    CL12                FILM                                         
FLTRFLM2 DS    CL12                SECOND FILM                                  
FLTRCOST DS    XL4                 COST                                         
FLTRPROD DS    CL3                 PRODUCT                                      
FLTRESTM DS    XL1                 ESTIMATE                                     
FLTRRCNT DS    XL3                 RESPONSE COUNT                               
FLTRNTWK DS    CL3                 NETWORK                                      
FLTRINTG DS    XL4                 INTEGRATION                                  
FLTRNPKG DS    X                   NETWORK PACKAGE                              
*                                                                               
RQOPTS   DS    0CL35                                                            
RQSW     DS    C                   REQUEST SWITCH                               
RQPRD    DS    CL6                         PRODUCT                              
RQEST    DS    CL3                         ESTIMATE                             
RQBOOK   DS    CL6                         BOOK                                 
RQNAME   DS    CL10                        NAME                                 
RQMOS    DS    CL6                         MONTH OF SERVICE                     
RQREP    DS    CL2                         REP                                  
RQBUYOPT DS    CL1                         BUY OPTION                           
*                                                                               
*                                                                               
LKDATA   DS    0X                                                               
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
LKDATALQ EQU   *-LKDATA                                                         
*                                                                               
IFLDH    DS    CL8                                                              
IFLD     DS    CL30                                                             
IFLDCNT  DS    CL30                                                             
*                                                                               
*                                  DETAILS THAT ARE ON UPDATE SCREEN            
DETLLIST DS    0XL(NLSTLINS*DLSTLENQ)                                           
         DS    (NLSTLINS)XL(DLSTLENQ)                                           
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
MELEM3   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
SVDMELEM DS    XL(L'MELEM)         SAVED MINIO ELEMENT                          
*                                                                               
PREFILTD DS    XL(DLSTLENQ)        SAVED PRE-FILTER DETAIL                      
*                                                                               
XPKEY    DS    XL64                                                             
XSVPKEY  DS    XL64                                                             
SVBPRD   DS    XL1                                                              
SVBPRD2  DS    XL1                                                              
*                                                                               
MATCHFL  DS    X                                                                
MFMATCHQ EQU   X'01'               MATCH OPTION ENTERED                         
MFOCOSTQ EQU   X'02'               ORIGINAL COST OPTION ENTERED                 
LASTLINE DS    A                                                                
*                                                                               
BOOKOVR  DS    CL5                 DEMO OVERRIDE FOR SOON I2                    
INVMOS   DS    XL2                                                              
*                                                                               
SAVEPID  DS    CL10                                                             
SVCTL    DS    X                                                                
*                                                                               
BLANKS   DS    CL132                                                            
*                                                                               
FLMTABTY DS    X                   FILM TABLE TYPE (FROM X'20'/X'29')           
FLMTAB   DS    XL1024                                                           
FLMTABX  EQU   *                                                                
*                                                                               
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
TWODECIM DS    C                   2-DECIMAL IMPRESSIONS (Y/N/)                 
*                                                                               
BUFFER   DS    XL(MXBUFNUM*(BUFFNEXT-BUFFCODE))                                 
BUFFERX  EQU   *                                                                
*                                                                               
***********************************************************************         
* EQUATES                                                                       
***********************************************************************         
NUMFLTRS EQU   2                   SO FAR 2 FILTER FLAGS                        
NLSTLINS EQU   14                  # OF LINES BTWN HEADING AND PFKEYS           
*****  USING    ===>   <===  INSTEAD OF SNVIDSLN BECAUSE WE SOMETIMES           
*****           ===>   <===  GET 00'S AND THE PROG THINKS NOTHING THERE         
*****           ===>   <===  TO VALIDATE ON THE LINE                            
*****           ===>   <===  NOTE:  USE  DLSTLENQ-1  FOR MINEKEY                
DLSTLENQ EQU   SNVIDPRD-SNVIDDAY   L(DETAIL ENTRY IN LIST)                      
MXBUFNUM EQU   200                 MAXIMUM NUMBER OF BUFFER ENTRIES             
***********************************************************************         
* ERROR EQUATES                                                                 
***********************************************************************         
FPRDERR  EQU   71                  FILM AND PRODUCT DO NOT AGREE                
FSLNERR  EQU   72                  FILM AND SPOT LENGTH DO NOT AGREE            
FLMCERR1 EQU   98                  INVALID FILM CODE                            
FLMCERR2 EQU   99                  INVALID SECOND FILM                          
AAANOGO  EQU   992                 PRODUCT AAA NOT ALLOWED!                     
POLNOGO  EQU   102                 PRODUCT CAN'T BE POL!                        
NOCOMEL  EQU  1170                 COMMERCIAL ELEMENT MISSING                   
INVPKG   EQU  1248                 INVALID PACKAGE CODE                         
MISSPIGQ EQU  1335                 PIGGYBACK PRODUCT MISSING                    
ERR255Q  EQU  1349                 MORE THAN 255 IDENTICAL DETAILS              
***********************************************************************         
* DSECT FOR THE FIELD TABLES                                                    
***********************************************************************         
TABLDSCT DSECT                                                                  
TABLELEN DS    XL1                 ENTRY LENGTH                                 
TABLFLEN DS    XL1                 FIELD LENGTH                                 
TABLFNUM DS    XL1                 FIELD NUMBER                                 
TABLTEXT DS    0C                  BEGINNING OF TEXT FOR COLUMN TITLE           
***********************************************************************         
* DSECT FOR THE ROUTINE TABLE                                                   
***********************************************************************         
ROUTTABD DSECT                                                                  
ROUTFLDN DS    XL1                 FIELD NUMBER                                 
ROUTVALR DS    XL4                 VALIDATION ROUTINE FOR THE FIELD             
ROUTDISR DS    XL4                 DISPLAY ROUTINE FOR THE FIELD                
ROUTNEXT DS    0X                  NEXT FIELD'S SET OF ROUTINES                 
***********************************************************************         
* DSECT FOR THE BUFFER ENTRIES                                                  
***********************************************************************         
BUFFDSCT DSECT                                                                  
BUFFCODE DS    XL1                 FILM INTERNAL CODE                           
BUFFSEQN DS    XL2                 FILM SEQUENCE ID NUMBER                      
BUFFSTAT DS    XL1                 STATUS                                       
BUFFSINV EQU   X'80'                - INVALID FILM CODE                         
BUFFSHD  EQU   X'40'                - HI-DEF                                    
BUFFCMCD DS    CL12                FILM/AD ID CODE                              
BUFFNEXT DS    0C                  NEXT FILM ENTRY                              
***********************************************************************         
* DSECT FOR THE FILMTAB ENTRIES                                                 
***********************************************************************         
FILMTABD DSECT                                                                  
FTABTYPE DS    XL1                 X'20' - FTABPRDS IS 0XL1                     
*                                  X'29' - FTABPRDS IS 0CL3                     
FTABCODE DS    XL1                 FILM INTERNAL CODE                           
FTABELEN DS    XL1                 ENTRY LENGTH                                 
FTABSLEN DS    XL1                 SECONDS LENGTH                               
FTABPRDS DS    0X                  VARIABLE LENGTH PRODUCT LIST                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077SPSNV03   06/08/20'                                      
         END                                                                    
