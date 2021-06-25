*          DATA SET SPSNV07    AT LEVEL 227 AS OF 05/03/11                      
*PHASE T21007A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21007 - FIXES FILM CODES FOR THE INVOICE RECORDS                     
*                  THIS IS NECESSARY WHEN THE USER ADDS THE INVOICE             
*                  DETAILS WITH SOME NON-EXISTANT FILM CODES AND THEN           
*                  ADDS THE FILM CODES TO THE TRAFFIC SYSTEM                    
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
T21007   TITLE 'SPSNV07 - FIXES FILM CODE SEQ NUMBERS'                          
T21007   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21007*,R7,R8,RR=R3                                           
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
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         GOTO1 =A(SETPFTBL),DMCB,(RC),RR=RELO                                   
*                                                                               
         TM    CTLRFLG1,CF1CKOFF   SAVE OFFSET OF SELECTED LINE?                
         BZ    *+14                                                             
         MVC   SVDOFFST,SELOFFST   YES                                          
         NI    CTLRFLG1,X'FF'-CF1CKOFF                                          
*                                                                               
         L     R1,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(R1)                                   
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
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
*                                                                               
         LA    R2,DTLKEYLH                                                      
         OI    6(R2),X'20'         PROTECT THIS FIELD                           
*                                                                               
         TM    4(R2),X'80'         FIELD WAS CHANGED?                           
         BZ    VK05                                                             
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
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
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
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    VK12                NO, NO NEED TO GET PROFILES AGAIN            
         GOTO1 GETPRFIL,DMCB,=C'sI2Y',PROFI2Y   GET PROFILES                    
         GOTO1 (RF),(R1),=C'S0T1',PROF0T1                                       
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
         CLC   QNTWK,=3C' '                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   SVQSTA,QSTA                                                      
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                            
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
         GOTO1 VGLOBBER,DMCB,=C'PUTD',12(R3),(R0),GLVSPPER                      
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
         CLI   SNVHDLEN,SNVHDLN3                                                
         BL    *+14                                                             
         CLC   SNVHDAP1,=C'   '    DO WE HAVE A GLOBAL PRODUCT?                 
         BH    VK14                                                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVSPPRD                                
         B     VK20                NO                                           
*                                                                               
VK14     MVC   CURRBPRD,SNVHDPRD   COPY GLOBAL PRODUCT                          
         MVC   CURRQPRD,=C'   '                                                 
         CLI   SNVHDLEN,SNVHDLN3                                                
         BL    *+10                                                             
         MVC   CURRQPRD,SNVHDAP1                                                
         OI    MISCFLG1,MF1GBLPR   YES                                          
         MVC   DTLKYL2(4),=CL4'PRD='                                            
*                                                                               
         CLC   CURRQPRD,=C'   '                                                 
         BNH   *+14                                                             
         MVC   DMCB+1(3),CURRQPRD                                               
         B     VK15                                                             
*                                                                               
         GOTO1 FINDQPRD,DMCB,(SNVHDPRD,0)  SHOW THE PRODUCT ON TOP PART         
         BE    VK15                                                             
*                                                                               
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
         BNE   VK15A               NO                                           
*                                                                               
         CLI   SNVHDLEN,SNVHDLN3                                                
         BL    VK16                                                             
         CLC   SNVHDAP2,=C'   '    DO WE HAVE A GLOBAL PRODUCT?                 
         BNH   VK16                                                             
*                                                                               
VK15A    DS    0H                                                               
         MVC   CURRBPR2,SNVHDPR2   COPY GLOBAL PIGGYBACK PRODUCT                
         CLI   SNVHDLEN,SNVHDLN3                                                
         BL    *+10                                                             
         MVC   CURRQPR2,SNVHDAP2                                                
         MVC   DTLKYL2+8(3),DMCB+1                                              
         MVC   DTLKYL2+7(1),P0TIPBS                                             
         MVC   FAKEFLD+3(1),P0TIPBS                                             
         CLI   P0TIPBS,0                                                        
         BE    *+12                                                             
         CLI   P0TIPBS,C'N'                                                     
         BNE   *+12                                                             
         MVI   DTLKYL2+7,C'-'      YES, SHOW PIGGY NEXT TO 1ST PRODUCT          
         MVI   FAKEFLD+3,C'-'                                                   
*                                                                               
         CLC   CURRQPR2,=C'   '                                                 
         BNH   *+14                                                             
         MVC   DMCB+1(3),CURRQPR2                                               
         B     VK15D                                                            
*                                                                               
         GOTO1 FINDQPRD,DMCB,(SNVHDPR2,0)                                       
         BE    VK15D                                                            
         MVC   DTLKYL2+8(3),=3C'?'                                              
         MVC   CURRQPR2,=3C'?'                                                  
         B     VK16                                                             
*                                                                               
VK15D    MVC   DTLKYL2+8(3),DMCB+1                                              
         MVC   CURRQPR2,DMCB+1                                                  
         MVC   FAKEFLD+4(3),DMCB+1                                              
         MVI   FAKEFLDH+5,7                                                     
*                                                                               
VK16     GOTO1 VGLOBBER,DMCB,=C'PUTF',FAKEFLDH,,GLVSPPRD                        
*                                                                               
VK20     MVC   GLOBLEST,SNVHDEST   COPY GLOBAL ESTIMATE                         
         XC    INTESTSD(L'INTESTSD*2),INTESTSD  CLEAR ESTIMATE DATES            
*                                                                               
         CLI   SNVHDEST,0          DO WE HAVE A GLOBAL ESTIMATE?                
         BNE   VK25                YES                                          
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVSPEST                                
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
         BE    VK27                                                             
         CLC   CURRQPR2,=3C'?'                                                  
         BE    VK27                                                             
         GOTO1 VALIEST                                                          
*                                                                               
         MVC   INTESTSD,ESTSTRT    FIND THE START AND END OF EST FOR            
         MVC   INTESTED,ESTEND        BOTH PRODUCTS                             
*                                                                               
         CLI   CURRBPR2,0          NO PIGGYBACK                                 
         BE    VK27                                                             
*                                                                               
         MVC   QPRD,CURRQPR2                                                    
         GOTO1 VALIEST                                                          
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
VK27     GOTO1 VGLOBBER,DMCB,=C'PUTF',FAKEFLDH,,GLVSPEST                        
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',FAKEFLDH,,GLVSPPRD                        
         LA    RE,FAKEFLD                                                       
         ZIC   R0,FAKEFLDH+5                                                    
         AR    RE,R0                                                            
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         EDIT  (B1,GLOBLEST),(3,0(RE)),FILL=0                                   
         ZIC   R1,FAKEFLDH+5                                                    
         AH    R1,=H'4'                                                         
         STC   R1,FAKEFLDH+5                                                    
         GOTO1 VGLOBBER,DMCB,=C'PUTF',FAKEFLDH,,GLVSPPRD                        
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
*****                                                                           
* DONE VALIDATING ALL THE KEY FIELDS                                            
*****                                                                           
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VREC     DS    0H                                                               
         XC    MINEKEY,MINEKEY     FIND ALL THE COMMERCIAL ELEMENTS             
         MVI   MINEKEY,SNVCMELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+12                                                             
*                                                                               
RDFLM10  CLI   MINERR,MINEEOF                                                   
         BE    VRX                                                              
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVCMELD,R6                                                      
         CLI   SNVCMEL,SNVCMELQ                                                 
         BNE   VRX                                                              
         MVC   QFILM,SNVCMCD                                                    
         MVC   BSEQ,=X'0000'       SET TO ZERO                                  
         MVI   BFLAG,X'00'                                                      
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
RDFLM15  XC    KEY,KEY             SET UP THE TRAFFIC FILM KEY                  
         LA    R3,KEY                                                           
         USING CMLRECD,R3                                                       
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         OC    CMLKCLT,BTRACLT     USE TRAFFIC CLIENT                           
         BNZ   *+10                                                             
         MVC   CMLKCLT,BCLT        OTHERWISE USE NORMAL CLIENT                  
         MVC   CMLKCML,QFILM                                                    
*                                                                               
         CLI   QFILM+8,C' '                                                     
         BNH   RDFLM20                                                          
*                                                                               
* WE HAVE AN HAVE AD-ID HERE                                                    
*                                                                               
         CLC   KEYSAVE(2),=X'0AC1'                                              
         BE    *+14                                                             
         MVC   CMLPIDAD,=X'0AC1'                                                
         B     *+10                                                             
         MVC   CMLPIDAD,=X'0AC2'                                                
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
         GOTO1 (RF),DMCB,(C'P',QFILM),CMLPADID                                  
         BNE   CFLM95                                                           
*                                                                               
RDFLM20  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'TRFDIR',KEY,KEY,(0(RA),0)         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CMLKEY,KEYSAVE                                                   
         BE    RDFLM30                                                          
*                                                                               
         CLI   QFILM+8,C' '                                                     
         BNH   CFLM95                                                           
         CLC   KEYSAVE(2),=X'0AC1'                                              
         BE    RDFLM15                                                          
         B     CFLM95              NOT FOUND, SET DETAIL TO X'0000'             
*                                                                               
RDFLM30  DS    0H                                                               
         CLC   =X'0A21',KEY        PRIMARY KEY?                                 
         BNE   *+12                NO, DON'T WORRY                              
         TM    KEY+CMLKSTAT-CMLKEY,CMLKSTA_PCKD IS CML PACKED?                  
         BO    CFLM95              YES - THIS IS AN AD-ID                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'TRFFILE',                X        
               KEY+14,AIO,(0(RA),DMWORK)                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO              FIND THE COMMERCIAL DATA ELEMENT             
         AHI   R3,24                                                            
CFLM85   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF THERE ISN'T ONE                       
         CLI   0(R3),X'10'                                                      
         BE    CFLM90                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFLM85                                                           
*                                                                               
         USING CMLDTAEL,R3                                                      
CFLM90   DS    0H                                                               
         TM    CMLSTAT,X'80'       CMML DELETED?                                
         BO    CFLM95                                                           
*                                                                               
         MVC   BSEQ,CMLSEQ+1    SET COMMERCIAL SEQ NO.                          
         CLC   KEY(2),=X'0AC2'                                                  
         BNE   *+8                                                              
         OI    BFLAG,SNVCMHDQ                                                   
*                                                                               
CFLM95   MVC   SNVCMSEQ,BSEQ        SET COMMERCIAL SEQ NO.                      
         NI    SNVCMFLG,X'FF'-SNVCMHDQ                                          
         TM    BFLAG,SNVCMHDQ                                                   
         BZ    *+8                                                              
         OI    SNVCMFLG,SNVCMHDQ                                                
         DROP  R3                                                               
*                                                                               
         BAS   RE,MINIOWRT         AFTER A WRITE IT MIGHT NOT BE STABLE         
*                                                                               
         XC    MINEKEY,MINEKEY     FIND ALL THE COMMERCIAL ELEMENTS             
         MVI   MINEKEY,SNVCMELQ                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),SNVCMICD                                 
         BAS   RE,MINIORD                                                       
*                                                                               
RDFLM90  BAS   RE,MINIOSEQ                                                      
         B     RDFLM10                                                          
*                                                                               
VRX      TM    MNIOFLAG,X'80'      DO WE NEED TO CLOSE MINIO BUFFER?            
         BZ    XIT                                                              
*                                                                               
         XC    MINEKEY,MINEKEY     FIND ALL THE COMMERCIAL ELEMENTS             
         MVI   MINEKEY,SNVHDELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MINIOCLS                                                      
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
*&&DO                                                                           
         XC    KEY,KEY                                                          
         L     RF,MINBUFF                                                       
         MVC   KEY(L'SNVKEY),0(RF)                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'XSPDIR',KEY,KEY,(0(RA),0)         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'SNVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',             X        
               KEY+36,AIO,(0(RA),DMWORK)                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,AIO                                                           
         L     RE,MINBUFF                                                       
         LHI   R1,LIOS                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),=C'XSPFIL',                 X        
               KEY+36,AIO,(0(RA),DMWORK)                                        
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE FINDS THE EBCDIC EQUIVALENT OF A BINARY PRODUCT CODE             
*                                                                               
* ON ENTRY:    PARAM1   BYTE 0     BINARY PRODUCT CODE                          
*              SVCLIST             CLIENT'S PRODUCT LIST                        
*                                                                               
* ON EXIT:     PARAM1   BYTE 1-3   EBCDIC PRODUCT                               
***********************************************************************         
FINDQPRD NTR1                                                                   
         LHI   R0,L'SVCLIST/4                                                   
         LA    R1,SVCLIST                                                       
*                                                                               
FQPRD10  CLI   0(R1),0             ANY MORE PRODUCTS?                           
         BE    FQPRDNO             NO                                           
*                                                                               
         CLC   3(L'BPRD,R1),DMCB   BINARY PRODUCT CODE MATCH?                   
         BE    *+12                                                             
         LA    R1,4(R1)            NO, CHECK NEXT PRODUCT                       
         BCT   R0,FQPRD10                                                       
*                                                                               
FQPRDNO  B     NO                                                               
*                                                                               
FQPRDYES DS    0H                                                               
         MVC   DMCB+1(L'QPRD),0(R1)  YES, COPY THE EBCDIC PRODUCT               
         B     YES                                                              
*                                                                               
*                                                                               
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1                                                                   
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
STPF20   GOTO1 INITIAL,DMCB,(R2)                                                
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
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVSPPRD                                
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
*                                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
***********************************************************************         
MINIOCLS NTR1                                                                   
         NI    MNIOFLAG,X'FF'-X'80'                                             
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
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
***********************************************************************         
* SYSTEM SPOT ERROR MESSAGES                                                    
***********************************************************************         
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
NORATBOK MVI   GERROR1,INVRATBK    RATING BOOK NOT ON FILE                      
*                                                                               
ERREXIT2 OI    GENSTAT2,USMYERSY   STICK WITH THIS MESSAGE SYSTEM               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         B     MYERRXIT                                                         
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT GOTO1 MYERR                                                            
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
         PRINT OFF                                                              
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
         PRINT ON                                                               
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
TEMPPL8  DS    PL8                 TEMPORARY PACKED VALUE                       
*                                                                               
VGLOBBER DS    A                   A(GLOBBER)                                   
AFTBENT  DS    A                   A(FILM TABLE ENTRY)                          
*                                                                               
SVDDMCB  DS    6F                  SAVED PARAMETER LIST                         
*                                                                               
DFIRSTLN DS    H                   D(FIRST RECORD LINE) INTO TWA                
DCURRENT DS    H                   D(CURRENT LINE) INTO TWA                     
BTWNR1R2 DS    H                   # OF BYTES BTWN DETAIL LINES 1 & 2           
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
TRFERR   DS    X                                                                
TRFLEN   DS    X                   LENGTH                                       
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
*                                                                               
TWOFILMS DS    XL2                 THE TWO POSSIBLE FILM CODES                  
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
BFILM    DS    X                   BINARY INTERNAL FILM                         
BFILMSQ  DS    XL2                 TRAFFIC SEQ NUMBER                           
QFILM    DS    CL12                FILM CODE                                    
BSEQ     DS    XL2                                                              
BFLAG    DS    X                                                                
*                                                                               
HFLEN    DS    X                   SPOT LENGTH                                  
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
*                                  FILTER FIELDS                                
FLTRDTE1 DS    XL1                 DATE 1 FROM START OF PERIOD                  
FLTRDTE2 DS    XL1                 DATE 2 FROM START OF PERIOD                  
FLTRSLEN DS    XL1                 SECONDS                                      
FLTRFILM DS    CL8                 FILM                                         
FLTRFLM2 DS    CL8                 SECOND FILM                                  
FLTRCOST DS    XL4                 COST                                         
FLTRPROD DS    CL3                 PRODUCT                                      
FLTRESTM DS    XL1                 ESTIMATE                                     
FLTRRCNT DS    XL3                 RESPONSE COUNT                               
FLTRNTWK DS    CL3                 NETWORK                                      
FLTRINTG DS    XL4                 INTEGRATION                                  
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
*                                  DETAILS THAT ARE ON UPDATE SCREEN            
DETLLIST DS    0XL(NLSTLINS*DLSTLENQ)                                           
         DS    (NLSTLINS)XL(DLSTLENQ)                                           
*                                                                               
FLMTAB   DS    XL170                                                            
FLMTABX  EQU   *                                                                
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
SVDMELEM DS    XL(L'MELEM)         SAVED MINIO ELEMENT                          
*                                                                               
BUFFER   DS    XL(MXBUFNUM*(BUFFNEXT-BUFFCODE))                                 
BUFFERX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                                       
***********************************************************************         
NUMFLTRS EQU   2                   SO FAR 2 FILTER FLAGS                        
NLSTLINS EQU   14                  # OF LINES BTWN HEADING AND PFKEYS           
DLSTLENQ EQU   SNVIDSLN-SNVIDDAY   L(DETAIL ENTRY IN LIST)                      
MXBUFNUM EQU   100                 MAXIMUM NUMBER OF BUFFER ENTRIES             
***********************************************************************         
* ERROR EQUATES                                                                 
***********************************************************************         
FPRDERR  EQU   71                  FILM AND PRODUCT DO NOT AGREE                
FSLNERR  EQU   72                  FILM AND SPOT LENGTH DO NOT AGREE            
FLMCERR1 EQU   98                  INVALID FILM CODE                            
FLMCERR2 EQU   99                  INVALID SECOND FILM                          
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'227SPSNV07   05/03/11'                                      
         END                                                                    
