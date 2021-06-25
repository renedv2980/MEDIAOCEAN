*          DATA SET SPSNV02    AT LEVEL 226 AS OF 02/18/20                      
*PHASE T21002A                                                                  
*INCLUDE RANDOM                                                                 
***********************************************************************         
*                                                                               
*  TITLE: T21002 - MAINTENANCE OF INVOICES                                      
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T21000), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     SPSNVFD (T210FD) -- MAINTENANCE                                 
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - WORK                                                            
*          R8 - SECOND BASE                                                     
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T21002   TITLE 'SPSNV02 - SPOT INVOICE MAINT OVERLAY'                           
T21002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21002*,R8,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         BRAS  RE,SETUP                                                         
         GOTO1 =A(SETPFTBL),DMCB,(RC),RR=RELO                                   
*                                                                               
         TM    CTLRFLG1,CF1CKOFF          SAVE OFFSET OF SELECTED LINE?         
         BZ    T21002A                                                          
         MVC   SVDOFFST,SELOFFST          YES                                   
         NI    CTLRFLG1,X'FF'-CF1CKOFF    DON'T OVERWRITE NEXT TIME             
         MVC   LSTQMED,DISMED                                                   
         MVC   LSTQCLT,DISCLT                                                   
         MVC   LSTQSTA,DISSTA                                                   
         MVC   LSTQPER,DISPERD                                                  
         MVC   LSTQINV,DISINVC                                                  
*                                                                               
T21002A  DS    0H                                                               
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
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
         NI    MISCFLG1,X'FF'-MF1SDSTA                                          
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VKMED00  DS    0H                                                               
         LA    R2,DISMEDH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   VKMED10                                                          
         OI    MISCFLG1,MF1KYCHG+MF1RPCHG                                       
         NI    DISCLTH+4,X'FF'-X'20'                                            
         NI    DISSTAH+4,X'FF'-X'20'                                            
         NI    DISPERDH+4,X'FF'-X'20'                                           
         NI    DISINVCH+4,X'FF'-X'20'                                           
*                                                                               
VKMED10  CLI   5(R2),0                                                          
         BNE   VKMED20                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPMD                                 
         CLI   5(R2),0                                                          
         BE    PLSENTER                                                         
*                                                                               
VKMED20  GOTO1 VALIMED                                                          
         MVC   DISMDNM(L'MEDNM),MEDNM       SHOW MEDIA NAME                     
         OI    DISMDNMH+6,X'80'                                                 
*                                                                               
         CLI   BILLROPT,C'Y'       ARE WE DOING BILLER NAME?                    
         BE    VKMED30                                                          
         OI    DISABTXH+1,X'0C'    HIDE AGENCY BILLER FIELD                     
         XC    DISABIL,DISABIL                                                  
         OI    DISABILH+1,X'20'    PROTECT INPUT FIELD                          
         B     VKMED35                                                          
*                                                                               
VKMED30  NI    DISABTXH+1,X'FF'-X'0C'   SHOW AGENCY BILLER FIELD                
         NI    DISABILH+1,X'FF'-X'20'   UNPROTECT INPUT FIELD                   
*                                                                               
VKMED35  OI    DISABTXH+6,X'80'                                                 
         OI    DISABILH+6,X'80'                                                 
*                                                                               
VKMED40  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPMD                                 
         OI    4(R2),X'20'                                                      
*                                                                               
VKMEDX   DS    0H                                                               
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VKCLT00  DS    0H                                                               
         LA    R2,DISCLTH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG+MF1RPCHG                                       
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKCLT10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPCLT                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKCLT10  GOTO1 VALICLT                                                          
         MVC   DISCLNM,CLTNM       SHOW CLIENT NAME                             
         OI    DISCLNMH+6,X'80'                                                 
*                                                                               
VKCLTX   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPCLT                                
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  DS    0H                                                               
         LA    R2,DISSTAH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG+MF1RPCHG                                       
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKSTA10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPSTA                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKSTA10  GOTO1 VALISTA                                                          
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         MVC   SVSTYPE,STYPE                                                    
         MVC   SVNETINV,STNETINV                                                
         DROP  R1                                                               
*                                                                               
         TM    SVSFLAG1,STPG       P&G STATION?                                 
         BZ    *+12                NO - PROCEED AS USUAL                        
         TM    SVCOPT4,COP4PG      P&G CLIENT?                                  
         BZ    CLTPGER             NO - ONLY P&G CLIENTS ALLOWED                
*                                                                               
         BRAS  RE,READPROF         READ I2,I2P,A0A PROFILES                     
*                                                                               
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,SPACES                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   DISSTNM,MKTNM       SHOW STATION NAME                            
         OI    DISSTNMH+6,X'80'                                                 
*                                                                               
* CHECK IF WE HAVE A DIGITAL STATION                                            
*                                                                               
VKSTAX   DS    0H                                                               
         BRAS  RE,CKDIGITL                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPSTA                                
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE PERIOD                                                           
*****                                                                           
VKPER00  DS    0H                                                               
         LA    R2,DISPERDH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG+MF1RPCHG                                       
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKPER10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPPER                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKPER10  XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLDH(L'DISPERDH+L'DISPERD),DISPERDH                          
*                                                                               
         XC    BLOCK(32*3),BLOCK                                                
         GOTO1 SCANNER,DMCB,(0,(R2)),(X'03',BLOCK),C',=-='                      
         CLI   DMCB+4,2                                                         
         BH    INVLFLD                                                          
         BL    VKPER15                                                          
         CLI   BLOCK+32,1                                                       
         BH    INVLFLD                                                          
         CLI   BLOCK+33,0                                                       
         BH    INVLFLD                                                          
         CLI   BLOCK+44,C'B'       BROADCAST MONTHS OVERRIDE?                   
         BE    *+12                                                             
         CLI   BLOCK+44,C'C'       CALENDAR MONTHS OVERRIDE                     
         BNE   INVLFLD                                                          
         MVC   P0I2BC(1),BLOCK+44  OVERRIDE WHAT IS IN PROFILE                  
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,BLOCK                                                         
         STC   R1,FAKEFLDH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),BLOCK+12                                              
*                                                                               
VKPER15  LA    R1,FAKEFLD                                                       
         ST    R1,DMCB                                                          
         MVC   DMCB(1),FAKEFLDH+5                                               
*                                                                               
VKPER20  GOTO1 PERVAL,DMCB,,PERVALST                                            
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
*                                                                               
VKPVALD  USING PERVALD,PERVALST                                                 
* WAS THE START DAY OR ANY PART OF THE END DAY ENTERED?                         
         TM    VKPVALD.PVALASSM,PVALASD+PVALAED+PVALAEM+PVALAEY                 
         BNO   BADDTFMT                WITHOUT THE DAY                          
*                                                                               
         MVC   BMOSS,VKPVALD.PVALCSTA     COMPRESSED DATES                      
         MVC   BMOSE,VKPVALD.PVALCEND                                           
*                                                                               
         MVC   VKPVALD.PVALESTA+4(2),=C'15'                                     
         GOTO1 GETBROAD,DMCB,(1,VKPVALD.PVALESTA),BRDDATES,GETDAY,ADDAY         
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                DATE SHOULDN'T BE INVALID                    
         DROP  VKPVALD                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(X'10',BRDDATES),(2,WORK)                            
         MVC   BRDCSDAT,WORK                                                    
         MVC   BRDCEDAT,WORK+3                                                  
*&&DO                                                                           
VKPERX   XC    8(L'DISPERD,R2),8(R2)                                            
         GOTO1 DATCON,DMCB,(2,BMOSS),(9,8(R2))                                  
         MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
*&&                                                                             
VKPERX   LA    R1,FAKEFLD                                                       
         ST    R1,DMCB+4                                                        
         ZIC   R1,FAKEFLDH+5                                                    
         ST    R1,DMCB+8                                                        
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',,,GLVSPPER                                    
         OI    4(R2),X'20'                                                      
*                                                                               
         XC    WORK,WORK           SHOW PERIOD DATES                            
         MVC   WORK(L'BMOSS+L'BMOSE),BMOSS                                      
         CLI   P0I2BC,C'C'         CALENDAR MONTHS?                             
         BE    *+10                                                             
         MVC   WORK(L'BRDCSDAT+L'BRDCEDAT),BRDCSDAT                             
*                                                                               
         OI    DISPDNMH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(X'12',WORK),(8,DISPDNM)                             
         GOTO1 (RF),(R1),(2,WORK),(0,PERDSYMD)                                  
         GOTO1 (RF),(R1),(2,WORK+L'BMOSS),(0,PERDEYMD)                          
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKINV00  DS    0H                                                               
         CLI   ACTNUM,ACTADD       ADDING AN INVOICE HEADER?                    
         BNE   VKINV01                                                          
         CLI   CALLSTCK,X'FC'      FROM DETAIL SCREEN?                          
         BNE   VKINV01                                                          
         MVI   CALLSTCK,0          YES, WON'T COME BACK HERE AGAIN              
         LA    R2,DISCLTH                                                       
         B     PLSENTER                                                         
*                                                                               
VKINV01  LA    R2,DISINVCH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG+MF1RPCHG                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    RE,DISINVC          DISALLOW '.' OR ',' IN INV NUMBER            
         ZIC   RF,DISINVCH+5                                                    
*                                                                               
         CLI   ACTNUM,ACTADD       ADDING AN INVOICE HEADER?                    
         BNE   VKINV01A                                                         
*                                                                               
         CLI   0(RE),C' '                                                       
         BE    INVLFLD                                                          
*                                                                               
VKINV01A CLI   0(RE),C'.'                                                       
         BE    INVLFLD                                                          
         CLI   0(RE),C','                                                       
         BE    INVLFLD                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VKINV01A                                                      
*                                                                               
         MVC   QINVOICE,DISINVC                                                 
         OC    QINVOICE,SPACES                                                  
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
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),INVDIR,KEY,AIO                   
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLI   DMCB+8,2            RECORD DELETED?                              
         BNE   VKINV05             NO                                           
         CLC   SNVKMAST,KEY        YES, SAME RECORD?                            
         BE    VKINV02                                                          
         CLI   ACTNUM,ACTADD            NO, ACTION ADD?                         
         BE    VKINV07                      YES, OKAY                           
         B     RECNTFND                     NO, RECORD NOT FOUND                
*                                                                               
VKINV02  CLI   ACTNUM,ACTREST           YES, RESTORE IT?                        
         BNE   RECDLTED                      NO, RECORD IS DELETED              
         B     VKINV60                       YES, GIVE THE KEY TO MINIO         
*                                                                               
VKINV05  CLI   DMCB+8,0            PROBLEM WITH DATAMGR?                        
         BE    *+6                                                              
         DC    H'0'                YES, DIE                                     
*                                                                               
VKINV07  CLC   SNVKMAST,KEY        RECORD EXISTS?                               
         BE    VKINV50             YES                                          
*                                                                               
         CLI   ACTNUM,ACTADD       RECORD NOT FOUND IF NOT ADD                  
         BNE   RECNTFND                                                         
         MVC   SVMASTKY(L'SNVKMAST),SNVKMAST                                    
         CLI   DISDTL1H+5,0        ANY INPUT IN DETAIL LINES?                   
         BNE   VKINVX              YES, NO NEED TO READ PREV MONTH'S            
         CLI   DISDTL2H+5,0                                                     
         BNE   VKINVX                                                           
         GOTO1 =A(COPYDTLS),DMCB,(RC),RR=RELO                                   
*                                                                               
         LA    RE,DISDTL1H         SET THE CORRECT LENGTHS IN                   
VKINV10  LA    R0,DISDTL2H            THE HEADERS                               
         CR    RE,R0                                                            
         BH    VKINV30                                                          
*                                                                               
         CLC   8(L'DISDTL1,RE),SPACES   ANY INPUT ON THIS LINE                  
         BNH   VKINV20                                                          
         LA    R1,L'DISDTL1        YES, CALCULATE THE LENGTH                    
         BCTR  R1,0                                                             
VKINV15  LA    RF,8(R1,RE)                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   R1,VKINV15                                                       
         LA    R1,1(R1)                                                         
         STC   R1,5(RE)            SAVE LENGTH OF THE DETAIL INPUT              
*                                                                               
VKINV20  ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         B     VKINV10                                                          
*                                                                               
VKINV30  XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKMAST),SVMASTKY   REESTABLISH THE INVOICE KEY           
         B     VKINVX                                                           
*                                                                               
VKINV50  CLI   ACTNUM,ACTADD       CAN'T ADD IF IT EXISTS ALREADY               
         BE    RECXISTS                                                         
         CLI   ACTNUM,ACTREST      RESTORE RECORD?                              
         BE    RCNOTDEL            YES, RECORD IS NOT DELETED                   
*                                                                               
VKINV60  MVC   SVMASTKY(L'SNVKMAST),SNVKMAST                                    
*                                                                               
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VKINVX              THEN NO D/A                                  
         MVC   CONHED2(4),=C'D/A='                                              
         GOTO1 HEXOUT,DMCB,SNVDDA,CONHED2+4,L'SNVDDA                            
         OI    CONHED2H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
VKINVX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VREC     DS    0H                                                               
         XC    SVLBLK(SVLBLKLQ),SVLBLK                                          
*                                                                               
         GOTO1 INITMNIO                                                         
*                                                                               
         CLI   PI2NAI2A,C'Y'                                                    
         BNE   *+12                                                             
         LA    R1,SVIHORIG                                                      
         BRAS  RE,BLDIHBLK                                                      
*                                                                               
         GOTO1 CKSPCLI2                                                         
         BNE   *+12                                                             
         LA    R1,SVIHORIG                                                      
         BRAS  RE,BLDIHBLK                                                      
*                                                                               
         XC    ORIGPRD(L'ORIGPRD*2+L'ORIGQPRD*2),ORIGPRD                        
         MVI   ORIGEST,0                                                        
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   *+8                                                              
         BRAS  RE,CHKINT                                                        
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    VREC01                                                           
         CLI   ACTNUM,ACTDEL                                                    
         BE    VREC01                                                           
         CLI   ACTNUM,ACTREST                                                   
         BNE   VREC05                                                           
         MVI   MINDELSW,C'Y'       PROCESS DELETED RECORD SET                   
*                                                                               
VREC01   DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    VREC02                                                           
         CLI   MINERR,MINERNF                                                   
         BE    RECDLTED                                                         
         CLI   MINERR,MINESNF                                                   
         BE    RECDLTED                                                         
         DC    H'0'                                                             
*                                                                               
VREC02   DS    0H                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BNE   RECDLTED                                                         
*                                                                               
         MVC   SVCTRLB,SNVHDCTL    SAVE CONTROL BYTE                            
*                                                                               
         MVC   ORIGEST,SNVHDEST                                                 
         MVC   ORIGPRD,SNVHDPRD                                                 
         MVC   ORIGPR2,SNVHDPR2                                                 
*****                                                                           
         CLI   SNVHDLEN,SNVHDLN2   WE HAVE ALPHA PRODUCTS?                      
         BNH   VREC03              NO, WE DON'T                                 
         MVC   ORIGQPRD,SNVHDAP1                                                
         MVC   ORIGQPR2,SNVHDAP2                                                
*****                                                                           
VREC03   XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BNE   VREC05                                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ       ANY DETAILS?                              
         BNE   VREC05                 NO                                        
         OI    MISCFLG1,MF1GTDTL      WE HAVE INVOICE DETAILS                   
         DROP  R6                                                               
*                                                                               
VREC05   DS    0H                   CHECK EST LOCK FOR ACT=DEL,REST             
         MVI   MINDELSW,C'N'       PROCESS DELETED RECORD SET                   
         CLI   ACTNUM,ACTDEL                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VREC06                                                           
*                                                                               
         XC    SVELOCK1,SVELOCK1                                                
         XC    SVELOCK2,SVELOCK2                                                
*                                                                               
         BRAS  RE,GETEST                                                        
         BE    *+12                                                             
         LA    R2,DISESTMH                                                      
         B     BADESTM                                                          
*                                                                               
         BRAS  RE,CHKEST                                                        
         BE    VREC06                                                           
         CLI   BYTE,X'01'                                                       
         BE    INVPAID                                                          
         B     ESTLOCK                                                          
*                                                                               
VREC06   DS    0H                   CHECK EST LOCK FOR ACT=DEL,REST             
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VREC07                                                           
         BRAS  RE,WRTIMGR                                                       
         BRAS  RE,DELREC                                                        
         BNE   ERREXIT                                                          
         B     XIT                                                              
*                                                                               
VREC07   DS    0H                                                               
         CLI   ACTNUM,ACTREST                                                   
         BNE   *+12                                                             
         BRAS  RE,RESTREC                                                       
         B     XIT                                                              
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         CLI   PFKEY,0             IF JUST A HIT OF ENTER                       
         BNE   VREC10                                                           
         CLC   TIOBFRST,TIOBLAST   WITHOUT ANY CHANGES                          
         BNE   VREC10                                                           
         CLI   CALLSP,0                                                         
         BE    VREC10                                                           
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED OR NOT VALIDATED YET?            
         BNZ   VREC10              YES                                          
         MVI   PFKEY,12            NO, RETURN BACK IF FROM SOMEWHERE            
         GOTO1 =A(SETPFTBL),DMCB,(RC),RR=RELO                                   
         DROP  R1                                                               
*                                                                               
VREC10   CLI   ACTNUM,ACTDIS       DISPLAY THE RECORD?                          
         BE    DREC                YES                                          
*                                                                               
         CLI   ACTNUM,ACTADD       ADDING THE RECORD?                           
         BNE   *+8                 NO                                           
         NI    MISCFLG1,X'FF'-MF1GTDTL   NEW INVOICE CAN'T HAVE DETAILS         
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    VREC20              NO                                           
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   DREC                                                             
*                                                                               
VREC20   XC    SVAREA(SVAREAX-SVAREA),SVAREA                                    
         BRAS  RE,TSTLOCK                                                       
         BE    VRDAT00                                                          
         MVC   GERROR,=AL2(808)                                                 
         LA    R2,DISMEDH                                                       
         B     ERREXIT                                                          
*****                                                                           
* VALIDATE THE INVOICE DATE                                                     
*****                                                                           
VRDAT00  XC    SVINVDAT,SVINVDAT                                                
         LA    R2,DISDATEH                                                      
*                                                                               
         BRAS  RE,INVPROF                                                       
         BNE   VRDDT00                                                          
*                                                                               
VRDAT01  LA    R0,8(R2)                                                         
         ST    R0,DMCB                                                          
         MVC   DMCB(1),5(R2)                                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(X'60',PERVALST)                                    
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
         MVC   SVINVDAT,PVALCSTA                                                
         DROP  R3                                                               
*****                                                                           
* VALIDATE THE INVOICE DUE DATE                                                 
*****                                                                           
VRDDT00  XC    SVDUEDAT,SVDUEDAT                                                
         LA    R2,DISDDATH                                                      
         CLI   5(R2),0                                                          
         BE    VRCON00                                                          
*                                                                               
         LA    R0,8(R2)                                                         
         ST    R0,DMCB                                                          
         MVC   DMCB(1),5(R2)                                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(X'60',PERVALST)                                    
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
         MVC   SVDUEDAT,PVALCSTA                                                
         DROP  R3                                                               
*****                                                                           
* CONTRACT NUMBER                                                               
*****                                                                           
VRCON00  XC    SVCONNUM,SVCONNUM                                                
         LA    R2,DISCONTH                                                      
         CLI   5(R2),0                                                          
         BE    VRTCS00                                                          
*                                                                               
         MVC   SVCONNUM,DISCONT                                                 
         OC    SVCONNUM,SPACES                                                  
*****                                                                           
* TOTAL COST                                                                    
*****                                                                           
VRTCS00  ZAP   SVTOTCOS,=P'0'                                                   
         LA    R2,DISTOTLH                                                      
         CLI   5(R2),0                                                          
         BE    VRTSP00                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BE    INVLFLD             INVALID FIELD                                
         ZAP   SVTOTCOS,DMCB+4(8)                                               
         CP    SVTOTCOS,=P'2000000000'                                          
         BH    INVLFLD                                                          
*****                                                                           
* TOTAL SPOTS                                                                   
*****                                                                           
VRTSP00  XC    SVTOTSPT,SVTOTSPT                                                
         LA    R2,DISSPTSH                                                      
         CLI   5(R2),0                                                          
         BE    VRTAX00                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),(R3)                                   
         CLI   0(R1),X'FF'                                                      
         BE    INVLFLD             INVALID FIELD                                
         MVC   SVTOTSPT,DMCB+6                                                  
*                                                                               
VRTAX00  DS    0H                                                               
         XC    SVTAXAMT,SVTAXAMT                                                
         CLI   DISPTAX,C'N'                                                     
         BE    VRPRD00                                                          
*                                                                               
         LA    R2,DISTAXAH                                                      
         ZIC   R3,5(R2)                                                         
         CHI   R3,0                                                             
         BE    VRPRD00                                                          
         GOTO1 CASHVAL,DMCB,(X'02',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BE    INVLFLD             INVALID FIELD                                
         MVC   SVTAXAMT,DMCB+4                                                  
*                                                                               
*****                                                                           
* VALIDATE THE INVOICE PRODUCT                                                  
*****                                                                           
VRPRD00  LA    R2,DISPRODH                                                      
         XC    SVQPRDCD,SVQPRDCD                                                
         MVI   SVPRDCOD,0                                                       
         XC    SVQPR2CD,SVQPR2CD                                                
         MVI   SVPR2COD,0                                                       
*                                                                               
         CLC   =CL3'AAA',DISPROD                                                
         BNE   VRPRD03                                                          
         CLI   5(R2),3                                                          
         BNE   VRPRD03                                                          
         MVC   GERROR,=AL2(AAANOGO)   PRODUCT AAA NOT ALLOWED (992)             
         B     ERREXIT2                                                         
*                                                                               
VRPRD03  DS    0H                                                               
*        CLC   =CL3'POL',DISPROD                                                
*        BNE   VRPRD05                                                          
*        CLI   5(R2),3                                                          
*        BNE   VRPRD05                                                          
*        MVC   GERROR,=AL2(POLNOGO)   PRODUCT POL NOT ALLOWED (102)             
*        B     ERREXIT2                                                         
*                                                                               
VRPRD05  TM    4(R2),X'80'         IF THIS FIELD CHANGES                        
         BZ    *+8                                                              
         OI    MISCFLG1,MF1RPCHG   THEN WE COULD DO A NEW TURNAROUND            
*        MVI   CHGFLAG,1                                                        
*                                                                               
         CLI   5(R2),0             IF NO INVOICE PRODUCT                        
         BNE   VRPRD10                                                          
         XC    DISPRNM,DISPRNM     CLEAR THE PRODUCT NAME                       
         OI    DISPRNMH+6,X'80'                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPRD                                    
         B     VRPRDX                                                           
*                                                                               
VRPRD10  XCEFL BLOCK,480           CLEAR THE BLOCK                              
*                                                                               
         ZIC   RE,5(R2)                                                         
         LA    RF,8(R2)                                                         
VRPRD10A CLI   0(RF),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         BCT   RE,VRPRD10A                                                      
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'82',BLOCK),C',=-='                          
         CLI   DMCB+4,0                                                         
         BE    BADPROD                                                          
*                                                                               
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
         MVC   FAKEFLDH,DISPRODH                                                
         MVC   FAKEFLDH+5(1),0(R3)                                              
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD                                                          
         MVC   SVPRDCOD,BPRD       SAVE PRIMARY PRODUCT CODE HERE FIRST         
         MVC   SVQPRDCD,QPRD                                                    
         MVC   DISPRNM(L'PRDNM),PRDNM                                           
         OI    DISPRNMH+6,X'80'                                                 
*                                                                               
         LA    R3,32(R3)           R3 = A(PIGGYBACK PRODUCT) IF ANY             
         CLI   0(R3),0                                                          
         BE    VRPRD20                                                          
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
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
         MVC   FAKEFLDH+5(1),0(R3) GIVE IT THE PROPER LENGTH                    
         XC    FAKEFLD,FAKEFLD                                                  
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R3)                                                
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPR2COD,BPRD                                                    
         MVC   SVQPR2CD,QPRD                                                    
*                                                                               
         MVI   DISPRNM+L'PRDNM,C'-'     SHOW PIGGY'S NAME TOO                   
         MVC   DISPRNM+L'PRDNM+1(L'PRDNM),PRDNM                                 
*                                                                               
VRPRD20  CLC   ORIGPRD,SVPRDCOD    THE BINARIES PRODUCTS CAN BE ZERO            
         BNE   VRPRD25               AND STILL BE CONSIDERED A CHANGE           
         CLC   ORIGPR2,SVPR2COD      AS THE PRODUCT CODE MIGHT BE > 255         
         BNE   VRPRD25                                                          
         CLC   ORIGQPRD,SVQPRDCD                                                
         BNE   VRPRD25                                                          
         CLC   ORIGQPR2,SVQPR2CD                                                
         BE    *+8                                                              
VRPRD25  MVI   CHGFLAG,1                                                        
*                                                                               
         L     R1,ATIOB            TAKE OFF ERROR MESSAGE CURSOR                
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
VRPRD30  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPPRD                                
*                                                                               
VRPRDX   DS    0H                                                               
*****                                                                           
* VALIDATE THE INVOICE ESTIMATE                                                 
*****                                                                           
VREST00  LA    R2,DISESTMH         VALIDATE THE INVOICE ESTIMATE                
*                                                                               
         MVI   SVESTNUM,0                                                       
         XC    SVELOCK1,SVELOCK1                                                
         XC    SVELOCK2,SVELOCK2                                                
*                                                                               
         TM    4(R2),X'80'         IF THIS FIELD CHANGES                        
         BZ    *+8                                                              
         OI    MISCFLG1,MF1RPCHG   THEN WE COULD DO A NEW TURNAROUND            
*        MVI   CHGFLAG,1                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VREST10                                                          
         XC    DISESNM,DISESNM     CLEAR ESTIMATE NAME IF NO ESTIMATE           
         OI    DISESNMH+6,X'80'                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPEST                                    
         B     VRESTX                                                           
*                                                                               
VREST10  CLI   DISPRODH+5,0        GOT A GLOBAL PRODUCT?                        
         BNE   *+12                                                             
         LA    R2,DISPRODH         NO, THEN WE CAN'T HAVE A GLOBAL EST          
         B     MISSFLD                                                          
*                                                                               
         MVC   QPRD,SVQPRDCD       VALIDATE ESTIMATE FOR PRIMARY PRD            
         GOTO1 VALIEST                                                          
*                                                                               
         L     R1,AIO                                                           
         MVC   SVECNTR1,ECNTRL-ESTHDR(R1)                                       
         MVC   SVELKYM1,ELOCKYR-ESTHDR(R1)                                      
*                                                                               
         MVC   DISESNM(L'ESTNM),ESTNM                                           
         OI    DISESNMH+6,X'80'                                                 
         MVC   SVESTNUM,BEST                                                    
*                                                                               
         CLC   SVESTNUM,ORIGEST                                                 
         BE    *+8                                                              
         MVI   CHGFLAG,1                                                        
*                                                                               
         CLC   ESTSTRT,PERDEYMD    MAKE SURE PERIOD INTERSECTS                  
         BH    BADDTEST                ESTIMATE'S PERIOD                        
         CLC   ESTEND,PERDSYMD                                                  
         BL    BADDTEST                                                         
*                                                                               
         MVC   INTESTSD,ESTSTRT                                                 
         MVC   INTESTED,ESTEND                                                  
*                                                                               
         CLI   SVPR2COD,0          ANY PIGGYBACK?                               
         BNE   *+14                                                             
         XC    SVELOCK2,SVELOCK2                                                
         B     VREST20             NO                                           
*                                                                               
         MVC   QPRD,SVQPR2CD       VALIDATE ESTIMATE FOR THIS PIGGYBACK         
         GOTO1 VALIEST                                                          
*                                                                               
         L     R1,AIO                                                           
         MVC   SVECNTR2,ECNTRL-ESTHDR(R1)                                       
         MVC   SVELKYM2,ELOCKYR-ESTHDR(R1)                                      
*                                                                               
         MVI   DISESNM+L'ESTNM,C'-'     SHOW PIGGY'S ESTIMATE NAME TOO          
         MVC   DISESNM+L'ESTNM+1(L'ESTNM),ESTNM                                 
*                                                                               
         CLC   ESTSTRT,PERDEYMD    MAKE SURE PERIOD INTERSECTS                  
         BH    BADDTEST                ESTIMATE'S PERIOD                        
         CLC   ESTEND,PERDSYMD                                                  
         BL    BADDTEST                                                         
*                                                                               
         CLC   INTESTSD,ESTSTRT    FIND THE INTERSECTION OF BOTH                
         BH    *+10                  ESTIMATE'S PERIODS                         
         MVC   INTESTSD,ESTSTRT                                                 
         CLC   INTESTED,ESTEND                                                  
         BL    *+10                                                             
         MVC   INTESTED,ESTEND                                                  
*                                                                               
         CLC   INTESTSD,INTESTED   ESTIMATE'S PERIODS DON'T CROSS?              
         BH    BADDTEST            NO, THEN WE HAVE A PROBLEM                   
*                                                                               
VREST20  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPEST                                
*                                                                               
VRESTX   DS    0H                                                               
*****                                                                           
* VALIDATE THE REP FIELD                                                        
*****                                                                           
VRREP00  XC    SVREP,SVREP                                                      
         LA    R2,DISREPH                                                       
         CLI   5(R2),0                                                          
         BE    VRREPX                                                           
*                                                                               
         MVC   REPRECD,=15C'0'                                                  
         LA    R3,REPRECD                                                       
         USING REPREC,R3                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,DISMED      TAKE MEDIA FROM SCREEN                       
         MVC   REPKREP,DISREP                                                   
         MVC   REPKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',STAFIL,REPRECD,AIO3                      
         L     R3,AIO3                                                          
         CLC   REPRECD,0(R3)                                                    
         BNE   BADREP                                                           
         MVC   SVREP,DISREP                                                     
*                                                                               
VRREPX   DS    0H                                                               
         GOTO1 HIGH                                                             
         DROP  R3                                                               
*                                                                               
*****                                                                           
* VALIDATE THE PACKAGE FIELD                                                    
*****                                                                           
VRPKG00  DS    0H                                                               
*                                                                               
         LA    R2,DISPKGH                                                       
         BRAS  RE,VALPKG                                                        
         BNE   INVLFLD                                                          
*                                                                               
VRPKGX   DS    0H                                                               
*                                                                               
*****                                                                           
* VALIDATE THE DEMO CATEGORY FIELD                                              
*****                                                                           
VRDEM00  DS    0H                                                               
         LA    R2,DISDEMOH                                                      
         BRAS  RE,VALDEM                                                        
         BNE   INVLFLD                                                          
VRDEMX   DS    0H                                                               
*                                                                               
*****                                                                           
* VALIDATE THE DETAIL FIELDS AND THEIR ORDER                                    
*****                                                                           
VRDTO00  DS    0H                                                               
         MVI   SVFLDCNT,0          NO FIELDS YET                                
         XC    SVFLDLST,SVFLDLST                                                
         NI    MISCFLG1,X'FF'-MF1RESPN                                          
         LA    R2,DISDTL1H         R2 = A(LINE TO VALIDATE)                     
*                                                                               
VRDTO10  LA    R0,DISDTL2H         DID WE CHECK BOTH DETAIL LINES?              
         CR    R2,R0                                                            
         BH    VRDTO50             YES                                          
*                                                                               
         CLI   5(R2),0             NOTHING ON THIS LINE?                        
         BE    VRDTONXL            CHECK NEXT LINE                              
*                                                                               
         L     R1,ATIOB            SET CURSOR ON ERROR                          
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         XCEFL BLOCK,480           CLEAR THE BLOCK                              
         GOTO1 SCANNER,DMCB,(R2),(X'8F',BLOCK)                                  
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
*                                                                               
         LA    RE,BLOCK                                                         
VRDTO20  CLI   0(RE),0             ANY MORE FIELDS FOR THIS LINE?               
         BE    VRDTONXL            NO, CHECK THIS LINE                          
*                                                                               
         CLI   1(RE),0             SUB-FIELDS ARE NOT ALLOWED                   
         BE    VRDTO30                                                          
VRDTOINV L     R1,ATIOB            ERROR WITH CURSOR ON CORRECT FIELD           
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(RE)                                                   
         DROP  R1                                                               
         B     INVLFLD                                                          
*                                                                               
VRDTO30  LARL  R1,FLDEQTBL                                                      
VRDTO33  CLI   0(R1),0             REACHED END OF TABLE?                        
         BE    VRDTOINV            YES, INVALID FIELD                           
*                                                                               
         ZIC   RF,0(RE)            NO, MATCHED ON THIS ENTRY?                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,RE),2(R1)                                                   
         BNE   *+12                                                             
         TM    1(R1),X'20'         DISPLAY-ONLY FIELD?                          
         BZ    *+12                                                             
         LA    R1,L'FLDEQTBL(R1)       NO, CHECK NEXT ENTRY                     
         B     VRDTO33                                                          
*                                                                               
         LA    R3,SVFLDLST         R3 = A(FIELD EQUATE LIST)                    
         CLI   SVFLDCNT,0          NOTHING IN THIS LIST?                        
         BE    VRDTO40             PUT FIELD IN THE LIST THEN                   
         CLI   SVFLDCNT,MAXFLDEQ   OVERFLOW THE LIST?                           
         BE    VRDTOINV                                                         
*                                                                               
         ZIC   R0,SVFLDCNT                                                      
VRDTO36  CLC   0(1,R3),0(R1)       DETAIL FIELD IN LIST ALREADY?                
         BE    VRDTOINV            YES                                          
         LA    R3,1(R3)                                                         
         BCT   R0,VRDTO36                                                       
*                                                                               
VRDTO40  CLI   NETPAKSW,C'Y'       NET MEDIA?                                   
         BE    *+12                                                             
         TM    1(R1),X'40'         NO, NET FIELD?                               
         BNZ   VRDTOINV                YES, CAN'T HAVE THIS                     
*                                                                               
         TM    MISCFLG1,MF1SDSTA   DIGITAL STATION?                             
         BO    *+12                                                             
         TM    1(R1),X'10'         DIGITAL-ONLY FIELD?                          
         BO    VRDTOINV                                                         
*                                                                               
         CLI   0(R1),FLDNRCNT      RESPONSE COUNT FIELD?                        
         BNE   VRDTO40B                                                         
         CLI   SVFLDCNT,0          NO FIELD IN LIST?                            
         BNE   *+12                                                             
VRDTO40A OI    MISCFLG1,MF1RESPN   RESPONSE COUNT NOW IN LIST                   
         B     VRDTO48                                                          
         ZIC   R0,SVFLDCNT                                                      
         LA    RF,SVFLDLST                                                      
         CLI   0(RF),FLDNCOST      MAKE SURE COST NOT IN LIST                   
         BE    VRDTOINV                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     VRDTO40A                                                         
*                                                                               
VRDTO40B CLI   0(R1),FLDNCOST      COST FIELD?                                  
         BNE   VRDTO41                                                          
         TM    MISCFLG1,MF1RESPN   MAKE SURE RESPONSE COUNT NOT IN LIST         
         BZ    VRDTO48                                                          
         B     VRDTOINV                                                         
*                                                                               
VRDTO41  CLI   0(R1),FLDNPROD      PRODUCT FIELD?                               
         BNE   VRDTO42                                                          
         CLI   DISPRODH+5,0        GOT ONE IN THE HEADER ALREADY?               
         BNE   VRDTOINV            YES, SHOULDN'T BE IN THE LIST THEN           
         B     VRDTO48                                                          
*                                                                               
VRDTO42  CLI   0(R1),FLDNESTM      ESTIMATE FIELD?                              
         BNE   VRDTO43                                                          
         CLI   DISESTMH+5,0        GOT ONE IN THE HEADER ALREADY?               
         BNE   VRDTOINV            YES, SHOULDN'T BE IN THE LIST THEN           
*                                                                               
         CLI   DISPRODH+5,0        CHECK IF WE HAVE PRODUCT COLUMN?             
         BNE   VRDTO48             NO, GOT A GLOBAL PRODUCT                     
*                                                                               
         LA    RF,SVFLDLST                                                      
VRDTO42A CLI   0(RF),0             PRODUCT BEFORE ESTIMATE?                     
         BNE   VRDTO42B                                                         
         L     R1,ATIOB            ERROR WITH CURSOR ON CORRECT FIELD           
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(RE)                                                   
         DROP  R1                                                               
         B     NDPRDEST            NO, PRODUCT NOT IN LIST YET                  
*                                                                               
VRDTO42B CLI   0(RF),FLDNPROD                                                   
         BE    VRDTO48             YES, THEN IT'S OKAY                          
         LA    RF,1(RF)                                                         
         B     VRDTO42A                                                         
*                                                                               
VRDTO43  CLI   0(R1),FLDNNTWK      NETWORK FIELD?                               
         BNE   VRDTO45                                                          
*                                                                               
         CLI   NETPAKSW,C'Y'       NETPAK?                                      
         BE    VRDTO48             YES, THEN IT'S ALRIGHT                       
*                                                                               
         CLI   BSTA,X'E8'          CABLE STATION?                               
         BL    VRDTOINV            NO, SHOULDN'T BE IN THE LIST THEN            
         TM    BSTA+2,X'7F'        YES, NETWORK SPECIFIED ALREADY?              
         BNZ   VRDTOINV                 YES, DON'T NEED NETWORK FIELD           
         B     VRDTO48                                                          
*                                                                               
VRDTO45  CLI   0(R1),FLDNFILM      FILMCODE FIELD?                              
         BNE   VRDTO46                                                          
         CLI   P0TIFCA,C'Y'        FILM CODES ACCEPTED?                         
         BNE   VRDTOINV            NO, SHOULDN'T BE IN THE LIST THEN            
         CLI   DISPRODH+5,0        CHECK IF WE HAVE PRODUCT COLUMN?             
         BNE   VRDTO48             NO, GOT A GLOBAL PRODUCT                     
*                                                                               
         LA    RF,SVFLDLST                                                      
VRDTO45A CLI   0(RF),0             PRODUCT BEFORE ESTIMATE?                     
         BNE   VRDTO45B                                                         
         L     R1,ATIOB            ERROR WITH CURSOR ON CORRECT FIELD           
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(RE)                                                   
         DROP  R1                                                               
         B     NDPRDFLM            NO, PRODUCT NOT IN LIST YET                  
*                                                                               
VRDTO45B CLI   0(RF),FLDNPROD                                                   
         BE    VRDTO48             YES, THEN IT'S OKAY                          
         LA    RF,1(RF)                                                         
         B     VRDTO45A                                                         
*                                                                               
VRDTO46  CLI   0(R1),FLDNPFLM      PIGGY FILM FIELD?                            
         BNE   VRDTO47                                                          
         CLI   P0TIFCA,C'Y'        FILM CODES ACCEPTED?                         
         BNE   VRDTOINV            NO, SHOULDN'T BE IN THE LIST THEN            
*                                                                               
         LA    RF,SVFLDLST         FILMCODE THERE FIRST?                        
VRDTO46A CLI   0(RF),0                                                          
         BE    NEEDF1ST            NO, WE NEED IT FIRST BEFORE 2NDFILM          
         CLI   0(RF),FLDNFILM                                                   
         BE    VRDTO48                                                          
         LA    RF,1(RF)                                                         
         B     VRDTO46A                                                         
*                                                                               
VRDTO47  CLI   0(R1),FLDNNPKG      PACKAGE FIELD?                               
         BNE   VRDTO48                                                          
         CLI   DISPKGH+5,0         GOT ONE IN THE HEADER ALREADY?               
         BNE   VRDTOINV            YES, SHOULDN'T BE IN THE LIST THEN           
*                                                                               
VRDTO48  MVC   0(1,R3),0(R1)           NO, COPY THE FIELD EQUATE                
         ZIC   R0,SVFLDCNT         INCREMENT NUMBER OF FIELDS IN LIST           
         AH    R0,=H'1'                                                         
         STC   R0,SVFLDCNT                                                      
                                                                                
         LA    RE,32(RE)           CHECK NEXT BLOCK ENTRY                       
         B     VRDTO20                                                          
*                                                                               
VRDTONXL LA    R2,DISDTL2H-DISDTL1H(R2)   GO TO THE NEXT LINE                   
         B     VRDTO10                                                          
*                                                                               
VRDTO50  L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
         CLI   SVFLDCNT,0          USE DEFAULT FIELDS?                          
         BNE   VRDTO50X            NO                                           
*****                                                                           
* DEFAULT LIST IS THE REQUIRED FIELDS                                           
*****                                                                           
         MVI   SVFLDCNT,4          YES, USE FIRST 4 FIELDS IN TABLE             
         ZIC   R1,SVFLDCNT                                                      
         LARL  R2,FLDEQTBL                                                      
         LA    R3,SVFLDLST                                                      
VRDTO51  MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
         LA    R2,L'FLDEQTBL(R2)                                                
         BCT   R1,VRDTO51                                                       
***************                                                                 
* CHECK THE DETAIL ORDER SCHEME FROM THE I2 PROFILE                             
***************                                                                 
         CLI   P0I2DTOR,C'B'       DANSER-SAATCHI  (D,T,L,P,C)                  
         BNE   VRDTO51X            NO, USE DEFAULT (D,T,L,C,P)                  
*********                                                                       
* PRODUCT BEFORE THE COST                                                       
*********                                                                       
         CLI   DISPRODH+5,0        CAN WE HAVE THE PRODUCT?                     
         BNE   VRDTO51X            NO                                           
         MVI   0(R3),FLDNCOST      YES, PUT COST AFTER                          
         BCTR  R3,0                                                             
         MVI   0(R3),FLDNPROD             THE PRODUCT FIELD                     
         LA    R3,2(R3)            POINT AFTER THE COST                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
         B     VRDTO52             CAN'T HAVE PRODUCT AGAIN                     
*                                                                               
VRDTO51X DS    0H                  DONE WITH SCHEME CHECKING                    
*                                                                               
         CLI   DISPRODH+5,0        IF WE HAVE A GLOBAL PRODUCT                  
         BNE   VRDTO52             THEN WE DON'T NEED PRODUCT FIELD             
         MVI   0(R3),FLDNPROD                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
VRDTO52  CLI   DISESTMH+5,0        IF WE HAVE A GLOBAL ESTIAMTE                 
         BNE   VRDTO53             THEN WE DON'T NEED ESTIMATE FIELD            
         CLI   PI2XERQ,C'Y'        IF ESTIMATE IS NOT REQUIRED                  
         BNE   VRDTO53             THEN SKIP IT                                 
         MVI   0(R3),FLDNESTM                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
VRDTO53  CLI   P0TIFCA,C'Y'        FILM CODES ACCEPTED?                         
         BNE   VRDTO54             NO, THEN WE DON'T NEED A FILM FIELD          
         MVI   0(R3),FLDNFILM                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
VRDTO54  CLI   NETPAKSW,C'Y'       DO WE NEED NETWORK FIELD?                    
         BE    VRDTO55             NO                                           
         CLI   BSTA,X'E8'          CABLE STATION?                               
         BL    VRDTO55             NO                                           
         TM    BSTA+2,X'7F'        YES, GOT NETWORK ALREADY?                    
         BNZ   VRDTO55                  YES                                     
         MVI   0(R3),FLDNNTWK                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
VRDTO55  B     VRDTO80                                                          
*                                                                               
VRDTO50X LARL  R1,FLDEQTBL         MAKE SURE ALL THE REQUIRED FIELDS            
         LA    R2,DISDTL1H             ARE IN THE LIST                          
*                                                                               
VRDTO60  CLI   0(R1),0             IF END OF TABLE THEN LIST IS GOOD            
         BE    VRDTO80                                                          
*                                                                               
         ZIC   RE,SVFLDCNT         RE = NUMBER OF ENTRIES IN LIST               
         LA    RF,SVFLDLST         RF = A(1ST ENTRY IN LIST)                    
*                                                                               
VRDTO62  TM    1(R1),X'80'         IS THIS A REQUIRED FIELD?                    
         BZ    VRDTO78             NO                                           
*                                                                               
VRDTO64  CLC   0(1,RF),0(R1)       THIS FIELD HAS TO BE IN THE LIST             
         BE    VRDTO78                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,VRDTO64                                                       
*                                                                               
         MVI   BLOCK,9             L(MESSAGE)  - LENGTH NOT INCL.               
         MVC   BLOCK+1(8),2(R1)    MESSAGE                                      
         MVI   BLOCK+9,0           TERMINATING 0                                
***************                                                                 
* FIELD IS NOT IN THE LIST.  THERE MUST BE A REASON WHY IT'S NOT                
***************                                                                 
         CLI   0(R1),FLDNCOST      COST FIELD?                                  
         BNE   VRDTO65                                                          
         TM    MISCFLG1,MF1RESPN   GOT RESPONSE COUNT FIELD IN LIST?            
         BZ    MISSAFLD            NO, MISSING COST OR RESPONSE                 
         B     VRDTO78             MISSING COST FIELD BUT HAVE RESPONSE         
*                                                                               
VRDTO65  CLI   0(R1),FLDNPROD      PRODUCT FIELD?                               
         BNE   VRDTO66                                                          
         CLI   DISPRODH+5,0        YES, GOT ONE IN THE HEADER ALREADY?          
         BE    MISSAFLD                 NO, THEN IT SHOULD BE IN LIST           
         B     VRDTO78                                                          
*                                                                               
VRDTO66  CLI   0(R1),FLDNESTM      ESTIMATE FIELD?                              
         BNE   VRDTO67                                                          
         CLI   DISESTMH+5,0        YES, GOT ONE IN THE HEADER ALREADY?          
         BNE   VRDTO78                  YES                                     
         CLI   PI2XERQ,C'Y'             NO, IS IT REQUIRED IN PROFILE?          
         BE    MISSAFLD                     YES, MISSING FIELD                  
         B     VRDTO78                                                          
*                                                                               
VRDTO67  CLI   0(R1),FLDNNTWK      NETWORK FIELD?                               
         BNE   VRDTO68                                                          
         CLI   NETPAKSW,C'Y'       NETPAK?                                      
         BE    VRDTO78             YES, THEN IT'S ALRIGHT                       
         CLI   BSTA,X'E8'          CABLE STATION?                               
         BL    VRDTO78             NO, THEN IT'S ALRIGHT                        
         TM    BSTA+2,X'7F'        YES, WITH A NETWORK ALREADY?                 
         BNZ   VRDTO78                  YES, THEN IT'S ALRIGHT                  
         B     MISSAFLD                 NO, WE NEED IT THEN                     
*                                                                               
VRDTO68  CLI   0(R1),FLDNFILM      FILMCODE FIELD?                              
         BNE   MISSAFLD            NO, SOME OTHER REQUIRED FIELD                
         CLI   P0TIFCA,C'Y'        YES, FILM CODES ACCEPTED?                    
         BE    MISSAFLD                 YES, NEED A COLUMN FOR IT               
*                                                                               
VRDTO78  LA    R1,L'FLDEQTBL(R1)   SEE IF NEXT FIELD IN TABLE IS REQ'D          
         B     VRDTO60                                                          
*                                                                               
VRDTO80  DS    0H                                                               
*                                                                               
VRDTOX   DS    0H                                                               
         EJECT                                                                  
*****                                                                           
* VALIDATE THE OPTIONS                                                          
*****                                                                           
VROPT00  DS    0H                                                               
         BRAS  RE,VALOPT                                                        
         BNE   INVLFLD                                                          
*                                                                               
*****                                                                           
* VALIDATE THE AGENCY BILLER                                                    
*****                                                                           
VRAGB00  DS    0H                                                               
         CLI   BILLROPT,C'Y'       ARE WE DOING BILLER NAME                     
         BNE   VRAGBX                                                           
*                                                                               
         LA    R2,DISABILH                                                      
*****    CLI   BPRD,0              IF BILLER PRD OR EST MISSING                 
*****    BE    *+12                                                             
         CLC   SVQPRDCD,SPACES     CAN'T RELY ON   BPRD=0  TEST ANYMORE         
         BNH   *+12                                                             
         CLI   BEST,0                                                           
         BNE   VRAGB10                                                          
         XC    8(L'DISABIL,R2),8(R2)    THEN SKIP BILLER NAME                   
         OI    6(R2),X'80'                                                      
         B     VRAGBX                                                           
*                                                                               
VRAGB10  XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING GETBUBLD,R2                                                      
*                                                                               
         MVC   GBCOMFAC,ACOMFACS                                                
         MVC   GBIOA,AIO                                                        
         LA    R1,DISABILH                                                      
         ST    R1,GBNAMFLD                                                      
         MVC   GBAGY,AGENCY                                                     
         MVC   GBMEDEBC,DISMED                                                  
         MVC   GBCLTEBC,DISCLT                                                  
         MVC   GBOFFICE,CLTOFFIC                                                
         MVC   GBAGYMD,BAGYMD                                                   
         MVC   GBCLT,BCLT                                                       
         MVC   GBPRD,BPRD                                                       
         MVC   GBEST,BEST                                                       
         MVC   GBMKT(5),BMKTSTA                                                 
         MVI   GBTYPE,C'I'         SET FOR BILLER NAME                          
*                                                                               
         GOTO1 GETBUBL,DMCB,(R2)                                                
*                                                                               
         CLI   GBERR,0                                                          
         BE    VRAGBX                                                           
         CLI   GBERR,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW ON DMGR ERROR                    
         DROP  R2                                                               
*                                                                               
         LA    R2,DISABILH                                                      
         B     MISSFLD                                                          
*                                                                               
VRAGBX   DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
         BRAS  RE,GETEST                                                        
         BE    *+12                                                             
         LA    R2,DISESTMH                                                      
         B     BADESTM                                                          
*                                                                               
         BRAS  RE,CHKEST                                                        
         BE    *+16                                                             
         CLI   BYTE,X'01'                                                       
         BE    INVPAID                                                          
         B     ESTLOCK                                                          
*                                                                               
         GOTO1 =A(BLDREC),DMCB,(RC),RR=RELO                                     
*                                                                               
         CLI   PI2NAI2A,C'Y'                                                    
         BNE   VRX05                                                            
*                                                                               
         LA    R1,SVIHCHG                                                       
         BRAS  RE,BLDIHBLK                                                      
         CLC   SVIHORIG,SVIHCHG                                                 
         BE    VRX10                                                            
         BRAS  RE,BLDREQ           TURNAROUND REQUESTS                          
         B     VRX10                                                            
*                                                                               
VRX05    DS    0H                                                               
         GOTO1 CKSPCLI2                                                         
         BNE   VRX10                                                            
         LA    R1,SVIHCHG                                                       
         BRAS  RE,BLDIHBLK                                                      
*                                                                               
         CLC   SVIHORIG,SVIHCHG                                                 
         BE    VRX10                                                            
*                                                                               
         GOTO1 BLDPRTAB                                                         
         GOTO1 GENI2                                                            
         B     BRX                                                              
*                                                                               
VRX10    DS    0H                                                               
         XC    DISPERD,DISPERD                                                  
         GOTO1 DATCON,DMCB,(2,BMOSS),(9,DISPERD)                                
         MVI   DISPERDH+5,6                                                     
         OI    DISPERDH+6,X'80'                                                 
*                                                                               
         CLI   ACTNUM,ACTADD       NO NEED TO OFFMATCH ON ADD                   
         BE    VRX20                                                            
         CLI   CHGFLAG,1           OFFMATCH IF EST/PRD HAS CHANGED              
         BNE   BRX                                                              
         BRAS  RE,OFFMATCH         CHANGING MATCHING STATUS                     
         CLI   PI2RAUTO,C'Y'                                                    
         BNE   *+8                                                              
         BRAS  RE,BLDREQ           TURNAROUND REQUESTS                          
*                                                                               
VRX20    DS    0H                                                               
         CLI   PI2NAI2A,C'Y'                                                    
         BNE   *+8                                                              
         BRAS  RE,BLDREQ           TURNAROUND REQUESTS                          
*                                                                               
BRX      B     DREC                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DREC     DS    0H                                                               
         BRAS  RE,VALOPT                                                        
         BNE   INVLFLD                                                          
*                                                                               
         TWAXC DISDATEH,DISEXT2H   CLEAR THE SCREEN                             
*                                                                               
         MVI   IMFLAG,C'N'                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIMELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BNE   DREC05                                                           
         L     R6,MINELEM                                                       
         CLI   0(R6),SNVIMELQ                                                   
         BNE   DREC05                                                           
         MVI   IMFLAG,C'Y'                                                      
*                                                                               
***********************************                                             
* GET THE HEADER ELEMENT                                                        
***********************************                                             
DREC05   DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    DREC10                                                           
         DC    H'0'                                                             
*                                                                               
DREC10   L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING HEADER ELEMENT          
*                                                                               
DRPER00  GOTO1 DATCON,DMCB,(X'12',SNVHDSDT),(8,DISPDNM)                         
         OI    DISPDNMH+6,X'80'                                                 
DRDAT00  GOTO1 DATCON,DMCB,(2,SNVHDIDT),(8,DISDATE)                             
         OI    DISDATEH+6,X'80'                                                 
DRDDT00  GOTO1 DATCON,DMCB,(2,SNVHDDDT),(8,DISDDAT)                             
         OI    DISDDATH+6,X'80'                                                 
DRCON00  MVC   DISCONT,SNVHDCON                                                 
         OI    DISCONTH+6,X'80'                                                 
DRPAID0  XC    DISPAID,DISPAID                                                  
         TM    SNVHDCTL,SNVHDPDQ   INVOICE IS PAID?                             
         BZ    *+10                                                             
         MVC   DISPAID(8),=C'**PAID**'                                          
         OI    DISPAIDH+6,X'80'                                                 
*        OI    DISCONTH+6,X'80'                                                 
*****                                                                           
* DISPLAY THE TOTAL                                                             
*****                                                                           
DRTCS00  ZAP   PCKOF16B,=P'0'                                                   
         OC    SNVHDTCS,SNVHDTCS   JUST IN CASE NO VALUE                        
         BNZ   *+10                                                             
         ZAP   SNVHDTCS,=P'0'                                                   
         ZAP   PCKOF16B,SNVHDTCS                                                
*&&DO                                                                           
         CLC   =C'CK',AGENCY                                                    
         BNE   DRTCS10                                                          
         ICM   R1,15,SNVHDTAX                                                   
         CVD   R1,DUB                                                           
         AP    PCKOF16B,DUB                                                     
*&&                                                                             
*                                                                               
DRTCS10  EDIT  (P16,PCKOF16B),(15,DISTOTL),2,ZERO=NOBLANK,ALIGN=LEFT,  X        
               FLOAT=-                                                          
*                                                                               
*****                                                                           
* DISPLAY THE NUMBER OF SPOTS                                                   
*****                                                                           
DRTSP00  EDIT  (B2,SNVHDTSP),(4,DISSPTS),ZERO=NOBLANK,ALIGN=LEFT                
*****                                                                           
* DISPLAY EASI INFORMATION IF ANY                                               
*****                                                                           
DREAS00  XC    DISEASI,DISEASI                                                  
         OI    DISEASIH+6,X'80'                                                 
         OC    SNVHDEZS,SNVHDEZS   ANY EASI SOURCE?                             
         BZ    DRTAX00             NONE                                         
         MVC   DISEASI(5),=C'EASI='                                             
         MVC   DISEASI+5(4),SNVHDEZS                                            
*                                                                               
         CLI   IMFLAG,C'Y'                                                      
         BNE   DRTAX00                                                          
*                                                                               
         LA    R1,DISEASI+L'DISEASI-1                                           
         LHI   R0,L'DISEASI-1                                                   
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         MVC   1(3,R1),=C',IM'                                                  
*                                                                               
*****                                                                           
* DISPLAY TAX IF ANY FOR COKE                                                   
*****                                                                           
DRTAX00  DS    0H                                                               
         CLI   DISPTAX,C'Y'                                                     
         BE    DRTAX10                                                          
*                                                                               
         MVC   DISTAXT,=CL7'Tax'                                                
         OI    DISTAXTH+1,X'0C'    ZERO INTENSITY                               
         OI    DISTAXTH+6,X'80'                                                 
         XC    DISTAXA,DISTAXA                                                  
         OI    DISTAXAH+1,X'0C'    ZERO INTENSITY                               
         OI    DISTAXAH+6,X'80'                                                 
*                                                                               
         B     DRPRD00                                                          
*                                                                               
*                                                                               
*        CLC   =C'CK',AGENCY       COKE AGENCY?                                 
*        BNE   DRPRD00                                                          
*                                                                               
DRTAX10  DS    0H                                                               
         NI    DISTAXTH+1,X'FF'-X'0C'     NORMAL INTENSITY                      
         OI    DISTAXTH+6,X'80'                                                 
         NI    DISTAXAH+1,X'FF'-X'0C'     NORMAL INTENSITY                      
         NI    DISTAXAH+1,X'FF'-X'20'     UNPROTECT                             
         OI    DISTAXAH+6,X'80'                                                 
*                                                                               
*        CLC   =C'CK',AGENCY              COKE AGENCY?                          
*        BE    DRTAX20                                                          
*                                                                               
*        CLI   DISPTAX,C'Y'                                                     
*        BNE   DRTAX20                                                          
         MVC   DISTAXT,=CL7'Net+Tax'                                            
         TM    SNVHDCT2,SNVHDTXQ                                                
         BO    DRTAX30                                                          
         B     DRPRD00             NONE                                         
*                                                                               
DRTAX20  DS    0H                                                               
         OC    SNVHDTAX,SNVHDTAX   ANY TAX FOR COKE?                            
         BZ    DRPRD00             NONE                                         
*                                                                               
DRTAX30  EDIT  (B4,SNVHDTAX),(15,DISTAXA),2,ZERO=NOBLANK,ALIGN=LEFT,   X        
               COMMAS=YES                                                       
*****                                                                           
* DISPLAY THE PRODUCT                                                           
*****                                                                           
DRPRD00  MVC   SVPRDCOD,SNVHDPRD                                                
         MVC   SVPR2COD,SNVHDPR2                                                
*                                                                               
         CLI   SNVHDLEN,SNVHDLN2   DO WE HAVE ALPHA PRODUCTS?                   
         BNH   DRPRD04             CAN'T POSSIBLY IF LEN IS NOT GREATER         
         CLC   SNVHDAP1,SPACES                                                  
         BNH   DRPRD04             USE OLD WAY IF NO ALPHA EVEN IF >            
         LA    R3,SNVHDAP1                                                      
         B     DRPRD20                                                          
*                                                                               
DRPRD04  CLI   SNVHDPRD,0          IF NO PRODUCT THEN SKIP ITS DISPLAY          
         BNE   DRPRD05                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPRD                                    
         B     DRPRDX                                                           
*                                                                               
DRPRD05  LA    R3,SVCLIST          DISPLAY PRODUCT                              
DRPRD10  CLI   0(R3),0                                                          
         BNE   DRPRD15                                                          
         MVC   DISPROD(3),=3C'?'                                                
         MVC   SVPRDCOD,SNVHDPRD                                                
         MVC   SVQPRDCD,=3C'?'                                                  
         B     DRPRD25                                                          
*                                                                               
DRPRD15  CLC   SNVHDPRD,3(R3)                                                   
         BE    DRPRD20                                                          
         LA    R3,4(R3)                                                         
         B     DRPRD10                                                          
*                                                                               
DRPRD20  MVC   DISPROD(3),0(R3)    GET THE PRODUCT NAME                         
         MVI   DISPRODH+5,3                                                     
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH+5,3                                                     
         MVC   FAKEFLD(3),0(R3)                                                 
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD                                                          
         MVC   SVPRDCOD,BPRD                                                    
         MVC   SVQPRDCD,QPRD                                                    
         MVC   DISPRNM(L'PRDNM),PRDNM                                           
         OI    DISPRNMH+6,X'80'                                                 
*                                                                               
DRPRD25  CLI   SNVHDLEN,SNVHDLN2   DO WE HAVE ALPHA PRODUCTS?                   
         BNH   DRPRD26             CAN'T POSSIBLY IF LEN IS NOT GREATER         
         CLC   SNVHDAP2,SPACES                                                  
         BNH   DRPRD26             USE OLD WAY IF NO ALPHA EVEN IF >            
         MVI   DISPROD+3,C'-'                                                   
         MVI   DISPRODH+5,7                                                     
         LA    R3,SNVHDAP2                                                      
         B     DRPRD40                                                          
*                                                                               
DRPRD26  CLI   SNVHDPR2,0          ANY PIGGY?                                   
         BE    DRPRD50             NO                                           
*                                                                               
         MVI   DISPROD+3,C'-'                                                   
         MVI   DISPRODH+5,7                                                     
         LA    R3,SVCLIST          DISPLAY PRODUCT                              
DRPRD30  CLI   0(R3),0                                                          
         BNE   DRPRD35                                                          
         MVC   DISPROD+4(3),=3C'?'                                              
         MVC   SVPR2COD,SNVHDPR2                                                
         MVC   SVQPR2CD,=3C'?'                                                  
         B     DRPRD50                                                          
*                                                                               
DRPRD35  CLC   SNVHDPR2,3(R3)                                                   
         BE    DRPRD40                                                          
*                                                                               
         LA    R3,4(R3)                                                         
         B     DRPRD30                                                          
*                                                                               
DRPRD40  MVC   DISPROD+4(3),0(R3)                                               
*                                                                               
         XC    FAKEFLD,FAKEFLD     GET PIGGY'S NAME                             
         MVI   FAKEFLDH+5,3                                                     
         MVC   FAKEFLD(3),0(R3)                                                 
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD                                                          
         MVC   SVPR2COD,BPRD                                                    
         MVC   SVQPR2CD,QPRD                                                    
         MVI   DISPRNM+L'PRDNM,C'-'                                             
         MVC   DISPRNM+L'PRDNM+1(L'PRDNM),PRDNM                                 
*                                                                               
DRPRD50  OI    DISPRODH+6,X'80'                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',DISPRODH,,GLVSPPRD                            
*                                                                               
DRPRDX   DS    0H                                                               
*****                                                                           
* DISPLAY THE ESTIMATE                                                          
*****                                                                           
DREST00  DS    0H                                                               
         XC    SVELOCK1,SVELOCK1                                                
         XC    SVELOCK2,SVELOCK2                                                
*                                                                               
         CLI   SNVHDEST,0          VARIOUS ESTIMATES?                           
         BNE   DREST10             NO                                           
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPEST                                    
         B     DRESTX              YES                                          
*                                                                               
DREST10  EDIT  (B1,SNVHDEST),(3,DISESTM),FILL=0                                 
         MVI   DISESTMH+5,3                                                     
         OI    DISESTMH+4,X'08'    VALID NUMERIC                                
         LA    R2,DISESTMH                                                      
         CLC   SVQPRDCD,=3C'?'                                                  
         BE    *+14                                                             
         CLC   SVQPR2CD,=3C'?'                                                  
         BE    DREST20                                                          
         MVC   QPRD,SVQPRDCD                                                    
         GOTO1 VALIEST                                                          
*                                                                               
         L     R1,AIO                                                           
         MVC   SVECNTR1,ECNTRL-ESTHDR(R1)                                       
         MVC   SVELKYM1,ELOCKYR-ESTHDR(R1)                                      
*                                                                               
         MVC   DISESNM(L'ESTNM),ESTNM                                           
*                                                                               
         CLI   SNVHDPR2,0          ANY PIGGYBACK?                               
         BNE   *+14                                                             
         XC    SVELOCK2,SVELOCK2                                                
         B     DREST20                                                          
*                                                                               
         MVC   QPRD,SVQPR2CD                                                    
         GOTO1 VALIEST                                                          
*                                                                               
         L     R1,AIO                                                           
         MVC   SVECNTR2,ECNTRL-ESTHDR(R1)                                       
         MVC   SVELKYM2,ELOCKYR-ESTHDR(R1)                                      
*                                                                               
         MVI   DISESNM+L'ESTNM,C'-'                                             
         MVC   DISESNM+L'ESTNM+1(L'ESTNM),ESTNM                                 
*                                                                               
DREST20  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',DISESTMH,,GLVSPEST                            
*                                                                               
DRESTX   DS    0H                                                               
*                                                                               
DRREP00  LA    R2,DISREPH                                                       
* AT THIS POINT R6 SHOULD ADDRESS THE HEADER ELEMENT                            
         CLC   SNVHDREP,=XL3'000000'                                            
         BNE   DRREP10                                                          
         XC    DISREPN,DISREPN                                                  
         XC    DISREP,DISREP                                                    
         OI    DISREPNH+6,X'80'                                                 
         OI    DISREPH+6,X'80'                                                  
         B     DRREPX                                                           
*                                                                               
DRREP10  LA    R2,DISREPH                                                       
         MVC   REPRECD,=15C'0'                                                  
         LA    R3,REPRECD                                                       
         USING REPREC,R3                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,DISMED      TAKE MEDIA FROM SCREEN                       
         MVC   REPKREP,SNVHDREP                                                 
         MVC   REPKAGY,AGENCY                                                   
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',STAFIL,REPRECD,AIO                       
         L     R3,AIO                                                           
         CLC   REPRECD,0(R3)                                                    
         BNE   DRREPX                                                           
         MVC   DISREP,SNVHDREP                                                  
         OI    DISREPH+6,X'80'                                                  
         MVC   DISREPN,REBNAME                                                  
         OI    DISREPNH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
DRREPX   DS    0H                                                               
         DROP  R3                                                               
*                                                                               
DRDEM    DS    0H                                                               
         LA    R2,DISDEMO                                                       
         BRAS  RE,DISDEM                                                        
         OI    DISDEMOH+6,X'80'                                                 
DRDEMX   DS    0H                                                               
*                                                                               
DRPKG00  DS    0H                                                               
         CLI   NETPAKSW,C'Y'       NET MEDIA?                                   
         BNE   DROPT00                                                          
*                                                                               
         CLI   DISPTAX,C'Y'                                                     
         BNE   DRPKG05                                                          
         OI    DISPKGLH+1,X'2C'                                                 
         OI    DISPKGNH+1,X'2C'                                                 
         OI    DISPKGH+1,X'2C'                                                  
         OI    DISPKGLH+6,X'80'                                                 
         OI    DISPKGNH+6,X'80'                                                 
         OI    DISPKGH+6,X'80'                                                  
         B     DROPT00                                                          
*                                                                               
DRPKG05  DS    0H                                                               
         LA    R2,DISPKGH                                                       
*                                                                               
* AT THIS POINT R6 SHOULD ADDRESS THE HEADER ELEMENT                            
         CLI   SNVHDPKG,X'00'            ANYTHING IN PACKAGE FIELD?             
         BNE   DRPKG10                                                          
         XC    DISPKGN,DISPKGN                                                  
         XC    DISPKG,DISPKG                                                    
         B     DRPKGX                                                           
*                                                                               
DRPKG10  DS    0H                                                               
         EDIT  SNVHDPKG,DISPKG,ZERO=BLANK,ALIGN=LEFT                            
*                                                                               
         BRAS  RE,READPAK                                                       
*                                                                               
DRPKGX   DS    0H                                                               
         OI    DISPKGNH+6,X'80'                                                 
         OI    DISPKGH+6,X'80'                                                  
         DROP  R6                                                               
         USING SNVHDELD,R6                                                      
*****                                                                           
* DISPLAY THE OPTIONS                                                           
*****                                                                           
DROPT00  CLI   SNVHDCTL,0          NO OPTIONS?                                  
         BE    DROPTX              NONE                                         
*                                                                               
         SR    R2,R2               R2 = NUMBER OF DIFFERENT OPTIONS             
         LA    R3,BLOCK            R3 = A(UNSCAN BLOCK)                         
*        LA    R1,OPTNTABL         R1 = A(1ST OPTION ENTRY)                     
         L     R1,=A(OPTNTABL)                                                  
         A     R1,RELO                                                          
*                                                                               
DROPT10  CLI   0(R1),0             REACHED END OF OPTIONS TABLE                 
         BE    DROPT30             YES, DO THE UNSCAN NOW                       
*                                                                               
         MVC   BYTE,SNVHDCTL       SEE IF THIS OPTION HAS BEEN SET              
         NC    BYTE,0(R1)                                                       
         BZ    DROPT20                                                          
         MVC   0(20,R3),SPACES                                                  
         MVC   0(8,R3),1(R1)                                                    
         LA    R3,20(R3)                                                        
         AH    R2,=H'1'            INCREMENT # OF OPTIONS TO BE SHOWN           
*                                                                               
DROPT20  LA    R1,L'OPTNTABL(R1)                                                
         B     DROPT10                                                          
*                                                                               
DROPT30  GOTO1 UNSCAN,DMCB,((R2),BLOCK),DISOPTNH                                
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DISOPTNH+6,X'80'                                                 
*                                                                               
DROPTX   DS    0H                                                               
*****                                                                           
* DISPLAY THE AGENCY BILLER                                                     
*****                                                                           
DRAGB00  DS    0H                                                               
         CLI   BILLROPT,C'Y'       ARE WE DOING BILLER NAME                     
         BNE   DRAGBX                                                           
*                                                                               
         LA    R2,DISABILH                                                      
         CLC   SVQPRDCD,=3C'?'          IF BILLER PRD OR EST MISSING            
         BE    *+12                                                             
         CLI   SNVHDEST,0                                                       
         BNE   DRAGB10                                                          
         XC    8(L'DISABIL,R2),8(R2)    THEN SKIP BILLER NAME                   
         OI    6(R2),X'80'                                                      
         B     DRAGBX                                                           
*                                                                               
DRAGB10  XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING GETBUBLD,R2                                                      
*                                                                               
         MVC   GBCOMFAC,ACOMFACS                                                
         MVC   GBIOA,AIO                                                        
         LA    R1,DISABILH                                                      
         ST    R1,GBNAMFLD                                                      
         MVC   GBAGY,AGENCY                                                     
         MVC   GBMEDEBC,DISMED                                                  
         MVC   GBCLTEBC,DISCLT                                                  
         MVC   GBOFFICE,CLTOFFIC                                                
         MVC   GBAGYMD,BAGYMD                                                   
         MVC   GBCLT,BCLT                                                       
         MVC   GBPRD,SNVHDPRD                                                   
         MVC   GBEST,SNVHDEST                                                   
         MVC   GBMKT(5),BMKTSTA                                                 
         MVI   GBTYPE,C'I'         SET FOR BILLER NAME                          
*                                                                               
         GOTO1 GETBUBL,DMCB,(R2)                                                
*                                                                               
         CLI   GBERR,0                                                          
         BE    DRAGBX                                                           
         CLI   GBERR,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW ON DMGR ERROR                    
         DROP  R2                                                               
*                                                                               
         LA    R2,DISABILH                                                      
         B     MISSFLD                                                          
*                                                                               
DRAGBX   DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************                                             
* GET THE DETAIL ORDER ELEMENT                                                  
***********************************                                             
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVDTELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    DREC20                                                           
*                                                                               
         CLI   MINERR,MINEEOF      ANY DETAIL ORDER ELEMENT?                    
         BE    DREC30                                                           
         DC    H'0'                                                             
*                                                                               
DREC20   L     R6,MINELEM                                                       
         USING SNVDTELD,R6                                                      
         CLI   SNVDTEL,SNVDTELQ                                                 
         BNE   DREC30                                                           
         BRAS  RE,SHOWDTOR                                                      
***********************************                                             
* SEE IF WE HAVE ANY DETAILS                                                    
***********************************                                             
DREC30   XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    DREC40                                                           
*                                                                               
         CLI   MINERR,MINEEOF      ANY DETAIL ORDER ELEMENT?                    
         BE    DREC50                                                           
         DC    H'0'                                                             
*                                                                               
DREC40   L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ                                                 
         BNE   DREC50                                                           
         OI    MISCFLG1,MF1GTDTL   WE HAVE INVOICE DETAILS                      
         DROP  R6                                                               
*                                                                               
DREC50   L     R6,MINELEM          DISP I2 DATE/TIME                            
         USING SNVMMELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVMMELQ    X'E8' MM                                     
         BRAS  RE,MINIOHI                                                       
         CLI   SNVMMEL,SNVMMELQ                                                 
         BE    DREC55             'E8' PRESENT - SKIP 'FE' CHECK                
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVMMELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'FE'       X'FE' - E8/E9 PLACE HOLDER                   
         BRAS  RE,MINIOHI                                                       
         CLI   SNVMMEL,X'FE'                                                    
         BE    DREC60              'FE' IS MISSING - SHOW 'I2=RUNOV'            
         MVC   DISEXT1(9),=C'I2=RUN OV'                                         
         OI    DISEXT1H+6,X'80'                                                 
         B     DREC60                                                           
*                               IF 'E8' IS MISSING - CHECK FOR 'FE'             
DREC55   DS    0H                                                               
         MVC   DISEXT1(3),=C'I2='                                               
         GOTO1 DATCON,DMCB,(2,SNVMMDAT),(11,DISEXT1+3)                          
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   DREC60              THEN DONE                                    
         MVI   DISEXT1+11,C'*'                                                  
         GOTO1 HEXOUT,DMCB,SNVMMTIM,DISEXT1+12,2,=C'TOG'                        
*                                                                               
DREC60   DS    0H                                                               
         LA    R2,DISEXT2H                                                      
         BRAS  RE,DISACT                                                        
*                                                                               
DRX      DS   0H                                                                
         CLI   PFKEY,11                                                         
         BNE   *+8                                                              
         BRAS  RE,GENI2RQ                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* GENERAL INFORMATIONAL MESSAGES                                                
***********************************************************************         
PLSENTER MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
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
*                                                                               
RCNOTDEL MVI   GERROR1,RECNTDEL    RECORD IS NOT DELETED                        
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM 23 ERROR MESSAGES                                                      
***********************************************************************         
CCHGDTO1 MVI   GERROR1,DORDERR1    CAN'T CHANGE ORDER FROM DEFAULT              
         B     DTORDERR                                                         
*                                                                               
CCHGDTO2 MVI   GERROR1,DORDERR2    CAN'T REDUCE NUMBER OF DETAIL FIELDS         
         B     DTORDERR                                                         
*                                                                               
CCHGFLD  MVI   GERROR1,CANTCHNG    CAN'T CHANGE FIELD, DETAILS EXIST            
         B     SYS23ERR                                                         
*                                                                               
NEEDF1ST MVI   GERROR1,FLMFIRST    NEED FILM CODE BEFORE 2ND FILM               
         B     SYS23ERR                                                         
*                                                                               
NDPRDEST MVI   GERROR1,PRDB4EST    PRODUCT SHOULD APPEAR BEFORE EST             
         B     SYS23ERR                                                         
*                                                                               
NDPRDFLM MVI   GERROR1,PRDB4FLM    PRODUCT SHOULD APPEAR BEFORE FILM            
         B     SYS23ERR                                                         
*                                                                               
DTORDERR LA    R2,DISDTL1H                                                      
SYS23ERR MVI   GETMSYS,23                                                       
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM 23 ERRORS WITH REPLACEMENT TEXT (&1)                                   
***********************************************************************         
MISSAFLD MVI   GERROR1,MISS1FLD    MISSING &1 FIELD                             
         B     ERR23TXT                                                         
*                                                                               
CCHGDTO3 MVI   GERROR1,DORDERR3    CAN'T REMOVE OLD DETAIL FIELD &1             
         B     ERR23TXT                                                         
*                                                                               
ERR23TXT MVI   GETMSYS,23                                                       
         B     ERRRTEXT                                                         
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
BADREP   MVI   GERROR1,29          INVALID REP                                  
         B     ERREXIT2                                                         
*                                                                               
BADPKG   MVC   GERROR,=AL2(INVPKG) INVALID PACKAGE                              
         B     ERREXIT2                                                         
*                                                                               
ESTLOCK  MVC   GERROR,=AL2(ESTLK)  ESTIMATE LOCKED                              
         LA    R2,CONACTH                                                       
         B     ERREXIT                                                          
*                                                                               
INVPAID  MVC   GERROR,=AL2(INVPAIDQ)  INVOICE PAID                              
         LA    R2,CONACTH                                                       
         B     ERREXIT                                                          
*                                                                               
CLTPGER  MVC   GERROR,=AL2(CLTPGQ)  INVOICE PAID                                
         LA    R2,CONACTH                                                       
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
AAANOGO  EQU   992                 PRODUCT AAA NOT ALLOWED!                     
POLNOGO  EQU   102                 PRODUCT CAN'T BE POL!                        
INVPKG   EQU   999                 PACKAGE NOT ON FILE                          
ESTLK    EQU   1221                ESTIMATE LOCKED - CAN'T ADD/DEL/CHA          
INVPAIDQ EQU   1223                INVOICD PAID - CAN'T ADD/DEL/CHA             
CLTPGQ   EQU   1383                CLT NOT AUTHORIZED TO USE STA (P&G)          
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
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALID OPTIONS TABLE                                                           
*                                                                               
* BYTE  0      - FLAG BIT                                                       
* BYTES 1-8    - TEXT EQUIVALENT                                                
***********************************************************************         
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
       ++INCLUDE SPSNVFEQTB        FIELD EQUATE TABLE                           
*                                                                               
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL DS    0H                                                               
         NMOD1 0,**STPF**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF50                                                           
         CLI   PFKEY,4                                                          
         BE    STPF50                                                           
         CLI   PFKEY,5                                                          
         BE    STPF50                                                           
         CLI   PFKEY,10                                                         
         BE    STPF50                                                           
         CLI   PFKEY,12                                                         
         BL    STPF100                                                          
STPF50   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR                                                
*                                                                               
STPF100  DS    0H                                                               
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+12                                                             
         CLI   PFKEY,5                                                          
         BE    PFERR                                                            
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
         CLI   PFKEY,12            RETURN OR GO TO LIST?                        
         BE    STPF100X            LET THEM RETURN OR EXIT OUT OF HERE          
*                                                                               
STPF100A TM    DISMEDH+4,X'20'     IF ANY OF THE KEY FIELDS CHANGE              
         BZ    STPFX                                                            
         TM    DISCLTH+4,X'20'                                                  
         BZ    STPFX                                                            
         TM    DISSTAH+4,X'20'                                                  
         BZ    STPFX                                                            
         TM    DISPERDH+4,X'20'                                                 
         BZ    STPFX                                                            
         TM    DISINVCH+4,X'20'                                                 
         BZ    STPFX               THEN DON'T ALLOW PFKEYS YET                  
*                                                                               
         CLI   CALLSP,1            DID WE COME FROM THE LIST?                   
         BNE   STPF100X            NO                                           
*                                                                               
         CLC   LSTQMED,DISMED      IF ANY OF THE KEY FIELDS CHANGE FROM         
         BNE   STPF100R              WHAT WE GOT CALLSP'D WITH THEN             
         CLC   LSTQCLT,DISCLT                                                   
         BNE   STPF100R                                                         
         CLC   LSTQSTA,DISSTA                                                   
         BNE   STPF100R                                                         
         CLC   LSTQPER,DISPERD                                                  
         BNE   STPF100R                                                         
         CLC   LSTQINV,DISINVC                                                  
         BE    STPF100X                                                         
STPF100R MVI   CALLSP,0            THEN RESET CALLSP TO 0                       
*                                                                               
STPF100X LA    R2,SPFTABLE                                                      
         CLI   PFKEY,4                                                          
         BE    STPF104                                                          
         CLI   PFKEY,5                                                          
         BE    STPF105                                                          
         CLI   PFKEY,10                                                         
         BE    STPF110                                                          
         CLI   PFKEY,12                                                         
         BE    STPF112                                                          
         B     STPFX                                                            
*********                                                                       
* PF4                                                                           
*********                                                                       
STPF104  CLI   CALLSP,0            DID WE GET CALL FROM LIST OVERLAY?           
         BNE   STPF104A                                                         
         XC    KEYLINE,KEYLINE     NO                                           
         LA    R3,KEYLINE                                                       
*                                                                               
         CLI   DISMEDH+5,0                                                      
         BNE   *+12                                                             
         LA    R2,DISMEDH                                                       
         B     MISSFLD                                                          
*                                                                               
         MVC   0(L'QMED,R3),QMED                                                
         MVI   L'QMED(R3),C'.'                                                  
         LA    R3,L'QMED+1(R3)                                                  
*                                                                               
         CLI   DISCLTH+5,0                                                      
         BNE   *+12                                                             
         LA    R2,DISCLTH                                                       
         B     MISSFLD                                                          
*                                                                               
         ZIC   R1,DISCLTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DISCLT                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   DISSTAH+5,0                                                      
         BNE   *+12                                                             
         LA    R2,DISSTAH                                                       
         B     MISSFLD                                                          
*                                                                               
         ZIC   R1,DISSTAH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DISSTA                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   DISPERDH+5,0                                                     
         BNE   *+12                                                             
         LA    R2,DISPERDH                                                      
         B     MISSFLD                                                          
*                                                                               
         ZIC   R1,DISPERDH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DISPERD                                                  
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   DISINVCH+5,0                                                     
         BNE   *+12                                                             
         LA    R2,DISINVCH                                                      
         B     MISSFLD                                                          
*                                                                               
         ZIC   R1,DISINVCH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DISINVC                                                  
*                                                                               
         MVI   PFKEY,16            NOW WE CAN CALL DETAIL DIRECTLY              
         B     STPF104W                                                         
*                                  TELL CONTROLLER TO IGNORE SEL CODES          
STPF104A OI    CTLRFLG1,CF1TSELQ+CF1CKOFF   AND LIST OVERLAY TO CHECK           
         MVC   SELOFFST,SVDOFFST            OFFSET OF SELECT                    
*                                                                               
STPF104W CLC   =C'ADD',CONACT         ON ACTION ADD?                            
         BNE   *+10                   YES, CHANGE IT SO WE WON'T GET            
         MVC   CONACT,=CL8'DISPLAY'        ERROR THAT RECORD EXISTS             
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPF104X B     STPFX                                                            
*********                                                                       
* PF5                                                                           
*********                                                                       
STPF105  CLI   DISPRODH+5,0                                                     
         BNE   STPF105A                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'POL',3,GLVSPPRD                            
         B     STPF105B                                                         
*                                                                               
STPF105A CLI   DISESTMH+5,0        ANY ESTIMATE FOR MATCH?                      
         BE    STPF105B            NO                                           
         ZIC   R1,DISPRODH+5       YES, PUT OUT TO GLOBBER AS PRD/EST           
         BCTR  R1,0                   IN THE GLVSPPRD OBJECT                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),DISPROD                                                 
         LA    RE,BLOCK+1                                                       
         AR    RE,R1                                                            
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         ZIC   R1,DISESTMH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DISESTM                                                  
         ZIC   R1,DISESTMH+5                                                    
         ZIC   R0,DISPRODH+5                                                    
         AR    R0,R1                                                            
         AH    R0,=H'1'                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',BLOCK,(R0),GLVSPPRD                           
*                                                                               
STPF105B LA    R6,KEY                                                           
         USING SNVKEYD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVMASTKY),SVMASTKY                                         
         OC    SVMASTKY,SVMASTKY                                                
         BNZ   STPF105C                                                         
         GOTO1 INITIAL,DMCB,(R2)   THIS SHOULD CAUSE AN ERROR                   
         B     STPFX                                                            
*                                                                               
STPF105C GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),INVDIR,KEY,AIO                       
         CLI   DMCB+8,2                                                         
         BE    RECDLTED            RECORD HAS BEEN DELETED                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   KEY(4),SNVDDA                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),INVFIL,KEY,AIO,DMWORK                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,SNVELS                                                        
         USING SNVHDELD,R1                                                      
         CLI   0(R1),SNVHDELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SNVHDCTL,SNVHDMCQ   MIDFLIGHT INVOICE?                           
         BZ    STPF105W                                                         
         DROP  R1                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'MCT',3,GLVSPOPT                            
         DROP  R6                                                               
*                                                                               
STPF105W XC    ELEM,ELEM                                                        
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
*                                                                               
STPF105X L     RD,SYSRD                                                         
         B     STPFX                                                            
*********                                                                       
* PF10                                                                          
*********                                                                       
STPF110  CLI   DISPRODH+5,0                                                     
         BNE   STPF110A                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPRD                                    
         B     STPF110B                                                         
*                                                                               
STPF110A DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',DISPRODH,,GLVSPPRD                            
*                                                                               
STPF110B CLI   CALLSP,0            DID WE GET CALL FROM LIST OVERLAY?           
         BNE   *+12                YES                                          
         MVI   PFKEY,22            NO, WE CAN CALL REQUEST DIRECTLY             
         B     STPF110W                                                         
*                                  TELL CONTROLLER TO IGNORE SEL CODES          
         OI    CTLRFLG1,CF1TSELQ+CF1CKOFF   AND LIST OVERLAY TO CHECK           
         MVC   SELOFFST,SVDOFFST            OFFSET OF SELECT                    
*                                                                               
STPF110W CLC   =C'ADD',CONACT         ON ACTION ADD?                            
         BNE   *+10                   YES, CHANGE IT SO WE WON'T GET            
         MVC   CONACT,=CL8'DISPLAY'        ERROR THAT RECORD EXISTS             
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPF110X B     STPFX                                                            
*********                                                                       
* PF12                                                                          
*********                                                                       
STPF112  CLI   CALLSP,0            DID WE GET CALL FROM LIST OVERLAY?           
         BNE   *+12                YES                                          
         MVI   PFKEY,12            NO, WE CAN JUST GO TO LIST                   
         B     STPF110W                                                         
         MVI   PFKEY,24            RETURN TO LIST OVERLAY                       
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPF112X B     STPFX                                                            
*                                                                               
STPFX    B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE LIST                                      
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* DETAIL UPDATE DRIVER                                                          
         DC    AL1(PF04X-*,04,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF04X    EQU   *                                                                
*                                                                               
* INVOICE MATCH DRIVER                                                          
         DC    AL1(PF05X-*,05,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF05X    EQU   *                                                                
*                                                                               
* INVOICE REQUEST DRIVER                                                        
         DC    AL1(PF10X-*,10,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF10X    EQU   *                                                                
*                                                                               
* PF12 KEY DRIVER                                                               
         DC    AL1(PF12X-*,12,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
* SPECIAL PFKEY TABLE DEFINITIONS                                               
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* GO BACK TO LIST OVERLAY TO INVOKE DETAIL UPDATE                               
         DC    AL1(SPF04X-*,04,PFTRPROG,0,PFTCLRKY)                             
         DC    CL3' ',CL8' ',CL8' '                                             
SPF04X   EQU   *                                                                
*                                                                               
* GO BACK TO LIST OVERLAY TO INVOKE INVOICE REQUEST                             
         DC    AL1(SPF10X-*,10,PFTRPROG,0,PFTCLRKY)                             
         DC    CL3' ',CL8' ',CL8' '                                             
SPF10X   EQU   *                                                                
*                                                                               
* GOTO TO LIST IF CALLSP=0 FOR PF12                                             
         DC    AL1(SPF12X-*,12,0,(SPF12X-SPF12)/KEYLNQ,0)                       
         DC    CL3' ',CL8'INVOICE',CL8'LIST'                                    
SPF12    DC    AL1(KEYTYTWA,L'DISMED-1),AL2(DISMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'DISCLT-1),AL2(DISCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'DISSTA-1),AL2(DISSTA-T210FFD)                     
SPF12X   EQU   *                                                                
*                                                                               
* IF THIS OVERLAY NOT INVOKED FROM LIST, WE CAN DO A DIRECT CALLPROG            
         DC    AL1(SPF16X-*,16,PFTCPROG,(SPF16X-SPF16)/KEYLNQ,0)                
         DC    CL3' ',CL8'DETAIL ',CL8'UPDATE  '                                
SPF16    DC    AL1(KEYTYWS,L'KEYLINE-1),AL2(KEYLINE-MYAREAD)                    
SPF16X   EQU   *                                                                
*                                                                               
* INVOICE REQUEST                                                               
         DC    AL1(SPF22X-*,22,PFTCPROG,(SPF22X-SPF22)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'REQUEST '                               
SPF22    DC    AL1(KEYTYTWA,L'DISMED-1),AL2(DISMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'DISCLT-1),AL2(DISCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'DISSTA-1),AL2(DISSTA-T210FFD)                     
SPF22X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF24X-*,24,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
SPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* BUILDS THE RECORD                                                             
***********************************************************************         
BLDREC   DS    0H                                                               
         NMOD1 0,**BREC**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
***********************************                                             
* CHECK DETAIL ORDER CHANGE IN NUMBER AND/OR ORDER IS ALLOWED OR NOT            
***********************************                                             
BRCKD00  TM    MISCFLG1,MF1GTDTL   PRESENCE OF DETAILS PROHIBITS THE            
         BZ    BRCKDX                  REMOVAL OF DETAIL FIELDS                 
*                                                                               
         LA    R6,MELEM                                                         
         XC    MINEKEY,MINEKEY     EXAMINE THE DETAIL ORDER ELEMENT             
         MVI   MINEKEY,SNVDTELQ                                                 
         BRAS  RE,MINIOHI          FOUND SOMETHING?                             
         BE    BRCKD10             YES                                          
         CLI   MINERR,MINEEOF                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    MELEM,MELEM         NO DETAIL ORDER ELEMENT                      
*                                                                               
         USING SNVDTELD,R6                                                      
BRCKD10  CLI   SNVDTEL,SNVDTELQ    DETAIL ORDER ELEMENT?                        
         BE    *+10                                                             
         XC    MELEM,MELEM         NO, NONE                                     
*                                                                               
         CLI   SVFLDCNT,0          ANY DETAIL FIELDS ENTERED?                   
         BNE   BRCKD30             YES                                          
*                                                                               
         CLI   MELEM,0             NO, ANY DETAIL ORDER ELEMENT BEFORE?         
         BE    BRCKDX                                                           
         LA    R2,DISDTL1H                                                      
         B     CCHGDTO2            CAN'T CHANGE NUMBER OF DETAIL FIELDS         
*****                                                                           
* FROM THIS POINT DETAIL FIELDS HAVE BEEN ENTERED                               
*****                                                                           
BRCKD30  CLI   MELEM,0             ANY DETAIL ORDER ELEMENT BEFORE?             
*        BE    CCHGDTO1            NO, CAN'T CHANGE ORDER FROM DEFAULT          
         BE    BRCKDX              NO, USER ADDS WHAT HE WANTS (EASI)           
*                                                                               
         MVC   MELEM2,MELEM                                                     
         LA    R6,MELEM2                                                        
         ZIC   R0,SNVDTLEN         R0 = NUMBER OF DETAIL FIELD IN REC           
         SH    R0,=Y(SNVDTOVQ)                                                  
         LA    R1,SNVDTFLD         R1 = A(1ST DETAIL FIELD FROM RECORD)         
         DROP  R6                                                               
*                                                                               
BRCKD40  ZIC   R6,SVFLDCNT         R6 = NUMBER OF DETAIL FIELDS ENTERED         
         LA    RE,SVFLDLST         RE = A(1ST DETAIL FIELD ENTERED)             
*                                                                               
BRCKD42  CLC   0(1,R1),0(RE)                                                    
         BE    BRCKD45                                                          
         LA    RE,1(RE)                                                         
         BCT   R6,BRCKD42                                                       
*                                                                               
         MVC   BYTE,0(R1)          SAVE FIELD EQUATE TO BE DELETED              
         BRAS  RE,REMOVFLD         CAN WE REMOVE THIS FIELD?                    
         BE    BRCKD45             YES                                          
*                                                                               
         LARL  R6,FLDEQTBL         FIND THE NAME OF THE FIELD                   
BRCKD44  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,L'FLDEQTBL(R6)                                                
         B     BRCKD44                                                          
*                                                                               
         MVI   BLOCK,9             L(MESSAGE)  - LENGTH NOT INCL.               
         MVC   BLOCK+1(8),2(R6)    PUT NAME OUT IN THE MESSAGE                  
         MVI   BLOCK+9,0           TERMINATING 0                                
         LA    R2,DISESTMH                                                      
         B     CCHGDTO3            NO, CAN'T REMOVE OLD DETAIL FIELD &1         
*                                                                               
BRCKD45  LA    R1,1(R1)            MAKE SURE ALL THE OLD DETAIL FIELDS          
         BCT   R0,BRCKD40              ALSO APPEAR IN THE NEW LIST              
*                                                                               
BRCKDX   DS    0H                                                               
***********************************                                             
* BUILD THE HEADER ELEMENT                                                      
***********************************                                             
BRHDR00  LA    R6,MELEM2           SET UP THE HEADER ELEMENT                    
         USING SNVHDELD,R6                                                      
         XC    MELEM2,MELEM2       CLEAR THE HEADER ELEMENT                     
         MVI   SNVHDEL,SNVHDELQ                                                 
***      MVI   SNVHDLEN,SNVHDLN2                                                
         MVI   SNVHDLEN,SNVHDLN3   (AS OF 2005-10-12)                           
         MVC   SNVHDPRD,SVPRDCOD                                                
         MVC   SNVHDAP1,SVQPRDCD                                                
*                                                                               
         OC    SVTAXAMT,SVTAXAMT                                                
         BZ    *+14                                                             
         MVC   SNVHDTAX,SVTAXAMT                                                
         OI    SNVHDCT2,SNVHDTXQ                                                
*                                                                               
         MVC   SNVHDPR2,SVPR2COD                                                
         MVC   SNVHDAP2,SVQPR2CD                                                
         MVC   SNVHDEST,SVESTNUM                                                
         MVC   SNVHDREP,SVREP                                                   
*                                                                               
         TM    SNVHDCT2,SNVHDTXQ    PACKAGE FIELS USED AS CONTROL BYTE?         
         BO    *+10                 YES - DO NOT SAVE PACKAGE                   
         MVC   SNVHDPKG,SVPKG                                                   
*                                                                               
         MVC   SNVHDDEM,SVDEMO                                                  
*                                                                               
         MVC   SNVHDCON,SVCONNUM                                                
         ZAP   SNVHDTCS,SVTOTCOS                                                
         MVC   SNVHDTSP,SVTOTSPT                                                
         GOTO1 DATCON,DMCB,(5,0),(2,SNVHDCDT)                                   
*                                                                               
         TM    SVCTLBYT,SNVHDCLQ   USING CALENDAR MONTHS?                       
         BZ    BRHDR10                                                          
         MVC   SNVHDSDT,BMOSS      YES                                          
         MVC   SNVHDEDT,BMOSE                                                   
         B     BRHDR20                                                          
*                                                                               
BRHDR10  MVC   SNVHDSDT,BRDCSDAT   NO, USING BROADCAST MONTHS                   
         MVC   SNVHDEDT,BRDCEDAT                                                
*                                                                               
BRHDR20  MVC   SNVHDIDT,SVINVDAT                                                
         MVC   SNVHDDDT,SVDUEDAT                                                
         MVC   SNVHDCTL,SVCTLBYT                                                
*                                                                               
         CLI   SVPAYOPT,C'P'                                                    
         BNE   *+8                                                              
         OI    SNVHDCTL,SNVHDPDQ                                                
*                                                                               
         CLI   SVPAYOPT,C'U'                                                    
         BNE   *+8                                                              
         NI    SNVHDCTL,X'FF'-SNVHDPDQ                                          
*                                                                               
         TM    MISCFLG1,MF1RESPN   RESPONSE COUNT INVOICE?                      
         BZ    *+8                                                              
         OI    SNVHDCTL,SNVHDRSQ   YES                                          
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING THE REC                            
         BNE   BRHDR30                                                          
*&&DO                                                                           
         CLC   AGENCY,=C'CK'          AND COKE AGENCY                           
         BNE   BRHDR25                                                          
*                                                                               
         GOTO1 =V(RANDOM),DMCB,99,0,RR=RELO  10% FOR AUDIT                      
         L     R0,4(R1)                                                         
         CH    R0,=H'90'                 BETWEEN 90-99 INCLUSIVELY?             
         BL    *+8                                                              
         OI    SNVHDCTL,SNVHDAUQ         YES, HOLD FOR AUDIT                    
*&&                                                                             
BRHDR25  GOTO1 DATCON,DMCB,(5,0),(3,SNVHDEZB)                                   
         MVC   MELEM,MELEM2        THEN ADD THE HEADER ELEMENT STRAIGHT         
*                                                                               
         MVC   HALF,SNVKMOS-SNVKEY+MINMKEY                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(X'20',WORK)                                
*                                                                               
         CLI   COUNTRY,C'C'                                                     
         BE    BRHDR26                                                          
         CLC   =C'2001',WORK                                                    
         BH    BRHDR28                                                          
         CLI   SVNETINV,C'Y'                                                    
         BNE   BRHDR28                                                          
         OI    SNVHDCTL-SNVHDEL+MELEM,SNVHDNTQ                                  
         B     BRHDR28                                                          
*                                                                               
BRHDR26  DS    0H                                                               
         CLC   =C'2001',WORK                                                    
         BH    BRHDR28                                                          
         OI    SNVHDCTL-SNVHDEL+MELEM,SNVHDNTQ                                  
*                                                                               
BRHDR28  DS    0H                                                               
         BRAS  RE,MINIOADD                                                      
         BE    BRHDRX                                                           
         DC    H'0'                                                             
*                                                                               
BRHDR30  XC    MINEKEY,MINEKEY     OTHERWISE WE HAVE TO CHANGE HEADER           
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BRHDRHD1 USING SNVHDELD,MELEM                                                   
         CLI   BRHDRHD1.SNVHDEL,SNVHDELQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SNVHDSDT,BRHDRHD1.SNVHDSDT   DO NOT MODIFY THESE FIELDS          
         MVC   SNVHDEDT,BRHDRHD1.SNVHDEDT     ON A CHANGE                       
         MVC   SNVHDEZS,BRHDRHD1.SNVHDEZS                                       
*        MVC   SNVHDEZD,BRHDRHD1.SNVHDEZD                                       
         MVC   SNVHDCDT,BRHDRHD1.SNVHDCDT                                       
*                                                                               
         GOTO1 DATCON,DMCB,(X'12',SNVHDSDT),(0,WORK)   PERIOD DATES             
         MVC   PERSDATE,WORK                                                    
         MVC   PEREDATE,WORK+L'PERSDATE+1                                       
*                                                                               
         CLI   DISPTAX,C'Y'                                                     
         BE    *+10                                                             
         MVC   SNVHDTAX,BRHDRHD1.SNVHDTAX                                       
*                                                                               
         CLI   BRHDRHD1.SNVHDLEN,SNVHDLNQ                                       
         BNH   *+10                                                             
         MVC   SNVHDEZB,BRHDRHD1.SNVHDEZB                                       
*                                                                               
         TM    MISCFLG1,MF1GTDTL   GOT ANY DETAILS?                             
         BNZ   BRHDR40             YES                                          
         MVC   MELEM,MELEM2        NO, CAN CHANGE HEADER AS WE PLEASE           
         BRAS  RE,MINIOWRT                                                      
         B     BRHDRX                                                           
*                                                                               
BRHDR40  CLI   SNVHDLEN,SNVHDLN2   ARE WE STORING PRODUCT ALPHAS?               
         BNH   BRHDR40A            NO, NOT YET                                  
         CLC   SNVHDAP1,SPACES     YES, DO WE HAVE PRODUCT NOW?                 
         BH    BRHDR41                                                          
         B     BRHDR40B                                                         
BRHDR40A OC    SNVHDPRD(2),SNVHDPRD    HAD A GLOBAL PRODUCT(S) BEFORE?          
         BNZ   BRHDR41                 YES                                      
*********                                                                       
* NO PRODUCTS PREVIOUSLY                                                        
*********                                                                       
BRHDR40B CLI   BRHDRHD1.SNVHDLEN,SNVHDLN2                                       
         BH    BRHDR40D                                                         
         OC    BRHDRHD1.SNVHDPRD(2),BRHDRHD1.SNVHDPRD                           
         BNZ   BRHDR41E                                                         
         B     BRHDR45                                                          
*                                                                               
BRHDR40D CLC   BRHDRHD1.SNVHDAP1,SPACES                                         
         BH    BRHDR41E            SHOULD NOT HAVE IT NOW                       
         B     BRHDR45                                                          
*********                                                                       
* HAD A PRODUCT(S) PREVIOUSLY                                                   
*********                                                                       
BRHDR41  CLI   BRHDRHD1.SNVHDLEN,SNVHDLN2                                       
         BH    BRHDR41B                                                         
         OC    BRHDRHD1.SNVHDPRD(2),BRHDRHD1.SNVHDPRD                           
         BNZ   BRHDR45                                                          
         B     BRHDR41E                                                         
*                                                                               
BRHDR41B CLC   BRHDRHD1.SNVHDAP1,SPACES                                         
         BH    BRHDR45                                                          
*                                                                               
BRHDR41E LA    R2,DISPRODH         SHOULD HAVE IT NOW                           
         B     INVLFLD                                                          
*                                                                               
* * * * *                                                                       
*                                                                               
BRHDR45  DS    0H                                                               
*                                                                               
* CHANGING ESTIMATE IN HEADER IS ALLOWED, SO NO NEED FOR THIS CHECK             
*                                                                               
*&&DO                                                                           
BRHDR45  CLI   SNVHDEST,0          HAD A GLOBAL ESTIMATE BEFORE?                
         BNE   BRHDR46             YES                                          
*********                                                                       
* NO ESTIMATE PREVIOUSLY                                                        
*********                                                                       
         CLI   BRHDRHD1.SNVHDEST,0   SHOULD NOT HAVE AN ESTIMATE NOW            
         BNE   BRHDR45E                                                         
         B     BRHDR48                                                          
*********                                                                       
* HAD AN ESTIMATE PREVIOUSLY                                                    
*********                                                                       
BRHDR46  DS    0H                                                               
         CLI   BRHDRHD1.SNVHDEST,0                                              
         BNE   BRHDR48                                                          
BRHDR45E LA    R2,DISESTMH           SHOULD HAVE AN ESTIMATE NOW                
         B     INVLFLD                                                          
*&&                                                                             
*                                                                               
* * * * *                                                                       
*                                                                               
BRHDR48  TM    BRHDRHD1.SNVHDCTL,SNVHDDNQ   INVOICE NETTED DOWN BEFORE?         
         BZ    BRHDR49             NO                                           
         TM    SNVHDCTL,SNVHDNTQ   YES, THEN NET OPTION SHOULD BE ON            
         BNZ   *+12                OKAY STILL ON AFTER CHANGE                   
         LA    R2,DISOPTNH         ERROR IF NOT ON                              
         B     INVLFLD                                                          
         NI    SNVHDCTL,X'FF'-SNVHDNTQ   SWITCH TO NETTED DOWN BIT              
         OI    SNVHDCTL,SNVHDDNQ                                                
*                                                                               
BRHDR49  TM    BRHDRHD1.SNVHDCTL,SNVHDMTQ   IF MILITARY TIME OFF                
         BZ    BRHDR50                        CAN'T MAKE IT ON                  
         TM    SNVHDCTL,SNVHDMTQ                                                
         BNZ   BRHDR50                                                          
         NI    BRHDRHD1.SNVHDCTL,X'FF'-SNVHDMTQ    BUT CAN TURN IT OFF          
*                                                                               
BRHDR50  TM    BRHDRHD1.SNVHDCTL,SNVHDAUQ                                       
         BZ    *+8                                                              
         OI    SNVHDCTL,SNVHDAUQ                                                
*                                                                               
* MAR01/2010 - IF PAYOPT USED, WE'RE FORCING PAY FLAG ON OR OFF                 
*              SO, IGNORE PREV. PAY FLAG SETTINGS (NEW PAY/UNPAY FUNC)          
         CLI   SVPAYOPT,C' '       PAY/UNPAY OPTIONS USED?                      
         BH    *+16                YES - SKIP                                   
         TM    BRHDRHD1.SNVHDCTL,SNVHDPDQ                                       
         BZ    *+8                                                              
         OI    SNVHDCTL,SNVHDPDQ                                                
*                                                                               
         MVC   HALF(1),BRHDRHD1.SNVHDCTL                                        
         MVC   HALF+1(1),SNVHDCTL                                               
         NI    HALF,SNVHDCLQ                                                    
         NI    HALF+1,SNVHDCLQ                                                  
         CLC   HALF(1),HALF+1                                                   
         BE    BRHDR52                                                          
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
         LA    R2,DISPERDH                                                      
         MVI   GERROR1,110         CHANGING FROM BROADCAST TO CALENDAR          
         B     SYS23ERR              OR VICE-VERSA, USE  -C OR -B               
*                                                                               
BRHDR52  DS    0H                                                               
* MAR01/2010 - OK FOR PAY FLAG TO CHANGE (NEW PAY/UNPAY OPTION)                 
*        CLC   SNVHDCTL,BRHDRHD1.SNVHDCTL                                       
         MVC   HALF(1),SNVHDCTL                                                 
         MVC   HALF+1(1),BRHDRHD1.SNVHDCTL                                      
         NI    HALF,X'FF'-SNVHDPDQ                                              
         NI    HALF+1,X'FF'-SNVHDPDQ                                            
         CLC   HALF(1),HALF+1                                                   
         BE    BRHDR55                                                          
         LA    R2,DISOPTNH                                                      
         B     INVLFLD                                                          
         DROP  BRHDRHD1                                                         
*                                                                               
BRHDR55  MVC   MELEM,MELEM2        WE CAN CHANGE HEADER NOW                     
         BRAS  RE,MINIOWRT                                                      
*                                                                               
BRHDRX   DS    0H                                                               
         DROP  R6                                                               
***********************************                                             
* BUILD THE HEADER'S DETAIL ORDER ELEMENT                                       
***********************************                                             
BRDTO00  LA    R6,MELEM2           SET UP THE DETAIL ORDER ELEMENT              
         USING SNVDTELD,R6                                                      
         XC    MELEM2,MELEM2                                                    
         MVI   SNVDTEL,SNVDTELQ                                                 
         ZIC   R1,SVFLDCNT                                                      
         LA    R0,SNVDTOVQ(R1)                                                  
         STC   R0,SNVDTLEN                                                      
*                                                                               
         CLI   SVFLDCNT,0          USE DEFAULT DETAIL ORDER?                    
         BNE   BRDTO10             NO                                           
         CLI   ACTNUM,ACTADD       YES, ARE WE ADDING A NEW RECORD?             
         BE    BRDTOX                   YES, NOTHING TO DO THEN                 
         B     BRDTO20                  NO, SEE IF WE HAD ONE BEFORE            
*                                                                               
BRDTO10  BCTR  R1,0                CONTINUE BUILDING DETAIL ORDER ELEM          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SNVDTFLD(0),SVFLDLST                                             
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING THE REC                            
         BNE   BRDTO20                                                          
BRDTOADD MVC   MELEM,MELEM2        THEN ADD THE HEADER ELEMENT STRAIGHT         
         BRAS  RE,MINIOADD                                                      
         BE    BRDTOX                                                           
         DC    H'0'                                                             
*****                                                                           
* ACTION CAN'T BE ADD FROM HERE ON                                              
*****                                                                           
BRDTO20  XC    MINEKEY,MINEKEY     OTHERWISE WE HAVE TO CHANGE ELEMENT          
         MVI   MINEKEY,SNVDTELQ                                                 
         BRAS  RE,MINIOHI          FOUND SOMETHING?                             
         BE    BRDTO25             YES                                          
         CLI   MINERR,MINEEOF                                                   
         BE    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
BRDTODT1 USING SNVDTELD,MELEM      DIDN'T HAVE DETAIL ORDER ELEM BEFORE         
BRDTO25  CLI   BRDTODT1.SNVDTEL,SNVDTELQ     DETAIL ORDER ELEMENT?              
         BE    BRDTO30                       NO                                 
*******                                                                         
* DID NOT HAVE DETAIL ORDER ELEM BEFORE                                         
*******                                                                         
         CLI   SVFLDCNT,0          DO WE NEED TO ADD DETAIL ORDER ELEM?         
         BE    BRDTOX              NO, SAME AS BEFORE                           
*                                                                               
*        TM    MISCFLG1,MF1GTDTL                                                
*        BZ    BRDTOADD                                                         
*        DC    H'0'                SHOULD HAVE BEEN CAUGHT BY   BRCKD           
         B     BRDTOADD            YES, EASI OR OLD STYLE                       
*******                                                                         
* HAD A DETAIL ORDER ELEM BEFORE                                                
*******                                                                         
BRDTO30  CLI   SVFLDCNT,0          WANT TO USE DEFAULT DETAIL ORDER?            
         BNE   BRDTO40                                                          
*                                                                               
         TM    MISCFLG1,MF1GTDTL                                                
         BZ    *+6                                                              
         DC    H'0'                SHOULD HAVE BEEN CAUGHT BY   BRCKD           
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))   THEN DELETE IT                      
         CLI   MINERR,0                                                         
         BE    BRDTOX                                                           
         DC    H'0'                DIE IF ANY ERRORS DELETING ELEMENT           
*                                                                               
BRDTO40  DS    0H                                                               
         MVC   MELEM,MELEM2                                                     
         BRAS  RE,MINIOWRT                                                      
*                                                                               
BRDTOX   DS    0H                                                               
         DROP  R6                                                               
*                                                                               
BLDRECX  B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILDS THE TURNAROUND REQUEST - ONE FOR OLD DATA AND ONE FOR NEW DATA         
***********************************************************************         
BLDREQ   NTR1  BASE=*,LABEL=*                                                   
         MVC   INVMOS,MINBLOCK+MINMKEY-MINBLKD+SNVKMOS-SNVKEY                   
         XC    INVMOS,=X'FFFF'                                                  
*                                                                               
         XC    RQOPTS,RQOPTS       WE DON'T HAVE ANY REQUEST OPTIONS            
*                                                                               
*        MVC   TEMPDATE(4),PEREDATE   PERIOD END DATE(4) CONTAIN MONTH          
*                                                                               
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
         OC    RCRDCLT,SPACES                                                   
*                                                                               
* ---   FIRST ADD REQUEST FOR NEW PRODUCT AND ESTIMATE                          
*                                                                               
         MVC   RCRDPRD,SVQPRDCD                                                 
         CLI   RCRDPRD,C' '                                                     
         BH    BTREQ90                                                          
BTREQ20  CLI   PI2RPOL,C'A'        IF NO PRD, TEST PROFILE                      
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
BTREQ90  CLI   SVQPR2CD,C' '                                                    
         BNH   *+10                                                             
         MVC   RCRDPRD2,SVQPR2CD                                                
*                                                                               
         BRAS  RE,SETPRD                                                        
         BE    *+16                                                             
         MVC   RCRDPRD,WORK                                                     
         MVC   RCRDPRD2,WORK+3                                                  
*                                                                               
         MVC   RCRDEST,=C'NO '                                                  
         CLI   SVESTNUM,0                                                       
         BE    BTREQ110                                                         
         EDIT  (B1,SVESTNUM),(3,RCRDEST),FILL=0                                 
*                                                                               
         BRAS  RE,SETEST                                                        
         BE    *+10                                                             
         MVC   RCRDEST,=C'NO '                                                  
*                                                                               
BTREQ110 MVC   HALF,BMKTSTA        MARKET                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDMKT,DUB                                                      
*                                                                               
         MVC   RCRDSTA,QSTA        STATION                                      
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
BTREQ120 OC    RCRDSTA,=6C' '                                                   
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+10                                                             
         MVC   RCRDAREA+56(1),QSUBMED SUBMEDIA PASSED IN QCOMPARE               
*                                                                               
* mar07/2012 - taking MOS from key, not period end                              
*        MVC   RCRDSDAT(4),PEREDATE   ELSE USE PERIOD END DATE                  
*        GOTO1 DATCON,DMCB,(2,INVMOS),(0,WORK)                                  
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
         BZ    BTREQ175                                                         
BTREQER1 MVC   RCRDBK1(6),=C'      '   INVALID BOOK - SO SKIP                   
         B     BTREQ180                                                         
*                                                                               
BTREQ175 CLC   KEY(5),0(R3)                                                     
         BE    BTREQ180            BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   BTREQER1            ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     BTREQ160                                                         
*                                                                               
BTREQ180 DS    0H                                                               
         MVC   RCRDRQTR(10),=CL10'AUTO U2 RQ'                                   
*                                                                               
         PACK  DUB,RCRDCODE                                                     
         CVB   R0,DUB                                                           
         STC   R0,RCRDCTL+10                                                    
         MVI   RCRDCTL+14,106                                                   
*                                                                               
         CLI   PI2NAI2A,C'Y'                                                    
         BNE   BTREQ190            NO, GENERATE AS USUAL                        
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
               FULL,RCRDCTL                                                     
         TM    8(R1),X'FF'                                                      
         BNZ   BTREQX                                                           
*                                                                               
* ---   NOW ADD REQUEST FOR OLD PRODUCT AND ESTIMATE                            
*                                                                               
         CLI   ACTNUM,ACTADD       DON'T HAVE OLD VALUES ON ADD                 
         BE    BTREQXX                                                          
*                                                                               
         CLC   ORIGQPRD,SPACES     IF WE HAVE ALPHA PRODUCT ALREADY             
         BNH   BTREQ200                                                         
         LA    RF,ORIGQPRD                                                      
         B     BTREQ212                                                         
*                                                                               
BTREQ200 CLI   ORIGPRD,0           ANY OLD PRODUCT                              
         BE    BTREQ220                                                         
         LA    RF,SVCLIST          GET CHAR PROD                                
BTREQ210 CLI   3(RF),0                                                          
         BE    BTREQ220                                                         
         CLC   ORIGPRD,3(RF)                                                    
         BE    BTREQ212                                                         
         LA    RF,4(RF)                                                         
         B     BTREQ210                                                         
BTREQ212 MVC   RCRDPRD,0(RF)                                                    
         B     BTREQ230                                                         
*                                                                               
BTREQ220 CLI   PI2RPOL,C'A'        IF NO PRD, TEST PROFILE                      
         BE    BTREQ222            PRD = ALL                                    
         MVC   RCRDPRD,=C'POL'     ELSE POL IF PRESENT                          
         B     *+10                                                             
BTREQ222 MVC   RCRDPRD,=C'ALL'     ELSE ALL                                     
*********                                                                       
* ONTO THE 2ND PRODUCT                                                          
*********                                                                       
BTREQ230 CLC   ORIGQPR2,SPACES     ANY PIGGY ALPHA?                             
         BNH   BTREQ231                                                         
         LA    RF,ORIGQPR2                                                      
         B     BTREQ234                                                         
*                                                                               
BTREQ231 CLI   ORIGPR2,0           PIGGY?                                       
         BE    BTREQ240                                                         
         LA    RF,SVCLIST                                                       
BTREQ232 CLI   3(RF),0                                                          
         BE    BTREQ290                                                         
         CLC   ORIGPR2,3(RF)                                                    
         BE    BTREQ234                                                         
         LA    RF,4(RF)                                                         
         B     BTREQ232                                                         
BTREQ234 MVC   RCRDPRD2,0(RF)                                                   
*                                                                               
BTREQ240 MVC   RCRDEST,=C'NO '                                                  
         CLI   ORIGEST,0                                                        
         BE    BTREQ290                                                         
         EDIT  (B1,ORIGEST),(3,RCRDEST),FILL=0                                  
*                                                                               
BTREQ290 DS    0H                                                               
         BRAS  RE,SETPRD                                                        
         BE    *+16                                                             
         MVC   RCRDPRD,WORK                                                     
         MVC   RCRDPRD2,WORK+3                                                  
*                                                                               
         BRAS  RE,SETEST                                                        
         BE    *+10                                                             
         MVC   RCRDEST,=C'NO '                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',                     X        
               FULL,RCRDCTL                                                     
         TM    8(R1),X'FF'                                                      
         BNZ   BTREQX                                                           
*                                                                               
BTREQXX  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'** REQUEST GENERATED **'                          
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
BTREQX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE DETAIL ORDER                                                     
*                                                                               
* ON ENTRY:    (R6)                A(DETAIL ORDER ELEMENT)                      
***********************************************************************         
SHOWDTOR NTR1  BASE=*,LABEL=*                                                   
         USING SNVDTELD,R6                                                      
         LA    RE,SNVDTFLD         RE = A(1ST DETAIL OF FIELD LIST)             
         ZIC   R0,SNVDTLEN         R0 = NUMBER OF FIELDS IN LIST                
         SH    R0,=Y(SNVDTOVQ)                                                  
         STC   R0,SVFLDCNT                                                      
         LA    R3,BLOCK            R3 = A(UNSCAN BLOCK)                         
*                                                                               
SDTO10   LARL  R1,FLDEQTBL                                                      
SDTOLP   CLI   0(R1),0             ENTRY NOT IN OUR TABLE?                      
         BNE   *+6                                                              
         DC    H'0'                YES, THEN DIE                                
*                                                                               
         CLC   0(1,R1),0(RE)       MATCH ON THIS FIELD?                         
         BE    *+12                                                             
         LA    R1,L'FLDEQTBL(R1)                                                
         B     SDTOLP                                                           
*                                                                               
         MVC   0(20,R3),SPACES     COPY TEXT TO BLOCK ENTRY                     
         MVC   0(8,R3),2(R1)                                                    
*                                                                               
         LA    R3,20(R3)           BUMP TO NEXT ENTRY                           
         LA    RE,1(RE)                                                         
         BCT   R0,SDTO10           UNTIL ALL ENTRIES COMPLETED                  
*                                                                               
         GOTO1 UNSCAN,DMCB,(SVFLDCNT,BLOCK),DISDTL1H                            
         CLI   0(R1),0                                                          
         BE    SDTOX                                                            
*                                                                               
         GOTO1 UNSCAN,DMCB,,DISDTL2H                                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SDTOX    J     XIT                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ LAST INVOICE FOR THE STATION AND COPY THE DETAIL LIST FROM IT            
***********************************************************************         
COPYDTLS DS    0H                                                               
         NMOD1 0,**CDTL**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
         LA    R1,KEY                                                           
         USING SNVKEY,R1                                                        
         XC    KEY,KEY                                                          
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),INVDIR,KEY,AIO                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVKMAST(SNVKMOS-SNVKEY),KEY    SAME STATION?                    
         BNE   CDTLSX                          NO, NOTHING TO COPY              
         MVC   KEY(4),SNVDDA                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),INVFIL,KEY,AIO,DMWORK                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,SNVELS                                                        
CDTLS10  CLI   0(R6),0             DEFAULT DETAIL LIST                          
         BE    CDTLSX                                                           
         CLI   0(R6),SNVDTELQ      DO WE HAVE A DETAIL ORDER ELEM?              
         BE    CDTLS20             YES                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CDTLS10                                                          
*                                                                               
CDTLS20  GOTO1 =A(SHOWDTOR),RR=RELO                                             
*        BRAS  RE,SHOWDTOR         SHOW THE DETAIL ORDER                        
         OI    DISDTL1H+6,X'80'                                                 
         OI    DISDTL2H+6,X'80'                                                 
*                                                                               
CDTLSX   B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*&&DO                                                                           
***********************************************************************         
* SETS MINIO UP TO USE THE MATCHING STATUS RECORD                               
***********************************************************************         
SETMSRRC NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINCLS',(R5))    YES                                
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    MNIOFLAG,X'FF'-X'80'                                             
*                                                                               
         MVI   MINOPEN,C'N'        MINIO SET NOT OPEN                           
         MVI   MINFKLEN,L'MSRKEY   KEY LENGTH                                   
         MVI   MINEKLEN,L'MSRKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'MSRKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,L'MSRRSTAT  NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(3900) MAXIMUM RECORD LENGTH                        
* BUILD MASTER KEY                                                              
         XC    MINMKEY,MINMKEY     CLEAR MASTER KEY FOR MINIO                   
         LA    R3,MINMKEY                                                       
         USING MSRKEY,R3                                                        
         MVI   MSRKTYPE,MSRKTYPQ                                                
         MVI   MSRKSUB,MSRKSUBQ                                                 
         MVC   MSRKAM,BAGYMD                                                    
         MVC   MSRKCLT,BCLT                                                     
         MVC   MSRKPRD,BPRD                                                     
         MVC   MSRKEST,BEST                                                     
         MVC   MSRKMKT(L'BMKTSTA),BMKTSTA                                       
         MVC   MSRKMOS,BMOSS                                                    
         XC    MSRKMOS,=X'FFFF'                                                 
         DROP  R3                                                               
*                                                                               
SETMSRX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*&&                                                                             
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
         MVC   L.LOCKRTY,=C'NV'    LOCKED INVOICE                               
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
***********************************************************************         
* THIS ROUTINE WILL SIGNAL TO MATCHMAKER THAT THIS RECORD NO LONGER             
* MATCHS BECAUSE CHANGES TO THE ESTIMATE OR/AND THE PRODUCT WERE MADE.          
*                                                                               
* ON EXIT:     RECORD MATCH FLAG TURNED OFF IN SNVDSTAT+1                       
*              SNVIDBES,SNVIDBLN,SNVIDBDT,SNVIDBNO,DNVIDBOR : CLEARED!          
*              DELETE PASSIVE KEY AND E8/E9 ELEMS AND ADD FE ELEM               
***********************************************************************         
OFFMATCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,MINMKEY                                                       
         USING SNVKEY,R6                                                        
***********************************************************************         
*        KEY:  TURN MATCH STATUS BIT OFF.                                       
***********************************************************************         
*                                                                               
         NI    SNVDSTAT+1,X'FF'-X'80'                                           
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
OFF01    XC    MINEKEY,MINEKEY     IF 'E8' ELEM NOT FOUND                       
         MVI   MINEKEY,X'E8'       PROBABLY MEANS NO DETAILS                    
         MVI   MINFILTL,1          WHICH MEANS NOTHING HAS TO BE CLRED          
         XC    MELEM,MELEM         AND TURNED OFF                               
         LA    R6,MELEM                                                         
         USING SNVMMELD,R6                                                      
         BRAS  RE,MINIOHI                                                       
         BNE   OFFX                                                             
         CLI   SNVMMEL,SNVMMELQ    X'E8'                                        
         BNE   OFFX                                                             
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
*        PASSIVE KEY: DELETE                                                    
***********************************************************************         
OFF03    MVC   XSVPKEY,XPKEY       SAVE THE PASSIVE KEY                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),INVDIR,XPKEY,XPKEY,0                 
         CLC   XSVPKEY(32),XPKEY                                                
         BNE   OFFX                                                             
         OI    XPKEY+32,X'80'                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),INVDIR,XPKEY,XPKEY,0                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*        DETAIL ELEMENT: CLEAR SNVIDBES(6)                                      
***********************************************************************         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'40'                                                    
         MVI   MINFILTL,1                                                       
         XC    MELEM,MELEM                                                      
         LA    R6,MELEM                                                         
         USING SNVIDELD,R6                                                      
         BRAS  RE,MINIOHI                                                       
         BNE   OFFX                NO MORE DETAIL ELEMENTS?                     
         CLI   SNVIDEL,SNVIDELQ    X'40'                                        
         BNE   OFFX                                                             
*                                                                               
OFF10    XC    SNVIDBES(6),SNVIDBES        CLEAR!                               
         BRAS  RE,MINIOWRT                 WRITE!                               
*                                                                               
         MVC   MINEKEY+1(5),SNVIDDAY       ***> MUST RESET AFTER WRITE          
         BRAS  RE,MINIOHI                  **>  I HATE MINIO                    
*                                                                               
         BRAS  RE,MINIOSEQ                 NEXT!                                
         BNE   OFF20                                                            
         CLI   SNVIDEL,SNVIDELQ                                                 
         BE    OFF10                       LOOP AND DO IT AGAIN!                
*                                                                               
OFF20    XC    MINEKEY,MINEKEY     DELETE X'E8' AND X'E9'                       
         MVI   MINEKEY,X'E8'                                                    
         MVI   MINFILTL,1                                                       
         XC    MELEM,MELEM                                                      
         BRAS  RE,MINIOHI                                                       
         BNE   OFF30                                                            
         BRAS  RE,MINIODEL                                                      
*                                                                               
OFF30    XC    MINEKEY,MINEKEY     DELETE X'E8' AND X'E9'                       
         MVI   MINEKEY,X'E9'                                                    
         XC    MELEM,MELEM                                                      
         BRAS  RE,MINIOHI                                                       
         BNE   OFF40               NO MORE DETAIL ELEMENTS?                     
         BRAS  RE,MINIODEL                                                      
*                                                                               
OFF40    XC    MINEKEY,MINEKEY     ADD X'FE' DUMMY ELEM                         
         MVI   MINEKEY,X'FE'                                                    
         XC    MELEM,MELEM                                                      
         BRAS  RE,MINIOHI                                                       
         BE    OFFX                ALREADY THERE                                
*                                                                               
         XC    MELEM,MELEM         ADD THIS SO UPDATIVE SOON WON'T DIE          
         MVI   MELEM,X'FE'            WHEN THE RECORD NEEDS TO SPLIT            
         MVI   MELEM+1,X'4B'                                                    
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OFFX     MVI   CHGFLAG,0                                                        
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CLEAREST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'40'                                                    
         MVI   MINFILTL,1                                                       
         XC    MELEM,MELEM                                                      
         LA    R6,MELEM                                                         
         USING SNVIDELD,R6                                                      
         BRAS  RE,MINIOHI                                                       
         BNE   CLRESTX             NO MORE DETAIL ELEMENTS?                     
         CLI   SNVIDEL,SNVIDELQ    X'40'                                        
         BNE   CLRESTX                                                          
*                                                                               
CLREST10 MVI   SNVIDEST,0         CLEAR ESTIMATE                                
         BRAS  RE,MINIOWRT                 WRITE!                               
*                                                                               
         MVC   MINEKEY+1(5),SNVIDDAY       ***> MUST RESET AFTER WRITE          
         BRAS  RE,MINIOHI                  **>  I HATE MINIO                    
*                                                                               
         BRAS  RE,MINIOSEQ                 NEXT!                                
         BNE   CLRESTX                                                          
         CLI   SNVIDEL,SNVIDELQ                                                 
         BE    CLREST10                    LOOP AND DO IT AGAIN!                
CLRESTX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DELPSSV- DELETE PASSIVE KEYS BASED ON STATUS ELEM                      
*                                                                               
*               - ON INPUT R5 POINTS TO MINIOBLOCK                              
***********************************************************************         
*                                                                               
DELPSSV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVMMELQ                                                 
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BNE   DELPX                                                            
*                                                                               
         L     R3,MINELEM                                                       
         BRAS  RE,BLDPSSV                                                       
*                                                                               
         L     R3,MINELEM                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY2,XKEY3,0                 
         CLC   XKEY2(SNVKMINK-SNVKEY),XKEY3                                     
         BNE   DELPX                                                            
         OI    XKEY3+32,X'80'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEY3,XKEY3,0                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELPX    DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        RSTPSSV- RESTORE PASSIVE KEYS BASED ON STATUS ELEM                     
*                                                                               
*               - ON INPUT R5 POINTS TO MINIOBLOCK                              
***********************************************************************         
*                                                                               
RSTPSSV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVMMELQ                                                 
         BRAS  RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BNE   RSTPX                                                            
*                                                                               
         L     R3,MINELEM                                                       
         BRAS  RE,BLDPSSV                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',XKEY2,XKEY3,0         
         CLC   XKEY2(SNVKMINK-SNVKEY),XKEY3                                     
         BNE   RSTPX                                                            
         NI    XKEY3+32,X'7F'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEY3,XKEY3,0                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSTPX    DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *               
* BLDPSSV - R3 IS EXPECTED TO ADDRESS MATCHMAKER STATUS ELEM                    
* PASSIVE KEY IS BUILT IN XKEY2                                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *               
*                                                                               
BLDPSSV  NTR1  BASE=*,LABEL=*                                                   
         USING SNVMMELD,R3                                                      
         XC    XKEY2,XKEY2                                                      
*                                                                               
         CLI   NETPAKSW,C'Y'         IS IT NETPAK                               
         BE    BLDP50                                                           
*                                                                               
         LA    R2,XKEY2            SPOT PASSIVE KEY                             
         USING SNVPKEY,R2                                                       
         MVI   SNVPTYP,SNVPTYPQ    X'0E'                                        
         MVI   SNVPSUB,SNVPSUBQ    X'83'                                        
         MVC   SNVPAM,BAGYMD                                                    
         MVC   SNVPMKT,SNVMMMKT                 MARKET                          
         MVC   SNVPSTA,BMKTSTA+2                STATION                         
*                                                                               
         CLI   SNVPSTA,X'E8'       CABLE?                                       
         BL    *+8                                                              
         NI    SNVPSTA+2,X'80'     TURN OFF NETWORK BITS                        
*                                                                               
         MVC   SNVPMOS,MINMKEY+SNVKMOS-SNVKEY   MONTH OF SERVICE                
         MVC   SNVPCLT,BCLT                     CLIENT                          
*                                                                               
         MVC   SNVPPRD,SNVMMRP1                 PRODUCTS                        
         MVC   SNVPPRD2,SNVMMRP2                                                
*                                                                               
         MVC   SNVPEST,SNVMMRE1                 ESTIMATE                        
         MVC   SNVPEST2,SNVMMRE2                ESTIMATE                        
         MVC   SNVPINV,MINMKEY+SNVKINV-SNVKEY   INVOICE NUMBER                  
*                                                                               
         B     BLDPX                                                            
         DROP  R2                                                               
*                                                                               
BLDP50   LA    R2,XKEY2            NETPAK PASSIVE KEY                           
         USING SNVNKEY,R2                                                       
         MVI   SNVNTYP,SNVNTYPQ    X'0E'                                        
         MVI   SNVNSUB,SNVNSUBQ    X'93'                                        
         MVC   SNVNAM,BAGYMD                                                    
         MVC   SNVNCLT,BCLT                     CLIENT                          
         MVC   SNVNNETW,SNVMMNWK                NETWORK                         
         MVC   SNVNMOS,MINMKEY+SNVKMOS-SNVKEY   MONTH OF SERVICE                
         MVC   SNVNPRD,SNVMMRP1                 PRODUCTS                        
         MVC   SNVNPRD2,SNVMMRP2                                                
         MVC   SNVNEST,SNVMMRE1                 ESTIMATE                        
         MVC   SNVNEST2,SNVMMRE2                ESTIMATE                        
         MVC   SNVNINV,MINMKEY+SNVKINV-SNVKEY   INVOICE NUMBER                  
         MVC   SNVNPGR,SNVMMRPG                 PRODUCT GROUP                   
         DROP  R2                                                               
*                                                                               
BLDPX    DS    0H                                                               
         J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
INVPROF  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DISDATEH                                                      
         CLI   5(R2),0              ANY INPUT?                                  
         JNE   YES                  YES                                         
         CLC   8(L'DISDATE,R2),SPACES                                           
         JH    YES                                                              
         CLI   PINVDAT,C'Y'         PROFILE REQUIRES DATE INPUT?                
         JNE   NO                   NO                                          
         MVC   GERROR,=AL2(982)     DATE REQUIRED                               
         J     ERREXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         JE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         LTORG                                                                  
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
         LTORG                                                                  
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
         LTORG                                                                  
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
         LTORG                                                                  
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
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
***********************************************************************         
MINIODEL NTR1  BASE=*,LABEL=*                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         JE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         LTORG                                                                  
*                                                                               
***********************************************************************         
* CHECKS TO SEE IF DETAIL FIELD TO BE DELETED HAS NULLS IN ALL THE              
* DETAIL ELEMENTS.  (MAKEGOOD AND BILLBOARD CHECK THE SNVIDCTL BYTE)            
***********************************************************************         
REMOVFLD NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE,FLDNMKGD       CHECKING MAKEGOOD?                           
         BNE   *+12                                                             
         MVI   BYTE,SNVIDMGQ                                                    
         B     RFLD10                                                           
*                                                                               
         CLI   BYTE,FLDNBILB       CHECKING BILLBOARD?                          
         BNE   RFLD20                                                           
         MVI   BYTE,SNVIDBLQ                                                    
RFLD10   OI    MISCFLG1,MF1CKBIT   WE NEED TO CHECK SNVIDCTL                    
         B     RFLD40                                                           
*                                                                               
RFLD20   NI    MISCFLG1,X'FF'-MF1CKBIT   NOT CHECKING SNVIDCTL                  
         L     RE,=A(PURGETBL)                                                  
         A     RE,RELO                                                          
RFLD25   CLI   0(RE),0             IF WE HIT END OF TABLE                       
         BE    RFLDNO              THEN THE FIELD CAN'T BE REMOVED              
*                                                                               
         CLC   BYTE,0(RE)          MATCH ON DETAIL FIELD?                       
         BE    RFLD30                                                           
         LA    RE,L'PURGETBL(RE)   NO                                           
         B     RFLD25                                                           
*                                                                               
RFLD30   ZIC   R2,1(RE)            R2 = # OF BYTES INTO DETAIL ELEM             
         ZIC   R3,2(RE)            R3 = # OF BYTES TO CHECK                     
         BCTR  R3,0                  DECREMENT FOR EX INSTRUCTION               
*                                                                               
RFLD40   XC    MINEKEY,MINEKEY     EXAMINE ALL THE DETAIL ELEMENTS              
         MVI   MINEKEY,SNVIDELQ                                                 
         LA    R6,MELEM                                                         
         BRAS  RE,MINIOHI                                                       
         BE    RFLD50                                                           
         CLI   MINERR,MINEEOF      NO DETAILS?                                  
         BE    RFLDYES             NONE, THEN IT'S OKAY                         
         DC    H'0'                                                             
*                                                                               
         USING SNVIDELD,R6                                                      
RFLD50   DS    0H                                                               
         CLI   SNVIDELD,SNVIDELQ   DETAIL ELEMENT?                              
         BE    RFLD55              OKAY TO REMOVE FIELD                         
*                                                                               
* HERE, WE LOOKED AT ALL DETAIL ELEMENTS, AND THEY'RE ALL OK                    
         CLI   BYTE,FLDNESTM                                                    
         BNE   RFLDYES             FOR ESTIMATE FIELD                           
         BRAS  RE,CLEAREST         CLEAR ESTIMATE VALUES                        
         B     RFLDYES                                                          
*                                                                               
RFLD55   DS    0H                                                               
         TM    MISCFLG1,MF1CKBIT   BIT TEST?                                    
         BZ    RFLD60                                                           
         MVC   HALF(1),BYTE        YES, TEST BIT AGAINST SNVIDCTL               
         NC    HALF(1),SNVIDCTL                                                 
         BNZ   RFLDNO              IF ON, THEN CAN'T REMOVE FIELD               
         B     RFLD70                                                           
*                                                                               
RFLD60   LA    R1,0(R2,R6)                                                      
*                                                                               
         CLI   BYTE,FLDNESTM                                                    
         BNE   RFLD65                                                           
         CLI   0(R1),0                                                          
         BE    RFLD70                                                           
         CLC   0(1,R1),SVESTNUM                                                 
         BE    RFLD70                                                           
         B     RFLDNO                                                           
*                                                                               
RFLD65   DS    0H                                                               
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         OC    0(0,R1),0(R1)       ANYTHING HERE?                               
         BNZ   RFLDNO              YES, CAN'T REMOVE DETAIL FIELD               
*                                                                               
RFLD70   BRAS  RE,MINIOSEQ         NO, CHECK NEXT DETAIL ELEMENT                
         BE    RFLD50              CHECK ALL DETAIL ELEMENTS                    
*                                                                               
RFLDYES  J     YES                                                              
*                                                                               
RFLDNO   J     NO                                                               
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* DETAIL FIELDS WE CAN REMOVE TABLE                                             
*                                                                               
* BYTE  0      - FIELD EQUATE                                                   
* BYTE  1      - NUMBER OF BYTES INTO DETAIL ELEMENT                            
* BYTE  2      - NUMBER OF BYTES TO TEST WITH 'OC'                              
***********************************************************************         
PURGETBL DS    0XL3                                                             
         DC    AL1(FLDNFILM),AL1(SNVIDCML-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNCOST),AL1(SNVIDCST-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNESTM),AL1(SNVIDEST-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNRCNT),AL1(SNVIDRSP-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNPFLM),AL1(SNVIDCM2-SNVIDELD),AL1(L'SNVIDEST)             
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SETUP    NTR1  BASE=*,LABEL=*                                                   
         CLI   CALLSP,1                                                         
         BNE   *+14                                                             
         MVC   DISPFLN+43(11),=CL11'PF12=Return'                                
         B     *+10                                                             
         MVC   DISPFLN+43(11),=CL11'PF12=List'                                  
         OI    DISPFLNH+6,X'80'                                                 
*                                                                               
         CLI   NETPAKSW,C'Y'       NETPAK?                                      
         BNE   *+10                                                             
         MVC   DISPFLN+12(9),=9C' '   DON'T SHOW PF5=MATCH                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           AGENCY RECORD                                
         MVC   KEY+1(2),AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                NO AGENCY RECORD FOUND                       
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO "01" ELEMENT FOUND                        
         USING AGYEL,R6                                                         
         MVC   CANADIEN,AGYPCNDA                                                
         DROP  R6                                                               
*                                                                               
         MVI   DISPTAX,C'N'                                                     
         CLI   CANADIEN,C'C'                                                    
         BNE   *+8                                                              
         MVI   DISPTAX,C'Y'                                                     
*                                                                               
         CLI   NETPAKSW,C'Y'       NET MEDIA?                                   
         JE    XIT                                                              
         OI    DISPKGLH+1,X'2C'                                                 
         OI    DISPKGNH+1,X'2C'                                                 
         OI    DISPKGH+1,X'2C'                                                  
         OI    DISPKGLH+6,X'80'                                                 
         OI    DISPKGNH+6,X'80'                                                 
         OI    DISPKGH+6,X'80'                                                  
         J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* RESTORE A DELETED INVOICE                                                     
***********************************************************************         
RESTREC  NTR1  BASE=*,LABEL=*                                                   
         MVI   MINDELSW,C'Y'       PROCESS DELETED RECORD SET                   
         GOTO1 MINIO,DMCB,('MINRSF',(R5))                                       
         CLI   MINERR,0                                                         
         BE    RESTREC1                                                         
         CLI   MINERR,MINENDEL     RECORD SET NOT DELETED, 2 USERS              
         JE    RCNOTDEL                                                         
         DC    H'0'                                                             
*                                                                               
RESTREC1 BRAS  RE,RSTPSSV                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BPRD,SNVHDPRD                                                    
         MVC   BEST,SNVHDEST                                                    
         DROP  R6                                                               
*&&DO                                                                           
         CLC   =C'CK',AGENCY       COKE?                                        
         BNE   RESTRECX                                                         
*                                                                               
         BRAS  RE,SETMSRRC                                                      
*                                                                               
         MVI   MINDELSW,C'Y'       PROCESS DELETED RECORD SET                   
         GOTO1 MINIO,DMCB,('MINRSF',(R5))                                       
         CLI   MINERR,MINESNF      RECORD SET NOT FOUND                         
         BE    RRX                                                              
         CLI   MINERR,MINENDEL     RECORD NOT DELETED                           
         BE    RRX                                                              
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
RESTRECX GOTO1 MINIO,DMCB,('MINCLS',(R5))    YES                                
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   MINDELSW,C'N'       PROCESS DELETED RECORD SET                   
RRX      XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
* DELETE THE WHOLE INVOICE                                                      
***********************************************************************         
DELREC   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,TSTLOCK                                                       
         BE    DELREC10                                                         
         MVC   GERROR,=AL2(808)                                                 
         LA    R2,DISMEDH                                                       
         J     NO                                                               
*        B     ERREXIT                                                          
*                                                                               
DELREC10 DS    0H                                                               
*                                                                               
         BRAS  RE,DELPSSV                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BPRD,SNVHDPRD                                                    
         MVC   BEST,SNVHDEST                                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 MINIO,DMCB,('MINDLF',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         CLC   =C'CK',AGENCY       COKE?                                        
         BNE   DELRECX                                                          
*                                                                               
         BRAS  RE,SETMSRRC                                                      
*                                                                               
         GOTO1 MINIO,DMCB,('MINDLF',(R5))                                       
         CLI   MINERR,MINESNF      RECORD SET NOT FOUND                         
         BE    DELRECX                                                          
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 MINIO,DMCB,('MINCLS',(R5))    YES                                
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
DELRECX  J     YES                                                              
*                                                                               
*                                                                               
DISACT   NTR1  BASE=*,LABEL=*                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LLC   R3,0(R2)            FIELD LENGTH                                 
         AR    R3,R2               R3 = END OF FIELD                            
         LA    R2,8(R2)            MOVE R2 TO FIELD ITSELF, FROM HEADER         
*                                                                               
         USING MINBLKD,R5                                                       
*                                                                               
         L     R6,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'F1'       X'F1' - ACTIVITY ELEMENT                     
         BRAS  RE,MINIOHI                                                       
         CLI   0(R6),X'F1'                                                      
         BNE   DISACTX             NO ACTIVITY ELEMENT                          
*                                                                               
         MVC   0(9,R2),=C'ADDED ON '                                            
         LA    R2,9(R2)                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT-ACTVD(R6)),(5,0(R2))                     
         LA    R2,8(R2)                                                         
*                                                                               
         OC    ACTVCHDT-ACTVD(3,R6),ACTVCHDT-ACTVD(R6)                          
         BZ    DISACTX             NO ACTIVITY DATE                             
*                                                                               
         MVC   0(2,R2),=CL2', '                                                 
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(16,R2),=C'LAST CHANGED ON '                                    
         LA    R2,16(R2)                                                        
         GOTO1 DATCON,DMCB,(3,ACTVCHDT-ACTVD(R6)),(5,0(R2))                     
         LA    R2,8(R2)                                                         
*                                                                               
         MVC   SVSECID,ACTVCHID-ACTVD(R6)  NEW SEC PERSONAL ID                  
*                                                                               
* GET THE SECURITY AGY FROM GETFACT                                             
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC   SECURITY AGENCY                              
         DROP  R1                                                               
*                                                                               
         CLC   ACTVCHID-ACTVD(2,R6),=X'0FFF'   DDS PID?                         
         BH    DISAC10             NO                                           
         LA    R2,1(R2)            SPACE AFTER CHANGE DATE                      
         MVC   0(6,R2),=C'BY DDS'                                               
         LA    R2,6(R2)                                                         
         B     DISACTX                                                          
*                                                                               
* READ PASSWORD RECORD                                                          
DISAC10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(2),SVSECAGY                                                
         CLC   SVSECAGY,=C'  '     DO WE HAVE SECURITY AGY?                     
         BH    *+10                YES, USE IT INSTEAD OF AGENCY                
         MVC   KEY+1(2),AGENCY                                                  
         MVC   KEY+23(2),SVSECID   PID                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO3                  
         L     R6,AIO3                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   DISACTX                                                          
*                                                                               
         MVI   ELCODE,X'C3'                                                     
         MVC   SVGETEL+4(2),DATADISP    SET UP DATADISP FOR CTFILE              
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         MVC   DATADISP,SVGETEL+4       RESTORE DATADISP                        
         BNE   DISACTX                  NO NAME ELEMENT                         
         MVC   SVSECPID,2(R6)                                                   
*                                                                               
*                                                                               
* READ PERSON RECORD                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,C'F'                                                         
         MVI   KEY+1,X'04'                                                      
         MVC   KEY+13(2),SVSECAGY                                               
         CLC   SVSECAGY,=C'  '     DO WE HAVE SECURITY AGY?                     
         BH    *+10                YES, USE IT INSTEAD OF AGENCY                
         MVC   KEY+13(2),AGENCY                                                 
         MVC   KEY+15(8),SVSECPID       PID                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO3                  
         L     R6,AIO3                                                          
         CLC   KEY(23),0(R6)                                                    
         BNE   DISACTX                                                          
*                                                                               
         MVI   ELCODE,X'C5'                                                     
         MVC   SVGETEL+4(2),DATADISP    SET UP DATADISP FOR CTFILE              
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         MVC   DATADISP,SVGETEL+4       RESTORE DATADISP                        
         BNE   DISACTX                  NO NAME ELEMENT                         
*                                                                               
         LA    R2,1(R2)            SPACE AFTER CHANGE DATE                      
         MVC   0(3,R2),=C'BY '                                                  
         LA    R2,3(R2)            SPACE AFTER CHANGE DATE                      
*                                                                               
         LA    R6,11(R6)           A(NAME SUB-ELEMENT)                          
*                                                                               
         LLC   R1,0(R6)            L'(FIRST NAME)                               
*                                                                               
* MAKE SURE IT FITS IN THE FIELD                                                
*                                                                               
         LA    RF,0(R2,R1)         RF - END OF TEXT                             
         CR    RF,R3               PAST END OF FIELD?                           
         BNH   *+8                 NO - WE'RE OK                                
         SR    R3,R2               CHARACTERS REMAINING                         
         LR    R1,R3               USE THAT, INSTEAD OF L'NAME                  
*                                                                               
         BCTR  R1,0                                                             
         CHI   R1,0                                                             
         BNH   DISACTX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(R6)           COPY FIRST NAME                          
*                                                                               
* ADVANCE SCREEN(R2), ELEMENT NAME POINTER (R6)                                 
*                                                                               
         LLC   R1,0(R6)                L'(FIRST NAME)                           
         AHI   R1,1                                                             
         AR    R2,R1                                                            
         AR    R6,R1                                                            
*                                                                               
         LLC   R1,0(R6)                L'(LAST NAME)                            
*                                                                               
* MAKE SURE IT FITS IN THE FIELD                                                
*                                                                               
         LA    RF,0(R2,R1)         RF - END OF TEXT                             
         CR    RF,R3               PAST END OF FIELD?                           
         BNH   *+8                 NO - WE'RE OK                                
         SR    R3,R2               CHARACTERS REMAINING                         
         LR    R1,R3               USE THAT, INSTEAD OF L'NAME                  
*                                                                               
         BCTR  R1,0                                                             
         CHI   R1,0                                                             
         BNH   DISACTX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(R6)                                                    
*                                                                               
DISACTX  DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
OPTNTABL DS    0CL9                                                             
         DC    AL1(SNVHDNTQ),CL8'NET'                                           
         DC    AL1(SNVHDDNQ),CL8'NET'  IGNORED IN BRLIDATING                    
         DC    AL1(SNVHDMCQ),CL8'MCT'                                           
         DC    AL1(SNVHDMTQ),CL8'MILITARY'                                      
*                                                                               
         DC    X'00'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ON RETURN IF BYTE CONTAINS X'01' - PAID INVOICE.  EST LOCK OTHERWISE          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHKEST   NTR1  BASE=*,LABEL=*                                                   
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   P0A0APAY,C'Y'       ARE WE LOCKING PAID INVOICES?                
         BNE   CHKE10              NO. SEE IF ANY ESTIMATES ARE LOCKED          
*                                                                               
         MVI   BYTE,X'01'          ERROR CODE                                   
         TM    SVCTRLB,SNVHDPDQ    PAID?                                        
         JO    NO                  YES - ISSUE ERROR MESSAGE                    
*                                                                               
CHKE10   DS    0H                                                               
         CLI   P0I2PLKE,C'Y'       PREVENTING CHANGES FOR LOCKED ESTS?          
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
         CLI   P0I2PLKE,C'Y'       PREVENTING CHANGES FOR LOCKED ESTS?          
         JNE   YES                 NO DON'T READ ESTIMATES                      
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
         MVC   SVCTRLB,SNVHDCTL    SAVE CONTROL BYTE                            
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
*                                                                               
* CHECK INDIVIDUAL PRODUCTS NOW                                                 
*                                                                               
         OC    SVELOCK1,SVELOCK1    INFO ALREADY SAVED?                         
         JNZ   YES                  YES - DON'T READ IT                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MINMKEY+2            A/M,CLT                            
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
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO3                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         L     R2,AIO3                                                          
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
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFIL',KEY+14,AIO3                 
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
         L     R2,AIO3                                                          
         USING ESTHDR,R2                                                        
         MVC   SVECNTR2,ECNTRL-ESTHDR(R2)                                       
         MVC   SVELKYM2,ELOCKYR-ESTHDR(R2)                                      
         DROP  R2                                                               
*                                                                               
         J     YES                                                              
*                                                                               
*                                                                               
*                                                                               
READPROF NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETPRFIL,DMCB,=C'S0I2',PROF0I2                                   
         GOTO1 GETPRFIL,DMCB,=C'sI2P',PROF0I2P  <= lowercase 's'!!!             
         GOTO1 GETPRFIL,DMCB,=C'sA0A',PROF0A0A  <= lowercase 's'!!!             
         GOTO1 GETPRFIL,DMCB,(X'90',=C'sI2N'),PROFI2N lowercase 's'!            
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         JNE   YES                                                              
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2Z',PROFI2Z    !!! lowercase 's' !!!          
         CLI   PROFI2Z+6,C'Y'     READ PROFILES BY SUBMEDIA?                    
         JNE   YES                                                              
         CLC   SVSTYPE,QMED                                                     
         JE    YES                                                              
         CLI   SVSTYPE,C' '                                                     
         JE    YES                                                              
         MVC   SVMEDIA,QMED                                                     
         MVC   QMED,SVSTYPE        GET SUBMEDIA                                 
         GOTO1 GETPRFIL,DMCB,=C'S0I2',PROF0I2     RE-READ I2 PROFILE            
         MVC   QMED,SVMEDIA                                                     
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE VALIDATES NETWORK PACKAGE FIELD                                       
* ON INPUT R2 EXPECTED TO ADDRESS THE PACKAGE FIELD HEADER                      
* EQUAL CONDITION AND SVPKG SET ON EXIT IF PACKAGE OK OR NO VALIDATION          
*                                                              REQUIRED         
* UNEQUAL CONDITION WHEN INVALID PACKAGE                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
VALPKG   NTR1  BASE=*,LABEL=*                                                   
         XC    SVPKG,SVPKG                                                      
*                                                                               
         CLI   NETPAKSW,C'Y'       NET MEDIA?                                   
         BNE   VALPQX                                                           
         CLI   5(R2),0                                                          
         BE    VALPQX                                                           
*                                                                               
         TM    4(R2),X'08'                                                      
         BZ    VALPNQX                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
*                                                                               
         CH    RE,=H'1'            TEST IN RANGE 1-255                          
         BL    VALPNQX                                                          
         CH    RE,=H'255'                                                       
         BH    VALPNQX                                                          
*                                                                               
         STC   RE,SVPKG            SET BINARY PACKAGE #                         
         B     VALPQX   !!! DO NOT READ PACKAGE RECORD NOW !!!                  
*                                                                               
*&&DO                                                                           
                                                                                
         BE    VRPKGX                                                           
*                                                                               
         XC    PKGRECD,PKGRECD                                                  
         LA    R3,PKGRECD                                                       
         USING NPRECD,R3                                                        
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,QSTA                                                      
         MVC   NPKEST,SVESTNUM                                                  
         MVC   NPKPACK,SVPKG                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR',PKGRECD,AIO3                  
         L     R3,AIO3                                                          
         CLC   PKGRECD,0(R3)                                                    
         BNE   BADPKG                                                           
*&&                                                                             
*                                                                               
VALPQX   J     YES                                                              
*                                                                               
VALPNQX  J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
VALDEM   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         XC    SVDEMO,SVDEMO                                                    
         CLI   5(R2),0                                                          
         BE    VALDQX                                                           
         TM    MISCFLG1,MF1SDSTA                                                
         BZ    VALDNQX                                                          
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = TP                            
         MVC   DBSELMED,QMED                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOVAL-COMFACSD(RF)                                         
         LA    R3,WORK                                                          
         GOTO1 (RF),DMCB,(1,(R2)),(1,(R3)),(C'S',DBLOCK),0                      
         CLI   DMCB+4,0                                                         
         BE    VALDNQX                                                          
         MVC   SVDEMO,WORK                                                      
*                                                                               
VALDQX   J     YES                                                              
VALDNQX  J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
DISDEM   NTR1  BASE=*,LABEL=*                                                   
         USING SNVHDELD,R6                                                      
         MVC   DISDEMO,SPACES                                                   
         OC    SNVHDDEM,SNVHDDEM                                                
         BZ    DISDQX                                                           
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = TP                            
         MVC   DBSELMED,QMED                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOCON-COMFACSD(RF)                                         
         LA    R3,WORK                                                          
         GOTO1 (RF),DMCB,(1,SNVHDDEM),(2,(R2)),(C'S',DBLOCK),0                  
*                                                                               
DISDQX   J     YES                                                              
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
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
         MVC   WORK,SPACES                                                      
*                                                                               
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BE    SETP20                                                           
*                                                                               
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
*                                                                               
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
*                                                                               
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         MVC   BOOKOVR,SPACES                                                   
         MVI   SVCTLBYT,0                                                       
*                                                                               
         CLI   P0I2BC,C'C'         CALENDAR MONTHS?                             
         BNE   *+8                                                              
         OI    SVCTLBYT,SNVHDCLQ                                                
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VALOPT05                                                         
         MVC   BYTE,SVCTRLB                                                     
         NI    BYTE,SNVHDCLQ                                                    
         OC    SVCTLBYT,BYTE                                                    
*                                                                               
VALOPT05 DS    0H                                                               
         CLI   DISOPTNH+5,0                                                     
         BE    VLOPTQX                                                          
*                                                                               
         LA    R2,DISOPTNH                                                      
         XCEFL BLOCK,480           CLEAR THE BLOCK                              
         GOTO1 SCANNER,DMCB,(R2),(X'88',BLOCK)                                  
         CLI   DMCB+4,0                                                         
         BE    VLOPTNQX                                                         
*                                                                               
         L     R1,ATIOB            SET CURSOR ON ERROR                          
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         LA    R3,BLOCK                                                         
VLOPT10  CLI   0(R3),0                                                          
         BE    VLOPTQX                                                          
*                                                                               
         L     RE,=A(OPTNTABL)                                                  
         A     RE,RELO                                                          
*                                                                               
VLOPT20  CLI   0(RE),0                                                          
         BNE   VLOPT40                                                          
*                                                                               
         CLI   PFKEY,11                                                         
         BE    VLOPT70                                                          
*                                                                               
* NON-TABLE, NON-I2 OPTIONS HERE                                                
*                                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'PAY'                                                 
         BNE   VLOPT20A                                                         
         MVI   SVPAYOPT,C'P'                                                    
         B     VLOPT21                                                          
*                                                                               
VLOPT20A DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'UNPAY'                                               
         BNE   VLOPT30                                                          
         MVI   SVPAYOPT,C'U'                                                    
*                                                                               
* THE PAY/UNPAY OPTION IS DDS ONLY                                              
*                                                                               
VLOPT21  DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VLOPT500            YES - CHECK NEXT OPTION                      
*                                                                               
VLOPT30  L     R1,ATIOB            1ST HALF BAD                                 
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)                                                   
         B     VLOPTNQX                                                         
*                                                                               
VLOPT40  ZIC   R1,0(R3)            MAKE SURE IT IS A VALID OPTION               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),1(RE)                                                   
         BE    VLOPT50                                                          
         LA    RE,L'OPTNTABL(RE)                                                
         B     VLOPT20                                                          
*                                                                               
VLOPT50  CLI   1(R3),0             SHOULDN'T HAVE A 2ND HALF                    
         BE    VLOPT60                                                          
         L     R1,ATIOB            2ND HALF BAD                                 
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,8(R3)                                                   
         B     VLOPTNQX                                                         
*                                                                               
VLOPT60  DS    0H                                                               
         CLI   0(RE),SNVHDNTQ                                                   
         BNE   *+12                                                             
         TM    MISCFLG1,MF1SDSTA   DIGITAL STATION?                             
         BO    VLOPTNQX                                                         
*                                                                               
         OC    SVCTLBYT,0(RE)                                                   
*                                                                               
         LA    R3,32(R3)           CHECK NEXT OPTION                            
         B     VLOPT10                                                          
*                                                                               
VLOPT70  DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'BOOK'                                                
         BNE   VLOPT80                                                          
*                                                                               
         CLI   NETPAKSW,C'Y'       NET MEDIA?                                   
         BE    VLOPTNQX                                                         
*                                                                               
         BRAS  RE,VALBOOK                                                       
         BNE   VLOPTNQX                                                         
         B     VLOPT500                                                         
*                                                                               
VLOPT80  DS    0H                                                               
* INT/INTONLY OPTIONS DISABLED FOR NOW 11/04/2007                               
         B     VLOPT30                                                          
*                                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'INT'                                                 
         BNE   VLOPT90                                                          
         MVI   INTOPT,C'I'                                                      
         B     VLOPT500                                                         
*                                                                               
VLOPT90  DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'INTONLY'                                             
         BNE   VLOPT30                                                          
         MVI   INTOPT,C'O'                                                      
         B     VLOPT500                                                         
*                                                                               
VLOPT500 DS    0H                                                               
         LA    R3,32(R3)           CHECK NEXT OPTION                            
         B     VLOPT10                                                          
*                                                                               
VLOPTQX  L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         J     YES                                                              
*                                                                               
VLOPTNQX J     NO                                                               
         DROP  R1                                                               
*                                                                               
*                                                                               
GENI2RQ  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,=C'SI2R'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'S'                               
         GOTO1 GETPRFIL,DMCB,(R2),PROFI2R                                       
         LA    R2,=C'SI2N'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'S'                               
         GOTO1 (RF),(R1),(R2),PROFI2N                                           
         LA    R2,=C'SI2X'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'S'                               
         GOTO1 (RF),(R1),(R2),PROFI2X                                           
*                                                                               
         MVI   UPDFLAG,C'Y'                                                     
         CLI   PI2POST,C'Y'                                                     
         BE    *+8                                                              
         MVI   UPDFLAG,C'N'                                                     
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
         MVC   SAVEPID,SPACES                                                   
         L     R1,ASECBLK                                                       
         LA    R1,(SECPID-SECD)(R1)                                             
*                                                                               
         CLC   0(L'SECPID,R1),SPACES                                            
         BNH   *+20                                                             
         CLC   =C'DDS',0(R1)                                                    
         BE    *+10                                                             
         MVC   SAVEPID,0(R1)                                                    
*                                                                               
         MVC   SPOOKDID,=C'SI2'                                                 
         CLC   SAVEPID,SPACES                                                   
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
         BH    GENI2005                                                         
*                                                                               
         TM    INTFLAG,IFINTQ      ANY INTEGRATION CHARGES?                     
         BZ    GENI2005            NO - DON'T BOTHER WITH INT OPTIONS           
         MVI   RCRDINT,C'I'        INDICATE SOME INT CHARGES                    
         TM    INTFLAG,IFCOSTQ     ANY NONZERO COST SPOTS?                      
         BO    GENI2005            NO                                           
         MVI   RCRDINT,C'O'        INDICATE INT-ONLY INVOICE                    
*                                                                               
GENI2005 DS    0H                                                               
         MVC   RCRDAGY,AGENCY                                                   
         MVC   RCRDMED,QMED                                                     
         MVC   RCRDCLT,QCLT                                                     
         OC    RCRDCLT,SPACES                                                   
*                                                                               
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
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         L     RF,MINELEM                                                       
         CLI   0(RF),SNVIDELQ                                                   
         BNE   GENI2NDX                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   SNVHDLEN,SNVHDLN3   HEADER WITH ALPHA PRODUCTS?                  
         BL    GENI2010            NO - PROCESS 1-BYTE BINARY PRODS             
*                                                                               
         CLC   SNVHDAP1,SPACES     PRODUCT IN THE HEADER?                       
         BNH   GENI2015            NO - USE POL                                 
*                                                                               
         MVC   RCRDPRD,SNVHDAP1                                                 
         MVC   RCRDPRD2,SNVHDAP2                                                
         OC    RCRDPRD2,SPACES                                                  
         B     GENI2020                                                         
*                                                                               
GENI2010 DS    0H                  BINARY PRODUCTS HERE                         
         CLI   SNVHDPRD,X'00'      PRODUCT IN THE HEADER?                       
         BE    GENI2015            NO - USE POL                                 
*                                                                               
         LA    R1,SNVHDPRD                                                      
         BRAS  RE,GETAPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCRDPRD,0(R1)                                                    
*                                                                               
         CLI   SNVHDPR2,X'00'      2ND PRODUCT PRESENT?                         
         BE    GENI2020            NO - PROCEED TO ESTIMATE                     
*                                                                               
         LA    R1,SNVHDPR2                                                      
         BRAS  RE,GETAPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCRDPRD2,0(R1)                                                   
         B     GENI2020            NO - PROCEED TO ESTIMATE                     
*                                                                               
GENI2015 DS    0H                                                               
         MVC   RCRDPRD,=C'ALL'                                                  
         CLI   PI2RPOL,C'A'                                                     
         BE    *+10                                                             
         MVC   RCRDPRD,=C'POL'                                                  
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
         CLI   SNVHDEST,0                                                       
         BE    GENI2100                                                         
         EDIT  (B1,SNVHDEST),(3,RCRDEST),FILL=0                                 
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
         MVC   WORK(6),RCRDSDAT  USE MOS                                        
         CLI   WORK+4,C' '            IF FULL DATE GIVEN                        
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
         CLC   SAVEPID,SPACES                                                   
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
         MVC   L.LOCKKEY,SPACES                                                 
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
         CLC   L.LKBAEST,SPACES    TEST ALL ESTIMATES CHECKED                   
         BE    TSTBALK8                                                         
         MVC   L.LKBAEST,SPACES                                                 
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
         MVC   WMGRSTA,SNVIMSTA                                                 
         MVC   WMGRMED,SNVIMMED                                                 
         MVC   WMGRBND,SNVIMBND                                                 
         MVC   WMGRVER,SNVIMVER                                                 
         MVC   WMGRMOS,SNVIMMOS                                                 
*                                                                               
* LINE BREAK                                                                    
         MVC   WMGRSEP,=C'<DDSSEPERATOR>'                                       
*                                                                               
* ACTION                                                                        
         MVI   WMGRACT,C'D'        DELETE                                       
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
* STATION (ORIGINAL)                                                            
WRT10    DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SNVKSTA                                                 
         GOTO1 MSUNPK,DMCB,DUB,WORK,WORK+4                                      
         MVC   WMGRNSTA,WORK+4                                                  
*                                                                               
* BAND                                                                          
         MVC   WMGRNBND(1),WORK+8                                               
*                                                                               
* MOS                                                                           
*        MVC   HALF,SNVKMOS                                                     
*        XC    HALF,=X'FFFF'                                                    
*        GOTO1 DATCON,DMCB,(2,HALF),(9,WMGRMOS)                                 
*                                                                               
* INVOICE NUMBER                                                                
         MVC   WMGRINV,SNVKINV                                                  
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
********************************************************************            
* GETS ALPHA PRODUCT CODE FROM SVCLIST                                          
* R1 EXPECTED TO ADDRESS 1-BYTE BINARY PRODUCT CODE                             
* ON EXIT R1 ADDRESSES 3-CHAR ALPHA PRODUCT CODE + EQ EXIT CONDITION            
* OR UNEQUAL EXIT CONDITION IF NO PRODUCT FOUND                                 
********************************************************************            
GETAPRD  DS    0H                                                               
         LA    RF,SVCLIST                                                       
         LA    R0,0(L'SVCLIST,RF)                                               
*                                                                               
GETAP10  CLI   0(RF),C' '                                                       
         JL    GETAPNQX                                                         
         CLC   0(1,R1),3(RF)                                                    
         JE    GETAPQX                                                          
         LA    RF,4(RF)                                                         
         CR    RF,R0                                                            
         JL    GETAP10                                                          
*                                                                               
GETAPNQX LTR   RC,RC                                                            
         BR    RE                                                               
*                                                                               
GETAPQX  LR    R1,RF                                                            
         CR    RC,RC                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
VALBOOK  NTR1  BASE=*,LABEL=*                                                   
         MVC   BOOKOVR,SPACES                                                   
*                                                                               
         CLC   =C'NO',22(R3)                                                    
         BNE   VALB10                                                           
         CLI   1(R3),2                                                          
         BNE   VALB10                                                           
         MVI   BOOKOVR,X'FF'                                                    
         B     VALBQX                                                           
*                                                                               
VALB10   DS    0H                                                               
         CLC   =C'ACT',22(R3)                                                   
         BNE   VALB20                                                           
         CLI   1(R3),3                                                          
         BNE   VALB20                                                           
         MVC   BOOKOVR(3),=C'ACT'                                               
         B     VALBQX                                                           
*                                                                               
VALB20   DS    0H                                                               
         GOTO1 DATVAL,DMCB,(2,22(R3)),WORK                                      
         CLC   =C'000000',WORK                                                  
         BE    VALBNQX                                                          
         MVI   WORK,C'0'                                                        
         MVC   BOOKOVR,WORK                                                     
         MVC   BOOKOVR+4(2),=C'  '                                              
         CLC   DMCB+3(1),1(R3)     ONLY DATE GIVEN?                             
         BE    VALB30              YES - NO HUT                                 
*                                                                               
         LA    RF,22(R3)                                                        
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
CHKINT   NTR1  BASE=*,LABEL=*                                                   
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
BLDIHBLK NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         USING SVIHFLDS,R2                                                      
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
*                                                                               
         CLI   ACTNUM,ACTREST                                                   
         BNE   *+8                                                              
         MVI   MINDELSW,C'Y'       PROCESS DELETED RECORD SET                   
*                                                                               
         BRAS  RE,MINIOHI                                                       
         BNE   BLDIHNQX                                                         
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         XC    SVIHFLDS(SVIHFLQ),SVIHFLDS                                       
*                                                                               
         MVC   SVIHDIDT,SNVHDIDT                                                
         MVC   SVIHDDDT,SNVHDDDT                                                
         MVC   SVIHDCON,SNVHDCON                                                
         MVC   SVIHDTCS,SNVHDTCS                                                
         MVC   SVIHDTSP,SNVHDTSP                                                
         MVC   SVIHDPRD,SNVHDPRD                                                
         MVC   SVIHDAP1,SNVHDAP1                                                
         MVC   SVIHDPR2,SNVHDPR2                                                
         MVC   SVIHDAP2,SNVHDAP2                                                
         MVC   SVIHDDEM,SNVHDDEM                                                
         MVC   SVIHDEST,SNVHDEST                                                
         MVC   SVIHDREP,SNVHDREP                                                
         MVC   SVIHDPKG,SNVHDPKG                                                
         MVC   SVIHDCTL,SNVHDCTL                                                
*                                                                               
BLDIHQX  J     YES                                                              
BLDIHNQX J     NO                                                               
         LTORG                                                                  
         DROP  R6,R2                                                            
*                                                                               
*                                                                               
*                                                                               
* CHECK IF WE HAVE A DIGITAL STATION                                            
*                                                                               
CKDIGITL NTR1  BASE=*,LABEL=*                                                   
         CLI   QSTA+4,C'S'         -SM IS DIGITAL                               
         BE    CKDIG10                                                          
         CLI   QSTA+4,C'D'         -DV IS DIGITAL                               
         BE    CKDIG10                                                          
         CLI   QMED,C'R'           FOR MEDIA R                                  
         JNE   YES                                                              
         CLI   QSTA+4,C'C'         -CM IS DIGITAL                               
         JNE   YES                                                              
*                                                                               
CKDIG10  DS    0H                                                               
         OI    MISCFLG1,MF1SDSTA                                                
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* R6 EXPECTED TO ADDRESS INVOICE HEADER ELEMENT                                 
READPAK  NTR1  BASE=*,LABEL=*                                                   
         USING SNVHDELD,R6                                                      
*                                                                               
         XC    PKGRECD,PKGRECD                                                  
*                                                                               
         LA    R3,PKGRECD                                                       
         USING NPRECD,R3                                                        
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,QSTA                                                      
         MVC   NPKEST,SNVHDEST                                                  
         MVC   NPKPACK,SNVHDPKG                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR',PKGRECD,AIO3                  
         L     R3,AIO3                                                          
         CLC   PKGRECD,0(R3)                                                    
         BNE   READPAKX                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFIL',21(R3),AIO3,WORK              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R6,SVGETEL                                                       
         MVC   SVGETEL+4(2),DATADISP                                            
         MVC   SVGETEL+6(1),ELCODE                                              
*                                                                               
         L     R6,AIO3                                                          
         MVC   DATADISP,=X'001B'                                                
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   READP20                                                          
*                                                                               
         USING NPAKEL,R6                                                        
         MVC   DISPKGN,NPAKNAME                                                 
*                                                                               
READP20  DS    0H                                                               
         L     R6,SVGETEL                                                       
         MVC   DATADISP,SVGETEL+4                                               
         MVC   ELCODE,SVGETEL+6                                                 
*                                                                               
READPAKX J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* INCLUDES AND DSECTS BELOW                                                     
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
*                                                                               
       ++INCLUDE SPSNVWORKD        SYSTEM AREAS                                 
         EJECT                                                                  
       ++INCLUDE SPSNVRCRD         REQUEST CARD DSECT                           
         EJECT                                                                  
       ++INCLUDE SPGENAGY          INVOICE RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENCLT          INVOICE RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENSNV          INVOICE RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENMSR          MATCHING STATUS RECORD DSECT                 
         EJECT                                                                  
       ++INCLUDE SPSNVFFD          BASE SCREEN FOR SYSTEM                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSNVFDD          OUR MAINTENANCE SCREEN                       
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
* FAGETTXTD                                                                     
* SPGETBUBLD                                                                    
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
       ++INCLUDE FAGETTXTD                                                      
         DSECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPWMGRD                                                        
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE DDREQHDR                                                       
*                                                                               
         PRINT ON                                                               
*                                                                               
* MY STORAGE AREA                                                               
*                                                                               
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
AFLDQTBL DS    A                                                                
VLOCKET  DS    A                   A(LOCKET)                                    
*                                                                               
SVDOFFST DS    XL1                 SAVED OFFSET IF REC IS SELECTED              
*                                                                               
MISCFLG1 DS    X                                                                
MF1KYCHG EQU   X'80'               KEY FIELD CHANGED                            
MF1GTDTL EQU   X'40'               THIS INVOICE HAS DETAILS                     
MF1RPCHG EQU   X'20'               REPORT CHANGE POSSIBLE                       
MF1RESPN EQU   X'10'               RESPONSE SWITCH                              
MF1CKBIT EQU   X'08'               USED FOR TESTING IN DELETING DETAIL          
MF1SDSTA EQU   X'04'               -S OR -D STATION                             
*                                                                               
INTOPT   DS    C                                                                
INTFLAG  DS    X                                                                
IFINTQ   EQU   X'01'               AT LEAST 1 INTEGRATION SPOT PRESENT          
IFCOSTQ  EQU   X'02'               AT LEAST 1 SPOT WITH NONZERO COST            
*                                                                               
SVAREA   DS    0C                                                               
SVPRDCOD DS    XL1                 SAVED PRODUCT CODE                           
SVTAXAMT DS    XL4                 SAVED PRODUCT CODE                           
SVQPRDCD DS    CL3                       EBCDIC PRODUCT CODE                    
SVPR2COD DS    XL1                       PIGGY PRODUCT CODE                     
SVQPR2CD DS    CL3                       EBCDIC PIGGYBACK PRODUCT CODE          
SVESTNUM DS    XL1                       ESTIMATE                               
SVCONNUM DS    CL12                      CONTRACT NUMBER                        
SVTOTCOS DS    PL8                       TOTAL COST                             
SVTOTSPT DS    XL2                       TOTAL NUMBER OF SPOTS                  
SVINVDAT DS    XL2                       INVOICE DATE                           
SVDUEDAT DS    XL2                       DUE DATE                               
SVCTLBYT DS    XL1                       CONTROL BYTE                           
SVFLDLST DS    XL(MAXFLDEQ)              FIELD NUMBER EQUATE LIST               
SVFLDCNT DS    XL1                       FIELD COUNT                            
SVREP    DS    CL3                       REP                                    
SVPKG    DS    XL3                       PACKAGE                                
SVDEMO   DS    XL3                       DEMO CATEGORY                          
SVPAYOPT DS    C                         PAY OPTION                             
SVAREAX  DS    0C                                                               
*                                                                               
LSTQMED  DS    CL1                 IF WE CAME FROM THE LIST THEN THESE          
LSTQCLT  DS    CL3                    FIELDS SHOULD BE FILLED IN                
LSTQSTA  DS    CL6                                                              
LSTQPER  DS    CL6                                                              
LSTQINV  DS    CL10                                                             
*                                                                               
SVTPRD   DS    XL1                 SAVED REQUEST PRODUCT CODE                   
SVTPRDN  DS    CL3                               PRODUCT NAME                   
SVTPRD2  DS    XL1                               PIGGY PRODUCT CODE             
SVTPRD2N DS    CL3                               PB PRODUCT NAME                
SVTEST   DS    XL1                               ESTIMATE                       
SVTESTN  DS    CL3                               ESTIAMTE CODE (EBCDIC)         
SVTIDNUM DS    XL1                               FILM ID NUMBER                 
*                                                                               
INTESTSD DS    CL6                 INTERSECTED ESTIMATE START DATE              
INTESTED DS    CL6                                      END DATE                
*                                                                               
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                 BROADCAST COMPRESSED START DATE              
BRDCEDAT DS    XL2                 BROADCAST COMPRESSED END DATE                
*                                                                               
PERDSYMD DS    CL6                 PERIOD'S START YYMMDD                        
PERDEYMD DS    CL6                          END   YYMMDD                        
*                                                                               
TEMPDATE DS    CL6                 TEMPORARY DATE FIELD                         
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE                               
*                                                                               
PCKOF16B DS    PL16                                                             
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
PROF0I2  DS    CL16                I2  PROFILE                                  
PI2POST  EQU   PROF0I2+1           POST AFFIDS TO BUY RECORDS?                  
P0I2BC   EQU   PROF0I2+9           BROADCAST/CALENDAR MONTHS                    
P0I2DTOR EQU   PROF0I2+14          DETAIL ORDER SCHEME                          
*                                                                               
PROFI2Z  DS    CL16                I2z PROFILE                                  
PI2ZSUB  EQU   PROFI2Z+6           RE-READ I2 BY SUBMEDIA                       
*                                                                               
SVMEDIA  DS    C                                                                
SVSTYPE  DS    C                                                                
SVNETINV DS    C                                                                
*                                                                               
PROF0I2P DS    CL16                I2P PROFILE                                  
P0I2PLKE EQU   PROF0I2P+2          NO LOCKED EST ADD/CHA/DEL/MOVE               
*                                                                               
PROF0A0A DS    CL16                A0A PROFILE                                  
P0A0APAY EQU   PROF0A0A+14         NO PAID INVOICE CHA/DEL/MOVE                 
*                                                                               
PROFI2N  DS    CL16                I2N PROFILE                                  
PI2NPOLS EQU   PROFI2N+4           SPOT-PRD=POL UPDATIVE I2S ONLY               
PI2NESTS EQU   PROFI2N+5           SPOT-EST=NO UPDATIVE I2S ONLY                
PI2NPOLN EQU   PROFI2N+6           NET -PRD=POL UPDATIVE I2S ONLY               
PI2NESTN EQU   PROFI2N+7           NET -EST=NO UPDATIVE I2S ONLY                
PI2NAI2B EQU   PROFI2N+8           Auto I2s by buy ID                           
PI2NAI2A EQU   PROFI2N+10          Summary I2's for NINV                        
*                                                                               
PI2XEAN  EQU   PROFI2X             EST=ALL=NO                                   
*                                                                               
UPDFLAG  DS    C                                                                
*                                                                               
RQOPTS   DS    0CL32                                                            
RQSW     DS    C                   REQUEST SWITCH                               
RQPRD    DS    CL6                         PRODUCT                              
RQBOOK   DS    CL6                         BOOK                                 
RQNAME   DS    CL10                        NAME                                 
RQMOS    DS    CL6                         MONTH OF SERVICE                     
RQREP    DS    CL2                         REP                                  
RQBUYOPT DS    CL1                         BUY OPTION                           
*                                                                               
SAVEPID  DS    CL10                                                             
*                                                                               
KEYLINE  DS    CL(L'WORK)          KEYLINE FOR THE DETAIL SCREEN                
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
*                                                                               
CHGFLAG  DS    XL1                 ESTIMATE OR PRODUCT WAS CHANGED              
CHGFLAG2 DS    XL1                 ANY CHANGES MADE                             
XPKEY    DS    XL64                                                             
XSVPKEY  DS    XL64                                                             
ORIGEST  DS    X                                                                
*****                                                                           
ORIGPRD  DS    X                   ORIGINAL PRODUCT CODES AND ALPHA             
ORIGPR2  DS    X                     NOTE: CLEARED WITH A SINGLE XC IN          
ORIGQPRD DS    CL3                        VREC ROUTINE                          
ORIGQPR2 DS    CL3                                                              
*****                                                                           
SVKEY    DS    XL13                                                             
SVXKEY   DS    XL32                                                             
REPRECD  DS    XL15                                                             
PKGRECD  DS    XL20                                                             
REPAGY   DS    CL2                                                              
SVGETEL  DS    F                R6+                                             
         DS    XL3              + DATADISP+ELCODE (2ND PART OF SVGETEL)         
SAVEADDR DS    CL2                                                              
XKEY2    DS    XL64                                                             
XKEY3    DS    XL64                                                             
CANADIEN DS    X                                                                
DISPTAX  DS    X                                                                
PERDATES DS    0CL12               INVOICE PERIOD DATES                         
PERSDATE DS    CL6                     START DATE                               
PEREDATE DS    CL6                     END DATE                                 
*                                                                               
SVLBLK   DS    0H                  ESTIMATE LOCK INFORMATION                    
SVCTRLB  DS    X                                                                
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
SVLBLKLQ EQU   *-SVLBLK                                                         
*                                                                               
SVSECAGY DS    CL2                                                              
SVSECPID DS    CL8                                                              
SVSECID  DS    XL2                                                              
*                                                                               
BOOKOVR  DS    CL5                                                              
*                                                                               
IFLDH    DS    CL8                                                              
IFLD     DS    CL30                                                             
IFLDCNT  DS    CL30                                                             
*                                                                               
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
IMFLAG   DS    C                                                                
INVMOS   DS    XL2                 MOS                                          
*                                                                               
SVIHFLDS DS    0X                                                               
SVIHDIDT DS    XL(L'SNVHDIDT)                                                   
SVIHDDDT DS    XL(L'SNVHDDDT)                                                   
SVIHDCON DS    XL(L'SNVHDCON)                                                   
SVIHDTCS DS    XL(L'SNVHDTCS)                                                   
SVIHDTSP DS    XL(L'SNVHDTSP)                                                   
SVIHDPRD DS    XL(L'SNVHDPRD)                                                   
SVIHDAP1 DS    XL(L'SNVHDAP1)                                                   
SVIHDPR2 DS    XL(L'SNVHDPR2)                                                   
SVIHDAP2 DS    XL(L'SNVHDAP2)                                                   
SVIHDDEM DS    XL(L'SNVHDDEM)                                                   
SVIHDEST DS    XL(L'SNVHDEST)                                                   
SVIHDREP DS    XL(L'SNVHDREP)                                                   
SVIHDPKG DS    XL(L'SNVHDPKG)                                                   
SVIHDCTL DS    XL(L'SNVHDCTL)                                                   
SVIHDETL DS    XL(MAXFLDEQ)        DETAILS(MAX)                                 
SVIHFLQ  EQU   *-SVIHFLDS                                                       
         ORG   SVIHFLDS                                                         
*                                                                               
SVIHORIG DS    CL(SVIHFLQ)                                                      
SVIHCHG  DS    CL(SVIHFLQ)                                                      
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'226SPSNV02   02/18/20'                                      
         END                                                                    
