*          DATA SET REREI03    AT LEVEL 006 AS OF 04/27/07                      
*PHASE T82603A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T82603 - MAINTENANCE OF DETAILS                                       
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T82600), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     REREIFC (T826FC) -- MAINTENANCE                                 
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
T82603   TITLE 'REREI03 - REP INVOICE DETAIL MAINT OVERLAY'                     
T82603   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82603*,R7,RR=R3                                              
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
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         BRAS  RE,SETPFTBL                                                      
         GOTOR MODSCRN                                                          
*                                                                               
         TM    CTLRFLG1,CF1CKOFF   SAVE OFFSET OF SELECTED LINE?                
         BZ    *+14                                                             
         MVC   SVDOFFST,SELOFFST   YES                                          
         NI    CTLRFLG1,X'FF'-CF1CKOFF                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREC                                                             
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
         MVI   CURRSYS,C'R'        ALWAYS START IN REP                          
*                                                                               
         TM    SCRSTAT,RECCHG+ACTCHG     FORCE KEY CHANGE                       
         BZ    *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  LA    R2,DTLSTAH                                                       
         TM    CTLRFLG1,X'10'      LOCAL STATION                                
         BZ    VKSTA02                                                          
         MVC   8(L'DTLSTA,R2),SVSIGNON                                          
         OI    6(R2),X'80'         RESTRANSMIT                                  
         OI    1(R2),X'20'         PROTECTED                                    
         MVI   5(R2),L'DTLSTA      LENGTH                                       
*                                                                               
         LA    R6,DTLSTA                                                        
         AHI   R6,L'DTLSTA-1       FIND THE LAST CHARACTER                      
         CLI   0(R6),X'40'                                                      
         BH    *+8                                                              
         BCT   R6,*-8                                                           
*                                                                               
         CLI   0(R6),C'L'          NO L FOR STATION                             
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
*                                                                               
VKSTA02  CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALISTA                                                          
         BNE   INVLFLD                                                          
         MVC   GOOFKEY,KEY         SAVE KEY TO GOOF GENCON ON VK EXIT           
*                                                                               
         MVC   DTLSTNM,MKTNM       SHOW MARKET NAME                             
         OI    DTLSTNMH+6,X'80'                                                 
*                                                                               
VKSTAX   DS    0H                                                               
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE PERIOD = MONTH                                                   
*****                                                                           
VKPER00  LA    R2,DTLMONH                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 PERVAL,DMCB,(DTLMONH+5,DTLMON),PERVALST                          
         TM    DMCB+4,X'01'                                                     
         BO    INVLFLD                                                          
*                                                                               
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALAED+PVALAEM+PVALAEY  ALL OF END ASSUMED?            
         BNO   INVLFLD                           NO                             
         MVC   BMOS,PVALCSTA                                                    
         MVC   BMOSFF,PVALCSTA                                                  
         XC    BMOSFF,=X'FFFF'                                                  
*                                                                               
         MVC   PVALESTA+4(2),=C'15'                                             
         GOTO1 GETBROAD,DMCB,(1,PVALESTA),BRDDATES,GETDAY,ADDAY                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                DATE SHOULDN'T BE INVALID                    
*                                                                               
         GOTO1 DATCON,DMCB,(X'10',BRDDATES),(2,WORK)                            
         MVC   BRDCSDAT,WORK                                                    
         MVC   BRDCEDAT,WORK+3                                                  
*                                                                               
         OI    DTLMNDTH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(X'12',BRDCSDAT),(8,DTLMNDT)                         
*                                                                               
VKPERX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
         DROP  R3                                                               
*****                                                                           
* VALIDATE THE CONTRACT                                                         
*****                                                                           
VKCON00  LA    R2,DTLCONH                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
*                                                                               
*        QSNVRFLG=Contract(Reppak)/Rep Contract(MO)/Station Ord#                
*                                                                               
         MVI   QSNVRFLG,0          clear flag                                   
         XC    BCONT99,BCONT99                                                  
         NI    CTLRFLG1,X'FF'-CF1MOREP-CF1MOSTA-CF1MOINV                        
*                                                                               
         TM    CTLRFLG1,X'10'      IS THIS A LOCAL REP?                         
         BZ    *+20                NO                                           
         MVC   QSTAORD#,8(R2)                                                   
         OC    QSTAORD#,=10C' '    PADDED WITH SPACE, THIS IS HOW               
         B     VKCONX              IT IS STORED IN THE RECORD                   
*                                                                               
         CLI   DTLTYPE,C'C'        C = CONTRACT NUMBER?                         
         BE    VKCON01             YES                                          
         OC    DTLTYPE,DTLTYPE     ALL # = CONTRACT NUMBER?(DEFAULT)            
         BZ    VKCON01             YES                                          
         CLI   DTLTYPE,C' '        SPACE = CONTRACT NUMBER?(DEFAULT)            
         BE    VKCON01             YES                                          
*                                                                               
         MVC   QORD#,8(R2)                                                      
         OC    QORD#,=10C' '                                                    
*                                                                               
         CLI   DTLTYPE,C'R'                                                     
         BNE   *+12                                                             
         MVI   QSNVRFLG,C'R'       SET FLAG TO MO CONTRACT#                     
         OI    CTLRFLG1,CF1MOINV+CF1MOREP                                       
         CLI   DTLTYPE,C'S'                                                     
         BNE   *+12                                                             
         OI    CTLRFLG1,CF1MOINV+CF1MOSTA                                       
         MVI   QSNVRFLG,C'S'       SET FLAG TO STATION ORD#                     
*                                                                               
         LA    RE,QORD#                                                         
         LA    RF,WORK                                                          
         LA    R1,L'QORD#                                                       
         XC    WORK(L'QORD#),WORK                                               
VKCON00A CLI   0(RE),X'40'                                                      
         BNH   VKCON00B                                                         
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
VKCON00B LA    RE,1(RE)                                                         
         BCT   R1,VKCON00A                                                      
         MVC   QORD#,WORK                                                       
         OC    QORD#,=10C' '                                                    
         B     VKCONX                                                           
*                                                                               
VKCON01  LA    RE,8(R2)            RE-FORMAT CONTRACT NUMBER                    
         LA    RF,WORK+20          CONTRACT# NOW HAS A 'C' AT THE END           
         LA    R1,L'DTLCON         BEFORE READING THE CONTRACT KEY,             
         SR    R0,R0               NEED TO CHOP OFF THE 'C',AND ALSO            
VKCON03  CLI   0(RE),X'40'         CALCULATE THE REAL LENGTH OF THE #           
         BNH   VKCON05                                                          
         CLI   0(RE),C'C'                                                       
         BE    VKCON05                                                          
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
         AHI   R0,1                COUNTER HOW MANY CHARACTERS                  
VKCON05  AHI   RE,1                                                             
         BCT   R1,VKCON03                                                       
*                                                                               
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(5),WORK+20(0)                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK(5)                                                
         SRP   WORK+5(5),1,0                                                    
         MVC   BCONT99,WORK+5                                                   
*                                                                               
         GOTO1 GETCONT             GET CONTRACT REC                             
         BNE   RECNTFND                     NO, RECORD NOT FOUND                
*                                                                               
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   VKCON10                                                          
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    VKCON10             YES                                          
         CLC   QOFF,TWAACCS+2      NO, MUST MATCH RESTRICTED OFFICE             
         BNE   NOACCESS            NOT AUTH'D FOR DATA                          
*                                  SHOW OFFICE FOR THIS CONTRACT                
VKCON10  MVC   DTLOFF,QOFF                                                      
         OI    DTLOFFH+6,X'80'                                                  
*                                                                               
VKCONX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKINV00  LA    R2,DTLINVH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKINV02                                                          
         TM    CTLRFLG1,X'10'      LOCAL STATION?                               
         BO    VKINVX              INVOICE NOT MANDATORY                        
         B     MISSFLD                                                          
*                                                                               
VKINV02  MVC   QINVOICE,8(R2)                                                   
         OC    QINVOICE,=CL10' '                                                
VKINVX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
VKKEY    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING SNVKEY,R3                                                        
*                                                                               
         TM    CTLRFLG1,X'10'      IS THIS A LOCAL REP?                         
         BZ    VKKEY06             NO                                           
*                                                                               
         MVI   SNVLTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVLSUB,SNVLSUBQ    X'B3'                                        
         MVC   SNVLID,TWAORIG                                                   
         MVC   SNVLSTA,QSTA                                                     
         MVC   SNVLMOS,BMOSFF                                                   
         MVC   SNVLSORD,QSTAORD#                                                
         MVC   SNVLPOW,TWAAGY                                                   
         B     VKKEY08                                                          
*                                                                               
VKKEY06  MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,AGENCY                                                    
         MVC   SNVRSTA,QSTA                                                     
         MVC   SNVRMOS,BMOSFF                                                   
         MVC   SNVRCON,BCONT99                                                  
         MVC   SNVRFLG,QSNVRFLG                                                 
         MVC   SNVRINV,QORD#                                                    
         CLI   SNVRFLG,0                                                        
         BNE   *+10                                                             
         MVC   SNVRINV,QINVOICE                                                 
         DROP  R3                                                               
*                                                                               
VKKEY08  CLI   CURRSYS,C'S'                                                     
         BE    VKKEY10                                                          
         GOTO1 SPTSYS                                                           
VKKEY10  GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),INVDIR,KEY,AIO                   
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         LA    R2,DTLCONH          SET CURSOR TO ORDER#/CON# FIELD              
         CLC   SNVRMAST,KEY        YES, SAME RECORD?                            
         BNE   RECNTFND                     NO, RECORD NOT FOUND                
*                                                                               
         MVC   SVMASTKY(L'SNVRMAST),SNVRMAST                                    
*                                                                               
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VKKEY20             THEN NO D/A                                  
         MVC   CONHED2(4),=C'D/A='                                              
         GOTO1 HEXOUT,DMCB,SNVDDA,CONHED2+4,L'SNVDDA                            
         OI    CONHED2H+6,X'80'                                                 
*                                                                               
VKKEY20  CLI   CURRSYS,C'S'                                                     
         BE    VKKEY24                                                          
         GOTO1 SPTSYS                                                           
VKKEY24  GOTO1 INITMNIO                                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    LOOK FOR THE HEADER ELEMENT                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING HEADER ELEMENT          
*                                                                               
         NI    MISCFLG2,X'FF'-MF2MTIME                                          
         TM    SNVHDCTL,SNVHDMTQ   INVOICE USES MILITARY TIME?                  
         BZ    *+8                                                              
         OI    MISCFLG2,MF2MTIME   YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(X'12',SNVHDSDT),(0,WORK)   PERIOD DATES             
         MVC   PERSDATE,WORK                                                    
         MVC   PEREDATE,WORK+L'PERSDATE+1                                       
*                                                                               
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
*                                                                               
         TM    4(R2),X'80'         FIELD WAS CHANGED?                           
         BZ    *+8                                                              
         OI    MISCFLG1,MF1KYCHG   YES                                          
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
         BE    VKFLTINV            YES, INVALID FILTER                          
*                                                                               
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
VKFDTINV L     R1,ATIOB            SET CURSOR IN CASE OF ERROR                  
         USING TIOBD,R1                                                         
         L     RE,PSNCOMP                                                       
         LA    RF,DTLOPTN                                                       
         SR    RE,RF                                                            
         STC   RE,TIOBCURI                                                      
         DROP  R1                                                               
         B     INVDTFLT                                                         
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
         BL    VKFDTINV            FILTER DATE SHOULD BE IN PERIOD              
         CLC   VKFLT20D.PVALESTA,PEREDATE                                       
         BH    VKFDTINV                                                         
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
         BL    VKFDTINV            FILTER DATE SHOULD BE IN PERIOD              
         CLC   VKFLT20D.PVALEEND,PEREDATE                                       
         BH    VKFDTINV                                                         
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
         OC    FLTRFILM,=20C' '                                                 
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
         OC    FLTRFLM2,=20C' '                                                 
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
         OC    FLTRPROD,=20C' '                                                 
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
         BNE   VKFLT37                                                          
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
*&&DO                                                                           
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
         OC    FLTRNTWK,=20C' '                                                 
         OI    FLTRFLG2,FF2DNTWK   SET FILTER FLAG BIT                          
         B     VKFLTNXT            CHECK NEXT FILTER OPTION                     
*&&                                                                             
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
VKFLT40  B     VKFLTINV            DOESN'T MATCH A VALID FILTER                 
*                                                                               
VKFLTNXT LA    R3,PSNL(R3)         CHECK NEXT SCANNER BLOCK                     
         B     VKFLT10                                                          
*                                                                               
VKFLT100 L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC    DON'T SET CURSOR ANYMORE              
         DROP  R1                                                               
*                                                                               
VKFLTX   DS    0H                                                               
         DROP  R3                                                               
*****                                                                           
* DONE VALIDATING ALL THE KEY FIELDS                                            
*****                                                                           
VKX      MVC   KEY,GOOFKEY         STATION REC TO MAKE GENCON HAPPY             
         CLI   CURRSYS,C'R'                                                     
         BE    VKXX                                                             
         GOTO1 REPSYS                                                           
VKXX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VREC     DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1RDFLM   HAVEN'T READ FILMS INTO BUFFER         
         NI    MISCFLG2,X'FF'-MF2CHNGD   NOTHING HAPPENED TO DETAILS            
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    VR00                                                             
         GOTO1 =A(SETSCRN),DMCB,(RC),RR=RELO                                    
*                                                                               
VR00     TM    MISCFLG1,MF1RDFLM   READ FILMS INTO BUFFER YET?                  
         BNZ   *+8                                                              
         BAS   RE,READFLMS         YES                                          
*                                                                               
         B     DREC                                                             
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
         ICM   R1,15,SNVIDCST                                                   
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
DR00X    DS    0H                                                               
*                                                                               
DR10     LH    R2,DFIRSTLN         CLEAR THE SCREEN                             
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
DR12     IC    RE,0(R1)                                                         
         TM    1(R1),X'20'         PROTECTED FIELD?                             
         BO    DR16                YES, DON'T TOUCH THEM                        
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
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    DR83                YES, CAN'T DISPLAY HERE                      
*                                                                               
         GOTO1 GTROUTIN,DMCB,(C'D',(R2))   GET THE DISPLAY ROUTINE              
         L     RF,DMCB                                                          
         A     RF,RELO                                                          
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
         OI    DTLSNUMH+6,X'80'                                                 
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
DRX      CLI   CURRSYS,C'R'        SWITCH BACK TO REP ON EXIT                   
         BE    DRXX                                                             
         GOTO1 REPSYS                                                           
DRXX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* NOW REPORT DONE HERE                                                          
***********************************************************************         
PREC     DS    0H                                                               
         TM    WHEN,X'40'         NOW REPORT?                                   
         BNO   PRXIT              NO                                            
*                                                                               
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,HSPEC                                                         
         TM    CTLRFLG1,X'10'      LOCAL STATION SIGN ON?                       
         BZ    *+8                                                              
         LA    R1,HSPECSTA         USE A DIFFERENT HEADING                      
         ST    R1,SPECS                                                         
*                                                                               
         XC    MINEKEY,MINEKEY     CALCULATE THE TOTAL OF THE DETAILS           
         MVI   MINEKEY,SNVIDELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    PR00                                                             
         CLI   MINERR,MINEEOF                                                   
         BE    PRX                                                              
         DC    H'0'                                                             
*                                                                               
PR00     L     R6,MINELEM          REPORT ALL THE DETAILS                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ    STILL A DETAIL REC?                          
         BNE   PRX                 NO...DONE REPORTING                          
*                                                                               
         BAS   RE,CHKFLTRS         GOT FILTER TOTALS?                           
         BNE   PRNEXT              NOT FOR THIS DETAIL                          
* DATE                                                                          
         ZIC   R3,SNVIDDAY                                                      
         GOTO1 ADDAY,DMCB,PERSDATE,WORK,(R3)                                    
         GOTO1 DATCON,DMCB,(0,WORK),(8,PDATE)                                   
* TIME                                                                          
         SR    R0,R0                                                            
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
         MVC   PTIME,WORK          COPY THE TIME                                
* LENGTH                                                                        
         EDIT  (B1,SNVIDSLN),(3,PLEN)                                           
         TM    FLTRFLG1,FF1DSLEN   CHECK IF FILTER                              
         BZ    PRCOST                                                           
         CLC   FLTRSLEN,SNVIDSLN                                                
         BNE   PRNEXT                                                           
* COST                                                                          
PRCOST   LA    R3,PCOST                                                         
*                                                                               
         TM    SNVIDCT2,SNVIDBON   BONUS ?                                      
         BZ    *+12                NO                                           
         MVI   0(R3),C'#'                                                       
         B     PRSPOOL                                                          
*                                                                               
         TM    SNVIDCTL,SNVIDICQ   IGNORE COST?                                 
         BZ    PR05                NO                                           
         MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
PR05     TM    SNVIDCT2,SNVIDCTQ   CONTRACT TRADE ITEM?                         
         BZ    PR10                NO                                           
         MVI   0(R3),C'T'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
PR10     ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         EDIT  (R1),(12,(R3)),2,FLOAT=-                                         
         TM    FLTRFLG1,FF1DCOST   CHECK IF FILTER                              
         BZ    PRSPOOL                                                          
         ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   SEE IF NEGATIVE AMOUNT                       
         BZ    *+6                                                              
         LNR   R1,R1               NEGATE                                       
         CLM   R1,15,FLTRCOST      MATCH?                                       
         BNE   PRNEXT              NO...IGNORE THIS WHOLE DETAIL LINE           
*                                                                               
PRSPOOL  GOTO1 SPOOL,DMCB,(R8)      PRINT A DETAILS REPORT LINE                 
*                                                                               
PRNEXT   BAS   RE,MINIOSEQ                                                      
         BE    PR00                                                             
         DROP  R6                                                               
*                                                                               
PRX      MVC   P(14),=C'HDR Total/Spts'                                         
         MVC   P+19(20),DTLHNUM                                                 
         GOTO1 SPOOL,DMCB,(R8)      HEADER TOTALS / SPOTS                       
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)      CLOSE REPORT                                
PRXIT    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        HEADHOOK AND HEADSPECS                                                 
*----------------------------------------------------------------------         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H3+14(L'DTLSTA),DTLSTA                                           
         MVC   H3+41(L'DTLMON),DTLMON                                           
         MVC   H4+14(L'DTLCON),DTLCON                                           
         MVC   H4+41(L'DTLINV),DTLINV                                           
         XIT1                                                                   
*                                                                               
HSPEC    DS    0H                                                               
         SSPEC H1,2,C'DETAIL LIST'                                              
         SSPEC H1,48,REQUESTOR                                                  
         SSPEC H2,2,RUN                                                         
         SSPEC H2,48,REPORT                                                     
         SSPEC H3,2,C'STATION  -'                                               
         SSPEC H3,30,C'MONTH   -'                                               
         SSPEC H4,2,C'CONTRACT -'                                               
         SSPEC H4,30,C'INVOICE -'                                               
         SSPEC H6,3,C'DATE'                                                     
         SSPEC H6,10,C'TIME'                                                    
         SSPEC H6,16,C'LENGTH'                                                  
         SSPEC H6,27,C'COST'                                                    
         SSPEC H7,3,C'----'                                                     
         SSPEC H7,10,C'----'                                                    
         SSPEC H7,16,C'------'                                                  
         SSPEC H7,27,C'----'                                                    
         DC    H'0'                                                             
*                                                                               
HSPECSTA DS    0H                                                               
         SSPEC H1,2,C'DETAIL LIST'                                              
         SSPEC H1,48,REQUESTOR                                                  
         SSPEC H2,2,RUN                                                         
         SSPEC H2,48,REPORT                                                     
         SSPEC H3,2,C'STATION  -'                                               
         SSPEC H3,30,C'MONTH   -'                                               
         SSPEC H4,2,C'STA-ORD# -'                                               
         SSPEC H4,30,C'INVOICE -'                                               
         SSPEC H6,3,C'DATE'                                                     
         SSPEC H6,10,C'TIME'                                                    
         SSPEC H6,16,C'LENGTH'                                                  
         SSPEC H6,27,C'COST'                                                    
         SSPEC H7,3,C'----'                                                     
         SSPEC H7,10,C'----'                                                    
         SSPEC H7,16,C'------'                                                  
         SSPEC H7,27,C'----'                                                    
         DC    H'0'                                                             
*                                                                               
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
         EJECT                                                                  
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
         XC    FLMTAB,FLMTAB                                                    
*                                                                               
         LA    R3,BUFFER           R3 = A(FIRST ENTRY IN TABLE)                 
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
         OC    BUFFCMCD,=12C' '                                                 
         LA    R3,BUFFNEXT                                                      
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         B     RDFLM10                                                          
*                                                                               
RDFLMX   B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE FILM NAME GIVEN A FILM CODE                          
*                                                                               
* ON ENTRY:    PARAM 1  BYTE 0     INTERNAL FILM CODE                           
*                       BYTES 1-3  A(WHERE FILM NAME IS RETURNED)               
***********************************************************************         
GETFLMNM NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         LA    R3,BUFFER           R3 = A(FIRST ENTRY IN TABLE)                 
         USING BUFFDSCT,R3                                                      
*                                                                               
GTFLM10  CLI   BUFFCODE,0          END OF TABLE ALREADY?                        
         BNE   *+6                                                              
         DC    H'0'                YES, DIE                                     
*                                                                               
         CLC   BUFFCODE,DMCB                                                    
         BE    *+12                                                             
         LA    R3,BUFFNEXT                                                      
         B     GTFLM10                                                          
*                                                                               
         MVC   0(L'SNVCMCD,R2),BUFFCMCD   COPY NAME OVER TO FIELD               
         OC    0(L'SNVCMCD,R2),=12C' '                                          
*                                                                               
         TM    BUFFSTAT,BUFFSINV   INVALID FILM?                                
         BZ    *+10                                                             
         MVC   8(2,R2),=C'*F'      YES                                          
*                                                                               
GTFLMX   B     XIT                                                              
         DROP  R3                                                               
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
INVDTFLT MVC   GERROR,=AL2(DTNOTINV)    DATE NOT IN INVOICE MONTH               
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
NOACCESS MVC   GERROR,=AL2(NOTAUTHD)     NOT AUTH'D FOR DATA                    
         B     ERREXIT                                                          
*                                                                               
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
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* INVOICE DISPLAY                                                               
         DC    AL1(SPF02X-*,02,PFTCPROG,(SPF02X-SPF02)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'DISPLAY '                               
SPF02    DC    AL1(KEYTYTWA,L'DTLSTA-1),AL2(DTLSTA-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DTLMON-1),AL2(DTLMON-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DTLCON-1),AL2(DTLCON-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DTLINV-1),AL2(DTLINV-T826FFD)                     
SPF02X   EQU   *                                                                
* INVOICE LIST                                                                  
         DC    AL1(SPF03X-*,03,PFTCPROG,(SPF03X-SPF03)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'LIST    '                               
SPF03    DC    AL1(KEYTYTWA,L'DTLSTA-1),AL2(DTLSTA-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DTLMON-1),AL2(DTLMON-T826FFD)                     
SPF03X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE REREIFLDS                                                      
***********************************************************************         
* ROUTINE TABLE FOR VALIDATING AND DISPLAYING OF THE DETAIL FIELDS              
*                                                                               
*   XL1        - FIELD EQUATE NUMBER                                            
*   AL4        - VALIDATION ROUTINE FOR THIS FIELD                              
*   AL4        - DISPLAY ROUTINE FOR THIS FIELD                                 
***********************************************************************         
ROUTINES DS    0CL9                                                             
         DC    AL1(FLDNDATE),AL4(0),AL4(DISDATE)                                
         DC    AL1(FLDNTIME),AL4(0),AL4(DISTIME)                                
         DC    AL1(FLDNSLEN),AL4(0),AL4(DISSLEN)                                
         DC    AL1(FLDNFILM),AL4(0),AL4(DISFILM)                                
         DC    AL1(FLDNCOST),AL4(0),AL4(DISCOST)                                
         DC    AL1(FLDNPROD),AL4(0),AL4(DISPROD)                                
         DC    AL1(FLDNESTM),AL4(0),AL4(DISESTM)                                
         DC    AL1(FLDNRCNT),AL4(0),AL4(DISRCNT)                                
         DC    AL1(FLDNINTG),AL4(0),AL4(DISINTG)                                
         DC    AL1(FLDNPROG),AL4(0),AL4(DISPROG)                                
*        DC    AL1(FLDNNTWK),AL4(0),AL4(DISNTWK)                                
         DC    AL1(FLDNPFLM),AL4(0),AL4(DISPFLM)                                
         DC    AL1(FLDNMKGD),AL4(0),AL4(DISMKGD)                                
         DC    AL1(FLDNBILB),AL4(0),AL4(DISBILB)                                
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
*        DC    AL1(1),AL1(FF2DNTWK),AL1(FLDNNTWK)                               
         DC    AL1(1),AL1(FF2BLLBD),AL1(FLDNBILB)                               
         DC    AL1(1),AL1(FF2INTEG),AL1(FLDNINTG)                               
         DC    X'FF'                                                            
         EJECT                                                                  
MODSCRN  TM    CTLRFLG1,X'10'      LOCAL REP SIGN ON?                           
         BZ    MODSX                                                            
*        MVC   LSTHEAD,=C'SEL OFF STA-ORD#   INVOICE#   ADV  AGY'               
*        OI    LSTHEADH+6,X'80'    YES, CHANGE HEADING                          
         MVC   DTLCONA,=C'Sta-Ord#'                                             
         OI    DTLCONAH+6,X'80'                                                 
MODSX    BR    RE                                                               
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF10                                                           
*                                                                               
STPF05   CLI   PFKEY,2                                                          
         BE    STPF10                                                           
         CLI   PFKEY,3                                                          
         BE    STPF10                                                           
         CLI   PFKEY,12                                                         
         BNE   STPF20                                                           
*                                                                               
STPF10   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR                                                
*                                                                               
STPF20   GOTO1 INITIAL,DMCB,(R2)                                                
STPFX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
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
SSCR07   B     SSCR08              DON'T DO NETWORK                             
         TM    BSTA,X'F0'          CABLE STATION?                               
         BNO   SSCR08              NO                                           
         TM    BSTA+2,X'7F'        YES, GOT NETWORK ALREADY?                    
         BNZ   SSCR08 '                 YES                                     
         MVI   0(R3),FLDNNTWK                                                   
         LA    R3,1(R3)                                                         
         ZIC   R1,SVFLDCNT                                                      
         AH    R1,=H'1'                                                         
         STC   R1,SVFLDCNT                                                      
*                                                                               
SSCR08   B     SSCR20                                                           
*                                                                               
SSCR10   XC    SVFLDLST,SVFLDLST                                                
         LA    R1,SNVDTFLD                                                      
         LA    R2,SVFLDLST                                                      
         ZIC   R3,SNVDTLEN         R1 = NUMBER OF FIELD IN LIST                 
         SH    R3,=Y(SNVDTOVQ)                                                  
         LR    RE,R3                                                            
SSCR14   CLI   0(R1),FLDNNTWK                                                   
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     SSCRNXT                                                          
*                                                                               
         MVC   0(1,R2),0(R1)                                                    
         LA    R2,1(R2)                                                         
*                                                                               
SSCRNXT  LA    R1,1(R1)                                                         
         BCT   R3,SSCR14                                                        
         STC   RE,SVFLDCNT                                                      
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
SSCR38   LA    R6,1(R6)            TRY TO PUT NEXT HEADING ON SCREEN            
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
SSCR66   BAS   RE,FLDTOSCR         SEND THE COLUMN HEADING TO SCREEN            
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
         MVC   SSCRELEM.TWAEDTA(19),=C'PF2=Header PF3=List'                     
         MVC   SSCRELEM.TWAEDTA+60(11),=C'           '                          
         CLI   CALLSP,0                                                         
         BE    *+10                                                             
         MVC   SSCRELEM.TWAEDTA+60(11),=C'PF12=Return'                          
*                                                                               
         MVC   PARAS(6*4),DMCB                                                  
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   *+10                                                             
         MVC   SSCRELEM.TWAEDTA+23(7),=7C' '                                    
         MVC   DMCB(6*4),PARAS                                                  
         DROP  R1                                                               
*                                                                               
         MVI   SSCRELEM.TWAELLN,TWAELLNQ+73  SET LENGTH OF THIS ELEMENT         
*                                                                               
         GOTO1 TWABLD,DMCB         BUILD THIS FIELD                             
         CLI   SSCRPARM.TWAPERRS,0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
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
         MVI   FSCRELEM.TWAEATB,0          NO ATTRIBUTES                        
***>>>   MVI   FSCRELEM.TWAEATB,X'20'      PROTECTED                            
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
         TM    FLTRFLG1,FF1DDATE   CHECK IF FILTER                              
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
         BNE   DTIMYES                                                          
         MVC   0(4,R3),WORK+1                                                   
         MVI   4(R3),0                                                          
         IC    R1,7(R2)            4 BYTE TIME                                  
         BCTR  R1,0                   LESS ONE BYTE THAN BEFORE                 
         STC   R1,7(R2)                                                         
*                                                                               
DTIMYES  B     YES                                                              
*                                                                               
DTIMNO   B     YES                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
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
         EDIT  (B1,SNVIDSLN),(3,8(R2)),ALIGN=LEFT                               
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
DISFILM  DS    0H                                                               
         NMOD1 0,**DFLM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   SNVIDCML,0          ANY COMMERCIAL CODE FOR THIS DETAIL?         
         BNE   DFLM10                                                           
         MVC   8(4,R2),=CL4'NONE'                                               
         MVI   8+4(R2),C' '                                                     
         MVC   8+5(3,R2),8+4(R2)                                                
         MVI   5(R2),4                                                          
         B     DFLMFCHK                                                         
*                                                                               
DFLM10   LA    R5,8(R2)                                                         
         MVI   5(R2),L'SNVCMCD                                                  
         TM    SNVIDCTL,SNVIDIFQ   IGNORE FILM?                                 
         BZ    DFLM20              NO                                           
         MVC   8(2,R2),=C'N-'      YES                                          
         MVI   5(R2),L'SNVCMCD+2                                                
         LA    R5,8+2(R2)                                                       
*                                                                               
DFLM20   GOTO1 GETFLMNM,DMCB,(SNVIDCML,(R5))                                    
         OC    0(LENFFILM,R5),=12C' '                                           
         MVC   QFILM,0(R5)      SAVE THE FILM CODE                              
*                                                                               
DFLMFCHK TM    FLTRFLG1,FF1DFILM   CHECK IF FILTER                              
         BZ    DFLMYES                                                          
         CLC   FLTRFILM,8(R2)                                                   
         BNE   DFLMNO                                                           
*                                                                               
DFLMYES  B     YES                                                              
*                                                                               
DFLMNO   B     NO                                                               
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
         EDIT  (R1),(12,(R3)),2,FLOAT=-                                         
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
*                                                                               
         B     DPRDYES                                                          
*                                                                               
*&&DO                                                                           
*                                                                               
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
*                                                                               
         GOTO1 FINDQPRD,DMCB,(SNVIDPRD,0)                                       
         BE    *+14                                                             
         MVC   8(3,R2),=C'???'                                                  
         B     *+10                                                             
         MVC   8(L'QPRD,R2),DMCB+1                                              
         MVC   QPRD,8(R2)                                                       
*                                                                               
         CLI   SNVIDPR2,0          ANY PIGGYBACK?                               
         BE    DPRDFCHK            NO, DONE DISPLAYING PRODUCT                  
*                                                                               
         MVI   8+L'QPRD(R2),C'-'                                                
         GOTO1 FINDQPRD,DMCB,(SNVIDPR2,0)                                       
         BE    *+14                                                             
         MVC   8+L'QPRD+1(3,R2),=C'???'                                         
         B     *+10                                                             
         MVC   8+L'QPRD+1(L'QPRD,R2),DMCB+1                                     
         MVC   QPRD2,8+L'QPRD+1(R2)                                             
*                                                                               
DPRDFCHK TM    FLTRFLG1,FF1DPROD   CHECK IF FILTER                              
         BZ    DPRDYES                                                          
         CLC   FLTRPROD,QPRD                                                    
         BNE   DPRDNO                                                           
*&&                                                                             
*                                                                               
DPRDYES  B     YES                                                              
*                                                                               
DPRDNO   B     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
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
* THIS ROUTINE DISPLAYS THE PIGGYBACK FILM CODE OF THE DETAIL ELEMENT           
* ON THE FIELD                                                                  
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(FIELD HEADER)                              
*              PARAM 3             A(DETAIL ELEMENT)                            
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = IF NO FILTER OR FILTER MATCHES          
*                                  NE = IF FILTER DOESN'T MATCH                 
***********************************************************************         
DISPFLM  DS    0H                                                               
         NMOD1 0,**DPFL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING SNVIDELD,R6                                                      
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   CURRBPR2,0          IF NO PIGGYBACK, THEN NO FILM FOR IT         
         BNE   *+12                                                             
         CLI   SNVIDPR2,0                                                       
         BE    DPFLMFCK                                                         
         CLI   SNVIDCM2,0                                                       
         BE    DPFLMFCK                                                         
*                                                                               
         MVI   5(R2),L'SNVCMCD                                                  
         GOTO1 GETFLMNM,DMCB,(SNVIDCM2,8(R2))                                   
         OC    8(LENFFILM,R2),=12C' '                                           
*                                                                               
DPFLMFCK TM    FLTRFLG1,FF1DFLM2   CHECK IF FILTER                              
         BZ    DPFLMYES                                                         
         CLC   FLTRFLM2,8(R2)                                                   
         BNE   DPFLMNO                                                          
*                                                                               
DPFLMYES B     YES                                                              
*                                                                               
DPFLMNO  B     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
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
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*        LITERALS                                                               
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
       ++INCLUDE REREIWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENSNVN         (INVOICE DSECT)                              
         EJECT                                                                  
       ++INCLUDE REREIFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REREIFCD           (OUR MAINT SCREEN)                          
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
*----------------------------------------------------------------               
* PRINT LINE DSECT                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PDATE    DS    CL8                                                              
         DS    CL1                                                              
PTIME    DS    CL5                                                              
         DS    CL3                                                              
PLEN     DS    CL3                                                              
         DS    CL2                                                              
PCOST    DS    CL12                                                             
*----------------------------------------------------------------               
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
PROFI2X  DS    CL16                I2X PROFILE                                  
PI2XERQ  EQU   PROFI2X+14          ESTIMATE REQUIRED                            
*                                                                               
PROFI2Y  DS    CL16                I2Y PROFILE                                  
PI2YDUP  EQU   PROFI2Y+3           ALLOW DUPLICATE INVOICE ITEMS                
PI2YCKEE EQU   PROFI2Y+15          CHECK DETAIL DATE WITH EST END DATE          
*                                                                               
PROFI2R  DS    CL16                I2R PROFILE                                  
PI2RAUTO EQU   PROFI2R+0           AUTO U2 FROM $INV                            
PI2RPOL  EQU   PROFI2R+2           POL/ALL IF PRD NOT STATED                    
PI2RHUT  EQU   PROFI2R+3           HUT ADJUSTMENT FOR AUTO BOOK LOOKUP          
*                                                                               
PROF0TI  DS    XL16                TRAFFIC PROFILE                              
P0TIFCA  EQU   PROF0TI+0           FILM CODES ACCEPTED                          
P0TIPBS  EQU   PROF0TI+14          PIGGYBACK SEPARATOR                          
P0TIAIF  EQU   PROF0TI+15          ACCEPT INVALID FILMS                         
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
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                 BROADCAST COMPRESSED START DATE              
BRDCEDAT DS    XL2                 BROADCAST COMPRESSED END DATE                
*                                                                               
GOOFKEY  DS    CL50                                                             
*                                                                               
BFILM    DS    X                   BINARY INTERNAL FILM                         
BFILMSQ  DS    XL2                 TRAFFIC SEQ NUMBER                           
QFILM    DS    CL8                 FILM CODE                                    
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
LKUPKEY  DS    XL16                LOCKUP KEY                                   
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
MELEM3   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
SVDMELEM DS    XL(L'MELEM)         SAVED MINIO ELEMENT                          
*                                                                               
XPKEY    DS    XL64                                                             
XSVPKEY  DS    XL64                                                             
SVBPRD   DS    XL1                                                              
SVBPRD2  DS    XL1                                                              
*                                                                               
BUFFER   DS    XL(MXBUFNUM*(BUFFNEXT-BUFFCODE))                                 
BUFFERX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                                       
***********************************************************************         
NUMFLTRS EQU   2                   SO FAR 2 FILTER FLAGS                        
NLSTLINS EQU   12                  # OF LINES BTWN HEADING AND PFKEYS           
*****  USING    ===>   <===  INSTEAD OF SNVIDSLN BECAUSE WE SOMETIMES           
*****           ===>   <===  GET 00'S AND THE PROG THINKS NOTHING THERE         
*****           ===>   <===  TO VALIDATE ON THE LINE                            
*****           ===>   <===  NOTE:  USE  DLSTLENQ-1  FOR MINEKEY                
DLSTLENQ EQU   SNVIDPRD-SNVIDDAY   L(DETAIL ENTRY IN LIST)                      
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
**PAN#1  DC    CL21'006REREI03   04/27/07'                                      
         END                                                                    
