*          DATA SET REREI01    AT LEVEL 012 AS OF 11/19/07                      
*PHASE T82601A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T82601 - LIST OF INVOICES                                             
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T82600), WHICH CALLS                        
*               GEGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     REREIFE (T826FE) -- LIST                                        
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
         TITLE 'REREI01 - REP INVOICE LIST OVERLAY'                             
T82601   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82601*,R7,RR=R3                                              
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
MAIN00   BAS   RE,SETPFTBL                                                      
*                                                                               
         GOTOR MODSCRN             MODIFY SCREEN FOR LOCAL REP                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LRECS                                                            
         CLI   MODE,PRINTREP       REPORT STATIONS?                             
         BE    PRECS                                                            
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         MVI   CURRSYS,C'R'        ALWAYS START IN REP                          
         MVC   LLIST,=Y(LINNEXTL-LINOFFH)  L(LINE W/O SELECT FIELD)             
         NI    MISCFLG1,X'FF'-MF1KYCHG   ASSUME NO KEY FIELDS CHANGED           
         XC    QSTA,QSTA                                                        
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  LA    R2,LSTSTAH                                                       
         TM    CTLRFLG1,CF1LOCAL   LOCAL STATION                                
         BZ    VKSTA02                                                          
*                                                                               
         MVC   8(L'LSTSTA,R2),SVSIGNON                                          
         OI    6(R2),X'80'         RESTRANSMIT                                  
         OI    1(R2),X'20'         PROTECTED                                    
         MVI   5(R2),L'LSTSTA      LENGTH                                       
*                                                                               
         LA    R6,LSTSTA                                                        
         AHI   R6,L'LSTSTA-1       FIND THE LAST CHARACTER                      
         CLI   0(R6),X'40'                                                      
         BH    *+8                                                              
         BCT   R6,*-8                                                           
*                                                                               
         CLI   0(R6),C'L'          NO L FOR STATION                             
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
                                                                                
*                                                                               
VKSTA02  CLI   5(R2),0                                                          
         BNE   VKSTA10                                                          
         TM    WHEN,X'40'          NOW REPORT                                   
         BNO   MISSFLD                                                          
         B     VKSTAX                                                           
*                                                                               
VKSTA10  TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALISTA                                                          
         BNE   INVLFLD                                                          
*                                                                               
         MVC   LSTSTNM,MKTNM       SHOW MARKET NAME                             
         OI    LSTSTNMH+6,X'80'                                                 
*                                                                               
VKSTAX   DS    0H                                                               
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE PERIOD = MONTH                                                   
*****                                                                           
VKPER00  LA    R2,LSTMONH                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 PERVAL,DMCB,(LSTMONH+5,LSTMON),PERVALST                          
         TM    DMCB+4,X'01'                                                     
         BO    INVLFLD                                                          
*                                                                               
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASD                  START DAY ASSUMED?             
         BZ    DATEERR                           YES...ERROR                    
*                                                                               
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
         OI    LSTMNDTH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(X'12',BRDCSDAT),(8,LSTMNDT)                         
*                                                                               
VKPERX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
         DROP  R3                                                               
*****                                                                           
* VALIDATE THE OFFICE                                                           
*****                                                                           
VKOFF00  LA    R2,LSTOFFH                                                       
         XC    FILTOFF,FILTOFF                                                  
         CLI   5(R2),0                                                          
         BE    VKOFFX                                                           
*                                                                               
         MVC   FILTOFF,LSTOFF                                                   
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   VKOFFX                                                           
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    VKOFFX              YES                                          
         CLC   LSTOFF,TWAACCS+2    NO, MUST MATCH RESTRICTED OFFICE             
         BNE   NOACCESS            NOT AUTH'D FOR DATA                          
*                                                                               
VKOFFX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE CONTRACT/STATION ORDER #                                         
*****                                                                           
VKCON00  LA    R2,LSTCONH                                                       
         XC    FILTCONT,FILTCONT                                                
         XC    FILTORD#,FILTORD#                                                
         MVI   FILTORDL,0                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKCONX                                                           
*                                                                               
         TM    4(R2),X'08'                                                      
         BZ    INVLFLD             MUST BE NUMERIC ONLY                         
*                                                                               
*                                                                               
         TM    CTLRFLG1,CF1LOCAL   LOCAL STATION?                               
         BZ    VKCON04                                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   FILTORD#(0),8(R2)   ORDER #                                      
         MVC   FILTORDL,5(R2)      LENGTH                                       
         B     VKCONX                                                           
*                                                                               
VKCON04  DS    0H                                                               
*&&DO                                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(5),8(0,R2)                                                  
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK(5)                                                
         SRP   WORK+5(5),1,0                                                    
         MVC   BCONT99,WORK+5                                                   
         MVC   FILTCONT,BCONT99                                                 
*&&                                                                             
         MVC   FILTCONT,8(R2)                                                   
         OC    FILTCONT,SPACES                                                  
         MVC   FILTCONL,5(R2)                                                   
*                                                                               
VKCONX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKINV00  LA    R2,LSTINVH                                                       
         XC    FILTINVC,FILTINVC                                                
         MVI   FILTINVL,0                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKINVX                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FILTINVC(0),8(R2)                                                
         MVC   FILTINVL,5(R2)                                                   
*                                                                               
VKINVX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE OPTIONS                                                          
*****                                                                           
VKOPT00  LA    R2,LSTOPTNH                                                      
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                 YES                                          
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         NI    FILTFLG1,FF1PERD    CLEAR ALL FILTERS BUT PERIOD                 
         MVI   FILTFLG2,0                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKOPTX                                                           
*                                                                               
         L     RE,ATIOB                                                         
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR FOR ERROR                         
         LR    R1,R2                                                            
         SR    R1,RA                                                            
         STCM  R1,3,TIOBCURD       OFFSET TO FIELD FROM START OF TWA            
         DROP  RE                                                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'88',BLOCK)                                  
         CLI   4(R1),0                                                          
         BE    INVLFLD                                                          
         CLI   4(R1),7                                                          
         BH    INVLFLD             TOO MANY INPUTS FOR OPTIONS                  
*                                                                               
         LA    R3,BLOCK                                                         
VKOPT10  CLI   0(R3),0             ANY MORE ENTRIES?                            
         BE    VKOPTX              NONE                                         
*                                                                               
         CLI   1(R3),0             SEPARATED BY AN '=' SIGN?                    
         BE    VKOPT50             NO                                           
***********************************                                             
* FIELD SEPARATED BY '=' SIGN                                                   
***********************************                                             
         L     RE,ATIOB            POINT TO 1ST HALF                            
         USING TIOBD,RE                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  RE                                                               
*                                                                               
         ZIC   R1,0(R3)            R1=L(1ST HALF)                               
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'INV'    INVOICE DATE FILTER?                         
         BNE   VKOPT13                                                          
         TM    FILTFLG2,FF2INVDT   GOT ONE ALREADY?                             
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2INVDT   YES                                          
         LA    R2,FILTIVDT         R0=A(FILTER INVOICE DATE)                    
         B     VKOPT16                                                          
*                                                                               
VKOPT13  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'DUE'    DUE DATE FILTER?                             
         BNE   VKOPT20                                                          
         TM    FILTFLG2,FF2DUEDT   GOT ONE ALREADY?                             
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2DUEDT   YES                                          
         LA    R2,FILTDUDT         R0=A(FILTER DUE DATE)                        
*                                                                               
VKOPT16  CLI   1(R3),0             ANY DATA ON 2ND HALF?                        
         BE    VKOPTINV                                                         
*                                                                               
         LA    R0,22(R3)                                                        
         ST    R0,DMCB                                                          
         MVC   DMCB(1),1(R3)                                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(X'40',PERVALST)                                    
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLFLD                                                          
*                                                                               
         LA    RE,PERVALST                                                      
         USING PERVALD,RE                                                       
         MVC   0(L'PVALCSTA,R2),PVALCSTA   SAVE THE DATE                        
         B     VKOPTLP                                                          
         DROP  RE                                                               
*                                                                               
VKOPT20  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'ADV='                                                
         BNE   VKOPT30                                                          
         TM    FILTFLG2,FF2ADV     ERROR IF WE HAVE ONE ALREADY                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2ADV                                                  
         XC    FILTADV,FILTADV                                                  
         MVC   FILTADV,22(R3)                                                   
         B     VKOPTLP                                                          
*                                                                               
VKOPT30  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'SAL='                                                
         BNE   VKOPT40                                                          
         TM    FILTFLG2,FF2SAL     ERROR IF WE HAVE ONE ALREADY                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2SAL                                                  
         XC    FILTSAL,FILTSAL                                                  
         MVC   FILTSAL,22(R3)                                                   
         B     VKOPTLP                                                          
*                                                                               
VKOPT40  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'AGY='                                                
         BNE   VKOPTINV                                                         
         TM    FILTFLG2,FF2AGY     ERROR IF WE HAVE ONE ALREADY                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2AGY                                                  
         XC    FILTAGY,FILTAGY                                                  
*                                                                               
         CLI   1(R3),4             NEED A VALUE AFTER 'AGY='                    
         BH    VKOPTINV                                                         
         CLI   1(R3),3             NEED A VALUE AFTER 'AGY='                    
         BL    VKOPTINV                                                         
*                                                                               
         MVC   FILTAGY,22(R3)                                                   
         B     VKOPTLP                                                          
*                                                                               
VKOPTINV B     INVLFLD                                                          
***********************************                                             
* PLAIN FIELD, NOT SEPARATED BY '=' SIGN                                        
***********************************                                             
VKOPT50  L     RE,ATIOB                                                         
         USING TIOBD,RE                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  RE                                                               
*                                                                               
         ZIC   R1,0(R3)            R1 = L(OPTION)                               
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8              FILTER ON RESPONSE INVOICES?                 
         B     *+10                                                             
         CLC   12(0,R3),=C'RESPONSE'                                            
         BNE   VKOPT52                                                          
         TM    FILTFLG1,FF1NRSPN                                                
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1RSPNS   YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT52  EX    R1,*+8              FILTER ON NON-RESPONSE INVOICES?             
         B     *+10                                                             
         CLC   12(0,R3),=C'-RESPONSE'                                           
         BNE   VKOPT54                                                          
         TM    FILTFLG1,FF1RSPNS                                                
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1NRSPN   YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT54  EX    R1,*+8              FILTER ON MIDFLIGHT CLEARANCE?               
         B     *+10                                                             
         CLC   12(0,R3),=C'MCT'                                                 
         BNE   VKOPT56                                                          
         TM    FILTFLG1,FF1NMCT                                                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1MCT     YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT56  EX    R1,*+8              FILTER ON NON-MIDFLIGHT CLEARANCE?           
         B     *+10                                                             
         CLC   12(0,R3),=C'-MCT'                                                
         BNE   VKOPT58                                                          
         TM    FILTFLG1,FF1MCT                                                  
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1NMCT    YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT58  EX    R1,*+8              FILTER ON EASI INVOICES?                     
         B     *+10                                                             
         CLC   12(0,R3),=C'EASI'                                                
         BNE   VKOPT60                                                          
         TM    FILTFLG1,FF1NEASI                                                
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1EASI    YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT60  EX    R1,*+8              FILTER ON EASI INVOICES?                     
         B     *+10                                                             
         CLC   12(0,R3),=C'-EASI'                                               
         BNE   VKOPTINV                                                         
         TM    FILTFLG1,FF1EASI                                                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1NEASI    YES                                         
         B     VKOPTLP                                                          
*                                                                               
VKOPTLP  LA    R3,32(R3)           CHECK NEXT ENTRY                             
         B     VKOPT10                                                          
*                                                                               
VKOPTX   LA    R2,LSTOPTNH         VALIDATE THE FIELD                           
         OI    4(R2),X'20'                                                      
*                                                                               
         L     RE,ATIOB            DON'T NEED SPECIAL ERROR CURSOR              
         USING TIOBD,RE               ANYMORE                                   
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  RE                                                               
*                                                                               
VKX      TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    *+10                                                             
         XC    SVMASTKY,SVMASTKY   YES, START FROM BEGINNING                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT NOW REPORT LIST OF STATIONS FOR A PARTICULAR MONTH                      
***********************************************************************         
PRECS    DS    0H                                                               
                                                                                
         LA    R3,LSTSELLH         CLEAR LIST PORTION OF SCREEN                 
         USING LINDSECT,R3                                                      
         TWAXC LSTSEL1H,LINTOTH,PROT=Y                                          
         DROP  R3                                                               
*                                                                               
         CLI   LSTSTAH+5,0         STATION SPECIFIC?                            
         BE    PR00                NO...LIST OF STATIONS                        
         BAS   RE,PRSTA            YES...LIST INVOICES FOR THIS STATION         
         B     PRX                 AND EXIT                                     
*                                                                               
PR00     LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,HSPEC                                                         
         ST    R1,SPECS                                                         
*                                                                               
         XC    LASTSTA,LASTSTA                                                  
*                                                                               
         LA    R1,KEY                                                           
         USING SNVRKEY,R1                                                       
         MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,AGENCY                                                    
         DROP  R1                                                               
*                                                                               
         MVC   KEYSAVED,KEY           SAVE A COPY OF THE BASE KEY               
*                                                                               
         BAS   RE,XSPDHIGH                                                      
         CLI   DMCB+8,0                                                         
         BE    PR100                                                            
         DC    H'0'                                                             
*                                                                               
PRSEQ    BAS   RE,XSPDSEQ                                                       
         CLI   DMCB+8,0                                                         
         BE    PR100                                                            
         DC    H'0'                                                             
*                                                                               
PR100    L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVKEY(SNVRSTA-SNVRKEY),KEYSAVED    SAME AGY?                    
         BNE   PRX                                                              
*                                                                               
         CLI   SNVRMIN,X'FF'       ONLY PRINT ONCE- MINIO RECS                  
         BNE   PRSEQ                                                            
*                                                                               
         OC    QSTA,QSTA           STATION FILTER?                              
         BZ    *+14                                                             
         CLC   SNVRSTA,QSTA                                                     
         BNE   PRSEQ                                                            
*                                                                               
         CLC   SNVRMOS,BMOSFF                                                   
         BNE   PRSEQ                                                            
*                                                                               
         CLC   SNVRSTA,LASTSTA     DON'T PRINT DUPS                             
         BE    PRSEQ                                                            
         MVC   LASTSTA,SNVRSTA                                                  
*                                                                               
         MVC   PSTA,SNVRSTA                                                     
*                                                                               
***      ZAP   WORK(5),=P'99999999'                                             
***      ZAP   WORK+20(5),=P'0'                                                 
***      MVO   WORK+20(5),SNVRCON               UNPACK                          
***      SP    WORK(5),WORK+20(5)                                               
***      MVC   BLOCK(5),WORK                                                    
***      EDIT  (P5,BLOCK),(8,PCONT),ALIGN=LEFT    CONTRACT #                    
*                                                                               
***      MVC   PINV,SNVRINV                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRSEQ                                                            
*                                                                               
PRX      XC    SVMASTKY,SVMASTKY   START LISTING NEW NEXT TIME                  
         B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
* PRINT NOW REPORT LIST OF INVOICES FOR A PARTICULAR STATION AND MONTH          
***********************************************************************         
PRSTA    DS    0H                                                               
*                                                                               
         LA    R1,HDHOOK2                                                       
         ST    R1,HEADHOOK                                                      
         LA    R1,HSPEC2                                                        
         TM    CTLRFLG1,CF1LOCAL   LOCAL STATION SIGN ON?                       
         BZ    *+8                                                              
         LA    R1,HSPEC6           USE A DIFFERENT HEADING                      
         ST    R1,SPECS                                                         
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R1,KEY                                                           
         USING SNVRKEY,R1                                                       
         MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
*                                                                               
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,AGENCY                                                    
         TM    CTLRFLG1,CF1LOCAL                                                
         BZ    *+14                                                             
         MVI   SNVRSUB,SNVLSUBQ    x'B3'                                        
         MVC   SNVREP,TWAORIG                                                   
*                                                                               
         MVC   SNVRSTA,QSTA                                                     
         MVC   SNVRMOS,BMOSFF                                                   
         DROP  R1                                                               
*                                                                               
PS20     MVC   KEYSAVED,KEY           SAVE A COPY OF THE BASE KEY               
*                                                                               
         BAS   RE,XSPDHIGH                                                      
         CLI   DMCB+8,0                                                         
         BE    PSLOOP                                                           
         DC    H'0'                                                             
*                                                                               
PSLOOP   L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
*                                                                               
         GOTOR SAMEKEY             GOT SAME BASE KEY?                           
         BNE   PSENDLST                                                         
*                                                                               
         CLC   SVMASTKY,SNVKMAST   SAME MASTER KEY?                             
         BE    PSNXTREC            YES, SAW THIS ONE ALREADY                    
         MVC   SVMASTKY,SNVKMAST   NO, SAVE A COPY OF THE MASTER KEY            
*                                                                               
         GOTOR GETMNIO             GET MINIO RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSFILT10 OC    FILTCONT,FILTCONT   NEED TO CHECK CONTRACT FILTER?               
         BZ    PSFILT12            NO                                           
         GOTOR GETORD#,DMCB,(X'80',0)                                           
         CLC   FILTCONT,WORK                                                    
         BNE   PSAFMNIO                                                         
*&&DO                                                                           
PSFILT12 OC    FILTORD#,FILTORD#   NEED TO CHECK STATION ORDER FILTER?          
         BZ    PSFILT15            NO                                           
         CLC   SNVLSORD,FILTORD#                                                
         BNE   PSAFMNIO                                                         
*&&                                                                             
PSFILT12 CLI   FILTORDL,0          NEED TO CHECK STATION ORDER FILTER?          
         BE    PSFILT15            NO                                           
         ZIC   R1,FILTORDL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SNVLSORD(0),FILTORD#                                             
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT15 CLI   FILTINVL,0          NEED TO CHECK INVOICE FILTER?                
         BE    PSFILT18            NO                                           
         ZIC   R1,FILTINVL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SNVRINV(0),FILTINVC                                              
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT18 DS    0H                                                               
         TM    CTLRFLG1,CF1LOCAL   LOCAL STATION SIGN ON?                       
         BZ    PS30                                                             
         CLC   SNVLPOW,TWAAGY                                                   
         BNE   PSAFMNIO                                                         
*                                                                               
PS30     CLI   CURRSYS,C'R'                                                     
         BE    PS35                                                             
         GOTO1 REPSYS                                                           
         BNE   INVLFLD             <-- CHANGE TO SYS NOT STARTED                
*                                                                               
PS35     XC    QOFF,QOFF                                                        
         TM    CTLRFLG1,CF1LOCAL+CF1MOINV                                       
         BNZ   PS38                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETCONT             GET CONTRACT RECORD                          
         MVC   AIO,AIO1                                                         
         BNE   PSAFMNIO            NOT FOUND SKIP                               
*                                                                               
*       MAKE SURE USER CAN SEE THIS OFFICE                                      
*                                                                               
PS38     CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   PS40                                                             
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    PS40                YES                                          
         CLC   QOFF,TWAACCS+2      NO, MUST MATCH RESTRICTED OFFICE             
         BNE   PSAFMNIO            NOT AUTH'D SO SKIP                           
*                                                                               
*       MAKE SURE USER WANTS TO SEE THIS OFFICE                                 
*                                                                               
PS40     OC    FILTOFF,FILTOFF                                                  
         BZ    PS42                                                             
         CLC   QOFF,FILTOFF                                                     
         BNE   PSAFMNIO                                                         
*                                                                               
*PS42     GOTO1 INITMNIO                                                        
*                                                                               
PS42     XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          NOT INVOICE HEADERS?                         
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    PS41                                                             
         MVC   PDATE,=C'INVALID'                                                
         B     PSAFMNIO                                                         
*                                                                               
PS41     DS    0H                                                               
         TM    FILTFLG2,FF2ADV     FILTERING ON ADVERTISER?                     
         BZ    PSFILT20                                                         
         CLC   QADV,FILTADV                                                     
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT20 TM    FILTFLG2,FF2AGY     FILTERING ON AGENCY?                         
         BZ    PSFILT22                                                         
         CLC   QAGY,FILTAGY                                                     
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT22 TM    FILTFLG2,FF2SAL     FILTERING ON SALESPERSON?                    
         BZ    PSFILT25                                                         
         CLC   QSAL,FILTSAL                                                     
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT25 TM    FILTFLG2,FF2INVDT   FILTERING ON INVOICE DATE?                   
         BZ    PSFILT30                                                         
         CLC   SNVHDIDT,FILTIVDT                                                
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT30 TM    FILTFLG2,FF2DUEDT   FILTERING ON DUE DATE?                       
         BZ    PSFILT32                                                         
         CLC   SNVHDDDT,FILTDUDT                                                
         BNE   PSAFMNIO                                                         
*                                                                               
PSFILT32 TM    FILTFLG1,FF1RSPNS   FILTERING ON RESPONSE INVOICES?              
         BZ    PSFILT34                                                         
         TM    SNVHDCTL,SNVHDRSQ                                                
         BZ    PSAFMNIO                                                         
         B     PS45                                                             
*                                                                               
PSFILT34 TM    FILTFLG1,FF1NRSPN   FILTERING ON NON-RESPONSE INVOICES?          
         BZ    PS45                                                             
         TM    SNVHDCTL,SNVHDRSQ                                                
         BNZ   PSAFMNIO                                                         
*                                                                               
PS45     ZAP   TAXAMNT,=P'0'       LIST INVOICE                                 
*                                                                               
         MVC   POFFICE,QOFF             OFFICE                                  
         MVC   PCONTRCT(L'QCONT),QCONT  CONTRACT                                
*                                                                               
         GOTOR GETORD#,DMCB,0                                                   
TEMP     USING ID#D,WORK                                                        
         MVC   PCONTRCT,TEMP.ID#                                                
         DROP  TEMP                                                             
*                                                                               
         MVC   PINVOICE,QINVOICE        INVOICE                                 
         MVC   PADV,QADV                ADV                                     
         MVC   PAGY,QAGY                AGENCY                                  
         MVC   PSAL,QSAL                SALES                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,SNVHDIDT),(8,PDATE)                               
         DROP  R6                                                               
*                                                                               
         XC    MINEKEY,MINEKEY     CALCULATE THE TOTAL OF THE DETAILS           
         MVI   MINEKEY,SNVIDELQ                                                 
         ZAP   PL16,TAXAMNT                                                     
         ZAP   FULL,=P'0'          NUMBER OF DETAILS                            
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    PS90LOOP                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    PS90X                                                            
         DC    H'0'                                                             
*                                                                               
PS90LOOP L     R6,MINELEM          SUM UP THE DETAIL AMOUNTS FROM THE           
         CLI   0(R6),SNVIDELQ                                                   
         BNE   PS90X                                                            
*                                                                               
         USING SNVIDELD,R6             DETAILS                                  
PS92     ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
PS94     CVD   R1,DUB                                                           
         AP    PL16,DUB                                                         
PS96     AP    FULL,=P'1'                                                       
*                                                                               
PS90NEXT BAS   RE,MINIOSEQ                                                      
         BE    PS90LOOP                                                         
*                                                                               
PS90X    EDIT  (P16,PL16),(15,PTTLSPTS),2,ZERO=NOBLANK,FLOAT=-                  
*                                                                               
PS90X2   MVI   PTTLSPTS+15,C'/'                                                 
         LA    RE,PTTLSPTS+16                                                   
         EDIT  (P4,FULL),(4,0(RE)),ZERO=NOBLANK                                 
*                                                                               
PSAFMNIO XC    KEY,KEY                    AFTER INITMNIO, YOU HAVE TO           
         MVC   KEY(L'SVMASTKY),SVMASTKY   REESTABLISH THE KEY FOR THE           
         BAS   RE,XSPDHIGH                READ SEQ                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSNXTREC GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,XSPDSEQ                                                       
         CLI   DMCB+8,0                                                         
         BE    PSLOOP                                                           
         CLI   DMCB+8,X'80'        END-OF-FILE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSENDLST NI    LSTSTAH+4,X'FF'-X'60'                                            
         OI    LSTSTAH+4,X'80'                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        HEADHOOK AND HEADSPECS                                                 
*----------------------------------------------------------------------         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+19(L'LSTMON),LSTMON                                           
         XIT1                                                                   
*                                                                               
HSPEC    DS    0H                                                               
         SSPEC H2,2,RUN                                                         
         SSPEC H1,48,REQUESTOR                                                  
         SSPEC H2,48,REPORT                                                     
         SSPEC H2,58,PAGE                                                       
         SSPEC H1,2,C'STATION INVOICE LIST'                                     
         SSPEC H4,2,C'MONTH OF SERVICE:'                                        
         SSPEC H5,2,C'AVAILABLE STATIONS:'                                      
***      SSPEC H5,11,C'CONTRACT'                                                
***      SSPEC H5,21,C'INVOICE#'                                                
         DC    H'0'                                                             
*                                                                               
HDHOOK2  NTR1                                                                   
         MVC   H1+28(L'LSTSTA),LSTSTA                                           
         MVC   H4+19(L'LSTMON),LSTMON                                           
         XIT1                                                                   
*                                                                               
HSPEC2   DS    0H                                                               
         SSPEC H1,2,C'INVOICE LIST FOR STATION - '                              
         SSPEC H1,48,REQUESTOR                                                  
         SSPEC H2,2,RUN                                                         
         SSPEC H2,48,REPORT                                                     
         SSPEC H2,58,PAGE                                                       
         SSPEC H4,2,C'MONTH OF SERVICE:'                                        
         SSPEC H6,2,C'OFF'                                                      
*                                                                               
         SSPEC H6,6,C' CON#/ORD#'                                               
*                                                                               
         SSPEC H6,19,C'INVOICE#'                                                
         SSPEC H6,30,C'ADV'                                                     
         SSPEC H6,35,C'AGY'                                                     
         SSPEC H6,40,C'SAL'                                                     
         SSPEC H6,47,C'DATE'                                                    
         SSPEC H6,61,C'TOTAL    SPTS'                                           
         SSPEC H7,2,C'---'                                                      
         SSPEC H7,6,C'-----------'                                              
         SSPEC H7,19,C'--------'                                                
         SSPEC H7,30,C'---'                                                     
         SSPEC H7,35,C'---'                                                     
         SSPEC H7,40,C'---'                                                     
         SSPEC H7,45,C'--------'                                                
         SSPEC H7,61,C'-------- ----'                                           
         DC    H'0'                                                             
HSPEC6   DS    0H                                                               
         SSPEC H1,2,C'INVOICE LIST FOR STATION - '                              
         SSPEC H1,48,REQUESTOR                                                  
         SSPEC H2,2,RUN                                                         
         SSPEC H2,48,REPORT                                                     
         SSPEC H2,58,PAGE                                                       
         SSPEC H4,2,C'MONTH OF SERVICE:'                                        
         SSPEC H6,2,C'OFF'                                                      
*                                                                               
         SSPEC H6,7,C'STA-ORD#'    YES, STATION ORDER #                         
*                                                                               
         SSPEC H6,19,C'INVOICE#'                                                
         SSPEC H6,29,C'ADV'                                                     
         SSPEC H6,34,C'AGY'                                                     
         SSPEC H6,39,C'SAL'                                                     
         SSPEC H6,46,C'DATE'                                                    
         SSPEC H6,60,C'TOTAL    SPTS'                                           
         SSPEC H7,2,C'---'                                                      
         SSPEC H7,6,C'----------'                                               
         SSPEC H7,18,C'----------'                                              
         SSPEC H7,29,C'---'                                                     
         SSPEC H7,34,C'---'                                                     
         SSPEC H7,39,C'---'                                                     
         SSPEC H7,44,C'--------'                                                
         SSPEC H7,60,C'-------- ----'                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORDS                                                              
***********************************************************************         
LRECS    DS    0H                                                               
         MVI   NLISTS,13                                                        
         XC    SEQLIST,SEQLIST     NO RECORDS SHOWN                             
         NI    MISCFLG1,X'FF'-MF1NDSCR   NOT END OF SCREEN YET                  
*                                                                               
*                                                                               
         LA    R2,LSTSEL1H         R2 = A(1ST LIST LINE)                        
         LA    R3,LSTSELLH         CLEAR LIST PORTION OF SCREEN                 
         USING LINDSECT,R3                                                      
         TWAXC LSTSEL1H,LINTOTH,PROT=Y                                          
         DROP  R3                                                               
*                                                                               
         USING LINDSECT,R2                                                      
         LA    R3,SEQLIST          R3 = A(1ST SAVED RECORD KEY)                 
         USING LSEQNTRY,R3                                                      
*                                                                               
         XC    KEY,KEY                                                          
         OC    SVMASTKY,SVMASTKY   DID WE FINISH OUR LIST BEFORE?               
         BZ    LR10                                                             
         MVC   KEY(L'SVMASTKY),SVMASTKY  NO, CONTINUE FROM BEFORE               
         XC    SVMASTKY,SVMASTKY                                                
         B     LR20                                                             
*                                                                               
LR10     LA    R1,KEY                                                           
         USING SNVRKEY,R1                                                       
*                                                                               
         MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
*                                                                               
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,AGENCY                                                    
         TM    CTLRFLG1,CF1LOCAL                                                
         BZ    *+14                                                             
         MVC   SNVREP,TWAORIG                                                   
         MVI   SNVRSUB,SNVLSUBQ    x'B3'                                        
*                                                                               
         MVC   SNVRSTA,QSTA                                                     
         MVC   SNVRMOS,BMOSFF                                                   
         DROP  R1                                                               
*                                                                               
LR20     MVC   KEYSAVED,KEY           SAVE A COPY OF THE BASE KEY               
*                                                                               
         BAS   RE,XSPDHIGH                                                      
         CLI   DMCB+8,0                                                         
         BE    LRLOOP                                                           
         DC    H'0'                                                             
*                                                                               
LRLOOP   L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
*                                                                               
         GOTOR SAMEKEY             SAME BASE KEY?                               
         BNE   LRENDLST                                                         
*                                                                               
         CLC   SVMASTKY,SNVKMAST   SAME MASTER KEY?                             
         BE    LRNXTREC            YES, SAW THIS ONE ALREADY                    
         MVC   SVMASTKY,SNVKMAST   NO, SAVE A COPY OF THE MASTER KEY            
*                                                                               
         GOTOR GETMNIO             GET MINIO RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRFILT10 OC    FILTCONT,FILTCONT   NEED TO CHECK CONTRACT FILTER?               
         BZ    LRFILT12            NO                                           
         GOTOR GETORD#,DMCB,(X'80',0)                                           
         ZIC   R1,FILTCONL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FILTCONT(0),WORK                                                 
         BNE   LRAFMNIO                                                         
*&&DO                                                                           
LRFILT12 OC    FILTORD#,FILTORD#   DISABLE FULL MATCH FILTER                    
         BZ    LRFILT15                                                         
         CLC   SNVLSORD,FILTORD#                                                
         BNE   LRAFMNIO                                                         
*&&                                                                             
LRFILT12 CLI   FILTORDL,0          NEED TO CHECK STATION ORDER FILTER?          
         BE    LRFILT15            NO                                           
         ZIC   R1,FILTORDL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SNVLSORD(0),FILTORD#                                             
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT15 CLI   FILTINVL,0          NEED TO CHECK INVOICE FILTER?                
         BE    LRFILT18            NO                                           
         ZIC   R1,FILTINVL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QINVOICE(0),FILTINVC                                             
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT18 DS    0H                                                               
         TM    CTLRFLG1,CF1LOCAL   LOCAL STATION SIGN ON?                       
         BZ    LR30                                                             
         CLC   SNVLPOW,TWAAGY                                                   
         BNE   LRAFMNIO                                                         
*                                                                               
LR30     TM    MISCFLG1,MF1NDSCR   GOT ANOTHER RECORD AND END OF LIST?          
         BNZ   LRDISPLD                                                         
*                                                                               
LR32     DS    0H                                                               
         CLI   CURRSYS,C'R'                                                     
         BE    LR35                                                             
         GOTO1 REPSYS                                                           
         BNE   INVLFLD             <-- CHANGE TO SYS NOT STARTED                
*                                                                               
LR35     XC    QOFF,QOFF                                                        
         TM    CTLRFLG1,CF1LOCAL+CF1MOINV                                       
         BNZ   LR38                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETCONT             GET CONTRACT RECORD                          
         MVC   AIO,AIO1                                                         
         BE    LR38                CONTRACT FOUND                               
         CLI   T826FFD+1,C'*'      DDS TERMINAL?                                
         BE    LR38                                                             
         B     LRAFMNIO            NOT FOUND SKIP                               
*                                                                               
*       MAKE SURE USER CAN SEE THIS OFFICE                                      
*                                                                               
LR38     TM    CTLRFLG1,CF1MOINV                                                
         BO    LR42                MO INVOICES SHOWS UP NO MATTER WHAT          
*                                                                               
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   LR40                                                             
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    LR40                YES                                          
         CLC   QOFF,TWAACCS+2      NO, MUST MATCH RESTRICTED OFFICE             
         BNE   LRAFMNIO            NOT auth'd so skip                           
*                                                                               
*       MAKE SURE USER WANTS TO SEE THIS OFFICE                                 
*                                                                               
LR40     OC    FILTOFF,FILTOFF                                                  
         BZ    LR42                                                             
         CLC   QOFF,FILTOFF                                                     
         BNE   LRAFMNIO                                                         
*                                                                               
LR42     MVC   LSEQPERD,BMOS       SAVE THESE VALUES                            
         MVC   LSEQINVC,QINVOICE                                                
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   LRAFMNIO            DID NOT FIND, JUST SKIP                      
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          NOT INVOICE HEADERS?                         
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    LR41                                                             
         MVC   LINDAT,=C'INVALID'                                               
         B     LR90X3                                                           
*                                                                               
LR41     DS    0H                                                               
         TM    FILTFLG2,FF2ADV     FILTERING ON ADVERTISER?                     
         BZ    LRFILT20                                                         
         CLC   QADV,FILTADV                                                     
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT20 TM    FILTFLG2,FF2AGY     FILTERING ON AGENCY?                         
         BZ    LRFILT22                                                         
         CLC   QAGY,FILTAGY                                                     
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT22 TM    FILTFLG2,FF2SAL     FILTERING ON SALESPERSON?                    
         BZ    LRFILT25                                                         
         CLC   QSAL,FILTSAL                                                     
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT25 TM    FILTFLG2,FF2INVDT   FILTERING ON INVOICE DATE?                   
         BZ    LRFILT30                                                         
         CLC   SNVHDIDT,FILTIVDT                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT30 TM    FILTFLG2,FF2DUEDT   FILTERING ON DUE DATE?                       
         BZ    LRFILT32                                                         
         CLC   SNVHDDDT,FILTDUDT                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT32 TM    FILTFLG1,FF1RSPNS   FILTERING ON RESPONSE INVOICES?              
         BZ    LRFILT34                                                         
         TM    SNVHDCTL,SNVHDRSQ                                                
         BZ    LRAFMNIO                                                         
         B     LR45                                                             
*                                                                               
LRFILT34 TM    FILTFLG1,FF1NRSPN   FILTERING ON NON-RESPONSE INVOICES?          
         BZ    LR45                                                             
         TM    SNVHDCTL,SNVHDRSQ                                                
         BNZ   LRAFMNIO                                                         
*                                                                               
LR45     ZAP   TAXAMNT,=P'0'       LIST INVOICE                                 
*                                                                               
         MVC   LININV,QINVOICE                                                  
*                                                                               
         GOTOR GETORD#,DMCB,0                                                   
TEMP     USING ID#D,WORK                                                        
         MVC   LINCON,TEMP.ID#                                                  
         MVC   LINTYPE,TEMP.IDTYPE                                              
         DROP  TEMP                                                             
         CLI   T826FFD+1,C'*'      DDS TERMINAL?                                
         BNE   *+8                                                              
         NI    LINTYPEH+1,X'FF'-X'0C'                                           
*                                                                               
         MVC   LINOFF,QOFF                                                      
         MVC   LINADV,QADV                                                      
         MVC   LINAGY,QAGY                                                      
         MVC   LINSAL,QSAL                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,SNVHDIDT),(8,LINDAT)                              
         DROP  R6                                                               
*                                                                               
         CLI   T826FFD+1,C'*'      DDS TERMINAL?                                
         BNE   LR50                                                             
         TM    CTLRFLG1,CF1LOCAL+CF1MOINV                                       
         BNZ   LR50                                                             
         OC    QCONT,QCONT         IS CONTRACT ALREADY THERE?                   
         BNZ   LR50                THEN DON'T NEED TO DO THIS                   
*                                                                               
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),BCONT99               UNPACK                          
         SP    WORK(5),WORK+20(5)                                               
         MVC   BLOCK(5),WORK                                                    
         EDIT  (P5,BLOCK),(8,QCONT),ALIGN=LEFT                                  
         MVC   LINCON(L'QCONT),QCONT                                            
*                                                                               
LR50     XC    MINEKEY,MINEKEY     CALCULATE THE TOTAL OF THE DETAILS           
         MVI   MINEKEY,SNVIDELQ                                                 
         ZAP   PL16,TAXAMNT                                                     
         ZAP   FULL,=P'0'          NUMBER OF DETAILS                            
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    LR90LOOP                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    LR90X                                                            
         DC    H'0'                                                             
*                                                                               
LR90LOOP L     R6,MINELEM          SUM UP THE DETAIL AMOUNTS FROM THE           
         CLI   0(R6),SNVIDELQ                                                   
         BNE   LR90X                                                            
*                                                                               
         USING SNVIDELD,R6             DETAILS                                  
LR92     ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
LR94     CVD   R1,DUB                                                           
         AP    PL16,DUB                                                         
LR96     AP    FULL,=P'1'                                                       
*                                                                               
LR90NEXT BAS   RE,MINIOSEQ                                                      
         BE    LR90LOOP                                                         
*                                                                               
LR90X    EDIT  (P16,PL16),(15,LINTOT),2,ZERO=NOBLANK,FLOAT=-                    
*                                                                               
LR90X2   MVI   LINTOT+15,C'/'                                                   
         LA    RE,LINTOT+16                                                     
         EDIT  (P4,FULL),(4,0(RE)),ZERO=NOBLANK                                 
*                                                                               
LR90X3   DS    0H                  NO X'10' ELEMENT FIX                         
         LA    R3,LSEQNEXT                                                      
         LA    R2,LINNEXTL         R2 = A(NEXT LINE)                            
         LA    R0,LSTSELLH                                                      
         CR    R2,R0               DID WE GO BEYOND END OF LIST SCREEN?         
         BNH   *+8                                                              
         OI    MISCFLG1,MF1NDSCR   YES                                          
*                                                                               
LRAFMNIO XC    KEY,KEY                    AFTER INITMNIO, YOU HAVE TO           
         MVC   KEY(L'SVMASTKY),SVMASTKY   REESTABLISH THE KEY FOR THE           
         BAS   RE,XSPDHIGH                READ SEQ                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRNXTREC BAS   RE,XSPDSEQ                                                       
         CLI   DMCB+8,0                                                         
         BE    LRLOOP                                                           
         CLI   DMCB+8,X'80'        END-OF-FILE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRENDLST LA    R2,LSTSEL1H                                                      
         XC    SVMASTKY,SVMASTKY   START FROM BEGINNING NEXT TIME               
         B     ENDOFLST            END OF LIST - SELECT OR HIT ENTER ..         
*                                                                               
LRDISPLD LA    R2,LSTSEL1H         LIST DISPLAYED - SELECT OR HIT ENT..         
         B     LSTDSPLD                                                         
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TEST TO SEE IF THE KEY IS THE SAME FROM KEY SAVE                              
* LOCAL REP USE A DIFFERENT KEY THAN NORMAL REP                                 
* CC EQU: SAME BASE KEY AS KEYSAVE                                              
* CC NOT EQU: DIFF BASE KEY AS KEYSAVE                                          
***********************************************************************         
SAMEKEY  L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         TM    CTLRFLG1,CF1LOCAL                                                
         BO    SAME20              LOCAL REP USE DIFFERENT KEY                  
*                                                                               
         CLC   SNVKEY(SNVRCON-SNVRKEY),KEYSAVED    GOT SAME BASE KEY?           
         B     SAMEX                                                            
SAME20   DS    0H                                                               
         CLC   SNVKEY(SNVLSORD-SNVRKEY),KEYSAVED                                
SAMEX    BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET MINIO RECORD                                                              
***********************************************************************         
         USING SNVKEYD,R6                                                       
         USING LINDSECT,R2                                                      
*                                                                               
GETMNIO  NTR1                                                                   
         L     R6,AIO                                                           
         TM    CTLRFLG1,CF1LOCAL   LOCAL REP?                                   
         BO    GMNIO20             NO - SKIP                                    
*                                                                               
         NI    CTLRFLG1,X'FF'-CF1MOINV-CF1MOREP-CF1MOSTA                        
         OC    SNVRCON,SNVRCON                                                  
         BNZ   GMNIO10                                                          
         OI    CTLRFLG1,CF1MOINV   SET MO INVOICE FLAG                          
*                                                                               
         CLI   SNVRFLG,C'R'                                                     
         BNE   *+8                                                              
         OI    CTLRFLG1,CF1MOREP                                                
*                                                                               
         CLI   SNVRFLG,C'S'                                                     
         BNE   *+8                                                              
         OI    CTLRFLG1,CF1MOSTA                                                
*                                                                               
GMNIO10  MVC   BCONT99,SNVRCON     SAVE CONTRACT AND INVOICE                    
         MVC   QINVOICE,SNVRINV                                                 
         MVC   QORD#,SNVRINV        FOR MO INVOICES                             
         MVC   QSNVRFLG,SNVRFLG    S/R/NULL                                     
         B     GMNIO22                                                          
*                                                                               
GMNIO20  MVC   QSTAORD#,SNVLSORD   SAVE STATION ORDER NUMBER                    
*                                                                               
GMNIO22  GOTO1 INITMNIO                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    GMNIO24             FIND, CONTINUE                               
         DC    H'0'                                                             
*                                                                               
GMNIO24  L     R6,MINELEM          NOT INVOICE HEADERS?                         
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    GMNIO26                                                          
         MVC   LINDAT,=C'INVALID'                                               
         B     NO                                                               
*                                                                               
GMNIO26  TM    CTLRFLG1,CF1LOCAL+CF1MOINV     LOCAL/MO INVOICE?                 
         BZ    YES                 NO - DONE                                    
*                                                                               
         XC    MINEKEY,MINEKEY     SAVE OFF INVOICE NUMBER                      
         MVI   MINEKEY,SNVRIELQ                                                 
         BAS   RE,MINIOHI                                                       
         BNE   GMNIO30                                                          
*                                                                               
         USING SNVRINVD,R6                                                      
GMNIO40  MVC   QINVOICE,SNVRINV#                                                
         MVC   QSAL,SNVRISNM                                                    
         MVC   QADV,SNVRIANM                                                    
         MVC   QAGY,SNVRIAGN                                                    
         B     YES                                                              
*                                                                               
GMNIO30  MVC   LINDAT,=C'NO INV#'  MISSING INVOICE ELEMENT                      
         B     NO                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CHANGE HEADING FOR LOCAL REP SIGN ON                                          
***********************************************************************         
MODSCRN  TM    CTLRFLG1,CF1LOCAL   LOCAL REP SIGN ON?                           
         BZ    MODSX                                                            
         MVC   LSTHEAD,=C'Sel     Sta-Ord#   Invoice#   ADV  AGY'               
         OI    LSTHEADH+6,X'80'    YES, CHANGE HEADING                          
         MVC   LSTCONA,=C'Sta-Ord#'                                             
         OI    LSTCONAH+6,X'80'                                                 
         OI    LSTOFFH+1,X'20'         PROTECTED                                
MODSX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY TABLE                                                           
***********************************************************************         
ID       USING ID#D,WORK                                                        
*                                                                               
GETORD#  NTR1                                                                   
         XC    WORK(2*LID),WORK                                                 
         MVC   BYTE,0(R1)                                                       
         TM    CTLRFLG1,CF1LOCAL                                                
         BZ    GORD#10                                                          
         MVC   ID.ID#,QSTAORD#                                                  
*        MVC   ID.IDTYPE,C'S'                                                   
         B     GORD#X                                                           
*                                                                               
GORD#10  TM    CTLRFLG1,CF1MOINV                                                
         BZ    GORD#50                                                          
         MVC   ID.ID#,QORD#                                                     
         TM    CTLRFLG1,CF1MOREP                                                
         BZ    GORD#20                                                          
         MVI   ID.IDTYPE,C'R'                                                   
         B     GORD#100                                                         
*                                                                               
GORD#20  TM    CTLRFLG1,CF1MOSTA                                                
         BZ    GORD#100                                                         
         MVI   ID.IDTYPE,C'S'                                                   
         B     GORD#100                                                         
*                                                                               
GORD#50  TM    BYTE,X'80'                                                       
         BO    GORD#60                                                          
         MVC   ID.ID#(L'QCONT),QCONT                                            
         MVI   ID.IDTYPE,C'C'                                                   
         B     GORD#100                                                         
GORD#60  ZAP   WORK+50(5),=P'99999999'                                          
         ZAP   WORK+70(5),=P'0'                                                 
         MVO   WORK+70(5),BCONT99               UNPACK                          
         SP    WORK+50(5),WORK+70(5)                                            
         MVC   BLOCK(5),WORK+50                                                 
         EDIT  (P5,BLOCK),(8,ID.ID#),ALIGN=LEFT                                 
         OC    ID.ID#,SPACES                                                    
         B     GORD#X                                                           
*                                                                               
GORD#100 DS    0H                  SQUASH THE ORDER# AND ORDER TYPE             
         TM    BYTE,X'80'                                                       
         BO    GORD#X                                                           
*        CLI   T826FFD+1,C'*'      DDS TERMINAL?                                
*        BE    GORD#X                                                           
*                                                                               
         LA    RE,WORK+(2*LID)-1                                                
         LA    RF,WORK+LID-1                                                    
         LA    R1,WORK                                                          
GORDLOOP CR    R1,RF                                                            
         BH    GORDLX                                                           
         CLI   0(RF),X'40'                                                      
         BNH   *+12                                                             
         MVC   0(1,RE),0(RF)                                                    
         BCTR  RE,0                                                             
*                                                                               
         BCTR  RF,0                                                             
         B     GORDLOOP                                                         
*                                                                               
GORDLX   MVC   ID.ID#(LID),ID.ID#COPY                                           
GORD#X   B     XIT                                                              
         EJECT                                                                  
         DROP  ID                                                               
***********************************************************************         
* SET THE PFKEY TABLE                                                           
***********************************************************************         
SETPFTBL NTR1                                                                   
*                                                                               
         XC    LSTPF12,LSTPF12                                                  
         OI    LSTPF12H+6,X'80'                                                 
         CLI   CALLSP,0                                                         
         BE    *+10                                                             
         MVC   LSTPF12,=C'PF12=Return'                                          
*                                                                               
         SR    R2,R2               NO TABLE NEEDED YET                          
STPF30   LA    R2,PFTABLE                                                       
         LA    R2,SPFTABLE                                                      
STPF50   GOTO1 INITIAL,DMCB,(R2)                                                
         B     XIT                                                              
*                                                                               
* CURDISP IS NOT SET CORRECTLY FOR LINES SELECTED THROUGH THE SELECT            
* FIELD UNTIL INITIAL GET A CHANCE TO CONVERT THE SELECT CODE,                  
* SO WE NEED TO DO TWO PASSES FOR THOSE PFKEYS WHERE CURDISP IS VITAL           
* TO THE CALCULATION OF THE INVOICE HEADER.                                     
*                                                                               
         CLI   PFKEY,2                                                          
         BE    STPF60                                                           
         CLI   PFKEY,4             CURDISP VITAL FOR THE PFKEY?                 
         BE    STPF60                                                           
         B     STPFX               NO                                           
*                                                                               
STPF60   TM    CTLRFLG1,CF1CKOFF          NEED TO CHECK THE OFFSET?             
         BZ    STPF70                     NO                                    
         NI    CTLRFLG1,X'FF'-CF1CKOFF    DON'T CHECK IT NEXT TIME IN           
*                                                                               
         CLI   SELOFFST,X'FF'      ANY PREVIOUS SELECTION?                      
         BE    STPF70                                                           
         ZIC   R1,SELOFFST         YES, CALCULATE LOCATION ON SCREEN            
         MH    R1,=Y(LINNEXTL-LINDSECT)                                         
         LA    R0,LSTSEL1H                                                      
         AR    R1,R0                                                            
         SR    R1,RA                                                            
         STH   R1,CURDISP                                                       
*                                                                               
STPF70   LH    R2,CURDISP          SEE IF CURSOR IS IN A VALID LOCATION         
         AR    R2,RA                   FOR A PFKEY SELECTION                    
         LA    R0,LSTSEL1H                                                      
         CR    R2,R0                                                            
         BL    MISSFLD                                                          
         LA    R0,LSTPFLNH                                                      
         CR    R2,R0                                                            
         BNL   MISSFLD                                                          
*                                                                               
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL1H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         STC   R1,SELOFFST                                                      
         MH    R1,=Y(LSEQNEXT-LSEQNTRY)                                         
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(LSEQNEXT-LSEQNTRY,R1),0(R1)   ANY INVOICE HEADER?              
         BZ    MISSFLD                         DON'T GO ANYWHERE                
*                                                                               
         CLI   PFKEY,4                                                          
         BNE   STPF100                                                          
*                                                                               
STPFK04  XC    KEYLINE,KEYLINE     YES, DETAILS IS SPECIAL                      
         LA    R3,KEYLINE                                                       
*                                                                               
         ZIC   R1,LSTSTAH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LSTSTA                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   0(0,R3),LSTMON                                                   
         LA    R3,1(6,R3)                                                       
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,SELOFFST               CALCULATE BEGINNING OF LINE            
         MH    R1,=Y(LINNEXTL-LINDSECT)      BECAUSE CURSOR COULD BE IN         
         LA    R2,LSTSEL1H(R1)               THE MIDDLE OF THE LINE             
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STH   RE,CURDISP                                                       
*                                                                               
         USING LINDSECT,R2                                                      
         OC    LINCON,LINCON       ANY DATA ON THIS LINE?                       
         BZ    STPFX               NONE, DON'T ALLOW PFKEY                      
*                                                                               
         ZIC   R1,LINOFFH+7                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LINOFF                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,LINCONH+7                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LINCON                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,LININVH+7                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LININV                                                   
*                                                                               
STPFK04X B     STPF100                                                          
         DROP  R2                                                               
*                                                                               
STPF100  LA    R2,SPFTABLE                                                      
         OI    CTLRFLG1,CF1TSELQ   IGNORE SELECT CODES THIS TIME IN             
         OI    CTLRFLG1,CF1CKOFF   DIS/CHA NEEDS TO SAVE SEL OFFSET             
         GOTO1 INITIAL,DMCB,(R2)                                                
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
         CLI   CURRSYS,C'S'                                                     
         BE    MHI10                                                            
         GOTO1 SPTSYS                                                           
         BNE   NOSPTSYS            SPOT SYSTEM NOT STARTED                      
MHI10    GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         CLI   CURRSYS,C'S'                                                     
         BE    MSQ10                                                            
         GOTO1 SPTSYS                                                           
         BNE   NOSPTSYS            SPOT SYSTEM NOT STARTED                      
MSQ10    GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         EJECT                                                                  
***********************************************************************         
* DATAMGR CALLS FOR XSPDIR AND XSPFIL                                           
***********************************************************************         
XSPDHIGH NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     XSPDRCT                                                          
*                                                                               
XSPDSEQ  NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
*                                                                               
XSPDRCT  DS    0H                                                               
         MVC   DMFILE,INVDIR                                                    
*                                                                               
         CLI   CURRSYS,C'S'                                                     
         BE    XSPD10                                                           
         GOTO1 SPTSYS                                                           
         BNE   NOSPTSYS            SPOT SYSTEM NOT STARTED                      
*                                                                               
XSPD10   GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         MVC   KEY(L'SNVKEY),SNVKEY                                             
         MVC   KEYSTATS,SNVDSTAT                                                
         MVC   KEYDSKAD,SNVDDA                                                  
         DROP  R6                                                               
*                                                                               
         CLI   DMCB+8,0            NO ERRORS?                                   
         BE    XSPD20                                                           
         TM    DMCB+8,X'82'        EOF OR RECORD DELETED?                       
         BNZ   XSPD20                                                           
         DC    H'0'                NO, THEN DIE                                 
*                                                                               
XSPD20   DS    0H                                                               
*                                                                               
XSPDX    B     XIT                                                              
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
NOACCESS MVC   GERROR,=AL2(NOTAUTHD)                                            
         B     ERREXIT                                                          
*                                                                               
NOSPTSYS MVC   GERROR,=AL2(SPTNOTST)                                            
         B     ERREXIT                                                          
*                                                                               
DATEERR  MVC   GERROR,=AL2(1138)   INVALID DATE FORMAT                          
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         B     MYERRXIT                                                         
*                                                                               
NEEDMDIA MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
*                                                                               
LSTDSPLD MVI   GERROR1,LSTDISPL    LIST DISPLAYED - SELECT OR HIT ENT..         
         B     INFEXIT                                                          
*                                                                               
ENDOFLST MVI   GERROR1,ENDOLIST    END OF LIST - SELECT OR HIT ENTER ..         
         B     INFEXIT                                                          
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
         MVI   GETMSYS,255         INFO MESSAGES HERE ARE FROM THIS SYS         
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE LIST                                      
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* INVOICE DISPLAY DRIVER                                                        
         DC    AL1(PF02X-*,02,0,0,PFTRETRN)                                     
         DC    CL3'S',CL8' ',CL8' '                                             
PF02X    EQU   *                                                                
*                                                                               
* DETAIL DISPLAY DRIVER                                                         
         DC    AL1(PF04X-*,04,0,0,PFTRETRN)                                     
         DC    CL3'D',CL8' ',CL8' '                                             
PF04X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
***********************************************************************         
* SPECIAL PFKEY TABLE AFTER CURDISP HAS BEEN ESTABLISHED                        
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* INVOICE DISPLAY                                                               
         DC    AL1(SPF02X-*,02,PFTCPROG,(SPF02X-SPF02)/KEYLNQ,0)                
         DC    CL3'S',CL8'INVOICE ',CL8'DISPLAY '                               
SPF02    DC    AL1(KEYTYTWA,L'LSTSTA-1),AL2(LSTSTA-T826FFD)                     
         DC    AL1(KEYTYTWA,L'LSTMON-1),AL2(LSTMON-T826FFD)                     
         DC    AL1(KEYTYCUR,L'LINCON-1),AL2(LINCON-LINOFF)                      
         DC    AL1(KEYTYCUR,L'LINTYPE-1),AL2(LINTYPE-LINOFF)                    
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINOFF)                      
SPF02X   EQU   *                                                                
*                                                                               
* DETAIL DISPLAY                                                                
         DC    AL1(SPF04X-*,04,PFTCPROG,(SPF04X-SPF04)/KEYLNQ,0)                
         DC    CL3' ',CL8'DETAIL ',CL8'DISPLAY '                                
SPF04    DC    AL1(KEYTYTWA,L'LSTSTA-1),AL2(LSTSTA-T826FFD)                     
         DC    AL1(KEYTYTWA,L'LSTMON-1),AL2(LSTMON-T826FFD)                     
         DC    AL1(KEYTYCUR,L'LINCON-1),AL2(LINCON-LINOFF)                      
         DC    AL1(KEYTYCUR,L'LINTYPE-1),AL2(LINTYPE-LINOFF)                    
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINOFF)                      
SPF04X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE REREIWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENSNVN         (RECORD DSECT)                               
         EJECT                                                                  
       ++INCLUDE REREIFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REREIFED         (OUR LIST SCREEN)                             
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAFACTS                                                                       
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDACTIVD                                                                      
* DDGLVXCTLD                                                                    
* DDMINBLK                                                                      
         PRINT ON                                                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
*----------------------------------------------------------------               
* PRINT LINE DSECT                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL7                                                              
PSTA     DS    CL5                                                              
         DS    CL2                                                              
PCONT    DS    CL8                                                              
         DS    CL2                                                              
PINV     DS    CL10                                                             
*                                                                               
         ORG   P                                                                
         DS    CL1                                                              
POFFICE  DS    CL2                                                              
         DS    CL2                                                              
PCONTRCT DS    CL10                                                             
         DS    CL2                                                              
PINVOICE DS    CL10                                                             
         DS    CL1                                                              
PADV     DS    CL4                                                              
         DS    CL1                                                              
PAGY     DS    CL4                                                              
         DS    CL1                                                              
PSAL     DS    CL3                                                              
         DS    CL2                                                              
PDATE    DS    CL8                                                              
         DS    CL1                                                              
PTTLSPTS DS    CL20                                                             
*                                                                               
*----------------------------------------------------------------               
*                                                                               
* MY STORAGE AREA                                                               
*                                                                               
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
*                                                                               
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                 BROADCAST COMPRESSED START DATE              
BRDCEDAT DS    XL2                 BROADCAST COMPRESSED END DATE                
*                                                                               
MISCFLG1 DS    XL1                                                              
MF1KYCHG EQU   X'80'               A KEY FIELD WAS CHANGED                      
MF1NDSCR EQU   X'40'               HIT END OF LIST SCREEN                       
*                                                                               
FILTFLG1 DS    XL1                                                              
FF1PERD  EQU   X'80'               FILTER ON PERIOD                             
FF1RSPNS EQU   X'40'                         RESPONSE INVOICES                  
FF1MCT   EQU   X'20'                         MIDFLIGHT CLEARENCE TRKING         
FF1EASI  EQU   X'10'                         EASI SOURCE INVOICES               
FF1NRSPN EQU   X'08'                     NON-RESPONSE INVOICES                  
FF1NMCT  EQU   X'04'                     NON-MIDFLIGHT CLEARENCE TRKING         
FF1NEASI EQU   X'02'                     NON-EASI SOURCE INVOICES               
*                                                                               
FILTFLG2 DS    XL1                                                              
FF2INVDT EQU   X'80'               FILTER ON INVOICE DATE                       
FF2DUEDT EQU   X'40'                         DUE DATE                           
FF2ADV   EQU   X'20'                         ADVERTISER                         
FF2AGY   EQU   X'10'                         AGENCY                             
FF2SAL   EQU   X'08'                         SALESPERSON                        
*                                                                               
FILTIVDT DS    XL(L'SNVHDIDT)      FILTER INVOICE DATE                          
FILTDUDT DS    XL(L'SNVHDDDT)      FILTER DUE DATE                              
FILTADV  DS    XL(L'QADV)          FILTER ON ADVERTISER CODE                    
FILTAGY  DS    XL(L'QAGY)          FILTER ON AGENCY CODE                        
FILTSAL  DS    XL(L'QSAL)          FILTER ON SALESPERSON                        
*                                                                               
FILTINVC DS    CL(L'SNVKINV)              INVOICE                               
FILTINVL DS    XL1                        INVOICE LENGTH                        
FILTCONT DS    CL10                       CONTRACT- COMPLEMENT                  
FILTCONL DS    XL1                 CONTRACT LENGTH                              
FILTORD# DS    CL(L'SNVLSORD)      STATION ORDER #                              
FILTORDL DS    XL1                 STATION ORDER # LENGTH                       
FILTOFF  DS    CL2                        OFFICE                                
*                                                                               
PL16     DS    PL16                USED FOR AMOUNT TOTALLING                    
TAXAMNT  DS    PL8                 TAX IN INVOICE HEADER                        
LASTSTA  DS    CL5                                                              
*                                                                               
KEYSAVED DS    XL(L'KEY)           SAVED KEY                                    
KEYSTATS DS    XL(L'SNVDSTAT)            DIRECTORY STATUS                       
KEYDSKAD DS    XL(L'SNVDDA)              DISK ADDRESS                           
*                                                                               
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE                               
*                                                                               
KEYLINE  DS    XL(L'WORK)          KEYLINE FOR THE DETAIL SCREEN                
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
*                                                                               
SEQLIST  DS    XL((LSTLINES+1)*(LSEQNEXT-LSEQNTRY))                             
*                                                                               
LSTLINES EQU   ((LSTSELLH-LSTSEL1H)/(LINNEXTL-LINDSECT))+1                      
         EJECT                                                                  
*****                                                                           
* LIST LINE DSECT                                                               
*****                                                                           
LINDSECT DSECT                                                                  
LINSELH  DS    CL(L'LSTSEL1H)                                                   
LINSEL   DS    CL(L'LSTSEL1)                                                    
LINOFFH  DS    CL(L'LSTOFF1H)                                                   
LINOFF   DS    CL(L'LSTOFF1)                                                    
LINCONH  DS    CL(L'LSTCON1H)                                                   
LINCON   DS    CL(L'LSTCON1)                                                    
LINTYPEH DS    CL(L'LSTTYPEH)                                                   
LINTYPE  DS    CL(L'LSTTYPE)                                                    
LININVH  DS    CL(L'LSTINV1H)                                                   
LININV   DS    CL(L'LSTINV1)                                                    
LINADVH  DS    CL(L'LSTADV1H)                                                   
LINADV   DS    CL(L'LSTADV1)                                                    
LINAGYH  DS    CL(L'LSTAGY1H)                                                   
LINAGY   DS    CL(L'LSTAGY1)                                                    
LINSALH  DS    CL(L'LSTSAL1H)                                                   
LINSAL   DS    CL(L'LSTSAL1)                                                    
LINDATH  DS    CL(L'LSTDAT1H)                                                   
LINDAT   DS    CL(L'LSTDAT1)                                                    
LINTOTH  DS    CL(L'LSTTOT1H)                                                   
LINTOT   DS    CL(L'LSTTOT1)                                                    
LINNEXTL DS    0C                                                               
*****                                                                           
* LIST LINE SEQUENCE DSECT                                                      
*****                                                                           
LSEQNTRY DSECT                                                                  
LSEQPERD DS    XL(L'SNVKMOS)                                                    
LSEQINVC DS    CL(L'SNVKINV)                                                    
LSEQNEXT DS    0C                                                               
*****                                                                           
*                                                                               
*****                                                                           
ID#D     DSECT                                                                  
ID#      DS    XL10                                                             
IDTYPE   DS    XL1                                                              
LID      EQU   *-ID#                                                            
ID#COPY  DS    XL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012REREI01   11/19/07'                                      
         END                                                                    
