*          DATA SET RERIS06S   AT LEVEL 067 AS OF 05/01/02                      
*PHASE T80D06A,*                                                                
         TITLE 'RIS - T80D06 - READ AND FOUT PROD NAME LIST'                    
***********************************************************************         
*                                                                     *         
*        RERIS06 --- RIS HEADLINE AND DETAIL LINE BUILDER             *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* JAN22/97 (PXZ) --- CREATED TO HANDLE PRODUCT NAME (X'9D') READ   #  *         
*                    **  END TOMBSTONE  **                            *         
*                                                                     *         
* OCT24/97 (BU ) --- RERISWRKB --> RERISWRKC                          *         
*                    RGENEROL INCLUDED EXPLICITLY                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80D06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0D06*,RR=RE                                                  
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         LA    R7,2048(R9)                                                      
         LA    R7,2048(R7)                                                      
         USING T80D06,RB,R9,R7                                                  
                                                                                
         L     RC,0(R1)                                                         
         USING T80DFFD,RA                                                       
         USING GENOLD,RC                                                        
         ST    RE,RELO2            SAVE RELOCATION FACTOR                       
         XC    KEYSAVE,KEYSAVE                                                  
         ZAP   PGTOT,=P'0'         CLEAR PAGE TOTAL                             
         OI    LISTBYTE,LISTPNAM   =0 IF IN MIDDLE OF READ, SO NEED TO          
*                                  SET FOR RIS READ MODULE                      
                                                                                
         TM    VIEWS,X'80'         REQUEST TOTALS?                              
         BO    *+8                 NO                                           
         MVI   RISTATUS,1          YES/READ RECS THROUGH FOR TOTALS             
*                                  =1 IN MIDDLE OF READ FOR TOTALS              
*                                  =2 TOTAL READ DONE                           
*                                  =0 NO BOTTMOM PAGE REQUEST TOTALS            
                                                                                
         LA    R2,RISTITLH                                                      
         EJECT                                                                  
*                                                                               
         CLI   PRNT,1              ARE WE PRINTING REPORT?                      
         BNE   TT00                                                             
         BAS   RE,INITP            YES/INITIALIZE                               
         BAS   RE,DOHEADS             PRINT HEADLINES                           
         B     FL12                                                             
                                                                                
XIT      XIT1                                                                   
                                                                                
*                                  TEST NEXT OPTION THIS TIME                   
* TEST NEXT OPTION THIS TIME                                                    
TT00     CLI   NEXTBYTE,0                                                       
         BE    FOUTITLE                                                         
         B     FIRSTLIN                                                         
                                                                                
FOUTITLE LA    R1,RISTITLH         CLEAR SCREEN                                 
*                                                                               
FT00     ZIC   R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    FT10                                                             
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         FOUT  (R1)                                                             
**       LA    R1,9(R3,R1)         I DON'T THINK I NEED THIS                    
**       B     FT00                                                             
                                                                                
FT10     MVC   RISTITL(L'TITLE1),TITLE1                                         
         FOUT  RISTITLH                                                         
*                                                                               
FIRSTLIN DS    0H                                                               
         LA    R2,RISTTL2H         CLEAR 2ND TITLE LINE                         
         XC    RISTTL2,RISTTL2                                                  
         FOUT  (R2)                                                             
         LA    R2,RISOUTH           CLEAR SCREEN                                
         LR    R1,R2                                                            
         LA    R4,RISBOTMH                                                      
FL10B    ZIC   R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    FL12                                                             
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         FOUT  (R1)                                                             
         LA    R1,9(R3,R1)                                                      
         CR    R1,R4               IF LAST LINE                                 
         BE    FL12                LEAVE IT ALONE                               
         B     FL10B                                                            
                                                                                
FL12     LA    R2,RISOUTH                                                       
         USING PLPNMD,R2                                                        
         LA    RE,RECORD                                                        
         ST    RE,AIOAREA                                                       
                                                                                
* USE RIPSKIP+1 TO STORE REQUEST DATE - 1 YEAR                                  
         LA    R4,REPIOTBL                                                      
         USING REPIOD,R4                                                        
         GOTO1 VDATCON,DMCB,(2,RIPDATSS),(3,RIPSKIP+1)                          
         DROP  R4                                                               
                                                                                
         OC    ACOMBSV,ACOMBSV     SAVE START OF COMBO LIST                     
         BNZ   *+10                                                             
         MVC   ACOMBSV,ACMBOSTA                                                 
                                                                                
READREC  GOTO1 VREAD,DMCB,(RC)                                                  
         CLI   DMCB+1,X'FF'        ARE WE OVER MAXIOCTR ?                       
         BE    MAXOUT              YES                                          
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         CLI   DMCB,X'FF'          ARE WE AT END OF FILE?                       
         BNE   READ10              NO - CONTINUE                                
         CLI   CMBOREP,C'Y'        YES - IS THIS A COMBO STATION RUN?           
         BNE   LISTDONE            NO  - FINISHED                               
         L     RF,ACMBOSTA         YES - CHECK FOR NEXT STATION                 
         AR    RF,RA               RE-ADDRESS REGISTER                          
         LA    RF,5(RF)            A(NEXT STATION IN LIST)                      
         LR    RE,RF                                                            
         SR    RE,RA               DON'T STORE ADDRESS -                        
*                                     STORE DISPLACEMENT                        
         ST    RE,ACMBOSTA         STORE A(NEW STATION) BACK                    
         CLI   0(RF),0             END OF LIST?                                 
         BE    LISTDONE            YES - FINISHED                               
                                                                                
* SET UP REPIOBLK FOR COMBO STATION SREADS                                      
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         XC    RIPKEY,RIPKEY                                                    
         MVI   RIPKEY,X'9D'                                                     
         MVC   RIPKEY+1(2),RIPREP                                               
         MVC   RIPKEY+3(5),0(RF)                                                
         MVC   RIPSTA,0(RF)        NEXT STATION                                 
         MVI   RIPSTAT,RIPRDHIO    AND GO HIGH                                  
                                                                                
         OC    RIPKEYSV,RIPKEYSV   AND SAVE FIRST KEY                           
         BNZ   *+10                                                             
         MVC   RIPKEYSV,RIPKEY                                                  
         B     READREC                                                          
         DROP  R1                                                               
*                                                                               
READ10   EQU   *                                                                
                                                                                
*                                                                               
READ15   CLI   0(R2),0             TEST SCREEN FILLED                           
         BE    SETNEXT                                                          
         LA    R1,RISBOTMH         TEST LAST LINE                               
         CR    R1,R2                                                            
         BE    SETNEXT                                                          
*                                                                               
READ20   DS    0H                                                               
         B     GETDATA          LOAD PRINT LINE                                 
                                                                                
*                                                                               
* ALWAYS HAVE AT LEAST 2 LINES FOR DATA + TOTALS                                
* IF DOING REQ TOTALS AT PAGE BOTTOM LEAVE 3 LINES                              
* ASSUMES ALL OUT LINES ARE OF SAME LENGTH                                      
NEXTLINE DS    0H                                                               
         ZIC   R1,0(R2)            R1 -> LENGTH OF LINE                         
         AR    R2,R1               R2 -> NEXT SCREEN LINE                       
         LA    R3,RISBOTMH         R3 -> BOTTOM SCREEN LINE                     
         CR    R2,R3               HAVE WE HIT BOTTOM?                          
         BE    SETNEXT             YES                                          
         LR    RE,R2               NO - BUMP ONE MORE LINE                      
         AR    RE,R1                                                            
         CR    RE,R3               IS THERE ROOM FOR 2ND LINE?                  
         BE    SETNEXT             NO                                           
         CLI   RISTATUS,0          NEED ROOM FOR REQ TOTS AT BOTTOM ?           
         BER   R5                  NO - SO 2 LINES IS ALL WE NEED               
         AR    RE,R1               DO WE HAVE A 3D LINE?                        
         CR    RE,R3                                                            
         BE    SETNEXT             NO                                           
         BR    R5                  YES- WE'RE OK                                
         EJECT                                                                  
* SETS TOTALS AND PF INFO AT BOTTOM OF SCREEN FOR LIST REQUESTS                 
* RETURNS TO R5                                                                 
FOUTOTS  NTR1                                                                   
         XC    RISBOTN,RISBOTN     CLEAR UP JUNK AT BOTTOM                      
                                                                                
         CLI   BYTE,C'T'           TOTALS?                                      
         BNE   FOTT5               NO                                           
*                                                                               
         MVC   18(10,R2),=C'CONTRACTS='                                         
         EDIT  CONTOT,(8,28(R2)),ALIGN=LEFT                                     
         XC    CONTOT,CONTOT                                                    
*                                                                               
         MVC   DUB,STATOTAL                                                     
         DP    DUB,=P'100'                                                      
         EDIT  (P6,DUB),(14,PLPNPRI-6),FLOAT=-                                  
*                                                                               
         SR    R1,R1                                                            
         CP    PMKTOT,=PL8'0'      PRIOR MKT TOTAL ?                            
         BE    FOTT1                                                            
         ZAP   WORK(16),STATOTAL+8(8)   SHR $AMT INTO WORK                      
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),PMKTOT     PRIOR MKT TOTAL                              
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
                                                                                
FOTT1    DS    0H                                                               
         EDIT  (R1),PLPNPRSH,2,TRAIL=C'%'                                       
*                                                                               
         MVC   DUB,STATOTAL+16                                                  
         DP    DUB,=P'100'           DROP CENTS                                 
         EDIT  (P6,DUB),(11,PLPNCUR-3),FLOAT=-       CURRENT EST                
                                                                                
         SR    R1,R1                                                            
         CP    CMKTOT,=PL8'0'      CURRENT MKT TOTAL ?                          
         BE    FOTT2                                                            
         ZAP   WORK(16),STATOTAL+24(8)    SHR $ AMT INTO WORK                   
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),CMKTOT     CURRENT MKT TOTAL                            
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
                                                                                
FOTT2    DS    0H                                                               
         EDIT  (R1),PLPNCRSH,2,TRAIL=C'%'                                       
         FOUT  (R2)                                                             
                                                                                
* CLEAR STATOTAL AREA FOR NEXT POSSIBLE STATION                                 
         LA    R1,STATOTAL                                                      
         LA    RE,6                                                             
FOTT4    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   RE,FOTT4                                                         
                                                                                
*                                                                               
FOTT5    LA    R2,RISBOTM                                                       
         XC    RISBOTM,RISBOTM                                                  
         MVC   0(L'PFTIT1,R2),PFTIT1    SET PF INFO                             
         CLI   RISTATUS,2            ,,BOTTOM OF PAGE REQUEST TOTALS ?          
         BNE   FOUTX                 ,,NO                                       
                                                                                
                                                                                
*                                                                               
* DISPLAY REQUEST TOTALS AT BOTTM OF PAGE                                       
* POINT R2 AT NEXT TO LAST LINE ON SCREEN                                       
*                                                                               
         LA    R2,RISOUTH          FIRST LINE                                   
         LA    R3,RISBOTMH         LAST LINE                                    
         LR    RE,R2                                                            
FOTT6    ZIC   R1,0(R2)                                                         
         AR    RE,R1                                                            
         CR    R3,RE             IS NEXT LINE BOTTOM?                           
         BE    FOTT6B            YES-SO R2 POINTS TO NEXT-TO-BOTMLINE           
         LR    R2,RE             NO - POINT R2 TO NEXT LINE                     
         B     FOTT6                  AND GO BUMP DOWN                          
*                                                                               
FOTT6B   MVC   18(10,R2),=C'CONTRACTS='                                         
         EDIT  CONTOTBP,(8,28(R2)),ALIGN=LEFT                                   
*                                                                               
         MVC   DUB,STATOTBP                                                     
         DP    DUB,=P'100'                                                      
         EDIT  (P6,DUB),(14,PLPNPRI-6),FLOAT=-                                  
*                                                                               
         SR    R1,R1                                                            
         CP    PMKTOTBP,=PL8'0'      PRIOR MKT TOTAL ?                          
         BE    FOTT7                                                            
         ZAP   WORK(16),STATOTBP+8(8)   SHR $AMT INTO WORK                      
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),PMKTOTBP   PRIOR MKT TOTAL                              
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
                                                                                
FOTT7    DS    0H                                                               
         EDIT  (R1),PLPNPRSH,2,TRAIL=C'%'                                       
*                                                                               
         MVC   DUB,STATOTBP+16                                                  
         DP    DUB,=P'100'           DROP CENTS                                 
         EDIT  (P6,DUB),(11,PLPNCUR-3),FLOAT=-        CURRENT EST               
                                                                                
         SR    R1,R1                                                            
         CP    CMKTOTBP,=PL8'0'      CURRENT MKT TOTAL ?                        
         BE    FOTT9                                                            
         ZAP   WORK(16),STATOTBP+24(8)    SHR $ AMT INTO WORK                   
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),CMKTOTBP   CURRENT MKT TOTAL                            
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
                                                                                
FOTT9    DS    0H                                                               
         EDIT  (R1),PLPNCRSH,2,TRAIL=C'%'                                       
         FOUT  (R2)                                                             
         B     FOUTX               THAT'S ALL FOR NOW                           
                                                                                
FOUTX    LA    R2,RISBOTMH                                                      
         FOUT  (R2)                                                             
         LA    R2,RISBOTNH                                                      
         FOUT  (R2)                                                             
         XIT1                                                                   
         EJECT                                                                  
* CONTRACT REC IN IOAREA                                                        
* RIPSKIP+1 HAS COMPRESSED REQUEST START DATE - 1 YEAR                          
*                                                                               
GETDATA  LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
         ST    RE,SAVERE                                                        
*******************************************                                     
****     CLC   =X'03155096',RCONKCON                                            
****8    BNE   READREC                                                          
********************************************                                    
         LA    R3,REPIOTBL                                                      
         USING REPIOD,R3                                                        
* CHECK YEAR/MONTH OF CONTRACT DATE VS EXPANDED REQUEST DATES                   
* EXPANDED REQ DATE IS SPECIFIC REQ START - 1 YEAR (TO GET PRIOR $)             
         CLC   RCONDATE+3(3),RIPSKIP+1   CON END DAT < REQ DAT-1YEAR ?          
         BL    READREC                   YES/GET NEXT RECORD                    
         CLC   RCONDATE(3),TBLEND        CON STRT DAT > REQ END ?               
         BH    READREC                   YES/GET NEXT RECORD                    
                                                                                
* CHECK IF CONTRACT DATE FITS SPECIFIC REQUEST DATES                            
*                                                                               
         CLC   RCONDATE+3(3),TBLBGN     CON END DATE < REQ START DATE?          
         BNL   DATA8                      NO-CONTINUE                           
                                                                                
* CONTRACT END DATE BEFORE REQ START DATE                                       
         BAS   RE,LSTDLR                  DOES IT HAVE PRIOR $ ?                
         OC    WORK(8),WORK                                                     
         BZ    READREC                    NO/GET NEXT RECORD                    
         DROP  R3                                                               
*                                                                               
DATA8    SR    RE,RE               CLEAR SCREEN LINE                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,XCFLD                                                         
                                                                                
*                                                                               
* CHECK DOLLAR FILTER                                                           
DATA20   DS    0H                                                               
         CLI   DLRS,0              ARE WE FILTERING DOLLARS?                    
         BE    DATA23              NO                                           
         BAS   RE,LSTDLR           RETURNS $ IN FULL AND R0                     
                                                                                
* SHOW 0 DOLLARS ONLY / EXCLUDE 0 DOLLARS?                                      
         CLI   DLRS,C'0'              SHOW 0 DOLLARS ONLY                       
         BNE   DATA22                                                           
         OC    WORK(16),WORK                                                    
         BNZ   READREC                                                          
DATA22   CLI   DLRS,C'-'               EXCLUDE 0 DOLLARS ?                      
         BNE   DATA23                  NO                                       
         OC    WORK(16),WORK           YES/ $ RETURNED IN WORK                  
         BZ    READREC                                                          
                                                                                
         EJECT                                                                  
*                                                                               
* USE MYP2 TO BUILD KEY AND $ OF CONTRACT RECORD                                
         DROP  R4                                                               
DATA23   EQU   *                                                                
         CLI   MYP2,0                   FIRST TIME ?                            
         BNE   DATA30                   NO                                      
                                                                                
DATA24   MVC   MYP2(5),RCONKSTA         YES                                     
         MVC   MYP2+5(4),RCONKADV                                               
         BAS   RE,GETPRDNM                                                      
         MVC   MYP2+9(20),WORK                                                  
         MVC   MYP2+29(6),RCONKAGY                                              
         MVC   MYP2+35(4),RCONKCON                                              
* IS IT COMBO STATION                                                           
         MVI   MYP2+90,0                                                        
         LA    R4,RCONELEM                                                      
DATA24B  ZIC   R1,1(R4)                                                         
         CLI   1(R4),0                                                          
         BE    DATA25                                                           
         AR    R4,R1                                                            
         CLI   0(R4),X'17'         COMBO ELEM?                                  
         BNE   DATA24B                                                          
         MVI   MYP2+90,C'C'        SET COMBO INDICATOR                          
                                                                                
DATA25   DS    0H                                                               
                                                                                
* LSTDLR RETURNS  $ IN WORK                                                     
* CL16 OF 4CL EACH - PRIOR INV/PRIOR BKD/CURRENT BEST/CURRENT BKD               
* CL16 OF PL8 EACH  - PRIOR MKT $$ / CURRENT MKT $$                             
         BAS   RE,LSTDLR                                                        
                                                                                
         MVC   MYP2+40(32),WORK         SET $$$ TO MYP2                         
         B     READREC                                                          
                                                                                
*                                                                               
* CHECK NEW CONTRACT REC AGAINST PREVIOUS IN MYP2                               
DATA30   DS    0H                                                               
         MVI   BYTE,0                                                           
*                                                                               
         CLC   MYP2(5),RCONKSTA         STATION MATCH ?                         
         BE    *+12                     YES                                     
         MVI   BYTE,C'S'                NO - SET STATION CHANGED FLAG           
         B     DATA40                                                           
*                                                                               
         TM    VIEWS3,X'80'            SHOW EACH CON # ? (LISTPX)               
         BO    DATA40                  YES                                      
                                                                                
         CLC   MYP2+5(4),RCONKADV       ADVERTISER MATCH ?                      
         BNE   DATA40                                                           
         BAS   RE,GETPRDNM                                                      
         CLC   MYP2+9(20),WORK          PRODUCT NAME ?                          
         BNE   DATA40                                                           
         CLC   MYP2+29(6),RCONKAGY      AGENCY ?                                
         BNE   DATA40                                                           
                                                                                
* STATION/ADVERTISER/PRODUCT NAME/AGENCY ARE THE SAME                           
* ADD $ TO MYP2                                                                 
                                                                                
         CLI   MYP2+39,C'*'        IS IT MULTIPLE CON#S?                        
         BNE   DATA33                                                           
         ICM   R1,15,MYP2+35       YES/BUMP NUMBER                              
         LA    R1,1(R1)                                                         
         STCM  R1,15,MYP2+35                                                    
         B     DATA34                                                           
DATA33   XC    MYP2+35(4),MYP2+35  CLEAR CON # FIELD                            
         MVI   MYP2+39,C'*'        SET MULTIPLE CON#S INDICATOR                 
         MVI   MYP2+38,2           AND SET IN 2 CONTRACTS                       
                                                                                
DATA34   BAS   RE,LSTDLR           RETURNS $ WORK                               
         LA    RE,WORK                                                          
         LA    R0,4                                                             
         LA    RF,MYP2+40                                                       
DATA35   MVC   DUB(4),0(RE)        LOOPS TO ADD 4 $BUCKETS TO MYP2              
         MVC   FULL,0(RF)                                                       
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         STCM  R1,15,0(RF)                                                      
         LA    RE,4(RE)            BUMP BUCKETS                                 
         LA    RF,4(RF)                                                         
         BCT   R0,DATA35                                                        
         AP    MYP2+56(8),WORK+16(8)  ADD PRIOR MKT TOTAL $$                    
         AP    MYP2+64(8),WORK+24(8)  ADD CURRENT MKT TOTAL $$                  
         B     READREC                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* CONTRACTS DON'T MATCH PRINT DATA IN MYP2                                      
* THEN PUT NEW CONTRACT INTO MYP2                                               
DATA40   DS    0H                                                               
                                                                                
         BAS   RE,PUTSCRN          WRITE MYP2 TO SCREEN                         
         XC    MYP2,MYP2                                                        
*                                                                               
         CLI   RISTATUS,1          ,,IN MIDDLE OF TOTALS READ  ?                
         BE    DATA24              ,,DON'T BUMP SCREEN LINE                     
*                                  ,,JUST DEAL WITH NEXT REC                    
*                                                                               
         BAS   R5,NEXTLINE         BUMP OUTLINE                                 
         CLI   BYTE,C'S'           STATION CHANGED?                             
         BNE   DATA24              NO-PUT CONTRACT DATA TO MYP2                 
         MVI   BYTE,C'T'           YES/GIVE TOTALS                              
         BAS   RE,FOUTOTS                                                       
         BAS   R5,NEXTLINE                                                      
         BAS   R5,NEXTLINE                                                      
         B     DATA24              GO PUT CONTRACT DATA TO MYP2                 
                                                                                
                                                                                
                                                                                
                                                                                
*  WRITE DATA IN MYP2 TO SCREEN                                                 
PUTSCRN  NTR1                                                                   
         MVC   PLPNSTA,MYP2        STATION                                      
         MVC   PLPNADV,MYP2+5      ADVERTISER                                   
         MVC   PLPNPRD,MYP2+9      PRODUCT NAME                                 
         MVC   PLPNAGY(6),MYP2+29     AGENCY                                    
                                                                                
         CLI   MYP2+39,C'*'        ARE THERE MULTIPLE CON#S ?                   
         BNE   PTS8                                                             
         EDIT  (B4,MYP2+35),(7,PLPNCON),TRAIL=C'*'                              
         B     PTS10                                                            
                                                                                
PTS8     UNPK  WORK(9),MYP2+35(5)  NO/UNPACK CONTRACT NUMBER                    
         MVC   PLPNCON,WORK                                                     
**       LA    R1,PLPNCON          DROP LEADING ZEROS                           
**       CLI   0(R1),C'0'                                                       
**       BNE   *+16                                                             
**       MVI   0(R1),C' '                                                       
**       LA    R1,1(R1)                                                         
**       B     *-16                                                             
                                                                                
         CLI   MYP2+90,C'C'                COMBO?                               
         BNE   PTS10                                                            
         MVI   PLPNCON+(L'PLPNCON),X'83'   YES/PUT LOWER CASE 'C'               
*                                                                               
PTS10    DS    0H                   PRIOR ACTUAL                                
         ICM   R0,15,MYP2+40                                                    
         SRDA  R0,32                                                            
         D     R0,=F'100'           DROP CENTS                                  
         EDIT  (R1),PLPNPRI,FLOAT=-                                             
*                                                                               
         SR    R1,R1                 PRIOR SHR                                  
         CP    MYP2+56(8),=PL8'0'    PRIOR MKT $$                               
         BE    PTS11                                                            
         ICM   RE,15,MYP2+44         $ FOR THIS STATION                         
         CVD   RE,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),MYP2+56(8)                                              
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
                                                                                
PTS11    DS    0H                                                               
         EDIT  (R1),PLPNPRSH,2,TRAIL=C'%'                                       
*                                                                               
         SR    R0,R0                 CURRENT EST                                
         ICM   R0,15,MYP2+48                                                    
         SRDA  R0,32                                                            
         D     R0,=F'100'            DROP CENTS                                 
         EDIT  (R1),PLPNCUR,FLOAT=-            CURRENT EST                      
*                                                                               
         SR    R1,R1                 CURRENT SHR                                
         CP    MYP2+64(8),=PL8'0'    CURRENT MKT $$                             
         BE    PTS12                                                            
         ICM   RE,15,MYP2+52         $ FOR THIS STATION                         
         CVD   RE,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),MYP2+64(8)                                              
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
                                                                                
PTS12    DS    0H                                                               
         EDIT  (R1),PLPNCRSH,2,TRAIL=C'%'                                       
                                                                                
*                                                                               
         FOUT  (R2)                                                             
                                                                                
* ADD TO TOTALS - USE 6 PL8 FIELDS STARTING WITH STATOTAL                       
* USE CONTOT (PL4) TO  STORE # OF CONTRACTS                                     
         L     R1,CONTOT           ADD TO CONTRACT NUMBER TOTAL                 
         CLI   MYP2+39,C'*'        MULTIPLE CONTRACTS PER LINE ?                
         BE    PTS15                                                            
         LA    R1,1(R1)            NO - BUMP CONTRACT COUNT                     
         ST    R1,CONTOT                                                        
         B     PTS20                                                            
PTS15    MVC   FULL,MYP2+35        YES/ADD NUMBER OF CONTRACTS                  
         A     R1,FULL                                                          
         ST    R1,CONTOT                                                        
                                                                                
PTS20    LA    R3,4                4 $ FIELDS                                   
         LA    R1,MYP2+40          R1 -> CONTRACT $ TOTS                        
         LA    RF,STATOTAL         RF -> STATION $ ACCUMS                       
PTS23    ICM   RE,15,0(R1)                                                      
         CVD   RE,DUB                                                           
         AP    0(8,RF),DUB                                                      
         LA    R1,4(R1)            BUMP CONTRACT BINARY TOTS                    
         LA    RF,8(RF)            BUMP STATION PACKED CUMS                     
         BCT   R3,PTS23                                                         
         AP    0(8,RF),MYP2+56(8)  PRIOR MKT $$                                 
         AP    8(8,RF),MYP2+64(8)  CURRENT MKT $$                               
*                                                                               
         XIT1                                                                   
                                                                                
         EJECT                                                                  
* PUT DATA TO PRINT LINE                                                        
PRNTDATA DS    0H                                                               
*                                                                               
         EJECT                                                                  
* - PRODUCT NAME FOR LISTD OPTION   - R4 POINTS TO 01 ELEM                      
GETPRDNM NTR1                                                                   
         CLC   RCONPRD,=3X'40'     PROD=SPACES ?                                
         BE    GT05                                                             
         XC    WORK(15),WORK       NO/PASS CODE                                 
         MVC   WORK(3),=C'PC='                                                  
         MVC   WORK+3(3),RCONPRD                                                
         B     XIT                                                              
GT05     ZIC   R1,1(R4)            YES/05 ELEMENT HAS PROD NAME                 
         LTR   R1,R1                                                            
         BZ    GTNOPRD                                                          
         AR    R4,R1                                                            
         CLI   0(R4),X'05'                                                      
         BNE   GT05                                                             
         LR    R1,R4                                                            
         USING RCONEXEL,R1                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(L'RCONEXPR),RCONEXPR                                        
         B     XIT                                                              
         DROP  R1                                                               
GTNOPRD  MVC   WORK(15),=CL15'NOT FOUND'                                        
         B     XIT                                                              
                                                                                
*                                                                               
                                                                                
         EJECT                                                                  
* WORK(4)  = PRIOR BEST                                                         
* WORK+4   = PRIOR BOOKED   (X'03' ELEM)                                        
* WORK+8   = CURRENT BEST   (X'04' IF NO $ , THEN USE X'03')                    
* WORK+12  = CURRENT BOOKED (X'03'ELEM)                                         
* WORK+16  = PL8 FOR PRIOR MKT $$                                               
* WORK+24  = PL8 FOR CURRENT MKT $$                                             
*                                                                               
* GRID HAS DATE TABLE FOR BEST $                                                
* CL12 - (CL2 DATE, CL2 SPARE) CL4 BOOKED $, CL4 INVOICE $                      
*                                                                               
                                                                                
* GRID BUILT IN RERIS00                                                         
LSTDLR   NTR1                                                                   
         LA    RE,GRID             CLEAR $ FROM GRID                            
         XC    4(8,RE),4(RE)                                                    
         LA    RE,12(RE)                                                        
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   *-14                                                             
                                                                                
         LA    RE,GRID2            CLEAR $ FROM GRID2                           
         XC    4(8,RE),4(RE)                                                    
         LA    RE,12(RE)                                                        
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   *-14                                                             
*                                                                               
         XC    WORK,WORK           CLEAR RETURN AREA                            
         ZAP   WORK+16(8),=P'0'    SET UP FOR TOTAL STATION DOLLARS             
         ZAP   WORK+24(8),=P'0'    SET UP FOR TOTAL STATION DOLLARS             
                                                                                
* - SET UP PRIOR START/END                                                      
         MVC   HALF,TBLBGN                                                      
         ZIC   R1,HALF                                                          
         BCTR  R1,0                                                             
         STC   R1,HALF                                                          
         MVC   HALF2,TBLEND                                                     
         ZIC   R1,HALF2                                                         
         BCTR  R1,0                                                             
         STC   R1,HALF2                                                         
                                                                                
* DEAL WITH X'03'ELEMENT HERE                                                   
         LA    R8,RCONREC                                                       
         USING RCONBKEL,R8                                                      
* FIRST CHECK IF CONTRACT HAS SPL X'06' ELEMENT                                 
         MVI   BYTE2,0           BYTE2=0=NO SPL                                 
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   BYTE2,6           BYTE2=6=SPL                                    
*                                                                               
         LA    R8,RCONREC          BUT IF RADIO                                 
         MVI   ELCODE,X'10'        MUST HAVE BOP ELEM                           
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   BYTE2,6             FUDGE SPL FLAG                               
*                                                                               
         LA    R8,RCONREC                                                       
         MVC   ELCODE,ESTBUCKT                                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LSTDLR2  BAS   RE,NEXTEL                                                        
         BNE   LSTDLR5                                                          
* SET INTO GRID FOR BEST $                                                      
         LA    R1,GRID                                                          
LSTDLR2A CLC   RCONBKYR(2),0(R1)      MATCH ON DATE?                            
         BE    LSTDLR2B            YES                                          
         LA    R1,12(R1)           BUMP TABLE                                   
         CLI   0(R1),X'FF'         EOF?                                         
         BE    LSTDLR2C            YES-CHECK PRIOR                              
         B     LSTDLR2A                                                         
LSTDLR2B MVC   DUB(4),RCONBKAM                                                  
         MVC   FULL,4(R1)          BOOKED DISPLACEMENT                          
         L     RE,DUB                                                           
         A     RE,FULL                                                          
         STCM  RE,15,4(R1)         BEST $                                       
*                                                                               
         CLI   BYTE2,6             IF NO SPL ELEMENT                            
         BNE   LSTDLR2             DON'T INCLUDE IN SHR CALCULATION             
         L     RE,DUB              YES/SPL/ADD                                  
         MVC   FULL,WORK+12        CURRENT BOOKED                               
         A     RE,FULL                                                          
         STCM  RE,15,WORK+12                                                    
         B     LSTDLR2                                                          
*                                                                               
* PRIOR BOOKED                                                                  
LSTDLR2C CLI   BYTE2,6              IF NO SPL ELEMENT                           
         BNE   LSTDLR2              DON'T INCLUDE IN SHR CALCULATION            
                                                                                
         CLC   RCONBKYR(2),HALF       DOES IT FIT PRIOR?                        
         BL    LSTDLR2                                                          
         CLC   RCONBKYR(2),HALF2                                                
         BH    LSTDLR2                                                          
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         MVC   FULL,WORK+4         PRIOR BOOKED                                 
         A     R1,FULL                                                          
         STCM  R1,15,WORK+4                                                     
                                                                                
*                                                                               
         LA    R1,GRID2            ADD TO GRID2 FOR PRIOR BEST                  
LSTDLR3  CLC   RCONBKYR(2),0(R1)      MATCH ON DATE?                            
         BE    LSTDLR3C                                                         
         CLI   0(R1),X'FF'         EOF?                                         
         BE    LSTDLR2             GET NEXT ELEM                                
         LA    R1,12(R1)                                                        
         B     LSTDLR3                                                          
LSTDLR3C MVC   DUB(4),RCONBKAM                                                  
         MVC   FULL,4(R1)                                                       
         L     RE,DUB                                                           
         A     RE,FULL                                                          
         STCM  RE,15,4(R1)                                                      
         B     LSTDLR2             GET NEXT ELEM                                
                                                                                
                                                                                
         EJECT                                                                  
                                                                                
* DEAL WITH X'04' ELEMENT HERE                                                  
LSTDLR5  MVC   ELCODE,INVBUCKT                                                  
         LA    R8,RCONREC                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LSTDLR7  BAS   RE,NEXTEL                                                        
         BNE   LSTDLR10                                                         
         CLC   RCONBKYR(2),HALF    CHECK DATE AGAINST PRIOR                     
         BL    LSTDLR7                                                          
         CLC   RCONBKYR(2),HALF2                                                
         BH    LSTDLR8                                                          
* SET PRIOR INVOICE INTO GRID2 FOR BEST $                                       
         LA    R1,GRID2                                                         
LSTDLR7B CLC   RCONBKYR(2),0(R1)                                                
         BE    LSTDLR7C                                                         
         CLI   0(R1),X'FF'         EOF?                                         
         BE    LSTDLR7                                                          
         LA    R1,12(R1)                                                        
         B     LSTDLR7B                                                         
LSTDLR7C MVC   DUB(4),RCONBKAM                                                  
         MVC   FULL,8(R1)         PRIOR INVOICE (PRIOR ACTUAL)                  
**************************************                                          
         OC    DUB(4),DUB          IF $0 INVOICE ELEMENT                        
         BNZ   LSTDLR7E                                                         
         OC    FULL,FULL           AND NO $ FROM PREVIOUS ELEM                  
         BZ    LSTDLR7D                                                         
         CLC   FULL,=X'FFFFFFFF'    IS IT 0$ FLG ?                              
         BNE   LSTDLR7E             NO                                          
LSTDLR7D MVC   8(4,R1),=X'FFFFFFFF' SET $0 INVOICE FLAG                         
         B     LSTDLR7F                                                         
****************************************                                        
LSTDLR7E L     RE,DUB                                                           
         A     RE,FULL                                                          
         STCM  RE,15,8(R1)                                                      
LSTDLR7F B     LSTDLR7                                                          
*                                                                               
                                                                                
* SET CURRENT INVOICE INTO GRID FOR BEST $                                      
LSTDLR8  LA    R1,GRID                                                          
*                                                                               
LSTDLR8A CLC   RCONBKYR(2),0(R1)      MATCH ON DATE?                            
         BE    LSTDLR9                                                          
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    LSTDLR7             YES/GET NEXT RECORD                          
         LA    R1,12(R1)                                                        
         B     LSTDLR8A                                                         
LSTDLR9  MVC   DUB(4),RCONBKAM                                                  
         MVC   FULL,8(R1)                                                       
**************************************                                          
         OC    DUB(4),DUB          IF $0 INVOICE ELEMENT                        
         BNZ   LSTDLR9E                                                         
         OC    FULL,FULL           AND NO $ FROM PREVIOUS ELEM                  
         BZ    LSTDLR9D                                                         
         CLC   FULL,=X'FFFFFFFF'    IS IT 0$ FLG ?                              
         BNE   LSTDLR9E             NO                                          
LSTDLR9D MVC   8(4,R1),=X'FFFFFFFF' SET $0 INVOICE FLAG                         
         B     LSTDLR9F                                                         
****************************************                                        
LSTDLR9E L     RE,DUB                                                           
         A     RE,FULL                                                          
         STCM  RE,15,8(R1)                                                      
LSTDLR9F B     LSTDLR7                                                          
         DROP  R8                                                               
*                                                                               
         EJECT                                                                  
* GO THROUGH GRID AND SET BEST $ TO WORK+8                                      
LSTDLR10 DS    0H                                                               
         LA    R1,GRID                                                          
         LA    R3,WORK+8           CURRENT BEST BUCKET                          
         BAS   RE,DOBEST                                                        
         LA    R1,GRID2                                                         
         LA    R3,WORK             PRIOR BEST BUCKET                            
         BAS   RE,DOBEST                                                        
*                                                                               
                                                                                
LSTDLR15 DS    0H                                                               
         BAS   RE,CALCSHR          GET SHR $ AMT                                
                                                                                
         LA    R8,RCONREC          IF RADIO                                     
         MVI   ELCODE,X'10'        MUST HAVE BOP ELEM                           
         BAS   RE,GETEL                                                         
         BE    LSTDLRX             NO                                           
                                                                                
         CLC   =PL8'0',WORK+24     IF NO MARKET $                               
         BNE   *+10                                                             
         XC    WORK+12(4),WORK+12  CLEAR CURRENT BOOKED $                       
*                                                                               
         CLC   =PL8'0',WORK+16     IF NO PRIOR MARKET $                         
         BNE   *+10                                                             
         XC    WORK+4(4),WORK+4    CLEAR PRIOR BOOKED $                         
LSTDLRX  XIT1                                                                   
*                                                                               
                                                                                
* SETS BEST DOLLARS                                                             
DOBEST   NTR1                                                                   
*                                                                               
LSTDLR12 MVC   DUB(4),8(R1)        INVOICE $                                    
         CLC   DUB(4),=X'FFFFFFFF' $0 INVOICE?                                  
         BNE   LSTDL12A                                                         
         XC    DUB(4),DUB          YES/SO USE 0$                                
         B     LSTDLR13                                                         
LSTDL12A OC    DUB(4),DUB          ARE THERE INVOICE $ ?                        
         BNZ   LSTDLR13            YES                                          
                                                                                
* NO INVOICE $, IS REP PROFILE SET TO USE 0 INVOICE $                           
*               INSTEAD OF BOOKED IN CALCULATING BEST $ ?                       
         CLI   CFLAG,C'Y'               CHECK FLAG                              
         BNE   LSTDL12B                 NO                                      
         BAS   RE,INVSTAT               YES - CHECK STATION CLOSE DATE          
         BNH   LSTDLR13                       BUCKET DATE > CLOSE DAT?          
                                                                                
LSTDL12B MVC   DUB(4),4(R1)            SET BOOKED $                             
                                                                                
LSTDLR13 MVC   FULL,0(R3)          BEST DOLLARS BUCKET                          
         L     RE,DUB                                                           
         A     RE,FULL                                                          
         STCM  RE,15,0(R3)                                                      
         LA    R1,12(R1)           BUMP GRID TABLE                              
         CLI   0(R1),X'FF'         END OF FILE?                                 
         BNE   LSTDLR12            NO                                           
         XIT1                      YES                                          
         EJECT                                                                  
**************************************************************                  
* IF FLAG ON IN REPPROF + 11                                                    
* AND IF STATION CLOSED OUT                                                     
* AND IF NO INVOICE $                                                           
* THEN USE 0$ FOR BEST $ FIGURE                                                 
*                                                                               
* R1 POINTS TO BUCKETS IN GRID                                                  
* (CL2-DATE, CL2-SPARE, CL4-BOOKED$, CL4-INVOICE$)                              
*                                                                               
INVSTAT  NTR1                                                                   
         CLC   CSTATION,RCONKSTA         DO WE ALREADY HAVE STATION?            
         BE    INVS10                    YES                                    
         XC    KEY,KEY                   NO - READ FOR STATION                  
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         MVC   FULL,AIOAREA        SAVE I/O AREA                                
         LA    RE,TEMPIO                                                        
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         L     R5,AIOAREA                                                       
         MVC   CDATE,RSTACLDT      STATION CLOSING DATE                         
         MVC   CSTATION,RCONKSTA                                                
*                                                                               
         MVC   AIOAREA,FULL        RESTORE AIO AREA                             
         LA    R3,REPIOTBL                                                      
         USING REPIOD,R3                                                        
         OI    RIPSTAT,RIPRDHI     READ HI BEFORE SEQ                           
*                                                                               
INVS10   CLC   0(2,R1),CDATE       BUCKET DATE VS STATION CLOSE DATE            
*                                  IF NOT >, USE INVOICE 0 $                    
INVSX    B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
SETNEXT  DS    0H                                                               
         MVI   NEXTBYTE,2          2=X'9D' KEY READ                             
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         MVI   RIPSTAT,RIPRDHIO    TELL REPIO TO RETURN SAME REC                
         MVI   BYTE,0                                                           
         BAS   RE,FOUTOTS                                                       
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(L'MSG1),MSG1                                             
         LA    R2,RISMESSH                                                      
         FOUT  (R2)                                                             
SETNXTX  B     EXIT                                                             
         SPACE 2                                                                
LISTDONE CLI   PRNT,1              ARE WE PRINTING REPORT                       
         BNE   LSTD10                                                           
         TM    VIEWS,X'80'         ARE WE PRINTING TOTALS ?                     
         BO    *+8                 NO                                           
         BAS   RE,TOTPRINT         YES-TOTALS                                   
         BAS   RE,LASTP            LAST TIME ROUTINES                           
         B     EXIT                                                             
                                                                                
LSTD10   CLI   RISTATUS,1          READING FOR REQUEST TOTALS ?                 
         BNE   LSTD10D             NO                                           
*****************************************************************               
         MVI   RISTATUS,2          YES/SET FINISHED TOTALS READ                 
         OI    VIEWS,X'80'             SET ON 'NO TOTALS'                       
         CLI   MYP2,0                  ANYTHING TO ADD TO TOTALS?               
         BE    *+8                     NO                                       
         BAS   RE,PUTSCRN              YES                                      
         XC    MYP2,MYP2                                                        
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         XC    RIPKEY,RIPKEY           TELL I/O READER START ANEW               
         MVI   RIPSTAT,0                                                        
         BAS   RE,REQTOTS              SAVE BOTTOM PG TOTS/CLEAR STATOT         
*                                                                               
         CLI   CMBOREP,C'Y'        IF COMBO RUN                                 
         BNE   LSTD10B                                                          
         MVC   ACMBOSTA,ACOMBSV    RESET START OF COMBO LIST                    
         MVC   RIPKEY,RIPKEYSV     RESET FIRST KEY                              
         OI    RIPSTAT,RIPRDHIO                                                 
*                                           CLEAR STATOTAL                      
LSTD10B  B     READREC                 START READ AGAIN                         
         DROP  R1                                                               
********************************************************************            
*                                                                               
LSTD10D  MVI   NEXTBYTE,0                                                       
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(L'MSG2),MSG2                                             
         FOUT  RISMESSH                                                         
         CLI   MYP2,0              ANYTHING LEFT TO PUT TO SCREEN ?             
         BE    LSTD12              NO                                           
         BAS   RE,PUTSCRN          YES - PUT MYP2 TO SCREEN                     
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
LSTD12   XC    MYP2,MYP2                 CLEAR MYP2                             
         MVI   BYTE,C'T'                                                        
         BAS   RE,FOUTOTS                                                       
*                                                                               
SKIPCNT  LA    R2,RISSTAH                                                       
*                                  SET ANY LIST FIELD TO INVALID                
         NI    RISSTAH+4,X'DF'                                                  
         NI    RISOFFH+4,X'DF'                                                  
         NI    RISAGYH+4,X'DF'                                                  
         NI    RISADVH+4,X'DF'                                                  
         NI    RISSLSH+4,X'DF'                                                  
         NI    RISCTGH+4,X'DF'                                                  
         NI    RISFILTH+4,X'DF'                                                 
         NI    RISDATEH+4,X'DF'                                                 
*                                                                               
         MVI   LISTBYTE,0          CLEAR BYTE                                   
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
ORFLD    OC    8(0,R2),8(R2)                                                    
*                                                                               
XCFLD    XC    8(0,R2),8(R2)                                                    
*                                                                               
         SPACE 3                                                                
         GETEL R8,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*                                                                               
* (STATION $) / (STATION SHR PCT) = MARKET $                                    
*                                                                               
* SAVE WORK IN MYP                                                              
*                                                                               
*                                                                               
CALCSHR  NTR1                                                                   
         MVC   MYP(16),WORK                                                     
         ZAP   MYP+16(8),=P'0'     PREPARE FOR PRIOR MKT $                      
         ZAP   MYP+24(8),=P'0'     PREPARE FOR CURRENT MKT$                     
*                                                                               
         MVC   FULL,WORK+12        CURRENT BOOKED $                             
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVI   BYTE,C'C'           TELL DOCALC IT'S CURRENT $                   
         BAS   RE,DOCALC                                                        
                                                                                
         MVC   FULL,WORK+4         PRIOR BOOKED $                               
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVI   BYTE,C'P'           TELL DOCALC IT'S PRIOR $                     
         BAS   RE,DOCALC                                                        
         B     XIT                                                              
                                                                                
*                                                                               
*  CALLED BY CALCSHR - CURRENT OR PRIOR $ PASSED IN DUB                         
*                                                                               
DOCALC   NTR1                                                                   
         L     R5,AIOAREA                                                       
         USING RCONREC,R5                                                       
         LA    R5,RCONELEM                                                      
CLC10    ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    CLCX                                                             
         AR    R5,R1                                                            
         CLI   0(R5),X'06'         IF NO X'06' ELEM - EXIT                      
         BH    CLCX                                                             
         BNE   CLC10                                                            
                                                                                
         USING RCONSPEL,R5                                                      
         TM    RCONSPES,X'04'      ,,IS IT PERCENTAGE                           
         BNO   CLCX                ,,IF NOT, SKIP IT                            
         ZAP   WORK(16),=P'0'                                                   
         ICM   R1,15,RCONSPAM      .GET PERCENT                                 
         LTR   R1,R1               .IF ZERO                                     
         BZ    CLC20               ..CHECK FOR LOSS                             
                                                                                
         CVB   RF,DUB              CHECK FOR NEGATIVE (CREDIT) $$               
         LTR   RF,RF               NEGATIVE?                                    
         BM    CLCX                YES - DON'T ADD ANYTHING                     
                                                                                
         AP    WORK(16),DUB        GET CONTRACT $ INTO WORK                     
         CVD   R1,DUB              PCT GOES INTO DUB                            
         MP    WORK(16),=P'20000'  MULT BY 10,000 FOR DECL ALIGN                
*                                     AND 2 FOR ROUNDING                        
         AP    WORK(16),DUB        ADD SHR PCT FOR ROUNDING                     
         DP    WORK(16),DUB        ($AMT) / (SHR PCT) = ORIGINAL $              
         DP    WORK(08),=P'2'      DIVIDE FOR ROUNDING                          
                                                                                
         CLI   BYTE,C'C'           IS IT CURRENT $ ?                            
         BNE   CLC17                                                            
         AP    MYP+24(8),WORK(7)   ADD $$ TO CURRENT MKT ACCUM                  
         B     CLC19                                                            
                                                                                
CLC17    CLI   BYTE,C'P'           IS IT PRIOR $ ?                              
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         AP    MYP+16(8),WORK(7)   ADD $$ TO CURRENT MKT ACCUM                  
CLC19    B     CLCX                SKIP OUT                                     
                                                                                
                                                                                
CLC20    EQU   *                                                                
         ZIC   R0,RCONSPNU         SET # OF MINI ELTS                           
         LA    RF,RCONSPAM         1ST STATION AMOUNT                           
CLC30    EQU   *                                                                
         OC    0(4,RF),0(RF)       ANY VALUE IN FIELD?                          
         BNZ   CLC40               YES - IT'S A REAL LOSS!!                     
         LA    RF,9(RF)            NO  - BUMP TO NEXT STA'S $$/%                
         BCT   R0,CLC30                                                         
         B     CLCX                NO VALUES - NOT A LOSS                       
CLC40    EQU   *                   PROCESS A LOSS                               
         DROP  R5                                                               
         LA    R5,RCONELEM                                                      
CLC50    ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    CLCX                END OF RECORD                                
         AR    R5,R1                                                            
         CLI   0(R5),X'08'         TRUE ACTIVITY DATE ELT?                      
         BH    CLCX                NOT PRESENT - EXIT                           
         BNE   CLC50               NO  - GO BACK FOR NEXT ELT                   
         USING RCONACEL,R5                                                      
         ZICM  RF,RCONAC$$,4       LOAD LOSS DOLLARS FOR CONVERT                
         CVD   RF,DUB                                                           
         MP    DUB,=P'100'         ADD TWO DECL PLACES                          
                                                                                
         CLI   BYTE,C'C'           CURRENT $ ?                                  
         BNE   CLC70                                                            
         AP    MYP+24(8),DUB       ADD LOSS$$ TO CURRENT ACCUM                  
         B     CLCX                                                             
                                                                                
CLC70    CLI   BYTE,C'P'           IS IT PRIOR $ ?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    MYP+16(8),DUB      ADD LOSS $$ TO PROD LINE ACCUM                
*                                                                               
CLCX     MVC   WORK(32),MYP        RESTORE WORK                                 
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
MSG1     DC    C'MORE CONTRACTS AVAILABLE-HIT ''ENTER'' TO CONTINUE'            
MSG2     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
         SPACE 1                                                                
*                                                                               
TITLE1   DC    C' STA   ADV  PRODUCT         AGY    CONTRACT   PRIFIN  X        
               PRISHR     CURBLG  CURSHR'                                       
*                123456789º123456789º123456789º                                 
PFTITL   DC    C'PF1=PRNT 2=DSM 3=CHA 5=MGL'                                    
PFTIT1   DC    C'PF2=DSM 3=CHA 5=MGL'                                           
         EJECT                                                                  
* TOTALS AT END OF PAGE WHEN PRINTING REPORT                                    
TOTPRINT NTR1                                                                   
         CLI   LNE,52              ENSURE THAT 4 TOTAL LINES                    
         BH    *+8                                                              
         MVI   LNE,0               PRINT ON SAME PAGE                           
         XC    MYP,MYP                                                          
                                                                                
         MVC   MYP+8(15),=C'*REPORT TOTALS*'                                    
         BAS   RE,PRINT                                                         
                                                                                
         OC    SHRPCT,SHRPCT       PRINT SHR %                                  
         BNZ   TOUT15                                                           
         CP    SHRTOT,=PL8'0'                                                   
         BE    TOUT15                                                           
         ZAP   WORK(16),SHRTOT     SHR $AMT INTO WORK                           
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),ALLSHR                                                  
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SHRPCT                                                        
                                                                                
TOUT15   MVC   MYP+8(6),=C'SHARE='                                              
         LA    R2,MYP+19                                                        
         EDIT  (B4,SHRPCT),(6,0(R2)),2,TRAIL=C'%',ALIGN=LEFT                    
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+8(9),=C'CONTRACT='                                           
         EDIT  (B4,CONTOT),(4,0(R2)),ALIGN=LEFT                                 
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+8(8),=C'DOLLARS='                                            
         EDIT  (P8,DLRTOTAL),(15,0(R2)),2,FLOAT=-,ALIGN=LEFT                    
         BAS   RE,PRINT                                                         
                                                                                
TOTPX    B     XIT                                                              
         EJECT                                                                  
* INITIALIZ PRINT QUE                                                           
INITP    NTR1                                                                   
         MVI   MAXLNES,78          MAX LINES                                    
         MVI   PG,1                PG COUNT                                     
* SET 1ST TIME FOR PRTQUE                                                       
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         MVI   MYP-1,0                                                          
         XC    MYP,MYP                                                          
         MVC   MYP(3),=C'RIS'      ID                                           
         MVC   MYP+3(4),=C'LSTD'                                                
         MVC   MYP+11(3),=C'RIS'   SET SUB-KEY                                  
         MVI   MYP+24,C'Z'         CLASS 'Z'                                    
*                                                                               
         LA    R3,MYP-1                                                         
         USING PQPLD,R3            PRINT QUEUE PRINT LNE                        
*                                                                               
*                                                                               
INIT30   EQU   *                                                                
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         MVC   QLSRCID,PLUSER                                                   
         MVC   QLSUBID,PLSUBID                                                  
         CLI   QLCLASS,0                                                        
         BNE   *+10                                                             
         MVC   QLCLASS,PLCLASS                                                  
         CLI   QLSTAT,0                                                         
         BNE   *+10                                                             
         MVC   QLSTAT,PLSTAT                                                    
         CLI   QLLPP,0                                                          
         BNE   *+10                                                             
         MVC   QLLPP,PLLPP                                                      
         MVC   QLDESC,PLDESC                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'       KEEP PRINTED REPORT 2 HOURS                  
*                                                                               
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         TM    DMCB+8,X'FF'                                                     
         BZ    INITPX                                                           
         DC    H'0',C'$PQFULL'                                                  
*                                                                               
INITPX   MVC   HALF2,QLREPRNO      SAVE REPORT NUMBER                           
         B     XIT                                                              
SAVIT    DS    H                                                                
         DROP  R3                                                               
         EJECT                                                                  
* - HEADLINES WHEN PRINTING REPORT                                              
DOHEADS  NTR1                                                                   
         MVI   LNE,0                 LNE CTR                                    
         XC    MYP,MYP                                                          
         MVI   MYP-1,X'89'           SKIP TO NEW PG                             
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP(7),=C'STATION'                                               
         MVC   MYP+8(7),RISSTA                                                  
         MVC   MYP+16(12),RISSTAN                                               
                                                                                
                                                                                
         MVI   MYP-1,SPACE3          SPACE 3 LINES AFTER PRINT                  
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+40(19),=C'    DETAIL LISTING '                               
         BAS   RE,PRINT                                                         
         MVC   MYP+40(19),=C'    ______ _______ '                               
         MVI   MYP-1,SPACE3          SPACE 3 LINES AFTER PRINT                  
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(L'MYPLHEAD),MYPLHEAD                                         
         BAS   RE,PRINT                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
                                                                                
MYPLHEAD DC    C'      STA/ADV  AGENCY/ADVERTISER      PRODUCT/SALES   X        
                      FLIGHT    CONT/DEMO     DOLLARS      CRD/ACTV   VX        
               ER/SNT'                                                          
                                                                                
         EJECT                                                                  
*                                                                               
*********************************************************************           
* PRINT ROUTINE                                                                 
*********************************************************************           
PRINT    NTR1                                                                   
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         TM    DMCB+8,X'FF'        ERROR?                                       
         BZ    PRT10                                                            
         DC    H'0',C'$PQFULL'     PRINT ERROR                                  
PRT10    XC    MYP,MYP                                                          
         ZIC   RE,LNE                                                           
         LA    RE,1(RE)            BUMP LNE COUNT                               
         CLI   MYP-1,SPACE2        2 LINES                                      
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRTXIT                                                           
         CLI   MYP-1,SPACE3        3 LINES                                      
         BNE   PRTXIT                                                           
         LA    RE,2(RE)                                                         
*                                                                               
PRTXIT   STC   RE,LNE                                                           
         MVI   MYP-1,SPACE1        DEFAULT TO 1 ALWAYS                          
**       C     RE,=F'74'           ENSURE LINES LEFT                            
         C     RE,=F'52'           ENSURE LINES LEFT                            
         BL    PRTX                                                             
         ZIC   R1,PG               NOT ENOUGH/BUMP PAGE NUMBER                  
         LA    R1,1(R1)                                                         
         STC   R1,PG                                                            
         BAS   RE,DOHEADS          DO HEADLINES                                 
                                                                                
PRTX     B     XIT                                                              
*                                                                               
SPACE1   EQU   X'09'                                                            
SPACE2   EQU   X'11'                                                            
SPACE3   EQU   X'19'                                                            
         EJECT                                                                  
* LAST TIME WHEN PRINTING REPORT                                                
LASTP    NTR1                                                                   
         MVI   MYP-1,X'FF'           LAST TIME                                  
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,RISMESSH                                                      
         MVC   8(43,R2),=C'*ACTION COMPLETED  RIS,      ON PRINT QUEUE'         
         EDIT  (B2,HALF2),(5,31(R2)),ALIGN=LEFT                                 
         FOUT (R2)                                                              
         MVI   PRNT,0                                                           
*                                                                               
         NI    RISMNTSH+4,X'DF'    FORCE VALIDATION WHEN RETURNING              
*                                                                               
         B     XIT                                                              
                                                                                
*                                                                               
MAXOUT   DS    0H                                                               
         LA    R2,RISMESSH                                                      
         MVC   8(40,R2),=C'*IO TIMEOUT - REDUCE REQUEST PARAMETERS*'            
         FOUT  (R2)                                                             
         B     SKIPCNT   TURN OFF VALIDATED BITS                                
                                                                                
* THIS ROUTINE IS CALLED IN BOTTOM OF PAGE REQUEST TOTALS                       
* IT IS CALLED AT END OF FIRST READ OF FILE                                     
* IT ROLLS OVER STATION TOTALS TO BOTTOM PAGE REQUEST TOTALS                    
* THEN CLEARS STATOTAL BEFORE READING FILE AGAIN                                
REQTOTS  NTR1                                                                   
         LA    R1,STATOTAL         STATION TOTALS                               
         LA    R2,STATOTBP         TOTALS FOR BOTM OF PAGE                      
         LA    R3,6                                                             
REQTOT5  ZAP   0(8,R2),0(8,R1)                                                  
         LA    R1,8(R1)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,REQTOT5                                                       
         MVC   CONTOTBP,CONTOT     SAVE TOTAL NUMBER OF CONTRACTS               
                                                                                
* CLEAR STATOTAL                                                                
         LA    R1,STATOTAL                                                      
         LA    R3,6                                                             
REQTOT10 ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R3,REQTOT10                                                      
         XC    CONTOT,CONTOT       CLEAR CONTRACT # TOTAL                       
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DBLOK    DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
                                                                                
TEMPIO   DS    CL1000                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE RGENEROL                                                       
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE REPIOBLK                                                       
       ++INCLUDE DMPRTQL                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RERIS06S  05/01/02'                                      
         END                                                                    
