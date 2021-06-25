*          DATA SET PPREP0502  AT LEVEL 034 AS OF 12/09/03                      
*PHASE PP0502A                                                                  
*INCLUDE PPBVAL                                                                 
         TITLE 'PP0502 - DDS BILLING'                                           
*                                                                               
*         CHANGE LOG                                                            
*                                                                               
*   SMYE 11/01/01 CHANGE AT PRBIL2 TO PREVENT POINTING PAST END                 
*                 OF "MONTH LIST"                                               
*                                                                               
*   KWAN 08/00    NEW PPBILLREC AND PPBVAL DSECTS AND PNBVAL FOR PPBVAL         
*                                                                               
*   BPLA 1/96        CHANGE FOR OFFICE LIST REQ                                 
*                                                                               
*   SMYE 12/18/95    CHANGED DTCNV TO DATCON WITH NEW PARAM'S                   
*                                                                               
*   BPLA 10/12/92    USE PPBVAL TO EXTRACT "EFFECTIVE" VALUES FROM              
*                    PBILLRECS                                                  
****                                                                            
****  FOR ACCOUNT UPDATE INCLUDE DDAPOST                                        
****                                                                            
*                                                                               
*              QPAY RATE TO 5 DECIMALS                                          
*              REPORT OPTIONS                                                   
*              QOPT1 C=GIVE CLIENT TOTALS                                       
*              QOPT2 BLANK=NO PCT TOTALS,G=PCT OF DDS,ELSE PCT OF AGY           
*              QOPT3 Y=GIVE OFFICE TOTALS                                       
*              QOPT4 Y=TEST RUN - NO ACCOUNT WORKER FILE                        
*                                                                               
*        THE FIELDS BELOW NEEDED ONLY IN FIRST REQ                              
*              QSTART AND QEND= CURRENT MONTH START AND END                     
****                                                                            
****  QOPT4 SET TO Y TO NO-OP ACCOUNT FILE UPDATE                               
****                                                                            
****                                                                            
**** BELOW FIELDS WERE USED FOR ACCOUNT FILE UPDATE                             
****                                                                            
****     QPUBCLAS   COL 58      INVOICE ID 3 CHARS                              
****                                                                            
****     QPUB+1(1)  COL 28     DDS OFFICE 1=NY,2=CANADA                         
****     QPUB+2(2)  COL 29-30  AGENCY GROUP                                     
****     QPUB+4(3)  COL 31-33  OPTIONAL OVERRIDE AGENCY                         
****                                                                            
*                                                                               
PP0502   CSECT                                                                  
         NMOD1 0,PP0502,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,4095(RC)                                                      
         LA    R9,1(R9)                                                         
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP05WKD,R8                                                       
         SPACE 3                                                                
*                                                                               
         CLI   MODE,PROCBIL                                                     
         BE    PRBIL                                                            
         CLI   MODE,LBUYCLI                                                     
         BE    LBILC                                                            
         CLI   MODE,FBUYREQ                                                     
         BE    FBILR                                                            
         CLI   MODE,LBUYREQ                                                     
         BE    LBILR                                                            
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         B     EXIT                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  FIRST BILL FOR REQ                           
FBILR    DS    0H                                                               
         BC    0,FBR2                                                           
         OI    *-3,X'F0'                                                        
         BAS   RE,INIT                                                          
FBR2     DS    0H                                                               
         XC    STADTE,STADTE                                                    
         CLI   QUESTOR,C'0'                                                     
         BNH   FBR3                                                             
*        GOTO1 DTCNV,DMCB,QUESTOR,(1,STADTE)                                    
         GOTO1 DATCON,DMCB,(0,QUESTOR),(3,STADTE)                               
*                                                                               
FBR3     DS    0H                                                               
         L     R2,AMACC                                                         
         BAS   RE,CLEAR                                                         
         CLC   OLDAGY,PAGYKAGY                                                  
         BNE   FBR3B                                                            
         CLC   OLDOFF,QCLIENT      CHG OF OFF TO CAUSE AGY BRK                  
         BE    FBR4                                                             
FBR3B    DS   0H                                                                
         OC    OLDAGY,OLDAGY                                                    
         BZ    FBR4                                                             
*                                  FINISH UP LAST AGY                           
         XC    SAVAGY,PAGYNAME                                                  
         XC    PAGYNAME(66),SAVAGY                                              
         XC    SAVAGY,PAGYNAME                                                  
         BAS   RE,ENDAGY                                                        
         MVC   PAGYNAME(66),SAVAGY                                              
         ZAP   YTDAGRS,=P'0'                                                    
         L     R2,ATACC                                                         
         BAS   RE,CLEAR                                                         
         L     R2,ACLTTAB                                                       
         XC    0(4,R2),0(R2)                                                    
*                                                                               
FBR4     DS    0H                                                               
         ZAP   YTDMGRS,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   OLDAGY,PAGYKAGY                                                  
         MVC   OLDOFF,QCLIENT                                                   
         MVC   SAVAGY,PAGYNAME                                                  
         MVC   CLTSW,QOPT1                                                      
         MVC   PCTSW,QOPT2                                                      
         PACK  RATE+1(L'RATE-1),QPAY(5)                                         
****                                                                            
****    ACCOUNT FILE UPDATE NO-OPED                                             
****     MVC   MAPINV(2),SVQSTART+2      MONTH                                  
****     MVI   MAPINV+2,C'P'           PRINT                                    
****     MVC   MAPINV+3(3),QPUBCLAS    COL 58                                   
****     MVC   MAPDATE,SVQEND                                                   
****                               SET ACCOUNT HEADERS                          
****     MVI   RCVACC,C' '                                                      
****     MVC   RCVACC+1(203),RCVACC    SET ALL ACCOUNT HEADERS TO SPACE         
****                                                                            
****     MVI   RCVACC,X'65'                                                     
****     MVC   RCVACC+1(2),=C'SR'  SRINT REC                                    
****     MVC   RCVACC+3(3),QPUB+1      DDS OFFICE + AGY GROUP                   
****     MVC   RCVACC+6(2),QAGENCY                                              
****     CLI   QPUB+4,C' '                                                      
****     BE    *+10                                                             
****     MVC   RCVACC+6(3),QPUB+4  OVERRIDE AGENCY                              
****     MVC   RCVCNTR+3(8),=C'PRINTPAK'                                        
****                                                                            
****     MVI   INCACC,X'65'                                                     
****     MVC   INCACC+1(3),=C'SIP'                                              
****     MVC   INCACC+4(1),PAGYKMED                                             
****     CLI   QPUB+1,C'2'         TEST FOR CANADA                              
****     BNE   *+10                                                             
****     MVC   INCACC+3(2),=C'CP'      CANADIAN PRINT                           
****     MVC   INCCNTR(9),RCVACC       INCOME CONTRA - LIKE RCVACC              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                  INITIALIZATION                               
INIT     NTR1                                                                   
         SPACE 2                                                                
         MVC   PAGE,=H'1'          RESET PAGE FOR EACH INVOICE                  
         LA    RE,PP05WK                                                        
         LH    RF,=Y(PP05WKX-PP05WK)                                            
         XCEF                                                                   
*                                                                               
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
*        GOTO1 (RF),(R1),QEND,(1,BEND)                                          
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
*                                                                               
         LA    R2,MLIST+NMOSB*2                                                 
         LR    RF,R2                                                            
         MVC   0(2,R2),BSTART                                                   
         LA    R3,NMOSB                                                         
         BAS   RE,MNTHBACK                                                      
         BCT   R3,*-4                                                           
         LR    R2,RF                                                            
         LA    R3,NMOSU                                                         
         BAS   RE,MNTHUP                                                        
         BCT   R3,*-4                                                           
         MVI   2(R2),X'FF'                                                      
*                                                                               
*        GOTO1 DTCNV,DMCB,QSTART,(5,PMOS)    MMM/YY                             
         GOTO1 DATCON,DMCB,(0,QSTART),(9,PMOS)   MMM/YY                         
         MVC   SVQSTART,QSTART                                                  
         MVC   SVQEND,QEND                                                      
*                                                                               
         LA    R3,ACLTTAB                                                       
         LA    R2,ACONS                                                         
         LA    R0,(ACONSX-ACONS)/4                                              
INIT4    DS    0H                                                               
         L     R5,0(R2)                                                         
         A     R5,RELO                                                          
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,INIT4                                                         
         L     R2,AMACC                                                         
         BAS   RE,CLEAR                                                         
         L     R2,ATACC                                                         
         BAS   RE,CLEAR                                                         
         ZAP   CLTGRS,=P'0'                                                     
         ZAP   CLTDUE,=P'0'                                                     
         ZAP   DDSGRS,=P'0'                                                     
         ZAP   DDSDUE,=P'0'                                                     
         ZAP   YTDAGRS,=P'0'                                                    
****                                                                            
****   QOPT4 SET TO Y TO NO-OP ACCOUNT FILE UPDATE                              
****   WILL LEAVE WKOPEN AS 0 - TO PREVENT UPDATE                               
****                                                                            
         MVI   QOPT4,C'Y'                                                       
****                                                                            
         CLI   QOPT4,C'Y'          TEST RUN                                     
         BE    INIT8                                                            
****     CLI   WKOPEN,1                                                         
****     BE    INIT8                                                            
****     L     R0,=V(DDAPOST)                                                   
****     A     R0,RELO                                                          
****     ST    R0,DDAPOST                                                       
****     L     R0,=A(WRKRBUFF)                                                  
****     A     R0,RELO                                                          
****     ST    R0,MAPFILE                                                       
****     MVC   MAPWRKR,WORKER                                                   
****     MVC   MAPRECUP,RECUP                                                   
****     MVC   MAPMOS,SVQSTART       YYMM                                       
****     ZAP   MAPRECNT,=P'0'                                                   
****     ZAP   MAPCASH,=P'0'                                                    
****     LA    R5,MAPID                                                         
****     USING UKRECD,R5                                                        
****     MVC   UKUSRID(2),=H'542'  DDS                                          
****     MVC   UKSYSPRG,=C'P05'                                                 
****     MVC   UKSUBPRG,FILENUM                                                 
****     PACK  DUB(2),RCDATE+3(3)                                               
****     MVC   UKDAY,DUB                                                        
****     MVI   UKCLASS,C'P'        POSTING FILE                                 
****     DROP  R5                                                               
****     GOTO1 WORKER,DMCB,=C'INDEX',MAPFILE,MAPID                              
****     TM    DMCB+8,X'90'                                                     
****     BNZ   INIT6                                                            
****     TM    RCUPSI,X'80'        SEE IF RERUN                                 
****     BNZ   INIT6                                                            
****     MVC   P+1(26),=C'WORKER FILE ALREADY EXISTS'                           
****     ZAP   RATE,=P'0'                                                       
****     BAS   RE,REPRT                                                         
****     DC    H'0'                                                             
****                                                                            
****6    MVI   WKOPEN,1       TAG WAS INIT6                                     
****                                                                            
*                                                                               
INIT8    DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*                                  BACK UP YM AT 0(R2)                          
*                                  PUT NEW YM AT -2(R2)                         
MNTHBACK DS    0H                                                               
         BCTR  R2,R0                                                            
         BCTR  R2,R0                                                            
         LH    R0,2(R2)                                                         
         LA    R1,1                                                             
         CLI   3(R2),1        JAN                                               
         BNE   *+8                                                              
         LA    R1,245                                                           
         SR    R0,R1                                                            
         STH   R0,0(R2)                                                         
         BR    RE                                                               
         SPACE 3                                                                
*                                  BUMP YM AT 0(R2)                             
*                                  PUT NEW YM AT 2(R2)                          
MNTHUP   DS    0H                                                               
         LH    R0,0(R2)                                                         
         LA    R1,1                                                             
         CLI   1(R2),12       DEC                                               
         BNE   *+8                                                              
         LA    R1,245                                                           
         AR    R0,R1                                                            
         STH   R0,2(R2)                                                         
         LA    R2,2(R2)                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                  PROCESS BILL                                 
PRBIL    DS    0H                                                               
*                                                                               
         CLC   PAGYKAGY(3),QAGENCY                                              
         BE    *+12                                                             
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
         OC    PBILKEST,PBILKEST   IGNORE PRODUCT TOTAL BILLS                   
         BZ    EXIT                                                             
         CLI   PBRETAIL,X'41'      IGNORE RETAIL SUMMARY BILLS                  
         BE    EXIT                                                             
         TM    PBILCMSW,X'20'      IGNORE AOR BILLS ALSO                        
         BO    EXIT                                                             
*                                                                               
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
         MVC   PBILLGRS,PPBVEBG  SET "EFFECTIVE" GROSS INTO PBILLREC            
*                                NOTE - PP0502 ONLY NEEDS GROSS                 
         CLC   PBILKBMN(2),BSTART                                               
         BL    PRBIL5                                                           
         CLC   PBILKBMN,BEND                                                    
         BH    EXIT                                                             
*                                                                               
         CLC   PBILKMOS,STADTE     CHECK VS START FOR AGY                       
         BL    EXIT                                                             
*                                                                               
         LA    R2,MLIST                                                         
*                                                                               
PRBIL2   DS    0H                                                               
         CLC   PBILKMOS(2),0(R2)                                                
         BNH   PRBIL4                                                           
         CLI   2(R2),X'FF'         AT END OF TABLE ?                            
         BE    PRBIL4              YES - DONE                                   
         LA    R2,2(R2)                                                         
         B     PRBIL2                                                           
*                                                                               
PRBIL4   DS    0H                                                               
         LA    R0,MLIST                                                         
         SR    R2,R0                                                            
         MH    R2,=H'6'            6X2=12                                       
         L     R3,AMACC                                                         
         AR    R2,R3                                                            
         AP    0(6,R2),PBILLGRS    ADD TO MONTHLY TOTALS                        
         AP    CLTGRS,PBILLGRS                                                  
*                                                                               
PRBIL5   DS    0H                  DO YTD BILLING                               
         CLC   PBILKBMN(1),BSTART      CHK THIS YEAR                            
         BL    EXIT                                                             
         AP    YTDMGRS,PBILLGRS                                                 
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                  LAST BILL FOR CLIENT                         
LBILC    DS    0H                                                               
         CP    CLTGRS,=P'0'                                                     
         BE    EXIT                                                             
         L     R2,ACLTTAB                                                       
         L     R3,0(R2)            A(NEXT)                                      
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,4(R2)                                                         
         XC    0(2,R3),0(R3)                                                    
         CLI   QOPT3,C'Y'          TEST DPINT OFFICE TOTALS                     
         BNE   LBILC2                                                           
         MVC   0(1,R3),PCLTOFF                                                  
         CLI   PCLTOFF,C' '                                                     
         BH    *+8                                                              
         MVI   0(R3),X'FF'         NO OFFICE                                    
LBILC2   DS    0H                                                               
         MVC   2(03,R3),PCLTKCLT                                                
         MVC   5(20,R3),PCLTNAME                                                
         MVC   25(1,R3),PCLTKMED                                                
         MVC   26(12,R3),CLTGRS                                                 
         LA    R3,CTABEL(R3)                                                    
         ST    R3,0(R2)                                                         
         ZAP   CLTGRS,=P'0'                                                     
         ZAP   CLTDUE,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                  LAST BILL FOR REQUEST                        
LBILR    DS    0H                                                               
         L     R3,AMACC                                                         
LBR2     LA    R2,MLIST                                                         
         ZAP   PDUB,=P'0'                                                       
         ZAP   PDUB2,=P'0'                                                      
         MVI   TOTSW,0                                                          
         MVI   MEDCHK,C'Z'                                                      
         CLC   OLDAGY,PAGYKAGY                                                  
         BNE   LBR3                                                             
         CLC   OLDOFF,QCLIENT      OFF BRK TREATED AS AGY BRK                   
         BNE   *+10                                                             
         MVC   MEDCHK,PAGYKMED                                                  
LBR3     DS    0H                                                               
         LA    R4,P                                                             
         USING BLINE,R4                                                         
LBR4     DS    0H                                                               
         CP    0(6,R3),=P'0'                                                    
         BZ    LBR8                                                             
*        GOTO1 DTCNV,DMCB,(1,0(R2)),(5,BLDATE)                                  
         GOTO1 DATCON,DMCB,(3,0(R2)),(9,BLDATE)                                 
*                                                                               
         CLI   MEDCHK,C'Z'                                                      
         BE    LBR4B                                                            
         LR    RF,R3                                                            
         BAS   RE,COMPDUE                                                       
LBR4B    DS    0H                                                               
         AP    PDUB,0(6,R3)                                                     
         AP    PDUB2,6(6,R3)                                                    
LBR5     LA    R5,0(R3)                                                         
         LA    R6,BLGRS                                                         
         BAS   RE,EDIT                                                          
         LA    R5,6(R3)                                                         
         LA    R6,BLDUE                                                         
         BAS   RE,EDIT                                                          
         BAS   RE,REPRT                                                         
LBR8     DS    0H                                                               
         CLI   TOTSW,C'T'                                                       
         BE    LBR10                                                            
         LA    R3,12(R3)                                                        
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   LBR4                                                             
         BAS   RE,REPRT            SKIP A LINE                                  
         MVC   BLDATE(5),=C'TOTAL'                                              
         MVI   TOTSW,C'T'                                                       
         MVI   SPACING,3                                                        
         LA    R3,PDUB                                                          
         B     LBR5                                                             
*                                                                               
LBR10    DS    0H                                                               
*                                                                               
         B     LBR11                                                            
****   ACCOUNT FILE UPDATE NO-OPED                                              
****                                                                            
****     CLI   WKOPEN,1            SEE IF POSTING TO ACC WORKER FILE            
****     BNE   LBR11               NO                                           
****     CLI   MEDCHK,C'Z'         DON'T POST TOTAL PRINT                       
****     BE    LBR11                                                            
****     ZAP   MAPAGRS,PDUB                                                     
****     ZAP   MAPDGRS,PDUB2                                                    
****     CP    MAPAGRS,=P'0'                                                    
****     BNE   LBR10D                                                           
****     CP    MAPDGRS,=P'0'                                                    
****     BE    LBR11               DON'T POST ZERO BILLS                        
****                                                                            
****0D   GOTO1 DDAPOST,DMCB,DDAPOSTW      TAG WAS LBR10D                        
****                                                                            
LBR11    DS    0H                                                               
*                                                                               
         MVC   P+26(21),=C'YEAR-TO-DATE BILLINGS'                               
         LA    R5,YTDMGRS                                                       
         LA    R6,BLGRS                                                         
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
*                                                                               
         AP    YTDAGRS,YTDMGRS     ADD TO AGY YTD                               
         ZAP   YTDMGRS,=P'0'                                                    
*                                                                               
         CLI   CLTSW,C'C'                                                       
         BNE   LBR20                                                            
         MVI   SPACING,2                                                        
         MVI   LINENEED,10                                                      
         MVC   BLNAME(13),=C'CLIENT TOTALS'                                     
         CLI   PCTSW,C' '                                                       
         BE    *+16                                                             
         MVC   BLPCT-2(08),=C' PERCENT'                                         
         MVC   132+BLPCT-2(08),=C'OF TOTAL'                                     
         BAS   RE,REPRT                                                         
*                                                                               
         MVI   TOTSW,0                                                          
*                                                                               
*                                  SORT CLIENT LIST BY OFFICE                   
         L     R2,ACLTTAB                                                       
         L     R7,0(R2)                                                         
         LTR   R7,R7                                                            
         BZ    LBR20                                                            
         SR    R6,R6                                                            
         SR    R7,R2                                                            
         SH    R7,=H'4'                                                         
         L     R0,=A(CTABEL)                                                    
         DR    R6,R0                                                            
         GOTO1 XSORT,DMCB,4(R2),(R7),(R0),25,0                                  
*                                                                               
         XC    LASTOFF,LASTOFF                                                  
         ZAP   OFFBIL,=P'0'                                                     
         ZAP   OFFDUE,=P'0'                                                     
         L     R2,ACLTTAB                                                       
         LA    R3,4(R2)                                                         
*                                                                               
LBR12    DS    0H                                                               
         C     R3,0(R2)                                                         
         BNL   LBR18                                                            
         CLC   MEDCHK,25(R3)                                                    
         BNE   LBR16                                                            
*                                                                               
         CLI   MEDCHK,C'Z'                                                      
         BE    LBR15                                                            
         LA    RF,26(R3)                                                        
         BAS   RE,COMPDUE                                                       
LBR15    DS    0H                                                               
         CLC   LASTOFF(2),0(R3)                                                 
         BE    *+8                                                              
         BAS   RE,PRTOFF                                                        
         AP    OFFBIL,26(6,R3)                                                  
         AP    OFFDUE,32(6,R3)                                                  
         MVC   BLNAME(3),2(R3)                                                  
         MVC   BLNAME+4(20),5(R3)                                               
         LA    R5,26(R3)                                                        
         LA    R6,BLGRS                                                         
         BAS   RE,EDIT                                                          
         LA    R5,32(R3)                                                        
         LA    R6,BLDUE                                                         
         BAS   RE,EDIT                                                          
*                                  PERCENT OF TOTAL                             
         CLI   PCTSW,C' '                                                       
         BE    LBR15B                                                           
         ZAP   WORK(6),PDUB                                                     
         ZAP   DUBDUB,26(6,R3)                                                  
         CLI   PCTSW,C'G'                                                       
         BE    *+16                                                             
         ZAP   WORK(6),PDUB2       DUE                                          
         ZAP   DUBDUB,32(6,R3)                                                  
         CP    WORK(6),=P'0'                                                    
         BE    LBR15B                                                           
         MP    DUBDUB,=P'10000'                                                 
         DP    DUBDUB,WORK(6)                                                   
         AP    DUBDUB(10),=P'5'                                                 
         MVO   DUBDUB(10),DUBDUB(9)                                             
         CP    DUBDUB(10),=P'0'                                                 
         BE    LBR15B                                                           
         LA    R1,DUBDUB+7                                                      
         EDIT  (P3,0(R1)),(5,BLPCT),1,FLOAT=-                                   
LBR15B   DS    0H                                                               
         BAS   RE,REPRT                                                         
LBR16    DS    0H                                                               
         CLI   TOTSW,C'T'                                                       
         BE    LBR20                                                            
         LA    R3,CTABEL(R3)                                                    
         B     LBR12                                                            
*                                                                               
LBR18    DS    0H                                                               
         BAS   RE,PRTOFF                                                        
*                                                                               
*                                                                               
LBR20    DS    0H                                                               
         L     R2,AMACC            ADD TO TOTALS ACCUM                          
         L     R3,ATACC                                                         
         LA    R4,NMOST*2                                                       
LBR21    DS    0H                                                               
         AP    0(6,R3),0(6,R2)                                                  
         LA    R2,6(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R4,LBR21                                                         
*                                                                               
*                                  ADD TO AGY LIST                              
         L     R2,ADDSTAB                                                       
         L     R3,0(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,4(R2)                                                         
         MVC   0(3,R3),PAGYKAGY                                                 
         MVC   3(33,R3),PAGYNAME                                                
*                                                                               
         CLI   OLDOFF,C'$'         OFFICE LIST REQ                              
         BE    *+12                                                             
         CLI   OLDOFF,C'*'                                                      
         BNE   *+10                                                             
         MVC   33(3,R3),OLDOFF     OFFICE NO.                                   
         MVC   36(10,R3),PAGYMED                                                
*                                                                               
****                               ACCOUNT INTERFACE NO-OPED                    
****                                                                            
****     MVC   58(2,R3),MAPINV     PUT INV IN AGY TABLE                         
****     MVC   60(3,R3),=C'-P-'                                                 
****     MVC   63(3,R3),MAPINV+3                                                
****                                                                            
         CLI   MEDCHK,C'Z'                                                      
         BNE   LBR22                                                            
         AP    DDSGRS,PDUB                                                      
         AP    DDSDUE,PDUB2                                                     
         MVC   36(10,R3),=C'ALL PRINT '                                         
         MVI   2(R3),C'Z'                                                       
LBR22    DS    0H                                                               
         MVC   46(12,R3),PDUB                                                   
         LA    R3,DTABEL(R3)                                                    
         ST    R3,0(R2)                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
PRTOFF   NTR1                                                                   
         SPACE 2                                                                
         OC    LASTOFF,LASTOFF                                                  
         BZ    PRTOFFX                                                          
         BAS   RE,REPRT                                                         
         LA    RF,BLPCT-20                                                      
         MVC   0(20,RF),=C'TOTAL FOR OFFICE X  '                                
         MVC   17(1,RF),LASTOFF                                                 
         CLI   LASTOFF,X'FF'                                                    
         BNE   *+8                                                              
         MVI   17(RF),C'*'         UNKNOWN OFFICE                               
*                                                                               
         LA    R5,OFFBIL                                                        
         LA    R6,BLGRS                                                         
         BAS   RE,EDIT                                                          
         LA    R5,OFFDUE                                                        
         LA    R6,BLDUE                                                         
         BAS   RE,EDIT                                                          
*                                                                               
*                                  PERCENT OF TOTAL                             
         CLI   PCTSW,C' '                                                       
         BE    PRTOFF4                                                          
         ZAP   WORK(6),PDUB                                                     
         ZAP   DUBDUB,OFFBIL                                                    
         CLI   PCTSW,C'G'                                                       
         BE    *+16                                                             
         ZAP   WORK(6),PDUB2       DUE                                          
         ZAP   DUBDUB,OFFDUE                                                    
         CP    WORK(6),=P'0'                                                    
         BE    PRTOFF4                                                          
         MP    DUBDUB,=P'10000'                                                 
         DP    DUBDUB,WORK(6)                                                   
         AP    DUBDUB(10),=P'5'                                                 
         MVO   DUBDUB(10),DUBDUB(9)                                             
         CP    DUBDUB(10),=P'0'                                                 
         BE    PRTOFF4                                                          
         LA    R1,DUBDUB+7                                                      
         EDIT  (P3,0(R1)),(5,BLPCT),1,FLOAT=-                                   
*                                                                               
PRTOFF4  DS    0H                                                               
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
*                                                                               
PRTOFFX  DS    0H                                                               
         MVC   LASTOFF,0(R3)                                                    
         ZAP   OFFBIL,=P'0'                                                     
         ZAP   OFFDUE,=P'0'                                                     
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
ENDAGY   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
         ZAP   RATE,=P'0'                                                       
*                                                                               
*                                  ROLL UP CLIENT TOTALS                        
         L     R2,ACLTTAB                                                       
         LA    R3,4(R2)                                                         
         L     R5,0(R2)                                                         
         LTR   R5,R5                                                            
         BZ    EA8                                                              
         MVI   0(R5),0                                                          
EA4      DS    0H                                                               
         MVI   25(R3),C'Z'                                                      
         CLC   0(25,R3),CTABEL(R3)                                              
         BNE   EA6                                                              
         AP    CTABEL+26(6,R3),26(6,R3)                                         
         AP    CTABEL+32(6,R3),32(6,R3)                                         
         MVI   25(R3),0                                                         
EA6      DS    0H                                                               
         LA    R3,CTABEL(R3)                                                    
         CR    R3,R5                                                            
         BL    EA4                                                              
EA8      DS    0H                                                               
         MVC   YTDMGRS,YTDAGRS                                                  
         L     R3,ATACC                                                         
         B     LBR2                                                             
         EJECT                                                                  
*                                                                               
LAST     DS    0H                                                               
         B     LAST1                                                            
****                                                                            
****     ACCOUNT UPDATE NO-OPED                                                 
****                                                                            
****     CLI   WKOPEN,1                                                         
****     BNE   LAST1                                                            
****                                                                            
****     GOTO1 DDAPOST,DMCB,(X'FF',DDAPOSTW)                                    
****                                                                            
LAST1    DS    0H                                                               
         MVI   OLDAGY,X'FF'                                                     
         BAS   RE,ENDAGY                                                        
*                                                                               
*              SINCE PP05 IS NOT RUN ACROSS AGYS                                
*              DON'T PRINT DDS TOTALS                                           
*                                                                               
* NO-OP-ED                                                                      
***      MVC   PAGE,=H'1'                                                       
***                                                                             
* NO-OPED                                                                       
**       MVC   PAGYNAME(66),SPACES                                              
**       MVC   PAGYNAME(10),=C'DDS TOTALS'                                      
* NO-OPED                                                                       
***      MVC   PAGYADDR(20),=C'* FOR PRINT FILE   *'                            
***      MVC   PAGYADDR+17(1),FILENUM                                           
         ZAP   RATE,=P'0'                                                       
         MVI   TOTSW,C'T'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    MAPINV,MAPINV                                                    
         L     R2,ADDSTAB                                                       
         LA    R3,4(R2)                                                         
         LA    R4,P                                                             
         USING BLINE,R4                                                         
LAST3    DS    0H                                                               
         CLI   TOTSW,C'T'                                                       
         BNE   LAST4                                                            
         MVI   TOTSW,0                                                          
         MVI   LINENEED,8                                                       
         MVC   BLNAME(02),0(R3)                                                 
         MVC   BLNAME+03(33),3(R3)                                              
         CLI   33(R3),C'*'         TEST OFFICE                                  
         BNE   LAST3B                                                           
         MVC   BLNAME+33(3),SPACES                                              
         LA    RF,BLNAME+3+132                                                  
         MVC   0(6,RF),=C'OFFICE'                                               
         MVC   7(2,RF),34(R3)                                                   
LAST3B   DS    0H                                                               
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
*                                                                               
LAST4    DS    0H                                                               
*                                                                               
         CLI   2(R3),C'Z'                                                       
         BNE   *+12                                                             
         MVI   SPACING,2                                                        
         MVI   TOTSW,C'T'                                                       
         MVC   BLINE+10(10),36(R3)                                              
         CLI   2(R3),C'Z'                                                       
         BE    *+10                                                             
         MVC   BLINE+25(8),58(R3)                                               
         LA    R5,46(R3)                                                        
         LA    R6,BLGRS                                                         
         BAS   RE,EDIT                                                          
         LA    R5,52(R3)                                                        
         LA    R6,BLDUE                                                         
         BAS   RE,EDIT                                                          
*                                  PERCENT OF TOTAL                             
         ZAP   DUBDUB,52(6,R3)                                                  
         MP    DUBDUB,=P'10000'                                                 
         CP    DDSDUE,=P'0'                                                     
         BE    LAST8                                                            
         DP    DUBDUB,DDSDUE                                                    
         AP    DUBDUB(10),=P'5'                                                 
         MVO   DUBDUB(10),DUBDUB(9)                                             
         CP    DUBDUB(10),=P'0'                                                 
         BE    LAST8                                                            
         LA    R1,DUBDUB+7                                                      
         EDIT  (P3,0(R1)),(5,BLPCT),1,FLOAT=-                                   
*                                                                               
LAST8    DS    0H                                                               
         BAS   RE,REPRT                                                         
         BAS   RE,POSTMED          ADD TO MEDIA TOTALS                          
         LA    R3,DTABEL(R3)                                                    
         C     R3,0(R2)                                                         
         BL    LAST3                                                            
*                                                                               
         B     EXIT                                                             
*                                                                               
*              SINCE PP05 IS NOT RUN ACROSS AGYS                                
*                                                                               
*** DON'T DO DDS TOTALS                                                         
*** NO-OPED                                                                     
***      C     R2,AMEDTAB                                                       
***      BE    EXIT                                                             
***      L     R2,AMEDTAB                                                       
***      LA    R3,4(R2)                                                         
***      B     LAST3                                                            
         SPACE 3                                                                
*                                                                               
POSTMED  NTR1                                                                   
         SPACE 2                                                                
         L     R2,AMEDTAB                                                       
         LA    R5,4+MTABEL*7(R2)                                                
         ST    R5,0(R2)                                                         
         LA    R5,4(R2)                                                         
PM2      DS    0H                                                               
         CLC   2(1,R3),2(R5)                                                    
         BE    PM4                                                              
         LA    R5,MTABEL(R5)                                                    
         C     R5,0(R2)                                                         
         BL    PM2                                                              
         DC    H'0'                                                             
*                                                                               
PM4      DS    0H                                                               
         AP    46(6,R5),46(6,R3)                                                
         AP    52(6,R5),52(6,R3)                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
REPRT    NTR1                                                                   
         SPACE 2                                                                
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LINENEED                                                      
         MVI   LINENEED,0                                                       
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   FORCEHED,C'Y'                                                    
         BNE   REPRT20                                                          
*                                                                               
         B     REPRT2              DDS TOTALS NO-OPED                           
*                                                                               
***      CLC   PAGYNAME(3),=C'DDS'                                              
***      BNE   REPRT2                                                           
* NO-OPED                                                                       
***      MVC   HEAD6+4(21),=C'** DO NOT SEND OUT **'                            
***                                                                             
***      MVC   HEAD12+26(7),=C'INVOICE'                                         
***      CLI   PCTSW,C' '                                                       
***      BE    REPRT1                                                           
***      MVC   HEAD11+39(10),=C'  PERCENT '                                     
***      MVC   HEAD12+39(10),=C' OF TOTAL '                                     
***                                                                             
REPRT1   B     REPRT4                                                           
*                                                                               
REPRT2   DS    0H                                                               
*                                                                               
****                               ACCOUNT INTERFACE CHGS NO-OPED               
****     MVC   HEAD7+53(8),=C'INVOICE#'                                         
****     MVC   HEAD7+62(2),MAPINV                                               
****     MVC   HEAD7+64(3),=C'-P-'                                              
****     MVC   HEAD7+67(3),MAPINV+3                                             
****                                                                            
         MVC   HEAD1(3),=C'TO-'                                                 
         CLI   OLDOFF,C'$'           OFFICE LIST REQ                            
         BNE   REPRT2A                                                          
         MVC   HEAD4+4(6),=C'OFFICE'                                            
         MVC   HEAD4+11(3),OLDOFF                                               
         B     REPRT2AX                                                         
*                                                                               
REPRT2A  CLI   OLDOFF,C'*'                                                      
         BNE   *+16                                                             
         MVC   HEAD4+4(6),=C'OFFICE'                                            
         MVC   HEAD4+11(2),OLDOFF+1                                             
*                                                                               
REPRT2AX MVC   HEAD6+4(19),=C'MEDIA   TOTAL PRINT'                              
         MVC   HEAD11+39(10),=C'  MONTH   '                                     
         MVC   HEAD12+39(10),=C'OF SERVICE'                                     
         C     R2,ACLTTAB          TEST DOING CLIENT TOTALS                     
         BE    REPRT2C                                                          
         C     R2,ADDSTAB          OR AGY TOTALS                                
         BNE   REPRT3                                                           
REPRT2C  CLI   PCTSW,C' '                                                       
         BE    REPRT3                                                           
         MVC   HEAD11+39(10),=C'  PERCENT '                                     
         MVC   HEAD12+39(10),=C' OF TOTAL '                                     
REPRT3   DS    0H                                                               
         CP    RATE,=P'0'                                                       
         BE    REPRT4                                                           
         MVC   HEAD6+10(1),PAGYKMED                                             
         MVC   HEAD6+12(10),PAGYMED                                             
         MVI   HEAD6+22,C' '                                                    
         MVC   HEAD8+4(4),=C'RATE'                                              
         LA    R4,HEAD8+12                                                      
         MVI   0(R4),C'.'                                                       
         UNPK  1(5,R4),RATE+1(L'RATE-1)                                         
         MVC   7(3,R4),=C'PCT'                                                  
REPRT4   DS    0H                                                               
         MVC   HEAD10+4(6),PMOS                                                 
*                                                                               
REPRT20  DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EDIT     DS    0H                                                               
EDITED   EDIT  (P6,0(R5)),(16,0(R6)),2,COMMAS=YES,CR=YES                        
*                                                                               
*                                                                               
         CLI   TOTSW,C'T'                                                       
         BNER  RE                                                               
         CP    0(6,R5),=P'0'                                                    
         BNPR  RE                                                               
         MVC   14(2,R6),=C'* '                                                  
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
CLEAR    DS    0H                                                               
         LA    R0,NMOST*2                                                       
         ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
COMPDUE  DS    0H                                                               
         ZAP   DUBDUB,0(6,RF)                                                   
         MP    DUBDUB,RATE                                                      
         MVN   ROUNDSN1,5(RF)      SET CORRECT SIGN                             
         AP    DUBDUB,ROUND1                                                    
         MVO   DUBDUB,DUBDUB(12)                                                
         ZAP   6(6,RF),DUBDUB                                                   
         BR    RE                                                               
         EJECT                                                                  
ACONS    DC    A(CLTTAB)                                                        
         DC    A(DDSTAB)                                                        
         DC    A(MEDTAB)                                                        
         DC    A(MACC)                                                          
         DC    A(TACC)                                                          
ACONSX   EQU   *                                                                
*                                                                               
ROUND1   DC    P'5000000'                                                       
         ORG   *-1                                                              
ROUNDSN1 DC    X'0C'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
CTABEL   EQU   38                  CLIENT TAB ENERY LEN                         
DTABEL   EQU   66                  DDS TAB ENTRY LEN                            
MTABEL   EQU   DTABEL              MED TAB ENTRY LEN                            
NMOSB    EQU   36                  MONTHS BACK                                  
NMOSU    EQU   24                  MONTHS UP                                    
NMOST    EQU   NMOSB+NMOSU+1         TOTAL MONTHS                               
*                                                                               
*                                                                               
         SPACE 3                                                                
PP05WKD  DSECT                                                                  
PP05WK   DS    0C                                                               
ACLTTAB  DS    A                                                                
ADDSTAB  DS    A                                                                
AMEDTAB  DS    A                                                                
AMACC    DS    A                                                                
ATACC    DS    A                                                                
DDAPOST  DS    A                                                                
*                                                                               
*                                                                               
RATE     DS    PL4                                                              
PDUB     DS    PL6                                                              
PDUB2    DS    PL6                                                              
YTDMGRS  DS    PL6                                                              
YTDAGRS  DS    PL6                                                              
CLTGRS   DS    PL6                                                              
CLTDUE   DS    PL6                                                              
DDSGRS   DS    PL6                                                              
DDSDUE   DS    PL6                                                              
DUBDUB   DS    PL16                                                             
*                                                                               
*                                                                               
MLIST    DS    0C                                                               
         ORG   *+(NMOST+1)*2                                                    
MLISTX   DS    XL2                                                              
PMOS     DS    CL6                                                              
SVQSTART DS    CL6                                                              
SVQEND   DS    CL6                                                              
LINENEED DS    X                                                                
OLDAGY   DS    CL2                                                              
OLDOFF   DS    CL3                                                              
LASTOFF  DS    XL2                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
STADTE   DS    XL3                                                              
TOTSW    DS    X                                                                
WKOPEN   DS    CL1                                                              
MEDCHK   DS    CL1                                                              
SAVAGY   DS    CL66                                                             
CLTSW    DS    CL1                                                              
PCTSW    DS    CL1                                                              
OFFBIL   DS    PL6                                                              
OFFDUE   DS    PL6                                                              
*                                                                               
       ++INCLUDE PPBVALD           NEW DSECT FOR PPBVAL                         
         DS    0F                  FOR ALIGNMENT                                
       ++INCLUDE DDAPOSTW                                                       
PP05WKX  EQU   *                                                                
         SPACE 3                                                                
*                             PRINT LINE DSECT                                  
BLINED   DSECT                                                                  
BLINE    DS    0C                                                               
BLNAME   DS    CL35                                                             
         DS    CL6                                                              
BLDATE   DS    CL6                                                              
BLPCT    EQU   BLDATE                                                           
         DS    CL2                                                              
BLGRS    DS    CL16                                                             
BLDUE    DS    CL16                                                             
         SPACE 3                                                                
CLTTAB   CSECT                                                                  
         DC    A(0)                                                             
         ORG   *+CTABEL*600                                                     
         DC    AL4(0)                                                           
*                                                                               
DDSTAB   CSECT                                                                  
         DC    A(0)                                                             
         ORG   *+DTABEL*250                                                     
         DC    AL4(0)                                                           
*                                                                               
MEDTAB   CSECT                                                                  
         DC    A(0)                                                             
         DC    C'  IDDS TOTALS                       INTERACTV '                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    C'  M                                 MAGAZINE  '                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    C'  N                                 NEWSPAPER '                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    C'  O                                 OUTDOOR   '                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    C'  S                                 SUPPLEMENT'                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    C'  T                                 TRADE     '                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    C'  Z                                 ALL PRINT '                
         DC    2PL6'0'                                                          
         DC    CL8' '                                                           
         DC    AL4(0)                                                           
*                                                                               
MACC     CSECT                                                                  
         DS    0C                                                               
         ORG   *+NMOST*12                                                       
         DC    AL4(0)                                                           
*                                                                               
TACC     CSECT                                                                  
         DS    0C                                                               
         ORG   *+NMOST*12                                                       
         DC    AL4(0)                                                           
*                                                                               
*                                                                               
WRKRBUFF CSECT                                                                  
         DS    4096X                                                            
*                                                                               
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         SPACE 2                                                                
       ++INCLUDE DDLOGOD                                                        
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPWORKD                                                        
*                                                                               
       ++INCLUDE PPNEWFILE         HAVE NEW DSECT FOR PBILLREC                  
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034PPREP0502 12/09/03'                                      
         END                                                                    
