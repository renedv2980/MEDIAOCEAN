*          DATA SET DENM09I    AT LEVEL 013 AS OF 09/30/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE DENM09IA                                                                 
*********************************************************************           
* NTI/NSS WEEKLY MOVIEGOER CONVERSION                                           
* IPHASE: DENM09I                                                               
* OPHASE: DENM09O                                                               
*********************************************************************           
         TITLE 'DEMO CONVERSION - NTI/NSS MOVIE GOER CONVERSION'                
NUMCATS  EQU   10                                                               
         EJECT                                                                  
DENM09I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DENM09I,RA,R3,R4                                               
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         SPACE 1                                                                
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         USING MIREC,RC                                                         
         SPACE 1                                                                
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     ENDJOB              E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* READ - GET INPUT TAPE RECORDS ONE AT A TIME AND PROCESS. BUILD INTERM         
*        RECDS.                                                                 
* *********************************************************************         
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
         L     RE,=A(PACKWT)                                                    
         ST    RE,APACKWT                                                       
         LA    RE,COMWRK           SAVE ADDR OF COMMON WRK AREA BETWN           
         ST    RE,ACOMWRK            INPUT AND OUTPUT PHASES                    
         MVI   NOTAVAL,0           DATA NOT AVAIL SWITCH                        
         MVI   BYPREAD,X'FF'       SET TO 1ST-TIME-THRU (GET RDR 1ST)           
         OPEN  (IN1,(INPUT))                                                    
         DS    0H                  NETWORK BIT MAP FOR NAD                      
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(C'N',VBITMAP1),0                          
         DS    0H                  SYN FOR NSS                                  
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(C'S',VBITMAP2),0                          
*                                                                               
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS               
         BNE   READ20                                                           
         MVI   RELOFRST,0                                                       
*                                                                               
READ20   DS    0H                                                               
         CLI   BYPREAD,C'U'        UNIVERSE WEEKLY RELEASE                      
         BE    RELUNV                                                           
         CLI   BYPREAD,C'T'        TP SUMMARY USAGE RECDS                       
         BE    RELUSG                                                           
*                                                                               
         CLI   BYPREAD,C'P'        PROGRAM RELEASE                              
         BE    RELPRG                                                           
         CLI   BYPREAD,C'S'        PRG SUMMARY                                  
         BE    RELSUM                                                           
         CLI   BYPREAD,C'Z'        DONE W/ALL RECDS --JUST EXIT                 
         BE    ENDJOB                                                           
*                                                                               
         L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'400'     HEADER                                       
         CLI   BYPREAD,1           TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
*                                                                               
RDTEST   GET   IN1,(RC)            GET NEXT RECORD                              
         CLI   MITORIG,C'0'        ONLY HANDLE ORIG DATA FOR NOW                
         BNE   RDTEST                                                           
         CLI   BYPREAD,X'FF'       1ST-TIME-THRU? (SHOULD BE RDR)               
         BNE   READ40                                                           
         CLC   MITSEQ,=C'00'       TEST FOR REPORT DESCR RECORD (RDR)           
         BE    RDR                                                              
         DC    H'0'                RDR NOT FOUND                                
*                                                                               
READ40   DS    0H                  DETERMINE/BRANCH TO RECD TYPE                
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         DROP  R1                                                               
         GOTO1 (RF),DMCB,VWTYPTTB                                               
         ICM   R7,15,DMCB                                                       
*                                                                               
         USING VWTYPTD,R7                                                       
READ41A  CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+12                                                             
         LA    R7,VWTYPTL(R7)                                                   
         B     READ41A                                                          
         MVC   KEYSRC,VWTKSRC      SAVE KEY SOURCE HERE                         
         MVC   SVVCR,VWTVCR        SAVE VCR                                     
*                                                                               
         CLC   MITSEQ,=C'00'       TEST FOR REPORT DESCR RECORD (RDR)           
         BE    RDR                                                              
         CLC   SAVEPNUM,MITPRG     WHEN PRG# CHANGES-RESET VARIABLES            
         BNE   READ42              SAME                                         
         CLC   SAVEBIT,MITBREAK    SAME BRKOUT/SPECIAL?                         
         BNE   READ42                                                           
         CLI   TAPETYP,C'S'        ADDL TEST FOR SYN                            
         BNE   READ50              NOT SYN, GO PROCESS RECD                     
         CLC   SAVESTYP,MITAUDTY   BREAK FOR GAA SWITCH                         
         BE    READ50              PROCESS RECD                                 
*                                                                               
READ42   MVC   SAVEPNUM,MITPRG     RESET FLAGS                                  
         MVC   SAVEBIT,MITBREAK                                                 
         MVC   SAVESTYP,MITAUDTY   BREAK FOR GAA SWITCH                         
         XC    SAVEDATE,SAVEDATE                                                
         MVI   BYPASS01,0          RESET FLAG TO ACCEPT ALL DATA                
         MVI   VARIOUS,0                                                        
         MVC   VARS,ZEROS                                                       
         MVC   DAYS,ZEROS                                                       
         XC    SAVVAR(SAVVARLQ),SAVVAR                                          
*                                                                               
READ50   DS    0H                  PROCESS RECORD                               
*                                                                               
UNIVERS  CLC   MITSEQ,=C'01'       UNIVERSE RECORD?                             
         BNE   UNVREL                                                           
         CLI   MITREC,C'H'         UNIVERSE RECORDS                             
         BE    HUE                 HOUSHOLD UNIV RECD                           
         CLI   MITREC,C'P'                                                      
         BE    PREC                DEMOS FOR UNIV RECD                          
*                                                                               
UNVREL   CLI   PRVUNV,1            RELEASE UNIVERSE RECD?                       
         BE    RELUNV              YES                                          
*                                                                               
SAMPLCNT CLC   MITSEQ,=C'02'         SAMPLE COUNTS?                             
         BE    READ20              YES--IGNORE FOR NOW                          
*                                                                               
USAGE    DS    0H                                                               
         CLC   MITSEQ,=C'03'         USAGE RECORDS?                             
         BNE   PROGRAM                                                          
         MVI   RECD,C'3'           THIS IS A USAGE RECD                         
         CLI   MITREC,C'H'                                                      
         BE    H3R                 HOUSHOLD USAGE RECD                          
         CLI   NOTAVAL,1           IGNORE THE NEXT P RECORD?                    
         BE    READ20              YES                                          
         CLI   MITREC,C'P'                                                      
         BE    PREC                DEMO USAGE RECD                              
         DC    H'0'                                                             
*                                                                               
PROGRAM  DS    0H                                                               
         CLC   MITSEQ,=C'04'         PROGRAM RECORDS?                           
         BE    *+6                 NO                                           
         DC    H'0'                UNKNOWN RECD TYPE                            
         MVI   RECD,C'4'           THIS IS A PROGRAM RECD                       
         CLI   PRVUSGSW,0            RELEASE LAST USAGE RECD?                   
         BNE   RELUSG              YES   (WE WILL RTN TO 'PROGRAM')             
         CLC   MITAVG,=C'01'       SINGLE WEEK AVG?                             
         BNE   PROG5                                                            
         CLC   SAVEDATE,MITSTART   TEST BYPASS FLG IF SAME DATES?               
         BNE   PROG5               DIFFERENT START-END DATES                    
         CLI   BYPASS01,1          BYPASS 1-WK AVG (W/ONLY 1 DAY IN IT)         
         BE    READ20                                                           
PROG5    MVI   BYPASS01,0          DTS/WKS DIFF->DIFF AVG->RESET FLAG           
         XC    SAVEDATE,SAVEDATE                                                
         CLI   MITREC,C'D'                                                      
         BE    D4RTN                                                            
         CLI   NOTAVAL,1           SKIP ALL RECDS ASSOC TO THIS PRG             
         BE    READ20                                                           
         CLI   MITREC,C'H'         HALF HOUR DETAIL                             
         BE    H4REC                                                            
         CLI   NOTAVAL,2           SKIP 1/2HR P-RECS (ASSOC W/H-RECS)           
         BE    READ20              READ NEXT RECORD                             
         CLI   MITREC,C'P'         PERSONS ESTIMATES                            
         BE    PREC                                                             
         B     READ20                                                           
*                                                                               
READ90   MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*RDR -  PROCESS REPORT DESCRIPTOR RECORD                                        
***********************************************************************         
RDR      DS    0H                                                               
         MVC   TAPWKMKT,=H'207'    NAD WKLY MOVIEGOER                           
         CLC   =C'MOVIE GOER REPORT',RDRRPT+4                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TAPETYP,C'N'        NAD                                          
         CLC   =C'NSS ',RDRRPT                                                  
         BNE   *+8                                                              
         MVI   TAPETYP,C'S'        NAD/NSS                                      
         GOTO1 VNETWEEK,DMCB,MITEND+1,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         MVC   TAPEBK,HALF                                                      
         MVI   BYPREAD,0           SET TO READ NEXT RECORD                      
         MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*HUE   -  SAVE AWAY UNIVERSE HOMES                                              
**********************************************************************          
HUE      DS    0H                                                               
         CLC   MITTYPE(4),=C'UES '                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVMPROJ,UNVEST       SAVE USA HUT UNIVERSE                       
         MVC   SVPROJ,UNVEST      MKT HUT=USA HUT                               
*                                                                               
HUE10    MVI   PRVUNV,1            SET SWITCH TO REL UNV RECD                   
         LA    RE,INTKEY                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   INTRTYP,C'P'        TIME PERIOD P-RECD                           
         MVC   INTBOOK,TAPEBK                                                   
         MVC   INTIBOOK,TAPEBK                                                  
         MVI   WKSFLG,C'W'         SET MEDIA IN KEY FOR WKLY                    
         MVC   INTSTA(5),=C'UUUUN' UNIVERSE                                     
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
*                                                                               
HUEX     B     READ20              PROCESS NEXT RECORD                          
         EJECT                                                                  
**********************************************************************          
* DUE -  DEMO  UNIVERSE ESTIMATES RECORD                                        
*        THREE RECDS FOR TOTAL HISP UNIV AND TWO FOR EACH MKT BREAK             
*        SAVE AWAY IN DUNIVS                                                    
**********************************************************************          
*                                                                               
DUE      DS    0H                                                               
         CLC   MITTYPE(3),=C'UES'                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         PACK  DUB,MITDEMG         DEMO GROUP:01,21,41 & 501,521                
         CVB   R1,DUB                                                           
         STH   R1,DMCB+2           DMCB+2=DEMO GROUP                            
         PACK  DUB,MITMKTBR        MKT BREAK                                    
         CVB   R1,DUB                                                           
         STH   R1,DMCB             DMCB=MKT BREAK                               
         LA    R1,MIPDEM1                                                       
         ST    R1,DMCB+4           DMCB+4=DEMO CATEGORY ESTIMATE                
*                                                                               
         L     R1,=A(CRCITAB)      GET MKT (ROW) DISPL INTO TABLE               
DUE10    CLI   0(R1),X'FF'         SRCH FOR MKT BREAK IN R1=CRCITAB             
         BNE   *+6                                                              
         DC    H'0'                MARKET BREAK NOT FOUND                       
         CLC   DMCB(2),0(R1)       MATCH ON MKT #?                              
         BE    *+12                                                             
         LA    R1,4(R1)            TRY NEXT ONE IN TABLE                        
         B     DUE10                                                            
*                                                                               
DUE15    DS    0H                                                               
         SR    R7,R7                                                            
         ICM   R7,3,2(R1)            R1=SLOT # IN DUNIV                         
         MH    R7,SLOTLN           MKT (ROW) DISPLACEMENT                       
         LA    R7,DUNIVS(R7)                                                    
         ST    R7,DMCB+8                                                        
         OC    UVHUTUNV,ZEROS      SLOT HUT UNV FOR TOT USA                     
         PACK  DUB,UVHUTUNV                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,RIUSA*4(R7)      4BYTE BUCKETS                             
*        OC    UVHUTMKT,ZEROS      SLOT HUT UNV FOR MKT BRK                     
*        PACK  DUB,UVHUTMKT                                                     
*        CVB   R1,DUB                                                           
         STCM  R1,15,RIHOMES*4(R7)                                              
*                                                                               
         LA    R0,20               20 DEMOS IN RECORD                           
DUE20    L     R1,=A(SLOTTAB)      FIND DISPL TO DEMO CATEGORY                  
DUE22    CLI   0(R1),X'FF'                                                      
         BE    DUE30               SOME DEMO CATS ARE OPEN                      
         CLC   DMCB+2(2),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     DUE22                                                            
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,2(R1)                                                       
         SLL   R7,2                                                             
         A     R7,DMCB+8           ADD DEMO CATG DISP TO MKT DISPL              
         L     RF,DMCB+4           A(DEMO VALUE)                                
         PACK  DUB,0(9,RF)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,0(R7)                                                      
*                                                                               
DUE30    LA    RF,9(RF)            BUMP ADDR OF DEMO CATEGORY                   
         ST    RF,DMCB+4                                                        
         LH    R1,DMCB+2                                                        
         LA    R1,1(R1)            BUMP TO DEMO GROUP NUMBER                    
         STH   R1,DMCB+2                                                        
         BCT   R0,DUE20            PROCESS/SLOT ALL DEMOS IN THIS REC           
*                                                                               
DUEX     B     READ20                                                           
         EJECT                                                                  
***********************************************************************         
*RELUNV   RELEASE UNIVERSE RECORDS                                              
***********************************************************************         
RELUNV   DS    0H                                                               
         CLI   BYPREAD,C'U'        1ST TIME IN, WON'T BE 'U'                    
         BE    *+8                                                              
         BAS   RE,GENCATS          GENERATE SUMMARY MKTBRK CATS                 
         MVI   PRVUNV,0                                                         
         MVI   BYPREAD,C'U'        LOOP THRU RELSING ALL MKT BRKS               
         MVI   WKSFLG,C'W'                                                      
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         MVC   INTBOOK,TAPEBK                                                   
         MVC   INTIBOOK,TAPEBK                                                  
*                                                                               
RLUNV5   L     RE,=A(CRCOTAB)                                                   
RLUNV6   CLI   0(RE),X'FF'                                                      
         BE    RLUNVX              END OF MKT BRK LIST/RELS                     
         CLC   1(1,RE),RELMKTB                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLUNV6                                                           
         CLI   3(RE),0                                                          
         BNE   RLUNV10                                                          
         ZIC   RE,RELMKTB                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     RLUNV5                                                           
*                                                                               
RLUNV10  MVC   MKTBRK,3(RE)        MKT BREAK                                    
         MVC   INTBTYP,4(RE)                                                    
         XC    INTPNUM,INTPNUM                                                  
         ZIC   RE,RELMKTB          MOVE DEMOS TO INTERIM RECD                   
         SR    RE,RE                                                            
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)    MOVE IN DEMOS                                
         CLI   MKTBRK,167          PRINCIPAL                                    
         BNE   *+10                                                             
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,168                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56                                         
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,169                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56+48                                      
         XC    INTACCS+56(96),INTACCS+56                                        
         ZIC   RE,RELMKTB                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     BLDKEY              OUTPUT UNV RECDS                             
*                                                                               
RLUNVX   MVI   BYPREAD,0           DONE RELS ALL UNV RECDS                      
         MVI   RELMKTB,0                                                        
         MVI   SUMPTR,0                                                         
         B     READ40              PROCESS RECD IN RREC BUFFER                  
         EJECT                                                                  
* *********************************************************************         
* H3R -  HOUSEHOLD USAGE RECORD                                                 
*        ONE RECORD PER 1/2HR FOR EACH DAY PLUS MON-FRI FOR TOT UNV             
*        AND FOR EACH MKT BREAK                                                 
*              RECORD SEQUENCE CODE = 3                                         
*              DATA TYPE CODE       = TVU                                       
*              RECORD TYPE          = H                                         
* *********************************************************************         
H3R      DS    0H                                                               
         CLC   MITTYPE(3),=C'TVU'  VERIFY RECD TYPE=USAGE RECD                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NOTAVAL,0           INIT FLAG TO ACCEPT ALL DATA                 
*                                                                               
H3RTN    DS    0H                                                               
         MVI   WKSFLG,C'W'         MEDIA IS ALWAYS W                            
         MVC   SVMPROJ,MIHPROJ     TAKE HUTS FOR ALL MKTS (INCL BREAKS)         
         CLC   MITMKTBR,=C'000'    ONLY PROCESS TOTAL SAMPLE DATA               
         BNE   READ20                                                           
         CLI   PRVUSGSW,0          DO WE HAVE A USG REC TO RELEASE?             
         BNE   RELUSG              YES- GO DO RELEASE PROCESSING                
*                                                                               
H3R10    MVI   PRVUSGSW,1          SET SWITCH TO RELEASE A USAGE RECD           
         MVC   SVPROJ,MIHPROJ      USA HOMES                                    
         XC    INTPNAME,INTPNAME                                                
         LA    RE,H3DAYTAB                                                      
H3R15    CLI   0(RE),X'FF'                                                      
         BE    H3R20                                                            
         CLC   MITRECTR,0(RE)                                                   
         BE    H3R20                                                            
         LA    RE,L'H3DAYTAB(RE)                                                
         B     H3R15                                                            
*                                                                               
H3R20    MVC   INTDYBIT,1(RE)      INTERNAL DAY CODE                            
         MVC   INTDAYWK,2(RE)                                                   
         MVC   INTPNAME(4),3(RE)   MOVE IN ALPHA DAY                            
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
         MVI   INTDUR,30                                                        
         MVC   INTIBOOK,TAPEBK                                                  
         MVC   INTBOOK,TAPEBK                                                   
         MVI   AVGWKS,0                                                         
         XC    STDATE,STDATE                                                    
H328A    CLC   MITAVG,=C'00'       INDIVIDUAL DAY AVG?                          
         BE    *+10                                                             
         CLC   MITAVG,=C'01'       ONE WK AVG?                                  
         BE    *+6                                                              
         DC    H'0'                MULTI WK AVGS N/A ON WKLY FILE               
*                                                                               
*CONVERT START TIME FOR INTPNAME FIELD - CHAR OUTPUT                            
H3R29    PACK  DUB,MITHOUR(2)                                                   
         CVB   RF,DUB                                                           
         STC   RF,DUB              DUB=HOUR 06(6AM)-29(5AM)                     
         CLI   DUB,24              24-29= 12AM-5AM                              
         BL    H3R35                                                            
         SH    RF,=H'24'                                                        
         STC   RF,DUB                                                           
         BNZ   H3R30                                                            
         MVI   DUB,12              24=12 MIDNIGHT(AM)                           
H3R30    MVI   DUB+1,C'A'          SET TO AM                                    
         B     H3R38                                                            
*                                                                               
H3R35    CLI   DUB,12                                                           
         BL    H3R30               AM                                           
         SH    RF,=H'12'                                                        
         STC   RF,DUB                                                           
         BNZ   *+8                                                              
         MVI   DUB,12              12=NOON (PM)                                 
         MVI   DUB+1,C'P'          SET TO AM                                    
*                                                                               
H3R38    MVI   DUB+2,C'M'          DUB=HOUR   DUB+1=AM/PM                       
         MVI   INTPNAME+4,C' '                                                  
         MVI   INTPNAME+7,C':'                                                  
         MVC   INTPNAME+8(2),MITMIN    MINUTE                                   
         MVC   INTPNAME+10(2),DUB+1     AM/PM                                   
         ZIC   RE,DUB              RE=START HOUR FOR INTPNAME                   
         LA    RF,INTPNAME+5                                                    
         EDIT  (RE),(2,0(RF))       OUTPUT START HOUR                           
         CLI   INTPNAME+5,C' '                                                  
         BNE   *+14                                                             
         MVC   INTPNAME+5(6),INTPNAME+6                                         
         MVI   INTPNAME+11,0                                                    
*                                                                               
*COMPUTE MILITARY TIME FOR NUMERIC MANIPULATIONS                                
         PACK  DUB,MITHOUR(4)                                                   
         CVB   RF,DUB                                                           
         STCM  RF,3,INTSTIM        START TIME IN MILITARY                       
         CLC   MITMIN,=C'30'       STARTS ON HALF HOUR                          
         BNE   *+12                                                             
         LA    RF,100(RF)          BUMP HOUR                                    
         SH    RF,=H'30'           ADJ HALF HOUR                                
         CLC   MITMIN,=C'00'       STARTS ON HOUR                               
         BNE   *+8                                                              
         LA    RF,30(RF)           BUMP MINUTES                                 
         STCM  RF,3,INTETIM        END TIME                                     
         PACK  DUB,MIHPRDUR(6)                                                  
         CVB   RF,DUB                                                           
         STCM  RF,3,INTCOV                                                      
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
*                                                                               
H3R60    MVC   INTSTA(5),=C'HUT N'  PLAIN USAGE RECORD (NOT A SUMARY)           
         XC    INTRSF,INTRSF                                                    
         MVI   INTADUR,30                                                       
         MVI   INTDUR,2                                                         
         MVI   INTDURM,30                                                       
         MVC   INTDURM2,=Y(30)                                                  
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
*                                                                               
H3RX     B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         EJECT                                                                  
***********************************************************************         
*RELUSG -  RELEASE TP SUMMARY USAGE DATA FOR NAD                                
***********************************************************************         
RELUSG   DS    0H                                                               
         CLI   BYPREAD,C'T'                                                     
         BE    *+8                                                              
         BAS   RE,GENCATS          ONLY GENERATE 1ST TIME IN                    
         MVI   BYPREAD,C'T'        TP SUMMARY DATA FOR NAD                      
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
*                                                                               
         ZIC   RE,SUMPTR                                                        
         LR    RF,RE                                                            
         MH    RE,=H'10'                                                        
         A     RE,=A(DPTTAB)                                                    
         CLI   0(RE),X'FF'                                                      
         BE    RLUSGX                                                           
*                                                                               
         CLI   INTDYBIT,X'7C'      BYPASS M-F RECDS                             
         BE    RLHISUM                                                          
         ZIC   R9,INTDYBIT         CHECK FOR ANY DAY OVERLAPS                   
         EX    R9,*+8                                                           
         B     *+8                                                              
         TM    8(RE),X'00'                                                      
         BZ    RLHISUM             NONE, BYPASS THIS DYPT SUMMARY               
*                                                                               
         CLC   INTSTIM(2),4(RE)                                                 
         BL    RLHISUM                                                          
         CLC   INTSTIM(2),6(RE)                                                 
         BNL   RLHISUM                                                          
         STC   RF,SUMPTR                                                        
         MVC   INTPNUM,0(RE)       SET DAYPART NUMBER                           
*                                                                               
RLUSG2B  L     R9,=A(DPNAME)                                                    
         XC    INTPNAME,INTPNAME                                                
RLUSG3   CLC   INTPNUM,0(R9)                                                    
         BE    RLUSG3A                                                          
         LA    R9,27(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     RLUSG3                                                           
*                                                                               
RLUSG3A  MVC   INTPNAME(25),2(R9)                                               
         L     R9,=A(DPTTAB)                                                    
         USING DPTTABD,R9                                                       
RLUSG3B  CLC   INTPNUM,0(R9)                                                    
         BE    RLUSG3C                                                          
         LA    R9,10(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   RLUSG3B                                                          
*                                                                               
RLUSG3C  MVI   INTRTYP,C'Q'         OUTPUT SUMARY USAGE RECS AS --'Q'--         
         MVC   INTSTIM,DPTSTIM     BUILD THE CONTROL DATA                       
         MVC   INTETIM,DPTETIM                                                  
         MVC   INTSTA(5),=C'HUTDN'  SUMMARY USAGE RECD                          
         MVC   INTPTYP,=C'  '                                                   
         MVI   INTDTYP,X'09'                                                    
         MVC   INTDAYWK,DPTDAYWK                                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,INTPNUM                                                     
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         MVC   INTPNTI,DUB+2       SAVE SUMMARY PRG# AS 5 CHAR PWOS             
*                                                                               
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         ZIC   RE,INTEQH                                                        
         ZIC   RF,INTSQH                                                        
*                                                                               
         SR    RE,RF                                                            
         STC   RE,INTDUR                                                        
         MH    RE,=H'15'                                                        
         STC   RE,INTDURM          SET DURATION MINUTES                         
         STCM  RE,3,INTDURM2       2-BYTE DURATION FIELD                        
         MVC   INTDYBIT,DPTDYBIT   SET RECORD DAY                               
         CLI   DPTDAYWK,X'80'      FOR PRIME                                    
         BNE   *+8                                                              
         MVI   INTDYBIT,X'7F'                                                   
         DROP  R9                                                               
*                                  RELEASE THE DEMOS                            
RLUSG4A  L     RE,=A(CRCOTAB)                                                   
RLUSG4B  CLI   0(RE),X'FF'                                                      
         BE    RLHISUM            NO RECD TO RELEASE.DO NEXT DAY CATGY          
         CLC   1(1,RE),RELMKTB                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLUSG4B                                                          
         CLI   3(RE),0             DON'T OUTPUT SLOT                            
         BNE   RLUSG4D                                                          
         ZIC   RE,RELMKTB          TRY THE NEXT ONE                             
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     RLUSG4A                                                          
*                                                                               
RLUSG4D  MVC   MKTBRK,3(RE)           SET OUTPUT SLOT                           
         MVC   INTBTYP,4(RE)                                                    
*                                                                               
         ZIC   RE,RELMKTB                                                       
         SR    RE,RE                                                            
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)                                                 
         CLI   MKTBRK,167          PRINCIPAL                                    
         BNE   *+10                                                             
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,168                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56                                         
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,169                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56+48                                      
         XC    INTACCS+56(96),INTACCS+56                                        
*                                                                               
*        LA    R1,INTACCS                                                       
*        AH    R1,SLOTLN           NEXT AVAIL SLOT AFTER DEMOS                  
*        ZIC   RE,RELMKTB          MOVE IN UNIVS                                
*        MH    RE,SLOTLN                                                        
*        LA    RE,DUNIVS(RE)                                                    
*        LH    RF,SLOTLN                                                        
*        BCTR  RF,0                                                             
*        EX    RF,*+8                                                           
*        B     *+10                                                             
*        MVC   0(0,R1),0(RE)       MOVE IN UNIVS AFTER DEMOS                    
*                                                                               
         ZIC   RE,RELMKTB                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
*&&DO                                                                           
*** REMOVED BY DEIS OCT15/2012 (THIS REALLY DOESN'T LOOK RIGHT)                 
         B     RLUSG5                                                           
*                                                                               
RLUSG5   L     R9,VCPRINT          RELEASE HUT RECORD                           
         USING DPRINT,R9                                                        
         UNPK  P+9(4),DUB+5(3)                                                  
         DROP  R9                                                               
*&&                                                                             
         B     BLDKEY                                                           
*                                                                               
RLHISUM  ZIC   RF,SUMPTR           NEXT SUMMARY CATAGORY                        
         LA    RF,1(RF)                                                         
         STC   RF,SUMPTR                                                        
         MVI   RELMKTB,0           PT TO 1ST MKT BREAK IN TABLE                 
         B     RELUSG                                                           
*                                                                               
RLUSGX   MVI   BYPREAD,0                                                        
         MVI   SUMPTR,0                                                         
         MVI   RELMKTB,0                                                        
         XC    INTSTA,INTSTA                                                    
         MVI   PRVUSGSW,0          CLEAR PREV USG RECD SWITCH                   
*                                 DETERMINE WHERE TO RTN CTL TO                 
         CLI   RECD,C'4'           IF THIS WAS =LAST= USG RECD, THEN            
         BE    PROGRAM              RETURN TO PROGRAM RECD                      
         CLI   RECD,C'3'           ELSE IT WAS JUST ANOTHER USAGE RECD          
         BE    H3R10                GO PROCESS THIS USAGE (H) RECD              
         CLI   RECD,C'Z'           EOF FOR TAPETYP=C'T'?                        
         BE    DONE2               YES, RELEASE 'Z' RECD FOR SRT RELS           
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* D4RTN- PROGRAM DESCRIPTOR RECORD EXTRACT FIELDS FOR INTERIM RECD              
*        ONLY PROCESS PDRS FOR TOTAL SAMPLE FOR EACH PROGRAM                    
*        IGNORE PDRS FOR EACH MKT BREAK                                         
* ********************************************************************          
D4RTN    DS    0H                                                               
         MVI   NOTAVAL,0           READ PROG RECDS                              
         CLI   MITORIG,C'0'        TEST CORRECTION RECORD                       
         BNE   D4SKIP              YES, DON'T HANDLE YET                        
         CLI   MITDNA,C'1'         TEST DATA NOT AVAIL FLAG                     
         BE    D4SKIP              NO DATA AVAILABLE                            
         CLC   MITAVG,=C'01'       ONLY PROCESS INDIV DAYS & 1 WK RECS          
         BH    D4SKIP              NO,  AVERAGE DATA                            
         CLC   MITHLFID,=C'00'     ONLY PROCESS TOT DURATION RECORDS            
         BNE   D4SKIP                                                           
         B     D4RTN1                                                           
*                                                                               
D4SKIP   MVI   NOTAVAL,1           BYPASS NEXT 'H' AND  'P' RECDS               
         B     READ20                                                           
*                                                                               
D4RTN1   CLC   MITMKTBR,=C'000'    TOTAL SAMPLE?                                
         BE    D4R01A              NO,IGNORE REST OF 'D' REC ON MKTBRKS         
         CLI   PRVHLF,0            RELEASE LAST PRG 1/2HR RECD?                 
         BNE   RELHLF              YES                                          
         MVI   BYPREAD,0                                                        
         MVC   SVMPROJ,MIDPROJ     SAVE -- MKT BRKS-- HUT                       
         MVC   INTVALS(L'DTOT),DTOT  INTREC FIELDS FROM TOT SAMPLE RECD         
         MVI   WKSFLG,C'W'         WKS FLAG IS ALWAYS W                         
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
         MVC   SVPROJ,DTOTHUT      RESTORE -HUT- FROM TOT SAMP RECD             
         MVC   SVPUT,DTOTPUT       RESTORE -PUT- FROM TOT SAMP RECD             
         B     READ20              READ NEXT RECD                               
*                                                                               
D4R01A   DS    0H                                                               
         CLI   PRVHLF,0                                                         
         BNE   RELHLF              RELEASE PREV HLF HOUR RECD                   
         CLI   PRVPRGSW,0          IF NOT THE 1ST PDR, RELEASE LAST ONE         
         BNE   RELPRG              GO OFF TO RELS RECD--THEN COME BACK          
         MVI   GAASW,C'N'                                                       
         CLI   MITAUDTY,C'2'                                                    
         BNE   *+8                                                              
         MVI   GAASW,C'Y'          INDICATE GAA DATA                            
         MVI   INTRTYP,C'Q'                                                     
         MVC   SVPROJ,MIDPROJ      TOTAL SAMPLE HUT                             
         MVC   SVMPROJ,MIDPROJ                                                  
         MVC   DTOTHUT,SVPROJ                                                   
         MVC   SVPUT,MIDUSHUT                                                   
         MVC   DTOTPUT,SVPUT                                                    
         LA    RE,MIDDAYS          SET UP SAVETIME TABLE FOR PDATA              
         MVC   PHUT,SVPROJ                                                      
         XC    SAVETIME(56),SAVETIME                                            
         LA    RF,SAVETIME                                                      
         LA    R0,7                                                             
D4R02    CLI   0(RE),C'0'                                                       
         BE    D4R03                                                            
         MVC   0(2,RF),MITHOUR     START HOUR                                   
         MVC   2(2,RF),MITMIN      START MINUTE                                 
         MVC   4(4,RF),MIDDUR      DURATION IN MINUTES                          
D4R03    LA    RE,1(RE)                                                         
         LA    RF,L'SAVETIME(RF)                                                
         BCT   R0,D4R02                                                         
*                                                                               
         MVC   DAYS,ZEROS                                                       
         MVC   VARS,ZEROS                                                       
         XC    SAVVAR(SAVVARLQ),SAVVAR                                          
*                                                                               
         CLC   MITAVG,=C'00'       INDIVIDUAL DAY DATA?                         
         BNE   D4R06                                                            
         CLI   MITBREAK,C'1'       BREAKOUT?                                    
         BE    D4R10                                                            
         CLI   MITSPC,C'1'         SPECIAL?                                     
         BE    D4R10                                                            
*                                                                               
         LA    RE,MIDDAYS          INDIV DAY DATA NOT BRKOUT OR SPCL            
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
D4R05    CLI   0(RE),C'0'                                                       
         BE    *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R05                                                         
         B     D4R10                                                            
         SPACE 2                                                                
*--AVERAGES: 01,04,05                                                           
D4R06    CLC   MIDDAYS,=C'1111100'  ---AVERAGED DATA --(01,04,05) M-F?          
         BE    D4R10                                                            
         CLC   MIDDAYS,=C'1111111'  M-S?                                        
         BE    D4R10                                                            
         CLC   MITTYPE(3),=C'SYN'  VARIOUS. SYN HAVE NO INDIV DAY               
         BNE   *+12                                                             
         MVI   VARIOUS,1                                                        
         B     D4R07                                                            
         CLI   VARIOUS,1                                                        
         BE    D4R07                                                            
         MVI   VARIOUS,1                                                        
         MVC   VARS,ZEROS          1ST TIME THRU                                
         XC    VARSTIME(56),VARSTIME                                            
*                                                                               
         CLC   DAYS,ZEROS          IF NOT YET SET, JUST GO SET IT               
         BE    D4R07               VARS W/OUT INDIV DAY RECDS                   
         LA    RE,DAYS             TEST NUMBER OF DAYS IN AVERAGE               
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CH    RF,=H'1'            IGNORE AVG WITH ONLY 1 DAY IN IT             
         BH    D4R07                                                            
         MVI   VARIOUS,0                                                        
         OC    INTVAR(SAVVARLQ),INTVAR                                          
         BNZ   D4RX                                                             
         MVI   BYPASS01,1                                                       
         MVC   SAVEDATE,MITSTART   SAVE START-END DATES                         
         B     D4RX                                                             
*                                                                               
D4R07    LA    RE,MIDDAYS                                                       
         LA    RF,VARS                                                          
         LA    R0,7                                                             
D4R08    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R08                                                         
         CLC   MITTYPE(3),=C'SYN'                                               
         BNE   *+10                                                             
         MVC   DAYS,VARS                                                        
         CLC   DAYS,ZEROS                                                       
         BNE   *+10                                                             
         MVC   DAYS,VARS                                                        
*                                                                               
D4R10    XC    PVALS,PVALS                                                      
         CLI   TAPETYP,C'N'        NETWORK NAD?                                 
         BNE   D4R12                                                            
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),MITPRG                        
         B     D4R13                                                            
*                                                                               
D4R12    DS    0H                                                               
         CLI   TAPETYP,C'S'        NSS?                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP2),MITPRG                        
*                                                                               
D4R13    MVC   PNUM,0(R1)          SAVE PROGRAM NUMBER                          
         MVC   PACK16(10),MITPRG   SAVE ORIG NTI#                               
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         MVC   PNTINUM,INTPNTI                                                  
                                                                                
         PACK  DUB,MIDPRDUR                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,PTOTDUR        AND DURATION                                 
         MVC   PDPT,MIDAYPT        TYPE DAYPART                                 
         MVC   PDPT2,MIDREPDP                                                   
         MVC   PNET,MITTYPE        SET UP R/S CALL LETTERS                      
         MVI   PNET+3,C' '                                                      
         CLC   MITTYPE(3),=C'SYN'  SYNDICATION                                  
         BNE   *+10                                                             
         MVC   PNET(3),MIDSYNOR    SET SYNDICATION CALL LETTERS                 
         MVC   PNAME,MIDPNAME                                                   
         MVC   PTITLE,MIDEPNAM                                                  
         MVC   PTYP,MIDPTYP                                                     
         MVC   PPREM,MIDPREM                                                    
         MVC   PSHORT,MIDSDUR                                                   
*                                                                               
         CLC   PNET,=C'UPN '       CALL LETTER CHANGE                           
         BNE   *+10                                                             
         MVC   PNET,=C'PAR '                                                    
         CLC   PNET,=C'ION '                                                    
         BNE   *+10                                                             
         MVC   PNET,=C'PAX '                                                    
         CLC   PNET,=C'UMA '                                                    
         BNE   *+10                                                             
         MVC   PNET,=C'TF  '                                                    
         CLC   PNET,=C'MMX '                                                    
         BNE   *+10                                                             
         MVC   PNET,=C'MFX '                                                    
         MVC   INTSTA(3),PNET      SAVE NETWORK IN KEY                          
         MVC   INTSTA+3(2),=C'PN'  NAD                                          
         CLI   TAPETYP,C'S'                                                     
         BNE   *+10                                                             
         MVC   INTSTA+3(2),=C' M'  NSS                                          
                                                                                
         PACK  DUB,MIDUSHUT        PROJECTION (XXX,XXX,XXX)                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,PWK1AUD        REACH                                        
*                                                                               
         PACK  DUB,MIDTEL                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1STAC       STATION COUNT                                
*                                                                               
         PACK  DUB,MIDCOVAR                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1COV        COVERAGE                                     
*                                                                               
         TM    MIDPROJ+(L'MIDPROJ-1),X'F0'                                      
         BO    *+10                                                             
         MVC   MIDPROJ,=16C'0'                                                  
         PACK  DUB,MIDPROJ                                                      
         CVB   RF,DUB                                                           
         ST    RF,SVAVGHI          SAVE AVG HOMES IMPS                          
*                                                                               
         LA    RE,TYPTAB           POINT RE AT TELECAST TYPE TABLE              
         LA    R0,TYPES            COUNTER                                      
D4R20    CLC   MITSPC,0(RE)        0=REGULAR   1/2=SPECIAL                      
         BNE   *+14                                                             
         CLC   MIDREP,1(RE)        WILL BE BLANK FOR NETWORK!                   
         BE    D4R30                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,D4R20                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
*                                                                               
D4R30    MVC   PWK1DTYP,2(RE)      X'09'=REGUALR  X'0C'=SPECIAL                 
*                                                                               
D4R31    CLI   400(RC),C'1'        TEST OPTIONAL DATA                           
         BNE   D4R34                                                            
         CLI   SVVCR,C' '          TEST VCR                                     
         BE    D4R33                                                            
         CLC   MITAVG,=C'00'       TEST AVERAGES                                
         BNE   D4R34                                                            
         CLI   MIDMULT,C' '        TEST MULTI-DAYS                              
         BE    D4R34                                                            
D4R33    OI    PWK1DTYP,X'80'      SET OPTIONAL BIT IN PHTDTYP                  
*                                                                               
D4R34    MVI   PWK1RSF,0                                                        
         CLI   MITBREAK,C'1'       TEST FOR REDUCED STATION                     
         BNE   *+8                                                              
         MVI   PWK1RSF,X'01'       REDUCED STATION INDICATOR- BREAK OUT         
         MVC   INTDPT,PDPT                                                      
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         MVC   INTPNUM,PNUM                                                     
         MVC   INTPTYP,PTYP                                                     
         MVC   INTPNAME,PNAME                                                   
         MVC   INTTITLE,PTITLE                                                  
         MVC   INTCOV,PTOTDUR                                                   
         MVI   INTMTYP,0                                                        
         MVI   INTSTYP,0           DEFAULT=0, BRK-OUT=B, SPCL=S                 
         CLI   MITBREAK,C'1'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,C'B'                                                     
         CLI   MITSPC,C'0'                                                      
         BE    *+8                                                              
         MVI   INTSTYP,C'S'                                                     
         MVC   INTAUD,PWK1AUD                                                   
         MVC   INTSTAC,PWK1STAC                                                 
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
         MVC   INTBOOK,TAPEBK      DEFAULT: BOOK=YYMM FOR MULTI WEEK            
         MVC   INTIBOOK,TAPEBK        "       "     "                           
*                                                                               
         CLC   MITAVG,=C'01'       SINGLE WEEK AVERAGE/INDIV DAY?               
         BH    D4R35               NO, MULTI WEEK                               
         MVI   SPCFLG,0                                                         
         CLI   MITBREAK,C'1'       FOR SPCLS & B/OUTS,OUTPUT:QNN & QWN          
         BE    *+12                                                             
         CLI   MITSPC,C'0'                                                      
         BE    *+8                                                              
         MVI   SPCFLG,1                                                         
*                                                                               
D4R34A   DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,MITSTART+1),(0,WORK)                             
         GOTO1 VNETWEEK,DMCB,WORK,VGETDAY,VADDAY                                
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
         MVI   WKSFLG,C'W'         SET MEDIA IN KEY FOR WEEKLY                  
*                                                                               
D4R35    MVI   PRVPRGSW,1                                                       
         LA    RE,MIDDAYS          POINT TO DAY FIELDS                          
D4R42    LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
*                                                                               
D4R50    CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    *+6                                                              
         OR    RF,R5               UPDATE CUMULATIVE BITS                       
         LA    RE,1(RE)                                                         
         SRL   R5,1                NEXT DAY                                     
         BCT   R0,D4R50                                                         
*                                                                               
         STC   RF,INTDAYWK         SAVE BITS                                    
         MVI   BYTE,X'90'          SET INTERNAL DAY CODE TO VAR                 
         LA    RE,NETDAYTB                                                      
         LA    R0,NETDAYS                                                       
         CLC   INTDAYWK,0(RE)      TEST BITS VS. TABLE                          
         BE    *+16                                                             
         LA    RE,L'NETDAYTB(RE)                                                
         BCT   R0,*-14                                                          
         B     *+10                                                             
         MVC   BYTE,1(RE)          INTERNAL DAY CODE                            
*                                                                               
         MVC   PDAY1,BYTE                                                       
         MVC   PDAY1BIT,INTDAYWK   NOT INTERNAL CODE                            
*                                                                               
         CLC   MITAVG,=C'04'       --MULTI-WEEK ONLY (04/05)--                  
         BL    D4R60               NO (00/01)                                   
         LA    R1,MIDWKS                                                        
         CLI   0(R1),C'0'          WEEK 1                                       
         BE    *+8                                                              
         OI    PWKBIT,X'08'                                                     
         CLI   1(R1),C'0'          WEEK 2                                       
         BE    *+8                                                              
         OI    PWKBIT,X'04'                                                     
         CLI   2(R1),C'0'          WEEK 3                                       
         BE    *+8                                                              
         OI    PWKBIT,X'02'                                                     
         CLI   3(R1),C'0'          WEEK 4                                       
         BE    *+8                                                              
         OI    PWKBIT,X'01'                                                     
         OC    INTDAYWK,PWKBIT                                                  
*                                                                               
D4R60    GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
*                                                                               
         MVI   INTRTYP,PMCODEQU    -Q-                                          
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         MVC   INTDURM,PWK1DURM    SHORT DURATION IN MINUTES (MAX 240)          
         MVC   INTDURM2,PWK2DURM   LONG DURATION IN MINUTES                     
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
*                                                                               
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
         SR    RF,RF                                                            
         ICM   RF,3,PWK2DURM       DURATION IN MINUTES                          
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         STC   R0,INTEQH                                                        
*                                                                               
         EJECT                                                                  
*              ADD DURATION IN MINUTES TO START TIME (MILITARY) TO              
*              GET END TIME                                                     
         SR    R0,R0                                                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM2         DURATION IN MINUTES                        
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CH    R0,=H'60'           TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SH    R0,=H'60'           SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MH    R1,=H'100'          HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INTETIM        END TIME                                     
*                                                                               
         CLC   MITAVG,=C'00'       ---INDIVIDUAL DAY DATA ONLY----              
         BNE   D4R77                                                            
         MVC   VARS,ZEROS                                                       
         LA    RE,MIDDAYS          USE INDIVIDUAL DAY(S) FOR INTVAR             
         LA    RF,SAVVAR           SET UP START TIMES IN VAR SAVE AREA          
         USING INTVARD,RF                                                       
         LA    R0,7                                                             
D4R74    CLI   0(RE),C'0'                                                       
         BE    D4R76                                                            
         MVC   INTVSQ,INTSQH       START QH                                     
         MVC   INTVEQ,INTEQH       END QH                                       
         PACK  DUB,MIDDUR                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,INTVLDUR       LONG DURATION IN MINUTES                     
*                                                                               
         CH    R1,=H'240'          4-HOUR MAXIMUM SUPPORTED                     
         BNH   *+8                                                              
         LH    R1,=H'240'                                                       
         STC   R1,INTVSDUR         SHORT DURATION IN MINUTES (MAX 240)          
*                                                                               
         MVC   INTVSTIM,INTSTIM    START TIME                                   
         MVC   INTVETIM,INTETIM    END TIME                                     
D4R76    LA    RE,1(RE)                                                         
         LA    RF,INTVARLQ(RF)                                                  
         BCT   R0,D4R74                                                         
         DROP  RF                                                               
*                                                                               
         CLC   MITTYPE(3),=C'SYN'  CAN BE VAR FOR SYND.                         
         BNE   D4R80                                                            
*                                                                               
         EJECT                                                                  
D4R77    CLI   VARIOUS,1           VARIOUS--FOR AVGS                            
         BNE   D4R80                                                            
         LA    RE,SAVETIME         ACCUMULATE INDIVIDUAL VAR DAY(S)             
         LA    RF,VARSTIME             INTO VARSTIME                            
         LA    R0,7                                                             
D4R78    OC    0(8,RE),0(RE)                                                    
         BZ    *+10                                                             
         MVC   0(8,RF),0(RE)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,D4R78                                                         
*                                                                               
         CLC   VARS,DAYS           HAVE ALL VARIOUS DAYS PROCESSED              
         BNE   D4R90                                                            
D4R79    MVI   VARIOUS,0           RESET                                        
         MVC   DAYS,ZEROS          RESET                                        
         MVC   INTVAR(SAVVARLQ),SAVVAR                                          
         XC    SAVVAR(SAVVARLQ),SAVVAR                                          
         MVC   SAVETIME(56),VARSTIME                                            
         XC    VARSTIME(56),VARSTIME                                            
         LA    RE,VARS             GENERATE VAR DATA USING                      
         B     D4R42                   ACCUMULATED DAYS                         
*                                                                               
D4R80    DS    0H                                                               
*                                                                               
D4R90    MVC   INTDAYWK,PDAY1                                                   
         OC    INTDAYWK,PWKBIT                                                  
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/PREC                          
         MVC   DTOT,INTVALS                                                     
         MVC   DTOTHUT,SVPROJ      TOTAL SAMPLE HUT                             
         MVC   DTOTPUT,SVPUT       TOTAL SAMPLE PUT HOMES                       
         MVC   DWKSFLG,WKSFLG      SAVED WKS FLAG                               
*                                                                               
D4RX     MVI   BYPREAD,0           FOR PREC, SKIP PROCESSING AND                
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* H4R -  HALF HOUR PROGRAM RECDS                                                
*        ONE MONTHLY & 1 WKLY RECD FOR EACH PRG 1/2HR FOR TOTAL SAMPLE          
*        NOT AVAILABLE FOR MKT BREAKS                                           
*              RECORD SEQUENCE CODE = 4                                         
*              DATA TYPE CODE       = UNI,TEL,AFFILIATES                        
*              RECORD TYPE          = H                                         
* *********************************************************************         
H4REC    DS    0H                                                               
*                                                                               
         B     READ20              BYPASS ALL H4 RECDS!                         
*                                                                               
*        CLI   BYPASSH4,1          BYPASS ALL BUT 1ST H4 RECORD                 
*        BE    READ20                                                           
*        MVI   BYPASSH4,1                                                       
*        MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN                  
*                                                                               
         MVC   PACK16(10),MITPRG                                                
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         PACK  DUB,MITHLFID        HALF HOUR CODE                               
         CVB   RF,DUB                                                           
         BCTR  RF,0                CONVERT TO QUARTER HOUR                      
         SLL   RF,1                                                             
         SR    R1,R1               CLEAR ACCUM                                  
         PACK  DUB,MIHPDUR1        MINUTES IN FIRST QH                          
         PACK  DUB1,MIHPDUR2       MINUTES IN SECOND QH                         
         CP    DUB,=P'0'           TEST IF RAN IN FIRST QH                      
         BE    *+12                NO                                           
         LA    R1,1(R1)            YES-BUMP QH DURATION                         
         B     *+8                                                              
         LA    RF,1(RF)            BUMP START QH                                
         CP    DUB1,=P'0'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST IF PROGRAM RAN DURING HALF HOUR         
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEINT,INTVALS     SAVE FOR P4RTN                               
         MVI   BYPREAD,0           FOR BLDEC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
*                                                                               
*----                                                                           
         CLI   NOTAVAL,1           BYPASS ALL 1/2HRS?                           
         BE    H4RSKIP                                                          
*        CLI   PRVHLF,0            PREV 1/2HR RECD TO RELEASE?                  
*        BNE   RELHLF              YES                                          
*                                                                               
         MVI   NOTAVAL,0           NO, INIT FLAG TO ACCEPT ALL DATA             
         MVI   BYPREAD,0                                                        
         MVC   SVMPROJ,MIHPROJ     USA HUT HOMES                                
         MVC   SVPROJ,MIHPROJ                                                   
         MVC   SVPUT,MIHUSHUT      USA PUT HOMES                                
         XC    INTKEY(INTACCS-INTKEY),INTKEY                                    
         MVC   PACK16(10),MITPRG   SAVE ORIG NTI# ON 1/2HR RECD                 
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         CLI   MITBREAK,C'1'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,C'B'                                                     
         CLI   MITSPC,C'0'                                                      
         BE    *+8                                                              
         MVI   INTSTYP,C'S'                                                     
         MVC   INTSTA(3),MITTYPE   NETWORK'S CALL LETTERS                       
         MVC   INTSTA+3(2),=C'PN'                                               
         MVI   INTBTYP,0                                                        
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
         XC    MKTBRK,MKTBRK       TOTAL SAMPLE ONLY                            
         MVC   INTBOOK,TAPEBK      BOOK OF THE MONTH                            
         MVC   INTIBOOK,TAPEBK                                                  
         MVI   WKSFLG,C'N'                                                      
         MVI   AVGWKS,0                                                         
         XC    STDATE,STDATE                                                    
*                                                                               
         XC    SAVETIME(56),SAVETIME                                            
         LA    RE,MIHDAYS          POINT TO DAY FIELDS                          
         LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
         LA    R1,SAVETIME         SAVE TIME/DUR TO CONVERT LATER               
H4R22    CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    H4R25                                                            
         OR    RF,R5               UPDATE CUMULATIVE BITS                       
         MVC   0(2,R1),MITHOUR     START HOUR                                   
         MVC   2(2,R1),MITMIN      START MINUTE                                 
         MVC   4(4,R1),MIHDUR      DURATION                                     
H4R25    LA    RE,1(RE)                                                         
         SRL   R5,1                NEXT DAY                                     
         LA    R1,L'SAVETIME(R1)                                                
         BCT   R0,H4R22                                                         
         STC   RF,INTDYBIT         DAYS PRG RAN                                 
         LA    RE,H3DAYTAB         CONVERT TO INTERNAL CODES                    
H4DAY    CLI   0(RE),X'FF'                                                      
         BE    H4R28               VARIOUS DAYS                                 
         CLC   INTDYBIT,1(RE)                                                   
         BE    *+12                                                             
         LA    RE,L'H3DAYTAB(RE)                                                
         B     H4DAY                                                            
*                                                                               
H4R28    MVC   INTDAYWK,2(RE)      DAY & WK CODE/ DEFAULT=X'90'                 
         CLC   MITAVG,=C'01'       1-WK AVG? BYPASS IF ONLY 1 DAY IN IT         
         BH    H4R35               NO, 4/5 WK AVG                               
         BL    H4R30               NO INDIV DAY DATA                            
         CLI   INTDYBIT,X'7C'      M-F RECD?                                    
         BE    H4R30                                                            
         CLI   INTDYBIT,X'7F'      M-S RECD?                                    
         BE    *+8                                                              
         B     H4RSKIP             SKIP 1/2 HR RECD                             
*                                                                               
H4R30    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,MITSTART+1),(0,WORK)                             
         GOTO1 VNETWEEK,DMCB,WORK,VGETDAY,VADDAY                                
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        SET WEEKLY BOOK                              
         MVC   INTIBOOK,HALF                                                    
         MVI   WKSFLG,C'W'                                                      
         B     H4R35                                                            
*                                                                               
**  HR35 LA    R1,MIHWKS           SET ACTIVE WEEKS NIBBLE                      
*        XC    PWKBIT,PWKBIT                                                    
*        CLI   0(R1),C'0'          WEEK 1                                       
*        BE    *+8                                                              
*        OI    PWKBIT,X'08'                                                     
*        CLI   1(R1),C'0'          WEEK 2                                       
*        BE    *+8                                                              
*        OI    PWKBIT,X'04'                                                     
*        CLI   2(R1),C'0'          WEEK 3                                       
*        BE    *+8                                                              
*        OI    PWKBIT,X'02'                                                     
*        CLI   3(R1),C'0'          WEEK 4                                       
*        BE    *+8                                                              
*        OI    PWKBIT,X'01'                                                     
*        OC    INTDAYWK,PWKBIT     SET WEEKS IN DAY/WK INDICATOR                
*                                                                               
H4R35    GOTO1 PDATA,DMCB,SAVETIME,INTDYBIT                                     
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         CLI   PWK1DURM,8          BYPASS DURATIONS < 8 MIN                     
         BL    H4RSKIP             SKIP THIS 1/2HR RECD                         
         MVC   INTDURM,PWK1DURM    SHORT DURATION IN MINUTES (MAX 240)          
         MVC   INTDURM2,PWK2DURM   LONG DURATION IN MINUTES                     
         MVC   INTADUR,PWK1DURM    SHORT AVG DUR                                
         SPACE 2                                                                
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH   P-RECD, PRG#=START QTR HOUR                
*                                                                               
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
         SR    RF,RF               DURATION IN MINUTES                          
         ICM   RF,3,PWK2DURM                                                    
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DUR OF AT LEAST ONE                 
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               START QH PLUS DUR                            
         STC   R0,INTEQH                                                        
*                                                                               
         SR    R0,R0               CALCULATE INTETIM                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM2       DURATION IN MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CH    R0,=H'60'           TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SH    R0,=H'60'           SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MH    R1,=H'100'          HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INTETIM        END TIME                                     
*                                                                               
         PACK  DUB,MIHPRDUR(6)     COVERAGE                                     
         CVB   RF,DUB                                                           
         STCM  RF,3,INTCOV                                                      
         PACK  DUB,MIHUSHUT        1/2HR US HUT PROJ (XXX,XXX,XXX)              
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,INTAUD         REACH                                        
         MVC   INTDPT,PDPT         SET IN PRG DESC D-RECD (D4RTN)               
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         CLI   INTVAR,0            ASSUMING NO VARS POSSIBLE                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   INTVAR+2(2),PNUM    SAVE ACTUAL PROGRAM NUMBER                   
         MVC   INTPTYP,PTYP                                                     
         MVC   INTPNAME,PNAME                                                   
         MVC   INTTITLE,PTITLE                                                  
         MVI   INTMTYP,0                                                        
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
         MVC   SAVEINT(L'SAVEINT),INTVALS   H-RECD'S INTERIM REC FLDS           
         MVI   PRVHLF,1            SET SWITCH TO RELEASE A USAGE RECD           
         B     H4RX                                                             
*                                                                               
H4RSKIP  MVI   NOTAVAL,2           SKIP FOLLOWING P-RECDS ONLY                  
*                                                                               
H4RX     B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         EJECT                                                                  
**********************************************************************          
* PREC - DEMO  RECORD                                                           
*              SEQUENCE CODE = 1,3,4                                            
*              DATA TYPE     = UES,TVU,PRG                                      
*              RECORD TYPE   = P                                                
* SLOT DEMOS FROM TAPE INTO INTERNAL BUCKETS IN DEMAREA                         
**********************************************************************          
PREC     DS    0H                                                               
         CLC   MITSEQ,=C'04'                                                    
         BNE   PREC20                                                           
         CLC   MITHLFID,=C'00'                                                  
         BNE   PRECX                                                            
         CLI   MITPUT,C'0'         ONLY USE DEMO DATA                           
         BNE   PRECX               BYPASS                                       
*                                                                               
PREC20   L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         PACK  DUB,MITDEMG                                                      
         CVB   R1,DUB                                                           
         STH   R1,DMCB+2                                                        
         PACK  DUB,MITMKTBR                                                     
         CVB   R1,DUB                                                           
         STH   R1,DMCB                                                          
         LA    R1,MIPDEM1                                                       
         ST    R1,DMCB+4                                                        
         BAS   RE,PROCDEMS                                                      
         MVI   BYPREAD,0                                                        
PRECX    B     READ20                                                           
         EJECT                                                                  
* *********************************************************************         
* BLDKEY -     BUILD PAV KEY- COME HERE WHEN INTERIM RECD IS COMPLETE           
* *********************************************************************         
BLDKEY   MVC   INTMRKT,TAPWKMKT    FASTNAD WKLY MKT NUMBER                      
         CLI   INTRTYP,PMCODEQU    TEST FOR 'Q' RECORD                          
         BE    BLD70                                                            
*                                                                               
BLD60    LA    R7,INTKEY           BUILD PAV KEY                                
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     -P- RECORD                                   
         MVC   PRMEDIA,WKSFLG      W=WKLY RECD                                  
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
BLD64    MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRBTYP+1(1),INTDAYWK  SORT BY: DAY--QTRHR--MKT BRK               
         MVC   PRBTYP+2(2),INTPNUM                                              
         MVC   PRBTYP+4(1),MKTBRK   MKT BREAK                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRDW+1(1),INTDUR    FORCE HIGHER DURATIONS FIRST                 
         XI    PRDW+1,X'FF'                                                     
         B     BLDX                                                             
         DROP  R7                                                               
*                                                                               
BLD70    LA    R7,INTKEY           BUILD 'Q' RECORD KEY                         
         USING PMKEY,R7                                                         
         MVI   PMCODE,PMCODEQU     -Q-                                          
         MVC   PMMEDIA,WKSFLG                                                   
         MVI   PMSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
BLD74    MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMBTYP+1(2),INTPNUM                                              
         MVC   PMBTYP+3(1),MKTBRK                                               
*                                                                               
         LA    RE,QSORTAB          SORT 'Q' RECORDS BY DAY...                   
         LA    R0,QSORTABS         M-S, M-F, VAR, MON...SUN                     
         MVC   PMRLEN(1),INTDAYWK                                               
         NI    PMRLEN,X'F0'                                                     
         CLC   PMRLEN(1),0(RE)                                                  
         BE    *+14                                                             
         LA    RE,L'QSORTAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   PMRLEN(1),1(RE)     USE FIELD FOLLOWING PMPNUM                   
         DROP  R7                                                               
*                                                                               
BLDX     DS    0H                                                               
         MVC   INTKEY+29(1),GAASW                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*RELHLF   RELEASE HALF HOUR PRG RECD                                            
***********************************************************************         
RELHLF   DS    0H                                                               
         MVI   PRVHLF,0            NO PREV RECD TO RELEASE                      
         MVI   BYPREAD,0           PROCESS CURRENT RECD IN BUFFER               
         MVI   RELMKTB,0           RESET                                        
         B     READ40                                                           
         EJECT                                                                  
***********************************************************************         
*RELPRG -      RELEASE PROGRAM RECORDS                                          
***********************************************************************         
RELPRG   DS    0H                                                               
         CLI   BYPREAD,C'P'                                                     
         BE    *+8                                                              
         BAS   RE,GENCATS                                                       
         MVI   BYPREAD,C'P'                                                     
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         MVC   INTBOOK,TAPEBK                                                   
         MVC   INTIBOOK,TAPEBK                                                  
*                                                                               
RELP10   L     RE,=A(CRCOTAB)                                                   
RELP12   CLI   0(RE),X'FF'                                                      
         BE    RELPRGX             DONE RELSG MKTBRK DATA, DO SUMMARYS          
         CLC   1(1,RE),RELMKTB                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RELP12                                                           
         CLI   3(RE),0                                                          
         BNE   RELP20                                                           
         ZIC   RE,RELMKTB                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     RELP10                                                           
*                                                                               
RELP20   MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         LA    RE,INTACCS          INIT/CLEAR BUFFER                            
         LA    RF,NDEMS*4*2        SLOTLN*2                                     
         XCEF                                                                   
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         ZIC   RE,RELMKTB          MOVE DEMOS TO INTERIM RECD                   
         SR    RE,RE                                                            
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)    MOVE IN DEMOS                                
         CLI   MKTBRK,167          PRINCIPAL                                    
         BNE   *+10                                                             
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,168                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56                                         
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,169                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56+48                                      
         XC    INTACCS+56(96),INTACCS+56                                        
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,SLOTLN           NEXT FREE SLOT AFTER DEMOS                   
         CLI   RELMKTB,0           TOTAL SAMPLE?                                
         B     RELP25                                                           
         LA    RE,PUTS             POINT TO PUT BUFFER                          
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)        MOVE IN PUTS (AFTER DEMOS)                  
*                                                                               
RELP25   ZIC   RE,RELMKTB          BUMP TABLE PTR AND GO RELS RECD              
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     BLDKEY                                                           
*                                                                               
RELPRGX  MVI   RELMKTB,0                                                        
         MVI   SUMPTR,0                                                         
         B     RELSUM                                                           
         EJECT                                                                  
* ********************************************************************          
* RELSUM -     RELEASE SUMARY RECORD --------PROGRAM RECORDS---------           
*********************************************************************           
RELSUM   DS    0H                                                               
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         CLI   INTSTYP,C'B'                                                     
         BE    RELSUMX             EXCLUDE BRKOUTS FROM SUMMARYS                
         MVI   BYPREAD,C'S'        CREATE SUMMARY                               
         ZIC   RE,SUMPTR                                                        
         LR    RF,RE                                                            
         MH    RE,=H'16'                                                        
*                                                                               
         CLC   TAPEBK,=X'6724'     DATE BEFORE SEP1/2003?                       
         BNL   *+12                                                             
         A     RE,=A(PTYPTAB)                                                   
         B     *+8                                                              
         A     RE,=A(PTYPTS03)     NEW TABLE BEGINNING SEP1/2003                
*                                                                               
         CLI   0(RE),X'FF'         DONE WITH PRG TYPES                          
         BE    RELSUMX                                                          
         MVC   INTBOOK,TAPEBK                                                   
         MVC   INTIBOOK,TAPEBK                                                  
         MVI   INTSTYP,0                                                        
         MVC   BYTE,INTDTYP        TEST REG OR SPECIAL                          
         NC    BYTE,8(RE)                                                       
         CLI   BYTE,0                                                           
         BE    INCRSUM                                                          
         CLI   BYTE,SP             FOR SPECIALS                                 
         BNE   *+12                                                             
         CLI   INTDURM,30          EXCLUDE DURATIONS OF UNDER 30 MIN            
         BL    INCRSUM                                                          
         CLI   PSHORT,C'Y'         AND SHORT DURATION PROGRAMS                  
         BE    INCRSUM                                                          
         CLC   0(2,RE),=C'  '      CHECK PROGRAM TYP (BLANK MEANS ALL)          
         BE    RELSUM1                                                          
*                                                                               
RELSUMA  CLC   0(2,RE),PTYP                                                     
*        BNE   INCRSUM                                                          
         BE    RELSUMD                                                          
*                                                                               
         CLC   INTBOOK,=X'6724'     DATE AFTER SEP1/2003?                       
         BL    INCRSUM                                                          
         L     R1,=A(XCLIST)        YES.SEARCH EXCLUDE LIST                     
RELSUMB  CLC   =X'FFFF',0(R1)                                                   
         BE    INCRSUM             NOT FOUND. DON'T USE IT                      
         CLC   0(2,RE),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,L'XCLIST(R1)                                                  
         B     RELSUMB                                                          
         LA    R9,2(R1)                                                         
RELSUMC  CLC   =C'  ',0(R9)        NO MORE EXCLUDES. I CAN KEEP IT              
         BE    RELSUMD                                                          
         CLC   =X'FFFF',0(R9)      END OF TABLE. I CAN KEEP IT                  
         BE    RELSUMD                                                          
         CLC   PTYP,0(R9)          EXCLUDE THIS PTYP?                           
         BE    INCRSUM             YES                                          
         LA    R9,2(R9)                                                         
         B     RELSUMC                                                          
*                                                                               
RELSUMD  OC    2(2,RE),2(RE)       CHECK SPOT LENGTH                            
         BZ    RELSUM1                                                          
         CLC   INTDURM,2(RE)                                                    
         BL    INCRSUM                                                          
         CLC   INTDURM,3(RE)                                                    
         BH    INCRSUM                                                          
*                                                                               
RELSUM1  CLC   11(3,RE),=C'   '    CHECK NETWORK                                
         BE    *+14                WAS *+10, WHY I DON'T KNOW                   
         CLC   11(3,RE),PNET                                                    
         BNE   INCRSUM                                                          
         LA    R1,4(RE)            CHECK DAYPART                                
*                                                                               
RELSUM2  CLI   0(R1),C' '          LOOP THRU DAYPT POSSIBILES IN TABLE          
         BE    INCRSUM                                                          
         CLI   0(R1),C'Z'          ACCEPT ANY DAYPART                           
         BE    *+10                                                             
         CLC   0(1,R1),PDPT        OR BE SPECIFIC                               
         BE    *+12                                                             
         LA    R1,1(R1)            TRY NEXT DAYPART TYPE IN TABLE ENTRY         
         B     RELSUM2                                                          
*                                                                               
         L     R1,=A(SPCTYPS)      SPC HANDLING FOR CERTAIN PRG TYPS            
RELS2AA  CLC   0(2,R1),=X'FFFF'    END OF TABLE?                                
         BE    RELS2AC                                                          
         CLC   9(2,RE),0(R1)       WAS THIS PRG TYPE IN TABLE                   
         BE    *+12                                                             
         LA    R1,L'SPCTYPS(R1)    NEXT ENTRY IN TABLE                          
         B     RELS2AA                                                          
*                                                                               
         ZIC   R9,2(R1)            R9=MASK IN TABLE                             
         ZIC   R1,INTDYBIT                                                      
         LA    R0,7                                                             
RELS2AB  STC   R9,DUB              SAVE MASK IN DUB                             
         STC   R1,DUB+1            SAVE RECD BITS IN DUB+1                      
         TM    DUB,X'01'           DOES TABLE ALLOW THIS DAY?                   
         BO    *+12                YES                                          
         TM    DUB+1,X'01'         DAY NOT ALLOWED-DOES RECD HAVE IT?           
         BO    INCRSUM             DISALLOW                                     
         SRL   R9,1                TEST NEXT BIT                                
         SRL   R1,1                                                             
         BCT   R0,RELS2AB          TEST ALL BITS                                
*                                                                               
RELS2AC  CLI   15(RE),C'Z'         SEND HUT ONLY                                
         BE    *+8                                                              
         CLI   11(RE),C' '         SPECIFIC NETWORK?                            
         BNE   *+10                                                             
         MVC   INTSTA(3),=C'HUT'                                                
         MVI   INTSTA+3,C'T'                                                    
         MVI   INTSTA+4,C'N'       NAD = N                                      
         CLI   TAPETYP,C'S'                                                     
         BNE   *+8                                                              
         MVI   INTSTA+4,C'M'       NSS = M                                      
*                                                                               
         STC   RF,SUMPTR                                                        
         SR    R1,R1                                                            
         ICM   R1,3,9(RE)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         STCM  R1,3,INTPNUM        SET OUTPUT PRG # & NAME (FROM TBLS)          
*                                                                               
         CLC   INTPNUM,=H'7500'    HANDLE SPECIAL INF                           
         BNE   RELSUM2B                                                         
         SR    RE,RE                                                            
         ZIC   R1,INTDYBIT         COUNT THE DAYS                               
         SR    R0,R0                                                            
         ZIC   R1,INTDYBIT                                                      
         SR    R0,R0                                                            
         SLL   R1,25                                                            
*                                                                               
RELSUM2A SLDA  R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    RE,1(RE)            BUMP COUNT OF DAYS                           
         LTR   R1,R1                                                            
         BNZ   RELSUM2A                                                         
         CH    RE,=H'1'                                                         
         BE    *+10                                                             
         MVC   INTPNUM,=H'7600'    MAKE INTO MULTI DAY                          
*                                                                               
RELSUM2B DS    0H                                                               
         L     R9,=A(PTDPNAME)     SET PRG TYPE NAME                            
         CLC   INTBOOK,=X'6724'    RECORD DATE AFTER SEP1/2003?                 
         BL    *+8                                                              
         L     R9,=A(PTDPNAS03)    YES. USE NEW TABLE                           
*                                                                               
         XC    INTPNAME,INTPNAME                                                
RELSUM3  CLC   INTPNUM,0(R9)                                                    
         BE    RELSUM3A                                                         
         LA    R9,27(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     RELSUM3                                                          
*                                                                               
RELSUM3A MVC   INTPNAME(25),2(R9)                                               
         L     R9,=A(PTYTAB)                                                    
         CLC   INTBOOK,=X'6724'    RECORD DATE AFTER SEP1/2003?                 
         BL    *+8                                                              
         L     R9,=A(PTYTAS03)     YES. USE NEW TABLE                           
*                                                                               
         USING PTYTABD,R9                                                       
RELSUM3B CLC   INTPNUM,0(R9)                                                    
         BE    RELSUM3C                                                         
         LA    R9,PTYLN(R9)                                                     
         CLI   0(R9),X'FF'                                                      
         BNE   RELSUM3B                                                         
RELSUM3C MVC   INTSTIM,PTYSTIM     BUILD THE CONTROL DATA                       
         MVC   INTETIM,PTYETIM                                                  
         MVC   INTDURM,PTYDUR                                                   
         XC    INTDURM2,INTDURM2                                                
         MVC   INTDURM2+1(1),PTYDUR   2-BYTE DURATION FIELD                     
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM2                                                    
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         STC   RF,INTDUR                                                        
         MVC   INTDYBIT,PTYDYBIT                                                
         MVC   INTPTYP,PTYPTYP                                                  
         MVC   INTDTYP,PTYDTYP                                                  
         MVC   INTDAYWK,PTYDAYWK                                                
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         DROP  R9                                                               
         SR    R1,R1                                                            
         ICM   R1,3,INTPNUM                                                     
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         MVC   INTPNTI,DUB+2       SAVE SUMMARY PRG# AS 5 CHAR PWOS             
*                                  RELEASE THE DEMOS                            
RELSUM4A L     RE,=A(CRCOTAB)                                                   
RELSUM4B CLI   0(RE),X'FF'                                                      
         BE    INCRSUM                                                          
         CLC   1(1,RE),RELMKTB                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RELSUM4B                                                         
         CLI   3(RE),0             DON'T OUTPUT SLOT                            
         BNE   RELSUM4D                                                         
         ZIC   RE,RELMKTB          TRY THE NEXT ONE                             
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     RELSUM4A                                                         
*                                                                               
RELSUM4D MVC   MKTBRK,3(RE)        SET OUTPUT SLOT                              
         MVC   INTBTYP,4(RE)                                                    
         LA    RE,INTACCS          INIT/CLEAR BUFFER                            
         LA    RF,NDEMS*4*2        SLOTLN*2                                     
         XCEF                                                                   
         ZIC   RE,RELMKTB          MOVE IN DEMOS                                
         SR    RE,RE                                                            
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)                                                 
         CLI   MKTBRK,167          PRINCIPAL                                    
         BNE   *+10                                                             
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,168                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56                                         
         XC    INTACCS+56(96),INTACCS+56                                        
         CLI   MKTBRK,169                                                       
         BNE   *+16                                                             
         MVC   INTACCS+8(48),INTACCS+56+48                                      
         XC    INTACCS+56(96),INTACCS+56                                        
*                                                                               
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,SLOTLN           NEXT FREE SLOT AFTER DEMOS FOR UNIVS         
         CLI   RELMKTB,0                                                        
         BNE   RELSUM6                                                          
         LA    RE,PUTS             POINT TO TOTAL SAMPLE PUTS                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS AFTER DEMOS                    
*                                                                               
RELSUM6  ZIC   RE,RELMKTB                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELMKTB                                                       
         B     BLDKEY                                                           
*                                                                               
INCRSUM  ZIC   RF,SUMPTR           NEXT SUMMARY CATAGORY                        
         LA    RF,1(RF)                                                         
         STC   RF,SUMPTR                                                        
         MVI   RELMKTB,0           PT TO TOP OF MKT BRK TABLE                   
         B     RELSUM                                                           
*                                                                               
RELSUMX  MVI   BYPREAD,0                                                        
         MVI   SUMPTR,0                                                         
         MVI   RELMKTB,0                                                        
         XC    INTSTA,INTSTA                                                    
         MVI   PRVPRGSW,0          CLEAR PREV PRG RECD SWITCH                   
         CLI   RECD,C'4'           ELSE IT WAS JUST ANOTHER PROGRAM RCD         
         BE    D4R01A               GO PROCESS THIS PRG (D) RECD                
         CLI   RECD,C'Z'           EOF FOR TAPEMKT=501?                         
         BE    DONE2               YES, RELS 'Z' RECD FOR SORT RELS             
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
* *******************************************************************           
* OUTSLOT- FINAL SLOTTING OF WGTD DEMOS FROM DEMAREA TO OUTPUT BUCKETS          
*   IN INTACCS TO PASS TO OPHASE.                                               
* *******************************************************************           
OUTSLOT  NTR1                                                                   
         LA    RE,INTACCS          CLEAR OUT INTACCS                            
         LA    RF,1000                                                          
         XCEF                                                                   
         CLC   INTSTA(5),=C'UUUUN' UNIVERSE RECORD SLOTS DIFFERENTLY            
         BE    OUTSL20                                                          
*                                                                               
         LA    RE,INTACCS          DESTINATION OF OUTPUT   ==DEMOS==            
         L     R7,=A(DEMAREA)      SOURCE                                       
         L     R9,=A(OUTDEMS)                                                   
*                                                                               
OUTSL10  CLI   0(R9),X'FF'         END OF TABLE?--DONE SLOTTING DEMOS           
         BE    OUTSL15             GO DO GAA'S                                  
         ZIC   RF,0(R9)            SOURCE BUCKET (INPUT)                        
         MH    RF,=H'4'            RF=SOURCE 4BYTE BUCKETS                      
         AR    RF,R7               SOURCE                                       
         ZIC   R1,1(R9)            DESTINATION BUCKET POSITION (OUTPUT)         
         MH    R1,=H'4'            R1=DESTIN 4BYTE BUCKETS                      
         AR    R1,RE               DESTIN IN INTACCS                            
         MVC   0(4,R1),0(RF)       MOVE DEMO TO APPROPRIATE OUTPUT SLOT         
         LA    R9,2(R9)            DO NEXT DEMO CATEGORY                        
         B     OUTSL10                                                          
         SPACE 1                                                                
*                                                                               
OUTSL15  DS    0H                  GAA SLOTS AFTER DEMOS & AFTER UNIVS          
         CLI   TAPETYP,C'S'        GAA FOR NSS SLOTS DIFFERENTLY                
         BNE   OUTSLX                                                           
         LA    RE,INTACCS+GAADSPQ  DESTINATION OF GAA'S                         
         L     R7,=A(DEMAREA)      SOURCE                                       
         AH    R7,SLOTLN           GAA BEGIN AT END OF DEMO LIST                
         L     R9,=A(OUTDEMS)                                                   
         B     OUTSL30                                                          
         SPACE 1                                                                
*                                                                               
OUTSL20  DS    0H                  PROCESS UNIVS                                
         LA    RE,INTACCS+UNVDSPQ  DESTINATION FOR OUTPUT  ==UNIVS==            
         L     R7,=A(DEMAREA)      SOURCE                                       
         L     R9,=A(OUTDEMS)                                                   
**       L     R9,=A(OUTUNVS)      TELLS WHAT UNIVS GO INTO OUTP CATAG          
         SPACE 1                                                                
*                                                                               
OUTSL30  CLI   0(R9),X'FF'         END OF TABLE?--DONE SLOTTING UNIVS           
         BE    OUTSLX              DONE                                         
         ZIC   RF,0(R9)            SOURCE BUCKET (INPUT)                        
         MH    RF,=H'4'            4BYTE BUCKETS                                
         AR    RF,R7               SOURCE                                       
         ZIC   R1,1(R9)            DESTINATION BUCKET POSITION (OUTPUT)         
         MH    R1,=H'4'            4BYTE BUCKETS                                
         AR    R1,RE               DESTIN IN INTACCS                            
         MVC   0(4,R1),0(RF)       MOVE DEMO TO APPROPRIATE OUTPUT SLOT         
         LA    R9,2(R9)            DO NEXT UNIV-DEMO CATEGORY                   
         B     OUTSL30                                                          
*                                                                               
*                                                                               
OUTSLX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* *******************************************************************           
* CNVWR - CALCULATE MISSING ACCUMULATED DEMO BUCKETS AND WEIGHTS.               
* SORT RECORD HOOK                                                              
* NON-NETWORK M-F ROTATORS EMITTED FROM INDIVIDUAL DAY RECORDS                  
* DUPLICATE HUT RECORDS DROPPED                                                 
* *******************************************************************           
*                                                                               
CNVWR    DS    0H                                                               
         MVI   BYPSORT,X'80'       SET NO RELEASE OF SORT REC                   
         L     R2,ASREC            R2 - USES INTERD DSECT                       
         MVC   GAASW,INTKEY+29                                                  
         MVC   HALF,INTCOV         COVERAGE                                     
         OC    HALF,HALF                                                        
         BNZ   *+8                                                              
         MVI   HALF+1,1                                                         
         CLI   CNWR1ST,1           1ST TIME THRU?                               
         BNE   CNV20               NO                                           
         MVI   CNWR1ST,0           YES                                          
         LA    R1,INTKEY                                                        
         CLI   INTRTYP,PMCODEQU    -Q- REC?                                     
         BNE   *+14                                                             
         MVC   MKTBRK,PMBTYP-PMKEY+3(R1)                                        
         B     *+10                                                             
         MVC   MKTBRK,PRBTYP-PRKEY+4(R1)                                        
         MVC   PRVRSF,MKTBRK       TREAT 1ST REC AS IF THERE WAS A REC          
         MVC   SAVEKEY,INTKEY      BEFORE IT-- IE. DON'T RELEASE IT YET         
         L     R1,APACKWT          CLEAR PACKED WGHT BUFFER ( P'0' )            
         LA    R0,NDEMS*2          PACKWT WILL HOLD DEMOS OF LAST REC           
         ZAP   0(8,R1),=P'0'       DEMS AND UNIVS (OR GAA'S)                    
         LA    R1,8(R1)            NEXT BUCKET IN PACKWT                        
         BCT   R0,*-10                                                          
         L     RE,=A(DEMAREA)      DEMAREA TEMP HOLDS FINAL DEMOS               
         LA    RF,NDEMS*2*4        (DEMS AND UNIVS)*4BYTE BUCKETS               
         XCEF                                                                   
         LA    RE,INTACCS          PT TO CURRENT KEY'S DEMO VALUES              
         B     CNV70               FOR FIRST TIME IN,SAVE DEMS- NO REL          
*                                                                               
CNV20    DS    0H                  NOT THE FIRST TIME IN                        
         LA    R1,INTKEY                                                        
         LA    RE,INTACCS          CURRENT KEY'S DEM VALS                       
         CLI   INTRTYP,PMCODEQU    -Q- REC?                                     
         BNE   *+14                                                             
         MVC   MKTBRK,PMBTYP-PMKEY+3(R1)                                        
         B     *+10                                                             
         MVC   MKTBRK,PRBTYP-PRKEY+4(R1)                                        
         CLC   INTKEY(19),SAVEKEY  DIFFERENT KEYS?                              
         BNE   CNV25               YES                                          
         CLI   INTKEY,C'P'         P-RECDS DO NOT MERGE ANY DATA                
         BE    CNV25               TREAT AS DIFF MKT BRKS                       
         CLC   MKTBRK,PRVRSF       NO, SAME MKT TYPE AS THE LAST KEY?           
         BE    CNV70               YES, WEIGHT AND SAVE (DON'T RELEASE)         
*                                                                               
* CONVERT PACKED DEMOS TO BINARY AND STORE IN DEMAREA                           
CNV25    MVC   PRVRSF,MKTBRK       NO, SAVE NEW TYPE OF RECD                    
         L     RE,=A(DEMAREA)      STORE RESULT OF PACKING HERE                 
         ZICM  RF,CURRDUR,2        DIVISOR IN PACK8                             
         LTR   RF,RF                                                            
         BNZ   *+16                                                             
         CLI   TAPETYP,C'S'                                                     
         BNE   *+8                                                              
         L     RF,=F'60000'                                                     
         CVD   RF,PACK8                                                         
         L     RF,APACKWT          DIVIDEND                                     
         LA    R0,NDEMS*2          LOOP CNTL= #DEMS AND GAA'S                   
*                                                                               
CNV30    ZAP   PACK16,0(8,RF)      8 BYTE PACKED DEMO BUCKET                    
         DP    PACK16,PACK8                                                     
         CVB   R1,PACK16           ONLY HIGH 8 BYTES!!!                         
         STCM  R1,15,0(RE)         STORE 4 BYTES IN DEMAREA                     
         LA    RE,4(RE)            NEXT DEMAREA BUCKET                          
         LA    RF,8(RF)            NEXT PACKWT BUCKET                           
         BCT   R0,CNV30                                                         
*                                                                               
* SWAP SAVEKEY WITH INTKEY IN ORDER TO RELEASE PREVIOUS RECD                    
         XC    SAVEKEY,INTKEY                                                   
         XC    INTKEY(L'SAVEKEY),SAVEKEY                                        
         XC    SAVEKEY,INTKEY                                                   
         LA    RE,SAVEREC                                                       
         MVC   0(NDEMS*4,RE),INTACCS    SAVE CURRENT RECD'S DEMOS               
         LA    RE,NDEMS*4(RE)                                                   
         MVC   0(NDEMS*4,RE),INTACCS+NDEMS*4    AND UNIVS                       
*                                   CALCULATE WEIGHT LATER (CNV70)              
*                                                                               
* SLOT OUTPUT DEMOS IN INTACCS                                                  
         BAS   RE,OUTSLOT          SLOT OUTPUT INTO RIGHT BUCKETS               
*                                                                               
         LA    RE,INTACCS          DESTINATION                                  
         CLC   INTSTA(5),=C'UUUUN'                                              
         BE    DCNV32                                                           
         B     DCNV34                                                           
         OC    INTACCS(ODHOMES*4),INTACCS                                       
         BNZ   DCNV34                                                           
         OC    INTACCS+GAADSPQ(ODHOMES*4),INTACCS+GAADSPQ                       
         BZ    CNV40               NO DEMOS OR GAA'S- DON'T RELEASE             
         B     DCNV34                                                           
*                                                                               
DCNV32   L     R1,=A(SVUTAB)                                                    
         MVC   0(1,R1),MKTBRK                                                   
         ZIC   RF,MKTBRK                                                        
         SHI   RF,167                                                           
         MH    RF,=H'57'                                                        
         AR    R1,RF                                                            
         LA    RE,INTACCS+UNVDSPQ                                               
         MVC   1(56,R1),0(RE)                                                   
         B     CNV35                                                            
*                                                                               
DCNV34   L     R1,=A(SVUTAB)                                                    
         MVC   0(1,R1),MKTBRK                                                   
         ZIC   RF,MKTBRK                                                        
         SHI   RF,167                                                           
         MH    RF,=H'57'                                                        
         AR    R1,RF                                                            
         LA    RE,INTACCS+UNVDSPQ                                               
         MVC   0(56,RE),1(R1)                                                   
         B     CNV35                                                            
*                                                                               
*                                                                               
CNV35    MVI   BYPSORT,0           RELEASE RECD                                 
         CLI   INTBTYP,C'Q'        PC USERS GET DIFF MKT BRK                    
         BNE   CNV40                                                            
         SR    R1,R1                                                            
         ICM   R1,3,INTMRKT                                                     
         AHI   R1,2            PC: NAD=202 NAW=203 NSS=402 NSSW=403             
         STCM  R1,3,INTMRKT                                                     
*                                                                               
CNV40    DS    0H                                                               
         L     R1,APACKWT          CLEAR TO P'0' FOR NEXT TIME                  
         LA    R0,NDEMS*2          DEMS+UNIVS                                   
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         XC    CURRDUR,CURRDUR                                                  
         L     RE,=A(DEMAREA)      CLEAR SAVE AREA FOR NEXT TIME                
         LA    RF,NDEMS*4*2                                                     
         XCEF                                                                   
         LA    RE,SAVEREC          RE PTS TO CURRENT KEY'S DEMOS                
*                                                                               
CNV70    DS    0H                                                               
         L     R1,APACKWT           PACKED WEIGHTED DEMOS                       
         CLI   GAASW,C'N'          IF GAA, DON'T ADD WGT TO TOTAL               
         BE    *+12                                                             
         LA    R1,NDEMS*8(R1)      APPEND GAA AFTER REG DEMOS                   
         B     CNV80                                                            
*                                                                               
         LH    RF,HALF             NON GAA DEMOS, ACCUMULATE WGT                
         AH    RF,CURRDUR                                                       
         STH   RF,CURRDUR                                                       
*                                                                               
* WEIGHT DEMOS AND STORE IN PACKWT                                              
CNV80    LH    RF,HALF                                                          
         CVD   RF,DUB1             DUB1=WEIGHT                                  
         LA    R0,NDEMS            REG DEMOS OR GAA SECTION                     
*                                                                               
CNV90    L     RF,0(RE)            0(RE)=CRNT DEMO (INTACCS/SAVEHREC)           
         CVD   RF,DUB              DUB=DECIMAL DEMO                             
         MP    DUB,DUB1+5(3)                                                    
         AP    0(8,R1),DUB                                                      
         LA    RE,4(RE)            NEXT DEMO                                    
         LA    R1,8(R1)            NEXT PACKED WEIGHTED DEMO                    
         BCT   R0,CNV90                                                         
*                                                                               
*        CLC   INTPNUM,=X'11D7'                                                 
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
* SAVE REL REC AND EXIT                                                         
CNVX     DS    0H                                                               
CNVX5    L     RF,AWREC                                                         
         XC    0(256,RF),0(RF)                                                  
         CLI   BYPSORT,X'80'       NO RELEASE                                   
         BE    CNVX1                                                            
         LR    RE,R2                                                            
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
CNVX1    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND ACTUAL RUN TIME AND AVERAGE DURATION                          
*              P1 =  A(SAVETIME)                                                
*              P2 =  A(DAY BITS FOR WEEK)                                       
***********************************************************************         
PDATA    NTR1                                                                   
         L     R6,0(R1)            ADDRESS OF SAVETIME                          
         L     RE,4(R1)            ADDRESS OF DAY BITS                          
         MVC   BYTE,0(RE)          DAY BITS                                     
         XC    FULL,FULL           CLEAR DAY COUNT FOR DURATION                 
         MVI   TMP,X'40'                                                        
         LA    R5,7                COUNTER                                      
         SR    R7,R7               CLEAR DURATION ACCUM                         
         XC    TIMTAB(7*L'TIMTAB),TIMTAB                                        
         XC    FULL1,FULL1         CLEAR TABLE ENTRY COUNT                      
*                                                                               
PDATA2   ZIC   RF,TMP                                                           
         EX    RF,*+8              TEST IF PROGRAM RAN ON DAY                   
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    PDATA4              NO                                           
         OC    0(8,R6),0(R6)       BYPASS IF NO TIMES                           
         BZ    PDATA4                                                           
*                                                                               
         PACK  DUB,0(2,R6)         START TIME                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,2(2,R6)         START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         PACK  DUB,4(4,R6)         DURATION IN MINUTES                          
         CVB   R0,DUB                                                           
         AR    R7,R0               UPDATE TOTAL DURATION                        
         L     RE,FULL                                                          
         LA    RE,1(RE)            UPDATE DAY COUNT                             
         ST    RE,FULL                                                          
         BAS   RE,UPTIME           UPDATE START TIME TABLE                      
*                                                                               
PDATA4   ZIC   RF,TMP              GET DAY                                      
         SRL   RF,1                NEXT DAY                                     
         STC   RF,TMP                                                           
         LA    R6,L'SAVETIME(R6)   NEXT DAY'S TIME DATA                         
         BCT   R5,PDATA2                                                        
*                                                                               
PDATA6   SR    RE,RE               CALCULATE AVERAGE DURATION                   
         LR    RF,R7               DURATION                                     
         SLDA  RE,1                                                             
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         STCM  RF,3,PWK2DURM       RETURN AVE LONG DURATION                     
         CH    RF,=H'240'          4-HOUR MAXIMUM SHORT DURATION                
         BNH   *+8                                                              
         LH    RF,=H'240'                                                       
         STC   RF,PWK1DURM         RETURN AVE SHORT DURATION                    
*                                                                               
         EJECT                                                                  
*                                  SORT TIMES IN DESCENDING ORDER               
         L     R0,FULL1            NUMBER OF ENTRIES IN TABLE                   
         GOTO1 VXSORT,DMCB,(X'FF',TIMTAB),(R0),3,1,2                            
         MVC   PWK1STIM,TIMTAB     FIRST OR MOST FREQUENT TIME                  
*                                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
***********************************************************************         
* ROUTINE TO UPDATE RUN TIME TABLE                                              
*              HALF CONTAINS TIME                                               
*              FULL1 CONTAINS TABLE ENTRY COUNT                                 
***********************************************************************         
*                                                                               
UPTIME   DS    0H                                                               
         LR    R0,RE                                                            
         LA    R1,7                COUNTER                                      
         LA    RE,TIMTAB                                                        
*                                                                               
UPTIME2  OC    0(L'TIMTAB,RE),0(RE)     TEST FOR E-O-T                          
         BZ    UPTIME4                  INSERT ENTRY                            
         CLC   HALF,0(RE)               TEST IF TIME IS IN TABLE                
         BE    *+12                     YES-BUMP FREQUENCY COUNT                
         LA    RE,L'TIMTAB(RE)                                                  
         BCT   R1,UPTIME2                                                       
*                                                                               
         ZIC   RF,2(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,2(RE)                                                         
         B     UPTIMEX                                                          
*                                                                               
UPTIME4  MVC   0(2,RE),HALF        ADD NEW ENTRY                                
         MVI   2(RE),1             SET FREQUENCY TO 1                           
         L     R1,FULL1                                                         
         LA    R1,1(R1)            INCREMENT TABLE ENTRY COUNT                  
         ST    R1,FULL1                                                         
*                                                                               
UPTIMEX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* *********************************************************************         
* PROCDEMS -   SLOT DEMOS IN INTERIM RECD                                       
*        ON ENTRY     DMCB(2)  = MARKET BREAK NUMBER                            
*                     DMCB+2(2)= DEMO GROUP NUMBER                              
*                     DMCB+4(4)= INPUT DEMO ADDRESS                             
* *********************************************************************         
PROCDEMS NTR1                                                                   
         CLC   MITSEQ,=C'04'       PRG RECD DEMOS?                              
         BNE   PROCDEM0                                                         
         OC    DMCB(2),DMCB        MKT BRK=0 = TOTAL SAMPLE?                    
         BNZ   PROCDEM0                                                         
         CLI   MITPUT,C'1'         PRG PUT?                                     
         BNE   PROCD1A             NO                                           
         LA    R7,PUTS             MOVE DEMOS INTO PUT BUFFER                   
         CLC   =C'00',MITHLFID     TOTAL DURATION?                              
         BE    *+8                                                              
         LA    R7,HPUTS            SAVE PUTS IN 1/2HR PUT BUFF                  
         B     PROCDEM2                                                         
*                                                                               
PROCD1A  CLC   =C'00',MITHLFID     NON-PUT & TOTAL DURATION?                    
         BE    PROCDEM0            YES, SAVE PRG EST IN DEMAREA                 
         LA    R7,HPRG             NON-PUT 1/2HR PRG EST--HPRG                  
         B     PROCDEM2                                                         
*                                                                               
PROCDEM0 L     R1,=A(CRCITAB)      GET THE CATAGORY INDEX                       
PROCDEM1 CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MARKET BREAK NOT FOUND                       
         CLC   DMCB(2),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PROCDEM1                                                         
*                                                                               
         LR    RE,R1                                                            
         SR    R7,R7                                                            
         ICM   R7,3,2(R1)          FIND OVERALL SLOT                            
         MH    R7,SLOTLN                                                        
         A     R7,=A(DEMAREA)      HAVE SLOT ADDRESS                            
*                                                                               
PROCDEM2 DS    0H                                                               
         ST    R7,DMCB+8           SAVE START ADDR WHERE TO STORE DEMS          
         LA    R0,20                                                            
         CLI   MITPUT,C'1'         PUT?                                         
         BNE   PROCDM2A                                                         
         OC    SVPUT,ZEROS                                                      
         PACK  DUB,SVPUT           SAVE PUT                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,RIHOMES*4(R7)                                              
         B     PROCDEM3                                                         
*                                                                               
PROCDM2A OC    SVPROJ,ZEROS                                                     
         PACK  DUB,SVPROJ                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,RIUSA*4(R7)                                                
*        OC    SVMPROJ,ZEROS                                                    
*        PACK  DUB,SVMPROJ                                                      
*        CVB   R1,DUB                                                           
         STCM  R1,15,RIHOMES*4(R7)                                              
*                                                                               
PROCDEM3 L     R1,=A(SLOTTAB)      FIND THE SLOT FOR DEMOS                      
PROCDEM4 CLI   0(R1),X'FF'         DEMO GROUP NOT FOUND                         
         BE    PROCDEM5            NEXT SLOT - SOME ARE OPEN                    
         CLC   DMCB+2(2),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PROCDEM4                                                         
         SPACE 1                                                                
         SR    R7,R7                                                            
         ICM   R7,3,2(R1)          GET THE DEMO SLOT                            
         SLL   R7,2                                                             
         A     R7,DMCB+8           NOW I HAVE AN OUTPUT SLOT                    
         L     RF,DMCB+4                                                        
         PACK  DUB,0(9,RF)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,0(R7)         SAVE THE CONVERTED VALUE                     
PROCDEM5 LA    RF,9(RF)                                                         
         ST    RF,DMCB+4           UPDATE INPUT POINTER                         
         LH    R1,DMCB+2           UPDATE FIELD NUMBER                          
         LA    R1,1(R1)                                                         
         STH   R1,DMCB+2                                                        
         BCT   R0,PROCDEM3                                                      
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*GENCATS- GENERATE MISSING BOOK CATAGORIES                                      
**********************************************************************          
GENCATS  NTR1                      GENERATE MISSING BOOK CATS                   
         L     R9,=A(CIGENTAB)                                                  
GENCATS2 CLI   0(R9),X'FF'                                                      
         BE    GENCATSX                                                         
         SR    R7,R7                                                            
         ICM   R7,3,0(R9)                                                       
         MH    R7,SLOTLN                                                        
         A     R7,=A(DEMAREA)                                                   
         XC    0(NDEMS*4,R7),0(R7)                                              
         SR    R1,R1                                                            
         ICM   R1,3,2(R9)            SOURCE SLOTS                               
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,4(R9)                                                       
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,6(R9)                                                       
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,8(R9)                                                       
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,10(R9)                                                      
         BAS   RE,SUMIT                                                         
         LA    R9,12(R9)                                                        
         B     GENCATS2                                                         
*                                                                               
GENCATSX XIT1                                                                   
**********************************************************************          
*SUMIT - ADDING CATAGORIES FOR GENCATS ROUTINE                                  
**********************************************************************          
SUMIT    LTR   R1,R1               NOT ACTIVE                                   
         BZR   RE                                                               
         LA    R6,0                                                             
         LA    R0,56                                                            
         MH    R1,SLOTLN                                                        
         A     R1,=A(DEMAREA)                                                   
         MVC   USAHOME,RIUSA*4(R1)                                              
*                                                                               
SUMIT1   DS    0H                  ADD  CATS TOGETHER                           
         LR    R5,R7                                                            
         AR    R5,R6                                                            
         ICM   RF,15,0(R5)         PICK UP RUNNING TOTAL                        
         A     RF,0(R1,R6)         ADD TO CURRENT CAT                           
         STCM  RF,15,0(R5)         UPDATE RUNNING TOTAL                         
SUMIT2   LA    R6,4(R6)                                                         
         BCT   R0,SUMIT1                                                        
         MVC   RIUSA*4(4,R7),USAHOME  SLOT HOMES (USA HOMES NOT ADDED)          
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
*                                                                               
DONE     DS    0H                                                               
         MVI   RECD,C'Z'           DONE                                         
         CLI   PRVPRGSW,0          RELEASE LAST PRG  RECD?                      
         BNE   RELPRG                                                           
*                                                                               
DONE2    MVI   INTKEY,C'Z'                                                      
         MVI   INTRTYP,C'Z'                                                     
         MVI   BYPREAD,C'Z'                                                     
         B     EXIT                                                             
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
**********************************************************************          
* ACOMWRK- COMMON WORK AREA BETWEEN INPUT AND OUTPUT PHASE                      
*          DO NOT MESS AROUND WITH ORDER OF VARIABLES--MAKE SURE                
*          OUTPUT PHASE AGREES WITH ANY CHANGES                                 
**********************************************************************          
COMWRK   DS    0H                                                               
AVGWKS   DC    X'00'               # WEEKS IN AVG: '00','01','04','05'          
STDATE   DS    CL6                 START DATE OF IREC                           
*                                                                               
**********************************************************************          
         EJECT                                                                  
* TABLE OF DAY BIT SETTINGS AND DAY CODES                                       
*                                                                               
NETDAYTB DS    0CL2                                                             
         DC    X'4010'             MON                                          
         DC    X'2020'             TUE                                          
         DC    X'1030'             WED                                          
         DC    X'0840'             THU                                          
         DC    X'0450'             FRI                                          
         DC    X'0260'             SAT                                          
         DC    X'0170'             SUN                                          
         DC    X'7C00'             M-F                                          
         DC    X'7F80'             M-SUN                                        
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                          
*                                                                               
H3DAYTAB DS    0CL7                       DAY CONVERSION TABLE                  
         DC    C'1',X'40',X'10',CL4'MON'                                        
         DC    C'2',X'20',X'20',CL4'TUE'                                        
         DC    C'3',X'10',X'30',CL4'WED'                                        
         DC    C'4',X'08',X'40',CL4'THU'                                        
         DC    C'5',X'04',X'50',CL4'FRI'                                        
         DC    C'6',X'02',X'60',CL4'SAT'                                        
         DC    C'7',X'01',X'70',CL4'SUN'                                        
         DC    C'8',X'7C',X'00',CL4'M-F'                                        
         DC    C'9',X'7F',X'80',CL4'M-SU'                                       
         DC    X'FF',X'80',X'90',CL4'VAR'                                       
         SPACE 3                                                                
         EJECT                                                                  
* TABLE OF DAY CODES AND SORT VALUES FOR 'Q' RECORDS                            
QSORTAB  DS    0XL2                                                             
         DC    X'8000'             M-S                                          
         DC    X'0001'             M-F                                          
         DC    X'9002'             VAR                                          
         DC    X'1003'             MON                                          
         DC    X'2004'             TUE                                          
         DC    X'3005'             WED                                          
         DC    X'4006'             THU                                          
         DC    X'5007'             FRI                                          
         DC    X'6008'             SAT                                          
         DC    X'7009'             SUN                                          
QSORTABS EQU   (*-QSORTAB)/L'QSORTAB                                            
*                                                                               
         SPACE 1                                                                
* TABLE OF TELECAST TYPES AND THEIR BIT SETTINGS USED FOR INTDTYP               
*                                                                               
TYPTAB   DS    0CL3                                                             
         DC    C'  ',X'09'         REGULAR - ORIGINAL (PROTECT)                 
         DC    C'0 ',X'09'         REGULAR - ORIGINAL                           
         DC    C' Y',X'11'         REGULAR - REPEAT   (PROTECT)                 
         DC    C'0Y',X'11'         REGULAR - REPEAT                             
         DC    C'1 ',X'0C'         SPECIAL - ORIGINAL                           
         DC    C'1Y',X'14'         SPECIAL - REPEAT                             
         DC    C'2 ',X'14'         SPECIAL - REPEAT                             
         DC    C'2Y',X'14'         SPECIAL - REPEAT                             
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
*                                                                               
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
* GENERAL AREAS                                                                 
*                                                                               
         DS    0D                                                               
PACK8    DS    PL8                                                              
PACK16   DS    PL16                                                             
*                                                                               
CNWR1ST  DC    X'01'                                                            
PRVRSF   DC    X'00'                                                            
CURRDUR  DC    H'00'                                                            
RELOFRST DC    X'01'                                                            
BYPREAD  DC    X'00'                                                            
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES                   
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
HUTSW    DC    X'00'               HUT RECORD GENERATION SWITCH                 
MTWTFSW  DC    X'00'               HUT RECORD (M-T-W-T-F USED)                  
PREVSTIM DC    X'FF'               HUT RECORD PREVIOUS START QTR HR             
*                                                                               
GAASW    DC    C'N'                GROSS AVG AUD SWITCH                         
RECD     DC    X'00'               TYPE OF RECD JUST READ FROM TAPE             
WKSFLG   DC    X'00'               '01'=WEEKLY DATA '04'/'05'=MULTI             
PRVUSGSW DC    X'00'               HAVE A PREVIOUS USAGE RECD                   
PRVPRGSW DC    X'00'               HAVE A PREVIOUS PROGRAM                      
PRVUNV   DC    X'00'               RELEASE LAST UNIVERSE RECD                   
PRVHLF   DC    X'00'               HAVE A PREVIOUS PRG HALF HOUR RECD           
PRVSTASW DC    X'00'               HAVE A PREVIOUS STATION RECD                 
RELMKTB  DC    X'00'               RELS MARKET BREAKS IN TABLE CNTR             
SUMPTR   DC    X'00'               SUMMARY TABLE CNTR                           
MI3FLAG  DC    X'00'                                                            
SPCFLG   DC    X'00'               SPECIAL PROG RECD                            
TAPETYP  DC    X'00'               TAPE TYPE: R=NAD S=NSS                       
TAPEBK   DC    XL2'0000'           KEY BOOK                                     
TAPWKMKT DS    H                   NADWKLY=021 NSSWKLY=401                      
MKTBRK   DS    X                   MKT BREAK NUMBER                             
QHSFRST  DS    C                                                                
QHRSW    DS    C                                                                
BYTE     DS    C                                                                
TMP      DS    C                                                                
SWITCH   DS    C                                                                
CNVWRSW  DS    C                                                                
USAHOME  DS    XL4                                                              
VARIOUS  DS    X                   1=VARIOUS                                    
DSCSTDT  DS    CL6                 REPORT DESCRIPTOR START DATE                 
DSCENDT  DS    CL6                 REPORT DESCRIPTOR END DATE                   
WEEK     DS    CL6                                                              
WKBOOK   DS    CL2                 SAVED NETWEEK BOOK                           
WKIBOOK  DS    CL2                 SAVED NETWEEK IBOOK                          
WKDAYWK  DS    CL1                 SAVED INTDAYWK                               
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
PHUT     DS    CL9                                                              
*                                                                               
KEYSRC   DS    C                                                                
SVVCR    DS    C                                                                
*                                                                               
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
*                                                                               
SAVETIME DS    7XL8                SAVE MITHOUR, MITMIN AND MIDDUR              
VARSTIME DS    7XL8                SAVE MITHOUR, MITMIN AND MIDDUR              
*                                                                               
SAVVAR   DS    7XL(INTVARLQ)       SAVE VAR INFO FROM INDIVIDUAL DAYS           
SAVVARLQ EQU   *-SAVVAR                                                         
*                                                                               
SAVEBIT  DS    XL2                                                              
*                                                                               
THREE    DS    CL3                                                              
         DS    0F                                                               
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD                          
SAVENET  DS    CL3                 SAVE NETWORK                                 
SAVEPNUM DS    CL10                SAVE PROGRAM NUMBER                          
SAVEDATE DS    CL14                SAVE START/END DATES OF RECS TO BYPS         
SAVESTYP DS    C                   SAVE AUDIENCE EST TYPE                       
SAVESYND DS    CL3                 SAVE SYNDICATOR                              
DTOT     DS    CL168                                                            
DTOTHUT  DS    CL9                                                              
DTOTPUT  DS    CL9                                                              
DWKSFLG  DS    C                                                                
**********                                                                      
         DS    0F                                                               
         DC    C'*STSREC*'                                                      
SAVERREC DS    CL150                                                            
         DC    C'*ENDREC*'                                                      
**********                                                                      
*                                                                               
TIMTAB   DS    7CL3                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* HOUSEHOLD USAGE SAVE AREA                                                     
*                                                                               
HUTSQH   DS    X                   NEXT QUARTER HOUR                            
HUTDUR   DS    X                                                                
HUTBOOK  DS    XL2                 KEY BOOK VALUE                               
HUTIBOOK DS    XL2                 INTERNAL BOOK VALUE                          
HUTSTA   DS    CL5                                                              
HUTDAYWK DS    X                                                                
*                                                                               
* PROGRAM DESCRIPTOR SAVE AREA                                                  
*                                                                               
PVALS    DS    0CL90                                                            
PNTINUM  DS    CL5                 ORIG NTI NUMBER                              
PNUM     DS    XL2                 PROGRAM NUMBER                               
PNET     DS    CL4                 NETWORK                                      
PDPT     DS    C                   NSI DAYPART (ALWAYS ZERO)                    
PDPT2    DS    CL2                 NTI DAYPART                                  
PTYP     DS    CL2                 PROGRAM TYPE                                 
PPREM    DS    C                   PREMIERE CODE                                
PSHORT   DS    C                   SHORT DURATION INDICATOR                     
PNAME    DS    CL25                PROGRAM NAME                                 
PTITLE   DS    CL32                EPISODE TITLE                                
*                                                                               
PWKBIT   DS    X                   WEEK BITS                                    
PDAY1    DS    X                   DAY CODE                                     
PDAY1BIT DS    X                   DAY BITS                                     
PTOTDUR  DS    XL2                 TOTAL DURATION                               
*                                                                               
PWK1STIM DS    XL2                 START TIME                                   
PWK1DURM DS    X                   SHORT DURATION IN MINUTES (MAX 240)          
PWK2DURM DS    XL2                 LONG DURATION IN MINUTES                     
PWK1AUD  DS    XL2                                                              
PWK1STAC DS    XL2                                                              
PWK1COV  DS    XL2                                                              
PWK1DTYP DS    X                                                                
PWK1RSF  DS    X                                                                
*                                                                               
         EJECT                                                                  
SVD4     DS    0CL59               SAVE D4 RCD'S POSITIONS 182-240              
*        THESE FIELDS ARE NOT REPORTED ON ALL D4 RECORDS                        
S4STA    DS    CL3        182-184  TOTAL PROGRAM STATION COUNT                  
S4COV    DS    CL2        185-186  TOTAL PROGRAM COVERAGE PERCENT               
         DS    CL3        187-189  (FILLER)                                     
S4TELE   DS    CL3        190-192  TELECASTS 01=DAY 02-07=AVERAGES              
S4TOTDUR DS    CL6        193-198  TOTAL DURATION                               
S4AVEHUT DS    CL9        199-207  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)          
S4AVERTG DS    CL3        208-210  PROGRAM AVERAGE RATING (XX.X)                
S4REPORT DS    CL1        211-211  REPORTABILITY IND. 0=REPORTABLE              
S4HUT    DS    CL9        212-220  PROGRAM HUT PROJ. (XXX,XXX,XXX)              
S4SHR    DS    CL2        221-222  PROGRAM SHARE (XX)                           
S4PROJ   DS    CL9        223-231  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)           
S4RTG    DS    CL3        232-234  TOTAL AUDIENCE RATING (XX.X)                 
         DS    CL6        235-240  (FILLER)                                     
*                                                                               
*        THESE FIELDS ARE NOT REPORTED ON ALL H4 RECORDS                        
SVPROJ   DS    CL9                 USA PROJ                                     
SVMPROJ  DS    CL9                 MKT BREAK AUD. PROJ. (XXX,XXX,XXX)           
SVPUT    DS    CL9                 PUT PROJ                                     
SVRECLN  DS    F                   SAVE INTERIM RECORD'S LENGTH                 
*                                                                               
         EJECT                                                                  
* FILE DCB AND BUFFER AREA                                                      
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=400,                                              X        
               MACRF=GM,                                               X        
               EODAD=DONE                                                       
*                                                                               
SUMWGHT  DS    CL2                                                              
SUMDAY   DS    C                                                                
SUMSQH   DS    C                                                                
SUMEQH   DS    C                                                                
SUMSTIM  DS    CL2                                                              
SUMETIM  DS    CL2                                                              
         DS    0F                                                               
SUMHPT   DS    CL188                                                            
SVAVGHI  DS    F                   SAVE PROGRAM HH                              
*                                                                               
APACKWT  DS    A                   A(PACKWT) AREA                               
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
FLAG     DS    X                                                                
         DS    0F                                                               
SLOTLN   DC    AL2(NDEMS*4)                                                     
HPRG     DS    XL(NDEMS*4)         1/2HR PRG EST DEMOS                          
HPUTS    DS    XL(NDEMS*4)         1/2HR PRG PUT DEMOS                          
PUTS     DS    XL(NDEMS*4)         PUT BUFFER                                   
SAVEKEY  DS    CL(INTACCS-INTKEY)  KEY OF ACCUMULATED DEMOS                     
         DS    0F                                                               
SAVEREC  DS    XL(NDEMS*4*2)       4BYTE DEMOS + UNIVS                          
*-------------------------------------------------------------------            
UVHUTMKT DS    CL(L'UNVEST)        HUT UNV FOR TOTAL USA                        
UVHUTUNV DS    CL(L'UNVEST)        HUT UNV FOR MKT BRK                          
*                                                                               
         DS    0F                                                               
DUNIVS   DS    (NDEMS*NMKTS)XL4    DEMO UNIVS                                   
         DS    0F                                                               
PACKWT   DS    (NDEMS*2)PL8      WEIGHTING FACTOR PACKED                        
         DS    0F                                                               
         DC    CL24'***DEMAREA****DEMAREA***'                                   
         DS    0F                                                               
DEMAREA  DS    (NDEMS*92)XL4      92 MKTBRKS*NDEMS*4BYTE BUCKETS EA             
DEMLNQ   EQU   *-DEMAREA                                                        
         EJECT                                                                  
MSU      EQU   8                                                                
MFR      EQU   0                                                                
NDEMS    EQU   38                  UNION OF TOTAL SAMPLE W/DEMO TABLE           
NMKTS    EQU   90                  INCLUDES TOTAL SAMPLE=000                    
         EJECT                                                                  
* ********************************************************************          
* EQUATE DEMO SLOT NUMBERS                                                      
* ?T = TOTAL SAMPLE RECORDS                                                     
* ?G = DEMO GROUP RECORDS                                                       
* ?I =  INTERIM RECD SLOTS FOR DEMOS  (ALMOST THE SAME AS OUTPUT)               
* ********************************************************************          
RIUSA    EQU   0                                                                
RIHOMES  EQU   1                                                                
PIF211   EQU   2                  PW2-11                                        
PIF1217  EQU   3                  PW12-17                                       
PIF1824  EQU   4                  PW18-24                                       
PIF2534  EQU   5                  PW25-34                                       
PIF3549  EQU   6                  PW35-49                                       
PIF50O   EQU   7                  PW50+                                         
PIM211   EQU   8                  PM2-11                                        
PIM1217  EQU   9                  PM12-17                                       
PIM1824  EQU   10                 PM18-24                                       
PIM2534  EQU   11                 PM25-34                                       
PIM3549  EQU   12                 PM35-49                                       
PIM50O   EQU   13                 PM50+                                         
*                                                                               
FIF211   EQU   14                 FW2-11                                        
FIF1217  EQU   15                 FW12-17                                       
FIF1824  EQU   16                 FW18-24                                       
FIF2534  EQU   17                 FW25-34                                       
FIF3549  EQU   18                 FW35-49                                       
FIF50O   EQU   19                 FW50+                                         
FIM211   EQU   20                 FM2-11                                        
FIM1217  EQU   21                 FM12-17                                       
FIM1824  EQU   22                 FM18-24                                       
FIM2534  EQU   23                 FM25-34                                       
FIM3549  EQU   24                 FM35-49                                       
FIM50O   EQU   25                 FM50+                                         
*                                                                               
AIF211   EQU   26                 AW2-11                                        
AIF1217  EQU   27                 AW12-17                                       
AIF1824  EQU   28                 AW18-24                                       
AIF2534  EQU   29                 AW25-34                                       
AIF3549  EQU   30                 AW35-49                                       
AIF50O   EQU   31                 AW50+                                         
AIM211   EQU   32                 AM2-11                                        
AIM1217  EQU   33                 AM12-17                                       
AIM1824  EQU   34                 AM18-24                                       
AIM2534  EQU   35                 AM25-34                                       
AIM3549  EQU   36                 AM35-49                                       
AIM50O   EQU   37                 AM50+                                         
*                                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* ?T..=  INPUT TAPE DISPLACEMENTS FOR  --TOTAL SAMPLE--  DEMOS                  
**********************************************************************          
*                                                                               
PTF211   EQU   61                 PW2-11                                        
PTF1217  EQU   62                 PW12-17                                       
PTF1824  EQU   63                 PW18-24                                       
PTF2534  EQU   64                 PW25-34                                       
PTF3549  EQU   65                 PW35-49                                       
PTF50O   EQU   66                 PW50+                                         
PTM211   EQU   81                 PM2-11                                        
PTM1217  EQU   82                 PM12-17                                       
PTM1824  EQU   83                 PM18-24                                       
PTM2534  EQU   84                 PM25-34                                       
PTM3549  EQU   85                 PM35-49                                       
PTM50O   EQU   86                 PM50+                                         
*                                                                               
FTF211   EQU   67                 FW2-11                                        
FTF1217  EQU   68                 FW12-17                                       
FTF1824  EQU   69                 FW18-24                                       
FTF2534  EQU   70                 FW25-34                                       
FTF3549  EQU   71                 FW35-49                                       
FTF50O   EQU   72                 FW50+                                         
FTM211   EQU   87                 FM2-11                                        
FTM1217  EQU   88                 FM12-17                                       
FTM1824  EQU   89                 FM18-24                                       
FTM2534  EQU   90                 FM25-34                                       
FTM3549  EQU   91                 FM35-49                                       
FTM50O   EQU   92                 FM50+                                         
*                                                                               
ATF211   EQU   73                 AW2-11                                        
ATF1217  EQU   74                 AW12-17                                       
ATF1824  EQU   75                 AW18-24                                       
ATF2534  EQU   76                 AW25-34                                       
ATF3549  EQU   77                 AW35-49                                       
ATF50O   EQU   78                 AW50+                                         
ATM211   EQU   93                 AM2-11                                        
ATM1217  EQU   94                 AM12-17                                       
ATM1824  EQU   95                 AM18-24                                       
ATM2534  EQU   96                 AM25-34                                       
ATM3549  EQU   97                 AM35-49                                       
ATM50O   EQU   98                 AM50+                                         
         EJECT                                                                  
**********************************************************************          
* RG..=  INPUT TAPE DISPS FOR  --MKT BRK-- DEMOS                                
**********************************************************************          
* THERE ARE NONE FOR THIS FILE                                                  
PGF211   EQU   61                 PW2-11                                        
PGF1217  EQU   62                 PW12-17                                       
PGF1824  EQU   63                 PW18-24                                       
PGF2534  EQU   64                 PW25-34                                       
PGF3549  EQU   65                 PW35-49                                       
PGF50O   EQU   66                 PW50+                                         
PGM211   EQU   81                 PM2-11                                        
PGM1217  EQU   82                 PM12-17                                       
PGM1824  EQU   83                 PM18-24                                       
PGM2534  EQU   84                 PM25-34                                       
PGM3549  EQU   85                 PM35-49                                       
PGM50O   EQU   86                 PM50+                                         
*                                                                               
FGF211   EQU   67                 FW2-11                                        
FGF1217  EQU   68                 FW12-17                                       
FGF1824  EQU   69                 FW18-24                                       
FGF2534  EQU   70                 FW25-34                                       
FGF3549  EQU   71                 FW35-49                                       
FGF50O   EQU   72                 FW50+                                         
FGM211   EQU   87                 FM2-11                                        
FGM1217  EQU   88                 FM12-17                                       
FGM1824  EQU   89                 FM18-24                                       
FGM2534  EQU   90                 FM25-34                                       
FGM3549  EQU   91                 FM35-49                                       
FGM50O   EQU   92                 FM50+                                         
*                                                                               
AGF211   EQU   73                 AW2-11                                        
AGF1217  EQU   74                 AW12-17                                       
AGF1824  EQU   75                 AW18-24                                       
AGF2534  EQU   76                 AW25-34                                       
AGF3549  EQU   77                 AW35-49                                       
AGF50O   EQU   78                 AW50+                                         
AGM211   EQU   93                 AM2-11                                        
AGM1217  EQU   94                 AM12-17                                       
AGM1824  EQU   95                 AM18-24                                       
AGM2534  EQU   96                 AM25-34                                       
AGM3549  EQU   97                 AM35-49                                       
AGM50O   EQU   98                 AM50+                                         
         EJECT                                                                  
**********************************************************************          
*SLOTTAB-TRANSLATE                                                              
*INPUT DEMOS TO INTERIM SLOTS                                                   
*        STORED IN THE INTERIM RECORD BEGINING AT: INTACCS + 0                  
**********************************************************************          
SLOTTAB  DS    0H                                                               
         DC    AL2(PTF211,PIF211)                                               
         DC    AL2(PTM211,PIM211)                                               
         DC    AL2(PTF1217,PIF1217)                                             
         DC    AL2(PTM1217,PIM1217)                                             
         DC    AL2(PTF1824,PIF1824)                                             
         DC    AL2(PTM1824,PIM1824)                                             
         DC    AL2(PTF2534,PIF2534)                                             
         DC    AL2(PTM2534,PIM2534)                                             
         DC    AL2(PTF3549,PIF3549)                                             
         DC    AL2(PTM3549,PIM3549)                                             
         DC    AL2(PTF50O,PIF50O)                                               
         DC    AL2(PTM50O,PIM50O)                                               
*                                                                               
         DC    AL2(FTF211,FIF211)                                               
         DC    AL2(FTM211,FIM211)                                               
         DC    AL2(FTF1217,FIF1217)                                             
         DC    AL2(FTM1217,FIM1217)                                             
         DC    AL2(FTF1824,FIF1824)                                             
         DC    AL2(FTM1824,FIM1824)                                             
         DC    AL2(FTF2534,FIF2534)                                             
         DC    AL2(FTM2534,FIM2534)                                             
         DC    AL2(FTF3549,FIF3549)                                             
         DC    AL2(FTM3549,FIM3549)                                             
         DC    AL2(FTF50O,FIF50O)                                               
         DC    AL2(FTM50O,FIM50O)                                               
*                                                                               
         DC    AL2(ATF211,AIF211)                                               
         DC    AL2(ATM211,AIM211)                                               
         DC    AL2(ATF1217,AIF1217)                                             
         DC    AL2(ATM1217,AIM1217)                                             
         DC    AL2(ATF1824,AIF1824)                                             
         DC    AL2(ATM1824,AIM1824)                                             
         DC    AL2(ATF2534,AIF2534)                                             
         DC    AL2(ATM2534,AIM2534)                                             
         DC    AL2(ATF3549,AIF3549)                                             
         DC    AL2(ATM3549,AIM3549)                                             
         DC    AL2(ATF50O,AIF50O)                                               
         DC    AL2(ATM50O,AIM50O)                                               
*                                                                               
         DC    X'FFFF',X'FFFF'                                                  
         EJECT                                                                  
**********************************************************************          
*OUTDEM -OUTPUT BUCKETS FOR DEMOS                                               
**********************************************************************          
ODUSA    EQU   0                                                                
ODHOMES  EQU   1                                                                
*                                                                               
PODF211  EQU   2                  PW2-11                                        
PODF1217 EQU   3                  PW12-17                                       
PODF1824 EQU   4                  PW18-24                                       
PODF2534 EQU   5                  PW25-34                                       
PODF3549 EQU   6                  PW35-49                                       
PODF50O  EQU   7                  PW50+                                         
PODM211  EQU   8                  PM2-11                                        
PODM1217 EQU   9                  PM12-17                                       
PODM1824 EQU   10                 PM18-24                                       
PODM2534 EQU   11                 PM25-34                                       
PODM3549 EQU   12                 PM35-49                                       
PODM50O  EQU   13                 PM50+                                         
*                                                                               
FODF211  EQU   14                 FW2-11                                        
FODF1217 EQU   15                 FW12-17                                       
FODF1824 EQU   16                 FW18-24                                       
FODF2534 EQU   17                 FW25-34                                       
FODF3549 EQU   18                 FW35-49                                       
FODF50O  EQU   19                 FW50+                                         
FODM211  EQU   20                 FM2-11                                        
FODM1217 EQU   21                 FM12-17                                       
FODM1824 EQU   22                 FM18-24                                       
FODM2534 EQU   23                 FM25-34                                       
FODM3549 EQU   24                 FM35-49                                       
FODM50O  EQU   25                 FM50+                                         
*                                                                               
AODF211  EQU   26                 AW2-11                                        
AODF1217 EQU   27                 AW12-17                                       
AODF1824 EQU   28                 AW18-24                                       
AODF2534 EQU   29                 AW25-34                                       
AODF3549 EQU   30                 AW35-49                                       
AODF50O  EQU   31                 AW50+                                         
AODM211  EQU   32                 AM2-11                                        
AODM1217 EQU   33                 AM12-17                                       
AODM1824 EQU   34                 AM18-24                                       
AODM2534 EQU   35                 AM25-34                                       
AODM3549 EQU   36                 AM35-49                                       
AODM50O  EQU   37                 AM50+                                         
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*OUTDEMS- OUTPUT DEMOS FORMAT. TRANSLATE INTERIM SLOTS TO OUTPUT SLOTS          
*         DEMOS STORED IN OUTPUT RECD STARTING AT:   INTACCS + 0                
***********************************************************************         
OUTDEMS  DS    0XL2                                                             
         DC    AL1(RIUSA,ODUSA)                                                 
         DC    AL1(RIHOMES,ODHOMES)                                             
         DC    AL1(PIF211,PODF211)                                              
         DC    AL1(PIF1217,PODF1217)                                            
         DC    AL1(PIF1824,PODF1824)                                            
         DC    AL1(PIF2534,PODF2534)                                            
         DC    AL1(PIF3549,PODF3549)                                            
         DC    AL1(PIF50O,PODF50O)                                              
*                                                                               
         DC    AL1(PIM211,PODM211)                                              
         DC    AL1(PIM1217,PODM1217)                                            
         DC    AL1(PIM1824,PODM1824)                                            
         DC    AL1(PIM2534,PODM2534)                                            
         DC    AL1(PIM3549,PODM3549)                                            
         DC    AL1(PIM50O,PODM50O)                                              
*                                                                               
         DC    AL1(FIF211,FODF211)                                              
         DC    AL1(FIF1217,FODF1217)                                            
         DC    AL1(FIF1824,FODF1824)                                            
         DC    AL1(FIF2534,FODF2534)                                            
         DC    AL1(FIF3549,FODF3549)                                            
         DC    AL1(FIF50O,FODF50O)                                              
*                                                                               
         DC    AL1(FIM211,FODM211)                                              
         DC    AL1(FIM1217,FODM1217)                                            
         DC    AL1(FIM1824,FODM1824)                                            
         DC    AL1(FIM2534,FODM2534)                                            
         DC    AL1(FIM3549,FODM3549)                                            
         DC    AL1(FIM50O,FODM50O)                                              
*                                                                               
         DC    AL1(AIF211,AODF211)                                              
         DC    AL1(AIF1217,AODF1217)                                            
         DC    AL1(AIF1824,AODF1824)                                            
         DC    AL1(AIF2534,AODF2534)                                            
         DC    AL1(AIF3549,AODF3549)                                            
         DC    AL1(AIF50O,AODF50O)                                              
*                                                                               
         DC    AL1(AIM211,AODM211)                                              
         DC    AL1(AIM1217,AODM1217)                                            
         DC    AL1(AIM1824,AODM1824)                                            
         DC    AL1(AIM2534,AODM2534)                                            
         DC    AL1(AIM3549,AODM3549)                                            
         DC    AL1(AIM50O,AODM50O)                                              
*                                                                               
         DC    X'FF',X'FFFF'                                                    
         EJECT                                                                  
**********************************************************************          
*OUTUNV -OUTPUT BUCKETS FOR UNIVERSES                                           
**********************************************************************          
                                                                                
UNVDSPQ  EQU   58*4                DISP TO RAW ('K') UNIVS                      
GAADSPQ  EQU   (58+14)*4           DISP TO RAW ('F') GAA IMPS FOR NSS           
                                                                                
*************************************************                               
* TABLE OF RATING SERVICE MARKET BREAK CODES                                    
*************************************************                               
*RUSA    EQU   0                  TOTAL USA                                     
*                                 MOVIEGOER                                     
CRMGP    EQU   0                    PRINCIPAL                                   
CRMGF    EQU   1                    FREQUENT                                    
CRMGA    EQU   2                    AVID                                        
*                                 TERRITORY                                     
CRNE     EQU   1                   NORTHEAST                                    
CREC     EQU   2                   EAST CENTRAL                                 
CRWC     EQU   3                   WEST CENTRAL                                 
CRSE     EQU   4                   SOUTHEAST                                    
CRSW     EQU   5                   SOUTHWEST                                    
CRPAC    EQU   6                   PACIFIC                                      
*                                 COUNTY SIZE                                   
CRCSA    EQU   20                  CS A                                         
CRCSB    EQU   21                  CS B                                         
CRCSCD   EQU   22                  CS C+D                                       
*                                 CABLE                                         
CRPCBL   EQU   60                  PAY CABLE                                    
CRBCBL   EQU   61                  BASIC CABLE                                  
CRNCBL   EQU   62                  NO CABLE                                     
CRCPPAY  EQU   66                  CABLE PLUS & PAY       (9/96)                
CRCPNOPY EQU   67                  CABLE PLUS W/OUT PAY   (9/96)                
CRBRDCST EQU   68                  BROADCAST ONLY         (9/96)                
*                                 HOUSEHOLD SIZE                                
CRHHS1   EQU   140                 1                                            
CRHHS2   EQU   141                 2                                            
CRHHS3   EQU   142                 3                                            
CRHHS4O  EQU   143                 4+                                           
*                                 INCOME                                        
CRIU20   EQU   240                 UNDER 20K                                    
CRIU30   EQU   241                 20-29,999                                    
CRIU40   EQU   242                 30-39,999                                    
CRIU50   EQU   243                 40-49,999                                    
CRIU60   EQU   244                 50-59,999                                    
CRIO60   EQU   245                 OVER  60K                                    
CRIU75   EQU   247                 60-74,999   (9/96)                           
CRIO75   EQU   248                 OVER  75K   (9/96)                           
*                                 SELECTED UPPER DEMOS                          
CRIUDNA  EQU   260                 40K+ & NON-ADULTS                            
CRIUDPOM EQU   261                 40K+ & POM                                   
CRIUDCOL EQU   262                 40K+ & COLLEGE                               
*                                  UPPER DEMOS W/50K & ..                       
CRIUDNA5 EQU   266                 50K+ & NON-ADULTS                            
CRIUDPM5 EQU   267                 50K+ & POM                                   
CRIUDCL5 EQU   268                 50K+ & COLLEGE                               
*                                 PRESENCE OF NON-ADULTS                        
CRNAU18  EQU   350                 ANY UNDER 18                                 
CRNAU12  EQU   351                 ANY UNDER 12                                 
CRNAU06  EQU   352                 ANY UNDER 6                                  
CRNAU03  EQU   353                 ANY UNDER 3                                  
CRNA611  EQU   354                 ANY 6-11                                     
CRNA1217 EQU   355                 ANY 12-17                                    
*                                 TIME ZONE                                     
CRTZEAST EQU   40                  EASTERN                                      
CRTZCENT EQU   41                  CENTRAL                                      
CRTZMP   EQU   42                  MOUNTAIN AND PACIFIC                         
*                                                                               
CRVCR    EQU   80                  PRESENCE OF VCR                              
*                                                                               
CRREMOTE EQU   100                 PRESENCE OF REMOTE CONTROL                   
*                                 NUMBER OF TV SETS                             
CRTV1    EQU   120                 1                                            
CRTV2    EQU   121                 2                                            
CRTV3    EQU   122                 3                                            
CRTV4O   EQU   123                 4+                                           
*                                 AGE OF HOH                                    
CRHO25U  EQU   160                 UNDER 25                                     
CRHO2534 EQU   161                 25-34                                        
CRHO3554 EQU   162                 35-54                                        
CRHO5564 EQU   163                 55-64                                        
CRHO65O  EQU   164                 65+                                          
*                                 AGE OF HOH WITHIN HH SIZE                     
CRHO50U  EQU   180                 1-2 PERSONS, HOH UNDER 50                    
CRHO50O  EQU   181                 1-2 PERSONS, HOH 50+                         
*                                 EDUCATION OF HOH                              
CRED8U   EQU   200                 UNDER 8 YEARS                                
CRED12U  EQU   201                 1-3 YEARS HS                                 
CRED12   EQU   202                 4 YEARS HS                                   
CRED16U  EQU   203                 1-3 YEARS COL                                
CRED16O  EQU   204                 4+ YEARS COL                                 
*                                 OCCUPATION OF HOH                             
CROCWC   EQU   220                 WHITE COLLAR                                 
CROCBC   EQU   221                 BLUE COLLAR                                  
CROCNO   EQU   222                 NOT IN LABOR FORCE                           
*                                 RACE                                          
CRRACEB  EQU   320                 BLACK                                        
CRRACEW  EQU   321                 WHITE                                        
*                                 CAR OWNERSHIP                                 
CRCARANY EQU   400                 ANY OWNERS                                   
CRCAR1   EQU   401                 1 CAR                                        
CRCAR2O  EQU   402                 2+ CARS                                      
CRCARNEW EQU   403                 ANY CAR, NEW PROSPECT                        
*                                 TRUCK OWNERSHIP                               
CRTRKANY EQU   420                 ANY OWNERS                                   
CRTRK1   EQU   421                 1 TRUCK                                      
CRTRK2O  EQU   422                 2+ TRUCKS                                    
CRTRKNEW EQU   423                 ANY TRUCK, NEW PROSPECT                      
*                                 PET OWNERSHIP                                 
CRDOG    EQU   440                 DOG                                          
CRCAT    EQU   441                 CAT                                          
*                                                                               
CRPC     EQU   450                 PC OWNERS                                    
CRNOPC   EQU   451                 NON-PC OWNER                                 
CRPCIA   EQU   452                 PC OWNER WITH INTERNET ACCESS                
CRPCNOIA EQU   453                 PC OWNER WITHOUT INTERNET ACCESS             
*                                                                               
CRDUAL   EQU   501                 DUAL INCOME HH 40K+                          
CRDUAL50 EQU   502                 DUAL INCOME HH 50K+                          
         SPACE 2                                                                
**********************************************************************          
* CI.. - SLOTTED INTO KEY OF INTERIM RECD AND LATER CONVERTED TO CO..           
*        TABLE OF INTERIM AREA SLOTS FOR MARKET BREAKS                          
**********************************************************************          
CIUSA    EQU   0                  TOTAL USA                                     
*                                 TERRITORY                                     
CINE     EQU   1                   NORTHEAST                                    
CIEC     EQU   2                   EAST CENTRAL                                 
CIWC     EQU   3                   WEST CENTRAL                                 
CISE     EQU   4                   SOUTHEAST                                    
CISW     EQU   5                   SOUTHWEST                                    
CIPAC    EQU   6                   PACIFIC                                      
*                                 COUNTY SIZE                                   
CICSA    EQU   7                   CS A                                         
CICSB    EQU   8                   CS B                                         
CICSCD   EQU   9                   CS C+D                                       
*                                 CABLE                                         
CIPCBL   EQU   13                  PAY CABLE                                    
CIBCBL   EQU   14                  BASIC CABLE                                  
CINCBL   EQU   15                  NO CABLE                                     
CICPPAY  EQU   77                  CABLE PLUS WITH PAY      (9/96)              
CICPNOPY EQU   78                  CABLE PLUS WITH OUT PAY  (9/96)              
CIBRDCST EQU   79                  BROADCAST ONLY           (9/96)              
*                                 HOUSEHOLD SIZE                                
CIHHS1   EQU   16                  1                                            
CIHHS2   EQU   17                  2                                            
CIHHS3   EQU   18                  3                                            
CIHHS4O  EQU   19                  4+                                           
*                                 INCOME                                        
CIIU20   EQU   20                  UNDER 20K                                    
CIIU30   EQU   21                  20-29,999                                    
CIIU40   EQU   22                  30-39,999                                    
CIIU50   EQU   23                  40-49,999                                    
CIIU60   EQU   24                  50-59,999                                    
CIIO60   EQU   25                  OVER  60K                                    
CIIU75   EQU   80                  60-74,999   (9/96)                           
CIIO75   EQU   81                  OVER  75K   (9/96)                           
*                                 SELECTED UPPER DEMOS                          
CIIUDNA  EQU   26                  40K+ & NON-ADULTS                            
CIIUDPOM EQU   27                  40K+ & POM                                   
CIIUDCOL EQU   28                  40K+ & COLLEGE                               
CIIUDNA5 EQU   82                  50K+ & NON-ADULTS                            
CIIUDPM5 EQU   83                  50K+ & POM                                   
CIIUDCL5 EQU   84                  50K+ & COLLEGE                               
*                                 PRESENCE OF NON-ADULTS                        
CINAU18  EQU   30                  ANY UNDER 18                                 
CINAU12  EQU   31                  ANY UNDER 12                                 
CINAU06  EQU   32                  ANY UNDER 6                                  
CINAU03  EQU   33                  ANY UNDER 3                                  
CINA611  EQU   34                  ANY 6-11                                     
CINA1217 EQU   35                  ANY 12-17                                    
*                                 TIME ZONE                                     
CITZEAST EQU   10                  EASTERN                                      
CITZCENT EQU   11                  CENTRAL                                      
CITZMP   EQU   12                  MOUNTAIN AND PACIFIC                         
*                                                                               
CIVCR    EQU   36                  PRESENCE OF VCR                              
*                                                                               
CIREMOTE EQU   37                  PRESENCE OF REMOTE CONTROL                   
*                                 NUMBER OF TV SETS                             
CITV1    EQU   38                  1                                            
CITV2    EQU   39                  2                                            
CITV3    EQU   40                  3                                            
CITV4O   EQU   41                  4+                                           
*                                 AGE OF HOH                                    
CIHO25U  EQU   42                  UNDER 25                                     
CIHO2534 EQU   43                  25-34                                        
CIHO3554 EQU   44                  35-54                                        
CIHO5564 EQU   45                  55-64                                        
CIHO65O  EQU   46                  65+                                          
*                                 AGE OF HOH WITHIN HH SIZE                     
CIHO50U  EQU   47                  1-2 PERSONS, HOH UNDER 50                    
CIHO50O  EQU   48                  1-2 PERSONS, HOH 50+                         
*                                 EDUCATION OF HOH                              
CIED8U   EQU   49                  UNDER 8 YEARS                                
CIED12U  EQU   50                  1-3 YEARS HS                                 
CIED12   EQU   51                  4 YEARS HS                                   
CIED16U  EQU   52                  1-3 YEARS COL                                
CIED16O  EQU   53                  4+ YEARS COL                                 
*                                 OCCUPATION OF HOH                             
CIOCWC   EQU   54                  WHITE COLLAR                                 
CIOCBC   EQU   55                  BLUE COLLAR                                  
CIOCNO   EQU   56                  NOT IN LABOR FORCE                           
*                                 RACE                                          
CIRACEB  EQU   57                  BLACK                                        
CIRACEW  EQU   58                  WHITE                                        
*                                 CAR OWNERSHIP                                 
CICARANY EQU   59                  ANY OWNERS                                   
CICAR1   EQU   60                  1 CAR                                        
CICAR2O  EQU   61                  2+ CARS                                      
CICARNEW EQU   62                  ANY CAR, NEW PROSPECT                        
*                                 TRUCK OWNERSHIP                               
CITRKANY EQU   63                  ANY OWNERS                                   
CITRK1   EQU   64                  1 TRUCK                                      
CITRK2O  EQU   65                  2+ TRUCKS                                    
CITRKNEW EQU   66                  ANY TRUCK, NEW PROSPECT                      
*                                 PET OWNERSHIP                                 
CIDOG    EQU   67                  DOG                                          
CICAT    EQU   68                  CAT                                          
*                                                                               
CIDUAL   EQU   29                  DUAL INCOME HH 40K+                          
CIDUAL50 EQU   85                  DUAL INCOME HH 50K+  (SEP/96)                
*                                                                               
CIPC     EQU   87                  PC OWNERS                                    
CINOPC   EQU   88                  NON-PC OWNER                                 
CIPCIA   EQU   89                  PC OWNER WITH INTERNET ACCESS                
CIPCNOIA EQU   90                  PC OWNER WITHOUT INTERNET ACCESS             
*                                 MOVIE GOERS                                   
CIMGP    EQU   0                    PRINCIPAL                                   
CIMGF    EQU   1                    FREQUENT                                    
CIMGA    EQU   2                    AVID                                        
*                                                                               
*        GENERATED CELLS                                                        
CISO     EQU   69                  SOUTH                                        
CIACBL   EQU   70                  ANY CABLE                                    
CIIO20   EQU   71                  20K+                                         
CIIO30   EQU   72                  30K+                                         
CIIO40   EQU   73                  40K+                                         
CIIO50   EQU   74                  50K+                                         
CIIU30U  EQU   75                  30K-                                         
CIHHS3O  EQU   76                  HHS3+                                        
CICBLP   EQU   86                  CBL+ = CBL+ W/PAY + CBL+ W/OUT PAY           
         EJECT                                                                  
**********************************************************************          
* CO.. - THE MKT BRK#'S ACTUALLY APPEAR ON THE FILE IN 23 ELEMENT               
*        TABLE OF OUTPUT SECTIONS FOR MARKET BREAKS                             
**********************************************************************          
COUSA    EQU   1                  TOTAL USA                                     
*                                 TERRITORY                                     
CONE     EQU   11                  NORTHEAST                                    
COEC     EQU   12                  EAST CENTRAL                                 
COWC     EQU   13                  WEST CENTRAL                                 
COSE     EQU   16   *              SOUTHEAST                                    
COSW     EQU   17   *              SOUTHWEST                                    
COPAC    EQU   15                  PACIFIC                                      
*                                 COUNTY SIZE                                   
COCSA    EQU   21                  CS A                                         
COCSB    EQU   22                  CS B                                         
COCSCD   EQU   23                  CS C+D                                       
*                                 CABLE                                         
COPCBL   EQU   32                  PAY CABLE                                    
COBCBL   EQU   33                  BASIC CABLE                                  
CONCBL   EQU   34                  NO CABLE                                     
COCPPAY  EQU   35                  CABLE PLUS WITH PAY      (9/96)              
COCPNOPY EQU   36                  CABLE PLUS WITH OUT PAY  (9/96)              
COBRDCST EQU   37                  BROADCAST ONLY           (9/96)              
*                                 HOUSEHOLD SIZE                                
COHHS1   EQU   41                  1                                            
COHHS2   EQU   42                  2                                            
***COHHS3O  EQU   43                  3+                                        
COHHS4O  EQU   44                  4+                                           
COHHS3   EQU   45                  3                                            
*                                 INCOME                                        
COIU20   EQU   68                  UNDER 20K                                    
COIU30   EQU   80                  20-29,999                                    
COIU40   EQU   81                  30-39,999                                    
COIU50   EQU   82                  40-49,999                                    
COIU60   EQU   83                  50-59,999                                    
COIO60   EQU   66                  OVER  60K                                    
COIU75   EQU   84                  60-74,999   (9/96)                           
COIO75   EQU   85                  OVER  75K   (9/96)                           
*                                                                               
*                                 SELECTED UPPER DEMOS                          
COIUDNA  EQU   74                  40K+ & NON-ADULTS                            
COIUDPOM EQU   75                  40K+ & POM                                   
COIUDCOL EQU   76                  40K+ & COLLEGE                               
COIUDNA5 EQU   86                  50K+ & NON-ADULTS                            
COIUDPM5 EQU   87                  50K+ & POM                                   
COIUDCL5 EQU   88                  50K+ & COLLEGE                               
*                                 PRESENCE OF NON-ADULTS                        
CONAU18  EQU   51                  ANY UNDER 18                                 
CONAU12  EQU   52                  ANY UNDER 12                                 
CONAU06  EQU   53                  ANY UNDER 6                                  
CONAU03  EQU   54                  ANY UNDER 3                                  
CONA611  EQU   55                  ANY 6-11                                     
CONA1217 EQU   56                  ANY 12-17                                    
*                                 TIME ZONE                                     
COTZEAST EQU   91                  EASTERN                                      
COTZCENT EQU   92                  CENTRAL                                      
COTZMP   EQU   93                  MOUNTAIN AND PACIFIC                         
*                                                                               
COVCR    EQU   101                 PRESENCE OF VCR                              
*                                                                               
COREMOTE EQU   102                 PRESENCE OF REMOTE CONTROL                   
*                                 NUMBER OF TV SETS                             
COTV1    EQU   103                 1                                            
COTV2    EQU   104                 2                                            
COTV3    EQU   105                 3                                            
COTV4O   EQU   106                 4+                                           
*                                 AGE OF HOH                                    
COHO25U  EQU   111                 UNDER 25                                     
COHO2534 EQU   112                 25-34                                        
COHO3554 EQU   113                 35-54                                        
COHO5564 EQU   114                 55-64                                        
COHO65O  EQU   115                 65+                                          
*                                 AGE OF HOH WITHIN HH SIZE                     
COHO50U  EQU   118                 1-2 PERSONS, HOH UNDER 50                    
COHO50O  EQU   119                 1-2 PERSONS, HOH 50+                         
*                                 EDUCATION OF HOH                              
COED8U   EQU   121                 UNDER 8 YEARS                                
COED12U  EQU   122                 1-3 YEARS HS                                 
COED12   EQU   123                 4 YEARS HS                                   
COED16U  EQU   124                 1-3 YEARS COL                                
COED16O  EQU   125                 4+ YEARS COL                                 
*                                 OCCUPATION OF HOH                             
COOCWC   EQU   131                 WHITE COLLAR                                 
COOCBC   EQU   132                 BLUE COLLAR                                  
COOCNO   EQU   133                 NOT IN LABOR FORCE                           
*                                 RACE                                          
CORACEB  EQU   141                 BLACK                                        
CORACEW  EQU   142                 WHITE                                        
*                                 CAR OWNERSHIP                                 
COCARANY EQU   151                 ANY OWNERS                                   
COCAR1   EQU   152                 1 CAR                                        
COCAR2O  EQU   153                 2+ CARS                                      
COCARNEW EQU   154                 ANY CAR, NEW PROSPECT                        
*                                 TRUCK OWNERSHIP                               
COTRKANY EQU   156                 ANY OWNERS                                   
COTRK1   EQU   157                 1 TRUCK                                      
COTRK2O  EQU   158                 2+ TRUCKS                                    
COTRKNEW EQU   159                 ANY TRUCK, NEW PROSPECT                      
*                                 PET OWNERSHIP                                 
CODOG    EQU   161                 DOG                                          
COCAT    EQU   162                 CAT                                          
COPC     EQU   163                 PC OWNERS                                    
CONOPC   EQU   164                 NON-PC OWNER                                 
COPCIA   EQU   165                 PC OWNER WITH INTERNET ACCESS                
COPCNOIA EQU   166                 PC OWNER WITHOUT INTERNET ACCESS             
*                                 MOVIE GOERS                                   
COMGP    EQU   167                  PRINCIPAL                                   
COMGF    EQU   168                  FREQUENT                                    
COMGA    EQU   169                  AVID                                        
*                                                                               
CODUAL   EQU   77                  DUAL INCOME HH 40K+                          
CODUAL50 EQU   78                  DUAL INCOME HH 50K+                          
*                                                                               
*        GENERATED CELLS                                                        
COSO     EQU   14                  SOUTH                                        
COACBL   EQU   31                  ANY CABLE                                    
COCBLP   EQU   38                  CABLE PLUS W/PAY + W/OUT PAY                 
COHHS3O  EQU   43                  HOUSEHOLD SIZE 3+                            
COIO20   EQU   62                  20K+                                         
COIO30   EQU   63                  30K+                                         
COIO40   EQU   64                  40K+                                         
COIO50   EQU   65                  50K+                                         
COIU30U  EQU   67                  30K-                                         
         SPACE 2                                                                
* TABLE TO CONVERT MARKET BREAKS TO INTERNAL SLOTS                              
CRCITAB  DS    0H                                                               
*        DC    AL2(CRUSA,CIUSA)                                                 
*                                 MOVIE GOER                                    
         DC    AL2(CRMGP,CIMGP)    PRINCIPAL                                    
         DC    AL2(CRMGF,CIMGF)    FREQUENT                                     
         DC    AL2(CRMGA,CIMGA)    AVID                                         
         DC    X'FFFF',X'FFFF'                                                  
         SPACE 2                                                                
* TABLE TO CONVERT MARKET BREAKS TO OUTPUT SECTIONS                             
CRCOTAB  DS    0H                                                               
*        DC    AL2(CIUSA,COUSA),C'M'                                            
*                                 MOVIE GOER                                    
         DC    AL2(CIMGP,COMGP),C'M'    PRINCIPAL                               
         DC    AL2(CIMGF,COMGF),C'M'    FREQUENT                                
         DC    AL2(CIMGA,COMGA),C'M'    AVID                                    
*                                 TERRITORY                                     
         DC    X'FFFF',X'FFFF',X'FF'                                            
         SPACE 2                                                                
CIGENTAB DS    0H                                                               
*        DC    AL2(CISO,CISE,CISW,0,0,0)                                        
         DC    X'FFFF'                                                          
         EJECT                                                                  
* EQUATE PROGRAM TYPES/ DAYPARTS                                                
LFALL    EQU   1200                ALL LATE FRINGE                              
LF11A1   EQU   1220                ALL M-F 11:30P-1A                            
DYALL    EQU   7999                M-F 7A-4:30P                                 
DYDD     EQU   8000                DAYTIME DRAMA                                
DYQA     EQU   8400                QUIZ & AUD. PARTICIPATION                    
DYAD     EQU   8500                ADULT 7-10AM. IT'S REALLY 6-10A              
DY10A1   EQU   8600                ALL 10A-1P                                   
DY1P4    EQU   8700                ALL 1P-4:30P                                 
DY10A4AR EQU   8710                ALL 10A-4.30P ABC REG                        
DY10A4CR EQU   8720                ALL 10A-4.30P CBS REG                        
DY10A4FR EQU   8725                ALL 10A-4.30P FOX REG                        
DY10A4NR EQU   8730                ALL 10A-4.30P NBC REG                        
DY10A4WR EQU   8731                ALL 10A-4.30P WB  REG                        
DY10A4PR EQU   8732                ALL 10A-4.30P PAX REG                        
DY10A4UR EQU   8733                ALL 10A-4.30P PAR REG                        
DY10A4A  EQU   8740                ALL 10A-4.30P ABC REG+SPEC                   
DY10A4C  EQU   8750                ALL 10A-4.30P CBS REG+SPEC                   
DY10A4F  EQU   8755                ALL 10A-4.30P FOX REG+SPEC                   
DY10A4N  EQU   8760                ALL 10A-4.30P NBC REG+SPEC                   
DY10A4W  EQU   8761                ALL 10A-4.30P WB  REG+SPEC                   
DY10A4P  EQU   8762                ALL 10A-4.30P PAX REG+SPEC                   
DY10A4U  EQU   8763                ALL 10A-4.30P PAR REG+SPEC                   
DY10A4   EQU   8800                ALL 10A-4.30P (3NET)- BEFORE SEP1/03         
DY10A44  EQU   8800                ALL 10A-4.30P (4NET) ?                       
DY10A4ZR EQU   8800                ALL 10A-4.30P ALL REG- AFTER SEP1/03         
DY10A4Z  EQU   8810                ALL 10A-4.30P ALL REG+SPEC                   
WKALL    EQU   8999                ALL WEEKEND DAYTIME                          
WKCH     EQU   9000                WK CHILDRENS                                 
WKCHA    EQU   9010                WK CHILDRENS ABC                             
WKCHC    EQU   9011                WK CHILDRENS CBS                             
WKCHF    EQU   9012                WK CHILDRENS FOX                             
WKCHN    EQU   9013                WK CHILDRENS NBC                             
WKCHW    EQU   9014                WK CHILDRENS WB                              
WKCHP    EQU   9015                WK CHILDRENS PAX                             
WKCHU    EQU   9016                WK CHILDRENS PAR                             
WKI      EQU   9050                WK INFORMATIONAL                             
WDCH     EQU   9100                WD CHILDRENS                                 
WKSPR    EQU   9500                SPORTS REGULAR                               
WKSPRA   EQU   9510                SPORTS REGULAR  (ABC)                        
WKSPRC   EQU   9511                SPORTS REGULAR  (CBS)                        
WKSPRN   EQU   9512                SPORTS REGULAR  (NBC)                        
WKSPRF   EQU   9513                SPORTS REGULAR  (FOX)                        
WKSPRW   EQU   9514                SPORTS REGULAR  (WB )                        
WKSPRU   EQU   9515                SPORTS REGULAR  (PAR)                        
WKSPS    EQU   9600                SPORTS SPECIAL                               
WKSPSA   EQU   9610                SPORTS SPECIAL                               
WKSPSC   EQU   9611                SPORTS SPECIAL                               
WKSPSF   EQU   9612                SPORTS SPECIAL                               
WKSPSN   EQU   9613                SPORTS SPECIAL                               
WKSPSW   EQU   9614                SPORTS SPECIAL (WB)                          
WKSPSU   EQU   9615                SPORTS SPECIAL (PAR)                         
WKSP     EQU   9700                SPORTS REGULAR+SPECIAL                       
WKSPA    EQU   9710                SPORTS REGULAR+SPECIAL                       
WKSPC    EQU   9711                SPORTS REGULAR+SPECIAL                       
WKSPF    EQU   9712                SPORTS REGULAR+SPECIAL                       
WKSPN    EQU   9713                SPORTS REGULAR+SPECIAL                       
WKSPW    EQU   9714                SPORTS REGULAR+SPECIAL (WB)                  
WKSPU    EQU   9715                SPORTS REGULAR+SPECIAL (PAR)                 
AD24     EQU   9800                ALL ACTION/ADVENTURE                         
CA24     EQU   9810                ALL CHILD ANIMATION                          
TI24     EQU   9820                ALL INFORMATIONAL                            
SP24     EQU   9830                ALL SPORTS                                   
SP24R    EQU   9831                ALL SPORTS REGUALR                           
SP24S    EQU   9832                ALL SPORTS SPECIAL                           
NW24     EQU   9840                ALL NEWS                                     
NW24R    EQU   9841                ALL NEWS  REGULAR                            
NW24S    EQU   9842                ALL NEWS  SPECIAL                            
EVALL    EQU   499                 EVENING 7-11P                                
EVGD     EQU   500                 GENERAL DRAMA                                
EVSM     EQU   1000                SUSPENSE & MYSTERY                           
EVSC     EQU   1500                SITUATON COMEDY                              
EVAD     EQU   1700                ADVENTURE                                    
EVPV     EQU   3500                PARTICIPATION VARIETY                        
EVFF     EQU   4500                FEATURE FILMS                                
EVFFS    EQU   4501                FEATURE FILMS(SPC)                           
EVFFRS   EQU   4502                FEATURE FILMS(REG+SPC)                       
EV30     EQU   5000                ALL 30 MINUTE                                
EV60     EQU   5500                ALL 60 MINUTE                                
EV7P9    EQU   6000                ALL 7-9PM                                    
EV9P11   EQU   6500                ALL 9-11P                                    
EV7P11AR EQU   6510                ALL 7-11P ABC(REG)                           
EV7P11CR EQU   6520                ALL 7-11P CBS(REG)                           
EV7P11NR EQU   6530                ALL 7-11P NBC(REG)                           
EV7P11FR EQU   6540                ALL 7-11P FOX(REG)                           
EV7P11WR EQU   6550                ALL 7-11P WB (REG)                           
EV7P11PR EQU   6560                ALL 7-11P PAX(REG)                           
EV7P11UR EQU   6570                ALL 7-11P PAR(REG)                           
EV7P11R  EQU   7000                ALL 7-11P REG(3NET)                          
EV7P11AS EQU   7010                ALL 7-11P ABC(SPC)                           
EV7P11CS EQU   7020                ALL 7-11P CBS(SPC)                           
EV7P11NS EQU   7030                ALL 7-11P NBC(SPC)                           
EV7P11FS EQU   7040                ALL 7-11P FOX(SPC)                           
EV7P11WS EQU   7050                ALL 7-11P WB (SPC)                           
EV7P11PS EQU   7060                ALL 7-11P PAX(SPC)                           
EV7P11US EQU   7070                ALL 7-11P PAR(SPC)                           
EV7P11S  EQU   7200                ALL 7-11P SPC(3NET)                          
EV7P11A  EQU   7210                ALL 7-11P ABC(REG+SPC)                       
EV7P11C  EQU   7220                ALL 7-11P CBS(REG+SPC)                       
EV7P11N  EQU   7230                ALL 7-11P NBC(REG+SPC)                       
EV7P11F  EQU   7240                ALL 7-11P FOX(REG+SPC)                       
EV7P11W  EQU   7250                ALL 7-11P WB (REG+SPC)                       
EV7P11P  EQU   7260                ALL 7-11P PAX(REG+SPC)                       
EV7P11U  EQU   7270                ALL 7-11P PAR(REG+SPC)                       
EV7P11   EQU   7300                ALL 7-11P REG+SPC(3NET)                      
EV7P114R EQU   7400                ALL 7-11P REG(4NET)                          
EV7P114S EQU   7410                ALL 7-11P SPC(4NET)                          
EV7P114  EQU   7420                ALL 7-11P REG+SPC(4NET)                      
EV6P7I   EQU   7500                INFORMATIONAL ONE-A-WK                       
EV6P7IM  EQU   7600                INFORMATIONAL MULTI-WK                       
EV7P11ZR EQU   7700                ALL 7-11P REG(ALL)                           
EV7P11ZS EQU   7710                ALL 7-11P SPC(ALL)                           
EV7P11Z  EQU   7720                ALL 7-11P REG+SPC(ALL)                       
*                                                                               
* TIME DAYPARTS FOR HUT RECORDS                                                 
*                                                                               
DP1000   EQU   1000                                                             
DP1010   EQU   1010                                                             
DP1020   EQU   1020                                                             
DP1030   EQU   1030                                                             
DP1040   EQU   1040                                                             
DP1050   EQU   1050                                                             
DP1060   EQU   1060                                                             
DP1069   EQU   1069                                                             
DP1070   EQU   1070                                                             
DP1071   EQU   1071                                                             
DP1078   EQU   1078                                                             
DP1080   EQU   1080                                                             
DP1090   EQU   1090                                                             
DP1099   EQU   1099                                                             
DP1100   EQU   1100                                                             
DP1101   EQU   1101                                                             
DP1110   EQU   1110                                                             
DP1118   EQU   1118                                                             
DP1119   EQU   1119                                                             
DP1120   EQU   1120                                                             
DP1130   EQU   1130                                                             
DP1140   EQU   1140                                                             
DP1150   EQU   1150                                                             
DP1159   EQU   1159                                                             
DP1160   EQU   1160                                                             
DP1170   EQU   1170                                                             
DP1180   EQU   1180                                                             
DP1181   EQU   1181                                                             
DP1182   EQU   1182                                                             
DP1183   EQU   1183                                                             
DP1185   EQU   1185                                                             
DP1187   EQU   1187                                                             
DP1189   EQU   1189                                                             
DP1190   EQU   1190                                                             
DP1191   EQU   1191                                                             
DP1192   EQU   1192                                                             
DP1193   EQU   1193                                                             
DP1194   EQU   1194                                                             
DP1195   EQU   1195                                                             
DP1196   EQU   1196                                                             
DP1197   EQU   1197                                                             
DP1198   EQU   1198                                                             
DP1199   EQU   1199                                                             
DP1200   EQU   1200                                                             
DP1210   EQU   1210                                                             
DP1219   EQU   1219                                                             
DP1220   EQU   1220                                                             
DP1230   EQU   1230                                                             
DP1240   EQU   1240                                                             
DP1250   EQU   1250                                                             
DP1259   EQU   1259                                                             
DP1260   EQU   1260                                                             
DP1270   EQU   1270                                                             
DP1275   EQU   1275                                                             
DP1280   EQU   1280                                                             
DP1290   EQU   1290                                                             
DP1300   EQU   1300                                                             
DP1310   EQU   1310                                                             
DP1311   EQU   1311                                                             
DP1320   EQU   1320                                                             
DP1330   EQU   1330                                                             
DP1340   EQU   1340                                                             
DP1350   EQU   1350                                                             
DP1360   EQU   1360                                                             
DP1370   EQU   1370                                                             
DP1380   EQU   1380                                                             
DP1390   EQU   1390                                                             
DP1400   EQU   1400                                                             
*                                                                               
RG       EQU   X'01'               INCLUDE REGULAR PROGRAMS                     
SP       EQU   X'04'               INCLUDE SPECIAL PROGRAMS                     
*                                                                               
PTYPTAB  DC    C'GD',AL2(000),C'12  ',AL1(RG),AL2(EVGD),C'     '                
         DC    C'OP',AL2(000),C'12  ',AL1(RG),AL2(EVSM),C'     '                
         DC    C'PD',AL2(000),C'12  ',AL1(RG),AL2(EVSM),C'     '                
         DC    C'SM',AL2(000),C'12  ',AL1(RG),AL2(EVSM),C'     '                
         DC    C'CS',AL2(000),C'12  ',AL1(RG),AL2(EVSC),C'     '                
         DC    C'A ',AL2(000),C'12  ',AL1(RG),AL2(EVAD),C'     '                
         DC    C'PV',AL2(000),C'12  ',AL1(RG),AL2(EVPV),C'     '                
         DC    C'GV',AL2(000),C'12  ',AL1(RG),AL2(EVPV),C'     '                
         DC    C'CV',AL2(000),C'12  ',AL1(RG),AL2(EVPV),C'     '                
         DC    C'FF',AL2(000),C'12  ',AL1(RG),AL2(EVFF),C'     '                
         DC    C'FF',AL2(000),C'12  ',AL1(SP),AL2(EVFFS),C'     '               
         DC    C'FF',AL2(000),C'12  ',AL1(RG+SP),AL2(EVFFRS),C'     '           
         DC    C'  ',AL1(25,30),C'12  ',AL1(RG),AL2(EV30),C'     '              
         DC    C'  ',AL1(55,60),C'12  ',AL1(RG),AL2(EV60),C'     '              
         DC    C'  ',AL2(000),C'1   ',AL1(RG),AL2(EV7P9),C'     '               
         DC    C'  ',AL2(000),C'2   ',AL1(RG),AL2(EV9P11),C'     '              
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11AR),C'ABC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11CR),C'CBS  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11NR),C'NBC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11FR),C'FOX  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11R),C'ABC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11R),C'CBS Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11R),C'NBC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11AS),C'ABC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11CS),C'CBS  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11NS),C'NBC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11FS),C'FOX  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11S),C'ABC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11S),C'CBS Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11S),C'NBC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11A),C'ABC  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11C),C'CBS  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11N),C'NBC  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11F),C'FOX  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11),C'ABC Z'           
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11),C'CBS Z'           
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11),C'NBC Z'           
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'ABC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'CBS Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'NBC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'FOX Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'ABC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'CBS Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'NBC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'FOX Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'ABC Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'CBS Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'NBC Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'FOX Z'          
         DC    C'CC',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'DO',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'DN',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'N ',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'IA',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
*        DC    C'CC',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'DO',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'DN',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'N ',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'IA',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
         DC    C'  ',AL2(000),C'B   ',AL1(RG),AL2(LF11A1),C'     '              
         DC    C'  ',AL2(000),C'E56 ',AL1(RG),AL2(DYALL),C'     '               
         DC    C'DD',AL2(000),C'E56 ',AL1(RG),AL2(DYDD),C'     '                
         DC    C'AP',AL2(000),C'E56 ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QG',AL2(000),C'E56 ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QP',AL2(000),C'E56 ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'  ',AL2(000),C'D   ',AL1(RG),AL2(DYALL),C'     '               
         DC    C'DD',AL2(000),C'D   ',AL1(RG),AL2(DYDD),C'     '                
         DC    C'AP',AL2(000),C'D   ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QG',AL2(000),C'D   ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QP',AL2(000),C'D   ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'X1',AL2(000),C'DE  ',AL1(RG),AL2(DYAD),C'     '                
         DC    C'  ',AL2(000),C'5   ',AL1(RG),AL2(DY10A1),C'     '              
         DC    C'  ',AL2(000),C'6   ',AL1(RG),AL2(DY1P4),C'     '               
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4AR),C'ABC  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4CR),C'CBS  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4NR),C'NBC  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4FR),C'FOX  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4A),C'ABC  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4C),C'CBS  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4N),C'NBC  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4F),C'FOX  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4),C'     '              
         DC    C'C ',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'CA',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'CN',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'CL',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'PC',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CC',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'DO',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'DN',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'N ',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'IA',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPR),C'     '               
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRA),C'ABC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRC),C'CBS  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRF),C'FOX  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRN),C'NBC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPR),C'     '               
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRA),C'ABC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRC),C'CBS  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRF),C'FOX  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRN),C'NBC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPS),C'     '               
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSA),C'ABC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSC),C'CBS  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSF),C'FOX  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSN),C'NBC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPS),C'     '               
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSA),C'ABC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSC),C'CBS  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSF),C'FOX  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSN),C'NBC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSP),C'     '             
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPA),C'ABC  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPC),C'CBS  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPF),C'FOX  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPN),C'NBC  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSP),C'     '             
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPA),C'ABC  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPC),C'CBS  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPF),C'FOX  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPN),C'NBC  '            
         DC    C'A ',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'OP',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'PD',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'SM',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'CA',AL2(000),C'Z   ',AL1(RG),AL2(CA24),C'     '                
         DC    C'EA',AL2(000),C'Z   ',AL1(RG),AL2(CA24),C'     '                
         DC    C'CC',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'DO',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'DN',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'N ',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'IA',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'SA',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SA',AL2(000),C'Z   ',AL1(SP),AL2(SP24S),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(SP),AL2(SP24S),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24),C'     '             
         DC    C'SA',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24),C'     '             
         DC    C'N ',AL2(000),C'Z   ',AL1(RG+SP),AL2(NW24),C'     '             
         DC    C'N ',AL2(000),C'Z   ',AL1(RG),AL2(NW24R),C'     '               
         DC    C'N ',AL2(000),C'Z   ',AL1(SP),AL2(NW24S),C'     '               
         DC    X'FFFF'                                                          
*NEW TABLE EFFECTIVE SEP1/2003                                                  
PTYPTS03 DC    C'GD',AL2(000),C'12  ',AL1(RG),AL2(EVGD),C'     '                
         DC    C'OP',AL2(000),C'12  ',AL1(RG),AL2(EVSM),C'     '                
         DC    C'PD',AL2(000),C'12  ',AL1(RG),AL2(EVSM),C'     '                
         DC    C'SM',AL2(000),C'12  ',AL1(RG),AL2(EVSM),C'     '                
         DC    C'CS',AL2(000),C'12  ',AL1(RG),AL2(EVSC),C'     '                
         DC    C'A ',AL2(000),C'12  ',AL1(RG),AL2(EVAD),C'     '                
         DC    C'PV',AL2(000),C'12  ',AL1(RG),AL2(EVPV),C'     '                
         DC    C'GV',AL2(000),C'12  ',AL1(RG),AL2(EVPV),C'     '                
         DC    C'CV',AL2(000),C'12  ',AL1(RG),AL2(EVPV),C'     '                
         DC    C'FF',AL2(000),C'12  ',AL1(RG),AL2(EVFF),C'     '                
         DC    C'FF',AL2(000),C'12  ',AL1(SP),AL2(EVFFS),C'     '               
         DC    C'FF',AL2(000),C'12  ',AL1(RG+SP),AL2(EVFFRS),C'     '           
         DC    C'  ',AL1(25,30),C'12  ',AL1(RG),AL2(EV30),C'     '              
         DC    C'  ',AL1(55,60),C'12  ',AL1(RG),AL2(EV60),C'     '              
         DC    C'  ',AL2(000),C'1   ',AL1(RG),AL2(EV7P9),C'     '               
         DC    C'  ',AL2(000),C'2   ',AL1(RG),AL2(EV9P11),C'     '              
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11AR),C'ABC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11CR),C'CBS  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11NR),C'NBC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11FR),C'FOX  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11WR),C'WB   '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11PR),C'PAX  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11UR),C'PAR  '            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11R),C'ABC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11R),C'CBS Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11R),C'NBC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11AS),C'ABC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11CS),C'CBS  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11NS),C'NBC  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11FS),C'FOX  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11WS),C'WB   '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11PS),C'PAX  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11US),C'PAR  '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11S),C'ABC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11S),C'CBS Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11S),C'NBC Z'             
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11A),C'ABC  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11C),C'CBS  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11N),C'NBC  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11F),C'FOX  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11W),C'WB   '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11P),C'PAX  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11U),C'PAR  '          
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11),C'ABC Z'           
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11),C'CBS Z'           
         DC    C'  ',AL2(000),C'12  ',AL1(RG+SP),AL2(EV7P11),C'NBC Z'           
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'ABC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'CBS Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'NBC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P114R),C'FOX Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'ABC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'CBS Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'NBC Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P114S),C'FOX Z'            
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'ABC Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'CBS Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'NBC Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P114),C'FOX Z'          
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EV7P11ZR),C'     '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EV7P11ZS),C'     '            
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EV7P11Z),C'     '          
         DC    C'CC',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'DO',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'DN',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'N ',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
         DC    C'IA',AL2(000),C'0   ',AL1(RG+SP),AL2(EV6P7I),C'     '           
*        DC    C'CC',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'DO',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'DN',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'N ',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
*        DC    C'IA',AL2(000),C'0   ',AL1(RG),AL2(EV6P7IM),C'     '             
         DC    C'  ',AL2(000),C'B   ',AL1(RG),AL2(LF11A1),C'     '              
         DC    C'  ',AL2(000),C'E56 ',AL1(RG),AL2(DYALL),C'     '               
         DC    C'DD',AL2(000),C'E56 ',AL1(RG),AL2(DYDD),C'     '                
         DC    C'AP',AL2(000),C'E56 ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QG',AL2(000),C'E56 ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QP',AL2(000),C'E56 ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'  ',AL2(000),C'D   ',AL1(RG),AL2(DYALL),C'     '               
         DC    C'DD',AL2(000),C'D   ',AL1(RG),AL2(DYDD),C'     '                
         DC    C'AP',AL2(000),C'D   ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QG',AL2(000),C'D   ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'QP',AL2(000),C'D   ',AL1(RG),AL2(DYQA),C'     '                
         DC    C'X1',AL2(000),C'DE  ',AL1(RG),AL2(DYAD),C'     '                
         DC    C'  ',AL2(000),C'5   ',AL1(RG),AL2(DY10A1),C'     '              
         DC    C'  ',AL2(000),C'6   ',AL1(RG),AL2(DY1P4),C'     '               
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4AR),C'ABC  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4CR),C'CBS  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4NR),C'NBC  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4FR),C'FOX  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4WR),C'WB   '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4PR),C'PAX  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4UR),C'PAR  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4A),C'ABC  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4C),C'CBS  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4N),C'NBC  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4F),C'FOX  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4W),C'WB   '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4P),C'PAX  '          
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4U),C'PAR  '          
*        DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4),C'     '              
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(DY10A4ZR),C'     '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG+SP),AL2(DY10A4Z),C'     '          
         DC    C'C ',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'CA',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'CN',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'CL',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'PC',AL2(000),C'67  ',AL1(RG),AL2(WDCH),C'     '                
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHW),C'WB   '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHP),C'PAX  '               
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WKCHU),C'PAR  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHW),C'WB   '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHP),C'PAX  '               
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WKCHU),C'PAR  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHW),C'WB   '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHP),C'PAX  '               
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WKCHU),C'PAR  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHW),C'WB   '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHP),C'PAX  '               
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WKCHU),C'PAR  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCH),C'     '                
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHA),C'ABC  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHC),C'CBS  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHN),C'NBC  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHF),C'FOX  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHW),C'WB   '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHP),C'PAX  '               
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WKCHU),C'PAR  '               
         DC    C'CC',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'DO',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'DN',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'N ',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'IA',AL2(000),C'8   ',AL1(RG),AL2(WKI),C'     '                 
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPR),C'     '               
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRA),C'ABC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRC),C'CBS  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRF),C'FOX  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRN),C'NBC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRW),C'WB   '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WKSPRU),C'PAR  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPR),C'     '               
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRA),C'ABC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRC),C'CBS  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRF),C'FOX  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRN),C'NBC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRW),C'WB   '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WKSPRU),C'PAR  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPS),C'     '               
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSA),C'ABC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSC),C'CBS  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSF),C'FOX  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSN),C'NBC  '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSW),C'WB   '              
         DC    C'SA',AL2(000),C'8   ',AL1(SP),AL2(WKSPSU),C'PAR  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPS),C'     '               
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSA),C'ABC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSC),C'CBS  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSF),C'FOX  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSN),C'NBC  '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSW),C'WB   '              
         DC    C'SE',AL2(000),C'8   ',AL1(SP),AL2(WKSPSU),C'PAR  '              
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSP),C'     '             
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPA),C'ABC  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPC),C'CBS  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPF),C'FOX  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPN),C'NBC  '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPW),C'WB   '            
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPU),C'PAR  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSP),C'     '             
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPA),C'ABC  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPC),C'CBS  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPF),C'FOX  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPN),C'NBC  '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPW),C'WB   '            
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WKSPU),C'PAR  '            
         DC    C'A ',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'OP',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'PD',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'SM',AL2(000),C'Z   ',AL1(RG),AL2(AD24),C'     '                
         DC    C'CA',AL2(000),C'Z   ',AL1(RG),AL2(CA24),C'     '                
         DC    C'EA',AL2(000),C'Z   ',AL1(RG),AL2(CA24),C'     '                
         DC    C'CC',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'DO',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'DN',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'N ',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'IA',AL2(000),C'Z   ',AL1(RG),AL2(TI24),C'     '                
         DC    C'SA',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SA',AL2(000),C'Z   ',AL1(SP),AL2(SP24S),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(SP),AL2(SP24S),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24),C'     '             
         DC    C'SA',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24),C'     '             
         DC    C'N ',AL2(000),C'Z   ',AL1(RG+SP),AL2(NW24),C'     '             
         DC    C'N ',AL2(000),C'Z   ',AL1(RG),AL2(NW24R),C'     '               
         DC    C'N ',AL2(000),C'Z   ',AL1(SP),AL2(NW24S),C'     '               
         DC    X'FFFF'                                                          
* EXCLUDE LISTS                                                                 
*XCLIST   DC    C'X1',C'C CA CN CL  '                                           
XCLIST   DS    0CL22       AT MOST 10 EXCLUDES                                  
         DC    C'X1',CL20'C CACLCNPC'                                           
         DC    X'FFFF'                                                          
         SPACE 2                                                                
*                                                                               
* SPECIAL PROGRAM TYPE HANDLING                                                 
SPCTYPS  DS    0XL3                         COMPARE ON DAYS TOO                 
         DC    AL2(LF11A1),B'01111100'      M-F ONLY                            
         DC    AL2(WKI),B'00000001'         SUN ONLY                            
         DC    X'FFFF'                                                          
*                                                                               
* PROGRAM TYPE/ DAYPART NAMES                                                   
PTDPNAME  DS   0C                                                               
          DC    AL2(LFALL),CL25'ALL LATE FRINGE'                                
          DC    AL2(LF11A1),CL25'ALL M-F 11:30P-1.00AM(4)'                      
          DC    AL2(DYALL),CL25'M-F 7.00AM-4:30PM'                              
          DC    AL2(DYDD),CL25'DAYTIME DRAMA'                                   
          DC    AL2(DYQA),CL25'QUIZ && AUD. PARTIC (1)'                         
          DC    AL2(DYAD),CL25'ADULT 7.00AM -10.00AM'                           
          DC    AL2(DY10A1),CL25'ALL 10.00AM-1.00PM'                            
          DC    AL2(DY1P4),CL25'ALL 1.00-4:30PM'                                
          DC    AL2(DY10A4AR),CL25'ALL 10A-4P ABC REG'                          
          DC    AL2(DY10A4CR),CL25'ALL 10A-4P CBS REG'                          
          DC    AL2(DY10A4NR),CL25'ALL 10A-4P NBC REG'                          
          DC    AL2(DY10A4FR),CL25'ALL 10A-4P FOX REG'                          
          DC    AL2(DY10A4A),CL25'ALL 10A-4P ABC REG+SPEC'                      
          DC    AL2(DY10A4C),CL25'ALL 10A-4P CBS REG+SPEC'                      
          DC    AL2(DY10A4N),CL25'ALL 10A-4P NBC REG+SPEC'                      
          DC    AL2(DY10A4F),CL25'ALL 10A-4P FOX REG+SPEC'                      
          DC    AL2(DY10A4),CL25'ALL 10A-4P'                                    
          DC    AL2(WKALL),CL25'ALL WEEKEND DAYTIME'                            
          DC    AL2(WDCH),CL25'WEEKDAY CHILDREN'                                
          DC    AL2(WKCH),CL25'CHILDREN''S (1)'                                 
          DC    AL2(WKCHA),CL25'CHILDREN''S (ABC)'                              
          DC    AL2(WKCHC),CL25'CHILDREN''S (CBS)'                              
          DC    AL2(WKCHF),CL25'CHILDREN''S (FOX)'                              
          DC    AL2(WKCHN),CL25'CHILDREN''S (NBC)'                              
          DC    AL2(WKI),CL25'SUNDAY INFORMATIONAL'                             
          DC    AL2(WKSPR),CL25'SPORTS, REGULAR'                                
          DC    AL2(WKSPRA),CL25'SPORTS, REGULAR (ABC)'                         
          DC    AL2(WKSPRC),CL25'SPORTS, REGULAR (CBS)'                         
          DC    AL2(WKSPRF),CL25'SPORTS, REGULAR (FOX)'                         
          DC    AL2(WKSPRN),CL25'SPORTS, REGULAR (NBC)'                         
          DC    AL2(WKSPS),CL25'SPORTS, SPECIAL'                                
          DC    AL2(WKSPSA),CL25'SPORTS, SPECIAL (ABC)'                         
          DC    AL2(WKSPSC),CL25'SPORTS, SPECIAL (CBS)'                         
          DC    AL2(WKSPSF),CL25'SPORTS, SPECIAL (FOX)'                         
          DC    AL2(WKSPSN),CL25'SPORTS, SPECIAL (NBC)'                         
          DC    AL2(WKSP),CL25'SPORTS, TOTAL'                                   
          DC    AL2(WKSPA),CL25'SPORTS, TOTAL (ABC)'                            
          DC    AL2(WKSPC),CL25'SPORTS, TOTAL (CBS)'                            
          DC    AL2(WKSPF),CL25'SPORTS, TOTAL (FOX)'                            
          DC    AL2(WKSPN),CL25'SPORTS, TOTAL (NBC)'                            
          DC    AL2(AD24),CL25'ACTION/ADVENTURE 24'                             
          DC    AL2(CA24),CL25'CHILD ANIMATION 24'                              
          DC    AL2(TI24),CL25'INFORMATIONAL 24'                                
          DC    AL2(SP24),CL25'SPORTS ALL (REG+SPC) 24'                         
          DC    AL2(SP24R),CL25'SPORTS ALL (REG) 24'                            
          DC    AL2(SP24S),CL25'SPORTS ALL (SPC) 24'                            
          DC    AL2(NW24),CL25'NEWS TOTAL 24'                                   
          DC    AL2(NW24R),CL25'NEWS TOTAL (REG) 24'                            
          DC    AL2(NW24S),CL25'NEWS TOTAL (SPC) 24'                            
          DC    AL2(EVALL),CL25'ALL 7.00-11.00PM'                               
          DC    AL2(EVGD),CL25'GENERAL DRAMA'                                   
          DC    AL2(EVSM),CL25'SUSPENSE && MYST. DRAMA(1)'                      
          DC    AL2(EVSC),CL25'SITUATION COMEDY'                                
          DC    AL2(EVAD),CL25'ADVENTURE'                                       
          DC    AL2(EVPV),CL25'PARTICIPATION VARIETY'                           
          DC    AL2(EVFF),CL25'FEATURE FILMS'                                   
          DC    AL2(EVFFRS),CL25'FEATURE FILMS(REG+SPC)'                        
          DC    AL2(EVFFS),CL25'FEATURE FILMS (SPC)'                            
          DC    AL2(EV30),CL25'ALL 25-30 MINUTE'                                
          DC    AL2(EV60),CL25'ALL 55-60 MINUTE'                                
          DC    AL2(EV7P9),CL25'ALL 7.00-9.00PM'                                
          DC    AL2(EV9P11),CL25'ALL 9.00-11.00P'                               
          DC    AL2(EV7P11AR),CL25'ALL 7-11P ABC(REG)'                          
          DC    AL2(EV7P11CR),CL25'ALL 7-11P CBS(REG)'                          
          DC    AL2(EV7P11NR),CL25'ALL 7-11P NBC(REG)'                          
          DC    AL2(EV7P11FR),CL25'ALL 7-11P FOX(REG)'                          
          DC    AL2(EV7P11R),CL25'ALL 7-11P REG(3NET)'                          
          DC    AL2(EV7P11AS),CL25'ALL 7-11P ABC(SPC)'                          
          DC    AL2(EV7P11CS),CL25'ALL 7-11P CBS(SPC)'                          
          DC    AL2(EV7P11NS),CL25'ALL 7-11P NBC(SPC)'                          
          DC    AL2(EV7P11FS),CL25'ALL 7-11P FOX(SPC)'                          
          DC    AL2(EV7P11S),CL25'ALL 7-11P SPC(3NET)'                          
          DC    AL2(EV7P11A),CL25'ALL 7-11P ABC(REG+SPC)'                       
          DC    AL2(EV7P11C),CL25'ALL 7-11P CBS(REG+SPC)'                       
          DC    AL2(EV7P11N),CL25'ALL 7-11P NBC(REG+SPC)'                       
          DC    AL2(EV7P11F),CL25'ALL 7-11P FOX(REG+SPC)'                       
          DC    AL2(EV7P11),CL25'ALL 7-11P REG+SPC(3NET)'                       
          DC    AL2(EV7P114),CL25'ALL 7-11P REG+SPC(4NET)'                      
          DC    AL2(EV7P114R),CL25'ALL 7-11P REG(4NET)'                         
          DC    AL2(EV7P114S),CL25'ALL 7-11P SPC(4NET)'                         
          DC    AL2(EV6P7I),CL25'INFORMATIONAL ONE-A-WK(1)'                     
          DC    AL2(EV6P7IM),CL25'INFORMATIONAL MULTI-WK(1)'                    
*NEW TABLE EFFECTIVE SEP1/2003                                                  
PTDPNAS03 DS   0C                                                               
          DC    AL2(LFALL),CL25'ALL LATE FRINGE'                                
          DC    AL2(LF11A1),CL25'ALL M-F 11:30P-1.00AM(4)'                      
          DC    AL2(DYALL),CL25'M-F 7.00AM-4.30PM'                              
          DC    AL2(DYDD),CL25'DAYTIME DRAMA'                                   
          DC    AL2(DYQA),CL25'QUIZ && AUD. PARTIC (1)'                         
*         DC    AL2(DYAD),CL25'ADULT 7.00AM -10.00AM'                           
          DC    AL2(DYAD),CL25'ADULT 6.00AM -10.00AM'                           
          DC    AL2(DY10A1),CL25'ALL 10.00AM-1.00PM'                            
          DC    AL2(DY1P4),CL25'ALL 1.00-4:30PM'                                
          DC    AL2(DY10A4AR),CL25'ALL 10A-4.30P ABC REG'                       
          DC    AL2(DY10A4CR),CL25'ALL 10A-4.30P CBS REG'                       
          DC    AL2(DY10A4NR),CL25'ALL 10A-4.30P NBC REG'                       
          DC    AL2(DY10A4FR),CL25'ALL 10A-4.30P FOX REG'                       
          DC    AL2(DY10A4WR),CL25'ALL 10A-4.30P WB REG'                        
          DC    AL2(DY10A4PR),CL25'ALL 10A-4.30P ION REG'                       
          DC    AL2(DY10A4UR),CL25'ALL 10A-4.30P PAR REG'                       
          DC    AL2(DY10A4A),CL25'ALL 10A-4.30P ABC REG+SPEC'                   
          DC    AL2(DY10A4C),CL25'ALL 10A-4.30P CBS REG+SPEC'                   
          DC    AL2(DY10A4N),CL25'ALL 10A-4.30P NBC REG+SPEC'                   
          DC    AL2(DY10A4F),CL25'ALL 10A-4.30P FOX REG+SPEC'                   
          DC    AL2(DY10A4W),CL25'ALL 10A-4.30P WB REG+SPEC'                    
          DC    AL2(DY10A4P),CL25'ALL 10A-4.30P ION REG+SPEC'                   
          DC    AL2(DY10A4U),CL25'ALL 10A-4.30P PAR REG+SPEC'                   
*         DC    AL2(DY10A4),CL25'ALL 10A-4.30P'                                 
          DC    AL2(DY10A4ZR),CL25'ALL 10A-4.30P ALL REG'                       
          DC    AL2(DY10A4Z),CL25'ALL 10A-4.30P ALL REG+SPEC'                   
          DC    AL2(WKALL),CL25'ALL WEEKEND DAYTIME'                            
          DC    AL2(WDCH),CL25'WEEKDAY CHILDREN'                                
          DC    AL2(WKCH),CL25'CHILDREN''S (1)'                                 
          DC    AL2(WKCHA),CL25'CHILDREN''S (ABC)'                              
          DC    AL2(WKCHC),CL25'CHILDREN''S (CBS)'                              
          DC    AL2(WKCHF),CL25'CHILDREN''S (FOX)'                              
          DC    AL2(WKCHN),CL25'CHILDREN''S (NBC)'                              
          DC    AL2(WKCHW),CL25'CHILDREN''S (WB)'                               
          DC    AL2(WKCHP),CL25'CHILDREN''S (ION)'                              
          DC    AL2(WKCHU),CL25'CHILDREN''S (PAR)'                              
          DC    AL2(WKI),CL25'SUNDAY INFORMATIONAL'                             
          DC    AL2(WKSPR),CL25'SPORTS, REGULAR'                                
          DC    AL2(WKSPRA),CL25'SPORTS, REGULAR (ABC)'                         
          DC    AL2(WKSPRC),CL25'SPORTS, REGULAR (CBS)'                         
          DC    AL2(WKSPRF),CL25'SPORTS, REGULAR (FOX)'                         
          DC    AL2(WKSPRN),CL25'SPORTS, REGULAR (NBC)'                         
          DC    AL2(WKSPRW),CL25'SPORTS, REGULAR (WB)'                          
          DC    AL2(WKSPRU),CL25'SPORTS, REGULAR (PAR)'                         
          DC    AL2(WKSPS),CL25'SPORTS, SPECIAL'                                
          DC    AL2(WKSPSA),CL25'SPORTS, SPECIAL (ABC)'                         
          DC    AL2(WKSPSC),CL25'SPORTS, SPECIAL (CBS)'                         
          DC    AL2(WKSPSF),CL25'SPORTS, SPECIAL (FOX)'                         
          DC    AL2(WKSPSN),CL25'SPORTS, SPECIAL (NBC)'                         
          DC    AL2(WKSPSW),CL25'SPORTS, SPECIAL (WB)'                          
          DC    AL2(WKSPSU),CL25'SPORTS, SPECIAL (PAR)'                         
          DC    AL2(WKSP),CL25'SPORTS, TOTAL'                                   
          DC    AL2(WKSPA),CL25'SPORTS, TOTAL (ABC)'                            
          DC    AL2(WKSPC),CL25'SPORTS, TOTAL (CBS)'                            
          DC    AL2(WKSPF),CL25'SPORTS, TOTAL (FOX)'                            
          DC    AL2(WKSPN),CL25'SPORTS, TOTAL (NBC)'                            
          DC    AL2(WKSPW),CL25'SPORTS, TOTAL (WB)'                             
          DC    AL2(WKSPU),CL25'SPORTS, TOTAL (PAR)'                            
          DC    AL2(AD24),CL25'ACTION/ADVENTURE 24'                             
          DC    AL2(CA24),CL25'CHILD ANIMATION 24'                              
          DC    AL2(TI24),CL25'INFORMATIONAL 24'                                
          DC    AL2(SP24),CL25'SPORTS ALL (REG+SPC) 24'                         
          DC    AL2(SP24R),CL25'SPORTS ALL (REG) 24'                            
          DC    AL2(SP24S),CL25'SPORTS ALL (SPC) 24'                            
          DC    AL2(NW24),CL25'NEWS TOTAL 24'                                   
          DC    AL2(NW24R),CL25'NEWS TOTAL (REG) 24'                            
          DC    AL2(NW24S),CL25'NEWS TOTAL (SPC) 24'                            
          DC    AL2(EVALL),CL25'ALL 7.00-11.00PM'                               
          DC    AL2(EVGD),CL25'GENERAL DRAMA'                                   
          DC    AL2(EVSM),CL25'SUSPENSE && MYST. DRAMA(1)'                      
          DC    AL2(EVSC),CL25'SITUATION COMEDY'                                
          DC    AL2(EVAD),CL25'ADVENTURE'                                       
          DC    AL2(EVPV),CL25'PARTICIPATION VARIETY'                           
          DC    AL2(EVFF),CL25'FEATURE FILMS'                                   
          DC    AL2(EVFFRS),CL25'FEATURE FILMS(REG+SPC)'                        
          DC    AL2(EVFFS),CL25'FEATURE FILMS (SPC)'                            
          DC    AL2(EV30),CL25'ALL 25-30 MINUTE'                                
          DC    AL2(EV60),CL25'ALL 55-60 MINUTE'                                
          DC    AL2(EV7P9),CL25'ALL 7.00-9.00PM'                                
          DC    AL2(EV9P11),CL25'ALL 9.00-11.00P'                               
          DC    AL2(EV7P11AR),CL25'ALL 7-11P ABC(REG)'                          
          DC    AL2(EV7P11CR),CL25'ALL 7-11P CBS(REG)'                          
          DC    AL2(EV7P11NR),CL25'ALL 7-11P NBC(REG)'                          
          DC    AL2(EV7P11FR),CL25'ALL 7-11P FOX(REG)'                          
          DC    AL2(EV7P11WR),CL25'ALL 7-11P WB(REG)'                           
          DC    AL2(EV7P11PR),CL25'ALL 7-11P ION(REG)'                          
          DC    AL2(EV7P11UR),CL25'ALL 7-11P PAR(REG)'                          
          DC    AL2(EV7P11R),CL25'ALL 7-11P REG(3NET)'                          
          DC    AL2(EV7P11AS),CL25'ALL 7-11P ABC(SPC)'                          
          DC    AL2(EV7P11CS),CL25'ALL 7-11P CBS(SPC)'                          
          DC    AL2(EV7P11NS),CL25'ALL 7-11P NBC(SPC)'                          
          DC    AL2(EV7P11FS),CL25'ALL 7-11P FOX(SPC)'                          
          DC    AL2(EV7P11WS),CL25'ALL 7-11P WB(SPC)'                           
          DC    AL2(EV7P11PS),CL25'ALL 7-11P ION(SPC)'                          
          DC    AL2(EV7P11US),CL25'ALL 7-11P PAR(SPC)'                          
          DC    AL2(EV7P11S),CL25'ALL 7-11P SPC(3NET)'                          
          DC    AL2(EV7P11A),CL25'ALL 7-11P ABC(REG+SPC)'                       
          DC    AL2(EV7P11C),CL25'ALL 7-11P CBS(REG+SPC)'                       
          DC    AL2(EV7P11N),CL25'ALL 7-11P NBC(REG+SPC)'                       
          DC    AL2(EV7P11F),CL25'ALL 7-11P FOX(REG+SPC)'                       
          DC    AL2(EV7P11W),CL25'ALL 7-11P WB(REG+SPC)'                        
          DC    AL2(EV7P11P),CL25'ALL 7-11P ION(REG+SPC)'                       
          DC    AL2(EV7P11U),CL25'ALL 7-11P PAR(REG+SPC)'                       
          DC    AL2(EV7P11),CL25'ALL 7-11P REG+SPC(3NET)'                       
          DC    AL2(EV7P114),CL25'ALL 7-11P REG+SPC(4NET)'                      
          DC    AL2(EV7P114R),CL25'ALL 7-11P REG(4NET)'                         
          DC    AL2(EV7P114S),CL25'ALL 7-11P SPC(4NET)'                         
          DC    AL2(EV7P11Z),CL25'ALL 7-11P ALL(REG+SPC)'                       
          DC    AL2(EV7P11ZR),CL25'ALL 7-11P ALL(REG)'                          
          DC    AL2(EV7P11ZS),CL25'ALL 7-11P ALL(SPC)'                          
          DC    AL2(EV6P7I),CL25'INFORMATIONAL ONE-A-WK(1)'                     
          DC    AL2(EV6P7IM),CL25'INFORMATIONAL MULTI-WK(1)'                    
*                                                                               
*         ***********DAYPART NAMES**********                                    
DPNAME    DC    AL2(DP1000),CL25'MON-FRI 7.00AM - 10.00 AM'                     
          DC    AL2(DP1010),CL25'MON-FRI 10.00AM - 1.00 PM'                     
          DC    AL2(DP1020),CL25'MON-FRI 1.00PM - 4.30PM  '                     
          DC    AL2(DP1030),CL25'MON-FRI 4.30PM - 7.30PM  '                     
          DC    AL2(DP1040),CL25'MON-FRI 4.30PM - 6.00PM  '                     
          DC    AL2(DP1050),CL25'MON-FRI 6.00PM - 7.30PM  '                     
          DC    AL2(DP1060),CL25'SAT 7.00AM - 1.00PM      '                     
          DC    AL2(DP1069),CL25'SAT 1.00PM - 4.00PM      '                     
          DC    AL2(DP1070),CL25'SAT 1.00PM - 4.30PM      '                     
          DC    AL2(DP1071),CL25'SAT 1.00PM - 7.00PM      '                     
          DC    AL2(DP1078),CL25'SAT 4.00PM - 7.30PM      '                     
          DC    AL2(DP1080),CL25'SAT 4.30PM - 7.30PM      '                     
          DC    AL2(DP1090),CL25'SUN 7.00AM - 1.00PM      '                     
          DC    AL2(DP1099),CL25'SUN 1.00PM - 4.00PM      '                     
          DC    AL2(DP1100),CL25'SUN 1.00PM - 4.30PM      '                     
          DC    AL2(DP1101),CL25'SUN 1.00PM - 7.00PM      '                     
          DC    AL2(DP1110),CL25'SUN 4.30PM - 7.30PM      '                     
          DC    AL2(DP1118),CL25'SUN 4.00PM - 7.00PM      '                     
          DC    AL2(DP1119),CL25'SUN 4.00PM - 7.30PM      '                     
          DC    AL2(DP1120),CL25'SUN 4.30PM - 7.00PM      '                     
          DC    AL2(DP1130),CL25'MON-SUN 7.30PM - 8.00PM  '                     
          DC    AL2(DP1140),CL25'MON-SUN 8.00PM - 11.00PM '                     
          DC    AL2(DP1150),CL25'MON-SUN 11.00PM - 11.30PM'                     
          DC    AL2(DP1159),CL25'MON-SUN 11.00PM - 1.00AM '                     
          DC    AL2(DP1160),CL25'MON-SUN 11.30PM - 1.00AM '                     
          DC    AL2(DP1170),CL25'MON-SUN 1.00AM - 7.00AM  '                     
          DC    AL2(DP1180),CL25'MON-SUN 24 HOUR TOTAL    '                     
          DC    AL2(DP1181),CL25'MON-FRI 6.00AM - 7.00AM  '                     
          DC    AL2(DP1182),CL25'MON-FRI 7.00AM - 10.00AM '                     
          DC    AL2(DP1183),CL25'MON-FRI 9.00AM - 1.00PM  '                     
          DC    AL2(DP1185),CL25'MON-FRI 9.00AM - 4.00PM  '                     
          DC    AL2(DP1187),CL25'MON-FRI 10.00AM - 1.00PM '                     
          DC    AL2(DP1189),CL25'MON-FRI 10.00AM - 4.00PM '                     
          DC    AL2(DP1190),CL25'MON-FRI 10.00AM - 4.30PM '                     
          DC    AL2(DP1191),CL25'MON-FRI 1.00PM - 4.00PM  '                     
          DC    AL2(DP1192),CL25'MON-FRI 1.00PM - 4.30PM  '                     
          DC    AL2(DP1193),CL25'MON-FRI 3.00PM - 5.00PM  '                     
          DC    AL2(DP1194),CL25'MON-FRI 4.00PM - 6.00PM  '                     
          DC    AL2(DP1195),CL25'MON-FRI 4.00PM - 7.30PM  '                     
          DC    AL2(DP1196),CL25'MON-FRI 4.30PM - 6.00PM  '                     
          DC    AL2(DP1197),CL25'MON-FRI 4.30PM - 7.30PM  '                     
          DC    AL2(DP1198),CL25'MON-FRI 6.00PM - 7.30PM  '                     
          DC    AL2(DP1199),CL25'MON-FRI 7.00PM - 8.00PM  '                     
          DC    AL2(DP1200),CL25'MON-FRI 7.30PM - 8.00PM  '                     
          DC    AL2(DP1210),CL25'MON-FRI 11.00PM - 11.30PM'                     
          DC    AL2(DP1219),CL25'MON-FRI 11.00PM - 1.00AM '                     
          DC    AL2(DP1220),CL25'MON-FRI 11.30PM - 1.00AM '                     
          DC    AL2(DP1230),CL25'MON-FRI 1.00AM - 2.30AM  '                     
          DC    AL2(DP1240),CL25'SAT 8.00AM - 1.00PM      '                     
          DC    AL2(DP1250),CL25'SAT 7.30PM - 8.00PM      '                     
          DC    AL2(DP1259),CL25'MON-SUN 6.00PM - 8.00PM  '                     
          DC    AL2(DP1260),CL25'MON-SUN 7.00PM - 8.00PM  '                     
          DC    AL2(DP1270),CL25'MON-SUN 8.00PM - 9.00PM  '                     
          DC    AL2(DP1275),CL25'MON-SUN PRIME            '                     
          DC    AL2(DP1280),CL25'MON-SUN 9.00PM - 10.00PM '                     
          DC    AL2(DP1290),CL25'MON-SUN 10.00PM - 11.00PM'                     
          DC    AL2(DP1300),CL25'MON-SUN 11.00PM - 12.00MD'                     
          DC    AL2(DP1310),CL25'MON-SUN 12.00MD - 1.00AM '                     
          DC    AL2(DP1311),CL25'MON-SUN 1.00AM - 6.00AM  '                     
          DC    AL2(DP1320),CL25'MON 8.00PM - 11.00PM     '                     
          DC    AL2(DP1330),CL25'TUE 8.00PM - 11.00PM     '                     
          DC    AL2(DP1340),CL25'WED 8.00PM - 11.00PM     '                     
          DC    AL2(DP1350),CL25'THU 8.00PM - 11.00PM     '                     
          DC    AL2(DP1360),CL25'FRI 8.00PM - 11.00PM     '                     
          DC    AL2(DP1370),CL25'SAT 8.00PM - 11.00PM     '                     
          DC    AL2(DP1380),CL25'SUN 8.00PM - 11.00PM     '                     
          DC    AL2(DP1390),CL25'SUN 7.00PM - 11.00PM     '                     
          DC    AL2(DP1400),CL25'SAT-SUN 4.00PM - 8.00PM  '                     
          DC   X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
DPTTAB   DC    AL2(1000),AL1(1,5),AL2(0700,1000),B'01111100',X'00'              
         DC    AL2(1010),AL1(1,5),AL2(1000,1300),B'01111100',X'00'              
         DC    AL2(1020),AL1(1,5),AL2(1300,1630),B'01111100',X'00'              
         DC    AL2(1030),AL1(1,5),AL2(1630,1930),B'01111100',X'00'              
         DC    AL2(1040),AL1(1,5),AL2(1630,1800),B'01111100',X'00'              
         DC    AL2(1050),AL1(1,5),AL2(1800,1930),B'01111100',X'00'              
         DC    AL2(1060),AL1(6,6),AL2(0700,1300),B'00000010',X'60'              
         DC    AL2(1069),AL1(6,6),AL2(1300,1600),B'00000010',X'60'              
         DC    AL2(1070),AL1(6,6),AL2(1300,1630),B'00000010',X'60'              
         DC    AL2(1071),AL1(6,6),AL2(1300,1900),B'00000010',X'60'              
         DC    AL2(1078),AL1(6,6),AL2(1600,1930),B'00000010',X'60'              
         DC    AL2(1080),AL1(6,6),AL2(1630,1930),B'00000010',X'60'              
         DC    AL2(1090),AL1(7,7),AL2(0700,1300),B'00000001',X'70'              
         DC    AL2(1099),AL1(7,7),AL2(1300,1600),B'00000001',X'70'              
         DC    AL2(1100),AL1(7,7),AL2(1300,1630),B'00000001',X'70'              
         DC    AL2(1101),AL1(7,7),AL2(1300,1900),B'00000001',X'70'              
         DC    AL2(1110),AL1(7,7),AL2(1630,1930),B'00000001',X'70'              
         DC    AL2(1118),AL1(7,7),AL2(1600,1900),B'00000001',X'70'              
         DC    AL2(1119),AL1(7,7),AL2(1600,1930),B'00000001',X'70'              
         DC    AL2(1120),AL1(7,7),AL2(1630,1900),B'00000001',X'70'              
         DC    AL2(1130),AL1(1,7),AL2(1930,2000),B'01111111',X'80'              
         DC    AL2(1140),AL1(1,7),AL2(2000,2300),B'01111111',X'80'              
         DC    AL2(1150),AL1(1,7),AL2(2300,2330),B'01111111',X'80'              
         DC    AL2(1159),AL1(1,7),AL2(2300,2500),B'01111111',X'80'              
         DC    AL2(1160),AL1(1,7),AL2(2330,2500),B'01111111',X'80'              
         DC    AL2(1170),AL1(1,7),AL2(2500,2945),B'01111111',X'80'              
         DC    AL2(1180),AL1(1,7),AL2(0600,3000),B'01111111',X'80'              
         DC    AL2(1181),AL1(1,5),AL2(0600,0700),B'01111100',X'00'              
         DC    AL2(1182),AL1(1,5),AL2(0700,1000),B'01111100',X'00'              
         DC    AL2(1183),AL1(1,5),AL2(0900,1300),B'01111100',X'00'              
         DC    AL2(1185),AL1(1,5),AL2(0900,1600),B'01111100',X'00'              
         DC    AL2(1187),AL1(1,5),AL2(1000,1300),B'01111100',X'00'              
         DC    AL2(1189),AL1(1,5),AL2(1000,1600),B'01111100',X'00'              
         DC    AL2(1190),AL1(1,5),AL2(1000,1630),B'01111100',X'00'              
         DC    AL2(1191),AL1(1,5),AL2(1300,1600),B'01111100',X'00'              
         DC    AL2(1192),AL1(1,5),AL2(1300,1630),B'01111100',X'00'              
         DC    AL2(1193),AL1(1,5),AL2(1500,1700),B'01111100',X'00'              
         DC    AL2(1194),AL1(1,5),AL2(1600,1800),B'01111100',X'00'              
         DC    AL2(1195),AL1(1,5),AL2(1600,1930),B'01111100',X'00'              
         DC    AL2(1196),AL1(1,5),AL2(1630,1800),B'01111100',X'00'              
         DC    AL2(1197),AL1(1,5),AL2(1630,1930),B'01111100',X'00'              
         DC    AL2(1198),AL1(1,5),AL2(1800,1930),B'01111100',X'00'              
         DC    AL2(1199),AL1(1,5),AL2(1900,2000),B'01111100',X'00'              
         DC    AL2(1200),AL1(1,5),AL2(1930,2000),B'01111100',X'00'              
         DC    AL2(1210),AL1(1,5),AL2(2300,2330),B'01111100',X'00'              
         DC    AL2(1219),AL1(1,5),AL2(2300,2500),B'01111100',X'00'              
         DC    AL2(1220),AL1(1,5),AL2(2330,2500),B'01111100',X'00'              
         DC    AL2(1230),AL1(1,5),AL2(2500,2630),B'01111100',X'00'              
         DC    AL2(1240),AL1(6,6),AL2(0800,1300),B'00000010',X'60'              
         DC    AL2(1250),AL1(6,6),AL2(1930,2000),B'00000010',X'60'              
         DC    AL2(1259),AL1(1,7),AL2(1800,2000),B'01111111',X'80'              
         DC    AL2(1260),AL1(1,7),AL2(1900,2000),B'01111111',X'80'              
         DC    AL2(1270),AL1(1,7),AL2(2000,2100),B'01111111',X'80'              
         DC    AL2(1275),AL1(1,6),AL2(2000,2300),B'01111110',X'80'              
         DC    AL2(1275),AL1(7,7),AL2(1900,2300),B'00000001',X'80'              
         DC    AL2(1280),AL1(1,7),AL2(2100,2200),B'01111111',X'80'              
         DC    AL2(1290),AL1(1,7),AL2(2200,2300),B'01111111',X'80'              
         DC    AL2(1300),AL1(1,7),AL2(2300,2400),B'01111111',X'80'              
         DC    AL2(1310),AL1(1,7),AL2(2400,2500),B'01111111',X'80'              
         DC    AL2(1311),AL1(1,7),AL2(2500,3000),B'01111111',X'80'              
         DC    AL2(1320),AL1(1,1),AL2(2000,2300),B'01000000',X'10'              
         DC    AL2(1330),AL1(2,2),AL2(2000,2300),B'00100000',X'20'              
         DC    AL2(1340),AL1(3,3),AL2(2000,2300),B'00010000',X'30'              
         DC    AL2(1350),AL1(4,4),AL2(2000,2300),B'00001000',X'40'              
         DC    AL2(1360),AL1(5,5),AL2(2000,2300),B'00000100',X'50'              
         DC    AL2(1370),AL1(6,6),AL2(2000,2300),B'00000010',X'60'              
         DC    AL2(1380),AL1(7,7),AL2(2000,2300),B'00000001',X'70'              
         DC    AL2(1390),AL1(7,7),AL2(1900,2300),B'00000001',X'70'              
         DC    AL2(1400),AL1(6,7),AL2(1600,2000),B'00000011',X'90'              
         DC    X'FF'                                                            
         SPACE 2                                                                
PTYTAB   DC    AL2(0500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'GD',X'05'                                           
         DC    AL2(1000),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'SM',X'05'                                           
         DC    AL2(1500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'CS',X'05'                                           
         DC    AL2(1700),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'A ',X'05'                                           
         DC    AL2(3500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'PV',X'05'                                           
         DC    AL2(4500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'01'                      <--WAS X'05'         
         DC    AL2(4501),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'04'                                           
         DC    AL2(4502),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'05'                      <--WAS X'01'         
         DC    AL2(5000),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'25',X'05'                                           
         DC    AL2(5500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(060),CL2'55',X'05'                                           
         DC    AL2(6000),AL1(MSU),AL2(1900,2100),B'01111111',X'80'              
         DC    AL1(030),CL2'79',X'05'                                           
         DC    AL2(6500),AL1(MSU),AL2(2100,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'91',X'05'                                           
         DC    AL2(6510),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(6520),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(6530),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(6540),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(7000),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(7010),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7020),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7030),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7040),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7200),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7210),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7220),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'           <--WAS X'04'                    
         DC    AL2(7230),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7240),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7300),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7400),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(7410),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'              <--WAS X'05'                 
         DC    AL2(7420),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'              <--WAS X'04'                 
         DC    AL2(7500),AL1(MSU),AL2(0700,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'I1',X'05'                                           
         DC    AL2(7600),AL1(MSU),AL2(0700,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'I4',X'05'                                           
         DC    AL2(7800),AL1(MSU),AL2(2330,2500),B'01111100',X'00'              
         DC    AL1(030),CL2'11',X'05'                                           
         DC    AL2(8000),AL1(MFR),AL2(1300,1630),B'01111100',X'00'              
         DC    AL1(030),CL2'DD',X'05'                                           
         DC    AL2(8400),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'Q+',X'05'                                           
         DC    AL2(8500),AL1(MSU),AL2(0700,1000),B'01111111',X'80'              
         DC    AL1(030),CL2'A7',X'05'                                           
         DC    AL2(8600),AL1(MSU),AL2(1000,1300),B'01111111',X'80'              
         DC    AL1(030),CL2'01',X'05'                                           
         DC    AL2(8700),AL1(MSU),AL2(1300,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'14',X'05'                                           
         DC    AL2(8710),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8720),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8725),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8730),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8740),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8750),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8755),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8760),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8800),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(9000),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9010),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9011),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9012),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9013),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9100),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9510),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9511),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9512),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9513),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9600),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9600),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9610),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9611),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9612),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9613),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9700),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9710),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9711),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9712),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9713),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
*                                                                               
         DC    AL2(9831),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'01'                                           
         DC    AL2(9832),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'04'                                           
         DC    AL2(9841),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'01'                                           
*                                                                               
         DC    XL2'FFFF',AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'05'                                           
         DC    X'FF'                                                            
*NEW TABLE EFFECTIVE SEP1/2003                                                  
PTYTAS03 DC    AL2(0500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'GD',X'05'                                           
         DC    AL2(1000),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'SM',X'05'                                           
         DC    AL2(1500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'CS',X'05'                                           
         DC    AL2(1700),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'A ',X'05'                                           
         DC    AL2(3500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'PV',X'05'                                           
         DC    AL2(4500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'01'                      <--WAS X'05'         
         DC    AL2(4501),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'04'                                           
         DC    AL2(4502),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'05'                      <--WAS X'01'         
         DC    AL2(5000),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'25',X'05'                                           
         DC    AL2(5500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(060),CL2'55',X'05'                                           
         DC    AL2(6000),AL1(MSU),AL2(1900,2100),B'01111111',X'80'              
         DC    AL1(030),CL2'79',X'05'                                           
         DC    AL2(6500),AL1(MSU),AL2(2100,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'91',X'05'                                           
         DC    AL2(6510),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(6520),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(6530),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(6540),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(EV7P11WR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(EV7P11PR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(EV7P11UR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(7000),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(7010),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7020),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7030),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7040),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(EV7P11WS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(EV7P11PS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(EV7P11US),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7200),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'                                           
         DC    AL2(7210),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7220),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'           <--WAS X'04'                    
         DC    AL2(7230),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7240),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(EV7P11W),AL1(MSU),AL2(1900,2300),B'01111111',X'80'           
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(EV7P11P),AL1(MSU),AL2(1900,2300),B'01111111',X'80'           
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(EV7P11U),AL1(MSU),AL2(1900,2300),B'01111111',X'80'           
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7300),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7400),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(EV7P11ZR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'01'                                           
         DC    AL2(7410),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'04'              <--WAS X'05'                 
         DC    AL2(EV7P11ZS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'71',X'04'              <--WAS X'05'                 
         DC    AL2(7420),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'71',X'05'              <--WAS X'04'                 
         DC    AL2(EV7P11Z),AL1(MSU),AL2(1900,2300),B'01111111',X'80'           
         DC    AL1(030),CL2'71',X'05'                                           
         DC    AL2(7500),AL1(MSU),AL2(0700,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'I1',X'05'                                           
         DC    AL2(7600),AL1(MSU),AL2(0700,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'I4',X'05'                                           
         DC    AL2(7800),AL1(MSU),AL2(2330,2500),B'01111100',X'00'              
         DC    AL1(030),CL2'11',X'05'                                           
         DC    AL2(8000),AL1(MFR),AL2(1300,1630),B'01111100',X'00'              
         DC    AL1(030),CL2'DD',X'05'                                           
         DC    AL2(8400),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'Q+',X'05'                                           
*        DC    AL2(8500),AL1(MSU),AL2(0700,1000),B'01111111',X'80'              
*        DC    AL1(030),CL2'A7',X'05'   IT'S REALLY 6-10A                       
         DC    AL2(8500),AL1(MSU),AL2(0600,1000),B'01111111',X'80'              
         DC    AL1(030),CL2'A6',X'05'                                           
         DC    AL2(8600),AL1(MSU),AL2(1000,1300),B'01111111',X'80'              
         DC    AL1(030),CL2'01',X'05'                                           
         DC    AL2(8700),AL1(MSU),AL2(1300,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'14',X'05'                                           
         DC    AL2(8710),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8720),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8725),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8730),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(DY10A4WR),AL1(MSU),AL2(1000,1630),B'01111111',X'80'          
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(DY10A4PR),AL1(MSU),AL2(1000,1630),B'01111111',X'80'          
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(DY10A4UR),AL1(MSU),AL2(1000,1630),B'01111111',X'80'          
         DC    AL1(030),CL2'74',X'01'                                           
         DC    AL2(8740),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8750),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8755),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8760),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(DY10A4W),AL1(MSU),AL2(1000,1630),B'01111111',X'80'           
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(DY10A4P),AL1(MSU),AL2(1000,1630),B'01111111',X'80'           
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(DY10A4U),AL1(MSU),AL2(1000,1630),B'01111111',X'80'           
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(8800),AL1(MSU),AL2(1000,1630),B'01111111',X'80'              
*        DC    AL1(030),CL2'74',X'05'                                           
         DC    AL1(030),CL2'74',X'01'   IS ONLY REGULAR                         
         DC    AL2(DY10A4Z),AL1(MSU),AL2(1000,1630),B'01111111',X'80'           
         DC    AL1(030),CL2'74',X'05'                                           
         DC    AL2(9000),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9010),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9011),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9012),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9013),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(WKCHW),AL1(MSU),AL2(0700,2330),B'01111111',X'80'             
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(WKCHP),AL1(MSU),AL2(0700,2330),B'01111111',X'80'             
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(WKCHU),AL1(MSU),AL2(0700,2330),B'01111111',X'80'             
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9100),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'C ',X'05'                                           
         DC    AL2(9500),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9510),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9511),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9512),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9513),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(WKSPRW),AL1(MSU),AL2(0700,2330),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(WKSPRU),AL1(MSU),AL2(0700,2330),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'01'                                           
         DC    AL2(9600),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9600),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9610),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9611),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9612),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9613),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(WKSPSW),AL1(MSU),AL2(0700,2330),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(WKSPSU),AL1(MSU),AL2(0700,2330),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'04'                                           
         DC    AL2(9700),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9710),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9711),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9712),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(9713),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(WKSPW),AL1(MSU),AL2(0700,2330),B'01111111',X'80'             
         DC    AL1(030),CL2'S ',X'05'                                           
         DC    AL2(WKSPU),AL1(MSU),AL2(0700,2330),B'01111111',X'80'             
         DC    AL1(030),CL2'S ',X'05'                                           
*                                                                               
         DC    AL2(9831),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'01'                                           
         DC    AL2(9832),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'04'                                           
         DC    AL2(9841),AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'01'                                           
*                                                                               
         DC    AL2(LF11A1),AL1(MFR),AL2(2330,2500),B'01111100',X'00'            
         DC    AL1(030),CL2'11',X'01'                                           
*                                                                               
         DC    XL2'FFFF',AL1(MSU),AL2(0700,2330),B'01111111',X'80'              
         DC    AL1(030),CL2'X ',X'05'                                           
         DC    X'FF'                                                            
         EJECT                                                                  
SVUTAB   DS    570C                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
DPTTABD  DSECT                                                                  
DPTPROG  DS    XL2                                                              
DPTSDAY  DS    XL1                                                              
DPTEDAY  DS    CL1                                                              
DPTSTIM  DS    CL2                                                              
DPTETIM  DS    CL2                                                              
DPTDYBIT DS    CL1                                                              
DPTDAYWK DS    CL1                                                              
         SPACE 2                                                                
PTYTABD  DSECT                                                                  
PTYPROG  DS    XL2                                                              
PTYDAY   DS    CL1                                                              
PTYSTIM  DS    CL2                                                              
PTYETIM  DS    CL2                                                              
PTYDYBIT DS    CL1                                                              
PTYDAYWK DS    CL1                                                              
PTYDUR   DS    CL1                                                              
PTYPTYP  DS    CL2                 PROGRAM TYPE                                 
PTYDTYP  DS    CL1                 DATA TYPE                                    
*                                   X'01' = REGULAR                             
*                                   X'04' = SPECIAL                             
*                                   X'08' = ORIGINAL                            
*                                   X'10' = REPEAT                              
PTYLN    EQU   *-PTYTABD                                                        
         EJECT                                                                  
*        DENTHID                                                                
       ++INCLUDE DENTHID                                                        
         EJECT                                                                  
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
         SPACE 1                                                                
*        DEINTNTID                                                              
       ++INCLUDE DEINTNT3DL                                                     
         EJECT                                                                  
*        DECALVPHD                                                              
       ++INCLUDE DECALVPHD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
*        DEDEMCNVD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        DEDEMTABD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013DENM09I   09/30/15'                                      
         END                                                                    
**********************************************************************          
*              NAD DEMO GROUPS                                                  
**********************************************************************          
                                                                                
**********************************************************************          
* RI..=  INTERIM RECD SLOTS FOR DEMOS  (ALMOST THE SAME AS OUTPUT)              
**********************************************************************          
RIF25    EQU   0                   W2-5                                         
RIF68    EQU   1                   W6-8                                         
RIF911   EQU   2                   W9-11                                        
RIF1214  EQU   3                   W12-14                                       
RIF1517  EQU   4                   W15-17                                       
RIF1820  EQU   5                   W18-20                                       
RIF2124  EQU   6                   W21-24                                       
RIF2529  EQU   7                   W25-29                                       
RIF3034  EQU   8                   W30-34                                       
RIF3539  EQU   9                   W35-39                                       
RIF4044  EQU   10                  W40-44                                       
RIF4549  EQU   11                  W45-49                                       
RIF5054  EQU   12                  WW50-54                                      
RIF5564  EQU   13                  WW55-64                                      
RIF65O   EQU   14                  WW65+                                        
*                                                                               
RIM25    EQU   15                  M2-5                                         
RIM68    EQU   16                  M6-8                                         
RIM911   EQU   17                  M9-11                                        
RIM1214  EQU   18                  M12-14                                       
RIM1517  EQU   19                  M15-17                                       
RIM1820  EQU   20                  M18-20                                       
RIM2124  EQU   21                  M21-24                                       
RIM2529  EQU   22                  M25-29                                       
RIM3034  EQU   23                  M30-34                                       
RIM3539  EQU   24                  M35-39                                       
RIM4044  EQU   25                  M40-44                                       
RIM4549  EQU   26                  M45-49                                       
RIM5054  EQU   27                  M50-54                                       
RIM5564  EQU   28                  M55-64                                       
RIM65O   EQU   29                  M65+                                         
*                                                                               
RIF1217  EQU   30                  W1217                                        
RIM1217  EQU   31                  M1217                                        
*IHOMES  EQU   32                  HOMES                                        
*IUSA    EQU   33                  USA HOMES                                    
RIL1849  EQU   34                  LOH1849   (LADY OF HOUSE)                    
RIL50O   EQU   35                  LOH50+                                       
RIWW1849 EQU   36                  WW1849                                       
RIWW50O  EQU   37                  WW50+                                        
RIPW1849 EQU   38                  PTWW1849  (PART TIME WORKING WOMEN)          
RIPW50O  EQU   39                  PTWW50+                                      
RIMOMS   EQU   40                  WMOMS  ** NOT USED ***                       
RIF611   EQU   41                  W6-11                                        
RIM611   EQU   42                  M6-11                                        
RIWW1820 EQU   43                  WW1820                                       
RIWW2124 EQU   44                  WW2124                                       
RIWW2534 EQU   45                  WW2534                                       
RIWW3544 EQU   46                  WW3544                                       
RIWW4549 EQU   47                  WW4549                                       
RIWW5054 EQU   48                  WW5054                                       
RIWW55O  EQU   49                  WW55O                                        
RIL1824  EQU   50                  L1824                                        
RIL2534  EQU   51                  L2534                                        
RIL3544  EQU   52                  L3544                                        
RIL4549  EQU   53                  L4549                                        
RIL5054  EQU   54                  L5054                                        
RIL55O   EQU   55                  L55+                                         
         EJECT                                                                  
**********************************************************************          
* RT..=  INPUT TAPE DISPLACEMENTS FOR  --TOTAL SAMPLE--  DEMOS                  
**********************************************************************          
*                                                                               
RTF25    EQU   1                                                                
RTM25    EQU   16                                                               
RTF68    EQU   2                                                                
RTM68    EQU   17                                                               
RTF911   EQU   3                                                                
RTM911   EQU   18                                                               
RTF1214  EQU   4                                                                
RTM1214  EQU   19                                                               
RTF1517  EQU   5                                                                
RTM1517  EQU   20                                                               
RTF1820  EQU   6                                                                
RTM1820  EQU   21                                                               
RTF2124  EQU   7                                                                
RTM2124  EQU   22                                                               
RTF2529  EQU   8                                                                
RTM2529  EQU   23                                                               
RTF3034  EQU   9                                                                
RTM3034  EQU   24                                                               
RTF3539  EQU   10                                                               
RTM3539  EQU   25                                                               
RTF4044  EQU   11                                                               
RTM4044  EQU   26                                                               
RTF4549  EQU   12                                                               
RTM4549  EQU   27                                                               
RTF5054  EQU   13                                                               
RTM5054  EQU   28                                                               
RTF5564  EQU   14                                                               
RTM5564  EQU   29                                                               
RTF65O   EQU   15                                                               
RTM65O   EQU   30                                                               
*                                                                               
RTWW1820 EQU   33                                                               
RTWW2124 EQU   34                                                               
RTWW2534 EQU   35                                                               
RTWW3544 EQU   36                                                               
RTWW4549 EQU   37                                                               
RTWW5054 EQU   38                                                               
RTWW55O  EQU   39                                                               
RTPW1849 EQU   41                                                               
RTPW50O  EQU   42                                                               
RTL1824  EQU   43                                                               
RTL2534  EQU   44                                                               
RTL3544  EQU   45                                                               
RTL4549  EQU   46                                                               
RTL5054  EQU   47                                                               
RTL55O   EQU   48                                                               
         EJECT                                                                  
**********************************************************************          
* RG..=  INPUT TAPE DISPLACEMENTS FOR  --GROUP--  DEMOS                         
**********************************************************************          
*                                                                               
RGF25    EQU   501                                                              
RGM25    EQU   516                                                              
RGF611   EQU   502                                                              
RGM611   EQU   517                                                              
RGF1217  EQU   504                                                              
RGM1217  EQU   519                                                              
RGF1820  EQU   506                                                              
RGM1820  EQU   521                                                              
RGF2124  EQU   507                                                              
RGM2124  EQU   522                                                              
RGF2529  EQU   508                                                              
RGM2529  EQU   523                                                              
RGF3034  EQU   509                                                              
RGM3034  EQU   524                                                              
RGF3539  EQU   510                                                              
RGM3539  EQU   525                                                              
RGF4044  EQU   511                                                              
RGM4044  EQU   526                                                              
RGF4549  EQU   512                                                              
RGM4549  EQU   527                                                              
RGF5054  EQU   513                                                              
RGM5054  EQU   528                                                              
RGF5564  EQU   514                                                              
RGM5564  EQU   529                                                              
RGF65O   EQU   515                                                              
RGM65O   EQU   530                                                              
RGWW1849 EQU   533                                                              
RGWW50O  EQU   534                                                              
RGL1849  EQU   535                                                              
RGL50O   EQU   536                                                              
         EJECT                                                                  
