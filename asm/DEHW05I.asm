*          DATA SET DEHW05I    AT LEVEL 006 AS OF 09/11/07                      
*PHASE DEHW05IA                                                                 
*-------------------------------------------------------------------*           
* NHTI -  HISPANIC WEEKLY TAPE - INPUT PHASE                                    
*                                                                               
* IPHASE: DEHWYYI (YY=YEAR OF NEW CONVERSION)                                   
* OPHASE: DEHWYYO (YY=YEAR OF NEW CONVERSION)                                   
*                                                                               
* THIS VERSION OF THE CONVERSION SHOULD BE USED FOR FILES BEFORE                
* 8/27/07.  FOR FILES AFTER THIS DATE USE DEHW07I AND DEHW07O.                  
*-------------------------------------------------------------------*           
         TITLE 'DEMO CONVERSION - NHTI HISPANIC WEEKLY MIT'                     
DEHW05I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEHW05I*,RA,R3,R4,RR=RE                                        
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         SPACE 1                                                                
         ST    RE,RELO                                                          
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
                                                                                
***********************************************************************         
* READ - GET INPUT TAPE RECORDS ONE AT A TIME AND PROCESS.                      
*        BUILD INTERIM RECORDS.                                                 
***********************************************************************         
                                                                                
READ     CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
                                                                                
         LH    RE,=AL2(DEMLNQ)     MAKE A(PACKWT) ADDRESSABLE                   
         A     RE,=A(DEMAREA)                                                   
         ST    RE,APACKWT                                                       
                                                                                
         LA    RE,COMWRK           SAVE ADDR OF COMMON WRK AREA BETWN           
         ST    RE,ACOMWRK          INPUT AND OUTPUT PHASES                      
                                                                                
         MVI   NOTAVAL,0           DATA NOT AVAIL SWITCH                        
         MVI   BYPREAD,X'FF'       SET TO 1ST-TIME-THRU                         
                                                                                
         OPEN  (IN1,(INPUT))                                                    
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS               
         BNE   READ20                                                           
         MVI   RELOFRST,0                                                       
                                                                                
*                                  GET PROGRAM NUMBER BIT MAP                   
*                                  FOR HISPANIC                                 
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(C'H',VBITMAP1),0,RR=RELO                  
*                                  AND NETWORK (AFFILIATES)                     
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(C'N',VBITMAP2),0,RR=RELO                  
                                                                                
                                                                                
READ20   CLI   BYPREAD,C'U'        UNIVERSE RELEASE                             
         BE    RELUNV                                                           
         CLI   BYPREAD,C'P'        PROGRAM RELEASE                              
         BE    RELPRG                                                           
         CLI   BYPREAD,C'S'        PRG SUMMARY                                  
         BE    RELSUM                                                           
         CLI   BYPREAD,C'G'        USAGE RELEASE                                
         BE    RLHPROG                                                          
         CLI   BYPREAD,C'H'        USG SUMMARY                                  
         BE    RLHSUM                                                           
         CLI   BYPREAD,C'I'        STN GROUP RELEASE                            
         BE    RLGPROG                                                          
         CLI   BYPREAD,C'J'        STN GRP SUMMARY                              
         BE    RLGSUM                                                           
         CLI   BYPREAD,C'K'        HLF HOUR PRG RECD RELEASE                    
         BE    RELHLF                                                           
         CLI   BYPREAD,C'Z'        DONE W/ALL RECDS --JUST EXIT                 
         BE    ENDJOB                                                           
                                                                                
         L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'241'     HEADER  ???                                  
         CLI   BYPREAD,1           TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
*                                                                               
RDREC    GET   IN1,(RC)            GET NEXT RECORD                              
*                                                                               
         CLC   MITSEQ,=C'05'                                                    
         BE    RD25B                                                            
*                                                                               
RD25     ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,VWTYPTTB  GET A(VIEWING TYPES DESCRIPTION)             
         ICM   R5,15,DMCB          A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD TABLEID PASSED                           
         USING VWTYPTD,R5                                                       
RD25A    CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+12                                                             
         LA    R5,VWTYPTL(R5)                                                   
         B     RD25A                                                            
         MVC   KEYSRC,VWTKSRC      SAVE KEY SOURCE HERE                         
         MVC   SVVCR,VWTVCR                                                     
*                                                                               
RD25B    CLI   MITORIG,C'0'        ONLY HANDLE ORIG DATA FOR NOW                
         BNE   RDREC               NO CORRECTIONS SUPPORTED                     
         CLI   SVVCR,C' '                                                       
         BNE   READ20              PROCESS ONLY NON-VCR                         
                                                                                
         CLC   MITMKTBR,=C'511'    SPANISH DOMINANT DEMOS                       
         BNE   READ30                                                           
         CLC   MITSEQ,=C'03'       '03'S DOESNT HAVE 515                        
         BE    READ30                                                           
         CLC   MITSEQ,=C'05'       '05'S DOESNT HAVE 515                        
         BNE   RDREC                                                            
                                                                                
READ30   CLI   BYPREAD,X'FF'       1ST-TIME-THRU?                               
         BNE   READ40                                                           
         CLC   MITSEQ,=C'00'       SHOULD BE DESCRIPTIVE RECORD 00              
         BE    RDR                                                              
         DC    H'0'                                                             
                                                                                
READ40   DS    0H                  DETERMINE/BRANCH TO RECD TYPE                
         CLC   SAVEPNUM,MITPRG     WHEN PRG# CHANGES-RESET VARIABLES            
         BNE   *+14                SAME                                         
         CLC   SAVEBIT,MITBREAK    SAME BRKOUT/SPECIAL?                         
         BE    UNIVERS                                                          
                                                                                
         MVC   SAVEPNUM,MITPRG                                                  
         MVC   SAVEBIT,MITBREAK                                                 
                                                                                
         CLC   MITSEQ,=C'05'       PRINT OUT ST GROUP CODE                      
         BNE   READ40B                                                          
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   P(L'MITPRG),MITPRG  AFFILIATE CODE                               
         GOTO1 VPRINTER                                                         
                                                                                
READ40B  XC    SAVEDATE,SAVEDATE                                                
         MVI   BYPASS01,0          RESET FLAG TO ACCEPT ALL DATA                
         MVI   VARIOUS,0                                                        
         MVC   VARS,ZEROS                                                       
         MVC   DAYS,ZEROS                                                       
         XC    SAVVAR(49),SAVVAR                                                
*                                                                               
UNIVERS  CLC   MITSEQ,=C'01'       UNIVERSE RECORDS                             
         BNE   UNVREL                                                           
         CLI   MITREC,C'H'                                                      
         BE    HUE                 HOUSHOLD UNIV RECD                           
         CLI   MITREC,C'P'                                                      
         BE    PREC                DEMOS FOR UNIV RECD                          
         DC    H'0'                                                             
UNVREL   CLI   PRVUNV,1            RELEASE UNIVERSE RECD?                       
         BE    RELUNV              YES                                          
                                                                                
SAMPLCNT CLC   MITSEQ,=C'02'       SAMPLE COUNTS                                
         BE    READ20              WE DON'T USE THEM                            
                                                                                
USAGE    CLC   MITSEQ,=C'03'       USAGE RECORDS                                
         BNE   PROGRAM                                                          
         MVI   RECD,C'3'                                                        
         CLI   MITREC,C'H'                                                      
         BE    H3R                 HOUSHOLD USAGE RECD                          
         CLI   NOTAVAL,1           IGNORE THE NEXT P RECORD?                    
         BE    READ20              YES                                          
         CLI   MITREC,C'P'                                                      
         BE    PREC                DEMOS FOR USAGE RECD                         
         DC    H'0'                                                             
                                                                                
PROGRAM  CLC   MITSEQ,=C'04'       PROGRAM RECORDS                              
         BNE   STGROUP                                                          
         MVI   RECD,C'4'                                                        
         CLI   PRVUSGSW,0          RELEASE LAST USAGE RECD?                     
         BNE   RLHPROG             YES (WE WILL RTN TO 'PROGRAM')               
         CLC   MITAVG,=C'01'       SINGLE WEEK AVG?                             
         BNE   PROG5                                                            
         CLC   SAVEDATE,MITSTART   TEST BYPASS FLG IF SAME DATES?               
         BNE   *+12                DIFFERENT START-END DATES                    
         CLI   BYPASS01,1          BYPASS 1-WK AVG (W/ONLY 1 DAY IN IT)         
         BE    READ20                                                           
PROG5    MVI   BYPASS01,0          DTS/WKS DIFF->DIFF AVG->RESET FLAG           
         XC    SAVEDATE,SAVEDATE                                                
                                                                                
         CLI   MITREC,C'D'                                                      
         BE    D4RTN               PROGRAM DESCRIPTIVE RECORD                   
         CLI   NOTAVAL,1           SKIP ALL RECDS ASSOC TO THIS PRG             
         BE    READ20                                                           
         CLI   MITREC,C'H'                                                      
         BE    H4REC               HALF HOUR DETAIL RECORD                      
         CLI   NOTAVAL,2           SKIP 1/2HR P-RECS (ASSOC W/H-RECS)           
         BE    READ20              READ NEXT RECORD                             
         CLI   MITREC,C'P'                                                      
         BE    PREC                PERSONS ESTIMATES                            
         B     READ20                                                           
                                                                                
* AGGREGATES WILL NOT BE PROCESSED FOR NOW                                      
* WE EILL NOT USE THEM UNLESS ABSOLUTELY NEEDED                                 
* AS OPPOSED TO THE WEEKLY DATA THAT COMES ON THE MONTHLY TAPES AND HAS         
*   M-F AVERAGES, THIS FILE HAS INDIVIDUAL DAY DATA. SOPPORTING THEM            
*   WOULD MEAN TO KEEP AROUND TWO SETS OF DATA: INDIVIDUAL DAYS + M-F           
*   AVERAGES FROM MONTHLY TAPE.                                                 
STGROUP  CLC   MITSEQ,=C'05'       STATION GROUP RECORDS                        
         BE    READ20              DO NOT PROCESS AGGREGATES FOR NOW            
                                                                                
         BNE   READ90                                                           
         MVI   RECD,C'5'                                                        
         CLI   PRVPRGSW,0          RELEASE LAST PROGRAM RECORD?                 
         BNE   RELPRG              YES   (WE WILL RTN TO 'STGROUP')             
                                                                                
         CLI   MITREC,C'H'                                                      
         BE    H5R                 HALF HOUR DETAIL                             
         CLI   NOTAVAL,1                                                        
         BE    READ20                                                           
         CLI   MITREC,C'P'                                                      
         BE    PREC                PERSONS ESTIMATES                            
         B     READ20                                                           
                                                                                
READ90   MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                                                             
                                                                                
***********************************************************************         
* RDR -  PROCESS REPORT DESCRIPTOR RECORD                                       
***********************************************************************         
                                                                                
RDR      CLC   =C'POCKETPIECE',RDRRPT                                           
         BE    *+10                                                             
         CLC   =C'NHTI POCKETPIECE',RDRRPT                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   TAPEMKT,=H'503'     MARKET NUMBER (USED FOR DCON ACCESS)         
         MVI   BOOKTYPE,0                                                       
                                                                                
         GOTO1 VNETWEEK,DMCB,MITSTART+1,VGETDAY,VADDAY                          
         MVC   TAPEBK(1),4(R1)     YEAR                                         
         MVC   TAPEBK+1(1),8(R1)   HUT BOOK                                     
                                                                                
         CLC   TAPEBK,=AL1(107,35) FOR DATA STARTING WITH 8/27/07               
         BL    *+6                                                              
         DC    H'0'                USE A DIFFERENT CONVERSION                   
                                                                                
         MVI   BYPREAD,0           SET TO READ NEXT RECORD                      
         MVI   INTAPESW,X'40'      DROP THIS RECORD                             
                                                                                
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
         B     EXIT                                                             
                                                                                
***********************************************************************         
* HUE -  SAVE AWAY UNIVERSE HOMES                                               
***********************************************************************         
                                                                                
HUE      CLC   MITTYPE(4),=C'UES '                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   SVMPROJ,UNVEST      SAVE USA HUT UNIVERSE                        
         CLC   MITMKTBR,=C'000'                                                 
         BNE   *+10                                                             
         MVC   SVPROJ,UNVEST                                                    
                                                                                
         MVI   PRVUNV,1            SET SWITCH TO REL UNV RECD                   
         LA    RE,INTKEY                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   INTRTYP,C'P'                                                     
         MVC   INTBOOK,TAPEBK                                                   
         MVC   INTIBOOK,TAPEBK                                                  
         MVC   INTSTA(5),=C'UUUUH'                                              
         MVC   INTMRKT,TAPEMKT                                                  
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
                                                                                
HUEX     B     READ20              PROCESS NEXT RECORD                          
                                                                                
***********************************************************************         
* RELUNV - RELEASE UNIVERSE RECORDS                                             
***********************************************************************         
                                                                                
RELUNV   MVI   BYPREAD,C'U'        RELEASE UNIVERSE RECORDS                     
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         MVI   PRVUNV,0                                                         
                                                                                
RLUNV5   L     RE,=A(CRCOTAB)                                                   
RLUNV6   CLI   0(RE),X'FF'                                                      
         BE    RLUNVX                                                           
                                                                                
RLUNV8   CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLUNV6                                                           
         CLI   3(RE),0                                                          
         BNE   RLUNV10                                                          
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLUNV5                                                           
                                                                                
RLUNV10  MVC   MKTBRK,3(RE)        MKT BREAK                                    
         MVC   INTBTYP,4(RE)                                                    
         XC    INTPNUM,INTPNUM                                                  
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)    MOVE IN DEMOS                                
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         BRAS  RE,BLDKEY           OUTPUT UNV RECDS                             
         B     EXIT                                                             
                                                                                
RLUNVX   MVI   BYPREAD,0           DONE RELS ALL UNV RECDS                      
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         B     READ40              PROCESS RECD IN RREC BUFFER                  
                                                                                
***********************************************************************         
* H3R -  HOUSEHOLD USAGE RECORD                                                 
*        ONE RECORD PER 1/2HR FOR EACH DAY PLUS MON-FRI FOR TOT UNV             
*              RECORD SEQUENCE CODE = 3                                         
*              DATA TYPE CODE       = TVU                                       
*              RECORD TYPE          = H                                         
***********************************************************************         
                                                                                
H3R      CLC   MITTYPE(3),=C'TVU'  VERIFY RECD TYPE=USAGE RECD                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NOTAVAL,0           INIT FLAG TO ACCEPT ALL DATA                 
                                                                                
H3RTN    MVC   SVMPROJ,MIHPROJ     TAKE HUTS FOR ALL MKTS (INCL BREAKS)         
         CLC   MITMKTBR,=C'000'    ONLY PROCESS TOTAL SAMPLE DATA               
         BNE   READ20                                                           
                                                                                
H3RTNA   CLI   PRVUSGSW,0          DO WE HAVE A USG REC TO RELEASE?             
         BNE   RLHPROG             YES- GO DO RELEASE PROCESSING                
                                                                                
H3RTN1   MVI   PRVUSGSW,1          SET SWITCH TO RELEASE A USAGE RECD           
         MVC   SVPROJ,MIHPROJ      USA HOMES                                    
         XC    INTPNAME,INTPNAME                                                
                                                                                
         LA    R0,7                CONVERT DAYS TO INTERNAL CODE                
         SR    RE,RE                                                            
         LA    RF,MIHDAYS                                                       
H3DAY1   CLI   0(RF),C'1'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         SLL   RE,1                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,H3DAY1                                                        
         SRL   RE,1                                                             
         STC   RE,INTDYBIT                                                      
                                                                                
         LA    RE,H3DAYTAB                                                      
H3DAY2   CLC   INTDYBIT,1(RE)                                                   
         BE    H3DAY3                                                           
         LA    RE,L'H3DAYTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BNE   H3DAY2                                                           
         DC    H'0'                                                             
                                                                                
H3DAY3   MVC   INTDAYWK,2(RE)      INTERNAL CODE                                
         MVC   INTPNAME(4),3(RE)   ALPHA DAY                                    
                                                                                
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
         MVI   INTDUR,30                                                        
                                                                                
         MVI   AVGWKS,0                                                         
         XC    STDATE,STDATE                                                    
                                                                                
         GOTO1 VNETWEEK,DMCB,MITSTART+1,VGETDAY,VADDAY                          
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
                                                                                
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
                                                                                
H3R35    CLI   DUB,12                                                           
         BL    H3R30               AM                                           
         SH    RF,=H'12'                                                        
         STC   RF,DUB                                                           
         BNZ   *+8                                                              
         MVI   DUB,12              12=NOON (PM)                                 
         MVI   DUB+1,C'P'          SET TO AM                                    
                                                                                
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
         PACK  DUB,MIHDUR                                                       
         CVB   RF,DUB                                                           
         STCM  RF,3,INTCOV                                                      
                                                                                
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
                                                                                
H3R60    MVC   INTSTA(5),=C'HUT H'  PLAIN USAGE RECORD (NOT A SUMARY)           
         XC    INTRSF,INTRSF                                                    
         MVI   INTADUR,30                                                       
         MVI   INTDUR,2                                                         
         MVI   INTDURM,30                                                       
         MVC   INTMRKT,TAPEMKT     MARKET CODE NTHI OR NTHAFF                   
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
                                                                                
H3RX     B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
                                                                                
***********************************************************************         
* PREC - RECORDS HOLDING DEMO VALUES                                            
*        SLOT DEMOS FROM TAPE INTO INTERNAL BUCKETS IN DEMAREA                  
*              RECORD SEQUENCE CODE = 1,3,4,5                                   
*              RECORD TYPE          = P                                         
***********************************************************************         
                                                                                
PREC     L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
                                                                                
         CLI   RECD,C'4'           PROGRAM PRIME PORTION                        
         BNE   PREC10                                                           
         CLI   PRIMEP,0            IS A PRIME PORTION BEING PROCESSED?          
         BE    PREC10              NO                                           
         CLC   MITHLFID,SVD4HH     KEEP P'S IF THEY MATCH DRECD                 
         BE    PREC10              ALSO KEEP ACTUAL 1/2HR RECDS                 
         CLC   MITHLFID,ZEROS      BYPASS TOT PRM                               
         BE    READ20                                                           
**P      CLC   P4HALF,=C'FP'    <- IF D-BYPASSED, BYPASS P-RECD TOO             
**P      BE    READ20                                                           
**P      CLC   P4HALF,=C'PP'    <-     "            "                           
**P      BE    READ20                                                           
                                                                                
PREC10   PACK  DUB,MITDEMG                                                      
         CVB   R1,DUB                                                           
         STH   R1,DMCB+2                                                        
         PACK  DUB,MITMKTBR                                                     
         CVB   R1,DUB                                                           
         STH   R1,DMCB                                                          
         LA    R1,MIPDEM1                                                       
         ST    R1,DMCB+4                                                        
         BRAS  RE,PROCDEMS                                                      
         MVI   BYPREAD,0                                                        
PRECX    B     READ20                                                           
                                                                                
***********************************************************************         
*RLHPROG -     RELEASE REGULAR USAGE RECORDS                                    
***********************************************************************         
                                                                                
RLHPROG  MVI   BYPREAD,C'G'        OUTPUT DEMS FOR THIS USAGE RECD              
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         B     RLHPROG2            OUTPUT THE USAGE RECS FIRST                  
                                                                                
RLHPROG1 MVI   BYPREAD,C'H'        THEN CREATE THE SUMMARY                      
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         B     RLHSUM                                                           
                                                                                
RLHPROG2 L     RE,=A(CRCOTAB)                                                   
RLHPROG3 CLI   0(RE),X'FF'                                                      
         BE    RLHPROG1            DONE WITH REG USG RECS--DO SUMARY            
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLHPROG3                                                         
         CLI   3(RE),0                                                          
         BNE   RLHPROG4                                                         
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLHPROG2                                                         
                                                                                
RLHPROG4 MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH    1/4 HR ID                                 
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
                                                                                
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)    MOVE IN DEMOS                                
                                                                                
         LA    R1,INTACCS                                                       
         AH    R1,SLOTLN           NEXT FREE SLOT AFTER DEMOS                   
         ZIC   RE,RELSLOT          MOVE UNIVERSES TO INTERIM RECD               
         MH    RE,SLOTLN                                                        
         LA    RE,DUNIVS(RE)                                                    
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS (AFTER DEMOS)                  
                                                                                
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         XC    INTPNTI,INTPNTI                                                  
         BRAS  RE,BLDKEY              OUTPUT THE USAGE RECDS                    
         B     EXIT                                                             
                                                                                
* *********************************************************************         
* RLHSUM -     RELEASE SUMARY USAGE RECORDS                                     
* *********************************************************************         
                                                                                
RLHSUM   ZIC   RE,SUMSLOT                                                       
         LR    RF,RE                                                            
         MH    RE,=H'10'                                                        
         A     RE,=A(DPTTAB)                                                    
         CLI   0(RE),X'FF'                                                      
         BE    RLHSUMX                                                          
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
                                                                                
         CLI   9(RE),X'80'         M-S SUMMARY CATEGORY?                        
         BNE   RLHSUM05            NO                                           
         CLI   INTDAYWK,X'80'      YES, IS THIS A M-S RECORD?                   
         BE    RLHISUM             YES, ACCUMULATE M-S FROM INDIV DAYS          
         CLI   INTDAYWK,X'00'      M-F RECD?                                    
         BE    RLHISUM             NO, INDIV DAY DATA--SUM DAYS                 
         CLI   INTDAYWK,X'90'                                                   
         BE    RLHISUM                                                          
         CLC   0(2,RE),=Y(DMSPRIME)  COMPARE ON DAY TOO                         
         BNE   RLHSUM12                                                         
         ZIC   R1,INTDAYWK                                                      
         SRL   R1,4                                                             
         STC   R1,DUB                                                           
         CLC   DUB(1),2(RE)        START DAY                                    
         BL    RLHISUM                                                          
         CLC   DUB(1),3(RE)        END DAY                                      
         BH    RLHISUM                                                          
         B     RLHSUM12            NO, INDIV DAY DATA--SUM DAYS                 
                                                                                
RLHSUM05 CLI   9(RE),X'90'         SAT-SUN CATEGORY?                            
         BNE   RLHSUM07            NO                                           
         CLI   INTDAYWK,X'60'      YES, IS THIS A SAT RECORD?                   
         BE    RLHSUM12            YES, ACCUMULATE M-S FROM INDIV DAYS          
         CLI   INTDAYWK,X'70'      SUN RECD?                                    
         BE    RLHSUM12            YES                                          
                                                                                
RLHSUM07 CLI   9(RE),X'00'         M-F SUMMARY RECD?                            
         BNE   RLHSUM10                                                         
         CLI   INTSQH,48           PRIME TIME?                                  
         BL    RLHSUM10                                                         
         CLI   INTSQH,66                                                        
         BH    RLHSUM10                                                         
         CLI   INTDAYWK,X'60'      DO NOT INCLUDE SAT,SUN,M-S DATA              
         BE    RLHSUM10            IN M-F PRIME TIME SUMMARY RECD               
         CLI   INTDAYWK,X'70'                                                   
         BE    RLHSUM10                                                         
         CLI   INTDAYWK,X'80'                                                   
         BE    RLHSUM10                                                         
         B     RLHSUM12            ACCEPT INDIV M,T..F FOR PRIME TIME           
                                                                                
RLHSUM10 CLC   INTDAYWK,9(RE)      CHECK DAY                                    
         BNE   RLHISUM                                                          
RLHSUM12 CLC   INTSTIM(2),4(RE)                                                 
         BL    RLHISUM                                                          
         CLC   INTSTIM(2),6(RE)                                                 
         BNL   RLHISUM                                                          
         STC   RF,SUMSLOT                                                       
         MVC   INTPNUM,0(RE)       SET DAYPART NUMBER                           
                                                                                
RLHSUM2B L     R9,=A(DPNAME)                                                    
         XC    INTPNAME,INTPNAME                                                
RLHSUM3  CLC   INTPNUM,0(R9)                                                    
         BE    RLHSUM3A                                                         
         LA    R9,27(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     RLHSUM3                                                          
                                                                                
RLHSUM3A MVC   INTPNAME(25),2(R9)                                               
         L     R9,=A(DPTTAB)                                                    
         USING DPTTABD,R9                                                       
RLHSUM3B CLC   INTPNUM,0(R9)                                                    
         BE    RLHSUM3C                                                         
         LA    R9,10(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   RLHSUM3B                                                         
                                                                                
RLHSUM3C MVI   INTRTYP,C'Q'        OUTPUT SUMARY USAGE RECS AS --'Q'--          
         MVC   INTSTIM,DPTSTIM     BUILD THE CONTROL DATA                       
         MVC   INTETIM,DPTETIM                                                  
         MVC   INTSTA(5),=C'HUTDH'  SUMMARY USAGE RECD                          
         MVC   INTMRKT,TAPEMKT                                                  
         MVC   INTPTYP,=C'  '                                                   
         MVI   INTDTYP,X'09'                                                    
         MVC   INTDAYWK,DPTDAYWK                                                
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,INTPNUM                                                     
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         MVC   INTPNTI,DUB+2       SAVE SUMMARY PRG# AS 5 CHAR PWOS             
                                                                                
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         ZIC   RE,INTEQH                                                        
         ZIC   RF,INTSQH                                                        
                                                                                
         SR    RE,RF                                                            
         STC   RE,INTDUR                                                        
         MH    RE,=H'15'                                                        
         STC   RE,INTDURM          SET DURATION MINUTES                         
         MVC   INTDYBIT,DPTDYBIT   SET RECORD DAY                               
         CLI   DPTDAYWK,X'80'      FOR PRIME                                    
         BNE   *+8                                                              
         MVI   INTDYBIT,X'7F'                                                   
         DROP  R9                                                               
                                                                                
RLHSUM4A L     RE,=A(CRCOTAB)     RELEASE THE DEMOS                             
RLHSUM4B CLI   0(RE),X'FF'                                                      
         BE    RLHISUM            NO RECD TO RELEASE.DO NEXT DAY CATGY          
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLHSUM4B                                                         
         CLI   3(RE),0             DON'T OUTPUT SLOT                            
         BNE   RLHSUM4D                                                         
         ZIC   RE,RELSLOT          TRY THE NEXT ONE                             
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLHSUM4A                                                         
                                                                                
RLHSUM4D MVC   MKTBRK,3(RE)        SET OUTPUT SLOT                              
         MVC   INTBTYP,4(RE)                                                    
                                                                                
         ZIC   RE,RELSLOT                                                       
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)                                                 
                                                                                
         LA    R1,INTACCS                                                       
         AH    R1,SLOTLN           NEXT AVAIL SLOT AFTER DEMOS                  
         ZIC   RE,RELSLOT          MOVE IN UNIVS                                
         MH    RE,SLOTLN                                                        
         LA    RE,DUNIVS(RE)                                                    
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS AFTER DEMOS                    
                                                                                
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLHSUM5                                                          
                                                                                
RLHSUM5  L     R9,VCPRINT          RELEASE HUT RECORD                           
         USING DPRINT,R9                                                        
         UNPK  P+9(4),DUB+5(3)                                                  
         DROP  R9                                                               
         BRAS  RE,BLDKEY                                                        
         B     EXIT                                                             
                                                                                
RLHISUM  ZIC   RF,SUMSLOT          NEXT SUMMARY CATAGORY                        
         LA    RF,1(RF)                                                         
         STC   RF,SUMSLOT                                                       
         MVI   RELSLOT,0           PT TO 1ST MKT BREAK IN TABLE                 
         B     RLHSUM                                                           
                                                                                
RLHSUMX  MVI   BYPREAD,0                                                        
         MVI   SUMSLOT,0                                                        
         MVI   RELSLOT,0                                                        
         XC    INTSTA,INTSTA                                                    
         MVI   PRVUSGSW,0          CLEAR PREV USG RECD SWITCH                   
         CLI   RECD,C'4'           IF THIS WAS =LAST= USG RECD, THEN            
         BE    PROGRAM              RETURN TO PROGRAM RECD                      
         CLI   RECD,C'3'           ELSE IT WAS JUST ANOTHER USAGE RECD          
         BE    H3RTN1               GO PROCESS THIS USAGE (H) RECD              
         CLI   RECD,C'Z'                                                        
         BE    DONE2               YES, RELEASE 'Z' RECD FOR SRT RELS           
         DC    H'0'                                                             
                                                                                
* ********************************************************************          
* D4RTN- PROGRAM DESCRIPTOR RECORD EXTRACT FIELDS FOR INTERIM RECD              
*        ONLY PROCESS PDRS FOR TOTAL SAMPLE FOR EACH PROGRAM                    
*        IGNORE PDRS FOR EACH MKT BREAK                                         
* ********************************************************************          
                                                                                
D4RTN    MVI   NOTAVAL,0           READ PROG RECDS                              
         CLI   MITORIG,C'0'        TEST CORRECTION RECORD                       
         BNE   D4SKIP              YES, DON'T HANDLE YET                        
         CLI   MITDNA,C'1'         TEST DATA NOT AVAIL FLAG                     
         BE    D4SKIP              NO DATA AVAILABLE                            
         CLC   MITHLFID,=C'PP'                                                  
         BE    D4RTN1                                                           
         CLC   MITHLFID,=C'FP'                                                  
         BE    D4RTN1                                                           
         CLC   MITHLFID,=C'LP'                                                  
         BE    D4RTN1                                                           
         CLC   MITHLFID,=C'00'     ONLY PROCESS TOT DURATION RECORDS            
         BNE   D4SKIP                                                           
         B     D4RTN1                                                           
                                                                                
D4SKIP   MVI   NOTAVAL,1           BYPASS NEXT 'H' AND  'P' RECDS               
         B     READ20                                                           
                                                                                
D4RTN1   CLI   PRIMEP,1            PRIME PORTION PROCESSED                      
         BNE   D4RTN2                                                           
         CLC   MIREC(MITHLFID-MIREC),SVD4KEY                                    
         BE    *+12                                                             
         MVI   PRIMEP,0            RESET SWITCH ON NEW KEY                      
         B     D4R02                                                            
         CLC   MITHLFID,SVD4KEY+(MITHLFID-MIREC)                                
         BNE   READ20              BYPASS TOT PRGM RECD                         
                                                                                
D4RTN2   CLC   MITMKTBR,=C'000'    TOTAL SAMPLE?                                
         BE    D4R01A              NO,IGNORE REST OF 'D' REC ON MKTBRKS         
         CLI   PRVHLF,0            RELEASE LAST PRG 1/2HR RECD?                 
         BNE   RELHLF              YES                                          
         MVI   BYPREAD,0                                                        
         MVC   SVMPROJ,MIDPROJ     SAVE -- MKT BRKS-- HUT                       
         MVC   INTVALS(L'DTOT),DTOT  INTD FIELDS FROM TOTSMP                    
         MVC   SVPROJ,DTOTHUT      RESTORE -HUT- FROM TOT SAMP RECD             
         MVC   SVPUT,DTOTPUT       RESTORE -PUT- FROM TOT SAMP RECD             
         MVC   D4MKTBRK,MITMKTBR                                                
         B     READ20              READ NEXT RECD                               
                                                                                
D4R01A   CLI   PRVHLF,0                                                         
         BNE   RELHLF              RELEASE PREV HLF HOUR RECD                   
         CLI   PRVPRGSW,0          IF NOT THE 1ST PDR, RELEASE LAST ONE         
         BNE   RELPRG              GO OFF TO RELS RECD--THEN COME BACK          
         MVI   INTRTYP,C'Q'                                                     
         MVC   D4MKTBRK,MITMKTBR                                                
         MVC   SVPROJ,MIDPROJ      TOTAL SAMPLE HUT                             
         MVC   SVMPROJ,MIDPROJ                                                  
         MVC   DTOTHUT,SVPROJ                                                   
         MVC   SVPUT,MIDUSHUT                                                   
         MVC   DTOTPUT,SVPUT                                                    
         MVC   SVD4HH,MITHLFID                                                  
         MVC   SVD4PRG(5),MITPRG+5                                              
         MVI   PRIMEP,0                                                         
         MVC   SVD4KEY,MIREC       SAVE KEY OF THIS D-RECD                      
**P      CLC   MITHLFID,ZEROS                                                   
**P      BE    *+8                                                              
**P      MVI   PRIMEP,1            SET TO A PRIME PORTION PRESENT               
                                                                                
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
                                                                                
         MVC   DAYS,ZEROS                                                       
         MVC   VARS,ZEROS                                                       
         XC    SAVVAR(49),SAVVAR                                                
                                                                                
         CLC   MITAVG,=C'00'       INDIVIDUAL DAY DATA?                         
         BNE   D4R06               NO,  AVERAGE DATA                            
         CLI   MITBREAK,C'1'       BREAKOUT?                                    
         BE    D4R10               YES, DIFFERENT PROCESSING                    
         CLI   MITSPC,C'1'         SPECIAL?                                     
         BE    D4R10                                                            
         LA    RE,MIDDAYS          --INDIV DAY DATA (NOT BRKOUT)--              
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
D4R05    CLI   0(RE),C'0'                                                       
         BE    *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R05                                                         
         B     D4R10                                                            
                                                                                
*--AVERAGES: 01,04,05 WEEKS                                                     
D4R06    CLC   MIDDAYS,=C'1111100'  ---AVERAGED DATA --(01,04,05) M-F?          
         BE    D4R10                                                            
         CLC   MIDDAYS,=C'1111111'  M-S?                                        
         BE    D4R10                                                            
         CLC   MITAVG,=C'01'       1-WK AVG? --SET TO VARIOUS                   
         BE    D4R06V                                                           
         LA    RE,MIDDAYS          MULTI WEEK-SEE IF MORE THAN 1 DAY            
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CH    RF,=H'1'            MULTI WK AVG ACROSS 1-DAY?                   
         BE    D4R10               YES, TREAT LIKE  M-F/M-S RECDS               
                                                                                
D4R06V   CLC   MITTYPE(3),=C'SYN'  VARIOUS. SYN HAVE NO INDIV DAY               
         BNE   *+12                                                             
         MVI   VARIOUS,1                                                        
         B     D4R07                                                            
         CLI   VARIOUS,1                                                        
         BE    D4R07                                                            
         MVI   VARIOUS,1                                                        
         MVC   VARS,ZEROS          1ST TIME THRU                                
         XC    VARSTIME(56),VARSTIME                                            
                                                                                
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
         OC    INTVAR(49),INTVAR                                                
         BNZ   D4RX                                                             
         MVI   BYPASS01,1                                                       
         MVC   SAVEDATE,MITSTART   SAVE START-END DATES                         
         B     D4RX                                                             
                                                                                
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
                                                                                
D4R10    XC    PVALS,PVALS                                                      
*                                   GEN'L MKT AFFL GO INTO NET MAP              
         CLC   MITTYPE(3),=C'ABC'        *** BITMAP 2***                        
         BE    *+10                                                             
         CLC   MITTYPE(3),=C'NBC'                                               
         BE    *+10                                                             
         CLC   MITTYPE(3),=C'CBS'                                               
         BE    *+10                                                             
         CLC   MITTYPE(3),=C'FOX'                                               
         BNE   D4R12                                                            
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP2),MITPRG,RR=RELO                
         B     D4R13                                                            
                                                                                
D4R12    DS    0H                  REGULAR NHTI -> BITMAP 1                     
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),MITPRG,RR=RELO                
                                                                                
D4R13    MVC   PNUM,0(R1)          SAVE PROGRAM NUMBER                          
         MVC   PACK16(10),MITPRG   SAVE ORIG NTI#                               
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         MVC   PNTINUM,INTPNTI                                                  
                                                                                
         PACK  DUB,MIDDUR                                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,PTOTDUR        AND DURATION                                 
         MVC   PDPT,MIDAYPT        TYPE DAYPART                                 
         MVC   PDPT2,MIDREPDP                                                   
         MVC   PNET,MITTYPE        SET UP R/S CALL LETTERS                      
         MVI   PNET+3,C' '                                                      
         MVC   PNAME,MIDPNAME                                                   
         MVC   PTITLE,MIDEPNAM                                                  
         MVC   PTYP,MIDPTYP                                                     
         MVC   PPREM,MIDPREM                                                    
         MVC   PSHORT,MIDSDUR                                                   
                                                                                
         MVC   INTMRKT,TAPEMKT     SET NTHI MKT=500 OR NHTAFF=501               
         MVC   INTSTA(3),MITTYPE   SAVE NETWORK IN KEY                          
         MVC   INTSTA+3(2),=C' H'  HISPANIC                                     
                                                                                
         PACK  DUB,MIDUSHUT        PROJECTION (XXX,XXX,XXX)                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,PWK1AUD        REACH                                        
                                                                                
         TM    MIDPROJ+(L'MIDPROJ-1),X'F0'                                      
         BO    *+10                                                             
         MVC   MIDPROJ,=16C'0'                                                  
         PACK  DUB,MIDPROJ                                                      
         CVB   RF,DUB                                                           
         ST    RF,SVAVGHI          SAVE AVG HOMES IMPS                          
                                                                                
         LA    RE,TYPTAB           POINT RE AT TELECAST TYPE TABLE              
         LA    R0,TYPES            COUNTER                                      
D4R20    CLC   MITSPC,0(RE)        0=REGULAR   1=SPECIAL                        
         BNE   *+14                                                             
         CLC   MIDREP,1(RE)        WILL BE BLANK FOR NETWORK!                   
         BE    D4R30                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,D4R20                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
                                                                                
D4R30    MVC   PWK1DTYP,2(RE)      X'09'=REGUALR  X'0C'=SPECIAL                 
         CLC   MITAVG,=C'00'       INDIV DAY REC?                               
         BNE   D4R34               NO, AN AVG RECORD                            
         CLI   MIDMULT,C' '        MULTI-DAYS?                                  
         BE    *+8                                                              
         OI    PWK1DTYP,X'80'      SET OPT BIT IN PHTDTYP                       
                                                                                
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
         CLI   MITSPC,C'1'                                                      
         BNE   *+8                                                              
         MVI   INTSTYP,C'S'                                                     
         MVC   INTAUD,PWK1AUD                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
                                                                                
D4R34A   GOTO1 VNETWEEK,DMCB,MITSTART+1,VGETDAY,VADDAY                          
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
                                                                                
D4R35    MVI   PRVPRGSW,1                                                       
         LA    RE,MIDDAYS          POINT TO DAY FIELDS                          
D4R42    LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
                                                                                
D4R50    CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    *+6                                                              
         OR    RF,R5               UPDATE CUMULATIVE BITS                       
         LA    RE,1(RE)                                                         
         SRL   R5,1                NEXT DAY                                     
         BCT   R0,D4R50                                                         
                                                                                
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
                                                                                
         MVC   PDAY1,BYTE                                                       
         MVC   PDAY1BIT,INTDAYWK   NOT INTERNAL CODE                            
                                                                                
D4R60    CLC   MITHLFID,=C'PP'                                                  
         BNE   *+8                                                              
         MVI   INTSTA+3,C'P'                                                    
         CLC   MITHLFID,=C'FP'                                                  
         BNE   *+8                                                              
         MVI   INTSTA+3,C'F'                                                    
         CLC   MITHLFID,=C'LP'                                                  
         BNE   *+8                                                              
         MVI   INTSTA+3,C'L'                                                    
         CLC   MITHLFID,=C'FP'                                                  
         BE    D4R73                                                            
                                                                                
         GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
                                                                                
         MVI   INTRTYP,PMCODEQU    -Q-                                          
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         MVC   INTDURM,PWK1DURM    DURATION IN MINUTES                          
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
                                                                                
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
         ZIC   RF,PWK1DURM         DURATION IN MINUTES                          
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         STC   R0,INTEQH                                                        
                                                                                
* ADD DURATION IN MINUTES TO START TIME (MILITARY) TO                           
* GET END TIME                                                                  
         SR    R0,R0                                                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         ZIC   RF,INTDURM          DURATION IN MINUTES                          
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
                                                                                
D4R73    CLC   MITAVG,=C'00'       ---INDIVIDUAL DAY DATA ONLY----              
         BNE   D4R77                                                            
         MVC   VARS,ZEROS                                                       
         LA    RE,MIDDAYS          USE INDIVIDUAL DAY(S) FOR INTVAR             
         LA    RF,SAVVAR           SET UP START TIMES IN VAR SAVE AREA          
         LA    R0,7                                                             
D4R74    CLI   0(RE),C'0'                                                       
         BE    D4R76                                                            
         MVC   0(1,RF),INTSQH      START QH                                     
         MVC   1(1,RF),INTEQH      END QH                                       
         PACK  DUB,MIDDUR                                                       
         CVB   R1,DUB                                                           
         CH    R1,=H'240'          4-HOUR MAXIMUM SUPPORTED                     
         BNH   *+8                                                              
         LH    R1,=H'240'                                                       
         STC   R1,2(RF)            DURATION IN MINUTES                          
         MVC   3(2,RF),INTSTIM     START TIME                                   
         MVC   5(2,RF),INTETIM     END TIME                                     
D4R76    LA    RE,1(RE)                                                         
         LA    RF,7(RF)                                                         
         BCT   R0,D4R74                                                         
         CLC   MITTYPE(3),=C'SYN'  CAN BE VAR FOR SYND.                         
         BNE   D4R90                                                            
                                                                                
D4R77    CLI   VARIOUS,1           VARIOUS--FOR AVGS                            
         BNE   D4R90                                                            
         LA    RE,SAVETIME         ACCUMULATE INDIVIDUAL VAR DAY(S)             
         LA    RF,VARSTIME             INTO VARSTIME                            
         LA    R0,7                                                             
D4R78    OC    0(8,RE),0(RE)                                                    
         BZ    *+10                                                             
         MVC   0(8,RF),0(RE)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,D4R78                                                         
                                                                                
         CLC   VARS,DAYS           HAVE ALL VARIOUS DAYS PROCESSED              
         BNE   D4R90                                                            
D4R79    MVI   VARIOUS,0           RESET                                        
         MVC   DAYS,ZEROS          RESET                                        
         MVC   INTVAR(49),SAVVAR                                                
         XC    SAVVAR(49),SAVVAR                                                
         MVC   SAVETIME(56),VARSTIME                                            
         XC    VARSTIME(56),VARSTIME                                            
         LA    RE,VARS             GENERATE VAR DATA USING                      
         B     D4R42                   ACCUMULATED DAYS                         
                                                                                
D4R90    MVC   INTDAYWK,PDAY1                                                   
         OC    INTDAYWK,PWKBIT                                                  
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/PREC                          
         MVC   DTOT,INTVALS                                                     
         MVC   DTOTHUT,SVPROJ      TOTAL SAMPLE HUT                             
         MVC   DTOTPUT,SVPUT       TOTAL SAMPLE PUT HOMES                       
                                                                                
D4RX     MVI   BYPREAD,0           FOR PREC, SKIP PROCESSING AND                
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
                                                                                
* *********************************************************************         
* H4R -  HALF HOUR PROGRAM RECDS                                                
*              RECORD SEQUENCE CODE = 4                                         
*              DATA TYPE CODE       = UNI,TEL,AFFILIATES                        
*              RECORD TYPE          = H                                         
* *********************************************************************         
                                                                                
H4REC    CLI   NOTAVAL,1           BYPASS ALL 1/2HRS?                           
         BE    H4RSKIP                                                          
         CLI   PRVHLF,0            PREV 1/2HR RECD TO RELEASE?                  
         BNE   RELHLF              YES                                          
                                                                                
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
         CLI   MITSPC,C'1'                                                      
         BNE   *+8                                                              
         MVI   INTSTYP,C'S'                                                     
         MVC   INTSTA(3),MITTYPE   NETWORK'S CALL LETTERS                       
         MVC   INTSTA+3(2),=C' H'                                               
         MVI   INTBTYP,0                                                        
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
                                                                                
         GOTO1 VNETWEEK,DMCB,MITSTART+1,VGETDAY,VADDAY                          
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
                                                                                
H4R15    MVI   AVGWKS,0                                                         
         XC    STDATE,STDATE                                                    
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
                                                                                
H4R28    MVC   INTDAYWK,2(RE)      DAY & WK CODE/ DEFAULT=X'90'                 
         CLC   MITAVG,=C'01'       1-WK AVG? BYPASS IF ONLY 1 DAY IN IT         
         BL    H4R30               NO INDIV DAY DATA                            
         CLI   INTDYBIT,X'7C'      M-F RECD?                                    
         BE    H4R30                                                            
         CLI   INTDYBIT,X'7F'      M-S RECD?                                    
         BE    *+8                                                              
         B     H4RSKIP             SKIP 1/2 HR RECD                             
                                                                                
H4R30    GOTO1 VNETWEEK,DMCB,MITSTART+1,VGETDAY,VADDAY                          
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        SET WEEKLY BOOK                              
         MVC   INTIBOOK,HALF                                                    
         B     H4R35                                                            
                                                                                
H4R35    GOTO1 PDATA,DMCB,SAVETIME,INTDYBIT                                     
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         CLI   PWK1DURM,8          BYPASS DURATIONS < 8 MIN                     
         BL    H4RSKIP             SKIP THIS 1/2HR RECD                         
         MVC   INTDURM,PWK1DURM    DURATION IN MINUTES                          
         MVC   INTADUR,PWK1DURM    AVG DUR                                      
         SPACE 2                                                                
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH   P-RECD, PRG#=START QTR HOUR                
                                                                                
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
         ZIC   RF,PWK1DURM         DURATION IN MINUTES                          
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DUR OF AT LEAST ONE                 
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               START QH PLUS DUR                            
         STC   R0,INTEQH                                                        
                                                                                
         SR    R0,R0               CALCULATE INTETIM                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         ZIC   RF,INTDURM          DURATION IN MINUTES                          
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
                                                                                
         PACK  DUB,MIHDUR          COVERAGE                                     
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
         MVC   INTMRKT,TAPEMKT     SET NTHI MKT=500 OR NHTAFF=501               
         MVC   SAVEINT(L'SAVEINT),INTVALS   H-RECD'S INTERIM REC FLDS           
         MVI   PRVHLF,1            SET SWITCH TO RELEASE A USAGE RECD           
         B     H4RX                                                             
                                                                                
H4RSKIP  MVI   NOTAVAL,2           SKIP FOLLOWING P-RECDS ONLY                  
                                                                                
H4RX     B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
                                                                                
***********************************************************************         
*RELHLF - RELEASE HALF HOUR PRG RECD                                            
***********************************************************************         
                                                                                
RELHLF   CLI   RELSLOT,X'80'       END OF LIST                                  
         BE    RELHLFX             DONE PROCESSING THIS RECORD                  
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FLDS FROM H4 RECD           
         CLI   INTDAYWK,X'90'      VAR DAYS?                                    
         BE    RLHLF10                                                          
         MVI   RELSLOT,X'80'       SET TO END OF LIST                           
         B     RLHLF25                                                          
                                                                                
RLHLF10  CLI   RELSLOT,X'80'       END OF LIST                                  
         BE    RELHLFX             DONE PROCESSING THIS RECORD                  
         CLI   RELSLOT,0                                                        
         BNE   *+8                                                              
         MVI   RELSLOT,X'01'       LOOP THRU DAYS: SUN,SAT,..TUE,MON            
         ZIC   RE,RELSLOT          WILL BE PREV INIT'D TO: 0                    
         ZIC   R1,INTDYBIT                                                      
         NR    R1,RE               DAYBITS ANDED WITH CURRENT DAY               
         BNZ   RLHLF20              BIT SET, RELS RECD AS THIS DAY              
         SLL   RE,1                                                             
         STC   RE,RELSLOT                                                       
         B     RLHLF10                                                          
                                                                                
RLHLF20  LA    RE,H3DAYTAB         FIND INTERNAL RECD DAY CODE                  
RLHLF22  CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                BIT BETTER BE HERE                           
         CLC   RELSLOT,1(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'H3DAYTAB(RE)                                                
         B     RLHLF22                                                          
         MVC   INTDAYWK,2(RE)      SET INTERNAL INDIV DAY                       
                                                                                
RLHLF25  CLC   D4MKTBRK,=C'515'    SPANISH DOMINANT PRG 1/2HRS                  
         BNE   *+10                                                             
         MVC   MKTBRK,=AL1(5)                                                   
         CLC   D4MKTBRK,=C'000'                                                 
         BNE   *+10                                                             
         MVC   MKTBRK,=AL1(COUSA)  TOTAL SAMPLE ONLY                            
         MVI   INTBTYP,X'00'                                                    
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH    1/4 HR ID                                 
         LA    RE,HPRG             MOVE IN 1/2HR PRG ESTS                       
         LA    R1,NDEMS*4*2        PRG EST AND PUTS                             
         MOVE  (INTACCS,(R1)),(RE)    MOVE IN DEMOS AND PUTS                    
         MVI   BYPREAD,C'K'        LOOP THRU DAY BITS TILL RELSLOT='80'         
         CLI   RELSLOT,X'80'       DON'T LOOP?                                  
         BE    RLHLF30             YES                                          
         ZIC   RE,RELSLOT                                                       
         SLL   RE,1                BUMP TO NEXT DAY FOR NEXT TIME IN            
         STC   RE,RELSLOT                                                       
                                                                                
RLHLF30  BRAS  RE,BLDKEY              OUTPUT THE 1/2HR RECD                     
         B     EXIT                                                             
                                                                                
RELHLFX  MVI   RELSLOT,0           RESET DAYS CNTR                              
         MVI   PRVHLF,0            NO PREV RECD TO RELEASE                      
         MVI   BYPREAD,0           PROCESS CURRENT RECD IN BUFFER               
         MVI   RELSLOT,0           RESET                                        
         B     READ40                                                           
                                                                                
***********************************************************************         
*RELPRG -      RELEASE PROGRAM RECORDS                                          
***********************************************************************         
                                                                                
RELPRG   MVI   BYPREAD,C'P'                                                     
         MVC   INTVALS(L'DTOT),DTOT    RESTORE FROM D4 RECD FIELDS              
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         XC    P(132),P                                                         
         B     RELPRG10                                                         
                                                                                
RELPRG1  MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
**       MVI   BYPREAD,C'S'        DON'T CREATE SUMMRS BY PROG TYPE             
**       B     RELSUM              THIS FILE IS MISSING MIDAYPT                 
         B     RELSUMX                                                          
                                                                                
RELPRG10 L     RE,=A(CRCOTAB)                                                   
RELPRG13 CLI   0(RE),X'FF'                                                      
         BE    RELPRG1             WHEN DONE FORMING INTERD'S FOR MKTS          
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RELPRG13                                                         
         CLI   3(RE),0                                                          
         BNE   RELPRG20                                                         
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RELPRG10                                                         
*                                                                               
RELPRG20 MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         LA    RE,INTACCS          INIT/CLEAR BUFFER                            
         LHI   RF,NDEMS*4*2        SLOTLN*2                                     
         XCEF                                                                   
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
                                                                                
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)    MOVE IN DEMOS                                
                                                                                
         LA    R1,INTACCS                                                       
         AH    R1,SLOTLN           NEXT FREE SLOT AFTER DEMOS                   
         CLI   RELSLOT,0                                                        
         BNE   *+12                                                             
         LA    RE,PUTS             POINT TO PUT BUFFER                          
         B     RELPRG22                                                         
                                                                                
         CLI   RELSLOT,7           SPANISH DOMINANT                             
         BNE   RELPRG25                                                         
         LA    RE,SDPUTS           POINT TO PUT BUFFER                          
                                                                                
RELPRG22 LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)        MOVE IN PUTS (AFTER DEMOS)                  
                                                                                
RELPRG25 ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
                                                                                
         TM    INTDTYP,X'04'      SPECIAL?                                      
         BNO   RELPRG30                                                         
         DROP  R9                                                               
                                                                                
RELPRG30 BRAS  RE,BLDKEY                                                        
         B     EXIT                                                             
                                                                                
*********************************************************************           
* RELSUM -     RELEASE SUMARY PROGRAM RECORDS                                   
*********************************************************************           
                                                                                
RELSUM   ZIC   RE,SUMSLOT                                                       
         LR    RF,RE                                                            
         MH    RE,=H'16'                                                        
         A     RE,=A(PTYPTAB)                                                   
         CLC   TAPEMKT,=H'501'     NTHI AFFILIATE NETS?                         
         BE    RELSUMX             YES, DON'T DO SUMMARY RECDS                  
         CLI   INTSTYP,C'B'                                                     
         BE    RELSUMX             NO SUMMARYS FOR BREAKOUTS                    
         CLI   INTSTA+3,C'P'       PRIME/LATE/FRINGE PORTION RECDS              
         BE    RELSUMX                                                          
         CLI   INTSTA+3,C'F'                                                    
         BE    RELSUMX                                                          
         CLI   INTSTA+3,C'L'                                                    
         BE    RELSUMX                                                          
         CLI   0(RE),X'FF'                                                      
         BNE   RELSUM01                                                         
         B     RELSUMX                                                          
                                                                                
RELSUM01 MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         MVC   INTVALS(L'DTOT),DTOT                                             
         CLI   INTSTYP,C'B'                                                     
         BE    RELSUMX             NO SUMMARYS FOR BREAKOUTS                    
         MVI   INTSTYP,0                                                        
                                                                                
RELSUM02 CLI   INTSTYP,C'B'                                                     
         BE    RELSUMX                                                          
         MVI   INTSTYP,0                                                        
         MVC   BYTE,INTDTYP        TEST REG OR SPECIAL                          
         NC    BYTE,8(RE)                                                       
         CLI   BYTE,0                                                           
         BE    INCRSUM                                                          
         CLI   BYTE,SP                                                          
         BNE   *+12                                                             
         CLI   INTDURM,5           EXCLUDE DURATIONS OF UNDER 5  MIN            
         BL    INCRSUM                                                          
         CLI   PSHORT,C'Y'         AND SHORT DURATION PROGRAMS                  
         BE    INCRSUM                                                          
         CLC   0(2,RE),=C'  '      CHECK PROGRAM TYP (BLANK MEANS ALL)          
         BE    RELSUM1                                                          
                                                                                
*ADULT GROUP CONSISTS OF ALL PRG CATEGORIES EXCLUDING CHILDREN'S                
         CLC   0(2,RE),=C'X1'      ADULT GROUP?-EXCL. CHILD CATEGS              
         BNE   RELSUMA                                                          
         CLC   PTYP,=C'C '         CHILD MULTI WEEKLY?                          
         BE    INCRSUM                                                          
         CLC   PTYP,=C'CL'         CHILD LIVE?                                  
         BE    INCRSUM                                                          
         CLC   PTYP,=C'CA'         CHILD ANIMATION                              
         BE    INCRSUM                                                          
         CLC   PTYP,=C'CN'         CHILD NEWS/INFO                              
         BE    INCRSUM                                                          
         B     RELSUMA1                                                         
                                                                                
RELSUMA  CLC   0(2,RE),PTYP                                                     
         BNE   INCRSUM                                                          
RELSUMA1 OC    2(2,RE),2(RE)       CHECK SPOT LENGTH                            
         BZ    RELSUM1                                                          
         CLC   INTDURM,2(RE)                                                    
         BL    INCRSUM                                                          
         CLC   INTDURM,3(RE)                                                    
         BH    INCRSUM                                                          
                                                                                
RELSUM1  CLC   11(3,RE),=C'   '    CHECK NETWORK                                
         BE    *+14                WAS *+10, WHY I DON'T KNOW                   
         CLC   11(3,RE),PNET                                                    
         BNE   INCRSUM                                                          
         LA    R1,4(RE)            CHECK DAYPART                                
                                                                                
RELSUM2  CLI   0(R1),C' '          LOOP THRU DAYPT POSSIBILES IN TABLE          
         BE    INCRSUM                                                          
         CLI   0(R1),C'Z'          ACCEPT ANY DAYPART                           
         BE    RELSUM21                                                         
         CLC   0(1,R1),PDPT        OR BE SPECIFIC                               
         BE    RELSUM21                                                         
         LA    R1,1(R1)            TRY NEXT DAYPART TYPE IN TABLE ENTRY         
         B     RELSUM2                                                          
                                                                                
RELSUM21 CLI   11(RE),C' '         SPECIFIC NETWORK?                            
         BNE   *+10                                                             
         MVC   INTSTA,=C'HUT H'                                                 
         MVI   INTSTA+3,C'T'       'T' FOR PRG TYPES HUT                        
                                                                                
         STC   RF,SUMSLOT                                                       
         SR    R1,R1                                                            
         ICM   R1,3,9(RE)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         STCM  R1,3,INTPNUM        SET OUTPUT PRG # & NAME (FROM TBLS)          
*                                  HANDLE SPECIAL PRG NUMBERS                   
         CLC   INTPNUM,=AL2(WKD11P1)     WEEKEND DATA                           
         BNE   *+12                                                             
         TM    INTDYBIT,X'03'      IS SAT OR SUN BIT ON?                        
         BZ    INCRSUM                                                          
         CLC   INTPNUM,=AL2(MF11P1) WEEKDAY DATA                                
         BNE   *+12                                                             
         TM    INTDYBIT,X'7C'      ANY MON-FRI BITS SET?                        
         BZ    INCRSUM                                                          
                                                                                
         CLC   INTPNUM,=AL2(WKDIN)   WEEKEND INFOR DATA                         
         BNE   *+12                                                             
         TM    INTDYBIT,X'03'      IS SAT OR SUN BIT ON?                        
         BZ    INCRSUM                                                          
         CLC   INTPNUM,=AL2(MFINF) MON-FRI INFOR DATA                           
         BNE   *+12                                                             
         TM    INTDYBIT,X'7C'      ANY MON-FRI BITS SET?                        
         BZ    INCRSUM                                                          
                                                                                
RELSUM2B L     R9,=A(PTDPNAME)     SET PRG TYPE NAME                            
         XC    INTPNAME,INTPNAME                                                
RELSUM3  CLC   INTPNUM,0(R9)                                                    
         BE    RELSUM3A                                                         
         LA    R9,27(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     RELSUM3                                                          
                                                                                
RELSUM3A MVC   INTPNAME(25),2(R9)                                               
         L     R9,=A(PTYTAB)                                                    
         USING PTYTABD,R9                                                       
RELSUM3B CLC   INTPNUM,0(R9)                                                    
         BE    RELSUM3C                                                         
         LA    R9,PTYLN(R9)                                                     
         CLI   0(R9),X'FF'                                                      
         BNE   RELSUM3B                                                         
RELSUM3C MVC   INTSTIM,PTYSTIM     BUILD THE CONTROL DATA                       
         MVC   INTETIM,PTYETIM                                                  
         MVC   INTDURM,PTYDUR                                                   
         ZIC   RF,INTDURM                                                       
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
                                                                                
RELSUM4A L     RE,=A(CRCOTAB)      RELEASE THE DEMOS                            
RELSUM4B CLI   0(RE),X'FF'                                                      
         BE    INCRSUM                                                          
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RELSUM4B                                                         
         CLI   3(RE),0             DON'T OUTPUT SLOT                            
         BNE   RELSUM4D                                                         
         ZIC   RE,RELSLOT          TRY THE NEXT ONE                             
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RELSUM4A                                                         
                                                                                
RELSUM4D MVC   MKTBRK,3(RE)        SET OUTPUT SLOT                              
         MVC   INTBTYP,4(RE)                                                    
         LA    RE,INTACCS          INIT/CLEAR BUFFER                            
         LHI   RF,NDEMS*4*2        SLOTLN*2                                     
         XCEF                                                                   
         ZIC   RE,RELSLOT          MOVE IN DEMOS                                
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)                                                 
                                                                                
         LA    R1,INTACCS                                                       
         AH    R1,SLOTLN           NEXT FREE SLOT AFTER DEMOS FOR UNIVS         
         CLI   RELSLOT,0                                                        
         BNE   RELSUM5                                                          
         LA    RE,PUTS             POINT TO TOTAL SAMPLE PUTS                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS AFTER DEMOS                    
         B     RELSUM6                                                          
                                                                                
RELSUM5  CLI   RELSLOT,7           SPANISH DOMINANT PUTS                        
         BNE   RELSUM6                                                          
         LA    RE,SDPUTS                                                        
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
                                                                                
RELSUM6  ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         BRAS  RE,BLDKEY                                                        
         B     EXIT                                                             
                                                                                
INCRSUM  ZIC   RF,SUMSLOT          NEXT SUMMARY CATAGORY                        
         LA    RF,1(RF)                                                         
         STC   RF,SUMSLOT                                                       
         MVI   RELSLOT,0           PT TO TOP OF MKT BRK TABLE                   
         B     RELSUM                                                           
                                                                                
RELSUMX  MVI   BYPREAD,0                                                        
         MVI   SUMSLOT,0                                                        
         MVI   RELSLOT,0                                                        
         XC    INTSTA,INTSTA                                                    
         MVI   PRVPRGSW,0          CLEAR PREV PRG RECD SWITCH                   
         CLI   RECD,C'5'           IF THIS WAS =LAST= PRG RECD, THEN            
         BE    STGROUP              RETURN TO STATION GROUP RECORD              
         CLI   RECD,C'4'           ELSE IT WAS JUST ANOTHER PROGRAM RCD         
         BE    D4R01A               GO PROCESS THIS PRG (D) RECD                
         CLI   RECD,C'Z'           EOF FOR TAPEMKT=501?                         
         BE    DONE2               YES, RELS 'Z' RECD FOR SORT RELS             
         DC    H'0'                                                             
                                                                                
*********************************************************************           
* OUTSLOT- SLOT THE INTERIM DEMOS IN THE OUTPUT BUCKETS                         
*********************************************************************           
                                                                                
OUTSLOT  NTR1                                                                   
         LA    RE,INTACCS          CLEAR OUT INTACCS                            
         LA    RF,1000                                                          
         XCEF                                                                   
         CLC   INTSTA(5),=C'UUUUH' UNIVERSE RECORD?                             
         BE    OUTSL20             SLOT DIFFERENTLY                             
                                                                                
*SLOT DEMOS FROM DEMAREA IN THE INTERIM RECD (TO INTACCS)                       
         LA    RE,INTACCS          DESTINATION OF OUTPUT DEMOS                  
         L     R7,=A(DEMAREA)      SOURCE                                       
         L     R9,=A(OUTDEMS)                                                   
                                                                                
OUTSL10  CLI   0(R9),X'FF'         END OF TABLE?--DONE SLOTTING DEMOS           
         BE    OUTSL15             GO DO PUTS                                   
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
                                                                                
OUTSL15  DS    0H                  PROCESS PUTS                                 
         LA    RE,INTACCS+PUTDSPQ  DESTINATION OF PUTS                          
         L     R7,=A(DEMAREA)      SOURCE                                       
         AH    R7,SLOTLN           PUTS BEGIN AT END OF DEMO LIST               
         L     R9,=A(OUTUNVS)      SAME DISP FORMAT AS UNIVS DO                 
         B     OUTSL30                                                          
         SPACE 1                                                                
                                                                                
OUTSL20  DS    0H                  PROCESS UNIVS                                
         LA    RE,INTACCS+UNVDSPQ  DESTINATION FOR OUTPUT  ==UNIVS==            
         L     R7,=A(DEMAREA)      SOURCE                                       
         L     R9,=A(OUTUNVS)      TELLS WHAT UNIVS GO INTO OUTP CATAG          
         SPACE 1                                                                
                                                                                
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
                                                                                
OUTSLX   XIT1                                                                   
                                                                                
* *********************************************************************         
* H5R -  STATION GROUP RECORDS                                                  
*        ONE RECORD PER 1/2HR FOR EACH DAY PLUS MON-FRI FOR TOT UNV             
*        AND FOR EACH MKT BREAK --FOR EACH STATION GROUP                        
*              RECORD SEQUENCE CODE = 5                                         
*              DATA TYPE CODE       = AGG                                       
*              RECORD TYPE          = H                                         
***********************************************************************         
                                                                                
H5R      CLC   MITTYPE(3),=C'AGG'                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NOTAVAL,0           BYPASS DAILY AVG RECORDS                     
                                                                                
H5RTN    MVC   SVMPROJ,MIHPROJ     MARKET BREAK HOMES                           
         CLC   MITMKTBR,=C'000'                                                 
         BNE   READ20                                                           
         CLI   PRVSTASW,0          FIRST TIME STATION GROUP RECORD?             
         BNE   RLGPROG             NO, RELEASE LAST ONE                         
                                                                                
H5RTN1   MVI   PRVSTASW,1          SET SWITCH TO RELEASE A USAGE RECD           
         MVC   SVPROJ,MIHPROJ      USA HUT HOMES                                
         MVC   SVPUT,MIHUSHUT      USA PUT HOMES                                
         XC    INTPNAME,INTPNAME                                                
                                                                                
         LA    R0,7                CONVERT DAYS TO INTERNAL CODE                
         SR    RE,RE                                                            
         LA    RF,MIHDAYS                                                       
H5DAY1   CLI   0(RF),C'1'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         SLL   RE,1                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,H5DAY1                                                        
         SRL   RE,1                                                             
         STC   RE,INTDYBIT                                                      
                                                                                
         LA    RE,H3DAYTAB                                                      
H5DAY2   CLC   INTDYBIT,1(RE)                                                   
         BE    H5DAY3                                                           
         LA    RE,L'H3DAYTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BNE   H5DAY2                                                           
         DC    H'0'                                                             
                                                                                
H5DAY3   MVC   INTDAYWK,2(RE)      INTERNAL CODE                                
         MVC   INTPNAME(4),3(RE)   ALPHA DAY                                    
                                                                                
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
         MVI   INTDUR,30                                                        
         MVI   INTKEY+1,C'W'       SET MEDIA IN KEY                             
                                                                                
         GOTO1 VNETWEEK,DMCB,MITSTART+1,VGETDAY,VADDAY                          
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF                                                    
                                                                                
*CONVERT START TIME FOR INTPNAME FIELD - CHAR OUTPUT                            
H5R29    PACK  DUB,MITHOUR(2)                                                   
         CVB   RF,DUB                                                           
         STC   RF,DUB              DUB=HOUR 06(6AM)-29(5AM)                     
         CLI   DUB,24              24-29= 12AM-5AM                              
         BL    H5R35                                                            
         SH    RF,=H'24'                                                        
         STC   RF,DUB                                                           
         BNZ   *+8                                                              
         MVI   DUB,12              24=12 MIDNIGHT(AM)                           
H5R30    MVI   DUB+1,C'A'          SET TO AM                                    
         B     H5R38                                                            
                                                                                
H5R35    CLI   DUB,12                                                           
         BL    H5R30               AM                                           
         SH    RF,=H'12'                                                        
         STC   RF,DUB                                                           
         BNZ   *+8                                                              
         MVI   DUB,12              12=NOON (PM)                                 
         MVI   DUB+1,C'P'          SET TO AM                                    
                                                                                
H5R38    MVI   DUB+2,C'M'          DUB=HOUR   DUB+1=AM/PM                       
         MVI   INTPNAME+4,C' '                                                  
         MVI   INTPNAME+7,C':'                                                  
         MVC   INTPNAME+8(2),MITMIN    MINUTE                                   
         MVC   INTPNAME+10(2),DUB+1     AM/PM                                   
         ZIC   RE,DUB               RE=START HOUR FOR INTPNAME                  
         LA    RF,INTPNAME+5                                                    
         EDIT  (RE),(2,0(RF))       OUTPUT START HOUR                           
         CLI   INTPNAME+5,C' '                                                  
         BNE   *+14                                                             
         MVC   INTPNAME+5(6),INTPNAME+6                                         
         MVI   INTPNAME+11,0                                                    
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
         PACK  DUB,MIHDUR                                                       
         CVB   RF,DUB                                                           
         STCM  RF,3,INTCOV                                                      
                                                                                
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
                                                                                
         MVC   INTSTA(5),=C'    H'   STATION RECORD   (NOT SUMMARY)             
         PACK  DUB,MITPRG+3(7)       STATION GROUP CODE                         
         CVB   RF,DUB                                                           
         STC   RF,INTSTA                                                        
                                                                                
         L     RE,=A(NETTAB)                                                    
         LA    R0,NETTABQ          NUMBER OF NETWORKS IN THE TABLE              
         CLC   INTSTA(1),1(RE)                                                  
         BE    H5R50                                                            
         LA    RE,L'NETTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                UNKNOWN NET CODE                             
                                                                                
H5R50    MVC   INTSTA(3),2(RE)     MOVE IN ALPHA AGGREGATE CODE                 
         MVC   INTMRKT,5(RE)       MOVE IN MKT NUMBER                           
         CLC   INTMRKT,=H'500'     REG NHTI TAPE                                
         BNE   H5R60                                                            
                                                                                
H5R60    MVI   INTADUR,30                                                       
         MVI   INTDUR,2                                                         
         MVI   INTDURM,30                                                       
         XC    INTRSF,INTRSF                                                    
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
                                                                                
H5RX     B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
                                                                                
***********************************************************************         
* RLGPROG - STATION GROUP RECORDS RELEASE                                       
***********************************************************************         
                                                                                
RLGPROG  MVI   BYPREAD,C'I'        OUTPUT DEMS FOR THIS STA GRP RECD            
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         XC    P(132),P                                                         
         B     RLGPROG2            OUTPUT THE USAGE RECS FIRST                  
                                                                                
RLGPROG1 MVI   BYPREAD,C'J'        THEN CREATE THE SUMMARY                      
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         B     RLGSUM                                                           
                                                                                
RLGPROG2 L     RE,=A(CRCOTAB)                                                   
RLGPROG3 CLI   0(RE),X'FF'                                                      
         BE    RLGPROG1            DONE WITH REG USG RECS--DO SUMARY            
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLGPROG3                                                         
         CLI   3(RE),0                                                          
         BNE   RLGPROG4                                                         
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLGPROG2                                                         
                                                                                
RLGPROG4 MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH    1/4 HR ID                                 
         XC    INTPNTI,INTPNTI                                                  
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
                                                                                
         LA    RE,INTACCS          INIT/CLEAR BUFFER                            
         LHI   RF,NDEMS*4*2        SLOTLN*2                                     
         XCEF                                                                   
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)    MOVE IN DEMOS                                
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         BRAS  RE,BLDKEY              OUTPUT THE STATION GROUP REC (P)          
         B     EXIT                                                             
                                                                                
* *********************************************************************         
* RLGSUM - STATION GROUP SUMARY RECORDS                                         
* *********************************************************************         
                                                                                
RLGSUM   ZIC   RE,SUMSLOT                                                       
         LR    RF,RE                                                            
         MH    RE,=H'10'                                                        
         A     RE,=A(DPTTAB)                                                    
         CLI   0(RE),X'FF'                                                      
         BE    RLGSUMX                                                          
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
                                                                                
         CLI   9(RE),X'80'         M-S SUMMARY CATEGORY?                        
         BNE   RLGSUM05            NO                                           
         CLI   INTDAYWK,X'80'      YES, IS THIS A M-S RECORD?                   
         BE    RLGISUM             YES, ACCUMULATE M-S FROM INDIV DAYS          
         CLI   INTDAYWK,X'00'      M-F RECD?                                    
         BE    RLGISUM             YES, IGNORELATE M-S FROM INDIV DAYS          
         CLI   INTDAYWK,X'90'                                                   
         BE    RLGISUM                                                          
         CLC   0(2,RE),=Y(DMSPRIME)  COMPARE ON DAY TOO                         
         BNE   RLGSUM12                                                         
         ZIC   R1,INTDAYWK                                                      
         SRL   R1,4                                                             
         STC   R1,DUB                                                           
         CLC   DUB(1),2(RE)        START DAY                                    
         BL    RLGISUM                                                          
         CLC   DUB(1),3(RE)        END DAY                                      
         BH    RLGISUM                                                          
         B     RLGSUM12            NO, INDIV DAY DATA--SUM DAYS                 
                                                                                
RLGSUM05 CLI   9(RE),X'90'         SAT-SUN CATEGORY?                            
         BNE   RLGSUM07            NO                                           
         CLI   INTDAYWK,X'60'      YES, IS THIS A SAT RECORD?                   
         BE    RLGSUM12            YES, ACCUMULATE M-S FROM INDIV DAYS          
         CLI   INTDAYWK,X'70'      SUN RECD?                                    
         BE    RLGSUM12            YES                                          
                                                                                
RLGSUM07 CLI   9(RE),X'00'         M-F SUMMARY RECD?                            
         BNE   RLGSUM10                                                         
*        CLI   INTSQH,48           PRIME TIME?                                  
*        BL    RLGSUM10            TAKE OUT BECAUSE WE DON'T RECEIVE            
*        CLI   INTSQH,66           M-F RECORDS FOR PRIME ON THIS FILE           
*        BH    RLGSUM10                                                         
         CLI   INTDAYWK,X'60'      DO NOT INCLUDE SAT,SUN,M-S DATA              
         BE    RLGSUM10            IN M-F PRIME TIME SUMMARY RECD               
         CLI   INTDAYWK,X'70'                                                   
         BE    RLGSUM10                                                         
         CLI   INTDAYWK,X'80'                                                   
         BE    RLGSUM10                                                         
         B     RLGSUM12            ACCEPT INDIV M,T..F FOR PRIME TIME           
                                                                                
RLGSUM10 CLC   INTDAYWK,9(RE)      CHECK FOR EXACT MATCH ON DAY                 
         BNE   RLGISUM                                                          
RLGSUM12 CLC   INTSTIM(2),4(RE)                                                 
         BL    RLGISUM                                                          
         CLC   INTSTIM(2),6(RE)                                                 
         BNL   RLGISUM                                                          
         STC   RF,SUMSLOT                                                       
         MVC   INTPNUM,0(RE)       SET DAYPART NUMBER                           
                                                                                
RLGSUM2B L     R9,=A(DPNAME)                                                    
         XC    INTPNAME,INTPNAME                                                
RLGSUM3  CLC   INTPNUM,0(R9)                                                    
         BE    RLGSUM3A                                                         
         LA    R9,27(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     RLGSUM3                                                          
RLGSUM3A MVC   INTPNAME(25),2(R9)                                               
         L     R9,=A(DPTTAB)                                                    
         USING DPTTABD,R9                                                       
RLGSUM3B CLC   INTPNUM,0(R9)                                                    
         BE    RLGSUM3C                                                         
         LA    R9,10(R9)                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   RLGSUM3B                                                         
                                                                                
RLGSUM3C MVI   INTRTYP,C'Q'        OUTPUT SUMARY TIME PERD AS 'Q' RECD          
         MVC   INTSTIM,DPTSTIM     BUILD THE CONTROL DATA                       
         MVC   INTETIM,DPTETIM                                                  
         MVC   INTSTA+3(2),=C'DH'                                               
         MVC   INTPTYP,=C'  '                                                   
         MVI   INTDTYP,X'09'                                                    
         MVC   INTDAYWK,DPTDAYWK                                                
         SR    R1,R1                                                            
         ICM   R1,3,INTPNUM                                                     
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         MVC   INTPNTI,DUB+2       SAVE SUMMARY PRG# AS 5 CHAR PWOS             
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         ZIC   RE,INTEQH                                                        
         ZIC   RF,INTSQH                                                        
                                                                                
         SR    RE,RF                                                            
         STC   RE,INTDUR                                                        
         MH    RE,=H'15'                                                        
         STC   RE,INTDURM          SET DURATION MINUTES                         
         MVC   INTDYBIT,DPTDYBIT   SET RECORD DAY                               
         CLI   DPTDAYWK,X'80'      FOR PRIME                                    
         BNE   *+8                                                              
         MVI   INTDYBIT,X'7F'                                                   
         DROP  R9                                                               
                                                                                
RLGSUM4A L     RE,=A(CRCOTAB)     RELEASE THE DEMOS                             
RLGSUM4B CLI   0(RE),X'FF'                                                      
         BE    RLGISUM            NO RECD TO RELEASE.DO NEXT DAY CATGY          
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     RLGSUM4B                                                         
         CLI   3(RE),0             DON'T OUTPUT SLOT                            
         BNE   RLGSUM4D                                                         
         ZIC   RE,RELSLOT          TRY THE NEXT ONE                             
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLGSUM4A                                                         
                                                                                
RLGSUM4D MVC   MKTBRK,3(RE)        SET OUTPUT SLOT                              
         MVC   INTBTYP,4(RE)                                                    
                                                                                
         LA    RE,INTACCS          INIT/CLEAR BUFFER                            
         LHI   RF,NDEMS*4*2        SLOTLN*2                                     
         XCEF                                                                   
         ZIC   RE,RELSLOT          MOVE IN DEMOS                                
         MH    RE,SLOTLN                                                        
         A     RE,=A(DEMAREA)                                                   
         LH    RF,SLOTLN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INTACCS(0),0(RE)                                                 
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RLGSUM5                                                          
                                                                                
RLGSUM5  BRAS  RE,BLDKEY              RELEASE HUT RECORD                        
         B     EXIT                                                             
                                                                                
RLGISUM  ZIC   RF,SUMSLOT          NEXT SUMMARY CATAGORY                        
         LA    RF,1(RF)                                                         
         STC   RF,SUMSLOT                                                       
         MVI   RELSLOT,0           PT TO 1ST MKT BREAK IN TABLE                 
         B     RLGSUM                                                           
                                                                                
RLGSUMX  MVI   BYPREAD,0                                                        
         MVI   SUMSLOT,0                                                        
         MVI   RELSLOT,0                                                        
         XC    INTSTA,INTSTA                                                    
         MVI   PRVSTASW,0          CLEAR PREV STA GRP RECD SWITCH               
         CLI   RECD,C'Z'           IF THIS WAS =LAST= STA RECD, THEN            
         BE    DONE2               RELS 'Z' RECD FOR SORT RELEASE               
         CLI   RECD,C'5'           ELSE IT WAS JUST ANOTHER STA GRP RCD         
         BE    H5RTN1               GO PROCESS THIS STATION RECD                
         DC    H'0'                                                             
                                                                                
* *******************************************************************           
* CNVWR - CALCULATE MISSING ACCUMULATED DEMO BUCKETS AND WEIGHTS.               
* SORT RECORD HOOK                                                              
* NON-NETWORK M-F ROTATORS EMITTED FROM INDIVIDUAL DAY RECORDS                  
* DUPLICATE HUT RECORDS DROPPED                                                 
* *******************************************************************           
                                                                                
CNVWR    MVI   BYPSORT,X'80'       SET NO RELEASE OF SORT REC                   
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
         ZAP   0(8,R1),=P'0'       DEMS AND UNIVS                               
         LA    R1,8(R1)            NEXT BUCKET IN PACKWT                        
         BCT   R0,*-10                                                          
         L     RE,=A(DEMAREA)      DEMAREA TEMP HOLDS FINAL DEMOS               
         LA    RF,NDEMS*2*4        (DEMS AND UNIVS)*4BYTE BUCKETS               
         XCEF                                                                   
         LA    RE,INTACCS          PT TO CURRENT KEY'S DEMO VALUES              
         B     CNV70               FOR FIRST TIME IN,SAVE DEMS- NO REL          
                                                                                
CNV20    LA    R1,INTKEY                                                        
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
                                                                                
* CONVERT PACKED DEMOS TO BINARY AND STORE IN DEMAREA                           
CNV25    CLI   INTKEY,PRCODEQU     'P*NTEL H'                                   
         BNE   CNV29                                                            
         CLC   INTKEY+PRSRC-PRKEY(6),=C'NTEL H'                                 
         BNE   CNV29                                                            
         CLI   MKTBRK,COUSA        BACKUP RIUSA VALUE FROM TOTAL SAMP           
         BNE   CNV26                                                            
         MVC   SVKEY1(PRBTYP-PRKEY+3),INTKEY  SAVE KEY UP TO MKT BREAK          
         LA    RE,INTACCS                                                       
         MVC   SVSPROJ,RIUSA*4(RE)                                              
         B     CNV29                                                            
                                                                                
CNV26    CLI   MKTBRK,5            SPANISH DOMINANT?                            
         BNE   CNV29                                                            
         CLC   INTKEY(PRBTYP-PRKEY+3),SVKEY1  EVERYTHING ELSE SAME              
         BNE   CNV29                                                            
         LA    RE,INTACCS                                                       
         MVC   RIUSA*4(4,RE),SVSPROJ     STORE INTO SLOT                        
                                                                                
CNV29    MVC   PRVRSF,MKTBRK       NO, SAVE NEW TYPE OF RECD                    
         L     RE,=A(DEMAREA)      STORE RESULT OF PACKING HERE                 
         ZICM  RF,CURRDUR,2        DIVISOR IN PACK8                             
         CVD   RF,PACK8                                                         
         L     RF,APACKWT          DIVIDEND                                     
         LA    R0,NDEMS*2          LOOP CNTL= #DEMS AND UNIVS                   
                                                                                
CNV30    ZAP   PACK16,0(8,RF)      8 BYTE PACKED DEMO BUCKET                    
         DP    PACK16,PACK8                                                     
         CVB   R1,PACK16           ONLY HIGH 8 BYTES!!!                         
                                                                                
         CLI   INTSTA+3,C'D'       DAYPART SUMMARIES,                           
         BE    CNV31               MATCH ROUNDING TO OUR MONTHLY AVGS           
         MVC   WORK(16),PACK16     M-F AVERAGES,                                
         MP    WORK+8(8),=PL1'2'   MATCH ROUNDING TO NIELSEN'S AVGS             
         CP    WORK+8(8),PACK8                                                  
         BL    *+8                                                              
         AHI   R1,1                                                             
                                                                                
CNV31    ST    R1,0(RE)            STORE 4 BYTES IN DEMAREA                     
         LA    RE,4(RE)            NEXT DEMAREA BUCKET                          
         LA    RF,8(RF)            NEXT PACKWT BUCKET                           
         BCT   R0,CNV30                                                         
                                                                                
* SWAP SAVEKEY WITH INTKEY IN ORDER TO RELEASE PREVIOUS RECD                    
         XC    SAVEKEY,INTKEY                                                   
         XC    INTKEY(L'SAVEKEY),SAVEKEY                                        
         XC    SAVEKEY,INTKEY                                                   
         LA    RE,SAVEREC                                                       
         MVC   0(NDEMS*4,RE),INTACCS    SAVE CURRENT RECD'S DEMOS               
         LA    RE,NDEMS*4(RE)                                                   
         MVC   0(NDEMS*4,RE),INTACCS+NDEMS*4    AND PUTS                        
                                                                                
* SLOT OUTPUT DEMOS IN INTACCS                                                  
         BAS   RE,OUTSLOT          SLOT OUTPUT INTO RIGHT BUCKETS               
         LA    RE,INTACCS          DESTINATION                                  
                                                                                
         CLC   INTSTA(5),=C'UUUUH'                                              
         BE    CNV35                                                            
         OC    INTACCS(NDEMS*4),INTACCS                                         
         BNZ   CNV35                                                            
         OC    INTACCS+PUTDSPQ(NDEMS*4),INTACCS+PUTDSPQ                         
         BZ    CNV40               NO DEMOS OR PUTS - DON'T RELEASE             
                                                                                
CNV35    MVI   BYPSORT,0           RELEASE RECD                                 
         TM    INTDTYP,X'04'      SPECIAL?                                      
         BNO   CNV40                                                            
         LA    R1,INTKEY                                                        
         CLI   PMBTYP-PMKEY+3(R1),1                                             
         BNE   CNV40                                                            
         CLI   PMMEDIA-PMKEY(R1),C'W'                                           
         BNE   CNV40                                                            
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   P(25),INTPNAME                                                   
         MVC   P+15(1),INTKEY+1                                                 
         LA    R1,INTACCS                                                       
         MVC   PHUT,ODHOMES*4(R1)                                               
         EDIT  (4,PHUT),(8,P+35)       HUT                                      
         EDIT  (2,INTCOV),(4,P+50)     COVERAGE                                 
         MVC   P+55(2),INTDPT2                                                  
         MVC   P+60(1),INTDPT                                                   
         MVC   P+65(25),INTTITLE                                                
         MVC   P+90(30),INTKEY                                                  
         DROP  R9                                                               
                                                                                
CNV40    L     R1,APACKWT          CLEAR TO P'0' FOR NEXT TIME                  
         LA    R0,NDEMS*2          DEMS+UNIVS                                   
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         XC    CURRDUR,CURRDUR                                                  
         L     RE,=A(DEMAREA)      CLEAR SAVE AREA FOR NEXT TIME                
         LH    RF,=Y(NDEMS*4*2)                                                 
         XCEF                                                                   
         LA    RE,SAVEREC          RE PTS TO CURRENT KEY'S DEMOS                
                                                                                
CNV70    L     R1,APACKWT           PACKED WEIGHTED DEMOS                       
         CLI   GAASW,C'G'                                                       
         BE    CNV80                                                            
         LH    RF,HALF                                                          
         AH    RF,CURRDUR                                                       
         STH   RF,CURRDUR                                                       
         B     *+8                                                              
CNV80    LA    R1,NDEMS*8(R1)      ??????????????????????                       
                                                                                
* WEIGHT DEMOS AND STORE IN PACKWT                                              
         LH    RF,HALF                                                          
         CVD   RF,DUB1             DUB1=WEIGHT                                  
         LA    R0,NDEMS*2          DEMS AND PUTS                                
                                                                                
CNV90    L     RF,0(RE)            0(RE) = CURRENT DEMO                         
*                                  RE SET TO INTACCS OR SAVEREC.                
         CVD   RF,DUB              DUB=DECIMAL DEMO                             
         MP    DUB,DUB1+5(3)                                                    
         AP    0(8,R1),DUB                                                      
         LA    RE,4(RE)            NEXT DEMO                                    
         LA    R1,8(R1)            NEXT PACKED WEIGHTED DEMO                    
         BCT   R0,CNV90                                                         
                                                                                
* SAVE REL REC AND EXIT                                                         
CNVX     L     RF,AWREC                                                         
         XC    0(256,RF),0(RF)                                                  
         CLI   BYPSORT,X'80'       NO RELEASE                                   
         BE    CNVX1                                                            
         LR    RE,R2                                                            
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
CNVX1    B     EXIT                                                             
                                                                                
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
                                                                                
PDATA2   ZIC   RF,TMP                                                           
         EX    RF,*+8              TEST IF PROGRAM RAN ON DAY                   
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    PDATA4              NO                                           
         OC    0(8,R6),0(R6)       BYPASS IF NO TIMES                           
         BZ    PDATA4                                                           
                                                                                
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
                                                                                
PDATA4   ZIC   RF,TMP              GET DAY                                      
         SRL   RF,1                NEXT DAY                                     
         STC   RF,TMP                                                           
         LA    R6,L'SAVETIME(R6)   NEXT DAY'S TIME DATA                         
         BCT   R5,PDATA2                                                        
                                                                                
PDATA6   SR    RE,RE               CALCULATE AVERAGE DURATION                   
         LR    RF,R7               DURATION                                     
         SLDA  RE,1                                                             
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         CH    RF,=H'240'          4-HOUR MAXIMUM DURATION SUPPORTED            
         BNH   *+8                                                              
         LH    RF,=H'240'                                                       
         STC   RF,PWK1DURM         RETURN AVE DURATION                          
                                                                                
         L     R0,FULL1            SORT TIMES IN DESCENDING ORDER               
         GOTO1 VXSORT,DMCB,(X'FF',TIMTAB),(R0),3,1,2                            
         MVC   PWK1STIM,TIMTAB     FIRST OR MOST FREQUENT TIME                  
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO UPDATE RUN TIME TABLE                                              
*              HALF CONTAINS TIME                                               
*              FULL1 CONTAINS TABLE ENTRY COUNT                                 
***********************************************************************         
                                                                                
UPTIME   LR    R0,RE                                                            
         LA    R1,7                COUNTER                                      
         LA    RE,TIMTAB                                                        
                                                                                
UPTIME2  OC    0(L'TIMTAB,RE),0(RE)     TEST FOR E-O-T                          
         BZ    UPTIME4                  INSERT ENTRY                            
         CLC   HALF,0(RE)               TEST IF TIME IS IN TABLE                
         BE    *+12                     YES-BUMP FREQUENCY COUNT                
         LA    RE,L'TIMTAB(RE)                                                  
         BCT   R1,UPTIME2                                                       
                                                                                
         ZIC   RF,2(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,2(RE)                                                         
         B     UPTIMEX                                                          
                                                                                
UPTIME4  MVC   0(2,RE),HALF        ADD NEW ENTRY                                
         MVI   2(RE),1             SET FREQUENCY TO 1                           
         L     R1,FULL1                                                         
         LA    R1,1(R1)            INCREMENT TABLE ENTRY COUNT                  
         ST    R1,FULL1                                                         
                                                                                
UPTIMEX  LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
                                                                                
DONE     MVI   RECD,C'Z'           DONE                                         
         CLI   PRVPRGSW,0          RELEASE LAST PRG  RECD?                      
         BNE   RELPRG                                                           
         CLI   PRVSTASW,0          RELEASE LAST STA GRP RECD?                   
         BNE   RLGPROG                                                          
                                                                                
DONE2    MVI   INTKEY,C'Z'                                                      
         MVI   INTRTYP,C'Z'                                                     
         MVI   BYPREAD,C'Z'                                                     
         B     EXIT                                                             
                                                                                
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
                                                                                
* LITERAL POOL                                                                  
         LTORG                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ACOMWRK- COMMON WORK AREA BETWEEN INPUT AND OUTPUT PHASE                      
*          DO NOT MESS AROUND WITH ORDER OF VARIABLES--MAKE SURE                
*          OUTPUT PHASE AGREES WITH ANY CHANGES                                 
*---------------------------------------------------------------------*         
COMWRK   DS    0H                                                               
AVGWKS   DC    X'00'               # WEEKS IN AVG: '00','01','04','05'          
STDATE   DS    CL6                 START DATE OF IREC                           
*                                                                               
                                                                                
* TABLE OF DAY BIT SETTINGS AND DAY CODES                                       
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
                                                                                
* DAY CONVERSION TABLE                                                          
H3DAYTAB DS    0CL7                                                             
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
                                                                                
* INTAB COUNTS TABLE                                                            
*              BYTES 1-3 = DATE (YYMMDD)                                        
*              BYTE 4 = DAY (MON=X'40' - SUN=X'01')                             
*              BYTES 5-156 = COUNTS                                             
         DS    0F                                                               
INTAB    DS    0XL156                                                           
         DS    XL3,XL1,38F         MON                                          
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
INTABS   EQU   (*-INTAB)/L'INTAB                                                
         DC    XL3'00'                                                          
                                                                                
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
                                                                                
* TABLE OF TELECAST TYPES AND THEIR BIT SETTINGS USED FOR INTDTYP               
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
                                                                                
*---------------------------------------------------------------------*         
* WORKING STORAGE                                                               
* GENERAL AREAS                                                                 
*---------------------------------------------------------------------*         
         DS    0F                                                               
RELO     DS    A                                                                
         DS    0D                                                               
PACK8    DS    PL8                                                              
PACK16   DS    PL16                                                             
PRVRSF   DC    X'00'                                                            
CNWR1ST  DC    X'01'                                                            
CURRDUR  DC    H'00'                                                            
RELOFRST DC    X'01'                                                            
BYPREAD  DC    X'00'                                                            
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES                   
GAASW    DC    X'FF'               GROSS AVG AUD SWITCH                         
RECD     DC    X'00'               TYPE OF RECD JUST READ FROM TAPE             
PRVUSGSW DC    X'00'               HAVE A PREVIOUS USAGE RECD                   
PRVPRGSW DC    X'00'               HAVE A PREVIOUS PROGRAM                      
PRVUNV   DC    X'00'               RELEASE LAST UNIVERSE RECD                   
PRVHLF   DC    X'00'               HAVE A PREVIOUS PRG HALF HOUR RECD           
PRVSTASW DC    X'00'               HAVE A PREVIOUS STATION RECD                 
RELSLOT  DC    X'00'               RELEASE SLOT                                 
SUMSLOT  DC    X'00'               SUMMARY SLOT                                 
MI3FLAG  DC    X'00'                                                            
MKTBRK   DS    X                   MKT BREAK NUMBER                             
QHSFRST  DS    C                                                                
QHRSW    DS    C                                                                
BYTE     DS    C                                                                
TMP      DS    C                                                                
CNVWRSW  DS    C                                                                
VARIOUS  DS    X                   1=VARIOUS                                    
WEEK     DS    CL6                 REPORT DESCRIPTOR START DATE                 
DSCSTDT  DS    CL6                 REPORT DESCRIPTOR START DATE                 
DSCENDT  DS    CL6                 REPORT DESCRIPTOR END DATE                   
WKBOOK   DS    CL2                 SAVED NETWEEK BOOK                           
WKIBOOK  DS    CL2                 SAVED NETWEEK IBOOK                          
WKDAYWK  DS    CL1                 SAVED INTDAYWK                               
TAPEMKT  DS    H                   NTHI=500  NHTAFF=501                         
TAPEBK   DC    XL2'0000'           BOOK FROM REPORT DESCR RECD                  
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
KEYSRC   DS    C                                                                
SVVCR    DS    C                                                                
PHUT     DS    CL9                                                              
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
SVD4PRG  DC    CL5'0000'    SAVE LAST D4 PRG NUMBER PROCESED                    
SVD4HH   DC    CL2'  '      SAVE HALF HOUR FLD (WHEN=PP,FP,LP)                  
PRIMEP   DC    X'00'                                                            
SAVETIME DS    7XL8                SAVE MITHOUR, MITMIN AND MIDDUR              
VARSTIME DS    7XL8                SAVE MITHOUR, MITMIN AND MIDDUR              
SAVVAR   DS    7XL7                SAVE VAR INFO FROM INDIVIDUAL DAYS           
SAVEBIT  DS    XL2                                                              
THREE    DS    CL3                                                              
         DS    0F                                                               
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD                          
SAVENET  DS    CL3                 SAVE NETWORK                                 
SAVEPNUM DS    CL10                SAVE PROGRAM NUMBER                          
SAVEDATE DS    CL14                SAVE START/END DATES OF RECS TO BYPS         
SAVESTYP DS    C                   SAVE AUDIENCE EST TYPE                       
SAVESYND DS    CL3                 SAVE SYNDICATOR                              
DTOT     DS    CL(INTACCS-INTVALS)   WAS 168 ???                                
DTOTHUT  DS    CL9                                                              
DTOTPUT  DS    CL9                                                              
D4MKTBRK DS    CL3                 MARKET BRK ON LAST D'RECD                    
         DS    0F                                                               
         DC    C'*STSREC*'                                                      
SAVERREC DS    CL150                                                            
         DC    C'*ENDREC*'                                                      
SVD4KEY  DS    CL115               NTI KEY OF RREC                              
TIMTAB   DS    7CL3                                                             
                                                                                
* PROGRAM DESCRIPTOR SAVE AREA                                                  
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
PWKBIT   DS    X                   WEEK BITS                                    
PDAY1    DS    X                   DAY CODE                                     
PDAY1BIT DS    X                   DAY BITS                                     
PTOTDUR  DS    XL2                 TOTAL DURATION                               
                                                                                
PWK1STIM DS    XL2                 START TIME                                   
PWK1DURM DS    X                   DURATION IN MINUTES                          
PWK1AUD  DS    XL2                                                              
PWK1STAC DS    XL2                                                              
PWK1COV  DS    XL2                                                              
PWK1DTYP DS    X                                                                
PWK1RSF  DS    X                                                                
                                                                                
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
                                                                                
*        THESE FIELDS ARE NOT REPORTED ON ALL H4 RECORDS                        
SVPROJ   DS    CL9                 USA PROJ                                     
SVSPROJ  DS    XL4                 USA PROJ  (SPANISH DOMINANT)                 
SVMPROJ  DS    CL9                 MKT BREAK AUD. PROJ. (XXX,XXX,XXX)           
SVSMPROJ DS    XL4                 MKT BREAK (SPANISH DOMINANT)                 
SVPUT    DS    CL9                 PUT PROJ                                     
SVSPUT   DS    XL4                 PUT PROJ  (SPANISH DOMINANT)                 
SVRECLN  DS    F                   SAVE INTERIM RECORD'S LENGTH                 
                                                                                
* FILE DCB AND BUFFER AREA                                                      
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=DONE                                                       
*EFFECTIVE WITH THE JAN 1, 2007 NHTI WEEKLY DATA, THE RECORDS ON THE            
*NIELSEN FILE HAVE A LENGTH OF 400 INSTEAD OF 401.  THE OPTIONAL INDCTR         
*HAS BEEN REMOVED AND THE NEW FILE FORMAT IS KNOWN AS 'DELUXE'.                 
*REMOVED THE DCB RECORD LENGTH AND BLOCK SIZE, WHICH ARE NOW GIVEN BY           
*THE JCL.                                                                       
*              LRECL=401,                                                       
*              BLKSIZE=16842,                                                   
*THIS CONVERSION RUNS VIA NFS.                                                  
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
APACKWT  DS    A                   A(PACKWT) AREA                               
IPGAASW  DS    C                                                                
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
FLAG     DS    X                                                                
         DS    0F                                                               
SLOTLN   DC    AL2(NDEMS*4)                                                     
HPRG     DS    XL(NDEMS*4)         1/2HR PRG EST DEMOS                          
HPUTS    DS    XL(NDEMS*4)         1/2HR PRG PUT DEMOS                          
PUTS     DS    XL(NDEMS*4)         PUT BUFFER                                   
SDPUTS   DS    XL(NDEMS*4)         SPANISH DOMINANT PUTS                        
SAVEKEY  DS    CL(INTACCS-INTKEY)  KEY OF ACCUMULATED DEMOS                     
SVKEY1   DS    CL(INTACCS-INTKEY)                                               
         DS    0F                                                               
SAVEREC  DS    XL(NDEMS*4*2)       4BYTE DEMOS + UNIVS                          
                                                                                
UVHUTMKT DS    CL(L'UNVEST)        HUT UNV FOR TOTAL USA                        
UVHUTUNV DS    CL(L'UNVEST)        HUT UNV FOR MKT BRK                          
                                                                                
         DC    CL24'***DUNIVS****DUNIVS***'                                     
         DS    0F                                                               
DUNIVS   DS    (NDEMS*NMKTS)XL4    DEMO UNIVERSES                               
                                                                                
         DC    CL24'***DEMAREA****DEMAREA***'                                   
         DS    0F                                                               
DEMAREA  DS    (NDEMS*80)XL4       DEMOS                                        
DEMLNQ   EQU   *-DEMAREA                                                        
PACKWT   DS    (NDEMS*2)PL8        WEIGHTING FACTOR PACKED                      
                                                                                
MSU      EQU   8                                                                
MFR      EQU   0                                                                
NDEMS    EQU   56                  UNION OF TOTAL SAMPLE W/DEMO TABLE           
NMKTS    EQU   1                   INCLUDES TOTAL SAMPLE=000                    
                                                                                
*--------------------------------------------------------------------*          
* RI =  INTERIM RECD SLOTS FOR DEMOS  (ALMOST THE SAME AS OUTPUT)               
*--------------------------------------------------------------------*          
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
                                                                                
RIF1217  EQU   30                  W1217                                        
RIM1217  EQU   31                  M1217                                        
RIHOMES  EQU   32                  HOMES                                        
RIUSA    EQU   33                  USA HOMES                                    
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
                                                                                
*--------------------------------------------------------------------*          
* RT = INPUT TAPE DISPLACEMENTS FOR  --TOTAL SAMPLE--  DEMOS                    
*--------------------------------------------------------------------*          
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
                                                                                
*--------------------------------------------------------------------*          
* RG = INPUT TAPE DISPLACEMENTS FOR  --GROUP--  DEMOS                           
*--------------------------------------------------------------------*          
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
                                                                                
*--------------------------------------------------------------------*          
* OUTDEM -OUTPUT BUCKETS FOR DEMOS                                              
*--------------------------------------------------------------------*          
ODW25    EQU   0                   UW2-5                                        
ODW68    EQU   1                   UW6-8                                        
ODW911   EQU   2                   UW9-11                                       
ODW1214  EQU   3                   UW12-14                                      
ODW1517  EQU   4                   UW15-17                                      
ODW1820  EQU   5                   UW18-20                                      
ODW2124  EQU   6                   UW21-24                                      
ODW2529  EQU   7                   UW25-29                                      
ODW3034  EQU   8                   UW30-34                                      
ODW3539  EQU   9                   UW35-39                                      
ODW4044  EQU   10                  UW40-44                                      
ODW4549  EQU   11                  UW45-49                                      
ODW5054  EQU   12                  UW50-54                                      
ODW5564  EQU   13                  UW55-64                                      
ODW65O   EQU   14                  UW65+                                        
                                                                                
ODM25    EQU   15                  UM2-5                                        
ODM68    EQU   16                  UM6-8                                        
ODM911   EQU   17                  UM9-11                                       
ODM1214  EQU   18                  UM12-14                                      
ODM1517  EQU   19                  UM15-17                                      
ODM1820  EQU   20                  UM18-20                                      
ODM2124  EQU   21                  UM21-24                                      
ODM2529  EQU   22                  UM25-29                                      
ODM3034  EQU   23                  UM30-34                                      
ODM3539  EQU   24                  UM35-39                                      
ODM4044  EQU   25                  UM40-44                                      
ODM4549  EQU   26                  UM45-49                                      
ODM5054  EQU   27                  UM50-54                                      
ODM5564  EQU   28                  UM55-64                                      
ODM65O   EQU   29                  UM65+                                        
                                                                                
ODW1217  EQU   30                  UW1217                                       
ODM1217  EQU   31                  UM1217                                       
ODHOMES  EQU   32                  HOMES                                        
ODUSA    EQU   33                  USA HOMES                                    
ODL1849  EQU   34                  ULOH1849   (LADY OF HOUSE)                   
ODL50O   EQU   35                  ULOH50+                                      
ODWW1849 EQU   36                  UWW1849                                      
ODWW50O  EQU   37                  UWW50+                                       
ODPW1849 EQU   38                  UPTWW1849  (PART TIME WORKING WOMEN)         
ODPW50O  EQU   39                  UPTWW50+                                     
ODWMOMS  EQU   40                  UWMOMS                                       
ODW611   EQU   41                  UW6-11                                       
ODM611   EQU   42                  UM6-11                                       
ODWW1820 EQU   43                  UWW1820                                      
ODWW2124 EQU   44                  UWW2124                                      
ODWW2534 EQU   45                  UWW2534                                      
ODWW3544 EQU   46                  UWW3544                                      
ODWW4549 EQU   47                  UWW4549                                      
ODWW5054 EQU   48                  UWW5054                                      
ODWW55O  EQU   49                  UWW55O                                       
ODL1824  EQU   50                  UL1824                                       
ODL2534  EQU   51                  UL2534                                       
ODL3544  EQU   52                  UL3544                                       
ODL4549  EQU   53                  UL4549                                       
ODL5054  EQU   54                  UL5054                                       
ODL55O   EQU   55                  UL55+                                        
                                                                                
*---------------------------------------------------------------------*         
* OUTDEMS - OUTPUT DEMOS FORMAT.TRANSLATE INTERIM SLOTS TO OUTPUT SLOTS         
*           DEMOS STORED IN OUTPUT RECD STARTING AT:   INTACCS + 0              
*---------------------------------------------------------------------*         
OUTDEMS  DS    0XL2                                                             
         DC    AL1(RIF25,ODW25)        W2-5                                     
         DC    AL1(RIF68,ODW68)        W6-8                                     
         DC    AL1(RIF911,ODW911)      W9-11                                    
         DC    AL1(RIF1214,ODW1214)    W12-14                                   
         DC    AL1(RIF1517,ODW1517)    W15-17                                   
         DC    AL1(RIF1820,ODW1820)    W18-20                                   
         DC    AL1(RIF2124,ODW2124)    W21-24                                   
         DC    AL1(RIF2529,ODW2529)    W25-29                                   
         DC    AL1(RIF3034,ODW3034)    W30-34                                   
         DC    AL1(RIF3539,ODW3539)    W35-39                                   
         DC    AL1(RIF4044,ODW4044)    W40-44                                   
         DC    AL1(RIF4549,ODW4549)    W45-49                                   
         DC    AL1(RIF5054,ODW5054)    W50-54                                   
         DC    AL1(RIF5564,ODW5564)    W55-64                                   
         DC    AL1(RIF65O,ODW65O)      W65+                                     
                                                                                
         DC    AL1(RIM25,ODM25)        M2-5                                     
         DC    AL1(RIM68,ODM68)        M6-8                                     
         DC    AL1(RIM911,ODM911)      M9-11                                    
         DC    AL1(RIM1214,ODM1214)    M12-14                                   
         DC    AL1(RIM1517,ODM1517)    M15-17                                   
         DC    AL1(RIM1820,ODM1820)    M18-20                                   
         DC    AL1(RIM2124,ODM2124)    M21-24                                   
         DC    AL1(RIM2529,ODM2529)    M25-29                                   
         DC    AL1(RIM3034,ODM3034)    M30-34                                   
         DC    AL1(RIM3539,ODM3539)    M35-39                                   
         DC    AL1(RIM4044,ODM4044)    M40-44                                   
         DC    AL1(RIM4549,ODM4549)    M45-49                                   
         DC    AL1(RIM5054,ODM5054)    M50-54                                   
         DC    AL1(RIM5564,ODM5564)    M55-64                                   
         DC    AL1(RIM65O,ODM65O)      M65+                                     
                                                                                
         DC    AL1(RIF1217,ODW1217)    W12-17                                   
         DC    AL1(RIM1217,ODM1217)    M12-17                                   
         DC    AL1(RIHOMES,ODHOMES)    AREA HOMES                               
         DC    AL1(RIUSA,ODUSA)        USA HOMES                                
         DC    AL1(RIL1849,ODL1849)    LOH18-49                                 
         DC    AL1(RIL50O,ODL50O)      LOH50+                                   
         DC    AL1(RIWW1849,ODWW1849)  WW1849                                   
         DC    AL1(RIWW50O,ODWW50O)    WW50+                                    
         DC    AL1(RIPW1849,ODPW1849)  PTWW1849                                 
         DC    AL1(RIPW50O,ODPW50O)    PTWW50O                                  
                                                                                
         DC    AL1(RIF611,ODW611)      W6-11                                    
         DC    AL1(RIM611,ODM611)      M6-11                                    
         DC    AL1(RIWW1820,ODWW1820)  WW18-20                                  
         DC    AL1(RIWW2124,ODWW2124)  WW21-24                                  
         DC    AL1(RIWW2534,ODWW2534)  WW25-34                                  
         DC    AL1(RIWW3544,ODWW3544)  WW35-44                                  
         DC    AL1(RIWW4549,ODWW4549)  WW45-49                                  
         DC    AL1(RIWW5054,ODWW5054)  WW50-54                                  
         DC    AL1(RIWW55O,ODWW55O)    WW55+                                    
         DC    AL1(RIL1824,ODL1824)    LOH18-24                                 
         DC    AL1(RIL2534,ODL2534)    LOH25-34                                 
         DC    AL1(RIL3544,ODL3544)    LOH35-44                                 
         DC    AL1(RIL4549,ODL4549)    LOH45-49                                 
         DC    AL1(RIL5054,ODL5054)    LOH45-49                                 
         DC    AL1(RIL55O,ODL55O)      LOH55+                                   
         DC    X'FF',X'FFFF'                                                    
                                                                                
*--------------------------------------------------------------------*          
* OUTUNV - OUTPUT BUCKETS FOR UNIVERSES                                         
*--------------------------------------------------------------------*          
UNVDSPQ  EQU   100*4               DISP TO RAW ('K') UNIVS                      
PUTDSPQ  EQU   155*4               DISP TO RAW ('Z') PUTS                       
                                                                                
* --THE FOLLOWING DISPS START FROM:  INTACCS + UNVDSPQ                          
OUW25    EQU   0                   UW2-5                                        
OUW68    EQU   1                   UW6-8                                        
OUW911   EQU   2                   UW9-11                                       
OUW1214  EQU   3                   UW12-14                                      
OUW1517  EQU   4                   UW15-17                                      
OUW1820  EQU   5                   UW18-20                                      
OUW2124  EQU   6                   UW21-24                                      
OUW2529  EQU   7                   UW25-29                                      
OUW3034  EQU   8                   UW30-34                                      
OUW3539  EQU   9                   UW35-39                                      
OUW4044  EQU   10                  UW40-44                                      
OUW4549  EQU   11                  UW45-49                                      
OUW5054  EQU   12                  UW50-54                                      
OUW5564  EQU   13                  UW55-64                                      
OUW65O   EQU   14                  UW65+                                        
                                                                                
OUM25    EQU   15                  UM2-5                                        
OUM68    EQU   16                  UM6-8                                        
OUM911   EQU   17                  UM9-11                                       
OUM1214  EQU   18                  UM12-14                                      
OUM1517  EQU   19                  UM15-17                                      
OUM1820  EQU   20                  UM18-20                                      
OUM2124  EQU   21                  UM21-24                                      
OUM2529  EQU   22                  UM25-29                                      
OUM3034  EQU   23                  UM30-34                                      
OUM3539  EQU   24                  UM35-39                                      
OUM4044  EQU   25                  UM40-44                                      
OUM4549  EQU   26                  UM45-49                                      
OUM5054  EQU   27                  UM50-54                                      
OUM5564  EQU   28                  UM55-64                                      
OUM65O   EQU   29                  UM65+                                        
                                                                                
OUW1217  EQU   30                  UW1217                                       
OUM1217  EQU   31                  UM1217                                       
OUHOMES  EQU   32                  HOMES                                        
OUL1849  EQU   33                  ULOH1849   (LADY OF HOUSE)                   
OUL50O   EQU   34                  ULOH50+                                      
OUWW1849 EQU   35                  UWW1849                                      
OUWW50O  EQU   36                  UWW50+                                       
OUPW1849 EQU   37                  UPTWW1849  (PART TIME WORKING WOMEN)         
OUPW50O  EQU   38                  UPTWW50+                                     
OUWMOMS  EQU   39                  UWMOMS                                       
OUW611   EQU   40                  UW6-11                                       
OUM611   EQU   41                  UM6-11                                       
OUWW1820 EQU   42                  OUWW1820                                     
OUWW2124 EQU   43                  OUWW2124                                     
OUWW2534 EQU   44                  OUWW2534                                     
OUWW3544 EQU   45                  OUWW3544                                     
OUWW4549 EQU   46                  OUWW4549                                     
OUWW5054 EQU   47                  OUWW5054                                     
OUWW55O  EQU   48                  OUWW55O                                      
OUL1824  EQU   49                  OUL1824                                      
OUL2534  EQU   50                  OUL2534                                      
OUL3544  EQU   51                  OUL3544                                      
OUL4549  EQU   52                  OUL3549                                      
OUL5054  EQU   53                  OUL5054                                      
OUL55O   EQU   54                  OUL55+                                       
                                                                                
*---------------------------------------------------------------------*         
* OUUNVS - OUTPUT UNIVERSES. TRANSLATE INTERIM SLOTS TO OUTPUT SLOTS            
*         DEMOS STORED IN OUTPUT RECD STARTING AT:   INTACCS + 400              
*         NOTE: DISPLACEMENTS ARE OFF OF INTACCS + UNVDSPQ (FOR OUTPUT)         
*               AND DEMAREA + SLOTLN (FOR INPUT).                               
*---------------------------------------------------------------------*         
OUTUNVS  DS    0XL2                                                             
         DC    AL1(RIF25,OUW25)        W2-5                                     
         DC    AL1(RIF68,OUW68)        W6-8                                     
         DC    AL1(RIF911,OUW911)      W9-11                                    
         DC    AL1(RIF1214,OUW1214)    W12-14                                   
         DC    AL1(RIF1517,OUW1517)    W15-17                                   
         DC    AL1(RIF1820,OUW1820)    W18-20                                   
         DC    AL1(RIF2124,OUW2124)    W21-24                                   
         DC    AL1(RIF2529,OUW2529)    W25-29                                   
         DC    AL1(RIF3034,OUW3034)    W30-34                                   
         DC    AL1(RIF3539,OUW3539)    W35-39                                   
         DC    AL1(RIF4044,OUW4044)    W40-44                                   
         DC    AL1(RIF4549,OUW4549)    W45-49                                   
         DC    AL1(RIF5054,OUW5054)    W50-54                                   
         DC    AL1(RIF5564,OUW5564)    W55-64                                   
         DC    AL1(RIF65O,OUW65O)      W65+                                     
                                                                                
         DC    AL1(RIM25,OUM25)        M2-5                                     
         DC    AL1(RIM68,OUM68)        M6-8                                     
         DC    AL1(RIM911,OUM911)      M9-11                                    
         DC    AL1(RIM1214,OUM1214)    M12-14                                   
         DC    AL1(RIM1517,OUM1517)    M15-17                                   
         DC    AL1(RIM1820,OUM1820)    M18-20                                   
         DC    AL1(RIM2124,OUM2124)    M21-24                                   
         DC    AL1(RIM2529,OUM2529)    M25-29                                   
         DC    AL1(RIM3034,OUM3034)    M30-34                                   
         DC    AL1(RIM3539,OUM3539)    M35-39                                   
         DC    AL1(RIM4044,OUM4044)    M40-44                                   
         DC    AL1(RIM4549,OUM4549)    M45-49                                   
         DC    AL1(RIM5054,OUM5054)    M50-54                                   
         DC    AL1(RIM5564,OUM5564)    M55-64                                   
         DC    AL1(RIM65O,OUM65O)      M65+                                     
                                                                                
         DC    AL1(RIF1217,OUW1217)    W12-17                                   
         DC    AL1(RIM1217,OUM1217)    M12-17                                   
         DC    AL1(RIHOMES,OUHOMES)    AREA HOMES                               
         DC    AL1(RIL1849,OUL1849)    LOH18-49                                 
         DC    AL1(RIL50O,OUL50O)      LOH50+                                   
         DC    AL1(RIWW1849,OUWW1849)  WW1849                                   
         DC    AL1(RIWW50O,OUWW50O)    WW50+                                    
         DC    AL1(RIPW1849,OUPW1849)  PTWW1849                                 
         DC    AL1(RIPW50O,OUPW50O)    PTWW50O                                  
                                                                                
         DC    AL1(RIF611,OUW611)      W6-11                                    
         DC    AL1(RIM611,OUM611)      M6-11                                    
         DC    AL1(RIWW1820,OUWW1820)  WW18-20                                  
         DC    AL1(RIWW2124,OUWW2124)  WW21-24                                  
         DC    AL1(RIWW2534,OUWW2534)  WW25-34                                  
         DC    AL1(RIWW3544,OUWW3544)  WW35-44                                  
         DC    AL1(RIWW4549,OUWW4549)  WW45-49                                  
         DC    AL1(RIWW5054,OUWW5054)  WW50-54                                  
         DC    AL1(RIWW55O,OUWW55O)    WW55+                                    
         DC    AL1(RIL1824,OUL1824)    LOH18-24                                 
         DC    AL1(RIL2534,OUL2534)    LOH25-34                                 
         DC    AL1(RIL3544,OUL3544)    LOH35-44                                 
         DC    AL1(RIL4549,OUL4549)    LOH45-49                                 
         DC    AL1(RIL5054,OUL5054)    LOH45-49                                 
         DC    AL1(RIL55O,OUL55O)      LOH55+                                   
         DC    X'FF',X'FFFF'                                                    
                                                                                
*--------------------------------------------------------------------*          
* SLOTTAB - TRANSLATE INPUT DEMOS TO INTERIM SLOTS                              
*           STORED IN THE INTERIM RECORD BEGINING AT: INTACCS + 0               
*--------------------------------------------------------------------*          
SLOTTAB  DS    0H                                                               
         DC    AL2(RTF25,RIF25)                                                 
         DC    AL2(RTM25,RIM25)                                                 
         DC    AL2(RTF68,RIF68)                                                 
         DC    AL2(RTM68,RIM68)                                                 
         DC    AL2(RTF911,RIF911)                                               
         DC    AL2(RTM911,RIM911)                                               
         DC    AL2(RTF1214,RIF1214)                                             
         DC    AL2(RTM1214,RIM1214)                                             
         DC    AL2(RTF1517,RIF1517)                                             
         DC    AL2(RTM1517,RIM1517)                                             
         DC    AL2(RTF1820,RIF1820)                                             
         DC    AL2(RTM1820,RIM1820)                                             
         DC    AL2(RTF2124,RIF2124)                                             
         DC    AL2(RTM2124,RIM2124)                                             
         DC    AL2(RTF2529,RIF2529)                                             
         DC    AL2(RTM2529,RIM2529)                                             
         DC    AL2(RTF3034,RIF3034)                                             
         DC    AL2(RTM3034,RIM3034)                                             
         DC    AL2(RTF3539,RIF3539)                                             
         DC    AL2(RTM3539,RIM3539)                                             
         DC    AL2(RTF4044,RIF4044)                                             
         DC    AL2(RTM4044,RIM4044)                                             
         DC    AL2(RTF4549,RIF4549)                                             
         DC    AL2(RTM4549,RIM4549)                                             
         DC    AL2(RTF5054,RIF5054)                                             
         DC    AL2(RTM5054,RIM5054)                                             
         DC    AL2(RTF5564,RIF5564)                                             
         DC    AL2(RTM5564,RIM5564)                                             
         DC    AL2(RTF65O,RIF65O)                                               
         DC    AL2(RTM65O,RIM65O)                                               
         DC    AL2(RTWW1820,RIWW1820)                                           
         DC    AL2(RTWW2124,RIWW2124)                                           
         DC    AL2(RTWW2534,RIWW2534)                                           
         DC    AL2(RTWW3544,RIWW3544)                                           
         DC    AL2(RTWW4549,RIWW4549)                                           
         DC    AL2(RTWW5054,RIWW5054)                                           
         DC    AL2(RTWW55O,RIWW55O)                                             
         DC    AL2(RTPW1849,RIPW1849)                                           
         DC    AL2(RTPW50O,RIPW50O)                                             
         DC    AL2(RTL1824,RIL1824)                                             
         DC    AL2(RTL2534,RIL2534)                                             
         DC    AL2(RTL3544,RIL3544)                                             
         DC    AL2(RTL4549,RIL4549)                                             
         DC    AL2(RTL5054,RIL5054)                                             
         DC    AL2(RTL55O,RIL55O)                                               
         DC    AL2(RGF25,RIF25)                                                 
         DC    AL2(RGM25,RIM25)                                                 
         DC    AL2(RGF611,RIF611)                                               
         DC    AL2(RGM611,RIM611)                                               
         DC    AL2(RGF1217,RIF1217)                                             
         DC    AL2(RGM1217,RIM1217)                                             
         DC    AL2(RGF1820,RIF1820)                                             
         DC    AL2(RGM1820,RIM1820)                                             
         DC    AL2(RGF2124,RIF2124)                                             
         DC    AL2(RGM2124,RIM2124)                                             
         DC    AL2(RGF2529,RIF2529)                                             
         DC    AL2(RGM2529,RIM2529)                                             
         DC    AL2(RGF3034,RIF3034)                                             
         DC    AL2(RGM3034,RIM3034)                                             
         DC    AL2(RGF3539,RIF3539)                                             
         DC    AL2(RGM3539,RIM3539)                                             
         DC    AL2(RGF4044,RIF4044)                                             
         DC    AL2(RGM4044,RIM4044)                                             
         DC    AL2(RGF4549,RIF4549)                                             
         DC    AL2(RGM4549,RIM4549)                                             
         DC    AL2(RGF5054,RIF5054)                                             
         DC    AL2(RGM5054,RIM5054)                                             
         DC    AL2(RGF5564,RIF5564)                                             
         DC    AL2(RGM5564,RIM5564)                                             
         DC    AL2(RGF65O,RIF65O)                                               
         DC    AL2(RGM65O,RIM65O)                                               
         DC    AL2(RGWW1849,RIWW1849)                                           
         DC    AL2(RGWW50O,RIWW50O)                                             
         DC    AL2(RGL1849,RIL1849)                                             
         DC    AL2(RGL50O,RIL50O)                                               
         DC    X'FFFF',X'FFFF'                                                  
                                                                                
*---------------------------------------------------------------------*         
* TABLE FOR CALCULATING 8 SPECIFIC DEMOS FOR SPANISH DOMINANT DATA              
*---------------------------------------------------------------------*         
SLOTSD   DS    0H                                                               
         DC    AL1(2),AL2(RIF68,RIF911,RIF611)                                  
         DC    AL1(2),AL2(RIF1214,RIF1517,RIF1217)                              
         DC    AL1(2),AL2(RIM68,RIM911,RIM611)                                  
         DC    AL1(2),AL2(RIM1214,RIM1517,RIM1217)                              
                                                                                
         DC    AL1(5),AL2(RIWW1820,RIWW2124,RIWW2534,RIWW3544,RIWW4549)         
         DC    AL2(RIWW1849)                                                    
                                                                                
         DC    AL1(2),AL2(RIWW5054,RIWW55O,RIWW50O)                             
         DC    AL1(4),AL2(RIL1824,RIL2534,RIL3544,RIL4549,RIL1849)              
         DC    AL1(2),AL2(RIL5054,RIL55O,RIL50O)                                
         DC    X'FF',X'FFFF'                                                    
                                                                                
*---------------------------------------------------------------------*         
* TABLE OF RATING SERVICE MARKET BREAK CODES                                    
*---------------------------------------------------------------------*         
* INPUT SECTIONS FOR MARKET BREAKS                                              
CRUSA    EQU   0                  TOTAL USA                                     
                                                                                
* INTERIM AREA SLOTS FOR MARKET BREAKS                                          
CIUSA    EQU   0                  TOTAL USA                                     
                                                                                
* OUTPUT SECTIONS FOR MARKET BREAKS                                             
COUSA    EQU   1                  TOTAL USA                                     
                                                                                
                                                                                
* TABLE TO CONVERT MARKET BREAKS TO INTERNAL SLOTS                              
CRCITAB  DS    0H                                                               
         DC    AL2(CRUSA,CIUSA)    TOTAL USA                                    
         DC    X'FFFF',X'FFFF'                                                  
                                                                                
* TABLE TO CONVERT MARKET BREAKS TO OUTPUT SECTIONS                             
CRCOTAB  DS    0H                                                               
         DC    AL2(CIUSA,COUSA),X'00' TOTAL USA                                 
         DC    X'FFFF',X'FFFF',X'FF'                                            
                                                                                
*---------------------------------------------------------------------*         
* NETTAB - TABLE OF NETWORK AFFILIATES CALL LETTERS AND MKT NUMBERS             
*---------------------------------------------------------------------*         
NETTAB   DS    0CL7                          DEFINITIONS AS OF 7/99             
         DC    AL2(03),CL3'PBS',AL2(503)     PBS AFFILIATES                     
         DC    AL2(04),CL3'PPY',AL2(503)     PREMIUM PAY                        
         DC    AL2(15),CL3'AGT',AL2(503)     TELEMUNDO AFFILIATES               
         DC    AL2(16),CL3'AGU',AL2(503)     UNIVISION AFFILIATES               
         DC    AL2(17),CL3'AGH',AL2(503)     SPANISH LANGUAGE TV                
         DC    AL2(19),CL3'AGA',AL2(503)     AZTECA AMERICA AFFILIATES          
         DC    AL2(24),CL3'ONA',AL2(503)     OTHER ENG LANG NET AFF             
         DC    AL2(26),CL3'TNA',AL2(503)     TOTAL ENG LANG NET AFF             
         DC    AL2(30),CL3'EIN',AL2(503)     ENG LANG INDEP BROADCAST           
         DC    AL2(31),CL3'ACX',AL2(503)     AD SUP CBL EXCL SP LNG CBL         
         DC    AL2(32),CL3'OCX',AL2(503)     OTHER CBL EXCL SP LNG CBL          
         DC    AL2(33),CL3'SCN',AL2(503)     SP LANG CABLE NETS                 
         DC    AL2(34),CL3'SIN',AL2(503)     SP LANG INDEPENDENTS               
         DC    AL2(35),CL3'TFA',AL2(503)     TELEFUTURA AFFILIATES              
         DC    AL2(36),CL3'AOT',AL2(503)     ALL OTHER TUNING (JUL05)           
NETTABQ  EQU   (*-NETTAB)/L'NETTAB                                              
                                                                                
*---------------------------------------------------------------------*         
* VARIOUS EQUATES, USED IN TABLES BELLOW                                        
*---------------------------------------------------------------------*         
* TIME DAYPARTS FOR HUT RECORDS                                                 
DM711P   EQU   1000                MON                                          
DM811P   EQU   1005                                                             
DT711P   EQU   1010                TUE                                          
DT811P   EQU   1015                                                             
DW711P   EQU   1020                WED                                          
DW811P   EQU   1025                                                             
DR711P   EQU   1030                THU                                          
DR811P   EQU   1035                                                             
DF711P   EQU   1040                FRI                                          
DF811P   EQU   1045                                                             
DSA8A1P  EQU   1050                SAT                                          
DSA10A1P EQU   1055                                                             
DSA14P   EQU   1060                                                             
DSA17P   EQU   1065                                                             
DSA46P   EQU   1070                                                             
DSA47P   EQU   1075                                                             
DSA711P  EQU   1080                                                             
DSA811P  EQU   1085                                                             
DSU10A1P EQU   1090                SUN                                          
DSU14P   EQU   1095                                                             
DSU17P   EQU   1100                                                             
DSU46P   EQU   1105                                                             
DSU47P   EQU   1110                                                             
DSU711P  EQU   1115                                                             
DSU811P  EQU   1120                                                             
DMF67A   EQU   1125                MON-FRI                                      
DMF79A   EQU   1130                                                             
DMF710A  EQU   1135                                                             
DMF912N  EQU   1140                                                             
DMF9A4P  EQU   1145                                                             
DMF124P  EQU   1150                                                             
DMF14P   EQU   1155                                                             
DMF35P   EQU   1160                                                             
DMF46P   EQU   1165                                                             
DMF637P  EQU   1170                                                             
DMF78P   EQU   1175                                                             
                                                                                
DMF711P  EQU   1180                                                             
DMF89P   EQU   1185                                                             
DMF911P  EQU   1190                                                             
DMF11HP  EQU   1195                                                             
DMF11P1A EQU   1200                                                             
DMF113P1 EQU   1205                                                             
DMS6A2A  EQU   1210                MON-SUN  6AM-2AM                             
DMS71A   EQU   1215                                                             
DMS9A12M EQU   1220                                                             
DMS68P   EQU   1225                                                             
DMS711P  EQU   1230                                                             
DMS24H   EQU   1235                MON-SUN  24HOUR TOTAL                        
DMSPRIME EQU   1260                (MON-SUN) M-SAT 8P-11P & SUN 7-11P           
DMS11HP  EQU   1240                                                             
DMS1112M EQU   1245                                                             
DMS1131A EQU   1250                                                             
DMS12M1A EQU   1255                                                             
DSS47P   EQU   1265                SAT-SUN                                      
DSS711P  EQU   1270                                                             
                                                                                
* EQUATE PROGRAM TYPES/ DAYPARTS                                                
EVGD     EQU   3700                GENERAL DRAMA                                
EVVA     EQU   3705                VARIETY                                      
EVIN7P11 EQU   3710                INFORMATIONAL 7-11P                          
EVFF7P11 EQU   3715                FEATURE FILM (REG) 7-11P                     
EVFFRS   EQU   3720                FEATURE FILMS (RG+SP) 7-11P                  
ALL7P9   EQU   3725                ALL 7:00PM-9:00PM                            
ALL9P11  EQU   3730                ALL 9:00PM-11:00PM                           
EVTLR    EQU   3735                EVENING TELEMUNDO (REG)                      
EVUNR    EQU   3740                EVENING UNIVISION (REG)                      
EVALLR   EQU   3745                EVENING-ALL(REG)                             
EVTLS    EQU   3750                EVENING TELEMUNDO (SPC)                      
EVUNS    EQU   3755                EVENING UNIVISION (SPC)                      
EVALLS   EQU   3760                EVENING-ALL(SPC)                             
EVTLRS   EQU   3765                EVENING TELEMUNDO (REG+SPC)                  
EVUNRS   EQU   3770                EVENING UNIVISION (REG+SPC)                  
EVALLRS  EQU   3775                EVENING-ALL(REG+SPC)                         
                                                                                
WKDIN    EQU   3500                WKND INFORMATIONAL                           
MFINF    EQU   3505                M-F INFORMATIONAL                            
                                                                                
WKD11P1  EQU   3600                WEEKEND 11P-1AM                              
MF11P1   EQU   3605                M-F 11P-1AM                                  
                                                                                
WKFF6A43 EQU   3000                WKDAY FEATURE FILMS 6-4:30P                  
WKDD6A43 EQU   3005                WKDAY DAYTIME DRAMA 6-4:30P                  
WKIN6A43 EQU   3010                WKDAY INFORMATIONAL 6-4:30P                  
WKAD6A10 EQU   3015                WKDAY ADULT 6A-10A                           
WKCH6A10 EQU   3020                WKDAY CHILDREN 6-10A                         
WK10A1   EQU   3025                WKDAY ALL 10A-1:00P                          
WK1P43   EQU   3030                WKDAY ALL 1P-4:30P                           
WK10A43T EQU   3035                WKDAY ALL 10A-4:30P (TEL)                    
WK10A43U EQU   3040                WKDAY ALL 10A-4:30P (UNI)                    
WK10A43A EQU   3045                WKDAY ALL 10A-4:30P (ALL)                    
                                                                                
WDCHTEL  EQU   2000                WKND CHILDRENS TELE                          
WDCHUNI  EQU   2005                WKND CHILDRENS UNIV                          
WDCHALL  EQU   2010                WKND CHILDRENS ALL                           
WDSPTR   EQU   2015                WKND SPORTS TEL (REG)                        
WDSPUR   EQU   2020                WKND SPORTS UNI (REG)                        
WDSPR    EQU   2025                WKND SPORTS ALL (REG)                        
WDSPTRS  EQU   2030                WKND SPORTS TEL (REG+SPC)                    
WDSPURS  EQU   2035                WKND SPORTS UNI (REG+SPC)                    
WDSPRS   EQU   2040                WKND SPORTS ALL (REG+SPC)                    
WDIN6A6  EQU   2045                WKND INFORMATIONAL 6A-6P                     
WDFF6A6  EQU   2050                WKND FEATURE FILMS 6A-6P                     
WDPM6A6  EQU   2055                WKND POPULAR MUSIC 6A-6P                     
                                                                                
SP24R    EQU   4000                SPORTS ALL 24H (REG)                         
SP24RS   EQU   4005                SPORTS ALL 24H (REG+SPC)                     
IN24     EQU   4010                INFORMATIONAL - 24H                          
NEWS24   EQU   4015                NEWS - 24H                                   
FF24     EQU   4020                FEATURE FILMS - 24H                          
VA24     EQU   4025                VARIETY - 24H                                
GD24     EQU   4030                GENERAL/DAYTIME DRAMA-24H                    
CA24     EQU   4035                CHILDREN'S ANIMATION-24H                     
SPC24T   EQU   4040                SPECIALS (TEL) -24H                          
SPC24U   EQU   4045                SPECIALS (UNI) -24H                          
SPC24A   EQU   4050                SPECIALS (ALL) -24H                          
                                                                                
RG       EQU   X'01'               INCLUDE REGULAR PROGRAMS                     
SP       EQU   X'04'               INCLUDE SPECIAL PROGRAMS                     
                                                                                
*--------------------------------------------------------------------*          
* PTYPTAB -    PGM TYPE,   ,DAYPART CODE, REG/SPC,EQU CODE,NET CODE             
*--------------------------------------------------------------------*          
PTYPTAB  DS    0H                                                               
*EVENING 7:00PM-11:00PM                                                         
         DC    C'GD',AL2(000),C'12  ',AL1(RG),AL2(EVGD),C'     '                
         DC    C'CV',AL2(000),C'12  ',AL1(RG),AL2(EVVA),C'     '                
         DC    C'GV',AL2(000),C'12  ',AL1(RG),AL2(EVVA),C'     '                
         DC    C'PV',AL2(000),C'12  ',AL1(RG),AL2(EVVA),C'     '                
         DC    C'CC',AL2(000),C'12  ',AL1(RG),AL2(EVIN7P11),C'     '            
         DC    C'DO',AL2(000),C'12  ',AL1(RG),AL2(EVIN7P11),C'     '            
         DC    C'DN',AL2(000),C'12  ',AL1(RG),AL2(EVIN7P11),C'     '            
         DC    C'IA',AL2(000),C'12  ',AL1(RG),AL2(EVIN7P11),C'     '            
         DC    C'N ',AL2(000),C'12  ',AL1(RG),AL2(EVIN7P11),C'     '            
                                                                                
         DC    C'FF',AL2(000),C'12  ',AL1(RG),AL2(EVFF7P11),C'     '            
         DC    C'FF',AL2(000),C'12  ',AL1(SP+RG),AL2(EVFFRS),C'     '           
         DC    C'  ',AL2(000),C'1   ',AL1(RG),AL2(ALL7P9),C'     '              
         DC    C'  ',AL2(000),C'2   ',AL1(RG),AL2(ALL9P11),C'     '             
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EVTLR),C'TEL  '               
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EVUNR),C'UNI  '               
         DC    C'  ',AL2(000),C'12  ',AL1(RG),AL2(EVALLR),C'     '              
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EVTLS),C'TEL  '               
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EVUNS),C'UNI  '               
         DC    C'  ',AL2(000),C'12  ',AL1(SP),AL2(EVALLS),C'     '              
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EVTLRS),C'TEL  '           
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EVUNRS),C'UNI  '           
         DC    C'  ',AL2(000),C'12  ',AL1(SP+RG),AL2(EVALLRS),C'     '          
                                                                                
*EVENING 6:00PM-7:00PM                                                          
* -INFORMATIONAL- MON-FRI                                                       
         DC    C'CC',AL2(000),C'0   ',AL1(RG),AL2(MFINF),C'     '               
         DC    C'DO',AL2(000),C'0   ',AL1(RG),AL2(MFINF),C'     '               
         DC    C'DN',AL2(000),C'0   ',AL1(RG),AL2(MFINF),C'     '               
         DC    C'IA',AL2(000),C'0   ',AL1(RG),AL2(MFINF),C'     '               
         DC    C'N ',AL2(000),C'0   ',AL1(RG),AL2(MFINF),C'     '               
                                                                                
* -INFORMATIONAL- WEEKEND                                                       
         DC    C'CC',AL2(000),C'0   ',AL1(RG),AL2(WKDIN),C'     '               
         DC    C'DO',AL2(000),C'0   ',AL1(RG),AL2(WKDIN),C'     '               
         DC    C'DN',AL2(000),C'0   ',AL1(RG),AL2(WKDIN),C'     '               
         DC    C'IA',AL2(000),C'0   ',AL1(RG),AL2(WKDIN),C'     '               
         DC    C'N ',AL2(000),C'0   ',AL1(RG),AL2(WKDIN),C'     '               
                                                                                
*EVENING 11:00PM-1:00AM                                                         
*-TIMES  M-F AND WKNKD                                                          
         DC    C'  ',AL2(000),C'AB  ',AL1(RG),AL2(MF11P1),C'     '              
         DC    C'  ',AL2(000),C'AB  ',AL1(RG),AL2(WKD11P1),C'     '             
                                                                                
*WEEKDAY DAYTIME 6:00AM-4:30PM                                                  
         DC    C'FF',AL2(000),C'DE56',AL1(RG),AL2(WKFF6A43),C'     '            
         DC    C'DD',AL2(000),C'DE56',AL1(RG),AL2(WKDD6A43),C'     '            
                                                                                
*-INFORMATIONAL    (CREATE --MFINF-- FOR MON-FRI DATA)                          
         DC    C'CC',AL2(000),C'DE56',AL1(RG),AL2(WKIN6A43),C'     '            
         DC    C'DO',AL2(000),C'DE56',AL1(RG),AL2(WKIN6A43),C'     '            
         DC    C'DN',AL2(000),C'DE56',AL1(RG),AL2(WKIN6A43),C'     '            
         DC    C'IA',AL2(000),C'DE56',AL1(RG),AL2(WKIN6A43),C'     '            
         DC    C'N ',AL2(000),C'DE56',AL1(RG),AL2(WKIN6A43),C'     '            
*-ADULT 6:00AM-10:00AM   ** HANDLE IN CODE-(TEST NOT=C,CL,CA,CN) ***            
         DC    C'X1',AL2(000),C'DE  ',AL1(RG),AL2(WKAD6A10),C'     '            
*-CHILDRENS 6:00AM-10:00AM                                                      
         DC    C'C ',AL2(000),C'DE  ',AL1(RG),AL2(WKCH6A10),C'     '            
         DC    C'CL',AL2(000),C'DE  ',AL1(RG),AL2(WKCH6A10),C'     '            
         DC    C'CA',AL2(000),C'DE  ',AL1(RG),AL2(WKCH6A10),C'     '            
         DC    C'CN',AL2(000),C'DE  ',AL1(RG),AL2(WKCH6A10),C'     '            
                                                                                
*-TIMES                                                                         
         DC    C'  ',AL2(000),C'5   ',AL1(RG),AL2(WK10A1),C'     '              
         DC    C'  ',AL2(000),C'6   ',AL1(RG),AL2(WK1P43),C'     '              
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(WK10A43T),C'TEL  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(WK10A43U),C'UNI  '            
         DC    C'  ',AL2(000),C'56  ',AL1(RG),AL2(WK10A43A),C'     '            
                                                                                
*WEEKEND DAYTIME 6:00AM-6:00P                                                   
*CHILDRENS TELEMUNDO                                                            
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WDCHTEL),C'TEL  '             
         DC    C' L',AL2(000),C'8   ',AL1(RG),AL2(WDCHTEL),C'TEL  '             
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WDCHTEL),C'TEL  '             
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WDCHTEL),C'TEL  '             
*CHILDRENS UNIVISION                                                            
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WDCHUNI),C'UNI  '             
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WDCHUNI),C'UNI  '             
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WDCHUNI),C'UNI  '             
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WDCHUNI),C'UNI  '             
*CHILDRENS ALL                                                                  
         DC    C'C ',AL2(000),C'8   ',AL1(RG),AL2(WDCHALL),C'     '             
         DC    C'CL',AL2(000),C'8   ',AL1(RG),AL2(WDCHALL),C'     '             
         DC    C'CA',AL2(000),C'8   ',AL1(RG),AL2(WDCHALL),C'     '             
         DC    C'CN',AL2(000),C'8   ',AL1(RG),AL2(WDCHALL),C'     '             
*SPORTS TELEMUNDO (REG)                                                         
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WDSPTR),C'TEL  '              
         DC    C'SC',AL2(000),C'8   ',AL1(RG),AL2(WDSPTR),C'TEL  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WDSPTR),C'TEL  '              
         DC    C'SN',AL2(000),C'8   ',AL1(RG),AL2(WDSPTR),C'TEL  '              
*SPORTS UNIVISION (REG)                                                         
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WDSPUR),C'UNI  '              
         DC    C'SC',AL2(000),C'8   ',AL1(RG),AL2(WDSPUR),C'UNI  '              
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WDSPUR),C'UNI  '              
         DC    C'SN',AL2(000),C'8   ',AL1(RG),AL2(WDSPUR),C'UNI  '              
*SPORTS ALL (REG)                                                               
         DC    C'SA',AL2(000),C'8   ',AL1(RG),AL2(WDSPR),C'     '               
         DC    C'SC',AL2(000),C'8   ',AL1(RG),AL2(WDSPR),C'     '               
         DC    C'SE',AL2(000),C'8   ',AL1(RG),AL2(WDSPR),C'     '               
         DC    C'SN',AL2(000),C'8   ',AL1(RG),AL2(WDSPR),C'     '               
*SPORTS TELEMUNDO (REG+SPC)                                                     
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPTRS),C'TEL  '          
         DC    C'SC',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPTRS),C'TEL  '          
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPTRS),C'TEL  '          
         DC    C'SN',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPTRS),C'TEL  '          
*SPORTS UNIVISION (REG+SPC)                                                     
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPURS),C'UNI  '          
         DC    C'SC',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPURS),C'UNI  '          
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPURS),C'UNI  '          
         DC    C'SN',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPURS),C'UNI  '          
*SPORTS ALL (REG+SPC)                                                           
         DC    C'SA',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPRS),C'     '           
         DC    C'SC',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPRS),C'     '           
         DC    C'SE',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPRS),C'     '           
         DC    C'SN',AL2(000),C'8   ',AL1(RG+SP),AL2(WDSPRS),C'     '           
*INFORMATIONAL                                                                  
         DC    C'CC',AL2(000),C'8   ',AL1(RG),AL2(WDIN6A6),C'     '             
         DC    C'DO',AL2(000),C'8   ',AL1(RG),AL2(WDIN6A6),C'     '             
         DC    C'DN',AL2(000),C'8   ',AL1(RG),AL2(WDIN6A6),C'     '             
         DC    C'IA',AL2(000),C'8   ',AL1(RG),AL2(WDIN6A6),C'     '             
         DC    C'N ',AL2(000),C'8   ',AL1(RG),AL2(WDIN6A6),C'     '             
*FEATURE FILMS                                                                  
         DC    C'FF',AL2(000),C'8   ',AL1(RG),AL2(WDFF6A6),C'     '             
*POPULAR MUSIC                                                                  
         DC    C'PC',AL2(000),C'8   ',AL1(RG),AL2(WDPM6A6),C'     '             
                                                                                
*MONDAY-SUNDAY 24 HOUR                                                          
*SPORTS ALL (REG)                                                               
         DC    C'SA',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SC',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SE',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
         DC    C'SN',AL2(000),C'Z   ',AL1(RG),AL2(SP24R),C'     '               
*SPORTS ALL (REG+SPCL)                                                          
         DC    C'SA',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24RS),C'     '           
         DC    C'SC',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24RS),C'     '           
         DC    C'SE',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24RS),C'     '           
         DC    C'SN',AL2(000),C'Z   ',AL1(RG+SP),AL2(SP24RS),C'     '           
*INFORMATIONAL                                                                  
         DC    C'CC',AL2(000),C'Z   ',AL1(RG),AL2(IN24),C'     '                
         DC    C'DO',AL2(000),C'Z   ',AL1(RG),AL2(IN24),C'     '                
         DC    C'DN',AL2(000),C'Z   ',AL1(RG),AL2(IN24),C'     '                
         DC    C'IA',AL2(000),C'Z   ',AL1(RG),AL2(IN24),C'     '                
         DC    C'N ',AL2(000),C'Z   ',AL1(RG),AL2(IN24),C'     '                
*NEWS                                                                           
         DC    C'N ',AL2(000),C'Z   ',AL1(RG),AL2(NEWS24),C'     '              
*FEATURE FILM                                                                   
         DC    C'FF',AL2(000),C'Z   ',AL1(RG),AL2(FF24),C'     '                
*VARIETY                                                                        
         DC    C'CV',AL2(000),C'Z   ',AL1(RG),AL2(VA24),C'     '                
         DC    C'GV',AL2(000),C'Z   ',AL1(RG),AL2(VA24),C'     '                
         DC    C'PV',AL2(000),C'Z   ',AL1(RG),AL2(VA24),C'     '                
*GENERAL/DAYTIME DRAMA                                                          
         DC    C'GD',AL2(000),C'Z   ',AL1(RG),AL2(GD24),C'     '                
         DC    C'DD',AL2(000),C'Z   ',AL1(RG),AL2(GD24),C'     '                
*CHILDREN'S ANIMATION                                                           
         DC    C'CA',AL2(000),C'Z   ',AL1(RG),AL2(CA24),C'     '                
         DC    C'EA',AL2(000),C'Z   ',AL1(RG),AL2(CA24),C'     '                
*SPECIALS                                                                       
         DC    C'  ',AL2(000),C'Z   ',AL1(SP),AL2(SPC24T),C'TEL  '              
         DC    C'  ',AL2(000),C'Z   ',AL1(SP),AL2(SPC24U),C'UNI  '              
         DC    C'  ',AL2(000),C'Z   ',AL1(SP),AL2(SPC24A),C'     '              
         DC    X'FFFF'                                                          
* EXCLUDE LISTS                                                                 
XCLIST   DC    C'X1',C'C  CA CN CL  '                                           
         DC    X'FFFF'                                                          
                                                                                
*--------------------------------------------------------------------*          
* PTDPNAME -   PROGRAM CATEGORIES NAMES                                         
*--------------------------------------------------------------------*          
PTDPNAME DC    AL2(EVGD),CL25'GENERAL DRAMA 7-11P'                              
         DC    AL2(EVVA),CL25'VARIETY 7-11P'                                    
         DC    AL2(EVIN7P11),CL25'INFORMATIONAL 7-11P'                          
         DC    AL2(EVFF7P11),CL25'FEATURE FILM (REG) 7-11P'                     
         DC    AL2(EVFFRS),CL25'FEATURE FILMS (RG+SP) 7-11P'                    
         DC    AL2(ALL7P9),CL25'ALL 7:00PM-9:00PM'                              
         DC    AL2(ALL9P11),CL25'ALL 9:00PM-11:00PM'                            
         DC    AL2(EVTLR),CL25'EVENING TELEMUNDO (REG)'                         
         DC    AL2(EVUNR),CL25'EVENING UNIVISION (REG)'                         
         DC    AL2(EVALLR),CL25'EVENING-ALL(REG)'                               
         DC    AL2(EVTLS),CL25'EVENING TELEMUNDO (SPC)'                         
         DC    AL2(EVUNS),CL25'EVENING UNIVISION (SPC)'                         
         DC    AL2(EVALLS),CL25'EVENING-ALL(SPC)'                               
         DC    AL2(EVTLRS),CL25'EVENING TELEMUNDO (REG+SPC)'                    
         DC    AL2(EVUNRS),CL25'EVENING UNIVISION (REG+SPC)'                    
         DC    AL2(EVALLRS),CL25'EVENING-ALL(REG+SPC)'                          
                                                                                
         DC    AL2(WKDIN),CL25'WKND INFORMATIONAL'                              
         DC    AL2(MFINF),CL25'M-F INFORMATIONAL'                               
                                                                                
         DC    AL2(WKD11P1),CL25'WEEKEND 11P-1AM'                               
         DC    AL2(MF11P1),CL25'M-F 11P-1AM'                                    
                                                                                
         DC    AL2(WKFF6A43),CL25'WKDY FEATR FILMS 6-4:30P'                     
         DC    AL2(WKDD6A43),CL25'WKDY DAY DRAMA 6-4:30P'                       
         DC    AL2(WKIN6A43),CL25'WKDY INFORMATIONL 6-4:30P'                    
         DC    AL2(WKAD6A10),CL25'WKDY ADULT 6A-10A'                            
         DC    AL2(WKCH6A10),CL25'WKDY CHILDREN 6-10A'                          
         DC    AL2(WK10A1),CL25'WKDY ALL 10A-1:00P'                             
         DC    AL2(WK1P43),CL25'WKDY ALL 1P-4:30P'                              
         DC    AL2(WK10A43T),CL25'WKDY ALL 10A-4:30P (TEL)'                     
         DC    AL2(WK10A43U),CL25'WKDY ALL 10A-4:30P (UNI)'                     
         DC    AL2(WK10A43A),CL25'WKDY ALL 10A-4:30P (ALL)'                     
                                                                                
         DC    AL2(WDCHTEL),CL25'WKND CHILDREN (TEL)'                           
         DC    AL2(WDCHUNI),CL25'WKND CHILDREN (UNI)'                           
         DC    AL2(WDCHALL),CL25'WKND CHILDREN (ALL)'                           
         DC    AL2(WDSPTR),CL25'WKND SPORTS TEL (REG)'                          
         DC    AL2(WDSPUR),CL25'WKND SPORTS UNI (REG)'                          
         DC    AL2(WDSPR),CL25'WKND SPORTS ALL (REG)'                           
         DC    AL2(WDSPTRS),CL25'WKND SPORTS TEL (REG+SPC)'                     
         DC    AL2(WDSPURS),CL25'WKND SPORTS UNI (REG+SPC)'                     
         DC    AL2(WDSPRS),CL25'WKND SPORTS ALL (REG+SPC)'                      
         DC    AL2(WDIN6A6),CL25'WKND INFORMATIONAL 6A-6P'                      
         DC    AL2(WDFF6A6),CL25'WKND FEATURE FILMS 6A-6P'                      
         DC    AL2(WDPM6A6),CL25'WKND POPULAR MUSIC 6A-6P'                      
                                                                                
         DC    AL2(SP24R),CL25'SPORTS ALL 24H (REG)'                            
         DC    AL2(SP24RS),CL25'SPORTS ALL 24H (REG+SPC)'                       
         DC    AL2(IN24),CL25'INFORMATIONAL - 24H'                              
         DC    AL2(NEWS24),CL25'NEWS - 24H'                                     
         DC    AL2(FF24),CL25'FEATURE FILMS - 24H'                              
         DC    AL2(VA24),CL25'VARIETY - 24H'                                    
         DC    AL2(GD24),CL25'GENERAL/DAYTIME DRAMA-24H'                        
         DC    AL2(CA24),CL25'CHILDREN''S ANIMATION-24H'                        
         DC    AL2(SPC24T),CL25'SPECIALS (TEL) -24H'                            
         DC    AL2(SPC24U),CL25'SPECIALS (UNI) -24H'                            
         DC    AL2(SPC24A),CL25'SPECIALS (ALL) -24H'                            
                                                                                
*--------------------------------------------------------------------*          
* DPNAME - DAYPART NAMES                                                        
*--------------------------------------------------------------------*          
DPNAME   DC    AL2(DM711P),CL25'MON 7.00PM - 11.00PM     '                      
         DC    AL2(DM811P),CL25'MON 8.00PM - 11.00PM     '                      
         DC    AL2(DT711P),CL25'TUE 7.00PM - 11.00PM     '                      
         DC    AL2(DT811P),CL25'TUE 8.00PM - 11.00PM     '                      
         DC    AL2(DW711P),CL25'WED 7.00PM - 11.00PM     '                      
         DC    AL2(DW811P),CL25'WED 8.00PM - 11.00PM     '                      
         DC    AL2(DR711P),CL25'THU 7.00PM - 11.00PM     '                      
         DC    AL2(DR811P),CL25'THU 8.00PM - 11.00PM     '                      
         DC    AL2(DF711P),CL25'FRI 7.00PM - 11.00PM     '                      
         DC    AL2(DF811P),CL25'FRI 8.00PM - 11.00PM     '                      
                                                                                
         DC    AL2(DSA8A1P),CL25'SAT 8.00AM - 1.00PM'                           
         DC    AL2(DSA10A1P),CL25'SAT 10.00AM - 1.00PM'                         
         DC    AL2(DSA14P),CL25'SAT 1.00PM - 4.00PM'                            
         DC    AL2(DSA17P),CL25'SAT 1.00PM - 7.00PM'                            
         DC    AL2(DSA46P),CL25'SAT 4.00PM - 6.00PM'                            
         DC    AL2(DSA47P),CL25'SAT 4.00PM - 7.00PM'                            
         DC    AL2(DSA711P),CL25'SAT 7.00PM - 11.00PM'                          
         DC    AL2(DSA811P),CL25'SAT 8.00PM - 11.00PM'                          
*SUN                                                                            
         DC    AL2(DSU10A1P),CL25'SUN 10.00AM - 1.00PM'                         
         DC    AL2(DSU14P),CL25'SUN 1.00PM - 4.00PM'                            
         DC    AL2(DSU17P),CL25'SUN 1.00PM - 7.00PM'                            
         DC    AL2(DSU46P),CL25'SUN 4.00PM - 6.00PM'                            
         DC    AL2(DSU47P),CL25'SUN 4.00PM - 7.00PM'                            
         DC    AL2(DSU711P),CL25'SUN 7.00PM - 11.00PM'                          
         DC    AL2(DSU811P),CL25'SUN 8.00PM - 11.00PM'                          
*MON-FRI                                                                        
         DC    AL2(DMF67A),CL25'MON-FRI 6.00AM - 7.00AM'                        
         DC    AL2(DMF79A),CL25'MON-FRI 7.00AM - 9.00AM'                        
         DC    AL2(DMF710A),CL25'MON-FRI 7.00AM - 10.00AM'                      
         DC    AL2(DMF912N),CL25'MON-FRI 9.00AM - 12.00NN'                      
         DC    AL2(DMF9A4P),CL25'MON-FRI 9.00AM - 4.00PM'                       
         DC    AL2(DMF124P),CL25'MON-FRI 12.00NN - 4.00PM'                      
         DC    AL2(DMF14P),CL25'MON-FRI 1.00PM - 4.00PM'                        
         DC    AL2(DMF35P),CL25'MON-FRI 3.00PM - 5.00PM'                        
         DC    AL2(DMF46P),CL25'MON-FRI 4.00PM - 6.00PM'                        
         DC    AL2(DMF637P),CL25'MON-FRI 6.30PM - 7.00PM'                       
         DC    AL2(DMF78P),CL25'MON-FRI 7.00PM - 8.00PM'                        
         DC    AL2(DMF711P),CL25'MON-FRI 7.00PM - 11.00PM'                      
         DC    AL2(DMF89P),CL25'MON-FRI 8.00PM - 9.00PM'                        
         DC    AL2(DMF911P),CL25'MON-FRI 9.00PM - 11.00PM'                      
         DC    AL2(DMF11HP),CL25'MON-FRI 11.00PM - 11.30PM'                     
         DC    AL2(DMF11P1A),CL25'MON-FRI 11.00PM - 1.00AM'                     
         DC    AL2(DMF113P1),CL25'MON-FRI 11.30PM - 1.00AM'                     
                                                                                
*MON-SUN ----- 24 HOUR TOTAL ----- (6:00AM-6:00AM)                              
         DC    AL2(DMS6A2A),CL25'MON-SUN 6.00AM-2.00AM'                         
         DC    AL2(DMS24H),CL25'MON-SUN 24-HOUR      '                          
                                                                                
         DC    AL2(DMS71A),CL25'MON-SUN 7.00AM - 1.00AM'                        
         DC    AL2(DMS9A12M),CL25'MON-SUN 9.00AM - 12.00MD'                     
         DC    AL2(DMS68P),CL25'MON-SUN 6.00PM - 8.00PM'                        
         DC    AL2(DMS711P),CL25'MON-SUN 7.00PM - 11.00PM'                      
**8-11** DC    AL2(       ),                                                    
**&7-11* DC    AL2(       ),                                                    
         DC    AL2(DMS11HP),CL25'MON-SUN 11.00PM - 11.30PM'                     
         DC    AL2(DMS1112M),CL25'MON-SUN 11.00PM - 12.00MD'                    
         DC    AL2(DMS1131A),CL25'MON-SUN 11.30PM - 1.00AM'                     
         DC    AL2(DMS12M1A),CL25'MON-SUN 12.00MD- 1.00AM'                      
         DC    AL2(DMSPRIME),CL25'MON-SUN   -PRIME-  '                          
         DC    AL2(DSS47P),CL25'SAT-SUN 4.00PM - 7.00PM'                        
         DC    AL2(DSS711P),CL25'SAT-SUN 7.00PM - 11.00PM'                      
         DC   X'FFFF'                                                           
                                                                                
*-----------------------------------------------------------------*             
* DPTTAB -  DAYPARTS TABLE                                                      
*-----------------------------------------------------------------*             
DPTTAB   DS    0XL10                                                            
*MON                                                                            
         DC    AL2(DM711P),AL1(1,1),AL2(1900,2300),B'01000000',X'10'            
         DC    AL2(DM811P),AL1(1,1),AL2(2000,2300),B'01000000',X'10'            
*TUE                                                                            
         DC    AL2(DT711P),AL1(2,2),AL2(1900,2300),B'00100000',X'20'            
         DC    AL2(DT811P),AL1(2,2),AL2(2000,2300),B'00100000',X'20'            
*WED                                                                            
         DC    AL2(DW711P),AL1(3,3),AL2(1900,2300),B'00010000',X'30'            
         DC    AL2(DW811P),AL1(3,3),AL2(2000,2300),B'00010000',X'30'            
*THU                                                                            
         DC    AL2(DR711P),AL1(4,4),AL2(1900,2300),B'00001000',X'40'            
         DC    AL2(DR811P),AL1(4,4),AL2(2000,2300),B'00001000',X'40'            
*FRI                                                                            
         DC    AL2(DF711P),AL1(5,5),AL2(1900,2300),B'00000100',X'50'            
         DC    AL2(DF811P),AL1(5,5),AL2(2000,2300),B'00000100',X'50'            
*SAT                                                                            
         DC    AL2(DSA8A1P),AL1(6,6),AL2(0800,1300),B'00000010',X'60'           
         DC    AL2(DSA10A1P),AL1(6,6),AL2(1000,1300),B'00000010',X'60'          
         DC    AL2(DSA14P),AL1(6,6),AL2(1300,1600),B'00000010',X'60'            
         DC    AL2(DSA17P),AL1(6,6),AL2(1300,1900),B'00000010',X'60'            
         DC    AL2(DSA46P),AL1(6,6),AL2(1600,1800),B'00000010',X'60'            
         DC    AL2(DSA47P),AL1(6,6),AL2(1600,1900),B'00000010',X'60'            
         DC    AL2(DSA711P),AL1(6,6),AL2(1900,2300),B'00000010',X'60'           
         DC    AL2(DSA811P),AL1(6,6),AL2(2000,2300),B'00000010',X'60'           
*SUN                                                                            
         DC    AL2(DSU10A1P),AL1(7,7),AL2(1000,1300),B'00000001',X'70'          
         DC    AL2(DSU14P),AL1(7,7),AL2(1300,1600),B'00000001',X'70'            
         DC    AL2(DSU17P),AL1(7,7),AL2(1300,1900),B'00000001',X'70'            
         DC    AL2(DSU46P),AL1(7,7),AL2(1600,1800),B'00000001',X'70'            
         DC    AL2(DSU47P),AL1(7,7),AL2(1600,1900),B'00000001',X'70'            
         DC    AL2(DSU711P),AL1(7,7),AL2(1900,2300),B'00000001',X'70'           
         DC    AL2(DSU811P),AL1(7,7),AL2(2000,2300),B'00000001',X'70'           
*MON-FRI                                                                        
         DC    AL2(DMF67A),AL1(1,5),AL2(0600,0700),B'01111100',X'00'            
         DC    AL2(DMF79A),AL1(1,5),AL2(0700,0900),B'01111100',X'00'            
         DC    AL2(DMF710A),AL1(1,5),AL2(0700,1000),B'01111100',X'00'           
         DC    AL2(DMF912N),AL1(1,5),AL2(0900,1200),B'01111100',X'00'           
         DC    AL2(DMF9A4P),AL1(1,5),AL2(0900,1600),B'01111100',X'00'           
         DC    AL2(DMF124P),AL1(1,5),AL2(1200,1600),B'01111100',X'00'           
         DC    AL2(DMF14P),AL1(1,5),AL2(1300,1600),B'01111100',X'00'            
         DC    AL2(DMF35P),AL1(1,5),AL2(1500,1700),B'01111100',X'00'            
         DC    AL2(DMF46P),AL1(1,5),AL2(1600,1800),B'01111100',X'00'            
         DC    AL2(DMF637P),AL1(1,5),AL2(1830,1900),B'01111100',X'00'           
         DC    AL2(DMF78P),AL1(1,5),AL2(1900,2000),B'01111100',X'00'            
         DC    AL2(DMF711P),AL1(1,5),AL2(1900,2300),B'01111100',X'00'           
         DC    AL2(DMF89P),AL1(1,5),AL2(2000,2100),B'01111100',X'00'            
         DC    AL2(DMF911P),AL1(1,5),AL2(2100,2300),B'01111100',X'00'           
         DC    AL2(DMF11HP),AL1(1,5),AL2(2300,2330),B'01111100',X'00'           
         DC    AL2(DMF11P1A),AL1(1,5),AL2(2300,2500),B'01111100',X'00'          
         DC    AL2(DMF113P1),AL1(1,5),AL2(2330,2500),B'01111100',X'00'          
*                                                                               
*MON-SUN (6AM-2AM) AND (24-HOUR TOTAL)                                          
         DC    AL2(DMS6A2A),AL1(1,7),AL2(0600,2600),B'01111111',X'80'           
         DC    AL2(DMS24H),AL1(1,7),AL2(0600,3000),B'01111111',X'80'            
                                                                                
         DC    AL2(DMS71A),AL1(1,7),AL2(0700,2500),B'01111111',X'80'            
         DC    AL2(DMS9A12M),AL1(1,7),AL2(0900,2400),B'01111111',X'80'          
         DC    AL2(DMS68P),AL1(1,7),AL2(1800,2000),B'01111111',X'80'            
         DC    AL2(DMS711P),AL1(1,7),AL2(1900,2300),B'01111111',X'80'           
                                                                                
         DC    AL2(DMSPRIME),AL1(1,6),AL2(2000,2300),B'01111110',X'80'          
         DC    AL2(DMSPRIME),AL1(7,7),AL2(1900,2300),B'00000001',X'80'          
                                                                                
         DC    AL2(DMS11HP),AL1(1,7),AL2(2300,2330),B'01111111',X'80'           
         DC    AL2(DMS1112M),AL1(1,7),AL2(2300,2400),B'01111111',X'80'          
         DC    AL2(DMS1131A),AL1(1,7),AL2(2330,2500),B'01111111',X'80'          
         DC    AL2(DMS12M1A),AL1(1,7),AL2(2400,2500),B'01111111',X'80'          
*SAT-SUN                                                                        
         DC    AL2(DSS47P),AL1(6,7),AL2(1600,1900),B'00000011',X'90'            
         DC    AL2(DSS711P),AL1(6,7),AL2(1900,2300),B'00000011',X'90'           
         DC    X'FF'                                                            
                                                                                
*------------------------------------------------------------------*            
* PTYTAB - NEW ONE                                                              
*------------------------------------------------------------------*            
PTYTAB   DS    0H                                                               
*EVG 7:00P-11:00P                                                               
         DC    AL2(EVGD),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'GD',X'01'      GENERAL DRAMA 7P11-REG               
         DC    AL2(EVVA),AL1(MSU),AL2(1900,2300),B'01111111',X'80'              
         DC    AL1(030),CL2'VA',X'01'      VARIETY 7P11- REG                    
         DC    AL2(EVIN7P11),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(030),CL2'IN',X'01'      INFORM  7P11- REG                    
         DC    AL2(EVFF7P11),AL1(MSU),AL2(1900,2300),B'01111111',X'80'          
         DC    AL1(120),CL2'FF',X'01'      FT-FILM 7P11- REG                    
         DC    AL2(EVFFRS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'            
         DC    AL1(120),CL2'FF',X'05'      FT-FILM 7P11- REG+SPEC               
         DC    AL2(ALL7P9),AL1(MSU),AL2(1900,2100),B'01111111',X'80'            
         DC    AL1(030),CL2'79',X'01'      ALL     7P9   REG                    
         DC    AL2(ALL9P11),AL1(MSU),AL2(2100,2300),B'01111111',X'80'           
         DC    AL1(030),CL2'91',X'01'      ALL     9P11- REG                    
         DC    AL2(EVTLR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'             
         DC    AL1(030),CL2'71',X'01'      EVG TEL 7P11- REG                    
         DC    AL2(EVUNR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'             
         DC    AL1(030),CL2'71',X'01'      EVG UNI 7P11- REG                    
         DC    AL2(EVALLR),AL1(MSU),AL2(1900,2300),B'01111111',X'80'            
         DC    AL1(030),CL2'71',X'01'      TEL+UNI 7P11- REG                    
         DC    AL2(EVTLS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'             
         DC    AL1(030),CL2'71',X'01'      TEL+UNI 7P11- REG                    
         DC    AL2(EVTLS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'             
         DC    AL1(030),CL2'71',X'04'      TEL     7P11- SPC                    
         DC    AL2(EVUNS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'             
         DC    AL1(030),CL2'71',X'04'      UNI     7P11- SPC                    
         DC    AL2(EVALLS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'            
         DC    AL1(030),CL2'71',X'04'      TEL+UNI 7P11- SPC                    
         DC    AL2(EVTLRS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'            
         DC    AL1(030),CL2'71',X'05'      TEL     7P11- REG+SPC                
         DC    AL2(EVUNRS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'            
         DC    AL1(030),CL2'71',X'05'      UNI     7P11- REG+SPC                
         DC    AL2(EVALLRS),AL1(MSU),AL2(1900,2300),B'01111111',X'80'           
         DC    AL1(030),CL2'71',X'05'      TEL+UNI 7P11- REG+SPC                
*EVG  6:00P-7:00P                                                               
         DC    AL2(WKDIN),AL1(MSU),AL2(1800,1900),B'00000011',X'90'             
         DC    AL1(030),CL2'IN',X'01'      WKDY INF 6P7- REG **WKND**           
         DC    AL2(MFINF),AL1(MSU),AL2(1800,1900),B'01111100',X'00'             
         DC    AL1(030),CL2'IN',X'01'      M-F INF  6P-7 -REG **M-F**           
*EVG 11:00P-1:00A                                                               
         DC    AL2(WKD11P1),AL1(MSU),AL2(2300,0100),B'00000011',X'90'           
         DC    AL1(030),CL2'11',X'01'      WKDY    11P-1 -REG **WKND**          
         DC    AL2(MF11P1),AL1(MSU),AL2(2300,0100),B'01111100',X'00'            
         DC    AL1(030),CL2'11',X'01'      M-F     11P-1 -REG  **M-F**          
*WKDAY 6:00AM-4:30PM                                                            
         DC    AL2(WKFF6A43),AL1(MSU),AL2(0600,1630),B'01111100',X'00'          
         DC    AL1(120),CL2'FF',X'01'      WKDY FF  6A-4:30P-REG                
         DC    AL2(WKDD6A43),AL1(MSU),AL2(0600,1630),B'01111100',X'00'          
         DC    AL1(030),CL2'DD',X'01'      WKDY DD  6A-4:30P-REG                
         DC    AL2(WKIN6A43),AL1(MSU),AL2(0600,1630),B'01111100',X'00'          
         DC    AL1(030),CL2'IN',X'01'      WKDY IN  6A-4:30P-REG                
         DC    AL2(WKAD6A10),AL1(MSU),AL2(0600,1000),B'01111100',X'00'          
         DC    AL1(030),CL2'AD',X'01'      WKDY ADU 6A-10AM-REG                 
         DC    AL2(WKCH6A10),AL1(MSU),AL2(0600,1000),B'01111100',X'00'          
         DC    AL1(030),CL2'CH',X'01'      WKDY-CHL 6A-10AM-REG                 
         DC    AL2(WK10A1),AL1(MSU),AL2(1000,1300),B'01111100',X'00'            
         DC    AL1(030),CL2'64',X'01'      WKDY-ALL 10A-1:00P-REG               
         DC    AL2(WK10A1),AL1(MSU),AL2(1000,1300),B'01111100',X'00'            
         DC    AL1(030),CL2'64',X'01'      WKDY-ALL 10A-1:00P-REG               
         DC    AL2(WK1P43),AL1(MSU),AL2(1300,1630),B'01111100',X'00'            
         DC    AL1(030),CL2'64',X'01'      WKDY-ALL 1P-4:30P-REG                
         DC    AL2(WK10A43T),AL1(MSU),AL2(1000,1630),B'01111100',X'00'          
         DC    AL1(030),CL2'64',X'01'      WKDY-TEL 10-4:30P-REG                
         DC    AL2(WK10A43U),AL1(MSU),AL2(1000,1630),B'01111100',X'00'          
         DC    AL1(030),CL2'64',X'01'      WKDY-UNI 10-4:30P-REG                
         DC    AL2(WK10A43A),AL1(MSU),AL2(1000,1630),B'01111100',X'00'          
         DC    AL1(030),CL2'64',X'01'      WKDY-ALL 10-4:30P-REG                
*WKEND:6AM-6PM                                                                  
         DC    AL2(WDCHTEL),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'CH',X'01'      CHILD TEL 6-6:00P -REG               
         DC    AL2(WDCHUNI),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'CH',X'01'      CHILD UNI 6-6:00P-REG                
         DC    AL2(WDCHALL),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'CH',X'01'      CHILD ALL 6-6:00P-REG                
         DC    AL2(WDSPTR),AL1(MSU),AL2(0600,1800),B'00000011',X'90'            
         DC    AL1(030),CL2'SP',X'01'      SPORT TEL 6-6:00P-REG                
         DC    AL2(WDSPUR),AL1(MSU),AL2(0600,1800),B'00000011',X'90'            
         DC    AL1(030),CL2'SP',X'01'      SPORT UNI 6-6:00P-REG                
         DC    AL2(WDSPR),AL1(MSU),AL2(0600,1800),B'00000011',X'90'             
         DC    AL1(030),CL2'SP',X'01'      SPORT ALL 6-6:00P-REG                
         DC    AL2(WDSPTRS),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'SP',X'05'      SPORT TEL 6-6:00P-REG+SPC            
         DC    AL2(WDSPURS),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'SP',X'05'      SPORT UNI 6-6:00P-REG+SPC            
         DC    AL2(WDSPRS),AL1(MSU),AL2(0600,1800),B'00000011',X'90'            
         DC    AL1(030),CL2'SP',X'05'      SPORT ALL 6-6:00P-REG+SPC            
         DC    AL2(WDIN6A6),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'IN',X'01'      INFORM    6-6:00P-REG                
         DC    AL2(WDFF6A6),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(120),CL2'FF',X'01'      FTR FILM  6-6:00P-REG                
         DC    AL2(WDPM6A6),AL1(MSU),AL2(0600,1800),B'00000011',X'90'           
         DC    AL1(030),CL2'PC',X'01'      POP MUSIC 6-6:00P-REG                
*M-S 24HR                                                                       
         DC    AL2(SP24R),AL1(MSU),AL2(0000,2400),B'01111111',X'80'             
         DC    AL1(030),CL2'SP',X'01'      SPRTS ALL - 24HR - REG               
         DC    AL2(SP24RS),AL1(MSU),AL2(0000,2400),B'01111111',X'80'            
         DC    AL1(030),CL2'SP',X'05'      SPRTS ALL - 24HR - REG+SPC           
         DC    AL2(IN24),AL1(MSU),AL2(0000,2400),B'01111111',X'80'              
         DC    AL1(030),CL2'SP',X'01'      INFO  ALL - 24HR - REG               
         DC    AL2(NEWS24),AL1(MSU),AL2(0000,2400),B'01111111',X'80'            
         DC    AL1(030),CL2'NW',X'01'      NEWS  ALL - 24HR - REG               
         DC    AL2(FF24),AL1(MSU),AL2(0000,2400),B'01111111',X'80'              
         DC    AL1(120),CL2'FF',X'01'      FTR FILM  - 24HR - REG               
         DC    AL2(VA24),AL1(MSU),AL2(0000,2400),B'01111111',X'80'              
         DC    AL1(030),CL2'NW',X'01'      VARIETY   - 24HR - REG               
         DC    AL2(GD24),AL1(MSU),AL2(0000,2400),B'01111111',X'80'              
         DC    AL1(030),CL2'GD',X'01'      GEN DRAMA - 24HR - REG               
         DC    AL2(CA24),AL1(MSU),AL2(0000,2400),B'01111111',X'80'              
         DC    AL1(030),CL2'CA',X'01'      CHILD ANIM- 24HR - REG               
         DC    AL2(SPC24T),AL1(MSU),AL2(0000,2400),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'04'      SPC TEL   - 24HR - SPC               
         DC    AL2(SPC24U),AL1(MSU),AL2(0000,2400),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'04'      SPC UNI     24HR - SPC               
         DC    AL2(SPC24A),AL1(MSU),AL2(0000,2400),B'01111111',X'80'            
         DC    AL1(030),CL2'S ',X'04'      SPC ALL     24HR - SPC               
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* BLDKEY -     BUILD PAV KEY- COME HERE WHEN INTERIM RECD IS COMPLETE           
***********************************************************************         
                                                                                
BLDKEY   NTR1  BASE=*,LABEL=*                                                   
         CLI   INTRTYP,PMCODEQU    TEST FOR 'Q' RECORD                          
         BE    BLD70                                                            
                                                                                
BLD60    LA    R7,INTKEY           BUILD PAV KEY                                
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     -P- RECORD                                   
         MVI   PRMEDIA,C'W'                                                     
         CLC   =C'UUUU',INTSTA                                                  
         BNE   *+8                                                              
         MVI   PRMEDIA,0           SORT UNIVERSES FIRST                         
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         MVC   PRSTAT,INTSTA                                                    
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
                                                                                
BLD70    LA    R7,INTKEY           BUILD 'Q' RECORD KEY                         
         USING PMKEY,R7                                                         
         MVI   PMCODE,PMCODEQU     -Q-                                          
         MVI   PMMEDIA,C'W'                                                     
         MVI   PMSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
BLD74    MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMBTYP+1(2),INTPNUM                                              
         MVC   PMBTYP+3(1),MKTBRK                                               
                                                                                
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
                                                                                
BLDX     MVC   INTKEY+29(1),GAASW                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* PROCDEMS -   SLOT DEMOS IN INTERIM RECD                                       
*        ON ENTRY     DMCB(2)  = MARKET BREAK NUMBER                            
*                     DMCB+2(2)= DEMO GROUP NUMBER                              
*                     DMCB+4(4)= INPUT DEMO ADDRESS                             
***********************************************************************         
                                                                                
PROCDEMS NTR1  BASE=*,LABEL=*                                                   
         CLC   MITSEQ,=C'04'       PRG RECD DEMOS?                              
         BNE   PROCDEM0                                                         
         OC    DMCB(2),DMCB        MKT BRK=0 = TOTAL SAMPLE?                    
         BZ    PD10                NEED TO CONFIGURE WHICH BUFFER.              
         CLC   DMCB(2),=H'515'     SPANISH DOMINANT                             
         BNE   PROCDEM0            -> DEMAREA                                   
                                                                                
PD10     CLI   MITPUT,C'1'         PUT DEMOS?                                   
         BNE   PD30                -> HPRG: ALL HALF-HOUR NON-PUTS              
                                                                                
         OC    DMCB(2),DMCB        TOTAL SAMPLE PUTS                            
         BNZ   PD20                                                             
                                                                                
         LA    R7,PUTS             -> REGULAR PUTS AREA                         
         CLC   =C'PP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'FP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'LP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'00',MITHLFID     HALF-HOUR PUTS?                              
         BE    PROCDEM2                                                         
         LA    R7,HPUTS            -> HALF HOUR PUTS AREA                       
         B     PROCDEM2                                                         
                                                                                
PD20     LA    R7,SDPUTS           -> SPANISH DOMINANT PUTS AREA                
         CLC   =C'00',MITHLFID                                                  
         BE    PROCDEM2                                                         
         CLC   =C'PP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'FP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'LP',MITHLFID                                                  
         BE    PROCDEM2                                                         
         LA    R7,HPUTS            -> HPUTS: ALL HALF-HOUR PUTS                 
         B     PROCDEM2                                                         
                                                                                
PD30     CLC   =C'00',MITHLFID     NON-PUT PROGRAM:                             
         BE    PROCDEM0                                                         
         CLC   =C'PP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'FP',MITHLFID                                                  
         BE    *+10                                                             
         CLC   =C'LP',MITHLFID                                                  
         BE    PROCDEM0                                                         
         LA    R7,HPRG             -> HPRG: HALF-HOUR PROGRAMS                  
         B     PROCDEM2                                                         
                                                                                
PROCDEM0 L     R1,=A(CRCITAB)      GET THE CATAGORY INDEX                       
PROCDEM1 CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MARKET BREAK NOT FOUND                       
         CLC   DMCB(2),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PROCDEM1                                                         
                                                                                
         LH    R7,2(R1)            FIND OVERALL SLOT                            
         MH    R7,SLOTLN                                                        
         A     R7,=A(DEMAREA)      HAVE SLOT ADDRESS                            
         CLC   =X'0007',2(R1)                                                   
         BNE   PROCDEM2                                                         
         CLC   =H'501',DMCB+2                                                   
         BNE   PROCDEM2                                                         
         LR    RE,R7               INIT/CLEAR BUFFER                            
         LA    RF,NDEMS*4          SLOTLN                                       
         XCEF                                                                   
                                                                                
PROCDEM2 ST    R7,DMCB+8           SAVE START ADDR WHERE TO STORE DEMS          
         LA    R0,20                                                            
         CLI   MITPUT,C'1'         PUT?                                         
         BNE   PROCDM2A                                                         
         OC    SVPUT,ZEROS                                                      
         PACK  DUB,SVPUT           SAVE PUT                                     
         CVB   R1,DUB                                                           
         ST    R1,RIHOMES*4(R7)                                                 
         B     PROCDEM3                                                         
                                                                                
PROCDM2A OC    SVPROJ,ZEROS                                                     
         PACK  DUB,SVPROJ                                                       
         CVB   R1,DUB                                                           
         ST    R1,RIUSA*4(R7)                                                   
         OC    SVMPROJ,ZEROS                                                    
         PACK  DUB,SVMPROJ                                                      
         CVB   R1,DUB                                                           
         ST    R1,RIHOMES*4(R7)                                                 
                                                                                
PROCDEM3 L     R1,=A(SLOTTAB)      FIND THE SLOT FOR DEMOS                      
PROCDEM4 CLI   0(R1),X'FF'         DEMO GROUP NOT FOUND                         
         BE    PROCDEM5            NEXT SLOT - SOME ARE OPEN                    
         CLC   DMCB+2(2),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PROCDEM4                                                         
         SPACE 1                                                                
         LH    R7,2(R1)            GET THE DEMO SLOT                            
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
                                                                                
         CLC   DMCB(2),=H'515'     SPANISH DOMINANT                             
         BNE   PROCDX                                                           
         CLC   MITDEMG,=C'041'     THE LAST PREC GROUP                          
         BNE   PROCDX                                                           
         BRAS  RE,PROCSD           NEED TO CALCULATE SEVERAL SD DEMOS           
                                                                                
PROCDX   XIT1                                                                   
         LTORG                                                                  
                                                                                
**********************************************************************          
* PROCSD - CALCULATE 8 SPECIFIC SPANISH DOMINANT DEMOS                          
*          DMCB+8A: DEMAREA - ALREADY FILLED IN WITH VALUES                     
**********************************************************************          
                                                                                
PROCSD   NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(SLOTSD)       POINTS TO THE CALCULATION TABLE              
PSD05    L     R2,DMCB+8           DEMAREA                                      
         ZIC   R7,0(R1)            NUMBER OF DEMOS TO ADD UP                    
         LA    R1,1(R1)            POINT TO EACH DEMO DISPLACEMENT              
         SR    R6,R6               CLEAR RUNNING TOTAL                          
                                                                                
PSD10    LH    R0,0(R1)                                                         
         SLL   R0,2                                                             
         AR    R2,R0               POINT TO DEMO TO BE ADDED UP                 
         L     R5,0(R2)                                                         
         AR    R6,R5                                                            
         LA    R1,2(R1)            BUMP UP TABLE INDEX ONCE                     
         L     R2,DMCB+8           RE POINT TO BEGINNING OF DEMAREA             
         BCT   R7,PSD10            PSD10                                        
                                                                                
         LH    R0,0(R1)                                                         
         SLL   R0,2                                                             
         AR    R2,R0                                                            
         STCM  R6,15,0(R2)                                                      
         LA    R1,2(R1)            POINT TO NEXT ENTRY!                         
         CLI   0(R1),X'FF'                                                      
         BNE   PSD05                                                            
                                                                                
PSDX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
DPTTABD  DSECT                                                                  
DPTPROG  DS    XL2                                                              
DPTSDAY  DS    XL1                                                              
DPTEDAY  DS    CL1                                                              
DPTSTIM  DS    CL2                                                              
DPTETIM  DS    CL2                                                              
DPTDYBIT DS    CL1                                                              
DPTDAYWK DS    CL1                                                              
                                                                                
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
                                                                                
       ++INCLUDE DENTHID                                                        
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTNT3D                                                      
       ++INCLUDE DECALVPHD                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMCNVD                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DEHW05I   09/11/07'                                      
         END                                                                    
