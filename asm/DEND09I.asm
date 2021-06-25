*          DATA SET DEND09I    AT LEVEL 083 AS OF 09/30/15                      
*PHASE DEND09IA                                                                 
         TITLE 'DEMO CONVERSION'                                                
DEND09I TITLE '- DEMO CONVERSION - NTI DAILIES'                                 
                                                                                
* Y=RAW IMPRESSIONS Z=RAW PUTS(PROGRAM) W=RAW PUTS(TIME PERIOD)                 
* F=DISPLACEMENT FILLER B=GROSS AVERAGE AUDIENCE                                
* O=CALCULATED PUTS                                                             
VHWC18   EQU   K0521               VHWC18                                       
RHWC18   EQU   K0621               RHWC18                                       
IHWC18   EQU   K0633               IHWC18                                       
GAHOMES  EQU   K1501-K0000+4       BWW55+                                       
AAHOMES  EQU   K0533-K0000         RHOMES                                       
TPPUT    EQU   K1197-K0000         WW25                                         
PUTSTRT  EQU   K0821-K0000         FIRST O                                      
PUTEND   EQU   K1041-K0000+4       LAST O BEFORE HOMES                          
TPHUTR   EQU   K1045-K0000         OHOMES                                       
GAIMPS   EQU   K1353-K0000         BW25                                         
AAIMPS   EQU   K0193-K0000         YW25                                         
*                                                                               
GAAIMP2  EQU   K1505-K0000         V18+                                         
AAIMP2   EQU   K0349-K0000         M18+                                         
GAALN1   EQU   K0345-K0193         COPY UNTIL YHOMES                            
GAALN2   EQU   K0641-K0349         COPY AFTER YHOMES                            
*                                                                               
NUMCATS  EQU   38                                                               
         EJECT                                                                  
DEND09I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEND09I,RA,R4,R5                                               
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         SPACE 1                                                                
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         SPACE 1                                                                
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     MORET               E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*READ -  MAIN CONTROL FOR READING RECDS                                         
**********************************************************************          
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
         XC    SVB0SD2,SVB0SD2                                                  
*                                                                               
READ01   MVI   NOTAVAL,0                                                        
         LAY   RE,PRGAVG                                                        
         ST    RE,ACOMWRK          PASS ON TO OUTPUT PHASE                      
         MVI   IPGAASW,C'N'                                                     
         MVI   BYPREAD,X'FF'       SET TO 1ST-TIME-THRU                         
         OPEN  (IN1,(INPUT))                                                    
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS               
         BNE   READ20                                                           
         L     RE,VPUTBUFF         CLEAR THE HPT SAVE AREA                      
         L     RF,HPTSAVLN                                                      
         XCEF                                                                   
         MVI   RELOFRST,0                                                       
         SPACE 1                                                                
READ20   DS    0H                                                               
         CLI   BYPREAD,C'H'        HPT RELEASE MODE                             
         BE    HPTGEN                                                           
         L     R3,ARREC                                                         
         XC    0(4,R3),0(R3)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,R3),=H'401'     HEADER                                       
*BEGINNING SEP 25,2006, THE RECORDS ON THE NTI OVERNIGHT NIELSEN FILES          
*HAVE A LENGTH OF 400 INSTEAD OF 401.  THE OPTIONAL INDICATOR                   
*HAS BEEN REMOVED AND THE NEW FILE FORMAT IS KNOWN AS 'DELUXE'.                 
*WE CHOSE TO KEEP THE INPUT DATA SETS WITH RECORD LENGTH DEFINED AS 401         
*FOR BACKWARDS COMPATIBILITY. THE NIELSEN TEXT FILE SHOULD CONTINUE TO          
*BE LOADED TO A DATASET WITH RECORD LENGTH 401 BEFORE RUNNING THE               
*CONVERSION.                                                                    
*                                                                               
         LA    R3,4(R3)                                                         
         CLI   BYPREAD,1           TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
         GET   IN1,(RC)            READ NEXT RECD                               
         USING MIREC,RC                                                         
*                                                                               
         L     RE,ANIINPUT         SET FOR RECORD COUNTS                        
         L     R0,0(RE)                                                         
         AHI   R0,1                                                             
         ST    R0,0(RE)                                                         
*                                                                               
         CLC   MISEQ,=C'02'        BYPASS IF OUTSIDE EXPECTED DATES             
         BL    MISTOK1                                                          
         CLI   PASSDAY2,1                                                       
         BE    MISTOK                                                           
         CLC   SVB0SD,MISTART      FIRST PASS                                   
         BNH   MISTOK                                                           
         MVC   SVB0SD2,MISTART                                                  
         B     READ20                                                           
*                                                                               
MISTOK   CLI   PASSDAY2,1          SECOND PASS                                  
         BNE   *+14                                                             
         CLC   MISTART,SVB0SD2                                                  
         BH    READ20                                                           
MISTOK1  DS    0C                                                               
         DROP  RC                                                               
*                                                                               
         USING H4REC,RC                                                         
         CLC   H4QH,=C'  '         NOT A QH RECORD                              
         BE    READ21              ITS OK                                       
         DROP  RC                                                               
*                                                                               
         USING MIREC,RC                                                         
         CLC   MISEQ(2),=C'03'     BYPASS QH RECORDS FOR                        
         BE    READ20              THESE                                        
         CLC   MISEQ(2),=C'04'     *                                            
         BE    READ20              *                                            
         CLC   MISEQ(2),=C'05'     *                                            
         BE    READ20              *                                            
*                                                                               
* PROCESS RECD READ IN:                                                         
*                                                                               
READ21   CLI   GAASW,C'P'          IGNORE ALL GAA ON -NTI-                      
         BNE   READ25                                                           
         CLC   MISEQ(2),=C'04'                                                  
         BNE   READ30                                                           
         CLI   MIESTYPE,C'2'       GAA'S?                                       
         BE    READ20              YES, BYPASS RECD                             
         B     READ30                                                           
*                                                                               
READ25   MVI   GAASW,C'N'          DEFAULT IS NON-GAA DATA                      
         CLC   MISEQ(2),=C'04'     CHECK GAA FOR DATA                           
         BNE   READ30                                                           
*4/27/98 CLC   MINET(3),=C'SYN'    --> ALWAYS ALLOW GAA                         
*4/27/98 BNE   READ30                                                           
         CLI   MIESTYPE,C'2'                                                    
         BNE   READ30                                                           
         MVI   GAASW,C'Y'                                                       
*                                                                               
READ30   CLI   BYPREAD,X'FF'       TEST FOR 1ST-TIME-THRU                       
         BNE   READ40                                                           
         CLC   MISEQ,=C'00'        TEST FOR REPORT RECORD                       
         BE    B0RTN                                                            
         L     R9,VCPRINT          1ST RECD NOT '00'-> GARBAGE                  
         USING DPRINT,R9                                                        
         MVC   P(50),MISEQ         PRINT OUT GARBAGE RECD                       
         GOTO1 VPRINTER                                                         
         DROP  R9                                                               
         B     READ20              SKIP AND SEE IF RDR IS ON TAPE               
*                                                                               
READ40   CLC   SAVEPNUM,MINUM      BREAK ON PROGRAM NUMBER                      
         BE    READ41                                                           
         MVC   SAVEPNUM,MINUM                                                   
         MVI   BYPASS01,0                                                       
         MVI   VARIOUS,0                                                        
         MVC   VARS,ZEROS                                                       
         MVC   DAYS,ZEROS                                                       
         XC    SAVVAR(SAVVARLQ),SAVVAR                                          
         MVI   SVD4,C'0'                                                        
         MVC   SVD4+1(L'SVD4-1),SVD4                                            
*                                                                               
READ41   DS    0H                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         DROP  R1                                                               
         GOTO1 (RF),DMCB,VWTYPTTB                                               
         ICM   R7,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING VWTYPTD,R7                                                       
READ41A  CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+10                                                             
         AR    R7,RF                                                            
         B     READ41A                                                          
         MVC   KEYSRC,VWTKSRC      SAVE KEY SOURCE HERE                         
*                                                                               
* SPECIAL CONVERSION SINCE ACM,AND NTI SHARE THE SAME NIELSON                   
* VTYPE NUMBER, CONVERT THE ACM MEDIAOCEN NUMBER, TO THE                        
* NTI MEDIAOCEAN NUMBER.                                                        
*                                                                               
         CLI   KEYSRC,SRCLIVE3                                                  
         BNE   *+8                                                              
         MVI   KEYSRC,SRCNTIL3     LIVE+3 DATA FROM NTI TAPE                    
         DROP  R7                                                               
*                                                                               
         CLC   MISEQ,=C'02'        TEST FOR PROGRAM HH EST RECORDS              
         BNE   READ42                                                           
         CLI   MITYPE,C'H'         HALF HOUR DETAIL                             
         BE    H2RTN                                                            
         CLI   MITYPE,C'P'         PERSONS ESTIMATES                            
         BE    P2RTN                                                            
         B     READ20                                                           
*                                                                               
READ42   CLC   MISEQ,=C'01'        TEST FOR UNIVERSE RECORD                     
         BNE   READ44                                                           
         CLI   MITYPE,C'H'         HOUSEHOLD                                    
         BE    H1RTN                                                            
         CLI   MITYPE,C'P'         PERSONS                                      
         BE    P1RTN                                                            
         B     READ20                                                           
*                                                                               
READ44   L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         DROP  R1                                                               
         GOTO1 (RF),DMCB,VWTYPTTB                                               
         ICM   R7,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING VWTYPTD,R7                                                       
READ45   CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+10                                                             
         AR    R7,RF                                                            
         B     READ45                                                           
*                                                                               
         CLI   BOOKTYPE,C'I'       TEST BOOKTYPE                                
         BNE   READ46                                                           
         CLI   VWTVCR,C'1'         CONVERT NON-VCR (INTEGRATED (SIC))           
         B     READ47                                                           
READ46   CLI   VWTVCR,C' '         CONVERT VCR (ASCRIBED)                       
READ47   BNE   READ20                                                           
         CLC   MISEQ,=C'04'        TEST FOR PROGRAM HH EST RECORDS              
         BNE   READ70                                                           
         CLI   HPTGENSW,C'Y'       FIRST CREATE HPT RECORDS                     
         BE    HPTGEN                                                           
         DROP  R7                                                               
*                                                                               
         CLC   MIDYSWKS,=C'00'                                                  
         BNE   *+8                                                              
         MVI   BYPASS01,0                                                       
         CLI   BYPASS01,1          BYPASS SINGLE DAY AVERAGES                   
         BE    READ20                                                           
         CLI   MIORIG,C'0'         ***TEMP***                                   
         BE    READ48              *        *                                   
         CLI   MIORIG,C'1'         *        *                                   
         BNE   READ20              *        *                                   
         CLC   MINET(3),=C'XYZ'    *        *                                   
         NOP   READ20              *        *                                   
READ48   EQU   *                   ***TEMP***                                   
         CLI   MITYPE,C'D'         DESCRIPTOR                                   
         BE    D4RTN                                                            
*                                                                               
         CLI   NOTAVAL,1           DATA NOT AVAILABLE                           
         BE    READ20              READ NEXT RECORD                             
         CLI   MITYPE,C'H'         HALF HOUR DETAIL                             
         BE    H4RTN                                                            
         CLI   MITYPE,C'P'         PERSONS ESTIMATES                            
         BNE   READ20                                                           
         MVI   BYPASSH4,0          RESET H4 RECORD BYPASS SWITCH                
         CLI   NOTAVAL,2           BYPASS 1/2HR P-RECDS (DNA)                   
         BE    READ20              NO DATA ON RECD, BYPASS THEM                 
         B     P4RTN               NO, PROCESS IT                               
*                                                                               
READ70   CLC   MISEQ,=C'03'        TEST FOR USAGE RECORDS                       
         BNE   READ80                                                           
         CLI   MITYPE,C'H'         HOUSEHOLD                                    
         BE    H3RTN                                                            
         CLI   MITYPE,C'P'         PERSONS DEMOGRAPHICS                         
         BE    P3RTN                                                            
         B     READ20                                                           
*                                                                               
READ80   CLC   MISEQ,=C'05'        TEST FOR NON-NETWORK SOURCES                 
         BNE   READ90                                                           
         CLI   MITYPE,C'H'         HOUSEHOLD                                    
         BE    H5RTN                                                            
         CLI   MITYPE,C'P'         PERSONS DEMOGRAPHICS                         
         BE    P5RTN                                                            
         B     READ20                                                           
*                                                                               
READ90   L     R9,VCPRINT          UNKNOWN RECD TYPE - PRINT PART KEY           
         USING DPRINT,R9                                                        
         MVC   P(50),MISEQ         PRINT OUT GARBAGE RECD                       
         GOTO1 VPRINTER                                                         
         DROP  R9                                                               
         MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
*********************************************************************           
*B0RTN - REPORT DESCRIPTOR RECORD                                               
*********************************************************************           
B0RTN    DS    0H                                                               
         USING B0REC,RC                                                         
         CLC   =C'PRELIMINARY-',B0REPORT                                        
         BE    *+6                                                              
         DC    H'0'                INPUT TAPE IS NOT NTI DAILIES                
*                                                                               
         CLI   PASSDAY2,1          NTI SENDS 3 HOURS OF PREV DAY                
         BNE   B0RTN02             BUT DOESN'T INDICATE IT                      
         LA    RE,P2DAYS           SET THE DAY TO PRIOR DAY                     
         MVC   FILTYP,B0REPORT+12                                               
B0RTN01  CLI   0(RE),X'FF'         NOT FOUND - JUST QUIT                        
         BE    ENDJOB                                                           
         CLC   FILTYP,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,6(RE)                                                         
         B     B0RTN01                                                          
         MVC   B0REPORT+12(3),3(RE)                                             
         MVC   B0START,SVB0SD2                                                  
         MVC   B0END,SVB0SD2                                                    
         MVC   SVB0SD,B0START                                                   
                                                                                
B0RTN02  MVC   FILTYP,B0REPORT+12  WHICH INPUT FILE: MON,TUE,S-S,M-F..          
         BAS   RE,DAYRTN           SET UP DAY TABLE                             
         LA    R1,FILBIT           CONVERT FILE NAME TO DAYS PRESENT            
B0RTN05  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(12,R1),B0REPORT+12                                             
         BE    *+12                                                             
         LA    R1,L'FILBIT(R1)                                                  
         B     B0RTN05                                                          
         MVC   FILDYS,12(R1)                                                    
*                                                                               
         MVC   HDAYTABS,HDAYTAB    SAVE DAY TABLE                               
         MVC   SVB0SD,B0START                                                   
                                                                                
PASSOK   DS    0C                                                               
                                                                                
         GOTO1 VNETWEEK,DMCB,B0END+1,VGETDAY,VADDAY                             
         MVC   HALF(1),4(R1)       YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         MVC   BOOK1,HALF          KEY BOOK ALLOWS BLACK WEEK                   
         MVC   IBOOK1,HALF                                                      
         MVC   BOOK1S,HALF                                                      
         MVC   IBOOK1S,HALF                                                     
         MVI   BYPREAD,0                                                        
         MVI   CORRSW,C'A'         TREAT EVERYTHING AS A CORRECTION             
*--                                                                             
         CLI   ORIGINAL,C'Y'       TREAT AS ORIGINAL INSTEAD OF CRCTN           
         BNE   B0RTN50                                                          
         CLC   =C'MON ',B0REPORT+12                                             
         BE    *+6                                                              
         DC    H'0'                IF NOT A MON TAPE, DON'T LOAD                
         MVI   CORRSW,0                                                         
*--                                                                             
*                                                                               
B0RTN50  DS    0H                                                               
         CLI   PASSDAY2,1           SECOND TIME ITS OK                          
         BE    B0RTN51              DON'T REINT BIT TABLE                       
         L     R6,VBITMAP1                                                      
         ICM   R6,8,=C'N'          FOR REG PCKTPC -> NETWORK MAP                
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(R6),0                                     
B0RTN51  MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
         B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*RESLOT- READ RECD FROM FILE BUILT BY REG NTI AND SLOT DEMOS BACK               
*        INTO APPROPRIATE TABLE AND BUCKETS (RE-SLOT THEM)                      
**********************************************************************          
RESLOT   NTR1                                                                   
         MVC   SVKEY,PAVKEY                                                     
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIDIR',PAVKEY,(R6)                 
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PAVKEY(18),0(R6)    KEY READ IN                                  
         BNE   RESLEOF             NOT FOUND                                    
         MVC   SVDA,PRNDXDA-PRKEY(R6)                                           
         MVC   SVSTATUS,PRKSTAT-PRKEY(R6)                                       
         MVC   PAVKEY,SVKEY        RESTORE SAVED KEY                            
         MVC   0(L'SVKEY,R6),SVKEY                                              
         MVC   PRRSTAT-PRKEY(1,R6),SVSTATUS                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIFIL',SVDA,(R6)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVKEY(18),0(R6)     SAME KEY?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,PRFRSTEL-PRKEY(R6) PT TO 1ST ELEMENT                          
         SR    R0,R0                                                            
RESL10   CLI   0(R6),X'00'         ENDREC?                                      
         BNE   *+6                                                              
         DC    H'0'                END OF REC --NO '5E' ELEMENT???              
         CLI   0(R6),X'41'                                                      
         BE    RESL15              DEMO ELEMENT                                 
         IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     RESL10                                                           
*                                                                               
RESL15   ZIC   R0,1(R6)                                                         
         AR    R0,R6                                                            
         ST    R0,DMCB             ADDR OF NEXT ELEM                            
         MVC   DMCB+4(1),2(R6)     SAVE PRECISION                               
         NI    DMCB+4,X'0F'                                                     
         ZIC   R0,DMCB+4           LENGTH OF EACH FLD IN ELEMT                  
         LA    R1,4                                                             
         SR    R1,R0               #BYTES TO DISP INTO UNIVSAVE                 
         L     R3,ASLOT            SLOT INTO BEGIN SLOT SPECIFIED               
         AR    R3,R1                                                            
         LA    R6,3(R6)            PT TO 1ST DEMO IN ELEMENT                    
*                                                                               
RESL20   ZIC   R1,DMCB+4           PRECISION                                    
         C     R6,DMCB             END OF THIS ELELMENT                         
         BL    RESL25                                                           
         SR    R1,R1               SET CC=ZERO ON EXIT                          
         B     RELSLOTX                                                         
*                                                                               
RESL25   BCTR  R1,0                MOVE DEMO INTO SLOT                          
         EXMVC R1,0(R3),0(R6)                                                   
         LA    R3,4(R3)            BUMP UNIV PTR                                
         AR    R6,R1               BUMP ELEMENT PTR                             
         LA    R6,1(R6)            +1 FROM BCTR                                 
         B     RESL20                                                           
*                                                                               
RESLEOF  LA    R1,1                SET CC TO NON ZERO                           
*                                                                               
RELSLOTX LTR   R1,R1                                                            
         B     EXIT                                                             
*                                                                               
*                                                                               
*********************************************************************           
*H1RTN - UNIVERSE RECORDS:  H-P-P                                               
*********************************************************************           
H1RTN    DS    0H                                                               
         USING H1REC,RC                                                         
         LA    RE,INTKEY                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         CLC   H1MKTBRK,=C'000'    BASIC CELL ONLY                              
         BNE   H1R10                                                            
         PACK  DUB,H1HHUNIV        UNIVERSE SAVED FOR CNVWR RTN                 
         CVB   RF,DUB                                                           
         ST    RF,SVHHUNIV         SAVE THE HOUSEHOLD UNIVERSE                  
*                                                                               
H1R10    MVI   INTAPESW,X'40'      DROP RECORD                                  
         MVI   BYPREAD,0           SET TO READ NEXT RECORD                      
         B     EXIT                                                             
         DROP  RC                                                               
         SPACE 2                                                                
*----------------------------------                                             
*        PROCESS PERSONS UNIVERSES                                              
*----------------------------------                                             
P1RTN    DS    0H                  P1 AND P2 RECORDS HAVE SAME FORMAT           
         USING P2REC,RC                                                         
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         LAY   R6,UNIVSAVE                                                      
         LA    RE,INTACCS-INTRECLN                                              
         SR    R6,RE               DISPLACEMENT TO INTACCS                      
         CLC   P2DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BNE   P1R40                                                            
         BRAS  RE,HKPROC           SLOT UNIVERSE 001-020                        
         MVI   BYPREAD,0                                                        
         B     READ20                                                           
*                                                                               
P1R40    BRAS  RE,IKPROC           2ND P-REC: SLOT UNIVERSES 021-040            
         LA    RE,INTKEY           CLEAR IREC AND RELEASE UNV RECD              
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   INTRTYP,C'P'        CREATE TIME PERIOD -P- UNV RECD              
         MVC   INTBOOK,BOOK1                                                    
         MVC   INTIBOOK,IBOOK1                                                  
         MVC   INTSTA(5),=C'UUUUD'  NTI UNIVERSES= 'UUUU-T'                     
         MVI   INTPNAME,C' '                                                    
         MVC   INTPNAME+1(L'INTPNAME-1),INTPNAME                                
         MVI   INTTITLE,C' '                                                    
         MVC   INTTITLE+1(L'INTTITLE-1),INTTITLE                                
         LAY   R1,UNIVSAVE                                                      
         MVC   INTACCS(NUMCATS*4),0(R1)      MOVE IN DEMO UNIVERSES             
         L     R7,AIREC                                                         
         MVC   K0345-K0000(4,R7),SVHHUNIV   SAVE HUT AT END OF ELEM             
*                                                                               
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     BUILD -P-  KEY                               
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVI   BYPREAD,0           READ NEXT RECD                               
         DROP  R7                                                               
*                                  BUILD UNIVERSE RECORDS TO BE USED            
         BRAS  RE,UFORMS           TO BUILD THE DFORMULA CTFILE RECDS           
*                                                                               
         B     EXIT                RELEASE UNIVERSE RECD                        
         DROP  RC                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
*H2RTN   - SAMPLE COUNTS RECORD                                                 
*********************************************************************           
H2RTN    DS    0H                                                               
         USING H2REC,RC                                                         
         USING INTABD,R1                                                        
         CLC   H2MKTBRK,=C'000'    BASIC CELL ONLY                              
         BNE   H2R90                                                            
         MVC   DAYS,H2DAYS                                                      
*                                                                               
         LAY   R1,INTAB            R1 - INTAB COUNT TABLE                       
         LA    R0,INTABS           7 DAYS                                       
         CLI   H2SCALED,C'1'                                                    
         BNE   H2R10                                                            
         LAY   R1,SINTAB           R1 - SCALED INTAB COUNT TABLE                
         LA    R0,SINTABS          7 DAYS                                       
H2R10    OC    INTABDAT,INTABDAT                                                
         BZ    *+14                NEXT AVAILABLE ENTRY IN TABLE                
         LA    R1,L'INTABTAB(R1)                                                
         BCT   R0,H2R10                                                         
         DC    H'0'                                                             
         ST    R1,INTABADR         SAVE ADDRESS OF ENTRY FOR P2RTN              
*                                                                               
H2R20    LA    RE,NETDAYTB                                                      
         LA    R6,DAYS                                                          
         LA    R0,L'H2DAYS         MON (X'40') THRU SUN (X'01')                 
H2R30    CLI   0(R6),C'1'                                                       
         BE    H2R40                                                            
         LA    R6,1(R6)                                                         
         LA    RE,L'NETDAYTB(RE)                                                
         BCT   R0,H2R30                                                         
         CLC   H2DYSWKS,=C'01'                                                  
         BE    H2R90               DONE                                         
         DC    H'0'                INDIV DAY RECD W/UNDEFINED DAY               
*                                                                               
H2R40    MVI   0(R6),C'0'          DAY PROCESSED - CLEAR IT                     
         MVC   INTABDAY,0(RE)      DAY OF WEEK                                  
         MVC   INTDAYWK,0(RE)                                                   
         SR    R7,R7                                                            
         IC    R7,1(RE)                                                         
         SRL   R7,4                SHIFT OUT LOW ORDER NIBBLE                   
         BCTR  R7,0                -1                                           
         LA    R6,INTABDAT                                                      
         ST    R1,ADDR             SAVE ADDRESS OF THIS BUFFER                  
*                                                                               
         MVC   PACK8,H2START+1                                                  
         CLC   H2DYSWKS,=C'01'                                                  
         BNE   H2R80                                                            
         LTR   R7,R7                                                            
         BZ    H2R80               IF MON, NO ADJUST NECCESSARY                 
         GOTO1 VADDAY,DMCB,PACK8,PACK8,(R7)   ADJUST DATE FOR M-F               
*                                                                               
H2R80    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,PACK8),(3,(R6))   DATE (YYMMDD)                  
         CLC   H2DYSWKS,=C'01'                                                  
         BNE   H2R90                                                            
         CLC   DAYS,ZEROS          ANYMORE DAYS IN LIST TO PROCESS?             
         BE    H2R90                                                            
         L     R1,ADDR             RESTORE PTR TO LAST ENTRY                    
         LA    R1,L'INTABTAB(R1)   NEXT OPEN ENTRY                              
         B     H2R20                                                            
         DROP  R1                                                               
*                                                                               
H2R90    MVI   BYPREAD,0           FOR P2REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         DROP  RC                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*P2RTN - PROCESS PERSONS SAMPLE COUNTS RECORD                                   
**********************************************************************          
P2RTN    DS    0H                                                               
         USING P2REC,RC                                                         
         USING INTABD,R1                                                        
         MVI   LOOP,0                                                           
         L     R1,INTABADR         RESTORE INTAB ADDRESS (FROM H2RTN)           
         ST    R1,ADDR                                                          
P2R20    L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         LA    R6,INTABCNT                                                      
         LA    RE,INTACCS-INTRECLN                                              
         SR    R6,RE               DISPLACEMENT TO INTACCS                      
         CLC   P2DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BNE   P2R25                                                            
         BRAS  RE,HKPROC                                                        
         B     P2R30                                                            
*                                                                               
P2R25    BRAS  RE,IKPROC           INTAB COUNTS 021-040                         
         CLC   P2DYSWKS,=C'01'     M-F RECD?  (DON'T RELS RECDS)                
         BNE   P2R40               NO, JUST PROCESS REGULARLY                   
*                                                                               
P2R30    CLC   P2DYSWKS,=C'01'     M-F RECD?                                    
         BNE   P2R35                                                            
         ZIC   R7,LOOP                                                          
         LA    R7,1(R7)                                                         
         STC   R7,LOOP                                                          
         CLI   LOOP,5                                                           
         BNL   P2R35               DONE WITH M-F                                
         L     R1,ADDR                                                          
         LA    R1,L'INTABTAB(R1)                                                
         ST    R1,ADDR             NEXT ENTRY IN INTAB                          
         B     P2R20                                                            
         DROP  R1                                                               
*                                                                               
P2R35    MVI   BYPREAD,0                                                        
         B     READ20                                                           
*                                                                               
P2R40    DS    0H                  RELEASE INTAB AS A PNN RECD                  
**             BRAS  RE,IKPROC           INTAB COUNTS 021-040                   
         MVI   INTRTYP,C'P'                                                     
         MVC   INTBOOK,BOOK1                                                    
         MVC   INTIBOOK,IBOOK1                                                  
         MVC   INTSTA(5),=C'ITABD'  INTABS HAVE STATION='ITAB'-D                
         CLI   P2SCALED,C'1'                                                    
         BNE   *+10                                                             
         MVC   INTSTA(5),=C'STABD'  SCALED INTABS HAVE STATION='STAB'-D         
         MVI   INTTITLE,C' '                                                    
         MVC   INTTITLE+1(L'INTTITLE-1),INTTITLE                                
         L     R1,INTABADR         RESTORE INTAB ADDRESS                        
         USING INTABD,R1                                                        
         MVC   INTACCS(NUMCATS*4),INTABCNT                                      
         MVC   INTPNAME(3),INTABDAT                                             
         DROP  R1                                                               
*                                                                               
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     BUILD -P-  KEY                               
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,C'A'                                                      
         BNE   *+14                                                             
         MVC   PRCODE(2),BOOK1                                                  
         MVI   PRSRC,PRCODEQU                                                   
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRDW,INTDAYWK       INTERNAL DAY CODE (MON..SUN)                 
         MVI   BYPREAD,0                                                        
         B     EXIT                RELEASE THIS RECD                            
         DROP  R7                                                               
         DROP  RC                                                               
         EJECT                                                                  
*********************************************************************           
*H3RTN - HOUSEHOLD USAGE RECORD                                                 
*        GENERATE 2 QTR-HOUR RECDS FROM EACH HALF-HOUR (H) RECD                 
*********************************************************************           
H3RTN    DS    0H                                                               
         USING H3REC,RC                                                         
         L     RF,AIREC            CLEAR YHOMES FOR UNIV RECD ONLY              
         USING KDSCT,RF                                                         
         XC    K0345,K0345                                                      
         DROP  RF                                                               
*                                                                               
         PACK  DUB,H3EVSHR         INITIALIZE START QUARTER HOUR                
         CVB   RF,DUB                                                           
         SHI   RF,6                START HOUR = 06-29                           
         SLL   RF,2                                                             
         CLC   H3EVSMIN,=C'30'     START MINUTE = 00 OR 30                      
         BNE   *+8                                                              
         LA    RF,2(RF)                                                         
         STC   RF,HUTSQH           FIRST QUARTER HOUR                           
         B     H3R14                                                            
*                                                                               
H3R14    MVI   HUTDUR,1            SET DURATION                                 
         MVC   HUTSTA,=C'HUT D'                                                 
         MVC   HUTBOOK,BOOK1                                                    
         MVC   HUTIBOOK,IBOOK1                                                  
*                                                                               
*                                                                               
         LA    RE,HDAYTAB                                                       
         LA    R0,HDAYS                                                         
H3R20    CLC   H3START,2(RE)       TEST START DAY VS. TABLE                     
         BNE   H3R24                                                            
         CLC   H3END,9(RE)         TEST END DAY VS. TABLE                       
         BE    H3R28                                                            
H3R24    LA    RE,L'HDAYTAB(RE)                                                 
         BCT   R0,H3R20                                                         
         DC    H'0'                                                             
H3R28    MVC   HUTDAYWK,1(RE)      INTERNAL DAY CODE                            
         LA    R6,HPTLN            LENGTH OF EACH ENTRY                         
         MHI   R6,48               * NUMBER OF HALF HOURS                       
         ZIC   R7,HUTDAYWK         GET THE DEMO DAY                             
         SRL   R7,4                SLIDE IT TO THE RIGHT HALF                   
         MR    R6,R6               ADJUST FOR THE DAY                           
         A     R7,VPUTBUFF         AND TABLE START                              
         ZIC   RE,HUTSQH           NOW DO IT FOR THE HALF HOUR                  
         SRL   RE,1                CHANGE QH TO HALF HR                         
         LA    RF,HPTLN                                                         
         MR    RE,RE                                                            
         AR    R7,RF                                                            
         ST    R7,HPTADDR          SAVE FOR PUT ROUTINES                        
         USING HPTD,R7                                                          
         MVC   HPTDY,HUTDAYWK                                                   
         MVC   HPTQHR,HUTSQH                                                    
         MVC   HPTFLAG,H3OPT       SAVE OPT. DATA FLAG                          
         PACK  DUB,H3AVEHUT        DO THE HALF HOUR FIGURES                     
         CVB   RE,DUB                                                           
         ST    RE,HPTAVEI          RE=1/2HR IMP                                 
         PACK  DUB,H3AVERTG                                                     
         CVB   RF,DUB                                                           
         ST    RF,HPTAVER          RF=1/2HR RTG                                 
         ST    RE,HPTQH1I                                                       
         ST    RF,HPTQH1R                                                       
         ST    RE,HPTQH2I                                                       
         ST    RF,HPTQH2R                                                       
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*P3RTN - PROCESS PERSONS DEMOGRAPHICS RECORD                                    
**********************************************************************          
P3RTN    DS    0H                                                               
         USING P3REC,RC                                                         
*                                                                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
*                                                                               
         CLC   P3DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BNE   P3R20                                                            
*                                  DEMOS 001-020                                
         BRAS  RE,AKPROC                                                        
*                                                                               
         MVI   BYPREAD,0                                                        
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
*                                                                               
*                                  DEMOS 021-040                                
P3R20    BRAS  RE,BKPROC                                                        
*                                                                               
         L     R7,HPTADDR                                                       
         MVC   HPTPUT,INTACCS                                                   
*                                                                               
P3R60    B     READ20              JUST GET NEXT RECORD                         
         DROP  R7                                                               
         DROP  RC                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
*HPTGEN -GENERATE TIME PERIOD RECDS                                             
*********************************************************************           
HPTGEN   CLI   HPTFRST,1                                                        
         BNE   HPTGEN2                                                          
         MVI   HPTFRST,0                                                        
         MVC   HPTNOW,VPUTBUFF                                                  
         MVI   QHRSW,0                                                          
*                                                                               
HPTGEN2  L     R3,HPTNOW                                                        
         USING HPTD,R3                                                          
         CLC   HPTCNT,=H'431'                                                   
         BH    HPTGEND                                                          
         OC    HPTPUT(L'HPTPUT),HPTPUT   NO DEMOS FOR TIME PERIOD               
         BNZ   HPTGEN2A                  DON'T GENERATE A RECD                  
         LH    RF,HPTCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,HPTCNT                                                        
         LA    R3,HPTLN(R3)                                                     
         ST    R3,HPTNOW                                                        
         B     HPTGEN2                                                          
*                                                                               
HPTGEN2A MVC   INTBOOK,HUTBOOK                                                  
         MVC   INTIBOOK,HUTIBOOK                                                
         MVC   INTSTA,HUTSTA                                                    
         MVC   INTDAYWK,HPTDY                                                   
         MVC   INTSQH,HPTQHR                                                    
         MVC   INTEQH,HPTQHR                                                    
         MVI   INTDTYP,0                                                        
         CLI   HPTFLAG,C'1'                                                     
         BNE   *+8                                                              
         MVI   INTDTYP,X'80'                                                    
         MVC   INTACCS(L'HPTPUT),HPTPUT                                         
         LA    RE,K0533-K0000(R2)   SET HOMES VALUES                            
         MVC   0(4,RE),HPTQH1R                                                  
         MVC   4(4,RE),HPTQH1I                                                  
         CLI   QHRSW,1                                                          
         BNE   HPTGEN3                                                          
         MVC   0(4,RE),HPTQH2R                                                  
         MVC   4(4,RE),HPTQH2I                                                  
         ZIC   RE,INTSQH                                                        
         LA    RE,1(RE)                                                         
         STC   RE,INTSQH                                                        
         STC   RE,INTEQH                                                        
         MVI   QHRSW,0             SET UP FOR FIRST QHR                         
         LH    RF,HPTCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,HPTCNT                                                        
         LA    R3,HPTLN(R3)                                                     
         ST    R3,HPTNOW                                                        
         B     HPTGEN4                                                          
*                                                                               
HPTGEN3  MVI   QHRSW,1             SET UP FOR SECOND QTR HR                     
*                                                                               
HPTGEN4  MVI   INTDUR,1            QTR HOUR DURATION                            
         MVC   INTMRKT,=H'221'                                                  
         MVI   INTMTYP,1                                                        
         MVI   INTPNAME,C' '                                                    
         MVC   INTPNAME+1(L'INTPNAME-1),INTPNAME                                
         MVI   INTTITLE,C' '                                                    
         MVC   INTTITLE+1(L'INTTITLE-1),INTTITLE                                
         XC    INTPNTI,INTPNTI                                                  
*                                                                               
         MVI   INTRTYP,PRCODEQU    -P-                                          
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     BUILD PAV KEY                                
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,C'A'                                                      
         BNE   *+14                                                             
         MVC   PRCODE(2),BOOK1                                                  
         MVI   PRSRC,PRCODEQU                                                   
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRSTIM,INTSQH                                                    
         MVI   INTKEY+29,255       SET LAST RECORD FLAG                         
         MVI   BYPREAD,C'H'                                                     
         MVI   HPTGENSW,C'N'                                                    
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
HPTGEND  MVI   BYPREAD,1                                                        
         MVI   HPTGENSW,C'N'       ONLY DO IT ONCE                              
         B     READ20                                                           
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
*D4RTN - PROGRAM DESCRIPTOR RECORD D-P-P                                        
*********************************************************************           
D4RTN    DS    0H                                                               
         USING D4REC,RC                                                         
* REMOVED FEB/05                                                                
*        CLI   D4GAPD,C'Y'         SKIP GAPPED PROGRAMS                         
*        BNE   *+12                (TEMPORARY, UNTIL WE FIND A WAY              
*        MVI   NOTAVAL,1           TO DEAL WITH THEM)                           
*        B     D4RX                                                             
*        MVI   NOTAVAL,0                                                        
*                                                                               
         CLI   PRIMEP,1            PRIME PORTION PROCESSED                      
         BNE   D4R02                                                            
         CLC   D4REC(D4HH-D4REC),SVD4KEY                                        
         BE    *+12                                                             
         MVI   PRIMEP,0            RESET SWITCH ON NEW KEY                      
         B     D4R02                                                            
         CLC   D4HH,SVD4KEY+(D4HH-D4REC)                                        
         BNE   READ20              BYPASS TOT PRGM RECD                         
*                                                                               
D4R02    CLC   D4MKTBRK,=C'000'    RESET AVAILABILITY ON PRIME REC              
         BNE   *+8                                                              
         MVI   NOTAVAL,0                                                        
         CLI   NOTAVAL,1           BYPASS EXTRA DESC. RECS                      
         BE    D4RX                WHICH MAY BE MISSING IND                     
         CLI   D4NAVAL,C'1'        N/A FLAG                                     
         BNE   *+12                                                             
         MVI   NOTAVAL,1                                                        
         B     D4RX                                                             
*                                                                               
D4R03    DS    0H                                                               
         XC    HWCAREA,HWCAREA                                                  
         CLI   CORRSW,C'A'         TREAT EVERYTHING AS CORRECTION               
         BE    *+16                                                             
         CLI   D4ORIG,C'0'         TEST CORRECTION RECORD                       
         BE    D4R04               NO                                           
         MVI   CORRSW,1            SET CORRECTION SWITCH                        
         MVC   INTPNO,BOOK1S       ORIGINATING BOOK FOR CORRECTIONS             
         BAS   RE,DAYRTN           SET UP DAY TABLE                             
         PACK  DUB,D4END+1(2)      ***TEMP*** CONVERT YEAR (YY)                 
         CVB   R0,DUB              ***TEMP***                                   
         STC   R0,HALF             ***TEMP***                                   
         GOTO1 VNETWEEK,DMCB,D4END+1,VGETDAY,VADDAY                             
         MVC   HALF(1),4(R1)       YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         MVC   BOOK1,HALF          KEY BOOK ALLOWS BLACK WEEK                   
         MVC   IBOOK1,HALF         WEEK TO PREVIOUS REGULAR WEEK                
*                                                                               
D4R04    MVC   SVPROJ,ZEROS        RESET H4 RECORD'S SAVE AREAS                 
         MVC   SVRTG,ZEROS                                                      
         MVC   SVSHR,ZEROS                                                      
         MVC   SVD4HH,D4HH                                                      
         MVC   SVD4PRG(5),D4NUM+5                                               
         MVI   PRIMEP,0                                                         
         MVC   SVD4KEY,D4REC       SAVE KEY OF THIS D-RECD                      
**P      CLC   D4HH,ZEROS                                                       
**P      BE    *+8                                                              
**P      MVI   PRIMEP,1            SET TO A PRIME PORTION PRESENT               
*                                                                               
         CLC   D4TOTDUR,=C'000000' ARE TOTAL FIELDS ON THIS RECORD              
         BE    D4R04A                                                           
         MVC   SVD4,D4STA          IF YES - SAVE POS.305-363                    
         LAY   RE,DBUFF                                                         
         LA    RF,DBUFFQ                                                        
         XCEF                                                                   
*                                                                               
D4R04A   MVC   D4STA(L'SVD4),SVD4  IF NOT - RESTORE FROM A PREVIOUS D4          
         XC    SAVETIME(56),SAVETIME                                            
         BAS   RE,SETDBUF          SET FLDS IN DBUFF & SAVETIME                 
*                                                                               
         CLC   D4DYSWKS,=C'01'     TEST AVERAGES                                
         BE    D4R06                                                            
         CLI   D4BREAK,C'1'        BREAKOUT                                     
         BE    D4R10                                                            
         LA    RE,D4DAYS                                                        
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
D4R05    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R05                                                         
         B     D4R10                                                            
*                                                                               
D4R06    CLC   D4DAYS,=C'1111100'  M-F                                          
         BE    D4R10                                                            
         CLC   D4DAYS,=C'1111111'  M-S                                          
         BE    D4R10                                                            
         CLC   D4NET(3),=C'SYN'    SYND HAVE NO INDIV. DAY                      
         BNE   *+12                                                             
         MVI   VARIOUS,1           SO FUDGE IT HERE                             
         B     D4R07                                                            
*                                  VARIOUS                                      
         CLI   VARIOUS,1                                                        
         BE    D4R07                                                            
         MVI   VARIOUS,1                                                        
         MVC   VARS,ZEROS          1ST-TIME-THRU                                
         XC    VARSTIME(56),VARSTIME                                            
*                                                                               
         LA    RE,DAYS             TEST NUMBER OF DAYS IN AVERAGE               
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CHI   RF,1                BYPASS SINGLE DAY AVERAGES                   
         BH    D4R07                                                            
         MVI   VARIOUS,0                                                        
         OC    INTVAR(SAVVARLQ),INTVAR  DO WE ALREADY HAVE A VAR                
         BNZ   D4RX                                                             
         MVI   BYPASS01,1                                                       
         B     D4RX                                                             
*                                                                               
D4R07    LA    RE,D4DAYS                                                        
         LA    RF,VARS             SET ON POINTERS FOR VARIOUS DAYS             
         LA    R0,7                                                             
D4R08    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R08                                                         
         CLC   D4NET(3),=C'SYN'    NO INDIV. DAYS FO SYND.                      
         BNE   *+10                                                             
         MVC   DAYS,VARS           SO FORCE THIS                                
*                                                                               
D4R10    XC    PVALS,PVALS                                                      
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),D4NUM                         
         MVC   PNUM,0(R1)          PNUM=DDS INTERNAL NUMBER                     
         MVC   PACK16(10),D4NUM                                                 
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         MVC   PNTINUM,INTPNTI     PNTINUM= NEILSENS NTI PRG NUMBER             
*                                                                               
         MVI   PDPT,0              PDPT IS ALWAYS ZERO                          
         MVC   PDPT2,D4DPT                                                      
         MVC   PNET,D4NET          SET UP R/S CALL LETTERS                      
         MVI   PNET+3,C' '                                                      
         CLC   D4NET,=C'AGG'       NON NETWORK SOURCES                          
         BNE   NOTAGG                                                           
         PACK  DUB,D4NUM+5(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,PNET+3                                                        
NOTAGG   CLC   D4NET(3),=C'SYN'       SYNDICATION                               
         BNE   *+10                                                             
         MVC   PNET(3),D4SYNID     SET SYNDICATION CALL LETTERS                 
         MVC   PNAME,D4NAME                                                     
         MVC   PTITLE,D4TITLE                                                   
         MVC   PTYP,D4ALPHA                                                     
         MVC   PPREM,D4PREM                                                     
         MVC   INTMRKT,=H'221'     SET NETWORK MARKET                           
                                                                                
         CLC   D4DYSWKS,=C'01'                                                  
         BNE   D4R18                                                            
         LAY   R1,PRGAVG           PUT PRG IN BUFFER                            
D4R15    OC    0(4,R1),0(R1)                                                    
         BZ    D4R16               ADD PRG NUMBER TO LIST                       
         CLC   0(2,R1),=X'FFFF'                                                 
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLC   INTSTA(3),0(R1)                                                  
         BNE   *+14                                                             
         CLC   PNUM,4(R1)                                                       
         BE    D4R18               PRG ALREADY IN TABLE                         
         LA    R1,L'PRGAVG(R1)                                                  
         B     D4R15                                                            
*                                                                               
D4R16    MVC   0(4,R1),INTSTA                                                   
         MVC   4(2,R1),PNUM                                                     
         MVI   6(R1),X'90'         ASSUME VARIOUS DAYS                          
         CLC   D4DAYS,=X'1111100'                                               
         BNE   *+8                                                              
         MVI   6(R1),X'00'         M-F                                          
         CLC   D4DAYS,=X'1111111'                                               
         BNE   *+8                                                              
         MVI   6(R1),X'80'         M-S                                          
         LA    R1,L'PRGAVG(R1)                                                  
         CLC   0(2,R1),=X'FFFF'                                                 
*********BE    *+14                *** BAD BRANCH REMOVED BY DEIS ***           
         JE    *+2                 DEIS DOESN'T KNOW WHAT ELSE TO DO            
         XC    0(2,R1),0(R1)       SET NEXT SLOT TO ZERO                        
*                                                                               
D4R18    MVC   NETWK,PNET                                                       
         BRAS  RE,GETNET           GET NETWORK CALL LETTERS INTO INTSTA         
*                                                                               
         PACK  DUB,D4PROJ          PROJECTION (XXX,XXX,XXX)                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AHI   R1,5000             ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,PWK1AUD        REACH                                        
*                                                                               
         PACK  DUB,D4STA                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1STAC       STATION COUNT                                
*                                                                               
         PACK  DUB,D4COV                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1COV        COVERAGE                                     
*                                                                               
         TM    D4AVEHUT+(L'D4AVEHUT-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVEHUT,=16C'0'                                                 
         PACK  DUB,D4AVEHUT                                                     
         CVB   RF,DUB                                                           
         ST    RF,SVAVGHI          SAVE AVG HOMES IMPS                          
*                                                                               
         LA    RE,TYPTAB           POINT RE AT TELECAST TYPE TABLE              
         LA    R0,TYPES            COUNTER                                      
D4R20    CLC   D4SPEC,0(RE)        0=REGULAR   1=SPECIAL                        
         BNE   *+14                                                             
         CLC   D4REPEAT,1(RE)      BLANK=ORIGINAL   Y=REPEAT                    
         BE    D4R30                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,D4R20                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
D4R30    MVC   PWK1DTYP,2(RE)      EXTRACT CORRESPONDING BIT SETTING            
*                                                                               
         CLI   INTSTA+4,C'S'       SYNDICATOR                                   
         BNE   D4R31                                                            
         CLI   D4TTYPE,C'C'                                                     
         BNE   *+8                                                              
         OI    PWK1DTYP,X'18'      ORIG+REPEAT PROGRAMMING                      
         CLI   D4TTYPE,C'M'                                                     
         BNE   *+8                                                              
         OI    PWK1DTYP,X'20'      MULTIPLE PROGRAMS                            
*                                                                               
D4R31    CLI   D4OPT,C'1'          TEST OPTIONAL DATA                           
         BNE   D4R34                                                            
* NOTE - EARLY TAPES OF SEP/87 HAVE ERROR FOR NON-VCR (INTEGRATED)              
         CLI   D4VCR,C' '          TEST VCR                                     
         BE    D4R33                                                            
         CLC   D4DYSWKS,=C'01'     TEST AVERAGES                                
         BE    D4R34                                                            
         CLI   D4MULTI,C' '        TEST MULTI-DAYS                              
         BE    D4R34                                                            
D4R33    OI    PWK1DTYP,X'80'      SET OPTIONAL BIT IN PHTDTYP                  
*                                                                               
D4R34    MVI   PWK1RSF,0                                                        
         CLI   D4BREAK,C'1'        TEST FOR REDUCED STATION                     
         BNE   *+8                                                              
         MVI   PWK1RSF,X'01'       REDUCED STATION INDICATOR                    
*                                                                               
         MVC   INTDPT,PDPT         PDPT IS ALWAYS ZERO                          
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         MVC   INTPNUM,PNUM                                                     
         MVC   INTPTYP,PTYP                                                     
         MVC   INTPNAME,PNAME                                                   
         MVC   INTTITLE,PTITLE                                                  
         MVI   INTSTYP,0           DEFAULT TO REG PROGRAM                       
*                                                                               
         CLI   D4BREAK,C'1'        BREAKOUT?                                    
         BNE   *+8                                                              
         MVI   INTSTYP,C'B'                                                     
         CLI   D4SPEC,C'1'         SPECIAL?                                     
         BNE   *+8                                                              
         MVI   INTSTYP,C'S'                                                     
*                                                                               
D4R39    MVI   INTMTYP,1           METRO A MARKET                               
         MVC   INTBOOK,BOOK1       WEEK VALUES                                  
         MVC   INTIBOOK,IBOOK1                                                  
         MVC   INTAUD,PWK1AUD                                                   
         MVC   INTSTAC,PWK1STAC                                                 
         MVC   INTCOV,PWK1COV                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
*                                                                               
D4R40    LA    RE,D4DAYS           POINT TO DAY FIELDS                          
D4R42    LA    R0,7                COUNTER                                      
         LA    R3,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
*                                                                               
D4R50    CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    D4R60                                                            
         CLI   0(RE),C'1'          TEST FOR PROGRAM RAN                         
         BNE   D4R60                                                            
         OR    RF,R3               UPDATE CUMULATIVE BITS                       
*                                                                               
D4R60    LA    RE,1(RE)                                                         
         SRL   R3,1                NEXT DAY                                     
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
         B     D4R70                                                            
         MVC   BYTE,1(RE)          INTERNAL DAY CODE                            
*                                                                               
D4R70    MVC   PDAY1,BYTE                                                       
         MVC   PDAY1BIT,INTDAYWK                                                
         CLC   D4HH,=C'PP'                                                      
         BNE   *+8                                                              
         MVI   INTSTA+3,C'P'                                                    
         CLC   D4HH,=C'FP'                                                      
         BNE   *+8                                                              
         MVI   INTSTA+3,C'F'                                                    
         CLC   D4HH,=C'LP'                                                      
         BNE   *+8                                                              
         MVI   INTSTA+3,C'L'                                                    
*                                                                               
* THE CODE BELOW WAS REMOVED ON 4/27/2010.                                      
* WE DON'T KNOW WHY THIS CODE IS HERE. IT SKIPS AROUND COMPUTING THE            
* START AND END QUARTER HOUR FOR DAYPART FRINGE. IT ALSO DOESN'T FILL           
* IN THE RECORD TYPE, WHICH CAUSED THE RECORD TO BE DROPPED BY THE              
* OUTPUT PHASE.  IN ADDITION, IT CAUSED PROBLEMS FOR PROGRAMS WITH A            
* DURATION SHORTER THAN THE SPAN OF THE PROGRAM (FOR EXAMPLE, GAPPED            
* PROGRAMS). IF THESE PROGRAMS HAVE A FRINGE PORTION, THE CONVERSION            
* GETS CONFUSED ABOUT THE START AND END QUARTER HOUR AND DIES WHILE             
* COMPUTING THE TP HUTS.                                                        
*                                                                               
* IF ANY PROBLEMS COME UP AFTER REMOVING THIS CODE, YOU CAN                     
* ADD THESE TWO LINES BACK IN.                                                  
**       CLC   D4HH,=C'FP'                                                      
**       BE    D4R73                                                            
         GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
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
         CHI   RF,1                TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         BCTR  R0,0                LESS ONE GIVE END QH                         
         STC   R0,INTEQH                                                        
*                                                                               
*              ADD DURATION IN MINUTES TO START TIME (MILITARY) TO              
*              GET END TIME                                                     
         SR    R0,R0                                                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM2       DURATION IN MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CHI   R0,60               TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SHI   R0,60               SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MHI   R1,100              HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CHI   R1,2400             TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SHI   R1,2400                                                          
         STCM  R1,3,INTETIM        END TIME                                     
*                                                                               
D4R73    CLC   D4NET(3),=C'SYN'    NO INDIV. DAY FOR SYNDS.                     
         BE    *+20                                                             
         CLC   D4DYSWKS,=C'01'     AVERAGES                                     
         BE    D4R77                                                            
         MVC   VARS,ZEROS                                                       
*                                  USE INDIVIDUAL DAY(S) FOR INTVAR             
         LA    RE,D4DAYS                                                        
         LA    RF,SAVVAR           SET UP START TIMES IN VAR SAVE AREA          
         USING INTVARD,RF                                                       
         LA    R7,SAVETIME                                                      
         LA    R0,7                                                             
D4R74    CLI   0(RE),C'1'                                                       
         BNE   D4R76                                                            
         MVC   INTVSQ,INTSQH       START QH                                     
         MVC   INTVEQ,INTEQH       END QH                                       
         PACK  DUB,4(4,R7)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,INTVLDUR       LONG DURATION IN MINUTES                     
         CHI   R1,240                                                           
         BNH   *+8                                                              
         LHI   R1,240                                                           
         STC   R1,INTVSDUR         SHORT DURATION IN MINUTES (MAX 240)          
         MVC   INTVSTIM,INTSTIM    START TIME                                   
         MVC   INTVETIM,INTETIM    END TIME                                     
D4R76    LA    RE,1(RE)                                                         
         LA    RF,INTVARLQ(RF)                                                  
         LA    R7,L'SAVETIME(R7)                                                
         BCT   R0,D4R74                                                         
         DROP  RF                                                               
                                                                                
         CLC   D4NET(3),=C'SYN'    CAN BE VAR FOR SYND.                         
         BNE   D4R80                                                            
*                                                                               
D4R77    CLI   VARIOUS,1           VARIOUS                                      
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
D4R80    LA    R1,CALVPH           SET CALVPH FIELDS                            
         USING CALVPHD,R1                                                       
         MVC   VPHDPT,D4DPT                                                     
         MVC   VPHPCOD,D4ALPHA                                                  
         MVI   VPHSPCL,C' '                                                     
         CLI   D4SPEC,C'1'         1=SPECIAL                                    
         BNE   *+8                                                              
         MVI   VPHSPCL,C'S'                                                     
         MVC   VPHACTD,PDAY1BIT    INTDYBIT NOT YET DEFINED!!!!                 
         CLI   VPHACTD,0                                                        
         BNE   *+8                                                              
         MVI   VPHACTD,X'40'                                                    
         MVI   VPHTELE,1                                                        
         MVC   VPHDUR,PWK2DURM                                                  
         DROP  R1                                                               
*                                                                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         BRAS  RE,FKPROC                                                        
*                                                                               
D4R90    MVC   INTDAYWK,PDAY1                                                   
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/P4RTN                         
*                                                                               
D4RX     MVI   BYPREAD,0           FOR P4REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*H4RTN - PROCESS HALF HOUR DETAIL RECORD                                        
**********************************************************************          
*                                                                               
H4RTN    DS    0H                                                               
         USING H4REC,RC                                                         
H4R20    CLC   H4TOTDUR,ZEROS                                                   
         BE    READ20              BYPASS IF TOT DUR NOT THERE                  
         MVI   NOTAVAL,0                                                        
         CLI   H4REPT,C'4'         WAS THE DATA PROVIDED?                       
         BNE   *+12                YES                                          
         MVI   NOTAVAL,2           NO, BYPASS 1/2HR PUTS                        
         B     READ20                                                           
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN                  
*                                                                               
H4R25    CLC   H4PROJ,ZEROS        ARE TOTAL FIELDS ON THIS RECORD              
         BE    *+22                                                             
         MVC   SVPROJ,H4PROJ       IF YES - SAVE 3 FIELDS                       
         MVC   SVRTG,H4RTG                                                      
         MVC   SVSHR,H4SHR                                                      
*                                                                               
         MVC   H4PROJ,SVPROJ       IF NOT - RESTORE FROM A PREVIOUS H4          
         MVC   H4RTG,SVRTG                                                      
         MVC   H4SHR,SVSHR                                                      
*                                                                               
         MVI   INTRTYP,PRCODEQU    -P-                                          
         MVC   PACK16(10),H4NUM    CONVERT PRG# TO 5 CHAR PWOS                  
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2                                                    
*                                                                               
         PACK  DUB,H4EVSHR(4)      START TIME - MILITARY                        
         CVB   RF,DUB                                                           
         STH   RF,HALF                                                          
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       GET START QHR                     
         NI    INTSQH,X'FE'                                                     
         MVC   STQHR,INTSQH                                                     
         BAS   RE,GTIME            GET AVG DUR FOR HLFHR (SET INTADUR)          
         CLI   HLFADUR,0           WAS PRGM ACTIVE?                             
         BNE   *+12                                                             
         MVI   NOTAVAL,2           BYPASS ASSOCD P-RECDS                        
         B     READ20                                                           
*                                                                               
         MVC   INTADUR,HLFADUR     MAX 30 MINS                                  
         MVC   INTEQH,INTSQH                                                    
         MVI   INTDUR,1            RAN IN AT LEAST 1 QHR                        
         ZIC   RF,INTADUR          AVG DUR IN MINS                              
         SHI   RF,1                -1 MINUTE                                    
         BZ    H4R30                                                            
         STH   RF,DUB              DUB CONTAINS DURATION IN MINUTES             
*                                                                               
         SR    R0,R0               GET MILITARY END TIME                        
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         LH    RF,DUB              DURATION IN MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CHI   R0,60               TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SHI   R0,60               SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MHI   R1,100              HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CHI   R1,2400             TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SHI   R1,2400                                                          
         STCM  R1,3,HALF           END TIME - MILITARY                          
         GOTO1 VHRTOQH,DMCB,HALF,INTEQH                                         
         ZIC   R1,INTEQH                                                        
         ZIC   R0,INTSQH                                                        
         SR    R1,R0                                                            
         BZ    *+8                                                              
         MVI   INTDUR,2            RAN IN 2 QHRS                                
*                                                                               
H4R30    MVC   SAVEINT,INTVALS     SAVE FOR P4RTN                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         BRAS  RE,GKPROC                                                        
         MVI   BYPREAD,0           FOR P4REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*P4RTN - PROCESS PERSONS ESTIMATES RECORD                                       
**********************************************************************          
P4RTN    DS    0H                                                               
         USING P4REC,RC                                                         
*                                                                               
         CLI   PRIMEP,0            IS A PRIME PORTION BEING PROCESSED?          
         BE    P4R05               NO                                           
         CLC   P4HALF,SVD4HH       KEEP P'S IF THEY MATCH DRECD                 
         BE    P4R05               ALSO KEEP ACTUAL 1/2HR RECDS                 
         CLC   P4HALF,ZEROS        BYPASS TOT PRM                               
         BE    READ20                                                           
**P      CLC   P4HALF,=C'FP'    <- IF D-BYPASSED, BYPASS P-RECD TOO             
**P      BE    READ20                                                           
**P      CLC   P4HALF,=C'PP'    <-     "            "                           
**P      BE    READ20                                                           
*                                                                               
P4R05    DS    0H                                                               
         MVI   SVD4,C'0'           RESET D4 RECORD'S SAVE AREA                  
         MVC   SVD4+1(L'SVD4-1),SVD4                                            
         MVI   BYPASSH4,0          RESET H4 RECORD BYPASS SWITCH                
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN/H4RTN            
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         CLI   P4DEMTYP,C'1'       PROGRAM PUTS                                 
         BE    P4R20                                                            
         CLC   P4DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BNE   P4R10                                                            
*                                  DEMOS 001-020                                
         MVI   HAVPP,0             RESET PROGRAM PUT SWITCH                     
         BRAS  RE,AKPROC                                                        
         B     P4R40                                                            
*                                  DEMOS 021-040                                
P4R10    BRAS  RE,BKPROC                                                        
         B     P4RX                                                             
*                                                                               
P4R20    CLC   P4DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BNE   P4R30                                                            
*                                  PUTS 001-020                                 
         MVI   HAVPP,1             SET HAVE PROG. PUTS                          
         BRAS  RE,CKPROC                                                        
         B     P4R40                                                            
*                                  PUTS 021-040                                 
P4R30    BRAS  RE,DKPROC                                                        
         B     P4RX                                                             
         EJECT                                                                  
**********************************************************************          
*P4R40 - BUILD KEY AND RELEASE RECD TO SORT                                     
**********************************************************************          
P4R40    CLI   INTRTYP,PMCODEQU    TEST FOR 'Q' RECORD                          
         BE    P4R70                                                            
*                                                                               
P4R60    LA    R7,INTKEY           BUILD PAV KEY                                
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     -P-                                          
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    P4R64                                                            
         MVC   PRCODE(2),BOOK1     (BOOK FORCES PROPER SORT)                    
         MVI   PRSRC,PRCODEQU      -P-                                          
P4R64    MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRDW+1(1),INTDUR    FORCE HIGHER DURATIONS FIRST                 
         XI    PRDW+1,X'FF'                                                     
*                                                                               
         B     P4RX                                                             
         DROP  R7                                                               
*                                                                               
P4R70    LA    R7,INTKEY           BUILD 'Q' RECORD KEY                         
         USING PMKEY,R7                                                         
         MVI   PMCODE,PMCODEQU     -Q-                                          
         MVI   PMMEDIA,C'N'                                                     
         MVI   PMSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    P4R74                                                            
         MVC   PMCODE(2),BOOK1     (BOOK FORCES PROPER SORT)                    
         MVI   PMSRC,PMCODEQU      -Q-                                          
P4R74    MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
*                                                                               
         LA    RE,QSORTAB          SORT 'Q' RECORDS BY DAY...                   
         LA    R0,QSORTABS         M-S, M-F, VAR, MON...SUN                     
         CLC   INTDAYWK,0(RE)                                                   
         BE    *+14                                                             
         LA    RE,L'QSORTAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   PMRLEN(1),1(RE)     USE FIELD FOLLOWING PMPNUM                   
         DROP  R7                                                               
*                                                                               
P4RX     MVI   BYPREAD,0                                                        
         MVC   1900(4,R2),=F'1'    FORCE LONG SORT RECORD                       
*                                   SO I CAN ADD TP PUTS LATER                  
*                                   AND GAA FIGURES                             
*                                                                               
         CLC   P4DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BE    READ20              FOR 2ND P4REC, SKIP PROCESSING AND           
*                                  READ NEXT REC W/O GOING TO CONTRLLER         
*                                                                               
         CLI   HAVPP,0             HANDLE MISSING PROG. PUTS                    
         BE    P4RX20                                                           
*        BE    *+10                 NTI MUCKS IT UP BY NOT PROVIDING            
*                                   THEM ON A RANDOM BASIS                      
         SPACE 1                                                                
         CLC   P4NET(3),=C'SYN'    NO PROG PUTS FOR SYN                         
         BE    *+12                                                             
         CLI   P4DEMTYP,C'1'       PUT OUT AFTER PROGRAM PUTS                   
         BNE   READ20                                                           
*                                                                               
*        CLC   P4NET(3),=C'SYN'    NO PROG PUTS FOR SYN                         
*        BE    P4RX20                                                           
*                                                                               
         LA    R1,CALVPH           CALL CALVPH                                  
         USING CALVPHD,R1                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS                                                       
         LAY   RE,INTAB                                                         
         ST    RE,VPHINT                                                        
         GOTO1 VCALVPH,CALVPH                                                   
         L     R7,AIREC                                                         
         USING K0000,R7                                                         
         MVC   VHWC18(12),HWCV18                                                
         MVC   RHWC18(12),HWCR18                                                
         MVC   IHWC18(12),HWCI18                                                
         XC    HWCAREA,HWCAREA                                                  
         DROP  R7                                                               
         DROP  R1                                                               
*                                                                               
P4RX20   MVI   INTKEY+29,255                                                    
         CLI   GAASW,C'Y'                                                       
         BNE   EXIT                                                             
         MVI   INTKEY+29,C'G'                                                   
         B     EXIT                                                             
         DROP  RC                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*H5RTN - PROCESS NON-NETWORK HOUSEHOLD USAGE RECORD                             
**********************************************************************          
*                                                                               
H5RTN    DS    0H                                                               
         USING H5REC,RC                                                         
         CLI   HPTGENSW,C'Y'       FILTERING MUST DO HPT TABLE HERE             
         BNE   H5R02                                                            
         MVI   HPTGENSW,C'N'                                                    
*                                                                               
H5R02    CLI   CORRSW,0            TEST CORRECTION SWITCH                       
         BE    H5R10                                                            
         CLI   CORRSW,C'A'         TREAT EVERYTHING?                            
         BE    *+8                 YES, DON'T RESET FLAG                        
         MVI   CORRSW,0            RESET CORRECTION SWITCH                      
         MVC   HDAYTAB(L'HDAYTABS),HDAYTABS RSTORE DAY TABLE                    
         MVC   BOOK1,BOOK1S        RESTORE BOOK1                                
         MVC   IBOOK1,IBOOK1S      RESTORE IBOOK1                               
*                                                                               
H5R10    PACK  DUB,H5EVSHR         INITIALIZE START QUARTER HOUR                
         CVB   RF,DUB                                                           
         SHI   RF,6                START HOUR = 06-29                           
         SLL   RF,2                                                             
         CLC   H5EVSMIN,=C'30'     START MINUTE = 00 OR 30                      
         BNE   *+8                                                              
         LA    RF,2(RF)                                                         
         STC   RF,HUTSQH           FIRST QUARTER HOUR                           
*                                                                               
         LA    RE,HDAYTAB                                                       
         LA    R0,HDAYS                                                         
H5R20    CLC   H5START,2(RE)       TEST START DAY VS. TABLE                     
         BNE   H5R24                                                            
         CLC   H5END,9(RE)         TEST END DAY VS. TABLE                       
         BE    H5R28                                                            
H5R24    LA    RE,L'HDAYTAB(RE)                                                 
         BCT   R0,H5R20                                                         
         DC    H'0'                                                             
H5R28    MVC   HUTDAYWK,1(RE)      INTERNAL DAY CODE                            
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         BRAS  RE,JKPROC                                                        
*                                                                               
         USING KDSCT,R6                                                         
         XC    K0541,K0541         CALCULATE SHOMES USING IHOMES                
         L     R1,K0537            IHOMES                                       
         LTR   R1,R1                                                            
         BZ    H5R50                                                            
         ZIC   RF,HUTDAYWK         RF - DAY OF WEEK (1 THRU 7)                  
         SRL   RF,4                                                             
         LA    RE,HPTLN            DISP FOR ONE WHOLE DAY                       
         STH   RE,HALF                                                          
         MHI   RE,48                                                            
         MR    RE,RE               SLIDE FOR NUMBER OF DAYS                     
         PACK  DUB,H5PHH                                                        
         CVB   RE,DUB              RF - HALF HOUR (1 THRU 48)                   
         BCTR  RE,0                                                             
         MH    RE,HALF             SLIDE TO HALF HOUR                           
         AR    RF,RE                                                            
         L     RE,VPUTBUFF         RE - INDIV DAY HUT TABLE                     
         USING HPTD,RE                                                          
         AR    RE,RF               INDEX TO 1 OF 48 HALF HOUR HUTS              
         OC    HPTAVEI,HPTAVEI                                                  
         BZ    H5R50               (ZERO HUT)                                   
         M     R0,=F'1000'                                                      
         D     R0,HPTAVEI          HUT PROJ                                     
         SR    R0,R0                                                            
         AHI   R1,5                ROUND                                        
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         ST    R1,K0541            SHOMES (XX)                                  
         DROP  R6                                                               
*                                                                               
H5R50    MVI   INTRTYP,PRCODEQU    -P-                                          
         MVC   INTBOOK,BOOK1                                                    
         MVC   INTIBOOK,IBOOK1                                                  
         MVC   INTSTA(3),H5NET                                                  
         MVI   INTSTA+3,C' '                                                    
         CLC   H5NET(3),=C'AGG'                                                 
         BNE   H5NOTAGG                                                         
         PACK  DUB,H5NUM+8(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,INTSTA+3                                                      
H5NOTAGG MVI   INTSTA+4,C'D'                                                    
         MVC   INTDAYWK,HUTDAYWK                                                
         MVC   INTSQH,HUTSQH                                                    
         ZIC   RF,HUTSQH                                                        
         LA    RF,1(RF)                                                         
         STC   RF,INTEQH                                                        
         MVI   INTDUR,2            DURATION IN QTR HOURS                        
         MVI   INTDURM,30          DURATION IN MINUTES                          
         MVC   INTDURM2,=Y(30)     2-BYTE DURATION FIELD                        
         MVC   INTMRKT,=H'221'                                                  
         MVI   INTMTYP,1                                                        
         XC    INTPNTI,INTPNTI                                                  
         B     READ20              READ THE NEXT RECORD                         
         DROP  RE                                                               
         DROP  RC                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
*P5RTN - PROCESS NON-NETWORK PERSONS DEMOGRAPHICS RECORD                        
*********************************************************************           
*                                                                               
P5RTN    DS    0H                                                               
         USING P5REC,RC                                                         
*                                                                               
         CLI   BYPREAD,1           MUST M-F RECORD BE EMITTED                   
         BE    P5R60                                                            
*                                                                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         CLI   P5DEMTYP,C'0'       VALIDITY TEST                                
         BE    *+6                                                              
         DC    H'0'                WHAT IS THIS DATA                            
         CLC   P5DEMGRP,=C'001'                                                 
         BNE   P5R10                                                            
         BRAS  RE,AKPROC           PROCESS REC 1 DEMOS                          
         B     READ20                                                           
*                                                                               
P5R10    BRAS  RE,BKPROC           PROCESS REC 2 DEMOS                          
         LA    R7,INTKEY           SET UP THE RECORD                            
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU                                                  
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,C'A'                                                      
         BNE   *+14                                                             
         MVC   PRCODE(2),BOOK1                                                  
         MVI   PRSRC,PRCODEQU                                                   
*                                                                               
         MVC   NETWK,INTSTA                                                     
         BRAS  RE,GETNET           GET NETWORK CALL LETTERS INTO INTSTA         
*                                                                               
P5R13    MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVI   INTPNAME,C' '                                                    
         MVC   INTPNAME+1(L'INTPNAME-1),INTPNAME                                
         MVI   INTTITLE,C' '                                                    
         MVC   INTTITLE+1(L'INTTITLE-1),INTTITLE                                
         MVC   INTPNAME(3),=C'N/A'                                              
         MVC   PRDW+1(1),INTDUR    HIGHEST DURATION FIRST                       
         XI    PRDW+1,X'FF'                                                     
*                                                                               
*----  DON'T GENERATE M-F AVGS ON DAILY FILE -------------                      
                                                                                
         B     EXIT            WE DON'T GET M-F DATA ON PRIME FILES             
         DROP  R7                                                               
                                                                                
*-----------------------------------------------------                          
*                                                                               
*        NOW TEST FOR MONDAY THRU FRIDAY (6A THRU 7P)                           
*                                                                               
         MVI   BYPREAD,0                                                        
         CLI   FILDYS,X'7C'        M-F (OR M-S) AVAIL ON FILE?                  
         BL    EXIT                                                             
         CLI   INTDAYWK,X'10'      TEST MON THRU FRI                            
         BL    EXIT                                                             
         CLI   INTDAYWK,X'50'                                                   
         BH    EXIT                                                             
         CLC   P5EVSHR,=C'06'      TEST 6A-7P                                   
         BL    EXIT                                                             
         CLC   P5EVSHR,=C'19'                                                   
         BNL   EXIT                                                             
         L     RE,AIREC                                                         
         LHI   R1,1900                                                          
         LAY   RF,SVIREC                                                        
         MOVE  ((RF),(R1)),(RE)    SAVE INTERIM RECORD TO SVIREC                
         MVI   BYPREAD,1           RETURN FOR M-F RECORD                        
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
P5R60    L     RF,AIREC                                                         
         LHI   R1,1900                                                          
         LAY   RE,SVIREC                                                        
         MOVE  ((RF),(R1)),(RE)    RESTORE INTERIM RECORD FROM SVIREC           
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         MVI   INTDAYWK,X'00'      PUT OUT M-F RECORD                           
         ZIC   RF,PRDW                                                          
         SRL   RF,4                SHIFT LEFT NIBBLE TO RIGHT                   
         STC   RF,PRDW             MON CHANGES FROM X'10' TO X'01', ETC         
         MVI   BYPREAD,0                                                        
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* CNVWR- SORT RECORD HOOK                                                       
*        NON-NETWORK M-F ROTATORS EMITTED FROM INDIVIDUAL DAY RECORDS           
*        DUPLICATE HUT RECORDS DROPPED                                          
* ********************************************************************          
*                                                                               
CNVWR    DS    0H                                                               
CNVWR40  L     R2,ASREC            R2 - USES INTERD DSECT                       
         CLI   INTKEY+29,C'G'      HANDLE GAA AUDIENCE FIGURES                  
         BE    CNVWR100                                                         
         CLC   INTSTA,=C'UUUUD'    UNIVERSE RECD                                
         BNE   CNVWR42                                                          
         LA    R1,CALVPH                                                        
         USING CALVPHD,R1                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS                                                       
         DROP  R1                                                               
         GOTO1 VCALUNV,CALVPH    CALCULATE UNIVERSE VPH'S                       
         B     CNVWR120                                                         
*                                                                               
CNVWR42  CLC   INTSTA,=C'ITABD'                                                 
         BE    CNVWR120            INTABS   RECD                                
         CLC   INTSTA,=C'STABD'                                                 
         BE    CNVWR120            SCALED INTABS RECD                           
*                                                                               
         CLC   INTSTA,=C'HUT D'    SKIP TV USAGE RECORDS                        
         BE    CNVWR50                                                          
         CLI   INTSTA+4,C'S'       AND SYNDICATORS ALSO                         
         BE    CNVWR50                                                          
         XC    SUMWGHT,SUMWGHT     INIT FOR TP PUTS                             
         XC    SUMHPT,SUMHPT       RAW IMPS                                     
         LAY   RE,PUTSUM           CALCULATED PERCENTS                          
         XC    0(L'PUTSUM,RE),0(RE)                                             
         CLI   INTDAYWK,X'8F'      THIS PART FOR NON-VAR ONLY                   
         BH    CNVWR45                                                          
         ZIC   RE,INTDAYWK         CONVERT DAY TO NORMAL                        
         SRL   RE,4                                                             
         STC   RE,SUMDAY                                                        
         MVC   SUMSQH,INTSQH                                                    
         MVC   SUMEQH,INTEQH                                                    
*                                                                               
         XC    SUMSTIM,SUMSTIM                                                  
         XC    SUMETIM,SUMETIM                                                  
         CLI   INTRTYP,C'Q'        PROGRAM AVERAGE                              
         BNE   *+16                                                             
         MVC   SUMSTIM,INTSTIM                                                  
         MVC   SUMETIM,INTETIM                                                  
         BAS   RE,QHSUM            SUM THE QUARTER HOURS                        
         B     CNVWR48                                                          
*                                                                               
CNVWR45  LA    R7,INTVAR-L'INTVAR  THIS PART FOR VAR                            
         LA    R9,1                                                             
CNVWR46  SR    R6,R6                                                            
         LA    R6,L'INTVAR(R6)                                                  
         STH   R6,HALF                                                          
         LR    R6,R9               SET THE DAY                                  
         MH    R6,HALF             INDEX                                        
         AR    R6,R7               THEN SLOT                                    
         OC    0(L'INTVAR,R6),0(R6)  ACTIVITY CHECK                             
         BZ    CNVWR47                                                          
         STC   R9,SUMDAY           SET DAY AND TIMES                            
         MVC   SUMSQH,0(R6)                                                     
         MVC   SUMEQH,1(R6)                                                     
*                                                                               
         XC    SUMSTIM,SUMSTIM                                                  
         XC    SUMETIM,SUMETIM                                                  
         CLI   INTRTYP,C'Q'        PROGRAM AVERAGE                              
         BNE   *+16                                                             
         MVC   SUMSTIM,3(R6)                                                    
         MVC   SUMETIM,5(R6)                                                    
*                                                                               
         BAS   RE,QHSUM            ADD UP THE HPT VALUES                        
CNVWR47  LA    R9,1(R9)                                                         
         CHI   R9,8                LOOP UNTIL 7 DAYS ARE DONE                   
         BL    CNVWR46                                                          
*                                                                               
CNVWR48  SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         GOTO1 HPTDIV,DMCB,SUMHPT,SUMHPT,(R6) GET THE AVERAGE                   
         LA    RE,SUMHPT                                                        
         USING HPTD,RE                                                          
         MVC   TPPUT(L'HPTPUT,R2),HPTPUT SEED THE TP RAW PUTS                   
         MVC   TPHUTR(4,R2),HPTAVER      SEED HH TP HUT                         
         DROP  RE                                                               
         LAY   RF,PUTSUM                                                        
         MVC   PUTSAVE,0(RF)                                                    
         BAS   RE,PUTDIV                                                        
         MVC   PUTSTRT(L'PUTSUM,R2),PUTSAVE SEED THE TP PUTS                    
         L     RE,ASREC                                                         
         L     RF,AWREC                                                         
         LH    R1,0(RE)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         B     CNVWR50                                                          
*                                                                               
CNVWR50  CLI   IPGAASW,C'Y'        MERGE IN GAA FIGURES IF THERE                
         BNE   CNVWR51                                                          
         LAY   RE,SVGAIMP          SRC                                          
         LA    RF,GAIMPS(R2)       DEST                                         
         LA    R1,GAALN1+GAALN2                                                 
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
CNVWR51  MVI   IPGAASW,C'N'        RESET GAA SWITCH                             
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         CLI   INTRTYP,PRCODEQU    -P-                                          
         BNE   EXIT                                                             
*                                                                               
         CLC   PRSTAT,=C'HUT D'    HUT OR                                       
         BNE   CNV10               AGG (NON-NETWORK)                            
*                                                                               
         B     EXIT                EMIT RECORD                                  
*                                                                               
CNVWR100 MVI   IPGAASW,C'Y'        TURN ON FOR MERGE                            
         MVI   BYPSORT,X'80'       DON'T RELEASE TO OP PHASE                    
         LA    RE,AAIMPS(R2)       SOURCE                                       
         LAY   RF,SVGAIMP          DEST  - SAVE THE GAA FIGURES                 
         LA    R1,GAALN1           LEN                                          
         MOVE  ((RF),(R1)),(RE)    MOVE DEMS FROM TOP UNTIL YHOMES              
         LA    RE,AAIMP2(R2)       DON'T MOVE HOMES--MOVE AFTER IT              
         LAY   RF,SVGAIMP          DEST                                         
         LA    RF,GAALN1(RF)                                                    
         LA    R1,GAALN2           LEN                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
CNVWR120 DS    0H                  UNIVERSE RECD JUST GETS RELEASED             
         USING KDSCT,R2                                                         
         MVC   K0537,SVHHUNIV      SAVE UNIV HOMES IMPRESSIONS                  
         B     EXIT                RELEASE UNIVERSE RECD                        
         DROP  R2                                                               
*                                                                               
* GENERATE M-F RECORDS FOR AGG (NON-NETWORK) SOURCES                            
         USING INTERD,R2           SORT RECORD                                  
CNV10    CLI   PRDW,X'05'          TEST X'01'(MON) THRU X'05'(FRI)              
         BH    EXIT                                                             
         CLI   PRDW,X'01'                                                       
         BL    EXIT                                                             
         BH    CNV20               TUE THRU FRI - ACCUMULATE TO SVSREC          
*                                                                               
         LAY   RF,SVSREC           MON - CLEAR SVSREC                           
         XCEF  (RF),2000                                                        
         XC    SVNOACCS,SVNOACCS                                                
         XC    SVRECLN,SVRECLN                                                  
         L     RE,ASREC            SAVE SORT RCD IN SVSREC                      
         LHI   R1,1900                                                          
         LAY   RF,SVSREC                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
CNV20    DS    0H                  MON THRU FRI                                 
         LA    R1,INTACCS-INTRECLN R1 - DISPLACEMENT TO INTACCS                 
         LAY   R3,SVSREC                                                        
         AR    R3,R1               R3 - INTACCS (SVSREC)                        
         LA    RE,INTACCS          RE - INTACCS (SREC)                          
         LH    RF,INTRECLN                                                      
         SR    RF,R1                                                            
         BP    CNV30               OK - WE HAVE DATA                            
*                                                                               
*                                  (NO INTACCS - ZERO DATA)                     
         CLI   PRDW,X'05'          TEST FOR FRIDAY                              
         BNE   CNV60               PURGE MON, TUE, WED & THU                    
         L     RF,SVNOACCS         FRI                                          
         LTR   RF,RF               WAS THERE ANY DATA AT ALL                    
         BNZ   CNV70               YES - AVERAGE THE DATA AND EMIT RCD          
         B     CNV90               NO - SIMPLY EMIT RECORD                      
*                                                                               
CNV30    LA    RF,3(RF)            (IN CASE OF TRUNCATED FULLWORD)              
         SRL   RF,2                RF - NUMBER OF INTACCS FULLWORDS             
         CLC   SVRECLN,INTRECLN                                                 
         BNL   CNV36                                                            
         MVC   SVRECLN,INTRECLN    SAVE MAX. RECORD LENGTH                      
         ST    RF,SVNOACCS         SAVE MAX. NO. OF INTACCS FULLWORDS           
*                                                                               
CNV36    CLI   PRDW,X'01'          TEST FOR MONDAY                              
         BE    CNV60               PURGE MON RECORD (DATA SAVED)                
*                                                                               
CNV40    L     R1,0(R3)            ACCUMULATE FIVE DAYS' DATA                   
         A     R1,0(RE)                                                         
         ST    R1,0(R3)                                                         
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)                                                         
         BCT   RF,CNV40                                                         
*                                                                               
         CLI   PRDW,X'05'          TEST FOR FRIDAY                              
         BE    CNV70                                                            
CNV60    MVI   INTRTYP,0           PURGE RECORD (MON, TUE, WED & THU)           
         B     EXIT                                                             
*                                                                               
CNV70    LAY   RE,SVSREC           FRIDAY - EMIT RECORD                         
         LHI   R1,1900                                                          
         L     RF,ASREC                                                         
         MOVE  ((RF),(R1)),(RE)    MOVE SVSREC TO SREC                          
         OC    SVRECLN,SVRECLN                                                  
         BZ    CNV90               (ZERO DATA)                                  
         LH    RF,SVRECLN          USE MAXIMUM RECORD LENGTH                    
         LA    RF,7(RF)            ADD 4 FOR SHOMES (& 3 FOR SAFETY)            
         STH   RF,INTRECLN         SET NEW MAXIMUM RECORD LENGTH                
*                                                                               
         LA    RE,INTACCS          RE - INTACCS (SREC)                          
         L     RF,SVNOACCS         RF - MAX. NO. OF INTACCS FULLWORDS           
CNV80    L     R1,0(RE)            AVERAGE FIVE DAYS' DATA                      
         LTR   R1,R1                                                            
         BZ    CNV84                                                            
         SR    R0,R0                                                            
         AHI   R1,2                ROUND                                        
         D     R0,=F'5'            DIVIDE BY FIVE DAYS                          
         ST    R1,0(RE)                                                         
CNV84    LA    RE,4(RE)                                                         
         BCT   RF,CNV80                                                         
         DROP  R2                                                               
*                                                                               
         SPACE 2                                                                
         USING KDSCT,R2                                                         
CNV88    XC    K0533,K0533         CALC RHOMES USING IHOMES & UNIV              
         L     R1,K0537            IHOMES                                       
         LTR   R1,R1                                                            
         BZ    CNV90                                                            
         M     R0,=F'10'                                                        
         D     R0,SVHHUNIV         (USE UNIVERSE SAVED IN H1RTN)                
         SR    R0,R0                                                            
         AHI   R1,5                ROUND                                        
         D     R0,=F'10'                                                        
         ST    R1,K0533            RHOMES (XX.X)                                
*                                                                               
         SPACE 2                                                                
         XC    K0541,K0541         CALC SHOMES USING IHOMES & HUT               
         L     R1,K0537            IHOMES                                       
         ZIC   RF,PRSTIM           6A=0, 630A=2  -  6P=48, 630P=50              
         SRL   RF,1                QHR --> HHR                                  
         LA    RE,HPTLN            INDEX INTO BUFFER                            
         MR    RE,RE                                                            
         LR    RE,RF               SHIFT PRODUCT                                
         A     RE,VPUTBUFF                                                      
         USING HPTD,RE                                                          
*                                                                               
         OC    HPTAVEI,HPTAVEI                                                  
         BZ    CNV90               (ZERO HUT)                                   
         M     R0,=F'1000'                                                      
         D     R0,HPTAVEI          HUT PROJ                                     
         SR    R0,R0                                                            
         AHI   R1,5                ROUND                                        
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         ST    R1,K0541            SHOMES (XX.0)                                
         MVC   K1045,HPTAVER       SET PUT                                      
         DROP  R2                                                               
*                                                                               
CNV90    MVI   PRDW,X'00'          SET DAY/WEEK TO M-F                          
         B     EXIT                EMIT RECORD                                  
         DROP  RE                                                               
         DROP  RC                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
*DAYRTN- ROUTINE TO SET UP DAYS (M-F, M-S, S-S, INDIV DAYS)                     
*********************************************************************           
*                                                                               
DAYRTN   NTR1                                                                   
         USING MIREC,RC                                                         
         CLC   MISTART,MIEND       TEST SINGLE DAY                              
         BNE   DAY60               NO, PROCESS MULTIPLE DAYS                    
*                                                                               
DAY05    DS    0H                  PROCESS SINGLE DAY IN HDAYTAB                
         LA    R1,X'10'            MON                                          
         LA    R0,7                                                             
         LA    RE,MIREC+(D4DAYS-D4REC)                                          
         CLC   MISEQ,=C'00'        RTN CALLED FROM REPT DESC RECD?              
         BNE   DAY20               NO, THEN WE HAVE DAY BITS                    
         LA    R1,NETDAYTB         YES, COMPARE DAY NAME TO TABLE               
DAY10    CLC   FILTYP,2(R1)        REPT DSC REC,CONVERT DAY TO BIT CODE         
         BE    DAY15                                                            
         LA    R1,L'NETDAYTB(R1)                                                
         BCT   R0,DAY10                                                         
         DC    H'0'                DAY NOT IN TABLE                             
DAY15    MVC   BYTE(1),1(R1)                                                    
         B     DAY25                                                            
*                                                                               
DAY20    CLI   0(RE),C'1'          DETERMINE DAY FROM D4REC DAYBITS             
         BE    DAY23                                                            
         AHI   R1,16               INCREMENT TO NEXT DAY                        
         LA    RE,1(RE)                                                         
         BCT   R0,DAY20                                                         
         DC    H'0'                                                             
DAY23    STC   R1,BYTE             SAVE DAY CODE IN BYTE                        
*                                                                               
DAY25    DS    0H                  SLOT DATE INTO HDAYTAB                       
         LA    R1,HDAYTAB          DAY SAVED IN BYTE                            
         LA    R0,HDAYS                                                         
DAY30    CLC   BYTE,1(R1)          BYTE=DAY CODE                                
         BE    DAY50                                                            
         LA    R1,L'HDAYTAB(R1)                                                 
         BCT   R0,DAY30                                                         
         DC    H'0'                                                             
*                                                                               
DAY50    MVC   2(7,R1),MISTART     2(7,R1) = START DAY                          
         MVC   9(7,R1),MIEND       9(7,R1) = END DAY                            
         B     EXIT                                                             
*                                                                               
DAY60    DS    0H                  MULTIPLE DAYS ON RECORD                      
         XC    HDMON+2(14),HDMON+2 INIT DAYS IN TABLE                           
         XC    HDTUE+2(14),HDTUE+2                                              
         XC    HDWED+2(14),HDWED+2                                              
         XC    HDTHU+2(14),HDTHU+2                                              
         XC    HDFRI+2(14),HDFRI+2                                              
         XC    HDSAT+2(14),HDSAT+2                                              
         XC    HDSUN+2(14),HDSUN+2                                              
         XC    HDM#F+2(14),HDM#F+2                                              
         XC    HDM#S+2(14),HDM#S+2                                              
*                                                                               
         CLC   FILTYP,=C'S-S'      SAT-SUN FILE?                                
         BNE   DAY63                                                            
         MVC   HDSAT+2(7),MISTART  SET UP SAT FROM START                        
         MVC   HDSAT+9(7),MISTART                                               
         MVC   HDSUN+2(7),MIEND    SET UP SUN FROM END                          
         MVC   HDSUN+9(7),MIEND                                                 
         B     EXIT                                                             
*                                                                               
DAY63    MVC   HDMON+2(7),MISTART  SET UP MONDAY FROM START                     
         MVC   HDMON+9(7),MISTART  FOR ALL OTHER FILES                          
         CLC   FILTYP,=C'EAR'      EARLY FRINGE IS M-F DATA                     
         BE    *+14                                                             
         CLC   FILTYP,=C'M-F'                                                   
         BNE   DAY65                                                            
         MVC   HDFRI+2(7),MIEND    SET UP FRIDAY FROM END FOR M-F FILE          
         MVC   HDFRI+9(7),MIEND                                                 
         B     DAY68                                                            
*                                                                               
DAY65    MVC   HDSUN+2(7),MIEND    SET SUN AS END FOR EAR/LATE FRINGE           
         MVC   HDSUN+9(7),MIEND                                                 
*                                                                               
DAY68    LA    R3,HDMON+3          FILL IN MISSING DATES                        
         LA    R7,HDTUE+3                                                       
         LA    R2,5                FILL IN TUE-SAT DATES                        
         CLC   FILTYP,=C'EAR'      EARLY FRINGE IS M-F DATA                     
         BE    *+14                                                             
         CLC   FILTYP,=C'M-F'      ONLY FILL TUE-THU DATES                      
         BNE   *+8                                                              
         LA    R2,3                                                             
DAY70    GOTO1 VADDAY,DMCB,(R3),(X'20',(R7)),1                                  
         BCTR  R7,0                                                             
         MVC   0(1,R7),HDMON+2                                                  
         MVC   7(7,R7),0(R7)       FOR EQUAL START AND END                      
         LA    R7,1(R7)                                                         
         LA    R3,16(R3)           DO REST OF DAYS                              
         LA    R7,16(R7)                                                        
         BCT   R2,DAY70                                                         
*                                                                               
DAY80    MVC   HDM#F+2(7),MISTART  SET UP MON-FRI                               
         MVC   HDM#F+9(7),HDFRI+9                                               
         CLC   FILTYP,=C'EAR'      EARLY FRINGE IS M-F DATA                     
         BE    *+10                                                             
         CLC   FILTYP,=C'M-F'                                                   
         BE    *+16                DON'T SET UP M-S SLOT IN TABLE               
         MVC   HDM#S+2(7),MISTART  SET UP MON-SUN                               
         MVC   HDM#S+9(7),MIEND                                                 
         B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
* ********************************************************************          
* PDATA- ROUTINE TO FIND ACTUAL RUN TIME AND AVERAGE DURATION                   
*              P1 =  A(SAVETIME)                                                
*              P2 =  A(DAY BITS FOR WEEK)                                       
* ********************************************************************          
PDATA    NTR1                                                                   
         L     R6,0(R1)            ADDRESS OF SAVETIME                          
         L     RE,4(R1)            ADDRESS OF DAY BITS                          
         MVC   BYTE,0(RE)          DAY BITS                                     
         XC    FULL,FULL           CLEAR DAY COUNT FOR DURATION                 
         LA    R3,X'40'            START AT MONDAY                              
         SR    R7,R7               CLEAR DURATION ACCUM                         
         XC    TIMTAB(7*L'TIMTAB),TIMTAB                                        
         XC    FULL1,FULL1         CLEAR TABLE ENTRY COUNT                      
         LA    R0,7                COUNTER                                      
*                                                                               
PDATA2   STC   R0,LOOP                                                          
         EX    R3,*+8              TEST IF PROGRAM RAN ON DAY                   
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    PDATA4              NO                                           
         OC    0(8,R6),0(R6)       BYPASS IF NO TIMES                           
         BZ    PDATA4                                                           
*                                                                               
         PACK  DUB,0(2,R6)         START TIME                                   
         CVB   R1,DUB                                                           
         MHI   R1,100              HOURS X 100                                  
         PACK  DUB,2(2,R6)         START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CHI   R1,2400             TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SHI   R1,2400             SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         PACK  DUB,4(4,R6)         DURATION IN MINUTES                          
         CVB   R0,DUB                                                           
         AR    R7,R0               UPDATE TOTAL DURATION                        
         L     RE,FULL                                                          
         LA    RE,1(RE)            UPDATE DAY COUNT                             
         ST    RE,FULL                                                          
         BAS   RE,UPTIME           UPDATE START TIME TABLE                      
*                                                                               
PDATA4   SRL   R3,1                NEXT DAY                                     
         LA    R6,L'SAVETIME(R6)   NEXT DAY'S TIME DATA                         
         SR    R0,R0                                                            
         IC    R0,LOOP                                                          
         BCT   R0,PDATA2                                                        
*                                                                               
PDATA6   SR    RE,RE               CALCULATE AVERAGE DURATION                   
         LR    RF,R7               DURATION                                     
         SLDA  RE,1                                                             
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         STCM  RF,3,PWK2DURM       RETURN AVE (LONG) DURATION                   
*                                                                               
         CHI   RF,240              4-HOUR MAXIMUM DURATION SUPPORTED            
         BNH   *+8                                                              
         LHI   RF,240                                                           
         STC   RF,PWK1DURM         RETURN AVE DURATION                          
*                                  SORT TIMES IN DESCENDING ORDER               
         L     R0,FULL1            NUMBER OF ENTRIES IN TABLE                   
         GOTO1 VXSORT,DMCB,(X'FF',TIMTAB),(R0),3,1,2                            
         MVC   PWK1STIM,TIMTAB     FIRST OR MOST FREQUENT TIME                  
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
* ********************************************************************          
* UPTIME - ROUTINE TO UPDATE RUN TIME TABLE                                     
*              HALF CONTAINS TIME                                               
*              FULL1 CONTAINS TABLE ENTRY COUNT                                 
* ********************************************************************          
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
* ********************************************************************          
* PUTDIV - ?                                                                    
* ********************************************************************          
PUTDIV   NTR1                                                                   
         SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         LR    R0,R6                                                            
         LA    R1,L'PUTSAVE                                                     
         SRL   R1,2                DIV BY 4                                     
         LA    RE,PUTSAVE                                                       
PUTDIV1  L     R6,0(RE)                                                         
         SR    R7,R7                                                            
         SRDA  R6,31                                                            
*                                                                               
         LTR   R0,R0               IF ZERO, BYPASS DIVIDE & CLR R7              
         BNZ   *+10                                                             
         SR    R7,R7                                                            
         B     PUTDIV4                                                          
*                                                                               
         DR    R6,R0                                                            
         AHI   R7,1                                                             
         SRA   R7,1                                                             
*                                                                               
PUTDIV4  DS    0H                                                               
         ST    R7,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R1,PUTDIV1                                                       
         XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* HPTDAY -ROUTINE TO PERFORM A ROUTINE FOR AN ENTIRE DAYS HPT DATA              
*         CALLING TEMPLATE:                                                     
*           GOTO1 HPTDAY   DMCB,A(ROUTINE),A(SOURCE),A(DEST),WEIGHT             
* ********************************************************************          
*                                                                               
         USING HPTD,RE                                                          
HPTDAY   NTR1                                                                   
         L     RF,0(R1)            ROUTINE TO PERFORM                           
         L     R3,4(R1)            SOURCE                                       
         L     R6,8(R1)            DESTINATION                                  
         L     R7,12(R1)           WEIGHT                                       
         L     RE,16(R1)           SEQUENTIAL HALF HOURS TO DO                  
HPTDAY1  ST    RE,ADDR             SAVE AWAY TEMPORARILY                        
         GOTO1 (RF),DMCB,(R3),(R6),(R7)                                         
         LA    R3,HPTLN(R3)        NEXT SLOT                                    
         LA    R6,HPTLN(R6)                                                     
         L     RE,ADDR                                                          
         BCT   RE,HPTDAY1                                                       
         B     EXIT                                                             
         SPACE 2                                                                
HPTMAD   NTR1                      WEIGHTED ADD ROUTINE                         
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
HPTMAD1  L     R6,0(RE)            GET A VALUE FROM SOURCE                      
         LR    R7,R0               SWAP IN WEIGHT                               
         MR    R6,R6               DO WEIGHTING                                 
         A     R7,0(RF)            ADD IN PREVIOUS VALUES                       
         ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,HPTMAD1          LOOP UNTIL DONE                              
         B     EXIT                                                             
         SPACE 2                                                                
HPTDIV   NTR1                      UNWEIGHT ACCUMS ROUTINE                      
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
HPTDIV1  L     R6,0(RE)            GET A VALUE                                  
         SR    R7,R7               GET READY FOR DIV                            
         LTR   R0,R0                                                            
         BZ    *+10                PROTECT AGAINST DIVIDE BY ZERO               
         SRDA  R6,31                                                            
         DR    R6,R0               UNWEIGHT                                     
*                                                                               
         AHI   R7,1                AND ROUND                                    
         SRA   R7,1                                                             
         ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,HPTDIV1          LOOP UNTIL DONE                              
         B     EXIT                                                             
         SPACE 2                                                                
SETHPT   NTR1                    INPUT  A(DAY),A(QTR HOUR)                      
         LM    RE,RF,0(R1)       OUT    A(SLOT)                                 
         ZIC   R6,0(RE)                                                         
         LR    RE,R6                                                            
         ZIC   R6,0(RF)                                                         
         LR    RF,R6                                                            
         LR    R7,RE                                                            
         LA    R6,HPTLN                                                         
         MHI   R6,48                                                            
         MR    R6,R6               ADJUST FOR ONE DAY                           
         LR    RE,R7               AND SAVE RESULT                              
         LA    R7,HPTLN                                                         
         LR    R6,RF                                                            
         SRL   R6,1                CONVERT TO HALF HOUR                         
         MR    R6,R6               ADJUST FOR TIME                              
         AR    R7,RE               NOW FOR BOTH                                 
         A     R7,VPUTBUFF                                                      
         ST    R7,0(R1)                                                         
         B     EXIT                                                             
         SPACE 2                                                                
HPTMFC   NTR1                                                                   
         LA    R9,1                                                             
         LA    RE,=F'0'            SEED THE QTR HRS FIRST                       
         LA    RF,=AL1(48)                                                      
         STM   RE,RF,DUB                                                        
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         LA    R1,10                                                            
         LA    R9,48                                                            
         L     RE,DUB                                                           
         STC   R9,HPTQHR                                                        
         LA    RE,HPTLN(RE)                                                     
         LA    R9,2(R9)                                                         
         BCT   R1,*-12                                                          
         LA    R9,1                                                             
*                                                                               
HPTMFC2  LA    RE,=F'0'            CALC MISSING M-F DEMOS                       
         LA    RF,=AL1(48)                                                      
         STM   RE,RF,DUB                                                        
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         LA    RE,HPTMAD                                                        
         ST    RE,HPTPARA                                                       
         MVC   HPTPARA+16(4),=F'10'   NO. OF SEQ. HALF HOURS                    
         MVC   HPTPARA+8(4),DUB    OUTPUT SLOT                                  
         MVC   HPTPARA+12(4),=F'1' WEIGHT                                       
         LA    RF,=AL1(48)         SET FOR START QH                             
         ST    RF,DUB+4                                                         
         LA    RF,FULL                                                          
         STC   R9,FULL             MAD THE INDIV. DAYS FOR M-F                  
         ST    RF,DUB                                                           
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         MVC   HPTPARA+4(4),DUB    INPUT SLOTS                                  
         LA    R1,HPTPARA                                                       
         BAS   RE,HPTDAY                                                        
         LA    R9,1(R9)                                                         
         CHI   R9,5                                                             
         BH    HPTMFC3                                                          
         B     HPTMFC2                                                          
         SPACE 2                                                                
HPTMFC3  LA    RE,=F'0'                                                         
         LA    RF,=AL1(48)                                                      
         STM   RE,RF,DUB                                                        
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         MVC   HPTPARA+4(4),DUB                                                 
         MVC   HPTPARA+8(4),DUB                                                 
         MVC   HPTPARA+12(4),=F'5' 5 DAYS                                       
         MVC   HPTPARA+16(4),=F'10'                                             
         LA    RE,HPTDIV                                                        
         ST    RE,HPTPARA                                                       
         LA    R1,HPTPARA                                                       
         BAS   RE,HPTDAY                                                        
         B     EXIT                                                             
         SPACE 2                                                                
HPTMSC   NTR1                      CALC M-S AVERAGES                            
         LA    R1,HPTPARA                                                       
         LA    R6,HPTMAD           ROUTINE (MAD)                                
         ST    R6,HPTPARA                                                       
         MVC   HPTPARA+16(4),=F'48'   NO. OF HALF HOURS                         
         MVC   HPTPARA+4(4),VPUTBUFF SOURCE (M-F)                               
         LA    R7,HPTLN            DEST    (M-S)                                
         MHI   R7,48                                                            
         MHI   R7,8                                                             
         A     R7,VPUTBUFF                                                      
         ST    R7,HPTPARA+8                                                     
*                                                                               
         SR    R6,R6               SEED M-S DAY AND TIMES                       
         LR    RE,R7               ESTABLISH ADDRESSABILITY                     
HPTMSC2  MVI   HPTDY,X'80'         M-S                                          
         STC   R6,HPTQHR                                                        
         LA    R6,2(R6)                                                         
         LA    RE,HPTLN(RE)                                                     
         CHI   R6,96                                                            
         BNH   HPTMSC2                                                          
         DROP  R7                                                               
*                                                                               
         LA    R9,7                ADD UP 7 DAYS                                
HPTMSC3  LA    R6,1                        (WEIGHT = 1)                         
         ST    R6,HPTPARA+12                                                    
         LR    R6,R9               SET FOR CURRENT DAY                          
         LA    R7,HPTLN                                                         
         MHI   R7,48               INDEX FOR NUMBER OF SLOTS IN DAY             
         MR    R6,R6               INDEX FOR THE DAY                            
         A     R7,VPUTBUFF         POINT TO DAY START                           
         ST    R7,HPTPARA+4        AND SAVE                                     
         BAS   RE,HPTDAY           ADD EM UP                                    
         BCT   R9,HPTMSC3          DO FOR ALL DAYS                              
*                                                                               
         LA    R6,HPTDIV                                                        
         ST    R6,HPTPARA                                                       
         MVC   HPTPARA+4(4),HPTPARA+8 SOURCE = DEST                             
         LA    R6,7                (WEIGHT = 7)                                 
         ST    R6,HPTPARA+12                                                    
         BAS   RE,HPTDAY           (DIV - UNWEIGHT M-S )                        
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
* ********************************************************************          
* QHSUM -REQUIRES SUMSQH,SUMEQH,SUMDAY TO BE SET                                
* ********************************************************************          
QHSUM    NTR1                                                                   
         MVI   QHSFRST,1                                                        
         CLC   SUMSTIM,SUMETIM     END LESS THAN START                          
         BNH   QHSUM1                                                           
         SR    RE,RE               ADJUST FOR AFTER MIDNIGHT                    
         ICM   RE,3,SUMETIM                                                     
         LA    RE,2400(RE)                                                      
         STCM  RE,3,SUMETIM                                                     
*                                                                               
QHSUM1   LA    R6,HPTLN                                                         
         STH   R6,HALF                                                          
         MHI   R6,48               48 HALF HOURS IN A DAY                       
         ZIC   R7,SUMDAY                                                        
         MR    R6,R6                                                            
         A     R7,VPUTBUFF                                                      
         OC    SUMSTIM,SUMSTIM     PROGS HAVE START AND END TIMES               
         BZ    QHSUM4                                                           
         CLI   QHSFRST,1           HANDLE ONE MINUTE PROGRAMS                   
         BE    *+14                                                             
         CLC   SUMSTIM,SUMETIM                                                  
         BNL   QHSUM8                                                           
         MVI   QHSFRST,0                                                        
         GOTO1 VHRTOQH,DMCB,SUMSTIM,SUMSQH                                      
         SR    RF,RF                                                            
         ICM   RF,3,SUMSTIM        GET THE START                                
         LA    RF,1(RF)            INC. BY 1 MINUTE                             
         SR    RE,RE                                                            
         D     RE,=F'100'          GET HOURS AND MINUTES                        
         CHI   RE,60               TAKE CARE OF 60 SEC BASE                     
         BNE   *+8                                                              
         LHI   RE,100              SET TO BUMP THE HOUR                         
         MHI   RF,100                                                           
         AR    RE,RF                                                            
         STCM  RE,3,SUMSTIM                                                     
         B     QHSUM5                                                           
*                                                                               
QHSUM4   CLC   SUMSQH,SUMEQH                                                    
         BH    QHSUM8                                                           
QHSUM5   ZIC   R6,SUMSQH                                                        
         SRL   R6,1                CONVERT TO HALF                              
         MH    R6,HALF             SET TO CORRECT DISP                          
         AR    R7,R6                                                            
         SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         LA    R6,1(R6)                                                         
         STCM  R6,3,SUMWGHT                                                     
         ZIC   R6,SUMSQH                                                        
         LA    R6,1(R6)                                                         
         STC   R6,SUMSQH                                                        
         GOTO1 HPTMAD,DMCB,(R7),SUMHPT,1                                        
*                                                                               
         LAY   R6,UNIVSAVE                                                      
         LA    R7,HPTPUT-HPTFLAG(R7) POINT TO RAW PUTS                          
         GOTO1 VCALPUT,DMCB,0,0,(R6),(R7),PUTSAVE                               
         LA    R1,PUTEND-PUTSTRT                                                
         SRL   R1,2                DIVIDE BY 4                                  
         LAY   RE,PUTSAVE                                                       
         LAY   RF,PUTSUM                                                        
QHSUM6   L     R7,0(RE)                                                         
         A     R7,0(RF)                                                         
         ST    R7,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,QHSUM6                                                        
         B     QHSUM1                                                           
*                                                                               
QHSUM8   B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* SET2BUF - USING D4 RECD FIELDS, SET FIELDS IN DBUFFER FOR ACTIVE DAYS         
* *********************************************************************         
*                                                                               
SETDBUF  NTR1                                                                   
         USING D4REC,RC                                                         
         OC    DBDAYS,D4DAYS       SAVE TOTAL DAYS PRGM RAN                     
         PACK  DUB,D4EVSHR         START TIME                                   
         CVB   R1,DUB                                                           
         MHI   R1,100              HOURS X 100                                  
         PACK  DUB,D4EVSMIN        START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CHI   R1,2400             TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SHI   R1,2400             SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         GOTO1 VHRTOQH,DMCB,HALF,STQHR         START QH                         
*                                                                               
         LAY   R7,DBUFF                                                         
         LA    R3,SAVETIME         SET IN SAVETIME AS WELL                      
         LA    R1,D4DAYS           PT TO DAY BITS                               
         LA    R0,7                LOOP THRU 7 DAYS                             
STDB10   CLI   0(R1),C'1'          IS BIT ON                                    
         BNE   STDB15                                                           
         BAS   RE,DTIME            SET 1/2HRS IN DBUFF AND SET SAVETIME         
*                                                                               
STDB15   LA    R1,1(R1)            NEXT DAY ON TAPE RECD                        
         LA    R7,DBUFDAYQ(R7)     NEXT DAY IN BUFFER                           
         LA    R3,L'SAVETIME(R3)                                                
         BCT   R0,STDB10                                                        
         OC    DBDAYS,D4DAYS       SAVE TOTAL DAYS PRGM RAN                     
*                                                                               
SETDBX   B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*DTIME - FILLS DBUFF WITH 1/2HRS AND DURTN PRG AIRED FOR DAY                    
*        INPUT:  R7     ->  DAY IN DBUFF                                        
*                R3     ->  DAY IN SAVETIME                                     
*                STSQH   =  START TIME                                          
*                D4DUR   =  DURATION                                            
*                D4EVSMIN =  START MINUTE                                       
**********************************************************************          
DTIME    NTR1                                                                   
         USING D4REC,RC                                                         
         MVC   DMCB(1),STQHR                                                    
         NI    DMCB,X'FE'          FORCE TO 1/2HR                               
         XC    DMCB+4(4),DMCB+4    ACCUM TOT DUR FOR DAY                        
*                                                                               
         OC    0(4,R7),0(R7)       WAS A START TIME SET FOR DAY?                
         BNZ   *+10                                                             
         MVC   0(4,R7),D4EVSHR     SAVE START TIME                              
         MVC   0(4,R3),0(R7)       PUT START TIME IN DBUFF IN SAVETIME          
         SR    R1,R1                                                            
         ICM   R1,3,4(R7)          PICK UP TOT DUR SO FAR FOR DAY               
         PACK  DUB,D4DUR                                                        
         CVB   R2,DUB              R2=DURATION OF PRGM                          
         AR    R1,R2               ADD THIS DUR TO TOT FOR DAY                  
         STCM  R1,3,4(R7)          SAVE TOT DUR                                 
         EDIT  (2,4(R7)),(4,4(R3))                                              
         OC    4(4,R3),ZEROS                                                    
         LA    R7,6(R7)            PT TO HLF HRS LIST                           
*                                                                               
DTIM05   CLI   0(R7),0                                                          
         BE    DTIM10                                                           
         CLC   DMCB(1),0(R7)       SAME QHR CODE?                               
         BE    DTIM10                                                           
         LA    R7,2(R7)            NEXT 1/2HR SLOT                              
         B     DTIM05                                                           
*                                                                               
DTIM10   MVC   0(1,R7),DMCB                                                     
         LA    R1,30               1ST HLF HR MAX = '30'                        
         CLC   D4EVSMIN,=C'30'     STARTS AFTER END OF 1ST HLFHR                
         BL    *+8                                                              
         LA    R1,60               MAX MIN = 60 FOR 2ND 1/2HR                   
         PACK  DUB,D4EVSMIN                                                     
         CVB   R0,DUB                                                           
         SR    R1,R0               CALC # MIN AVAIL TO RUN IN 1/2 HR            
*                                                                               
DTIM15   CR    R1,R2               AT LEAST 30 MIN LEFT IN TOTDUR               
         BNH   *+6                                                              
         LR    R1,R2               ELSE USE REMAINING DURN                      
         ZIC   R0,1(R7)                                                         
         AR    R0,R1               R1 + ANY EXISTING DUR                        
         STC   R0,1(R7)                                                         
         SR    R2,R1               REDUCE TOT DUR BY AMT USED IN 1/2HR          
         BZ    DTIMEX                       ACCTD FOR ALL MINS OF DURN          
*                                                                               
DTIM20   ZIC   R3,0(R7)                                                         
         LA    R3,2(R3)            BUMP TO NEXT 1/2HR                           
         LA    R7,2(R7)            NEXT 1/2HR SLOT FOR DAY                      
         ZIC   R0,0(R7)            SEE IF MATCH ON 1/2HR                        
         BNZ   *+12                IF EMPTY SLOT, SAVE 1/2HR CODE               
         STC   R3,0(R7)                                                         
         B     *+12                                                             
         CR    R0,R3               BUCKET NOT EMPTY --SAME 1/2 HR?              
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,30               30 MIN MAX IN 1/2HR                          
         B     DTIM15                                                           
*                                                                               
DTIMEX   B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*GTIME - GET ACCUMULATED DATA FROM DBUFF                                        
*        INPUT - STQHR  = 1/2 HOUR CODE                                         
*                DBDAYS = DAYS PROGRAM RAN                                      
*                                                                               
*        OUTPUT- HLFADUR= AVG DUR FOR 1/2HR                                     
*                HLFDAYS= DAYS ACTIVE FOR THIS 1/2HR                            
*                HTOTDUR= TOTAL DUR FOR HLFHR ACROSS ACTIVE DAYS                
**********************************************************************          
GTIME    NTR1                                                                   
         XC    HTOTDUR,HTOTDUR                                                  
         XC    HLFADUR,HLFADUR                                                  
         MVC   HLFDAYS,ZEROS                                                    
         XC    DMCB(8),DMCB                                                     
         LA    R0,7                                                             
         LAY   R7,DBUFF                                                         
         LA    R3,DBDAYS                                                        
         LA    R6,HLFDAYS                                                       
*                                                                               
GTIME5   CLI   0(R3),C'1'          WAS THIS DAY ACTIVE                          
         BE    GTIME7                                                           
         MVI   0(R6),C'0'                                                       
GTIME6   LA    R7,DBUFDAYQ(R7)                                                  
         LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,GTIME5                                                        
         B     GTIM30                                                           
*                                                                               
GTIME7   DS    0H                                                               
         ST    R7,DMCB             SAVE DAY PTR TO DBUFF ENTY                   
         LA    R7,6(R7)            PT TO 1/2HR CODE LIST                        
         SR    R1,R1                                                            
*                                                                               
GTIM10   CLI   0(R7),0                                                          
         BE    GTIM20              DONE WITH DAY                                
         CLC   STQHR,0(R7)                                                      
         BE    *+12                                                             
         LA    R7,2(R7)                                                         
         B     GTIM10                                                           
         MVI   0(R6),C'1'          SET ACTIVE DAY                               
         ZIC   R1,1(R7)                                                         
         A     R1,HTOTDUR          SAVE HLF HOUR DUR                            
         ST    R1,HTOTDUR                                                       
         L     R1,DMCB+4           BUMP ACTIVE DAYS                             
         LA    R1,1(R1)                                                         
         ST    R1,DMCB+4                                                        
GTIM20   L     R7,DMCB                                                          
         B     GTIME6                                                           
*                                                                               
GTIM30   DS    0H                                                               
         OC    DMCB+4(4),DMCB+4    NUMBER OF ACTIVE DAYS                        
         BZ    GTIMEX                                                           
         SR    RE,RE               CALCULATE AVERAGE DURATION                   
         L     RF,HTOTDUR          DURATION                                     
         SLDA  RE,1                                                             
         D     RE,DMCB+4                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         STC   RF,HLFADUR          RETURN AVG DURATION                          
*                                                                               
GTIMEX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* *******************************************************************           
* IOERROR - READ ERROR - SET ERROR FLAG AND CLOSE FILE                          
* *******************************************************************           
*                                                                               
IOERROR  DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'***READ ERROR ON INPUT***'                      
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'82'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
********************************************************************            
* WLR -  WRONG LENGTH RECORD ON INPUT                                           
********************************************************************            
*                                                                               
WLR      DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'WRONG LENGTH RECORD ON INPUT'                   
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'82'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
* EOF ON INPUT FILE                                                             
*                                                                               
MORET    DS    0H                                                               
ENDJOB   CLOSE (IN1,REWIND)                                                     
         OC    SVB0SD2,SVB0SD2                                                  
         BZ    ENDJOB2                                                          
         CLI   PASSDAY2,1                                                       
         BE    *+12                                                             
         MVI   PASSDAY2,1                                                       
         B     READ01                                                           
*                                                                               
ENDJOB2  MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*         SET DAY TO PREVIOUS DAY                                               
P2DAYS  DS     0CL6                                                             
         DC    CL6'MONSUN'                                                      
         DC    CL6'TUEMON'                                                      
         DC    CL6'WEDTUE'                                                      
         DC    CL6'THUWED'                                                      
         DC    CL6'FRITHU'                                                      
         DC    CL6'SATFRI'                                                      
         DC    CL6'SUNSAT'                                                      
         DC    X'FF'                                                            
*                                                                               
* CONVERTS DESCRIPTOR RECD TO A DAY CODE SETTING FOR FILE                       
FILBIT   DS    0CL13                                                            
         DC    CL12'MON EVENING ',X'40'                                         
         DC    CL12'TUE EVENING ',X'20'                                         
         DC    CL12'WED EVENING ',X'10'                                         
         DC    CL12'THU EVENING ',X'08'                                         
         DC    CL12'FRI EVENING ',X'04'                                         
         DC    CL12'SAT EVENING ',X'02'                                         
         DC    CL12'SUN EVENING ',X'01'                                         
         DC    CL12'S-S DAYTIME ',X'03'                                         
         DC    CL12'M-F DAYTIME ',X'7C'                                         
         DC    CL12'EARLY FRINGE',X'7C'                                         
         DC    CL12'LATE FRINGE ',X'7F'                                         
         DC    X'FF'                                                            
*                                                                               
* TABLE OF DAY BIT SETTINGS AND DAY CODES                                       
*                                                                               
NETDAYTB DS    0CL5                                                             
         DC    X'4010',C'MON'      MON                                          
         DC    X'2020',C'TUE'      TUE                                          
         DC    X'1030',C'WED'      WED                                          
         DC    X'0840',C'THU'      THU                                          
         DC    X'0450',C'FRI'      FRI                                          
         DC    X'0260',C'SAT'      SAT                                          
         DC    X'0170',C'SUN'      SUN                                          
         DC    X'7C00',C'M-F'      M-F                                          
         DC    X'7F80',C'M-S'      M-SUN                                        
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                          
*                                                                               
         SPACE 3                                                                
* HUT RECORD DAY CODE TABLE                                                     
*              BYTE 0 = NTI DAY CODE                                            
*              BYTE 1 = INTERNAL DAY CODE                                       
*              BYTES 3-9 = START DAY (CYYMMDD)                                  
*              BYTES 10-16 = END DAY (CYYMMDD)                                  
*                                                                               
HDAYTAB  DS    0CL16                                                            
HDM#S    DC    C'0',X'80',14X'00'  M-SUN                                        
HDMON    DC    C'2',X'10',14X'00'  MON                                          
HDTUE    DC    C'3',X'20',14X'00'  TUE                                          
HDWED    DC    C'4',X'30',14X'00'  WED                                          
HDTHU    DC    C'5',X'40',14X'00'  THU                                          
HDFRI    DC    C'6',X'50',14X'00'  FRI                                          
HDSAT    DC    C'7',X'60',14X'00'  SAT                                          
HDSUN    DC    C'8',X'70',14X'00'  SUN                                          
HDM#F    DC    C'9',X'00',14X'00'  M-FRI                                        
HDAYS    EQU   (*-HDAYTAB)/L'HDAYTAB                                            
*                                                                               
HDAYTABS DS    CL(L'HDAYTAB*HDAYS) SAVE AREA                                    
         SPACE 3                                                                
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
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
***********************************************************************         
* SYNDICATORS THAT GET STORED WITH DIFFERENT CALL LETTERS THAN HOW              
* THEY COME ON THE MIT TAPES.                                                   
***********************************************************************         
SYNDNETT DC    CL4'B/O ',CL4'BLOR'       BLAIR ENT./ORBIS                       
         DC    CL4'ESPN',CL4'ESPN'       ESPN SPORTS NETWORK                    
         DC    CL4'20  ',CL4'20F '       20TH CENTURY FOX TV                    
         DC    X'FF'                                                            
***********************************************************************         
* BROADCAST NETS THAT GET STORED WITH DIFFERENT CALL LETTERS THAN HOW           
* THEY COME ON THE MIT TAPES.                                                   
***********************************************************************         
BRDDNETT DC    CL4'UPN ',CL4'PAR '                                              
         DC    CL4'ION ',CL4'PAX '                                              
         DC    CL4'UMA ',CL4'TF  '                                              
         DC    CL4'MMX ',CL4'MFX '                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
* GENERAL AREAS                                                                 
*                                                                               
INTABADR DS    A                   INTAB TABLE ADDRESS                          
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
ASLOT    DS    A                                                                
SVHHUNIV DS    F                   HOUSEHOLD UNIVERSE FROM H1 RECORD            
HPTPARA  DS    6F                                                               
HPTNOW   DS    F                                                                
HPTCNT   DC    H'0'                                                             
HPTGENSW DC    C'Y'                                                             
HPTFRST  DC    X'01'                                                            
LOOP     DS    X                                                                
ADDR     DS    A                                                                
*                                                                               
RELOFRST DC    X'01'                                                            
BYPREAD  DC    X'00'                                                            
BYPASSH4 DC    X'00'               BYPASS ALL BUT 1ST H4 RECORD                 
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES                   
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
HUTSW    DC    X'00'               HUT RECORD GENERATION SWITCH                 
MTWTFSW  DC    X'00'               HUT RECORD (M-T-W-T-F USED)                  
PREVSTIM DC    X'FF'               HUT RECORD PREVIOUS START QTR HR             
*                                                                               
GAASW    DC    C'N'                GROSS AVG AUD SWITCH                         
HAVPP    DC    X'00'               HAVE PROGRAM PUTS                            
STQHR    DS    X                                                                
QHSFRST  DS    C                                                                
QHRSW    DS    C                                                                
BYTE     DS    C                                                                
SWITCH   DS    C                                                                
CNVWRSW  DS    C                                                                
VARIOUS  DS    X                   1=VARIOUS                                    
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
PACK8    DC    PL8'00'                                                          
PACK16   DC    PL16'00'                                                         
*                                                                               
KEYSRC   DS    C                                                                
SVB0SD   DS    CL7                 SAVE START DATE                              
SVB0SD2  DS    CL7                 SAVE START DATE2                             
PASSDAY2 DC    X'00'                                                            
SVDA     DS    F                                                                
SVSTATUS DS    C                                                                
SVKEY    DC    XL24'00'                                                         
PAVKEY   DC    XL24'00'                                                         
*                                                                               
ZEROS    DC    16C'0'              ZEROS - USE FOR A VARIETY OF THINGS          
*                                                                               
SVD4PRG  DC    CL5'0000'           SAVE LAST D4 PRG NUMBER PROCESED             
SVD4HH   DC    CL2'  '             SAVE HALF HOUR FLD (WHEN=PP,FP,LP)           
PRIMEP   DC    X'00'                                                            
SAVETIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR             
VARSTIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR             
SAVVAR   DS    7XL(INTVARLQ)       SAVE VAR INFO FROM INDIVIDUAL DAYS           
SAVVARLQ EQU   *-SAVVAR                                                         
NETWK    DS    CL4                                                              
*                                                                               
UFREC    DS    XL12                8-CHAR DEMO NAME, 4-BYTE DEMO NUMBER         
*                                                                               
DBDAYS   DS    CL7                 DAYS ACTIVE IN DBUFF                         
*--------                                                                       
HLFDAYS  DS    CL7                 DAYS THIS 1/2HR WAS ACTIVE                   
HLFADUR  DS    X                   AVERAGE 1/2HR DUR                            
HTOTDUR  DS    F                   TOTAL DUR FOR 1/2HR ACROSS DAYS              
*--------                                                                       
*                                                                               
         DS    0F                                                               
CALVPH   DS    CL(VPHINT-VPHDEMS+4) CALCULATE VPH'S                             
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD                          
SAVEPNUM DS    CL10                SAVE PROGRAM NUMBER                          
SVD4KEY  DS    CL115               NTI KEY OF RREC                              
*                                                                               
TIMTAB   DS    7CL3                                                             
*                                                                               
         EJECT                                                                  
* REPORT DESCRIPTOR SAVE AREA (RESET FOR CORRECTION RECORDS)                    
*                                                                               
BOOK1    DS    XL2                 KEY BOOK                                     
IBOOK1   DS    XL2                 INTERNAL BOOK                                
*                                                                               
BOOK1S   DS    XL2                 KEY BOOK (SAVE AREA)                         
IBOOK1S  DS    XL2                 INTERNAL BOOK (SAVE AREA)                    
FILTYP   DS    CL3                 3-CHAR FILE TYPE (B0REPORT+12)               
FILDYS   DS    X                   CONVERTED TO A DAY BIT CODE                  
TMPTYP   DS    CL3                 FOR PROCESSING                               
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
PVALS    DS    0CL87                                                            
PNUM     DS    XL2                 PROGRAM NUMBER                               
PNTINUM  DS    XL5                 NTI ORIG PROG NUMBER                         
PNET     DS    CL4                 NETWORK                                      
PDPT     DS    C                   NSI DAYPART (ALWAYS ZERO)                    
PDPT2    DS    CL2                 NTI DAYPART                                  
PTYP     DS    CL2                 PROGRAM TYPE                                 
PPREM    DS    C                   PREMIERE CODE                                
PNAME    DS    CL25                PROGRAM NAME                                 
PTITLE   DS    CL32                EPISODE TITLE                                
*                                                                               
PDAY1    DS    X                   DAY CODE                                     
PDAY1BIT DS    X                   DAY BITS                                     
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
SVD4     DS    0CL83               SAVE D4 RCD'S POSITIONS 305-387              
*        THESE FIELDS ARE NOT REPORTED ON ALL D4 RECORDS                        
S4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT                  
         DS    CL5        310-314  TOT PROGRAM HEADEND COUNT                    
S4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT               
         DS    CL2        317-318  CA PROGRAM COVERAGE                          
         DS    CL9        319-327  (FILLER)                                     
S4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES              
S4TOTDUR DS    CL6        331-336  TOTAL DURATION                               
S4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)          
S4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)                
S4REPORT DS    CL1        349-349  REPORTABILITY IND. 0=REPORTABLE              
         DS    CL3        350-352  CA HH RTG                                    
         DS    CL1        353      CA REPORTABILITY                             
S4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)              
S4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                           
         DS    CL9        365-373  CA AUD PROJ                                  
         DS    CL2        374-375  CA HH SHR                                    
S4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)           
S4RTG    DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)                 
         DS    CL13       388-400  (FILLER)                                     
*                                                                               
*        THESE FIELDS ARE NOT REPORTED ON ALL H4 RECORDS                        
SVPROJ   DS    CL9         92-100  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)           
SVRTG    DS    CL3        101-103  HALF HOUR AUDIENCE RATING (XX.X)             
SVSHR    DS    CL2        114-115  HALF HOUR PROGRAM SHARE (XX)                 
*                                                                               
SVRECLN  DS    F                   SAVE INTERIM RECORD'S LENGTH                 
SVNOACCS DS    F                   SAVE NUMBER OF INTACCS FULLWORDS             
*                                                                               
*                                                                               
SVD4FP   DS    CL83                SAVE H4 FIELDS INSTEAD                       
         EJECT                                                                  
* FILE DCB AND BUFFER AREA                                                      
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
*                                                                               
HPTADDR  DS    F                                                                
HPTSAVLN DC    A(HPTLN*48*9)                                                    
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
HWCAREA  DS    0CL36               SAVE HH WITH CHILD DATA                      
HWCV18   DS    F                                                                
HWCV12   DS    F                                                                
HWCV06   DS    F                                                                
HWCR18   DS    F                                                                
HWCR12   DS    F                                                                
HWCR06   DS    F                                                                
HWCI18   DS    F                                                                
HWCI12   DS    F                                                                
HWCI06   DS    F                                                                
*                                                                               
*                                                                               
IPGAASW  DS    C                                                                
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
PUTSAVE  DS    CL(PUTEND-PUTSTRT)  SAVE AREA FOR CALCULATED PUTS                
PUTSUM   DS    CL(PUTEND-PUTSTRT)  SUM AREA FOR CALCULATED PUTS                 
UNIVSAVE DS    50F                 SAVE AREA FOR PER. UNIVERSES                 
*                                                                               
*-------                                                                        
*DBUFF - CLEARED WHEN D-RECD W/TOT DUR PRESENT.                                 
*        HOLDS START TIME OF PRGM ON DAY, TOTAL DUR,                            
*        HHR (QHR CODE) WITH DURATION RAN IN THAT HLF HR                        
*--------                                                                       
DBUFF    DS    0C                  7-DAYS                                       
DBUFMON  DS    CL4                 START HOUR/MIN                               
         DS    CL2                 TOTAL DUR FOR DAY                            
         DS    48CL2               HLFHR CODE/DUR IN HLFHR                      
DBUFDAYQ EQU   *-DBUFMON           DISP TO NEXT DAY IN BUFFER                   
         DS    6CL(DBUFDAYQ)       6 MORE DAYS                                  
DBUFFQ   EQU   *-DBUFF             L'DBUFFER                                    
*                                                                               
         EJECT                                                                  
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
*                                                                               
         DS    0F                                                               
SINTAB   DS    0XL156              SCALED INTAB COUNTS                          
         DS    XL3,XL1,38F         MON                                          
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
SINTABS  EQU   (*-SINTAB)/L'SINTAB                                              
         DC    XL3'00'                                                          
*                                                                               
         EJECT                                                                  
*        THE NEXT 1500 BYTES USED FOR EITHER THE SORT OR INTERIM RECORD         
*                                                                               
SVSREC   EQU   *                   SAVE SORT RECORD                             
SVIREC   DS    2000C               SAVE INTERIM RECORD                          
SVGAIMP  DS    444C                                                             
         SPACE 1                                                                
*                            NET(4),PRG#(2),DAY(1) E.G. 7C,7F,90                
PRGAVG   DS    0XL7                PROG#'S HAVING ASSOCD AVG RECD               
         DC    1000X'00'           PROG#'S HAVING ASSOCD AVG RECD               
         DC    X'FFFF'                                                          
         EJECT                                                                  
                                                                                
*********************************************************************           
* ROUTINE WILL FILL IN INTSTA WITH THE STATION CALL LETTERS.        *           
* ON ENTRY, NETWK HAS THE 4-CHAR NETWORK CODE.                      *           
*********************************************************************           
GETNET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING INTERD,R2                                                        
*                                                                               
GETNT30  CLC   =C'AGG',NETWK       FOR AGGREGATES/AFFILIATES,                   
         BNE   GETNT50                                                          
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),NETWK+3                                                
         ICM   RF,15,ACOMFACS      GET TABLE OF BROADCAST AGGREGATES            
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEBAGG                                                 
         ICM   RE,15,DMCB          A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING NEBAGNMD,RE                                                      
GETNT35  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN AGGREGATE/VIEWNG SOURCE #            
         CLC   HALF,NEBAGNUM                                                    
         BE    GETNT40                                                          
         AR    RE,RF                                                            
         B     GETNT35                                                          
GETNT40  MVC   INTSTA(3),NEBAGNET                                               
         MVC   INTSTA+3(2),=C' D'                                               
         B     GETNETX                                                          
         DROP  RE                                                               
*                                                                               
GETNT50  LAY   RE,BRDDNETT         CHECK SPECIAL BROADCAST STATIONS             
GETNT55  CLI   0(RE),X'FF'                                                      
         BE    GETNT70                                                          
         CLC   NETWK,0(RE)                                                      
         BE    GETNT60                                                          
         LA    RE,8(RE)                                                         
         B     GETNT55                                                          
GETNT60  MVC   INTSTA(4),4(RE)     MOVE IN MODIFIED CALL LETTERS                
         MVI   INTSTA+4,C'D'                                                    
         B     GETNETX                                                          
*                                                                               
GETNT70  ICM   RF,15,ACOMFACS      GET TABLE OF BROADCAST NETWORKS              
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEBROD                                                 
         ICM   RE,15,DMCB          A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING NEBRDNMD,RE                                                      
GETNT75  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN NETWORK                              
         MVC   FULL(3),NEBRDNET    NETWORK                                      
         MVI   FULL+3,C' '         PADDED WITH SPACES                           
         CLC   NETWK,FULL                                                       
         BE    GETNT80                                                          
         AR    RE,RF                                                            
         B     GETNT75                                                          
GETNT80  MVC   INTSTA(4),NETWK                                                  
         MVI   INTSTA+4,C'D'                                                    
         B     GETNETX                                                          
         DROP  RE                                                               
*                                                                               
GETNETX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD RECORDS USED TO SEND UNIVERSE INFORMATION TO THE PROGRAM THAT           
* WILL WRITE DEMO FORMULA  UNIVERSE RECORDS TO THE CONTROL FILE.                
***********************************************************************         
UFORMS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PASSDAY2,1              ONLY CREATE FORMULAS ON 1ST PASS         
         BE    UFORMSXX                                                         
*                                                                               
         LA    R3,UDEMOS                                                        
         OPEN  (UFILE,OUTPUT)                                                   
*                                                                               
         L     R7,AIREC                THE FIRST RECORD IN UFILE                
*                                      HOLDS ONLY THE BOOK                      
         XC    UFREC,UFREC                                                      
         MVC   UFREC(2),INTBOOK-INTERD(R7)                                      
         PUT   UFILE,UFREC                                                      
*                                                                               
UFORMS10 CLI   0(R3),X'FF'                                                      
         BE    UFORMSX                                                          
         MVC   UFREC(8),0(R3)                                                   
         SR    RE,RE                                                            
         ICM   RE,3,8(R3)                                                       
         AR    RE,R7                                                            
         MVC   UFREC+8(4),0(RE)                                                 
         PUT   UFILE,UFREC                                                      
         LA    R3,10(R3)                                                        
         B     UFORMS10                                                         
*                                                                               
UFORMSX  CLOSE UFILE                                                            
UFORMSXX XIT1                                                                   
         LTORG                                                                  
*                                                                               
UFILE    DCB   DDNAME=UFILE,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=12,                                               X        
               MACRF=PM                                                         
*                                                                               
***********************************************************************         
* TABLE OF UNIVERSE DEMOS.                                                      
* 8-CHAR DEMO NAME, 2-BYTE DISPLACEMENT TO DEMO VALUE ON IREC                   
***********************************************************************         
UDEMOS   DS    0X                                                               
         DC    CL8'G2-5    ',AL2(K0193-K0000)                                   
         DC    CL8'G6-8    ',AL2(K0197-K0000)                                   
         DC    CL8'G9-11   ',AL2(K0201-K0000)                                   
         DC    CL8'W12-14  ',AL2(K0205-K0000)                                   
         DC    CL8'W15-17  ',AL2(K0209-K0000)                                   
         DC    CL8'W18-20  ',AL2(K0213-K0000)                                   
         DC    CL8'W21-24  ',AL2(K0217-K0000)                                   
         DC    CL8'W25-29  ',AL2(K0221-K0000)                                   
         DC    CL8'W30-34  ',AL2(K0225-K0000)                                   
         DC    CL8'W35-39  ',AL2(K0229-K0000)                                   
         DC    CL8'W40-44  ',AL2(K0233-K0000)                                   
         DC    CL8'W45-49  ',AL2(K0237-K0000)                                   
         DC    CL8'W50-54  ',AL2(K0241-K0000)                                   
         DC    CL8'W55-64  ',AL2(K0245-K0000)                                   
         DC    CL8'W65+    ',AL2(K0249-K0000)                                   
         DC    CL8'B2-5    ',AL2(K0253-K0000)                                   
         DC    CL8'B6-8    ',AL2(K0257-K0000)                                   
         DC    CL8'B9-11   ',AL2(K0261-K0000)                                   
         DC    CL8'M12-14  ',AL2(K0265-K0000)                                   
         DC    CL8'M15-17  ',AL2(K0269-K0000)                                   
         DC    CL8'M18-20  ',AL2(K0273-K0000)                                   
         DC    CL8'M21-24  ',AL2(K0277-K0000)                                   
         DC    CL8'M25-29  ',AL2(K0281-K0000)                                   
         DC    CL8'M30-34  ',AL2(K0285-K0000)                                   
         DC    CL8'M35-39  ',AL2(K0289-K0000)                                   
         DC    CL8'M40-44  ',AL2(K0293-K0000)                                   
         DC    CL8'M45-49  ',AL2(K0297-K0000)                                   
         DC    CL8'M50-54  ',AL2(K0301-K0000)                                   
         DC    CL8'M55-64  ',AL2(K0305-K0000)                                   
         DC    CL8'M65+    ',AL2(K0309-K0000)                                   
         DC    CL8'WW18-20 ',AL2(K0317-K0000)                                   
         DC    CL8'WW21-24 ',AL2(K0321-K0000)                                   
         DC    CL8'WW25-34 ',AL2(K0325-K0000)                                   
         DC    CL8'WW35-44 ',AL2(K0329-K0000)                                   
         DC    CL8'WW45-49 ',AL2(K0333-K0000)                                   
         DC    CL8'WW50-54 ',AL2(K0337-K0000)                                   
         DC    CL8'WW55+   ',AL2(K0341-K0000)                                   
         DC    CL8'HOMES   ',AL2(K0345-K0000)                                   
         DC    X'FF'                                                            
         DROP  RB,RA,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
* DSECT TO COVER NIELSEN'S MEDIA INFORMATION TAPE (1ST 60 POSITIONS)            
*                                                                               
MIREC    DSECT                                                                  
MISEQ    DS    CL2          1-2    RECORD SEQUENCE CODE                         
MISAMPLE DS    CL1          3      SAMPLE INDICATOR                             
*                                    BLANK = NATIONAL PEOPLE METER              
*                                    H     = NATIONAL HISPANIC                  
MICORDTE DS    CL7          4-10   START OF CORRECTION INTERVAL                 
*                                    CYYMMDD                                    
MIORIG   DS    CL1         11      0=ORIGINAL 1=CORRECTION 2=DELETION           
MICORWHY DS    CL3         12-14   CORRECTION REASON                            
*                                   BLANK = N/A                                 
*                                   100=NEW 101=VIEWING CHANGE                  
*                                   102=DESCRIPTIVE DATA CHANGE                 
*                                   103=INFORMATIONAL ONLY CHANGE               
*                                   200=PROPRIETARY DATA CHANGE                 
MINET    DS    CL6         15-20   NETWORK (ABC, CBS, NBC, ETC.)                
*                                  OR - DATA TYPE CODE (TVU, INT, ETC.)         
MINUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE                
MITRACK  DS    CL3         31-33   TRACKAGE ID (CABLE ONLY)                     
MIFEED   DS    CL1         34      FEED PATTERN                                 
*                                   " "=NTI D=DUAL L=LIVE H=HISPANIC            
MIBREAK  DS    CL1         35      0=NOT A BREAKOUT...1=BREAKOUT                
MISPEC   DS    CL1         36      0=NOT A SPECIAL....1=SPECIAL                 
*                                  2=RECURRING SPECIAL                          
MIESTYPE DS    CL1         37      AUDIENCE ESTIMATE TYPE                       
*                                  1=AVG AUD  2=GAA 3=GSA(SYND W/SPOT)          
MIDELVRY DS    CL2         38-39   DELIVERY OPTIONS                             
*                                   "  "  = NOT USED                            
*                                   01-20 = AIRS ACROSS MULTIPLE DPTS           
*                                   21-40 = DELIVERY VEHICLE                    
*                                   41-40 = IN-HOME/OUT-OF-HOME                 
MISYNORG DS    CL1         40      SYNDICATORS ONLY - ORIG/REPEAT/COMB          
MISYNPRO DS    CL1         41      SYNDICATORS ONLY - PROGRAM SPOTS             
MIDYSWKS DS    CL2         42-43   NUMBER OF DAYS/WEEKS                         
MISTART  DS    CL7         44-50   CYYMMDD                                      
MIEND    DS    CL7         51-57   CYYMMDD                                      
MITELNUM DS    CL10        58-67   TELECAST NUMBER                              
MICOMNUM DS    CL3         68-70   COMPONENT NUMBER                             
MICOVSAM DS    CL6         71-76   COVERAGE SAMPLE ID                           
MICOVCAL DS    CL1         77      COVERAGE CALCULATION IND                     
*                                   BLANK = TOTAL US                            
*                                   1     = HOMES ABLE TO VIEW                  
*                                   2     = TOTAL CABLE UNIVERSE                
*                                   3     = OTHER                               
MIMKTBRK DS    CL3         78-80   MARKET BREAK IDENTIFIER                      
MITPHH   DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER           
MIQHID   DS    CL2         83-84   QUARTER HOUR ID                              
MITYPE   DS    CL1         85      RECORD TYPE - B,C,D,E,F,G,H,M,P              
*                                                OR BLANK                       
MIEVSHR  DS    CL2         86-87   EVENT START HOUR                             
MIEVSMIN DS    CL2         88-89   EVENT START MINUTE                           
MIEVSSEC DS    CL2         90-91   EVENT START SECOND                           
MIDEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
MIDEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE                      
MIDEMAVI DS    CL1         96      DEMO AVERAGE IND                             
MIVWTYP  DS    CL1         97      VIEWING TYPE(SEE VWTYPT FOR OPTIONS)         
*IVCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR           
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK           
MINA     DS    CL1         98      NOT AVAILABLE FLAG - BLANK=OK                
*                                    1=NOT AVAILABLE                            
MIRCTR   DS    CL1         99      RECORD CTR - 0=UNIQUE RECORD IN 1-58         
*                                    1-9=MULTIPLE OCCURRENCES                   
MIBUCTYP DS    CL1        100      BUCKET TYPE                                  
MIPPPV   DS    CL1        101      POCKETPIECE VS PRELIMINARY IND               
MIVARRSN DS    CL4        102-105  VARIANCE REASON                              
*                                                                               
         DS    CL9        106-114  (FILLER - ALWAYS BLANK)                      
*                                                                               
         EJECT                                                                  
* DSECT TO COVER REPORT DESCRIPTOR RECORD                                       
*                                                                               
B0REC    DSECT                                                                  
B0SEQ    DS    CL2          1-2    C'00' - REPORT DESCRIPTOR RECORD             
         DS    CL8          3-10                                                
B0ORIG   DS    CL1         11      0=ORIGINAL 1=CORRECTION 2=DELETION           
         DS    CL30        12-41                                                
B0DYSWKS DS    CL2         42-43   NUM DAYS/WEEKS=01                            
B0START  DS    CL7         44-50   CYYMMDD                                      
B0END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL57        58-114                                               
B0REPORT DS    CL25        115-139 REPORT/FILE ID - "POCKETPIECE"               
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER HOUSEHOLD UNIVERSE ESTIMATE RECORD                             
*                                                                               
H1REC    DSECT                                                                  
H1SEQ    DS    CL2          1-2    C'01' - UNIVERSE ESTIMATE RECORD             
H1SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK                     
         DS    CL7          4-10                                                
H1CORR   DS    CL1         11      CORRECTION FLAG                              
         DS    CL3         12-14                                                
H1DTC    DS    CL6         15-20   DATA TYPE CODE = 'UES   '                    
         DS    CL23        21-43                                                
H1START  DS    CL7         44-50   CYYMMDD (START OF SEASON)                    
H1END    DS    CL7         51-57   CYYMMDD (END OF SEASON)                      
         DS    CL20        58-77                                                
H1MKTBRK DS    CL3         78-80                                                
         DS    CL4         81-84                                                
H1TYPE   DS    CL1         85      C'H' - HOUSEHOLD DATA                        
         DS    CL94        86-179                                               
H1HHUNIV DS    CL9        180-188  HH UNIVERSE ESTIMATE (000,XXX,XX0)           
*                                    EXPRESSED AS THOUSANDS                     
         SPACE 2                                                                
* DSECT TO COVER PERSONS UNIVERSE ESTIMATE RECORD                               
*                                                                               
P1REC    DSECT                                                                  
P1SEQ    DS    CL2          1-1    C'01' - UNIVERSE ESTIMATE RECORD             
P1SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK                     
         DS    CL7          4-10                                                
P1CORR   DS    CL1         11      CORRECTION FLAG                              
         DS    CL3         12-14                                                
P1DTC    DS    CL6         15-20   DATA TYPE CODE = 'UES   '                    
         DS    CL23        21-43                                                
P1START  DS    CL7         44-50   CYYMMDD (START OF SEASON)                    
P1END    DS    CL7         51-57   CYYMMDD (END OF SEASON)                      
         DS    CL20        58-77                                                
P1MKTBRK DS    CL3         78-80                                                
         DS    CL4         81-84                                                
P1TYPE   DS    CL1         85      C'P' - PERSONS DATA                          
         DS    CL30        86-115                                               
P1DATA   DS    0CL9       116      DEMO GROUP CATAGORIES                        
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER HOUSEHOLD SAMPLE COUNTS RECORD                                 
*                                                                               
H2REC    DSECT                                                                  
H2SEQ    DS    CL2          1-2    C'02' - SAMPLE COUNTS RECORD                 
H2SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK                     
         DS    CL7          4-10                                                
H2CORR   DS    CL1         11      CORRECTION FLAG                              
         DS    CL3         12-14                                                
H2DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '                    
         DS    CL21        21-42                                                
H2DYSWKS DS    CL2         42-43                                                
H2START  DS    CL7         44-50   CYYMMDD                                      
H2END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL10        58-67                                                
H2SCALED DS    C           68      SCALED INTABS INDICATOR                      
         DS    CL9         69-77                                                
H2MKTBRK DS    CL3         78-80                                                
         DS    CL4         81-84                                                
H2TYPE   DS    CL1         85      C'H' - PERSONS DATA                          
         DS    CL33        86-118                                               
H2DAYS   DS    0CL7       (119-125)  DAYS OF WEEK INDICATOR                     
H2MON    DS    CL1        119        MONDAY      (1 OR 0)                       
H2TUE    DS    CL1        120        TUESDAY     (1 OR 0)                       
H2WED    DS    CL1        121        WEDNESDAY   (1 OR 0)                       
H2THU    DS    CL1        122        THURSDAY    (1 OR 0)                       
H2FRI    DS    CL1        123        FRIDAY      (1 OR 0)                       
H2SAT    DS    CL1        124        SATURDAY    (1 OR 0)                       
H2SUN    DS    CL1        125        SUNDAY      (1 OR 0)                       
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER PERSONS SAMPLE COUNTS RECORD                                   
*                                                                               
P2REC    DSECT                                                                  
P2SEQ    DS    CL2          1-2    C'02' - SAMPLE COUNTS RECORD                 
P2SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK                     
         DS    CL7          4-10                                                
P2CORR   DS    CL1         11      CORRECTION FLAG                              
         DS    CL3         12-14                                                
P2DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '                    
         DS    CL21        21-41                                                
P2DYSWKS DS    CL2         42-43   CYYMMDD                                      
P2START  DS    CL7         44-50   CYYMMDD                                      
P2END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL10        58-67                                                
P2SCALED DS    C           68      SCALED INTABS INDICATOR                      
         DS    CL9         69-77                                                
P2MKTBRK DS    CL3         78-80                                                
         DS    CL4         81-84                                                
P2TYPE   DS    CL1         85      C'P' - PERSONS DATA                          
         DS    CL6         86-91                                                
P2DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
         DS    CL21        95-115                                               
P2DATA   DS    0CL9       116      DEMO GROUP CATAGORIES                        
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER HOUSEHOLD USAGE RECORD                                         
*                                                                               
H3REC    DSECT                                                                  
H3SEQ    DS    CL2          1-1    C'03' - HOUSEHOLD USAGE RECORD               
H3SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK                     
         DS    CL7          4-10                                                
H3CORR   DS    CL1         11      CORRECTION FLAG                              
         DS    CL3         12-14                                                
H3DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '                    
         DS    CL16        21-36                                                
H3AET    DS    C           37      AUDIENCE TYPE 1=AVG AUD                      
         DS    CL6         38-43                                                
H3START  DS    CL7         44-50   CYYMMDD                                      
H3END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL23        58-80                                                
H3PHH    DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER           
         DS    CL2         83-84                                                
H3TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA              
H3EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)                     
H3EVSMIN DS    CL2         88-89   EVENT START MINUTE (00 OR 30)                
         DS    CL7         90-96                                                
H3VCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR           
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK           
         DS    CL17        98-114                                               
H3DUR    DS    CL4        115-118  EVENT DURATION   0000-1440 MINUTES           
         DS    CL61       119-179                                               
H3AVEHUT DS    CL9        180-188  AUDIENCE AVE HUT PROJ. (XXX,XXX,XXX)         
H3AVERTG DS    CL3        189-191  AUDIENCE AVE HUT RATING (XX.X)               
         DS    CL68       192-259                                               
H3QH1HUT DS    CL9        260-268  1ST QTR HR HUT PROJ (XXX,XXX,XXX)            
H3QH1RTG DS    CL3        269-271  1ST QTR HR HUT RATING (XX.X)                 
         DS    CL37       272-308                                               
H3QH2HUT DS    CL9        309-317  2ND QTR HR HUT PROJ (XXX,XXX,XXX)            
H3QH2RTG DS    CL3        318-320  2ND QTR HR HUT RATING (XX.X)                 
         DS    CL80       321-400                                               
H3OPT    DS    CL1        401      0=REGULAR DATA   1=OPTIONAL DATA             
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER DEMO USAGE RECORD                                              
*                                                                               
P3REC    DSECT                                                                  
P3SEQ    DS    CL2          1-2    C'03' - DEMO USAGE RECORD                    
P3SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK                     
         DS    CL7          4-10                                                
P3CORR   DS    CL1         11      CORRECTION FLAG                              
         DS    CL3         12-14                                                
P3DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '                    
         DS    CL16        21-36                                                
P3AET    DS    C           37      AUDIENCE TYPE 1=AVG AUD                      
         DS    CL6         38-43                                                
P3START  DS    CL7         44-50   CYYMMDD                                      
P3END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL27        58-84                                                
P3TYPE   DS    CL1         85      C'P' - PERSONS                               
         DS    CL6         86-91                                                
P3DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
         DS    CL21        95-115                                               
P3DCATS  DS    CL180      116-295  DEMOGRAPHIC CATEGORIES (20CL9)               
         DS    CL105      296-400                                               
P3OPT    DS    CL1        401-401  0=REGULAR DATA   1=OPTIONAL DATA             
*                                                                               
         EJECT                                                                  
* DSECT TO COVER PROGRAM DESCRIPTOR RECORD                                      
*                                                                               
D4REC    DSECT                                                                  
D4SEQ    DS    CL2          1-2    C'04' - PROGRAM DESCRIPTOR RECORD            
         DS    CL8          3-10                                                
D4ORIG   DS    CL1         11      0=ORIGINAL.........1=CORRECTION              
         DS    CL3         12-14                                                
D4NET    DS    CL6         15-20   NETWORK (ABC, CBS OR NBC)                    
D4NUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE                
         DS    CL4         31-34                                                
D4BREAK  DS    CL1         35      0=NOT A BREAKOUT...1=BREAKOUT                
D4SPEC   DS    CL1         36      0=NOT A SPECIAL....1=SPECIAL                 
         DS    CL1         37                                                   
         DS    CL2         38-39                                                
D4TTYPE  DS    CL1         40      TELECAST TYPE (SYND)                         
*                                  O=ORIGINAL   R=REPEAT                        
*                                  C=COMBINED   M=MULTIPLE                      
         DS    CL1         41                                                   
D4DYSWKS DS    CL2         42-43   NUMBER OF DAYS/WEEKS                         
D4START  DS    CL7         44-50   CYYMMDD                                      
D4END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL20        58-77                                                
D4MKTBRK DS    CL3         78-80   MARKET BREAK                                 
D4HH     DS    CL2         81-82   '00', 'PP', 'FP' PORTION OF FRINGE           
D4QH     DS    CL2         83-84   QUARTER HOUR                                 
D4TYPE   DS    CL1         85      C'D' - PROGRAM DESCRIPTOR DATA               
D4EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)                     
D4EVSMIN DS    CL2         88-89   EVENT START MINUTE (00-59)                   
         DS    CL7         90-96                                                
D4VCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR           
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK           
D4NAVAL  DS    CL1         98      1 = DATA NOT AVAILABLE                       
         DS    CL16        99-114                                               
D4NAME   DS    CL25       115-139  PROGRAM NAME                                 
         DS    CL40       140-179                                               
D4TITLE  DS    CL32       180-211  EPISODE TITLE                                
         DS    CL4        212-215                                               
D4ALPHA  DS    CL4        216-219  PROGRAM TYPE (ALPHA)                         
         DS    CL9        220-228                                               
D4MULTI  DS    CL1        229      MULTI-DAY     Y=MULTIPLE TELECASTS           
D4REPEAT DS    CL1        230      REPEAT IND.   Y=COMPLEX PROGRAM              
         DS    CL7        231-237                                               
D4PREM   DS    CL1        238      PREMIER IND.  Y=PREMIER TELECAST             
D4GAPD   DS    CL1                 GAPPED INDICATOR                             
         DS    CL10       240-249                                               
D4SYNID  DS    CL4        250-253  SYNDICATOR ID (SEE NETWORK TABLE)            
         DS    CL22       254-275                                               
D4DPT    DS    CL2        276-277  REPORTED DAYPART                             
*                                    PR=PRIME TIME   WM=WEEKDAY MORNING         
*                                    EF=EARLY FRINGE WD=WEEKDAY DAYTIME         
*                                    LF=LATE FRINGE  ED=WEEKEND DAYTIME         
D4DUR    DS    CL4        278-281  EVENT DURATION   0000-1440 MINUTES           
D4DAYS   DS    0CL7      (282-288) DAYS OF WEEK INDICATOR                       
D4MON    DS    CL1        282        MONDAY      (1 OR 0)                       
D4TUE    DS    CL1        283        TUESDAY     (1 OR 0)                       
D4WED    DS    CL1        284        WEDNESDAY   (1 OR 0)                       
D4THU    DS    CL1        285        THURSDAY    (1 OR 0)                       
D4FRI    DS    CL1        286        FRIDAY      (1 OR 0)                       
D4SAT    DS    CL1        287        SATURDAY    (1 OR 0)                       
D4SUN    DS    CL1        288        SUNDAY      (1 OR 0)                       
         DS    CL16       289-304                                               
*                                                                               
         EJECT                                                                  
*        THESE FIELDS (305-   ) ARE NOT REPORTED ON ALL D4 RECORDS              
D4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT                  
         DS    CL5        310-314                                               
D4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT               
         DS    CL11       317-327                                               
D4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES              
D4TOTDUR DS    CL6        331-336  TOTAL DURATION                               
D4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)          
D4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)                
D4REPORT DS    CL1        349      REPORTABILITY IND. 0=REPORTABLE              
         DS    CL4        350-353                                               
D4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)              
D4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                           
         DS    CL11       365-375                                               
D4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)           
D4RTG    DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)                 
         DS    CL12       388-400                                               
*                                                                               
D4OPT    DS    CL1        401-401  0=REGULAR DATA   1=OPTIONAL DATA             
*                                                                               
         EJECT                                                                  
* DSECT TO COVER HOUSEHOLD PROGRAM RECORD                                       
*                                                                               
H4REC    DSECT                                                                  
H4SEQ    DS    CL2          1-2    C'04' - HALF HOUR DETAIL RECORD              
         DS    CL8          3-10                                                
H4ORIG   DS    CL1         11      0=ORIGINAL.........1=CORRECTION              
         DS    CL3         12-14                                                
H4NET    DS    CL6         15-20   NETWORK (ABC, CBS OR NBC)                    
H4NUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE                
         DS    CL13        31-43                                                
H4START  DS    CL7         44-50   CYYMMDD                                      
H4END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL23        58-80                                                
H4HALF   DS    CL2         81-82   HALF HOUR ID  (01-48)                        
H4QH     DS    CL2         83-84   QUARTER HOUR (01-96)                         
H4TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA              
H4EVSHR  DS    CL2         86-87   EVENT START HOUR                             
H4EVSMIN DS    CL2         88-89   EVENT START MINUTE                           
         DS    CL25        90-114                                               
H4EVDUR  DS    CL4        115-118  DURATION W/IN 1/2HR                          
*                                                                               
H4DAYS   DS    0CL7      (119-125) DAYS OF WEEK INDICATOR                       
H4MON    DS    CL1        119        MONDAY      (1 OR 0)                       
H4TUE    DS    CL1        120        TUESDAY     (1 OR 0)                       
H4WED    DS    CL1        121        WEDNESDAY   (1 OR 0)                       
H4THU    DS    CL1        122        THURSDAY    (1 OR 0)                       
H4FRI    DS    CL1        123        FRIDAY      (1 OR 0)                       
H4SAT    DS    CL1        124        SATURDAY    (1 OR 0)                       
H4SUN    DS    CL1        125        SUNDAY      (1 OR 0)                       
         DS    CL48       126-173                                               
H4TOTDUR DS    CL6        174-179  TOTAL PRG 1/2HR CONTR DUR                    
H4PROJ   DS    CL9        180-188  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)           
H4RTG    DS    CL3        189-191  HALF HOUR AUDIENCE RATING (XX.X)             
H4REPT   DS    CL1        192      1/2HR REPORTABILITY INDICATOR                
         DS    CL13       192-205                                               
H4SHR    DS    CL2        206-207  HALF HOUR PROGRAM SHARE (XX)                 
         DS    CL46       208-253                                               
H4DUR1   DS    CL6        254-259  1ST QTR HOUR   1-15 MINS.-INDIV. DAY         
*                                    DURATION     2-75 MINS.-AVERAGES           
         DS    CL43       260-302                                               
H4DUR2   DS    CL6        303-308  2ND QTR HOUR   0-15 MINS.-INDIV. DAY         
*                                    DURATION     2-75 MINS.-AVERAGES           
         SPACE 2                                                                
* DSECT TO COVER DEMO PROGRAM RECORD                                            
*                                                                               
P4REC    DSECT                                                                  
P4SEQ    DS    CL2          1-2    C'04' - DEMO PROGRAM RECORD                  
         DS    CL12         3-14                                                
P4NET    DS    CL6         15-20   NETWORK                                      
         DS    CL60        21-80                                                
P4HALF   DS    CL2         81-82   IGNORE 'PP' AND 'FP'                         
P4QH     DS    CL2         83-84   QUARTER HOUR                                 
P4TYPE   DS    CL1         85      C'P' - PERSONS                               
         DS    CL6         86-91                                                
P4DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
P4DEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE                      
*                                    0=NOT PROGRAM PUTS  1=PROGRAM PUTS         
         DS    CL20        96-115                                               
P4DCATS  DS    CL180      116-295  DEMOGRAPHIC CATEGORIES (20CL9)               
         EJECT                                                                  
* DSECT TO COVER NON-NETWORK HOUSEHOLD USAGE RECORD                             
*                                                                               
H5REC    DSECT                                                                  
H5SEQ    DS    CL2          1-2    C'05' - NON-NETWORK HOUSEHOLD RECORD         
         DS    CL12         3-14                                                
H5NET    DS    CL6         15-20   DATA TYPE CODE (AGG)                         
H5NUM    DS    CL10        21-30   STATION CODE   1=IND   2=SUP                 
*                                         3=PBS   4=PAY   5=CAB                 
         DS    CL13        31-43                                                
H5START  DS    CL7         44-50   CYYMMDD                                      
H5END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL23        58-80                                                
H5PHH    DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER           
H5QH     DS    CL2         83-84   QUARTER HOUR                                 
H5TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA              
H5EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)                     
H5EVSMIN DS    CL2         88-89   EVENT START MINUTE (00 OR 30)                
         DS    CL90        90-179                                               
H5HOME   DS    CL9        180-188  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)           
H5RTG    DS    CL3        189-191  HALF HOUR AUD. RATING (XX.X)                 
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER NON-NETWORK DEMO USAGE RECORD                                  
*                                                                               
P5REC    DSECT                                                                  
P5SEQ    DS    CL2          1-2    C'05' - NON-NETWORK DEMO RECORD              
         DS    CL80         3-82                                                
P5QH     DS    CL2          83-84  QUARTER HOUR                                 
P5TYPE   DS    CL1         85      C'P' - PERSONS                               
P5EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)                     
P5EVSMIN DS    CL2         88-89   EVENT START MINUTE (00 OR 30)                
         DS    CL2         90-91                                                
P5DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
P5DEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE                      
*                                    0=NOT PROGRAM PUTS  1=PROGRAM PUTS         
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
       ++INCLUDE DEINTNT3DL                                                     
         EJECT                                                                  
       ++INCLUDE DECALVPHD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         TITLE '- DEMO CONVERSION - NTI POCKETPIECE'                            
DEND09I  CSECT                                                                  
*                                                                               
AKPROC   NMOD1 0,AKPROC                                                         
         USING KDSCT,R6                                                         
         USING ADSCT,R7                                                         
* YW25                                                                          
         TM    A0116+(L'A0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0116,=16C'0'                                                    
         PACK  DUB,A0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0193,FULL+(4-L'K0193)                                           
* YW68                                                                          
         TM    A0125+(L'A0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0125,=16C'0'                                                    
         PACK  DUB,A0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0197,FULL+(4-L'K0197)                                           
* YW911                                                                         
         TM    A0134+(L'A0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0134,=16C'0'                                                    
         PACK  DUB,A0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0201,FULL+(4-L'K0201)                                           
* YW1214                                                                        
         TM    A0143+(L'A0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0143,=16C'0'                                                    
         PACK  DUB,A0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0205,FULL+(4-L'K0205)                                           
* YW1517                                                                        
         TM    A0152+(L'A0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0152,=16C'0'                                                    
         PACK  DUB,A0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0209,FULL+(4-L'K0209)                                           
* YW1820                                                                        
         TM    A0161+(L'A0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0161,=16C'0'                                                    
         PACK  DUB,A0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0213,FULL+(4-L'K0213)                                           
* YW2124                                                                        
         TM    A0170+(L'A0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0170,=16C'0'                                                    
         PACK  DUB,A0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0217,FULL+(4-L'K0217)                                           
* YW2529                                                                        
         TM    A0179+(L'A0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0179,=16C'0'                                                    
         PACK  DUB,A0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0221,FULL+(4-L'K0221)                                           
* YW3034                                                                        
         TM    A0188+(L'A0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0188,=16C'0'                                                    
         PACK  DUB,A0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0225,FULL+(4-L'K0225)                                           
* YW3539                                                                        
         TM    A0197+(L'A0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0197,=16C'0'                                                    
         PACK  DUB,A0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0229,FULL+(4-L'K0229)                                           
* YW4044                                                                        
         TM    A0206+(L'A0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0206,=16C'0'                                                    
         PACK  DUB,A0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0233,FULL+(4-L'K0233)                                           
* YW4549                                                                        
         TM    A0215+(L'A0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0215,=16C'0'                                                    
         PACK  DUB,A0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0237,FULL+(4-L'K0237)                                           
* YW5054                                                                        
         TM    A0224+(L'A0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0224,=16C'0'                                                    
         PACK  DUB,A0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0241,FULL+(4-L'K0241)                                           
* YW5564                                                                        
         TM    A0233+(L'A0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0233,=16C'0'                                                    
         PACK  DUB,A0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0245,FULL+(4-L'K0245)                                           
* YW65+                                                                         
         TM    A0242+(L'A0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0242,=16C'0'                                                    
         PACK  DUB,A0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0249,FULL+(4-L'K0249)                                           
* YM25                                                                          
         TM    A0251+(L'A0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0251,=16C'0'                                                    
         PACK  DUB,A0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0253,FULL+(4-L'K0253)                                           
* YM68                                                                          
         TM    A0260+(L'A0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0260,=16C'0'                                                    
         PACK  DUB,A0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0257,FULL+(4-L'K0257)                                           
* YM911                                                                         
         TM    A0269+(L'A0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0269,=16C'0'                                                    
         PACK  DUB,A0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0261,FULL+(4-L'K0261)                                           
* YM1214                                                                        
         TM    A0278+(L'A0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0278,=16C'0'                                                    
         PACK  DUB,A0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0265,FULL+(4-L'K0265)                                           
* YM1517                                                                        
         TM    A0287+(L'A0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0287,=16C'0'                                                    
         PACK  DUB,A0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0269,FULL+(4-L'K0269)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
BKPROC   NMOD1 0,BKPROC                                                         
         USING KDSCT,R6                                                         
         USING BDSCT,R7                                                         
* YM1820                                                                        
         TM    B0116+(L'B0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0116,=16C'0'                                                    
         PACK  DUB,B0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0273,FULL+(4-L'K0273)                                           
* YM2124                                                                        
         TM    B0125+(L'B0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0125,=16C'0'                                                    
         PACK  DUB,B0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0277,FULL+(4-L'K0277)                                           
* YM2529                                                                        
         TM    B0134+(L'B0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0134,=16C'0'                                                    
         PACK  DUB,B0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0281,FULL+(4-L'K0281)                                           
* YM3034                                                                        
         TM    B0143+(L'B0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0143,=16C'0'                                                    
         PACK  DUB,B0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0285,FULL+(4-L'K0285)                                           
* YM3539                                                                        
         TM    B0152+(L'B0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0152,=16C'0'                                                    
         PACK  DUB,B0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0289,FULL+(4-L'K0289)                                           
* YM4044                                                                        
         TM    B0161+(L'B0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0161,=16C'0'                                                    
         PACK  DUB,B0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0293,FULL+(4-L'K0293)                                           
* YM4549                                                                        
         TM    B0170+(L'B0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0170,=16C'0'                                                    
         PACK  DUB,B0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0297,FULL+(4-L'K0297)                                           
* YM5054                                                                        
         TM    B0179+(L'B0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0179,=16C'0'                                                    
         PACK  DUB,B0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0301,FULL+(4-L'K0301)                                           
* YM5564                                                                        
         TM    B0188+(L'B0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0188,=16C'0'                                                    
         PACK  DUB,B0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0305,FULL+(4-L'K0305)                                           
* YM65+                                                                         
         TM    B0197+(L'B0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0197,=16C'0'                                                    
         PACK  DUB,B0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0309,FULL+(4-L'K0309)                                           
* YWMOMS                                                                        
         TM    B0206+(L'B0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0206,=16C'0'                                                    
         PACK  DUB,B0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0313,FULL+(4-L'K0313)                                           
* YWW1820                                                                       
         TM    B0224+(L'B0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0224,=16C'0'                                                    
         PACK  DUB,B0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0317,FULL+(4-L'K0317)                                           
* YWW2124                                                                       
         TM    B0233+(L'B0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0233,=16C'0'                                                    
         PACK  DUB,B0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0321,FULL+(4-L'K0321)                                           
* YWW2534                                                                       
         TM    B0242+(L'B0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0242,=16C'0'                                                    
         PACK  DUB,B0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0325,FULL+(4-L'K0325)                                           
* YWW3544                                                                       
         TM    B0251+(L'B0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0251,=16C'0'                                                    
         PACK  DUB,B0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0329,FULL+(4-L'K0329)                                           
* YWW4549                                                                       
         TM    B0260+(L'B0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0260,=16C'0'                                                    
         PACK  DUB,B0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0333,FULL+(4-L'K0333)                                           
* YWW5054                                                                       
         TM    B0269+(L'B0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0269,=16C'0'                                                    
         PACK  DUB,B0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0337,FULL+(4-L'K0337)                                           
* YWW55+                                                                        
         TM    B0278+(L'B0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0278,=16C'0'                                                    
         PACK  DUB,B0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0341,FULL+(4-L'K0341)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
CKPROC   NMOD1 0,CKPROC                                                         
         USING KDSCT,R6                                                         
         USING CDSCT,R7                                                         
* ZW25                                                                          
         TM    C0116+(L'C0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0116,=16C'0'                                                    
         PACK  DUB,C0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1049,FULL+(4-L'K1049)                                           
* ZW68                                                                          
         TM    C0125+(L'C0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0125,=16C'0'                                                    
         PACK  DUB,C0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1053,FULL+(4-L'K1053)                                           
* ZW911                                                                         
         TM    C0134+(L'C0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0134,=16C'0'                                                    
         PACK  DUB,C0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1057,FULL+(4-L'K1057)                                           
* ZW1214                                                                        
         TM    C0143+(L'C0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0143,=16C'0'                                                    
         PACK  DUB,C0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1061,FULL+(4-L'K1061)                                           
* ZW1517                                                                        
         TM    C0152+(L'C0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0152,=16C'0'                                                    
         PACK  DUB,C0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1065,FULL+(4-L'K1065)                                           
* ZW1820                                                                        
         TM    C0161+(L'C0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0161,=16C'0'                                                    
         PACK  DUB,C0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1069,FULL+(4-L'K1069)                                           
* ZW2124                                                                        
         TM    C0170+(L'C0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0170,=16C'0'                                                    
         PACK  DUB,C0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1073,FULL+(4-L'K1073)                                           
* ZW2529                                                                        
         TM    C0179+(L'C0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0179,=16C'0'                                                    
         PACK  DUB,C0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1077,FULL+(4-L'K1077)                                           
* ZW3034                                                                        
         TM    C0188+(L'C0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0188,=16C'0'                                                    
         PACK  DUB,C0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1081,FULL+(4-L'K1081)                                           
* ZW3539                                                                        
         TM    C0197+(L'C0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0197,=16C'0'                                                    
         PACK  DUB,C0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1085,FULL+(4-L'K1085)                                           
* ZW4044                                                                        
         TM    C0206+(L'C0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0206,=16C'0'                                                    
         PACK  DUB,C0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1089,FULL+(4-L'K1089)                                           
* ZW4549                                                                        
         TM    C0215+(L'C0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0215,=16C'0'                                                    
         PACK  DUB,C0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1093,FULL+(4-L'K1093)                                           
* ZW5054                                                                        
         TM    C0224+(L'C0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0224,=16C'0'                                                    
         PACK  DUB,C0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1097,FULL+(4-L'K1097)                                           
* ZW5564                                                                        
         TM    C0233+(L'C0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0233,=16C'0'                                                    
         PACK  DUB,C0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1101,FULL+(4-L'K1101)                                           
* ZW65+                                                                         
         TM    C0242+(L'C0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0242,=16C'0'                                                    
         PACK  DUB,C0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1105,FULL+(4-L'K1105)                                           
* ZM25                                                                          
         TM    C0251+(L'C0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0251,=16C'0'                                                    
         PACK  DUB,C0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1109,FULL+(4-L'K1109)                                           
* ZM68                                                                          
         TM    C0260+(L'C0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0260,=16C'0'                                                    
         PACK  DUB,C0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1113,FULL+(4-L'K1113)                                           
* ZM911                                                                         
         TM    C0269+(L'C0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0269,=16C'0'                                                    
         PACK  DUB,C0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1117,FULL+(4-L'K1117)                                           
* ZM1214                                                                        
         TM    C0278+(L'C0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0278,=16C'0'                                                    
         PACK  DUB,C0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1121,FULL+(4-L'K1121)                                           
* ZM1517                                                                        
         TM    C0287+(L'C0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0287,=16C'0'                                                    
         PACK  DUB,C0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1125,FULL+(4-L'K1125)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
DKPROC   NMOD1 0,DKPROC                                                         
         USING KDSCT,R6                                                         
         USING DDSCT,R7                                                         
* ZM1820                                                                        
         TM    D0116+(L'D0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0116,=16C'0'                                                    
         PACK  DUB,D0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1129,FULL+(4-L'K1129)                                           
* ZM2124                                                                        
         TM    D0125+(L'D0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0125,=16C'0'                                                    
         PACK  DUB,D0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1133,FULL+(4-L'K1133)                                           
* ZM2529                                                                        
         TM    D0134+(L'D0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0134,=16C'0'                                                    
         PACK  DUB,D0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1137,FULL+(4-L'K1137)                                           
* ZM3034                                                                        
         TM    D0143+(L'D0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0143,=16C'0'                                                    
         PACK  DUB,D0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1141,FULL+(4-L'K1141)                                           
* ZM3539                                                                        
         TM    D0152+(L'D0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0152,=16C'0'                                                    
         PACK  DUB,D0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1145,FULL+(4-L'K1145)                                           
* ZM4044                                                                        
         TM    D0161+(L'D0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0161,=16C'0'                                                    
         PACK  DUB,D0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1149,FULL+(4-L'K1149)                                           
* ZM4549                                                                        
         TM    D0170+(L'D0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0170,=16C'0'                                                    
         PACK  DUB,D0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1153,FULL+(4-L'K1153)                                           
* ZM5054                                                                        
         TM    D0179+(L'D0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0179,=16C'0'                                                    
         PACK  DUB,D0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1157,FULL+(4-L'K1157)                                           
* ZM5564                                                                        
         TM    D0188+(L'D0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0188,=16C'0'                                                    
         PACK  DUB,D0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1161,FULL+(4-L'K1161)                                           
* ZM65+                                                                         
         TM    D0197+(L'D0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0197,=16C'0'                                                    
         PACK  DUB,D0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1165,FULL+(4-L'K1165)                                           
* ZWMOMS                                                                        
         TM    D0206+(L'D0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0206,=16C'0'                                                    
         PACK  DUB,D0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1169,FULL+(4-L'K1169)                                           
* ZWW1820                                                                       
         TM    D0224+(L'D0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0224,=16C'0'                                                    
         PACK  DUB,D0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1173,FULL+(4-L'K1173)                                           
* ZWW2124                                                                       
         TM    D0233+(L'D0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0233,=16C'0'                                                    
         PACK  DUB,D0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1177,FULL+(4-L'K1177)                                           
* ZWW2534                                                                       
         TM    D0242+(L'D0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0242,=16C'0'                                                    
         PACK  DUB,D0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1181,FULL+(4-L'K1181)                                           
* ZWW3544                                                                       
         TM    D0251+(L'D0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0251,=16C'0'                                                    
         PACK  DUB,D0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1185,FULL+(4-L'K1185)                                           
* ZWW4549                                                                       
         TM    D0260+(L'D0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0260,=16C'0'                                                    
         PACK  DUB,D0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1189,FULL+(4-L'K1189)                                           
* ZWW5054                                                                       
         TM    D0269+(L'D0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0269,=16C'0'                                                    
         PACK  DUB,D0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1193,FULL+(4-L'K1193)                                           
* ZWW55+                                                                        
         TM    D0278+(L'D0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0278,=16C'0'                                                    
         PACK  DUB,D0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1197,FULL+(4-L'K1197)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
FKPROC   NMOD1 0,FKPROC                                                         
         USING KDSCT,R6                                                         
         USING FDSCT,R7                                                         
* RHOMES                                                                        
         TM    F0346+(L'F0346-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0346,=16C'0'                                                    
         PACK  DUB,F0346                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    F0337+(L'F0337-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0337,=16C'0'                                                    
         PACK  DUB,F0337                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
* SHOMES                                                                        
         TM    F0363+(L'F0363-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0363,=16C'0'                                                    
         PACK  DUB,F0363                                                        
         CVB   RF,DUB                                                           
         MVC   HALF,=H'01'                                                      
         BRAS  R9,PRECISN                                                       
         ST    RF,FULL                                                          
         MVC   K0541,FULL+(4-L'K0541)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         SPACE 2                                                                
PRECISN  LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MHI   R0,10                                                            
         BRCT  RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         JO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         BR    R9                                                               
         EJECT                                                                  
GKPROC   NMOD1 0,GKPROC                                                         
         USING KDSCT,R6                                                         
         USING GDSCT,R7                                                         
* RHOMES                                                                        
         TM    G0189+(L'G0189-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0189,=16C'0'                                                    
         PACK  DUB,G0189                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    G0180+(L'G0180-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0180,=16C'0'                                                    
         PACK  DUB,G0180                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
* SHOMES                                                                        
         TM    G0206+(L'G0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0206,=16C'0'                                                    
         PACK  DUB,G0206                                                        
         CVB   RF,DUB                                                           
         MVC   HALF,=H'01'                                                      
         BRAS  R9,PRECISN                                                       
         ST    RF,FULL                                                          
         MVC   K0541,FULL+(4-L'K0541)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
HKPROC   NMOD1 0,HKPROC                                                         
         USING KDSCT,R6                                                         
         USING HDSCT,R7                                                         
* YW25                                                                          
         TM    H0116+(L'H0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0116,=16C'0'                                                    
         PACK  DUB,H0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0193,FULL+(4-L'K0193)                                           
* YW68                                                                          
         TM    H0125+(L'H0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0125,=16C'0'                                                    
         PACK  DUB,H0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0197,FULL+(4-L'K0197)                                           
* YW911                                                                         
         TM    H0134+(L'H0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0134,=16C'0'                                                    
         PACK  DUB,H0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0201,FULL+(4-L'K0201)                                           
* YW1214                                                                        
         TM    H0143+(L'H0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0143,=16C'0'                                                    
         PACK  DUB,H0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0205,FULL+(4-L'K0205)                                           
* YW1517                                                                        
         TM    H0152+(L'H0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0152,=16C'0'                                                    
         PACK  DUB,H0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0209,FULL+(4-L'K0209)                                           
* YW1820                                                                        
         TM    H0161+(L'H0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0161,=16C'0'                                                    
         PACK  DUB,H0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0213,FULL+(4-L'K0213)                                           
* YW2124                                                                        
         TM    H0170+(L'H0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0170,=16C'0'                                                    
         PACK  DUB,H0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0217,FULL+(4-L'K0217)                                           
* YW2529                                                                        
         TM    H0179+(L'H0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0179,=16C'0'                                                    
         PACK  DUB,H0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0221,FULL+(4-L'K0221)                                           
* YW3034                                                                        
         TM    H0188+(L'H0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0188,=16C'0'                                                    
         PACK  DUB,H0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0225,FULL+(4-L'K0225)                                           
* YW3539                                                                        
         TM    H0197+(L'H0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0197,=16C'0'                                                    
         PACK  DUB,H0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0229,FULL+(4-L'K0229)                                           
* YW4044                                                                        
         TM    H0206+(L'H0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0206,=16C'0'                                                    
         PACK  DUB,H0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0233,FULL+(4-L'K0233)                                           
* YW4549                                                                        
         TM    H0215+(L'H0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0215,=16C'0'                                                    
         PACK  DUB,H0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0237,FULL+(4-L'K0237)                                           
* YW5054                                                                        
         TM    H0224+(L'H0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0224,=16C'0'                                                    
         PACK  DUB,H0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0241,FULL+(4-L'K0241)                                           
* YW5564                                                                        
         TM    H0233+(L'H0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0233,=16C'0'                                                    
         PACK  DUB,H0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0245,FULL+(4-L'K0245)                                           
* YW65+                                                                         
         TM    H0242+(L'H0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0242,=16C'0'                                                    
         PACK  DUB,H0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0249,FULL+(4-L'K0249)                                           
* YM25                                                                          
         TM    H0251+(L'H0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0251,=16C'0'                                                    
         PACK  DUB,H0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0253,FULL+(4-L'K0253)                                           
* YM68                                                                          
         TM    H0260+(L'H0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0260,=16C'0'                                                    
         PACK  DUB,H0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0257,FULL+(4-L'K0257)                                           
* YM911                                                                         
         TM    H0269+(L'H0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0269,=16C'0'                                                    
         PACK  DUB,H0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0261,FULL+(4-L'K0261)                                           
* YM1214                                                                        
         TM    H0278+(L'H0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0278,=16C'0'                                                    
         PACK  DUB,H0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0265,FULL+(4-L'K0265)                                           
* YM1517                                                                        
         TM    H0287+(L'H0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0287,=16C'0'                                                    
         PACK  DUB,H0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0269,FULL+(4-L'K0269)                                           
*                                                                               
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
IKPROC   NMOD1 0,IKPROC                                                         
         USING KDSCT,R6                                                         
         USING IDSCT,R7                                                         
* YM1820                                                                        
         TM    I0116+(L'I0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0116,=16C'0'                                                    
         PACK  DUB,I0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0273,FULL+(4-L'K0273)                                           
* YM2124                                                                        
         TM    I0125+(L'I0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0125,=16C'0'                                                    
         PACK  DUB,I0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0277,FULL+(4-L'K0277)                                           
* YM2529                                                                        
         TM    I0134+(L'I0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0134,=16C'0'                                                    
         PACK  DUB,I0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0281,FULL+(4-L'K0281)                                           
* YM3034                                                                        
         TM    I0143+(L'I0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0143,=16C'0'                                                    
         PACK  DUB,I0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0285,FULL+(4-L'K0285)                                           
* YM3539                                                                        
         TM    I0152+(L'I0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0152,=16C'0'                                                    
         PACK  DUB,I0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0289,FULL+(4-L'K0289)                                           
* YM4044                                                                        
         TM    I0161+(L'I0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0161,=16C'0'                                                    
         PACK  DUB,I0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0293,FULL+(4-L'K0293)                                           
* YM4549                                                                        
         TM    I0170+(L'I0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0170,=16C'0'                                                    
         PACK  DUB,I0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0297,FULL+(4-L'K0297)                                           
* YM5054                                                                        
         TM    I0179+(L'I0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0179,=16C'0'                                                    
         PACK  DUB,I0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0301,FULL+(4-L'K0301)                                           
* YM5564                                                                        
         TM    I0188+(L'I0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0188,=16C'0'                                                    
         PACK  DUB,I0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0305,FULL+(4-L'K0305)                                           
* YM65+                                                                         
         TM    I0197+(L'I0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0197,=16C'0'                                                    
         PACK  DUB,I0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0309,FULL+(4-L'K0309)                                           
* YWMOMS                                                                        
         TM    I0206+(L'I0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0206,=16C'0'                                                    
         PACK  DUB,I0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0313,FULL+(4-L'K0313)                                           
* YWW1820                                                                       
         TM    I0224+(L'I0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0224,=16C'0'                                                    
         PACK  DUB,I0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0317,FULL+(4-L'K0317)                                           
* YWW2124                                                                       
         TM    I0233+(L'I0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0233,=16C'0'                                                    
         PACK  DUB,I0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0321,FULL+(4-L'K0321)                                           
* YWW2534                                                                       
         TM    I0242+(L'I0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0242,=16C'0'                                                    
         PACK  DUB,I0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0325,FULL+(4-L'K0325)                                           
* YWW3544                                                                       
         TM    I0251+(L'I0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0251,=16C'0'                                                    
         PACK  DUB,I0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0329,FULL+(4-L'K0329)                                           
* YWW4549                                                                       
         TM    I0260+(L'I0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0260,=16C'0'                                                    
         PACK  DUB,I0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0333,FULL+(4-L'K0333)                                           
* YWW5054                                                                       
         TM    I0269+(L'I0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0269,=16C'0'                                                    
         PACK  DUB,I0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0337,FULL+(4-L'K0337)                                           
* YWW55+                                                                        
         TM    I0278+(L'I0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0278,=16C'0'                                                    
         PACK  DUB,I0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0341,FULL+(4-L'K0341)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
JKPROC   NMOD1 0,JKPROC                                                         
         USING KDSCT,R6                                                         
         USING JDSCT,R7                                                         
* RHOMES                                                                        
         TM    J0189+(L'J0189-1),X'F0'                                          
         BO    *+10                                                             
         MVC   J0189,=16C'0'                                                    
         PACK  DUB,J0189                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    J0180+(L'J0180-1),X'F0'                                          
         BO    *+10                                                             
         MVC   J0180,=16C'0'                                                    
         PACK  DUB,J0180                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
         XMOD1 1                                                                
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP RB                                                                
         EJECT                                                                  
*        DEDEMFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMCNVD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         EJECT                                                                  
ADSCT    DSECT                                                                  
A0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
A0116    DS    CL0009              YW25                                         
A0125    DS    CL0009              YW68                                         
A0134    DS    CL0009              YW911                                        
A0143    DS    CL0009              YW1214                                       
A0152    DS    CL0009              YW1517                                       
A0161    DS    CL0009              YW1820                                       
A0170    DS    CL0009              YW2124                                       
A0179    DS    CL0009              YW2529                                       
A0188    DS    CL0009              YW3034                                       
A0197    DS    CL0009              YW3539                                       
A0206    DS    CL0009              YW4044                                       
A0215    DS    CL0009              YW4549                                       
A0224    DS    CL0009              YW5054                                       
A0233    DS    CL0009              YW5564                                       
A0242    DS    CL0009              YW65+                                        
A0251    DS    CL0009              YM25                                         
A0260    DS    CL0009              YM68                                         
A0269    DS    CL0009              YM911                                        
A0278    DS    CL0009              YM1214                                       
A0287    DS    CL0009              YM1517                                       
         EJECT                                                                  
BDSCT    DSECT                                                                  
B0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
B0116    DS    CL0009              YM1820                                       
B0125    DS    CL0009              YM2124                                       
B0134    DS    CL0009              YM2529                                       
B0143    DS    CL0009              YM3034                                       
B0152    DS    CL0009              YM3539                                       
B0161    DS    CL0009              YM4044                                       
B0170    DS    CL0009              YM4549                                       
B0179    DS    CL0009              YM5054                                       
B0188    DS    CL0009              YM5564                                       
B0197    DS    CL0009              YM65+                                        
B0206    DS    CL0009              YWMOMS                                       
B0215    DS    CL0009              *                                            
B0224    DS    CL0009              YWW1820                                      
B0233    DS    CL0009              YWW2124                                      
B0242    DS    CL0009              YWW2534                                      
B0251    DS    CL0009              YWW3544                                      
B0260    DS    CL0009              YWW4549                                      
B0269    DS    CL0009              YWW5054                                      
B0278    DS    CL0009              YWW55+                                       
B0287    DS    CL0009              *                                            
         EJECT                                                                  
CDSCT    DSECT                                                                  
C0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
C0116    DS    CL0009              ZW25                                         
C0125    DS    CL0009              ZW68                                         
C0134    DS    CL0009              ZW911                                        
C0143    DS    CL0009              ZW1214                                       
C0152    DS    CL0009              ZW1517                                       
C0161    DS    CL0009              ZW1820                                       
C0170    DS    CL0009              ZW2124                                       
C0179    DS    CL0009              ZW2529                                       
C0188    DS    CL0009              ZW3034                                       
C0197    DS    CL0009              ZW3539                                       
C0206    DS    CL0009              ZW4044                                       
C0215    DS    CL0009              ZW4549                                       
C0224    DS    CL0009              ZW5054                                       
C0233    DS    CL0009              ZW5564                                       
C0242    DS    CL0009              ZW65+                                        
C0251    DS    CL0009              ZM25                                         
C0260    DS    CL0009              ZM68                                         
C0269    DS    CL0009              ZM911                                        
C0278    DS    CL0009              ZM1214                                       
C0287    DS    CL0009              ZM1517                                       
         EJECT                                                                  
DDSCT    DSECT                                                                  
D0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
D0116    DS    CL0009              ZM1820                                       
D0125    DS    CL0009              ZM2124                                       
D0134    DS    CL0009              ZM2529                                       
D0143    DS    CL0009              ZM3034                                       
D0152    DS    CL0009              ZM3539                                       
D0161    DS    CL0009              ZM4044                                       
D0170    DS    CL0009              ZM4549                                       
D0179    DS    CL0009              ZM5054                                       
D0188    DS    CL0009              ZM5564                                       
D0197    DS    CL0009              ZM65+                                        
D0206    DS    CL0009              ZWMOMS                                       
D0215    DS    CL0009              *                                            
D0224    DS    CL0009              ZWW1820                                      
D0233    DS    CL0009              ZWW2124                                      
D0242    DS    CL0009              ZWW2534                                      
D0251    DS    CL0009              ZWW3544                                      
D0260    DS    CL0009              ZWW4549                                      
D0269    DS    CL0009              ZWW5054                                      
D0278    DS    CL0009              ZWW55+                                       
D0287    DS    CL0009              *                                            
         EJECT                                                                  
EDSCT    DSECT                                                                  
E0000    DS    0C                                                               
         ORG   *+0140              DISPLACEMENT TO DEMOS ON NIELSEN REC         
E0141    DS    CL0009              IHOMES                                       
E0150    DS    CL0003              RHOMES                                       
         EJECT                                                                  
FDSCT    DSECT                                                                  
F0000    DS    0C                                                               
         ORG   *+0336              DISPLACEMENT TO DEMOS ON NIELSEN REC         
F0337    DS    CL0009              IHOMES                                       
F0346    DS    CL0003              RHOMES                                       
F0349    DS    CL0014              *                                            
F0363    DS    CL0002              SHOMES                                       
         EJECT                                                                  
GDSCT    DSECT                                                                  
G0000    DS    0C                                                               
         ORG   *+0179              DISPLACEMENT TO DEMOS ON NIELSEN REC         
G0180    DS    CL0009              IHOMES                                       
G0189    DS    CL0003              RHOMES                                       
G0192    DS    CL0014              *                                            
G0206    DS    CL0002              SHOMES                                       
         EJECT                                                                  
HDSCT    DSECT                                                                  
H0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
H0116    DS    CL0009              YW25                                         
H0125    DS    CL0009              YW68                                         
H0134    DS    CL0009              YW911                                        
H0143    DS    CL0009              YW1214                                       
H0152    DS    CL0009              YW1517                                       
H0161    DS    CL0009              YW1820                                       
H0170    DS    CL0009              YW2124                                       
H0179    DS    CL0009              YW2529                                       
H0188    DS    CL0009              YW3034                                       
H0197    DS    CL0009              YW3539                                       
H0206    DS    CL0009              YW4044                                       
H0215    DS    CL0009              YW4549                                       
H0224    DS    CL0009              YW5054                                       
H0233    DS    CL0009              YW5564                                       
H0242    DS    CL0009              YW65+                                        
H0251    DS    CL0009              YM25                                         
H0260    DS    CL0009              YM68                                         
H0269    DS    CL0009              YM911                                        
H0278    DS    CL0009              YM1214                                       
H0287    DS    CL0009              YM1517                                       
         EJECT                                                                  
IDSCT    DSECT                                                                  
I0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
I0116    DS    CL0009              YM1820                                       
I0125    DS    CL0009              YM2124                                       
I0134    DS    CL0009              YM2529                                       
I0143    DS    CL0009              YM3034                                       
I0152    DS    CL0009              YM3539                                       
I0161    DS    CL0009              YM4044                                       
I0170    DS    CL0009              YM4549                                       
I0179    DS    CL0009              YM5054                                       
I0188    DS    CL0009              YM5564                                       
I0197    DS    CL0009              YM65+                                        
I0206    DS    CL0009              YWMOMS                                       
I0215    DS    CL0009              *                                            
I0224    DS    CL0009              YWW1820                                      
I0233    DS    CL0009              YWW2124                                      
I0242    DS    CL0009              YWW2534                                      
I0251    DS    CL0009              YWW3544                                      
I0260    DS    CL0009              YWW4549                                      
I0269    DS    CL0009              YWW5054                                      
I0278    DS    CL0009              YWW55+                                       
I0287    DS    CL0009              *                                            
         EJECT                                                                  
JDSCT    DSECT                                                                  
J0000    DS    0C                                                               
         ORG   *+0179              DISPLACEMENT TO DEMOS ON NIELSEN REC         
J0180    DS    CL0009              IHOMES                                       
J0189    DS    CL0003              RHOMES                                       
         EJECT                                                                  
KDSCT    DSECT                                                                  
K0000    DS    0C                                                               
         ORG   *+(INTACCS-INTRECLN)  DISPLACEMENT TO DEMOS ON INTREC            
K0193    DS    CL0004              YW25                                         
K0197    DS    CL0004              YW68                                         
K0201    DS    CL0004              YW911                                        
K0205    DS    CL0004              YW1214                                       
K0209    DS    CL0004              YW1517                                       
K0213    DS    CL0004              YW1820                                       
K0217    DS    CL0004              YW2124                                       
K0221    DS    CL0004              YW2529                                       
K0225    DS    CL0004              YW3034                                       
K0229    DS    CL0004              YW3539                                       
K0233    DS    CL0004              YW4044                                       
K0237    DS    CL0004              YW4549                                       
K0241    DS    CL0004              YW5054                                       
K0245    DS    CL0004              YW5564                                       
K0249    DS    CL0004              YW65+                                        
K0253    DS    CL0004              YM25                                         
K0257    DS    CL0004              YM68                                         
K0261    DS    CL0004              YM911                                        
K0265    DS    CL0004              YM1214                                       
K0269    DS    CL0004              YM1517                                       
K0273    DS    CL0004              YM1820                                       
K0277    DS    CL0004              YM2124                                       
K0281    DS    CL0004              YM2529                                       
K0285    DS    CL0004              YM3034                                       
K0289    DS    CL0004              YM3539                                       
K0293    DS    CL0004              YM4044                                       
K0297    DS    CL0004              YM4549                                       
K0301    DS    CL0004              YM5054                                       
K0305    DS    CL0004              YM5564                                       
K0309    DS    CL0004              YM65+                                        
K0313    DS    CL0004              YWMOMS                                       
K0317    DS    CL0004              YWW1820                                      
K0321    DS    CL0004              YWW2124                                      
K0325    DS    CL0004              YWW2534                                      
K0329    DS    CL0004              YWW3544                                      
K0333    DS    CL0004              YWW4549                                      
K0337    DS    CL0004              YWW5054                                      
K0341    DS    CL0004              YWW55+                                       
K0345    DS    CL0004              YHOMES                                       
K0349    DS    CL0004              VW18+                                        
K0353    DS    CL0004              VW1849                                       
K0357    DS    CL0004              VM18+                                        
K0361    DS    CL0004              VW2554                                       
K0365    DS    CL0004              VWWRK                                        
K0369    DS    CL0004              VW1217                                       
K0373    DS    CL0004              VM1849                                       
K0377    DS    CL0004              VM2554                                       
K0381    DS    CL0004              VM55+                                        
K0385    DS    CL0004              VW2549                                       
K0389    DS    CL0004              VW55+                                        
K0393    DS    CL0004              VWW1849                                      
K0397    DS    CL0004              VM1217                                       
K0401    DS    CL0004              VM2549                                       
K0405    DS    CL0004              VV2+                                         
K0409    DS    CL0004              VW1834                                       
K0413    DS    CL0004              VM1834                                       
K0417    DS    CL0004              VWMOMS                                       
K0421    DS    CL0004              VM2149                                       
K0425    DS    CL0004              VM3564                                       
K0429    DS    CL0004              VW21+                                        
K0433    DS    CL0004              VW3564                                       
K0437    DS    CL0004              VM2154                                       
K0441    DS    CL0004              VM21+                                        
K0445    DS    CL0004              VW2149                                       
K0449    DS    CL0004              VW2154                                       
K0453    DS    CL0004              VWW2554                                      
K0457    DS    CL0004              VV611                                        
K0461    DS    CL0004              VV1217                                       
K0465    DS    CL0004              VV211                                        
K0469    DS    CL0004              VW211                                        
K0473    DS    CL0004              VM211                                        
K0477    DS    CL0004              VW611                                        
K0481    DS    CL0004              VM611                                        
K0485    DS    CL0004              VV1214                                       
K0489    DS    CL0004              VV25                                         
K0493    DS    CL0004              VV1517                                       
K0497    DS    CL0004              VW1824                                       
K0501    DS    CL0004              VW1524                                       
K0505    DS    CL0004              VM1824                                       
K0509    DS    CL0004              VM1524                                       
K0513    DS    CL0004              VV68                                         
K0517    DS    CL0004              VV911                                        
K0521    DS    CL0004              VHWC18                                       
K0525    DS    CL0004              VHWC12                                       
K0529    DS    CL0004              VHWC6                                        
K0533    DS    CL0004              RHOMES                                       
K0537    DS    CL0004              IHOMES                                       
K0541    DS    CL0004              SHOMES                                       
K0545    DS    CL0004              FN03                                         
K0549    DS    CL0004              FN04                                         
K0553    DS    CL0004              FN05                                         
K0557    DS    CL0004              FN06                                         
K0561    DS    CL0004              FN07                                         
K0565    DS    CL0004              FN08                                         
K0569    DS    CL0004              VV18+                                        
K0573    DS    CL0004              VV1834                                       
K0577    DS    CL0004              VV1849                                       
K0581    DS    CL0004              VV2549                                       
K0585    DS    CL0004              VV2554                                       
K0589    DS    CL0004              VV3564                                       
K0593    DS    CL0004              VV5564                                       
K0597    DS    CL0004              VV55+                                        
K0601    DS    CL0004              VV1824                                       
K0605    DS    CL0004              VV2149                                       
K0609    DS    CL0004              VV2154                                       
K0613    DS    CL0004              VV21+                                        
K0617    DS    CL0004              VV1524                                       
K0621    DS    CL0004              RHWC18                                       
K0625    DS    CL0004              RHWC12                                       
K0629    DS    CL0004              RHWC6                                        
K0633    DS    CL0004              IHWC18                                       
K0637    DS    CL0004              IHWC12                                       
K0641    DS    CL0004              IHWC6                                        
K0645    DS    CL0004              FN00                                         
K0649    DS    CL0004              FN01                                         
K0653    DS    CL0004              FN02                                         
K0657    DS    CL0004              FN03                                         
K0661    DS    CL0004              FN04                                         
K0665    DS    CL0004              FN05                                         
K0669    DS    CL0004              FN06                                         
K0673    DS    CL0004              FN07                                         
K0677    DS    CL0004              FN08                                         
K0681    DS    CL0004              FN09                                         
K0685    DS    CL0004              FN0A                                         
K0689    DS    CL0004              FN0B                                         
K0693    DS    CL0004              FN0C                                         
K0697    DS    CL0004              FN0D                                         
K0701    DS    CL0004              FN0E                                         
K0705    DS    CL0004              FN0F                                         
K0709    DS    CL0004              FN10                                         
K0713    DS    CL0004              FN11                                         
K0717    DS    CL0004              FN12                                         
K0721    DS    CL0004              FN13                                         
K0725    DS    CL0004              FN14                                         
K0729    DS    CL0004              FN15                                         
K0733    DS    CL0004              FN16                                         
K0737    DS    CL0004              FN17                                         
K0741    DS    CL0004              FN18                                         
K0745    DS    CL0004              FN19                                         
K0749    DS    CL0004              FN1A                                         
K0753    DS    CL0004              FN1B                                         
K0757    DS    CL0004              FN1C                                         
K0761    DS    CL0004              FN1D                                         
K0765    DS    CL0004              FN1E                                         
K0769    DS    CL0004              FN1F                                         
K0773    DS    CL0004              FN20                                         
K0777    DS    CL0004              FN21                                         
K0781    DS    CL0004              FN22                                         
K0785    DS    CL0004              FN23                                         
K0789    DS    CL0004              FN24                                         
K0793    DS    CL0004              FN25                                         
K0797    DS    CL0004              FN26                                         
K0801    DS    CL0004              FN27                                         
K0805    DS    CL0004              FN28                                         
K0809    DS    CL0004              FN29                                         
K0813    DS    CL0004              FN2A                                         
K0817    DS    CL0004              FN2B                                         
K0821    DS    CL0004              OW18+                                        
K0825    DS    CL0004              OW1849                                       
K0829    DS    CL0004              OM18+                                        
K0833    DS    CL0004              OW2554                                       
K0837    DS    CL0004              OWWRK                                        
K0841    DS    CL0004              OW1217                                       
K0845    DS    CL0004              OM1849                                       
K0849    DS    CL0004              OM2554                                       
K0853    DS    CL0004              OM55+                                        
K0857    DS    CL0004              OW2549                                       
K0861    DS    CL0004              OW55+                                        
K0865    DS    CL0004              OWW1849                                      
K0869    DS    CL0004              OM1217                                       
K0873    DS    CL0004              OM2549                                       
K0877    DS    CL0004              OV2+                                         
K0881    DS    CL0004              OW1834                                       
K0885    DS    CL0004              OM1834                                       
K0889    DS    CL0004              OWMOMS                                       
K0893    DS    CL0004              OM2149                                       
K0897    DS    CL0004              OM3564                                       
K0901    DS    CL0004              OW21+                                        
K0905    DS    CL0004              OW3564                                       
K0909    DS    CL0004              OM2154                                       
K0913    DS    CL0004              OM21+                                        
K0917    DS    CL0004              OW2149                                       
K0921    DS    CL0004              OW2154                                       
K0925    DS    CL0004              OWW2554                                      
K0929    DS    CL0004              OV611                                        
K0933    DS    CL0004              OV1217                                       
K0937    DS    CL0004              OV211                                        
K0941    DS    CL0004              OW211                                        
K0945    DS    CL0004              OM211                                        
K0949    DS    CL0004              OW611                                        
K0953    DS    CL0004              OM611                                        
K0957    DS    CL0004              OV1214                                       
K0961    DS    CL0004              OV25                                         
K0965    DS    CL0004              OV1517                                       
K0969    DS    CL0004              OW1824                                       
K0973    DS    CL0004              OW1524                                       
K0977    DS    CL0004              OM1824                                       
K0981    DS    CL0004              OM1524                                       
K0985    DS    CL0004              OV611                                        
K0989    DS    CL0004              OV911                                        
K0993    DS    CL0004              OV18+                                        
K0997    DS    CL0004              OV1834                                       
K1001    DS    CL0004              OV1849                                       
K1005    DS    CL0004              OV2549                                       
K1009    DS    CL0004              OV2554                                       
K1013    DS    CL0004              OV3564                                       
K1017    DS    CL0004              OV5564                                       
K1021    DS    CL0004              OV55+                                        
K1025    DS    CL0004              OV1824                                       
K1029    DS    CL0004              OV2149                                       
K1033    DS    CL0004              OV2154                                       
K1037    DS    CL0004              OV21+                                        
K1041    DS    CL0004              OV1524                                       
K1045    DS    CL0004              OHOMES                                       
K1049    DS    CL0004              ZW25                                         
K1053    DS    CL0004              ZW68                                         
K1057    DS    CL0004              ZW911                                        
K1061    DS    CL0004              ZW1214                                       
K1065    DS    CL0004              ZW1517                                       
K1069    DS    CL0004              ZW1820                                       
K1073    DS    CL0004              ZW2124                                       
K1077    DS    CL0004              ZW2529                                       
K1081    DS    CL0004              ZW3034                                       
K1085    DS    CL0004              ZW3539                                       
K1089    DS    CL0004              ZW4044                                       
K1093    DS    CL0004              ZW4549                                       
K1097    DS    CL0004              ZW5054                                       
K1101    DS    CL0004              ZW5564                                       
K1105    DS    CL0004              ZW65+                                        
K1109    DS    CL0004              ZM25                                         
K1113    DS    CL0004              ZM68                                         
K1117    DS    CL0004              ZM911                                        
K1121    DS    CL0004              ZM1214                                       
K1125    DS    CL0004              ZM1517                                       
K1129    DS    CL0004              ZM1820                                       
K1133    DS    CL0004              ZM2124                                       
K1137    DS    CL0004              ZM2529                                       
K1141    DS    CL0004              ZM3034                                       
K1145    DS    CL0004              ZM3539                                       
K1149    DS    CL0004              ZM4044                                       
K1153    DS    CL0004              ZM4549                                       
K1157    DS    CL0004              ZM5054                                       
K1161    DS    CL0004              ZM5564                                       
K1165    DS    CL0004              ZM65+                                        
K1169    DS    CL0004              ZWMOMS                                       
K1173    DS    CL0004              ZWW1820                                      
K1177    DS    CL0004              ZWW2124                                      
K1181    DS    CL0004              ZWW2534                                      
K1185    DS    CL0004              ZWW3544                                      
K1189    DS    CL0004              ZWW4549                                      
K1193    DS    CL0004              ZWW5054                                      
K1197    DS    CL0004              ZWW55+                                       
K1201    DS    CL0004              WW25                                         
K1205    DS    CL0004              WW68                                         
K1209    DS    CL0004              WW911                                        
K1213    DS    CL0004              WW1214                                       
K1217    DS    CL0004              WW1517                                       
K1221    DS    CL0004              WW1820                                       
K1225    DS    CL0004              WW2124                                       
K1229    DS    CL0004              WW2529                                       
K1233    DS    CL0004              WW3034                                       
K1237    DS    CL0004              WW3539                                       
K1241    DS    CL0004              WW4044                                       
K1245    DS    CL0004              WW4549                                       
K1249    DS    CL0004              WW5054                                       
K1253    DS    CL0004              WW5564                                       
K1257    DS    CL0004              WW65+                                        
K1261    DS    CL0004              WM25                                         
K1265    DS    CL0004              WM68                                         
K1269    DS    CL0004              WM911                                        
K1273    DS    CL0004              WM1214                                       
K1277    DS    CL0004              WM1517                                       
K1281    DS    CL0004              WM1820                                       
K1285    DS    CL0004              WM2124                                       
K1289    DS    CL0004              WM2529                                       
K1293    DS    CL0004              WM3034                                       
K1297    DS    CL0004              WM3539                                       
K1301    DS    CL0004              WM4044                                       
K1305    DS    CL0004              WM4549                                       
K1309    DS    CL0004              WM5054                                       
K1313    DS    CL0004              WM5564                                       
K1317    DS    CL0004              WM65+                                        
K1321    DS    CL0004              WWMOMS                                       
K1325    DS    CL0004              WWW1820                                      
K1329    DS    CL0004              WWW2124                                      
K1333    DS    CL0004              WWW2534                                      
K1337    DS    CL0004              WWW3544                                      
K1341    DS    CL0004              WWW4549                                      
K1345    DS    CL0004              WWW5054                                      
K1349    DS    CL0004              WWW55+                                       
K1353    DS    CL0004              BW25                                         
K1357    DS    CL0004              BW68                                         
K1361    DS    CL0004              BW911                                        
K1365    DS    CL0004              BW1214                                       
K1369    DS    CL0004              BW1517                                       
K1373    DS    CL0004              BW1820                                       
K1377    DS    CL0004              BW2124                                       
K1381    DS    CL0004              BW2529                                       
K1385    DS    CL0004              BW3034                                       
K1389    DS    CL0004              BW3539                                       
K1393    DS    CL0004              BW4044                                       
K1397    DS    CL0004              BW4549                                       
K1401    DS    CL0004              BW5054                                       
K1405    DS    CL0004              BW5564                                       
K1409    DS    CL0004              BW65+                                        
K1413    DS    CL0004              BM25                                         
K1417    DS    CL0004              BM68                                         
K1421    DS    CL0004              BM911                                        
K1425    DS    CL0004              BM1214                                       
K1429    DS    CL0004              BM1517                                       
K1433    DS    CL0004              BM1820                                       
K1437    DS    CL0004              BM2124                                       
K1441    DS    CL0004              BM2529                                       
K1445    DS    CL0004              BM3034                                       
K1449    DS    CL0004              BM3539                                       
K1453    DS    CL0004              BM4044                                       
K1457    DS    CL0004              BM4549                                       
K1461    DS    CL0004              BM5054                                       
K1465    DS    CL0004              BM5564                                       
K1469    DS    CL0004              BM65+                                        
K1473    DS    CL0004              BWMOMS                                       
K1477    DS    CL0004              BWW1820                                      
K1481    DS    CL0004              BWW2124                                      
K1485    DS    CL0004              BWW2534                                      
K1489    DS    CL0004              BWW3544                                      
K1493    DS    CL0004              BWW4549                                      
K1497    DS    CL0004              BWW5054                                      
K1501    DS    CL0004              BWW55+                                       
K1505    DS    CL0004              MW18+                                        
K1509    DS    CL0004              MW1849                                       
K1513    DS    CL0004              MM18+                                        
K1517    DS    CL0004              MW2554                                       
K1521    DS    CL0004              MWWRK                                        
K1525    DS    CL0004              MW1217                                       
K1529    DS    CL0004              MM1849                                       
K1533    DS    CL0004              MM2554                                       
K1537    DS    CL0004              MM55+                                        
K1541    DS    CL0004              MW2549                                       
K1545    DS    CL0004              MW55+                                        
K1549    DS    CL0004              MWW1849                                      
K1553    DS    CL0004              MM1217                                       
K1557    DS    CL0004              MM2549                                       
K1561    DS    CL0004              MV2+                                         
K1565    DS    CL0004              MW1834                                       
K1569    DS    CL0004              MM1834                                       
K1573    DS    CL0004              MWMOMS                                       
K1577    DS    CL0004              MM2149                                       
K1581    DS    CL0004              MM3564                                       
K1585    DS    CL0004              MW21+                                        
K1589    DS    CL0004              MW3564                                       
K1593    DS    CL0004              MM2154                                       
K1597    DS    CL0004              MM21+                                        
K1601    DS    CL0004              MW2149                                       
K1605    DS    CL0004              MW2154                                       
K1609    DS    CL0004              MWW2554                                      
K1613    DS    CL0004              MV611                                        
K1617    DS    CL0004              MV1217                                       
K1621    DS    CL0004              MV211                                        
K1625    DS    CL0004              MW211                                        
K1629    DS    CL0004              MM211                                        
K1633    DS    CL0004              MW611                                        
K1637    DS    CL0004              MM611                                        
K1641    DS    CL0004              MV1214                                       
K1645    DS    CL0004              MV25                                         
K1649    DS    CL0004              MV1517                                       
K1653    DS    CL0004              MW1824                                       
K1657    DS    CL0004              MW1524                                       
K1661    DS    CL0004              MM1824                                       
K1665    DS    CL0004              MM1524                                       
K1669    DS    CL0004              MV68                                         
K1673    DS    CL0004              MV911                                        
K1677    DS    CL0004              MHWC18                                       
K1681    DS    CL0004              MHWC12                                       
K1685    DS    CL0004              MHWC6                                        
K1689    DS    CL0004              LHOMES                                       
K1693    DS    CL0004              NHOMES                                       
K1697    DS    CL0004              FN02                                         
K1701    DS    CL0004              FN03                                         
K1705    DS    CL0004              FN04                                         
K1709    DS    CL0004              FN05                                         
K1713    DS    CL0004              FN06                                         
K1717    DS    CL0004              FN07                                         
K1721    DS    CL0004              FN08                                         
K1725    DS    CL0004              MV18+                                        
K1729    DS    CL0004              MV1834                                       
K1733    DS    CL0004              MV1849                                       
K1737    DS    CL0004              MV2549                                       
K1741    DS    CL0004              MV2554                                       
K1745    DS    CL0004              MV3564                                       
K1749    DS    CL0004              MV5564                                       
K1753    DS    CL0004              MV55+                                        
K1757    DS    CL0004              MV1824                                       
K1761    DS    CL0004              MV2149                                       
K1765    DS    CL0004              MV2154                                       
K1769    DS    CL0004              MV21+                                        
K1773    DS    CL0004              MV1524                                       
K1777    DS    CL0004              LHWC18                                       
K1781    DS    CL0004              LHWC12                                       
K1785    DS    CL0004              LHWC6                                        
K1789    DS    CL0004              NHWC18                                       
K1793    DS    CL0004              NHWC12                                       
K1797    DS    CL0004              NHWC6                                        
         EJECT                                                                  
* DEDEMTABD                                                                     
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083DEND09I   09/30/15'                                      
         END                                                                    
