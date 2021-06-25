*          DATA SET PPCON10    AT LEVEL 028 AS OF 11/05/03                      
*PHASE T40D10A                                                                  
*INCLUDE NUMED                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40D10 - PRINTPAK CONTRACT ADD/CHANGE'                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 10/15/01 BROWSE FUNCTION FOR SPACE DESCRIPTION                           
*                                                                               
* KWAN 09/18/01 ADD/CHG SPACE DESCRIPTION (USE STD SPC DESP REC, X'5A')         
*                                                                               
* KWAN  12/99  REVISE CODES FOR TEL AND FAX FIELDS                              
*                                                                               
* KWAN  08/99  ADD CODES FOR NEW FIELDS (TEL AND FAX)                           
*                                                                               
* BPLA   5/99  FIX CURSOR BUG IN EDT150                                         
*                                                                               
* BPLA  10/98  ALLOW FOR NEWPAPER RATES BY PRODUCT (IN PRBOPEN)                 
*              IF PRBOPEN WAS P'0'. PRBOPEN IS                                  
*              CURRENTLY NOT P'0' ONLY WHEN RATE IS NOT ENTERED                 
*                                                                               
* BPLA  6/98   ALLOW PRODUCT RATES FOR MASTER CONTRACTS                         
*              (DON'T ATTEMPT TO READ PRODUCT)                                  
*                                                                               
* BPLA  12/97  CHANGE TO ALLOW SHOWINGS UP TO 999 (FOR GRP'S)                   
*                                                                               
* SMYE 3/97    DISALLOW NEGATIVE ENTRY IN PCATPCT (CASH DISCOUNT LEVEL)         
*                                                                               
* BPLA 1/96    ALLOW "N" BEFORE LEVEL (FOR NET $) IF PRBLIND IS "$"             
*              THEN CHANGE PRBLIND TO "N".                                      
*                                                                               
* SMYE 12/6/95 CHANGED VDTCNV TO VDATCON WITH "NEW" PARAM'S.                    
*                                                                               
* BPLA 3/95    COPIED T40D10 (LEVEL 15 3/16/95)                                 
*              CHANGES FOR INCH RATES                                           
*                                                                               
* BPLA 3/15/95 CHECK DATES VS SPUBKILL                                          
*                                                                               
* BPLA 1/13/94 ALLOW LEVEL = $ FOR RATE CODES IF RATE IS NOT                    
*              BEING LOOKED-UP                                                  
*                                                                               
* BPLA 6/4/93  CHANGES FOR MAX/ISSUE ACROSS ZONES/EDTS                          
*              AND CONTRACT UNITS FOR NON-NEWSPAPERS                            
* BPLA 6/4/93  COPIED FROM PPCON10                                              
*                                                                               
* BPLA 10/21/92  CHANGES FOR NEW AOR FEATURES                                   
*                                                                               
* BPLA 2/21/92  ADD MAX/ISSUE AND ADV LEVEL AND RATE LOOK-UP                    
*                                                                               
* BPLA 1/24/92  INCLUDE PPCONWRKA (HAS ADVERTISER DATA)                         
*                                                                               
* BPLA 10/23/91 IF LEVEL IND MISSING - DISALLOW INPUT IN OTHER RATE             
*               FIELDS                                                          
* ROSA 4/11/90 *BUG01** PGM UNABLE TO ELIMINATE ANY CONTRACT CD BUG01           
*              OVERRIDES.  EG CANNOT CHANGE 2.00CD TO DEFAULT   BUG01           
*              TO PUB OVERRIDE. IF INPUT IS 00.0, X'FFFF' IS                    
*              ENTERED INTO CD TO INDICATE NO CD. NOW IF NOTHING                
*              IS INPUT THE CD WILL BE BIN 0000.                BUG01           
* ROSA 6/2/88  ALLOW FOR C RATES// COMMISSION ONLY                  L03         
*                                                                   L03         
* ROSA 5/26/88 ALLOW ENTERING S RATES // S FOLLOWED BY RATE         L02         
*                   NO AGENCY COMMISSION                            L02         
*                                                                   L01         
* ROSA 4/28/88 ALLOW ENTERING NET RATES // N FOLLOWED BY RATE       L01         
*                                                                   L01         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
T40D10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40D10                                                         
         LA    R6,4095(RB)                                                      
         LA    R6,1(R6)                                                         
         USING T40D10+4096,R6                                                   
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
         USING T40DFFD,RA                                                       
*                                                                               
         RELOC RELO10                                                           
*                                                                               
         FOUT  KBAPAGH,SPACES,2                                                 
         XC    NEXTNUM,NEXTNUM                                                  
         CLC   KBAACT(3),=C'ADD'                                                
         BE    CON100                                                           
*              GET K FOR CHANGE                                                 
         LA    R2,KBANUMH                                                       
         LA    R3,12               INVALID ACTION                               
         TM    4(R2),X'20'         DID USER CHANGE CONTRACT NUMBER?             
         BZ    ERROR                                                            
*                                                                               
*                                                                               
         MVC   KEY+27(4),SAVKKEY+27   SAVE K DISK ADDR                          
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON10                                                            
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON10                                                            
         TM    SADVDATA+15,X'20'    OR IF I HAVE MY OWN CONTRACTS               
         BNO   CON10                                                            
*                                                                               
*        MUST SWITCH TO AOR                                                     
*        TO READ CONTRACT                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    CON10                                                            
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
CON10    DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON12                                                            
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON12                                                            
         TM    SADVDATA+15,X'20'    OR IF I HAVE MY OWN CONTRACTS               
         BNO   CON12                                                            
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CON12    DS    0H                                                               
*                                                                               
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         MVC   DMWORK2,DMWORK                                                   
         MVC   SAVDATES,PCONSDT    SAVE K DATES                                 
         CLC   KBAACT(4),=C'LOCK'                                               
         BNE   CON20                                                            
         OI    PCONLIND,X'80'                                                   
         B     CON40                                                            
*                                                                               
CON20    CLC   KBAACT(6),=C'UNLOCK'                                             
         BNE   CON50               MUST BE CHA                                  
         NI    PCONLIND,X'7F'      SET OFF X'80'                                
CON40    CLC   PCONMOD,TODAY                                                    
         BE    CON45                                                            
         SR    RE,RE                                                            
         IC    RE,PCONMODN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PCONMODN                                                      
         MVC   PCONMOD,TODAY                                                    
CON45    B     EXXMOD                                                           
*                                                                               
CON50    CLC   PCONMOD,TODAY                                                    
         BE    EDITCON                                                          
         SR    RE,RE                                                            
         IC    RE,PCONMODN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PCONMODN                                                      
         MVC   PCONMOD,TODAY                                                    
         B     EDITCON                                                          
* MUST BE ADD                                                                   
CON100   DS    0H                                                               
         LA    R2,KBANUMH          CHK FOR INPUT IN NUMBER                      
         CLI   5(R2),0                                                          
         BE    CON110              IF MISSING - NEXT AVAILABLE IS USED          
         LA    R3,2                INVALID INPUT                                
         CLI   5(R2),3             MAX LENGHT FOR CONTRACT                      
         BH    ERROR                                                            
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*              BUILD K KEY                                                      
         LA    R3,2                INVALID                                      
         CHI   R0,999              MAX CONTRACT NUMBER IS 999                   
         BH    ERROR                                                            
         STH   R0,HALF                                                          
         LA    R3,52               DUPLICATE KEY ON ADD                         
         MVC   PCONKEY(13),SAVKKEY                                              
         MVC   PCONNUM,HALF                                                     
         MVC   KEY,PCONKEY                                                      
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ERROR                                                            
         LH    R2,HALF                                                          
         BCTR  R2,0                MUST DECREMENT BY ONE                        
         STH   R2,HALF                                                          
         MVC   NEXTNUM,HALF        SAVE AS NEXT AVAIL CON NUM                   
*                                                                               
CON110   MVI   PCONLEN+1,70        REC LEN (33 + 37)                            
         MVC   PCONELEM(2),=X'1025'     ELEM CODE + LEN                         
*              TURN OFF VALID BITS IN CASE PREVIOUS ADD ERROR                   
         LA    R2,CONSDTH          FIRST K FIELD                                
         SR    RE,RE                                                            
         NI    4(R2),X'DF'                                                      
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BNE   *-16                                                             
         MVC   PCONKEY(13),SAVKKEY                                              
         MVI   PCONMODN,0          SET MOD NO. TO 0                             
         MVC   PCONPRD,SAVPRD                                                   
         MVC   PCONMOD,TODAY                                                    
         EJECT                                                                  
*              VALIDATE START-END DATES                                         
EDITCON  DS    0H                                                               
         LA    R2,CONSPRH               AUTO SPACE RES FIELD MUST BE            
         CLI   CONSPR,C'*'              HOLD, MAKE NO CHANGES                   
         BE    EDITCTF                                             L04          
*                                                                  L04          
*        TM    TWAKIND,3               MIXTURE OF HIGH OR LOW RATESL04          
*        BZ    CLICONSP                                            L04          
*IGHLOW  FOUT  RATDESCH                                            L04          
*        MVC   RATDESC+50(28),=C'/ NO AUTO SPACE RES REQSTD /' L04              
*        B     EDITCTF                                             L04          
*                                                                  L04          
         CLI   CONSPR,C'Y'              YES, AUTO SPACE RESV                    
         BE    EDSPR5                                                           
         CLI   CONSPR,C'N'              NO, SO DELETE ANY EXISTING ELM          
         BE    EDSPRDEL                                                         
         CLI   5(R2),0                  NO INPUT, DEFAULT TO *                  
         BE    EDITCTF                                                          
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
EDSPRDEL GOTO1 VDELELEM,DMCB,(X'85',PCONREC)   IF N DELETE ANY X'85' EL         
         B     EDITCTF                                                          
*                                                                               
EDSPR5   GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAY)                                   
         LA    R4,PCONREC+33                                                    
EDSPR10  CLI   0(R4),X'00'                 IF Y DOES X'85' EL ALREADY           
         BE    EDSPR40                     EXIST                                
         CLI   0(R4),X'85'                                                      
         BE    EDSPR20                                                          
EDSPR15  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     EDSPR10                                                          
*                                                                               
         USING PASRELEM,R4                                                      
EDSPR20  CLC   PASRCLT,SPACES                                                   
         BH    EDSPR15                                                          
         CLC   SVTODAY,PASRCDAT            IS CURRENT DATE = TODAY              
         BE    EDITCTF                                                          
         MVC   PASRLDAT,PASRCDAT           IF NOT MOVE CURRENT DATE TO          
         MVC   PASRCDAT,SVTODAY            LAST DATE AND TODAY TO               
         B     EDITCTF                     CURRENT DATE                         
*                                                                               
EDSPR40  LA    R4,WORK                     IF NO X'85' EL EXISTS                
         XC    WORK,WORK                   ADD NEW ONE WITH TODAY IN            
         CLC   KBAACT(3),=C'ADD'           IF ADDING NEW CONTRACT PUT           
         BE    EDSPR42                     NO DATES IN X'85' EL.                
         MVC   PASRCDAT,SVTODAY                                                 
EDSPR42  MVC   WORK(2),=X'850D'            CURRENT DATE                         
         GOTO1 VADDELEM,DMCB,PCONREC,0(R4)                                      
*                                                                               
*                                                                               
*                                                                               
EDITCTF  DS    0H                  EDITING CONTRACT TEL & FAX                   
         LA    R4,PCONREC+33                                                    
EDCTF10  CLI   0(R4),X'55'         TEL & FAX ELEM ALREADY EXIST?                
         BE    EDCTF30             EXIST                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'00'                                                      
         BNE   EDCTF10                                                          
         B     EDCTF50             ELEM NOT FOUND, BUILD NEW ONE                
*                                                                               
EDCTF30  GOTO1 VDELELEM,DMCB,(X'55',PCONREC)                                    
*                                                                               
EDCTF50  DS    0H                                                               
         XC    WORK,WORK           BUILD NEW PCTFELEM (X'55')                   
         LA    R4,WORK                                                          
         USING PCTFELEM,R4                                                      
*                                                                               
         MVI   0(R4),X'55'         ELEM CODE                                    
         MVI   PCTFLEN,34          ELEM LENGTH                                  
*                                                                               
         LA    R2,CONTELH                                                       
         LA    R3,2                FIELD INVALID ERROR MSG                      
         CLI   5(R2),0                                                          
         BE    EDCTF60                                                          
         CLC   =C'NONE',8(R2)                                                   
         BNE   EDCTF55H                                                         
         CLI   5(R2),4             "NONE" IS 4 CHARS                            
         BNE   ERROR                                                            
         MVC   PCONTELE(4),8(R2)                                                
         B     EDCTF55V                                                         
EDCTF55H CLC   =C'DEFAULT',8(R2)                                                
         BNE   EDCTF55M                                                         
         CLI   5(R2),7             "DEFAULT" IS 7 CHARS                         
         BNE   ERROR                                                            
         MVC   PCONTELE(7),8(R2)                                                
         B     EDCTF55V                                                         
EDCTF55M ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCONTELE(0),8(R2)                                                
EDCTF55V CLI   CONFAXH+5,0         FAX FIELD MUST ALSO BE PRESENT               
         BNE   EDCTF60                                                          
         LA    R2,CONFAXH                                                       
         LA    R3,1                MISSING INPUT ERROR MSG                      
         B     ERROR                                                            
*                                                                               
EDCTF60  DS    0H                                                               
         LA    R2,CONFAXH                                                       
         LA    R3,2                FIELD INVALID ERROR MSG                      
         CLI   5(R2),0                                                          
         BE    EDCTF70                                                          
         CLC   =C'NONE',8(R2)                                                   
         BNE   EDCTF65H                                                         
         CLI   5(R2),4             "NONE" IS 4 CHARS                            
         BNE   ERROR                                                            
         MVC   PCONFAX(4),8(R2)                                                 
         B     EDCTF65V                                                         
EDCTF65H CLC   =C'DEFAULT',8(R2)                                                
         BNE   EDCTF65M                                                         
         CLI   5(R2),7             "DEFAULT" IS 7 CHARS                         
         BNE   ERROR                                                            
         MVC   PCONFAX(7),8(R2)                                                 
         B     EDCTF65V                                                         
EDCTF65M ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCONFAX(0),8(R2)                                                 
EDCTF65V CLI   CONTELH+5,0         FAX FIELD MUST ALSO BE PRESENT               
         BNE   EDCTF70                                                          
         LA    R2,CONTELH                                                       
         LA    R3,1                MISSING INPUT ERROR MSG                      
         B     ERROR                                                            
*                                                                               
EDCTF70  OC    WORK+2(12),WORK+2   ANYTHING IN THERE?                           
         BZ    EDITATN             NO, EDIT NEXT ITEM                           
*                                                                               
         GOTO1 VADDELEM,DMCB,PCONREC,0(R4)                                      
*                                                                               
*                                                                               
*                                                                               
EDITATN  DS    0H                                                               
         LA    R4,PCONREC+33                                                    
EDATN10  CLI   0(R4),X'50'         DOES ATTENTION OF ELEMENT ALREADY            
         BE    EDATN30             EXIST                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'00'                                                      
         BNE   EDATN10                                                          
*                                                                               
EDATN20  DS    0H                                                               
         LA    R2,CONATTNH                                                      
         XC    WORK,WORK                BUILD NEW PCATELE X'50' EL              
         LA    R4,WORK                                                          
         USING PCATELEM,R4                                                      
         MVC   WORK(2),=X'502A'                                                 
         CLI   5(R2),0                                                          
         BE    EDATN25                                                          
         MVC   PCATNAM,CONATTN          ATTENTION OF FIELD                      
********************************************************************            
* IF NOTHING ENTERED IN CD OVERRIDE FIELD ON CON SCREEN ,PCATPCT   *            
* IN X'50' EL WILL BE LEFT X'0000'.  IF 0.0 ENTERED WILL BE SET TO *            
* X'FFFF'.  DONE THIS WAY SO CHECK AT EDATN28 WILL WORK AND        *            
* WILL ELIMINATE ADDING ELEMENT IF NO DATA TO ADD.                 *            
********************************************************************            
EDATN25  LA    R2,CONCDPH                                                       
         CLI   5(R2),0                                                          
         BE    EDATN28                  CONVERT PERCENT TO BINARY               
         SR    R8,R8                                                            
         IC    R8,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(1,8(R2)),(R8)                                     
         OI    DMCB,0                                                           
         BNZ   EDATERR                                                          
         CLI   DMCB+6,X'80'        NEGATIVE ?                                   
         BNL   EDATERR             YES - ERROR                                  
         OC    DMCB+6(2),DMCB+6                                                 
         BNZ   EDATN27                                                          
         MVC   PCATPCT,=2X'FF'                                                  
         B     EDATN28                                                          
EDATN27  MVC   PCATPCT,DMCB+6                                                   
*                                                                               
EDATN28  DS    0H                                                               
         LA    R2,CONMAXH                                                       
         CLI   5(R2),0                                                          
         BE    EDATN28D                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                FIELD NON NUMERIC OR ZERO                   
         BZ    EDATERR              IS INVALID                                  
         STH   R0,HALF                                                          
         MVC   PCATMAX,HALF                                                     
*                                                                               
EDATN28D DS    0H                                                               
         LA    R2,CONMIZEH       ACROSS ZONES/EDITIONS                          
         CLI   5(R2),0                                                          
         BE    EDATN29                                                          
         BAS   RE,PACK                                                          
         LTR   R0,R0                FIELD NON NUMERIC OR ZERO                   
         BZ    EDATERR              IS INVALID                                  
         STH   R0,HALF                                                          
         MVC   PCATMAXZ,HALF                                                    
*                                                                               
EDATN29  OC    WORK+2(40),WORK+2       CHECK FOR ANY DATA IN ELEM               
         BZ    EDITSDT                                                          
         GOTO1 VADDELEM,DMCB,PCONREC,0(R4)                                      
         B     EDITSDT                                                          
*                                                                               
EDATERR  LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
EDATN30  MVC   PCATNAM,CONATTN                                                  
         LA    R2,CONCDPH                                                       
         CLI   5(R2),0         WAS ANYTHING INPUT IN CD OVERRIDE                
         BNE   EDATN3A                                            BUG01         
         XC    PCATPCT,PCATPCT     CLEAR ANY PREVIOUS NUMBER      BUG01         
         B     EDATN50                                            BUG01         
EDATN3A  SR    R8,R8                                                            
         IC    R8,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(1,8(R2)),(R8)                                     
         OI    DMCB,0                                                           
         BNZ   EDATERR                                                          
         CLI   DMCB+6,X'80'        NEGATIVE ?                                   
         BNL   EDATERR             YES - ERROR                                  
         OC    DMCB+6(2),DMCB+6                                                 
         BNZ   EDATN40                                                          
         MVC   PCATPCT,=2X'FF'                                                  
         B     EDATN50                                                          
EDATN40  MVC   PCATPCT,DMCB+6                                                   
*                                                                               
EDATN50  LA    R2,CONMAXH                                                       
         XC    PCATMAX,PCATMAX                                                  
         CLI   5(R2),0                                                          
         BE    EDATN50D                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    EDATERR                                                          
         STH   R0,HALF                                                          
         MVC   PCATMAX,HALF                                                     
*                                                                               
EDATN50D DS    0H                                                               
         LA    R2,CONMIZEH       ACROSS ZONES/EDITIONS                          
         XC    PCATMAXZ,PCATMAXZ                                                
         CLI   5(R2),0                                                          
         BE    EDATN50G                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    EDATERR                                                          
         STH   R0,HALF                                                          
         MVC   PCATMAXZ,HALF                                                    
*                                                                               
EDATN50G DS    0H                                                               
*                                                                               
EDITSDT  LA    R2,CONSDTH                                                       
         LA    R3,20               INVALID DATE                                 
         TM    4(R2),X'20'                                                      
         BO    EDITEDT                                                          
         GOTO1 VDATVAL,DMCB,CONSDT,DUB       START DATE                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         NI    CONEDTH+4,X'DF'     END DATE - REVALIDATE                        
*                                                                               
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PCONSDT)                                 
*                                                                               
         OC    SPUBKILL,SPUBKILL      SEE IF I HAVE A PUB KILL DATE             
         BZ    EDITS5                                                           
*                                                                               
         CLC   PCONSDT,SPUBKILL                                                 
         BNH   EDITS5                                                           
         LA    R3,217          PAST KILL DATE                                   
         B     ERROR                                                            
*                                                                               
EDITS5   DS    0H                                                               
*                                                                               
*                                                                               
*              VALIDATE END DATE                                                
EDITEDT  LA    R2,CONEDTH                                                       
         LA    R3,20                                                            
         TM    4(R2),X'20'                                                      
         BO    EDITREV                                                          
*              EDIT END DATE                                                    
         GOTO1 VDATVAL,DMCB,CONEDT,DUB                                          
*                                                                               
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PCONEDT)                                 
*                                                                               
         OC    SPUBKILL,SPUBKILL      SEE IF I HAVE A KILL DATE                 
         BZ    EDITE5                                                           
*                                                                               
         CLC   PCONEDT,SPUBKILL                                                 
         BNH   EDITE5                                                           
         LA    R3,217          PAST KILL DATE                                   
         B     ERROR                                                            
*                                                                               
EDITE5   DS    0H                                                               
         LA    R3,80               START DATE LATER THAN END                    
         CLC   PCONSDT,PCONEDT                                                  
         BH    ERROR                                                            
*              TEST FOR K DATE OVERLAP                                          
         CLC   KBAACT(3),=C'CHA'                                                
         BNE   *+14                                                             
         CLC   PCONSDT(6),SAVDATES                                              
         BE    EDT100                                                           
*                                  NEXTNUM WAS CLEARED AT START                 
         MVC   KEY(13),SAVKKEY     K KEY                                        
         XC    KEY+13(19),KEY+13                                                
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         BAS   RE,HIGH             PRTDIR                                       
         B     *+8                                                              
NEXTK    BAS   RE,SEQ                                                           
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDT100                                                           
         CLC   KBAACT(3),=C'CHA'                                                
         BNE   *+14                                                             
         CLC   KEY+13(2),PCONNUM   SAME K FOR CHANGE                            
         BE    NEXTK                                                            
         CLI   KBANUMH+5,0         SEE IF CONTRACT NUMBER INPUT                 
         BNE   NEXTK1              YES - LEAVE NEXTNUM ALONE                    
         MVC   HALF,NEXTNUM        NEXTNUM STARTS AS 0                          
         LH    R1,HALF             WILL SET TO NEXT AVAILABLE NUMBER            
         LA    R1,1(R1)            MINUS ONE                                    
         STH   R1,HALF                                                          
         CLC   HALF,KEY+13                                                      
         BNE   NEXTK1                                                           
         MVC   NEXTNUM,KEY+13      SAVE K NUMBER FOR ADD                        
         CLC   NEXTNUM,=H'999'     999 MAX CONTRACT NUMBER                      
         BL    *+6                                                              
         DC    H'0'                DIE - SHOULD NEVER HAPPEN                    
*                                  NEXTNUM HAS FIRST AVAILABLE NUMBER           
*                                  MINUS ONE                                    
NEXTK1   TM    KEY+25,X'80'        DELETED?                                     
         BO    NEXTK                                                            
         MVC   AREC,APUBIO         READ INTO PUBIO                              
         BAS   RE,GETREC           PRTFILE - K                                  
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         L     R5,APUBIO                                                        
         MVI   0(R5),0                                                          
         CLI   50(R5),C'A'      IS THIS A PRD-CONTRACT                          
         BL    NEXTK2              NO - MUST CHECK FOR OVERLAP                  
         CLI   SAVPRD,0                                                         
         BE    NEXTK2                                                           
         CLC   SAVPRD,50(R5)          MATCH PRDS                                
         BNE   NEXTK               CHECK FOR OVERLAP ON MATCH                   
*                                                                               
NEXTK2   DS    0H                                                               
         LA    R3,49               K DATE OVERLAP                               
         CLC   PCONSDT,38(R5)   NEW K START V EXISTING K END                    
         BH    NEXTK                                                            
         CLC   PCONEDT,35(R5)   NEW K END V EXISTING K START                    
         BL    NEXTK                                                            
         B     ERROR                                                            
*              TEST IF K PERIOD GREATER THAN YEAR                               
EDT100   LA    R3,20               INVALID DATE                                 
         NI    DMINBTS,X'F7'       ELIM. PASS DELETES                           
         B     EDT110              ** NOP YEAR TEST **                          
         GOTO1 VDATCON,DMCB,(3,PCONSDT),WORK                                    
         GOTO1 VADDAY,DMCB,WORK,WORK+6,366                                      
         CLC   DUB(6),WORK+6       K END V K START + 366 DAYS                   
         BH    ERROR                                                            
* IF CHANGE CHECK FOR NAKED BUYS                                                
EDT110   DS    0H                                                               
         CLC   KBAACT(3),=C'CHA'                                                
         BNE   EDITREV                                                          
         CLI   SAVCLTPR+12,C'N'    CONTRACTS NOT REQUIRED                       
         BE    EDITREV             - DONT CHECK FOR NAKED BUYS                  
*                                                                               
*                                                                               
         CLC   PCONSDT,SAVDATES    NEW V OLD START DATE                         
         BH    *+14                                                             
         CLC   PCONEDT,SAVDATES+3  NEW END V OLD END DATE                       
         BNL   EDITREV                                                          
* CHECK BUYS TO SEE IF ANY UNCOVERED                                            
         XC    IOAREA(32),IOAREA                                                
         MVC   PBUYKAGY,AGYALPHA                                                
         MVC   PBUYKMED,KBAMED     MEDIA                                        
         MVI   PBUYKRCD,X'21'      CLIENT MODE POINTER                          
         MVC   PBUYKCLT,SAVKKEY+4  CLIENT                                       
         MVC   PBUYKPRD(6),SAVKKEY+7   PUB NUMBER                               
         MVC   KEY,IOAREA                                                       
         BAS   RE,HIGH             GET FIRST BUY RECORD                         
         B     *+8                                                              
*                                                                               
EDT150   BAS   RE,SEQ              NEXT BUY RECORD                              
         CLC   KEY(13),KEYSAVE     SAME CLT-PUB?                                
         BNE   EDITREV                                                          
* CHECK AGAINST NEW K START                                                     
         LA    R3,66               START DATE CANNOT BE ADVANCED                
         CLC   KEY+16(3),SAVDATES  BUY V OLD K START DATE                       
         BL    *+14                                                             
         CLC   KEY+16(3),PCONSDT   BUY V NEW K START                            
         BL    STERR                                                            
* CHECK BUY AGAINST NEW K END DATE                                              
         LA    R3,67               END DATE CANNOT BE CUT BACK                  
         CLC   KEY+16(3),SAVDATES+3   BUY V OLD K END DATE                      
         BH    EDT150                                                           
         CLC   KEY+16(3),PCONEDT   BUY V NEW K END DATE                         
         BH    ENDERR                                                           
         B     EDT150                                                           
*                                                                               
STERR    LA    R2,CONSDTH          CURSOR TO START                              
         B     ERROR                                                            
*                                                                               
ENDERR   LA    R2,CONEDTH          CURSOR TO END DATE                           
         B     ERROR                                                            
         EJECT                                                                  
*              EDIT REVISION DATE                                               
EDITREV  LA    R2,CONREQH                                                       
         TM    4(R2),X'20'                                                      
         BO    EDITRV                                                           
* MOVE AUTHORIZED SIGNATURE                                                     
         BAS   RE,MOVE                                                          
         MVC   PCONREQ,WORK                                                     
EDITRV   LA    R2,CONREVH                                                       
         LA    R3,20               INVALID DATE                                 
         TM    4(R2),X'20'                                                      
         BO    EDITCV                                                           
         XC    PCONREV,PCONREV                                                  
         CLI   5(R2),0                                                          
         BE    EDITCV                                                           
         GOTO1 VDATVAL,DMCB,CONREV,DUB                                          
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PCONREV)                                 
*                                                                               
*              VALIDATE CONTRIBUTING VOLUME                                     
EDITCV   LA    R2,CONCONH                                                       
         LA    R3,2                INVALID INPUT FIELD                          
         TM    4(R2),X'20'                                                      
         BO    EDITTYP                                                          
         ZAP   PCONCON,=P'0'                                                    
         CLI   5(R2),0                                                          
         BE    EDITTYP                                                          
*                                                                               
         SR    R8,R8                                                            
         IC    R8,5(R2)            INPUT LENGTH                                 
         GOTO1 VCASHVAL,DMCB,CONCON,(R8)                                        
         CLI   DMCB,0              ERROR?                                       
         BNE   ERROR                                                            
         L     R1,DMCB+4           CONTRIB VOLUME                               
         LTR   R1,R1                                                            
         BM    ERROR                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'          ELIM DECIMALS                                
         CVD   R1,DUB                                                           
*                                                                               
         MVC   PCONCON,DUB+3                                                    
*                                                                               
*              VALIDATE TYPE                                                    
EDITTYP  LA    R2,CONTYPH                                                       
         LA    R3,2                                                             
         TM    4(R2),X'20'                                                      
         BO    EDITRATE                                                         
         MVC   PCONTYP,CONTYP                                                   
         CLI   8(R2),C'M'          MASTER?                                      
         BE    EDITRATE                                                         
         CLI   8(R2),C'R'          RESERVATION                                  
         BE    EDITRATE                                                         
         CLI   8(R2),X'40'                                                      
         BE    EDITRATE                                                         
         CLI   8(R2),0                                                          
         BNE   ERROR                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40D10 - PRINTPAK CONTRACT RATE BASIS LINE EDIT'                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDITRATE LA    R2,CONLI1H          FIRST RATE BASIS FIELD                       
         LA    R3,CONED4H          LAST FIELD                                   
         SR    RE,RE                                                            
RATE50   TM    4(R2),X'20'         CHANGED?                                     
         BZ    RATE100                                                          
         IC    RE,0(R2)            LEN                                          
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BH    EDITSTD             NO CHANGE IF HIGH                            
         B     RATE50                                                           
*                                                                               
* EDIT RATE BASIS FIELDS - BUILD ELEMENTS                                       
*                                                                               
RATE100  LA    R2,CONLI1H          FIRST FIELD                                  
         LA    R3,2                INVALID INPUT ERROR                          
         XC    HALF2,HALF2         USED TO COUNT RATES                          
         MVI   AORCFND,0           INITIALIZE AOR CONTRACT SWITCH               
         SR    R8,R8                                                            
*                                                                               
* CAN CHANGE RATES ONLY IF THERE ARE LESS THAN 5 ELEMS                          
*                                                                               
         LA    R4,PCONREC+33                                                    
         ZAP   DUB,=P'0'                                                        
RATE100A CLI   0(R4),X'00'         END OF REC                                   
         BE    RATE100C                                                         
         CLI   0(R4),X'20'                                                      
         BNE   *+10                                                             
         AP    DUB,=P'1'                                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RATE100A                                                         
*                                                                               
RATE100C CP    DUB,=P'4'                                                        
         BNH   RATE100D                                                         
         MVC   KBAMSG(L'KBAMSG),SPACES                                          
         MVC   KBAMSG(44),=C'USE ''DISR'' TO CHANGE RATES FOR THIS CONTX        
               RACT'                                                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
RATE100D DS    0H                                                               
         BRAS  RE,CKSPDESP         CHECKING SPACE DESP FIELD                    
         CHI   R3,SPDSPERR         SPACE DESCRIPTION NOT FOUND?                 
         BE    ERROR                                                            
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'20',PCONREC)                                    
*                                                                               
         LA    R4,CONED4H          SEE IF ANY RATE DATA PRESENT                 
         SR    RE,RE                                                            
RATE101  CLI   5(R2),0             ANY                                          
         BNE   RATE102             YES                                          
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R4                                                            
         BH    EDITSTD             NO RATE DATE GIVEN                           
         B     RATE101                                                          
RATE102  DS    0H                                                               
         LA    R2,CONLI1H                                                       
         LA    R4,WORK2            ELEMENT AREA                                 
*                                                                               
* EDIT RATE BASIS LINE                                                          
*                                                                               
         MVC   0(2,R4),=X'202A'    RATE BASIS ELEM CODE AND LEN                 
*                                                                               
RATE200  XC    WORK2+2(40),WORK2+2                                              
         CLI   5(R2),0             CHK FOR INPUT IN LEVEL FLD                   
         BNE   RATE220             YES - PROCESS RATE LINE                      
*                                                                               
* NO INPUT IN LEVEL IND, CHECK FOR INPUT OTHER RATE FIELDS                      
* IF FOUND ERROR - LEVEL MUST BE GIVEN                                          
*                                                                               
         LR    R5,R2                                                            
         ZIC   R8,0(R5)            BUMP TO LEVEL FIELD                          
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)            BUMP TO PCT FIELD                            
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)            BUMP TO RATE FIELD                           
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)            BUMP TO SPACE FIELD                          
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)            BUMP TO EFFECTIVE DATE FIELD                 
         AR    R5,R8                                                            
         CLI   5(R5),0             CHECK FOR INPUT                              
         BE    RATE210                                                          
RATE205  LA    R3,1                MISSING INPUT                                
         B     ERROR                                                            
*                                                                               
RATE210  LA    R2,RATELEN(R2)      BUMP TO NEXT RATE LINE                       
         LA    R5,CONLI4H          LAST LINE                                    
         CR    R2,R5                                                            
         BH    RTLOOK                                                           
         B     RATE200             GO DO NEXT LINE                              
*                                                                               
* LVL IND                                                                       
*                                                                               
RATE220  ZAP   34(5,R4),=P'0'      OPEN RATE                                    
         MVC   5(1,R4),8(R2)       LVL IND                                      
         CLI   8(R2),C'L'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'$'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'P'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'S'          SPECIAL?                                     
         BE    RATE250                                                          
         CLI   8(R2),C'X'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'I'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'U'                                                       
         BNE   ERROR               VALID LVL IND                                
*                                                                               
* EDIT LEVEL                                                                    
*                                                                               
RATE250  IC    R8,0(R2)            FIELD LEN                                    
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         ST    R2,LEVADDR                                                       
         ZAP   6(5,R4),=P'0'                                                    
         SR    R1,R1                                                            
         CLC   8(4,R2),=C'FLAT'                                                 
         BNE   *+12                                                             
         OI    16(R4),1                                                         
         B     RATE275                                                          
         CLC   8(4,R2),=C'OPEN'                                                 
         BE    RATE275                                                          
         CLI   5(R2),0             LEN                                          
         BE    RATE275                                                          
         CLC   8(3,R2),=C'ADV'     ADV LEVEL LOOK-UP                            
         BNE   RATE270                                                          
         CLC   SADVDATA(2),AGYALPHA                                             
         BE    ERROR                                                            
         TM    SADVDATA+15,X'04'   CHK LEVEL LOOK-UP ALLOWED                    
         BZ    ERROR                                                            
         ZAP   6(5,R4),=P'-2'      SET TO LOOK=UP ADV LEVEL                     
         B     RATE280                                                          
*                                                                               
RATE270  DS    0H                                                               
         IC    R8,5(R2)            LEN                                          
         LR    R9,R2                                                            
         CLI   8(R2),C'N'          CHECK FOR NET LEVEL - ONLY FOR $             
         BNE   RATE270C                                                         
         CLI   5(R4),C'$'          PRBLIND MUST HAVE BEEN "$"                   
         BNE   ERROR                                                            
         AHI   R8,-1                                                            
         BNP   ERROR                                                            
         LA    R9,1(R9)            BUMP PAST 'N'                                
         MVI   5(R4),C'N'          CHANGE "$" TO "N" IN PRDLIND                 
*                                                                               
RATE270C GOTO1 VCASHVAL,DMCB,8(R9),(R8)                                         
*                                                                               
         CLI   DMCB,0              VALID?                                       
         BNE   ERROR                                                            
         L     R1,DMCB+4           LEVEL                                        
         LTR   R1,R1                                                            
         BM    ERROR                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
RATE275  CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   6(5,R4),DUB+3       LEVEL TO RATE BASIS ELEMENT                  
*                                                                               
* CHECKING FOE PERCENT FIELD                                                    
*                                                                               
RATE280  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)                                                      
         IC    R8,5(R2)            INPUT LENGTH                                 
         ZAP   39(3,R4),=P'0'                                                   
         CLI   5(R2),0                                                          
         BE    RATE285                                                          
         GOTO1 VCASHVAL,DMCB,8(R2),(R8)                                         
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         L     R9,DMCB+4                                                        
         LTR   R9,R9                                                            
         BM    ERROR                                                            
         C     R9,=F'9999'                                                      
         BH    ERROR                                                            
         CVD   R9,DUB                                                           
         MVC   39(3,R4),DUB+5                                                   
*                                                                               
* CHECKING FOR RATE FIELD                                                       
*                                                                               
RATE285  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD - RATE                            
         IC    R8,5(R2)            INPUT LENGTH                                 
         ST    R2,RATADDR                                                       
         ZAP   11(5,R4),=P'0'                                                   
         SR    R9,R9                                                            
         CLI   5(R2),0             NO INPUT?                                    
         BE    RATE350                                                          
         CLC   8(3,R2),=C'ADV'     ADV RATE LOOK-UP                             
         BNE   RATE285D                                                         
         CLC   SADVDATA(2),AGYALPHA                                             
         BE    ERROR                                                            
         TM    SADVDATA+15,X'02'   RATE LOOK-UP ALLOWED                         
         BZ    ERROR                                                            
         ZAP   11(5,R4),=P'-2'     SET FOR ADV RATE LOOK-UP                     
         B     RATE380                                                          
*                                                                               
RATE285D OI    16(R4),X'80'        SET ON MANUAL RATE INPUT                     
         CLI   8(R2),C'N'          NET FIRST?                                   
         BNE   *+8                                                              
         OI    16(R4),X'10'        NET INDICATIOR                               
*                                                                               
         CLI   9(R2),C'N'          NET SECOND?                                  
         BNE   *+8                                                              
         OI    16(R4),X'10'        NET INDICATIOR                               
*                                                                               
         CLI   8(R2),C'S'          S FIRST?                                     
         BNE   *+8                                                              
         OI    16(R4),X'02'        S INDICATIOR                                 
*                                                                               
         CLI   9(R2),C'S'          S SECOND?                                    
         BNE   *+8                                                              
         OI    16(R4),X'02'        NET INDICATIOR                               
*                                                                               
         CLI   8(R2),C'C'          C FIRST?                                     
         BNE   *+8                                                              
         OI    16(R4),X'04'        C INDICATIOR                                 
*                                                                               
         CLI   9(R2),C'C'          C SECOND?                                    
         BNE   *+8                                                              
         OI    16(R4),X'04'        COMM INDICATIOR                              
*                                                                               
CLI9R2T  CLI   9(R2),C'T'          TOTAL RATE                                   
         BNE   *+8                                                              
         OI    16(R4),X'40'        TOTAL                                        
         CLI   9(R2),C'U'          UNIT RATE                                    
         BNE   *+8                                                              
         OI    16(R4),X'20'                                                     
*                                                                               
         CLI   8(R2),C'T'          TOTAL RATE                                   
         BNE   *+8                                                              
         OI    16(R4),X'40'        TOTAL                                        
         CLI   8(R2),C'U'          UNIT RATE                                    
         BNE   *+8                                                              
         OI    16(R4),X'20'                                                     
*                                                                               
         TM    16(R4),X'60'        CANNOT HAVE T AND U                          
         BO    ERROR                                                            
         TM    16(R4),X'12'        CANNOT HAVE S AND N RATE                     
         BO    ERROR                                                            
         TM    16(R4),X'06'        CANNOT HAVE C AND S RATE                     
         BO    ERROR                                                            
         TM    16(R4),X'14'        CANNOT HAVE C AND N RATE                     
         BO    ERROR                                                            
*                                                                               
         TM    16(R4),X'40'        WAS TOTAL ENCOUNTERED                        
         BO    RATE300                                                          
         TM    16(R4),X'20'        WAS UNIT ENCOUNTERED                         
         BO    RATE286                                                          
*                                                                               
         CLI   KBAMED,C'N'         IF NON-NEWS ASSUME TOTAL                     
         BNE   RATE300                                                          
         ZIC   RF,0(R2)                                                         
         LA    RF,0(RF,R2)         POINT TO NEXT FIELD (SPACE)                  
*                                                                               
         CLC   8(2,RF),=C'R='                                                   
         BE    RATE286                                                          
*                                                                               
         CLI   5(RF),0             TEST ANY INPUT                               
         BNE   RATE300             YES - RATE WILL BNE'TOTAL'                   
*                                                                               
RATE286  DS    0H                  LINE RATE - 5 DECIMALS                       
         LA    R9,8(R2)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE288                                                          
         CLI   0(R9),X'EF'                                                      
         BH    RATE288                                                          
         BCTR  R8,R0               YES - ADTRST LENGTH                          
         LA    R9,1(R9)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE288                                                          
         CLI   0(R9),X'EF'                                                      
         BH    RATE288                                                          
         BCTR  R8,0                                                             
         LA    R9,1(R9)                                                         
RATE288  DS    0H                                                               
         OI    16(R4),X'20'        SET UNIT RATE                                
         XC    MYTEMP,MYTEMP       MOVE REMAINING INPUT TO MYTEMP               
         BCTR  R8,0                                                             
         EX    R8,MVRAT                                                         
         B     *+10                                                             
*                                                                               
MVRAT    MVC   MYTEMP(0),0(R9)     EXECUTED                                     
*                                                                               
         SR    R1,R1               USED FOR INPUT LENGTH FOR CASHVAL            
*                                                                               
         LA    R5,MYTEMP                                                        
RATE290  CLI   0(R5),0                                                          
         BE    RATE294                                                          
         CLI   0(R5),C'/'                                                       
         BE    RATE292                                                          
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         B     RATE290                                                          
*                                                                               
RATE292  CLI   1(R5),C'L'          LINE RATE?                                   
         BNE   RATE293                                                          
         CLI   5(R4),C'I'          DISALLOW FOR INCH LEVEL IND                  
         BE    ERROR                                                            
         B     RATE294                                                          
*                                                                               
RATE293  CLI   1(R5),C'I'                                                       
         BNE   ERROR                                                            
         CLI   5(R4),C'L'          DISALLOW FOR LINE LEVEL IND                  
         BE    ERROR                                                            
         OI    16(R4),X'08'        INCH RATE INPUT                              
*                                                                               
RATE294  DS    0H                                                               
         LR    R8,R1                                                            
         MVC   0(2,R5),=C'    '   JUST IN CASE                                  
*                                                                               
         GOTO1 VCASHVAL,DMCB,(5,MYTEMP),(R8)                                    
         B     RATE325                                                          
*                                                                               
RATE300  DS    0H                  NON-LINE RATE - 2 DECIMALS                   
         LA    R9,8(R2)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE301                                                          
         CLI   0(R9),X'EF'                                                      
         BH    RATE301                                                          
         BCTR  R8,R0               YES - SHORTEN LENGTH                         
         LA    R9,1(R9)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE301                                                          
         CLI   0(R9),X'EF'                                                      
         BH    RATE301                                                          
         BCTR  R8,0                                                             
         LA    R9,1(R9)                                                         
*                                                                               
RATE301  DS    0H                                                               
         OI    16(R4),X'40'        SET TOTAL RATE IND                           
RATE302  DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,(R9),(R8)                                          
*                                                                               
RATE325  CLI   DMCB,0              VALID?                                       
         BNE   ERROR                                                            
         L     R9,DMCB+4           RATE                                         
         LTR   R9,R9                                                            
         BNZ   RATE330                                                          
         LA    R9,1                SET ZERO TO -.01                             
         LCR   R9,R9               TO PREVENT RATELOOK                          
         B     RATE350                                                          
*                                                                               
RATE330  DS    0H                                                               
         CLI   8(R2),C'P'          TAKE PCT DISCOUNT OFF RATE                   
         BNE   RATE350                                                          
         ZAP   DUB,39(3,R4)        PCT                                          
         CVB   R5,DUB                                                           
         LHI   R0,10000                                                         
         SR    R0,R5                                                            
         LR    R1,R9                                                            
         MR    R0,R0                                                            
         LHI   RF,10000                                                         
         BAS   RE,RTDIV                                                         
         LR    R9,R1                                                            
*                                                                               
RATE350  CVD   R9,DUB                                                           
         MVC   11(5,R4),DUB+3      RATE                                         
*              DESCRIPTION                                                      
RATE380  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         BUMP TO DESCRIPTION FIELD                    
RATE400  BAS   RE,MOVE                                                          
*                                                                               
* CHECKING FOR DESCRIPTION FIELD                                                
*                                                                               
         CLC   WORK(2),=C'R='      DOING RATE CODE?                             
         BNE   RATE425                                                          
         LA    R1,17(R4)                                                        
         LA    R9,WORK                                                          
         LA    R8,6                                                             
RATE410  CLI   0(R9),C','                                                       
         BE    RATE415                                                          
         CLI   0(R9),0                                                          
         BE    RATE415                                                          
         MVC   0(1,R1),0(R9)                                                    
         LA    R1,1(R1)                                                         
         LA    R9,1(R9)                                                         
         BCT   R8,RATE410                                                       
         B     ERROR                   RCODE TOO LONG                           
*                                                                               
RATE415  MVC   22(12,R4),1(R9)         MOVE DESCRIPTION                         
         OC    17(17,R4),SPACES                                                 
         CLC   19(3,R4),SPACES         R= WITH NO RATE CODE                     
         BE    ERROR                                                            
         CLI   5(R4),C'L'                                                       
         BE    RATE425A                                                         
         CLI   5(R4),C'I'                                                       
         BE    RATE425A                                                         
         CLI   5(R4),C'$'            SEE IF $ VOLUME                            
         BE    RATE416                                                          
         CLI   5(R4),C'N'            SEE IF NET $ VOLUME                        
         BE    RATE416                                                          
         CLI   5(R4),C'P'            SEE IF PAGES                               
         BE    RATE416                                                          
         CLI   5(R4),C'X'            SEE IF TIMES                               
         BNE   RATE417                                                          
RATE416  CP    11(5,R4),=P'0'        SEE IF RATE ENTERED                        
         BNE   RATE425A                                                         
*                                                                               
RATE417  MVI   ERRAREA,X'FF'                                                    
         MVC   KBAMSG,SPACES                                                    
         MVC   KBAMSG(32),=C'INVALID LVL IND - MUST BE L OR I'                  
         FOUT  KBAMSGH                                                          
         LH    R4,=AL2(CONDS1H-CONLI1H)                                         
         SR    R2,R4                                                            
         B     EXIT                                                             
*                                                                               
RATE425  MVC   17(17,R4),WORK      DESCRIPTION                                  
RATE425A CLI   KBAMED,C'O'                                                      
         BNE   RATE450                                                          
         LA    R3,2                                                             
         BAS   RE,CHKOUT           CHECK FOR SRI= (OUTDOORS)                    
         BNZ   ERROR                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* EFFECTIVE DATE                                                                
*                                                                               
RATE450  IC    R8,0(R2)            FIELD LEN                                    
         LA    R2,0(R8,R2)         NEXT FIELD - EFF. DATE                       
         CLI   5(R2),0             INPUT?                                       
         BE    RATE550                                                          
*                                                                               
         GOTO1 VDATVAL,DMCB,8(R2),DUB                                           
         LA    R9,8(R2)                                                         
         CLI   0(R9),C'-'                                                       
         BE    RATE475                                                          
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(3,2(R4))                                   
*                                                                               
RATE460  CLI   0(R9),0                                                          
         BE    RATE500                                                          
         CLI   0(R9),C' '                                                       
         BE    RATE500                                                          
         CLI   0(R9),C'-'                                                       
         BE    RATE475                                                          
         LA    R9,1(R9)                                                         
         B     RATE460                                                          
*                                                                               
RATE475  DS    0H                                                               
         CLI   KBAMED,C'N'         NON NEWS                                     
         BNE   RATE476                                                          
         CP    11(5,R4),=P'0'      RATE ENTERED?                                
         BE    ERROR               MUST ACCOMDATE PRD FOR NEWSPAPER             
*                                                                               
* RATELOOK WILL TRY TO FIND OPEN RATE AND STORE IT IN RPBOPEN                   
* DISALLOW IT WHEN THEY ENTER A PRODUCT FOR NEWSPAPERS                          
*                                                                               
RATE476  XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),KBAMED                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),SAVKKEY+4  CLIENT                                       
         MVC   KEY+7(3),1(R9)                                                   
         OC    KEY+7(3),=C'   '                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    RATE480                                                          
*                                                                               
         CLI   SAVCLTPR+5,C'1'     SEE IF MASTER CLIENT                         
         BE    RATE480             WON'T FIND PRODUCT                           
*                                                                               
         LA    R3,15               PRODUCT NOT FOUND                            
         B     ERROR                                                            
*                                                                               
RATE480  MVC   34(3,R4),KEYSAVE+7  PRODUCT IN PRBOPEN                           
RATE500  DS    0H                                                               
*                                                                               
RATE550  DS    0H                  ADD RATE BASIS ELEMENT                       
         CP    6(5,R4),=P'-2'                                                   
         BE    RATE560                                                          
         CP    11(5,R4),=P'-2'                                                  
         BE    RATE560                                                          
         B     RATE600                                                          
*                                                                               
RATE560  DS    0H                                                               
         BAS   RE,CKADVC                                                        
         CLI   ERRAREA,X'FF'                                                    
         BNE   RATE600                                                          
         L     R2,LEVADDR                                                       
         CP    6(5,R4),=P'-2'                                                   
         BE    *+8                                                              
         L     R2,RATADDR                                                       
         B     EXIT                                                             
*                                                                               
RATE600  GOTO1 VADDELEM,DMCB,PCONREC,WORK2                                      
         CLI   DMCB,X'FF'                                                       
         BE    OVFERR                                                           
         LH    R0,HALF2                                                         
         AHI   R0,1                                                             
         STH   R0,HALF2            BUMP RATE LINE COUNTER                       
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
*                                                                               
         LA    R5,CONED4H                                                       
         CR    R2,R5               LAST RATE BASIS LINE?                        
         BH    RTLOOK                                                           
         B     RATE200             GO DO NEXT LINE                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RTLOOK   DS    0H                                                               
         MVC   KEY+27(4),SAVPUBA   RE-READ PBU REC                              
         BAS   RE,GETPUB                                                        
         LA    R3,MAXRTS                                                        
         GOTO1 VRTLOOK,DMCB,0,APUBIO,PCONREC,((R3),ARTLKWRK)                    
*                                                                               
         CLI   DMCB,0                                                           
         BE    RTL6                                                             
         CLI   DMCB,208            TOO MANY RATES                               
         BE    RTL4B                                                            
*                                  POINT CURSOR TO ERROR ELEM                   
         LA    R2,CONLI1H                                                       
         LA    R0,CONLI2H-CONLI1H                                               
         LA    R3,PCONREC+33                                                    
*                                                                               
RTL2     DS    0H                                                               
         CLI   5(R2),0             TEST INPUT THIS LINE                         
         BE    RTL3                                                             
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'20'                                                      
         BNE   RTL2                                                             
*                                                                               
         CLI   PRBIND-PRBELEM(R3),X'FF'                                         
         BE    RTL4                                                             
*                                                                               
RTL3     DS    0H                                                               
         AR    R2,R0               BUMP CURSOR                                  
         B     RTL2                                                             
*                                                                               
RTL4     DS    0H                                                               
         LA    R2,CONRT1H-CONLI1H(R2)                                           
RTL4B    DS    0H                                                               
         IC    R3,DMCB                                                          
         B     ERROR                                                            
*                                                                               
RTL6     DS    0H                                                               
         MVC   ARTLKWRK(1),DMCB+12 SAVE ELEM COUNT                              
*                                                                               
* DELETE RATE ELEMS IN RECORD (WILL ADD THEM LATER FROM RTLKWRK)                
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'20',PCONREC)                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40D10 - PRINTPAK CONTRACT STANDARD COMMENT EDIT'               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDITSTD  LA    R2,CONSTDH                                                       
         LA    R3,36               INVALID COMMENT                              
*                                                                               
         LA    R4,CONSTD           STD COMMENTS                                 
         TM    4(R2),X'20'                                                      
         BO    COM100                                                           
         SR    R8,R8                                                            
         IC    R8,5(R2)            INPUT LEN                                    
*              DELETE PREVIOUS STANDARD COMMENT ELEMENTS                        
         GOTO1 VDELELEM,DMCB,(X'30',PCONREC)                                    
         CLI   5(R2),0             INPUT?                                       
         BE    COM100                                                           
*                                                                               
         MVC   WORK2(2),=X'3008'   ELEM CODE + LEN                              
         LA    R8,8(R8,R2)         INPUT END                                    
COM5     SR    R9,R9                                                            
         LR    R5,R4                                                            
*                                                                               
COM10    CLI   0(R4),C','          STOP CHAR                                    
         BE    COM25                                                            
         LA    R9,1(R9)                                                         
         LA    R4,1(R4)                                                         
         CR    R4,R8               LAST CHAR INPUT?                             
         BL    COM10                                                            
*                                                                               
COM25    MVC   WORK2+2(6),SPACES                                                
         LTR   R9,R9                                                            
         BZ    ERROR                                                            
         LA    RE,6                                                             
         SR    RE,R9                                                            
         LA    RE,WORK2+2(RE)      TO RIGHT ALIGN                               
         BCTR  R9,R0                                                            
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)                                                    
         CHI   R9,5                                                             
         BH    ERROR                                                            
*              VALIDATE STANDARD COMMENT                                        
         XC    IOAREA(32),IOAREA                                                
         MVC   PCOMKAGY,PCONKAGY                                                
         MVC   PCOMKMED,KBAMED                                                  
         MVI   PCOMKRCD,X'40'      REC CODE                                     
         MVC   PCOMKNUM,WORK2+2    NUMBER                                       
         MVC   KEY,IOAREA                                                       
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   WORK2+2(6),PCOMKNUM                                              
*              ADD STD COMMENT ELEMENT                                          
         GOTO1 VADDELEM,DMCB,PCONREC,WORK2                                      
         CLI   DMCB,X'FF'                                                       
         BE    OVFERR                                                           
*                                                                               
         CLI   0(R4),C','          COMMA STOP?                                  
         BNE   COM100                                                           
         LA    R4,1(R4)            NEXT                                         
         B     COM5                                                             
         TITLE 'T40D10 - PRINTPAK CONTRACT SPECIAL COMMENTS EDIT'               
*              VALIDATE SPECIAL COMMENTS FOR THIS CONTRACT                      
COM100   LA    R2,CONCM1H                                                       
*              TEST FOR CHANGES                                                 
         SR    R8,R8                                                            
COM150   TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BZ    COM200                                                           
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BE    COM400                                                           
         B     COM150                                                           
*              EDIT SPECIAL COMMENTS                                            
COM200   LA    R2,CONCM1H                                                       
         LA    R3,36               INVALID COMMENT                              
*                                                                               
*              ELIMINATE PREVIOUS COMMENTS                                      
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'40',PCONREC)                                    
         MVI   WORK2,X'40'         ELEM CODE                                    
*                                                                               
COM250   CLI   5(R2),0             LEN                                          
         BE    COM300                                                           
         IC    R8,5(R2)            INPUT LEN                                    
         BCTR  R8,R0                                                            
         EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BE    COM300                                                           
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)                                                 
         LA    R8,3(R8)                                                         
         LA    RE,WORK2-1(R8)                                                   
* ELIMINATE TRAILING BLANKS                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,R0                                                            
         BCT   RE,*-10                                                          
         STC   R8,WORK2+1          ELEM LEN                                     
         LA    R3,223                                                           
         CLC   WORK2+2(3),=C'RC='                                               
         BNE   *+12                                                             
         CLI   WORK2+1,35                                                       
         BH    ERROR                                                            
         LA    R3,222              SPECIAL COMMENT ERROR                        
         CLC   WORK2+2(2),=C'E+'                                                
         BE    *+14                                                             
         CLC   WORK2+2(2),=C'E-'   SPECIAL ESTIMATE ONLY COMMENT?               
         BNE   COM275                                                           
         CLI   WORK2+1,32                                                       
         BH    ERROR                                                            
COM275   DS    0H                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,PCONREC,WORK2                                      
         CLI   DMCB,X'FF'                                                       
         BE    OVFERR                                                           
*                                                                               
COM300   IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BNE   COM250                                                           
*                                                                               
COM400   DS    0H                                                               
         SPACE 3                                                                
*                             NOW ADD RATE ELEMS TO RECORD                      
*                             FROM RTLKWRK                                      
RTADD    DS    0H                                                               
*                                  FIRST SORT ELEMS OD DATE                     
         ZIC   R5,ARTLKWRK         NO. OF ELEMS                                 
         LTR   R5,R5                                                            
         BNP   RTADD1                                                           
         MVI   ARTLKWRK,0                                                       
         GOTO1 =V(XSORT),DMCB,ARTLKWRK,(R5),42,5,0,RR=RELO10                    
*                                                                               
RTADD1   DS    0H                                                               
         L     R5,ARTLKWRK                                                      
         SR    R4,R4                                                            
RTADD2   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    RTADD4                                                           
         LA    R4,1(R4)       COUNT ELEMS                                       
*                                            SET -.01 BACK TO ZERO              
         CP    PRBRATE-PRBELEM(5,R5),=P'-1'                                     
         BNE   *+10                                                             
         ZAP   PRBRATE-PRBELEM(5,R5),=P'0'                                      
*                                                                               
         GOTO1 VADDELEM,DMCB,PCONREC,0(R5)                                      
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     RTADD2                                                           
*                                                                               
RTADD4   DS    0H                                                               
         MVI   RTOVFL,C'N'                                                      
         CHI   R4,4                                                             
         BNH   *+8                                                              
         MVI   RTOVFL,C'Y'         OVERFLOW TO ADDTIONAL RATE SCREEN            
         B     EXXMOD              FINIS                                        
         SPACE 3                                                                
CHKOUT   NTR1                                                                   
         SPACE 2                                                                
         CLC   WORK(4),=C'SRI='                                                 
         BNE   COX                                                              
         XC    17(17,R4),17(R4)                                                 
         MVI   17(R4),X'FF'                                                     
         ZAP   18(3,R4),=P'99999'                                               
         LA    R5,WORK+7                                                        
         CLC   WORK+4(3),=C'SPC'                                                
         BE    CO4                                                              
         GOTO1 =V(NUMED),DMCB,WORK+4,DUB,RR=RELO10                              
*                                                                               
         CP    DUB,=P'999'          SHOWING CAN'T EXCEED 100                    
         BH    COERR                ALLOW UP TO 999 TO                          
*                                   HANDLE GRP'S                                
         ZAP   18(3,R4),DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    COERR                                                            
CO4      DS    0H                                                               
         GOTO1 =V(NUMED),DMCB,1(R5),DUB,RR=RELO10                               
*                                                                               
         ZAP   21(3,R4),DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    COERR                                                            
         GOTO1 (RF),(R1),1(R5)                                                  
*                                                                               
         ZAP   24(3,R4),DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BH    COERR                                                            
COX      DS    0H                                                               
         SR    R0,R0               SET CC OK                                    
COXX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
RTDIV    DS    0H                                                               
         LTR   RF,RF                                                            
         BP    *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 2                                                                
COERR    DS    0H                                                               
         LTR   RE,RE                                                            
         B     COXX                                                             
*                                                                               
OVFERR   DS    0H                                                               
         LA    R3,208                                                           
         B     ERROR                                                            
*                                                                               
SVTODAY  DS    CL3                                                              
MYTEMP   DS    CL20                                                             
*                                                                               
LEVADDR  DS    F                                                                
RATADDR  DS    F                                                                
         EJECT                                                                  
*        THIS ROUTINE LOOKS-UP LEVELS AND RATES FOR THE ADV CONTRACT            
*        AND STORES THEM IN THE CONTRACT RATE ELEM                              
*                            R4 POINTS TO CONTRACT RATE ELEM                    
*                            R2 STILL POINTS TO FIELD                           
CKADVC   NTR1                                                                   
         XC    IOAREA(100),IOAREA     FIRST I MUST FIND AOR CONTRACT            
         MVC   IOAREA+32(64),KEY        SAVE KEY AND KEYSAVE                    
*                                                                               
         CLI   AORCFND,2               SEE IF I HAVE ALREADY                    
         BE    CKADVC29                                                         
*                                                                               
*                                                                               
         MVC   IOAREA(2),SADVDATA      AOR                                      
         MVC   IOAREA+2(1),SAVKMED                                              
         MVI   IOAREA+3,X'10'                                                   
         MVC   IOAREA+4(3),SADVDATA+2     ADV                                   
         MVC   IOAREA+7(6),SAVKPUB                                              
*                                                                               
         TM    SADVDATA+15,X'01'       PUB LINK REQUIRED                        
         BZ    CKADVC10                                                         
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),SAVKMED                                                 
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(3),SADVDATA+2        ADV                                   
         MVC   KEY+7(2),SADVDATA+0        AOR                                   
         MVC   KEY+9(6),SAVKPUB                                                 
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CKADVCX5                   ERROR                                 
         MVC   IOAREA+7(6),KEY+15                                               
*                                                                               
CKADVC10 DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14  SE NUMBER                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    CKADVC20                                                         
         MVC   KBAMSG,=CL60'*** AGENCY FILE NOT ACTIVE ***'                     
         MVI   ERRAREA,X'FF'                                                    
         B     CKADVCXX                                                         
*                                                                               
CKADVC20 DS    0H                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 HIGH                                                             
         B     CKADVC27                                                         
CKADVC25 GOTO1 SEQ                                                              
CKADVC27 CLC   KEY(13),KEYSAVE     CHK AGY/MED/CLT/PUB                          
         BNE   CKADVCX5            AOR CONTRACT NOT FOUND                       
         MVC   AREC,APUBIO         READ INTO PUBIO                              
         GOTO1 GETREC                                                           
         L     RF,APUBIO                                                        
         CLC   PCONSDT,PCONSDT-PCONREC(RF)      CHK DATES                       
         BL    CKADVC25                                                         
         CLC   PCONEDT,PCONEDT-PCONREC(RF)                                      
         BH    CKADVC25                                                         
*                                                                               
         MVI   AORCFND,3        SET JUST READ INDICATOR                         
*                                                                               
CKADVC29 L     RF,APUBIO                                                        
         LA    R5,33(RF)                                                        
CKADVC30 CLI   0(R5),X'20'       FIND FIRST RATE ELEM                           
         BE    CKADVC35                                                         
CKADVC33 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BNE   CKADVC30                                                         
         B     CKADVCX5          NO RATE ELEMS                                  
*                                                                               
CKADVC35 DS    0H                                                               
         OC    2(3,R5),2(R5)     CHK FOR EFFECTIVE DATE IN AOR ELEM             
         BZ    CKADVC37                                                         
         OC    2(3,R4),2(R4)     CHK FOR EFFECTIVE DATE IN MY ELEM              
         BNZ   CKADVC36                                                         
         CLC   2(3,R5),PCONEDT   AOR EFFECTIVE AFTER MY END                     
         BH    CKADVC33          IGNORE THIS ELEM                               
         B     CKADVC37                                                         
*                                                                               
CKADVC36 CLC   2(3,R5),2(R4)     CHK AOR EFF DATE VS. MY EFF DATE               
         BH    CKADVC33          SKIP THIS ELEM                                 
*                                                                               
CKADVC37 CLC   5(1,R4),5(R5)     SEE IF LEVEL IND MATCH                         
         BNE   CKADVC33          NO - THEN IGNORE THIS ELEM                     
         CLC   17(17,R4),17(R5)   MATCH DESCRIPTIONS                            
         BNE   CKADVC33           SKIP THIS AOR ELEM                            
         CP    6(5,R4),=P'-2'    SEE IF LOOKING-UP LEVEL                        
         BNE   CKADVC40                                                         
*                                                                               
         ZAP   6(5,R4),6(5,R5)   LEVEL FROM AOR CONTRACT                        
*                                                                               
CKADVC40 CP    11(5,R4),=P'-2'   SEE IF LOOKING-UP RATE                         
         BNE   CKADVC50                                                         
*                                                                               
         ZAP   11(5,R4),11(5,R5)  USE AOR RATE                                  
         MVC   16(1,R4),16(R5)    AND INDICATOR                                 
*                                                                               
CKADVC50 DS    0H                                                               
         CP    6(5,R4),=P'-2'                                                   
         BE    CKADVCX5                                                         
         CP    11(5,R4),=P'-2'                                                  
         BE    CKADVCX5                                                         
         B     CKADVCX7                                                         
*                                                                               
CKADVCX5 MVC   KBAMSG,=CL60'*** ADVERTISER DATA NOT FOUND ***'                  
         MVI   ERRAREA,X'FF'                                                    
         CLI   KEYSAVE,X'FE'          SEE IF LINK NOT FOUND                     
         BE    CKADVCXX               DON'T SWITCH BACK                         
*                                                                               
CKADVCX7 DS    0H                                                               
         CLI   AORCFND,2    SEE IF ALREADY READ                                 
         BE    CKADVCXX                                                         
*                                                                               
         CLI   AORCFND,3            SEE IF JUST FOUND                           
         BNE   *+8                                                              
         MVI   AORCFND,2            SET ALREADY FOUND INDICATOR                 
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                TROUBLE SWITCHING BACK                       
*                                                                               
CKADVCXX MVC   KEY(64),IOAREA+32   RESTORE KEY AND KEYSAVE                      
         XIT                                                                    
*                                                                               
       ++INCLUDE PPGENEROL         IN-LINE CODES                                
*                                                                               
         LTORG                                                                  
SPACES   DC    CL70' '                                                          
PATCH    DS    CL30                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSPDESP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CONDS1H          FIRST DESCRIPTION FIELD                      
         LA    R4,CONED4H          LAST FIELD FOR LOOPING                       
*                                                                               
CKSPD08  CR    R2,R4               LAST FIELD REACHED?                          
         BH    CKSPD10             ALL SPACE DESP FIELDS ARE CHECKED            
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKSPD12             NO, BUMP TO NEXT SPACE DESP FIELD            
*                                                                               
         CLI   8(R2),C'='          BROWSING?                                    
         BNE   CKSPD12                                                          
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(KBAMED,C' STD'),0,RR=RELO10                                   
*                                                                               
         DC    H'0'                BROWSE KNOWS WHERE TO GET BACK               
*                                                                               
CKSPD12  LA    RF,6                6 FIELDS TO NEXT SPACE DESP FIELD            
         BAS   RE,CKSPBMP                                                       
         BCT   RF,*-4                                                           
         B     CKSPD08                                                          
*                                                                               
CKSPD10  MVC   CKSPKEY(L'KEY),KEY                                               
         MVC   CKSPKEY+L'KEY(L'KEYSAVE),KEYSAVE                                 
*                                                                               
         XC    CKSPWORK,CKSPWORK                                                
         MVC   CKSPWORK+30(4),=C'P0BY'                                          
         MVC   CKSPWORK+34(2),AGYALPHA                                          
         MVC   CKSPWORK+36(1),KBAMED                                            
         MVC   CKSPWORK+37(3),SAVCLT                                            
         CLI   SAVCLTOF,C' '       OFFICE CODE PRESENT?                         
         BNH   *+14                                                             
         MVI   CKSPWORK+40,C'*'                                                 
         MVC   CKSPWORK+41(1),SAVCLTOF                                          
*                                                                               
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8                                                      
         GOTO1 CGETPROF,DMCB,(0,CKSPWORK+30),CKSPWORK,VDATAMGR                  
         DROP  R8                                                               
*                                                                               
         CLI   CKSPWORK+9,C'Y'     PROFILE ALLOWS SPACE DESP LOOK UP?           
         BNE   CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         LA    R2,CONDS1H          FIRST DESCRIPTION FIELD                      
         LA    R4,CONED4H          LAST FIELD FOR LOOPING                       
*                                                                               
CKSPD20  CR    R2,R4               LAST FIELD REACHED?                          
         BH    CKSPDX2             ALL SPACE DESP FIELDS ARE VALIDATED          
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKSPD30             NO, BUMP TO NEXT SOACE DESP FIELD            
*                                                                               
* SEE IF ANY SPECIAL FORMAT NEED TO BE SKIPPED                                  
*                                                                               
         CLC   8(2,R2),=C'R='      RATE CODE?                                   
         BE    CKSPD30                                                          
         CLI   KBAMED,C'O'         OUTDOOR?                                     
         BNE   *+14                                                             
         CLC   8(3,R2),=C'SRI='    SHOWING FOR OUTDOOR?                         
         BE    CKSPD30                                                          
*                                                                               
         XC    CKSPWORK,CKSPWORK                                                
         ZIC   RE,5(R2)            GET INPUT LENGTH                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CKSPWORK(0),8(R2)   EX MOVE IN INPUT                             
*                                                                               
         OC    CKSPWORK(17),=17C' '                                             
*                                                                               
* SEE IF SPACE DESP ENTERED IS SAME AS IN RECORD NOW                            
*                                                                               
         MVI   CKSPIND,C'N'        INIT TO NOT SAME                             
*                                                                               
         MVI   CKSPECOD,X'20'      RATE BASIS ELEM (CURRENT LEVEL)              
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPD30             NEXT SPACE DESCRIPTION FIELD                 
*                                                                               
         MVI   CKSPECOD,X'21'      (LOWER LEVEL)                                
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPD30             NEXT SPACE DESCRIPTION FIELD                 
*                                                                               
         MVI   CKSPECOD,X'22'      (HIGHER LEVEL)                               
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPD30             NEXT SPACE DESCRIPTION FIELD                 
*                                                                               
         MVI   CKSPECOD,X'24'      (OPEN RATES)                                 
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPD30             NEXT SPACE DESCRIPTION FIELD                 
*                                                                               
         XC    KEY,KEY             BUILD KEY TO READ SPC DESP RECORD            
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),KBAMED                                                
         MVI   KEY+03,X'5A'        STANDARD SPACE DESCRIPTION REC CODE          
         MVC   KEY+04(17),CKSPWORK                                              
*                                                                               
         MVC   KEYSAVE,KEY         FOR COMPARISON LATER                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(21),KEYSAVE     INPUT MATCHED THAT OF RECORD?                
         BE    *+12                YES                                          
         LA    R3,SPDSPERR                                                      
         B     CKSPDX1             CURSOR AND ERROR MSG ARE SET                 
*                                                                               
CKSPD30  LA    RF,6                6 FIELDS TO NEXT SPACE DESP FIELD            
         BAS   RE,CKSPBMP                                                       
         BCT   RF,*-4                                                           
         B     CKSPD20                                                          
*                                                                               
CKSPDX1  DS    0H                                                               
         MVC   KEY,CKSPKEY                                                      
         MVC   KEYSAVE,CKSPKEY+L'KEY                                            
         XIT1  REGS=(R2,R3)        ERROR MSG AND CURSOR POSITION                
*                                                                               
CKSPDX2  DS    0H                                                               
         MVC   KEY,CKSPKEY                                                      
         MVC   KEYSAVE,CKSPKEY+L'KEY                                            
         XIT1                      ALL REGISTERS RESTORED                       
*                                                                               
*                                                                               
*                                                                               
CKSPD70  DS    0H                                                               
         LA    R5,PCONREC+33                                                    
         CLI   0(R5),X'10'         FIRST CONTRACT ELEM EXIST?                   
         BE    *+6                                                              
         DC    H'0'                BAD CONTRACT RECORD                          
CKSPD70H BAS   RE,CKSPNEL                                                       
         BNER  R8                                                               
         USING PRBELEM,R5                                                       
         CLC   PRBDESC,CKSPWORK    SAME AS ON RECORD?                           
         BNE   CKSPD70H            NO, CKECK NEXT ELEM                          
         MVI   CKSPIND,C'Y'                                                     
         BR    R8                                                               
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
CKSPBMP  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT FIELD                           
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
CKSPNEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   CKSPECOD,0(R5)                                                   
         BCR   8,RE                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
CKSPNELX LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
CKSPWORK DS    CL250               WORKING STORAGE AREA                         
CKSPCLTO DS    C                   CLIENT OFFICE CODE (IF PRESENT)              
CKSPECOD DS    C                   BYTE FOR ELEMENT CODE                        
CKSPIND  DS    C                   INDICATOR FOR SAME SPACE DESP                
CKSPKEY  DS    CL100               FOR RESTORING KEYS                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPCONWRK                                                       
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028PPCON10   11/05/03'                                      
         END                                                                    
