*          DATA SET DECNADO    AT LEVEL 061 AS OF 04/22/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECNADOA                                                                 
*INCLUDE DETABQH                                                                
*INCLUDE DEBKUPDT                                                               
*INCLUDE SMTP                                                                   
********************************************************************            
         TITLE 'DEMCON - CABLE NAD CONVERSION - OUTPUT PHASE'                   
*                                                                               
*IPHASE:DECNADI                                                                 
*OPHASE:DECNADO                                                                 
*--------------                                                                 
*                                                                               
DECNVMTA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NADWRKX-NADWRKD,**OUTPHS,RA,R4,END=DECNVMTX                      
*                                                                               
* DEIS SAYS: NOTE THAT R4 IS APPARENTLY BEING USED AS A BASE REGISTER           
*            FOR FIELD DDSBKTY IN THE DELOLDJS ROUTINE. THIS IS                 
*            SOMEWHAT UNEXPECTED. IT MEANS THAT WE CAN'T SAFELY DROP R4         
*            BEFORE THE "BASE=*" ROUTINES WITHOUT MOVING THE FIELD              
*            ELSEWHERE.                                                         
*                                                                               
         USING NADWRKD,RC          RC=A(TEMP W/S)                               
         USING DPRINT,R7           R7=PRINT                                     
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     RF,AOREC                                                         
         ST    RF,SVAOREC                                                       
         LA    RF,ATREC                                                         
         ST    RF,SVATREC                                                       
         LA    RF,MYREC                                                         
         ST    RF,AMYREC                                                        
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
*                                                                               
         B     *+4(R1)                                                          
         B     CNV00               PROCESS A RECORD                             
         B     LASTHK              LAST TIME HOOK                               
         SPACE 2                                                                
*                                                                               
CNV00    MVI   PRINTSW,0                                                        
*                                                                               
         CLI   FRSTREC,YES         FIRST RECORD TO OPHASE                       
         BNE   CNV25                                                            
*                                                                               
         XC    UNKNTAB(UNKNTABL),UNKNTAB                                        
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC     NETCTREC=Y                            
         BNO   CNV25                                                            
         GOTO1 =V(DEBKUPDT),DMCB,,INITQ   INITIALIZE DEBKUPDT                   
*                                                                               
CNV25    MVI   FRSTREC,NO                                                       
*                                                                               
         BAS   RE,KNOWNSTA         SAVE UNKNOWN STATIONS TO UNKNTAB             
*                                                                               
         L     R2,AIREC            OPEN FILE                                    
         CLI   INTKEY,C'R'         CORRECTION/DELETION/ADDITION RECD?           
         BE    RCNV                                                             
         CLI   INTRTYP,PMCODEQU    PROGRAM RECS -Q- ?                           
         BE    QCNV                GO BUILD Q RECD KEY                          
         CLI   INTRTYP,PRCODEQU    USAGE   RECS -P- ?                           
         BE    PCNV                GO BUILD P RECD KEY                          
*                                                                               
CNVX     XMOD1                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*QCNV -   BUILD 'Q' PROGRAM RECD KEY                                            
***********************************************************************         
QCNV     DS    0H                                                               
*                                                                               
         LA    R6,INTKEY                                                        
         USING QPRGKD,R6                                                        
         MVC   MKTBRK,QMKTB              SAVE MARKET BRAKE                      
*                                                                               
         CLC   INTKEY(QNTIQ),PRGKEY   SAME AS LAST KEY?                         
         BE    QMERGE                MERGE PROGRAM/TRACKS/TELECASTS AND         
*                                     MKTBRKS IN ONE Q RECORD                   
QCNV8    OC    PREVKEY,PREVKEY     RELEASE PREVIOUS RECORD?                     
         BZ    QCNV10              NO                                           
         CLI   PREVKEY,PMCODEQU    'Q' RECORD?                                  
         BNE   *+12                                                             
         BAS   RE,FINFF                                                         
         B     QCNV10                                                           
         BAS   RE,PUTTAPE                                                       
         XC    PREVKEY,PREVKEY     NOTHING TO RELEASE                           
*                                                                               
QCNV10   MVI   CORRFLAG,NO         NOT A CORRECTION                             
         OC    CNTKEY,CNTKEY       RELEASE PREV CONTROL INFO RECD?              
         BZ    *+8                                                              
         BAS   RE,CNTREC                                                        
         DROP  R6                                                               
*                                                                               
QCNV20   MVC   PRGKEY,INTKEY       SAVE  RECDS PRG KEY                          
         XC    THISKEY,THISKEY                                                  
         LA    R6,THISKEY                                                       
         USING PMKEY,R6            BUILD -Q- RECD NEW PRG KEY                   
         MVI   PMCODE,PMCODEQU                                                  
         MVI   PMMEDIA,C'C'        CABLE                                        
         MVC   PMSRC,OUTSRC                                                     
         CLC   INTBOOK,=AL2(JAN_07)  HAVE ALL STREAMS AFTER JAN/07              
         BL    *+10                                                             
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         BAS   RE,NTIDDS           UPDATE NTI-DDS PRG NUMBER TABLE              
         BAS   RE,DDSFIL           UPDATE FILE NUMBER                           
         MVC   STFILE,FILENUM      SAVE FILE# OF BGEGINNING OF PRG              
         MVC   PMPNUM,FILENUM      INTERNAL FILE NUMBER                         
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMSTYP,INTSTYP                                                   
         MVC   CNTKEY,THISKEY      SAVE KEY OF CNTL RECD                        
         MVC   CNTMTYP,INTMTYP     AND OTHER NECC INFO                          
         MVC   CNTSTYP,INTSTYP                                                  
         XC    PCNTBITS,PCNTBITS   CLEAR TIME BITS FOR PROG AVE                 
         XC    TCNTBITS,TCNTBITS   AND TRACKS                                   
         XC    CNTUTRK,CNTUTRK     CLEAR TRKD/UNTRKD INDICATOR                  
         XC    CNTORIG,CNTORIG                                                  
         MVC   PREVKEY,THISKEY                                                  
         BAS   RE,DDSFIL           BUMP TO NEXT FILE NUMBER                     
         MVC   PMPNUM,FILENUM      SET FILE NUMBER FOR CURRENT RECD             
         GOTO1 ABLDREC,DMCB,PMKEY                                               
         MVC   AORKEY,INTKEY       SAVE QPRGKD KEY                              
         DROP  R6                                                               
*                                                                               
*        LA    R6,INTKEY                                                        
*        USING QPRGKD,R6                                                        
*        CLI   QSPBR,0             SPECIAL OR BREAKOUT?                         
*        BE    QCNV50              YES. DON'T BUILD AVERAGE LEVELS              
         MVI   NOSUMY,1            CREATE PRG AVG ELEMENTS                      
         OC    INTTRK,INTTRK       SHOULD BE BLANK                              
         BZ    *+12                MAYBE NO PRG AVG ON AN UNTRK TELC            
         BAS   RE,BLDELEM          PRG AVG RECD MISSING--CREATE ELEMS           
         MVI   NOSUMY,2            CREATE TRK SUMMARY?                          
         OC    INTTNUM,INTTNUM     SHOULD BE BLANK                              
         BZ    *+8                                                              
         BAS   RE,BLDELEM          BUILD TRK AVG ELEMS (OR PRG ELEMS)           
QCNV50   MVI   NOSUMY,0                                                         
         BAS   RE,BLDELEM          BUILD ELEMS FOR THIS TRKG/EPISODE            
         BAS   RE,BLDEMS           BUILD DEMO ELEMENTS                          
*                                                                               
         B     CNVX                                                             
         EJECT                                                                  
***********************************************************************         
*PCNV -   BUILD 'P' TIME PERIOD RECD KEY                                        
***********************************************************************         
PCNV     DS    0H                                                               
         XC    THISKEY,THISKEY                                                  
         LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU     BUILD -P- RECD KEY                           
         MVI   PRMEDIA,C'C'        MEDIA FROM INTKEY                            
         MVC   PRSRC,OUTSRC                                                     
         CLC   INTBOOK,=AL2(JAN_07)  HAVE ALL STREAMS AFTER JAN/07              
         BL    *+10                                                             
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTYP,THISSTYP     FOR TIME PERIOD RECORDS,                     
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRSTIM,INTSQH       INCREASE STYP '0' WHEN MULTIPLE              
         MVC   PRDW,INTDAYWK       RECORDS NEEDED (FOR LONG DATA)               
*                                                                               
         CLI   INTSTA+4,C'U'       UNIVERSE RECORDS?                            
         BNE   *+10                                                             
         MVC   PRKMINOR,THISMINR   YES. UPDATE MINOR KEY                        
PCNV1    CLC   PREVKEY,THISKEY     IF SAME KEY AS LAST RECD,                    
         BE    PMERGE              DIFF MKT BRK, MERGE THE RECORDS              
         MVI   THISSTYP,0                                                       
         MVC   PRSTYP,THISSTYP                                                  
         CLI   INTSTA+4,C'U'       UNIVERSE RECORDS?                            
         BNE   *+16                                                             
         XC    PRKMINOR,PRKMINOR   INITALIZE MINOR KEY TO ZEROES                
         MVC   THISMINR,PRKMINOR                                                
*                                                                               
PCNV6    OC    PREVKEY,PREVKEY                                                  
         BZ    PCNV7                                                            
         CLI   PREVKEY,PMCODEQU    'Q' RECORD?                                  
         BNE   PCNV6AA                                                          
         LA    R1,TEMP             BUILD PRG/TRKG/EPISODE ID ELEMENT            
         XC    TEMP,TEMP                                                        
         USING DIDELEM,R1                                                       
         MVI   DIDCODE,DIDCODEQ      '0F' ELEMENT                               
         MVI   DIDELLN,DIDELNEQ      LENGTH                                     
         MVI   DIDTYPE,X'FF'        END OF RECDS FOR THIS PROGRAM #             
         GOTO1 APUTEL,DIDELEM                                                   
         DROP  R1                                                               
*                                                                               
PCNV6AA  DS    0H                                                               
         BAS   RE,PUTTAPE          PUT OUT PREV RECD                            
*                                                                               
PCNV7    DS    0H                                                               
         MVI   CORRFLAG,NO         NOT A CORRECTION RECORD                      
         MVC   PREVKEY,THISKEY     SAVE KEY VALUES                              
         MVC   PRGKEY,INTKEY                                                    
         GOTO1 ABLDREC,DMCB,PRKEY  BUILD -P- RECD                               
         CLI   UNVOFLG,1           FOR UNIVERSE RECORDS, DO NOT REPEAT          
         BE    *+8                   ELEMENTS ON EXPANDED RECORDS               
         BAS   RE,BLDELEM          BUILD ELEMENTS                               
         MVI   UNVOFLG,0                                                        
         BAS   RE,BLDEMS           BUILD DEMO ELEMENTS                          
         B     CNVX                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*RCNV -  PROCESS 'R' RECDS = PROGRAM CORRECTION RECORDS                         
***********************************************************************         
RCNV     DS    0H                                                               
         CLC   INTKEY(QNTIQ),PRGKEY  W/IN SAME PROGRAM?                         
         BE    RMERGE              YES, MERGE THIS CORR W/LAST ONE              
         OC    PREVKEY,PREVKEY     NO, NEW PRGM.  RELEASE LAST RECD?            
         BZ    RCNV5               NO, NOTHING TO RELEASE                       
         CLI   PRGKEY,C'P'         YES. IS RECD IN BUFFER A P-RECD?             
         BNE   RCNV3                                                            
         BAS   RE,PUTTAPE          RELEASE P-RECD AS IS                         
         XC    PREVKEY,PREVKEY                                                  
         B     RCNV8                                                            
*                                                                               
RCNV3    CLI   PRGKEY,C'Q'         WAS LAST RECD A Q-RECD?                      
         BE    *+8                                                              
         BAS   RE,RELS             NO,IT WAS CRCT REC.RELS REST OF PGM          
         BAS   RE,FINFF            TACK ON '0F-FF' ELEMENT AND RELEASE          
*                                                                               
RCNV5    OC    CNTKEY,CNTKEY       RELEASE PREV RECD'S CNTL RECD?               
         BZ    *+8                                                              
         BAS   RE,CNTREC                                                        
*                                                                               
RCNV8    XC    RVARS(RVARSLN),RVARS   NEW KEY, RESET PTRS                       
         XC    IPTR,IPTR                                                        
         XC    FILENUM,FILENUM                                                  
*                                                                               
         MVI   CORRFLAG,YES        WE ARE PROCESSING A CORRECTION RECD          
*                                                                               
         LA    RE,INTKEY                                                        
         USING QPRGKD,RE                                                        
         MVC   DDSNUM,QDDSNUM                                                   
         MVC   OLDFIL,QFILNUM      QFILNUM=FILE# OF PRG ON ORIG BK              
         CLI   INTORIG,C'2'                                                     
         BNE   RCNV10                                                           
         OC    OLDFIL,OLDFIL       RECD TO BE DELETED NOT ON FILE?              
         BZ    RCNVX               THEN BYPASS REQST                            
         DROP  RE                                                               
*                                                                               
RCNV10   LA    RE,INTKEY           NEW BOOK AND NETWORK?                        
         LA    RF,PRGKEY                                                        
         USING QPRGKD,RE                                                        
         CLC   QBOOK,QBOOK-QPRGKD(RF)                                           
         BNE   *+10                                                             
         CLC   QNET,QNET-QPRGKD(RF)                                             
         BNE   *+14                                                             
         CLC   QSTYP,QSTYP-QPRGKD(RF)                                           
         BE    *+12                                                             
         MVI   COPY,C'Y'                                                        
         BAS   RE,RDRECS           RELEASE ALL PRGMS FOR THIS NET-BOOK          
         DROP  RE                                                               
         OC    DDSNUM,DDSNUM       DO WE HAVE AN INTERNAL PRG# ?                
         BNZ   *+8                                                              
         BAS   RE,NTIDDS           NO, ASSIGN ONE                               
         BAS   RE,DDSFIL           ASSIGN A NEW/LATEST FILE NUMBER              
*                                                                               
         MVC   PRGKEY,INTKEY                                                    
         MVC   PRGORIG,INTORIG                                                  
         MVC   STFILE,FILENUM                                                   
         XC    THISKEY,THISKEY                                                  
         LA    R6,THISKEY          BUILD NEW KEY                                
         USING PMKEY,R6                                                         
         MVI   PMCODE,PMCODEQU                                                  
         MVI   PMMEDIA,C'C'                                                     
         MVC   PMSRC,OUTSRC                                                     
         CLC   INTBOOK,=AL2(JAN_07)  HAVE ALL STREAMS AFTER JAN/07              
         BL    *+10                                                             
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMPNUM,FILENUM                                                   
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PREVKEY,THISKEY                                                  
         MVC   CNTKEY,THISKEY      SAVE CNTL RECD KEY                           
         MVC   CNTMTYP,INTMTYP                                                  
         MVC   CNTSTYP,INTSTYP                                                  
         XC    PCNTBITS,PCNTBITS                                                
         XC    TCNTBITS,TCNTBITS                                                
         XC    CNTUTRK,CNTUTRK                                                  
         MVI   CNTORIG,C'1'        SET TO MODIFIED                              
*                                                                               
         MVC   PREVKEY,THISKEY                                                  
         BAS   RE,DDSFIL           CNTL RECD GETS 1ST FILE#,GET NEXT#           
         MVC   PMPNUM,FILENUM                                                   
         GOTO1 ABLDREC,DMCB,PMKEY  OUTPUT KEY IN AOREC                          
         MVC   AORKEY,INTKEY                                                    
         DROP  R6                                                               
*NEW CORRECTIONS/DELETIONS FORMAT                                               
*        CLI   INTORIG,C'2'        DELETE?                                      
*        BNE   RCNV30                                                           
*        OC    INTTRK,INTTRK       DELETE ALL TRACKAGES?                        
*        BNZ   RCNV30                                                           
*        OC    INTTNUM,INTTNUM     DELETE ALL TELECASTS?                        
*        BNZ   RCNV30                                                           
*        XC    RPTR,RPTR           DON'T COPY THE OTHER ELEMS ON RECD           
*        MVI   CNTORIG,C'2'        MARK RECORD DELETED                          
*        B     RCNVX                                                            
*                                                                               
RCNV30   MVI   BYPASS,C'N'         COPY INITIAL REC'D ELEMENTS FLAG             
         XC    RVARS(RVARSLN),RVARS    CLEAR ALL PTRS                           
         XC    IKEY,IKEY                                                        
         OC    OLDFIL,OLDFIL                                                    
         BZ    *+8                                                              
         BAS   RE,GETORIG          READ ORIG RECD INTO AIREC                    
         OC    IKEY,IKEY           DID WE GET ORIGINAL RECD?                    
         BNZ   RCNV32              YES                                          
*                                                                               
         MVI   NOSUMY,1            CREATE PRG AVG ELEMENTS                      
         OC    INTTRK,INTTRK       SHOULD BE BLANK                              
         BZ    *+12                MAYBE NO PRG AVG ON AN UNTRK TELC            
         BAS   RE,BLDELEM          PRG AVG RECD MISSING--CREATE ELEMS           
         MVI   NOSUMY,2            CREATE TRK SUMMARY?                          
         OC    INTTNUM,INTTNUM     SHOULD BE BLANK                              
         BZ    *+8                                                              
         BAS   RE,BLDELEM          BUILD TRK AVG ELEMS (OR PRG ELEMS)           
         MVI   NOSUMY,0                                                         
         BAS   RE,BLDELEM          BUILD ELEMS FOR THIS LEVEL                   
         BAS   RE,BLDEMS                                                        
         B     RCNVX               DONE PROCESSING THIS SREC                    
*                                                                               
RCNV32   XC    RPTR,RPTR           RPTR=0                                       
         L     R6,AIREC            PT TO RECD IN AIREC                          
         LA    R6,4(R6)                                                         
         MVC   DATADISP,=H'23'                                                  
         MVI   ELCODE,TIMCODEQ     CNTL RECD?                                   
         BAS   RE,GETEL            IF TIME ELEMENT FOUND, 1ST REC IS            
         BNE   *+8                 CNTL RECD--BYPASS IT                         
         BAS   RE,FSEQ                                                          
         L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         LA    R6,PMDATA-PMKEY(R6)  PT TO FIRST DATA ELEMENT                    
         ST    R6,IPTR             PT TO 1ST DATA ELEMENT                       
         CLI   0(R6),MARCODEQ      MARKET ELEMENT FIRST?                        
         BNE   RCNV35                                                           
*                                                                               
         ZIC   RE,1(R6)            BUMP TO NEXT ELEMENT                         
         BCTR  RE,0                                                             
         EXMVC RE,TEMP,0(R6)       COPY MARKET ELEMENT                          
         LA    R6,TEMP                                                          
         USING MARELEM,R6                                                       
         MVC   MARDATE,TODAYB      TODAY'S DATE ON MKT ELEM                     
         GOTO1 APUTEL,MARELEM      SAVE NEW MKT ELEM ON RECD                    
         DROP  R6                                                               
         L     R6,IPTR                                                          
         ZIC   RE,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RE                                                            
         ST    R6,IPTR             PT TO 1ST DATA ELEMENT                       
*                                                                               
RCNV35   BAS   RE,GETLV            GET FIRST/NEXT  LEVEL IN RECD                
         BAS   RE,CPYELS           COPY ELEMENTS FROM IPTR -> RPTR              
*                                                                               
RCNV40   MVC   PRGKEY,INTKEY                                                    
         L     R6,RPTR                                                          
         CLC   0(3,R6),=X'0F03FF'  END OF ALL PROGRAM DATA (RECORD)             
         BE    TSTADD              YES-TEST IF WE SHOULD ADD THIS LEV           
         CLI   0(R6),X'00'         END OF DATA ON THIS FILE#                    
         BNE   RCNV45              YES                                          
         BAS   RE,FSEQ             READ IN NEXT FILE #                          
         XC    RPTR,RPTR                                                        
         B     RCNV35                                                           
*                                                                               
RCNV45   CLC   RTRK,INTTRK                                                      
         BL    NEXTLV              TRK WANTED NOT FND YET--NEXT LEV             
         BH    TSTADD              LEV NOT FND -- ADD IT OR IGNORE IT           
*NEW CORRECTIONS/DELETIONS FORMAT                                               
*        CLI   INTORIG,C'2'        TRK FND: DELETE ENTIRE TRK?                  
*        BNE   *+14                NO                                           
*        OC    INTTNUM,INTTNUM     ALL TRKG OR SPECIFIC TELC?                   
*        BZ    MATCH               DO FOR ALL TRKGS                             
*                                                                               
         CLC   RTLC,INTTNUM        NO,LOOK FOR SPECIFIC TELECAST?               
         BL    NEXTLV              HAVEN'T REACHED THIS TELC YET                
         BE    MATCH               YES, MATCH                                   
         B     TSTADD              SEE IF ADD REQUEST                           
*                                                                               
NEXTLV   MVI   BYPASS,C'N'         GET NEXT LEVEL-- COPY OLD ELEMENTS           
         B     RCNV35                                                           
*                                                                               
MATCH    CLI   INTORIG,C'2'        DELETION                                     
         BE    *+12                IF DELETION, JUST BUMP PTR                   
         BAS   RE,ADDLEV           ADD CORRECTION LEVEL                         
         B     MATCH1                                                           
         OC    INTTRK,INTTRK       PROGRAM AVERAGE DELETE?                      
         BNZ   *+10                                                             
         OC    INTTNUM,INTTNUM     NO TCAST MEANS TRACK AVG OR PROG AVG         
         BNZ   *+8                                                              
         MVI   PAVGDEL,C'Y'        SET FLAG TO AVERAGE DELETE MATCH             
MATCH1   MVI   BYPASS,C'Y'         DON'T COPY ELEMENTS OF THIS LEVEL            
         BAS   RE,GETLV            BUMP RPTR                                    
         BAS   RE,CPYELS           COPY ELEMENTS START:IPTR TIL RPTR            
         CLI   INTORIG,C'2'        DELETE?                                      
         BNE   RCNVX               DONE PROCESSING THIS LEVEL                   
*NEW CORRECTIONS/DELETIONS FORMAT                                               
*        OC    INTTNUM,INTTNUM                                                  
**       BZ    RCNV35              DELETE ALL TELC FOR THIS LEV                 
*        BNZ   TSTADD                                                           
*        CLI   RTYP,2              ANOTHER TELECAST FOR THIS TRACK?             
*        BE    RCNV35              GO DELETE IT                                 
*                                                                               
TSTADD   CLI   INTORIG,C'2'        DELETE?                                      
**       BE    *+8                 YES                                          
         BE    *+12                                                             
         BAS   RE,ADDLEV           NO, ADD THIS LEVEL                           
         MVI   BYPASS,C'N'         COPY ELEMENTS                                
         B     RCNVX               DONE                                         
         EJECT                                                                  
*                                                                               
RMERGE   DS    0H                                                               
*                                                                               
RMRG10   DS    0H                                                               
         OC    IPTR,IPTR           IS ORIGINAL RECD EMPTY?                      
         BNZ   *+16                                                             
         CLI   INTORIG,C'2'        DELETE REQUEST?                              
         BE    RCNVX               YES, ALREADY DELETED/NOT THERE               
         B     QMERGE              ORIG RECD DOESN'T EXIST                      
*                                                                               
         CLC   IPTR,RPTR           SHOULD BE THE SAME                           
         BE    *+6                 IF NOT, DIE                                  
         DC    H'0'                                                             
*                                                                               
         CLC   PRGKEY(QMKTB-QPRGKD),INTKEY   SAME RECD,DIFF MKT BRK?            
         BNE   RCNV40              NO.PROCESS AS IF FROM SCRATCH                
         CLI   INTORIG,C'2'                                                     
         BNE   QMERGE                                                           
*                                                                               
RCNVX    B     CNVX                                                             
         EJECT                                                                  
***********************************************************************         
*CNTREC- LEAD RECD WITH IMPORTANT CONTROL INFO TO AID THE                       
*        PROCESSING OF THE RECDS BELOW IT FOR THE PROGRAM                       
***********************************************************************         
CNTREC   NTR1                                                                   
         LA    R5,CNTKEY                                                        
         USING PMKEY,R5                                                         
         GOTO1 ABLDREC,DMCB,PMKEY  BUILD CNTL RECD KEY IN AOREC                 
         LA    R6,TEMP             BUILD MARKET TYPE ELEMENT                    
         USING MARELEM,R6                                                       
         XC    TEMP,TEMP                                                        
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
*                                                                               
         LHI   R1,1                STATION CODES >9999 ARE 3-BYTE PWOS          
         CLI   PMSTAT,0            DON'T STORE STATION CODE AS MKT#             
         BE    CNTR10                                                           
*                                                                               
         PACK  DUB,PMSTAT(4)                                                    
         CVB   R1,DUB                                                           
CNTR10   STCM  R1,3,MARNO          CABLE STATION CODE IS MARKET                 
         MVC   MARTYPE,CNTMTYP     MARKET TYPE                                  
         MVC   MARSTYP,CNTSTYP     STATION TYPE                                 
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         XC    TEMP,TEMP                                                        
         USING TIMELEM,R6          DAYS AND TIMES ELEM                          
         MVI   TIMCODE,TIMCODEQ                                                 
         MVI   TIMELN,TIMELNQ                                                   
         OC    PCNTBITS,TCNTBITS                                                
         MVC   TIMBITS,PCNTBITS     MOVE IN BITS                                
         GOTO1 APUTEL,TIMELEM                                                   
         DROP  R6                                                               
*                                                                               
         XC    TEMP,TEMP           SPECIAL FIELDS FOR CNTL RECD                 
         USING CNTELEM,R6                                                       
         MVI   CNTCODE,CNTCODEQ                                                 
         MVI   CNTELN,CNTELNQ                                                   
         MVC   CNTTRKD,CNTUTRK     1=TRACKED EPISODES EXIST                     
         MVC   CNTREPR,CNTORIG     ORIG/CHG/DELETED                             
         GOTO1 APUTEL,CNTELEM                                                   
         DROP  R5                                                               
*                                                                               
CNTRECX  DS    0H                                                               
         BAS   RE,PUTTAPE          RELEASE CONTROL RECD                         
         XC    CNTKEY,CNTKEY       CLEAR CONTROL RECD KEY                       
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*ADDLEV -BUILD ELEMENTS AND DEMOS IN ATREC. COMPARE SIZES. IF IT FITS,          
*        COPY TO AOREC.  IF NOT, RELASE AOREC, CREATE NEW RECD IN               
*        AOREC WITH ELEMENTS IN ATREC.                                          
**********************************************************************          
ADDLEV   NTR1                      BUILD LEVEL IN ATREC FOR SIZING              
         MVC   AOREC,SVATREC                                                    
         GOTO1 ABLDREC,DMCB,THISKEY                                             
*                                                                               
         LA    RE,INTKEY           WAS THERE A TRKG AVG REC FOR TELC?           
         USING QPRGKD,RE                                                        
         OC    QTELC,QTELC         IS THIS IS A TRK AVG RECD                    
         BZ    ADD5                YES                                          
         MVI   NOSUMY,0                                                         
         CLC   OTRK,INTTRK         DOES A TRK AVG EXIST?                        
         BE    ADD5                YES                                          
         MVI   NOSUMY,2            NO,CREATE TRK AVG ELEMS --NO DEMS            
         BAS   RE,BLDELEM                                                       
         DROP  RE                                                               
*                                                                               
ADD5     BAS   RE,BLDELEM          CREATE ELEMS FOR THIS TRK/TELECAST           
         BAS   RE,BLDEMS                                                        
         L     RE,SVATREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,PMRLEN-PMKEY+4(RE)   RF=LENGTH OF ATREC                     
         LA    RE,PMDATA-PMKEY     LENGTH OF KEY                                
         SR    RF,RE                                                            
         L     RE,SVAOREC          LENGTH OF RECD IN AOREC                      
         SR    R1,R1                                                            
         ICM   R1,3,PMRLEN-PMKEY+4(RE)  R1=LENGTH OF AOREC                      
         AR    R1,RF                                                            
         MVC   AOREC,SVAOREC                                                    
         CH    R1,=H'1980'         MAX RECD LENGTH                              
         BH    ADD20               RECD TOO BIG                                 
         L     R1,SVATREC                                                       
         LA    R1,PMDATA-PMKEY+4(R1)                                            
*                                                                               
ADD10    CLI   0(R1),0                                                          
         BE    ADD30                                                            
         CLI   0(R1),MARCODEQ      ALREADY CREATED                              
         BE    ADD15                                                            
         GOTO1 APUTEL,(R1)                                                      
ADD15    ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     ADD10                                                            
*                                                                               
ADD20    BAS   RE,PUTTAPE          RELASE RECD IN AOREC                         
         L     RE,SVAOREC          CLEAR BUFFER                                 
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R6,SVATREC          PT TO ATREC                                  
         LA    R6,4(R6)                                                         
         USING PMKEY,R6                                                         
         BAS   RE,DDSFIL           BUMP FILENUM                                 
         MVC   PMPNUM,FILENUM                                                   
         L     RE,SVATREC                                                       
         L     RF,SVAOREC                                                       
         ICM   R1,3,PMRLEN                                                      
         LA    R1,4(R1)                                                         
         MOVE  ((RF),(R1)),(RE)      AOREC <--ATREC                             
         LA    R6,THISKEY                                                       
         MVC   PMPNUM,FILENUM                                                   
*                                                                               
ADD30    DS    0H                                                               
         MVC   PREVKEY,THISKEY                                                  
         MVC   AOREC,SVAOREC                                                    
         MVC   OTRK,INTTRK                                                      
         MVC   OTLC,INTTNUM                                                     
*                                                                               
*        MVC   LTYP,RTYP                                                        
*        MVC   LSTIM,RSTIM                                                      
*        MVC   LETIM,RETIM                                                      
*        MVC   LDAY,RDAY                                                        
*        MVC   LSQH,RSQH                                                        
*        MVC   LDURM,RDURM                                                      
*        MVC   LDAYWK,RDAYWK                                                    
*        MVC   LTRK,RTRK                                                        
*                                                                               
*        MVC   RTRK,INTTRK                                                      
*        LA    RE,INTKEY                                                        
*        USING QPRGKD,RE                                                        
*        OC    QTELC,QTELC                                                      
*        BZ    *+10                                                             
*        MVC   RTLC,INTTNUM                                                     
*        DROP  RE                                                               
*        MVC   RTYP,TYPE                                                        
*        MVC   REPS,INTEPS                                                      
*                                                                               
ADDLEVX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*CPYELS -COPY ELEMENTS FROM ORIG RECD IN AIREC TO NEW RECD IN AOREC             
***********************************************************************         
CPYELS   NTR1                                                                   
         CLC   IPTR,RPTR                                                        
         BE    CPYELX              NO ELEMENTS NEED TO BE COPIED                
         CLI   BYPASS,C'Y'         DON'T COPY-- BYPASS ELEMENTS                 
         BNE   CPY7                                                             
******** KEEP NON-DEMO ELEMENTS IF PROGRAM AVERAGE DELETE ******                
         CLI   PAVGDEL,C'Y'        PROGRAM AVERAGE DELETE?                      
         BNE   CPY5                                                             
         L     R6,IPTR             START ADDRESS                                
         L     R5,RPTR             END ADDRESS                                  
         SR    RF,RF                                                            
CPY2     CR    R6,R5                                                            
         BNL   CPY4                                                             
         CLI   0(R6),SLCODEQ       X'23' ELEM MARKS BEGINNING OF DEMOS          
         BNL   CPY4                                                             
         ZIC   R1,1(R6)                                                         
         AR    RF,R1               ACUMULATE LENGTH                             
         LA    R6,0(R1,R6)                                                      
         B     CPY2                                                             
CPY4     SR    R1,R1                                                            
         L     RE,AOREC                                                         
         ICM   R1,3,PMRLEN-PMKEY+4(RE)    LENGTH OF RECD IN AOREC               
         AR    R1,RF                                                            
         CH    R1,=H'1980'                                                      
         BL    CPY4A               THERE'S ENOUGH ROOM IN RECD                  
         BAS   RE,PUTTAPE          RELEASE RECD IN AOREC                        
         L     RE,AOREC            CLEAR BUFFER                                 
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R6,THISKEY                                                       
         USING PMKEY,R6                                                         
         MVI   USETHBK,C'Y'                                                     
         BAS   RE,DDSFIL           BUMP FILENUM                                 
         MVI   USETHBK,C'N'        RESET DEFAULT                                
         MVC   PMPNUM,FILENUM                                                   
         GOTO1 ABLDREC,DMCB,THISKEY     CONTINUED OUTPUT RECD                   
*                                                                               
CPY4A    L     R6,IPTR             START ADDRESS                                
         L     R5,RPTR             END ADDRESS                                  
CPY4B    CR    R6,R5                                                            
         BNL   CPY5                                                             
         CLI   0(R6),SLCODEQ       X'23' ELEM MARKS BEGINNING OF DEMOS          
         BNL   CPY5                                                             
         GOTO1 APUTEL,(R6)                                                      
         ZIC   R1,1(R6)                                                         
         LA    R6,0(R1,R6)                                                      
         B     CPY4B                                                            
*                                                                               
CPY5     MVC   IPTR,RPTR           BUMP IPTR                                    
         MVI   PAVGDEL,C'N'        RESET AVERAGE FLAG                           
         B     CPYELX                                                           
*                                                                               
CPY7     CLI   LTYP,0              PROGRAM AVERAGE?                             
         BNE   CPY8                                                             
         LA    R6,=C'SET'          SET PROGRAM AVE CONTROL BITS                 
         ICM   R6,8,LDAY                                                        
         GOTO1 =V(DETABQH),DMCB,(R6),PCNTBITS,LSTIM,LETIM                       
         B     CPY10                                                            
*                                                                               
CPY8     DS    0H                  TRACKAGE OR TELECAST=SPECL/BREAKOUT          
* CLEAR BITS SET BY PRG AVG FOR THIS DAY                                        
* REASON: IF THERE IS A TRACK/TELC FOR DAY,OVERWRITE PRG AVE FOR DAY            
* (ALL TIMES). IF THERE ARE DAYS IN PRG AVG W/O TRACKS/TELECASTS, KEEP          
* THE TIME GIVEN BY PRG AVG.                                                    
         LA    R6,=C'CLR'                                                       
         ICM   R6,8,LDAY                                                        
         GOTO1 =V(DETABQH),DMCB,(R6),PCNTBITS,=H'0000',=H'2359'                 
* SET TRACKAGE/TELECAST CONTROL BITS                                            
         LA    R6,=C'SET'                                                       
         ICM   R6,8,LDAY                                                        
         GOTO1 =V(DETABQH),DMCB,(R6),TCNTBITS,LSTIM,LETIM                       
*        MVI   DMCB,1              DON'T GENERATE N-PTRS FOR CNAD               
*        BAS   RE,GENN             GENERATE PASSIVE PTRS (N-RECS)               
*                                                                               
CPY10    L     R6,IPTR             START ADDRESS                                
         L     R5,RPTR             END ADDRESS                                  
         SR    R5,R6                                                            
         L     RE,AOREC                                                         
         SR    RF,RF                                                            
         ICM   RF,3,PMRLEN-PMKEY+4(RE)    LENGTH OF RECD IN AOREC               
         AR    RF,R5                                                            
         CH    RF,=H'1980'                                                      
         BL    CPY20               THERE'S ENOUGH ROOM IN RECD                  
*                                                                               
         BAS   RE,PUTTAPE          RELEASE RECD IN AOREC                        
         L     RE,AOREC            CLEAR BUFFER                                 
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R6,THISKEY                                                       
         USING PMKEY,R6                                                         
         MVI   USETHBK,C'Y'                                                     
         BAS   RE,DDSFIL           BUMP FILENUM                                 
         MVI   USETHBK,C'N'        RESET DEFAULT                                
         MVC   PMPNUM,FILENUM                                                   
         GOTO1 ABLDREC,DMCB,THISKEY     CONTINUED OUTPUT RECD                   
*                                                                               
CPY20    L     R6,IPTR             START ADDRESS                                
         L     R5,RPTR             END ADDRESS                                  
*                                                                               
CPY30    CLI   0(R6),X'00'         END OF RECORD?                               
         BE    CPY40                                                            
         CLI   0(R6),MARCODEQ      DON'T COPY PREV MKT ELEMS                    
         BE    CPY35               SKIP, GO TO NEXT ELEMNT                      
         GOTO1 APUTEL,(R6)         ADD ELEMENTS TO AOREC                        
CPY35    ZIC   RE,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RE                                                            
         CR    R6,R5               HAVE WE COPIED ALL ELEMENTS YET?             
         BL    CPY30               YES                                          
         ST    R6,IPTR             NEW IPTR                                     
*                                                                               
CPY40    MVC   OTRK,LTRK           INFO OF LAST LEV WRITTEN TO NEW REC          
         MVC   OTLC,LTLC                                                        
*                                                                               
CPYELX   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*FINFF - ADD FINAL '0F/FF'  ELEMENT TO AOREC, RELEASE RECD AND                  
*        CLEAR PREVKEY                                                          
***********************************************************************         
FINFF    NTR1                                                                   
         LA    R6,TEMP             BUILD FINAL '0F-03-FF' ELEMENT               
         XC    TEMP,TEMP                                                        
         USING DIDELEM,R6                                                       
         MVI   DIDCODE,DIDCODEQ                                                 
         MVI   DIDELLN,DIDELNEQ                                                 
         MVI   DIDTYPE,X'FF'                                                    
         GOTO1 APUTEL,DIDELEM                                                   
         DROP  R6                                                               
         BAS   RE,PUTTAPE          RELEASE RECORD                               
         XC    PREVKEY,PREVKEY                                                  
FINFFX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*RELS -  RELEASE REST OF PROGRAM RECD                                           
***********************************************************************         
RELS     NTR1                                                                   
         OC    RPTR,RPTR           IF EMPTY, NO ORIG RECD EXISTS                
         BZ    RELSX                                                            
         MVI   BYPASS,C'N'         SET TO COPY REMAINING ELEMS                  
         L     RE,RPTR                                                          
         CLI   0(RE),X'0F'         REACHED NEXT LEVEL?                          
         BE    *+8                 YES. COPY REST OF ELEMENTS                   
         MVI   BYPASS,C'Y'         STILL ON THE LEVEL THAT WAS PROCESSD         
*                                  SKIP THESE ELEMENTS                          
RELS10   L     RE,RPTR                                                          
         CLC   0(3,RE),=X'0F03FF'                                               
         BE    RELSX                                                            
*                                                                               
RELS20   CLI   0(RE),X'00'         END OF RECD (FILE#), BUMP FILE#              
         BNE   RELS30                                                           
         BAS   RE,FSEQ             REST OF DATA FOR THIS PRGM                   
         XC    RPTR,RPTR                                                        
         L     RE,AIREC                                                         
         LA    RE,PMDATA-PMKEY+4(RE)                                            
         ST    RE,IPTR                                                          
RELS30   BAS   RE,GETLV                                                         
         L     RE,RPTR                                                          
         CLI   0(RE),X'0F'         REACHED NEXT LEVEL YET?                      
         BNE   RELS35                                                           
         CLI   BYPASS,C'Y'         DID WE COPY SO FAR?                          
         BNE   *+14                YES.KEEP COPYING                             
         MVI   BYPASS,C'N'         NO.FIRST 0F ELEM.START COPYING               
         MVC   IPTR,RPTR            STARTING FROM NEW LEVEL('0F'ELEM)           
RELS35   BAS   RE,CPYELS                                                        
         B     RELS10                                                           
*                                                                               
RELSX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*GETORIG- READ IN AIREC THE OLD/ORIGINAL RECRD WHICH NEEDS REPROCESSING         
***********************************************************************         
GETORIG  NTR1                                                                   
         LA    R6,IKEY             BUILD 'Q' RECD USING FILE NUMBER             
         USING PMKEY,R6                                                         
         XC    IKEY,IKEY                                                        
         MVC   PMCODE(3),=C'QCN'                                                
         CLC   INTBOOK,=AL2(JAN_07)    HAVE ALL STREAMS AFTER JAN/07            
         BL    *+10                                                             
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMSTYP,INTSTYP                                                   
*                                                                               
         MVC   SVIKEY,IKEY                                                      
         BAS   RE,DHIGH            READ 'Q' RECD WITH PASSIVE KEY               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVIKEY(PMKSTAT-PMKEY),IKEY   GET DISK ADDR FOR BK/NET            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,PMNDXDA-PMKEY(R6)                                           
         MVC   IKEY,SVIKEY         RESTORE ORIG KEY                             
         MVC   PMPNUM,OLDFIL                                                    
         MVC   SVIKEY,IKEY                                                      
         L     RE,AIREC                                                         
         MVC   4(L'SVIKEY,RE),SVIKEY                                            
         BAS   RE,FHIGH            READ FILE INTO AIREC                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                ERROR READING FILE                           
         CLC   IKEY(PMRLEN-PMKEY),SVIKEY                                        
         BE    *+6                                                              
         DC    H'0'                                                             
GETORGX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*GETLV - BUMP TO NEXT '0F' ELEMENT. DETERMINE WHAT LEVEL TYPE AND SAVE          
*        IN : RVARS: RTYPE, RTRK, REPS, RTLC                                    
*        ADDRESS OF '0F' WILL GET SAVED IN RPTR                                 
*                                                                               
***********************************************************************         
GETLV    NTR1                                                                   
         MVC   LTYP,RTYP           SAVE PREV LEVEL'S VALUES                     
         MVC   LSTIM,RSTIM                                                      
         MVC   LETIM,RETIM                                                      
         MVC   LDAY,RDAY                                                        
         MVC   LSQH,RSQH                                                        
         MVC   LDURM,RDURM                                                      
         MVC   LDAYWK,RDAYWK                                                    
         MVC   LTRK,RTRK                                                        
         MVC   LTLC,RTLC                                                        
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         MVC   DATADISP,=H'23'     DISP TO FIRST DATA ELEMENT                   
         L     R6,RPTR                                                          
         OC    RPTR,RPTR           START OF RECD?                               
         BZ    GETLV5                                                           
         CLI   RTYP,X'FF'                                                       
         BE    GETLVX              DONE PROCESSING THIS PRG RECD                
         BAS   RE,NEXTEL                                                        
         B     GETLV10                                                          
*                                                                               
GETLV5   DS    0H                                                               
         L     R1,AIREC            IPTR-> FIRST ELEM ON RECORD                  
         LA    R1,4(R1)            (EXCEPT IF '01'ELEM,GO TO NEXT ELEM)         
         AH    R1,DATADISP                                                      
         ST    R1,IPTR                                                          
         CLI   0(R1),MARCODEQ                                                   
         BNE   GETLV6                                                           
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         ST    R1,IPTR                                                          
*                                                                               
GETLV6   L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         BAS   RE,GETEL                                                         
*                                                                               
GETLV10  ST    R6,RPTR             SAVE A(0F ELEMENT) IN IREC                   
         BNE   GETLVX              RTN CODE OF NEXTEL/GETEL: ANY '0F'S          
*                                                                               
GETLV12  DS    0H                                                               
         MVC   RTYP,2(R6)                                                       
         XC    RTLC,RTLC                                                        
         XC    REPS,REPS                                                        
         CLI   RTYP,X'FF'          END OF PRG RECD                              
         BE    GETLVX                                                           
         CLI   RTYP,0              PRG AVG RECD                                 
         BNE   GETLV15                                                          
         XC    RTRK,RTRK           PRG TOTAL AVG                                
         XC    RTLC,RTLC                                                        
         XC    REPS,REPS                                                        
*        B     GETLVX                                                           
*                                                                               
GETLV15  ZIC   RE,1(R6)                                                         
         AR    R6,RE               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),X'10'         PHTELEM?                                     
         BNE   GETLV15                                                          
         CLI   RTYP,0                                                           
         BE    GETLV18                                                          
         CLI   RTYP,1              TRK AVG?                                     
         BNE   *+14                                                             
         MVC   RTRK,PHTNTI-PHTELEM+3(R6)  TRACKAGE NUMBER                       
         B     *+10                                                             
         MVC   REPS,PHTNTI-PHTELEM+2(R6)  EPISODE NUMBER (BLANK)                
*                                                                               
GETLV18  ZIC   RE,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RE                                                            
         CLI   0(R6),X'20'         PHELEM?                                      
         BNE   GETLV18                                                          
*        CLI   RTYP,2                                                           
*        BNE   GETLV20                                                          
         MVC   RDAYWK,PHDWKS-PHELEM(R6)                                         
*                                                                               
GETLV20  ZIC   RE,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RE                                                            
         CLI   0(R6),X'22'         NTELEM?                                      
         BNE   GETLV20                                                          
         USING NTELEM,R6                                                        
         CLI   RTYP,2                                                           
*        BNE   GETLVX                                                           
         BNE   *+10                                                             
         MVC   RTLC,NTTNUM                                                      
         MVC   RSTIM,NTSTIM        MILITARY START TIME                          
         MVC   RETIM,NTETIM        END TIME                                     
         MVC   RDAY,NTDAY          DAY                                          
         MVC   RSQH,NTSQH          START QUARTER HOUR                           
         MVC   RDURM,NTDUR          DURATION IN MINUTES                         
*                                                                               
GETLVX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*NTIDDS- FOR EACH 10 CHAR NTI NUMBER CREATE A 2-CHAR DDS PRG NUMBER             
*        SAVE DDS NUMBER IN TABLE TO BE REALEASED DURING THE LAST               
*        HOOK AS PASSIVE RECORDS                                                
***********************************************************************         
NTIDDS   NTR1                                                                   
         LA    R5,KEY              LOOK UP NTI PRG CODE PASSIVE RECD            
         XC    KEY,KEY                                                          
         USING PJKEY,R5                                                         
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   PJSTAT(4),INTSTA    NETWORK                                      
         MVI   PJSTAT+4,C'C'       CABLE                                        
         MVC   PJEXTNUM,INTPNUM    NTI NUMBER                                   
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,HIGH                                                          
         XC    DDSNUM,DDSNUM                                                    
         CLC   KEY(PJINTNUM-PJKEY),KEYSAVE   WAS NTI-DDS NUMBER FOUND           
         BNE   *+14                                                             
         MVC   DDSNUM,PJINTNUM                                                  
         B     NTIDDSX                                                          
         CLI   INTORIG,C'2'        DELETION REQUEST?                            
         BE    NTIDDSX             YES, BUT REC DOESN'T EXIST- EXIT             
*                                                                               
         MVC   KEY,KEYSAVE         GET HIGHEST DDS NUMBER                       
         MVC   PJEXTNUM,=5X'00'                                                 
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(PJINTNUM-PJKEY),KEYSAVE    SAME NETWORK/BOOK?                
         BNE   *+16                                                             
         MVC   DDSNUM,PJINTNUM     LATEST DDS PRG NUMBER                        
         XC    DDSNUM,=X'FFFF'                                                  
         L     R1,=A(NTITBL)           PT TO NTI-DDS TABLE                      
         XC    DMCB(4),DMCB                                                     
*                                                                               
NTIDD5   OC    0(5,R1),0(R1)       SEARCH TABLE FOR NTI PRG NUMBER              
         BZ    NTIDD10                                                          
         CLC   0(5,R1),=X'FFFFFFFFFF'                                           
         BNE   *+6                 NTITBL OVERFLOW                              
         DC    H'0'                                                             
         CLC   0(5,R1),INTSTA      COMPARE ON NETWORK                           
         BNE   NTIDD7                                                           
         CLC   5(5,R1),INTPNUM     SAME NTI PRG NUMBER?                         
         BNE   *+14                NEWTORK & PRG NUMBER FOUND                   
         MVC   DDSNUM,10(R1)       DDS NUMBER FOUND                             
         B     NTIDDSX                                                          
         CLC   5(5,R1),=5X'FF'     HIGHEST DDS NUMBER FOR NETWORK?              
         BNE   NTIDD7                                                           
         MVC   DDSNUM,10(R1)       SAVE LATEST  DDS #                           
         ST    R1,DMCB             SAVE ADR OF 'FF' ENTRY IN TABLE              
NTIDD7   LA    R1,L'NTITBL(R1)     NOT FOUND                                    
         B     NTIDD5                                                           
*                                                                               
NTIDD10  LH    RE,DDSNUM           DDS INTERNAL NUMBER                          
         LA    RE,1(RE)                                                         
         STH   RE,DDSNUM                                                        
         MVC   0(5,R1),INTSTA      SAVE NETWORK                                 
         MVC   5(5,R1),INTPNUM     SAVE NTI PRG NUMBER                          
         MVC   10(2,R1),DDSNUM     SAVE DDS INTERNAL PRG NUMBER                 
         LA    R1,L'NTITBL(R1)                                                  
*                                                                               
         OC    DMCB,DMCB                                                        
         BZ    *+8                                                              
         L     R1,DMCB             GET ADDR OF 'FF' IN TABLE                    
         MVC   0(5,R1),INTSTA      SAVE NETWORK                                 
         MVC   5(5,R1),=5X'FF'     LATEST DDS NUMBER FOR NETWORK                
         MVC   10(2,R1),DDSNUM     SAVE DDS INTERNAL PRG NUMBER                 
         B     NTIDDSX                                                          
*                                                                               
NTIDDSX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*DDSFIL- SAVE THE INTERNAL FILE NUMBER FOR EACH DDS INTERNAL                    
*        PROGRAM NUMBER.  IF NEW NETWORK, RESTART FILE NUMBER                   
***********************************************************************         
DDSFIL   NTR1                                                                   
         CLI   USETHBK,C'Y'        USE THISKY FIELD VS INTERD                   
         BE    DDSF30               YES, JUST BUMP FILENUM FOR NEW KEYS         
         MVC   DDSBK,INTBOOK                                                    
         MVC   DDSBKTY,INTBTYP                                                  
         MVI   DUB,0               PROGRAM NUMBER NOT IN TABLE                  
         XC    TEMP(4),TEMP                                                     
         L     R1,=A(DDSTBL)       PT TO NTI-DDS TABLE                          
         USING DDSTBLD,R1                                                       
*                                                                               
DDSF5    CLC   0(5,R1),=X'FFFFFFFFFF'                                           
         BNE   *+6                 DDSTBL OVERFLOW                              
         DC    H'0'                                                             
         OC    DDSBOOK,DDSBOOK     END OF TABLE?                                
         BZ    DDSF10                                                           
         CLC   DDSBOOK,INTBOOK     SAME BOOK?                                   
         BNE   DDSF7                                                            
         CLC   DDSNET,INTSTA       COMPARE ON NETWORK                           
         BNE   DDSF7                                                            
         CLC   DDSSTYP,INTSTYP     COMPARE ON STATION TYPE                      
         BNE   DDSF7                                                            
         CLC   DDSPRG,DDSNUM       SAME DDS PRG NUMBER?                         
         BNE   DDSF6               NEWTORK & PRG NUMBER FOUND                   
         MVI   DUB,1               PROGRAM NUMBER PRESENT                       
         OC    TEMP(4),TEMP        WAS 'FF' RECD FOUND ALREADY?                 
         BNZ   DDSF15              YES- BUMP FILENUM AND UPDATE 'FFFF'          
         B     DDSF7                                                            
*                                                                               
DDSF6    CLC   DDSPRG,=X'FFFF'     LATEST FILE NUMBER                           
         BNE   DDSF7                                                            
         MVC   FILENUM,DDSFILE     SAVE LATEST FILE NUMBER                      
         STCM  R1,15,TEMP          SAVE ADDRESS OF ENTRY                        
         CLI   DUB,1               WAS PRG NUMBER FOUND ALREADY?                
         BE    DDSF15              YES                                          
*                                                                               
DDSF7    LA    R1,L'DDSTBL(R1)     NOT FOUND                                    
         B     DDSF5                                                            
*                                                                               
DDSF10   OC    TEMP(4),TEMP        WAS 'FFFF' RECD FOUND?                       
         BNZ   DDSF12              YES,                                         
         XC    FILENUM,FILENUM     NO, RESET FILE NUMBER                        
         CLI   INTORIG,C'0'        IF CRCT REC WE MUST HAVE 'FFFF' REC          
         BE    DDSF12                                                           
         MVI   COPY,C'N'           ONLY READ RECDS/DON'T COPY                   
         CLC   LSTBK,INTBOOK       HAVE WE ALREADY DONE THE READ?               
         BNE   *+14                                                             
         CLC   LSTSTA,INTSTA                                                    
         BE    *+8                                                              
         BAS   RE,RDRECS           GET LATEST FILE# FROM FILE                   
         MVC   FILENUM,LSTFNUM                                                  
*                                  IF NOT ON FILE, FILE#=0                      
DDSF12   LH    RE,FILENUM                                                       
         LA    RE,1(RE)                                                         
         STH   RE,FILENUM                                                       
         MVC   DDSBOOK,INTBOOK     SAVE BOOK                                    
         MVC   DDSNET,INTSTA       SAVE NETWORK                                 
         MVC   DDSPRG,DDSNUM       SAVE DDS PRG NUMBER                          
         MVC   DDSSTYP,INTSTYP     SAVE STATION TYPE                            
         MVC   DDSFILE,FILENUM     SAVE DDS INTERNAL PRG NUMBER                 
         LA    R1,L'DDSTBL(R1)     IF 'FF' DOESNT EXIST, ADD IT                 
         B     DDSF20                                                           
*                                                                               
DDSF15   LH    RE,FILENUM          BUMP LATEST DDS FILE NUMBER                  
         LA    RE,1(RE)                                                         
         STH   RE,FILENUM                                                       
*                                                                               
DDSF20   DS    0H                                                               
         OC    TEMP(4),TEMP        DO WE HAVE ADDRESS OF 'FFFF' RECD?           
         BZ    *+8                 IF ZERO, R1 PTS TO END OF TABLE              
         ICM   R1,15,TEMP          ADDRESS OF LAST FILE NUMBER RECORD           
         MVC   DDSBOOK,INTBOOK     SAVE BOOK                                    
         MVC   DDSNET,INTSTA       SAVE NETWORK                                 
         MVC   DDSPRG,=X'FFFF'     LATEST FILE NUMBER MARKER                    
         MVC   DDSFILE,FILENUM     SAVE LATEST FILE NUMBER                      
         MVC   DDSSTYP,INTSTYP     STATION TYPE                                 
         B     XIT                                                              
*                                                                               
DDSF30   DS    0H             BUMP FILE NUMBER BASED ON BK IN THISKEY           
         L     R1,=A(DDSTBL)                                                    
         LA    RE,THISKEY                                                       
DDSF35   CLC   0(5,R1),=X'FFFFFFFFFF'                                           
         BNE   *+6                                                              
         DC    H'0'                BOOK NOT IN TABLE ALREADY???                 
         CLC   DDSPRG,=X'FFFF'                                                  
         BNE   DDSF40                                                           
         CLC   DDSBOOK,PMBOOK-PMKEY(RE)                                         
         BNE   DDSF40                                                           
         CLC   DDSNET,PMSTAT-PMKEY(RE)                                          
         BNE   DDSF40                                                           
         CLC   DDSSTYP,PMSTYP-PMKEY(RE)                                         
         BNE   DDSF40                                                           
         SR    RE,RE                                                            
         ICM   RE,3,DDSFILE                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,DDSFILE                                                     
         MVC   FILENUM,DDSFILE                                                  
         B     DDSF50                                                           
*                                                                               
DDSF40   LA    R1,L'DDSTBL(R1)                                                  
         B     DDSF35                                                           
*                                                                               
DDSF50   B     XIT                                                              
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
*BLDELEM - BUILD ELEMENTS FOR EACH OF TH  -P- AND -Q- RECS                      
**********************************************************************          
BLDELEM  NTR1                                                                   
         CLI   THISKEY,PMCODEQU    IF NOT Q-RECD JUST BUILD MKT ELEM            
         BNE   BLDEL05                                                          
         CLI   NOSUMY,1            FORCED PRG SUMMARY RECD?                     
         BE    BLDEL05             YES, OUTPUT ELEMENT                          
         OC    INTTRK,INTTRK                                                    
         BNZ   BLDEL08                                                          
         OC    INTTNUM,INTTNUM     PRG SUMMARY?                                 
         BNZ   BLDEL08                                                          
*                                                                               
BLDEL05  LA    R6,TEMP             BUILD MARKET TYPE ELEMENT                    
         XC    TEMP,TEMP                                                        
         USING MARELEM,R6                                                       
         MVI   MARCODE,MARCODEQ    X'01' ELEMENT                                
         MVI   MARELN,MARLNEQ                                                   
         LA    R1,1                1 = USA                                      
         CLC   INTSTA(4),=C'HUT '  ASSIGN HUT MKT = 1                           
         BE    BLDEL06                                                          
         CLC   INTSTA(4),=C'UUUU'  TOTAL US UNIVERSE.ASSIGN MKT = 1             
         BE    BLDEL06                                                          
         CLI   INTSTA,0            STATION CODES >9999 ARE 3 BYTE PWOS          
         BE    BLDEL06             DO NOT STORE STATION AS MKT NUMBER           
*                                                                               
         PACK  DUB,INTSTA(4)       CHARACTER COVERAGE AT INTSTA                 
         CVB   R1,DUB                                                           
BLDEL06  STCM  R1,3,MARNO          CABLE STATION CODE IS MARKET                 
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         CLC   INTSTA(4),=C'UUUU'  UNIVERSE P-RECD?                             
         BE    BLDEL08             DEMOS ELEMENTS ONLY                          
         CLC   INTSTA+4(1),=C'U'   UNIVERSE P-RECD?                             
         BE    BLDEL08             DEMOS ELEMENTS ONLY                          
*                                                                               
         CLC   INTBOOK,=AL2(JAN_06)                                             
         BL    BLDEL08                                                          
         L     RF,=A(BLDDT)                                                     
         BRAS  RE,BLDDT                                                         
         GOTO1 APUTEL,TEMP                                                      
*                                                                               
BLDEL08  CLI   THISKEY,PMCODEQU    --Q-- RECORD ONLY                            
         BNE   BLDEL60             GO BUILD 'P' RECORD ELEMENTS                 
         LA    R6,TEMP             BUILD PRG/TRKG/EPISODE ID ELEMENT            
         XC    TEMP,TEMP                                                        
         USING DIDELEM,R6                                                       
         MVI   DIDCODE,DIDCODEQ      '0F' ELEMENT                               
         MVI   DIDELLN,DIDELNEQ      LENGTH                                     
         MVI   DIDTYPE,0            PROGRAM SUMARY                              
         CLI   NOSUMY,1            CREATE SET OF PRG HEADER ELEMENTS            
         BE    BLDEL10                                                          
         CLI   NOSUMY,2            CREATE  TRKG SUMMRY HDR ELEMNTS              
         BNE   *+12                                                             
         MVI   DIDTYPE,1            DUMMY HDR FOR MISSING TRKG AVG              
         B     BLDEL10                                                          
*                                                                               
         OC    INTTRK,INTTRK                                                    
         BZ    *+8                                                              
         MVI   DIDTYPE,1            TRACKAGE SUMARY                             
         OC    INTTNUM,INTTNUM                                                  
         BZ    *+8                                                              
         MVI   DIDTYPE,2            EPISODE/TELECAST                            
*                                                                               
BLDEL10  MVC   TYPE,DIDTYPE        SAVE IDTYPE                                  
         GOTO1 APUTEL,DIDELEM                                                   
         MVI   NOSUMY,0            RESET FLAG                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP                                                          
         USING PHTELEM,R6          BUILD NETWORK PRG/TRK/EPISODE ELMT           
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHTCODE,PHTCODEQ    X'10' ELEMENT                                
         MVI   PHTLEN,PHTLNEQ                                                   
         MVC   PHTDTYP,INTDTYP                                                  
****     MVC   PHTDPT,INTDPT                                                    
         MVC   PHTPREM,INTPREM                                                  
****     MVC   PHTRCH,INTAUD                                                    
         MVC   PHTSCNT,INTSTAC                                                  
         MVC   PHTCOVR,INTCOV      TOTAL PRG CONTRIBUTING DURATION              
         MVC   PHTRSF,INTRSF                                                    
         CLI   TYPE,0              PROGRAM SUMARY                               
         BNE   *+10                                                             
         MVC   PHTNTI,INTPNUM                                                   
         CLI   TYPE,1              TRACKAGE?                                    
         BNE   *+10                                                             
         MVC   PHTNTI+3(2),INTTRK                                               
****     CLI   TYPE,2                                                           
****     BNE   *+10                                                             
****     MVC   PHTNTI+2(3),INTEPS  EPISODE  - NO EPISODE NO PROVIDED            
         MVC   PHTPTYP4,INTPTYP    PROGRAM TYPE                                 
         MVC   PHTPTYP,=C'  '                                                   
         CLC   PHTPTYP4+2(2),=C'  '                                             
         BNE   *+10                                                             
         MVC   PHTPTYP,PHTPTYP4                                                 
         MVC   PHTSPTYP,INTSBTYP   SUB PROGRAM TYPE                             
         MVC   PHTDDS,DDSNUM       DDS INTERNAL PROGRAM NUMBER                  
         GOTO1 APUTEL,PHTELEM                                                   
         DROP  R6                                                               
*                                                                               
         USING PHELEM,R6           BUILD DAY/QTR HOUR ELEMENT                   
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHCODE,PHCODEQ      X'20' ELEMENT                                
         MVI   PHELN,PHELNEQ                                                    
         MVC   PHPNUM,INTPNUM                                                   
         MVC   PHDUR,INTDUR        QUARTER HOUR DURATION                        
         MVC   PHDTYPE,INTDTYP                                                  
         MVC   PHDWKS,INTDAYWK                                                  
         NI    PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,INTBOOK                                                  
         MVC   PHDURTOT,INTDUR     TOTAL QHRS ON THIS RECD                      
         GOTO1 APUTEL,PHELEM                                                    
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING PPNELEM,R6          BUILD PROGRAM NAME ELEMENT                   
         MVI   PPNCODE,PPNCODEQ    X'21' ELEMENT                                
         CLI   TYPE,0                                                           
         BNE   *+10                                                             
         MVC   PPNNME(L'INTPNAME),INTPNAME    PROGRAM NAME                      
         CLI   TYPE,1                                                           
         BNE   *+10                                                             
         MVC   PPNNME(L'INTPNAME),INTTRNAM    TRACKAGE NAME                     
         CLI   TYPE,2                                                           
         BNE   *+10                                                             
         MVC   PPNNME(L'INTPNAME),INTEPNAM    EPISODE NAME- (NO LONGER)         
*                                                                               
         OC    PPNNME(L'INTPNAME),BLANKS             BLNK PAD PRG NAME          
         CLC   PPNNME(L'INTPNAME),BLANKS                                        
         BE    BLDEL30                                                          
         LA    R5,PPNNME+L'INTPNAME-1                                           
         LA    R1,L'INTPNAME-1                                                  
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,3(R1)                                                         
         STC   R1,PPNELN                                                        
         GOTO1 APUTEL,PPNELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP                                                          
         USING NTELEM,R6           BUILD NETWORK PROG RUN/TIME ELEM             
BLDEL30  XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU     X'22' ELEMENT                                
*        MVI   NTLEN,NTLENEQ                                                    
         MVI   NTLEN,NTLENEQ2                                                   
         MVC   NTSQH,INTSQH                                                     
         MVC   NTEQH,INTEQH                                                     
         MVC   NTDUR,INTDURM       DURATION IN MINUTES                          
         MVC   NTDUR2,INTDURM2                                                  
         MVC   NTWKS2,INTWEEKS                                                  
         MVC   NTSTIM,INTSTIM                                                   
         MVC   NTETIM,INTETIM                                                   
         MVC   NTDAY,INTDYBIT      DAY CODE'S BIT SETTING                       
         MVC   NTFEED,INTFEED                                                   
         MVC   NTAUDES,INTAUDES                                                 
         CLI   TYPE,2              TELECAST RECORD?                             
         BNE   *+10                                                             
         MVC   NTTNUM,INTTNUM                                                   
         MVC   NTCOVCL,INTCOVCL                                                 
****     MVC   NTLIB,INTLIB         THESE FIELDS ARE NOT ON CBL NAD MIT         
****     MVC   NTCLTEP,INTCLTEP                                                 
         MVC   NTCMCL,INTCMCL                                                   
****     MVC   NTLIVE,INTLIVE                                                   
****     MVC   NTPOA,INTPOA                                                     
****     MVC   NTEOA,INTEOA                                                     
         MVC   NTGAP,INTGAP                                                     
         GOTO1 APUTEL,NTELEM                                                    
*&&DO                                                                           
         CLI   INTDAYWK,X'00'      BUILD VAR RUN/TIME ELEMENTS. M-F?            
         BE    *+20                                                             
         CLI   INTDAYWK,X'80'      M-S?                                         
         BE    *+12                                                             
         CLI   INTDAYWK,X'90'      VAR                                          
         BNE   BLDEL39                                                          
         LA    R5,INTVAR                                                        
         LA    R2,7                7 DAYS                                       
         LA    R3,X'40'            START WITH MONDAY                            
BLDEL35  CLI   0(R5),0                                                          
         BE    BLDEL37                                                          
         XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ2                                                   
         MVC   NTSQH,0(R5)         START QH                                     
         MVC   NTEQH,1(R5)         END QH                                       
         MVC   NTDUR,2(R5)         DURATION IN MINUTES                          
         MVC   NTSTIM,3(R5)        START TIME                                   
         MVC   NTETIM,5(R5)        END TIME                                     
         MVC   NTDUR2,7(R5)        LONG DURATION (2 BYTES)                      
         MVC   NTWKS2,INTWEEKS                                                  
*?       STC   R3,NTELEM           INDIVIDUAL DAY CODE                          
         STC   R3,NTDAY            INDIVIDUAL DAY CODE                          
         GOTO1 APUTEL,NTELEM                                                    
*                                                                               
BLDEL37  LA    R5,L'INTVAR(R5)            NEXT DAY IN INTVAR                    
         SRL   R3,1                NEXT INDIVIDUAL DAY                          
         BCT   R2,BLDEL35                                                       
*&&                                                                             
*                                  BUILD DAY/TIME BITS FOR '04' ELEM            
BLDEL39  CLI   TYPE,0              PROGRAM AVERAGE?                             
         BNE   BLDEL40                                                          
         LA    R6,=C'SET'          SET PROGRAM AVE CONTROL BITS                 
         ICM   R6,8,INTDYBIT                                                    
         GOTO1 =V(DETABQH),DMCB,(R6),PCNTBITS,INTSTIM,INTETIM                   
         B     BLDEL50                                                          
*                                                                               
BLDEL40  DS    0H                                                               
         LA    R6,=C'CLR'        CLEAR BITS SET BY PRG AVG FOR THIS DAY         
         ICM   R6,8,INTDYBIT     REASON: IF THERE IS A TRACK FOR DAY,           
*                                  OVERWRITE PRG AVE FOR DAY(ALL TIMES)         
         GOTO1 =V(DETABQH),DMCB,(R6),PCNTBITS,=H'0000',=H'2359'                 
*                                                                               
         LA    R6,=C'SET'          SET TRACKAGE CONTROL BITS                    
         ICM   R6,8,INTDYBIT                                                    
         GOTO1 =V(DETABQH),DMCB,(R6),TCNTBITS,INTSTIM,INTETIM                   
*        MVI   DMCB,0              DON'T GENERATE N-PTRS FOR MOVIE GOER         
*        BAS   RE,GENN             GENERATE PASSIVE PTRS (N-RECS)               
*                                                                               
BLDEL50  OC    INTTRK,INTTRK       ANY TRACKAGE?                                
         BZ    *+8                                                              
         MVI   CNTUTRK,1           TRKD EPISODES EXIST                          
*                                                                               
BLDELQX  B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*'P' RECD ELEMENTS ONLY                                                         
*--------------------------------------------------------------------           
BLDEL60  DS    0H                  BUILD 'P' RECORD ELEMENTS                    
         CLI   THISKEY,PRCODEQU                                                 
         BNE   BLDELX                                                           
         LA    R6,TEMP                                                          
         USING PHTELEM,R6          BUILD ELEMENT TO SAVE INTCOV                 
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHTCODE,PHTCODEQ    X'10' ELEMENT                                
         MVI   PHTLEN,PHTLNEQ                                                   
         MVC   PHTDTYP,INTDTYP                                                  
****     MVC   PHTDPT,INTDPT                                                    
         MVC   PHTPREM,INTPREM                                                  
         MVC   PHTCOVR,INTCOV      DURATION IN MINUTES                          
         GOTO1 APUTEL,PHTELEM                                                   
         DROP  R6                                                               
*                                                                               
         USING PHELEM,R6           BUILD DAY/QTR HOUR ELEMENT                   
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHCODE,PHCODEQ      X'20' ELEMENT                                
         MVI   PHELN,PHELNEQ                                                    
         MVC   PHPNUM,INTPNUM                                                   
         MVC   PHDUR,INTDUR        QUARTER HOUR DURATION                        
         MVC   PHDTYPE,INTDTYP                                                  
         MVC   PHDWKS,INTDAYWK                                                  
         NI    PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,INTBOOK                                                  
         MVC   PHDURTOT,INTDUR     TOTAL QHRS ON THIS RECD                      
         GOTO1 APUTEL,PHELEM                                                    
         DROP  R6                                                               
*                                                                               
         USING PPNELEM,R6          BUILD PROGRAM NAME ELEMENT                   
         MVI   PPNCODE,PPNCODEQ    X'21' ELEMENT                                
         MVC   PPNNME(L'INTPNAME),INTPNAME                                      
         OC    PPNNME(L'INTPNAME),BLANKS             BLNK PAD PRG NAME          
         CLC   PPNNME(L'INTPNAME),BLANKS             BLNK PAD PRG NAME          
         BE    BLDELX                                                           
         LA    R5,PPNNME+L'INTPNAME-1                                           
         LA    R1,L'INTPNAME-1                                                  
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,3(R1)                                                         
         STC   R1,PPNELN                                                        
         GOTO1 APUTEL,PPNELEM                                                   
         DROP  R6                                                               
*                                                                               
BLDELX   B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*BLDEMS  - BUILD SECTION LEADS AND DEMO ELEMENTS                                
**********************************************************************          
BLDEMS   NTR1                                                                   
         XC    TEMP,TEMP           BUILD SECTION LEAD ELEM                      
         LA    R6,TEMP                                                          
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ     NEW TYPE X'23' ELEMENT                        
         MVI   SLLEN,4            EXTENDED TO INLUDE OPTIONAL INDICATOR         
         MVC   SLOPT,INTBTYP+1    OPTIONAL INDICATOR                            
         LA    RE,INTKEY                                                        
         CLI   0(RE),C'R'         CORRECTION RECDS ARE PRGM RECDS               
         BE    BLDEM5Q                                                          
         CLI   0(RE),PMCODEQU     Q RECORD?                                     
         BNE   BLDEM5P                                                          
BLDEM5Q  MVC   SLSECT,QMKTB-QPRGKD(RE)    MKT NUMBER ON -Q- RECD                
*        MVI   SLLEN,4                                                          
*        MVC   SLSECT+1(1),INTBTYP                                              
*        B     *+14                                                             
         B     *+10                                                             
BLDEM5P  MVC   SLSECT,PRBTYP-PRKEY+4(RE)  MKT NUMBER ON -P- RECD                
*        MVI   SLLEN,3                                                          
         GOTO1 APUTEL,SLELEM                                                    
*                                                                               
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVI   DBSELMED,C'N'       --MEDIA--                                    
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTIBOOK                                                 
         L     RF,ACOMFACS                                                      
         ST    RF,DBCOMFCS                                                      
         MVC   WORK(10),OFORMAT    SET UP OUTPUT FORMAT BLOCK                   
*        MVC   WORK+7(2),=X'5D26'                                               
         MVC   WORK+7(2),INTBOOK    TO FORCE BOOK ON X'5E' ELEMENT              
         MOVE  (CONDWORK,1500),INTACCS     TO CURRENT BOOK                      
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDEMEL                                                        
         GOTO1 (RF),DMCB,(C'C',WORK),DBLOCKD,CONDWORK                           
         DROP  RF                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CONDWORK(2),CONDWORK   TEST FOR ANY DEMO ELEMENTS                
         BZ    BLDEMX              NONE-PUT RECORD TO TAPE                      
         LA    R1,CONDWORK+2                                                    
*                                  ADD DEMO ELEMENTS TO RECORD                  
         SR    R0,R0                                                            
BLDEM30  CLI   0(R1),0                                                          
         BE    BLDEMX                                                           
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLDEM30                                                          
*                                                                               
BLDEMX   MVC   PREVKEY,THISKEY      PUT RECORD TO TAPE & SAVE VALUES            
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*GENN -  GENEREREATE THE -N- RECS                                               
*        DMCB = 1  CALLED  FROM COPYEL RTN IN CRCTNS PROCESSING                 
**********************************************************************          
GENN     NTR1                                                                   
         MVC   SVADDR,AOREC                                                     
         MVC   AOREC,AMYREC        BUILD PTR RECD IN DIFFERENT BUFFER           
         XC    PTRKEY,PTRKEY                                                    
         LA    R2,1                                                             
         MVI   HALF,C'Y'           SET SWITCH FOR SINGLE DAY                    
*                                                                               
GENN2    LA    R6,PTRKEY           BUILD 'N' RECORD KEY                         
         USING PNKEY,R6                                                         
         MVI   PNCODE,PNCODEQU                                                  
         MVI   PNMEDIA,C'C'        MEDIA FROM INTKEY                            
         MVC   PNSRC,OUTSRC                                                     
         CLC   INTBOOK,=AL2(JAN_07)  HAVE ALL STREAMS AFTER JAN/07              
         BL    *+10                                                             
         MVC   PNSRC,INTKSRC                                                    
         CLI   DMCB,1              CALLED FROM CRCT RECD COPY                   
         BNE   GENN10                                                           
         LA    RE,CNTKEY                                                        
         USING PMKEY,RE                                                         
         MVC   PNBOOK,PMBOOK                                                    
         MVC   PNSTAT,PMSTAT                                                    
         MVC   PNDW,LDAYWK                                                      
         MVC   PNSTIM,LSQH                                                      
         MVC   PNPNUM,PMPNUM       ---ORIG FILE NUMBER FOR PROGRAM---           
         MVC   PNACTDAY,LDAYWK     ACTUAL DAY                                   
         MVC   PNACTDUR,LDURM      DURATION IN MINUTES                          
         B     GENN20                                                           
         DROP  RE                                                               
*                                                                               
GENN10   MVC   PNBOOK,INTBOOK                                                   
         MVC   PNSTAT,INTSTA                                                    
         MVC   PNDW,INTDAYWK                                                    
         MVC   PNSTIM,INTSQH                                                    
         MVC   PNPNUM,STFILE       ---ORIG FILE NUMBER FOR PROGRAM---           
         MVC   PNACTDAY,INTDAYWK   ACTUAL DAY                                   
         MVC   PNACTDUR,INTDURM    DURATION IN MINUTES                          
*                                                                               
GENN20   GOTO1 ABLDREC,DMCB,(C'P',PNKEY)                                        
         BAS   RE,PUTTAPE                                                       
*                                                                               
GENNX    MVC   AOREC,SVADDR        RESTORE ADDR OF OREC                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*RDRECS  - SEARCH FILE FOR LATEST FILE NUMBER FOR CORR RECDS                    
*          FILE READ INTO AIREC                                                 
**********************************************************************          
RDRECS   NTR1                      READ PAVDIR FOR 'FFFF'  J-RECD               
         XC    FILENUM,FILENUM                                                  
         XC    IKEY,IKEY                                                        
         LA    R6,IKEY                                                          
         USING PMKEY,R6            BUILD -Q- RECD KEY FOR STAT/BOOK             
         MVI   PMCODE,PMCODEQU                                                  
         MVI   PMMEDIA,C'C'                                                     
         MVC   PMSRC,OUTSRC                                                     
         CLC   INTBOOK,=AL2(JAN_07)  HAVE ALL STREAMS AFTER JAN/07              
         BL    *+10                                                             
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMSTYP,INTSTYP                                                   
         MVC   SVIKEY,IKEY                                                      
         BAS   RE,DHIGH            READ PAVDIR FOR BOOK/STATION                 
         BE    *+6                 ERROR ON READ                                
         DC    H'0'                                                             
*        CLC   SVIKEY(PMSTYP-PMKEY),IKEY                                        
*        BNE   RDRECX              NO RECDS FOR THIS STATION/BOOK               
         CLC   SVIKEY(PMOPIID-PMKEY),IKEY                                       
         BNE   RDRECX              CK STYP/BTYP AS WELL                         
         LA    R6,IKEY                                                          
         MVC   SVDA,PMNDXDA-PMKEY(R6)                                           
         L     RE,AIREC                                                         
         MVC   4(L'SVIKEY,RE),SVIKEY                                            
         BAS   RE,FHIGH            READ PAVFIL UNTIL EOF (SAVING FILE#)         
         BNE   RDRECX              EOF?                                         
*                                                                               
RDREC20  CLI   COPY,C'Y'           COPY ENTIRE BOOK?                            
         BNE   RDREC25             NO, JUST LOOK UP FILE NUMBER                 
         MVC   AOREC,AIREC                                                      
         MVC   P(L'IKEY),IKEY                                                   
*        GOTO1 VPRINTER                                                         
         L     RE,AIREC                                                         
         ICM   RF,3,PMRLEN-PMKEY+4(RE)  LENGTH OF RECD READ IN IREC             
         LA    RF,4(RF)                                                         
         STCM  RF,3,0(RE)                                                       
         BAS   RE,PUTTAPE          RELEASE RECD--FROM AIREC                     
         MVC   AOREC,SVAOREC                                                    
RDREC25  L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         MVC   FILENUM,PMPNUM      SAVE FILE #                                  
         BAS   RE,FSEQ             READ NEXT RECD TIL EOF                       
         BE    RDREC20                                                          
         DROP  R6                                                               
*                                                                               
RDRECX   MVC   LSTBK,INTBOOK       SAVE FOR DDS TABLE                           
         MVC   LSTSTA,INTSTA                                                    
         MVC   LSTFNUM,FILENUM     FILENUM                                      
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*QMERGE -MERGE PROGRAM RECORDS WITH SAME KEYS                                   
**********************************************************************          
QMERGE   DS    0H                                                               
         MVC   AOREC,SVATREC       SAVED ADDRESS OF TEMP RECD                   
         GOTO1 ABLDREC,DMCB,THISKEY  COPY KEY INTO ATREC                        
         CLC   PRGKEY(QMKTB-QPRGKD),INTKEY   SAME RECD,DIFF MKT BRK?            
         BE    QMERG5                                                           
         LA    RE,INTKEY                                                        
         USING QPRGKD,RE                                                        
         CLC   QTRK,PRGKEY+(QTRK-QPRGKD)                                        
         BE    QMERG1              SAME TRKG                                    
         CLI   QAVG,0              TRACKAGE SUMMARY RECD?                       
         BE    QMERG1              YES, DON'T HAVE TO CREATE ONE                
         MVI   NOSUMY,2            CREATE TRKG SUMMARY ELEMENTS                 
         BAS   RE,BLDELEM                                                       
         DROP  RE                                                               
*                                                                               
QMERG1   BAS   RE,BLDELEM          BUILD INITIAL ELEMENTS  <IN ATREC>           
QMERG5   BAS   RE,BLDEMS                                                        
         L     RE,SVATREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,PMRLEN-PMKEY+4(RE)   RF=LENGTH OF ATREC                     
         LA    RE,PMDATA-PMKEY     LENGTH OF KEY                                
         SR    RF,RE               RF=ATREC-L'KEY                               
         L     RE,SVAOREC          GET LENGTH OF RECD IN AOREC                  
         SR    R1,R1                                                            
         ICM   R1,3,PMRLEN-PMKEY+4(RE)  R1=LENGTH OF AOREC                      
         AR    R1,RF               L'AREC + L'AOREC                             
         MVC   AOREC,SVAOREC       PT TO AOREC AGAIN                            
         CH    R1,=H'1980'         MAXREC=2000: 3BYTES FOR '0F-FF' ELMT         
         BH    QMERG20                                                          
         L     R1,SVATREC                                                       
         LA    R1,PMDATA-PMKEY+4(R1)                                            
*                                                                               
QMERG10  CLI   0(R1),0             COPY ELMNTS FROM ATREC INTO AOREC            
         BE    QMERG30                                                          
         GOTO1 APUTEL,(R1)                                                      
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     QMERG10                                                          
*                                                                               
QMERG20  BAS   RE,PUTTAPE          RELEASE AOREC                                
         L     R6,SVATREC          PT TO ATREC                                  
         LA    R6,4(R6)                                                         
         USING PMKEY,R6                                                         
         BAS   RE,DDSFIL           BUMP FILNUM COUNTER                          
         MVC   PMPNUM,FILENUM                                                   
         L     RE,SVATREC          MOVE RECD FROM ATREC TO AOREC                
         L     RF,SVAOREC                                                       
         ICM   R1,3,PMRLEN         LENGTH OF ATREC                              
         LA    R1,4(R1)            +4 BYTES IN THE FRONT OF RECD                
         MOVE  ((RF),(R1)),(RE)    AOREC <-- ATREC                              
         LA    R6,THISKEY                                                       
         MVC   PMPNUM,FILENUM                                                   
*                                                                               
QMERG30  MVC   PRGKEY,INTKEY       SAVE CURRENT RECD'S INTKEY                   
         MVC   PRGORIG,INTORIG                                                  
         MVC   PREVKEY,THISKEY                                                  
         MVC   AOREC,SVAOREC                                                    
         B     CNVX                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
*PMERGE -MERGE USAGE RECORDS WITH SAME KEYS                                     
**********************************************************************          
PMERGE   DS    0H                                                               
         L     RE,AOREC                                                         
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)         RF=LENGTH OF EXISTING RECORD                  
         BNZ   *+6                                                              
         DC    H'0'               LENGTH BETTER BE NON-ZERO                     
         LA    R0,1700            ENOUGH ROOM FOR MKT BRK?(MAXREC=2000)         
         CR    RF,R0                                                            
         BL    PMERG10                                                          
         CLI   INTSTA+4,C'U'      TREAT UNIVERSE RECORDS DIFFERENT              
         BE    PMERG5U                                                          
*TIME PERIOD RECORDS-INCREASE BOOK TYPE FOR MULTIPLE RECORDS                    
*(DATA THAT DOESN'T FIT ON ONE RECORD)                                          
PMERG5T  ZIC   R1,THISSTYP                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISSTYP                                                      
         LA    RF,THISKEY                                                       
         USING PRKEY,RF                                                         
         MVC   PRSTYP,THISSTYP     UPDATE BOOK TYPE                             
         B     PCNV6                                                            
         DROP  RF                                                               
*UNIVERSE RECORDS-INCREASE MINOR KEY FOR MULTIPLE RECORDS                       
PMERG5U  SR    R1,R1                                                            
         ICM   R1,3,THISMINR                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,THISMINR                                                    
         LA    RF,THISKEY                                                       
         USING PRKEY,RF                                                         
         MVC   PRKMINOR,THISMINR   UPDATE MINOR KEY                             
         MVI   UNVOFLG,1           EXPANDED UNIVERSE RECORD FLAG                
         B     PCNV6                 (OVER MULTIPLE RECORDS)                    
         DROP  RF                                                               
*                                                                               
PMERG10  L     RE,AOREC                                                         
         LA    R5,DBLOCKA                                                       
         AR    RE,RF                                                            
         ST    RE,DBAQUART         AND SAVE AS A(QTR HR)                        
         XC    0(60,RE),0(RE)                                                   
         OI    PRINTSW,X'80'       SUPPRESS PRINTING                            
*                                                                               
*        LA    RF,THISKEY                                                       
*        USING PRKEY,RF            BUILD USAGE RECD KEY -P-                     
*                                                                               
         BAS   RE,BLDEMS           BLD SECTION LEADS AND DEMO ELEMNTS           
         MVC   PRGKEY,INTKEY       SAVE CURRENT RECD'S INTKEY                   
         MVC   PREVKEY,THISKEY                                                  
         B     CNVX                                                             
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* LASTHK-LAST TIME HOOK TO RELEASE FINAL RECORDS                                
* ********************************************************************          
LASTHK   DS    0H                                                               
*        L     R1,ACOMWRK          ADDRESS OF COMMON WORK AREA                  
*        MVC   AVGWKS,0(R1)        NMBER OF WKS IN AVG                          
*        MVC   STDATE,1(R1)        START DATE OF AVG RECD                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    LST15                                                            
         CLI   PRGKEY,C'P'         TIME PERIOD RECD?                            
         BNE   LST10               JUST RELEASE RECD FROM BUFFER                
         BAS   RE,PUTTAPE                                                       
         B     LST15                                                            
*                                                                               
LST10    CLI   PRGKEY,C'R'         CORRECTION RECD?                             
         BNE   *+8                 NO, THEN IT'S AN ORIG 'Q'-PRG RECD           
         BAS   RE,RELS             RELEASE REST OF CORRECTION RECD              
         BAS   RE,FINFF            OUTPUT '0F-FF' ON PRG RECDS                  
*                                                                               
LST15    OC    CNTKEY,CNTKEY       RELEASE LAST CNTL RECD                       
         BZ    *+18                                                             
         MVC   INTKSRC,CNTKEY+2                                                 
         MVI   HALF,X'0'           NEW CONTROL RECORD                           
         BAS   RE,CNTREC                                                        
         BAS   RE,SVTBLS           RELEASE J-RECDS (NTI-DDS-FILE#'S)            
         MVI   SORTSW,0                                                         
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   LST30                                                            
         XC    DBUPARMS,DBUPARMS        RELEASE CONTROL RECORDS                 
         LA    RE,DBUPARMS              TO CTAPE                                
         USING DBUPARMD,RE                                                      
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         DROP  RE                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,RELQ  RELEASE                         
*                                                                               
LST30    BAS   RE,EMAILUNK         SEND EMAIL WITH UNKNOWN STATIONS,            
*                                  IF ANY                                       
         B     CNVX                                                             
         EJECT                                                                  
***********************************************************************         
* SVTBLS -     SAVE NTI-DDS AND DDS-FILE PROGRAM NUMBER TABLES                  
*              AS PASSIVE KEYS.  INTERNAL-EXTERNAL TRANSLATION.                 
***********************************************************************         
SVTBLS   NTR1                                                                   
         L     R5,=A(NTITBL)                                                    
SVTB10   OC    0(5,R5),0(R5)       IS TABLE EMPTY?                              
         BZ    SVTB20                                                           
         LA    R6,KEY              LOOK UP NTI PGR CODE PASSIVE RECD            
         XC    KEY,KEY                                                          
         USING PJKEY,R6                                                         
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   PJSTAT(4),0(R5)     NETWORK                                      
         MVI   PJSTAT+4,C'C'       CABLE                                        
         MVC   PJINTNUM(2),10(R5)  DDS NUMBER                                   
         CLC   5(5,R5),=5X'FF'     MAX DDS NUMBER RECORD?                       
         BNE   *+14                YES, SEND OUT AS NTI=0                       
         XC    PJINTNUM(2),=X'FFFF'     FORCE LATEST DDS NUMBER FIRST           
         B     *+10                SEND OUT AS NTI=0                            
         MVC   PJEXTNUM,5(R5)      NTI NUMBER                                   
         GOTO1 ABLDREC,DMCB,(C'P',PJKEY)                                        
         BAS   RE,PUTTAPE                                                       
SVTB15   LA    R5,L'NTITBL(R5)                                                  
         B     SVTB10                                                           
*                                                                               
SVTB20   DS    0H                  GO THRU DDSTBL                               
         L     R5,=A(DDSTBL)                                                    
         USING DDSTBLD,R5                                                       
SVTB25   OC    0(5,R5),0(R5)       IS TABLE EMPTY?                              
         BZ    SVTBLX                                                           
         LA    R6,KEY              LOOK UP NTI PGR CODE PASSIVE RECD            
         XC    KEY,KEY                                                          
         USING PJKEY,R6                                                         
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVI   PJSRC,C'N'                                                       
         CLC   DDSBOOK,=AL2(JAN_07)  HAVE ALL STREAMS AFTER JAN/07              
         BL    *+10                                                             
         MVC   PJSRC,INTKSRC                                                    
         MVC   PJBOOK,DDSBOOK      SET BOOK                                     
         MVC   PJBTYPE,DDSBKTY     BOOK TYPE                                    
         MVC   PJEXTNUM(1),DDSSTYP STATION TYPE                                 
         MVC   PJSTAT(4),DDSNET    NETWORK                                      
         MVI   PJSTAT+4,C'C'       CABLE                                        
         CLC   DDSPRG,=X'FFFF'     OUTPUT MAX FILE# AS DDSPRG#=0                
         BE    *+10                                                             
         MVC   PJEXTNUM+3(2),DDSPRG    DDS NUMBER                               
         MVC   PJINTNUM(2),DDSFILE     FILE NUMBER                              
         XC    PJINTNUM(2),=X'FFFF'    LATEST FILE NUMB FIRST FOR DDSN          
         GOTO1 ABLDREC,DMCB,(C'P',PJKEY)                                        
         BAS   RE,PUTTAPE                                                       
*                                                                               
         CLC   DDSBOOK,PRVBOOK                                                  
         BNE   SVTB28                                                           
         CLC   DDSNET,PRVNET                                                    
         BNE   SVTB28                                                           
         CLC   DDSSTYP,PRVSTYP                                                  
         BE    SVTB30                                                           
SVTB28   BRAS  RE,DELOLDJS          DELETE OLD J-PNTRS FOR THIS BK/STA          
*                                                                               
SVTB30   MVC   PRVBOOK,DDSBOOK                                                  
         MVC   PRVNET,DDSNET                                                    
         MVC   PRVSTYP,DDSSTYP                                                  
         LA    R5,L'DDSTBL(R5)                                                  
         B     SVTB25                                                           
         DROP  R5,R6                                                            
*                                                                               
SVTBLX   B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*PUTTAPE: ROUTINE TO PUT RECORD TO OUTPUT TAPE                                  
*         THIS ROUTINE ALSO CALLS DEBKUPDT TO ADD A BOOK FOR EVERY              
*         ACTIVE RECORD RELEASED TO TAPE.                                       
**********************************************************************          
*                                                                               
PUTTAPE  NTR1                                                                   
*                                                                               
         GOTO1 APUTTAPE                                                         
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   PUTTAPEX                                                         
         TM    FLAGS1,EXCLUDE_CORRECTIONS   EXCLUDE CORRECTIONS?                
         BNO   PUTTP05                     (VIA CARD NETCTCOR=N)                
         CLI   CORRFLAG,YES                 YES                                 
         BE    PUTTAPEX                     EXIT FOR CORRECTIONS                
*                                                                               
* CALL DEBKUPDT FOR EACH P AND Q REC CREATED. DEBKUPDT KEEPS TRACK OF           
* THE BOOKS BEING LOADED AND WILL END UP CREATING CONTROL RECORDS WHEN          
* CALLED WITH RELEASE MODE.                                                     
*                                                                               
PUTTP05  XC    DBUPARMS,DBUPARMS                                                
         LA    R3,DBUPARMS                                                      
         USING DBUPARMD,R3                                                      
*                                                                               
         MVI   DBUPSTYP,DBUPCBLQ   POSTING TYPE FOR CABLE                       
*                                                                               
         L     RE,AOREC                                                         
         LA    RE,4(RE)            POINT TO DEMO RECORD TO BE RELEASED          
         USING PRKEY,RE                                                         
         CLI   PRCODE,PRCODEQU     P RECORDS                                    
         BNE   PUTTP10                                                          
         MVC   DBUBOOK,PRBOOK      BOOK FOR P RECORDS                           
         B     PUTTP20                                                          
         DROP  RE                                                               
*                                                                               
         USING PMKEY,RE                                                         
PUTTP10  CLI   PMCODE,PMCODEQU     Q RECORDS                                    
         BNE   PUTTAPEX            EXIT FOR ALL OTHER RECORD TYPES              
         MVC   DBUBOOK,PMBOOK      BOOK FOR Q RECORDS                           
         DROP  RE                                                               
*                                                                               
PUTTP20  MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         BAS   RE,WKTOMON          CONVERT WEEKLY TO MONTHLY BOOK               
         MVI   DBUBKFLG,DBUBKMQ    MONTHLY BOOK                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,ADDQ    ADD DEMO UPDATE BOOK          
*                                                                               
PUTTAPEX XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CONVERT WEEKLY TO MONTHLY BOOK                                                
* THE CNAD FILE IS A MONTHLY FILE THAT IS STORE AS A WEEKLY BOOK (THE           
* (OUTPUT RECORDS HAVE AS BOOK THE FIRST WEEK OF THE MONTH)                     
* THE CONTROL RECORDS NEED TO COVER THE ENTIRE MONTH RANGE.                     
* AT ENTRY, R3 POINTS TO DBUPARMD                                               
**********************************************************************          
WKTOMON  NTR1                                                                   
*                                                                               
         USING DBUPARMD,R3                                                      
         L     RF,ACOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,NENACWKS                                               
         ICM   R5,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R0,15,DMCB+4        L'TABLE ENTRY                                
*                                                                               
         USING NEWKSD,R5                                                        
WKMO10   CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BOOK NOT FOUND                               
         CLC   DBUBOOK(1),NEWKSYR  COMPARE ON YEAR                              
         BNE   WKMO20                                                           
         CLC   DBUBOOK+1(1),NEWKSLST COMPARE THE WEEK#                          
         BH    WKMO20                                                           
         MVC   DBUBOOK,NEWKSYR                                                  
         B     WKTOMONX                                                         
*                                                                               
WKMO20   AR    R5,R0               TRY NEXT ENTRY IN THE TABLE                  
         B     WKMO10                                                           
*                                                                               
         DROP  R3                                                               
         DROP  R5                                                               
*                                                                               
WKTOMONX B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*ROUTINES USED GLOBALLY                                                         
*        DATAMAGR CALLS FOR READING DIR,FILE- HIGH,SEQ ETC INTO AIREC           
**********************************************************************          
*                                                                               
HIGH     NTR1                                                                   
         L     R6,AWREC            READ WITH 'KEY' INTO WREC                    
         LA    R6,4(R6)                                                         
         XC    0(L'KEY,R6),0(R6)                                                
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIDIR',KEY,(R6)                    
         L     RE,AWREC                                                         
         LA    RE,4(RE)                                                         
         MVC   KEY,0(RE)                                                        
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
DHIGH    NTR1                      READ HIGH ON DIRECTORY                       
         MVC   COMMAND,=C'DMRDHI '                                              
         B     DMGRD                                                            
*                                                                               
DSEQ     NTR1                      READ SEQ ON DIRECTORY                        
         MVC   COMMAND,=C'DMRSEQ '                                              
         B     DMGRD                                                            
*                                                                               
DMGRD    L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         GOTO1 VDATAMGR,DMCB,COMMAND,=C'NTIDIR',IKEY,(R6)                       
         CLI   0(R1),0                                                          
         BNE   DMGRX               ERROR OR EOF                                 
         L     RE,AIREC                                                         
         LA    RE,4(RE)                                                         
         MVC   SVDA,19(RE)                                                      
         MVC   SVSTATUS,PRKSTAT-PRKEY(RE)                                       
         MVC   IKEY,0(RE)          COPY NEW KEY BACK                            
         CR    R1,R1               SET CONDITION CODE OK                        
         B     DMGRX                                                            
         SPACE 2                                                                
*                                                                               
FHIGH    NTR1                      READ HIGH ON PAVFIL                          
         MVC   COMMAND,=C'DMRDHI '                                              
         L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         MVC   PRRSTAT-PRKEY(1,R6),SVSTATUS                                     
         GOTO1 VDATAMGR,DMCB,COMMAND,=C'NTIFIL',SVDA,(R6)                       
         B     DMGRF                                                            
*                                                                               
FSEQ     NTR1                      READ SEQ  ON PAVFIL                          
         MVC   COMMAND,=C'DMRSEQ '                                              
         L     R6,AIREC                                                         
         LA    R6,4(R6)                                                         
         MVC   PRRSTAT-PRKEY(1,R6),SVSTATUS                                     
         GOTO1 VDATAMGR,DMCB,COMMAND,=C'NTIFIL',SVDA,(R6)                       
         B     DMGRF                                                            
*                                                                               
DMGRF    CLI   8(R1),0                                                          
         BNE   DMGRX                                                            
         L     RE,AIREC                                                         
         LA    RE,4(RE)                                                         
*        MVC   SVDA,19(RE)         DISK ADDRESS                                 
         MVC   IKEY,0(RE)          COPY NEW KEY BACK                            
         CLI   8(R1),0             SET CONDITION CODE                           
         B     DMGRX                                                            
         SPACE 2                                                                
*                                                                               
DMGRX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CHECK INTSTA AGAINST LIST OF KNOWN STATION CODES.                             
* IF STATION CODE UNKNOWN, ADD BINARY STATION CODE TO TABLE UNKNTAB.            
* UNKNTAB WILL END UP CONTAINING 4-BYTE BINARY STATION CODES FOR THE            
* UKNOWN STATIONS.                                                              
* AT THE END OF THE CONVERSION, AN EMAIL WILL BE SENT WITH THE UNKNOWN          
* STATION CODES.                                                                
**********************************************************************          
*                                                                               
KNOWNSTA NTR1                                                                   
*                                                                               
         OC    INTSTA,INTSTA                                                    
         BZ    KNSTAX                                                           
         TM    INTSTA,X'F0'        APPLIES ONLY TO NUMERIC                      
         BO    KNSTA05                                                          
         CLI   INTSTA,0            OR 3-BYTE BINARY STATION CODES               
         BE    KNSTA05                                                          
         B     KNSTAX                                                           
*                                                                               
KNSTA05  ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABNAM  TABLE OF CABLE STATIONS                      
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
                                                                                
         USING NECBNAMD,RE                                                      
KNSTA10  CLI   0(RE),X'FF'                                                      
         BE    KNSTA50             UNKNOWN STATION CODE                         
*                                                                               
         TM    INTSTA,X'F0'        STATION IS NUMERIC                           
         BNO   KNSTA20                                                          
         PACK  DUB,INTSTA(4)                                                    
         CVB   R5,DUB                                                           
         B     KNSTA40                                                          
*                                                                               
KNSTA20  ICM   R5,15,INTSTA        STATION IS BINARY                            
*                                                                               
KNSTA40  CLM   R5,7,NECBNNML                                                    
         BE    KNSTAX              THIS IS A KNOWN STATION                      
*                                                                               
         AR    RE,RF                                                            
         B     KNSTA10                                                          
         DROP  RE                                                               
*                                                                               
KNSTA50  LA    RE,UNKNTAB          ADD UNKNOWN STATION CODE TO TABLE            
KNSTA55  LA    RF,UNKNTAB                                                       
         LA    RF,UNKNTABL(RF)                                                  
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'0'                UNKNTAB TABLE FULL                           
         OC    0(4,RE),0(RE)                                                    
         BZ    KNSTA60                                                          
         CLM   R5,15,0(RE)                                                      
         BE    KNSTAX              STATION CODE ALREADY IN TABLE                
         LA    RE,4(RE)                                                         
         B     KNSTA55                                                          
*                                                                               
KNSTA60  STCM  R5,15,0(RE)         ADD STATION CODE TO TABLE                    
*                                                                               
KNSTAX   B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SEND AN EMAIL CONTAINING THE UNKNOWN STATION CODES.                           
* NOTE: THESE STATIONS ARE NOT LEFT OUT. THE DATA FOR THESE STATIONS            
*       WILL BE PART OF THE OUTPUT FILE.                                        
**********************************************************************          
*                                                                               
EMAILUNK NTR1                                                                   
*                                                                               
         OC    UNKNTAB(4),UNKNTAB                                               
         BZ    EMAILUNX                                                         
*                                                                               
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
         LA    R1,ASIDFLD                                                       
         L     R5,0(R1)                                                         
         LOCASCB ASID=(R5)                                                      
         L     R5,ASCBASSB-ASCB(R1)                                             
*                                                                               
         SAM31                     SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R5,ASSBJSAB-ASSB(R5) R5 = A(JSAB)                                
         USING JSAB,R5                                                          
         L     RE,=A(SUBJJBID)                                                  
         MVC   0(8,RE),JSABJBID    SAVE JOBID/JOBNAME IN E-MAIL SUBJECT         
         L     RE,=A(SUBJJBNM)                                                  
         MVC   0(8,RE),JSABJBNM                                                 
         DROP  R5                                                               
*                                                                               
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',SENDTO),('SUBJCTLQ',SUBJECT)           
*                                                                               
         LA    R2,EMALIN1          ADD FIRST 3 LINES TO EMAIL BODY              
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         LA    R2,EMALIN2                                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         LA    R2,EMALIN3                                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
*                                                                               
         LA    R3,UNKNTAB                                                       
EMUNK10  LA    RF,UNKNTAB          ADD STATION CODES TO EMAIL BODY              
         LA    RF,UNKNTABL(RF)                                                  
         CR    R3,RF                                                            
         BNL   EMUNK50                                                          
         OC    0(4,R3),0(R3)                                                    
         BZ    EMUNK50                                                          
         MVC   EMALIN1,BLANKS                                                   
         EDIT  (4,(R3)),(5,EMALIN1)                                             
         LA    R2,EMALIN1                                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         LA    R3,4(R3)                                                         
         B     EMUNK10                                                          
*                                                                               
EMUNK50  GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)   DETACH JESMAIL                    
*                                                                               
EMAILUNX B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------            
*               GETEL                                                           
*-------------------------------------------------------------------            
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*               LITERALS                                                        
*-------------------------------------------------------------------            
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------            
* VARIABLES, TABLES, ETC.                                                       
*-------------------------------------------------------------------            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
SVADDR   DS    A                                                                
AMYREC   DS    A                                                                
SVATREC  DS    A                                                                
SVAOREC  DS    A                                                                
*                                                                               
BOOK     DS    XL2                                                              
MKTBRK   DS    X                   MARKET BREAK NUMBER                          
AVGWKS   DS    X                                                                
STDATE   DS    CL6                                                              
THISKEY  DC    XL20'00'                                                         
THISSTYP DC    X'00'                                                            
THISMINR DC    XL(L'PRKMINOR)'00'                                               
OLDFIL   DC    H'00'               FILE NUMBER OF ORIG RECD ON FILE             
DDSNUM   DC    H'00'               DDS NUMBER FOR NTI PROGRAM                   
DDSBK    DC    H'00'               BOOK FOR DDS NUMBERS                         
DDSBKTY  DC    X'00'               BOOK TYPE FOR DDS NUMBERS                    
FILENUM  DC    H'00'               FILE NUMBER FOR DDS PROGRAM                  
STFILE   DS    H'00'               START FILE# FOR ENTIRE PRG                   
TYPE     DC    X'00'               LEVEL:0=PRG AVG,1=TRACKG,2=TELECAST          
NOSUMY   DC    X'00'                                                            
TEST     DC    X'00'                                                            
USETHBK  DC    C'N'                                                             
UNVOFLG  DC    X'00'                                                            
BLANKS   DC    CL80' '                                                          
PAVGDEL  DC    C'N'                                                             
FRSTREC  DC    AL1(YES)            FIRST RECORD TO OPHASE                       
CORRFLAG DC    AL1(NO)             CORRECTION FLAG                              
*                                                                               
ASIDFLD  DC    F'0'                                                             
*                                  LAST TIME VALUES                             
PRGORIG  DC    X'00'               INTORIG FIELD OF PREV RECD                   
PREVKEY  DC    XL20'00'                                                         
PRGKEY   DC    XL30'00'            PROGRAM KEY (FROM INTKEY)                    
AORKEY   DC    XL(QLENQ)'00'       QPRGKD KEY FROM INTKEY                       
* -----CNTRL RECD FIELDS -------------                                          
CNTKEY   DC    XL20'00'            CNTL RECD KEY FOR PREV PRG                   
CNTMTYP  DC    X'00'               MARKET TYPE-- FROM INTMTYP                   
CNTSTYP  DC    X'00'               STATION TYPE-- FROM INTSTYP                  
PCNTBITS DC    XL96'00'            DAY & TIME PROGRAM AVG BITS                  
TCNTBITS DC    XL96'00'            DAY & TIME TRACKAGE BITS                     
CNTUTRK  DC    X'00'               TRKD OR UNTRKD PROGRAM INDICATOR             
CNTORIG  DC    X'00'               '1'=MODIFIED, '2'=ENTIRE RECD DELTD          
OFORMAT  DS    0CL10                                                            
         DC    C'NADNNNN',3X'00'                                                
*                                                                               
PAVFIL   DC    C'PAVFIL'           HELLO FILE NAME                              
*                                                                               
PRVBOOK  DC    XL(L'DDSBOOK)'00'                                                
PRVNET   DC    XL(L'DDSNET)'00'                                                 
PRVSTYP  DC    XL(L'DDSSTYP)'00'                                                
         SPACE 2                                                                
*                                                                               
SENDTO   DC    C'US-DEMOSTEAM                  '                                
*                                                                               
SUBJECT  DC    C'UNKNOWN STATIONS IN THE CABLE NAD CONVERSION, '                
SUBJJBID DS    CL8                                                              
         DC    C' '                                                             
SUBJJBNM DS    CL8                                                              
SUBJCTLQ EQU   *-SUBJECT                                                        
*                                                                               
EMALIN1  DC    CL80'YOU MAY NEED TO ADD THESE STATIONS TO DEMTABS.'             
EMALIN2  DC    CL80'DATA FOR STATIONS BELOW WAS LOADED.'                        
EMALIN3  DC    CL80'UNKNOWN CABLE STATION CODES:'                               
*                                                                               
*                                                                               
* TABLE OF DAY CODES AND THEIR BIT REPRESENTATIONS                              
*                                                                               
DAYTAB   DS    0CL2                                                             
         DC    X'10',X'40'         MON                                          
         DC    X'20',X'20'         TUE                                          
         DC    X'30',X'10'         WED                                          
         DC    X'40',X'08'         THU                                          
         DC    X'50',X'04'         FRI                                          
         DC    X'60',X'02'         SAT                                          
         DC    X'70',X'01'         SUN                                          
         DC    X'80',X'7F'         M-SUN                                        
         DC    X'00',X'7C'         M-F                                          
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
*                                                                               
DECNVMTX EQU   *                                                                
         EJECT                                                                  
*-------------------------------------------------------------------            
* BUILD THE DATA TYPE ELEMENT                                                   
* USING INTKSRC, X'08' ELEMENT PASSED BACK IN TEMP                              
*-------------------------------------------------------------------            
BLDDT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING DTELEM,R6           BUILD DEMO DATA TYPE ELEMENT                 
         MVI   DTCODE,DTCODEQ      X'08'                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,VWDESCTB                                               
         ICM   R1,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R0,15,DMCB+4        L'TABLE ENTRY                                
*                                                                               
         USING VWDESCD,R1                                                       
BDT10    CLC   INTKSRC,VWDSRC                                                   
         BE    BDT20                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     BDT10                                                            
*                                                                               
BDT20    ZIC   RE,VWDLEN                                                        
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DTTYPE(0),VWDSCP                                                 
         DROP  R1                                                               
*                                                                               
         AHI   RE,DTLENQ+1         DESC LENGTH + ELEM LEN + 1 FOR EX            
         STC   RE,DTLEN                                                         
         DROP  R6                                                               
*                                                                               
BDTX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------            
* DELETE OLD J-POINTERS FROM FILE FOR THE THIS BOOK/STATION.                    
* R5-> ENTRY IN DDSTBL                                                          
*                                                                               
* THIS IS DONE IN ORDER TO AVOID THE SITUATION WHERE A REISSUE OF THE           
* PROGRAM DATA WILL CAUSE THE OLD J-POINTERS TO BE OUT OF SYNC                  
* WITH THE NEW Q-RECORDS.                                                       
*-------------------------------------------------------------------            
DELOLDJS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DDSTBLD,R5                                                       
*                                                                               
         XC    IKEY,IKEY                                                        
         LA    R6,IKEY                                                          
         USING PJKEY,R6                                                         
*                                                                               
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVC   PJSRC,INTKSRC                                                    
         MVC   PJSTAT(4),DDSNET                                                 
         MVI   PJSTAT+4,C'C'                                                    
         MVC   PJBOOK,DDSBOOK                                                   
         MVC   PJBTYPE,DDSBKTY                                                  
         MVC   PJEXTNUM(1),DDSSTYP                                              
*                                                                               
         MVC   KEYSAVE,IKEY           READ ALL J-POINTERS                       
         BRAS  RE,DHIGH                 AND MARK THEM FOR DELETION              
DELJ20   BNE   DELJX                                                            
         CLC   IKEY(PJEXTNUM+1-PJKEY),KEYSAVE                                   
         BNE   DELJX                                                            
         GOTO1 ABLDREC,DMCB,(C'P',IKEY)                                         
         L     RF,AOREC                                                         
         OI    26(RF),X'80'                                                     
         BRAS  RE,PUTTAPE             PUT DELETED J-POINTER TO FILE             
*                                                                               
         BRAS  RE,DSEQ                READ NEXT J-POINTER                       
         B     DELJ20                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
DELJX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
NTITBL   DC    XL12'00'            NTI-DDS: NET(5) , NTI(5) ,  DDS(2)           
         DC    2500XL12'00'                                                     
         DC    X'FFFFFFFFFF'                                                    
*                                                                               
DDSTBL   DC    XL(DDSQ)'00'        DDS-FILE: NET(5) , DDS(2), FILE(2)           
         DC    7500XL(DDSQ)'00'                                                 
         DC    X'FFFFFFFFFF'                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
* DSECT TO COVER TEMP W/S                                                       
**********************************************************************          
*                                                                               
NADWRKD  DSECT                                                                  
SAVEREG  DS    A                                                                
AHUTEL   DS    A                                                                
SVKEY    DS    XL40                                                             
KEY      DS    XL23                                                             
KEYSAVE  DS    XL23                                                             
PTRKEY   DS    XL23                                                             
CONDWORK DS    1500C                                                            
*                                                                               
SVDA     DS    F                   SAVED DISK ADDRESS                           
SVSTATUS DS    C                   SAVED DIR STATUS FOR SPLIT FILE              
IKEY     DS    CL24                                                             
SVIKEY   DS    CL24                                                             
SVJKEY   DS    CL24                SAVED J-RECD KEY FOR ORIG RECD               
*                                                                               
COMMAND  DS    CL7                                                              
ELCODE   DS    X                                                                
DATADISP DS    H                                                                
BYPASS   DS    C                   COPY ELEMS FROM ORIG RECD FLAG: Y/N          
*                                                                               
COPY     DS    C                   'Y'=COPY OLD BOOKS RECS FOR CORCTNS          
LSTBK    DS    CL2                 BOOK OF RECS LAST READ BY RDRECS             
LSTSTA   DS    CL(L'INTSTA)        STATON   "   "                               
LSTFNUM  DS    CL(L'FILENUM)       LAST FILE NUMBER ON "   "                    
IPTR     DS    F             PTR TO WHERE WE LEFT OFF CPYG IN ORG RECD          
*                                                                               
RVARS    DS    0C                  W/CRCTN RECDS:USED TO PARSE ORG RECD         
RPTR     DS    F                   PTR TO CURRENT LEVEL                         
RTYP     DS    X                   TYPE:0,1,2  (0F-TYPE)                        
RTRK     DS    CL(L'INTTRK)        TRACKAGE OF CURRENT LEVEL                    
REPS     DS    CL(L'INTEPS)        TELECAST OF CURRENT LEVEL                    
RTLC     DS    CL(L'INTTNUM)       TELECAST OF CURRENT LEVEL                    
REND     DS    F                   PTR TO END OF LEVEL                          
RUNV     DS    F                   ADDRESS OF UNIVS IN OLD RECD                 
RSTIM    DS    XL2                 TELC START TIME                              
RETIM    DS    XL2                 TELC END TIME                                
RDAY     DS    X                   TELC DAY BIT SETTING                         
RDAYWK   DS    X                   INTDAYWK EQUIV                               
RDURM    DS    X                   INTDURM EQUIV                                
RSQH     DS    X                   INTSQH EQUIV                                 
RVARSLN  EQU   *-RVARS                                                          
*                                                                               
LVARS    DS    0X                  LAST LEVEL ON ORIG RECD                      
LTYP     DS    X                   X'0F' TYPE                                   
LSTIM    DS    XL2                 PREV TELC START TIME                         
LETIM    DS    XL2                 PREV TELC END TIME                           
LDAY     DS    X                   PREV TELC DAY BIT SETTING                    
LDAYWK   DS    X                   INTDAYWK EQUIV                               
LDURM    DS    X                   INTDURM EQUIV                                
LSQH     DS    X                   INTSQH EQUIV                                 
LTLC     DS    CL(L'INTTNUM)       TELECAST NUMBER                              
LTRK     DS    CL(L'INTTRK)        TRACKAGE OF LAST LEVEL                       
*                                                                               
OVARS    DS    0X                  LAST LEVEL WRITTEN TO NEW RECD               
OTRK     DS    CL(L'INTTRK)        TRK NUMBER                                   
OTLC     DS    CL(L'INTTNUM)       TELECAST NUMBER                              
*                                                                               
UNKNTAB  DS    10AL4               SAVED UNKNOWN CABLE CODES                    
UNKNTABL EQU   *-UNKNTAB                                                        
*                                                                               
DBUPARMS DS    XL(DBUPARML)        PARAMETER AREA FOR DEBKUPDT                  
*                                                                               
ATREC    DS    2000C                                                            
MYREC    DS    2000C                                                            
*                                                                               
NADWRKX  EQU   *                                                                
         EJECT                                                                  
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           
*                     DSECTS                                                    
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           
         SPACE 2                                                                
*                                                                               
DDSTBLD  DSECT                                                                  
DDSBOOK  DS    CL2                 BOOK                                         
DDSNET   DS    CL5                 NETWORK                                      
DDSSTYP  DS    CL1                 STATION TYPE                                 
DDSPRG   DS    CL2                 DDS PROGRAM NUMBER                           
DDSFILE  DS    CL2                 DDS INTERNAL FILE NUMBER                     
DDSSTAT  DS    CL1                 STATUS- 'A'=ADD 'D'=DELETE                   
DDSQ     EQU   *-DDSTBLD                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTMI2D                                                      
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDMONYREQU                                                     
       ++INCLUDE DBUPARMSD                                                      
         PRINT ON                                                               
                                                                                
       ++INCLUDE DDSMTPD                                                        
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061DECNADO   04/22/19'                                      
         END                                                                    
