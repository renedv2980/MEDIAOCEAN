*          DATA SET DERENTPO   AT LEVEL 076 AS OF 09/06/11                      
*PHASE DERNTPOA                                                                 
*INCLUDE DETABQH                                                                
*INCLUDE DEBKUPDT                                                               
         TITLE 'DEMCON - CABLE MIT CONVERSION - OUTPUT PHASE'                   
*                    ALSO FOR WEIGHTED CABLE MIT                                
*                                                                               
*######################################################################         
*---  WARNING  ----  WARNING  ----  WARNING  ----  WARNING    --- !!!!          
*                                                                               
* THESE EQUATES MUST!!! BE IN SYNC WITH DEMDISP AND INPUT PHASE                 
*                                                                               
NDEMS    EQU   41         NUMBER OF OUTPUT DEMOS (UNIVS). SEE INP PHASE         
RIUNVSQ  EQU   NDEMS*4*2  CORRESPONDS TO INPUT PHASE AND DISP TABLE             
UNIVELQ  EQU   X'57'      FIRST UNIVERSE ELEMENT FROM DEMDISP                   
*                                                                               
*                                                                               
*---  WARNING  ----  WARNING  ----  WARNING  ----  WARNING    ---  !!!!         
*######################################################################         
*                                                                               
DERENTPO CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NADWRKX-NADWRKD,**OUTPHS,RA,R4                                   
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
         TM    FLAGS1,CREATE_NETCTREC     NETCTREC=Y                            
         BNO   CNV25                                                            
         GOTO1 =V(DEBKUPDT),DMCB,,INITQ   INITIALIZE DEBKUPDT                   
*                                                                               
CNV25    MVI   FRSTREC,NO                                                       
*                                                                               
         CLI   INTKSRC,0                                                        
         BNE   *+10                                                             
         MVC   INTKSRC,OUTSRC                                                   
*                                                                               
         L     R2,AIREC            OPEN FILE                                    
         CLI   INTRTYP,PRCODEQU    USAGE   RECS -P- ?                           
         BE    PCNV                GO BUILD P RECD KEY                          
*                                                                               
CNVX     XMOD1                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*PCNV -   BUILD 'P' TIME PERIOD RECD KEY                                        
***********************************************************************         
PCNV     DS    0H                                                               
         XC    THISKEY,THISKEY                                                  
         LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU     BUILD -P- RECD KEY                           
         MVI   PRMEDIA,C'R'        MEDIA FROM INTKEY                            
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
                                                                                
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
                                                                                
PCNV6AA  DS    0H                                                               
         BAS   RE,PUTTAPE          PUT OUT PREV RECD                            
                                                                                
PCNV7    DS    0H                                                               
         MVI   CORRFLAG,NO         NOT A CORRECTION RECORD                      
         MVC   PREVKEY,THISKEY     SAVE KEY VALUES                              
         MVC   PRGKEY,INTKEY                                                    
         GOTO1 ABLDREC,DMCB,PRKEY  BUILD -P- RECD                               
         BAS   RE,BLDELEM          BUILD ELEMENTS                               
         BAS   RE,BLDEMS           BUILD DEMO ELEMENTS                          
         B     CNVX                                                             
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
**********************************************************************          
*BLDELEM - BUILD ELEMENTS FOR EACH OF TH  -P- AND -Q- RECS                      
**********************************************************************          
BLDELEM  NTR1                                                                   
                                                                                
BLDEL05  LA    R6,TEMP             BUILD MARKET TYPE ELEMENT                    
         XC    TEMP,TEMP                                                        
         USING MARELEM,R6                                                       
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         LA    R1,1                1 = USA                                      
         CLC   INTSTA(4),=C'HUT '  ASSIGN HUT MKT = 1                           
         BE    BLDEL06                                                          
         CLC   INTSTA(4),=C'UUUU'  ASSIGN UUU MKT = 1                           
         BE    BLDEL06                                                          
         PACK  DUB,INTSTA(4)                                                    
         CVB   R1,DUB                                                           
BLDEL06  STCM  R1,3,MARNO          CABLE STATION CODE IS MARKET                 
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         BAS   RE,BLDDT            BUILD DEMO DATA TYPE ELEMENT                 
         GOTO1 APUTEL,TEMP         X'08'                                        
                                                                                
*--------------------------------------------------------------------           
*'P' RECD ELEMENTS ONLY                                                         
*--------------------------------------------------------------------           
BLDEL60  DS    0H                  BUILD 'P' RECORD ELEMENTS                    
         CLI   THISKEY,PRCODEQU                                                 
         BNE   BLDELX                                                           
         LA    R6,TEMP                                                          
         USING PHTELEM,R6          BUILD ELEMENT TO SAVE INTCOV                 
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHTCODE,PHTCODEQ                                                 
         MVI   PHTLEN,PHTLNEQ                                                   
         MVC   PHTDTYP,INTDTYP                                                  
         MVC   PHTDPT,INTDPT                                                    
         MVC   PHTPREM,INTPREM                                                  
         MVC   PHTCOVR,INTCOV      SAVE COVERAGE                                
         GOTO1 APUTEL,PHTELEM                                                   
         DROP  R6                                                               
*                                                                               
         USING PHELEM,R6           BUILD DAY/QTR HOUR ELEMENT                   
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHCODE,PHCODEQ                                                   
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
         MVI   PPNCODE,PPNCODEQ                                                 
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
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
         LA    RE,INTKEY                                                        
         MVC   SLSECT,PRBTYP-PRKEY+4(RE)   MKT NUMBER ON -P- RECD               
         GOTO1 APUTEL,SLELEM                                                    
*                                                                               
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'CAB'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVI   DBSELMED,C'N'       --MEDIA--                                    
         MVI   DBSELSRC,C'R'                                                    
         MVC   DBSELBK,INTIBOOK                                                 
         L     RF,ACOMFACS                                                      
         ST    RF,DBCOMFCS                                                      
         MVC   WORK(10),OFORMAT    SET UP OUTPUT FORMAT BLOCK                   
*        MVC   WORK+7(2),=X'5D26'                                               
         MOVE  (CONDWORK,1000),INTACCS                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDEMEL                                                        
***      GOTO1 (RF),DMCB,(C'C',WORK),DBLOCKD,CONDWORK                           
         GOTO1 (RF),DMCB,(C'C',0),DBLOCKD,CONDWORK                              
         DROP  RF                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CONDWORK(2),CONDWORK   TEST FOR ANY DEMO ELEMENTS                
         BZ    BLDEMX              NONE-PUT RECORD TO TAPE                      
         LA    R1,CONDWORK+2                                                    
         SR    R0,R0                                                            
*                                  ADD DEMO ELEMENTS TO RECORD                  
BLDEM30  CLI   0(R1),0                                                          
         BE    BLDEMX                                                           
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLDEM30                                                          
*                                  PUT RECORD TO TAPE & SAVE VALUES             
BLDEMX   MVC   PREVKEY,THISKEY                                                  
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* LASTHK-LAST TIME HOOK TO RELEASE FINAL RECORDS                                
* ********************************************************************          
LASTHK   DS    0H                                                               
         OC    PREVKEY,PREVKEY                                                  
         BZ    LST15                                                            
         CLI   PRGKEY,C'P'         TIME PERIOD RECD?                            
         BNE   LST10               JUST RELEASE RECD FROM BUFFER                
         BAS   RE,PUTTAPE                                                       
         B     LST15                                                            
*                                                                               
LST10    BAS   RE,FINFF            OUTPUT '0F-FF' ON PRG RECDS                  
*                                                                               
LST15    MVI   SORTSW,0                                                         
*                                                                               
LST30    B     CNVX                                                             
         EJECT                                                                  
**********************************************************************          
* USING INTKSRC, X'08' ELEMENT PASSED BACK IN TEMP                              
**********************************************************************          
BLDDT    NTR1                                                                   
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING DTELEM,R6           BUILD DEMO DATA TYPE ELEMENT                 
         MVI   DTCODE,DTCODEQ      X'08'                                        
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         GOTO1 (RF),DMCB,VWDESCTB                                               
         ICM   R1,15,DMCB                                                       
         DROP  R1                                                               
*                                                                               
BDT10    CLC   INTKSRC,0(R1)                                                    
         BE    BDT20                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,VWDESCL(R1)                                                   
         B     BDT10                                                            
*                                                                               
BDT20    ZIC   RE,1(R1)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DTTYPE(0),2(R1)                                                  
         AHI   RE,DTLENQ+1         DESC LENGTH + ELEM LEN + 1 FOR EX            
         STC   RE,DTLEN                                                         
         DROP  R6                                                               
*                                                                               
BDTX     B     XIT                                                              
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
PUTTAPEX B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
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
SVFULL   DS    A                                                                
*                                                                               
AVGWKS   DS    X                                                                
STDATE   DS    CL6                                                              
THISKEY  DC    XL20'00'                                                         
NTIFIL   DC    C'PAVFIL  '                                                      
THISBOOK DC    X'00'                                                            
OLDFIL   DC    H'00'               FILE NUMBER OF ORIG RECD ON FILE             
DDSNUM   DC    H'00'               DDS NUMBER FOR NTI PROGRAM                   
DDSBK    DC    H'00'               BOOK FOR DDS NUMBERS                         
DDSBKTY  DC    X'00'               BOOK TYPE FOR DDS NUMBERS                    
FILENUM  DC    H'00'               FILE NUMBER FOR DDS PROGRAM                  
STFILE   DS    H'00'               START FILE# FOR ENTIRE PRG                   
TYPE     DC    X'00'                                                            
NOSUMY   DC    X'00'                                                            
TEST     DC    X'00'                                                            
USETHBK  DC    C'N'                                                             
BLANKS   DC    CL80' '                                                          
AVGDEL   DC    C'N'                                                             
MARFLAG  DC    C'N'                FLAG TO MARK X'01' ELEM FOUND                
COPYMAR  DC    C'N'                COPY X'01' ELEM?                             
FRSTREC  DC    AL1(YES)            FIRST RECORD TO OPHASE                       
CORRFLAG DC    AL1(NO)             CORRECTION FLAG                              
*                                  LAST TIME VALUES                             
PRGORIG  DC    X'00'               INTORIG FIELD OF PREV RECD                   
PREVKEY  DC    XL20'00'                                                         
PRGKEY   DC    XL30'00'            PROGRAM KEY (FROM INTKEY)                    
AORKEY   DC    XL(QLENQ)'00'       QPRGKD KEY FROM INTKEY                       
* -----CNTRL RECD FIELDS -------------                                          
CNTKEY   DC    XL20'00'            CNTL RECD KEY FOR PREV PRG                   
CNTMTYP  DC    X'00'               MARKET TYPE-- FROM INTMTYP                   
CNTSTYP  DC    X'00'               STATION TYPE-- FROM INTSTYP                  
CNTBITS  DC    XL96'00'            DAY & TIME TELCS AIRED BITS                  
CNTUTRK  DC    X'00'               TRKD OR UNTRKD PROGRAM INDICATOR             
CNTFLG   DC    X'00'               0=UNIV DEMOS LIST  1=ACTUAL ELEMS            
CNTORIG  DC    X'00'               '1'=MODIFIED, '2'=ENTIRE RECD DELTD          
CNTUNVS  DS    1000X               UNIVERSES                                    
*                                                                               
OFORMAT  DS    0CL10                                                            
         DC    C'CABNTCR',AL1(11,06,00)                                         
RATING   DC    X'01',C'R',X'01',X'01',C'K',X'01',X'FF'                          
PAVFIL   DC    C'PAVFIL'           HELLO FILE NAME                              
SORTCARD DC    C'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(1500,,,,) '                              
         SPACE 2                                                                
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
NTITBL   DC    XL12'00'            NTI-DDS: NET(5) , NTI(5) ,  DDS(2)           
         DC    2500XL12'00'                                                     
         DC    X'FFFFFFFFFF'                                                    
*                                                                               
DDSTBL   DC    XL(DDSQ)'00'        DDS-FILE: NET(5) , DDS(2), FILE(2)           
*        DC    2500XL(DDSQ)'00'                                                 
         DC    3500XL(DDSQ)'00'                                                 
         DC    X'FFFFFFFFFF'                                                    
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER TEMP W/S                                                       
**********************************************************************          
*                                                                               
NADWRKD  DSECT                                                                  
SAVEREG  DS    A                                                                
AHUTEL   DS    A                                                                
KEY      DS    XL23                                                             
KEYSAVE  DS    XL23                                                             
PTRKEY   DS    XL23                                                             
CONDWORK DS    1000C                                                            
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
DDSPRG   DS    CL2                 DDS PROGRAM NUMBER                           
DDSFILE  DS    CL2                 DDS INTERNAL FILE NUMBER                     
DDSSTAT  DS    CL1                 STATUS- 'A'=ADD 'D'=DELETE                   
DDSQ     EQU   *-DDSTBLD                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTCMITD                                                     
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
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076DERENTPO  09/06/11'                                      
         END                                                                    
