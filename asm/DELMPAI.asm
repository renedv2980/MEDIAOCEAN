*          DATA SET DELMPAI    AT LEVEL 015 AS OF 07/31/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMPAIA                                                                 
*INCLUDE LMPAIR                                                                 
         TITLE 'LOCAL MONTHLY CONVERSION'                                       
*****************************************************************               
* JUL/2010: CREATED DELMPAI TO PROCESS LOCAL MONTHLY DATA (SEAN)                
*   INPUT : PROGRAM AVERAGE WORK RECORDS CREATED BY OANA.                       
*   OUTPUT: DDS DEMO RECORDS     :  PTN                                         
*   NOTE!!: COPIED SOME OF THE HARDCODE EXCEPTIONS FROM THE OLD                 
*           CONVERSION, MADE NOTE OF EACH.                                      
*****************************************************************               
*                                                                               
DELMPAI  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DELMPAI                                                        
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)                                                         
         USING LMDSECT,RC                                                       
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA                                                      
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
***********************************************************************         
OPENOK   DS    0H                                                               
*                                                                               
         L     R4,ARREC            CLEAR THE LAST RECORD READ.                  
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),=Y(RRECL+4)                                              
         LA    R4,4(R4)                                                         
*                                                                               
         GET   IN1,(R4)                                                         
         BAS   RE,CALCTEEN         CREATE M AND W 12-17                         
*                                                                               
         CLC   W_KMKTC,=C'124'    ATLANTA HARDCODE EXCEPTION                    
         BNE   *+10                                                             
         MVC   W_KMKTC,=C'168'                                                  
*                                                                               
         CLC   W_KMKTC,=C'295'    SANTA CLARA HARDCODE EXCEPTION                
         BNE   *+10                                                             
         MVC   W_KMKTC,=C'480'                                                  
*                                                                               
***********************************************************************         
* RECTYPE                          INTRTYP                                      
***********************************************************************         
         CLC   W_MHQMKTC,TEMPMNO                                                
         BE    LMRT10                                                           
         XC    UNIQBTY,UNIQBTY                                                  
         MVI   UNIQBTY,X'FF'                                                    
         MVC   TEMPMNO,W_MHQMKTC                                                
         MVC   TEMPMKT,W_MHQGEONM                                               
*                                                                               
***********************************************************************         
LMRT10   MVI   INTRTYP,PRCODEQU   PAV RECORD TYPE = " P "                       
***********************************************************************         
* MARKET                           INTMRKT                                      
***********************************************************************         
LMMRKT   XC    PREVKEY,PREVKEY                                                  
         PACK  DUB,W_KMKTC                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,INTMRKT                                                     
***********************************************************************         
* MARKET TYPE                      INTMTYP                                      
***********************************************************************         
         MVC   INTMTYP,W_MHQCMETH                                               
***********************************************************************         
* BOOK                             INTBOOK                                      
***********************************************************************         
LMBOOK   PACK  DUB,W_KRPYR                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'1999'                                                      
         BH    *+12                                                             
         SH    RE,=H'1900'                                                      
         B     *+8                                                              
         SH    RE,=H'2000'                                                      
         CHI   RE,27               Y2K FIX--IF 2-CHAR YEAR < 27,                
         BH    *+8                                                              
         AHI   RE,100              MAKE IT A 21ST CENTURY YEAR                  
         STC   RE,INTBOOK                                                       
         PACK  DUB,W_KRPRD                                                      
         CVB   RE,DUB                                                           
         STC   RE,INTBOOK+1                                                     
*                                                                               
         CLI   FORCE,C'Y'                                                       
         BNE   *+10                                                             
         MVC   INTBOOK,FILTBOOK+1  SET FORCE BOOK VALUE                         
*                                                                               
***********************************************************************         
* STATION                          INTSTA                                       
***********************************************************************         
LMSTAT   MVC   INTSTA(4),W_KOCALL                                               
         MVI   INTSTA+4,C'T'                                                    
***********************************************************************         
* STATION TYPE                     INTSTYP/INTSPILL                             
***********************************************************************         
LMSTYP   CLI   W_DHRSTYPE,C'0'                                                  
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLI   W_DHRSTYPE,C'1'                                                  
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLI   W_DHRSTYPE,C'2'                                                  
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLI   W_DHRSTYPE,C'5'                                                  
         BNE   *+8                                                              
         MVI   INTSTYP,PS1E                                                     
         CLI   W_DHRSTYPE,C'8'                                                  
         BNE   *+8                                                              
         MVI   INTSTYP,S1EQU                                                    
         CLI   W_DHRSTYPE,C'9'                                                  
         BNE   *+8                                                              
         MVI   INTSTYP,OUTMARE                                                  
*                                                                               
LMST10   TM    INTSTYP,X'20'                                                    
         BZ    *+8                                                              
         MVI   INTSPILL,C'Y'                                                    
***********************************************************************         
* BOOKTYPE                         INTBTYP                                      
* .      NUMEROUS BOOKTYPES DO NOT APPLY FOR PROGRAM AVERAGE                    
* ..     HOWEVER, WE WILL STILL FILL IN THE KEY TO MATCH AGAINST                
* ...    THE TABLE, WE WILL LET THE CONTENT OF THE TABLE DECIDE                 
* ....   WHETHER WE WILL BYPASS A CERTAIN RECORD OR NOT.                        
* .....  EXAMPLE:  WIRED, CABLE, ETHNIC                                         
* ...... THESE RECORDS SHOULD BE EXCLUDED BEFORE THE CONVERSION                 
* .......BUT WE NEED TO BE CAREFUL, TO AVOID DUPLICATE RECORDS.                 
***********************************************************************         
LMLKUP   DS    0H                  FIRST CHECK TO SEE IF THERE IS MKT           
LMLK10   GOTO1 VLDCREC,DMCB,0,0    LEVEL PREDEFINED BOOKTYPE                    
         L     RE,DMCB                                                          
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NONE THERE                            
LMLK20   CLC   0(2,RE),=C'NT'      FIND THE USA NIELSEN TV ONE                  
         BE    LMLK30                                                           
         ICM   RE,7,2(RE)          NF - TRY NEXT ONE                            
         OC    0(2,RE),0(RE)       EOT - DIE SOMETHING WRONG                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     LMLK20                                                           
*                                                                               
LMLK30   LA    RE,5(RE)            BYPASS HEADER                                
LMLK40   OC    0(2,RE),0(RE)       EOT                                          
         BNZ   *+6                 DIE -- PREVENT OVERWRITING                   
         DC    H'0'                 DATA ALREADY LOADED                         
                                                                                
         CLC   0(2,RE),INTMRKT     HAVE IT                                      
         BE    *+12                                                             
         A     RE,DMCB+4           L'ENTRY                                      
         B     LMLK40              NEXT ONE                                     
         MVC   MYBTYP,2(RE)                                                     
*                                                                               
LMBTYP   MVC   LMBTKEY,SPACE                                                    
         MVC   LMBTKEY(1),W_KPBACK L/LS/L3/L7                                   
* EFFECTIVE JUL18 SURVEY, DO NOT FOCE LIVE+1 TO STANDARD                        
* FOR JUN2018 WE WILL NOT HAVE LIVE+1, WE WILL HAVE LIVE+7 AS STANDARD          
* JUL2018 WE WILL HAVE BOTH LIVE+1 AND LIVE+7                                   
* LIVE+7 SHOULD REMAIN AS STANDARD                                              
*                                                                               
         CLC   INTBOOK,=AL2(JUL_18)                                             
***      CLC   INTBOOK,=AL2(MAY_16)  TESTING PURPOSE ONLY                       
         BNL   LMBTYP10                                                         
*                                                                               
         CLI   LMBTKEY,C'1'        LIVE+1 IS STANDARD FOR DIARY                 
         BNE   *+8                                                              
         MVI   LMBTKEY,C'7'        LIVE+7 IS THE STANDARD BOOKTYPE KEY          
*                                                                               
LMBTYP10 CLI   W_MHQCMETH,W_COLLECTION_METHOD_LPM_PRELIM                        
         BNE   *+8                                                              
         MVI   LMBTKEY,C'P'                                                     
*  -----------------------------------------------------------------            
*  FIELD #2 IS DATA TYPE: ALL DATA TYPES ARE MUTUALLY EXCLUSIVE                 
*  THEY ARE NOT EXPECTED TO CO-EXIST.  THERE ARE NO BOOKTYPE                    
*  SUPPORT FOR THE COMBINATION OF ANY OF THE DATATYPES BESIDES                  
*  STANDARD. EX: THERE WON'T BE BLACK+HISPANIC, OR OLYMPIC+PRNT ONLY            
*  -----------------------------------------------------------------            
*                                                                               
         CLI   MYBTYP,C' '         OVERRIDE?                                    
         BE    *+14                                                             
         MVC   LMBTKEY+1(1),MYBTYP                                              
         B     LMBT10                                                           
*                                                                               
         CLI   W_MHQRSRVC,RSRVC_NHSI  NHSI?                                     
         BNE   *+12                                                             
         MVI   LMBTKEY+1,C'H'                                                   
         B     LMBT10                                                           
*                                                                               
         OC    W_KSUBSI,SPACE      TURN TO UPPER-CASE                           
         CLC   W_KSUBSI,=C'AF-AM'                                               
         BNE   LMBT03                                                           
         MVI   LMBTKEY+1,C'B'      AFRICAN AMERICAN                             
*                                                                               
* FOR BLACK ETHNIC SURVEYS, THERE IS NO PAV DATA GENERATED. THIS WOULD          
* CAUSE DEMCNV TO ISSUE AN "EMPTY OTAPE GENERATED" WARNING E-MAIL.              
* WE DO *NOT* WANT THAT WARNING E-MAIL SENT UNLESS IT'S APPROPRIATE.            
* SO WE SET A FLAG HERE TO TELL TO TELL DEMCNV TO INHIBIT THAT E-MAIL.          
* NOTE THAT WE CANNOT TELL THE DIFFERENCE IN THIS PROGRAM BETWEEN A             
* LEGITIMATE EMPTY OTAPE DATASET AND AN EMPTY OTAPE DATASET WHICH               
* SHOULDN'T BE EMPTY. SO IT COULD THEORETICALLY HAPPEN THAT WE WON'T            
* GET A WARNING E-MAIL WHEN WE REALLY DO WANT ONE. THAT'S A CALCULATED          
* RISK WE'RE CHOOSING TO TAKE.                                                  
*                                                                               
         OI    FLAGS1,EMPTY_OTAPE_OKAY   EMPTY OTAPE DATASET IS OKAY            
         B     LMBT10                                                           
*                                                                               
LMBT03   DS    0H                                                               
*                                                                               
*  THE FIELD TO SET THE HYBRID (EXTENDED) DATA TYPE (AKA "IMPACT" DATA)         
*                                                                               
         CLI   W_KRSRVC,RSRVC_NSIX HYBRID (EXTENDED) OR "IMPACT"                
         BNE   LMBT04                                                           
         MVI   LMBTKEY+1,C'E'                                                   
         CLC   W_KSUBSI,=C'HISP '                                               
         BNE   LMBT10                                                           
         MVI   LMBTKEY+1,C'D'      HYBRID HISPANIC                              
         B     LMBT10                                                           
*                                                                               
LMBT04   DS    0C                                                               
         CLC   W_KSUBSI,=C'HISP '                                               
         BNE   LMBT05                                                           
         MVI   LMBTKEY+1,C'H'      HISPANIC AMERICAN                            
         CLC   INTBOOK,=AL2(NOV_11) PRE NOV-11, NO PAV FOR ETHNIC               
         BNL   *+8                                                              
         MVI   LMBTKEY+1,C'S'      DIFFERENTIATE FROM NHSI                      
         B     LMBT10                                                           
*                                                                               
LMBT05   CLC   W_KSPEXC,=C'SX'                                                  
         BNE   *+12                                                             
         MVI   LMBTKEY+1,C'O'      OLYMPIC EXCLUSION                            
         B     LMBT10                                                           
*                                                                               
*  -----------------------------------------------------------                  
*                                                                               
LMBT10   CLI   W_DHRSTYPE,C'1'     HOME STATION OR LORIG                        
         BE    LMBT20                                                           
*                                                                               
         CLI   W_DHRDSRC,C'C'      BROADCAST OR CABLE?                          
         BNE   *+8                                                              
         MVI   LMBTKEY+2,C'C'                                                   
*                                                                               
*  -----------------------------------------------------------                  
LMBT20   CLI   W_KSAMTY,STYP_WIRED TOTAL DMA OR WIRED                           
         BNE   *+8                                                              
         MVI   LMBTKEY+3,C'W'                                                   
*                                                                               
         LA    R1,BTYPTAB                                                       
LMBT30   CLI   0(R1),X'FF'                                                      
         BE    SKIP                                                             
         CLC   LMBTKEY,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,L'BTYPTAB(R1)                                                 
         B     LMBT30                                                           
         MVC   INTBTYP,L'BTYPTAB-1(R1)                                          
         B     LMBTX                                                            
*                                                                               
LMBTKEY  DS    XL(L'BTYPTAB-1)                                                  
                                                                                
LMBTX    DS    0H                                                               
         BAS   RE,MSGREC                                                        
         BAS   RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BE    *+12                                                             
         MVI   MSGFLAG,0                                                        
         B     SKIP                                                             
***********************************************************************         
* WEEKS                            INTWEEKS                                     
***********************************************************************         
LMWEEK   MVI   INTWEEKS,0          SET UP ACTIVE WEEKS                          
         CLI   W_KAWK1,C' '                                                     
         BE    *+8                                                              
         OI    INTWEEKS,X'08'                                                   
         CLI   W_KAWK2,C' '                                                     
         BE    *+8                                                              
         OI    INTWEEKS,X'04'                                                   
         CLI   W_KAWK3,C' '                                                     
         BE    *+8                                                              
         OI    INTWEEKS,X'02'                                                   
         CLI   W_KAWK4,C' '                                                     
         BE    *+8                                                              
         OI    INTWEEKS,X'01'                                                   
***********************************************************************         
* RECODE: DDS TREAT AV'S AND M-F'S DIFFERENT FROM NIELSEN                       
***********************************************************************         
         CLC   W_KDAY_ALF,=AL3(AV5_ALF)  AV5 EXCLUDING WEEKEND=M-F              
         BNE   LMRC10                                                           
         CLI   W_KDAY,AV_EXCL_WKND                                              
         BNE   LMRC10                                                           
         MVC   W_KDAY_ALF,=AL3(MF_ALF)                                          
         MVI   W_KDAY,MONDAY_FRIDAY                                             
         B     LMRCX                                                            
                                                                                
LMRC10   DS    0H                                                               
         CLI   W_KDAY,AV_INCL_WKND CHANGE THE DAYCOE TO '6' FOR ALL             
         BNE   LMRCX               AV'S NOT 7 DAY AVERAGE.                      
         CLC   W_KDAY_ALF,=AL3(AV7_ALF)                                         
         BE    LMRCX                                                            
         MVI   W_KDAY,AV_EXCL_WKND                                              
                                                                                
LMRCX    DS    0H                                                               
***********************************************************************         
* DAY/TIME                         INTDAY,INTSQH,INTEQH                         
***********************************************************************         
         LA    RF,DAYTABL          GET DAYS                                     
LMDT10   CLC   W_KDAY,0(RF)                                                     
         BE    LMDT20                                                           
         LA    RF,L'DAYTABL(RF)                                                 
         CLI   0(RF),X'FF'                                                      
         BNE   LMDT10                                                           
         DC    H'0'                                                             
*                                                                               
LMDT20   MVC   INTDAYWK,1(RF)                                                   
         MVC   INTNDAYS,2(RF)                                                   
*                                                                               
         CLI   INTDAYWK,9          MULTI-DAY AVERAGE?                           
         BNE   LMDT30                                                           
         CLC   W_KDAY_ALF(2),=C'AV'  AV#                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   INTNDAYS,W_KDAY_ALF+2                                            
         NI    INTNDAYS,X'0F'                                                   
*                                                                               
LMDT30   ZIC   RE,W_PNRSQH         MATCH DDS INTERNAL TIME                      
***      CLC   INTBOOK,=AL2(NOV_13) PRE NOV13 IS 5A START-TESTING               
         CLC   INTBOOK,=AL2(DEC_15) DEC15-1ST 3AM START OF DAY BOOK             
         BNL   LMDT36                                                           
*                                                                               
* 5A START TIME CODE                                                            
         CLI   W_PNRSQH,3          5A-545A ARE 92-95                            
         BH    *+12                                                             
         AHI   RE,92                                                            
         B     *+8                                                              
         SHI   RE,4                6A-445A ARE 0-91                             
         STC   RE,INTSQH                                                        
*                                                                               
         ZIC   RE,W_PNREQH         MATCH DDS INTERNAL TIME                      
         CLI   W_PNREQH,3          5A-545A ARE 92-95                            
         BH    *+12                                                             
         AHI   RE,92                                                            
         B     *+8                                                              
         SHI   RE,4                6A-445A ARE 0-91                             
         B     LMDT38                                                           
*                                                                               
* 3A START TIME CODE                                                            
LMDT36   CLI   W_PNRSQH,11         5A-545A ARE 92-95                            
         BH    *+12                                                             
         AHI   RE,84                                                            
         B     *+8                                                              
         SHI   RE,12               6A-445A ARE 0-91                             
         STC   RE,INTSQH                                                        
*                                                                               
         ZIC   RE,W_PNREQH         MATCH DDS INTERNAL TIME                      
         CLI   W_PNREQH,11         5A-545A ARE 92-95                            
         BH    *+12                                                             
         AHI   RE,84                                                            
         B     *+8                                                              
         SHI   RE,12               6A-445A ARE 0-91                             
*                                                                               
LMDT38   AHI   RE,1                EX: EQH OF 845 IS REALLY 900                 
         STC   RE,INTEQH                                                        
         CLC   INTEQH,INTSQH       EQH CANT BE LOWER THAN SQH                   
         BNL   LMDT40                                                           
         AHI   RE,96                                                            
         STC   RE,INTEQH                                                        
*                                                                               
LMDT40   ICM   RE,3,W_PNRLQH       WORKREC HAS 2 BYTES FOR TOTAL DUR            
         STC   RE,INTADUR                                                       
         STCM  RE,3,INTADUR2       2-BYTE TOTAL DURATION                        
***********************************************************************         
* PROGRAM INFO                     INTPNAM,INTPNUM,INTPTYP,INTPRSRC             
***********************************************************************         
LMPRGM   MVC   INTPNAME,W_PNRPNAME                                              
         MVI   INTDTYP,1                                                        
         CLI   W_RECTYPE,RECTYPE_ASTCAST                                        
         BE    LMPG10                                                           
         MVC   INTPNAME+9(5),=C'(NOR)'                                          
         MVI   INTDTYP,0                                                        
*                                                                               
LMPG10   BAS   RE,AIRDAYS          STORE DAY MAPS IN BIT PATTERN                
*                                                                               
         CLC   W_PNRPCODE,SPACE                                                 
         BE    *+14                                                             
         CLC   W_PNRPCODE,ZEROS                                                 
         BNE   *+14                                                             
         XC    INTPNUM,INTPNUM                                                  
         B     LMPG15                                                           
         PACK  DUB,W_PNRPCODE                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,INTPNUM                                                    
LMPG15   MVC   INTPTYP,W_PNRPTYP                                                
         MVC   INTPRSRC,W_PNRPSRC                                               
*                                                                               
LMPGX    DS    0H                                                               
*                                                                               
*              BEGIN DEMO SLOTTING BY TABLES                                    
*                                                                               
* DMA IMPRESSIONS                                                               
LMSLOT   DS    0H                                                               
         LA    R4,CDMA2INT                                                      
         LA    R6,INTACCS                                                       
         LA    R7,W_QDRMTROA                                                    
         BAS   RE,CNVRTOI                                                       
***********************************************************************         
* TSA IMPRESSIONS                                                               
         LA    R4,CTSA2INT                                                      
         LA    R6,INTACCS                                                       
         LA    R7,W_STRHHLDS                                                    
         BAS   RE,CNVRTOI                                                       
***********************************************************************         
* UNIVERSE                                                                      
LMUNIV   LA    R4,CUNV2INT                                                      
         LA    R6,INTACCS                                                       
         LA    R7,W_UERDEMOS                                                    
         BAS   RE,CNVRTOI                                                       
*                                                                               
***********************************************************************         
* HUT/PUT                                                                       
LMHPUT   LA    R4,CHPT2INT                                                      
         LA    R6,INTACCS                                                       
         LA    R7,W_HPTDEMOS                                                    
         BAS   RE,CNVRTOI                                                       
***********************************************************************         
* MARKET TOTALS (Q)                                                             
LMTOTAL  LA    R4,CTOT2INT                                                      
         LA    R6,INTACCS                                                       
         LA    R7,W_HPSDEMOS                                                    
         BAS   RE,CNVRTOI                                                       
*                                                                               
***********************************************************************         
         BAS   RE,SETKEY                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CNVWR: LAST TIME HOOK CALLED BY DEMCNV BEFORE FINALLY GOING TO OPHASE         
***********************************************************************         
CNVWR    DS    0H                                                               
         L     R6,ASREC            POINT AT SORT RECORD                         
         LR    R2,R6               ADDRESSABILITY FOR INTERIM VALUES            
         LA    R6,4(R6)            POINT AT RECORD START                        
         USING PRKEY,R6                                                         
         CLC   PREVKEY,PRKEY       TEST FOR KEYS IN SEQUENCE                    
         BL    CNVWR2              OK                                           
         LA    RE,PREVKEY          PICK UP DAY/WEEK FROM PREV SORT KEY          
         ZIC   R1,PRDW-PRKEY(RE)                                                
         LA    R1,1(R1)            INCREMENT VALUE TO INSURE                    
         STC   R1,PRDW             SEQUENTIALITY                                
         STC   R1,INTDAYWK         FORCE OUTPUT PHASE TO USE BUMPED KEY         
         SPACE 1                                                                
CNVWR2   MVC   PREVKEY,PRKEY       UPDATE PREVIOUS KEY                          
         BAS   RE,CALCHHLD                                                      
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
SETKEY   NTR1  ,                   BUILD PAV SORT KEY                           
         L     R6,AIREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'P'                                                     
         MVI   PRCODE,PRCODEQU                                                  
         MVI   PRMEDIA,C'T'                                                     
         MVI   PRSRC,C'N'                                                       
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP      FORCE SATELLITES TO SORT TOG                 
         TM    INTSTYP,X'20'       SET UP SPILL STATION                         
         BZ    *+14                                                             
         MVI   INTSPILL,C'Y'                                                    
         MVC   PRKMKT,INTMRKT                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK       SET UP DAY AND WEEK                          
         ZIC   RE,INTDAYWK                                                      
         SLL   RE,4                                                             
         STC   RE,PRDW                                                          
         TM    INTWEEKS,X'0F'                                                   
         BO    CNVDWX                                                           
         CLI   INTWEEKS,0                                                       
         BE    CNVDWX                                                           
         ZIC   RF,INTWEEKS         CALCULATE PROPER WEEKS                       
         SLL   RF,28                                                            
         SR    RE,RE                                                            
         LA    R1,1                                                             
CNVDW1   SLDL  RE,1                COUNT UP TO START WEEK                       
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         B     CNVDW2                                                           
         LA    R1,1(R1)                                                         
         B     CNVDW1                                                           
CNVDW2   ZIC   RE,PRDW             GET SHIFTED DAY                              
         SLL   R1,1                SHIFT START WEEK                             
         OR    RE,R1               INSERT INTO KEY                              
         STC   RE,PRDW                                                          
CNVDWX   MVC   INTDAYWK,PRDW       SET INTERIM DAY AND WEEK                     
         MVC   PRDW+1(1),INTDTYP   FORCE (NOR) TO SORT FIRST                    
         DROP  R6                                                               
         XIT1                                                                   
***********************************************************************         
CALCTEEN NTR1                                                                   
         ICM   R1,15,W_QDRM1214         DMA                                     
         ICM   R2,15,W_QDRM1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_QDRM1214                                                 
         XC    W_QDRM1517,W_QDRM1517                                            
*                                                                               
         ICM   R1,15,W_QDRF1214                                                 
         ICM   R2,15,W_QDRF1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_QDRF1214                                                 
         XC    W_QDRF1517,W_QDRF1517                                            
*                                                                               
         ICM   R1,15,W_UERM1214         UNIVERSE                                
         ICM   R2,15,W_UERM1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_UERM1214                                                 
         XC    W_UERM1517,W_UERM1517                                            
*                                                                               
         ICM   R1,15,W_UERF1214                                                 
         ICM   R2,15,W_UERF1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_UERF1214                                                 
         XC    W_UERF1517,W_UERF1517                                            
*                                                                               
         ICM   R1,15,W_STRM1214         TSA                                     
         ICM   R2,15,W_STRM1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_STRM1214                                                 
         XC    W_STRM1517,W_STRM1517                                            
*                                                                               
         ICM   R1,15,W_STRF1214                                                 
         ICM   R2,15,W_STRF1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_STRF1214                                                 
         XC    W_STRF1517,W_STRF1517                                            
*                                                                               
         ICM   R1,15,W_HPTM1214         HPT                                     
         ICM   R2,15,W_HPTM1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_HPTM1214                                                 
         XC    W_HPTM1517,W_HPTM1517                                            
*                                                                               
         ICM   R1,15,W_HPTF1214                                                 
         ICM   R2,15,W_HPTF1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_HPTF1214                                                 
         XC    W_HPTF1517,W_HPTF1517                                            
*                                                                               
         ICM   R1,15,W_HPSM1214         MKT TOTAL                               
         ICM   R2,15,W_HPSM1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_HPSM1214                                                 
         XC    W_HPSM1517,W_HPSM1517                                            
*                                                                               
         ICM   R1,15,W_HPSF1214                                                 
         ICM   R2,15,W_HPSF1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_HPSF1214                                                 
         XC    W_HPSF1517,W_HPSF1517                                            
CTX      XIT1                                                                   
***********************************************************************         
*&&DO                                                                           
GETSI    NTR1                                                                   
         XC    MKTKEY,MKTKEY                                                    
         LA    R5,MKTKEY                                                        
         USING MLKEY,R5                                                         
         MVI   MLCODE,MLCODEQU                                                  
         MVI   MLMEDIA,C'T'                                                     
         MVI   MLSRC,C'N'                                                       
         MVC   MLBOOK,INTBOOK                                                   
         XC    MLBOOK,=X'FFFF'                                                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
         MVC   MLSTAT,INTSTA                                                    
         MVC   MLSTYP,INTSTYP                                                   
         MVC   MLBTYP,INTBTYP                                                   
*                                                                               
         MVC   MKTKEYSV,MKTKEY                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'DEMDIR',MKTKEY,MKTKEYSV             
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   MKTKEY,MKTSV                                                     
         BE    GETBX                                                            
*                                                                               
         CLC   MKTKEY(MLSTAT+5-MLKEY),MKTSV                                     
         BNE   GETBX                                                            
         OC    MLKMKT,MLKMKT                                                    
         BZ    GETBX                                                            
         MVI   INTSPILL,C'Y'                                                    
*                                                                               
         MVC   MLBTYPE,INTBTYP                                                  
         MVC   MKTKEYSV,MKTKEY                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'DEMDIR',MKTKEY,MKTKEYSV             
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
GETBX    XIT1                                                                   
*&&                                                                             
***********************************************************************         
AIRDAYS  NTR1                                                                   
         XC    INTPDMAP,INTPDMAP   4 BYTE DAYS AIRED MAP                        
         MVI   INTPDNUM,0                                                       
         GOTO1 =V(LMPAIR),DMCB,(0,W_KDAYMAP),INTPDMAP,0,0                       
         ZIC   R0,4(R1)                                                         
         STC   R0,INTPDNUM                                                      
*                                                                               
ADX      XIT1                                                                   
***********************************************************************         
MSGREC   NTR1                                                                   
         L     RE,=A(UNIQBTY)      STORE UNIQUE BOOKTYPES IN TABLE              
MSGR05   CLI   0(RE),X'FF'                                                      
         BNE   MSGR10                                                           
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    MSGRX                                                            
         B     MSGR20                                                           
MSGR10   CLC   INTBTYP,0(RE)        NOT UNIQUE                                  
         BE    MSGRX                                                            
         LA    RE,1(RE)                                                         
         B     MSGR05                                                           
MSGR20   MVC   0(1,RE),INTBTYP                                                  
         MVI   1(RE),X'FF'                                                      
************************************************************                    
         L     R6,AMREC                                                         
         LA    R6,4(R6)                                                         
         MVC   0(3,R6),TEMPMNO     MARKET NUMBER                                
         LA    R6,3(R6)                                                         
         MVI   0(R6),C','          ,                                            
         LA    R6,1(R6)                                                         
         LA    R1,TEMPMKT+25       MARKET NAME - SPACES                         
MSGR30   CLI   0(R1),X'40'                                                      
         BNE   MSGR40                                                           
         SHI   R1,1                                                             
         B     MSGR30                                                           
*                                                                               
MSGR40   LA    RF,TEMPMKT          CALCULATE LENGTH                             
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),TEMPMKT                                                  
         AHI   R1,1                                                             
         AR    R6,R1                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         MVC   0(1,R6),INTBTYP     BOOKTYPE                                     
         CLI   INTBTYP,0                                                        
         BE    MSGR70                                                           
*                                                                               
         GOTO1 CDEMTABS,DMCB,SPBOOKTB                                           
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)           LENGTH OF TABLE ENTRIES                       
         USING SPBKTYPD,RE                                                      
*                                                                               
MSGR50   CLI   0(RE),X'FF'        MUST FIND MARKET IN TABLE                     
         BE    MSGR71                                                           
         CLC   INTBTYP,SPBKTYPN                                                 
         BE    *+10                                                             
         AR    RE,RF                                                            
         B     MSGR50                                                           
*                                                                               
         MVC   0(L'SPBKTYPA,R6),SPBKTYPA                                        
         AHI   R6,L'SPBKTYPA                                                    
         B     MSGR80                                                           
*                                                                               
MSGR70   MVC   0(1,R6),=C' '                                                    
MSGR71   AHI   R6,1                                                             
*                                                                               
MSGR80   MVI   0(R6),C','                                                       
         MVC   TEMPBK(2),INTBOOK                                                
         MVI   TEMPBK+2,X'1'                                                    
         GOTO1 VDATCON,DMCB,(3,TEMPBK),(6,1(R6))                                
         AHI   R6,7                                                             
*                                                                               
         L     RF,AMREC                                                         
         SR    R6,RF                                                            
         STC   R6,1(RF)                                                         
         MVI   MSGFLAG,1                                                        
MSGRX    J     EXIT                                                             
*                                                                               
************************************************************                    
FLTMKT   NTR1                                                                   
*                                                                               
         SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         ICM   RF,1,FILTMRKT                                                    
         BZ    FLTMKTX                                                          
         OC    INTMRKT,INTMRKT                                                  
         BZ    FLTMKTX                                                          
         LA    R1,FILTMRKT+1                                                    
*                                                                               
         TM    FLAGS1,NEGATIVE_FILTMRKT                                         
         BZ    FLTMKT10                                                         
FLTMKT05 CLC   INTMRKT,0(R1)                                                    
         BNE   *+12                                                             
         MVI   PASSFLT,C'N'                                                     
         B     FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT05                                                      
         B     FLTMKTX                                                          
*                                                                               
FLTMKT10 CLC   INTMRKT,0(R1)                                                    
         BE    FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT10                                                      
         MVI   PASSFLT,C'N'                                                     
         B     FLTMKTX                                                          
FLTMKTX  J     EXIT                                                             
***********************************************************************         
*        R4  CUNV2INT (UNIVERSE -> INTACCS) DISPLACEMENT TABLE                  
*        R6: INTACCS                                                            
*        R7  UNIVERSES                                                          
***********************************************************************         
* R4 = A(CONVERSION TABLE)                                                      
* R6 = A(START OF INTERIM RECORD DEMO DATA)                                     
* R7 = A(START OF RATING SERVICE DEMO DATA)                                     
***********************************************************************         
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,4                                                             
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MHI   R5,4                ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
         L     RF,0(R5)                                                         
CNVRTOI4 ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MH    R5,=H'4'            ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
CNVRTOI5 ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
         B     CNVRTOI1                                                         
*                                                                               
CNVRTOIX XIT1                                                                   
***********************************************************************         
CALCHHLD NTR1                                                                   
         L     R7,ASREC                                                         
         USING DDSCT,R7                                                         
*****DMA HHLD RATING                                                            
         SR    R0,R0                                                            
         ICM   R1,15,D0144         DHOMES (IMPRESSION)                          
         M     R0,=F'100000'                                                    
         STM   R0,R1,DIVIDEND                                                   
*                                                                               
         ICM   R1,15,D0252         UHOMES (UNIVERSE)                            
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0092,QUOTIENT      RHOMES=DHOMES/UHOMES                         
*****DMA HHLD SHARE                                                             
         ICM   R1,15,D0408         MHOMES (DMA HOMES USING TV)                  
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0112,QUOTIENT      SHOMES=DHOMES/MHOMES                         
*****METROA HHLD RATING                                                         
         SR    R0,R0                                                            
         ICM   R1,15,D0172         DMETROA (IMPRESSION)                         
         M     R0,=F'100000'                                                    
         STM   R0,R1,DIVIDEND                                                   
*                                                                               
         ICM   R1,15,D0280         UMETROA (UNIVERSE)                           
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0124,QUOTIENT      RMETROA=DMETROA/UMETROA                      
*****METROA HHLD SHARE                                                          
         ICM   R1,15,D0424         MMETROA (METROA HOMES USING TV)              
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0120,QUOTIENT      SMETROA=DMETROA/MMETROA                      
*****METROA HUT %                                                               
         SR    R0,R0                                                            
         ICM   R1,15,D0124         RMETROA                                      
         M     R0,=F'100000'                                                    
         STM   R0,R1,DIVIDEND                                                   
*                                                                               
         ICM   R1,15,D0120         SMETROA                                      
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0116,QUOTIENT      PMETROB=RMETROB/SMETROB                      
*****METROB HHLD RATING                                                         
         SR    R0,R0                                                            
         ICM   R1,15,D0176         DMETROB (IMPRESSION)                         
         M     R0,=F'100000'                                                    
         STM   R0,R1,DIVIDEND                                                   
*                                                                               
         ICM   R1,15,D0284         UMETROB (UNIVERSE)                           
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0136,QUOTIENT      RMETROB=DMETROB/UMETROB                      
*****METROB HHLD SHARE                                                          
         ICM   R1,15,D0428         MMETROB (METROB HOMES USING TV)              
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0132,QUOTIENT      SMETROB=DMETROB/MMETROB                      
*****METROB HUT %                                                               
         SR    R0,R0                                                            
         ICM   R1,15,D0136         RMETROB                                      
         M     R0,=F'100000'                                                    
         STM   R0,R1,DIVIDEND                                                   
*                                                                               
         ICM   R1,15,D0132         SMETROB                                      
         ST    R1,DIVISOR                                                       
*                                                                               
         BAS   RE,DIVIDE                                                        
         MVC   D0128,QUOTIENT      PMETROB=RMETROB/SMETROB                      
         XIT1                                                                   
***********************************************************************         
DIVIDE   NTR1                                                                   
         XC    QUOTIENT,QUOTIENT                                                
         XC    REMAINDR,REMAINDR                                                
                                                                                
         ICM   RF,15,DIVISOR       IF DIVISOR IS ZERO,                          
         BZ    DIVIDEX              CALLER GETS ZERO BACK                       
         OC    DIVIDEND,DIVIDEND   IF DIVIDEND IS ZERO                          
         BZ    DIVIDEX              CALLER GETS ZERO BACK ALSO                  
                                                                                
         DS    0H                  CALCULATE QUOTIENT                           
         LM    R0,R1,DIVIDEND                                                   
         SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,QUOTIENT                                                      
                                                                                
         DS    0H                  CALCULATE REMAINDER                          
         AHI   R0,1                                                             
         SRA   R0,1                                                             
         ST    R0,REMAINDR                                                      
                                                                                
DIVIDEX  DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
*                                                                               
SKIP     DS    0H                                                               
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                RETURN TO DEMCNV                             
         LTORG                                                                  
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4                                                                
S1EQU    EQU   8                                                                
OUTMARE  EQU   32                                                               
METROBE  EQU   2                                                                
SPACE    DC    CL30' '                                                          
ZEROS    DC    CL20'0'                                                          
PASSFLT  DC    C'Y'                                                             
PREVKEY  DC    XL20'00'                                                         
TEMPMNO  DS    CL3                                                              
TEMPMKT  DS    CL26                                                             
TEMPBK   DS    XL3                                                              
MYBTYP   DC    X'40'                                                            
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
REMAINDR DS    F                                                                
QUOTIENT DS    F                                                                
SETACCL  DC    X'00'                                                            
UNIQBTY  DS    0XL10                                                            
         DC    X'FF'                                                            
         DS    XL9                                                              
         DC    X'FF'                                                            
MKTKEY   DS    XL18                                                             
MKTKEYSV DS    XL18                                                             
DAYTABL  DS    0CL4                                                             
         DC    C'0',X'00057C'      M-F DAYTIME                                  
         DC    C'1',X'010140'      MON                                          
         DC    C'2',X'020120'      TUE                                          
         DC    C'3',X'030110'      WED                                          
         DC    C'4',X'040108'      THU                                          
         DC    C'5',X'050104'      FRI                                          
         DC    C'6',X'09017C'      M-F OR MULTI-DAY AVERAGE                     
         DC    C'7',X'060102'      SAT                                          
         DC    C'8',X'070101'      SUN                                          
         DC    C'9',X'08077F'      M-S AVERAGE                                  
         DC    X'FF'                                                            
AFFO2N   DS    0CL9                                                             
         DC    CL7'ABC',CL2'A'     ABC NETWORK                                  
         DC    CL7'CBS',CL2'C'     CBS NETWORK                                  
         DC    CL7'FOX',CL2'F'     FOX NETWORK                                  
         DC    CL7'NBC',CL2'N'     NBC NETWORK                                  
         DC    CL7'PAX',CL2'PX'    PAX TV NETWORK                               
         DC    CL7'UPN',CL2'UP'    UPN NETWORK                                  
         DC    CL7'WB',CL2'WB'     THE WB NETWORK                               
         DC    CL7'IND',CL2'I'     INDEPENDENT                                  
         DC    CL7'PBS',CL2'P'     PBS                                          
         DC    CL7'PBS-C',CL2'PC'  PBS COMMERCIAL                               
         DC    CL7'IS',CL2'IS'     INDEPENDENT, SPANISH LANGUAGE                
         DC    CL7'IT',CL2'IT'     TURNER BROADCAST                             
         DC    CL7'TEL',CL2'T'     TELEMUNDO                                    
         DC    CL7'UNI',CL2'U'     UNIVISION                                    
         DC    X'00'                                                            
PSRCO2N  DS    0CL9                                                             
         DC    CL7'ABC',CL2'A'     ABC NETWORK                                  
         DC    CL7'CBS',CL2'C'     CBS NETWORK                                  
         DC    CL7'FOX',CL2'F'     FOX NETWORK                                  
         DC    CL7'L',CL2'L'       LOCAL                                        
         DC    CL7'LM',CL2'LM'     LOCAL MOVIE                                  
         DC    CL7'LN',CL2'LN'     LOCAL NEWS                                   
         DC    CL7'LS',CL2'LS'     LOCAL SPORTS                                 
         DC    CL7'NBC',CL2'N'     NBC NETWORK                                  
         DC    CL7'PAX',CL2'PX'    PAX TV NETWORK                               
         DC    CL7'UPN',CL2'UP'    UPN NETWORK                                  
         DC    CL7'WB',CL2'WB'     THE WB NETWORK                               
         DC    CL7'IND',CL2'I'     INDEPENDENT                                  
         DC    CL7'PBS',CL2'P'     PBS                                          
         DC    CL7'PBS-C',CL2'PC'  PBS COMMERCIAL                               
         DC    CL7'IS',CL2'IS'     INDEPENDENT, SPANISH LANGUAGE                
         DC    CL7'IT',CL2'IT'     TURNER BROADCAST                             
         DC    CL7'TEL',CL2'T'     TELEMUNDO                                    
         DC    CL7'UNI',CL2'U'     UNIVISION                                    
         DC    CL7'SYN',CL2'S'     SYNDICATED                                   
         DC    CL7'CBL',CL2' '     CABLE                                        
         DC    CL7'X  ',CL2'IO'    ION                                          
         DC    CL7'Y  ',CL2'CW'    CW                                           
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
BTYPTAB  DS    0CL5                                                             
*       :KEY:  1: STREAM                                                        
*       :KEY:  2: DATA TYPE                                                     
*       :KEY:  3: BROADCAST OR CABLE                                            
*       :KEY:  4: TOTAL DMA OR WIRED                                            
*       :KEY:  5: ASSIGNED BOOKTYPE                                             
*  STANDARD                                                                     
         DC    C'O',C' ',C' ',C' ',AL1(BOOKTYPE_L)                              
         DC    C'S',C' ',C' ',C' ',AL1(BOOKTYPE_LS)                             
         DC    C'3',C' ',C' ',C' ',AL1(BOOKTYPE_L3)                             
         DC    C'7',C' ',C' ',C' ',AL1(BOOKTYPE_STANDARD)                       
         DC    C'P',C' ',C' ',C' ',AL1(BOOKTYPE_P)                              
         DC    C'1',C' ',C' ',C' ',AL1(BOOKTYPE_L1)                             
*  HISPANIC                                                                     
         DC    C'O',C'H',C' ',C' ',AL1(BOOKTYPE_J)                              
         DC    C'S',C'H',C' ',C' ',AL1(BOOKTYPE_HS)                             
         DC    C'3',C'H',C' ',C' ',AL1(BOOKTYPE_H3)                             
         DC    C'7',C'H',C' ',C' ',AL1(BOOKTYPE_H)                              
         DC    C'P',C'H',C' ',C' ',AL1(BOOKTYPE_I)                              
         DC    C'1',C'H',C' ',C' ',AL1(BOOKTYPE_H1)                             
*                                                                               
*  TRADE                                                                        
         DC    C'7',C'T',C' ',C' ',AL1(BOOKTYPE_T)                              
         DC    C'7',C'T',C'C',C' ',AL1(BOOKTYPE_T)                              
*  METRO                                                                        
         DC    C'7',C'M',C' ',C' ',AL1(BOOKTYPE_M)                              
         DC    C'7',C'M',C'C',C' ',AL1(BOOKTYPE_M)                              
*  EXCLUSIONS                                                                   
         DC    C'7',C'X',C' ',C' ',AL1(BOOKTYPE_X)                              
*  OLYMPIC EXCLUSIONS                                                           
         DC    C'O',C'O',C' ',C' ',AL1(BOOKTYPE_OL)                             
         DC    C'S',C'O',C' ',C' ',AL1(BOOKTYPE_OS)                             
         DC    C'3',C'O',C' ',C' ',AL1(BOOKTYPE_O3)                             
         DC    C'7',C'O',C' ',C' ',AL1(BOOKTYPE_O)                              
         DC    C'1',C'O',C' ',C' ',AL1(BOOKTYPE_O1)                             
*  HYBRID  (EXTENDED) (AKA "IMPACT")                                            
         DC    C'O',C'E',C' ',C' ',AL1(BOOKTYPE_YL)                             
         DC    C'S',C'E',C' ',C' ',AL1(BOOKTYPE_YS)                             
         DC    C'3',C'E',C' ',C' ',AL1(BOOKTYPE_Y3)                             
         DC    C'7',C'E',C' ',C' ',AL1(BOOKTYPE_Y7)                             
         DC    C'1',C'E',C' ',C' ',AL1(BOOKTYPE_Y1)                             
*  HYBRID (EXTENDED) HISPANIC (AKA "IMPACT")                                    
         DC    C'O',C'D',C' ',C' ',AL1(BOOKTYPE_YJ)                             
         DC    C'S',C'D',C' ',C' ',AL1(BOOKTYPE_YE)                             
         DC    C'3',C'D',C' ',C' ',AL1(BOOKTYPE_YF)                             
         DC    C'7',C'D',C' ',C' ',AL1(BOOKTYPE_YH)                             
         DC    C'1',C'D',C' ',C' ',AL1(BOOKTYPE_YN)                             
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
* TABLE TO CONVERT UNIVERSES FROM NSI TO DDS INTREC (M2 RECORD)                 
CUNV2INT DS    0C                                                               
         DC    AL1(RRMETROA,OUMETROA)                                           
         DC    AL1(RRMETROB,OUMETROB)                                           
         DC    AL1(RRHOMES,OUHOMES)                                             
         DC    AL1(RRV25,OUV25)                                                 
         DC    AL1(RRV611,OUV611)                                               
         DC    AL1(RRM1217,OUM1217)                                             
         DC    AL1(RRW1217,OUW1217)                                             
         DC    AL1(RRM1820,OUM1820)                                             
         DC    AL1(RRM2124,OUM2124)                                             
         DC    AL1(RRM2534,OUM2534)                                             
         DC    AL1(RRM3549,OUM3549)                                             
         DC    AL1(RRM5054,OUM5054)                                             
         DC    AL1(RRM5564,OUM5564)                                             
         DC    AL1(RRM65O,OUM65O)                                               
         DC    AL1(RRW1820,OUW1820)                                             
         DC    AL1(RRW2124,OUW2124)                                             
         DC    AL1(RRW2534,OUW2534)                                             
         DC    AL1(RRW3549,OUW3549)                                             
         DC    AL1(RRW5054,OUW5054)                                             
         DC    AL1(RRW5564,OUW5564)                                             
         DC    AL1(RRW65O,OUW65O)                                               
         DC    AL1(RRWWRK,OUWWRK)                                               
         DC    X'FF'                                                            
CDMA2INT DS    0C                                                               
         DC    AL1(RRMETROA,ODMETROA)                                           
         DC    AL1(RRMETROB,ODMETROB)                                           
         DC    AL1(RRHOMES,ODHOMES)                                             
         DC    AL1(RRV25,ODV25)                                                 
         DC    AL1(RRV611,ODV611)                                               
         DC    AL1(RRM1217,ODM1217)                                             
         DC    AL1(RRW1217,ODW1217)                                             
         DC    AL1(RRM1820,ODM1820)                                             
         DC    AL1(RRM2124,ODM2124)                                             
         DC    AL1(RRM2534,ODM2534)                                             
         DC    AL1(RRM3549,ODM3549)                                             
         DC    AL1(RRM5054,ODM5054)                                             
         DC    AL1(RRM5564,ODM5564)                                             
         DC    AL1(RRM65O,ODM65O)                                               
         DC    AL1(RRW1820,ODW1820)                                             
         DC    AL1(RRW2124,ODW2124)                                             
         DC    AL1(RRW2534,ODW2534)                                             
         DC    AL1(RRW3549,ODW3549)                                             
         DC    AL1(RRW5054,ODW5054)                                             
         DC    AL1(RRW5564,ODW5564)                                             
         DC    AL1(RRW65O,ODW65O)                                               
         DC    AL1(RRWWRK,ODWWRK)                                               
         DC    X'FF'                                                            
CTSA2INT DS    0C                                                               
         DC    AL1(RTHOMES,OTHOMES)                                             
         DC    AL1(RTV25,OTV25)                                                 
         DC    AL1(RTV611,OTV611)                                               
         DC    AL1(RTM1217,OTM1217)                                             
         DC    AL1(RTW1217,OTW1217)                                             
         DC    AL1(RTM1820,OTM1820)                                             
         DC    AL1(RTM2124,OTM2124)                                             
         DC    AL1(RTM2534,OTM2534)                                             
         DC    AL1(RTM3549,OTM3549)                                             
         DC    AL1(RTM5054,OTM5054)                                             
         DC    AL1(RTM5564,OTM5564)                                             
         DC    AL1(RTM65O,OTM65O)                                               
         DC    AL1(RTW1820,OTW1820)                                             
         DC    AL1(RTW2124,OTW2124)                                             
         DC    AL1(RTW2534,OTW2534)                                             
         DC    AL1(RTW3549,OTW3549)                                             
         DC    AL1(RTW5054,OTW5054)                                             
         DC    AL1(RTW5564,OTW5564)                                             
         DC    AL1(RTW65O,OTW65O)                                               
         DC    AL1(RTWWRK,OTWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
CHPT2INT DS    0C                                                               
         DC    AL1(RRMETROA,OMMETROA)                                           
         DC    AL1(RRMETROB,OMMETROB)                                           
         DC    AL1(RRHOMES,OMHOMES)                                             
         DC    AL1(RRV25,OMV25)                                                 
         DC    AL1(RRV611,OMV611)                                               
         DC    AL1(RRM1217,OMM1217)                                             
         DC    AL1(RRW1217,OMW1217)                                             
         DC    AL1(RRM1820,OMM1820)                                             
         DC    AL1(RRM2124,OMM2124)                                             
         DC    AL1(RRM2534,OMM2534)                                             
         DC    AL1(RRM3549,OMM3549)                                             
         DC    AL1(RRM5054,OMM5054)                                             
         DC    AL1(RRM5564,OMM5564)                                             
         DC    AL1(RRM65O,OMM65O)                                               
         DC    AL1(RRW1820,OMW1820)                                             
         DC    AL1(RRW2124,OMW2124)                                             
         DC    AL1(RRW2534,OMW2534)                                             
         DC    AL1(RRW3549,OMW3549)                                             
         DC    AL1(RRW5054,OMW5054)                                             
         DC    AL1(RRW5564,OMW5564)                                             
         DC    AL1(RRW65O,OMW65O)                                               
         DC    AL1(RRWWRK,OMWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
CTOT2INT DS    0C                                                               
         DC    AL1(RTHOMES,OQHOMES)                                             
         DC    AL1(RTV25,OQV25)                                                 
         DC    AL1(RTV611,OQV611)                                               
         DC    AL1(RTM1217,OQM1217)                                             
         DC    AL1(RTW1217,OQW1217)                                             
         DC    AL1(RTM1820,OQM1820)                                             
         DC    AL1(RTM2124,OQM2124)                                             
         DC    AL1(RTM2534,OQM2534)                                             
         DC    AL1(RTM3549,OQM3549)                                             
         DC    AL1(RTM5054,OQM5054)                                             
         DC    AL1(RTM5564,OQM5564)                                             
         DC    AL1(RTM65O,OQM65O)                                               
         DC    AL1(RTW1820,OQW1820)                                             
         DC    AL1(RTW2124,OQW2124)                                             
         DC    AL1(RTW2534,OQW2534)                                             
         DC    AL1(RTW3549,OQW3549)                                             
         DC    AL1(RTW5054,OQW5054)                                             
         DC    AL1(RTW5564,OQW5564)                                             
         DC    AL1(RTW65O,OQW65O)                                               
         DC    AL1(RTWWRK,OQWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
         EJECT                                                                  
* INTERIM RECORD DISPLACEMENTS                                                  
ORHOMES  EQU   1                                                                
ORHH1    EQU   2                                                                
ORHH2    EQU   3                                                                
ORHH3    EQU   4                                                                
ORHH4    EQU   5                                                                
OSHOMES  EQU   6                                                                
OPMETROA EQU   7                                                                
OSMETROA EQU   8                                                                
ORMETROA EQU   9                                                                
OPMETROB EQU   10                                                               
OSMETROB EQU   11                                                               
ORMETROB EQU   12                                                               
OTHOMES  EQU   13                                                               
ODHOMES  EQU   14                                                               
OTWWRK   EQU   15                                                               
ODWWRK   EQU   16                                                               
OTW65O   EQU   17                                                               
ODW65O   EQU   18                                                               
OTM65O   EQU   19                                                               
ODM65O   EQU   20                                                               
ODMETROA EQU   21                                                               
ODMETROB EQU   22                                                               
OTM1217  EQU   23                                                               
OTW1217  EQU   24                                                               
OTM1820  EQU   25                                                               
OTW1820  EQU   26                                                               
OTM2124  EQU   27                                                               
OTW2124  EQU   28                                                               
OTM2534  EQU   29                                                               
OTW2534  EQU   30                                                               
OTM3549  EQU   31                                                               
OTW3549  EQU   32                                                               
OTM5564  EQU   33                                                               
OTW5564  EQU   34                                                               
OTM5054  EQU   35                                                               
OTW5054  EQU   36                                                               
OTV25    EQU   37                                                               
ODV25    EQU   38                                                               
OTV611   EQU   39                                                               
ODV611   EQU   40                                                               
OUHOMES  EQU   41                                                               
OUM65O   EQU   42                                                               
OUW65O   EQU   43                                                               
OUWWRK   EQU   44                                                               
OUA1HH   EQU   45                                                               
OUA2HH   EQU   46                                                               
OUA3HH   EQU   47                                                               
OUMETROA EQU   48                                                               
OUMETROB EQU   49                                                               
OUM1217  EQU   50                                                               
OUW1217  EQU   51                                                               
OUM1820  EQU   52                                                               
OUW1820  EQU   53                                                               
OUM2124  EQU   54                                                               
OUW2124  EQU   55                                                               
OUM2534  EQU   56                                                               
OUW2534  EQU   57                                                               
OUM3549  EQU   58                                                               
OUW3549  EQU   59                                                               
OUM5564  EQU   60                                                               
OUW5564  EQU   61                                                               
OUM5054  EQU   62                                                               
OUW5054  EQU   63                                                               
OUV25    EQU   64                                                               
OUV611   EQU   65                                                               
ODM1217  EQU   66                                                               
ODW1217  EQU   67                                                               
ODM1820  EQU   68                                                               
ODW1820  EQU   69                                                               
ODM2124  EQU   70                                                               
ODW2124  EQU   71                                                               
ODM2534  EQU   72                                                               
ODW2534  EQU   73                                                               
ODM3549  EQU   74                                                               
ODW3549  EQU   75                                                               
ODM5564  EQU   76                                                               
ODW5564  EQU   77                                                               
ODM5054  EQU   78                                                               
ODW5054  EQU   79                                                               
OMHOMES  EQU   80                                                               
OMWWRK   EQU   81                                                               
OMW65O   EQU   82                                                               
OMM65O   EQU   83                                                               
OMMETROA EQU   84                                                               
OMMETROB EQU   85                                                               
OMM1217  EQU   86                                                               
OMW1217  EQU   87                                                               
OMM1820  EQU   88                                                               
OMW1820  EQU   89                                                               
OMM2124  EQU   90                                                               
OMW2124  EQU   91                                                               
OMM2534  EQU   92                                                               
OMW2534  EQU   93                                                               
OMM3549  EQU   94                                                               
OMW3549  EQU   95                                                               
OMM5564  EQU   96                                                               
OMW5564  EQU   97                                                               
OMM5054  EQU   98                                                               
OMW5054  EQU   99                                                               
OMV25    EQU   100                                                              
OMV611   EQU   101                                                              
OQHOMES  EQU   102                                                              
OQWWRK   EQU   103                                                              
OQW65O   EQU   104                                                              
OQM65O   EQU   105                                                              
OQM1217  EQU   106                                                              
OQW1217  EQU   107                                                              
OQM1820  EQU   108                                                              
OQW1820  EQU   109                                                              
OQM2124  EQU   110                                                              
OQW2124  EQU   111                                                              
OQM2534  EQU   112                                                              
OQW2534  EQU   113                                                              
OQM3549  EQU   114                                                              
OQW3549  EQU   115                                                              
OQM5564  EQU   116                                                              
OQW5564  EQU   117                                                              
OQM5054  EQU   118                                                              
OQW5054  EQU   119                                                              
OQV25    EQU   120                                                              
OQV611   EQU   121                                                              
ORV2O    EQU   122                                                              
ORV18O   EQU   123                                                              
ORV1234  EQU   124                                                              
ORV1224  EQU   125                                                              
ORV1217  EQU   126                                                              
ORV611   EQU   127                                                              
ORV211   EQU   128                                                              
ORW18O   EQU   129                                                              
ORW1834  EQU   130                                                              
ORW1849  EQU   131                                                              
ORW2549  EQU   132                                                              
ORW2554  EQU   133                                                              
ORW1224  EQU   134                                                              
ORW2564  EQU   135                                                              
ORW1234  EQU   136                                                              
ORM18O   EQU   137                                                              
ORM1834  EQU   138                                                              
ORM1849  EQU   139                                                              
ORM2554  EQU   140                                                              
ORM2564  EQU   141                                                              
ORWWRK   EQU   142                                                              
ORM2549  EQU   143                                                              
ORA1849  EQU   144                                                              
ORA1834  EQU   145                                                              
ORA2554  EQU   146                                                              
OPV2O    EQU   147                                                              
OPV18O   EQU   148                                                              
OPV1234  EQU   149                                                              
OPV1224  EQU   150                                                              
OPV1217  EQU   151                                                              
OPV611   EQU   152                                                              
OPV211   EQU   153                                                              
OPW18O   EQU   154                                                              
OPW1834  EQU   155                                                              
OPW1849  EQU   156                                                              
OPW2549  EQU   157                                                              
OPW2554  EQU   158                                                              
OPW1224  EQU   159                                                              
OPW2564  EQU   160                                                              
OPW1234  EQU   161                                                              
OPM18O   EQU   162                                                              
OPM1834  EQU   163                                                              
OPM1849  EQU   164                                                              
OPM2554  EQU   165                                                              
OPM2564  EQU   166                                                              
OPWWRK   EQU   167                                                              
OPM2549  EQU   168                                                              
OPA1849  EQU   169                                                              
OPA1834  EQU   170                                                              
OPA2554  EQU   171                                                              
         EJECT                                                                  
* FIELD DISPLACEMENTS FOR RATING SERVICE RECORDS                                
RRMETROA EQU   1                                                                
RRMETROB EQU   2                                                                
RRHOMES  EQU   3                                                                
RRV25    EQU   4                                                                
RRV611   EQU   5                                                                
RRM1217  EQU   6                                                                
*              7            ORIGINALLY 1517, BLANKED OUT                        
RRM1820  EQU   8                                                                
RRM2124  EQU   9                                                                
RRM2534  EQU   10                                                               
RRM3549  EQU   11                                                               
RRM5054  EQU   12                                                               
RRM5564  EQU   13                                                               
RRM65O   EQU   14                                                               
RRW1217  EQU   15                                                               
*              16           ORIGINALLY 1517, BLANKED OUT                        
RRW1820  EQU   17                                                               
RRW2124  EQU   18                                                               
RRW2534  EQU   19                                                               
RRW3549  EQU   20                                                               
RRW5054  EQU   21                                                               
RRW5564  EQU   22                                                               
RRW65O   EQU   23                                                               
RRWWRK   EQU   24                                                               
*                                                                               
* FIELD DISPLACEMENTS FOR STATION TOTAL RECORDS                                 
RTHOMES  EQU   1                                                                
RTV25    EQU   2                                                                
RTV611   EQU   3                                                                
RTM1217  EQU   4                                                                
*              5            ORIGINALLY 1517, BLANKED OUT                        
RTM1820  EQU   6                                                                
RTM2124  EQU   7                                                                
RTM2534  EQU   8                                                                
RTM3549  EQU   9                                                                
RTM5054  EQU   10                                                               
RTM5564  EQU   11                                                               
RTM65O   EQU   12                                                               
RTW1217  EQU   13                                                               
*              14           ORIGINALLY 1517, BLANKED OUT                        
RTW1820  EQU   15                                                               
RTW2124  EQU   16                                                               
RTW2534  EQU   17                                                               
RTW3549  EQU   18                                                               
RTW5054  EQU   19                                                               
RTW5564  EQU   20                                                               
RTW65O   EQU   21                                                               
RTWWRK   EQU   22                                                               
*                                                                               
RRECL    EQU   W_LMRECLQ                                                        
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DELMDSECT                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DELMPINTD                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
DDSCT    DSECT                     DSECT TO THE DEMO#'S IN INTACCS              
D0000    DS    0C                                                               
         ORG   *+INTACCQ                                                        
D0092    DS    CL0004              RHOMES                                       
D0096    DS    CL0004              RHH1                                         
D0100    DS    CL0004              RHH2                                         
D0104    DS    CL0004              RHH3                                         
D0108    DS    CL0004              RHH4                                         
D0112    DS    CL0004              SHOMES                                       
D0116    DS    CL0004              PMETROA                                      
D0120    DS    CL0004              SMETROA                                      
D0124    DS    CL0004              RMETROA                                      
D0128    DS    CL0004              PMETROB                                      
D0132    DS    CL0004              SMETROB                                      
D0136    DS    CL0004              RMETROB                                      
D0140    DS    CL0004              THOMES                                       
D0144    DS    CL0004              DHOMES                                       
D0148    DS    CL0004              TWWRK                                        
D0152    DS    CL0004              DWWRK                                        
D0156    DS    CL0004              TW65+                                        
D0160    DS    CL0004              DW65+                                        
D0164    DS    CL0004              TM65+                                        
D0168    DS    CL0004              DM65+                                        
D0172    DS    CL0004              DMETROA                                      
D0176    DS    CL0004              DMETROB                                      
D0180    DS    CL0004              TM1217                                       
D0184    DS    CL0004              TW1217                                       
D0188    DS    CL0004              TM1820                                       
D0192    DS    CL0004              TW1820                                       
D0196    DS    CL0004              TM2124                                       
D0200    DS    CL0004              TW2124                                       
D0204    DS    CL0004              TM2534                                       
D0208    DS    CL0004              TW2534                                       
D0212    DS    CL0004              TM3549                                       
D0216    DS    CL0004              TW3549                                       
D0220    DS    CL0004              TM5564                                       
D0224    DS    CL0004              TW5564                                       
D0228    DS    CL0004              TM5054                                       
D0232    DS    CL0004              TW5054                                       
D0236    DS    CL0004              TV25                                         
D0240    DS    CL0004              DV25                                         
D0244    DS    CL0004              TV611                                        
D0248    DS    CL0004              DV611                                        
D0252    DS    CL0004              UHOMES                                       
D0256    DS    CL0004              UM65+                                        
D0260    DS    CL0004              UW65+                                        
D0264    DS    CL0004              UWWRK                                        
D0268    DS    CL0004              UA1HH                                        
D0272    DS    CL0004              UA2HH                                        
D0276    DS    CL0004              UA3HH                                        
D0280    DS    CL0004              UMETROA                                      
D0284    DS    CL0004              UMETROB                                      
D0288    DS    CL0004              UM1217                                       
D0292    DS    CL0004              UW1217                                       
D0296    DS    CL0004              UM1820                                       
D0300    DS    CL0004              UW1820                                       
D0304    DS    CL0004              UM2124                                       
D0308    DS    CL0004              UW2124                                       
D0312    DS    CL0004              UM2534                                       
D0316    DS    CL0004              UW2534                                       
D0320    DS    CL0004              UM3549                                       
D0324    DS    CL0004              UW3549                                       
D0328    DS    CL0004              UM5564                                       
D0332    DS    CL0004              UW5564                                       
D0336    DS    CL0004              UM5054                                       
D0340    DS    CL0004              UW5054                                       
D0344    DS    CL0004              UV25                                         
D0348    DS    CL0004              UV611                                        
D0352    DS    CL0004              DM1217                                       
D0356    DS    CL0004              DW1217                                       
D0360    DS    CL0004              DM1820                                       
D0364    DS    CL0004              DW1820                                       
D0368    DS    CL0004              DM2124                                       
D0372    DS    CL0004              DW2124                                       
D0376    DS    CL0004              DM2534                                       
D0380    DS    CL0004              DW2534                                       
D0384    DS    CL0004              DM3549                                       
D0388    DS    CL0004              DW3549                                       
D0392    DS    CL0004              DM5564                                       
D0396    DS    CL0004              DW5564                                       
D0400    DS    CL0004              DM5054                                       
D0404    DS    CL0004              DW5054                                       
D0408    DS    CL0004              MHOMES                                       
D0412    DS    CL0004              MWWRK                                        
D0416    DS    CL0004              MW65+                                        
D0420    DS    CL0004              MM65+                                        
D0424    DS    CL0004              MMETROA                                      
D0428    DS    CL0004              MMETROB                                      
D0432    DS    CL0004              MM1217                                       
D0436    DS    CL0004              MW1217                                       
D0440    DS    CL0004              MM1820                                       
D0444    DS    CL0004              MW1820                                       
D0448    DS    CL0004              MM2124                                       
D0452    DS    CL0004              MW2124                                       
D0456    DS    CL0004              MM2534                                       
D0460    DS    CL0004              MW2534                                       
D0464    DS    CL0004              MM3549                                       
D0468    DS    CL0004              MW3549                                       
D0472    DS    CL0004              MM5564                                       
D0476    DS    CL0004              MW5564                                       
D0480    DS    CL0004              MM5054                                       
D0484    DS    CL0004              MW5054                                       
D0488    DS    CL0004              MV25                                         
D0492    DS    CL0004              MV611                                        
D0496    DS    CL0004              QHOMES                                       
D0500    DS    CL0004              QWWRK                                        
D0504    DS    CL0004              QW65+                                        
D0508    DS    CL0004              QM65+                                        
D0512    DS    CL0004              QM1217                                       
D0516    DS    CL0004              QW1217                                       
D0520    DS    CL0004              QM1820                                       
D0524    DS    CL0004              QW1820                                       
D0528    DS    CL0004              QM2124                                       
D0532    DS    CL0004              QW2124                                       
D0536    DS    CL0004              QM2534                                       
D0540    DS    CL0004              QW2534                                       
D0544    DS    CL0004              QM3549                                       
D0548    DS    CL0004              QW3549                                       
D0552    DS    CL0004              QM5564                                       
D0556    DS    CL0004              QW5564                                       
D0560    DS    CL0004              QM5054                                       
D0564    DS    CL0004              QW5054                                       
D0568    DS    CL0004              QV25                                         
D0572    DS    CL0004              QV211                                        
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015DELMPAI   07/31/18'                                      
         END                                                                    
