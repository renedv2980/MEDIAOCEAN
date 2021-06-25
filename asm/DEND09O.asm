*          DATA SET DEND09O    AT LEVEL 050 AS OF 05/28/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEND09OA                                                                 
*INCLUDE DEBKUPDT                                                               
         TITLE 'OUTPUT PHASE FOR DEND09O NTI CNV'                               
DEND09O  CSECT                                                                  
NUMCATS  EQU   38                                                               
         PRINT NOGEN                                                            
         NMOD1 NTIWRKX-NTIWRKD,DEND09O,R7                                       
         USING NTIWRKD,RC          RC=A(TEMP W/S)                               
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
*                                                                               
         B     *+4(R1)                                                          
         B     CNV20               PROCESS A RECORD                             
         B     CNV300              LAST TIME HOOK (PROCESS YEARLY HUT)          
*                                                                               
CNV20    OC    INTKEY,INTKEY       UNUSABLE RECORD                              
         BZ    CNVX                                                             
*                                                                               
         CLI   FRSTREC,YES         FIRST RECORD TO OPHASE                       
         BNE   CNV25                                                            
         TM    FLAGS1,CREATE_NETCTREC     NETCTREC=Y                            
         BNO   CNV25                                                            
         GOTO1 =V(DEBKUPDT),DMCB,,INITQ   INITIALIZE DEBKUPDT                   
*                                                                               
CNV25    MVI   FRSTREC,NO                                                       
*                                                                               
         CLC   INTBOOK,=AL1(YR_1991,WEEK_51)  STUPID BLACK WEEK SHIT            
         BNE   *+10                                                             
         MVC   INTIBOOK,=AL1(YR_1991,WEEK_33)                                   
*                                                                               
         CLI   INTRTYP,PRCODEQU    PAV TIME PERIOD RECORD (P)                   
         BE    CNV30                                                            
         CLI   INTRTYP,PMCODEQU    NETWORK PROGRAM RECORD (Q)                   
         BE    CNV30                                                            
*                                  (DROP RECORD IF NOT -P- OR -Q-)              
CNVX     XMOD1 1                                                                
         SPACE 1                                                                
* DETERMINE IF CORRECTION RECORDS MUST BE PROCESSED                             
*                                                                               
CNV30    CLI   INTKEY+2,C'P'       TEST CORRECTION RECORD                       
         BE    CNV32                                                            
         CLI   INTKEY+2,C'Q'       TEST CORRECTION RECORD                       
         BNE   CNV38                                                            
*                                  CORRECTION RECORDS                           
CNV32    XC    PREVKEY,PREVKEY     DISABLE PREVKEY LOGIC                        
         XC    PREVSQH,PREVSQH                                                  
         XC    PREVDW,PREVDW                                                    
         OC    PREVCORR,PREVCORR   TEST 1ST-TIME-THRU                           
         BZ    CNV36                                                            
         CLI   INTKEY+2,C'P'                                                    
         BNE   *+14                                                             
         CLC   PREVCORR(PRBTYP-PRKEY+1),INTKEY  P-KEY THRU BKTYPE               
         B     *+10                                                             
         CLC   PREVCORR(PMBTYP-PMKEY+1),INTKEY  Q-KEY THRU BKTYPE               
         BE    CNV40                                                            
         CLI   EOFSW,1             EOF ON FILE ALREADY                          
         BE    CNV36               DON'T OUTPUT                                 
         GOTO1 PUTTAPE,4           PUT REMAINDER OF PREVIOUS CORR BOOK          
*                                                                               
CNV36    DS    0H                                                               
         XC    PREVCORR,PREVCORR                                                
         CLI   INTKEY+2,C'P'                                                    
         BNE   *+14                                                             
         MVC   PREVCORR(PRBTYP-PRKEY+1),INTKEY  P-KEY THRU BKTYPE               
         B     *+10                                                             
         MVC   PREVCORR(PMBTYP-PMKEY+1),INTKEY  Q-KEY THRU BKTYPE               
         MVI   EOFSW,0             RESET EOF ON FILE                            
         B     CNV40                                                            
*                                  REGULAR RECORDS                              
CNV38    OC    BOOKSW,BOOKSW       TEST PREVIOUS CORRECTION BOOK                
         BZ    CNV40                                                            
         CLI   EOFSW,1             EOF ON FILE ALREADY                          
         BE    CNV39               DON'T OUTPUT                                 
         GOTO1 PUTTAPE,4           PUT REMAINDER OF PREVIOUS CORR BOOK          
*                                                                               
CNV39    XC    BOOKSW,BOOKSW                                                    
         MVI   EOFSW,0                                                          
         XC    PREVKEY,PREVKEY     RESET PREVKEY                                
         XC    PREVSQH,PREVSQH                                                  
         XC    PREVDW,PREVDW                                                    
*                                                                               
         EJECT                                                                  
* BUILD DEMOGRAPHIC RECORDS                                                     
*                                                                               
CNV40    MVI   PRINTSW,0                                                        
         LA    R6,THISKEY                                                       
         XC    THISKEY,THISKEY                                                  
         MVC   TAPE,INTSTA+4       USED TO DETERMINE IF DAILIES TAPE            
*                                                                               
         CLI   INTRTYP,PMCODEQU    TEST FOR NETWORK PROGRAM RECORD (Q)          
         BE    CNV60                                                            
         SPACE 3                                                                
*                                  ************* -P- ONLY ***********           
         CLI   PREVKEY,PRCODEQU    WAS PREVIOUS RECORD PAV (P)                  
         BE    CNV44                                                            
         XC    PREVKEY,PREVKEY                                                  
         XC    PREVSQH,PREVSQH                                                  
         XC    PREVDW,PREVDW                                                    
*                                                                               
         USING PRKEY,R6                                                         
CNV44    MVI   PRCODE,PRCODEQU     BUILD PAV KEY (P)                            
         MVC   PRMEDIA,MEDIA                                                    
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,BOOKTYPE     SET TYPE OF DATA                             
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         CLC   PREVKEY(3),PRKEY    SAME FILE/MEDIA/SOURCE?                      
         BNE   CNV48               NO. DON'T BUMP DAY/TIME                      
         CLC   PREVKEY,PRKEY       TEST LAST KEY AGAINST CURRENT KEY            
         BL    CNV48               IF EQUAL OR HIGH, FORCE CURRENT INTO         
         LA    RE,PREVKEY          SEQUENCE BY BUMPING DAY/WEEK BYTE            
         ZIC   R1,PRDW-PRKEY(RE)   OF LAST KEY AND USING IT IN                  
         LA    R1,1(R1)            CURRENT KEY                                  
         STC   R1,PRDW                                                          
*                                                                               
CNV48    MVC   THISSQH,PRSTIM                                                   
         MVC   THISDW,PRDW                                                      
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         B     CNV70                                                            
         DROP  R6                                                               
*                                                                               
         SPACE 3                                                                
*                                  ************* -Q- ONLY ***********           
         USING PMKEY,R6            BUILD NETWORK PROGRAM KEY (Q)                
CNV60    MVI   PMCODE,PMCODEQU                                                  
         MVC   PMMEDIA,MEDIA                                                    
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PMBTYP,BOOKTYPE                                                  
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
         GOTO1 ABLDREC,DMCB,PMKEY                                               
         B     CNV70                                                            
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                  ************* -P- & -Q- **********           
CNV70    LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET TYPE ELEMENT                    
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVI   MARSTYP,2           CHANGED FROM INTSTYP                         
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         CLC   INTBOOK,=AL2(JAN_06)   AFTER JAN/06,                             
         BL    CNV76                                                            
         USING DTELEM,R6           BUILD DEMO DATA TYPE ELEMENT                 
         XC    TEMP,TEMP                                                        
         MVI   DTCODE,DTCODEQ      X'08'                                        
*                                                                               
         ICM   RF,15,CDEMTABS                                                   
         GOTO1 (RF),DMCB,VWDESCTB                                               
         ICM   R1,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,DMCB+4           LENGTH OF TABLE ENTRY                        
*                                                                               
         USING VWDESCD,R1                                                       
CNV72    CLC   INTKSRC,VWDSRC                                                   
         BE    CNV75                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     CNV72                                                            
*                                                                               
CNV75    LLC   RE,VWDLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DTTYPE(0),VWDSCP                                                 
         DROP  R1                                                               
*                                                                               
         AHI   RE,DTLENQ+1         DESC LENGTH + ELEM LEN + 1 FOR EX            
         STC   RE,DTLEN                                                         
         GOTO1 APUTEL,DTELEM                                                    
         DROP  R6                                                               
*                                                                               
         USING PHTELEM,R6          BUILD NETWORK PROGRAM TYPE ELEMENT           
CNV76    XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHTCODE,PHTCODEQ                                                 
         MVI   PHTLEN,PHTLNEQ                                                   
         MVC   PHTDTYP,INTDTYP                                                  
         MVC   PHTPTYP,INTPTYP                                                  
         MVC   PHTDPT,INTDPT       SET TO ZERO BY I/P PHASE                     
         MVC   PHTPREM,INTPREM                                                  
         MVC   PHTRCH,INTAUD                                                    
         MVC   PHTSCNT,INTSTAC                                                  
         MVC   PHTCOVR,INTCOV                                                   
         MVC   PHTRSF,INTRSF                                                    
         MVC   PHTDPT2,INTDPT2     2-BYTE DAYPART                               
         MVC   PHTNTI,INTPNTI                                                   
         GOTO1 APUTEL,PHTELEM                                                   
         DROP  R6                                                               
*                                                                               
         USING PHELEM,R6           BUILD DAY/QTR HOUR ELEMENT                   
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHCODE,PHCODEQ                                                   
         MVI   PHELN,PHELNEQ                                                    
         MVC   PHPNUM,INTPNUM                                                   
         MVC   PHDUR,INTDUR        QUARTER HOUR DURATION                        
         CLI   INTRTYP,PRCODEQU    TEST FOR PAV RECORD (P)                      
         BNE   *+16                                                             
         MVC   PHPRVST,PREVSQH     INSERT PREVIOUS VALUES                       
         MVC   PHPRVDW,PREVDW                                                   
         MVC   PHDTYPE,INTDTYP                                                  
         OI    PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,INTBOOK                                                  
         MVC   PHDURTOT,INTADUR    DUR IN MIN PRG RAN W/IN THIS 1/2HR           
         GOTO1 APUTEL,PHELEM                                                    
         DROP  R6                                                               
*                                                                               
         USING PPNELEM,R6          BUILD PROGRAM NAME ELEMENT                   
         MVI   PPNCODE,PPNCODEQ                                                 
         MVC   PPNNME(L'INTPNAME),INTPNAME                                      
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
         EJECT                                                                  
         CLI   INTRTYP,PMCODEQU    TEST FOR NETWORK PROGRAM RECORD (Q)          
         BNE   CNV100                                                           
*                                  ************* -Q- ONLY ***********           
         MVC   THISSQH,INTSQH                                                   
         MVC   THISDW,INTDAYWK                                                  
*                                  BUILD NETWORK PROG RUN/TIME ELEM             
         USING NTELEM,R6                                                        
         XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ                                                    
         MVC   NTSQH,INTSQH                                                     
         MVC   NTEQH,INTEQH                                                     
         MVC   NTDUR,INTDURM       DURATION IN MINUTES                          
*                                                                               
         ZIC   R0,INTDURM          IF 2-BYTE DURATION DIFFERENT THAN            
         SR    R1,R1               THE 1-BYTE VALUE,                            
         ICM   R1,3,INTDURM2                                                    
         CR    R0,R1                                                            
         BE    *+14                                                             
         MVC   NTDUR2,INTDURM2     ADD LONG DURATION TO ELEMENT                 
         MVI   NTLEN,NTLENEQ2                                                   
*                                                                               
         MVC   NTSTIM,INTSTIM                                                   
         MVC   NTETIM,INTETIM                                                   
         MVC   NTDAY,INTDYBIT      DAY CODE'S BIT SETTING                       
         GOTO1 APUTEL,NTELEM                                                    
*                                  BUILD VAR RUN/TIME ELEMENTS                  
         CLI   INTDAYWK,X'00'      M-F                                          
         BE    *+20                                                             
         CLI   INTDAYWK,X'80'      M-S                                          
         BE    *+12                                                             
         CLI   INTDAYWK,X'90'      VAR                                          
         BNE   CNV100                                                           
         LA    R5,INTVAR                                                        
         USING INTVARD,R5                                                       
         LA    R2,7                7 DAYS                                       
         LA    R3,X'40'            START WITH MONDAY                            
CNV80    CLI   0(R5),0                                                          
         BE    CNV90                                                            
         XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ                                                    
         MVC   NTSQH,INTVSQ        START QH                                     
         MVC   NTEQH,INTVEQ        END QH                                       
         MVC   NTDUR,INTVSDUR      DURATION IN MINUTES                          
*                                                                               
         ZIC   R0,INTVSDUR         IF 2-BYTE DURATION DIFFERENTS THAN           
         SR    R1,R1               THE 1-BYTE VALUE,                            
         ICM   R1,3,INTVLDUR                                                    
         CR    R0,R1                                                            
         BE    *+14                                                             
         MVC   NTDUR2,INTVLDUR     ADD LONG DURATION TO ELEMENT                 
         MVI   NTLEN,NTLENEQ2                                                   
*                                                                               
         MVC   NTSTIM,INTVSTIM     START TIME                                   
         MVC   NTETIM,INTVETIM     END TIME                                     
         STC   R3,NTDAY            INDIVIDUAL DAY CODE                          
         GOTO1 APUTEL,NTELEM                                                    
         DROP  R6                                                               
*                                                                               
CNV90    LA    R5,INTVARLQ(R5)     NEXT DAY IN INTVAR                           
         SRL   R3,1                NEXT INDIVIDUAL DAY                          
         BCT   R2,CNV80                                                         
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*                                  ************* -P- & -Q- **********           
         USING PPTELEM,R6                                                       
CNV100   CLI   INTTITLE,C' '       BUILD EPISODE TITLE ELEMENT                  
         BNE   *+14                                                             
         CLC   INTTITLE(L'INTTITLE-1),INTTITLE+1                                
         BE    CNV106              BYPASS IF NO EPISODE PROVIDED                
         MVI   PPTCODE,PPTCODEQ                                                 
         MVC   PPTTITLE(L'INTTITLE),INTTITLE                                    
         LA    R5,PPTTITLE+L'INTTITLE-1                                         
         LA    R1,L'INTTITLE-1                                                  
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,3(R1)                                                         
         STC   R1,PPTELN                                                        
         GOTO1 APUTEL,PPTELEM                                                   
         DROP  R6                                                               
*                                                                               
         USING CRRELEM,R6                                                       
CNV106   OC    BOOKSW,BOOKSW       BUILD CORRECTION ELEMENT                     
         BZ    CNV110              BYPASS IF REGULAR BOOK                       
         MVI   CRRCODE,CRRCODEQ                                                 
         MVI   CRRLEN,CRRLENEQ                                                  
         MVI   CRRTYPE,X'FF'       TYPE WILL BE RESET IN UPDATE ROUTINE         
         MVC   CRROBOOK,INTPNO     ORIGINATING BOOK                             
         GOTO1 APUTEL,CRRELEM                                                   
         DROP  R6                                                               
*                                  BUILD DEMO ELEMENTS                          
CNV110   LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
         MVC   WORK(10),OFORMAT    SET UP OUTPUT FORMAT BLOCK                   
         MVC   WORK+7(2),INTBOOK  TO FORCE BOOK ON X'5E' ELEMENT                
         MOVE  (CONDWORK,2000),INTACCS                                          
         GOTO1 CDEMEL,DMCB,(C'C',WORK),DBLOCKD,CONDWORK                         
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CONDWORK(2),CONDWORK   TEST FOR ANY DEMO ELEMENTS                
         BZ    CNV140              NONE-PUT RECORD TO TAPE                      
         LA    R1,CONDWORK+2                                                    
         SR    R0,R0                                                            
*                                                                               
         EJECT                                                                  
*                                  ADD DEMO ELEMENTS TO RECORD                  
CNV120   CLI   0(R1),0                                                          
         BE    CNV140                                                           
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV120                                                           
*                                                                               
         SPACE 2                                                                
*                                  PUT RECORD TO TAPE & SAVE VALUES             
CNV140   CLC   INTSTA(3),=C'HUT'                                                
         BNE   *+16                                                             
         CLI   HUTPRINT,C'D'       TEST PRINT DETAILS ONLY                      
         BE    *+8                                                              
         OI    PRINTSW,X'80'       SUPPRESS PRINTING OF OPR                     
         GOTO1 PUTTAPE,0           PUT -P- OR -Q- RECORD                        
         MVC   PREVKEY,THISKEY                                                  
         MVC   PREVSQH,THISSQH                                                  
         MVC   PREVDW,THISDW                                                    
*                                                                               
         CLC   INTSTA(3),=C'HUT'   TEST FOR HUT RECORD                          
         BE    CNV240              YES-POST HUT BUFFER                          
*----------------------------------                                             
* THE -N- RECORDS ARE CREATED HERE                                              
*----------------------------------                                             
         LA    R2,1                SET COUNTER                                  
         MVI   HALF,C'Y'           SET SWITCH FOR SINGLE DAY                    
         CLI   INTSTA+3,C'S'       TEST FOR SEASON-TO-DATE (ONLY 'Q'S)          
         BE    *+12                YES-TREAT IT LIKE A 'P'                      
         CLI   INTRTYP,PMCODEQU    TEST FOR NETWORK PROG RECORD (Q)             
         BE    CNV160                                                           
*                                  ************* -P- ONLY ***********           
         CLI   INTSTYP,C'B'        DOESN'T APPLY TO BRKOUTS                     
         BE    CNV155                                                           
         L     R1,ACOMWRK          PT TO PRGAVG TABLE CREATED IN IPHASE         
CNV150   OC    0(3,R1),0(R1)                                                    
         BZ    CNV155              NOT FOUND?                                   
         CLC   INTSTA(3),0(R1)                                                  
         BNE   *+14                                                             
         CLC   INTPNUM,4(R1)       PROGRAM FOUND                                
         BE    CNV153                                                           
         LA    R1,7(R1)            L'PRGAVG                                     
         CLC   0(2,R1),=X'FFFF'    END OF TABLE?                                
         BE    CNV155                                                           
         B     CNV150                                                           
                                                                                
*                                                                               
CNV153   DS    0H             MAKE SURE M-F/M-S/VAR N-REC COMES OUT             
         LA    R6,THISKEY                                                       
         USING PNKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVC   PNACTDAY,6(R1)      DAY CODE FROM TABLE                          
         MVI   PNCODE,PNCODEQU                                                  
         MVC   PNMEDIA,MEDIA                                                    
         MVC   PNSRC,INTKSRC                                                    
         MVC   PNBOOK,INTBOOK                                                   
         MVC   PNBTYP,BOOKTYPE                                                  
         MVC   PNSTAT,INTSTA                                                    
         MVC   PNBSIND,INTSTYP                                                  
         MVC   PNDW,PREVDW                                                      
         MVC   PNSTIM,INTSQH       START QH                                     
         MVC   PNACTDUR,INTDURM    DURATION IN MINUTES                          
         MVC   PNPNUM,INTPNUM                                                   
         GOTO1 ABLDREC,DMCB,(C'P',PNKEY)                                        
         GOTO1 PUTTAPE,0           PUT -N- RECORD                               
         DROP  R6                                                               
*                                                                               
CNV155   ZIC   R1,PREVDW           FIND ACTUAL DAY/WEEK IN KEY                  
         SRL   R1,4                                                             
         STC   R1,HALF                                                          
         LA    R2,5                                                             
         LA    R4,X'40'            R4=MASK FOR DAY BITS (MON-SUN)               
         LA    R3,X'10'            START AT MONDAY AND EXPLODE SINGLE           
         CLI   HALF,0              DAYS FOR M-F OR M-SUN RECS,                  
         BE    CNV160              ALSO ADDING PASSIVE RECS UNDER M-F,          
         LA    R2,7                M-SUN AND VAR                                
         CLI   HALF,8              TEST FOR M-SUN                               
         BE    CNV160                                                           
         CLI   HALF,9              TEST FOR VAR                                 
         BE    CNV160                                                           
         LA    R2,1                                                             
         MVI   HALF,C'Y'           SET SWITCH FOR SINGLE DAY                    
         B     CNV160                                                           
*                                                                               
         EJECT                                                                  
CNV159   TM    INTDYBIT,0          EXECUTED CODE (-P- ONLY)                     
*                                                                               
*                                  ************* -P- & -Q- **********           
CNV160   CLI   HALF,C'Y'           TEST FOR SINGLE DAY                          
         BE    *+12                                                             
         EX    R4,CNV159           SHOULD THIS DAY BE SELECTED ?                
         BZ    CNV220                                                           
         LA    R6,THISKEY          BUILD 'N' RECORD KEY                         
         USING PNKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVI   PNCODE,PNCODEQU                                                  
         MVC   PNMEDIA,MEDIA                                                    
         MVC   PNSRC,INTKSRC                                                    
         MVC   PNBOOK,INTBOOK                                                   
         MVC   PNBTYP,BOOKTYPE                                                  
         MVC   PNSTAT,INTSTA                                                    
         MVC   PNDW,PREVDW                                                      
         MVC   PNBSIND,INTSTYP                                                  
         CLI   INTRTYP,PRCODEQU    TEST FOR PAV RECORD (P)                      
         BE    *+16                                                             
         CLI   INTSTA+3,C'S'       TEST FOR SEASON-TO-DATE                      
         BE    *+8                                                              
         MVI   PNDW,X'90'          SET DAY TO VARIOUS FOR 'Q' REC               
         MVC   PNSTIM,INTSQH       START QH                                     
         MVC   PNACTDUR,INTDURM    DURATION IN MINUTES                          
         CLI   HALF,C'Y'           TEST FOR SINGLE DAY                          
         BE    CNV200                                                           
         STC   R3,PNDW                                                          
         MVC   DUB(1),PREVDW                                                    
         NI    DUB,X'0F'           ISOLATE WEEK NIBBLE                          
         OC    PNDW,DUB            COMBINE DAY AND WEEK NIBBLES                 
*                                                                               
CNV200   MVC   PNPNUM,INTPNUM                                                   
         MVC   PNACTDAY,PREVDW     ACTUAL DAY                                   
         CLI   INTRTYP,PRCODEQU    TEST FOR PAV RECORD (P)                      
         BE    *+16                                                             
         CLI   INTSTA+3,C'S'       TEST FOR SEASON-TO-DATE                      
         BE    *+8                                                              
         MVI   PNACTDAY,X'90'                                                   
         GOTO1 ABLDREC,DMCB,(C'P',PNKEY)                                        
         GOTO1 PUTTAPE,0           PUT -N- RECORD                               
         SPACE 1                                                                
         CLI   PNSTAT+4,C'S'       ONLY FOR SYNDICATION                         
         BNE   CNV220                                                           
*                                                                               
         LA    RF,PROGBUF          PREVIOUSLY DONE TABLE                        
         CLC   PRVPKSTA,PNSTAT                                                  
         BNE   *+10                                                             
         CLC   PRVPKBK,PNBOOK                                                   
         BNE   *+10                                                             
         CLC   PRVPKBTYP,PNBTYP                                                 
         BE    CNV210                                                           
         LA    RE,PROGBUF                                                       
         LHI   RF,6000                                                          
         XCEF                                                                   
         LA    RF,PROGBUF                                                       
         MVC   PRVPKSTA,PNSTAT                                                  
         MVC   PRVPKBTYP,PNBTYP                                                 
         MVC   PRVPKBK,PNBOOK                                                   
*                                                                               
CNV210   OC    0(2,RF),0(RF)                                                    
         BZ    CNV212                                                           
         CLC   PNPNUM,0(RF)        DONE BEFORE                                  
         BE    CNV220               DON'T SEND DUPS                             
         LA    RF,2(RF)                                                         
         B     CNV210                                                           
*                                                                               
CNV212   MVC   0(2,RF),PNPNUM      SAVE PROGRAM NUMBER                          
         LA    RF,THISKEY2                                                      
         USING PKKEY,RF                                                         
         XC    THISKEY2,THISKEY2                                                
         MVI   PKCODE,PKCODEQU     C'K'                                         
         MVI   PKMEDIA,C'N'                                                     
         MVC   PKSRC,INTKSRC                                                    
         MVC   PKPNUM+L'PKPNUM-L'PNPNUM(L'PNPNUM),PNPNUM                        
         MVC   PKBOOK,PNBOOK                                                    
         MVC   PKSTA,PNSTAT                                                     
         MVC   PKBTYP,PNBTYP                                                    
         DROP  RF                                                               
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY2)                                     
         GOTO1 PUTTAPE,0                                                        
         DROP  R6                                                               
         SPACE 1                                                                
CNV220   SRL   R4,1                BUMP TO NEXT INDIVIDUAL DAY                  
         LA    R3,X'10'(R3)        BUMP TO NEXT DAY CODE                        
         BCT   R2,CNV160                                                        
*                                                                               
         CLI   HALF,C'Y'           TEST FOR SINGLE DAY                          
         BE    CNV230                                                           
         LA    R2,1                                                             
         MVI   HALF,C'Y'           NOW ADD THE M-F, M-SUN OR VAR RCD            
         B     CNV160                                                           
*                                                                               
CNV230   B     CNVX                ALL DONE                                     
*                                                                               
         EJECT                                                                  
* FOR EACH QUARTER-HOUR HUT RECORD, POST INTO A HALF-HOUR BUFFER                
*                                                                               
CNV240   CLI   INTRTYP,PRCODEQU    TEST FOR PAV RECORD (P)                      
         BNE   CNV280                                                           
*                                                                               
*                                  ************* -P- ONLY ***********           
         LA    R6,INTKEY                                                        
         USING PRKEY,R6                                                         
         TM    PRDW,X'0F'          USE NORMAL DAYS ONLY (00,10,---70)           
         BNZ   CNV280                                                           
         CLI   INTDAYWK,X'80'      TEST M-S                                     
         BE    CNV280              YES - BYPASS                                 
         CLI   INTDAYWK,X'00'      TEST M-F                                     
         BNE   CNV260              NO                                           
         CLI   INTSQH,48           TEST M-F (6P-11P)                            
         BL    CNV260                                                           
         CLI   INTSQH,68                                                        
         BL    CNV280              BYPASS M-F (6P-11P)                          
         DROP  R6                                                               
*                                                                               
CNV260   LA    R5,DBLOCKA          LOOK UP THE HUT                              
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         ST    RA,DBCOMFCS                                                      
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,PRFRSTEL-PRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         GOTO1 CDEMOUT,DMCB,(C'D',RATING),DBLOCKD,DUB                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,ADDBUFF                                                       
*                                                                               
CNV280   B     CNVX                                                             
         EJECT                                                                  
* LAST TIME HOOK - UPDATE THE YEARLY HUT SUMMARY RECORDS                        
*                  BASED ON DATA IN BUFFER.                                     
*                                                                               
CNV300   OC    BOOKSW,BOOKSW       PUT OUT REMAINDR OF PREV CORR BOOK?          
         BZ    CNV310              NOTHING TO RELEASE                           
         CLI   EOFSW,1             REACHED END OF CORR BOOK?                    
         BE    CNV310              YES                                          
         GOTO1 PUTTAPE,4           NO, RELEASE REST OF BOOK                     
*                                                                               
CNV310   CLI   NBOOKS,0            TEST IF ANY BOOKS IN HUT BUFFER              
         BE    CNV400              NO,                                          
         CLI   TAPE,C'D'           IF DAILIES TAPE, DON'T GEN HUTS              
         BE    CNV400                                                           
         CLI   FILTTIME,0          TEST FOR TIME FILTERING                      
         BNE   CNV400              YES-SKIP HUT SUMMARY UPDATE                  
         CLI   BOOKTYPE,C'Z'       TEST BOOK - DON'T GEN HUTS                   
         BE    CNV400                                                           
         CLI   HUTGEN,C'Y'         ONLY FOR THE FIRST MARKET                    
         BNE   CNV400                                                           
         MVI   HUTGEN,C'N'                                                      
*                                                                               
         XC    HUTVALS(HUTVALLN),HUTVALS                                        
         CLI   HUTPRINT,C'S'                                                    
         BE    CNV320                                                           
         OI    PRINTSW,X'80'                                                    
*                                                                               
CNV320   ZIC   R2,HUTHALF          GET HALF HOUR VALUE                          
         SLL   R2,1                CONV. TO QH                                  
         STC   R2,DUB              CONV. QH TO MIL TIME                         
         GOTO1 VQHTOHR,DMCB,DUB,HUTTIME                                         
         SR    R1,R1               ADD 1 MINUTE TO START TIME                   
         ICM   R1,3,HUTTIME                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,HUTTIME+2                                                   
*                                                                               
         LA    R2,DAYS             R2=COUNTER OF DAYS                           
         LA    R3,DAYTAB           R3=A(DAY TABLE)                              
CNV340   MVC   HUTDW(2),0(R3)                                                   
         BAS   RE,GETSUM           GET SUMMARY RECORD                           
         BAS   RE,UPSUM            UPDATE IT WITH 1/2 HR HUT                    
         CLI   SORTSW,0                                                         
         BNE   CNV350                                                           
         GOTO1 VSORTER,DMCB,SORTCARD,(X'80',RECCARD),(X'80',0)                  
         MVI   SORTSW,1                                                         
*                                                                               
CNV350   GOTO1 VSORTER,DMCB,=C'PUT',AOREC                                       
         LA    R3,L'DAYTAB(R3)     NEXT DAY                                     
         BCT   R2,CNV340                                                        
*                                                                               
         ZIC   R1,HUTHALF          INCREMENT HALF HOUR                          
         LA    R1,1(R1)                                                         
         CHI   R1,48               TEST IF ALL HALF-HOURS DONE                  
         BNL   CNV380              YES                                          
         STC   R1,HUTHALF                                                       
         B     CNV320              DO NEXT HALF-HOUR                            
*                                                                               
CNV380   GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   RE,15,4(R1)         RE=A(SORT RECORD)                            
         BZ    CNV390              EOF                                          
         LH    R1,0(RE)                                                         
         L     RF,AOREC                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         GOTO1 APUTTAPE            PUT YEARLY HUT RECORD                        
         B     CNV380                                                           
*                                                                               
CNV390   MVI   SORTSW,0                                                         
*                                                                               
CNV400   DS    0H                  EOF                                          
         DS    0H                  RELEASE UPDATED BIT MAP                      
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
         DS    0H                  GENERATE J-RECS                              
         GOTO1 VNTIPRG,DMCB,=C'JREC',(0,VBITMAP1),0                             
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   CNV410                                                           
         XC    DBUPARMS,DBUPARMS        RELEASE CONTROL RECORDS                 
         LA    RE,DBUPARMS              TO CTAPE                                
         USING DBUPARMD,RE                                                      
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         DROP  RE                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,RELQ   RELEASE                        
*                                                                               
CNV410   B     CNVX                                                             
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
* SUB-ROUTINE TO POST A HUT VALUE INTO BUFFER (AT ENTRY, DUB CONTAINS           
* HUT VALUE)                                                                    
*                                                                               
ADDBUFF  NTR1                                                                   
         CLI   FRSTSW,YES          TEST FOR FIRST TIME THROUGH                  
         BNE   ADDBUFF1                                                         
         MVI   FRSTSW,NO                                                        
*                                                                               
ADDBUFF1 LA    R0,BOOKS            R3=COUNTER                                   
         LA    RE,BOOKLIST         RE=BOOK LIST POINTER                         
         SR    R2,R2               R2=INDEX REGISTER                            
ADDBUFF2 OC    0(L'INTBOOK,RE),0(RE)                                            
         BZ    ADDBUFF3                                                         
         CLC   INTBOOK,0(RE)                                                    
         BE    ADDBUFFX                                                         
         LA    R2,1(R2)                                                         
         LA    RE,L'INTBOOK(RE)                                                 
         BCT   R0,ADDBUFF2                                                      
         DC    H'0'                BLOWN BOOK LIMIT                             
*                                                                               
ADDBUFF3 MVC   0(L'INTBOOK,RE),INTBOOK                                          
         LA    R1,1(R2)            N'BOOKS=INDEX+1                              
         STC   R1,NBOOKS                                                        
*                                                                               
ADDBUFFX B     CNVX                                                             
         EJECT                                                                  
* SUB-ROUTINE TO READ HUT SUMMARY RECORDS FROM FILE. IF A HUT SUMMARY           
* RECORD DOES NOT EXIST FOR DAY/HALF-HOUR, IT WILL BE BUILT BY ROUTINE          
*                                                                               
* ON ENTRY - HUTTIME =MILITARY START/END TIME FOR HALF-HOUR                     
*            HUTDW   =DAY CODE                                                  
*            HUTDAY  =DAY BITS                                                  
*            HUTHALF =HALF HOUR (0-47)                                          
*                                                                               
GETSUM   NTR1                                                                   
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         ST    RA,DBCOMFCS                                                      
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELAGY,=C'11'                                                  
         MVC   DBSELSTA,=C'HUT T'                                               
         MVC   DBSELBK(1),BOOKLIST EXTRACT YEAR TO UPDATE                       
         MVC   DBBTYPE,BOOKTYPE                                                 
         MVC   DBSELDAY,HUTDAY                                                  
         MVC   DBSELTIM,HUTTIME                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         OC    DBDIVSOR,DBDIVSOR   TEST IF RECORD FOUND                         
         BZ    GETSUM2             NO                                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DBAREC           SET SORT RECORD LEN AT AOREC                 
         L     RF,AOREC                                                         
         LH    R1,PRRLEN-PRKEY(RE) GET REC LEN                                  
         LA    R1,4(R1)            + REC HEADER LEN                             
         XC    0(4,RF),0(RF)                                                    
         STH   R1,0(RF)                                                         
         B     GETSUMX                                                          
*                                                                               
GETSUM2  CLI   BOOKTYPE,C'Z'       X-RATED BOOK                                 
         BE    GETSUM4                                                          
*                                                                               
         CLI   BOOKLIST+1,1        TEST FOR FIRST WEEK IN YEAR                  
         BE    GETSUM4                                                          
         DC    H'0'                NO-CANNOT BE MISSING RECORD                  
*                                                                               
         EJECT                                                                  
GETSUM4  LA    R6,THISKEY          BUILD A KEY-INITIALIZE RECORD                
         XC    THISKEY,THISKEY                                                  
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU                                                  
         MVC   PRMEDIA,MEDIA                                                    
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,=C'HUT T'                                                 
         MVC   PRBOOK(1),BOOKLIST  EXTRACT YEAR                                 
         MVC   PRBTYP,BOOKTYPE     SET TYPE OF DATA                             
         ZIC   R1,HUTHALF                                                       
         SLL   R1,1                                                             
         STC   R1,PRSTIM                                                        
         MVC   PRDW,HUTDW                                                       
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING PHELEM,R6                                                        
         MVI   PHCODE,PHCODEQ      BUILD QHR ELEMENT                            
         MVI   PHELN,PHELNEQ                                                    
         MVI   PHDUR,2             HALF-HOUR                                    
         OI    PHDWKS,X'0F'                                                     
         MVC   PHDBOOK(1),BOOKLIST                                              
         GOTO1 APUTEL,PHELEM                                                    
         DROP  R6                                                               
*                                                                               
         XC    TEMP,TEMP                                                        
         MVI   0(R6),X'90'         ADD SKELETAL HUT ELEMENT                     
         MVI   1(R6),108                                                        
         GOTO1 APUTEL,TEMP                                                      
*                                                                               
GETSUMX  B     CNVX                                                             
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET HUT VALUE FOR DAY/HALF HOUR AND TO UPDATE                  
* SUMMARY RECORD                                                                
*                                                                               
UPSUM    NTR1                                                                   
         ZIC   R2,NBOOKS           R2=COUNTER                                   
         LA    R3,BOOKLIST                                                      
         LA    R5,DBLOCKA          RE-ESTABLISH DBLOCK ADDRESSABILITY           
         USING DBLOCKD,R5                                                       
         GOTO1 CHELLO,DMCB,(C'G',PAVFIL),(X'90',DBAREC),0                       
         CLI   12(R1),0            MUST FIND HUT ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AHUTEL,12(R1)       SAVE A(HUT ELEMENT)                          
         SR    R1,R1               CLEAR BOOK INDEX FOR GETBUFF                 
*                                                                               
UPSUM2   BAS   RE,GETBUFF          GET HUT ENTRY FROM BUFFER                    
         L     RE,DUB              A(HPT ENTRY)                                 
         USING HPTD,RE                                                          
         L     RF,HPTAVER                                                       
         DROP  RE                                                               
*                                                                               
         L     RE,AHUTEL                                                        
         LA    RE,2(RE)            POINT TO FIRST HUT VALUE                     
         ZIC   R0,1(R3)            GET WEEK NUMBER FROM BOOK                    
         BCTR  R0,0                                                             
         SLL   R0,1                INDEX INTO HUT VALUE LIST                    
         AR    RE,R0                                                            
         STCM  RF,3,0(RE)          SLOT VALUE INTO POSITION                     
*                                                                               
         LA    R1,1(R1)            INCREMENT BOOK INDEX                         
         LA    R3,L'INTBOOK(R3)    NEXT BOOK                                    
         BCT   R2,UPSUM2                                                        
*                                                                               
UPSUMX   B     CNVX                                                             
         DROP  R5                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET AN ENTRY FROM THE HUT BUFFER                               
*                                                                               
* ON ENTRY, R1      = BOOK INDEX (RELATIVE TO ZERO)                             
*           R4      = DAY CODE (HUTDW)                                          
*           HUTHALF = HALF HOUR NUMBER                                          
* ON EXIT, DUB CONTAINS ENTRY                                                   
*                                                                               
GETBUFF  NTR1                                                                   
         LA    R6,HPTLN                                                         
         MHI   R6,48                                                            
         ZIC   R7,HUTDW                                                         
         SRL   R7,4                                                             
         MR    R6,R6                                                            
         A     R7,VPUTBUFF                                                      
         ZIC   RE,HUTHALF                                                       
         LA    RF,HPTLN                                                         
         MR    RE,RE                                                            
         AR    R7,RF                                                            
         ST    R7,DUB                                                           
         XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO SIMPLY PUT A RECORD TO TAPE FOR A REGULAR BOOK.                
*             FOR AN IRREGULAR BOOK (CORRECTION RECORDS),                       
*             THE EXISTING BOOK MUST BE READ IN, UPDATED AND REWRITTEN.         
PUTTAPE  NTR1                                                                   
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   *+8                                                              
         BAS   RE,ADDCTBK          CALL DEBKUPDT TO ADD NEW BOOK                
*                                                                               
         MVI   OLDBK,C'N'          -                                            
         OC    BOOKSW,BOOKSW                                                    
         BNZ   PT100                                                            
         GOTO1 APUTTAPE            REGULAR BOOK                                 
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         B     EXIT                                                             
*                                                                               
PT100    B     *+4(R1)             CORRECTION BOOK                              
         B     PT200               PUT RECORD                                   
         B     PT600               PUT REMAINDER OF BOOK                        
*                                                                               
PT200    CLI   THISKEY,C'P'        TEST RECORD TYPE                             
         BNE   PT220                                                            
         BAS   RE,PKEY                                                          
         B     EXIT                                                             
*                                                                               
PT220    CLI   THISKEY,C'Q'        TEST RECORD TYPE                             
         BNE   PT240                                                            
         BAS   RE,QKEY                                                          
         B     EXIT                                                             
*                                                                               
PT240    CLI   THISKEY,C'N'        TEST RECORD TYPE                             
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,NKEY                                                          
         B     EXIT                                                             
*                                                                               
         SPACE 1                                                                
PT600    MVI   THISKEY,X'FF'       INSURE THAT THISKEY COMPARES HIGH            
         MVC   THISKEY+1(L'THISKEY-1),THISKEY                                   
         MVI   OLDBK,C'Y'          TEST IF OLD BK                               
         CLI   PREVCORR+2,C'P'                                                  
         BE    PT620                                                            
         CLI   PREVCORR+2,C'Q'                                                  
         BE    PT640                                                            
         DC    H'0'                                                             
*                                                                               
PT620    MVI   THISKEY,C'P'                                                     
         BAS   RE,PKEY             PUT OUT REMAINDER OF OLD BOOK'S P'S          
         B     EXIT                                                             
*                                                                               
PT640    MVI   THISKEY,C'Q'                                                     
         BAS   RE,QKEY             PUT OUT REMAINDER OF OLD BOOK'S Q'S          
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* CALL DEBKUPDT FOR EACH P AND Q REC CREATED. DEBKUPDT KEEPS TRACK OF           
* THE BOOKS BEING LOADED AND WILL END UP CREATING CONTROL RECORDS WHEN          
* CALLED WITH RELEASE MODE.                                                     
*                                                                               
ADDCTBK  NTR1                                                                   
*                                                                               
         TM    FLAGS1,EXCLUDE_CORRECTIONS   EXCLUDE CORRECTIONS?                
         BNO   ADDCB05                     (VIA CARD NETCTCOR=N)                
         OC    BOOKSW,BOOKSW                YES                                 
         BNZ   ADDCTBKX                     EXIT FOR CORRECTIONS                
*                                                                               
ADDCB05  XC    DBUPARMS,DBUPARMS                                                
         LA    R2,DBUPARMS                                                      
         USING DBUPARMD,R2                                                      
*                                                                               
         MVI   DBUPSTYP,DBUPNETQ   POSTING TYPE. DEFAULT IS NETWORK             
*                                                                               
         L     RE,AOREC                                                         
         LA    RE,4(RE)            POINT TO DEMO RECORD TO BE RELEASED          
         USING PRKEY,RE                                                         
         CLI   PRCODE,PRCODEQU     P RECORDS                                    
         BNE   ADDCB10                                                          
         MVC   DBUBOOK,PRBOOK      BOOK FOR P RECORDS                           
         CLI   PRSTAT+4,C'S'                                                    
         BNE   *+8                                                              
         MVI   DBUPSTYP,DBUPSYNQ   POSTING TYPE FOR SYNDICATION                 
         DROP  RE                                                               
         B     ADDCB20                                                          
*                                                                               
         USING PMKEY,RE                                                         
ADDCB10  CLI   PMCODE,PMCODEQU     Q RECORDS                                    
         BNE   ADDCTBKX            EXIT FOR ALL OTHER RECORD TYPES              
         MVC   DBUBOOK,PMBOOK      BOOK FOR Q RECORDS                           
         CLI   PMSTAT+4,C'S'                                                    
         BNE   *+8                                                              
         MVI   DBUPSTYP,DBUPSYNQ   POSTING TYPE FOR SYNDICATION                 
         DROP  RE                                                               
*                                                                               
ADDCB20  CLI   DBUBOOK+1,0         SKIP IT FOR YEARLY HUTS                      
         BE    ADDCTBKX                                                         
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         MVI   DBUBKFLG,DBUBKWQ                                                 
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,ADDQ    ADD DEMO UPDATE BOOK          
*                                                                               
ADDCTBKX B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PROCESS PROGRAM AVERAGE DIRECTORY ENTRIES                                     
*                                                                               
PKEY     NTR1                                                                   
         USING PRKEY,R2                                                         
         CLI   EOFSW,1             ALREADY AT EOF?                              
         BE    PKEY90              YES, DON'T READ SEQ                          
         LA    R2,KEYP                                                          
         CLI   PRCODE,PRCODEQU     TEST 1ST-TIME-THRU                           
         BE    PKEY30              NO                                           
         XC    KEYP,KEYP                                                        
         MVI   PRCODE,PRCODEQU     YES - SET UP KEY (C'PN')                     
         MVI   PRMEDIA,C'N'                                                     
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,INTSTA       STATION (NETWORK)                            
         MVC   PRSTYP,INTSTYP      BREAKOUT/SPECIAL INDICATOR                   
         MVC   PRBOOK,BOOKSW       CORRECTION BOOK (YYWW)                       
         MVC   PRBTYP,BOOKTYPE     BOOK TYPE ('A' OR 'I')                       
         CLI   OLDBK,C'Y'          SET FIELDS IF OLD BK                         
         BNE   PKEY10              NO                                           
         MVC   PRSTAT,SVSTA        YES, RESET STATION'                          
         MVC   PRBTYP,SVBK         RESET BK                                     
         MVC   PRSTYP,SVSTYP       RESET INTSTYP                                
         DROP  R2                                                               
*                                                                               
PKEY10   MVC   KEY,KEYP                                                         
         MVI   RDHIPSW,1           FLAG LAST READ AS HI                         
         BAS   RE,HIGH                                                          
         BNE   PKEY32                                                           
         CLC   KEY(PRBTYP-PRCODE+1),KEYP  DID WE GET SAME KEY                   
         B     PKEY32                                                           
*                                                                               
PKEY30   MVC   KEY,KEYP                                                         
         MVI   RDHIPSW,0           FLAG LAST READ AS SEQ                        
         BAS   RE,SEQ                                                           
PKEY32   BNE   PKEY90              EOF                                          
*                                                                               
PKEY40   MVC   KEYP,KEY                                                         
         CLC   THISKEY,IOAREA+4    ATTEMPT TO MATCH CORRECTION TO OLD           
         BE    PKEY50              CHANGE RECORD                                
         BL    PKEY70              ADD RECORD                                   
*       (BH)                       STILL IN DOUBT                               
         MVC   SVAOREC,AOREC       SAVE THE OUTPUT RECORD ADDRESS               
         LA    RE,IOAREA                                                        
         ST    RE,AOREC            PUT OUT OLD RECORD                           
         GOTO1 APUTTAPE                                                         
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         MVC   AOREC,SVAOREC       RSTORE THE OUTPUT RECORD ADDRESS             
         B     PKEY30              RESUME SEARCH FOR MATCH                      
*                                                                               
PKEY50   MVI   ADDCHA,C'C'         CHANGE - PUT CORRECTION RECORD               
         BAS   RE,UPDATE           FLAG NETWORK PROGRAM TYPE ELEMENT            
         GOTO1 APUTTAPE                                                         
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         CLI   TAPE,C'D'           BYPASS FOR DAILIES                           
         BE    *+8                                                              
         BAS   RE,CREATE           CREATE CORRECTION POINTER                    
         B     PKEYX                                                            
*                                                                               
PKEY70   MVI   ADDCHA,C'A'         ADD - PUT CORRECTION RECORD                  
         MVC   IOSAVE,IOAREA+4                                                  
         BAS   RE,UPDATE           FLAG NETWORK PROGRAM TYPE ELEMENT            
         GOTO1 APUTTAPE                                                         
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         CLI   TAPE,C'D'           BYPASS FOR DAILIES                           
         BE    *+8                                                              
         BAS   RE,CREATE           CREATE CORRECTION POINTER                    
         MVC   KEYP,KEYSAVE        REREAD CURRENT RECORD                        
         CLI   RDHIPSW,0           CURRENT RECORD WAS READ                      
         BE    PKEYX                OK                                          
         XC    KEYP,KEYP           ADDED BEFORE FIRST RECORD                    
         B     PKEYX                                                            
*                                                                               
PKEY90   MVI   EOFSW,1             SET EOF                                      
         CLI   THISKEY+1,X'FF'     JUST FORCE RELS TILL END OF BK?              
         BE    PKEY95                                                           
         GOTO1 APUTTAPE            ADD - PUT CORRECTION RECORD AT EOF           
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
*                                                                               
PKEY95   XC    KEY,KEY             DONE RELSG RECDS IN BOOK.RESET KEYS          
         XC    KEYP,KEYP                                                        
         XC    SVSTA,SVSTA         RESET SAVED STATION                          
         XC    SVBK,SVBK           RESET SAVED BOOK                             
         XC    SVSTYP,SVSTYP                                                    
         MVI   OLDBK,C'N'          INIT FLG TO NO                               
PKEYX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* PROCESS NETWORK PROGRAM ENTRIES                                               
*                                                                               
QKEY     NTR1                                                                   
         USING PMKEY,R2                                                         
         CLI   EOFSW,1                                                          
         BE    QKEY90                                                           
         LA    R2,KEYQ                                                          
         CLI   PMCODE,PMCODEQU     TEST 1ST-TIME-THRU                           
         BE    QKEY30              NO                                           
         XC    KEYQ,KEYQ                                                        
         MVI   PMCODE,PMCODEQU     YES - SET UP KEY (C'QN')                     
         MVI   PMMEDIA,C'N'                                                     
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,BOOKSW       CORRECTION BOOK (YYWW)                       
         MVC   PMSTAT,INTSTA       STATION (NETWORK)                            
         MVC   PMSTYP,INTSTYP      BREAKOUT/SPECIAL INDICATOR                   
         MVC   PMBTYP,BOOKTYPE     BOOK TYPE ('A' OR 'I')                       
         CLI   OLDBK,C'Y'          TEST IF PREV BK                              
         BNE   QKEY10              NO                                           
         MVC   PMSTAT,SVSTA        YES, RESET STATION                           
         MVC   PMBTYP,SVBK         YES, RESET BOOOK                             
         MVC   PMSTYP,SVSTYP       YES, RESET INTSTYP                           
         DROP  R2                                                               
*                                                                               
QKEY10   MVC   KEY,KEYQ                                                         
         MVI   RDHIQSW,1                                                        
         BAS   RE,HIGH                                                          
         BNE   QKEY32              EOF                                          
         CLC   KEY(PMBTYP-PMCODE+1),KEYQ  DID WE GET SAME KEY                   
         B     QKEY32                                                           
*                                                                               
QKEY30   MVC   KEY,KEYQ                                                         
         MVI   RDHIQSW,0                                                        
         BAS   RE,SEQ                                                           
QKEY32   BNE   QKEY90              EOF                                          
*                                                                               
QKEY40   MVC   KEYQ,KEY                                                         
         CLC   THISKEY,IOAREA+4    ATTEMPT TO MATCH CORRECTION TO OLD           
         BE    QKEY50              CHANGE RECORD                                
         BL    QKEY70              ADD RECORD                                   
*       (BH)                       STILL IN DOUBT                               
         MVC   SVAOREC,AOREC       SAVE THE OUTPUT RECORD ADDRESS               
         LA    RE,IOAREA                                                        
         ST    RE,AOREC            PUT OUT OLD RECORD                           
         GOTO1 APUTTAPE                                                         
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         MVC   AOREC,SVAOREC       RSTORE THE OUTPUT RECORD ADDRESS             
         B     QKEY30              RESUME SEARCH FOR MATCH                      
*                                                                               
QKEY50   MVI   ADDCHA,C'C'         CHANGE - PUT CORRECTION RECORD               
         BAS   RE,UPDATE           FLAG NETWORK PROGRAM TYPE ELEMENT            
         GOTO1 APUTTAPE                                                         
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         CLI   TAPE,C'D'           BYPASS FOR DAILIES                           
         BE    *+8                                                              
         BAS   RE,CREATE           CREATE CORRECTION POINTER                    
         B     QKEYX                                                            
*                                                                               
QKEY70   MVI   ADDCHA,C'A'         ADD - PUT CORRECTION RECORD                  
         MVC   IOSAVE,IOAREA+4                                                  
         BAS   RE,UPDATE           FLAG NETWORK PROGRAM TYPE ELEMENT            
         GOTO1 APUTTAPE                                                         
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
         CLI   TAPE,C'D'           BYPASS FOR DAILIES                           
         BE    *+8                                                              
         BAS   RE,CREATE           CREATE CORRECTION POINTER                    
         MVC   KEYQ,KEYSAVE        REREAD CURRENT RECORD                        
         CLI   RDHIQSW,0           CURRENT RECORD WAS READ                      
         BE    QKEYX                                                            
         XC    KEYQ,KEYQ           NO - SET FOR READ HI                         
         B     QKEYX                                                            
*                                                                               
QKEY90   MVI   EOFSW,1                                                          
         CLI   THISKEY+1,X'FF'      JUST FORSCE RELS TILL END OF BK?            
         BE    QKEY95              YES, NO RECD TO RELEASE                      
         GOTO1 APUTTAPE            RELEASE CORRECTION RECD                      
         BAS   RE,SVFLDS           SAVE  BOOK AND STATION                       
*                                                                               
QKEY95   XC    KEY,KEY             EOF/END OF RECD RELSG RESET FLDS             
         XC    KEYQ,KEYQ                                                        
         XC    SVSTA,SVSTA                                                      
         XC    SVBK,SVBK                                                        
         XC    SVSTYP,SVSTYP                                                    
         MVI   OLDBK,C'N'                                                       
QKEYX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************                                                          
*SAVE STATION AND BOOK                                                          
**********************                                                          
SVFLDS   NTR1                                                                   
         LA    R2,KEY              -                                            
         USING PRKEY,R2                                                         
         CLI   PRCODE,PRCODEQU     -                                            
         BNE   SVFLDS10            -                                            
         MVC   SVSTA,PRSTAT        -                                            
         MVC   SVBK,PRBTYP         SAVE BOOK  TYPE                              
         MVC   SVSTYP,PRSTYP                                                    
         DROP  R2                                                               
*                                                                               
SVFLDS10 DS    0H                                                               
         USING PMKEY,R2                                                         
         CLI   PMCODE,PMCODEQU     -                                            
         BNE   SVFLDX              -                                            
         MVC   SVSTA,PMSTAT        -                                            
         MVC   SVBK,PMBTYP         SAVE BOOK TYPE                               
         MVC   SVSTYP,PMSTYP                                                    
         DROP  R2                                                               
*                                                                               
SVFLDX   B     EXIT                -                                            
*                                                                               
* ************************                                                      
* PROCESS PASSIVE POINTERS                                                      
* ************************                                                      
NKEY     NTR1                                                                   
         GOTO1 APUTTAPE            CHANGE - PUT CORRECTION RECORD               
         B     EXIT                                                             
*                                                                               
         SPACE 4                                                                
* FILE I/O ROUTINES                                                             
*                                                                               
HIGH     NTR1                                                                   
         MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         XC    IOSAVE,IOSAVE                                                    
         B     IO                                                               
*                                                                               
SEQ      NTR1                                                                   
         MVC   COMMAND,DMRSEQ                                                   
         MVC   KEYSAVE,KEY                                                      
         CLI   KEY,C'N'            PASSIVE POINTER                              
         BNE   IO60                                                             
*                                                                               
IO       DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(1,COMMAND),NTIDIR,KEY,IOAREA+4                    
         CLI   8(R1),0             TEST EOF OR NOT FOUND CONDITION              
         BNE   EXIT                                                             
         LA    RE,IOAREA+4                                                      
         CLI   0(RE),X'FF'         TEST EOF                                     
         BNE   IO20                                                             
         CLI   0(RE),X'0'          SET CC AND EXIT                              
         B     EXIT                                                             
*                                                                               
IO20     MVC   KEY,0(RE)           FOUND KEY                                    
         MVC   SVSTATUS,PRKSTAT-PRKEY(RE)                                       
*                                                                               
         CLI   KEYSAVE,C'N'        PASSIVE POINTER                              
         BE    EXIT                                                             
         OC    KEY+19(4),KEY+19    TEST IF INDEX D/A PRESENT                    
         BZ    EXIT                CC TO ZERO FOR -N- KEYS                      
*                                                                               
IO60     LA    RE,IOAREA+4                                                      
         XC    0(23,RE),0(RE)                                                   
         MVC   0(18,RE),KEY                                                     
         OC    KEY+19(4),KEY+19                                                 
         BNZ   *+6                                                              
         DC    H'0'                NO DA - DIE HERE                             
         OC    IOSAVE,IOSAVE                                                    
         BZ    *+16                                                             
         MVC   IOAREA+4(L'IOSAVE),IOSAVE                                        
         MVC   COMMAND,DMRDHI                                                   
         XC    IOSAVE,IOSAVE                                                    
         MVC   IOAREA+4+PRRSTAT-PRKEY(1),SVSTATUS                               
         GOTO1 VDATAMGR,DMCB,(1,COMMAND),NTIFIL,KEY+19,IOAREA+4                 
         XC    IOAREA(4),IOAREA                                                 
         LH    RE,22(R1)                                                        
         LA    RE,4(RE)                                                         
         STCM  RE,3,IOAREA         RETURNED RECORD LENGTH(+4)                   
*                                                                               
         CLI   8(R1),0             SET CONDITION                                
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
* UPDATE NETWORK CORRECTION (CRRELEM) ELEMENT FOR ADD OR CHANGE                 
*                                                                               
UPDATE   NTR1                                                                   
         USING CRRELEM,RE                                                       
         L     RE,AOREC                                                         
         LA    RE,27(RE)           POINT PAST KEY                               
UD20     CLI   0(RE),0             FIND CRRELEM IN OUTPUT RECORD                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),CRRCODEQ                                                   
         BE    UD40                                                             
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     UD20                                                             
*                                                                               
UD40     MVI   CRRTYPE,X'00'                                                    
         CLI   ADDCHA,C'C'                                                      
         BNE   EXIT                                                             
         OI    CRRTYPE,X'01'       0=ADD   1=CHANGE                             
         B     EXIT                                                             
         DROP  RE                                                               
*                                                                               
* CREATE CORRECTIONS POINTER (CHKEY)                                            
CREATE   NTR1                                                                   
         LA    R6,IOAREA+4                                                      
         XC    0(23,R6),0(R6)                                                   
         USING CHKEY,R6                                                         
         MVI   CHCODE,CHCODEQU     -C-                                          
         MVC   CHMEDIA,MEDIA       -N-                                          
         MVC   CHSRC,INTKSRC       -N,1,7-                                      
         MVC   CHOBOOK,INTPNO      ORIGINATING BOOK                             
         MVC   CHSTAT,INTSTA                                                    
         MVC   CHBTYP,BOOKTYPE                                                  
         MVC   CHDW,INTDAYWK                                                    
         CLI   PREVCORR+2,PMCODEQU                                              
         BNE   *+8                                                              
         MVI   CHDW,X'90'          SET DAY TO VARIOUS FOR 'Q' REC               
         MVC   CHSTIM,INTSQH                                                    
         MVC   CHPNUM,INTPNUM                                                   
         MVC   CHRTYP,PREVCORR+2   -P- OR -Q-                                   
         MVC   CHRBOOK,BOOKSW      REVISION BOOK                                
         GOTO1 ABLDREC,DMCB,(C'P',CHKEY)                                        
         DROP  R6                                                               
         GOTO1 APUTTAPE                                                         
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                  THIS TIME VALUES                             
THISKEY  DC    XL20'00'                                                         
THISSQH  DC    X'00'                                                            
THISDW   DC    X'00'                                                            
THISKEY2 DC    XL20'00'                                                         
*                                  LAST TIME VALUES                             
PREVKEY  DC    XL20'00'                                                         
PREVSQH  DC    X'00'                                                            
PREVDW   DC    X'00'                                                            
*                                                                               
PRVPKSTA DS    CL5                 PREVIOUS 'K' VALUES                          
PRVPKBK  DS    XL2                                                              
PRVPKBTYP DS   C                                                                
*                                                                               
OFORMAT  DS    0CL10                                                            
         DC    C'NTINPNN',3X'00'                                                
*                                                                               
RATING   DC    X'00',C'R',X'01'                                                 
PAVFIL   DC    C'PAVFIL  '         HELLO FILE NAME                              
NTIFIL   DC    C'NTIFIL '                                                       
NTIDIR   DC    C'NTIDIR '                                                       
SVSTATUS DS    C                                                                
SORTCARD DC    C'SORT FIELDS=(5,20,A),FORMAT=BI '                               
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2000,,,,) '                              
*                                                                               
FRSTSW   DC    C'Y'                FIRST TIME SWITCH                            
HUTGEN   DC    C'Y'                GENERATE YEARLY HUTS                         
*                                                                               
FRSTREC  DC    AL1(YES)            FIRST RECORD TO OPHASE                       
*                                                                               
         DS    0F                                                               
HUTBUFFL DC    A(BOOKS*DAYS*48*4)  LENGTH OF HUT BUFFER                         
BOOKLEN  DC    Y(DAYS*48*4)                                                     
DAYLEN   DC    Y(48*4)                                                          
*                                                                               
NBOOKS   DC    X'00'                                                            
BOOKLIST DC    (BOOKS*L'INTBOOK)X'00'                                           
*                                                                               
* TABLE OF DAY CODES AND THEIR BIT REPRESENTATIONS                              
*                                                                               
DAYTAB   DS    0CL2                                                             
         DC    X'00',X'7C'         M-F                                          
         DC    X'10',X'40'         MON                                          
         DC    X'20',X'20'         TUE                                          
         DC    X'30',X'10'         WED                                          
         DC    X'40',X'08'         THU                                          
         DC    X'50',X'04'         FRI                                          
         DC    X'60',X'02'         SAT                                          
         DC    X'70',X'01'         SUN                                          
         DC    X'80',X'7F'         M-S                                          
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         SPACE 2                                                                
*                                                                               
*                                                                               
* EQUATES                                                                       
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
BOOKS    EQU   2                                                                
*                                                                               
* CONSTANTS                                                                     
*                                                                               
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
*                                                                               
         DS    0F                  CORRECTION RECORDS FIELDS                    
SVAOREC  DS    A                   SAVE ADDRESS OF OUTPUT RECORD                
PREVCORR DS    0XL18                PREVIOUS CORRECTION KEY                     
BOOKSW   DC    X'0000',16X'00'     PREV CORRECTION BOOK & KEY                   
ADDCHA   DS    C                   A=ADD   C=CHANGE                             
TAPE     DS    C                   TAPE TYPE--INTSTA+4                          
RDHIPSW  DC    X'00'               LAST READ WAS HIGH P                         
RDHIQSW  DC    X'00'               LAST READ WAS HIGH Q                         
EOFSW    DC    X'00'               EOF ON FILE FOR A BOOK                       
OLDBK    DC    C'N'                FLAG TO RELEASE OF REST OF BOOK              
SVSTA    DC    XL5'00'             CORRECTION RECD'S  STATION                   
SVBK     DS    X'00'               CORR RECD'S BOOK  TYPE                       
SVSTYP   DS    X'00'               CORR RECD'S BOOK  TYPE                       
*                                                                               
KEYP     DC    XL23'00'            -P- KEY                                      
KEYQ     DC    XL23'00'            -Q- KEY                                      
COMMAND  DS    CL8                                                              
KEY      DS    CL23                                                             
KEYSAVE  DS    CL23                                                             
IOSAVE   DS    CL23                SAVE AREA FOR REREAD                         
IOAREA   DS    CL1024                                                           
DBUPARMS DS    XL(DBUPARML)        PARAMETER AREA FOR DEBKUPDT                  
PROGBUF  DS    6000C                                                            
*                                                                               
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
NTIWRKD  DSECT                                                                  
AHUTEL   DS    A                                                                
*                                                                               
HUTVALS  DS    0C                                                               
HUTHALF  DS    X                                                                
HUTTIME  DS    XL4                                                              
HUTDW    DS    X                                                                
HUTDAY   DS    X                                                                
HUTVALLN EQU   *-HUTVALS                                                        
*                                                                               
CONDWORK DS    2000C                                                            
NTIWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTNT3DL                                                     
         EJECT                                                                  
* DEDBLOCK                                                                      
* DEDEMFILE                                                                     
* DDCOMFACS                                                                     
* DEDEMCNVD                                                                     
* DEDEMTABD                                                                     
* DDMONYREQU                                                                    
* DBUPARMSD                                                                     
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
       ++INCLUDE DBUPARMSD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050DEND09O   05/28/15'                                      
         END                                                                    
