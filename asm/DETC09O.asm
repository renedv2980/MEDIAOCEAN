*          DATA SET DETC09O    AT LEVEL 088 AS OF 10/04/11                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETC09OA                                                                 
*INCLUDE DEBKUPDT                                                               
         TITLE 'OUTPUT PHASE FOR TCAR STARTING MAY/2009'                        
DETC09O  CSECT                                                                  
NUMCATS  EQU   38                                                               
         PRINT NOGEN                                                            
         NMOD1 TCRWRKX-TCRWRKD,**TCROUT                                         
         USING TCRWRKD,RC          RC=A(TEMP W/S)                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING DETC09O+4096,R7     R7=2ND BASE REGISTER                         
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
*                                                                               
         B     *+4(R1)                                                          
         B     MAIN                PROCESS A RECORD                             
         B     LASTHK              LAST TIME HOOK (PROCESS YEARLY HUT)          
*                                                                               
         EJECT                                                                  
*********************************************************************           
*MAIN - BUILD OUTPUT RECD IN AOREC RELEASE AFTER NEXT RECD COMES IN             
*       IN ORDER TO MERGE DUPLICATE KEYS WITH DIFFERENT MKT BRK DATA            
*       ONTO A SINGLE RECD.                                                     
*********************************************************************           
MAIN     DS    0H                                                               
         MVI   PRINTSW,0                                                        
         OC    INTKEY,INTKEY       UNUSABLE RECORD                              
         BZ    MAINX                                                            
*                                                                               
         CLI   FRSTREC,YES         FIRST RECORD TO OPHASE                       
         BNE   MAIN05                                                           
         TM    FLAGS1,CREATE_NETCTREC     NETCTREC=Y                            
         BNO   MAIN05                                                           
         GOTO1 =V(DEBKUPDT),DMCB,,INITQ   INITIALIZE DEBKUPDT                   
*                                                                               
MAIN05   MVI   FRSTREC,NO                                                       
*                                                                               
         CLI   INTRTYP,PRCODEQU    P-KEY                                        
         BE    *+8                                                              
         CLI   INTRTYP,PMCODEQU    Q-KEY                                        
         BNE   MAINX               DELETE OTHER RECD TYPES                      
         CLI   INTKEY+2,C'N'       SURE SRC=N ELSE ITS CORTCN RECD              
         BE    MAIN10                                                           
         BAS   RE,CRCTN            HANDLE CORRECTION RECD                       
         B     MAINX                                                            
*                                                                               
MAIN10   DS    0H                                                               
         OC    BOOKSW,BOOKSW                                                    
         BZ    MAIN15                                                           
         MVI   LASTREC,C'Y'                                                     
         BAS   RE,CRCTN                                                         
*                                                                               
MAIN15   BAS   RE,BLDKEY           BLD KEY FOR SREC IN THISKEY                  
         CLI   RELS,C'Y'           RELEASE PREV RECD 1ST?                       
         BNE   MAIN20                                                           
         GOTO1 APUTTAPE,0          RELEASE RECD IN AOREC                        
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   *+8                                                              
         BAS   RE,ADDCTBK          CALL DEBKUPDT TO ADD NEW BOOK                
*                                                                               
         BAS   RE,BLDNREC          BUILD NRECS AND DO MISC STUFF                
         LA    RE,AOKEY                                                         
         XC    PREVSQH(2),PREVSQH  CLEAR PREV RECD FIELDS                       
         CLI   0(RE),C'P'          ONLY SAVE IF P-REC                           
         BNE   MAIN20                                                           
         MVC   PREVSQH,PRSTIM-PRKEY(RE)     ACTUAL SQH IN KEY                   
         MVC   PREVDW,PRDW-PRKEY(RE)        ACTUAL DW IN KEY                    
*                                                                               
MAIN20   CLI   MERGE,C'Y'          MERGE REC W/SAME KEYS/DIFF BRKS              
         BE    MAIN25                                                           
         GOTO1 ABLDREC,DMCB,THISKEY  SETUP THISKEY IN AOREC                     
         MVC   AOINTD,INTERD       SAVE SORT RECD FIELDS                        
         MVC   AOKEY,THISKEY                                                    
         BAS   RE,BLDELEM          BUILD MISC RECD ELEMS                        
*                                                                               
MAIN25   BAS   RE,BLDDEMS          SLOT AND BUILD DEMO ELEMS INTO AOREC         
         B     MAINX               RETURN TO CONTROLLER FOR NEXT SREC           
*                                                                               
MAINX    XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*BLDKEY- BUILD KEYS FOR RECORDS                                                 
*********************************************************************           
*                                                                               
BLDKEY   NTR1                                                                   
         MVI   MERGE,0                                                          
         MVI   RELS,0                                                           
         LA    R6,THISKEY                                                       
         XC    THISKEY,THISKEY                                                  
         CLI   INTRTYP,PMCODEQU    Q-RECD?                                      
         BE    BLDK50                                                           
*                                                                               
BLDK10   DS    0H                  ************* -P- ONLY ***********           
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU     BUILD PAV KEY (P)                            
         MVC   PRMEDIA,MEDIA                                                    
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,BOOKTYPE     SET TYPE OF DATA                             
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
*                                                                               
         OC    AOKEY,AOKEY         NOTHING PREV IN BUFFER?                      
         BZ    BLDKEYX               YES, JUST BUILD KEY, NOT RELS              
*                                                                               
         CLC   AOKEY(PRDW-PRKEY),PRKEY                                          
         BE    *+12                                                             
         MVI   RELS,C'Y'           RELS RECD IN AOREC                           
         B     BLDKEYX                                                          
*                                                                               
         LA    RE,AOINTD           INTACCS FIELDS OF REC IN AO BUFFER           
         CLC   INTDAYWK,INTDAYWK-INTERD(RE)                                     
         BE    *+12                                                             
         MVI   RELS,C'Y'           DIFF RECDS. RELS OLD RECD                    
         B     BLDKEYX              AND BUILD NEW KEY/RECD                      
*                                                                               
         CLC   INTPNUM,INTPNUM-INTERD(RE)  SAME RECD, DIFF MKT BRK?             
         BE    BLDK15                                                           
         MVI   RELS,C'Y'           NO, RELEASE PREV RECD IN AOREC               
         LA    RE,AOKEY            BUMP PRDW OF NEW KEY FOR MULTI RECDS         
         ZIC   R1,PRDW-PRKEY(RE)   FOR SAME DAY/TIME                            
         LA    R1,1(R1)                                                         
         STC   R1,PRDW             SAVE IN THIS KEY (BECOMES AOREC KEY)         
         B     BLDKEYX                                                          
*                                                                               
BLDK15   MVI   MERGE,C'Y'          MERGE MKT BRKS                               
         B     BLDKEYX                                                          
*                                                                               
BLDK50   DS    0H                  ************* -Q- ONLY ***********           
         USING PMKEY,R6                                                         
         MVI   PMCODE,PMCODEQU     PROGRAM Q-RECDS                              
         MVC   PMMEDIA,MEDIA                                                    
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PMBTYP,BOOKTYPE                                                  
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
*                                                                               
         CLC   AOKEY,THISKEY       IF SAME KEY AS LAST RECD,                    
         BNE   BLDK60               THEN MERGE MKT BREAKS                       
*                                                                               
         LA    RE,AOINTD           UNLESS IT'S A DIFFERENT DAY,                 
         CLC   INTDAYWK,INTDAYWK-INTERD(RE)                                     
         BNE   BLDK60               BUILD INDIVIDUAL RECDS FOR EACH DAY         
*                                                                               
         MVI   MERGE,C'Y'                                                       
         B     BLDKEYX                                                          
*                                                                               
BLDK60   OC    AOKEY,AOKEY         DIFFERENT KEYS RELS LAST RECD 1ST            
         BZ    BLDKEYX              UNLESS IT WAS A ZERO KEY                    
         MVI   RELS,C'Y'           RELEASE PREV RECD 1ST                        
*                                                                               
BLDKEYX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
*BLDELEM - BUILD ELEMENTS FOR RECORDS                                           
*********************************************************************           
*                                                                               
BLDELEM  NTR1                                                                   
         LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET TYPE ELEMENT                    
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT       215 FOR TCAR                                 
         MVC   MARTYPE,INTMTYP                                                  
         MVI   MARSTYP,2           CHANGED FROM INTSTYP                         
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
         CLC   INTBOOK,=AL2(JAN_06)                                             
         BL    BLDEL20                                                          
         L     RF,=A(BLDDT)                                                     
         BASR  RE,RF                                                            
         GOTO1 APUTEL,TEMP                                                      
*                                                                               
         USING PHTELEM,R6          BUILD NETWORK PROGRAM TYPE ELEMENT           
BLDEL20  XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
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
*                                                                               
         CLI   INTRTYP,PMCODEQU    ************* -Q- ONLY ***********           
         BNE   BLDEL70                                                          
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
         BNE   BLDEL70                                                          
         LA    R5,INTVAR                                                        
         USING INTVARD,R5                                                       
         LA    R2,7                7 DAYS                                       
         LA    R3,X'40'            START WITH MONDAY                            
BLDEL50  CLI   0(R5),0                                                          
         BE    BLDEL60                                                          
         XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ                                                    
         MVC   NTSQH,INTVSQ        START QH                                     
         MVC   NTEQH,INTVEQ        END QH                                       
         MVC   NTDUR,INTVSDUR      SHORT DURATION IN MINUTES (MAX 240)          
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
BLDEL60  LA    R5,INTVARLQ(R5)    NEXT DAY IN INTVAR                            
         SRL   R3,1                NEXT INDIVIDUAL DAY                          
         BCT   R2,BLDEL50                                                       
         DROP  R5                                                               
*                                  ************* -P- & -Q- **********           
         USING PPTELEM,R6                                                       
BLDEL70  CLI   INTTITLE,C' '       BUILD EPISODE TITLE ELEMENT                  
         BNE   *+14                                                             
         CLC   INTTITLE(L'INTTITLE-1),INTTITLE+1                                
         BE    BLDEL80             BYPASS IF NO EPISODE PROVIDED                
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
*                                                                               
BLDEL80  CLI   INTRTYP,PMCODEQU    ************* -Q- ONLY ***********           
         BNE   BLDELEMX                                                         
*                                                                               
         USING CRRELEM,R6                                                       
         OC    BOOKSW,BOOKSW       BUILD CORRECTION ELEMENT                     
         BZ    BLDELEMX            BYPASS IF REGULAR BOOK                       
         MVI   CRRCODE,CRRCODEQ                                                 
         MVI   CRRLEN,CRRLENEQ                                                  
         MVI   CRRTYPE,X'FF'       TYPE WILL BE RESET IN UPDATE ROUTINE         
         MVC   CRROBOOK,INTPNO     ORIGINATING BOOK                             
         GOTO1 APUTEL,CRRELEM                                                   
*                                                                               
BLDELEMX B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*BLDDEMS- BUILD SECTION LEAD ELEM AND ADD DEMO ELEMS ON AOREC                   
*********************************************************************           
BLDDEMS  NTR1                                                                   
         LA    R6,TEMP                                                          
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
         LA    RE,INTKEY           MKT BRK IS AT INTKEY+25                      
         MVC   SLSECT,25(RE)                                                    
         GOTO1 APUTEL,SLELEM                                                    
*                                  BUILD DEMO ELEMENTS                          
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,PRFRSTEL-PRKEY(RE)                                            
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
         BZ    BLDDEMX             NONE-PUT RECORD TO TAPE                      
         LA    R1,CONDWORK+2                                                    
         SR    R0,R0                                                            
*                                                                               
BLDD50   CLI   0(R1),0                                                          
         BE    BLDDEMX                                                          
         GOTO1 APUTEL,(R1)         SLOTS AT END OF RECD IN AOREC                
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLDD50                                                           
*                                                                               
BLDDEMX  B     XIT                                                              
*********************************************************************           
*BLDNREC - BULILD N-RECDS AND DO SOME ADDITIONAL PROCESSING                     
*********************************************************************           
BLDNREC  NTR1                                                                   
         LA    R9,AOINTD           INTERD WORKS OFF FLDS OF AOREC RELSD         
         USING INTERD,R9                                                        
         LA    R2,1                SET COUNTER                                  
         MVI   HALF,C'Y'           SET SWITCH FOR SINGLE DAY                    
         CLI   INTSTA+3,C'S'       TEST FOR SEASON-TO-DATE (ONLY 'Q'S)          
         BE    *+12                YES-TREAT IT LIKE A 'P'                      
         CLI   INTRTYP,PMCODEQU    TEST FOR NETWORK PROG RECORD (Q)             
         BE    BLDN50                                                           
*                                  ************* -P- ONLY ***********           
         CLI   INTSTYP,C'B'        DOESN'T APPLY TO BRKOUTS                     
         BE    BLDN30                                                           
         L     R1,ACOMWRK          PT TO PRGAVG TABLE CREATED IN IPHASE         
BLDN10   OC    0(3,R1),0(R1)                                                    
         BZ    BLDN30              NOT FOUND?                                   
         CLC   INTSTA(3),0(R1)                                                  
         BNE   *+14                                                             
         CLC   INTPNUM,4(R1)       PROGRAM FOUND                                
         BE    BLDN20                                                           
         LA    R1,7(R1)            L'PRGAVG                                     
         CLC   0(2,R1),=X'FFFF'    END OF TABLE?                                
         BE    BLDN30                                                           
         B     BLDN10                                                           
                                                                                
*                                                                               
BLDN20   DS    0H             MAKE SURE M-F/M-S/VAR N-REC COMES OUT             
         LA    R6,WORK                                                          
         USING PNKEY,R6                                                         
         XC    WORK,WORK                                                        
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
BLDN30   ZIC   R1,PREVDW           FIND ACTUAL DAY/WEEK IN KEY                  
         SRL   R1,4                                                             
         STC   R1,HALF                                                          
         LA    R2,5                                                             
         LA    R4,X'40'            R4=MASK FOR DAY BITS (MON-SUN)               
         LA    R3,X'10'            START AT MONDAY AND EXPLODE SINGLE           
         CLI   HALF,0              DAYS FOR M-F OR M-SUN RECS,                  
         BE    BLDN50              ALSO ADDING PASSIVE RECS UNDER M-F,          
         LA    R2,7                M-SUN AND VAR                                
         CLI   HALF,8              TEST FOR M-SUN                               
         BE    BLDN50                                                           
         CLI   HALF,9              TEST FOR VAR                                 
         BE    BLDN50                                                           
         LA    R2,1                                                             
         MVI   HALF,C'Y'           SET SWITCH FOR SINGLE DAY                    
         B     BLDN50                                                           
*                                                                               
         EJECT                                                                  
BLDN40   TM    INTDYBIT,0          EXECUTED CODE (-P- ONLY)                     
*                                                                               
*                                  ************* -P- & -Q- **********           
BLDN50   CLI   HALF,C'Y'           TEST FOR SINGLE DAY                          
         BE    *+12                                                             
         EX    R4,BLDN40           SHOULD THIS DAY BE SELECTED ?                
         BZ    BLDN80                                                           
         LA    R6,WORK             BUILD 'N' RECORD KEY                         
         USING PNKEY,R6                                                         
         XC    WORK,WORK                                                        
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
         BE    BLDN60                                                           
         STC   R3,PNDW                                                          
         MVC   DUB(1),PREVDW                                                    
         NI    DUB,X'0F'           ISOLATE WEEK NIBBLE                          
         OC    PNDW,DUB            COMBINE DAY AND WEEK NIBBLES                 
*                                                                               
BLDN60   MVC   PNPNUM,INTPNUM                                                   
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
         BNE   BLDN80                                                           
         L     RF,=A(PROGBUF)      PREVIOUSLY DONE TABLE                        
         CLC   PRVPKSTA,PNSTAT                                                  
         BNE   *+10                                                             
         CLC   PRVPKBK,PNBOOK                                                   
         BNE   *+10                                                             
         CLC   PRVPKBTYP,PNBTYP                                                 
         BE    BLDN70                                                           
         L     RE,=A(PROGBUF)                                                   
         L     RF,=F'6000'                                                      
         XCEF                                                                   
         L     RF,=A(PROGBUF)                                                   
         MVC   PRVPKSTA,PNSTAT                                                  
         MVC   PRVPKBTYP,PNBTYP                                                 
         MVC   PRVPKBK,PNBOOK                                                   
*                                                                               
BLDN70   OC    0(2,RF),0(RF)                                                    
         BZ    BLDN75                                                           
         CLC   PNPNUM,0(RF)        DONE BEFORE                                  
         BE    BLDN80               DON'T SEND DUPS                             
         LA    RF,2(RF)                                                         
         B     BLDN70                                                           
         SPACE 1                                                                
BLDN75   MVC   0(2,RF),PNPNUM      SAVE PROGRAM NUMBER                          
         LA    RF,THISKEY2                                                      
         USING PKKEY,RF                                                         
         XC    THISKEY2,THISKEY2                                                
         MVC   PKKEY(2),=C'KN'                                                  
         MVC   PKSRC,INTKSRC                                                    
         MVC   PKPNUM+L'PKPNUM-L'PNPNUM(L'PNPNUM),PNPNUM                        
         MVC   PKBOOK,PNBOOK                                                    
         MVC   PKSTA,PNSTAT                                                     
         MVC   PKBTYP,PNBTYP                                                    
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY2)                                     
         GOTO1 PUTTAPE,0                                                        
         SPACE 1                                                                
BLDN80   SRL   R4,1                BUMP TO NEXT INDIVIDUAL DAY                  
         LA    R3,X'10'(R3)        BUMP TO NEXT DAY CODE                        
         BCT   R2,BLDN50                                                        
*                                                                               
         CLI   HALF,C'Y'           TEST FOR SINGLE DAY                          
         BE    BLDNX                                                            
         LA    R2,1                                                             
         MVI   HALF,C'Y'           NOW ADD THE M-F, M-SUN OR VAR RCD            
         B     BLDN50                                                           
*                                                                               
BLDNX    B     XIT                 ALL DONE                                     
         EJECT                                                                  
*********************************************************************           
*CRCTN - PROCESS CORRECTION RECD                                                
*********************************************************************           
CRCTN    NTR1                                                                   
*                                                                               
         CLI   LASTREC,C'Y'        NO MORE CORRECTIONS, PUT LASTREC             
         BE    CRC30                                                            
         CLI   INTKEY+2,C'P'       TEST CORRECTION RECORD                       
         BE    XIT                 BYPASSING 'P' FOR NOW                        
         CLI   INTKEY+2,C'Q'       TEST CORRECTION RECORD                       
         BNE   CRC30                                                            
*                                                                               
CRC10    DS    0H                                                               
*        XC    AOKEY,AOKEY         DISABLE AOKEY LOGIC                          
         XC    PREVSQH,PREVSQH                                                  
         XC    PREVDW,PREVDW                                                    
         OC    PREVCORR,PREVCORR   TEST 1ST-TIME-THRU                           
         BZ    CRC20                                                            
         CLI   INTKEY+2,C'P'                                                    
         BNE   *+14                                                             
         CLC   PREVCORR(PRBTYP-PRKEY+1),INTKEY  P-KEY THRU BKTYPE               
         BNE   *+10                                                             
         CLC   PREVCORR(PMBTYP-PMKEY+1),INTKEY  Q-KEY THRU BKTYPE               
         BNE   CRC15                                                            
*                                                                               
         CLC   INTPNUM,AOKEY+18    SAME PROGRAM?                                
         BE    CRC14                                                            
         GOTO1 PUTTAPE,0           PUT OUT PREVIOUS MERGED DATA                 
         B     CRC20               THEN PROCESS CURRENT RECORD IN INT           
*                                                                               
CRC14    BAS   RE,BLDDEMS          MERGE ONTO LAST MARKET BREAK                 
         B     CRCX                                                             
*                                                                               
CRC15    CLI   EOFSW,1             EOF ON FILE ALREADY                          
         BE    CRC20               DON'T OUTPUT                                 
         GOTO1 PUTTAPE,0                                                        
         GOTO1 PUTTAPE,4           PUT REMAINDER OF PREVIOUS CORR BOOK          
*                                                                               
CRC20    DS    0H                                                               
         XC    PREVCORR,PREVCORR                                                
         CLI   INTKEY+2,C'P'                                                    
         BNE   *+14                                                             
         MVC   PREVCORR(PRBTYP-PRKEY+1),INTKEY  P-KEY THRU BKTYPE               
         B     *+10                                                             
         MVC   PREVCORR(PMBTYP-PMKEY+1),INTKEY  Q-KEY THRU BKTYPE               
*                                                                               
         MVC   CORROBK,INTPNO                                                   
         MVC   CORRDYWK,INTDAYWK                                                
         MVC   CORRKSRC,INTKSRC                                                 
         MVC   CORRSQH,INTSQH                                                   
         MVC   CORRPNUM,INTPNUM                                                 
*                                                                               
         MVI   EOFSW,0             RESET EOF ON FILE                            
         BAS   RE,BLDKEY                                                        
         GOTO1 ABLDREC,DMCB,THISKEY  SETUP THISKEY IN AOREC                     
         MVC   AOKEY,THISKEY                                                    
         MVC   AOINTD,INTERD                                                    
         BAS   RE,BLDELEM          BUILD MISC RECD ELEMS                        
         BAS   RE,BLDDEMS                                                       
         B     CRCX                                                             
*                                                                               
CRC30    OC    BOOKSW,BOOKSW       TEST PREVIOUS CORRECTION BOOK                
         BZ    CRCX                                                             
         CLI   EOFSW,1             EOF ON FILE ALREADY                          
         BE    CRC40               DON'T OUTPUT                                 
         GOTO1 PUTTAPE,0           PUT RECORD                                   
         GOTO1 PUTTAPE,4           PUT REMAINDER OF PREVIOUS CORR BOOK          
*                                                                               
CRC40    XC    BOOKSW,BOOKSW                                                    
         MVI   EOFSW,0                                                          
         XC    AOKEY,AOKEY         RESET AOKEY                                  
         XC    PREVSQH,PREVSQH                                                  
         XC    PREVDW,PREVDW                                                    
*                                                                               
CRCX     B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*LASTHK -RELEASE LAST RECD IN AOREC AND UPDATED BIT MAPS                        
*********************************************************************           
LASTHK   DS    0H                                                               
         OC    AOKEY,AOKEY         RELEASE LAST RECD IN AOREC?                  
         BZ    LST10                                                            
         GOTO1 APUTTAPE,0          YES                                          
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   *+8                                                              
         BAS   RE,ADDCTBK          CALL DEBKUPDT TO ADD NEW BOOK                
*                                                                               
         BAS   RE,BLDNREC          BUILD NRECS AND DO MISC STUFF                
*                                                                               
LST10    OC    BOOKSW,BOOKSW       PUT OUT REMAINDR OF PREV CORR BOOK?          
         BZ    LST20               NOTHING TO RELEASE                           
         CLI   EOFSW,1             REACHED END OF CORR BOOK?                    
         BE    LST20               YES                                          
         GOTO1 PUTTAPE,4           NO, RELEASE REST OF BOOK                     
*                                                                               
LST20    DS    0H                  RELEASE UPDATED BIT MAP                      
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
         DS    0H                  GENERATE J-RECS                              
         GOTO1 VNTIPRG,DMCB,=C'JREC',(0,VBITMAP1),0                             
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   LST40                                                            
         XC    DBUPARMS,DBUPARMS        RELEASE CONTROL RECORDS                 
         LA    RE,DBUPARMS              TO CTAPE                                
         USING DBUPARMD,RE                                                      
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         DROP  RE                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,RELQ   RELEASE                        
*                                                                               
LST40    B     MAINX                                                            
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO SIMPLY PUT A RECORD TO TAPE FOR A REGULAR BOOK.                
*             FOR AN IRREGULAR BOOK (CORRECTION RECORDS),                       
*             THE EXISTING BOOK MUST BE READ IN, UPDATED AND REWRITTEN.         
*********************************************************************           
PUTTAPE  NTR1                                                                   
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   *+8                                                              
         BAS   RE,ADDCTBK          CALL DEBKUPDT TO ADD NEW BOOK                
*                                                                               
         MVI   OLDBK,C'N'                                                       
         OC    BOOKSW,BOOKSW                                                    
*        BNZ   PT100                                                            
         BZ    PT50                                                             
         CLI   THISKEY,C'P'                                                     
         BNE   PT100                                                            
*                                                                               
PT50     GOTO1 APUTTAPE            REGULAR BOOK                                 
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
*        BNE   *+6                                                              
*        DC    H'0'                                                             
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
*********************************************************************           
* CALL DEBKUPDT FOR EACH P AND Q REC CREATED. DEBKUPDT KEEPS TRACK OF           
* THE BOOKS BEING LOADED AND WILL END UP CREATING CONTROL RECORDS WHEN          
* CALLED WITH RELEASE MODE.                                                     
*********************************************************************           
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
*********************************************************************           
*PKEY -  PROCESS PROGRAM AVERAGE DIRECTORY ENTRIES                              
*********************************************************************           
*                                                                               
PKEY     NTR1                                                                   
         USING PRKEY,R2                                                         
         CLI   EOFSW,1             ALREADY AT EOF?                              
         BE    PKEY90              YES, DON'T READ SEQ                          
         LA    R2,KEYP                                                          
         CLI   PRCODE,PRCODEQU     TEST 1ST-TIME-THRU                           
         BE    PKEY30              NO                                           
         XC    KEYP,KEYP                                                        
         MVC   PRCODE(2),=C'PN'    YES - SET UP KEY                             
         MVC   PRSRC,INTKSRC                                                    
         MVC   PRSTAT,INTSTA       STATION (NETWORK)                            
         MVC   PRSTYP,INTSTYP      BREAKOUT/SPECIAL INDICATOR                   
         MVC   PRBOOK,BOOKSW       CORRECTION BOOK (YYWW)                       
         MVC   PRBTYP,BOOKTYPE     BOOK TYPE ('V' OR 'T')                       
         CLI   OLDBK,C'Y'          SET FIELDS IF OLD BK                         
         BNE   PKEY10              NO                                           
         MVC   PRSTAT,SVSTA        YES, RESET STATION'                          
         MVC   PRBTYP,SVBK         RESET BK                                     
         MVC   PRSTYP,SVSTYP       RESET INTSTYP                                
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
*********************************************************************           
*QKEY -  PROCESS NETWORK PROGRAM ENTRIES                                        
*********************************************************************           
*                                                                               
QKEY     NTR1                                                                   
         USING PMKEY,R2                                                         
         CLI   EOFSW,1                                                          
         BE    QKEY90                                                           
         LA    R2,KEYQ                                                          
         CLI   PMCODE,PMCODEQU     TEST 1ST-TIME-THRU                           
         BE    QKEY30              NO                                           
         XC    KEYQ,KEYQ                                                        
         MVC   PMCODE(2),=C'QN'    YES - SET UP KEY                             
         MVC   PMSRC,INTKSRC                                                    
         MVC   PMBOOK,BOOKSW       CORRECTION BOOK (YYWW)                       
         MVC   PMSTAT,PREVCORR+PMSTAT-PMKEY                                     
         MVC   PMSTYP,PREVCORR+PMSTYP-PMKEY                                     
         MVC   PMBTYP,PREVCORR+PMBTYP-PMKEY                                     
*        MVC   PMSTAT,INTSTA       STATION (NETWORK)                            
*        MVC   PMSTYP,INTSTYP      BREAKOUT/SPECIAL INDICATOR                   
*        MVC   PMBTYP,BOOKTYPE     BOOK TYPE ('A' OR 'I')                       
         CLI   OLDBK,C'Y'          TEST IF PREV BK                              
         BNE   QKEY10              NO                                           
         MVC   PMSTAT,SVSTA        YES, RESET STATION                           
         MVC   PMBTYP,SVBK         YES, RESET BOOOK                             
         MVC   PMSTYP,SVSTYP       YES, RESET INTSTYP                           
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
         BNE   SVFLDQ              -                                            
         MVC   SVSTA,PRSTAT        -                                            
         MVC   SVBK,PRBTYP         SAVE BOOK  TYPE                              
         MVC   SVSTYP,PRSTYP                                                    
         DROP  R2                                                               
*                                                                               
         USING PMKEY,R2                                                         
SVFLDQ   CLI   PMCODE,PMCODEQU     -                                            
         BNE   SVFLDX              -                                            
         MVC   SVSTA,PMSTAT        -                                            
         MVC   SVBK,PMBTYP         SAVE BOOK TYPE                               
         MVC   SVSTYP,PMSTYP                                                    
         MVC   SVPNO,INTPNO                                                     
         MVC   SVDAYWK,INTDAYWK                                                 
         MVC   SVSQH,INTSQH                                                     
         MVC   SVPNUM,INTPNUM                                                   
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
         MVC   KEY,0(RE)           FOUND KEY                                    
         MVC   SVDA,PRNDXDA-PRKEY(RE)                                           
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
         MVC   PRRSTAT-PRKEY(1,RE),SVSTATUS                                     
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
         MVC   CHSRC,CORRKSRC      -N-                                          
         MVC   CHOBOOK,CORROBK     ORIGINATING BOOK                             
         MVC   CHSTAT,SVSTA                                                     
         MVC   CHBTYP,SVBK                                                      
         MVC   CHDW,CORRDYWK                                                    
         CLI   PREVCORR+2,PMCODEQU                                              
         BNE   *+8                                                              
         MVI   CHDW,X'90'          SET DAY TO VARIOUS FOR 'Q' REC               
         MVC   CHSTIM,CORRSQH                                                   
         MVC   CHPNUM,CORRPNUM                                                  
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
SVDA     DS    F                   DA OF NTI FILE                               
SVSTATUS DS    C                   STATUS BYTE FOR NTIFILE                      
PNUMTAB  DS    20XL2                                                            
THISKEY  DC    XL20'00'                                                         
THISKEY2 DC    XL20'00'                                                         
*                                  LAST TIME VALUES                             
PREVSQH  DC    X'00'                                                            
PREVDW   DC    X'00'                                                            
*                                                                               
AOINTD   DS    XL(INTACCS-INTERD)  INT FIELD FOR RECD BUILD IN AOREC            
AOKEY    DC    XL20'00'                                                         
*                                                                               
PRVPKSTA DS    CL5                 PREVIOUS 'K' VALUES                          
PRVPKBK  DS    XL2                                                              
PRVPKBTYP DS   C                                                                
*                                                                               
MERGE    DC    X'00'                                                            
RELS     DC    X'00'                                                            
*                                                                               
OFORMAT  DS    0CL10                                                            
         DC    C'NTINPNN',3X'00'                                                
*                                                                               
RATING   DC    X'00',C'R',X'01'                                                 
PAVFIL   DC    C'PAVFIL  '         HELLO FILE NAME                              
SORTCARD DC    C'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1 '                        
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
* EQUATES                                                                       
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
BOOKS    EQU   2                                                                
*                                                                               
* CONSTANTS                                                                     
*                                                                               
PAVDIR   DC    CL8'PAVDIR'                                                      
NTIDIR   DC    CL8'NTIDIR'                                                      
NTIFIL   DC    CL8'NTIFIL'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYS    DC    CL8'SPOT'                                                        
*MSYSFLS DC    C'UPAVDIR NPAVFIL X'                                             
*                                                                               
         DS    0F                  CORRECTION RECORDS FIELDS                    
SVAOREC  DS    A                   SAVE ADDRESS OF OUTPUT RECORD                
*                                                                               
PREVCORR DS    0XL18                PREVIOUS CORRECTION KEY                     
BOOKSW   DC    X'0000',16X'00'     PREV CORRECTION BOOK & KEY                   
CORROBK  DS    CL2                 CORRECTION ORIGINATING BOOK                  
CORRDYWK DS    XL1                 CORRECTION DAY OF THE WEEK                   
CORRKSRC DS    CL1                 CORRECTION SOURCE                            
CORRSQH  DS    XL1                 CORRECTION START QH                          
CORRPNUM DS    XL2                 CORRECTION PROGRAM NUMBER                    
*                                                                               
ADDCHA   DS    C                   A=ADD   C=CHANGE                             
TAPE     DS    C                   TAPE TYPE--INTSTA+4                          
RDHIPSW  DC    X'00'               LAST READ WAS HIGH P                         
RDHIQSW  DC    X'00'               LAST READ WAS HIGH Q                         
EOFSW    DC    X'00'               EOF ON FILE FOR A BOOK                       
OLDBK    DC    C'N'                FLAG TO RELEASE OF REST OF BOOK              
SVSTA    DC    XL5'00'             CORRECTION RECD'S  STATION                   
SVBK     DS    X'00'               CORR RECD'S BOOK  TYPE                       
SVSTYP   DS    X'00'               CORR RECD'S BOOK  TYPE                       
SVPNO    DS    XL2                                                              
SVDAYWK  DS    XL1                                                              
SVSQH    DS    XL1                                                              
SVPNUM   DS    XL2                                                              
LASTREC  DC    X'00'                                                            
*                                                                               
KEYP     DC    XL23'00'            -P- KEY                                      
KEYQ     DC    XL23'00'            -Q- KEY                                      
KEYN     DC    XL23'00'            -N- KEY                                      
COMMAND  DS    CL8                                                              
KEY      DS    CL23                                                             
KEYSAVE  DS    CL23                                                             
IOSAVE   DS    CL23                SAVE AREA FOR REREAD                         
IOAREA   DS    CL1024                                                           
DBUPARMS DS    XL(DBUPARML)        PARAMETER AREA FOR DEBKUPDT                  
PROGBUF  DS    6000C                                                            
*                                                                               
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
         ICM   RF,15,CDEMTABS                                                   
         GOTO1 (RF),DMCB,VWDESCTB                                               
         ICM   R1,15,DMCB                                                       
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
BDTX     XIT1                                                                   
*-------------------------------------------------------------------            
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
TCRWRKD  DSECT                                                                  
SAVEREG  DS    A                                                                
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
TCRWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTNT3DL                                                     
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
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
**PAN#1  DC    CL21'088DETC09O   10/04/11'                                      
         END                                                                    
